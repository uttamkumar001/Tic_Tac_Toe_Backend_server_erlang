%  This Erlang module (game_logic) implements a server for managing a tic-tac-toe game between two players over TCP,
%  handling game state, player moves, timeouts, and win/draw conditions with supervision and communication features.

-module(game_logic).
-behaviour(gen_server).

-record(state, {
    board = #{},
    player1,
    player2,
    reply1,
    reply2,
    symbol,
    position,
    current_player,
    status = "ongoing", % Status can be ongoing, draw, win
    timer_ref=undefined,
    id

}).


-export([start_link/5, init/1, handle_info/2, handle_cast/2,terminate/2, code_change/3,handle_call/3]).

% Start the gen_server with player and game IDs
start_link(A_id,A_sckt_id, B_id,B_sckt_id,Id) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,[A_id,A_sckt_id, B_id,B_sckt_id,Id], []).
   
% Initialize the state of the game
init([Player1,Reply_to_1, Player2,Reply_to_2,Id]) ->
    Board = maps:new(),   
    State = #state{board = Board, player1 = Player1,reply1 = Reply_to_1, reply2 = Reply_to_2, player2 = Player2, current_player = Player1,symbol = "",position = "",timer_ref = undefined,id=Id},
    {ok, State}.  

% Handle cast messages
handle_cast({message_from_queue_server, Data}, State) ->

    % Cancel any previous timer if any

    case State#state.timer_ref of
        undefined -> ok;
        _ -> erlang:cancel_timer(State#state.timer_ref)
    end,
    % Extract the player and position data
    [Recent_player, Position_marked] = Data,

    % Check if the position is already taken 

    case maps:is_key(Position_marked, State#state.board) of
        true ->

            % Send error response if position is taken

            Response = "{\"namespace\":\"Error\",\"Description\":\"Position already taken\"}",
            gen_tcp:send(State#state.reply1, Response),
            gen_tcp:send(State#state.reply2, Response),
            {noreply, State};
        false ->

            % Update the board with the new move

            NewBoard = maps:put(Position_marked, Recent_player, State#state.board),
           
            NewState = State#state{board = NewBoard, current_player = next_player(Recent_player, State)},
            Next_player = next_player(Recent_player, State),
            Symbol_marked = check_prev_marked_symbol(Recent_player, State),

            % Send move successful response to the Players 
            Response = "{\"namespace\":\"move_successful\",\"timer\":\"7000\",\"index\":\"" ++ Position_marked ++ "\",\"symbol\":\"" ++ Symbol_marked ++ "\"}",
            gen_tcp:send(State#state.reply1, Response),
            gen_tcp:send(State#state.reply2, Response),
            A_sckt_id = State#state.reply1,
            B_sckt_id = State#state.reply2,

            % Check if the game is won or drawn

            FinalState = final_state(NewState),
            case FinalState#state.status of
                "win" ->
                    {noreply, FinalState};
                "draw" ->
                    {noreply, FinalState};
                "ongoing" ->

                    % Set a new timer for the next player

                    TimerRef = erlang:send_after(7000, self(), {timeout_move, Next_player, NewBoard}),
                    UpdatedState = FinalState#state{timer_ref = TimerRef},

                    % Notify the next player to move
                if 
                                            Recent_player == State#state.player1 -> 
                                                Response1 = lists:flatten(io_lib:format("{\"namespace\":\"move\",\"turn\":~p,\"symbol\":\"o\"}", [Next_player])),
                                                gen_tcp:send(A_sckt_id, Response1),
                                                gen_tcp:send(B_sckt_id, Response1);
                                            Recent_player == State#state.player2 ->
                                                Response2 = lists:flatten(io_lib:format("{\"namespace\":\"move\",\"turn\":~p,\"symbol\":\"x\"}", [Next_player])),
                                                gen_tcp:send(A_sckt_id, Response2),
                                                gen_tcp:send(B_sckt_id, Response2);
                                            true -> 
                                                io:fwrite("False")
                                        end,
                    {noreply, UpdatedState}
                end
    end;
handle_cast({exit_player, Data}, State) ->
        
         [D_id] = Data ,
         NewState = State#state{status = "win"},
        if
        D_id == State#state.player1 ->
         Response = lists:flatten(io_lib:format("{\"namespace\":\"result\",\"winner\":~p,\"description\":\"win\"}", [State#state.player2])),
         B_sckt_id = State#state.reply2,
         gen_tcp:send(B_sckt_id, Response);

        D_id == State#state.player2 ->
         Response2 = lists:flatten(io_lib:format("{\"namespace\":\"result\",\"winner\":~p,\"description\":\"win\"}", [State#state.player1])),
         A_sckt_id = State#state.reply1,
         gen_tcp:send(A_sckt_id, Response2);

         true ->
             io:fwrite("Error in exit ~n") 
        end,
    {noreply, NewState};
    handle_cast(stop, State) ->
       {stop, normal, State};
    handle_cast(_Other, State) ->
       {noreply, State}.
 
% Handle timeout move message
handle_info({timeout_move, Next_player, Board}, State) ->

        % Find the first unmarked position and update the board

        Unmarked_pos = find_first_unmarked_position(Board),
        NewBoard = maps:put(integer_to_list(Unmarked_pos), Next_player, State#state.board),
        NewState = State#state{board = NewBoard, current_player = next_player(Next_player, State)},
        Symbol_marked = check_prev_marked_symbol(Next_player, State),

        % Send move successful response

        Response = io_lib:format("{\"namespace\":\"move_successful\",\"timer\":\"7000\",\"index\":\"~p\",\"symbol\":\"~s\"}", [Unmarked_pos, Symbol_marked]),
        gen_tcp:send(State#state.reply1, Response),
        gen_tcp:send(State#state.reply2, Response),
        A_sckt_id = State#state.reply1,
        B_sckt_id = State#state.reply2,
        Nxt_player = next_player(Next_player, State),
        FinalState = final_state(NewState),
        case FinalState#state.status of
            "win" ->
                {noreply, FinalState};
            "draw" ->
                {noreply, FinalState};
            "ongoing" ->
                TimerRef = erlang:send_after(7000, self(), {timeout_move, NewState#state.current_player, NewBoard}),
                UpdatedState = FinalState#state{timer_ref = TimerRef},
                if 
                      Next_player == State#state.player1 -> 
                        Response1 = lists:flatten(io_lib:format("{\"namespace\":\"move\",\"turn\":~p,\"symbol\":\"o\"}", [Nxt_player])),
                        gen_tcp:send(A_sckt_id, Response1),
                        gen_tcp:send(B_sckt_id, Response1);
                       Next_player == State#state.player2 ->
                        Response2 = lists:flatten(io_lib:format("{\"namespace\":\"move\",\"turn\":~p,\"symbol\":\"x\"}", [Nxt_player])),
                        gen_tcp:send(A_sckt_id, Response2),
                        gen_tcp:send(B_sckt_id, Response2);
                    true -> 
                        io:fwrite("False")
                end,
         
              {noreply, UpdatedState}
            end.

        
% Utility function to find the first unmarked position
find_first_unmarked_position(Board) ->
    % Convert string keys to integers and sort them
    SortedKeys = lists:sort([list_to_integer(Key) || {Key, _Value} <- maps:to_list(Board)]),
    % Find the first missing integer
    find_first_missing_integer(SortedKeys, 0).

find_first_missing_integer([], Expected) ->
    Expected;
find_first_missing_integer([Expected | Rest], Expected) ->
    find_first_missing_integer(Rest, Expected + 1);
find_first_missing_integer(_, Expected) ->
    Expected.

% Check the symbol of the previously marked position
check_prev_marked_symbol(Recent_player,State) ->
    if
        Recent_player == State#state.player1->
           "o";
        true ->
           "x"
        end.

% Terminate the server
terminate(_Reason, _State) ->
    ok.

% Handle code change
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Determine the next player
next_player(Player,State) ->
    if
        Player == State#state.player1 -> State#state.player2;
     true ->
           State#state.player1
    end.

% Determine the final state of the game (win, draw, or ongoing)

final_state(State) ->
    case check_win(State#state.board, State#state.player1) of
        true ->
            NewState = State#state{status = "win"},
            Response = lists:flatten(io_lib:format("{\"namespace\":\"result\",\"winner\":~p,\"description\":\"win\"}", [State#state.player1])),
            A_sckt_id = State#state.reply1,
            gen_tcp:send(A_sckt_id, Response),
            B_sckt_id = State#state.reply2,
            gen_tcp:send(B_sckt_id, Response),
            NewState;
        false ->
            case check_win(State#state.board, State#state.player2) of
                true ->
                    % io:format("True ~n"),
                    NewState = State#state{status = "win"},
                    Response = lists:flatten(io_lib:format("{\"namespace\":\"result\",\"winner\":~p,\"description\":\"win\"}", [State#state.player2])),
                    B_sckt_id = State#state.reply2,
                    gen_tcp:send(B_sckt_id, Response),
                    A_sckt_id = State#state.reply1,
                    gen_tcp:send(A_sckt_id, Response),
                    NewState;
                false -> 
                    case maps:size(State#state.board) == 9 of
                        true ->
                            NewState = State#state{status = "draw"},
                            Response = lists:flatten(io_lib:format("{\"namespace\":\"result\",\"winner\":\"Empty\",\"description\":\"draw\"}", [])),
                            B_sckt_id = State#state.reply2,
                            gen_tcp:send(B_sckt_id, Response),
                            A_sckt_id = State#state.reply1,
                            gen_tcp:send(A_sckt_id, Response),
                            NewState;
                        false ->
                            State
                    end
            end
    end.


% Check if a player has won
check_win(Board, Player) ->
    WinningCombinations = [
        ["0", "1", "2"],
        ["3", "4", "5"],
        ["6", "7", "8"],
        ["0", "3", "6"],
        ["1", "4", "7"],
        ["2", "5", "8"],
        ["0", "4", "8"],
        ["2", "4", "6"]
    ],
    lists:any(fun(Combination) ->
        lists:all(fun(Position) ->
            maps:get(Position, Board, none) == Player
        end, Combination)
    end, WinningCombinations).

handle_call(_Request, _From, State) ->
    {reply, State, State}.
