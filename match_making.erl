% This Erlang module (match_making) manages a queue system for pairing players into games,
%  handling timeouts and initiating game processes with communication between players over TCP.

-module(match_making).
-behaviour(gen_server).

-export([start_link/1, init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-record(state, {
    queue = queue:new()   % Initialize a new queue
}).



% Start the gen_server with a given name
start_link(Name) ->
    gen_server:start_link({local, Name}, match_making, [], []).
% Initialize the state
init([]) ->
    {ok, #state{}}.

% Handle synchronous call to parse the queue  
handle_call({parse_queue}, _From, State) ->
    {reply, State#state.queue, State}.
   
% % Handle asynchronous cast message from the queue server           
% handle_cast({message_from_queue_server, Data}, State) ->

%     NewQueue = queue:in(Data, State#state.queue),
%     schedule_timeout(),
%     {noreply, State#state{queue = NewQueue}}.
handle_cast({message_from_queue_server, Data}, State) ->
    [DeviceID,C_sckt] = Data,
    Queue = State#state.queue,
    io:format("D_id ~p~n",[DeviceID]),
    io:format("C+sckt_id ~p~n",[C_sckt]),
    % Check if the device ID is already in the queue
    case device_id_exists(DeviceID, Queue) of
        true ->
            % Device ID already exists, do not add it to the queue
            io:format("Device ID ~p already in the queue. Ignoring.~n", [DeviceID]),
            Response = "{\"namespace\":\"error\",\"description\":\"Already added in to Queue,Please wait for your turn\"}",
            gen_tcp:send(C_sckt, Response),
            {noreply, State};
        false ->
            % Device ID not in the queue, add it
            NewQueue = queue:in(Data, Queue),
            schedule_timeout(),
            {noreply, State#state{queue = NewQueue}}
    end.

% Helper function to check if a device ID exists in the queue
device_id_exists(DeviceID, Queue) ->
    lists:any(fun([ID, _]) -> ID =:= DeviceID end, queue:to_list(Queue)).

% Schedule a timeout event to process the queue           
schedule_timeout() ->
    erlang:send_after(2000, self(), timeout).   
% Handle timeout events        
handle_info(timeout, State) ->
    case queue:len(State#state.queue) of
        Size when Size > 1 ->
            % If there are more than one element, split the queue and spawn a new process
            {NewQueue, _Rest} = queue:split(2, State#state.queue),
            spawn_new_process(NewQueue),         
            {noreply, State#state{queue = _Rest}};
            1 ->
            % If there is only one element, log and wait for more
            io:format("Waiting for more elements in the queue~n"),
            {noreply, State};
            _ ->
            % No elements or unexpected case   
            {noreply, State}  
    end.


% Spawn a process for a new game with the players from the queue
spawn_new_process(Queue) ->

    {[[A_id,A_sckt_id]],[[B_id,B_sckt_id]]} = Queue,
    {ok,Table_id} = game_supervisor:start_game(A_id,A_sckt_id,B_id,B_sckt_id), 

    io:format("Process created for two player Player1 having id ~p and Player2 having id ~p have id : ~p~n",[A_id,B_id,Table_id]),

    Response = lists:flatten(io_lib:format("{\"namespace\":\"join\",\"table_id\":\"~p\",\"opponent_id\":~p,\"turn\":~p,\"symbol\":\"x\"}", [Table_id, B_id, A_id])),
    % Notify Player1
    gen_tcp:send(A_sckt_id,Response),

    % Notify Player2
    Response2 = lists:flatten(io_lib:format("{\"namespace\":\"join\",\"table_id\":\"~p\",\"opponent_id\":~p,\"turn\":~p,\"symbol\":\"x\"}", [Table_id, A_id, A_id])),
    gen_tcp:send(B_sckt_id,Response2).
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.