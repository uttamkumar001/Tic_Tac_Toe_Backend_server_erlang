%% This module checks the category of client packets, determining whether they are of "join" or "move" type, 
%% and sends the packet to the appropriate handler accordingly.

-module(queue_server).
-behaviour(gen_server).

-export([start_link/1, init/1,handle_call/3,handle_cast/2,terminate/2,code_change/3]).
-record(state, {
    queue = queue:new()   % Initialize a new queue
}).
%% API functions
start_link(Name) ->
    gen_server:start_link({local, Name}, queue_server, [], []).

init([]) ->
    {ok,#state{queue = queue:new()}}.

handle_call(_Request, _From, State) ->
    {reply, State, State}.

% Handle asynchronous cast to send data        
handle_cast({send_data, Data}, State) ->
    [[D_id,C_sckt,Ns,T_id,Idx]| Pid] = Data,

    if 
        Ns == "join" -> 
             % Handle "join" namespace
            [A] =Pid,

            gen_server:cast(A, {message_from_queue_server, [D_id,C_sckt]});


        Ns == "move" ->
             % Handle "move" namespace
            {_,B} = extract_pid_from_string(T_id), 
            Data_2_send = [D_id, Idx],

            gen_server:cast(B,{message_from_queue_server,Data_2_send});
        Ns == "exit" ->
            % Handle "exit" namespace
             io:format("Under name space exit ~n"),
            {_,E} = extract_pid_from_string(T_id), 
            Data_to_send = [D_id],

            gen_server:cast(E,{exit_player,Data_to_send});

        true -> 
            % Handle unrecognized namespace
           io:format("Unrecognized namespace: ~p~n", [Ns]),
           io:fwrite("False") 

    end,
    {noreply, State}.
extract_pid_from_string(String) ->
    %% Define the regex pattern to match the PID format 

    Pattern = "<[0-9]+\\.[0-9]+\\.[0-9]+>",    
    %% Use re:run to match the pattern

    case re:run(String, Pattern, [{capture, first, list}]) of
        {match, [PidString]} ->

            %% Convert the matched string to a PID
            Pid = list_to_pid(PidString),
            {ok, Pid};

        nomatch ->

            {error, not_a_pid}
    end.

% Handle termination of the server
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.