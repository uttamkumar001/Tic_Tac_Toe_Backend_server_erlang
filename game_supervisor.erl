% This Erlang module (game_supervisor) supervises dynamic game instances,
% ensuring fault tolerance and managing game logic processes for player interactions over TCP.

-module(game_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_game/4,init/1]).

%  start The Supervisor 
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
start_game(Player1, Reply_to_1, Player2, Reply_to_2) ->
    ChildId = make_ref(),  % Generate a unique identifier for each child process
    case supervisor:start_child(?MODULE, [Player1, Reply_to_1, Player2, Reply_to_2, ChildId]) of
        {ok, Pid} ->
          {ok, Pid};
        {error, {already_started, Pid}} ->
                %% Handle the case where the process is already started
                %% Terminate the existing process and start a new one
                % io:format("Process already started: ~p. Restarting...~n", [Pid]),
            supervisor:terminate_child(?MODULE, Pid),
            
        case supervisor:start_child(?MODULE, [Player1, Reply_to_1, Player2, Reply_to_2, ChildId]) of
            {ok, NewPid} ->
            {ok, NewPid};
            {error, Reason} ->
                {error, Reason}
            end;
            {error, Reason} ->
                {error, Reason}
    end.   

% Initialize the supervisor
init([]) ->
    % Define the child specification for game_logic processes
    GameLogicSpec = {
        game_logic,                      % Child ID
        {game_logic, start_link, []},    % Start function
        permanent,                       % Restart strategy
        10000,                           % Shutdown timeout in milliseconds
        worker,                          % Worker type
        [game_logic]                     % Modules implemented by the child
    },
    % Define the supervisor strategy
 {ok, {{simple_one_for_one, 5, 10000}, [GameLogicSpec]}}.