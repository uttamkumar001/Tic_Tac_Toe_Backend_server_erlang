%  This Erlang module implements a supervisor (sup1) managing child processes (queue_server and match_making) for handling and dispatching client data efficiently. 

-module(sup1).
-behaviour(supervisor).

% Exported functions
-export([start_link/0, init/1, send_to_queue_server/1]).

% Start the supervisor and link it to the calling process
start_link() ->
    supervisor:start_link({local, sup1}, sup1, []).

% Initialize the supervisor with child specifications
init([]) ->
    QueueServer = {
      child1,  % Child ID
      {queue_server, start_link, [child1]},  % Child start function
       permanent,  % Restart strategy
       5000,  % Restart intensity (time in ms)
       worker,  % Child type
      [queue_server]  % Modules
    },
    MatchMaking = {
     child2,  % Child ID
     {match_making, start_link, [child2]},  % Child start function
      permanent,  % Restart strategy
      5000,  % Restart intensity (time in ms)
      worker,  % Child type
     [match_making]  % Modules
    },
   % Supervisor specification with one_for_one strategy
{ok, {{one_for_one, 5, 100}, [QueueServer, MatchMaking]}}.



send_to_queue_server(Data) ->
        % Get the list of child processes managed by the supervisor
    Children = supervisor:which_children(sup1),
        % Extract the PID of the first child (queue_server)
    [{_, Pid, _, _} | _ ] =Children,   
        % Prepare data to be sent to the queue server    
    Data_2_send = [Data,Pid],
        % Send a cast message to the queue server with the data
    gen_server:cast(child1, {send_data, Data_2_send}).

           


