%  This Erlang module implements a TCP server using the gen_server behavior, handling client connections and parsing JSON data for further processing.

-module(tcp_server).
-behavior(gen_server).


-export([start_link/0, init/1, handle_info/2, terminate/2,handle_call/3,handle_cast/2]).
%% State definition

-record(state, {
    queue = queue:new()   % Initialize a new queue
}).

%% API functions
start_link() ->
    gen_server:start_link({local, tcp_server}, tcp_server, [], []).

%% Initialization function
init([]) ->

     % Open a TCP socket for listening on port 9000
    {ok, ListenSocket} = gen_tcp:listen(9000, [{active, false}, {packet, 0},{reuseaddr, true}]),
    % Spawn a process to accept client connections
    spawn_link(fun() -> accept_clients(ListenSocket) end),
   
    % Start the necessary supervisors
    sup1:start_link(),
    game_supervisor:start_link(),

{ok,#state{queue = queue:new()}}.

accept_clients(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, ClientSocket} ->
            % Spawn a process to handle each client
            spawn_link(fun() -> handle_client(ClientSocket) end),
            accept_clients(ListenSocket);
        {error, _Reason} ->
            ok
    end.

handle_client(ClientSocket) ->
    case gen_tcp:recv(ClientSocket, 0) of
        {ok, Data} ->
         io:format("Data is ~p~n",[Data]),
        DataMap = parse_json((list_to_binary(Data))),
        Deviceid = maps:get(device_id,DataMap),
        Namespace = maps:get(namespace,DataMap),
        Table_id =maps:get(table_id, DataMap),
        Index= maps:get(index, DataMap),

        % Prepare data to send to the queue server
        Data_2_send = [binary_to_list(Deviceid), ClientSocket, binary_to_list(Namespace), binary_to_list(Table_id), binary_to_list(Index)],
       
        sup1:send_to_queue_server(Data_2_send),
        handle_client(ClientSocket);

        {error, closed} ->
            io:format("Client closed connection~n");

        Error ->
            io:format("Error: ~p~n", [Error])
    end.
parse_json(JsonStr) ->
    {ok, JsonTerm} = decode(JsonStr),
    store_json_data(JsonTerm).
    
decode(JsonStr) ->
    try
    JsonTerm = mochijson2:decode(JsonStr),
        {ok, JsonTerm}
    catch
         _:_->
         {error, invalid_json}
    end.

store_json_data(JsonTerm) ->
            % Extract relevant fields from the JSON term
    {_, Lstoftup} = JsonTerm,


    DeviceId    = proplists:get_value(<<"device_id">>, Lstoftup, <<"unknown">>),
    Name        = proplists:get_value(<<"name">>, Lstoftup, <<"unknown">>),
    Description = proplists:get_value(<<"description">>, Lstoftup, <<"unknown">>),
    Namespace   = proplists:get_value(<<"namespace">>, Lstoftup,<<"unknown">>),
    Table_id    = proplists:get_value(<<"table_id">>, Lstoftup,<<"unknown">>),
    Index       = proplists:get_value(<<"index">>, Lstoftup,<<"unknown">>),


    DataMap = #{device_id => DeviceId,
                name => Name,
                description => Description,
                namespace => Namespace,
                table_id => Table_id,
                index => Index
                },
    DataMap.
        

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
   
handle_call(_Request, _From, State) ->
  {reply, State, State}.
handle_cast(_Request, State) ->
    {noreply, normal, State}.