-module(consumer_server).

-export([child_spec/0]).

-define(APP, consumer).
-define(DEFAULT_ACCEPTORS_POOLSIZE, 100).
-define(DEFAULT_IP_ADDR, "::").
-define(DEFAULT_PORT, 8081).

-spec child_spec() ->
    supervisor:child_spec().
child_spec() ->
    {Transport, TransportOpts} = get_socket_transport(),
    CowboyOpts = get_cowboy_config(consumer_logic_handler),
    ranch:child_spec(?MODULE, Transport, TransportOpts, cowboy_clear, CowboyOpts).

get_socket_transport() ->
    {ok, IP} = inet:parse_address(application:get_env(?APP, ip, ?DEFAULT_IP_ADDR)),
    Port = application:get_env(?APP, port, ?DEFAULT_PORT),
    AcceptorsPool = application:get_env(?APP, acceptors_poolsize, ?DEFAULT_ACCEPTORS_POOLSIZE),
    {ranch_tcp, #{socket_opts => [{ip, IP}, {port, Port}], num_acceptors => AcceptorsPool}}.

get_cowboy_config(LogicHandler) ->
    Routes = openapi_consumer_server_router:get_paths(LogicHandler),
    Dispatch = #{dispatch => cowboy_router:compile(Routes)},
    #{env => Dispatch}.
