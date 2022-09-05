%%%-------------------------------------------------------------------
%% @doc sender public API
%% @end
%%%-------------------------------------------------------------------

-module(sender_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, C} = epgsql:connect(#{
        host => "db",
        username => "postgres",
        password => "postgres",
        database => "postgres",
        timeout => 4000
    }),
    pgsql_migration:migrate(C, "priv/migrations"),
    sender_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
