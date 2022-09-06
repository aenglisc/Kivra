%%%-------------------------------------------------------------------
%% @doc receiver public API
%% @end
%%%-------------------------------------------------------------------

-module(consumer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Storage = application:get_env(consumer, storage, consumer_storage),
    ok = Storage:start(),
    consumer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
