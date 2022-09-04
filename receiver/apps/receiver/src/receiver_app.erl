%%%-------------------------------------------------------------------
%% @doc receiver public API
%% @end
%%%-------------------------------------------------------------------

-module(receiver_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    receiver_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
