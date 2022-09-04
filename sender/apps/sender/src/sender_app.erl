%%%-------------------------------------------------------------------
%% @doc sender public API
%% @end
%%%-------------------------------------------------------------------

-module(sender_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sender_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
