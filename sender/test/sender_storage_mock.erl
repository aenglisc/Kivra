-module(sender_storage_mock).

-behaviour(gen_server).
-behaviour(sender_storage).

-export([start/0, upload/1]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start() ->
    {ok, _Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    ok.

stop() ->
    gen_server:stop(?MODULE).

upload(Body) ->
    gen_server:call(?MODULE, {upload, Body}).

init(Storage) ->
    {ok, Storage}.

handle_call({upload, Body}, _From, Storage) ->
    case lists:search(fun({_ID, Item}) -> Item =:= Body end, Storage) of
        {value, {ID, _}} ->
            {reply, {ok, {already_uploaded, ID}}, Storage};
        false ->
            ID = length(Storage) + 1,
            NewStorage = [{ID, Body} | Storage],
            {reply, {ok, ID}, NewStorage}
    end.

handle_cast(_, State) ->
    {noreply, State}.
