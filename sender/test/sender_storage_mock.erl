-module(sender_storage_mock).

-behaviour(sender_storage).

-export([init/0, upload/1]).

init() -> ok.

upload(#{<<"file_name">> := <<"Success.md">>}) ->
    {ok, 1};
upload(#{<<"file_name">> := <<"Already.md">>}) ->
    {ok, already_uploaded}.
