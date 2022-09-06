-module(consumer_storage_mock).

-behaviour(gen_server).
-behaviour(consumer_storage).

-include("helpers.hrl").

-export([start/0, list_content/1, pay_for_content/1]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2]).

start() ->
    {ok, _Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
    ok.

stop() ->
    gen_server:stop(?MODULE).

list_content(<<?RECEIVER_ID_WITHOUT_CONTENT>>) ->
    [];
list_content(<<?RECEIVER_ID_WITH_CONTENT>>) ->
    [
        #{
            content_id => 1,
            receiver_id => <<"1">>,
            sender_id => <<"2">>,
            is_payable => true,
            payment_status => <<"unpaid">>,
            file_name => <<"README.md">>,
            file_type => <<"Document">>,
            file_content =>
                <<"IyBLaXZyYSBCYWNrZW5kIGNhbmRpZGF0ZSBjYXNlCgpUaGUgYmFja2VuZCBzaG91bGQgYmUgY29tcG9zZWQgb2YgMiBBUElzOgogICogU2VuZGVyIEFQSTogYWNjZXB0IGEgZmlsZSBpbiBiaW5hcnkgZm9ybWF0IHdpdGggbWV0YWRhdGE6CiAgICAqIHNlbmRlciBpZAogICAgKiBmaWxlIHR5cGUKICAgICogcmVjZWl2ZXIgaWQKICAgICogaXNfcGF5YWJsZQogICogUmVjZWl2ZXIgQVBJOiBxdWVyeSBieSByZWNlaXZlciBpZCwgcGF5CiAgCiMjIFJlcXVpcmVtZW50cwogICogYG1ha2VgCiAgKiBgZG9ja2VyYAoKIyMgSW5zdHJ1Y3Rpb25zCiAgdGFyZ2V0fGRlc2NyaXB0aW9uCiAgLXwtCiAgYG1ha2UgdXBgfFN0YXJ0IHRoZSBkZXYgZW52aXJvbm1lbnQKICBgbWFrZSBkb3duYHxTdG9wIHRoZSBkZXYgZW52aXJvbm1lbnQK">>
        }
    ].

pay_for_content(?PAYMENT_SUCCESSFUL_CONTENT_ID) -> {ok, success};
pay_for_content(?NOT_FOUND_CONTENT_ID) -> {error, not_found};
pay_for_content(?NON_PAYABLE_CONTENT_ID) -> {error, not_payable};
pay_for_content(?ALREADY_PAID_CONTENT_ID) -> {error, paid}.

init(Storage) ->
    {ok, Storage}.

handle_call(_Msg, _From, Storage) ->
    {reply, ok, Storage}.

handle_cast(_, State) ->
    {noreply, State}.
