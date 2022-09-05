-module(sender_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    upload_success/1,
    upload_already_done/1,
    upload_400/1
]).

all() ->
    [
        upload_success,
        upload_already_done,
        upload_400
    ].

init_per_suite(Config) ->
    application:ensure_all_started(hackney),
    application:set_env(sender, storage, sender_storage_mock),
    application:ensure_all_started(sender),
    Config.

end_per_suite(_Config) ->
    ok.

upload_success(_Config) ->
    Body = #{
        'file_content' =>
            <<"IyBLaXZyYSBCYWNrZW5kIGNhbmRpZGF0ZSBjYXNlCgpUaGUgYmFja2VuZCBzaG91bGQgYmUgY29tcG9zZWQgb2YgMiBBUElzOgogICogU2VuZGVyIEFQSTogYWNjZXB0IGEgZmlsZSBpbiBiaW5hcnkgZm9ybWF0IHdpdGggbWV0YWRhdGE6CiAgICAqIHNlbmRlciBpZAogICAgKiBmaWxlIHR5cGUKICAgICogcmVjZWl2ZXIgaWQKICAgICogaXNfcGF5YWJsZQogICogUmVjZWl2ZXIgQVBJOiBxdWVyeSBieSByZWNlaXZlciBpZCwgcGF5CiAgCiMjIFJlcXVpcmVtZW50cwogICogYG1ha2VgCiAgKiBgZG9ja2VyYAoKIyMgSW5zdHJ1Y3Rpb25zCiAgdGFyZ2V0fGRlc2NyaXB0aW9uCiAgLXwtCiAgYG1ha2UgdXBgfFN0YXJ0IHRoZSBkZXYgZW52aXJvbm1lbnQKICBgbWFrZSBkb3duYHxTdG9wIHRoZSBkZXYgZW52aXJvbm1lbnQK">>,
        'file_name' => <<"Success.md">>,
        'file_type' => <<"Document">>,
        'sender_id' => <<"some_id">>,
        'receiver_id' => <<"some_other_id">>,
        'is_payable' => false
    },
    Result = openapi_sender_client_default_api:upload_post(#{}, Body, #{
        cfg => #{host => "http://localhost:8080"}
    }),
    ?assertMatch({ok, _Body, #{status := 201}}, Result).

upload_already_done(_Config) ->
    Body = #{
        'file_content' =>
            <<"IyBLaXZyYSBCYWNrZW5kIGNhbmRpZGF0ZSBjYXNlCgpUaGUgYmFja2VuZCBzaG91bGQgYmUgY29tcG9zZWQgb2YgMiBBUElzOgogICogU2VuZGVyIEFQSTogYWNjZXB0IGEgZmlsZSBpbiBiaW5hcnkgZm9ybWF0IHdpdGggbWV0YWRhdGE6CiAgICAqIHNlbmRlciBpZAogICAgKiBmaWxlIHR5cGUKICAgICogcmVjZWl2ZXIgaWQKICAgICogaXNfcGF5YWJsZQogICogUmVjZWl2ZXIgQVBJOiBxdWVyeSBieSByZWNlaXZlciBpZCwgcGF5CiAgCiMjIFJlcXVpcmVtZW50cwogICogYG1ha2VgCiAgKiBgZG9ja2VyYAoKIyMgSW5zdHJ1Y3Rpb25zCiAgdGFyZ2V0fGRlc2NyaXB0aW9uCiAgLXwtCiAgYG1ha2UgdXBgfFN0YXJ0IHRoZSBkZXYgZW52aXJvbm1lbnQKICBgbWFrZSBkb3duYHxTdG9wIHRoZSBkZXYgZW52aXJvbm1lbnQK">>,
        'file_name' => <<"Already.md">>,
        'file_type' => <<"Document">>,
        'sender_id' => <<"some_id">>,
        'receiver_id' => <<"some_other_id">>,
        'is_payable' => false
    },
    Result = openapi_sender_client_default_api:upload_post(#{}, Body, #{
        cfg => #{host => "http://localhost:8080"}
    }),
    ?assertMatch({ok, _Body, #{status := 200}}, Result).

upload_400(_Config) ->
    InvalidBody = #{
        'file_name' => <<"Wrong.md">>,
        'file_type' => <<"Document">>,
        'sender_id' => <<"some_id">>,
        'receiver_id' => <<"some_other_id">>,
        'is_payable' => false
    },
    Result = openapi_sender_client_default_api:upload_post(#{}, InvalidBody, #{
        cfg => #{host => "http://localhost:8080"}
    }),
    ?assertMatch({error, _Body, #{status := 400}}, Result).
