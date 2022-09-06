-module(consumer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("helpers.hrl").

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1
]).

-export([
    list_success/1,
    list_empty/1,
    pay_not_found/1,
    pay_non_payable/1,
    pay_success/1,
    pay_already_paid/1
]).

all() ->
    [
        list_success,
        list_empty,
        pay_not_found,
        pay_non_payable,
        pay_success,
        pay_already_paid
    ].

init_per_suite(Config) ->
    application:ensure_all_started(hackney),
    application:set_env(consumer, storage, consumer_storage_mock),
    application:ensure_all_started(consumer),
    Config.

end_per_suite(_Config) ->
    consumer_storage_mock:stop(),
    ok.

list_success(_Config) ->
    Result = openapi_consumer_client_default_api:receiver_id_content_get(
        #{}, ?RECEIVER_ID_WITH_CONTENT, #{
            cfg => #{host => "http://localhost:8081"}
        }
    ),
    EmptyList = jsx:encode([]),
    ?assertMatch({ok, _Body, #{status := 200}}, Result),
    ?assertNotMatch({ok, EmptyList, #{status := 200}}, Result).

list_empty(_Config) ->
    Result = openapi_consumer_client_default_api:receiver_id_content_get(
        #{}, ?RECEIVER_ID_WITHOUT_CONTENT, #{
            cfg => #{host => "http://localhost:8081"}
        }
    ),
    EmptyList = jsx:encode([]),
    ?assertMatch({ok, EmptyList, #{status := 200}}, Result).

pay_not_found(_Config) ->
    Result = openapi_consumer_client_default_api:content_pay_patch(
        #{}, #{content_id => ?NOT_FOUND_CONTENT_ID}, #{
            cfg => #{host => "http://localhost:8081"}
        }
    ),
    ?assertMatch({error, _Body, #{status := 404}}, Result).

pay_non_payable(_Config) ->
    Result = openapi_consumer_client_default_api:content_pay_patch(
        #{}, #{content_id => ?NON_PAYABLE_CONTENT_ID}, #{
            cfg => #{host => "http://localhost:8081"}
        }
    ),
    ?assertMatch({error, _Body, #{status := 422}}, Result).

pay_success(_Config) ->
    Result = openapi_consumer_client_default_api:content_pay_patch(
        #{}, #{content_id => ?PAYMENT_SUCCESSFUL_CONTENT_ID}, #{
            cfg => #{host => "http://localhost:8081"}
        }
    ),
    ?assertMatch({ok, _Body, #{status := 201}}, Result).

pay_already_paid(_Config) ->
    Result = openapi_consumer_client_default_api:content_pay_patch(
        #{}, #{content_id => ?ALREADY_PAID_CONTENT_ID}, #{
            cfg => #{host => "http://localhost:8081"}
        }
    ),
    ?assertMatch({ok, _Body, #{status := 200}}, Result).
