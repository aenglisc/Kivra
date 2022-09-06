-module(consumer_logic_handler).

-behaviour(openapi_consumer_server_logic_handler).

-export([handle_request/3]).

-spec handle_request(
    OperationID :: openapi_consumer_server_api:operation_id(),
    Req :: cowboy_req:req(),
    Context :: #{}
) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.
handle_request('ReceiverIdContentGet', _Req, #{receiver_id := ReceiverID}) ->
    Storage = storage(),
    {200, #{}, Storage:list_content(ReceiverID)};
handle_request('ContentPayPatch', _Req, Context) ->
    Body = maps:get('Payment', Context),
    ContentID = maps:get(<<"content_id">>, Body),
    Storage = storage(),
    case Storage:pay_for_content(ContentID) of
        {ok, success} -> {201, #{}, #{message => <<"Success">>}};
        {error, paid} -> {200, #{}, #{message => <<"Already paid">>}};
        {error, not_payable} -> {422, #{}, #{message => <<"Not payable">>}};
        {error, not_found} -> {404, #{}, #{message => <<"Not found">>}}
    end;
handle_request(OperationID, Req, Context) ->
    error_logger:error_msg("Unknown request to process: ~p~n", [{OperationID, Req, Context}]),
    {501, #{}, #{}}.

storage() ->
    application:get_env(consumer, storage, consumer_storage).
