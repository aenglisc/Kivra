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
handle_request(OperationID, Req, Context) ->
    error_logger:error_msg("Unknown request to process: ~p~n", [{OperationID, Req, Context}]),
    {501, #{}, #{}}.

storage() ->
    application:get_env(consumer, storage, consumer_storage).
