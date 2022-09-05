-module(sender_logic_handler).

-behaviour(openapi_sender_server_logic_handler).

-export([handle_request/3]).

-spec handle_request(
    OperationID :: openapi_sender_server_api:operation_id(),
    Req :: cowboy_req:req(),
    Context :: #{}
) ->
    {Status :: cowboy:http_status(), Headers :: cowboy:http_headers(), Body :: jsx:json_term()}.
handle_request('UploadPost', _Req, Context) ->
    Storage = storage(),
    Body = maps:get('V1UploadPost', Context),
    case Storage:upload(Body) of
        {ok, already_uploaded} ->
            {200, #{}, #{<<"message">> => <<"Already uploaded">>}};
        {ok, ContentID} ->
            {201, #{}, #{<<"message">> => <<"Success!">>, <<"id">> => integer_to_binary(ContentID)}}
    end;
handle_request(OperationID, Req, Context) ->
    error_logger:error_msg("Unknown request to process: ~p~n", [{OperationID, Req, Context}]),
    {501, #{}, #{}}.

storage() ->
    application:get_env(sender, storage, sender_storage).
