-module(openapi_sender_client_default_api).

-export([v1_upload_post/2, v1_upload_post/3]).

-define(BASE_URL, <<"">>).

%% @doc Upload a binary with metadata 
%% 
-spec v1_upload_post(ctx:ctx(), openapi_sender_client_v1_upload_post:openapi_sender_client_v1_upload_post()) -> {ok, openapi_sender_client__v1_upload_post_201_response:openapi_sender_client__v1_upload_post_201_response(), openapi_sender_client_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), openapi_sender_client_utils:response_info()}.
v1_upload_post(Ctx, OpenapiSenderClientV1UploadPost) ->
    v1_upload_post(Ctx, OpenapiSenderClientV1UploadPost, #{}).

-spec v1_upload_post(ctx:ctx(), openapi_sender_client_v1_upload_post:openapi_sender_client_v1_upload_post(), maps:map()) -> {ok, openapi_sender_client__v1_upload_post_201_response:openapi_sender_client__v1_upload_post_201_response(), openapi_sender_client_utils:response_info()} | {ok, hackney:client_ref()} | {error, term(), openapi_sender_client_utils:response_info()}.
v1_upload_post(Ctx, OpenapiSenderClientV1UploadPost, Optional) ->
    _OptionalParams = maps:get(params, Optional, #{}),
    Cfg = maps:get(cfg, Optional, application:get_env(kuberl, config, #{})),

    Method = post,
    Path = [<<"/v1/upload">>],
    QS = [],
    Headers = [],
    Body1 = OpenapiSenderClientV1UploadPost,
    ContentTypeHeader = openapi_sender_client_utils:select_header_content_type([<<"application/json">>]),
    Opts = maps:get(hackney_opts, Optional, []),

    openapi_sender_client_utils:request(Ctx, Method, [?BASE_URL, Path], QS, ContentTypeHeader++Headers, Body1, Opts, Cfg).


