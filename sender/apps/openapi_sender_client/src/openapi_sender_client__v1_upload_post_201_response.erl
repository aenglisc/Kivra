-module(openapi_sender_client__v1_upload_post_201_response).

-export([encode/1]).

-export_type([openapi_sender_client__v1_upload_post_201_response/0]).

-type openapi_sender_client__v1_upload_post_201_response() ::
    #{ 'id' => binary()
     }.

encode(#{ 'id' := Id
        }) ->
    #{ 'id' => Id
     }.
