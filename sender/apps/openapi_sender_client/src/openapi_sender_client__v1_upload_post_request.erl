-module(openapi_sender_client__v1_upload_post_request).

-export([encode/1]).

-export_type([openapi_sender_client__v1_upload_post_request/0]).

-type openapi_sender_client__v1_upload_post_request() ::
    #{ 'file' => binary(),
       'sender_id' => binary(),
       'receiver_id' => binary(),
       'is_payable' => boolean()
     }.

encode(#{ 'file' := File,
          'sender_id' := SenderId,
          'receiver_id' := ReceiverId,
          'is_payable' := IsPayable
        }) ->
    #{ 'file' => File,
       'sender_id' => SenderId,
       'receiver_id' => ReceiverId,
       'is_payable' => IsPayable
     }.
