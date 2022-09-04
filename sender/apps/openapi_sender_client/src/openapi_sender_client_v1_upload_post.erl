-module(openapi_sender_client_v1_upload_post).

-export([encode/1]).

-export_type([openapi_sender_client_v1_upload_post/0]).

-type openapi_sender_client_v1_upload_post() ::
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
