-module(consumer_storage).

-export([start/0, list_content/1, pay_for_content/1]).

-type receiver_id() :: binary().
-type content_id() :: binary().
-type content() :: #{
    content_id := pos_integer(),
    receiver_id := binary(),
    sender_id := binary(),
    is_payable := boolean(),
    payment_status := binary(),
    file_name := binary(),
    file_type := binary(),
    file_content := binary()
}.

-callback start() -> ok.
-callback list_content(receiver_id()) -> [content()].
-callback update_payment_status(content_id()) -> ok.
-callback pay_for_content(content_id()) -> {ok, success} | {error, not_found | not_payable | paid}.

start() ->
    {ok, Conn} = epgsql:connect(#{
        host => application:get_env(consumer, db_host, "db"),
        username => application:get_env(consumer, db_username, "postgres"),
        password => application:get_env(consumer, db_password, "postgres"),
        database => application:get_env(consumer, db_database, "postgres"),
        timeout => application:get_env(consumer, db_timeout, 4000)
    }),
    pgsql_migration:migrate(Conn, code:priv_dir(consumer) ++ "/migrations"),
    epgsql:close(Conn),
    pgapp:connect([
        {size, 10},
        {host, application:get_env(consumer, db_host, "db")},
        {database, application:get_env(consumer, db_username, "postgres")},
        {username, application:get_env(consumer, db_password, "postgres")},
        {password, application:get_env(consumer, db_database, "postgres")}
    ]),
    ok.

list_content(ReceiverID) ->
    Keys = [
        content_id,
        file_content,
        file_name,
        file_type,
        sender_id,
        receiver_id,
        payment_status,
        is_payable
    ],
    Query = lists:concat([
        "select id, file_content, file_name, file_type, sender_id, receiver_id, payment_status, is_payable from content ",
        "where receiver_id = $1 "
    ]),
    case pgapp:with_transaction(fun() -> pgapp:equery(Query, [ReceiverID]) end) of
        {ok, _Columns, Results} ->
            lists:map(result_mapper(Keys), Results)
    end.

pay_for_content(ContentID) ->
    f:do(ContentID, [
        fun get_payment_data/1,
        fun validate_payability/1,
        fun validate_status/1,
        fun update_payment_status/1
    ]).

get_payment_data(ContentID) ->
    Keys = [
        content_id,
        payment_status,
        is_payable
    ],
    Query = lists:concat([
        "select id, payment_status, is_payable from content ",
        "where id = $1 "
    ]),
    case pgapp:with_transaction(fun() -> pgapp:equery(Query, [ContentID]) end) of
        {ok, _Columns, []} -> f:err(not_found);
        {ok, _Columns, [Result]} -> f:ok((result_mapper(Keys))(Result))
    end.

validate_payability(#{is_payable := true} = PaymentData) -> f:ok(PaymentData);
validate_payability(#{is_payable := false}) -> f:err(not_payable).

validate_status(#{payment_status := <<"unpaid">>} = PaymentData) -> f:ok(PaymentData);
validate_status(#{payment_status := <<"paid">>}) -> f:err(paid).

update_payment_status(#{content_id := ContentID}) ->
    Query = lists:concat([
        "update content ",
        "set payment_status = 'paid' ",
        "where id = $1 "
    ]),
    case pgapp:with_transaction(fun() -> pgapp:equery(Query, [ContentID]) end) of
        {ok, 1} -> {ok, success}
    end.

result_mapper(Keys) ->
    Zip = f:curry(fun lists:zip/2),
    ZipWithKeys = Zip(Keys),
    (f:flip(fun f:pipe/2))([
        fun tuple_to_list/1,
        ZipWithKeys,
        fun maps:from_list/1
    ]).
