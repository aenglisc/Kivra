-module(consumer_storage).

-export([start/0, list_content/1, update_payment_status/1]).

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

start() ->
    {ok, Conn} = epgsql:connect(#{
        host => application:get_env(consumer, db_host, "db"),
        username => application:get_env(consumer, db_username, "postgres"),
        password => application:get_env(consumer, db_password, "postgres"),
        database => application:get_env(consumer, db_database, "postgres"),
        timeout => application:get_env(consumer, db_timeout, 4000)
    }),
    pgsql_migration:migrate(Conn, "priv/migrations"),
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
    Query = lists:concat([
        "select id, file_content, file_name, file_type, sender_id, receiver_id, payment_status, is_payable from content ",
        "where receiver_id = $1 "
    ]),
    case pgapp:with_transaction(fun() -> pgapp:equery(Query, [ReceiverID]) end) of
        {ok, _Column, Results} ->
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
            Zip = f:curry(fun lists:zip/2),
            ZipWithKeys = Zip(Keys),
            MapperPipeline = [
                fun tuple_to_list/1,
                ZipWithKeys,
                fun maps:from_list/1
            ],
            ResultMapper = (f:flip(fun f:pipe/2))(MapperPipeline),
            lists:map(ResultMapper, Results)
    end.

update_payment_status(_ContentID) ->
    erlang:display(implementing),
    ok.
