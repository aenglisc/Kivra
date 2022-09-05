-module(sender_storage).

-export([init/0, upload/1]).

-callback init() -> ok.
-callback upload(map()) -> {ok, ID :: pos_integer() | already_uploaded} | {error, Reason :: term()}.

init() ->
    {ok, Conn} = epgsql:connect(#{
        host => application:get_env(sender, db_host, "db"),
        username => application:get_env(sender, db_username, "postgres"),
        password => application:get_env(sender, db_password, "postgres"),
        database => application:get_env(sender, db_database, "postgres"),
        timeout => application:get_env(sender, db_timeout, 4000)
    }),
    pgsql_migration:migrate(Conn, "priv/migrations"),
    epgsql:close(Conn),
    pgapp:connect([
        {size, 10},
        {host, application:get_env(sender, db_host, "db")},
        {database, application:get_env(sender, db_username, "postgres")},
        {username, application:get_env(sender, db_password, "postgres")},
        {password, application:get_env(sender, db_database, "postgres")}
    ]),
    ok.

upload(Body) ->
    FileContent = maps:get(<<"file_content">>, Body),
    MD5 = crypto:hash(md5, FileContent),
    SenderID = maps:get(<<"sender_id">>, Body),
    ReceiverID = maps:get(<<"receiver_id">>, Body),
    FileType = maps:get(<<"file_type">>, Body),
    IsPayable = maps:get(<<"is_payable">>, Body),
    Query = lists:concat([
        "insert into content ",
        "(sender_id, receiver_id, file_content, file_type, md5, is_payable) ",
        "values ($1, $2, $3, $4, $5, $6)"
    ]),
    QueryArgs = [SenderID, ReceiverID, FileContent, FileType, MD5, IsPayable],
    case pgapp:with_transaction(fun() -> pgapp:equery(Query, QueryArgs) end) of
        {ok, ID} ->
            {ok, ID};
        {error, {error, error, _, unique_violation, _, _}} ->
            {ok, already_uploaded};
        {error, Reason} ->
            {error, Reason}
    end.
