-module(sender_storage).

-export([start/0, upload/1]).

-type id() :: pos_integer().

-callback start() -> ok.
-callback upload(map()) -> {ok, id() | {already_uploaded, id()}} | {error, Reason :: term()}.

start() ->
    {ok, Conn} = epgsql:connect(#{
        host => application:get_env(sender, db_host, "db"),
        username => application:get_env(sender, db_username, "postgres"),
        password => application:get_env(sender, db_password, "postgres"),
        database => application:get_env(sender, db_database, "postgres"),
        timeout => application:get_env(sender, db_timeout, 4000)
    }),
    pgsql_migration:migrate(Conn, code:priv_dir(sender) ++ "/migrations"),
    epgsql:close(Conn),
    pgapp:connect([
        {size, 10},
        {host, application:get_env(sender, db_host, "db")},
        {database, application:get_env(sender, db_username, "postgres")},
        {username, application:get_env(sender, db_password, "postgres")},
        {password, application:get_env(sender, db_database, "postgres")}
    ]),
    ok.

upload(#{<<"file_content">> := FileContent} = Body) ->
    MD5 = crypto:hash(md5, FileContent),
    SenderID = maps:get(<<"sender_id">>, Body),
    ReceiverID = maps:get(<<"receiver_id">>, Body),
    FileType = maps:get(<<"file_type">>, Body),
    FileName = maps:get(<<"file_name">>, Body),
    IsPayable = maps:get(<<"is_payable">>, Body),
    UniquenessArgs = [SenderID, ReceiverID, MD5],
    UploadArgs = [SenderID, ReceiverID, FileContent, FileName, FileType, MD5, IsPayable],
    Pipeline = [
        (fun(_) -> check_uniqueness(UniquenessArgs) end),
        (fun(_) -> do_upload(UploadArgs) end)
    ],
    case f:do(init, Pipeline) of
        {ok, ID} -> {ok, ID};
        {error, {already_uploaded, ID}} -> {ok, {already_uploaded, ID}};
        {error, Reason} -> {error, Reason}
    end.

check_uniqueness(QueryArgs) ->
    Query = lists:concat([
        "select id from content ",
        "where sender_id = $1 and receiver_id = $2 and md5 = $3 "
    ]),
    case pgapp:with_transaction(fun() -> pgapp:equery(Query, QueryArgs) end) of
        {ok, _Column, []} ->
            {ok, unique};
        {ok, _Column, [{ID}]} ->
            {error, {already_uploaded, ID}}
    end.

do_upload(QueryArgs) ->
    Query = lists:concat([
        "insert into content ",
        "(sender_id, receiver_id, file_content, file_name, file_type, md5, is_payable) ",
        "values ($1, $2, $3, $4, $5, $6, $7) ",
        "returning id "
    ]),
    case pgapp:with_transaction(fun() -> pgapp:equery(Query, QueryArgs) end) of
        {ok, 1, _Meta, [{ID}]} ->
            {ok, ID};
        {error, Reason} ->
            {error, Reason}
    end.
