-module(erllambda_sam_example).
-behavior(erllambda).

-export([handle/2]).

%%---------------------------------------------------------------------------
-spec handle( Event :: map(), Context :: map() ) ->
    ok | {ok, iolist() | map()} | {error, iolist()}.
%%%---------------------------------------------------------------------------
%% @doc Handle lambda invocation
%%
handle( Event, Context ) ->
    erllambda:message( "event: ~p", [Event] ),
    Result = handle_request(Event, Context),
    erllambda:message( "Result: ~p", [Result] ),
    {ok, response(Result)}.

%%---------------------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------------------

response({ok, Items}) ->
    #{statusCode => <<"200">>,
      body => encode_items(Items),
      headers => #{<<"Content-Type">> => <<"application/json">>}};
response({error, not_found}) ->
    #{statusCode => <<"404">>, body => <<>>}.


%% List items
handle_request(#{<<"httpMethod">>            := <<"GET">>,
                 <<"pathParameters">>        := null,
                 <<"queryStringParameters">> := null},
               Context) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    erlcloud_ddb_util:scan_all(TableName);

handle_request(#{<<"httpMethod">>            := <<"GET">>,
                 <<"pathParameters">>        := null,
                 <<"queryStringParameters">> := FilterParameters},
               Context) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    ScanFilter = scan_filter(FilterParameters),
    Opts = [{scan_filter, ScanFilter}],
    erlcloud_ddb_util:scan_all(TableName, Opts);

%% Get Item by Id
handle_request(#{<<"httpMethod">> := <<"GET">>} = Event, Context) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    #{<<"pathParameters">> := #{<<"id">> := Id}} = Event,
    Key = [{<<"id">>, Id}],
    case erlcloud_ddb2:get_item(TableName, Key) of
        {ok, []} ->
            {error, not_found};
        {ok, _Item} = OK ->
            OK
    end;

%% Create a new Item
handle_request(#{<<"httpMethod">> := <<"POST">>} = Event, Context) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    #{<<"body">> := Body} = Event,
    Item = jsx:decode(Body),
    erlcloud_ddb2:put_item(TableName, Item);

%% Update an Item
handle_request(#{<<"httpMethod">> := <<"PUT">>} = Event, Context) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    #{<<"pathParameters">> := #{<<"id">> := Id}} = Event,
    Key = [{<<"id">>, Id}],
    #{<<"body">> := Body} = Event,
    Item = jsx:decode(Body),
    erlcloud_ddb2:update_item(TableName, Key, Item);

%% Delete an Item
handle_request(#{<<"httpMethod">> := <<"DELETE">>} = Event, Context) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    #{<<"pathParameters">> := #{<<"id">> := Id}} = Event,
    Key = [{<<"id">>, Id}],
    erlcloud_ddb2:delete_item(TableName, Key).


scan_filter(null) ->
    [];
scan_filter(Parameters) ->
    maps:to_list(Parameters).

encode_items(Items) ->
    jsx:encode(Items).
