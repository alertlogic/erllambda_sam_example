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
    AWSConfig = erllambda:config(),
    Result = handle_request(Event, Context, AWSConfig),
    erllambda:message( "Result: ~p", [Result] ),
    {ok, response(Result)}.

%%---------------------------------------------------------------------------
%% Private functions
%%---------------------------------------------------------------------------

response({ok, Items}) ->
    #{statusCode => 200,
      body => encode_items(Items),
      headers => #{<<"Content-Type">> => <<"application/json">>}};
response({error, not_found}) ->
    #{statusCode => 404, body => <<>>}.


%% List items
handle_request(#{<<"httpMethod">>            := <<"GET">>,
                 <<"pathParameters">>        := null,
                 <<"queryStringParameters">> := null},
               Context,
               AWSConfig) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    erlcloud_ddb_util:scan_all(TableName, [], AWSConfig);

handle_request(#{<<"httpMethod">>            := <<"GET">>,
                 <<"pathParameters">>        := null,
                 <<"queryStringParameters">> := FilterParameters},
               Context,
               AWSConfig) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    ScanFilter = scan_filter(FilterParameters),
    Opts = [{scan_filter, ScanFilter}],
    erlcloud_ddb_util:scan_all(TableName, Opts, AWSConfig);

%% Get Item by Id
handle_request(#{<<"httpMethod">> := <<"GET">>} = Event, Context, AWSConfig) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    #{<<"pathParameters">> := #{<<"id">> := Id}} = Event,
    Key = [{<<"id">>, Id}],
    case erlcloud_ddb2:get_item(TableName, Key, [], AWSConfig) of
        {ok, []} ->
            {error, not_found};
        {ok, _Item} = OK ->
            OK
    end;

%% Create a new Item
handle_request(#{<<"httpMethod">> := <<"POST">>} = Event, Context, AWSConfig) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    #{<<"body">> := Body} = Event,
    Item = jsx:decode(Body),
    erlcloud_ddb2:put_item(TableName, Item, [], AWSConfig);

%% Update an Item
handle_request(#{<<"httpMethod">> := <<"PUT">>} = Event, Context, AWSConfig) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    #{<<"pathParameters">> := #{<<"id">> := Id}} = Event,
    Key = [{<<"id">>, Id}],
    #{<<"body">> := Body} = Event,
    Item = jsx:decode(Body),
    erlcloud_ddb2:update_item(TableName, Key, Item, [], AWSConfig);

%% Delete an Item
handle_request(#{<<"httpMethod">> := <<"DELETE">>} = Event, Context, AWSConfig) ->
    #{<<"TABLE_NAME">> := TableName} = Context,
    #{<<"pathParameters">> := #{<<"id">> := Id}} = Event,
    Key = [{<<"id">>, Id}],
    erlcloud_ddb2:delete_item(TableName, Key, [], AWSConfig).


scan_filter(null) ->
    [];
scan_filter(Parameters) ->
    maps:to_list(Parameters).

encode_items(Items) ->
    jsx:encode(Items).
