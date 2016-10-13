-module(huawei_push).

%%API
-export([get_access_token_info/2,
         single_send/1, single_send/2, single_send/3,
         batch_send/1, batch_send/2, batch_send/3,
         notification_send/1,
         notification_send_tokens/3, notification_send_all/2, notification_send_tags/4
        ]).

-export([send/1]).


-include("huawei_push.hrl").
-include_lib("eutil/include/eutil.hrl").

%%return code
-define(SUCCESS, 0).
-define(ACCESS_TOKEN_EXPIRE, 6).

-define(PUSH_TYPE_TOKENS, 1).
-define(PUSH_TYPE_ALL, 2).
-define(PUSH_TYPE_TAGS, 3).

-define(URL, <<"https://api.vmall.com/rest.php">>).
-define(HEADERS, [{<<"Content-Type">>, <<"application/x-www-form-urlencoded; charset=utf-8">>}]).

get_access_token_info() ->
    {ok, AppId} = application:get_env(huawei_push, app_id),
    {ok, AppSecret} = application:get_env(huawei_push, app_secret),
    get_access_token_info(AppId, AppSecret).

get_access_token() ->
    TokenInfo = get_access_token_info(),
    maps:get(<<"access_token">>, TokenInfo).

get_access_token_info(AppId, AppSecret) ->
    Datas = [{grant_type, "client_credentials"}, {client_id, AppId},
             {client_secret, AppSecret}],
    Method = post,
    URL = <<"https://login.vmall.com/oauth2/token">>,
    Payload = eutil:urlencode(Datas),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, URL, ?HEADERS,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    jiffy:decode(ResultBin, [return_maps]).

%get_access_token(AppId, AppSecret) ->
%    TokenInfo = get_access_token_info(AppId, AppSecret),
%    maps:get(<<"access_token">>, TokenInfo).



single_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = maps:merge(?HW_SINGLE_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps),
    send(NewPayload).

single_send(DeviceToken, Message) ->
    AccessToken = get_access_token(),
    single_send(AccessToken, DeviceToken, Message).

single_send(AccessToken, DeviceToken, Message) ->
    Payload = ?HW_SINGLE_ARGS#{<<"access_token">> => AccessToken,
                               <<"deviceToken">> => list_to_binary(DeviceToken),
                               <<"message">> => Message},
    send(Payload).

batch_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = maps:merge(?HW_BATCH_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps),
    send(NewPayload).

batch_send(DeviceTokenList, Message) ->
    AccessToken = get_access_token(),
    batch_send(AccessToken, DeviceTokenList, Message).

batch_send(AccessToken, DeviceTokenList, Message) ->
    NewList = lists:flatten(io_lib:format("~p", [DeviceTokenList])),
    Payload = ?HW_BATCH_ARGS#{<<"access_token">> => AccessToken,
                              <<"deviceTokenList">> => NewList,
                              <<"message">> => Message},
    send(Payload).



notification_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = maps:merge(?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps),
    send(NewPayload).

notification_send_tokens(Tokens, Title, Content) ->
    AccessToken = get_access_token(),
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary(Title),
                                <<"notification_content">> => unicode:characters_to_binary(Content),
                                <<"doings">> => 1}),
    NewPayload = ?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken,
                                        <<"tokens">> => Tokens,
                                        <<"android">> => AndroidMsg},
    send(NewPayload).

notification_send_all(Title, Content) ->
    AccessToken = get_access_token(),
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary(Title),
                                <<"notification_content">> => unicode:characters_to_binary(Content),
                                <<"doings">> => 1}),
    NewPayload = ?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken,
                                        <<"android">> => AndroidMsg,
                                        <<"push_type">> => ?PUSH_TYPE_ALL},
    send(NewPayload).

notification_send_tags(Tags, ExcludeTags, Title, Content) ->
    AccessToken = get_access_token(),
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary(Title),
                                <<"notification_content">> => unicode:characters_to_binary(Content),
                                <<"doings">> => 1}),
    NewPayload = ?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken,
                                        <<"android">> => AndroidMsg,
                                        <<"tags">> => Tags,
                                        <<"exclude_tags">> => ExcludeTags,
                                        <<"push_type">> => ?PUSH_TYPE_TAGS},
    send(NewPayload).



    



send(PayloadMaps) ->
    Method = post,
    Payload = eutil:urlencode(PayloadMaps),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, ?URL, ?HEADERS,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    ResultOri = jiffy:decode(ResultBin, [return_maps]),
    Result = case erlang:is_map(ResultOri) of
                 true -> ResultOri;
                 false -> jiffy:decode(ResultOri, [return_maps])
             end,
    Code = case maps:get(<<"resultcode">>, Result, undefined) of
               undefined -> maps:get(<<"result_code">>, Result);
               Other -> Other
           end,
    case Code of
        ?SUCCESS ->
            {ok, Code};
        ?ACCESS_TOKEN_EXPIRE ->
            %self() ! refresh_access_token_now,
            {access_token_expire, Code};
        _ ->
            ?ERROR_MSG("huawei_push error, PayloadMaps: ~p, Result: ~p", [PayloadMaps, Result]),
            {error, Code}
    end.

