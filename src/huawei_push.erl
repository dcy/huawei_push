-module(huawei_push).

%%API
-export([get_access_token_info/2,
         single_send/1, single_send/2, single_send/3, single_send/4,
         batch_send/1, batch_send/2, batch_send/3, batch_send/4,
         notification_send/1,
         notification_send_tokens/3, notification_send_tokens/4, notification_send_tokens/5,
         notification_send_all/2, notification_send_all/3, notification_send_all/4,
         notification_send_tags/4, notification_send_tags/5, notification_send_tags/6,
         set_user_tag/3, set_user_tag/4, set_user_tag/5,
         query_app_tags/0, query_app_tags/1, query_app_tags/2,
         delete_user_tag/2, delete_user_tag/3, delete_user_tag/4,
         query_user_tag/1, query_user_tag/2, query_user_tag/3
        ]).

-export([send/1]).


-include("huawei_push.hrl").
-include_lib("eutil/include/eutil.hrl").

%%return code
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

get_access_token(AppId, AppSecret) ->
    TokenInfo = get_access_token_info(AppId, AppSecret),
    maps:get(<<"access_token">>, TokenInfo).



single_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = case maps:get(<<"access_token">>, PayloadMaps, undefined) of
                     undefined ->
                         maps:merge(?HW_SINGLE_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps);
                     AccessToken ->
                         PayloadMaps(?HW_NOTIFICATION_ARGS, PayloadMaps)
                 end,
    send(NewPayload).

single_send(DeviceToken, Message) ->
    AccessToken = get_access_token(),
    single_send(AccessToken, DeviceToken, Message).

single_send(AccessToken, DeviceToken, Message) ->
    Payload = ?HW_SINGLE_ARGS#{<<"access_token">> => AccessToken,
                               <<"deviceToken">> => list_to_binary(DeviceToken),
                               <<"message">> => Message},
    send(Payload).

single_send(AppId, AppSecret, DeviceToken, Message) ->
    AccessToken = get_access_token(AppId, AppSecret),
    single_send(AccessToken, DeviceToken, Message).

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

batch_send(AppId, AppSecret, DeviceTokenList, Message) ->
    AccessToken = get_access_token(AppId, AppSecret),
    batch_send(AccessToken, DeviceTokenList, Message).



notification_send(PayloadMaps) ->
    AccessToken = get_access_token(),
    NewPayload = maps:merge(?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken}, PayloadMaps),
    send(NewPayload).

notification_send_tokens(Tokens, Title, Content) ->
    AccessToken = get_access_token(),
    notification_send_tokens(AccessToken, Tokens, Title, Content).

notification_send_tokens(AccessToken, Tokens, Title, Content) ->
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary(Title),
                                <<"notification_content">> => unicode:characters_to_binary(Content),
                                <<"doings">> => 1}),
    NewPayload = ?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken,
                                        <<"tokens">> => Tokens,
                                        <<"android">> => AndroidMsg},
    send(NewPayload).

notification_send_tokens(AppId, AppSecret, Tokens, Title, Content) ->
    AccessToken = get_access_token(AppId, AppSecret),
    notification_send_tokens(AccessToken, Tokens, Title, Content).

notification_send_all(Title, Content) ->
    AccessToken = get_access_token(),
    notification_send_all(AccessToken, Title, Content). 

notification_send_all(AccessToken, Title, Content) ->
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary(Title),
                                <<"notification_content">> => unicode:characters_to_binary(Content),
                                <<"doings">> => 1}),
    NewPayload = ?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken,
                                        <<"android">> => AndroidMsg,
                                        <<"push_type">> => ?PUSH_TYPE_ALL},
    send(NewPayload).

notification_send_all(AppId, AppSecret, Title, Content) ->
    AccessToken = get_access_token(AppId, AppSecret),
    notification_send_all(AccessToken, Title, Content).

notification_send_tags(Tags, ExcludeTags, Title, Content) ->
    AccessToken = get_access_token(),
    notification_send_tokens(AccessToken, Tags, ExcludeTags, Title, Content).

notification_send_tags(AccessToken, Tags, ExcludeTags, Title, Content) ->
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary(Title),
                                <<"notification_content">> => unicode:characters_to_binary(Content),
                                <<"doings">> => 1}),
    NewPayload = ?HW_NOTIFICATION_ARGS#{<<"access_token">> => AccessToken,
                                        <<"android">> => AndroidMsg,
                                        <<"tags">> => Tags,
                                        <<"exclude_tags">> => ExcludeTags,
                                        <<"push_type">> => ?PUSH_TYPE_TAGS},
    send(NewPayload).

notification_send_tags(AppId, AppSecret, Tags, ExcludeTags, Title, Content) ->
    AccessToken = get_access_token(AppId, AppSecret),
    notification_send_tags(AccessToken, Tags, ExcludeTags, Title, Content).
    


set_user_tag(Token, TagKey, TagValue) ->
    AccessToken = get_access_token(),
    set_user_tag(AccessToken, Token, TagKey, TagValue).

set_user_tag(AccessToken, Token, TagKey, TagValue) ->
    Payload = ?HW_SET_USER_TAG_ARGS#{<<"access_token">> => AccessToken,
                                     <<"token">> => list_to_binary(Token),
                                     <<"tag_key">> => list_to_binary(TagKey),
                                     <<"tag_value">> => list_to_binary(TagValue)},
    ResultOri = do_send(Payload),
    Result = jiffy:decode(ResultOri, [return_maps]),
    case maps:get(<<"result_code">>, Result) of
        <<"0">> -> %%一下是0,一下是"0"
            ok;
        _ ->
            ?ERROR_MSG("huawei_push set_user_tag's error, Payload: ~p, Result: ~p", [Payload, Result]),
            error
    end.

set_user_tag(AppId, AppSecret, Token, TagKey, TagValue) ->
    AccessToken = get_access_token(AppId, AppSecret),
    set_user_tag(AccessToken, Token, TagKey, TagValue).


query_app_tags() ->
    AccessToken = get_access_token(),
    query_app_tags(AccessToken).

query_app_tags(AccessToken) ->
    Payload = ?HW_QUERY_APP_TAGS_ARGS#{<<"access_token">> => AccessToken},
    ResultOri = do_send(Payload),
    Result = jiffy:decode(ResultOri, [return_maps]),
    TagsStr = maps:get(<<"tags">>, Result),
    jiffy:decode(TagsStr, [return_maps]).
    

query_app_tags(AppId, AppSecret) ->
    AccessToken = get_access_token(AppId, AppSecret),
    query_app_tags(AccessToken).


delete_user_tag(Token, TagKey) ->
    AccessToken = get_access_token(),
    delete_user_tag(AccessToken, Token, TagKey).

delete_user_tag(AccessToken, Token, TagKey) ->
    Payload = ?HW_DELETE_USER_TAG_ARGS#{<<"access_token">> => AccessToken,
                                        <<"token">> => list_to_binary(Token),
                                        <<"tag_key">> => list_to_binary(TagKey)},
    ResultOri = do_send(Payload),
    Result = jiffy:decode(ResultOri, [return_maps]),
    case maps:get(<<"result_code">>, Result) of
        <<"0">> ->
            ok; %%一下是0,一下是"0"
        _ ->
            ?ERROR_MSG("huawei_push delete_user_tag's error, Payload: ~p, Result: ~p", [Payload, Result]),
            error
    end.


delete_user_tag(AppId, AppSecret, Token, TagKey) ->
    AccessToken = get_access_token(AppId, AppSecret),
    delete_user_tag(AccessToken, Token, TagKey).

query_user_tag(Token) ->
    AccessToken = get_access_token(),
    query_user_tag(AccessToken, Token).

query_user_tag(AccessToken, Token) ->
    Payload = ?HW_QUERY_USER_TAG_ARGS#{<<"access_token">> => AccessToken,
                                       <<"token">> => list_to_binary(Token)},
    ResultOri = do_send(Payload),
    Result = jiffy:decode(ResultOri, [return_maps]),
    TagsStr = maps:get(<<"tags">>, Result),
    jiffy:decode(TagsStr, [return_maps]).

query_user_tag(AppId, AppSecret, Token) ->
    AccessToken = get_access_token(AppId, AppSecret),
    query_user_tag(AccessToken, Token).




    
do_send(PayloadMaps) ->
    Method = post,
    Payload = eutil:urlencode(PayloadMaps),
    Options = [{pool, default}],
    {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(Method, ?URL, ?HEADERS,
                                                                 Payload, Options),
    {ok, ResultBin} = hackney:body(ClientRef),
    jiffy:decode(ResultBin, [return_maps]).

send(PayloadMaps) ->
    ResultOri = do_send(PayloadMaps),
    Result = case erlang:is_map(ResultOri) of
                 true -> ResultOri;
                 false -> jiffy:decode(ResultOri, [return_maps])
             end,
    Code = case maps:get(<<"resultcode">>, Result, undefined) of
               undefined -> maps:get(<<"result_code">>, Result);
               Other -> Other
           end,
    case Code of
        ?SUCCESS_0 ->
            {ok, Code};
        ?ACCESS_TOKEN_EXPIRE ->
            {access_token_expire, Code};
        _ ->
            ?ERROR_MSG("huawei_push error, PayloadMaps: ~p, Result: ~p", [PayloadMaps, Result]),
            {error, Code}
    end.

