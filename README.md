# huawei_push
> 华为Huawei推送push server sdk for Erlang    
> 集成版本：https://github.com/dcy/epush    
> 使用例子：[/src/huawei_push_example.erl](/src/huawei_push_example.erl)

## AcceceToken
* get_access_token_info(AppId, AppSecret)
```erlang
huawei_push:get_access_token_info(123456, "HuaweiAppSecret").
```

## single_send 透传

* single_send(PayloadMaps)
```erlang
MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
Maps = #{<<"deviceToken">> => "08670650250202362000003019000001", <<"message">> => MsgContent},
huawei_push:single_send(Maps),
```

* single_send(DeviceToken, Message)
* single_send(AccessToken, DeviceToken, Message)
* single_send(AcceceToken, DeviceToken, Message)
```erlang
MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
huawei_push:single_send("08670650250202362000003019000001", MsgContent).
```


## batch_send 批量透传

* batch_send(PayloadMaps) 
```erlang
MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
DeviceTokenList = ["08670650250202362000003019000001"],
NewList = lists:flatten(io_lib:format("~p", [DeviceTokenList])),
Maps = #{<<"deviceTokenList">> => NewList, <<"message">> => MsgContent},
huawei_push:batch_send(Maps).
```

* batch_send(DeviceTokenList, Message)
* batch_send(AccessToken, DeviceTokenList, Message)
* batch_send(AppId, AppSecret, DeviceTokenList, Message) ->
```erlang
MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
DeviceTokenList = ["08670650250202362000003019000001"],
huawei_push:batch_send(DeviceTokenList, MsgContent).
```

## notification_send 通知栏

* notification_send(PayloadMaps)
```erlang
AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary("标题"),
                            <<"notification_content">> => unicode:characters_to_binary("内容"),
                            <<"doings">> => 1}),
Maps = #{<<"push_type">> => 2, <<"android">> => AndroidMsg},
huawei_push:notification_send(Maps).
```
### notification_send_tokens 根据token发送通知栏

* notification_send_tokens(Tokens, Title, Content) 
* notification_send_tokens(AccessToken, Tokens, Title, Content)
* notification_send_tokens(AppId, AppSecret, Tokens, Title, Content)
```erlang
huawei_push:notification_send_tokens("08670650250202362000003019000001" ,"Title中文", "Content中文").
```

### notification_send_all 发送全部通知栏
* notification_send_all(Title, Content)
* notification_send_all(AccessToken, Title, Content)
* notification_send_all(AppId, AppSecret, Title, Content)
```erlang
huawei_push:notification_send_all("Title中文", "Content中文"),
```

### notification_send_tags 根据tags发送通知栏
* notification_send_tags(Tags, ExcludeTags, Title, Content)
* notification_send_tags(AccessToken, Tags, ExcludeTags, Title, Content)
* notification_send_tags(AppId, AppSecret, Tags, ExcludeTags, Title, Content)
```erlang
Tags = jiffy:encode(#{<<"tags">> => [#{<<"location">> => [<<"Guangzhou">>]}]}),
ExcludeTags = jiffy:encode(#{<<"exclude_tags">> => [#{<<"music">> => [<<"blue">>]}]}),
huawei_push:notification_send_tags(Tags, ExcludeTags, "Title中文", "Content中文"),
```

## set_user_tag
* set_user_tag(Token, TagKey, TagValue)
* set_user_tag(AccessToken, Token, TagKey, TagValue)
* set_user_tag(AppId, AppSecret, Token, TagKey, TagValue)
```erlang
huawei_push:set_user_tag("08670650250202362000003019000001", "location", "Guangzhou").
```

## query_app_tags
* query_app_tags() ->
* query_app_tags(AccessToken) ->
* query_app_tags(AppId, AppSecret) ->
```erlang
huawei_push:query_app_tags().
```

## delete_user_tag
* delete_user_tag(Token, TagKey) ->
* delete_user_tag(AccessToken, Token, TagKey) ->
* delete_user_tag(AppId, AppSecret, Token, TagKey) ->
```erlang
huawei_push:delete_user_tag("08670650250202362000003019000001", "location").
```
## query_user_tag
* query_user_tag(Token) ->
* query_user_tag(AccessToken, Token) ->
* query_user_tag(AppId, AppSecret, Token) ->
```erlang
huawei_push:query_user_tag("08670650250202362000003019000001").
```


## Todo:
- [ ] lbs_send
