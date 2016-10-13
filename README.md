# huawei_push
> 华为Huawei推送push server sdk for Erlang    
> 集成版本：https://github.com/dcy/epush

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
```erlang
MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
huawei_push:single_send("08670650250202362000003019000001", MsgContent).
```

* single_send(AcceceToken, DeviceToken, Message)

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
```erlang
MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
DeviceTokenList = ["08670650250202362000003019000001"],
huawei_push:batch_send(DeviceTokenList, MsgContent).
```

* batch_send(AcceceToken, DeviceTokenList, Message)

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
```erlang
huawei_push:notification_send_tokens("08670650250202362000003019000001" ,"Title中文", "Content中文").
```

### notification_send_all 发送全部通知栏
* notification_send_all(Title, Content)
```erlang
huawei_push:notification_send_all("Title中文", "Content中文"),
```

### notification_send_tags 根据tags发送通知栏
* notification_send_tags(Tags, ExcludeTags, Title, Content) ->
```erlang
Tags = jiffy:encode(#{<<"tags">> => [#{<<"location">> => [<<"Guangzhou">>]}]}),
ExcludeTags = jiffy:encode(#{<<"exclude_tags">> => [#{<<"music">> => [<<"blue">>]}]}),
huawei_push:notification_send_tags(Tags, ExcludeTags, "Title中文", "Content中文"),
```






