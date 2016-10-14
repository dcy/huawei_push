-module(huawei_push_example).
-compile(export_all).

single_send() ->
    MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
    Message = jiffy:encode(#{<<"message_type">> => <<"Common">>, <<"content">> => MsgContent}),
    Maps = #{<<"deviceToken">> => "08670650250202362000003019000001", <<"message">> => Message},
    huawei_push:single_send(Maps),
    ok.

single_send2() ->
    MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
    Message = jiffy:encode(#{<<"message_type">> => <<"Common">>, <<"content">> => MsgContent}),
    huawei_push:single_send("08670650250202362000003019000001", Message).

batch_send() ->
    MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
    Message = jiffy:encode(#{<<"message_type">> => <<"Common">>, <<"content">> => MsgContent}),
    DeviceTokenList = ["08670650250202362000003019000001"],
    NewList = lists:flatten(io_lib:format("~p", [DeviceTokenList])),
    Maps = #{<<"deviceTokenList">> => NewList, <<"message">> => Message},
    huawei_push:batch_send(Maps).

batch_send2() ->
    MsgContent = jiffy:encode(#{<<"body">> => <<"haha">>}),
    Message = jiffy:encode(#{<<"message_type">> => <<"Common">>, <<"content">> => MsgContent}),
    DeviceTokenList = ["08670650250202362000003019000001"],
    huawei_push:batch_send(DeviceTokenList, Message).


notification_send() ->
    AndroidMsg = jiffy:encode(#{<<"notification_title">> => unicode:characters_to_binary("标题"),
                                <<"notification_content">> => unicode:characters_to_binary("内容"),
                                <<"doings">> => 1}),
    Maps = #{<<"push_type">> => 2, <<"android">> => AndroidMsg},
    huawei_push:notification_send(Maps).


notification_send_all() ->
    huawei_push:notification_send_all("Title中文", "Content中文"),
    ok.

notification_send_tokens() ->
    huawei_push:notification_send_tokens("08670650250202362000003019000001", "Title中文", "Content中文"),
    ok.

notification_send_tags() ->
    Tags = jiffy:encode(#{<<"tags">> => [#{<<"location">> => [<<"Guangzhou">>]}]}),
    ExcludeTags = jiffy:encode(#{<<"exclude_tags">> => [#{<<"music">> => [<<"blue">>]}]}),
    huawei_push:notification_send_tags(Tags, ExcludeTags, "Title中文", "Content中文"),
    ok.

set_user_tag() ->
    huawei_push:set_user_tag("08670650250202362000003019000001", "location", "Guangzhou").

query_app_tags() ->
    huawei_push:query_app_tags().

delete_user_tag() ->
    huawei_push:delete_user_tag("08670650250202362000003019000001", "location").

query_user_tag() ->
    huawei_push:query_user_tag("08670650250202362000003019000001").
