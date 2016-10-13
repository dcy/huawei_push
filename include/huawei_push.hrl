-ifndef(__HUAWEI_PUSH_HRL__).
-define(__HUAWEI_PUSH_HRL__, 0).

-define(HW_SINGLE_ARGS, #{<<"deviceToken">> => "", <<"message">> => <<"message">>, <<"priority">> => 1,
                          <<"nsp_svc">> => <<"openpush.message.single_send">>, 
                          <<"nsp_ts">> => erlang:system_time(seconds),
                          <<"cacheMode">> => 0, <<"msgType">> => rand:uniform(100)}).

-define(HW_NOTIFICATION_ARGS, #{<<"push_type">> => 1,
                                <<"nsp_ts">> => erlang:system_time(seconds),
                                <<"nsp_svc">> => <<"openpush.openapi.notification_send">>}).

-define(HW_BATCH_ARGS, #{<<"nsp_svc">> => <<"openpush.message.batch_send">>,
                         <<"nsp_ts">> => erlang:system_time(seconds),
                         <<"cacheMode">> => 0, <<"msgType">> => rand:uniform(100)}).



-endif.
