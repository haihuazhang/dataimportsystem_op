class /ZHD/CL_DMP_SEND_DATA definition
  public
  final
  create public .

public section.

  interfaces IF_HTTP_EXTENSION .

  data IT_DATA type /ZHD/TT_DMP_DATA_LIST .
  data TOKEN type STRING .
  data RETURN_CODE type I value 0 ##NO_TEXT.
  data RETURN_MESSAGE type STRING .
  data UUID type STRING .

  methods SEND_ORIGN_DATA_TO_DMP
    importing
      !IT_DATA type /ZHD/TT_DMP_DATA_LIST .
protected section.
private section.

  methods GET_TOKEN
    importing
      !IV_URL type STRING
      !IV_USERNAME type STRING
      !IV_PASSWORD type STRING
      !IV_METHOD type STRING
    changing
      !CT_COOKIES type TIHTTPCKI
    returning
      value(EV_TOKEN) type STRING .
  methods CREATE_IMPORT_RECORD
    importing
      !IV_URL type STRING
      !IV_USERNAME type STRING
      !IV_PASSWORD type STRING
      !IT_COOKIES type TIHTTPCKI .
  methods GET_DATA_MESSAGE
    importing
      !IV_URL type STRING
      !IV_USERNAME type STRING
      !IV_PASSWORD type STRING
      !IT_COOKIES type TIHTTPCKI .
  methods ACTIVATE_IMPORT_RECORD
    importing
      !IV_URL type STRING
      !IV_USERNAME type STRING
      !IV_PASSWORD type STRING
      !IT_COOKIES type TIHTTPCKI .
  methods SEND_DATA_CONCURRENT_PROCESS
    importing
      !IV_LIMIT type I default 5
      !IT_DATA type /ZHD/TT_DMP_DATA_LIST optional .
ENDCLASS.



CLASS /ZHD/CL_DMP_SEND_DATA IMPLEMENTATION.


  METHOD activate_import_record.

    DATA:lo_http_client TYPE REF TO if_http_client,
         lv_msg         TYPE string,
         lv_url         TYPE string
         .

    DATA:lt_cookies TYPE tihttpcki.
    lv_url = iv_url && '/Import(UUID=' && me->uuid && ',IsActiveEntity=false)/com.sap.gateway.srvd_a2x.zhdui_dmp_import_o4.v0001.Activate'.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        pse_not_found      = 4
        pse_not_distrib    = 5
        pse_errors         = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
*
    lo_http_client->authenticate(
            username = iv_username
            password = iv_password ).
    lo_http_client->request->set_header_field( name = 'x-csrf-token' value = me->token ).
    lo_http_client->request->set_header_field( name = 'If-Match' value = '*' ).

    "设置cookies
    LOOP AT it_cookies ASSIGNING FIELD-SYMBOL(<cookie>).
      lo_http_client->request->set_cookie( name = <cookie>-name
                                           path = <cookie>-path
                                           value = <cookie>-value
*                                           domain = <cookie>-domain
                                           expires = <cookie>-expires
                                           secure = <cookie>-secure ).
    ENDLOOP.


    CALL METHOD lo_http_client->request->set_method
      EXPORTING
        method = 'POST'.

    lo_http_client->send(
*    EXPORTING
*      TIMEOUT                    = CO_TIMEOUT_DEFAULT " Timeout of Answer Waiting Time
      EXCEPTIONS
        http_communication_failure = 1                  " Communication Error
        http_invalid_state         = 2                  " Invalid state
        http_processing_failed     = 3                  " Error when processing method
        http_invalid_timeout       = 4                  " Invalid Time Entry
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      lo_http_client->get_last_error(
        IMPORTING
*        CODE           =                  " Return Value, Return Value After ABAP Statements
          message        = lv_msg                 " Error Message
*        MESSAGE_CLASS  =                  " Application Area
*        MESSAGE_NUMBER =                  " Message Number
      ).

      WRITE:/ lv_msg.
      RETURN.
    ENDIF.

    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1                " Communication Error
        http_invalid_state         = 2                " Invalid state
        http_processing_failed     = 3                " Error when processing method
        OTHERS                     = 4
    ).

    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      lo_http_client->get_last_error(
        IMPORTING
*        CODE           =                  " Return Value, Return Value After ABAP Statements
        message        = lv_msg                 " Error Message
*        MESSAGE_CLASS  =                  " Application Area
*        MESSAGE_NUMBER =                  " Message Number
    ).
*      WRITE:/ lv_msg.
      RETURN.
    ENDIF.


    DATA:lv_status TYPE i.
    lo_http_client->response->get_status( IMPORTING code = lv_status ).
    DATA(ev_json) = lo_http_client->response->get_cdata( ).
    DATA(ev_json_msg) = lo_http_client->response->get_header_field( name = 'sap-messages' ).
    IF lv_status NE 200 AND lv_status NE 201.
      me->return_code = 4.
      me->return_message = TEXT-004.
    ENDIF.
  ENDMETHOD.


  METHOD create_import_record.
    DATA:lo_http_client  TYPE REF TO if_http_client,
         lo_http_client1 TYPE REF TO if_http_client,
         lv_msg          TYPE string,
         lv_url          TYPE string,
         lv_username     TYPE string,
         lv_password     TYPE string
         .

    TYPES:BEGIN OF ty_import_item,
            _line      TYPE numc5,
            _data_json TYPE string,
          END OF ty_import_item.
    TYPES:
         tty_import_item TYPE  STANDARD TABLE OF ty_import_item WITH NON-UNIQUE KEY _line.
    TYPES:
      BEGIN OF ty_import_record,
        _data_type     TYPE string,
        _name          TYPE string,
        _import_type1  TYPE string,
        _import_system TYPE string,
        _timestamp_src TYPE timestampl,
*        _timestamp_src TYPE utclong,
        __import_item  TYPE tty_import_item,
      END OF ty_import_record.
    TYPES:BEGIN OF ty_uuid,
            uuid TYPE string,
          END OF ty_uuid.

    DATA:ls_import_record TYPE ty_import_record.

    DATA: lv_tstmp   TYPE timestamp,
          lt_cookies TYPE tihttpcki,
          ls_uuid    TYPE ty_uuid.
    IF me->uuid IS NOT INITIAL.
      lv_url = iv_url && '/Import(UUID=' && me->uuid && ',IsActiveEntity=false)/_ImportItem'.
    ELSE.
      lv_url = iv_url && '/Import'.
    ENDIF.


    ls_import_record = VALUE  #(
      _data_type = 'Material'
      _name = '9766_ZHOUZQ_周志强' && sy-datum
      _import_type1 = 'IMP'
      _import_system = 'DM2'
    ).
    GET TIME STAMP FIELD ls_import_record-_timestamp_src.


    APPEND INITIAL LINE TO ls_import_record-__import_item.
    ls_import_record-__import_item[ 1 ]-_line = 1.
    ls_import_record-__import_item[ 1 ]-_data_json = me->it_data[ 1 ]-data_json.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lo_http_client1
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        pse_not_found      = 4
        pse_not_distrib    = 5
        pse_errors         = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
*
    lo_http_client1->authenticate(
            username = iv_username
            password = iv_password ).
*    lo_http_client->response->get_cookies( CHANGING cookies = lt_cookies ).
    lo_http_client1->request->set_header_field( name = 'x-csrf-token' value = me->token ).
    DATA(lv_token) = lo_http_client1->request->get_header_field(  name = 'x-csrf-token' ).
    lo_http_client1->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
*    lo_http_client->authenticate(
*            username = lv_username
*            password = lv_password ).
    "设置cookies
    LOOP AT it_cookies ASSIGNING FIELD-SYMBOL(<cookie>).
      lo_http_client1->request->set_cookie( name = <cookie>-name
                                            path = <cookie>-path
                                           value = <cookie>-value
*                                           xdomain = <cookie>-xdomain
                                           expires = <cookie>-expires
                                           secure = <cookie>-secure ).

    ENDLOOP.



    CALL METHOD lo_http_client1->request->set_method
      EXPORTING
        method = 'POST'.


*&---序列化json
    DATA lv_data_str TYPE string.
    /ui2/cl_json=>serialize(
                    EXPORTING
                     data          = ls_import_record
                     compress      = ''
                     pretty_name   = /ui2/cl_json=>pretty_mode-camel_case
                     ts_as_iso8601 = abap_true
                   RECEIVING
                     r_json        = lv_data_str ).
    REPLACE '_importItem' IN lv_data_str WITH  '_ImportItem' .
    lo_http_client1->request->if_http_entity~set_cdata( lv_data_str ).
    lo_http_client1->send(
*    EXPORTING
*      TIMEOUT                    = CO_TIMEOUT_DEFAULT " Timeout of Answer Waiting Time
      EXCEPTIONS
        http_communication_failure = 1                  " Communication Error
        http_invalid_state         = 2                  " Invalid state
        http_processing_failed     = 3                  " Error when processing method
        http_invalid_timeout       = 4                  " Invalid Time Entry
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      lo_http_client1->get_last_error(
        IMPORTING
*        CODE           =                  " Return Value, Return Value After ABAP Statements
          message        = lv_msg                 " Error Message
*        MESSAGE_CLASS  =                  " Application Area
*        MESSAGE_NUMBER =                  " Message Number
      ).

      WRITE:/ lv_msg.
      RETURN.
    ENDIF.

    lo_http_client1->receive(
      EXCEPTIONS
        http_communication_failure = 1                " Communication Error
        http_invalid_state         = 2                " Invalid state
        http_processing_failed     = 3                " Error when processing method
        OTHERS                     = 4
    ).

    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      lo_http_client1->get_last_error(
        IMPORTING
*        CODE           =                  " Return Value, Return Value After ABAP Statements
        message        = lv_msg                 " Error Message
*        MESSAGE_CLASS  =                  " Application Area
*        MESSAGE_NUMBER =                  " Message Number
    ).
      WRITE:/ lv_msg.
      RETURN.
    ENDIF.
    DATA(ev_json) = lo_http_client1->response->get_cdata( ).
    IF ev_json CS 'error'.
      me->return_code = 2.
      me->return_message = TEXT-002.
    ENDIF.

*-----获取json字符串某个字段值
    /zhd/cl_json=>deserialize(
      EXPORTING
        json = ev_json
      CHANGING
        data = ls_uuid
    ).
    me->uuid = ls_uuid-uuid.

  ENDMETHOD.


  METHOD get_data_message.
    DATA:lo_http_client TYPE REF TO if_http_client,
         lv_msg         TYPE string,
         lv_url         TYPE string,
         lv_username    TYPE string,
         lv_password    TYPE string
         .

    TYPES:BEGIN OF ty_import_item,
            _line      TYPE numc5,
            _data_json TYPE string,
          END OF ty_import_item.
    TYPES:
         tty_import_item TYPE  STANDARD TABLE OF ty_import_item WITH NON-UNIQUE KEY _line.
    TYPES:
      BEGIN OF ty_import_record,
        _data_type     TYPE string,
        _name          TYPE string,
        _import_type1  TYPE string,
        _import_system TYPE string,
        _timestamp_src TYPE timestampl,
*        _timestamp_src TYPE utclong,
        __import_item  TYPE tty_import_item,
      END OF ty_import_record.

    DATA:ls_import_record TYPE ty_import_record.

    DATA: lv_tstmp   TYPE timestamp,
          lt_cookies TYPE tihttpcki.
    lv_url = iv_url && '/Import(UUID=' && me->uuid && ',IsActiveEntity=false)/com.sap.gateway.srvd_a2x.zhdui_dmp_import_o4.v0001.Prepare'.

    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = lv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        pse_not_found      = 4
        pse_not_distrib    = 5
        pse_errors         = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.
*
    lo_http_client->authenticate(
            username = iv_username
            password = iv_password ).
*    lo_http_client->response->get_cookies( CHANGING cookies = lt_cookies ).
    lo_http_client->request->set_header_field( name = 'x-csrf-token' value = me->token ).
*    lo_http_client->request->set_header_field( name = 'Content-Type' value = 'application/json' ).
    lo_http_client->request->set_header_field( name = 'If-Match' value = '*' ).
*    lo_http_client->authenticate(
*            username = lv_username
*            password = lv_password ).
    "设置cookies
    LOOP AT it_cookies ASSIGNING FIELD-SYMBOL(<cookie>).
      lo_http_client->request->set_cookie( name = <cookie>-name
                                           path = <cookie>-path
                                           value = <cookie>-value
*                                           domain = <cookie>-domain
                                           expires = <cookie>-expires
                                           secure = <cookie>-secure ).
    ENDLOOP.



    CALL METHOD lo_http_client->request->set_method
      EXPORTING
        method = 'POST'.

    lo_http_client->send(
*    EXPORTING
*      TIMEOUT                    = CO_TIMEOUT_DEFAULT " Timeout of Answer Waiting Time
      EXCEPTIONS
        http_communication_failure = 1                  " Communication Error
        http_invalid_state         = 2                  " Invalid state
        http_processing_failed     = 3                  " Error when processing method
        http_invalid_timeout       = 4                  " Invalid Time Entry
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      lo_http_client->get_last_error(
        IMPORTING
*        CODE           =                  " Return Value, Return Value After ABAP Statements
          message        = lv_msg                 " Error Message
*        MESSAGE_CLASS  =                  " Application Area
*        MESSAGE_NUMBER =                  " Message Number
      ).

      WRITE:/ lv_msg.
      RETURN.
    ENDIF.

    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1                " Communication Error
        http_invalid_state         = 2                " Invalid state
        http_processing_failed     = 3                " Error when processing method
        OTHERS                     = 4
    ).

    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      lo_http_client->get_last_error(
        IMPORTING
*        CODE           =                  " Return Value, Return Value After ABAP Statements
        message        = lv_msg                 " Error Message
*        MESSAGE_CLASS  =                  " Application Area
*        MESSAGE_NUMBER =                  " Message Number
    ).
      WRITE:/ lv_msg.
      RETURN.
    ENDIF.
*    DATA(ev_json) = lo_http_client->response->get_cdata( ).
**    DATA(lt_fields) =  lo_http_client->response->get_header_fields( ).
    DATA:lv_status TYPE i.
    lo_http_client->response->get_status( IMPORTING code = lv_status ).
    DATA(ev_json_msg) = lo_http_client->response->get_header_field( name = 'sap-messages' ).
    IF lv_status ne 204.
      me->return_code = 3.
      me->return_message = TEXT-003.
    ENDIF.
  ENDMETHOD.


  METHOD get_token.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_msg         TYPE string.

*-----创建http对象
    CALL METHOD cl_http_client=>create_by_url
      EXPORTING
        url                = iv_url
      IMPORTING
        client             = lo_http_client
      EXCEPTIONS
        argument_not_found = 1
        plugin_not_active  = 2
        internal_error     = 3
        pse_not_found      = 4
        pse_not_distrib    = 5
        pse_errors         = 6
        OTHERS             = 7.
    IF sy-subrc <> 0.
*   Implement suitable error handling here
    ENDIF.

*------授权
    lo_http_client->authenticate(
            username = iv_username
            password = iv_password ).


*------请求类型设置
    CALL METHOD lo_http_client->request->set_method
      EXPORTING
        method = iv_method.

*-----请求抬头参数设置
    lo_http_client->request->set_header_field( name = 'x-csrf-token' value = 'fetch').


*-----发送请求
    lo_http_client->send(
*    EXPORTING
*      TIMEOUT                    = CO_TIMEOUT_DEFAULT " Timeout of Answer Waiting Time
      EXCEPTIONS
        http_communication_failure = 1                  " Communication Error
        http_invalid_state         = 2                  " Invalid state
        http_processing_failed     = 3                  " Error when processing method
        http_invalid_timeout       = 4                  " Invalid Time Entry
        OTHERS                     = 5
    ).
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      lo_http_client->get_last_error(
        IMPORTING
*        CODE           =                  " Return Value, Return Value After ABAP Statements
          message        = lv_msg                 " Error Message
*        MESSAGE_CLASS  =                  " Application Area
*        MESSAGE_NUMBER =                  " Message Number
      ).

      WRITE:/ lv_msg.
      RETURN.
    ENDIF.

*------结果接收
    lo_http_client->receive(
      EXCEPTIONS
        http_communication_failure = 1                " Communication Error
        http_invalid_state         = 2                " Invalid state
        http_processing_failed     = 3                " Error when processing method
        OTHERS                     = 4
    ).

    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      lo_http_client->get_last_error(
        IMPORTING
*        CODE           =                  " Return Value, Return Value After ABAP Statements
        message        = lv_msg                 " Error Message
*        MESSAGE_CLASS  =                  " Application Area
*        MESSAGE_NUMBER =                  " Message Number
    ).
      WRITE:/ lv_msg.
      RETURN.
    ENDIF.

*-----获取api返回的token
    ev_token = lo_http_client->response->get_header_field( name = 'x-csrf-token' ).

*------获取api 访问的cookies
    lo_http_client->response->get_cookies( CHANGING cookies = ct_cookies ).

*------设置数据推送结果码和消息
    IF ev_token IS INITIAL OR ct_cookies IS INITIAL.
      me->return_code = 1.
      me->return_message = TEXT-001.
    ENDIF.

  ENDMETHOD.


  METHOD if_http_extension~handle_request.

    DATA:lr_json_ser    TYPE REF TO cl_trex_json_serializer,
         lr_json_des    TYPE REF TO cl_trex_json_deserializer,
         lv_json_string TYPE string,
         "lt_list        TYPE TABLE OF /zhd/s_dmp_data_list,
         lv_request     TYPE string.
    DATA:lt_message TYPE  bapiret2_tab.

    DATA:BEGIN OF lw_request,
           iv_data_type       TYPE char100,
           iv_where_condition TYPE string,
         END OF lw_request.
    DATA:lt_request LIKE STANDARD TABLE OF lw_request.  "该内表格式要和外部传入参数格式一致

*----获取调用时候传入的参数
    CLEAR:lt_request.
    lv_json_string = server->request->get_cdata( ).
    CREATE OBJECT lr_json_des.
    CALL METHOD lr_json_des->deserialize
      EXPORTING
        json = lv_json_string
      IMPORTING
        abap = lt_request.

*-----获取返回数据格式
    CALL METHOD server->response->if_http_entity~set_content_type
      EXPORTING
        content_type = 'application/json'.

*-----获取要返回的数据
    IF lt_request[] IS NOT INITIAL.
      READ TABLE lt_request INTO lw_request INDEX 1.
      IF sy-subrc = 0.
        REPLACE ALL OCCURRENCES OF '\' IN lw_request-iv_where_condition WITH space.

* 获取数据
*        CALL FUNCTION '/ZHD/FM_DMP_GET_DATA'
*          EXPORTING
*            iv_data_type       = lw_request-iv_data_type
*            iv_where_condition = lw_request-iv_where_condition
**     IMPORTING
**           EV_STATUS          =
*          TABLES
*            et_list            = lt_list
*            et_message         = lt_message.
      ENDIF.
    ENDIF.

*-----要返回的数据转换成JSON格式
    CLEAR:lv_json_string.
    CREATE OBJECT lr_json_ser
      EXPORTING
        data = me->it_data.
    CALL METHOD lr_json_ser->serialize.
    CALL METHOD lr_json_ser->get_data
      RECEIVING
        rval = lv_json_string.

*-----设置返回数据
    server->response->set_cdata(
          EXPORTING
              data = lv_json_string
                    ).









  ENDMETHOD.


  METHOD send_data_concurrent_process.

**    v_page = 1. " 当前页码
**    v_limit = 10. " 每页显示条数
**
**    v_total = lines( it_data ). " 总记录数
**    v_skip = ( v_page - 1 ) * v_limit. " 跳过的记录数
**
**    " 分页处理
**    IF v_skip + v_limit < v_total.
**      " 当前页数据未超过总数时
**      APPEND LINES OF lt_data FROM v_skip + 1 TO v_skip + v_limit TO lt_page.
**    ELSE.
**      " 当前页数据超过总数时，取到最后一条数据
**      APPEND LINES OF lt_data FROM v_skip + 1 TO v_total TO lt_page.
**    ENDIF.
*
*    " 处理lt_page内表，进行后续操作
*
*    " 获取 RFC Serve Group name         Start--*
*    " 一般系统默认g_classname = 'parallel_generators'，但为了通用性按照如下方法获取
*    DATA:g_taskname(10) TYPE c,                "task name（同时运行的任务名称必须保持唯一）
*         g_classname    TYPE rzlli_apcl,          "Server Group Name
*         g_applserver   TYPE rzllitab-applserver, "RFC Serve Group
*         excp_flag(1)   TYPE c.     "Number of RESOURCE_FAILUREs
*    DATA:lt_data TYPE /zhd/tt_dmp_data_list.
*
*    IF it_data[] IS NOT INITIAL.
*      CALL 'C_SAPGPARAM'                                  "#EC CI_CCALL
*       ID 'NAME'  FIELD 'rdisp/myname'
*       ID 'VALUE' FIELD g_applserver.
*
*      SELECT SINGLE classname
*        FROM rzllitab
*        INTO g_classname   "Server Group Name
*        WHERE applserver = g_applserver
*        AND grouptype = 'S'.   "S:服务器组，空:登陆组
*      "  获取 RFC Serve Group name         End--*
*      CALL FUNCTION '/ZHD/FM_DMP_SEND_DATA' STARTING NEW TASK g_taskname
*        DESTINATION IN GROUP g_classname
*        PERFORMING
*        EXPORTING
*          it_data = lt_data.
*    ENDIF.








  ENDMETHOD.


  METHOD send_orign_data_to_dmp.
    DATA: lo_http_client TYPE REF TO if_http_client,
          lv_msg         TYPE string,
          lv_url         TYPE string,
          lv_username    TYPE string,
          lv_password    TYPE string,
          lt_cookies     TYPE tihttpcki.


*-----获取token
    lv_url = 'https://my200127-api.s4hana.sapcloud.cn/sap/opu/odata4/sap/zhdapi_dmp_import_o4/srvd_a2x/sap/zhdui_dmp_import_o4/0001'.
    lv_username = 'DMP'. " 用户名
    lv_password = 'z3BgSUlVX]gDhmhiMMBdazTyKhgxwnWSisrisZfj'. " 密码
    me->token = me->get_token( EXPORTING iv_url = lv_url iv_username = lv_username iv_password = lv_password iv_method = 'GET' CHANGING ct_cookies = lt_cookies ).
    CHECK me->return_code = 0.

*-----创建一条推送记录
    me->create_import_record(  iv_url = lv_url iv_username = lv_username iv_password = lv_password  it_cookies = lt_cookies ).
    CHECK me->return_code = 0.

*-----获取prepare消息
    me->get_data_message(  iv_url = lv_url iv_username = lv_username iv_password = lv_password  it_cookies = lt_cookies ).
    CHECK me->return_code = 0.

*-----激活推送记录
    me->activate_import_record(  iv_url = lv_url iv_username = lv_username iv_password = lv_password  it_cookies = lt_cookies ).

*-----返回处理结果
    IF me->return_code = 0.
      MESSAGE TEXT-005 TYPE 'I'.
    ELSE.
      MESSAGE TEXT-006 TYPE 'E'.
    ENDIF.
*-----数据清理
    clear: me->token, me->uuid.

  ENDMETHOD.
ENDCLASS.
