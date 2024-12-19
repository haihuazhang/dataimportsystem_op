FUNCTION /zhd/fm_dmp_log_save.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     REFERENCE(IV_DATA_TYPE) TYPE  CHAR100 OPTIONAL
*"     REFERENCE(IV_WHERE_CONDITION) TYPE  STRING OPTIONAL
*"     REFERENCE(IV_FILE_NAME) TYPE  STRING
*"     REFERENCE(IV_STAUTS) TYPE  /ZHD/ZE_STATUS OPTIONAL
*"  TABLES
*"      IT_MESSAGE TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  DATA:lo_if_dmp_src_data TYPE REF TO /zhd/if_dmp_src_data.
  DATA:lv_status TYPE char1,
       lt_list   TYPE /zhd/tt_dmp_data_list.

  DATA:lv_log_handle TYPE balloghndl,
       lv_lognumber  TYPE balhdr-lognumber.

  DATA:lo_if_log_tool TYPE REF TO /zhd/if_log_tool.

  DATA:ls_log TYPE bal_s_log.

  DATA:ls_dmp_log TYPE /zhd/t_dmp_log,
       lt_dmp_log TYPE TABLE OF /zhd/t_dmp_log.
  DATA:lv_classname TYPE char100.

  DATA: lv_object TYPE balobj_d  VALUE '/ZHD/DMP_LOG',  "对象名
        lv_subobj TYPE balsubobj VALUE 'DMP_LOG',       "子对象名
        lv_id     TYPE balnrext.
  DATA: lt_msg TYPE TABLE OF bal_s_msg,
        ls_msg TYPE bal_s_msg.
*日志保存
  DATA:
    lt_log_handle     TYPE bal_t_logh,
    ls_new_lognumber  TYPE bal_s_lgnm,
    lt_new_lognumbers TYPE bal_t_lgnm.

* 获取数据类型对应的UUID
  SELECT SINGLE *
  FROM /zhd/t_dmp_datat
  INTO @DATA(ls_datat)
  WHERE data_type = @iv_data_type.
  IF sy-subrc = 0.

  ENDIF.

* 获取JSON字符串
  lv_classname = ls_datat-class.
  CALL FUNCTION '/ZHD/FM_DMP_GET_DATA'
    EXPORTING
      iv_class_name      = lv_classname
      iv_where_condition = iv_where_condition
    IMPORTING
      ev_status          = lv_status
      et_list            = lt_list
      et_message         = it_message[].


  SORT it_message BY row.
  LOOP AT lt_list INTO DATA(ls_list).

    CLEAR lv_log_handle.

*&---First  create log header
    ls_log-object    = lv_object."对象名
    ls_log-subobject = lv_subobj."子对象名
    ls_log-extnumber = sy-datum.

* create the log header
    CALL FUNCTION 'BAL_LOG_CREATE'
      EXPORTING
        i_s_log      = ls_log
      IMPORTING
        e_log_handle = lv_log_handle
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    READ TABLE it_message INTO DATA(is_message) WITH KEY row = ls_list-line BINARY SEARCH.
    IF sy-subrc = 0.
      ls_msg-msgty   =   is_message-type.
      ls_msg-msgid   =   is_message-id.
      ls_msg-msgno   =   is_message-number.
      ls_msg-msgv1   =   is_message-message+0(50).
      ls_msg-msgv2   =   is_message-message+50(50).
      ls_msg-msgv3   =   is_message-message+100(50).
      ls_msg-msgv4   =   is_message-message+150(50).
    ENDIF.

* create the message
    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle = lv_log_handle
        i_s_msg      = ls_msg
      EXCEPTIONS
        OTHERS       = 1.
    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    CLEAR lt_log_handle.
    INSERT lv_log_handle INTO TABLE lt_log_handle.


*  * save this log
    CALL FUNCTION 'BAL_DB_SAVE'
      EXPORTING
        i_t_log_handle   = lt_log_handle
      IMPORTING
        e_new_lognumbers = lt_new_lognumbers
      EXCEPTIONS
        OTHERS           = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.

* find out the lognumber of this saved log
      CLEAR lv_lognumber.
      READ TABLE lt_new_lognumbers INTO ls_new_lognumber
                                   WITH KEY log_handle = lv_log_handle.
      IF sy-subrc EQ 0.
        lv_lognumber = ls_new_lognumber-lognumber.
* 数据写入
        ls_dmp_log-mandt = sy-mandt.
        ls_dmp_log-uuid = cl_system_uuid=>create_uuid_x16_static( ).
        ls_dmp_log-uuid_data_type = ls_datat-uuid.
        ls_dmp_log-where_conditions = iv_where_condition.
        ls_dmp_log-status = iv_stauts.
        ls_dmp_log-lognumber = lv_lognumber.
        ls_dmp_log-created_by = sy-uname.
        ls_dmp_log-last_changed_by = sy-uname.
        GET TIME STAMP FIELD ls_dmp_log-created_at.
        GET TIME STAMP FIELD ls_dmp_log-last_changed_at.
        GET TIME STAMP FIELD ls_dmp_log-local_last_changed_at.
        APPEND ls_dmp_log TO lt_dmp_log.
      ENDIF.

    ENDIF.
  ENDLOOP.
*  将日志数据插入日志表
  INSERT /zhd/t_dmp_log FROM TABLE lt_dmp_log .
  IF sy-subrc = 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.


*保存到SLG1: /ZHD/CL_LOG_MAIN

*获取SLG1 handle log id

*生成日志UUID

*找到data type对应的UUID

*记录日期戳
*
*保存日志到自建表


ENDFUNCTION.
