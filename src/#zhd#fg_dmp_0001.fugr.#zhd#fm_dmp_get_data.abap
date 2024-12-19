FUNCTION /zhd/fm_dmp_get_data.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IV_CLASS_NAME) TYPE  CHAR100
*"     VALUE(IV_WHERE_CONDITION) TYPE  STRING
*"  EXPORTING
*"     VALUE(EV_STATUS) TYPE  CHAR1
*"     VALUE(ET_LIST) TYPE  /ZHD/TT_DMP_DATA_LIST
*"     VALUE(ET_MESSAGE) TYPE  BAPIRET2_TAB
*"----------------------------------------------------------------------
  DATA:lo_if_dmp_src_data TYPE REF TO /zhd/if_dmp_src_data.
  DATA:lv_status  TYPE char1,
       lt_list    TYPE /zhd/tt_dmp_data_list,
       lt_message TYPE bapiret2_tab,
       ls_message TYPE bapiret2.
  DATA:lt_implementings TYPE seor_implementings_r,
       ls_implementings TYPE vseoimplem.
  DATA:lv_clskey TYPE seoclskey.
  CLEAR:lv_status,lt_list,lt_message,ls_message,lt_implementings.

* 配置表中未填写类名
  IF iv_class_name IS INITIAL.
    ls_message-message = '配置表/ZHD/T_DMP_DATAT中未填写CLASS或系统中CLASS未创建，请检查！'.
    ls_message-type = 'E'.
    APPEND ls_message TO et_message.
    ev_status = 'E'.
  ELSE.
    lv_clskey-clsname = iv_class_name.
    "系统中不存在填写的类名
    CALL FUNCTION 'SEO_CLASS_EXISTENCE_CHECK'
      EXPORTING
        clskey        = lv_clskey
      EXCEPTIONS
        not_specified = 1
        not_existing  = 2
        is_interface  = 3
        no_text       = 4
        inconsistent  = 5
        OTHERS        = 6.
    IF sy-subrc <> 0.
      ls_message-message = '配置表/ZHD/T_DMP_DATAT中未填写CLASS或系统中CLASS未创建，请检查！'.
      ls_message-type = 'E'.
      APPEND ls_message TO et_message.
      ev_status = 'E'.
    ELSE.
      lv_clskey-clsname = iv_class_name.
      CALL FUNCTION 'SEO_CLASS_RESOLVE_INHERITANCE'
        EXPORTING
          clskey        = lv_clskey
        IMPORTING
          implementings = lt_implementings
        EXCEPTIONS
          not_existing  = 1
          is_interface  = 2
          model_only    = 3
          OTHERS        = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ELSE.
        READ TABLE lt_implementings INTO ls_implementings INDEX 1.
        "配置类名未引用接口/zhd/if_dmp_src_data
        IF ls_implementings-refclsname <> '/ZHD/IF_DMP_SRC_DATA'.
          ls_message-message = iv_class_name && '未实施正确接口/ZHD/IF_DMP_SRC_DATA，请检查！'.
          ls_message-type = 'E'.
          APPEND ls_message TO et_message.
          ev_status = 'E'.
        ELSE.
          "调用类取数
          CREATE OBJECT lo_if_dmp_src_data TYPE (iv_class_name).
          CALL METHOD lo_if_dmp_src_data->get_data(
            EXPORTING
              iv_where_condition = iv_where_condition
            IMPORTING
              ev_status          = lv_status
              et_list            = lt_list
              et_message         = lt_message ).

          et_message = lt_message.
          ev_status = lv_status.
          et_list = lt_list.
          IF ev_status = 'F'.
            ev_status = 'E'.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFUNCTION.
