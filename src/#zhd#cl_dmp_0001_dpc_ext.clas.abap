class /ZHD/CL_DMP_0001_DPC_EXT definition
  public
  inheriting from /ZHD/CL_DMP_0001_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
protected section.
PRIVATE SECTION.

*  types TS_DEEP type CHAR12 .
  TYPES : BEGIN OF ts_item_deep.
            INCLUDE TYPE /zhd/cl_dmp_0001_mpc_ext=>ts_item.
  TYPES :   to_messages TYPE STANDARD TABLE OF /zhd/cl_dmp_0001_mpc_ext=>ts_message WITH DEFAULT KEY,
          END OF ts_item_deep,
          tt_item_deep TYPE STANDARD TABLE OF ts_item_deep WITH DEFAULT KEY.

  TYPES : BEGIN OF ts_deep.
            INCLUDE TYPE /zhd/cl_dmp_0001_mpc_ext=>ts_head.
  TYPES:   to_Items TYPE tt_item_deep,
          END OF ts_deep.
ENDCLASS.



CLASS /ZHD/CL_DMP_0001_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
*  EXPORTING
**    iv_entity_name          =
**    iv_entity_set_name      =
**    iv_source_name          =
*    IO_DATA_PROVIDER        =
**    it_key_tab              =
**    it_navigation_path      =
*    IO_EXPAND               =
**    io_tech_request_context =
**  IMPORTING
**    er_deep_entity          =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
    DATA : ls_data   TYPE ts_deep,
           lo_writer TYPE REF TO /zhd/if_dmp_write.
    TRY.

        io_data_provider->read_entry_data(
          IMPORTING
            es_data = ls_data
        ).

        CREATE OBJECT lo_writer TYPE (ls_data-class).

        LOOP AT ls_data-to_items ASSIGNING FIELD-SYMBOL(<fs_item>).


          lo_writer->write(
            EXPORTING
              is_line_data = CORRESPONDING #( <fs_item> )                " DMP平台数据传出结构
            IMPORTING
*              ev_status    =                  " 消息类型: S 成功,E 错误,W 警告,I 信息,A 中断
              et_message   = DATA(lt_message)                 " 错误消息
          ).
          <fs_item>-to_messages = VALUE #(
            FOR ls_message IN lt_message (
              VALUE #(
                BASE CORRESPONDING #( ls_message )
                messagestring = ls_message-message
                messagev1 = ls_message-message_v1
                messagev2 = ls_message-message_v2
                messagev3 = ls_message-message_v3
                messagev4  = ls_message-message_v4
                version = ls_data-version
                datatype = ls_data-datatype
                line = <fs_item>-line
             )
           )
          ).
          <fs_item>-datatype = ls_data-datatype.
          <fs_item>-version = ls_data-version.

        ENDLOOP.

        copy_data_to_ref(
          EXPORTING
            is_data = ls_data
          CHANGING
            cr_data = er_deep_entity
        ).

      CATCH /iwbep/cx_mgw_tech_exception INTO DATA(lx_tech). " mgw technical exception(
        " 读取数据失败
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
*           textid   =
            previous = lx_tech
*           message_container      =
*           http_status_code       =
*           http_header_parameters =
*           sap_note_id            =
*           msg_code =
*           exception_category     =
*           entity_type            =
*           message  =
*           message_unlimited      =
*           filter_param           =
*           operation_no           =
          .

      CATCH cx_sy_create_object_error INTO DATA(lx_no_class).
        " 创建类失败
        RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
          EXPORTING
*           textid   =
            previous = lx_no_class
*           message_container      =
*           http_status_code       =
*           http_header_parameters =
*           sap_note_id            =
*           msg_code =
*           exception_category     =
*           entity_type            =
*           message  =
*           message_unlimited      =
*           filter_param           =
*           operation_no           =
          .
    ENDTRY.


  ENDMETHOD.
ENDCLASS.
