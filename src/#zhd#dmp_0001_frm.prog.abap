*&---------------------------------------------------------------------*
*& 包含               /ZHD/DMP_0001_FRM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form frm_datatype_f4_help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_datatype_f4_help .

  SELECT
    data_type,
    name
  FROM /zhd/t_dmp_datat
  INTO TABLE @DATA(lt_datat).

  IF sy-subrc = 0.
    SORT lt_datat BY data_type.
    DELETE ADJACENT DUPLICATES FROM lt_datat COMPARING data_type.
  ENDIF.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield         = 'DATA_TYPE'
      dynpprog         = sy-repid
      dynpnr           = sy-dynnr
      dynprofield      = 'P_TYPE'
      value_org        = 'S'
      callback_program = sy-repid
    TABLES
      value_tab        = lt_datat
    EXCEPTIONS
      parameter_error  = 1
      no_values_found  = 2
      OTHERS           = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_file_f4_help
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_file_f4_help .

  CALL FUNCTION 'WS_FILENAME_GET'
    EXPORTING
      def_path         = 'C:\'
*     mask             = ',Text Files,*.txt;*.prn,All Files,*.*.'(101)
      mask             = ',*.xls,*.*.' " upload for excel
      title            = 'Select File'(100)
    IMPORTING
      filename         = p_file
    EXCEPTIONS
      inv_winsys       = 1
      no_batch         = 2
      selection_cancel = 3
      selection_error  = 4
      OTHERS           = 5.
  IF sy-subrc <> 0 AND sy-subrc <> 3.
*    MESSAGE e001(00) WITH 'Error Selecting File'(007).
    MESSAGE e008(ytr).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_set_attribute
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_set_attribute .
  LOOP AT SCREEN.
    IF rb_input IS NOT INITIAL.
      IF screen-group1 = 'D'.
        screen-invisible = 1.
        screen-active = 0.
        screen-required = 0.
      ELSE.
        screen-invisible = 0.
        screen-active = 1.
      ENDIF.
    ELSE.
      IF screen-group1 = 'I'.
        screen-invisible = 1.
        screen-active = 0.
        screen-required = 0.
      ELSE.
        screen-invisible = 0.
        screen-active = 1.
        screen-required = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_down_excel
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_down_excel .

  DATA:lo_dmp_src_data TYPE REF TO /zhd/cl_dmp_src_data.
  DATA:lv_fpath TYPE string.
  DATA:filename TYPE string.
  DATA:lv_status TYPE char1.
  DATA:lt_message TYPE bapiret2_tab,
       ls_message TYPE bapiret2.
  DATA lr_temp TYPE REF TO data.
  DATA lr_data TYPE REF TO data.
  FIELD-SYMBOLS: <ls_data_temp> TYPE any,
                 <ls_data>      TYPE any.
  DATA:
    lo_descr_ref TYPE REF TO cl_abap_typedescr,
    lo_struc     TYPE REF TO cl_abap_structdescr.

  DATA: lt_components TYPE abap_compdescr_tab.

*  filename = '下载数据.xlsx'.
*  lv_fpath = p_file.
*  CONCATENATE lv_fpath '\' filename INTO filename .
  filename = p_file.
  LOOP AT gt_list INTO DATA(ls_list).

    /ui2/cl_json=>deserialize( EXPORTING json = ls_list-data_json assoc_arrays = abap_true CHANGING data = lr_data ).
    ASSIGN lr_data->* TO <ls_data_temp>.
    CREATE DATA go_data LIKE <ls_data_temp>.
    ASSIGN go_data->* TO <ls_data>.
    <ls_data> = <ls_data_temp>.
    PERFORM frm_abap2xlsx USING go_data
                                <ls_data>
                                filename.
  ENDLOOP.

* 保存日志
  lv_status = 'S'.
  ls_message-message = '数据下载成功'.
  ls_message-type = 'S'.
  APPEND ls_message TO lt_message.

  CALL FUNCTION '/ZHD/FM_DMP_LOG_SAVE'
    EXPORTING
      iv_data_type       = p_type
      iv_where_condition = p_where
      iv_file_name       = p_file
      iv_stauts          = lv_status
    TABLES
      it_message         = lt_message.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form FRM_ABAP2XLSX
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GO_DATA
*&---------------------------------------------------------------------*
FORM frm_abap2xlsx  USING go_data
                          ps_data
                          pv_filename.
  CHECK go_data IS NOT INITIAL.

  DATA: lo_type    TYPE REF TO cl_abap_typedescr,
        lo_struc   TYPE REF TO cl_abap_structdescr,
        lv_lines   TYPE i VALUE 1,
        lv_message TYPE string.
  FIELD-SYMBOLS: <ls_data> TYPE any.

  " 检查文件路径
  DATA lv_valid TYPE abap_bool.

  PERFORM check_file_path USING pv_filename
                          CHANGING lv_valid
                                   lv_message.

  IF lv_valid <> abap_true.
    MESSAGE e001(00) WITH lv_message RAISING invalid_path.
  ENDIF.

  ASSIGN go_data->* TO <ls_data>.
  <ls_data> = ps_data.
  CHECK <ls_data> IS ASSIGNED.

  lo_type ?= cl_abap_typedescr=>describe_by_data( <ls_data> ).
  DATA(lv_type) = lo_type->kind.

  IF lv_type = 'S'.
    lo_struc ?= lo_type.
    DATA(lt_nodes) = lo_struc->components.
    lv_lines = lines( lt_nodes ).
  ENDIF.

  " 创建Excel
  CREATE OBJECT gs_excel 'EXCEL.APPLICATION'.
  SET PROPERTY OF gs_excel 'VISIBLE' = 1.
  GET PROPERTY OF gs_excel 'WORKBOOKS' = gs_wbook.
  " 初始化Sheet页
  SET PROPERTY OF gs_excel 'SHEETSINNEWWORKBOOK' = lv_lines.
  CALL METHOD OF gs_wbook 'ADD'.

  DATA:lo_data TYPE REF TO data.
  FIELD-SYMBOLS: <lt_node_data>  TYPE ANY TABLE.
  " 赋值
  IF lt_nodes[] IS NOT INITIAL.
    LOOP AT lt_nodes INTO DATA(ls_nodes).
      DATA(lv_tabix) = sy-tabix.
      ASSIGN COMPONENT ls_nodes-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_node>).
      CHECK <lv_node> IS ASSIGNED.
      ASSIGN <lv_node>->* TO <lt_node_data>.
      lo_type ?= cl_abap_typedescr=>describe_by_data( <lt_node_data> ).
      IF lo_type->kind = 'T'.
        PERFORM frm_fill_table2xlsx_1 USING <lt_node_data>
                                            ls_nodes-name
                                            lv_tabix.
      ENDIF.
    ENDLOOP.
  ENDIF.

  " 保存Excel
  GET PROPERTY OF gs_excel 'ACTIVEWORKBOOK' = gs_wbook.

  CALL METHOD OF gs_wbook 'SAVEAS'
    EXPORTING
      #1 = pv_filename
      #2 = 1.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_fill_table2xlsx_1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> <LV_NODE>
*&      --> LS_NODES_NAME
*&      --> LV_TABIX
*&---------------------------------------------------------------------*
FORM frm_fill_table2xlsx_1  USING    ps_data
                                     p_ls_nodes_name
                                     p_lv_tabix.

  DATA: lv_row    TYPE i,
        lv_column TYPE i,
        ls_table  TYPE REF TO data,
        ls_sheets TYPE ole2_object,
        ls_range  TYPE ole2_object,
        ls_cell1  TYPE ole2_object,
        ls_cell2  TYPE ole2_object,
        ls_cell   TYPE ole2_object.

  DATA: BEGIN OF ls_char,
          line TYPE c LENGTH 8000,
        END OF ls_char,
        lt_char  LIKE TABLE OF ls_char,
        lt_char2 LIKE TABLE OF ls_char.

  DATA: lo_tab       TYPE REF TO cl_abap_tabledescr,
        lo_tab_struc TYPE REF TO cl_abap_structdescr,
        lo_type      TYPE REF TO cl_abap_typedescr.
  DATA: lt_exclude TYPE TABLE OF i.

  FIELD-SYMBOLS: <lt_table> TYPE ANY TABLE.
  FIELD-SYMBOLS: <ls_data_temp> TYPE any.

  GET REFERENCE OF ps_data INTO ls_table.
  ASSIGN ls_table->* TO <lt_table>.

  LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table_c>).
    ASSIGN <ls_table_c>->* TO <ls_data_temp>.
    lo_type ?= cl_abap_typedescr=>describe_by_data( <ls_data_temp> ).
    DATA(lv_type) = lo_type->kind.
    IF lv_type = 'S'.
      lo_tab_struc ?= lo_type.
      DATA(lt_fields) = lo_tab_struc->components.
    ENDIF.
    EXIT.
  ENDLOOP.

  " 切换Sheet页
  GET PROPERTY OF gs_excel 'SHEETS' = gs_sheet
    EXPORTING
      #1 = p_lv_tabix.

  CALL METHOD OF gs_sheet 'ACTIVATE'.

  " 设置sheet名称
  IF p_ls_nodes_name IS NOT INITIAL.
    SET PROPERTY OF gs_sheet 'NAME' = p_ls_nodes_name.
  ENDIF.

  " 设置标题行
  LOOP AT lt_fields INTO DATA(ls_fields).
    lv_column = sy-tabix.

    " 排除嵌套结构
    IF ls_fields-type_kind = cl_abap_typedescr=>typekind_table OR
       ls_fields-type_kind = cl_abap_typedescr=>typekind_struct2.
      APPEND lv_column TO lt_exclude.
      CONTINUE.
    ENDIF.

    CALL METHOD OF gs_sheet 'CELLS' = ls_cell
      EXPORTING
        #1 = 1
        #2 = lv_column.
    SET PROPERTY OF ls_cell 'VALUE' = ls_fields-name.
    CALL METHOD OF ls_cell 'INTERIOR' = ls_cell1.
    SET PROPERTY OF ls_cell1 'COLORINDEX' = 15."设置标题背景为灰色
  ENDLOOP.

  " 设置表格数据
  DATA(lv_lines) = lines( lt_fields ).
  FIELD-SYMBOLS: <ls_table_temp> TYPE any.
  LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).
    ASSIGN <ls_table>->* TO <ls_table_temp>.
    CLEAR ls_char.
    DO lv_lines TIMES.
      lv_column = sy-index.
      " 排除嵌套结构
      READ TABLE lt_exclude WITH KEY table_line = lv_column TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        CONTINUE.
      ENDIF.

      READ TABLE lt_fields INTO ls_fields INDEX sy-index.
      ASSIGN COMPONENT ls_fields-name OF STRUCTURE <ls_table_temp> TO FIELD-SYMBOL(<lv_value>).
      CHECK <lv_value> IS ASSIGNED.
      ASSIGN <lv_value>->* TO FIELD-SYMBOL(<lv_n>).
      CHECK <lv_n> IS ASSIGNED.
      CONCATENATE ls_char-line <lv_n>
      INTO ls_char-line SEPARATED BY cl_abap_char_utilities=>horizontal_tab.

    ENDDO.
    SHIFT ls_char-line BY 1 PLACES LEFT.
    APPEND ls_char TO lt_char.
  ENDLOOP.

  DATA lv_rc TYPE i.

  IF lt_char[] IS NOT INITIAL.
    CALL FUNCTION 'CONTROL_FLUSH'
      EXCEPTIONS
        OTHERS = 3.

    " 复制数据到剪贴板
    CALL METHOD cl_gui_frontend_services=>clipboard_export
      EXPORTING
        no_auth_check        = abap_true
      IMPORTING
        data                 = lt_char
      CHANGING
        rc                   = lv_rc
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        not_supported_by_gui = 3
        no_authority         = 4
        OTHERS               = 5.

    " 选择单元格
    CALL METHOD OF gs_sheet 'CELLS' = ls_cell
      EXPORTING
        #1 = 2
        #2 = 1.

    CALL METHOD OF ls_cell 'SELECT'.

    " 粘贴数据
    CALL METHOD OF gs_sheet 'PASTE'.
  ENDIF.

  " 选择所有数据
  CALL METHOD OF gs_sheet 'CELLS' = ls_cell1
    EXPORTING
      #1 = 1
      #2 = 1.

  DATA(lv_rows) = lines( <lt_table> ) + 1.
  DATA(lv_columns) = lines( lt_fields ) - lines( lt_exclude ).
  CALL METHOD OF gs_sheet 'CELLS' = ls_cell2
    EXPORTING
      #1 = lv_rows
      #2 = lv_lines.

  CALL METHOD OF gs_excel 'RANGE' = ls_range
    EXPORTING
      #1 = ls_cell1
      #2 = ls_cell2.

  CALL METHOD OF ls_range 'SELECT'.

  " 设置Border
  GET PROPERTY OF ls_range 'BORDERS' = ls_cell.
  SET PROPERTY OF ls_cell 'WEIGHT' = '2'.
  SET PROPERTY OF ls_cell 'LINESTYLE' = 1.

  " 设置Columns
  GET PROPERTY OF ls_range 'COLUMNS' = ls_cell.
  CALL METHOD OF ls_cell 'AUTOFIT'.
  SET PROPERTY OF ls_cell 'NUMBERFORMATLOCAL' = '@'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form check_file_path
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PV_FILENAME
*&      <-- LV_VALID
*&      <-- LV_MESSAGE
*&---------------------------------------------------------------------*
FORM check_file_path  USING    p_pv_filename
                      CHANGING p_lv_valid
                               p_lv_message.

  DATA: lv_filename TYPE string,
        lv_result   TYPE abap_bool.

  p_lv_valid = abap_false.

  CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_pv_filename
    IMPORTING
      stripped_name = lv_filename.

  SPLIT lv_filename AT '.' INTO DATA(lv_fname) DATA(lv_ftype).
  TRANSLATE lv_ftype TO UPPER CASE.
  IF lv_ftype <> 'XLSX' AND lv_ftype <> 'XLS'.
    MESSAGE e001(00) WITH '只支持XLSX/XLS文件' INTO p_lv_message.
    RETURN.
  ENDIF.

  p_lv_valid = abap_true.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_search_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_search_data .

  DATA:lv_message TYPE string.
  DATA:lv_classname TYPE char100.

* 获取数据类型对应的实例类名
  SELECT SINGLE *
  FROM /zhd/t_dmp_datat
  INTO @DATA(ls_datat)
  WHERE data_type = @p_type.
  IF sy-subrc = 0.
*            "调用类取数
    lv_classname = ls_datat-class.

    CALL FUNCTION '/ZHD/FM_DMP_GET_DATA'
      EXPORTING
        iv_class_name      = lv_classname
        iv_where_condition = p_where
      IMPORTING
        et_list            = gt_list
        et_message         = gt_message.

    IF gt_list IS NOT INITIAL.
* 将数据转换为JSON
      CLEAR gv_json.
      gv_json = /ui2/cl_json=>serialize( data          = gt_list
                                         pretty_name   = /ui2/cl_json=>pretty_mode-camel_case ).
    ELSE.
* 未查询到数据，进行报错
      CLEAR:lv_message.
      LOOP AT gt_message INTO DATA(ls_message) WHERE type = 'E' OR type = 'A'.
        lv_message = lv_message && ls_message-message.
      ENDLOOP.
      MESSAGE lv_message TYPE 'E'.
    ENDIF.

  ELSE.
    "未在配置表中查询到相应数据
    MESSAGE '传入数据类型' && p_type && '未在配置表/ZHD/T_DMP_DATAT中查询到相应数据，请检查！' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_batch_push
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_batch_push .

  DATA: lv_status  TYPE char1,
        lt_message TYPE bapiret2_tab.
  DATA: lo_dmp_send_data TYPE REF TO /zhd/cl_dmp_send_data.
  DATA: lo_sever TYPE REF TO if_http_server.

  CREATE OBJECT lo_dmp_send_data.

* 检查当前可用并发空间

  IF p_pages IS INITIAL.
* 推送全部数据
    lo_dmp_send_data->it_data = gt_list.
    lo_dmp_send_data->send_orign_data_to_dmp( gt_list ).
*    CALL METHOD lo_dmp_send_data->if_http_extension~handle_request( lo_sever ).

  ELSE.
    PERFORM send_data_concurrent_process USING p_pages.
  ENDIF.

* 保存日志
  CALL FUNCTION '/ZHD/FM_DMP_LOG_SAVE'
    EXPORTING
      iv_data_type       = p_type
      iv_where_condition = p_where
      iv_file_name       = p_file
      iv_stauts          = lv_status
    TABLES
      it_message         = lt_message.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_test_push
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_test_push .

* 预览JSON数据
*  gv_json = '{ "MARA": [{"KEY": "MAT001","PARENT_KEY": "","MATNR": "MAT001","MTART": "RAW","MEINS": "EA"}]}'.
  cl_demo_output=>display_json( gv_json ).

ENDFORM.

*&---------------------------------------------------------------------*
*& Form send_data_concurrent_process
*&---------------------------------------------------------------------*
*& 并发处理推送数据
*&---------------------------------------------------------------------*
*& -->  p_pages        并发每页数据条数
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM send_data_concurrent_process USING p_pages.

  " 获取 RFC Serve Group name         Start--*
  " 一般系统默认g_classname = 'parallel_generators'，但为了通用性按照如下方法获取
  DATA:g_taskname(100) TYPE c,                "task name（同时运行的任务名称必须保持唯一）
       g_classname     TYPE rzlli_apcl,          "Server Group Name
       g_applserver    TYPE rzllitab-applserver, "RFC Serve Group
       excp_flag(1)    TYPE c,
       open_task_num   TYPE i.     "Number of RESOURCE_FAILUREs
  DATA:lt_data TYPE /zhd/tt_dmp_data_list.
  DATA:lv_page  TYPE i,
       lv_limit TYPE i,
       lv_total TYPE i,
       lv_skip  TYPE i,
       lv_uuid  TYPE string.
  DATA: lo_dmp_send_data TYPE REF TO /zhd/cl_dmp_send_data.

  lv_page = 1. " 当前页码
  lv_limit = p_pages. " 每页显示条数
  lv_total = lines( gt_list ). " 总记录数

  IF lv_total > 0.
    CALL 'C_SAPGPARAM'                                    "#EC CI_CCALL
     ID 'NAME'  FIELD 'rdisp/myname'
     ID 'VALUE' FIELD g_applserver.

    SELECT SINGLE classname
      FROM rzllitab
      INTO g_classname   "Server Group Name
      WHERE applserver = g_applserver
      AND grouptype = 'S'.   "S:服务器组，空:登陆组

    DATA(lv_threadno) = ceil( lv_total / p_pages ).

    DO lv_threadno TIMES.
*    " 分页处理
      lv_skip = ( lv_page - 1 ) * lv_limit. " 跳过的记录数

      IF lv_skip + lv_limit < lv_total ." 当前页数据未超过总数时
        APPEND LINES OF gt_list FROM lv_skip + 1 TO lv_skip + lv_limit TO lt_data.
      ELSE."当前页数据超过总数时，取到最后一条数据
        APPEND LINES OF gt_list FROM lv_skip + 1 TO lv_total TO lt_data.
      ENDIF.
      "第一页需要单独处理，获取UUID,并传入后续分页数据推送进程
      IF lv_page  = 1.
        lo_dmp_send_data->it_data = gt_list.
        lo_dmp_send_data->send_orign_data_to_dmp( lt_data ).
        lv_uuid = lo_dmp_send_data->uuid.
      ELSE.
        g_taskname = '操作者:' && sy-uname && '，日期：' && sy-datum && ',数据推送,第' && lv_page && '页'.
        CALL FUNCTION '/ZHD/FM_DMP_SEND_DATA' STARTING NEW TASK g_taskname
          DESTINATION IN GROUP g_classname
          PERFORMING frm_subroutine_done ON END OF TASK
          EXPORTING
            it_data = lt_data
            iv_uuid = lv_uuid.
        IF sy-subrc = 0.
          snd_jobs = snd_jobs + 1.
        ENDIF.
*      FREE lt_co_tasks.
        open_task_num = open_task_num + 1.   "记录启动的进程数量
      ENDIF.



      IF open_task_num = p_wp.    "p_wp = RZ12中的 Max. requests in queue
        "     获取并发进程返回的结果
        WAIT UNTIL rcv_jobs >= snd_jobs.          "多线程收尾工作 判断是否结束   开始做回收结果 直到5次后进行下一步清空
        COMMIT WORK.
        CLEAR:open_task_num,rcv_jobs,snd_jobs.
      ENDIF.

      lv_page = lv_page + 1.

    ENDDO.

    WAIT UNTIL fcall EQ 'X' AND rcv_jobs = snd_jobs.
    COMMIT WORK."回收线程
    CLEAR:open_task_num,rcv_jobs,snd_jobs,fcall.

    DATA(lv_message) = '并行数据推送成功，总计开启了' && lv_threadno && '个进程'.
    MESSAGE  lv_message TYPE 'I'.
  ELSE.
    MESSAGE '未找到任何数据' TYPE 'E'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form frm_subroutine_done
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM frm_subroutine_done USING g_taskname.
  IF sy-subrc EQ 0.
    fcall = 'X'.
  ENDIF.
  rcv_jobs = rcv_jobs + 1.
ENDFORM.
