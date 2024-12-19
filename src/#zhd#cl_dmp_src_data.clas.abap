class /ZHD/CL_DMP_SRC_DATA definition
  public
  abstract
  create public .

public section.

  interfaces /ZHD/IF_DMP_SRC_DATA .

  aliases GT_MESSAGE
    for /ZHD/IF_DMP_SRC_DATA~GT_MESSAGE .
  aliases GV_MESSAGE
    for /ZHD/IF_DMP_SRC_DATA~GV_MESSAGE .
  aliases GV_STATUS
    for /ZHD/IF_DMP_SRC_DATA~GV_STATUS .
  aliases ABAP2XLSX
    for /ZHD/IF_DMP_SRC_DATA~ABAP2XLSX .
  aliases GET_DATA
    for /ZHD/IF_DMP_SRC_DATA~GET_DATA .

  types:
    BEGIN OF ty_fields,
        table TYPE char20,
        field TYPE char20,
      END OF ty_fields .
  types:
    tty_fields TYPE TABLE OF ty_fields .

  class-data GV_KEY_FNAME type CHAR20 value 'KEY' ##NO_TEXT.
  class-data GV_PARENT_KEY_FNAME type CHAR20 value 'PARENT_KEY' ##NO_TEXT.

  class-methods SERIALIZE
    importing
      !IV_DATA type ANY
      !IT_CONVERT type TTY_FIELDS optional
    exporting
      !EV_JSON type STRING .
  class-methods CONVERT_TO_CAMEL_CASE
    importing
      !IV_VALUE type ANY
    returning
      value(RV_VALUE) type STRING .
protected section.

  data GO_DATA type ref to DATA .
  data GS_EXCEL type OLE2_OBJECT .
  data GS_WBOOK type OLE2_OBJECT .
  data GS_SHEET type OLE2_OBJECT .

  methods SET_DATA
    importing
      !IV_DATA type ANY .
  methods SET_TABLE_KEY
    importing
      !IV_TABLE_NAME type ANY optional
      !IT_KEYS type TTY_FIELDS optional
      !IT_PARENT_KEYS type TTY_FIELDS optional
    changing
      !CV_DATA type ANY TABLE .
  methods FILL_TABLE2XLSX
    importing
      !IV_DATA type ANY TABLE
      !IV_SHEET_NAME type ANY optional
      !IV_SHEET_INDEX type I default '1' .
  methods CHECK_FILE_PATH
    importing
      !IV_PATH type STRING
    exporting
      !EV_VALID type ABAP_BOOL
      !EV_MESSAGE type STRING .
private section.

  class-methods SERIALIZE_TABLE
    importing
      !IV_DATA type ANY TABLE
      !IT_CONVERT type TTY_FIELDS optional
      !IV_NAME type ABAP_COMPNAME optional
    exporting
      !EV_JSON type STRING .
  class-methods SERIALIZE_STRUC
    importing
      !IV_DATA type ANY
      !IT_CONVERT type TTY_FIELDS optional
      !IV_NAME type ABAP_COMPNAME optional
    exporting
      !EV_JSON type STRING .
ENDCLASS.



CLASS /ZHD/CL_DMP_SRC_DATA IMPLEMENTATION.


  METHOD /zhd/if_dmp_src_data~abap2xlsx.
    CHECK go_data IS NOT INITIAL.

    DATA: lo_type  TYPE REF TO cl_abap_typedescr,
          lo_struc TYPE REF TO cl_abap_structdescr,
          lv_lines TYPE i VALUE 1.
    FIELD-SYMBOLS: <ls_data> TYPE any.

    " 检查文件路径
    DATA lv_valid TYPE abap_bool.
    check_file_path( EXPORTING iv_path = iv_path
                     IMPORTING ev_valid = lv_valid
                               ev_message = gv_message ).
    IF lv_valid <> abap_true.
      MESSAGE e001(00) WITH gv_message RAISING invalid_path.
    ENDIF.

    ASSIGN me->go_data->* TO <ls_data>.

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

    " 赋值
    IF lt_nodes[] IS NOT INITIAL.
      LOOP AT lt_nodes INTO DATA(ls_nodes).
        DATA(lv_tabix) = sy-tabix.

        ASSIGN COMPONENT ls_nodes-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_node>).
        CHECK <lv_node> IS ASSIGNED.

        lo_type ?= cl_abap_typedescr=>describe_by_data( <lv_node> ).
        IF lo_type->kind = 'T'.
          me->fill_table2xlsx( EXPORTING iv_data        = <lv_node>
                                         iv_sheet_name  = ls_nodes-name
                                         iv_sheet_index = lv_tabix ).
        ENDIF.
      ENDLOOP.
    ELSE.
      me->fill_table2xlsx( EXPORTING iv_data = <ls_data> ).
    ENDIF.

    " 保存Excel
    GET PROPERTY OF gs_excel 'ACTIVEWORKBOOK' = gs_wbook.

    CALL METHOD OF gs_wbook 'SAVEAS'
      EXPORTING
        #1 = iv_path
        #2 = 1.

  ENDMETHOD.


  METHOD /zhd/if_dmp_src_data~get_data.

  ENDMETHOD.


  METHOD check_file_path.
    DATA: lv_filename TYPE string,
          lv_result   TYPE abap_bool.

    ev_valid = abap_false.

    CALL FUNCTION 'SO_SPLIT_FILE_AND_PATH'
      EXPORTING
        full_name     = iv_path
      IMPORTING
        stripped_name = lv_filename.

    SPLIT lv_filename AT '.' INTO DATA(lv_fname) DATA(lv_ftype).
    TRANSLATE lv_ftype TO UPPER CASE.
    IF lv_ftype <> 'XLSX' AND lv_ftype <> 'XLS'.
      MESSAGE e001(00) WITH '只支持XLSX/XLS文件' INTO ev_message.
      RETURN.
    ENDIF.

*    CALL METHOD cl_gui_frontend_services=>file_exist
*      EXPORTING
*        file                 = iv_path
*      RECEIVING
*        result               = lv_result
*      EXCEPTIONS
*        cntl_error           = 1
*        error_no_gui         = 2
*        wrong_parameter      = 3
*        not_supported_by_gui = 4
*        OTHERS               = 5.
*    IF sy-subrc <> 0 OR lv_result = abap_true.
*      MESSAGE e001(00) WITH '文件已存在' INTO ev_message.
*      RETURN.
*    ENDIF.

    ev_valid = abap_true.
  ENDMETHOD.


  METHOD convert_to_camel_case.
    DATA: lt_table TYPE TABLE OF string,
          ls_table LIKE LINE OF lt_table,
          lv_out   TYPE string,
          lv_begin TYPE c.

    SPLIT iv_value AT '_' INTO TABLE lt_table.

    LOOP AT lt_table INTO ls_table.
      TRANSLATE ls_table TO LOWER CASE.

      lv_begin = ls_table+0(1).
      IF lv_out IS NOT INITIAL.
        TRANSLATE lv_begin TO UPPER CASE.
      ENDIF.

      CONCATENATE lv_out lv_begin INTO lv_out RESPECTING BLANKS.
      SHIFT ls_table LEFT BY 1 PLACES.
      CONCATENATE lv_out ls_table INTO lv_out.
    ENDLOOP.

    rv_value = lv_out.

  ENDMETHOD.


  METHOD fill_table2xlsx.
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

    GET REFERENCE OF iv_data INTO ls_table.
    ASSIGN ls_table->* TO <lt_table>.

    lo_tab ?= cl_abap_tabledescr=>describe_by_data_ref( ls_table ).
    lo_tab_struc ?= lo_tab->get_table_line_type( ).
    DATA(lt_fields) = lo_tab_struc->components.

    " 切换Sheet页
    GET PROPERTY OF gs_excel 'SHEETS' = gs_sheet
      EXPORTING
        #1 = iv_sheet_index.

    CALL METHOD OF gs_sheet 'ACTIVATE'.

    " 设置sheet名称
    IF iv_sheet_name IS NOT INITIAL.
      SET PROPERTY OF gs_sheet 'NAME' = iv_sheet_name.
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
    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).
      CLEAR ls_char.
      DO lv_lines TIMES.
        lv_column = sy-index.

        " 排除嵌套结构
        READ TABLE lt_EXCLUDE WITH KEY table_line = lv_column TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.

        ASSIGN COMPONENT lv_column OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<lv_value>).
        CHECK <lv_value> IS ASSIGNED.

        CONCATENATE ls_char-line <lv_value>
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

  ENDMETHOD.


  METHOD serialize.
    DATA: ls_data TYPE REF TO data,
          lv_json TYPE string,
          lv_type TYPE c,
          lo_type TYPE REF TO cl_abap_typedescr.

    GET REFERENCE OF iv_data INTO ls_data.

    lo_type ?= cl_abap_typedescr=>describe_by_data_ref( ls_data ).
    lv_type = lo_type->kind.

    IF lv_type = 'S'.
      serialize_struc( EXPORTING iv_data    = iv_data
                                 it_convert = it_convert
                       IMPORTING ev_json    = lv_json ).
    ELSEIF lv_type = 'T'.
      serialize_table( EXPORTING iv_data    = iv_data
                                 it_convert = it_convert
                       IMPORTING ev_json    = lv_json ).
    ENDIF.

    ev_json = lv_json.

  ENDMETHOD.


  METHOD serialize_struc.
    DATA: ls_data      TYPE REF TO data,
          lv_value     TYPE string,
          lv_json      TYPE string,
          lv_json_node TYPE string.

    DATA: lo_struc TYPE REF TO cl_abap_structdescr,
          lo_type  TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <ls_data> TYPE any.

    GET REFERENCE OF iv_data INTO ls_data.
    ASSIGN ls_data->* TO <ls_data>.

    lo_struc ?= cl_abap_structdescr=>describe_by_data_ref( ls_data ).
    DATA(lt_nodes) = lo_struc->components.

    LOOP AT lt_nodes INTO DATA(ls_nodes).
      CLEAR lv_json_node.

      DATA(lv_node_name) = ls_nodes-name.
      lv_node_name = convert_to_camel_case( lv_node_name ).

      ASSIGN COMPONENT ls_nodes-name OF STRUCTURE <ls_data> TO FIELD-SYMBOL(<lv_node>).
      CHECK <lv_node> IS ASSIGNED.


      lo_type ?= cl_abap_typedescr=>describe_by_data( <lv_node> ).
      IF lo_type->kind = 'T'.
        serialize_table( EXPORTING iv_data    = <lv_node>
                                   iv_name    = lv_node_name
                                   it_convert = it_convert
                         IMPORTING ev_json    = lv_json_node ).

      ELSEIF lo_type->kind = 'S'.
        serialize_struc( EXPORTING iv_data    = <lv_node>
                                   iv_name    = lv_node_name
                                   it_convert = it_convert
                         IMPORTING ev_json    = lv_json_node ).
      ELSE.
        lv_value = <lv_node>.

        READ TABLE it_convert TRANSPORTING NO FIELDS
                              WITH KEY table = iv_name
                                       field = lv_node_name.
        IF sy-subrc = 0.
          lv_value = escape( val    = lv_value
                             format = cl_abap_format=>e_url_full ).
        ENDIF.

        lv_json_node = '"' && lv_node_name && '":"' && lv_value &&  '"'.
      ENDIF.

      IF lv_json IS INITIAL.
        lv_json = lv_json_node.
      ELSE.
        lv_json = lv_json && ',' && lv_json_node.
      ENDIF.
    ENDLOOP.

    ev_json = '{' && lv_json && '}'.

    IF iv_name IS NOT INITIAL.
      ev_json = '"' && iv_name && '":' && ev_json.
    ENDIF.
  ENDMETHOD.


  METHOD serialize_table.
    DATA: ls_table     TYPE REF TO data,
          lv_json_node TYPE string,
          lv_json_tab  TYPE string,
          lv_json_line TYPE string.

    FIELD-SYMBOLS: <lt_table> TYPE ANY TABLE.

    GET REFERENCE OF iv_data INTO ls_table.
    ASSIGN ls_table->* TO <lt_table>.

    CLEAR lv_json_tab.
    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).
      CLEAR: lv_json_line.

      serialize_struc( EXPORTING iv_data    = <ls_table>
                                 it_convert = it_convert
                       IMPORTING ev_json    = lv_json_line ).

      IF lv_json_line IS NOT INITIAL.
        IF lv_json_tab IS INITIAL.
          lv_json_tab = lv_json_line.
        ELSE.
          lv_json_tab = lv_json_tab && ',' && lv_json_line.
        ENDIF.
      ENDIF.
    ENDLOOP.

    ev_json = '[' && lv_json_tab && ']'.

    IF iv_name IS NOT INITIAL.
      ev_json = '"' && iv_name && '":' && ev_json.
    ENDIF.
  ENDMETHOD.


  METHOD set_data.

    DATA lr_temp TYPE REF TO data.

    FIELD-SYMBOLS: <ls_data_temp> TYPE any,
                   <ls_data>      TYPE any.

    GET REFERENCE OF iv_data INTO lr_temp.
    ASSIGN lr_temp->* TO <ls_data_temp>.
    CREATE DATA go_data LIKE <ls_data_temp>.
    ASSIGN me->go_data->* TO <ls_data>.
    <ls_data> = <ls_data_temp>.

  ENDMETHOD.


  METHOD set_table_key.
    DATA: ls_table      TYPE REF TO data,
          lv_json_tab   TYPE string,
          lv_json_line  TYPE string,
          lv_key        TYPE string,
          lv_parent_key TYPE string.

    DATA: lo_tab       TYPE REF TO cl_abap_tabledescr,
          lo_tab_struc TYPE REF TO cl_abap_structdescr,
          lo_type      TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <lt_table> TYPE ANY TABLE.

    GET REFERENCE OF cv_data INTO ls_table.
    ASSIGN ls_table->* TO <lt_table>.

    lo_tab ?= cl_abap_tabledescr=>describe_by_data_ref( ls_table ).
    lo_tab_struc ?= lo_tab->get_table_line_type( ).
    DATA(lt_fields) = lo_tab_struc->components.

    CLEAR lv_json_tab.
    LOOP AT <lt_table> ASSIGNING FIELD-SYMBOL(<ls_table>).
      CLEAR: lv_json_line, lv_key, lv_parent_key.

      LOOP AT lt_fields INTO DATA(ls_fields).
        ASSIGN COMPONENT ls_fields-name OF STRUCTURE <ls_table> TO FIELD-SYMBOL(<lv_value>).
        CHECK <lv_value> IS ASSIGNED.


        READ TABLE it_parent_keys INTO DATA(ls_parent_keys)
                                  WITH KEY table = iv_table_name
                                           field = ls_fields-name.
        IF sy-subrc = 0.
          lv_parent_key = lv_parent_key && <lv_value>.
        ENDIF.

        READ TABLE it_keys INTO DATA(ls_keys)
                           WITH KEY table = iv_table_name
                                    field = ls_fields-name.
        IF sy-subrc = 0.
          lv_key = lv_key && <lv_value>.
        ENDIF.
      ENDLOOP.

      IF gv_key_fname IS NOT INITIAL.
        ASSIGN COMPONENT gv_key_fname OF STRUCTURE <ls_table> TO <lv_value>.
        IF sy-subrc = 0.
          <lv_value> = lv_key.
        ENDIF.
      ENDIF.

      IF gv_parent_key_fname IS NOT INITIAL.
        ASSIGN COMPONENT gv_parent_key_fname OF STRUCTURE <ls_table> TO <lv_value>.
        IF sy-subrc = 0.
          <lv_value> = lv_parent_key.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
