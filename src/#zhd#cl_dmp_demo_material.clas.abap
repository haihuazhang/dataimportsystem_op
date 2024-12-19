class /ZHD/CL_DMP_DEMO_MATERIAL definition
  public
  inheriting from /ZHD/CL_DMP_SRC_DATA
  final
  create public .

public section.

  methods /ZHD/IF_DMP_SRC_DATA~GET_DATA
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS /ZHD/CL_DMP_DEMO_MATERIAL IMPLEMENTATION.


  METHOD /zhd/if_dmp_src_data~get_data.

    DATA:BEGIN OF ls_mara,
           key        TYPE string,
           parent_key TYPE string,
           matnr      TYPE mara-matnr,
           mtart      TYPE mara-mtart,
           meins      TYPE mara-meins,
         END OF ls_mara.

    DATA:BEGIN OF ls_makt,
           key        TYPE string,
           parent_key TYPE string,
           spras      TYPE makt-spras,
           maktx      TYPE makt-maktx,
         END OF ls_makt.

    DATA:BEGIN OF ls_marc,
           key        TYPE string,
           parent_key TYPE string,
           werks      TYPE marc-werks,
           xchar      TYPE marc-xchar,
         END OF ls_marc.

    DATA:BEGIN OF ls_mard,
           key        TYPE string,
           parent_key TYPE string,
           werks      TYPE mard-werks,
           lgort      TYPE mard-lgort,
           labst      TYPE mard-labst,
         END OF ls_mard.

    DATA:lt_mara LIKE TABLE OF ls_mara,
         lt_makt LIKE TABLE OF ls_makt,
         lt_marc LIKE TABLE OF ls_marc,
         lt_mard LIKE TABLE OF ls_mard.

    DATA:BEGIN OF ls_json,
           mara LIKE lt_mara,
           makt LIKE lt_makt,
           marc LIKE lt_marc,
           mard LIKE lt_mard,
         END OF ls_json.

    DATA:BEGIN OF ls_makt_1,
           key        TYPE string,
           parent_key TYPE string,
           matnr      TYPE mara-matnr,

           spras      TYPE makt-spras,
           maktx      TYPE makt-maktx,
         END OF ls_makt_1.

    DATA:BEGIN OF ls_marc_1,
           key        TYPE string,
           parent_key TYPE string,
           matnr      TYPE mara-matnr,

           werks      TYPE marc-werks,
           xchar      TYPE marc-xchar,
         END OF ls_marc_1.

    DATA:BEGIN OF ls_mard_1,
           key        TYPE string,
           parent_key TYPE string,
           matnr      TYPE mara-matnr,

           werks      TYPE mard-werks,
           lgort      TYPE mard-lgort,
           labst      TYPE mard-labst,
         END OF ls_mard_1.
    DATA:lt_makt_1 LIKE TABLE OF ls_makt_1,
         lt_marc_1 LIKE TABLE OF ls_marc_1,
         lt_mard_1 LIKE TABLE OF ls_mard_1.

    TYPES:
      BEGIN OF ty_fields,
        table TYPE char20,
        field TYPE char20,
      END OF ty_fields .

    DATA:it_key         TYPE TABLE OF  ty_fields,
         is_key         TYPE ty_fields,
         it_parent_keys TYPE TABLE OF  ty_fields,
         is_parent_keys TYPE ty_fields.

    DATA:lv_mara TYPE char4 VALUE 'MARA'.
    DATA:lv_makt TYPE char4 VALUE 'MAKT'.
    DATA:lv_marc TYPE char4 VALUE 'MARC'.
    DATA:lv_mard TYPE char4 VALUE 'MARD'.

    DATA:lv_line TYPE i.
    DATA:ls_msg TYPE bapiret2.
    DATA:lv_where TYPE string.
    DATA:lt_where_tab TYPE TABLE OF string,
         ls_where_tab TYPE string.
    DATA:es_list TYPE /zhd/s_dmp_data_list.
****检查WHERE条件
    IF iv_where_condition IS  INITIAL.
      ev_status = 'E'.
      ls_msg-type = 'E'.
      ls_msg-message = '请输入取数条件'.
      APPEND ls_msg TO et_message.
      CLEAR ls_msg.
      RETURN.
    ELSE.
      ls_where_tab = iv_where_condition.
      APPEND ls_where_tab TO lt_where_tab.

    ENDIF.
* 主表取数
    SELECT
      matnr,
      mtart,
      meins
      FROM mara
      INTO CORRESPONDING FIELDS OF TABLE @lt_mara
      WHERE (lt_where_tab)
      .
    IF sy-subrc = 0 .
      CLEAR :it_key, it_parent_keys.
* 添加key值
      is_key-table = lv_mara.
      is_key-field = 'MATNR'.             "字段名称
      APPEND is_key TO it_key.

*    添加KEY字段
      set_table_key( EXPORTING iv_table_name = lv_mara
                               it_keys = it_key
                               it_parent_keys = it_parent_keys
                CHANGING cv_data = lt_mara ).

      CLEAR :it_key, it_parent_keys.

      IF lt_mara IS NOT INITIAL.
*    其他内表获取数据(MAKT)
        SELECT
          matnr,
          spras,
          maktx
          FROM makt
          INTO CORRESPONDING FIELDS OF TABLE @lt_makt_1
          FOR ALL ENTRIES IN @lt_mara
          WHERE matnr = @lt_mara-matnr.
        IF sy-subrc = 0.

*    其他内表添加KRY字段(MAKT)
          is_key-table = lv_makt.
          is_key-field = 'MATNR'.             "字段名称
          APPEND is_key TO it_key.

          is_key-table = lv_makt.
          is_key-field = 'SPRAS'.             "字段名称
          APPEND is_key TO it_key.

          is_parent_keys-table = lv_makt.
          is_parent_keys-field = 'MATNR'.             "字段名称
          APPEND is_parent_keys TO it_parent_keys.
*    添加KEY字段
          set_table_key( EXPORTING iv_table_name = lv_makt
                                   it_keys = it_key
                                   it_parent_keys = it_parent_keys
                    CHANGING cv_data = lt_makt_1 ).
*      最终内表数据添加
          LOOP AT lt_makt_1 INTO ls_makt_1.
            MOVE-CORRESPONDING ls_makt_1 TO ls_makt.
            APPEND ls_makt TO lt_makt.
          ENDLOOP.

          CLEAR :it_key, it_parent_keys.

*    其他内表获取数据(MARC)
          SELECT
            matnr,
            werks,
            xchar
            FROM marc
            INTO CORRESPONDING FIELDS OF TABLE @lt_marc_1
            FOR ALL ENTRIES IN @lt_mara
            WHERE matnr = @lt_mara-matnr.
          IF sy-subrc = 0.
*    其他内表添加KRY字段(MARC)

            is_key-table = lv_marc.
            is_key-field = 'MATNR'.             "字段名称
            APPEND is_key TO it_key.

            is_key-table = lv_marc.
            is_key-field = 'WERKS'.             "字段名称
            APPEND is_key TO it_key.

            is_parent_keys-table = lv_marc.
            is_parent_keys-field = 'MATNR'.             "字段名称
            APPEND is_parent_keys TO it_parent_keys.
*    添加KEY字段
            set_table_key( EXPORTING iv_table_name = lv_marc
                                     it_keys = it_key
                                     it_parent_keys = it_parent_keys
                      CHANGING cv_data = lt_marc_1 ).
*      最终内表数据添加
            LOOP AT lt_marc_1 INTO ls_marc_1.
              MOVE-CORRESPONDING ls_marc_1 TO ls_marc.
              APPEND ls_marc TO lt_marc.
            ENDLOOP.

            CLEAR :it_key, it_parent_keys.
          ENDIF.

*    其他内表获取数据(MARD)
          SELECT
            matnr,
            werks,
            lgort,
            labst
            FROM mard
            INTO CORRESPONDING FIELDS OF TABLE @lt_mard_1
            FOR ALL ENTRIES IN @lt_mara
            WHERE matnr = @lt_mara-matnr.
          IF sy-subrc = 0.
*    其他内表添加KRY字段(MARD)
            is_key-table = lv_mard.
            is_key-field = 'MATNR'.             "字段名称
            APPEND is_key TO it_key.

            is_key-table = lv_mard.
            is_key-field = 'WERKS'.             "字段名称
            APPEND is_key TO it_key.

            is_key-table = lv_mard.
            is_key-field = 'LGORT'.             "字段名称
            APPEND is_key TO it_key.

            is_parent_keys-table = lv_mard.
            is_parent_keys-field = 'MATNR'.             "字段名称
            APPEND is_parent_keys TO it_parent_keys.

            is_parent_keys-table = lv_mard.
            is_parent_keys-field = 'WERKS'.             "字段名称
            APPEND is_parent_keys TO it_parent_keys.

*    添加KEY字段
            set_table_key( EXPORTING iv_table_name = lv_mard
                                     it_keys = it_key
                                     it_parent_keys = it_parent_keys
                      CHANGING cv_data = lt_mard_1 ).
*      最终内表数据添加
            LOOP AT lt_mard_1 INTO ls_mard_1.
              MOVE-CORRESPONDING ls_mard_1 TO ls_mard.
              APPEND ls_mard TO lt_mard.
            ENDLOOP.
            CLEAR :it_key, it_parent_keys.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    CLEAR:ls_mara,lv_line.
    lv_line = 0.
    LOOP AT lt_mara INTO ls_mara.
*      es_list-line = lv_line + 1.
      es_list-line = lv_line.
      es_list-uuid = cl_system_uuid=>create_uuid_x16_static( ).
      APPEND ls_mara TO ls_json-mara.
      LOOP AT lt_makt INTO ls_makt WHERE parent_key = ls_mara-key.
        APPEND ls_makt TO ls_json-makt.
      ENDLOOP.
      LOOP AT lt_marc INTO ls_marc WHERE parent_key = ls_mara-key.
        APPEND ls_marc TO ls_json-marc.
        LOOP AT lt_mard INTO ls_mard WHERE parent_key = ls_marc-key.
          APPEND ls_mard TO ls_json-mard.
        ENDLOOP.
      ENDLOOP.
*    最终内表转json
      CALL METHOD serialize
        EXPORTING
          iv_data = ls_json
        IMPORTING
          ev_json = es_list-data_json.
      APPEND es_list TO et_list  .
      lv_line += 1.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
