*&---------------------------------------------------------------------*
*& 包含               /ZHD/DMP_0001_SCR
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& SELECT SCREEN/选择屏幕                                              *
*&---------------------------------------------------------------------*
* WHERE条件

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
* 数据类型
  PARAMETERS:p_type TYPE char100 OBLIGATORY.
*  SELECTION-SCREEN BEGIN OF LINE.
*    SELECTION-SCREEN PUSHBUTTON 1(12) TEXT-005 USER-COMMAND where.
*  SELECTION-SCREEN END OF LINE.
  PARAMETERS:p_where TYPE string OBLIGATORY.
  SELECTION-SCREEN BEGIN OF LINE.
* 推送数据
    SELECTION-SCREEN POSITION 1.
    PARAMETERS:rb_input TYPE c RADIOBUTTON GROUP g1 USER-COMMAND input DEFAULT 'X'.
    SELECTION-SCREEN COMMENT 2(15) TEXT-003 FOR FIELD rb_input.
* 下载数据
    SELECTION-SCREEN POSITION 25.
    PARAMETERS:rb_down   TYPE c RADIOBUTTON GROUP g1.
    SELECTION-SCREEN COMMENT 26(15) TEXT-004 FOR FIELD rb_down.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b1.
* 推送数据屏幕
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
* 分页大小
  PARAMETERS:p_pages TYPE numc5 MODIF ID i.
  PARAMETERS:c_start TYPE c RADIOBUTTON GROUP g2 DEFAULT 'X' MODIF ID i,
             c_test  TYPE c RADIOBUTTON GROUP g2 MODIF ID i.
SELECTION-SCREEN END OF BLOCK b2.
* 下载数据屏幕
SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004.
* 下载路径
  PARAMETERS: p_file TYPE string MODIF ID d.
SELECTION-SCREEN END OF BLOCK b3.
*&---------------------------------------------------------------------*
*& SEARCH HELP/搜索帮助                                                *
*&---------------------------------------------------------------------*
* 数据类型搜索帮助
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_type.
  PERFORM frm_datatype_f4_help.
* 路径搜索帮助
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM frm_file_f4_help.
*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN OUTPUT/选择屏幕输出                             *
*&---------------------------------------------------------------------*
* 推送数据/下载数据界面显示
AT SELECTION-SCREEN OUTPUT.
  PERFORM frm_set_attribute.
*&---------------------------------------------------------------------*
*& INITIALIZATION/选择屏幕前初始化                                     *
*&---------------------------------------------------------------------*
INITIALIZATION.
*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN/选择屏幕开始                                    *
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  IF rb_input = 'X'.
* 查询数据
    PERFORM frm_search_data.
* 数据并发处理/测试推送
*    IF c_start = 'X' AND c_test = ''.
*
*    ELSEIF c_start = '' AND c_test = 'X'.
*
*    ELSEIF c_start = 'X' AND c_test = 'X'.
*
*    ELSEIF c_start = '' AND c_test = ''.
*
*    ENDIF.
    IF c_start = 'X'.
* 并发处理
      PERFORM frm_batch_push.
    ELSEIF c_test = 'X'.
* 测试推送
      PERFORM frm_test_push.
    ENDIF.
  ELSE.
    IF p_file IS NOT INITIAL.
* 查询数据
      PERFORM frm_search_data.
* 下载数据到EXCEL
      PERFORM frm_down_excel.
    ENDIF.
  ENDIF.
