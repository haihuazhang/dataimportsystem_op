*&---------------------------------------------------------------------*
*& 包含               /ZHD/DMP_0001_TOP
*&---------------------------------------------------------------------*
TABLES:sscrfields.

DATA: gt_list    TYPE /zhd/tt_dmp_data_list,
      gt_message TYPE bapiret2_tab,
      gv_json TYPE string.

DATA:go_data  TYPE REF TO  data,
     gs_excel TYPE  ole2_object,
     gs_wbook TYPE  ole2_object,
     gs_sheet TYPE  ole2_object.

*-----------------------------------------------------------------------*
*DESC:异步取数变量定义
*-----------------------------------------------------------------------*
data: g_taskname(12) type c, "task name（同时运行的任务名称必须保持唯一）
      g_classname    type rzlli_apcl,   "Server Group Name
      g_applserver   type rzllitab-applserver."RFC Serve Group

*data: snd_jobs type i,
*      rcv_jobs type i,
*      functioncall1(1) type c.

DATA:snd_jobs TYPE i,
     rcv_jobs TYPE i,
     fcall(1) TYPE c.
CONSTANTS: done(1) TYPE c VALUE 'X'.
DATA:p_wp TYPE c VALUE 5.
