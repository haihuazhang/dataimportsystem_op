FUNCTION /zhd/fm_dmp_send_data.
*"----------------------------------------------------------------------
*"*"本地接口：
*"  IMPORTING
*"     VALUE(IT_DATA) TYPE  /ZHD/TT_DMP_DATA_LIST
*"     REFERENCE(IV_UUID) TYPE  STRING OPTIONAL
*"----------------------------------------------------------------------

  DATA: lv_status  TYPE char1,
        lt_message TYPE bapiret2_tab.
  DATA: lo_dmp_send_data TYPE REF TO /zhd/cl_dmp_send_data.
  DATA: lo_sever TYPE REF TO if_http_server.

  CREATE OBJECT lo_dmp_send_data.

* 检查当前可用并发空间

* 推送全部数据
  lo_dmp_send_data->uuid = iv_uuid.
  lo_dmp_send_data->it_data = it_data.
  lo_dmp_send_data->send_orign_data_to_dmp( it_data ).
ENDFUNCTION.
