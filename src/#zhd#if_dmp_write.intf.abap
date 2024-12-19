interface /ZHD/IF_DMP_WRITE
  public .


  methods WRITE
    importing
      !IS_LINE_DATA type /ZHD/S_DMP_DATA_LIST
    exporting
      !EV_STATUS type BAPI_MTYPE
      !ET_MESSAGE type BAPIRET2_TAB .
endinterface.
