interface /ZHD/IF_DMP_SRC_DATA
  public .


  data GV_STATUS type STRING read-only .
  data GV_MESSAGE type STRING read-only .
  data GT_MESSAGE type BAPIRET2_TAB read-only .

  methods GET_DATA
    importing
      !IV_WHERE_CONDITION type STRING
    exporting
      !EV_STATUS type CHAR01
      !ET_MESSAGE type BAPIRET2_TAB
      !ET_LIST type /ZHD/TT_DMP_DATA_LIST .
  methods ABAP2XLSX
    importing
      !IV_PATH type STRING
    exceptions
      INVALID_PATH .
endinterface.
