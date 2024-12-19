*---------------------------------------------------------------------*
*    program for:   TABLEFRAME_/ZHD/T_DMP_DATAT
*---------------------------------------------------------------------*
FUNCTION TABLEFRAME_/ZHD/T_DMP_DATAT   .

  PERFORM TABLEFRAME TABLES X_HEADER X_NAMTAB DBA_SELLIST DPL_SELLIST
                            EXCL_CUA_FUNCT
                     USING  CORR_NUMBER VIEW_ACTION VIEW_NAME.

ENDFUNCTION.
