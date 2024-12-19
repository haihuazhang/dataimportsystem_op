*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: /ZHD/T_DMP_DATAT................................*
DATA:  BEGIN OF STATUS_/ZHD/T_DMP_DATAT              .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_/ZHD/T_DMP_DATAT              .
CONTROLS: TCTRL_/ZHD/T_DMP_DATAT
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: */ZHD/T_DMP_DATAT              .
TABLES: /ZHD/T_DMP_DATAT               .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
