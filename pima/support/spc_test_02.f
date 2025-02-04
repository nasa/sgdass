      PROGRAM    SPC_TEST_02
      INCLUDE   'spd.i'
      TYPE     ( SPD_DEL__TYPE ) :: SPD_DEL
      INTEGER*4  M
      PARAMETER  ( M = 8 )
      INTEGER*4  L, LIS(M), IUER
      CHARACTER  DATE_NOW*19
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ADD_LIS
!
      CALL SPD_DEL_INIT ( SPD_DEL, IUER )
      DATE_NOW = GET_CDATE()
      L = 0
      IS = ADD_LIS ( M, L, LIS, 1, IUER )
      IS = ADD_LIS ( M, L, LIS, 2, IUER )
      CALL ERR_LOG ( 0, IUER )
!
      END  !#!  
