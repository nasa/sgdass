      FUNCTION   STP_STA_IDX ( STP, STA_NAME )
! **********************************************************************
! *                                                                    *
! *   Auxiliary function STP_STA_IDX finds the index of the station    *
! *   STA_NAME in internal data structure of STP.                      * 
! *   If STA_NAME is not found, it returns 0.                          *
! *                                                                    *
! *   INPUT:                                                           *
! *          STP       =  STP File Object               { STP }        *
! *                                                                    *
! *          STA_NAME  =  Station name                  { CHAR }       *
! *                                                                    *
! *   OUTPUT:                                                          *
! *          <STP_STA_IDX>  = Station index             { INT }        *
! *                           Index of station in the internal STP     *
! *                           structure.                               *
! *                                                                    *
! * ### 03-DEC-2020   STP_STA_IDX  v1.0 (c)  N. Habana 03-DEC-2020 ### *
! *                                                                    *
! **********************************************************************
      IMPLICIT   NONE 
      INCLUDE   'stp.i'
      TYPE     ( STP__TYPE ) :: STP
      CHARACTER  STA_NAME*(*)
      INTEGER*4  STP_STA_IDX
      INTEGER*4  J1
!
      DO 410 J1 = 1, STP%NSTA
         IF ( STP%STA(J1)%NAME .EQ. STA_NAME ) THEN
            STP_STA_IDX  =  J1
            RETURN 
         END IF
 410  CONTINUE 
!
      STP_STA_IDX  =  0
!
      RETURN
      END  FUNCTION   STP_STA_IDX  !#!
!
! ---------------------------------------------------------------------
!
      FUNCTION   STP_STA_ID_IDX ( STP, STA_ID )
! *************************************************************************
! *                                                                       *
! *   Auxiliary function STP_STA_ID_IDX finds the index of the            *
! *   station SHORT NAME in internal data structure of VEX.               * 
! *   If STA_ID is not found, it returns 0.                               *
! *                                                                       *
! *   INPUT:                                                              *
! *          STP     =  STP File Object                 { VEX }           *
! *                                                                       *
! *          STA_ID  =  Station ID                      { CHAR }          *
! *                                                                       *
! *   OUTPUT:                                                             *
! *          <STP_STA_ID_IDX>  = Station ID index       { INT }           *
! *                              Index of station in the internal STP     *
! *                              structure.                               *
! *                                                                       *
! * ### 03-DEC-2020   STP_STA_ID_IDX  v1.0 (c)  N. Habana 03-DEC-2020 ### *
! *                                                                       *
! *************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'stp.i'
      TYPE     ( STP__TYPE ) :: STP
      CHARACTER  STA_ID*(*)
      INTEGER*4  STP_STA_ID_IDX
      INTEGER*4  J1
!
      DO 410 J1 = 1, STP%NSTA
         IF ( STP%STA(J1)%SHORT_NAME .EQ. STA_ID ) THEN
            STP_STA_ID_IDX  =  J1
            RETURN 
         END IF
 410  CONTINUE 
!
      STP_STA_ID_IDX = 0
!
      RETURN
      END  FUNCTION   STP_STA_ID_IDX !#!#!

