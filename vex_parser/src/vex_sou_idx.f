      FUNCTION   VEX_SOU_IDX ( VEX, SOU_NAME )
! **********************************************************************
! *                                                                    *
! *   Auxiliary function VEX_SOU_IDX finds the index of the source     *
! *   SOU_NAME (using the IAU name) in internal data structure of      * 
! *   VEX.                                                             *
! *   If SOU_NAME is not found, it returns 0.                          *
! *                                                                    *
! *   INPUT:                                                           *
! *          VEX       =  VLBI Experiment File Object   { VEX }        *
! *                                                                    *
! *          SOU_NAME  =  Source name                  { CHAR }        *
! *                                                                    *
! *   OUTPUT:                                                          *
! *          <VEX_SOU_IDX>  = Source index             { INT }         *
! *                           Index of source in the internal VEX      *
! *                           structure.                               *
! *                                                                    *
! * ### 06-NOV-2020   VEX_SOU_IDX  v1.0 (c)  N. Habana 06-NOV-2020 ### *
! *                                                                    *
! **********************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vex.i'
      TYPE     ( VEX_TYPE ) :: VEX
      CHARACTER  SOU_NAME*(*)
      INTEGER*4  VEX_SOU_IDX
      INTEGER*4  J1
!
      DO 410 J1 = 1, VEX%N_SOU
         IF ( VEX%SOU(J1)%IAU_NAME .EQ. SOU_NAME ) THEN
            VEX_SOU_IDX  =  J1
            RETURN 
         END IF
 410  CONTINUE 
!
      VEX_SOU_IDX  =  0
!
      RETURN
      END  FUNCTION   VEX_SOU_IDX  

