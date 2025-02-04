      FUNCTION   VTD_STA_INDEX ( VTD, STA_NAME )
! ************************************************************************
! *                                                                      *
! *   Auxiliary function VTD_STA_INDEX finds the index of the station    *
! *   STA_NAME in internal data structure of VTD. If it does not find    *
! *   the station name, it returns 0.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  STA_NAME ( CHARACTER ) -- Station name.                             *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <VTD_STA_INDEX> ( INTEGER*4 ) -- Index of the station in the         *
! *                                  internal VTD structure.             *
! *                                                                      *
! * ### 09-MAR-2004   VTD_STA_INDEX  v1.0 (c)  L. Petrov 09-MAR-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  STA_NAME*(*)
      INTEGER*4  VTD_STA_INDEX
      INTEGER*4  J1
!
      DO 410 J1=1,VTD%L_STA
         IF ( VTD%STA(J1)%IVS_NAME .EQ. STA_NAME ) THEN
              VTD_STA_INDEX = J1
              RETURN 
         END IF
 410  CONTINUE 
!
      VTD_STA_INDEX = 0
      RETURN
      END  FUNCTION   VTD_STA_INDEX  
