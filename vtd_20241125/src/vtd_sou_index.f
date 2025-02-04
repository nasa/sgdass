      FUNCTION   VTD_SOU_INDEX ( VTD, SOU_NAME )
! ************************************************************************
! *                                                                      *
! *   Auxiliary function VTD_SOU_INDEX finds the index of the source     *
! *   SOU_NAME in internal data structure of VTD. If it does not find    *
! *   the source name, it returns 0.                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  SOU_NAME ( CHARACTER ) -- Source name.                              *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! * <VTD_SOU_INDEX> ( INTEGER*4 ) -- Index of the source in the source   *
! *                                  list stored in the internal fields  *
! *                                  of the objkect VTD.                 *
! *                                                                      *
! * ### 09-MAR-2004   VTD_SOU_INDEX  v1.0 (c)  L. Petrov 09-MAR-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  SOU_NAME*(*)
      INTEGER*4  VTD_SOU_INDEX
      INTEGER*4  J1
!
      DO 410 J1=1,VTD%L_SOU
         IF ( VTD%SOU(J1)%IVS_NAME .EQ. SOU_NAME ) THEN
              VTD_SOU_INDEX = J1
              RETURN 
         END IF
 410  CONTINUE 
!
      VTD_SOU_INDEX = 0
      RETURN
      END  FUNCTION   VTD_SOU_INDEX  
