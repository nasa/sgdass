      FUNCTION   VTD_PH_DEL_STRUC ( BX, BY, FREQ, MAP, IUER )
! ************************************************************************
! *                                                                      *
! *   Function  VTD_PH_DEL_STRUC 
! *                                                                      *
! * ### 19-MAR-2007 VTD_PH_DEL_STRUC v1.0 (c) L. Petrov  19-MAR-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      REAL*8     BX, BY, FREQ
      TYPE     ( SOUMAP__TYPE ) :: MAP
      REAL*8     VTD_PH_DEL_STRUC 
      INTEGER*4  IUER
      REAL*8     DRE, DIM, DRE_DER, DIM_DER, XI, GS, BP, VTD__C
      PARAMETER  ( VTD__C = 299792458.0D0 )
      INTEGER*4  J1
      REAL*8,    EXTERNAL :: ATAN_CS 
!
      VTD_PH_DEL_STRUC = 0.0D0
      IF ( MAP%STATUS_CC .NE. SMP__LOAD ) THEN
           CALL ERR_LOG ( 2631, IUER, 'VTD_PH_DEL_STRUC', 'Trap of internal '// &
     &         'control: clean components were not loaded for image '// &
     &          MAP%FINAM )
           RETURN 
      END IF
!
      DRE = 0.0D0
      DIM = 0.0D0
!
      DO 410 J1=1,MAP%NUM_CC
         BP = ( BX*MAP%COOR_CC(1,J1) + BY*MAP%COOR_CC(2,J1) )
         DRE = DRE + MAP%FLUX_CC(J1) * DCOS ( PI2*FREQ/VTD__C * BP )
         DIM = DIM + MAP%FLUX_CC(J1) * DSIN ( PI2*FREQ/VTD__C * BP )
 410  CONTINUE 
      VTD_PH_DEL_STRUC = ATAN_CS ( DRE, DIM )
      IF ( VTD_PH_DEL_STRUC > PI__NUM ) VTD_PH_DEL_STRUC = VTD_PH_DEL_STRUC - PI2
      VTD_PH_DEL_STRUC = 1.0D0/(PI2*FREQ) * VTD_PH_DEL_STRUC 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION   VTD_PH_DEL_STRUC  !#!#
