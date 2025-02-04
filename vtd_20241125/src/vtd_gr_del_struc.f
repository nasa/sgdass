      FUNCTION   VTD_GR_DEL_STRUC ( BX, BY, FREQ, MAP, IUER )
! ************************************************************************
! *                                                                      *
! *   Function  VTD_GR_DEL_STRUC 
! *                                                                      *
! *  ### 19-MAR-2007 VTD_GR_DEL_STRUC v1.0 (c) L. Petrov 19-MAR-2007 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      REAL*8     BX, BY, FREQ
      TYPE     ( SOUMAP__TYPE ) :: MAP
      REAL*8     VTD_GR_DEL_STRUC 
      INTEGER*4  IUER
      REAL*8     DRE, DIM, DRE_DER, DIM_DER, AMP, BP, VTD__C
      REAL*8     DRE1, DIM1, DRE0, DIM0, D1, GS1, FLUX_MAX, FLUX_SUM, BETA
      PARAMETER  ( VTD__C = 299792458.0D0 )
      LOGICAL*1  FL_TEST
      INTEGER*4  K1, J1, J2, J3, IER
!
      FL_TEST = .FALSE.
!
      VTD_GR_DEL_STRUC = 0.0D0
      IF ( MAP%STATUS_CC .NE. SMP__LOAD ) THEN
           CALL ERR_LOG ( 2621, IUER, 'VTD_GRDEL_STRUC', 'Trap of internal '// &
     &         'control: clean components were not loaded for image '// &
     &          MAP%FINAM )
           RETURN 
      END IF
!
      DRE = 0.0D0
      DIM = 0.0D0
      DRE_DER = 0.0D0
      DIM_DER = 0.0D0
!
      FLUX_MAX = -1.0D0
      DO 410 J1=1,MAP%NUM_CC
         IF ( MAP%FLUX_CC(J1) > FLUX_MAX ) FLUX_MAX = MAP%FLUX_CC(J1) 
 410  CONTINUE 
!
      FLUX_SUM = 0.0D0
!!      do 420 j2=6,map%num_cc
      DO 420 J2=1,MAP%NUM_CC
         IF ( ABS(MAP%FLUX_CC(J2)) < 0.1D0*MAP%NOISE ) GOTO 420
         BP = ( BX*MAP%COOR_CC(1,J2) + BY*MAP%COOR_CC(2,J2) )
         DRE = DRE + MAP%FLUX_CC(J2) * DCOS ( PI2*FREQ/VTD__C * BP )
         DIM = DIM + MAP%FLUX_CC(J2) * DSIN ( PI2*FREQ/VTD__C * BP )
         DRE_DER = DRE_DER - MAP%FLUX_CC(J2)/VTD__C * BP * DSIN ( PI2*FREQ/VTD__C * BP )
         DIM_DER = DIM_DER + MAP%FLUX_CC(J2)/VTD__C * BP * DCOS ( PI2*FREQ/VTD__C * BP )
         FLUX_SUM = FLUX_SUM + MAP%FLUX_CC(J2)
         IF ( FL_TEST ) THEN
              WRITE ( 6, 110 ) J2, MAP%COOR_CC(1,J2)*RAD__TO__MAS, &
     &                         MAP%COOR_CC(2,J2)*RAD__TO__MAS, MAP%FLUX_CC(J2), BP
 110          FORMAT ( 'Comp: ', I4, ' Xc: ', F6.3, ' Yc: ', F6.3, &
     &                 ' Flux: ', F8.5, ' Bp= ', F12.6 )
         END IF
 420  CONTINUE 
      VTD_GR_DEL_STRUC = (DRE*DIM_DER - DIM*DRE_DER)/(DRE**2 + DIM**2)
      AMP = DSQRT ( ABS(DRE**2 - DIM**2) ) 
      IF ( FL_TEST ) THEN
           WRITE ( 6, 120 ) AMP, DRE, DIM, DRE_DER, DIM_DER, MAP%NOISE
 120       FORMAT ( 'Amp: ', F12.6, ' Jy  Re/Im: ', F10.7, 1X, F10.7, &
     &                              ' Der_re/im: ', 1PD12.5, 1X, 1PD12.5,  &
     &                              ' Map_noise: ', 1PD12.5, ' Jy' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  VTD_GR_DEL_STRUC  !#!#
