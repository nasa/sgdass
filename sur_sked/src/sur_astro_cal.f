      SUBROUTINE SUR_ASTRO_CAL ( SUR, VTD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SUR_ASTRO_CAL
! *                                                                      *
! * ### 18-AUG-2012   SUR_ASTRO_CAL  v1.1 (c)  L. Petrov 09-JAN-2018 ### *
! *                                                                      *
! ************************************************************************
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      INCLUDE   'vtd.i'
      TYPE     ( SUR__TYPE ) :: SUR
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  IVRB, IUER
      INTEGER*4  J1, J2, IND_CAL_MIN, IER
      LOGICAL*1  FL_STA(SUR__M_STA)
      REAL*8     SLEW_TIME, SLEW_TIME_MIN, PREOB_DUR
      REAL*8,    EXTERNAL :: SUR_SLEW_TIME 
!
      SLEW_TIME_MIN = 86400.0D0
      IND_CAL_MIN = 0
      DO 410 J1=1,SUR%L_CAL
         CALL ERR_PASS ( IUER, IER ) 
         SLEW_TIME = SUR_SLEW_TIME ( SUR, VTD, SUR__TYP_CAL, J1, &
     &               SUR%IND_SRC(SUR%L_SCN), SUR%SRC_TYP(SUR%L_SCN), &
     &               0, SUR__FINE, IER )
         IF ( SLEW_TIME .LE. 0.1D0 ) GOTO 410
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1641, IUER, 'SUR_ASTRO_CAL', 'Error in '// &
     &            'computing slew time for calibrator source' )
              RETURN 
         END IF
         IF ( SLEW_TIME < SLEW_TIME_MIN ) THEN
              SLEW_TIME_MIN = SLEW_TIME
              IND_CAL_MIN = J1
         END IF
 410  CONTINUE 
      IF ( SUR%PREOBS_SHORT > 0.0 .AND. SKIP_PREOBS_LONG > 0 ) THEN
           PREOB_DUR = SUR%PREOBS_SHORT 
         ELSE 
           PREOB_DUR = SUR%PREOBS_LONG
      END IF
!
      FL_STA = .TRUE.
      CALL ERR_PASS ( IUER, IER ) 
      CALL SUR_TROPO_SCAN_UPDATE ( SUR, VTD, SUR__TYP_CAL, IND_CAL_MIN, &
     &               SUR%MJD_CUR, &
     &               SUR%TAI_CUR + SLEW_TIME_MIN + PREOB_DUR, &
     &               SUR%TROPO_SCAN_LEN, FL_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1642, IUER, 'SUR_ASTRO_CAL', 'Error in '// &
     &         'an attempt to update calibrator scan' )
           RETURN 
      END IF
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  write ( 6, 112 ) slew_time_min + sur%preobs_short + sur%tropo_scan_len  ! %%%%%
! 112  FORMAT ( /'@@@ ', f6.1/ ) ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SUR_ASTRO_CAL  !#!#
