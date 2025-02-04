      SUBROUTINE SIMOBS_GET ( GVH, SIMUL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SIMOBS_GET
! *                                                                      *
! *  ### 14-DEC-2020   SIMOBS_GET   v1.0 (c) L. Petrov  17-DEC-2020 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'simul.i'
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU   ) :: GVH
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      CHARACTER  DESCR*80, STR*80, STR1*80
      INTEGER*4  DIMS(2), J1, J2, IER 
      REAL*8     ARR_R8(128)
      REAL*4     ARR_R4(128)
      INTEGER*4  IUER 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NOBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3811, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode NUMB_OBS' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SCA', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NSCA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3812, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode NUMB_SCA' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_STA', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NSTA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3813, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode NUMB_STA' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_SOU', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NSOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3814, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode NUMB_SOU' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUM_BAND', 0, 0, 4, DIMS(1), DIMS(2), SIMUL%NBND, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3815, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode NBND' )
           RETURN 
      END IF
!
      CALL CLRCH ( SIMUL%EXPER_DESCR )
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'EXP_DESC', 0, 0, LEN(SIMUL%EXPER_DESCR), &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%EXPER_DESCR), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3816, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode EXP_DESC' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL CLRCH ( SIMUL%EXPER_NAME )
      CALL GVH_GLCODE ( GVH, 'EXP_CODE', 0, 0, LEN(SIMUL%EXPER_NAME), &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%EXPER_NAME), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3817, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode EXP_CODE' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'PI_NAME ', 0, 0, LEN(SIMUL%PI_NAME), &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%PI_NAME), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3818, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode PI_NAME' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( 0, IER )
      CALL GVH_GLCODE ( GVH, 'UTC_MTAI', 0, 0, 8, DIMS(1), DIMS(2), &
     &                  SIMUL%UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3819, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode UTC_MTAI' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( 0, IER )
      CALL GVH_GLCODE ( GVH, 'SAMPLRAT', 0, 0, 8, DIMS(1), DIMS(2), &
     &                  SIMUL%SAMPLE_RATE(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3820, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode SAMPLE_RATE' )
           RETURN 
      END IF
      SIMUL%SAMPLE_RATE = SIMUL%SAMPLE_RATE(1)
!
      CALL ERR_PASS   ( 0, IER )
      CALL GVH_GLCODE ( GVH, 'BITSAMPL', 0, 0, 8, DIMS(1), DIMS(2), &
     &                  SIMUL%BITS_SAMPLE(1), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3821, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode BITS_SAMPLE' )
           RETURN 
      END IF
      SIMUL%BITS_SAMPLE = SIMUL%BITS_SAMPLE(1)
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SITNAMES', 0, 0, SIMUL%NSTA*8, &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%STA_NAM), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3822, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode SITNAMES' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SRCNAMES', 0, 0, SIMUL%NSOU*8, &
     &                  DIMS(1), DIMS(2), %REF(SIMUL%SOU_NAM), IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3823, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode SRCNAMES' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SIT_COOR', 0, 0, 3*SIMUL%NSTA*8, &
     &                  DIMS(1), DIMS(2), SIMUL%STA_COO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3824, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode SIT_COOR' )
           RETURN 
      END IF
!     
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'SOU_COOR', 0, 0, 2*SIMUL%NSOU*8, &
     &                  DIMS(1), DIMS(2), SIMUL%SOU_COO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3825, IUER, 'SIMOBS_GET', 'Error in '// &
     &         'getting lcode SOU_COOR' )
           RETURN 
      END IF
!
      DO 410 J1=1,SIMUL%NOBS
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SCANNAME', J1, 0, 16, DIMS(1), DIMS(2), &
     &                     %REF(SIMUL%SCAN_NAME(J1)), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3826, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode SCANNAME' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'MJD_OBS ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     SIMUL%MJD_OBS(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3827, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode MJD_OBS' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'UTC_OBS ', J1, 0, 8, DIMS(1), DIMS(2), &
     &                     SIMUL%UTC_OBS(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3828, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode UTC_OBS' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SCAN_DUR', J1, 1, 2*8, DIMS(1), &
     &                     DIMS(2), SIMUL%SCAN_DUR(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3829, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode SCAN_DUR' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SOU_IND ', J1, 0, 4, DIMS(1), DIMS(2), &
     &                     SIMUL%SOU_IND(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3830, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode SOU_IND' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'STA_IND ', J1, 0, 2*4, DIMS(1), DIMS(2), &
     &                     SIMUL%STA_IND(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3831, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode STA_IND' )
              RETURN
         END IF
!
         SIMUL%GR_DEL_ERR(1:SIM__MBND,J1) = 0.0D0
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'GRDELERR', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     SIMUL%GR_DEL_ERR(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3832, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode GRDELERR' )
              RETURN
         END IF
!
         SIMUL%PH_RAT_ERR(1:SIM__MBND,J1) = 0.0D0
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'PHRATERR', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     SIMUL%PH_RAT_ERR(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3833, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode PHRATERR' )
              RETURN
         END IF
!
         SIMUL%SNR(1:SIM__MBND,J1) = 0.0D0
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'SNRATIO ', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     SIMUL%SNR(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3834, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode SNRATIO' )
              RETURN
         END IF
!
         SIMUL%EFF_FREQ(1:3,1:SIM__MBND,J1) = 0.0D0
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'EFF_FREQ', J1, 1, 3*SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     ARR_R8, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3835, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode EFF_FREQ for the second station' )
              RETURN
         END IF
         DO 420 J2=1,SIMUL%NBND
            SIMUL%EFF_FREQ(1,J2,J1) = ARR_R8(1+(J2-1)*3)
            SIMUL%EFF_FREQ(2,J2,J1) = ARR_R8(2+(J2-1)*3)
            SIMUL%EFF_FREQ(3,J2,J1) = ARR_R8(3+(J2-1)*3)
 420     CONTINUE 
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'REF_FREQ', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     SIMUL%REF_FREQ, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3836, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode REF_FREQ' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'FRN_AMPL', J1, 0, SIMUL%NBND*8, DIMS(1), DIMS(2), &
     &                     SIMUL%AMP(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3837, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode FRN_AMPL' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'NOISERMS', J1, 0, SIMUL%NBND*4, DIMS(1), DIMS(2), &
     &                     ARR_R4, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3838, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode NOISERMS' )
              RETURN
         END IF
         SIMUL%NOI(1:SIMUL%NBND,J1) = ARR_R4(1:SIMUL%NBND)
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'AZIMUTH ', J1, 1, 8, DIMS(1), DIMS(2), &
     &                     SIMUL%AZ(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3839, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode AZIMUTH' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'AZIMUTH ', J1, 2, 8, DIMS(1), DIMS(2), &
     &                     SIMUL%AZ(2,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3840, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode AZIMUTH' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'ELEV    ', J1, 1, 8, DIMS(1), DIMS(2), &
     &                     SIMUL%EL(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3841, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode ELEV' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'ELEV    ', J1, 2, 8, DIMS(1), DIMS(2), &
     &                     SIMUL%EL(2,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3842, IUER, 'SIMOBS_GET', 'Error in '// &
     &            'getting lcode ELEV' )
              RETURN
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER ) 
      RETURN
      END  SUBROUTINE  SIMOBS_GET  !#!#
