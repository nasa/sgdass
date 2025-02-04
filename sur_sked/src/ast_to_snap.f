      PROGRAM    AST_TO_SNAP_MAIN
! ************************************************************************
! *                                                                      *
! *   Program AST_TO_SNAP transforms the schedule in ast format to       *
! *   a number of schedule files in snap format for each station.        *
! *                                                                      *
! * ### 30-DEC-2017 AST_TO_SNAP_MAIN v1.0 (c) L. Petrov  30-DEC-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      CHARACTER  FILAST*128, DIROUT*128
      INTEGER*4  IUER
      IF ( IARGC() .GE. 2 ) THEN
           CALL GETARG ( 1, FILAST )
           CALL GETARG ( 2, DIROUT )
         ELSE 
           WRITE ( 6, '(A)' ) 'Usage: ast_to_snap ast_file output_directory'
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL AST_TO_SNAP ( FILAST, DIROUT, SUR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4701, IUER, 'AST_TO_SNAP', 'Error in parsing '// &
     &         'the input schedule file '//FILAST )
           RETURN 
      END IF
      END  PROGRAM  AST_TO_SNAP_MAIN !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE AST_TO_SNAP ( FILAST, DIROUT, SUR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine AST_TO_SNAP parses schedule in AST format and puts the     *
! *   results of parsing in SUR object.                                  *
! *                                                                      *
! *  ### 08-JAN-2018   AST_TO_SNAP   v1.6 (c) L. Petrov  07-FEB-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILAST*128, DIROUT*(*)
      INCLUDE   'astro_constants.i'
      INCLUDE   'sur_sked.i'
      TYPE     ( SUR__TYPE ) :: SUR
      INTEGER*4  IUER
      INTEGER*4    MBUF, MIND, M_PAR
      PARAMETER  ( MBUF  = 128*1024 )
      PARAMETER  ( MIND  = 128 )
      PARAMETER  ( M_PAR = M__SUR_STP )
      CHARACTER    BUF(MBUF)*384, STR*128, STR1*128, STR2*128, OUT(MBUF)*512, &
     &             C_STA(SUR__M_STA)*8, C_SOU(SUR__M_SOU)*8, FILOUT*128, &
     &             USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128, &
     &             ALP_STR*14, DEL_STR*13, CUR_SCA*8, STA_PAR(M_PAR,SUR__M_STA)*128, &
     &             WRAP*8, SKD_MOUNT*4, SLEW_AZ_MODE*1, SLEW_EL_MODE*1, SLEW_MODE*2, &
     &             MODE_NAME*16
      REAL*8       ALP(SUR__M_SOU), DEL(SUR__M_SOU), SLEW_DUR, PREOB_DUR, POSTOB_DUR, &
     &             REC_DUR, REC_ACC, D_AZ, D_EL, SLEW_AZ, SLEW_EL, SLEW_TIME
      REAL*8       UTC_REC_START, UTC_REC_STOP, UTC_PREOB_START, UTC_PREOB_STOP, &
     &             UTC_POSTOB_START, UTC_POSTOB_STOP, UTC_SLEW_START, UTC_SLEW_STOP, &
     &             EL_CUR, AZ_CUR, AZ_ACC_CUR, AZ_ACC_PREV, HA_CUR, HA_ACC_CUR, &
     &             HA_ACC_PREV, EL_PREV, AZ_PREV, HA_PREV, RD, &
     &             IDLE_AFTER_SLEW, IDLE_AFTER_PREOB, S_J2000(3), S_B1950(3), &
     &             ALP_B1950, DEL_B1950, SET_MODE_DUR
      LOGICAL*1    FL_NEW_SLEW_TIME
      INTEGER*4    J1, J2, J3, J4, J5, J6, IFMT, NBUF, LIND, IND(2,MIND), ISTA, ISOU, &
     &             MJD_REC_START, MJD_REC_STOP, MJD_PREOB_START, MJD_PREOB_STOP, &
     &             MJD_POSTOB_START, MJD_POSTOB_STOP, &
     &             MJD_SLEW_START, MJD_SLEW_STOP, NPAR(SUR__M_STA), NO, IER
      REAL*8       ADS3000__PROG
      PARAMETER  ( ADS3000__PROG = 20.0 ) ! Pre-start for ADS3000 antenna control unit
      INTEGER*4,   EXTERNAL :: ILEN, I_LEN, ADD_CLIST, LTM_DIF
      CHARACTER,   EXTERNAL :: GET_CDATE*19, MJDSEC_TO_SNAP*18
!
      FL_NEW_SLEW_TIME = .FALSE.
      CALL GETINFO_USER ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FILAST, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4711, IUER, 'AST_TO_SNAP', 'Error in reading '// &
     &         'input schedule file '//FILAST )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(SUR__AST_FORMAT)) .EQ. SUR__AST_FORMAT ) THEN
           IFMT = 102
         ELSE
           CALL ERR_LOG ( 4712, IUER, 'AST_TO_SNAP', 'Cannot recognize schedule '// &
     &         'format: the first line of file '//TRIM(FILAST)//' is supposed '// &
     &         'to be '//SUR__AST_FORMAT//' but got '//TRIM(BUF(1)) )
           RETURN 
      END IF
!
      CALL NOUT ( SIZEOF(SUR), SUR )
      SUR%L_STA = 0
      SUR%L_SOU = 0
      SUR%L_SCN = 0
      NPAR      = 0
!
! --- The first run
!
      SUR%SCHEDULE_REVISION = '1'
      DO 410 J1=1,NBUF
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(32)//CHAR(0)//CHAR(9), IER )
         IF ( BUF(J1)(1:1) == '#' ) THEN
              CONTINUE 
            ELSE IF ( ILEN(BUF(J1)) == 0 ) THEN
              CONTINUE 
            ELSE IF ( BUF(J1)(1:LEN('Experiment:')) == 'Experiment:' ) THEN
               SUR%EXP_CODE = BUF(J1)(IND(1,2):IND(2,2))
            ELSE IF ( BUF(J1)(1:LEN('  Schedule_revision:')) == '  Schedule_revision:' ) THEN
               SUR%SCHEDULE_REVISION = BUF(J1)(IND(1,3):IND(2,3))
            ELSE IF ( BUF(J1)(1:LEN('  Scheduler_name:')) == '  Scheduler_name:' ) THEN
               SUR%SCHEDULER_NAME = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(1:LEN('  Scheduler_email:')) == '  Scheduler_email:' ) THEN
               SUR%SCHEDULER_EMAIL = BUF(J1)(IND(1,3):IND(2,3))
            ELSE IF ( BUF(J1)(1:LEN('  Scheduler_phone:')) == '  Scheduler_phone:' ) THEN
               SUR%SCHEDULER_PHONE = BUF(J1)(IND(1,3):IND(2,3))
            ELSE IF ( BUF(J1)(1:LEN('  Observer_phone:')) == '  Observer_phone:' ) THEN
               SUR%OBSERVER_PHONE = BUF(J1)(IND(1,3):IND(2,3))
            ELSE IF ( BUF(J1)(1:LEN('  Experiment_description:')) == '  Experiment_description:' ) THEN
               SUR%EXP_DESCR = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(1:LEN('  UTC_experiment_dates:'))   == '  UTC_experiment_dates:'   ) THEN
               SUR%DATE_START_UTC = BUF(J1)(IND(1,3):IND(2,3))
               SUR%DATE_STOP_UTC  = BUF(J1)(IND(1,4):IND(2,4))
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( SUR%DATE_START_UTC, SUR%MJD_START, SUR%UTC_START, IER )
               CALL ERR_PASS ( IUER, IER )
               CALL DATE_TO_TIME ( SUR%DATE_STOP_UTC,  SUR%MJD_STOP,  SUR%UTC_STOP,  IER )
            ELSE IF ( BUF(J1)(1:LEN('  Corr_spectral_resolution:')) == '  Corr_spectral_resolution:' ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=* ) SUR%CORR_SPECTRAL_RESOLUTION
            ELSE IF ( BUF(J1)(1:LEN('  Corr_time_resolution:')) == '  Corr_time_resolution:' ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=* ) SUR%CORR_TIME_RESOLUTION
            ELSE IF ( BUF(J1)(1:LEN('Station_parameters:')) == 'Station_parameters:'   ) THEN
               IF ( LIND < 4 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J1, STR ) 
                    CALL ERR_LOG ( 4713, IUER, 'AST_TO_SNAP', 'Error in parsing line '// &
     &                   TRIM(STR)//' of schedule file '//TRIM(FILAST)// &
     &                   ' -- the line has less than 4 words' )
                    RETURN 
               END IF
               ISTA = ADD_CLIST ( SUR__M_STA, SUR%L_STA, C_STA, BUF(J1)(IND(1,2):IND(2,2)), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( SUR__M_STA, STR ) 
                    CALL ERR_LOG ( 4714, IUER, 'AST_TO_SNAP', 'Error in parsing '// &
     &                   ' of schedule file '//TRIM(FILAST)// &
     &                   ' -- it has more than '//TRIM(STR)//' stations' )
                    RETURN 
               END IF
               SUR%STA(ISTA)%NAME       = BUF(J1)(IND(1,2):IND(2,2))
               SUR%STA(ISTA)%SHORT_NAME = BUF(J1)(IND(1,4):IND(2,4))
            ELSE IF ( BUF(J1)(1:LEN('  Last_update:'))    == '  Last_update:' ) THEN
               SUR%STA(ISTA)%LAST_UPDATE = BUF(J1)(IND(1,3):IND(2,3))
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  Coordinates:'))    == '  Coordinates:' ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%COO_TRS(1)
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%COO_TRS(2)
               READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%COO_TRS(3)
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  Mount:'))          == '  Mount:'       ) THEN
               SUR%STA(ISTA)%MOUNT_TYPE = BUF(J1)(IND(1,3):IND(2,3))
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  1st_axis_range:')) == '  1st_axis_range:'   ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%AZ_RANGE(1)
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%AZ_RANGE(2)
               READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%AZ_RANGE(3)
               READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%AZ_RANGE(4)
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  2nd_axis_range:')) == '  2nd_axis_range:'   ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%EL_MIN
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%EL_MAX
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  1st_axis_slewing_rate:')) == '  1st_axis_slewing_rate:'   ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%SLEW_RATE_AZ
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  2nd_axis_slewing_rate:')) == '  2nd_axis_slewing_rate:'   ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%SLEW_RATE_EL
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  1st_axis_slewing_accl:')) == '  1st_axis_slewing_accl:'   ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%SLEW_ACCL_AZ
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  2nd_axis_slewing_accl:')) == '  2nd_axis_slewing_accl:'   ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%SLEW_ACCL_EL
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  1st_axis_settle_time:')) == '  1st_axis_settle_time:'     ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%TIME_SETTLE_AZ
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  2nd_axis_settle_time:')) == '  2nd_axis_settle_time:'     ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%TIME_SETTLE_EL
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  Preob_proc_diration:'))  == '  Preob_proc_duration:'      ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%PREOB
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  Postob_proc_diration:'))  == '  Postob_proc_duration:'      ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%POSTOB
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  Recorder:'))       == '  Recorder:'    ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%RECORDER
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('  Recording_rate:')) == '  Recording_rate:' ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%REC_RATE
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN(' Setmode_duration:'))   == '  Setmode_duration'   ) THEN
               READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT=*, IOSTAT=IER ) SUR%STA(ISTA)%SETMODE
               NPAR(ISTA) = NPAR(ISTA) + 1 ; STA_PAR(NPAR(ISTA),ISTA) = BUF(J1)
            ELSE IF ( BUF(J1)(1:LEN('Scan:')) == 'Scan:'   ) THEN
               SUR%L_SCN = SUR%L_SCN + 1
               IF ( LIND < 16 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( J1, STR ) 
                    CALL ERR_LOG ( 4715, IUER, 'AST_TO_SNAP', 'Error in parsing line '// &
     &                   TRIM(STR)//' of schedule file '//TRIM(FILAST)// &
     &                   ' -- the line has less than 16 words' )
                    RETURN 
               END IF
               ISOU = ADD_CLIST ( SUR__M_SOU, SUR%L_SOU, C_SOU, BUF(J1)(IND(1,4):IND(2,4)), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( SUR__M_SOU, STR ) 
                    CALL ERR_LOG ( 4716, IUER, 'AST_TO_SNAP', 'Error in parsing '// &
     &                   ' of schedule file '//TRIM(FILAST)// &
     &                   ' -- it has more than '//TRIM(STR)//' sources' )
                    RETURN 
               END IF
               CALL ERR_PASS ( IUER, IER )
               CALL HR_TAT ( BUF(J1)(IND(1,8):IND(2,8)), ALP(ISOU), IER )
               CALL ERR_PASS ( IUER, IER )
               CALL GR_TAT ( BUF(J1)(IND(1,10):IND(2,10)), DEL(ISOU), IER )
            ELSE IF ( BUF(J1)(1:LEN('  Station:'))    == '  Station:'   ) THEN
               CONTINUE 
            ELSE IF ( BUF(J1)(1:LEN('    Set_mode:')) == '    Set_mode:' ) THEN
               SUR%STA(ISTA)%HDS_NAME = BUF(J1)(IND(1,3):IND(2,3))
            ELSE IF ( BUF(J1)(1:LEN('    Preob:'))     == '    Preob:'    ) THEN
               CONTINUE 
            ELSE IF ( BUF(J1)(1:LEN('    Slew:'))      == '    Slew:'     ) THEN
               CONTINUE 
            ELSE IF ( BUF(J1)(1:LEN('    Record:'))    == '    Record:'   ) THEN
               CONTINUE 
            ELSE IF ( BUF(J1)(1:LEN('    Postob:'))    == '    Postob:'   ) THEN
               CONTINUE 
            ELSE
             CALL CLRCH ( STR )
             CALL INCH  ( J1, STR ) 
             CALL ERR_LOG ( 4719, IUER, 'AST_TO_SNAP', 'Error in parsing line '// &
     &            TRIM(STR)//' of schedule file '//TRIM(FILAST)// &
     &            ' -- cannot recognize keyword '//BUF(J1)(IND(1,1):IND(2,1)) )
             RETURN 
         END IF
 410  CONTINUE 
!%      write ( 6, * ) 'sur%l_sta= ', sur%l_sta ! %%%
!%      write ( 6, * ) 'sur%l_sou= ', sur%l_sou ! %%%
!%      write ( 6, * ) 'sur%l_scn= ', sur%l_scn ! %%%
!
      DO 420 J2=1,SUR%L_STA
         FILOUT = TRIM(SUR%EXP_CODE)//TRIM(SUR%STA(J2)%SHORT_NAME)//'.snp'
         CALL TRAN ( 12, FILOUT, FILOUT )
         FILOUT = TRIM(DIROUT)//'/'//FILOUT
         write ( 6, * ) 'Fil: '//TRIM(FILOUT) ! %%%
!
         NO = 0
         NO = NO + 1 ; OUT(NO)= '" '//SUR%EXP_CODE(1:8)//'  '// &
     &                                SUR%DATE_START_UTC(1:4)//'  '// &
     &                                SUR%STA(J2)%NAME//'  '// &
     &                                SUR%STA(J2)%SHORT_NAME(1:1)//' '// &
     &                                SUR%STA(J2)%SHORT_NAME(1:2)
         IF ( SUR%STA(J2)%MOUNT_TYPE == 'ALTAZ' ) THEN
              SKD_MOUNT = 'AZEL'
            ELSE IF ( SUR%STA(J2)%MOUNT_TYPE == 'XY_E' ) THEN
              SKD_MOUNT = 'XYEW'
            ELSE IF ( SUR%STA(J2)%MOUNT_TYPE == 'EQUAT' ) THEN
              SKD_MOUNT = 'HADC'
            ELSE 
              SKD_MOUNT = 'UNKN'
         END IF 
         NO = NO + 1 ; OUT(NO)= '" '//SUR%STA(J2)%SHORT_NAME(1:1)// &
     &                          '               '// &
     &                          SUR%STA(J2)%NAME//'  '// &
     &                          SKD_MOUNT
         NO = NO + 1
         WRITE ( UNIT=OUT(NO), FMT=110 ) SUR%STA(J2)%SHORT_NAME, &
     &                                   SUR%STA(J2)%NAME, &
     &                                   SUR%STA(J2)%COO_TRS
 110     FORMAT ( '" ', A, 14X, A, 3(2X,F12.3) )
         NO = NO + 1 ; OUT(NO)= '" '
         NO = NO + 1 ; OUT(NO)= '" VLBI experiment schedule in snap format'
         NO = NO + 1 ; OUT(NO)= '" ======================================='
         NO = NO + 1 ; OUT(NO)= '" '
         NO = NO + 1 ; OUT(NO)= '" Experiment_code:          '//SUR%EXP_CODE
         NO = NO + 1 ; OUT(NO)= '" Experiment_description:   '//SUR%EXP_DESCR
         NO = NO + 1 ; OUT(NO)= '" Station:                  '//SUR%STA(J2)%NAME//'    '// &
     &                                                          SUR%STA(J2)%SHORT_NAME
         NO = NO + 1 ; OUT(NO)= '" Schedule_revision:        '//SUR%SCHEDULE_REVISION
         NO = NO + 1 ; OUT(NO)= '" Scheduler_name:           '//SUR%SCHEDULER_NAME
         NO = NO + 1 ; OUT(NO)= '" Scheduler_email:          '//SUR%SCHEDULER_EMAIL
         NO = NO + 1 ; OUT(NO)= '" Scheduler_phone:          '//SUR%SCHEDULER_PHONE
         NO = NO + 1 ; OUT(NO)= '" Observer_phone:           '//SUR%OBSERVER_PHONE
         NO = NO + 1 ; OUT(NO)= '" UTC_experiment_dates:     '//SUR%DATE_START_UTC//', '// &
     &                             SUR%DATE_STOP_UTC
         NO = NO + 1 ; OUT(NO)= '" '
         NO = NO + 1 ; OUT(NO)= '" Station parameters used for generating this schedule:'
         NO = NO + 1 ; OUT(NO)= '" '
         DO 430 J3=1,NPAR(J2)
            NO = NO + 1 ; OUT(NO)= '" '//STA_PAR(J3,J2)(3:)
 430     CONTINUE 
         NO = NO + 1 ; OUT(NO)= '" '
         NO = NO + 1 ; OUT(NO)= '" Generated with    '//A2S__LABEL
         NO = NO + 1 ; OUT(NO)= '" Generated using   '//FILAST
         NO = NO + 1 ; OUT(NO)= '" Generated on      '//GET_CDATE()
         NO = NO + 1 ; OUT(NO)= '" Generated by      '//USER_REALNAME
         NO = NO + 1 ; OUT(NO)= '" '
!
         REC_DUR  = -1.0001D0
         REC_ACC  =  0.0D0
         SLEW_DUR = 0
         DO 440 J4=1,NBUF+1
            IF ( J4 .LE. NBUF ) THEN
                 CALL EXWORD ( BUF(J4), MIND, LIND, IND, CHAR(32)//CHAR(0)//CHAR(9), IER )
                 IF ( LIND < 2 ) GOTO 440
                 IF ( BUF(J4)(IND(1,2):IND(2,2)) .NE. TRIM(SUR%STA(J2)%NAME) ) GOTO 440
            END IF
            IF ( BUF(J4)(1:LEN('  Station:')) == '  Station:' .OR. J4 == NBUF + 1 ) THEN
                 IF ( REC_DUR > -1.0D0 ) THEN
                      IF ( CUR_SCA == 'no0001' .AND. SUR%STA(J2)%RECORDER == 'ads3000' ) THEN
                           STR = MJDSEC_TO_SNAP ( MJD_PREOB_START, UTC_PREOB_START - ADS3000__PROG, IER )
                           NO = NO + 1 ; OUT(NO) = STR 
                           NO = NO + 1 ; OUT(NO) = 'program'
                           NO = NO + 1 ; OUT(NO) = '+20s'
                      END IF 
                      NO = NO + 1
                      PREOB_DUR = SUR%STA(J2)%PREOB
                      POSTOB_DUR = SUR%STA(J2)%POSTOB
                      CALL INCH ( IDNINT(REC_DUR + PREOB_DUR), STR )
                      CALL INCH ( IDNINT(REC_DUR), STR1 )
                      OUT(NO) ='scan_name='//TRIM(CUR_SCA)//','// &
     &                          TRIM(SUR%EXP_CODE)//','//TRIM(SUR%STA(J2)%SHORT_NAME)// &
     &                          ','//TRIM(STR1)//','//TRIM(STR1)
                      CALL TRAN ( 12, OUT(NO), OUT(NO) )
!
                      IF ( SUR%STA(J2)%RECORDER .NE. 'ads3000' ) THEN
                           CALL RH_TAT ( ALP(ISOU), 4, ALP_STR, IER )
                           CALL CHASHL ( ALP_STR )
                           CALL RG_TAT ( DEL(ISOU), 3, DEL_STR, IER )
                           IF ( DEL_STR(1:1) == ' ') DEL_STR(1:1) = '+'
                           NO = NO + 1
                           OUT(NO) ='source='//TRIM(C_SOU(ISOU))//','// &
     &                               ALP_STR(1:2)//ALP_STR(4:5)//ALP_STR(7:13)//','// &
     &                               DEL_STR(1:3)//DEL_STR(5:6)//DEL_STR(8:13)//',2000.0'
                        ELSE IF ( SUR%STA(J2)%RECORDER .EQ. 'ads3000' ) THEN
                           S_J2000(1) = DCOS(DEL(ISOU))*DCOS(ALP(ISOU))
                           S_J2000(2) = DCOS(DEL(ISOU))*DSIN(ALP(ISOU))
                           S_J2000(3) = DSIN(DEL(ISOU))
                           CALL J2000_TO_B1950 ( S_J2000, S_B1950 )
                           CALL DECPOL ( 3, S_B1950, RD, ALP_B1950, DEL_B1950, IER )
!
                           CALL RH_TAT ( ALP_B1950, 4, ALP_STR, IER )
                           CALL CHASHL ( ALP_STR )
                           CALL RG_TAT ( DEL_B1950, 3, DEL_STR, IER )
                           IF ( DEL_STR(1:1) == ' ') DEL_STR(1:1) = '+'
                           NO = NO + 1
                           OUT(NO) ='source='//TRIM(C_SOU(ISOU))//','// &
     &                               ALP_STR(1:2)//ALP_STR(4:5)//ALP_STR(7:13)//','// &
     &                               DEL_STR(1:3)//DEL_STR(5:6)//DEL_STR(8:13)//',1950.0'
                      END IF
                      IF ( SUR%STA(J2)%MOUNT_TYPE == 'ALTAZ' ) THEN
                           OUT(NO) = TRIM(OUT(NO))//','//WRAP
                      END IF
                      CALL TRAN ( 12, OUT(NO), OUT(NO) )
!
                      NO = NO + 1
                      IF ( REC_ACC .LE. 0.001 ) THEN
                           WRITE ( UNIT=OUT(NO), FMT=120 ) AZ_CUR, EL_CUR, HA_CUR
 120                       FORMAT ( '" New az/el/ha: ', F6.2, ', ', F7.2, ', ', F7.2 )
                         ELSE
                           WRITE ( UNIT=OUT(NO), FMT=130 ) AZ_CUR,  EL_CUR,  HA_CUR, &
     &                                                     AZ_PREV, EL_PREV, HA_PREV, SLEW_DUR
 130                       FORMAT ( '" New az/el/ha: ', F7.2, ', ', F6.2, ', ', F7.2, &
     &                              '  Old az/el/ha: ', F7.2, ', ', F6.2, ', ', F7.2, &
     &                              '  Slewing time: ', F6.1, ' sec' )
                           D_AZ = DABS(AZ_ACC_CUR - AZ_ACC_PREV)
                           IF ( D_AZ > SUR%STA(J2)%SLEW_RATE_AZ**2/SUR%STA(J2)%SLEW_ACCL_AZ ) THEN
                                SLEW_AZ = (D_AZ - SUR%STA(J2)%SLEW_RATE_AZ**2/SUR%STA(J2)%SLEW_ACCL_AZ)/ &
     &                                    SUR%STA(J2)%SLEW_RATE_AZ + &
     &                                    2.0D0*SUR%STA(J2)%SLEW_RATE_AZ/SUR%STA(J2)%SLEW_ACCL_AZ
                                SLEW_AZ_MODE = 'f'
                              ELSE
                                SLEW_AZ = 2.D0*DSQRT(D_AZ/SUR%STA(J2)%SLEW_ACCL_AZ)
                                SLEW_AZ_MODE = 's'
                           END IF
                           D_EL = DABS(EL_CUR - EL_PREV)
                           IF ( D_EL > SUR%STA(J2)%SLEW_RATE_EL**2/SUR%STA(J2)%SLEW_ACCL_EL ) THEN
                                SLEW_EL = (D_EL - SUR%STA(J2)%SLEW_RATE_EL**2/SUR%STA(J2)%SLEW_ACCL_EL)/ &
     &                                    SUR%STA(J2)%SLEW_RATE_EL + &
     &                                    2.0D0*SUR%STA(J2)%SLEW_RATE_EL/SUR%STA(J2)%SLEW_ACCL_EL
                                SLEW_EL_MODE = 'f'
                              ELSE
                                SLEW_EL = 2.D0*DSQRT(D_EL/SUR%STA(J2)%SLEW_ACCL_EL)
                                SLEW_EL_MODE = 's'
                           END IF
                           IF ( SLEW_AZ + SUR%STA(J2)%TIME_SETTLE_AZ > SLEW_EL + SUR%STA(J2)%TIME_SETTLE_EL ) THEN
                                SLEW_MODE = 'A'//SLEW_AZ_MODE
                                SLEW_TIME = SLEW_AZ + SUR%STA(J2)%TIME_SETTLE_AZ 
                              ELSE
                                SLEW_MODE = 'E'//SLEW_EL_MODE
                                SLEW_TIME = SLEW_EL + SUR%STA(J2)%TIME_SETTLE_EL
                           END IF
                           NO = NO + 1
                           IF ( .NOT. FL_NEW_SLEW_TIME ) THEN
                                WRITE ( UNIT=OUT(NO), FMT=140 ) AZ_ACC_CUR - AZ_ACC_PREV, EL_CUR - EL_PREV, &
     &                                                          SLEW_AZ, SLEW_EL, &
     &                                                          SUR%STA(J2)%TIME_SETTLE_AZ, &
     &                                                          SUR%STA(J2)%TIME_SETTLE_EL, SLEW_MODE
 140                            FORMAT ( '" D_az/D_el: ', 3X, F7.2, ', ', F6.2, &
     &                                   '  Slew_az/Slew_el: ',               F5.1, ', ', F5.1, &
     &                                   '  Settle_time_az/Settle_time_el: ', F5.1, ', ', F5.1, &
     &                                   '  Slew_mode: ', A )
                              ELSE
!
! ----------------------------- Add extra parameter: slewing time
!
                                WRITE ( UNIT=OUT(NO), FMT=145 ) AZ_ACC_CUR - AZ_ACC_PREV, EL_CUR - EL_PREV, &
     &                                                          SLEW_AZ, SLEW_EL, &
     &                                                          SUR%STA(J2)%TIME_SETTLE_AZ, &
     &                                                          SUR%STA(J2)%TIME_SETTLE_EL, SLEW_MODE, SLEW_TIME
 145                            FORMAT ( '" D_az/D_el: ', 3X, F7.2, ', ', F6.2, &
     &                                   '  Slew_az/Slew_el: ',               F5.1, ', ', F5.1, &
     &                                   '  Settle_time_az/Settle_time_el: ', F5.1, ', ', F5.1, &
     &                                   '  Slew_mode: ', A, ' Slew_time: ', F5.1 )
                           END IF 
                           IDLE_AFTER_SLEW = (MJD_PREOB_START - MJD_SLEW_STOP)*86400.0D0 + &
     &                                       (UTC_PREOB_START - UTC_SLEW_STOP)
                           IF ( IDLE_AFTER_SLEW .GE. 0.099D0 ) THEN
                                NO = NO + 1
                                WRITE ( UNIT=OUT(NO), FMT=150 ) IDLE_AFTER_SLEW 
 150                            FORMAT ( '" Idle after slewing for ', F8.1, ' sec' )
                           END IF
                      END IF
!
                      IF ( REC_ACC .GT. 0.001 ) THEN
                           IF ( SUR%STA(J2)%RECORDER .EQ. 'rdbe' ) THEN
                                CALL INCH ( IDNINT(1000.0D0*SUR%STA(J2)%REC_RATE), STR1 )
                                NO = NO + 1
                                OUT(NO) = 'mk6=rtime?'//TRIM(STR1)//';'
                                NO = NO + 1 ; OUT(NO)= 'checkmk6'
                             ELSE IF ( SUR%STA(J2)%RECORDER .EQ. 'ads3000' ) THEN
                                CONTINUE
                             ELSE 
                                NO = NO + 1 ; OUT(NO)= 'checkmk5'
                           END IF
                         ELSE IF ( SUR%STA(J2)%RECORDER == 'rdbe' ) THEN
                           NO = NO + 1 ; OUT(NO) = 'setmode_'//TRIM(MODE_NAME)
                           CALL INCH  ( IDNINT(SUR%STA(ISTA)%SETMODE), STR1 )
                           NO = NO + 1 ; OUT(NO) = '!+'//TRIM(STR1)//'s'
                         ELSE
                           NO = NO + 1 ; OUT(NO)= 'ready_disk'
                      END IF
                      IER = -1
                      IF ( SUR%STA(J2)%RECORDER == 'rdbe' ) THEN
                           NO = NO + 1 ; OUT(NO)= 'setupbb'
                         ELSE
                           NO = NO + 1 ; OUT(NO)= 'setup01'
                      END IF
!
                      STR = MJDSEC_TO_SNAP ( MJD_PREOB_START, UTC_PREOB_START, IER )
                      IF ( SUR%STA(J2)%RECORDER == 'rdbe' ) THEN
                           CALL INCH ( IDNINT(REC_DUR), STR1 )
                           CALL INCH ( IDNINT(REC_DUR*SUR%STA(J2)%REC_RATE/8.192D0), STR2 )
                           NO = NO + 1
                           OUT(NO) = 'mk6=record='//STR(2:5)//'y'//STR(7:9)//'d'// &
     &                                STR(11:12)//'h'//STR(14:15)//'m'//STR(17:18)//'s'//':'// &
     &                                TRIM(STR1)//':'// &
     &                                TRIM(STR2)//':'// &
     &                                TRIM(CUR_SCA)//':'// &
     &                                TRIM(SUR%EXP_CODE)//':'// &
     &                                SUR%STA(J2)%SHORT_NAME//';'
                           CALL TRAN ( 12, OUT(NO), OUT(NO) )
                      END IF
                      NO = NO + 1 ; OUT(NO) = STR
                      NO = NO + 1 ; OUT(NO) = 'preob'
                      IF ( SUR%STA(J2)%RECORDER == 'rdbe' ) THEN
                           CALL INCH ( IDNINT(1000.0D0*SUR%STA(J2)%REC_RATE), STR1 )
                           NO = NO + 1
                           OUT(NO) = 'mk6=rtime?'//TRIM(STR1)//';'
                      END IF
                      IDLE_AFTER_PREOB = (MJD_REC_START - MJD_PREOB_STOP)*86400.0D0 + &
     &                                   (UTC_REC_START - UTC_PREOB_STOP)
                      IF ( IDLE_AFTER_PREOB > 0.099D0 ) THEN
                           NO = NO + 1
                           WRITE ( UNIT=OUT(NO), FMT=160 ) IDLE_AFTER_PREOB
 160                       FORMAT ( '" Idle after preob   for ', F8.1, ' sec' )
                      END IF
                      STR = MJDSEC_TO_SNAP ( MJD_REC_START, UTC_REC_START, IER )
                      NO = NO + 1 ; OUT(NO)= STR
                      IF ( SUR%STA(J2)%RECORDER(1:5) == 'mark5' ) THEN
                           NO = NO + 1 ; OUT(NO) = 'disk_pos'
                      END IF
                      IF ( SUR%STA(J2)%RECORDER(1:5) == 'mark5' .OR. SUR%STA(J2)%RECORDER == 'flexbuf' ) THEN
                           NO = NO + 1 ; OUT(NO) = 'disk_record=on'
                           NO = NO + 1 ; OUT(NO) = 'disk_record'
                      END IF
                      NO = NO + 1 ; OUT(NO) = 'data_valid=on'
                      IF ( SUR%STA(J2)%RECORDER == 'rdbe' ) THEN
                           CALL INCH ( IDNINT(1000.0D0*SUR%STA(J2)%REC_RATE), STR1 )
                           NO = NO + 1
                           OUT(NO) = 'mk6=rtime?'//TRIM(STR1)//';'
                      END IF
                      NO = NO + 1 ; OUT(NO) = 'midob'
                      STR = MJDSEC_TO_SNAP ( MJD_REC_START, UTC_REC_STOP, IER )
                      NO = NO + 1 ; OUT(NO)= STR
                      NO = NO + 1 ; OUT(NO) = 'data_valid=off'
                      IF ( SUR%STA(J2)%RECORDER(1:5) == 'mark5' .OR. SUR%STA(J2)%RECORDER == 'flexbuf' ) THEN
                           NO = NO + 1 ; OUT(NO) = 'disk_record=off'
                      END IF
                      IF ( SUR%STA(J2)%RECORDER(1:5) == 'mark5' ) THEN
                           NO = NO + 1 ; OUT(NO) = 'disk_pos'
                      END IF
                      NO = NO + 1 ; OUT(NO) = 'postob'
                      STR = MJDSEC_TO_SNAP ( MJD_POSTOB_START, UTC_POSTOB_START + POSTOB_DUR, IER )
                      IF ( J4 .LE. NBUF ) THEN
                           NO = NO + 1 ; OUT(NO) = '" '//STR(2:18)//'  Start slewing to the next source'
                      END IF
                      NO = NO + 1 ; OUT(NO) = '"'
!
                      REC_ACC = REC_ACC + REC_DUR
                      ISOU = -1
                 END IF                      
                 IF ( J4 == NBUF + 1 ) THEN
                      NO = NO + 1
                      WRITE ( UNIT=OUT(NO), FMT=170 ) REC_ACC, REC_ACC*SUR%STA(J2)%REC_RATE/8*1.D-3
 170                  FORMAT ( '" Total recording time: ', F8.1, ' sec  or ', F12.3, ' TB' )
                      IF ( SUR%STA(J2)%RECORDER(1:5) == 'mark5' .OR. SUR%STA(J2)%RECORDER == 'flexbuf' ) THEN
                           NO = NO + 1 ; OUT(NO) = 'checkmk5'
                        ELSE IF ( SUR%STA(J2)%RECORDER == 'ads3000' ) THEN
                           NO = NO + 1 ; OUT(NO) = 'source=azeluncr,0d,89.986d'
                           NO = NO + 1 ; OUT(NO) = '!+8m'
                           NO = NO + 1 ; OUT(NO) = 'standby'
                      END IF
                      NO = NO + 1 ; OUT(NO) = 'sched_end'
                      IF ( SUR%STA(J2)%RECORDER == 'ads3000' ) THEN
                           NO = NO + 1 ; OUT(NO) = 'terminate'
                      END IF
                      GOTO 440
                 END IF
                 IF ( BUF(J4)(IND(1,6):IND(2,6)) .NE. 'skipping' ) THEN
                      CUR_SCA = BUF(J4)(IND(1,4):IND(2,4))
                      CALL TRAN ( 12, CUR_SCA, CUR_SCA  )
                      IF ( IFMT == 100 ) THEN
                           ISOU = LTM_DIF ( 0, SUR%L_SOU, C_SOU, BUF(J4)(IND(1,7):IND(2,7)) )
                         ELSE
                           ISOU = LTM_DIF ( 0, SUR%L_SOU, C_SOU, BUF(J4)(IND(1,8):IND(2,8)) )
                      END IF
                    ELSE 
                      REC_DUR  = -1.0001D0
                 END IF
               ELSE IF ( BUF(J4)(1:LEN('    Set_mode:'))  == '    Set_mode:' ) THEN
                 IF ( BUF(J4)(IND(1,10):IND(2,10)) == '&n' ) THEN
                      WRAP = 'neutral'
                    ELSE IF ( BUF(J4)(IND(1,10):IND(2,10)) == 'und' ) THEN
                      WRAP = 'neutral'
                    ELSE IF ( BUF(J4)(IND(1,10):IND(2,10)) == '&cw' ) THEN
                      WRAP = 'cw'
                    ELSE IF ( BUF(J4)(IND(1,10):IND(2,10)) == '&ccw' ) THEN
                      WRAP = 'ccw'
                    ELSE 
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 4720, IUER, 'AST_TO_SNAP', 'Error in parsing '// &
     &                    'the '//TRIM(STR)//' th line of the input ast file: '// &
     &                    ' unsupported wrap '//BUF(J4)(IND(1,10):IND(2,10)) )
                      RETURN 
                 END IF
                 MODE_NAME = BUF(J4)(IND(1,8):IND(2,8))
                 IF ( LIND .GE. 12 ) THEN
                      READ ( UNIT=BUF(J4)(IND(1,12):IND(2,12)), FMT='(F10.1)' ) SET_MODE_DUR
                    ELSE
                      SET_MODE_DUR = 0.0D0
                 END IF
               ELSE IF ( BUF(J4)(1:LEN('    Preob:'))     == '    Preob:'    ) THEN
                 CALL DATE_TO_TIME ( BUF(J4)(IND(1,3):IND(2,3)), MJD_PREOB_START, UTC_PREOB_START, IER )
                 CALL DATE_TO_TIME ( BUF(J4)(IND(1,4):IND(2,4)), MJD_PREOB_STOP,  UTC_PREOB_STOP,  IER )
                 READ ( UNIT=BUF(J4)(IND(1,10):IND(2,10)), FMT=*, IOSTAT=IER ) PREOB_DUR
               ELSE IF ( BUF(J4)(1:LEN('    Slew:'))      == '    Slew:'     ) THEN
                 READ ( UNIT=BUF(J4)(IND(1,11):IND(2,11)), FMT=*, IOSTAT=IER ) SLEW_DUR
                 READ ( UNIT=BUF(J4)(IND(1,13):IND(2,13)), FMT=*, IOSTAT=IER ) EL_PREV
                 READ ( UNIT=BUF(J4)(IND(1,16):IND(2,16)), FMT=*, IOSTAT=IER ) AZ_ACC_PREV
                 READ ( UNIT=BUF(J4)(IND(1,17):IND(2,17)), FMT=*, IOSTAT=IER ) AZ_ACC_CUR
                 READ ( UNIT=BUF(J4)(IND(1,19):IND(2,19)), FMT=*, IOSTAT=IER ) HA_ACC_PREV
                 READ ( UNIT=BUF(J4)(IND(1,20):IND(2,20)), FMT=*, IOSTAT=IER ) HA_ACC_PREV
                 CALL DATE_TO_TIME ( BUF(J4)(IND(1,3):IND(2,3)), MJD_SLEW_START, UTC_SLEW_START, IER )
                 CALL DATE_TO_TIME ( BUF(J4)(IND(1,4):IND(2,4)), MJD_SLEW_STOP,  UTC_SLEW_STOP,  IER )
                 IF ( BUF(J4)(IND(1,22):IND(2,22)) == '&n' ) THEN
                      WRAP = 'neutral'
                    ELSE IF ( BUF(J4)(IND(1,22):IND(2,22)) == 'und' ) THEN
                      WRAP = 'neutral'
                    ELSE IF ( BUF(J4)(IND(1,22):IND(2,22)) == '&cw' ) THEN
                      WRAP = 'cw'
                    ELSE IF ( BUF(J4)(IND(1,22):IND(2,22)) == '&ccw' ) THEN
                      WRAP = 'ccw'
                    ELSE 
                      CALL CLRCH ( STR )
                      CALL INCH  ( J4, STR )
                      CALL ERR_LOG ( 4720, IUER, 'AST_TO_SNAP', 'Error in parsing '// &
     &                    'the '//TRIM(STR)//' th line of the input ast file: '// &
     &                    ' unsupported wrap '//BUF(J4)(IND(1,22):IND(2,22)) )
                      RETURN 
                 END IF
               ELSE IF ( BUF(J4)(1:LEN('    Record:'))    == '    Record:'   ) THEN
                 AZ_PREV = AZ_CUR
                 HA_PREV = HA_CUR
                 CALL DATE_TO_TIME ( BUF(J4)(IND(1,3):IND(2,3)), MJD_REC_START, UTC_REC_START, IER )
                 CALL DATE_TO_TIME ( BUF(J4)(IND(1,4):IND(2,4)), MJD_REC_STOP,  UTC_REC_STOP,  IER )
                 READ ( UNIT=BUF(J4)(IND(1,10):IND(2,10)), FMT=*, IOSTAT=IER ) REC_DUR
                 READ ( UNIT=BUF(J4)(IND(1,12):IND(2,12)), FMT=*, IOSTAT=IER ) EL_CUR
                 READ ( UNIT=BUF(J4)(IND(1,14):IND(2,14)), FMT=*, IOSTAT=IER ) AZ_CUR
                 READ ( UNIT=BUF(J4)(IND(1,16):IND(2,16)), FMT=*, IOSTAT=IER ) HA_CUR
               ELSE IF ( BUF(J4)(1:LEN('    Postob:'))     == '    Postob:'    ) THEN
                 CALL DATE_TO_TIME ( BUF(J4)(IND(1,3):IND(2,3)), MJD_POSTOB_START, UTC_POSTOB_START, IER )
                 CALL DATE_TO_TIME ( BUF(J4)(IND(1,4):IND(2,4)), MJD_POSTOB_STOP,  UTC_POSTOB_STOP,  IER )
                 READ ( UNIT=BUF(J4)(IND(1,10):IND(2,10)), FMT=*, IOSTAT=IER ) PREOB_DUR
            END IF
 440     CONTINUE 
!
         CALL ERR_PASS ( IUER, IER )
         CALL WR_TEXT  ( NO, OUT, FILOUT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4722, IUER, 'AST_TO_SNAP', 'Error in writing '// &
     &            'the output file '//FILOUT )
              RETURN 
         END IF
 420  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE AST_TO_SNAP  !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION MJDSEC_TO_SNAP ( MJD, TIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine MJDSEC_TO_SNAP writes down time in snap format. *
! *                                                                      *
! *  ### 09-JAN-2018 MJDSEC_TO_SNAP v1.0 (c) L. Petrov  09-JAN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  MJDSEC_TO_SNAP*18
      INTEGER*4  MJD, IUER
      REAL*8     TIM
      INTEGER*4  MJD_NY, IER
      REAL*8     TIM_NY
      CHARACTER  STR*30
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      STR = MJDSEC_TO_DATE ( MJD, 1.0D0*IDNINT(TIM + 0.499D0), IER )
      MJDSEC_TO_SNAP = '!'//STR(1:5)//'???.'//STR(12:19)
      STR(6:19) = '01.01_00:00:00'
      CALL DATE_TO_TIME ( STR(1:19), MJD_NY, TIM_NY, IER )
      WRITE ( UNIT=MJDSEC_TO_SNAP(7:9), FMT='(I3)' ) MJD - MJD_NY + 1
      CALL BLANK_TO_ZERO ( MJDSEC_TO_SNAP(7:9) )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  FUNCTION  MJDSEC_TO_SNAP  !#!  
