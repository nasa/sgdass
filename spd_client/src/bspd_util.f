#include <mk5_preprocessor_directives.inc>
      SUBROUTINE BSPD_CHECK ( BSPD_DIR, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Program BSPD_CHECK checks the consistency of the directory with    *
! *   slant ath delay in a binary form.                                  *
! *                                                                      *
! *  ### 18-NOV-2024   BSPD_CHECK  v1.1 (c)  L. Petrov  25-NOV-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  IUER 
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      CHARACTER  BSPD_DIR*(*)
      INTEGER*4  IVRB
      INTEGER*4  M_STA
      PARAMETER  ( M_STA = 8192 )
      CHARACTER  FILSUM*128, FIL_BSPD*128, MODE*128, &
     &           BUF(M_STA)*128, SUM_BEG_STR*21, SUM_END_STR*21, &
     &           FIL_PREFIX*128, C_ERR(M_STA)*128, STA_NAM*8, &
     &           STATUS_LINE*256, STA_MIN_BEG_STR*21, STA_MAX_BEG_STR*21, &
     &           STA_MIN_END_STR*21, STA_MAX_END_STR*21, NUM_EPC_STR*9, &
     &           NUM_EPC_MIN_STR*9, NUM_EPC_MAX_STR*9
      LOGICAL*1  LEX, FL_WRONG_SIZE_LEN
      INTEGER*4  MIND
      REAL*8     EPS
      PARAMETER  ( MIND = 16      )
      PARAMETER  ( EPS  = 180.0D0 )
      REAL*8     SUM_TAI_BEG, SUM_TAI_END, SUM_TIM_BEG, SUM_TIM_END, &
     &           STA_TIM_BEG, STA_TIM_END, MIN_STA_TIM_BEG, MAX_STA_TIM_BEG, &
     &           MIN_STA_TIM_END, MAX_STA_TIM_END, TIM_STEP
      INTEGER*4  NB, IS, IP, LIND, IND(2,MIND), SUM_MJD_BEG, SUM_MJD_END, &
     &           SUM_NEPC, STA_MJD_BEG, STA_MJD_END, UNIX_DATE, &
     &           N_ERR, N_NFD, J1, J2, J3, J4, J5, NUM_EPC, NUM_EPC_MIN, &
     &           NUM_EPC_MAX, IER
      INTEGER*8  DIR_DESC, FIL_LEN_INT, SIZE_I8
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4, EXTERNAL :: CLOSEDIR, FILE_INFO, I_LEN, ILEN
      DIR_DESC = FUNC_OPENDIR ( TRIM(BSPD_DIR)//CHAR(0) )
!
      DIR_DESC = FUNC_OPENDIR ( TRIM(BSPD_DIR)//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
           CALL ERR_LOG ( 5411, IUER, 'BSPD_CHECK', 'Input directory '// &
     &          TRIM(BSPD_DIR)//' does not exist' )
           RETURN
      END IF
      IP = CLOSEDIR ( %VAL(DIR_DESC) )
!
      FILSUM = TRIM(BSPD_DIR)//'/bspd_summary.txt'
      INQUIRE ( FILE=FILSUM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5412, IUER, 'BSPD_CHECK', 'Summary file '// &
     &          TRIM(FILSUM)//' was not found' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILSUM, M_STA, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5413, IUER, 'BSPD_CHECK', 'Error in reading '// &
     &          'summary file '//FILSUM )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(BSPD_SUMM__LABEL)) .NE. BSPD_SUMM__LABEL ) THEN
           CALL ERR_LOG ( 5414, IUER, 'BSPD_CHECK', 'Wrong format '// &
     &         'of file '//TRIM(FILSUM)//' -- its first line is '// &
     &          TRIM(BUF(1))//' while '//BSPD_SUMM__LABEL//' was expected' )
           RETURN
      END IF
!
      N_NFD = 0
      N_ERR = 0
      STA_MJD_BEG = -999999
      FL_WRONG_SIZE_LEN = .FALSE.
      DO 410 J1=1,NB
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Min_epoch:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)'    ) SUM_MJD_BEG
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F10.5)' ) SUM_TAI_BEG
              SUM_BEG_STR = BUF(J1)(IND(1,4):IND(2,4))
              SUM_TIM_BEG = (SUM_MJD_BEG - J2000__MJD)*86400.D0 + SUM_TAI_BEG
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Max_epoch:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)'    ) SUM_MJD_END
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F10.5)' ) SUM_TAI_END
              SUM_END_STR = BUF(J1)(IND(1,4):IND(2,4))
              SUM_TIM_END = (SUM_MJD_END - J2000__MJD)*86400.D0 + SUM_TAI_END
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Num_Epoch:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)'    ) SUM_NEPC
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Prefix:' ) THEN
              FIL_PREFIX = BUF(J1)(IND(1,2):IND(2,2))
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Sample_Interval:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' ) TIM_STEP
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Station_name:' ) THEN
              STA_NAM = '________'
              CALL TRAN  ( 12, BUF(J1)(IND(1,2):IND(2,2)), STA_NAM )
              FIL_BSPD = TRIM(BSPD_DIR)//'/'//TRIM(FIL_PREFIX)//TRIM(STA_NAM)//'.bspd'
              INQUIRE ( FILE=FIL_BSPD, EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   N_NFD = N_NFD + 1
                   N_ERR = N_ERR + 1
                   C_ERR(N_ERR) = 'Missing file: '//TRIM(FIL_BSPD)
                   GOTO 410
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_3D_BIN_READ_HEAD ( FIL_BSPD, SPD_3D_DEL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5415, IUER, 'BSPD_CHECK', 'Error in reading '// &
             &          'the header of file '//TRIM(FIL_BSPD)//' for station '//STA_NAM )
                   RETURN 
              END IF
              STA_TIM_BEG = (SPD_3D_DEL%TIM%MJD_BEG - J2000__MJD)*86400.D0 + &
     &                       SPD_3D_DEL%TIM%TAI_BEG
              STA_TIM_END = (SPD_3D_DEL%TIM%MJD_END - J2000__MJD)*86400.D0 + &
     &                       SPD_3D_DEL%TIM%TAI_END
              IF ( STA_MJD_BEG == -999999 ) THEN
                   MIN_STA_TIM_BEG = STA_TIM_BEG
                   MAX_STA_TIM_BEG = STA_TIM_BEG
                   MIN_STA_TIM_END = STA_TIM_END 
                   MAX_STA_TIM_END = STA_TIM_END
                   STA_MJD_BEG     = SPD_3D_DEL%TIM%MJD_BEG 
                 ELSE
                   IF ( (STA_TIM_BEG - MIN_STA_TIM_BEG) < -EPS ) THEN
                        MIN_STA_TIM_BEG = STA_TIM_BEG
                   END IF
                   IF ( (STA_TIM_BEG - MAX_STA_TIM_BEG) >  EPS ) THEN
                        MAX_STA_TIM_BEG = STA_TIM_BEG
                   END IF
                   IF ( (STA_TIM_END - MIN_STA_TIM_END) < -EPS ) THEN
                        MIN_STA_TIM_END = STA_TIM_END
                        N_ERR = N_ERR + 1
                        NUM_EPC = NINT(SUM_TIM_END - MIN_STA_TIM_END)/TIM_STEP
                        CALL CLRCH (          NUM_EPC_STR )
                        CALL INCH  ( NUM_EPC, NUM_EPC_STR )
                        C_ERR(N_ERR) = 'Min epoch is too early for station '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                                 ' at '//TRIM(NUM_EPC_STR)//' epochs' 
                   END IF
                   IF ( (STA_TIM_END - MAX_STA_TIM_END) >  EPS ) THEN
                        MAX_STA_TIM_END = STA_TIM_END
                        N_ERR = N_ERR + 1
                        C_ERR(N_ERR) = 'Max epoch is too late for station '//BUF(J1)(IND(1,2):IND(2,2))
                   END IF
              END IF
              FIL_LEN_INT = SPD_3D_DEL%LAB%OFF_DEL + SPD_3D_DEL%LAB%TOT_NUM_DEL*SPD_3D_DEL%LAB%LEN_DEL
              IS = FILE_INFO ( TRIM(FIL_BSPD)//CHAR(0), UNIX_DATE, SIZE_I8 )
              IF ( FIL_LEN_INT .NE. SIZE_I8 ) THEN
                   FL_WRONG_SIZE_LEN = .TRUE.
                   N_ERR = N_ERR + 1
                   WRITE ( UNIT=C_ERR(N_ERR), FMT=210 ) TRIM(FIL_BSPD), FIL_LEN_INT, SIZE_I8
 210               FORMAT ( 'Wrong size of file ', A, ' Computed: ', I9, ' Actual: ', I9 )
                   GOTO 410
              ENDIF
         END IF
 410  CONTINUE 
      STATUS_LINE = 'OK'
      IF ( DABS(MIN_STA_TIM_BEG - SUM_TIM_BEG) > EPS .OR. &
     &     DABS(MAX_STA_TIM_BEG - SUM_TIM_BEG) > EPS      ) THEN
           STATUS_LINE = 'Mismatch in the start date'
      END IF
!
      IF ( DABS(MIN_STA_TIM_END - SUM_TIM_END) > EPS .OR. &
     &     DABS(MAX_STA_TIM_END - SUM_TIM_END) > EPS      ) THEN
           NUM_EPC = NINT(SUM_TIM_END - MIN_STA_TIM_END)/TIM_STEP
           STA_MJD_END = MIN_STA_TIM_END/86400.0 + J2000__MJD
           STA_TIM_END = MIN_STA_TIM_END - (STA_MJD_END - J2000__MJD)*86400.0D0
           WRITE  ( UNIT=STATUS_LINE, FMT=220 ) NUM_EPC, NINT(MIN_STA_TIM_END/TIM_STEP), &
     &                                          TIM_TO_DATE ( MIN_STA_TIM_END, IER ), &
     &                                          STA_MJD_END, STA_TIM_END 
 220       FORMAT ( 'Mismatch in the end date at ', I9, ' epochs. ', &
     &              'Number of common epochs: ', I9, &
     &              ' Min epoch: ', A, 2X, I5, 2X, F7.1 )
      END IF
      STA_MIN_BEG_STR = TIM_TO_DATE ( MIN_STA_TIM_BEG, IER )
      STA_MAX_BEG_STR = TIM_TO_DATE ( MAX_STA_TIM_BEG, IER )
      STA_MIN_END_STR = TIM_TO_DATE ( MIN_STA_TIM_END, IER )
      STA_MAX_END_STR = TIM_TO_DATE ( MAX_STA_TIM_END, IER )
      NUM_EPC_MIN = NINT( (SUM_TIM_END - MIN_STA_TIM_END)/TIM_STEP )
      NUM_EPC_MAX = NINT( (SUM_TIM_END - MAX_STA_TIM_END)/TIM_STEP )
      IF ( N_NFD > 0 ) STATUS_LINE = 'Not all files with path were found'
      IF ( FL_WRONG_SIZE_LEN ) STATUS_LINE = 'Wrong file size'
!
      WRITE ( 6, 110   ) TRIM(STATUS_LINE)
      WRITE ( 6, 120   ) SUM_NEPC, TIM_STEP
      WRITE ( 6, 130   ) SUM_BEG_STR, STA_MIN_BEG_STR, STA_MAX_BEG_STR
      WRITE ( 6, 140   ) SUM_END_STR, STA_MIN_END_STR, STA_MAX_END_STR, &
     &                   NUM_EPC_MIN, NUM_EPC_MAX
 110  FORMAT ( 'Status: ', A )
 120  FORMAT ( 'BSPD_Summary Num_epochs: ', I6, ' Time_step: ', F8.1 ) 
 130  FORMAT ( 'BSPD_Summary Beg epoch:  ', A, &
     &         ' BSPD Beg_min epoch: ', A, ' BSPD Beg_max epoch: ', A )
 140  FORMAT ( 'BSPD_Summary End epoch:  ', A, &
     &         ' BSPD End_min epoch: ', A, ' BSPD End_max epoch: ', A, &
     &         ' Num_min_epochs: ', I9, ' Num_max_epochs: ', I9 )
      IF ( N_ERR > 0 ) THEN
!
! -------- Print error messages
!
           DO 420 J2=1,N_ERR
              WRITE ( 6, '(A)' ) TRIM(C_ERR(J2))
 420       CONTINUE 
      END IF
!
      IF ( STATUS_LINE == 'OK' ) THEN
           CALL ERR_LOG ( 0, IUER )
         ELSE
           CALL ERR_PASS ( 1, IUER )
      END IF
      RETURN
      END  SUBROUTINE  BSPD_CHECK  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  BSPD_INFO ( FIL_BSPD, STR_MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BSPD_INFO
! *                                                                      *
! *  ### 30-SEP-2024    BSPD_INFO   v1.0 (c)  L. Petrov  30-SEP-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      INTEGER*4  IUER
      CHARACTER  FIL_BSPD*(*), STR_MODE*(*)
      CHARACTER  DATE_BEG*21, DATE_END*21, STR*128
      LOGICAL*1  LEX
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      REAL*8     TAI_TRUN
      INTEGER*4  J1, IP, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN
!
      INQUIRE ( FILE=FIL_BSPD, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5421, IUER, 'BSPD_INFO', 'File '// &
     &          TRIM(FIL_BSPD)//' does not exist' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_BIN_READ_HEAD ( FIL_BSPD, SPD_3D_DEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5422, IUER, 'BSPD_INFO', 'Error in '// &
     &          'parsing the header of the binary file with slant '// &
     &          'path delay '//FIL_BSPD )
           RETURN 
      END IF
!
      IF ( STR_MODE == '-d' .OR. STR_MODE == '--date' ) THEN
           DATE_BEG = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_BEG, SPD_3D_DEL%TIM%TAI_BEG, IER )
           DATE_END = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_END, SPD_3D_DEL%TIM%TAI_END, IER )
           WRITE ( 6, 110 ) SPD_3D_DEL%TIM%MJD_BEG,  SPD_3D_DEL%TIM%TAI_BEG, DATE_BEG, &
     &                      SPD_3D_DEL%TIM%MJD_END,  SPD_3D_DEL%TIM%TAI_END, DATE_END, &
     &                      SPD_3D_DEL%TIM%TIM_STEP, SPD_3D_DEL%LAB%TOT_NUM_DEL, &
     &                      SPD_3D_DEL%TIM%NREC
 110       FORMAT ( 'MJD_BEG: ', I5, ' TAI_BEG: ', F8.1, ' BEG_DATE: ', A, &
     &                ' MJD_END: ', I5, ' TAI_END: ', F8.1, ' END_DATE: ', A, &
     &                ' TIM_STEP: ', F8.1, ' NUM_EPOCHS: ', I9, ' TIM_NREC: ', I4  )
        ELSE IF ( STR_MODE == '-nd' .OR. STR_MODE == '--num_delays' ) THEN
           WRITE ( 6, 120 ) SPD_3D_DEL%LAB%TOT_NUM_DEL
 120       FORMAT ( 'N_DEL: ', I6 )
        ELSE IF ( STR_MODE == '-s' .OR. STR_MODE == '--station' ) THEN
           WRITE ( 6, 130 ) SPD_3D_DEL%STA%NAME, SPD_3D_DEL%STA%COO_CFS
 130       FORMAT ( 'STA: ',A, ' COO: ', 3(F14.3,1X) )
        ELSE IF ( STR_MODE == '-m' .OR. STR_MODE == '--model' ) THEN
           IP = 1
           DO 410 J1=1,SPD_3D_DEL%MOD%N_LINES
              CALL CLRCH ( STR )
              CALL STRNCPY ( STR, SPD_3D_DEL%MOD%TEXT(IP) )
              IP = IP + ILEN(STR) + 1
              WRITE ( 6, 140 ) TRIM(STR)
 140          FORMAT ( A )
 410       CONTINUE 
        ELSE 
           CALL ERR_LOG ( 5423, IUER, 'BSPD_INFO', 'Unsupported mode: '// &
     &          TRIM(STR_MODE)//' Supported modes: -d, --date, -nd, --num_delays, '// &
     &          '-s, --station, -m, --model' )
           RETURN 
      END IF
!
      RETURN 
      END  SUBROUTINE  BSPD_INFO  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE BSPD_TRUNCATE ( FIL_BSPD, DATE_TRUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BSPD_TRUNCATE truncates file with slant path delays in    *
! *   binary format till the specified epoch. The specified eopoch       *
! *   becomes the last epoch of the file.                                *
! *                                                                      *
! *  ### 30-SEP-2024 BSPD_TRUNCATE  v1.0 (c)  L. Petrov  19-NOV-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      CHARACTER  FIL_BSPD*(*), DATE_TRUN*(*)
      INTEGER*4  IUER
      CHARACTER  FIL_TMP*128, STR*128
      LOGICAL*1  LEX
      INTEGER*8  OFF_TRUN
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      REAL*8     TAI_TRUN
      INTEGER*4  IS, LUN, MJD_TRUN, NREC_TRUN, SEEK_SET, ARG_LEN, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, SYSTEM, RENAME
      ADDRESS__TYPE, EXTERNAL :: LSEEK, WRITE, FTRUNCATE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- Check whether the file in quetion exists
!
      INQUIRE ( FILE=FIL_BSPD, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5431, IUER, 'BSPD_TRUNCATE', 'File '// &
     &          TRIM(FIL_BSPD)//' does not exist' )
           RETURN 
      END IF
!
! --- Tansform tranction date to MJD/TAI
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_TRUN, MJD_TRUN, TAI_TRUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5432, IUER, 'BSPD_TRUNCATE', 'Failure '// &
     &          'in parsing truncate date '//DATE_TRUN )
           RETURN 
      END IF
!
! --- Read the header of the file with slant path delay
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_BIN_READ_HEAD ( FIL_BSPD, SPD_3D_DEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5433, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'parsing the header of the input binary file with '// &
     &          'slant path delay '//FIL_BSPD )
           RETURN 
      END IF
!
! --- Determine how many delay records will be truncated
!
      NREC_TRUN = ( (SPD_3D_DEL%TIM%MJD_END - MJD_TRUN)*86400 + &
     &              (SPD_3D_DEL%TIM%TAI_END - TAI_TRUN)       + EPS )/SPD_3D_DEL%TIM%TIM_STEP
!
! --- Determine the length of the truncated file
!
      OFF_TRUN = SPD_3D_DEL%LAB%OFF_DEL + SPD_3D_DEL%LAB%LEN_DEL*(SPD_3D_DEL%LAB%TOT_NUM_DEL - NREC_TRUN)
!
! --- Check whether the truncation request can be performed
!
      IF ( NREC_TRUN == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Nothing to truncate'
           CALL ERR_LOG ( 0, IUER )
         ELSE IF ( NREC_TRUN < 0 ) THEN
!
! -------- The requested truncation date is later than the last date
!
           STR = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_END, SPD_3D_DEL%TIM%TAI_END, IER )
           STR = 'Last epoch: '//STR(1:21)//' Requested: truncation: '//DATE_TRUN
           CALL ERR_LOG ( 5434, IUER, 'BSPD_TRUNCATE', 'Requested truncation '// &
     &         'date is too late: '//TRIM(STR)//' for file '//FIL_BSPD )
           RETURN 
         ELSE IF ( OFF_TRUN .LE. 0 ) THEN
!
! -------- The requested truncation date is earlier than the first date
!
           STR = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_BEG, SPD_3D_DEL%TIM%TAI_BEG, IER )
           STR = 'First epoch: '//STR(1:21)//' Requested: truncation: '//DATE_TRUN
           CALL ERR_LOG ( 5435, IUER, 'BSPD_TRUNCATE', 'Requested truncation '// &
     &         'date is too early: '//TRIM(STR)//' for file '//FIL_BSPD )
           RETURN 
      END IF
!
! --- Copy the file into a temporary 
!
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(I8)') GETPID()
      CALL CHASHL ( STR )
      FIL_TMP = TRIM(FIL_BSPD)//'__'//TRIM(STR)
      IS = SYSTEM ( 'cp '//TRIM(FIL_BSPD)//' '//TRIM(FIL_TMP)//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR(STR)
           CALL ERR_LOG ( 5436, IUER, 'BSPD_TRUNCATE', 'Error in making '// &
     &         ' a temporary copy of file '//TRIM(FIL_BSPD)//' : '//STR )
           RETURN 
      END IF
!
! --- Update the last date and the total number of delay records
!
      SPD_3D_DEL%TIM%MJD_END = MJD_TRUN
      SPD_3D_DEL%TIM%TAI_END = TAI_TRUN
      SPD_3D_DEL%LAB%TOT_NUM_DEL = ( (SPD_3D_DEL%TIM%MJD_END - SPD_3D_DEL%TIM%MJD_BEG)*86400 + &
     &                               (SPD_3D_DEL%TIM%TAI_END - SPD_3D_DEL%TIM%TAI_BEG) + EPS )/ &
     &                                SPD_3D_DEL%TIM%TIM_STEP + 1
!
! --- Open the temporary output file
!
      LUN = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_TMP, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5437, IUER, 'BSPD_TRUNCATE', 'Error in opening '// &
     &         'temporary copy of file '//FIL_BSPD )
           RETURN 
      END IF
!
! --- Seek the beginning of the LAB record
!
      IS = LSEEK ( %VAL(LUN), %VAL(0), %VAL(SEEK_SET) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5438, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to position the temporary file '//TRIM(FIL_TMP)// &
     &          ' to the LAB record' )
           RETURN 
      END IF
!
! --- Update the lab record
!
      IS = WRITE ( %VAL(LUN), SPD_3D_DEL%LAB, %VAL(SIZEOF(SPD_3D_DEL%LAB)) )
      IF ( IS .NE. SIZEOF(SPD_3D_DEL%LAB) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5439, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to write LAB record into the output file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Seek the beginning of the time record
!
      IS = LSEEK ( %VAL(LUN), %VAL(SPD_3D_DEL%LAB%OFF_TIM), %VAL(SEEK_SET) )
      IF ( IS .NE. SPD_3D_DEL%LAB%OFF_TIM ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( SPD_3D_DEL%LAB%OFF_TIM, STR )
           CALL ERR_LOG ( 5440, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           TRIM(FIL_BSPD)//' to offset '//TRIM(STR)// &
     &          ' for seaking the TIM_REC section' )
           RETURN 
      END IF
!
! --- Update the time record
!
      IS = WRITE ( %VAL(LUN), SPD_3D_DEL%TIM, %VAL(SPD_3D_DEL%LAB%LEN_TIM) )
      IF ( IS .NE. SPD_3D_DEL%LAB%LEN_TIM ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5441, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to write TIM record into output temporary '// &
     &          'file '//FIL_BSPD )
           RETURN 
      END IF
!
! --- Truncate the temporary file FIL_TMP
!
      IS = FTRUNCATE ( %VAL(LUN), %VAL(OFF_TRUN) )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5442, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to truncate the output file '// &
     &          TRIM(FIL_TMP)//' : '//STR  )
           RETURN 
      END IF
!
! --- Close the output temporary file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5443, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to close file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Rename the truncated file back
!
      IS = RENAME ( TRIM(FIL_TMP)//CHAR(0), TRIM(FIL_BSPD)//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5444, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to move the output file '//TRIM(FIL_TMP)// &
     &          ' to '//TRIM(FIL_BSPD) )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BSPD_TRUNCATE  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE BSPDIR_TRUNCATE ( BSPD_DIR, DATE_TRUN, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BSPDIR_TRUNCATE truncates all files in the directory with *
! *   slant path delays in binary format till the specified epoch and    *
! *   updates the summary file. The specified epoch becomes the last     *
! *   epoch of the file.                                                 *
! *                                                                      *
! *  ### 30-SEP-2024 BSPDIR_TRUNCATE  v1.0 (c) L. Petrov 24-NOV-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      CHARACTER  BSPD_DIR*(*), DATE_TRUN*(*)
      INTEGER*4  IUER
      INTEGER*4  M_STA
      PARAMETER  ( M_STA = 8192 )
      CHARACTER  FILSUM*128, BUF(M_STA)*128, &
     &           FIL_BSPD(M_STA)*128, FIL_PREFIX*128, STA_NAM*8, &
     &           SUM_BEG_STR*21, SUM_END_STR*21, STR*128
      LOGICAL*1  LEX
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 64 )
      INTEGER*8  LEN_BSPD, LEN_BSPD_1ST
      INTEGER*4  IS, L_BSPD, UNIX_DATE, MJD_BEG, MJD_END, &
     &           MJD_BEG_1ST, MJD_END_1ST, &
     &           SUM_MJD_BEG, SUM_MJD_END, SUM_NEPC, NB, LIND, &
     &           IND(2,MIND), NEPC, NEPC_1ST, J1, J2, IER
      REAL*8     TAI_TRUN, TAI_BEG, TAI_END, TAI_BEG_1ST, TAI_END_1ST, &
     &           SUM_TAI_BEG, SUM_TAI_END, SUM_TIM_BEG, SUM_TIM_END, SUM_TIM_STEP
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, SYSTEM, RENAME, FILE_INFO 
!
      FILSUM = TRIM(BSPD_DIR)//'/bspd_summary.txt'
      INQUIRE ( FILE=FILSUM, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5511, IUER, 'BSPDIR_TRUNCATE', 'Summary file '// &
     &          TRIM(FILSUM)//' was not found' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILSUM, M_STA, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5512, IUER, 'BSPDIR_TRUNCATE', 'Error in reading '// &
     &          'summary file '//FILSUM )
           RETURN
      END IF
!
      IF ( BUF(1)(1:LEN(BSPD_SUMM__LABEL)) .NE. BSPD_SUMM__LABEL ) THEN
           CALL ERR_LOG ( 5513, IUER, 'BSPDIR_TRUNCATE', 'Wrong format '// &
     &         'of file '//TRIM(FILSUM)//' -- its first line is '// &
     &          TRIM(BUF(1))//' while '//BSPD_SUMM__LABEL//' was expected' )
           RETURN
      END IF
!
      L_BSPD = 0
      DO 410 J1=1,NB
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Min_epoch:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)'    ) SUM_MJD_BEG
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F10.5)' ) SUM_TAI_BEG
              SUM_BEG_STR = BUF(J1)(IND(1,4):IND(2,4))
              SUM_TIM_BEG = (SUM_MJD_BEG - J2000__MJD)*86400.D0 + SUM_TAI_BEG
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Max_epoch:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I5)'    ) SUM_MJD_END
              READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F10.5)' ) SUM_TAI_END
              SUM_END_STR = BUF(J1)(IND(1,4):IND(2,4))
              SUM_TIM_END = (SUM_MJD_END - J2000__MJD)*86400.D0 + SUM_TAI_END
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Num_Epoch:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(I6)'    ) SUM_NEPC
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Prefix:' ) THEN
              FIL_PREFIX = BUF(J1)(IND(1,2):IND(2,2))
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Sample_Interval:' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' ) SUM_TIM_STEP
         END IF
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'Station_name:' ) THEN
              L_BSPD = L_BSPD + 1
              STA_NAM = '________'
              CALL TRAN  ( 12, BUF(J1)(IND(1,2):IND(2,2)), STA_NAM )
              FIL_BSPD(L_BSPD) = TRIM(BSPD_DIR)//'/'//TRIM(FIL_PREFIX)//TRIM(STA_NAM)//'.bspd'
              INQUIRE ( FILE=FIL_BSPD(L_BSPD), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 5514, IUER, 'BSPDIR_TRUNCATE', 'Cannot '// &
     &                 'find file with slant path delay '//FIL_BSPD(L_BSPD) )
                   RETURN 
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_3D_BIN_READ_HEAD ( FIL_BSPD(L_BSPD), SPD_3D_DEL, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5515, IUER, 'BSPDIR_TRUNCATE', 'Error in '// &
     &                  'parsing the header of the binary file with slant '// &
     &                  'path delay '//FIL_BSPD(L_BSPD) )
                   RETURN 
              END IF
!
! ----------- Learn the length of the input file with slant path delay extension
!
              IS = FILE_INFO ( TRIM(FIL_BSPD(L_BSPD))//CHAR(0), UNIX_DATE, LEN_BSPD )
              IF ( IS .NE. 0 ) THEN
                   CALL GERROR ( STR )
                   CALL ERR_LOG ( 5516, IUER, 'BSPDIR_TRUNCATE', 'Error in '// &
     &                 'checking the size of slant path delay file '//FIL_BSPD(L_BSPD) )
                   RETURN 
              END IF
!
! ----------- Perform checks
!
              IF ( L_BSPD == 1 ) THEN
                   NEPC_1ST    = SPD_3D_DEL%LAB%TOT_NUM_DEL
                   MJD_BEG_1ST = SPD_3D_DEL%TIM%MJD_BEG
                   TAI_BEG_1ST = SPD_3D_DEL%TIM%TAI_BEG
                   MJD_END_1ST = SPD_3D_DEL%TIM%MJD_END
                   TAI_END_1ST = SPD_3D_DEL%TIM%TAI_END
                   IF ( NEPC_1ST .NE. SUM_NEPC ) THEN
                        CALL ERR_LOG ( 5517, IUER, 'BSPDIR_TRUNCATE', 'Mismatch '// &
     &                       'in the number of delays in the summary file '// &
     &                       TRIM(FILSUM)//' and in slant path delay file '// &
     &                       FIL_BSPD(L_BSPD) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_BEG_1ST - J2000__MJD)*86400.0D0 + TAI_BEG_1ST) - &
     &                        ((SUM_MJD_BEG - J2000__MJD)*86400.0D0 + SUM_TAI_BEG) ) > &
     &                  EPS ) THEN
                        CALL ERR_LOG ( 5518, IUER, 'BSPDIR_TRUNCATE', 'Mismatch '// &
     &                       'in the start epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in slant path delay file '// &
     &                       FIL_BSPD(L_BSPD) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_END_1ST - J2000__MJD)*86400.0D0 + TAI_END_1ST) - &
     &                        ((SUM_MJD_END - J2000__MJD)*86400.0D0 + SUM_TAI_END) ) > &
     &                  EPS ) THEN
                        CALL ERR_LOG ( 5519, IUER, 'BSPDIR_TRUNCATE', 'Mismatch '// &
     &                       'in the end epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in slant path delay file '// &
     &                       FIL_BSPD(L_BSPD) )
                        RETURN 
                   END IF
                 ELSE
                   NEPC    = SPD_3D_DEL%LAB%TOT_NUM_DEL
                   MJD_BEG = SPD_3D_DEL%TIM%MJD_BEG
                   TAI_BEG = SPD_3D_DEL%TIM%TAI_BEG
                   MJD_END = SPD_3D_DEL%TIM%MJD_END
                   TAI_END = SPD_3D_DEL%TIM%TAI_END
                   IF ( SPD_3D_DEL%LAB%TOT_NUM_DEL .NE. NEPC ) THEN
                        CALL ERR_LOG ( 5520, IUER, 'BSPDIR_TRUNCATE', 'Mismatch '// &
     &                       'in the number of delays in the summary file '// &
     &                       TRIM(FILSUM)//' and in slant path delay file '// &
     &                       FIL_BSPD(L_BSPD) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_BEG     - J2000__MJD)*86400.0D0 + TAI_BEG) - &
     &                        ((SUM_MJD_BEG - J2000__MJD)*86400.0D0 + SUM_TAI_BEG) ) > &
     &                  EPS ) THEN
                        CALL ERR_LOG ( 5521, IUER, 'BSPDIR_TRUNCATE', 'Mismatch '// &
     &                       'in the start epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in slant path delay file '// &
     &                       FIL_BSPD(L_BSPD) )
                        RETURN 
                   END IF
!
                   IF ( DABS( ((MJD_END     - J2000__MJD)*86400.0D0 + TAI_END    ) - &
     &                        ((SUM_MJD_END - J2000__MJD)*86400.0D0 + SUM_TAI_END) ) < &
     &                  -EPS ) THEN
                        WRITE ( 6, * ) 'MJD_END=     ', MJD_END,     ' TAI_END=     ', TAI_END 
                        WRITE ( 6, * ) 'SUM_MJD_END= ', SUM_MJD_END, ' SUM_TAI_END= ', SUM_TAI_END 
                        CALL ERR_LOG ( 5522, IUER, 'BSPDIR_TRUNCATE', 'Mismatch '// &
     &                       'in the end epoch in the summary file '// &
     &                       TRIM(FILSUM)//' and in slant path delay file '// &
     &                       FIL_BSPD(L_BSPD) )
                        RETURN 
                   END IF
              END IF
         END IF
 410  CONTINUE 
!
! --- Now truncate files
!
      DO 420 J2=1,L_BSPD
         CALL ERR_PASS ( IUER, IER )
         CALL BSPD_TRUNCATE ( FIL_BSPD(J2), DATE_TRUN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5524, IUER, 'BSPDIR_TRUNCATE', 'Failure in '// &
     &            'an attempt to truncate file slant path delays in binary &
     &             format '//FIL_BSPD(J2) )
              RETURN
        END IF
 420  CONTINUE 
!
! --- And finally, create the summary file
!
      CALL ERR_PASS ( IUER, IER )
      CALL CREATE_BSPD_SUMMARY ( BSPD_DIR, L_BSPD, FIL_BSPD, FIL_PREFIX, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5524, IUER, 'BSPDIR_TRUNCATE', 'Failure in '// &
     &         'an attempt to re-generate the summary of directory '// &
     &         'with slant path delays in binary format '//BSPD_DIR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BSPDIR_TRUNCATE  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE BSPD_EXTRACT( FIL_BSPD, DATE_EXTR, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BSPD_EXTRACT extracts an array of delay data starting     *
! *   with the specified epoch. This delay is written as a binary        *
! *   section into the output file. The name of the output file has the  *
! *   following format: IIIII_yyyymmdd_hhmmss__YYYYMMDD_HHMMSS.dat ,     *
! *   where                                                              *
! *   IIIII           -- input file name.                                *
! *   yyyymmdd_hhmmss -- is the date of the beginning of the extracted   *
! *                      section that corresponds to DATE_EXTR date;     *
! *   YYYYMMDD_HHMMSS -- is the date of the last epoch of the extracted  *
! *                      section.                                        *
! *   .dat            -- extension.                                      *
! *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FIL_BSPD  ( CHARACTER     ) -- Name of the input file with slant     *
! *                                path delay in BSPD format.            *
! * DATE_EXTR ( CHARACTER     ) -- Date of the first epoch to be         *
! *                                extracted from the input file in      *
! *                                YYYY.MM.DD_hh:mm:ss format.           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
!! *  ### 19-NOV-2024  BSPD_EXTRACT  v1.0 (c)  L. Petrov 21-NOV-2024 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      CHARACTER  FIL_BSPD*(*), DATE_EXTR*(*)
      INTEGER*4  IUER
      CHARACTER  FIL_OUT*128, STR*128, STR_DAT_BEG*21, STR_DAT_END*21
      LOGICAL*1  LEX
      INTEGER*8  OFF_EXTR, LEN_EXTR
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      REAL*8     TAI_EXTR
      INTEGER*1, ALLOCATABLE :: DEL_BIN(:)
      INTEGER*4  IS, LUN_IN, LUN_OUT, MJD_EXTR, NREC_EXTR, &
     &           SEEK_SET, ARG_LEN, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, SYSTEM, RENAME
      ADDRESS__TYPE, EXTERNAL :: LSEEK, READ, WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- Check whether the file in quetion exists
!
      INQUIRE ( FILE=FIL_BSPD, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5451, IUER, 'BSPD_EXTRACT', 'File '// &
     &          TRIM(FIL_BSPD)//' does not exist' )
           RETURN 
      END IF
!
! --- Tansform extraction date to MJD/TAI
!
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( DATE_EXTR, MJD_EXTR, TAI_EXTR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5452, IUER, 'BSPD_EXTRACT', 'Failure '// &
     &          'in parsing truncate date '//DATE_EXTR )
           RETURN 
      END IF
!
! --- Read the header of the file with slant path delay
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_BIN_READ_HEAD ( FIL_BSPD, SPD_3D_DEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5453, IUER, 'BSPD_EXTRACT', 'Error in '// &
     &          'parsing the header of the binary file with slant '// &
     &          'path delay '//FIL_BSPD )
           RETURN 
      END IF
!
! --- Determine how many delay records have to be extracted
!
      NREC_EXTR = ( (SPD_3D_DEL%TIM%MJD_END - MJD_EXTR)*86400 + &
     &              (SPD_3D_DEL%TIM%TAI_END - TAI_EXTR)       + EPS )/SPD_3D_DEL%TIM%TIM_STEP + 1
!
! --- Determine the offset of the extracted data
!
      OFF_EXTR = SPD_3D_DEL%LAB%OFF_DEL + SPD_3D_DEL%LAB%LEN_DEL*(SPD_3D_DEL%LAB%TOT_NUM_DEL - NREC_EXTR)
      LEN_EXTR = SPD_3D_DEL%LAB%LEN_DEL*NREC_EXTR
!
! --- Check whether the truncation request can be performed
!
      IF ( NREC_EXTR == 0 ) THEN
           WRITE ( 6, '(A)' ) 'Nothing to truncate'
           CALL ERR_LOG ( 0, IUER )
         ELSE IF ( NREC_EXTR < 0 ) THEN
!
! -------- The requested extraction date is later than the last date
!
           STR = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_END, SPD_3D_DEL%TIM%TAI_END, IER )
           STR = 'Last epoch: '//STR(1:21)//' Requested erxtraction: '//DATE_EXTR
           CALL ERR_LOG ( 5454, IUER, 'BSPD_EXTRACT', 'Requested extraction '// &
     &         'date is too late: '//TRIM(STR)//' for file '//FIL_BSPD )
           RETURN 
         ELSE IF ( OFF_EXTR .LE. 0 ) THEN
!
! -------- The requested truncation date is earlier than the first date
!
           STR = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_BEG, SPD_3D_DEL%TIM%TAI_BEG, IER )
           STR = 'First epoch: '//STR(1:21)//' Requested extraction: '//DATE_EXTR
           CALL ERR_LOG ( 5455, IUER, 'BSPD_EXTRACT', 'Requested extaction '// &
     &         'date is too early: '//TRIM(STR)//' for file '//FIL_BSPD )
           RETURN 
      END IF
!
      STR_DAT_BEG = MJDSEC_TO_DATE ( MJD_EXTR, TAI_EXTR, IER )
      STR_DAT_END = MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_END, SPD_3D_DEL%TIM%TAI_END, IER )
      FIL_OUT = TRIM(FIL_BSPD)//'_'//STR_DAT_BEG(1:4)//STR_DAT_BEG(6:7)//STR_DAT_BEG(9:10)// &
     &                          '_'//STR_DAT_BEG(12:13)//STR_DAT_BEG(15:16)//STR_DAT_BEG(18:19)// &
     &                         '__'//STR_DAT_END(1:4)//STR_DAT_END(6:7)//STR_DAT_END(9:10)// &
     &                          '_'//STR_DAT_END(12:13)//STR_DAT_END(15:16)//STR_DAT_END(18:19)// &
     &                          '.dat'
!
! --- Open the input file with slant path delay
!
      LUN_IN  = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_BSPD, 'OLD', LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5456, IUER, 'BSPD_EXTRACT', 'Error in opening '// &
     &         'temporary copy of file '//FIL_BSPD )
           RETURN 
      END IF
!
! --- Seek the beginning of the relevant delay record
!
      IS = LSEEK ( %VAL(LUN_IN), %VAL(OFF_EXTR), %VAL(SEEK_SET) )
      IF ( IS .NE. OFF_EXTR ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( OFF_EXTR, STR )
           CALL ERR_LOG ( 5457, IUER, 'BSPD_EXTRACT', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           TRIM(FIL_BSPD)//' to offset '//TRIM(STR)// &
     &          ' for seaking the relevant delay section' )
           RETURN 
      END IF
!
! --- Allocate memory for the delay buffer
!
      ALLOCATE ( DEL_BIN(LEN_EXTR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( LEN_EXTR, STR )
           CALL ERR_LOG ( 5458, IUER, 'BSPD_EXTRACT', 'Error in '// &
     &          'an attempt to allocate '//TRIM(STR)//' bytes of '// &
     &          'dynamic memory for the extracted delay array' )
           RETURN 
      END IF
!
! --- Read delay section
!
      IS = READ ( %VAL(LUN_IN), DEL_BIN, %VAL(LEN_EXTR) )
      IF ( IS .NE. LEN_EXTR ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5459, IUER, 'BSPD_EXTRACT', 'Error in '// &
     &          'an attempt to read the input file '//TRIM(FIL_BSPD)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Close the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5460, IUER, 'BSPD_EXTRACT', 'Error in '// &
     &          'an attempt to close file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Open the output file
!
      LUN_OUT = 11
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_OUT, 'UNKNOWN', LUN_OUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5461, IUER, 'BSPD_EXTRACT', 'Error in opening '// &
     &         'the output file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write delay section
!
      IS = WRITE ( %VAL(LUN_OUT), DEL_BIN, %VAL(LEN_EXTR) )
      IF ( IS .NE. LEN_EXTR ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5462, IUER, 'BSPD_EXTRACT', 'Error in '// &
     &          'an attempt to write into the output file '//FIL_OUT )
           RETURN 
      END IF
      DEALLOCATE ( DEL_BIN )
!
! --- Close the output file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_OUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5463, IUER, 'BSPD_EXTRACT', 'Error in '// &
     &          'an attempt to close file '//FIL_OUT )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BSPD_EXTRACT  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE BSPD_EXTEND ( FIL_BSPD, DATA_EXTN_FIL, FL_INPLACE, IUER )
! ************************************************************************
! *                                                                      *
! *   Program  BSPD_EXTEND  extends a given file with slant path delays  *
! *   using the input delay section. The first epoch of the delay        *
! *   section file should just follow the last epoch of the input slant  *
! *   path delay file.                                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FIL_BSPD      ( CHARACTER ) -- Name of the input file with slant     *
! *                                path delay in BSPD format.            *
! * DATA_EXTN_FIL ( CHARACTER ) -- Name of a file with a section of      *
! *                                slant path delays extracted from an   *
! *                                external file file.                   *
! * FL_INPLAC     ( LOGICAL*1 ) -- If .TRUE., then the input file will   *
! *                                be updated in place.                  *
! *                                If .FALSE., then the output file will *
! *                                be written in the same directory as   *
! *                                DATA_EXTN_FIL.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4, OPT ) -- Universal error handler.              *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 19-NOV-2024  BSPD_EXTEND  v1.0 (c)  L. Petrov  23-NOV-2024  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE       ( SPD_DEL__TYPE ) :: SPD_3D_DEL
      LOGICAL*1  FL_INPLACE
      CHARACTER  FIL_BSPD*(*), DATA_EXTN_FIL*(*)
      INTEGER*4  IUER
      CHARACTER  FIL_TMP*128, FIL_OUT*128, STR_DAT_BEG*21, STR_DAT_END*21, &
     &           STR*128
      LOGICAL*1  LEX
      INTEGER*8  OFF_EXTN, LEN_FIL, LEN_EXTN
      REAL*8     EPS
      PARAMETER  ( EPS = 180.0D0 )
      REAL*8     EXTN_TAI_BEG, EXTN_TAI_END
      INTEGER*1, ALLOCATABLE :: DEL_BIN(:)
      INTEGER*4  IS, LUN_IN, LUN_OUT, NREC_EXTN, EXTN_MJD_BEG, EXTN_MJD_END, &
     &           UNIX_DATE, SEEK_SET, ARG_LEN, IDB, IDD, IL, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GETPID, ILEN, I_LEN, SYSTEM, RENAME
      ADDRESS__TYPE, EXTERNAL :: LINDEX, FILE_INFO, LSEEK, READ, WRITE
!
      CALL GET_SYSTEM_CONSTANT ( 'SEEK_SET', SEEK_SET, ARG_LEN )
!
! --- Check whether the file with slant path delay to be exended exists
!
      INQUIRE ( FILE=FIL_BSPD, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5471, IUER, 'BSPD_EXTEND', 'File '// &
     &          TRIM(FIL_BSPD)//' does not exist' )
           RETURN 
      END IF
!
! --- Check whether the file with slant path delay extension exists
!
      INQUIRE ( FILE=DATA_EXTN_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5472, IUER, 'BSPD_EXTEND', 'Extension file '// &
     &          TRIM(DATA_EXTN_FIL)//' does not exist' )
           RETURN 
      END IF
!
! --- Read the header of the file with slant path delay
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_BIN_READ_HEAD ( FIL_BSPD, SPD_3D_DEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5473, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'parsing the header of the binary file with slant '// &
     &          'path delay '//FIL_BSPD )
           RETURN 
      END IF
!
! --- Extract the first date of the delay segment from the file name
!
      IL = ILEN(DATA_EXTN_FIL)
      STR_DAT_BEG = DATA_EXTN_FIL(IL-35:IL-32)//'.'//DATA_EXTN_FIL(IL-31:IL-30)//'.'// &
     &              DATA_EXTN_FIL(IL-29:IL-25)//':'//DATA_EXTN_FIL(IL-24:IL-23)//':'// &
     &              DATA_EXTN_FIL(IL-22:IL-21)
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( STR_DAT_BEG, EXTN_MJD_BEG, EXTN_TAI_BEG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5474, IUER, 'BSPD_EXTEND', 'Malformed name of '// &
     &         'file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
! --- Extract the last date of the delay segment from the file name
!
      IL = ILEN(DATA_EXTN_FIL)
      STR_DAT_END = DATA_EXTN_FIL(IL-18:IL-15)//'.'//DATA_EXTN_FIL(IL-14:IL-13)//'.'// &
     &              DATA_EXTN_FIL(IL-12:IL-8)//':'//DATA_EXTN_FIL(IL-7:IL-6)//':'// &
     &              DATA_EXTN_FIL(IL-5:IL-4)
      CALL ERR_PASS ( IUER, IER )
      CALL DATE_TO_TIME ( STR_DAT_END, EXTN_MJD_END, EXTN_TAI_END, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5475, IUER, 'BSPD_EXTEND', 'Malformed name of '// &
     &         'file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
! --- Learn the length of the input file with slant path delay extension
!
      IS = FILE_INFO ( TRIM(DATA_EXTN_FIL)//CHAR(0), UNIX_DATE, LEN_EXTN )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5476, IUER, 'BSPD_EXTEND', 'Error in inquiring '// &
     &         'extention file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
! --- Determine how many delay records have to be exended
!
      NREC_EXTN = ( (EXTN_MJD_END - SPD_3D_DEL%TIM%MJD_END)*86400 + &
     &              (EXTN_TAI_END - SPD_3D_DEL%TIM%TAI_END)       + EPS )/SPD_3D_DEL%TIM%TIM_STEP
      IF ( NREC_EXTN*SPD_3D_DEL%LAB%LEN_DEL .NE. LEN_EXTN ) THEN
           WRITE ( 6, * ) 'EXTN_END: '//MJDSEC_TO_DATE ( EXTN_MJD_END, EXTN_TAI_END, IER )
           WRITE ( 6, * ) 'FIL_END:  '//MJDSEC_TO_DATE ( SPD_3D_DEL%TIM%MJD_END, SPD_3D_DEL%TIM%TAI_END, IER )
           WRITE ( 6, * ) 'LEN_EXTN= ', LEN_EXTN
           WRITE ( 6, * ) 'NREC_EXTN*SPD_3D_DEL%LAB%LEN_DEL= ', NREC_EXTN*SPD_3D_DEL%LAB%LEN_DEL
           CALL ERR_LOG ( 5477, IUER, 'BSPD_EXTEND', 'File length mismatch: '// &
     &         'the length of extension file '//TRIM(DATA_EXTN_FIL)// &
     &         ' does not correspond to the length comptued from its name' )
           RETURN 
      END IF
!
! --- Copy the file into a temporary 
!
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR(1:8), FMT='(I8)') GETPID()
      CALL CHASHL ( STR )
      IF ( FL_INPLACE ) THEN
           FIL_TMP = TRIM(FIL_BSPD)//'__'//TRIM(STR)
         ELSE
           IDB = LINDEX ( FIL_BSPD,      '/' )
           IDD = LINDEX ( DATA_EXTN_FIL, '/' )
           IF ( IDD == 0 ) THEN
                FIL_TMP = TRIM(FIL_BSPD(IDB+1:))//'__'//TRIM(STR)
              ELSE
                FIL_TMP = DATA_EXTN_FIL(1:IDD)//TRIM(FIL_BSPD(IDB+1:))//'__'//TRIM(STR)
           END IF
      END IF
!
      IS = SYSTEM ( 'cp '//TRIM(FIL_BSPD)//' '//TRIM(FIL_TMP)//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR(STR)
           CALL ERR_LOG ( 5478, IUER, 'BSPD_EXTEND', 'Error in making '// &
     &         ' a temporary copy of file '//TRIM(FIL_BSPD)//' : '//STR )
           RETURN 
      END IF
!
! --- Open the input file with slant path delay segment
!
      LUN_IN  = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( DATA_EXTN_FIL, 'OLD', LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5479, IUER, 'BSPD_EXTEND', 'Error in opening '// &
     &         'temporary copy of file '//FIL_BSPD )
           RETURN 
      END IF
!
! --- Allocate memory for the delay buffer
!
      ALLOCATE ( DEL_BIN(LEN_EXTN), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( LEN_EXTN, STR )
           CALL ERR_LOG ( 5480, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'an attempt to allocate '//TRIM(STR)//' bytes of '// &
     &          'dynamic memory for the extracted delay array' )
           RETURN 
      END IF
!
! --- Read delay section
!
      IS = READ ( %VAL(LUN_IN), DEL_BIN, %VAL(LEN_EXTN) )
      IF ( IS .NE. LEN_EXTN ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5481, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'an attempt to read the input file '//TRIM(FIL_BSPD)// &
     &          ' : '//STR )
           RETURN 
      END IF
!
! --- Close the input file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_IN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5482, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'an attempt to close input file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
      IS = FILE_INFO ( TRIM(FIL_TMP)//CHAR(0), UNIX_DATE, LEN_FIL )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5483, IUER, 'BSPD_EXTEND', 'Error in inquiring '// &
     &         'extention file '//DATA_EXTN_FIL )
           RETURN 
      END IF
!
! --- Open the output file with slant path delay 
!
      LUN_OUT  = 10
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FIL_TMP, 'UNKNOWN', LUN_OUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5484, IUER, 'BSPD_EXTEND', 'Error in opening '// &
     &         'temporary copy of file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Seek the beginning of the LAB record
!
      IS = LSEEK ( %VAL(LUN_OUT), %VAL(0), %VAL(SEEK_SET) )
      IF ( IS .NE. 0 ) THEN
           CALL ERR_LOG ( 5485, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to position the input file '// &
     &           TRIM(FIL_TMP)//' to the LAB_REC section' )
           RETURN 
      END IF
!
! --- Update the lab record
!
      SPD_3D_DEL%TIM%MJD_END = EXTN_MJD_END
      SPD_3D_DEL%TIM%TAI_END = EXTN_TAI_END
      SPD_3D_DEL%LAB%TOT_NUM_DEL = SPD_3D_DEL%LAB%TOT_NUM_DEL + NREC_EXTN
!
      IS = WRITE ( %VAL(LUN_OUT), SPD_3D_DEL%LAB, %VAL(SIZEOF(SPD_3D_DEL%LAB)) )
      IF ( IS .NE. SIZEOF(SPD_3D_DEL%LAB) ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5486, IUER, 'BSPD_TRUNCATE', 'Error in '// &
     &          'an attempt to write into output file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Seek the TIM section
!
      IS = LSEEK ( %VAL(LUN_OUT), %VAL(SPD_3D_DEL%LAB%OFF_TIM), %VAL(SEEK_SET) )
      IF ( IS .NE. SPD_3D_DEL%LAB%OFF_TIM ) THEN
           CALL ERR_LOG ( 5487, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'seeking the TIM section in the temporary file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Write the TIM section
!
      IS = WRITE ( %VAL(LUN_OUT), SPD_3D_DEL%TIM, %VAL(SPD_3D_DEL%LAB%LEN_TIM) )
      IF ( IS .NE. SPD_3D_DEL%LAB%LEN_TIM ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 5488, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'an attempt to write into output file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Seek the end of the file
!
      IS = LSEEK ( %VAL(LUN_OUT), %VAL(LEN_FIL), %VAL(SEEK_SET) )
      IF ( IS .NE. LEN_FIL ) THEN
           CALL CLRCH  ( STR )
           CALL INCH8  ( LEN_FIL, STR )
           CALL ERR_LOG ( 5489, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'seeking the end of the input file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Write delay section
!
      IS = WRITE ( %VAL(LUN_OUT), DEL_BIN, %VAL(LEN_EXTN) )
      IF ( IS .NE. LEN_EXTN ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5490, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'an attempt to write into the output file '//FIL_TMP )
           RETURN 
      END IF
      DEALLOCATE ( DEL_BIN )
!
! --- Close the output file
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN_OUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5491, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'an attempt to close file '//FIL_TMP )
           RETURN 
      END IF
!
! --- Generate the name of the output file and rename the 
! --- temporary file
!
      IF ( FL_INPLACE ) THEN
           FIL_OUT = FIL_BSPD
           IS = RENAME ( TRIM(FIL_TMP)//CHAR(0), TRIM(FIL_BSPD)//CHAR(0) )
         ELSE
           IL = ILEN(FIL_TMP)
           FIL_OUT = FIL_TMP(1:IL-9)
           IS = RENAME ( TRIM(FIL_TMP)//CHAR(0), TRIM(FIL_OUT)//CHAR(0) )
      END IF
!
      IF ( IS .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR(STR)
           CALL ERR_LOG ( 5492, IUER, 'BSPD_EXTEND', 'Error in '// &
     &          'an attempt to move the output file '//TRIM(FIL_TMP)// &
     &          ' to '//TRIM(FIL_OUT) )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BSPD_EXTEND !#!#
