      SUBROUTINE CEX_CHECK ( CEX, DATA_DIR, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  CEX_CHECK  checks correctness of the directory name and   *
! *   calls routine CEX_MK4DIR_CHECK which checks correctness of         *
! *   directory content.                                                 *
! *                                                                      *
! *  ### 07-APR-2005   CEX_CHECK   v1.3 (c)  L. Petrov  03-SEP-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'corel_export.i'
      TYPE      ( CEX__TYPE ) :: CEX
      CHARACTER  DATA_DIR*(*)
      INTEGER*4  IVRB, IUER
      CHARACTER  NAME*128, STR*512, MASTER_FILE*255, MATCH_EXP*8, &
     &           MATCH_EXP_UC*8, NAME_UC*128, MATCH_DAT*5, &
     &           EXP_NAM*8, EXP_NAM_UC*8, EXP_DAT*5, MON(12)*3, REG*5
      DATA       MON / &
     &                 'JAN',  'FEB',  'MAR',  'APR',  'MAY',  'JUN', &
     &                 'JUL',  'AUG',  'SEP',  'OCT',  'NOV',  'DEC'  &
     &               /
      INTEGER*4  M_BUF
      INTEGER*4  SES__FLD, DAT__FLD
      PARAMETER  ( SES__FLD =  2 )
      PARAMETER  ( DAT__FLD =  3 )
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//'|,' )
      PARAMETER  ( M_BUF = 1024 )
      LOGICAL*4  LEX
      CHARACTER  BUF(M_BUF)*256
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4  ICB, ICE, IMB, IME, IL, IDAY, IMON, IP, IPT, IYEAR, J1, &
     &           NBUF, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, MULTI_INDEX
      ADDRESS__TYPE, EXTERNAL :: CLOSEDIR, OPENDIR 
!
      IL = ILEN(DATA_DIR)
      IF ( IL < 14 ) THEN
           CALL ERR_LOG ( 4821, IUER, 'CEX_CHECK', 'Directory name is too '// &
     &         'short, less than 14 characters' )
           RETURN 
      END IF
      IF ( DATA_DIR(1:1) .NE. '/' ) THEN
           DATA_DIR = './'//DATA_DIR
           IL = ILEN(DATA_DIR)
      END IF
!
! --- Check whether directory DATA_DIR exists
!
      DIR_DESC = OPENDIR ( DATA_DIR(1:I_LEN(DATA_DIR))//CHAR(0) )
      IF ( DIR_DESC .LE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4841, IUER, 'CEX_CHECK', 'Error in attempt '// &
     &         'to read directory '//DATA_DIR(1:I_LEN(DATA_DIR))// &
     &         '  OPRENDIR: '//STR )
           RETURN
         ELSE
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
      IP = LINDEX ( DATA_DIR(1:IL-1), '/' )
      NAME = DATA_DIR(IP+1:IL)
!
! --- Check year
!
      CALL CHIN ( NAME(1:4), IYEAR )
      IF ( IYEAR > 1970  .AND.  IYEAR < 2050 ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 4822, IUER, 'CEX_CHECK', 'Incorrect directory '// &
     &                   'name '//NAME(1:I_LEN(NAME))//' the first 4 '// &
     &                   'charaters should be the experiment year in '// &
     &                   'the range (1970, 2050)' )
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Check month
!
      CALL CHIN ( NAME(6:7), IMON )
      IF ( IMON .GE. 1  .AND.  IMON .LE. 12 ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 4823, IUER, 'CEX_CHECK', 'Incorrect directory '// &
     &                   'name '//NAME(1:I_LEN(NAME))//' the field '// &
     &                   'month should be the experiment month in '// &
     &                   'the range (1, 12)' )
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Check month
!
      CALL CHIN ( NAME(9:10), IDAY )
      IF ( IDAY .GE. 1  .AND.  IDAY .LE. 31 ) THEN
           CONTINUE 
         ELSE 
           CALL ERR_LOG ( 4824, IUER, 'CEX_CHECK', 'Incorrect directory '// &
     &                   'name '//NAME(1:I_LEN(NAME))//' the field '// &
     &                   'day should be the experiment day in '// &
     &                   'the range (1, 31)' )
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( NAME(5:5)   .NE. '_' .OR. &
     &     NAME(8:8)   .NE. '_' .OR. &
     &     NAME(11:11) .NE. '_'      ) THEN
           CALL ERR_LOG ( 4825, IUER, 'CEX_CHECK', 'Incorrect directory '// &
     &                   'name '//NAME(1:I_LEN(NAME))//' underscore should '// &
     &                   'be used as a delimiter separator' )
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Build master filename
!
      CALL CLRCH ( MASTER_FILE )
      MASTER_FILE = CEX%MASTER_DIR(1:I_LEN(CEX%MASTER_DIR))//'/master'// &
     &              NAME(3:4)//'.txt'
!
! --- Check whether the file with analysis centers codes exists
!
      INQUIRE ( FILE = MASTER_FILE, EXIST = LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4826, IUER, 'CEX_CHECK', 'Master file '// &
     &                     MASTER_FILE(1:I_LEN(MASTER_FILE))//' has not '// &
     &                    'been found' )
           RETURN
      END IF
!
! --- Read master-file with analysis centers names and solutions codes
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( MASTER_FILE, M_BUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4827, IUER, 'CEX_CHECK', 'Error in an attempt to '// &
     &         'read master file '//MASTER_FILE(1:I_LEN(MASTER_FILE))// &
     &         ' has not been found' )
           RETURN
      END IF
!
      IPT = ILEN(NAME)
      IF ( NAME(IPT:IPT) == '/' ) IPT =IPT-1
      MATCH_DAT = MON(IMON)//NAME(9:10)
      CALL TRAN ( 11, MATCH_DAT, MATCH_DAT )
      MATCH_EXP = NAME(12:IPT)
      MATCH_EXP_UC = MATCH_EXP 
      CALL TRAN ( 11, MATCH_EXP_UC, MATCH_EXP_UC )
      DO 410 J1=1,NBUF
!
! ------ Extract month/day
!
         IMB = MULTI_INDEX ( DAT__FLD,   BUF(J1), '|' ) +1
         IME = MULTI_INDEX ( DAT__FLD+1, BUF(J1), '|' ) -1
         IF ( IME .LT. IMB ) IMB = IME
         IF ( IME .LE. 0   ) GOTO 410
         CALL CLRCH  (                       EXP_DAT )
         CALL TRAN   ( 11, BUF(J1)(IMB:IME), EXP_DAT )
         CALL CHASHL (                       EXP_DAT )
!
! ------ Extract the experiment name
!
         ICB = MULTI_INDEX ( SES__FLD,   BUF(J1), '|' ) +1
         ICE = MULTI_INDEX ( SES__FLD+1, BUF(J1), '|' ) -1
         IF ( ICE .LT. ICB ) ICB = ICE
         IF ( ICE .LE. 0   ) GOTO 410
         CALL CLRCH  (                       EXP_NAM )
         CALL TRAN   ( 12, BUF(J1)(ICB:ICE), EXP_NAM )
         CALL CHASHL (                       EXP_NAM )
!
         IF ( MATCH_EXP == EXP_NAM  .AND.  MATCH_DAT == EXP_DAT ) THEN
              GOTO 810
              RETURN 
         END IF
!
! ------ Check: whether a user accidentally named the directory in upper case?
!
         EXP_NAM_UC = EXP_NAM
         CALL TRAN ( 11, EXP_NAM_UC, EXP_NAM_UC )
         IF ( MATCH_EXP_UC == EXP_NAM_UC  .AND.  MATCH_DAT == EXP_DAT ) THEN
              NAME_UC = NAME 
              CALL TRAN ( 11, NAME_UC, NAME_UC ) 
              CALL ERR_LOG ( 4828, IUER, 'CEX_CHECK', 'Please rename '// &
     &            'directory '//NAME(1:I_LEN(NAME))//' into '// &
     &             NAME_UC(1:I_LEN(NAME_UC))//' -- according '// &
     &            'to specifications directory name should be in lower case ' )
              RETURN 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 4829, IUER, 'CEX_CHECK', 'The name '// &
     &     MATCH_EXP(1:I_LEN(MATCH_EXP))//' which corresponds directory '// &
     &     NAME(1:I_LEN(NAME))//' was not found in the master file. Keep '// &
     &    'in mind: the name of the directory should be in form '// &
     &    'YYYY_MM_DD_eeeee where YYYY_MM_DD is the nominal start date '// &
     &    'of the experiment and eeeee is the session code in lower case. '// &
     &    'Please, check the name against the master file' )
 810  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL CEX_MK4DIR_CHECK ( DATA_DIR, CEX, IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4830, IUER, 'CEX_CHECK', 'Error in integerity '// &
     &         'test of direcotry '//DATA_DIR )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CEX_CHECK
