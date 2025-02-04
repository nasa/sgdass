      SUBROUTINE RESOLVE_SESSCODE ( SESS_CODE, DATE_TAG, MASTER_DIR, DB_NAME, &
     &                              IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RESOLVE_SESSCODE  resolves the session code: it finds the *
! *   database name which corresponds to experiment with this code.      *
! *   In order to do it, the routine reads all master files with IVS     *
! *   multi-agency schedules which it expects to find in the MASTER_DIR  *
! *   directory. Variable DATE_TAG may be either empty or contain the    *
! *   date of the experiment. In the latter case  RESOLVE_SESSCODE       *
! *   reads not all master files files but only the files of that year.  *
! *   In the case of success it returns database name (for X-band),      *
! *   otherwise it sets an error completion code.                        *
! *                                                                      *
! *   If DATA_TAG is specified then the routine finally makes a check:   *
! *   whether the database name corresponds to the DATE_TAG.             *
! *   The database filename contains the date. This date should be the   *
! *   same as DATE_TAG.                                                  *
! *                                                                      *
! *   It is assumed that the MASTER_DIR directory contains only master   *
! *   files and nothing else. Otherwise RESOLVE_SESSCODE may look at the *
! *   wrong file and derail with strange error messages.                 *
! *                                                                      *
! *   The current version supports master file format 1.0 of 2001.08.21  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  SESS_CODE ( CHARACTER ) -- Session code in letters of lower         *
! *                             register.                                *
! *   DATE_TAG ( CHARACTER ) -- Date of the experiment in Solve format:  *
! *                             yyyy.mm.dd  (f.e. 2001.01.08 )           *
! * MASTER_DIR ( CHARACTER ) -- The directory name on local machine      *
! *                             where master files are located. These    *
! *                             files can be downloaded from the IVS     *
! *                             Data Center by get_master.f subroutine.  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    DB_NAME ( CHARACTER ) -- Database name for the X-band database    *
! *                             with preceding dollar sign. DB_NAME is   *
! *                             the empty line in the case of failure to *
! *                             resolve session name.                    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       GEX ( RECORD    ) -- Data structure which keeps settings       *
! *                            for geo_export, parameters specified      *
! *                            by user in the arguments line and some    *
! *                            internal variables.                       *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 02-JAN-2001 RESOLVE_SESSCODE v3.0 (c) L. Petrov 28-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  SESS_CODE*(*), DATE_TAG*(*), MASTER_DIR*(*), DB_NAME*(*)
      INTEGER*4  IUER
      CHARACTER  STR*128, SESS_CODE_TRIAL*6, MASTER_NAM*256, MASTER_SESS*32, &
     &           MASTER_FIL*256, MASTER_YEAR*2, MASTER_YEAR_4D*4, &
     &           SUFFIX*32, MON_DAY*32
      CHARACTER  VER1_SIGNATURE*64, VER2_SIGNATURE*80, YEAR_4D_CHR*4
      PARAMETER  ( VER1_SIGNATURE = &
     &  '## Master file format version 1.0           2001.08.21 CCT&NRV  ' )
      PARAMETER  ( VER2_SIGNATURE = &
     &  '## Master file format version 2.0                             2022.11.01 CAD&CCT' )
      LOGICAL*4  FL_MAS
      INTEGER*4  SES__FLD1, DAT__FLD1, CAT__FLD1, SES__FLD2, DAT__FLD2, CAT__FLD2, M__MOU, M__SUF
      PARAMETER  ( SES__FLD1 =  2 )
      PARAMETER  ( DAT__FLD1 =  3 )
      PARAMETER  ( CAT__FLD1 = 12 )
      PARAMETER  ( SES__FLD2 =  3 )
      PARAMETER  ( DAT__FLD2 =  2 )
      PARAMETER  ( CAT__FLD2 = 11 )
      PARAMETER  ( M__MOU   = 12 )
      PARAMETER  ( M__SUF   =  5 )
      INTEGER*4  MBUF, STATB(12), IP, IS, ICB, ICE, IMB, IME, IMON, &
     &           ISB, ISE, IYEAR, NBUF, IYEAR_DBN, IMON_DBN, IDAY_DBN, &
     &           IYEAR_TAG, IMON_TAG, IDAY_TAG, J1, J2, IVER_FMT, IER
      ADDRESS__TYPE :: DIR_DESC
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  BUF(MBUF)*256, MOU(12)*3
      DATA       MOU / &
     &                 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', &
     &                 'JUL', 'AUG', 'SEP', 'OCT', 'NOV', &
     &               'DEC'/
      INTEGER*4, EXTERNAL :: READDIR, FOR_STAT, ILEN, I_LEN, LTM_DIF, MULTI_INDEX
      ADDRESS__TYPE, EXTERNAL :: OPENDIR, CLOSEDIR
!
      FL_MAS = .FALSE.
      CALL CLRCH ( DB_NAME )
      CALL CLRCH ( SESS_CODE_TRIAL )
      CALL TRAN  ( 12, SESS_CODE, SESS_CODE_TRIAL )
!
! --- Open directory
!
      DIR_DESC = OPENDIR ( MASTER_DIR(1:I_LEN(MASTER_DIR))//CHAR(0) )
      DO 410 J1=1,1024*1024
!
! ------ Read the next line of the directory file
!
         IP = READDIR ( %VAL(DIR_DESC) )
         IF ( IP .EQ. 0 ) GOTO 810
!
! ------ Extract the filename form the internal data structures
!
         CALL GET_NAME_FROM_DIR ( %VAL(IP), MASTER_NAM )
!
! ------ Check, whether this file has a valid name for the master file.
! ------ Master files should have names like:
! ------ master??.txt
! ------ master??-int.txt
!
         IF ( ( ILEN(MASTER_NAM) .EQ. 12       .AND. &
     &          MASTER_NAM(1:6)  .EQ. 'master' .AND. &
     &          MASTER_NAM(9:12) .EQ. '.txt'         ) .OR. &
     &        ( ILEN(MASTER_NAM) .EQ. 16       .AND. &
     &          MASTER_NAM(1:6)  .EQ. 'master' .AND. &
     &          MASTER_NAM(9:16) .EQ. '-int.txt'     )    ) THEN
              IVER_FMT = 1
           ELSE IF ( ( ILEN(MASTER_NAM) .EQ. 14       .AND. &
     &          MASTER_NAM(1:6)  .EQ. 'master' .AND. &
     &          MASTER_NAM(11:14) .EQ. '.txt'         ) .OR. &
     &        ( ILEN(MASTER_NAM) .EQ. 18       .AND. &
     &          MASTER_NAM(1:6)  .EQ. 'master' .AND. &
     &          MASTER_NAM(11:18) .EQ. '-int.txt'     )    ) THEN
              IVER_FMT = 2
            ELSE 
!
! ----------- We don't consider files which do not conform master-file naming
! ----------- convention
!
              GOTO 410
         END IF
!
         CALL CLRCH ( MASTER_FIL )
         IF ( MASTER_FIL(I_LEN(MASTER_FIL):I_LEN(MASTER_FIL)) == '/' ) THEN
              MASTER_FIL = MASTER_DIR(1:I_LEN(MASTER_DIR))//MASTER_NAM
            ELSE
              MASTER_FIL = MASTER_DIR(1:I_LEN(MASTER_DIR))//'/'//MASTER_NAM
         END IF
!
! ------ Check whether the file has non-zero length
!
         IS = FOR_STAT ( MASTER_FIL, STATB )
         IF ( STATB(8) .LE. 0 ) GOTO 410
!
! ------ The the year to which this master files corresponds
!
         IF ( IVER_FMT == 1 ) THEN 
              IF ( INDEX ( MASTER_NAM, '-int.txt' ) .GT. 0 .AND. &
     &             ILEN(MASTER_NAM) .GE. 10 ) THEN
                   MASTER_YEAR = MASTER_NAM(ILEN(MASTER_NAM)-9:)
                 ELSE IF ( ILEN(MASTER_NAM) .GE. 6 ) THEN
                   MASTER_YEAR = MASTER_NAM(ILEN(MASTER_NAM)-5:)
                 ELSE
                   CALL ERR_LOG ( 4851, IUER, 'RESOLVE_SESSCODE', 'Wrong master '// &
     &                 'file name '//MASTER_NAM )
                   RETURN
              END IF
!
! ----------- Check the year of the master file
!
              CALL CHIN ( MASTER_YEAR, IYEAR )
              IF ( IYEAR .LT. 0  .OR. IYEAR .GT. 99 ) THEN
                   CALL ERR_LOG ( 4852, IUER, 'RESOLVE_SESSCODE', 'Wrong master '// &
     &                 'file name '//MASTER_NAM )
                   RETURN
              END IF
            ELSE IF ( IVER_FMT == 2 ) THEN 
              IF ( INDEX ( MASTER_NAM, '-int.txt' ) .GT. 0 .AND. &
     &             ILEN(MASTER_NAM) .GE. 12 ) THEN
                   MASTER_YEAR_4D = MASTER_NAM(ILEN(MASTER_NAM)-9:ILEN(MASTER_NAM)-6)
                 ELSE IF ( ILEN(MASTER_NAM) .GE. 8 ) THEN
                   MASTER_YEAR_4D = MASTER_NAM(ILEN(MASTER_NAM)-5:ILEN(MASTER_NAM)-2)
                 ELSE
                   CALL ERR_LOG ( 4853, IUER, 'RESOLVE_SESSCODE', 'Wrong master '// &
     &                 'file name '//MASTER_NAM )
                   RETURN
              END IF
!
! ----------- Check the year of the master file
!
              CALL CHIN ( MASTER_YEAR_4D, IYEAR )
              IF ( IYEAR .LT. 1970  .OR. IYEAR .GT. 2099 ) THEN
                   CALL ERR_LOG ( 4854, IUER, 'RESOLVE_SESSCODE', 'Wrong master '// &
     &                 'file name '//MASTER_NAM )
                   RETURN
              END IF
              MASTER_YEAR = MASTER_YEAR_4D(3:4)
         END IF
!
         IF ( ILEN(DATE_TAG) .GT. 0   .AND.   LEN(DATE_TAG) .GE. 4 ) THEN
!
! ----------- If DATE_TAG is not empty we compare the year from the DATE_TAG
! ----------- with the year of the master file. If they are different we
! ----------- skip this master file
!
              IF ( MASTER_YEAR .NE. DATE_TAG(3:4) ) GOTO 410
         END IF
!
! ------ Read this master file
!
         FL_MAS = .TRUE.
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( MASTER_FIL, MBUF, BUF, NBUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 4853, IUER, 'RESOVLE_SESSCODE', 'Error in '// &
     &            'attempt to read schedule master file '//MASTER_FIL )
              RETURN
         END IF
!
         IF ( BUF(1)(1:64) .EQ. VER1_SIGNATURE ) THEN
              CONTINUE 
            ELSE IF ( BUF(1)(1:80) .EQ. VER2_SIGNATURE ) THEN
              CONTINUE 
            ELSE 
              CALL TRAN  ( 13, BUF(1)(1:64), STR(1:64) )
              CALL ERR_LOG ( 4854, IUER, 'RESOLVE_SESSCODE', 'Master file '// &
     &             MASTER_FIL(1:I_LEN(MASTER_FIL))//' has an '// &
     &            'unrecognizable format. The first line has an unknown '// &
     &            'signature: '//STR(1:80) )
              RETURN
         END IF
!
! ------ Parse the buffer with contents of master file
!
         DO 420 J2=1,NBUF
            IF ( BUF(J2)(1:1) .NE. '|' ) GOTO 420
!
! --------- Extract the suffix field and convert to the upper register
!
            IF ( IVER_FMT == 1 ) THEN
                 ISB = MULTI_INDEX ( SES__FLD1,   BUF(J2), '|' ) +1
                 ISE = MULTI_INDEX ( SES__FLD1+1, BUF(J2), '|' ) -1
                 IF ( ISE .LT. ISB ) ISB = ISE
                 IF ( ISE .LE. 0   ) GOTO 420
              ELSE IF ( IVER_FMT == 2 ) THEN
                 ISB = MULTI_INDEX ( SES__FLD2,   BUF(J2), '|' ) +1
                 ISE = MULTI_INDEX ( SES__FLD2+1, BUF(J2), '|' ) -1
                 IF ( ISE .LT. ISB ) ISB = ISE
                 IF ( ISE .LE. 0   ) GOTO 420
            END IF
            CALL CLRCH  (                       MASTER_SESS )
            CALL TRAN   ( 12, BUF(J2)(ISB:ISE), MASTER_SESS )
            CALL CHASHL (                       MASTER_SESS )
            IF ( MASTER_SESS .EQ. SESS_CODE_TRIAL ) THEN
!
! -------------- Very well! We found the experiment!
!
! -------------- Extract the day and month field and convert
! -------------- to the upper register
!
                 IF ( IVER_FMT == 1 ) THEN
                      IMB = MULTI_INDEX ( DAT__FLD1,   BUF(J2), '|' ) + 1
                      IME = MULTI_INDEX ( DAT__FLD1+1, BUF(J2), '|' ) - 1
                      IF ( IME .LT. IMB ) IMB = IME
                      IF ( IME .LE. 0   ) GOTO 420
                      CALL CLRCH  (                       MON_DAY )
                      CALL TRAN   ( 11, BUF(J2)(IMB:IME), MON_DAY )
                      CALL CHASHL (                       MON_DAY )
                    ELSE IF ( IVER_FMT == 2 ) THEN
                      IMB = MULTI_INDEX ( DAT__FLD2,   BUF(J2), '|' ) + 1
                      IME = MULTI_INDEX ( DAT__FLD2+1, BUF(J2), '|' ) - 1
                      IF ( IME .LT. IMB ) IMB = IME
                      IF ( IME .LE. 0   ) GOTO 420
                      CALL CHIN ( BUF(J2)(IMB+4:IMB+5), IMON )
                      MON_DAY = MOU(IMON)//BUF(J2)(IMB+6:IMB+7)
                 END IF
!
! -------------- Extract the suffix field and convert to the upper register
!
                 IF ( IVER_FMT == 1 ) THEN
                      ICB = MULTI_INDEX ( CAT__FLD1,   BUF(J2), '|' ) +1
                      ICE = MULTI_INDEX ( CAT__FLD1+1, BUF(J2), '|' ) -1
                    ELSE
                      ICB = MULTI_INDEX ( CAT__FLD2,   BUF(J2), '|' ) +1
                      ICE = MULTI_INDEX ( CAT__FLD2+1, BUF(J2), '|' ) -1
                 END IF
                 IF ( ICE .LT. ICB ) ICB = ICE
                 IF ( ICE .LE. 0   ) GOTO 420
                 CALL CLRCH  (                       SUFFIX )
                 CALL TRAN   ( 11, BUF(J2)(ICB:ICE), SUFFIX )
                 CALL CHASHL (                       SUFFIX )
!
                 DB_NAME = '$'//MASTER_YEAR//MON_DAY(1:5)//SUFFIX(1:2)
            END IF
 420     CONTINUE
 410  CONTINUE
 810  CONTINUE
!
! --- Close directory
!
      IP = CLOSEDIR ( %VAL(DIR_DESC) )
      IF ( .NOT. FL_MAS  .AND.  ILEN(DATE_TAG) .GT. 0 ) THEN
           CALL ERR_LOG ( 4854, IUER, 'RESOLVE_SESSCODE', 'No masterfile '// &
     &         'for the date '//DATE_TAG(1:I_LEN(DATE_TAG))//' was found '// &
     &         'in directory '//MASTER_DIR )
           RETURN
      END IF
!
      IF ( .NOT. FL_MAS ) THEN
           CALL ERR_LOG ( 4855, IUER, 'RESOLVE_SESSCODE', 'No masterfile '// &
     &         'was found in the directory '//MASTER_DIR )
           RETURN
      END IF
!
      IF ( ILEN(DB_NAME) .EQ. 0  .AND.  ILEN(DATE_TAG) .GT. 0 ) THEN
           CALL ERR_LOG ( 4856, IUER, 'RESOLVE_SESSCODE', 'No database name '// &
     &         'which would match session code '// &
     &          SESS_CODE(1:I_LEN(SESS_CODE))//' and date '// &
     &          DATE_TAG(1:I_LEN(DATE_TAG))//' was found in the master files '// &
     &         'located in the directory '//MASTER_DIR )
           RETURN
      END IF
!
      IF ( ILEN(DB_NAME) .EQ. 0 ) THEN
           CALL ERR_LOG ( 4857, IUER, 'RESOLVE_SESSCODE', 'No database name '// &
     &         'which would match session code '// &
     &          SESS_CODE(1:I_LEN(SESS_CODE))//' was found in the master '// &
     &         'files located in the directory '//MASTER_DIR )
           RETURN
      END IF
!
      IF ( ILEN(DATE_TAG) .GE. 10 ) THEN
!
! -------- Check whether database name and date tag agree. Since the database
! -------- name contains the date we are able to check their consistency
!
           IMON_DBN  = LTM_DIF ( 0, 12, MOU, DB_NAME(4:6) )
           CALL CHIN ( DB_NAME(2:3), IYEAR_DBN )
           CALL CHIN ( DB_NAME(7:8), IDAY_DBN  )
           IF ( IYEAR_DBN .GE. 0  .AND.  IYEAR_DBN .LE. 69 ) THEN
                IYEAR_DBN = 2000 + IYEAR_DBN
              ELSE IF ( IYEAR_DBN .GE. 70  .AND.  IYEAR_DBN .LE. 99 ) THEN
                IYEAR_DBN = 1900 + IYEAR_DBN
           END IF
           CALL CHIN ( DATE_TAG(1:4),  IYEAR_TAG )
           CALL CHIN ( DATE_TAG(6:7),  IMON_TAG  )
           CALL CHIN ( DATE_TAG(9:10), IDAY_TAG  )
           IF ( IYEAR_DBN .NE. IYEAR_TAG .OR. &
     &          IMON_DBN  .NE. IMON_TAG  .OR. &
     &          IDAY_DBN  .NE. IDAY_TAG       ) THEN
                CALL ERR_LOG ( 4858, IUER, 'RESOLVE_SESCODE', 'The database '// &
     &              'name '//DB_NAME(1:10)//' which correspond to the '// &
     &              'experiment '//SESS_CODE(1:I_LEN(SESS_CODE))//' DOES NOT '// &
     &              'correspond to the date '//DATE_TAG(1:I_LEN(DATE_TAG)) )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  RESOLVE_SESSCODE  !#!#
