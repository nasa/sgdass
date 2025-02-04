      SUBROUTINE READ_OPC ( OPC_FILE, OPA, DB_NAME, SESS_CODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_OPC  reads operational analysis control file         *
! *   OPC_FILE and fills appropriate fields of the data structure  OPA.  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  OPC_FILE ( CHARACTER ) -- Operational analysis control file.        *
! *   DB_NAME ( CHARACTER ) -- Database name (without leading dollar     *
! *                            sign )                                    *
! * SESS_CODE ( CHARACTER ) -- Session code as defined in master file.   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     OPA ( RECORD         ) -- Data structure which keeps settings    *
! *                               of OPA and current information related *
! *                               to processing this session.            *
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
! *  ### 14-AUG-2000    READ_OPC   v1.6 (c)  L. Petrov  20-DEC-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'opa.i'
      CHARACTER  OPC_FILE*(*), DB_NAME*(*), SESS_CODE*(*)
      TYPE ( OPA__STRU ) ::  OPA
      INTEGER*4  IUER
      INTEGER*4  MBUF, MIND
      PARAMETER  ( MBUF = 256, MIND = 128 )
      CHARACTER  BUF(MBUF)*256, DELIM*3, STR*80, WORD1*80, WORD2*80, WORD3*80
      INTEGER*4  IND(2,MIND), LIND, J1, NBUF, IER
      REAL*8     VERS, EPS_VERS
      PARAMETER  ( EPS_VERS = 0.001D0 )
      PARAMETER  ( DELIM = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Read configuration file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( OPC_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4161, IUER, 'READ_OPC', 'Error in attempt '// &
     &         'to open Operational Contrpl File '//OPC_FILE )
           RETURN
      END IF
!
! --- Check a signature "OPC ver." which would allow to suggest whether this
! --- file is in OPC format
!
      IF ( BUF(1)(1:7) .EQ. 'OPC ver.' ) THEN
           CALL ERR_LOG ( 4162, IUER, 'READ_OPC', 'File '// &
     &          OPC_FILE(1:I_LEN(OPC_FILE))//' is not in OPC format' )
           RETURN
      END IF
!
! --- Read version field
!
      READ ( BUF(1)(10:13), FMT='(F4.0)' ) VERS
      IF ( ( VERS - OPC__VER_LAST) .LT. -EPS_VERS  .OR. &
     &       BUF(1)(1:13) .GT. OPC__LABEL ) THEN
!
! -------- Version mismatch: session file has too old version, or, contrary,
! -------- the configuration file has been created by the future version of OPA.
! -------- Then we create file OPA control for this session anew...
!
           CALL ERR_PASS   ( IUER, IER )
           CALL CREATE_OPC ( OPC_FILE, DB_NAME, SESS_CODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4163, IUER, 'READ_OPC', 'Error in attempt '// &
     &              'to create a new file of operational control for '// &
     &              'database '//DB_NAME(1:I_LEN(DB_NAME))//' -- '//OPC_FILE )
                RETURN
           END IF
!
! -------- Then read that what we created
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( OPC_FILE, MBUF, BUF, NBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4164, IUER, 'READ_OPC', 'Error in attempt '// &
     &              'to open Operational Contrpl File '//OPC_FILE )
                RETURN
           END IF
      END IF
!
      IF ( BUF(1)(1:13) == OPC__LABEL_21 ) THEN
           OPA%STS(OPA__EOM) = 'N'
           OPA%STS(OPA__EOM) = 'N'
           OPA%ACT(OPA__SBM) = '-'
           OPA%ACT(OPA__SBM) = '-'
      END IF
!
! --- Scan the buffer with copy of configuration file
!
      DO 410 J1=2,NBUF
!
! ------ Skip comments
!
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         IF ( BUF(J1)(1:1)  .EQ. '#'  ) GOTO 410
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, -3 )
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ It should be exactly three words. Let's check it
!
         IF ( LIND .LT. 2 ) THEN
              CALL ERR_LOG ( 4165, IUER, 'READ_OPC', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'Operational configuration File '// &
     &             OPC_FILE(1:I_LEN(OPC_FILE))//': too few words' )
              RETURN
            ELSE IF ( LIND .EQ. 2 ) THEN
!
! ----------- There were not the third word. Make it as a blank
!
              LIND = 3
              IND(1,3) = IND(2,2) + 1
              IND(2,3) = IND(2,2) + 1
         END IF
!
! ------ Well. Three words. Parse them.
!
! ------ Transform the keyword to the letters of upper case
!
         CALL CLRCH ( WORD1 )
         CALL CLRCH ( WORD2 )
         CALL CLRCH ( WORD3 )
!
         CALL TRAN ( 11, BUF(J1)(IND(1,1):IND(2,1)), WORD1 )
         CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), WORD2 )
         CALL TRAN ( 11, BUF(J1)(IND(1,3):IND(2,3)), WORD3 )
!
         IF ( WORD1 .EQ. 'EXPERIMENT:' ) THEN
              CALL CLRCH ( OPA%SESS_CODE )
              OPA%SESS_CODE = BUF(J1)(IND(1,2):IND(2,LIND))
            ELSE IF ( WORD1 .EQ. 'DATABASE:' ) THEN
              CALL CLRCH ( OPA%DB_NAME )
              OPA%DB_NAME = BUF(J1)(IND(1,2):IND(2,2))
            ELSE IF ( WORD1 .EQ. 'ARC_LINE:' ) THEN
              CALL CLRCH ( OPA%ARC_LINE )
              OPA%ARC_LINE = BUF(J1)(IND(1,2):IND(2,LIND))
            ELSE IF ( WORD1 .EQ. 'GLOBAL_ARCLIST_UPDATE:' ) THEN
              OPA%STS(OPA__GAL) = WORD2
              OPA%ACT(OPA__GAL) = WORD3
            ELSE IF ( WORD1 .EQ. 'SUPERFILE:' ) THEN
              OPA%STS(OPA__SUP) = WORD2
              OPA%ACT(OPA__SUP) = WORD3
            ELSE IF ( WORD1 .EQ. 'SITE_WEIGHTS:' ) THEN
              OPA%STS(OPA__STW) = WORD2
              OPA%ACT(OPA__STW) = WORD3
            ELSE IF ( WORD1 .EQ. 'BASELINE_WEIGHTS:' ) THEN
              OPA%STS(OPA__BAW) = WORD2
              OPA%ACT(OPA__BAW) = WORD3
            ELSE IF ( WORD1 .EQ. 'EOPS_SOLUTION:' .OR. &
     &                WORD1 .EQ. 'EOPI_SOLUTION:' .OR. &
     &                WORD1 .EQ. 'EOP_SOLUTION:'       ) THEN
              OPA%STS(OPA__EOS) = WORD2
              OPA%ACT(OPA__EOS) = WORD3
            ELSE IF ( WORD1 .EQ. 'MULTI-EOP_SOLUTION:' ) THEN
              OPA%STS(OPA__EOM) = WORD2
              OPA%ACT(OPA__EOM) = WORD3
            ELSE IF ( WORD1 .EQ. 'STANDALONE_SOLUTION:' ) THEN
              OPA%STS(OPA__STN) = WORD2
              OPA%ACT(OPA__STN) = WORD3
            ELSE IF ( WORD1 .EQ. 'EOPK_SERIES:' ) THEN
              OPA%STS(OPA__EOK) = WORD2
              OPA%ACT(OPA__EOK) = WORD3
            ELSE IF ( WORD1 .EQ. 'SNR_ANALYSIS:' ) THEN
              OPA%STS(OPA__SNR) = WORD2
              OPA%ACT(OPA__SNR) = WORD3
            ELSE IF ( WORD1 .EQ. 'DATABASE_SUBMISSION:' ) THEN
              OPA%STS(OPA__SBD) = WORD2
              OPA%ACT(OPA__SBD) = WORD3
            ELSE IF ( WORD1 .EQ. 'EOPS_SUBMISSION:' .OR. &
     &                WORD1 .EQ. 'EOPI_SUBMISSION:' .OR. &
     &                WORD1 .EQ. 'EOP_SUBMISSION:'       ) THEN
              OPA%STS(OPA__SBE) = WORD2
              OPA%ACT(OPA__SBE) = WORD3
            ELSE IF ( WORD1 .EQ. 'MULTI-EOP_SUBMISSION:' ) THEN
              OPA%STS(OPA__SBM) = WORD2
              OPA%ACT(OPA__SBM) = WORD3
            ELSE IF ( WORD1 .EQ. 'SINEX_SUBMISSION:' ) THEN
              OPA%STS(OPA__SNX) = WORD2
              OPA%ACT(OPA__SNX) = WORD3
            ELSE IF ( WORD1 .EQ. 'VDB_UPDATE:' ) THEN
              OPA%STS(OPA__VDB) = WORD2
              OPA%ACT(OPA__VDB) = WORD3
            ELSE
              CALL ERR_LOG ( 4166, IUER, 'READ_OPC', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'OPC control file '//OPC_FILE(1:I_LEN(OPC_FILE))// &
     &            ': unsupported keyword '//WORD1 )
              RETURN
         END IF
 410  CONTINUE
!
      IF ( ILEN(OPA%SESS_CODE) .EQ. 0 ) THEN
           CALL ERR_LOG ( 4167, IUER, 'READ_OPC', 'OPC control file does not '// &
     &         'contain session code' )
           RETURN
      END IF
!
      IF ( ILEN(OPA%DB_NAME) .EQ. 0 ) THEN
           CALL ERR_LOG ( 4168, IUER, 'READ_OPC', 'OPC control file does not '// &
     &         'contain database name' )
           RETURN
      END IF
!
      IF ( ILEN(OPA%ARC_LINE) .EQ. 0 ) THEN
           OPA%ARC_LINE = '! '//OPA%SESS_CODE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_OPC #!#
