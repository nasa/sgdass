      SUBROUTINE CONF_GAE ( FILCNF, EXTFMT, URLEXT, URLEXT2, FILEXT, FILEXT2, &
     &                      FILERP, FILOUT, FILUPM, WGET_EXE, FL_ROT, &
     &                      FL_EQUWEI, MJD_BEGROT, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  CONF_GAE  reads configuration file of use_finals       *
! *   program, parses it and extracts parameters which use_finals will   *
! *   be using: file format, fielname, mode names and so on.             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FILCNF    ( CHARACTER ) -- Name of the configuration file to be     *
! *                             read and parsed.                         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  EXTFMT    ( CHARACTER ) -- Format of the externl EOP file.          *
! *                             One of IERS_C04 or USNO_FINALS.          *
! *  URLEXT    ( CHARACTER ) -- URL of the external EOP file.            *
! *  URLEXT2   ( CHARACTER ) -- URL of the second external EOP file,     *
! *                             which if define will overwrite EOP       *
! *                             from the first file. This name can be    *
! *                             left blank if not specified in the       *
! *                             control file.                            *
! *  FILEXT    ( CHARACTER ) -- Name of the local temporary file where   *
! *                             the external EOP is to be written.       *
! *  FILEXT2   ( CHARACTER ) -- Name of the local temporary file where   *
! *                             the second external EOP is to be         *
! *                             written if defined.                      *
! *  FILERP    ( CHARACTER ) -- Name of the local reference erp file.    *
! *  FILOUT    ( CHARACTER ) -- Name of the local output eop file in     *
! *                             erp modfile-format.                      *
! *  FILUPM    ( CHARACTER ) -- Name of the local output eop file in     *
! *                             erp modfile-format.                      *
! *  WGET_EXE  ( CHARACTER ) -- Full name with path of program wget for  *
! *                             retrieving external file via ftp.        *
! *  FL_ROT    ( LOGICAL*4 ) -- Flag. If .TRUE. then reference file      *
! *                             should be read, linear regression should *
! *                             be computed and parameters of linear     *
! *                             trend should be subtracted from the      *
! *                             output EOP file.                         *
! *  FL_EQUWEI ( LOGICAL*4 ) -- Flag. If .TRUE. then equal weights are   *
! *                             to be used for computation of trend.     *
! *                             If not, then the weights produced by     *
! *                             sum in quadrature of the reported formal *
! *                             errors in external file and in the       *
! *                             reference file should be used.           *
! * MJD_BEGROT ( INTEGER*4 ) -- Modified Julian date of the first epoch  *
! *                             which should be used for computation of  *
! *                             linear regression.                       *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
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
! *  ### 04-DEC-2000    CONF_GAE   v3.0 (c)  L. Petrov  01-APR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FILCNF*(*), EXTFMT*(*),  URLEXT*(*), URLEXT2*(*), &
     &           FILEXT*(*), FILEXT2*(*), FILERP*(*), FILOUT*(*),  &
     &           FILUPM*(*), WGET_EXE*(*)
      LOGICAL*4  FL_ROT, FL_EQUWEI
      INTEGER*4  MJD_BEGROT, IUER
      INTEGER*4  M_BUF, M_IND, M_PAR
      PARAMETER  ( M_BUF = 128 )
      PARAMETER  ( M_IND = 16  )
      PARAMETER  ( M_PAR = 10  )
      CHARACTER  BUF(M_BUF)*80, STR*80, STR1*80, DELIM*3
      PARAMETER  ( DELIM = CHAR(0)//CHAR(32)//CHAR(9) )
      INTEGER*4  N_BUF, IND(2,M_IND), LIND, I_PAR, J1, IER
      REAL*8     SEC
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      CALL CLRCH ( EXTFMT   )
      CALL CLRCH ( FILEXT   )
      CALL CLRCH ( FILERP   )
      CALL CLRCH ( FILOUT   )
      CALL CLRCH ( URLEXT   )
      CALL CLRCH ( FILUPM   )
      CALL CLRCH ( WGET_EXE )
!
! --- Read configuration file into the buffer BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILCNF, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4121, IUER, 'CONF_GAE', 'Error in attempt '// &
     &         'to open configuration file '//FILCNF )
           RETURN
      END IF
!
! --- Scan the buffer with copy of configuration file
!
      I_PAR = 0
      DO 410 J1=1,N_BUF
!
! ------ Skip comments
!
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         IF ( BUF(J1)(1:1)  .NE. '#'  ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '#!' ) GOTO 410
         IF ( BUF(J1)(1:2)  .EQ. '##' ) GOTO 410
!
! ------ Split the line onto words
!
         CALL EXWORD ( BUF(J1), M_IND, LIND, IND, DELIM, -3 )
         CALL CLRCH  ( STR )
         CALL INCH   ( J1, STR )
!
! ------ It should be exactly three words. Let's check it
!
         IF ( LIND .LT. 3 ) THEN
              CALL ERR_LOG ( 4122, IUER, 'CONF_GAE', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//FILCNF(1:I_LEN(FILCNF))// &
     &            ': too few words' )
              RETURN
         END IF
!
! ------ Well. Three words. Parse them
!
! ------ Transform the keyword to the letters of upper case
!
         CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), &
     &                   BUF(J1)(IND(1,2):IND(2,2))  )
!
         IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'URLEXT:' ) THEN
              URLEXT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'URLEXT2:' ) THEN
              URLEXT2 = BUF(J1)(IND(1,3):IND(2,LIND))
              IF ( URLEXT2(1:5) == 'NONE ' .OR. &
     &             URLEXT2(1:5) == 'none '      ) THEN
                   CALL CLRCH ( URLEXT2 )
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'EXTFMT:' ) THEN
              EXTFMT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'FILEXT:' ) THEN
              FILEXT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'FILEXT2:' ) THEN
              FILEXT2 = BUF(J1)(IND(1,3):IND(2,LIND))
              IF ( FILEXT2(1:5) == 'NONE ' .OR. &
     &             FILEXT2(1:5) == 'none '      ) THEN
                   CALL CLRCH ( FILEXT2 )
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'FILERP:' ) THEN
              FILERP = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'FILOUT:' ) THEN
              FILOUT = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'FILUPM:' ) THEN
              FILUPM = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WGET_EXE:' ) THEN
              WGET_EXE = BUF(J1)(IND(1,3):IND(2,LIND))
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'FL_ROT:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,3):IND(2,LIND)), &
     &                        BUF(J1)(IND(1,3):IND(2,LIND))  )
              IF ( BUF(J1)(IND(1,3):IND(2,LIND)) .EQ. 'TRUE' ) THEN
                   FL_ROT = .TRUE.
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,LIND)) .EQ. 'FALSE' ) THEN
                   FL_ROT = .FALSE.
                 ELSE
                   CALL ERR_LOG ( 4123, IUER, 'CONF_GAE', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '//FILCNF(1:I_LEN(FILCNF))// &
     &                 ': TRUE or FALSE were expected' )
                   RETURN
              END IF
           ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WEIGHTS:' ) THEN
              CALL TRAN ( 11, BUF(J1)(IND(1,3):IND(2,LIND)), &
     &                        BUF(J1)(IND(1,3):IND(2,LIND))  )
              IF ( BUF(J1)(IND(1,3):IND(2,LIND)) .EQ. 'IN' ) THEN
                   FL_EQUWEI = .FALSE.
                 ELSE IF ( BUF(J1)(IND(1,3):IND(2,LIND)) .EQ. 'EQUAL' ) THEN
                   FL_EQUWEI = .TRUE.
                 ELSE
                   CALL ERR_LOG ( 4124, IUER, 'CONF_GAE', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '//FILCNF(1:I_LEN(FILCNF))// &
     &                 ': IN or EQUAL were expected' )
                  RETURN
              END IF
            ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'ROT_FROM:' ) THEN
              STR = BUF(J1)(IND(1,3):IND(2,LIND))//'_00:00:00.0'
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( STR, MJD_BEGROT, SEC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH  ( STR )
                   CALL INCH   ( J1, STR )
                   CALL ERR_LOG ( 4125, IUER, 'CONF_GAE', 'Error in '// &
     &                 'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &                 'configuration file '//FILCNF(1:I_LEN(FILCNF))// &
     &                 ': wronge date format. YYYY.MM.DD format was expected' )
                   RETURN
              END IF
            ELSE
              CALL ERR_LOG ( 4126, IUER, 'CONF_GAE', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line in the '// &
     &            'configuration file '//FILCNF(1:I_LEN(FILCNF))// &
     &            ': unsupported keyword '//BUF(J1)(IND(1,2):IND(2,2)) )
              RETURN
         END IF
         I_PAR = I_PAR + 1
 410  CONTINUE
!
! --- Check: did we define all fields of FTE?
!
      IF ( I_PAR .LT. M_PAR ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( I_PAR, STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( M_PAR, STR1 )
           CALL ERR_LOG ( 4127, IUER, 'CONF_GAE', 'Not all parameters '// &
     &         'were found in OPA configuration file '// &
     &          FILCNF(1:I_LEN(FILCNF))//' --  '//STR(1:I_LEN(STR))// &
     &         ' instead of '//STR1 )
           RETURN
      END IF
!
      IF ( EXTFMT(1:8) .EQ. 'IERS_C04' ) THEN
           CONTINUE
         ELSE IF ( EXTFMT(1:11) .EQ. 'USNO_FINALS' ) THEN
           CONTINUE
         ELSE
           CALL ERR_LOG ( 4128, IUER, 'CONF_GAE', 'Wrong value of keyword '// &
     &         'EXTFMT: '//EXTFMT(1:I_LEN(EXTFMT))//' -- one of IERS_C04 or '// &
     &         'USNO_FINALS was expected' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  CONF_GAE  #!#
