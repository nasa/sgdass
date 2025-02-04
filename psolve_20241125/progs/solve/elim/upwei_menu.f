      SUBROUTINE UPWEI_MENU ( VER_UPWEI, IDB2, N_OBS, L_SCA, L_STA, &
     &           OBSHLD, DBOBJ, OBSSCA, OBSSTA, OBSBAS, RES, RST, CHIOBJ, &
     &           REWAY_FLODEL,  REWAY_CHITOL, REWAY_MAXIT, REWAY_TYPE, &
     &           REWAY_VERBOSE, F_CHI, IACT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  UPWEI_MENU  takes the parameters of the program UPWEI     *
! *   in the mode of menu.                                               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * VER_UPWEI ( CHARACTER ) -- String with UPWEI-identifier and number   *
! *                            of the current version.                   *
! *      IDB2 ( INTEGER*2 ) -- Index of the considered database in the   *
! *                            scratch file.                             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     L_SCA ( INTEGER*4 ) -- Number of common scans.                   *
! *     L_STA ( INTEGER*4 ) -- Number of participated stations.          *
! *    OBSHLD ( RECORD    ) -- Data structure which keeps the current    *
! *                            status of the database and some           *
! *                            session-dependent information.            *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSSCA ( RECORD    ) -- Array of data structures which keeps      *
! *                            scan-dependent information about the      *
! *                            session.                                  *
! *    OBSSTA ( RECORD    ) -- Array of data structures which keeps      *
! *                            station dependent information about the   *
! *                            session.                                  *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *       RST ( RECORD    ) -- Data structure keeping the statisitcs of  *
! *                            postfit residuals.                        *
! *    CHIOBJ ( RECORD    ) -- Object with data structure for keeping    *
! *                            accumulators of the chi-squares and their *
! *                            mathematical expectations.                *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *      IACT ( INTEGER*4 ) -- Action to be done:                        *
! *                            IACT=1 -- iterative update of reweight    *
! *                                      constants;                      *
! *                            IACT=2 -- update reweight constants       *
! *                                      only once;                      *
! *                            IACT=3 -- display reweight constants      *
! *                                      partial chi/ndg and other       *
! *                                      statistics;                     *
! *                            IACT=4 -- terminate execution of UPWEI    *
! *                                      and return to the calling       *
! *                                      program.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   REWAY_FLODEL ( REAL*8    ) -- Reweiting floor: minimal acceptable  *
! *                                 value of the correction to weights   *
! *                                 (in psec). Correction to weights     *
! *                                 will never be less than this value.  *
! *   REWAY_CHITOL ( REAL*8    ) -- Tolerance criterion for the          *
! *                                 iterations convergence: Iterations   *
! *                                 will be stopped when the ratio of    *
! *                                 chi-square to its mathematical       *
! *                                 expectation will differ from unity   *
! *                                 by less than REWAY_CHITOL.           *
! *   REWAY_MAXIT  ( INTEGER*2 ) -- Maximal number of iterations.        *
! *   REWAY_TYPE   ( CHARACTER ) -- Type of reweight: 'GL' -- global,    *
! *                                 baseline-independent; 'BA' --        *
! *                                 baseline-dependent. Acceptable input *
! *                                 value are 'ST' (the same as BA) of   *
! *                                 'NO' -- no preference. If the input  *
! *                                 value of REWAY_TYPE is 'NO' then it  *
! *                                 is changed by UPWEI_MENU to 'GL' if  *
! *                                 all old reweight constant are the    *
! *                                 same for all baselines or to 'BA'    *
! *                                 otherwise.                           *
! *  REWAY_VERBOSE ( LOGICAL*2 ) -- Verbosity mode for. If .TRUE. then   *
! *                                 verbose intermediary infromation     *
! *                                 will be printed at the screen and to *
! *                                 SPOOL-file.                          *
! *          F_CHI ( LOGICAL*4 ) -- Flag: .TRUE. means that the ratio of *
! *                                 chi-square to its mathematical       *
! *                                 expectation has been calculated and  *
! *                                 available.                           *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  28-JAN-98    UPWEI_MENU   v1.2  (c) L. Petrov  03-AUG-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
!
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'fast.i'
!
      INTEGER*4  N_OBS, L_SCA, L_STA, IACT, IUER
      INTEGER*2  IDB2, REWAY_MAXIT
!
      TYPE ( HLD_O__STRU ) ::  OBSHLD
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( SCA_O__STRU ) ::  OBSSCA(L_SCA)
      TYPE ( STA_O__STRU ) ::  OBSSTA(L_STA)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( RST_O__STRU ) ::  RST
      TYPE ( CHIACC__STRU ) ::  CHIOBJ
      CHARACTER  VER_UPWEI*(*), REWAY_TYPE*(*)
      REAL*8     REWAY_FLODEL,  REWAY_CHITOL
      LOGICAL*2  REWAY_VERBOSE
      LOGICAL*4  F_CHI
      CHARACTER  STR*120, STR1*80, CC4*4, SIM*1, SOLTYP*32
      INTEGER*4  IX, IY, IVAL, IP, I5, J1, J2, IER
      REAL*8     VAL, EPS
      PARAMETER  ( EPS = 1.D-7 )
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      LOGICAL*4, EXTERNAL :: DATYP_INQ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Start curser
!
      CALL START_MN()
      IACT = 0
 910  CONTINUE
!
! --- Printing the screen form
!     ~~~~~~~~~~~~~~~~~~~~~~~~
!
! --- Printing the first line: title of the program
!
      CALL CLEAR_MN()
      CALL SETCR_MN (  0, 0 )
!
      CALL ADDSTR_F ( 'Updating weights utility' )
      CALL SETCR_MN (  79-ILEN(VER_UPWEI), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( VER_UPWEI )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
! --- And then printing the statistics of the session
!
      CALL ADDSTR_F ( DBOBJ%NAME(1:I_LEN(DBOBJ%NAME)) )
      IF ( DBOBJ%STATUS .EQ. DBOBJ__DON ) THEN
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%L_SOU, STR )
           CALL ADDSTR_F ( '  SOU='//STR(1:I_LEN(STR))//'(' )
           CALL INCH     ( DBOBJ%U_SOU, STR )
           CALL ADDSTR_F ( STR(1:I_LEN(STR))//')' )
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%L_STA, STR )
           CALL ADDSTR_F ( '  STA='//STR(1:I_LEN(STR))//'(' )
           CALL INCH     ( DBOBJ%U_STA, STR )
           CALL ADDSTR_F ( STR(1:I_LEN(STR))//')' )
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%L_BAS, STR )
           CALL ADDSTR_F ( '  BAS='//STR(1:I_LEN(STR))//'(' )
           CALL INCH     ( DBOBJ%U_BAS, STR )
           CALL ADDSTR_F ( STR(1:I_LEN(STR))//')' )
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%CG_OBS, STR )
           CALL ADDSTR_F ( '  OBS='//STR(1:I_LEN(STR))//'(' )
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%U_OBS, STR )
           CALL ADDSTR_F ( STR(1:I_LEN(STR))//'){' )
!
           CALL CLRCH    ( STR )
           CALL INCH     ( DBOBJ%R_OBS, STR )
           CALL ADDSTR_F ( STR(1:I_LEN(STR))//'}' )
           CALL NL_MN()
!C
           CALL CLRCH ( SOLTYP )
!
! -------- Decoding solution type
!
           CALL DATYP_SHOW ( DBOBJ%IDATYP, SOLTYP )
!
! -------- Decoding w.r.m.s. for entire solution
!
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(F8.1)' ) RST%WRMS_G*1.D12
           CALL CHASHL ( STR )
           IF ( STR(1:1) .EQ. '.' ) STR='0'//STR
!
! -------- Ratio of Chi-square to its mathematical expectation
!
           CALL CLRCH ( STR1 )
           IF ( F_CHI ) THEN
                WRITE ( UNIT=STR1, FMT='(F8.3)' ) CHIOBJ%CHI_GLO/ &
     &                                      (CHIOBJ%NEQU_GLO-CHIOBJ%CHIMAT_GLO)
                CALL CHASHL ( STR1 )
                IF ( STR1(1:1) .EQ. '.' ) STR1='0'//STR1
              ELSE
                STR1 = 'N/A'
           END IF
!
! -------- ... and printing them at the screen
!
           CALL ADDSTR_F ( 'Solution type: '//SOLTYP(1:I_LEN(SOLTYP))//'  '// &
     &                     'w.r.m.s. = '//STR(1:I_LEN(STR))// &
     &                     ' psec  chi/ndg = '//STR1(1:I_LEN(STR1)) )
           CALL NL_MN()
        ELSE
           CALL NL_MN()
           CALL ADDSTR_F ( 'Information about residuals is not available yet' )
           CALL NL_MN()
           CALL NL_MN()
           CALL NL_MN()
      END IF
!
      CALL NL_MN()
      CALL ADDSTR_F ( "(H) On-line help                    " )
      IF ( REWAY_TYPE  .EQ. 'NO' ) THEN
           IF ( DATYP_INQ ( DBOBJ%IDATYP, GROUP__DTP ) .OR. &
     &          DATYP_INQ ( DBOBJ%IDATYP, SINGL__DTP )      ) THEN
!
! ------------- Group delay case
!
                IF ( DABS(CHIOBJ%WEIGR_BAS(1) - CHIOBJ%WEIGR_BAS(2) ) .GT. &
     &               CHIOBJ%WEIGR_BAS(1)*EPS ) THEN
!
                     REWAY_TYPE  = 'BA'
                  ELSE
                     REWAY_TYPE  = 'GL'
                END IF
             ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Phase delay case
!
                IF ( DABS(CHIOBJ%WEIPH_BAS(1) - CHIOBJ%WEIPH_BAS(2) ) .GT. &
     &               CHIOBJ%WEIPH_BAS(1)*EPS ) THEN
!
                     REWAY_TYPE  = 'BA'
                  ELSE
                     REWAY_TYPE  = 'GL'
                END IF
           END IF
      END IF
!
      CALL CLRCH ( STR )
      IF ( REWAY_TYPE .EQ. 'GL' ) THEN
           STR = 'Global'
         ELSE IF ( REWAY_TYPE .EQ. 'BA'  .OR.  REWAY_TYPE .EQ. 'ST' ) THEN
           STR = 'Baseline'
      END IF
      CALL ADDSTR_F ( "Type of weights in use: "//STR(1:I_LEN(STR)) )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( "(I) Iterate to unity                " )
      CALL CLRCH    ( STR )
      CALL INCH     ( INT4(REWAY_MAXIT), STR )
      CALL ADDSTR_F ( "(M)aximum iterations:          "//STR(1:I_LEN(STR)) )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( "(U) Update weights once             " )
      CALL CLRCH ( STR )
      WRITE ( STR, '(F6.4)' ) REWAY_CHITOL
!
! --- Improvment of the appeareance of CHI_TOL
!
      IF ( STR(1:1) .EQ. ' ' ) STR(1:1) = '0'
      IF ( STR(ILEN(STR):ILEN(STR)) .EQ. '0' ) STR(ILEN(STR):ILEN(STR)) = ' '
      IF ( STR(ILEN(STR):ILEN(STR)) .EQ. '0' ) STR(ILEN(STR):ILEN(STR)) = ' '
      IF ( STR(ILEN(STR):ILEN(STR)) .EQ. '0' ) STR(ILEN(STR):ILEN(STR)) = ' '
      CALL ADDSTR_F ( "(T) Tolerance for convergence: "//STR(1:I_LEN(STR)) )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( "(N) Re-read the namfil weights      " )
      CALL ADDSTR_F ( "(E) Enter initial weights" )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL CLRCH ( STR )
      WRITE ( UNIT=STR, FMT='(F6.1)' ) REWAY_FLODEL
      CALL CHASHL ( STR )
      IF ( STR(1:1) .EQ. '.' ) STR = '0'//STR
      CALL ADDSTR_F ( "(L) Set floor "//STR(1:6)//" psec           " )
!
      CALL CLRCH ( STR )
      IF ( REWAY_VERBOSE ) THEN
           STR(1:3) = 'on '
         ELSE
           STR(1:3) = 'off'
      END IF
      CALL ADDSTR_F ( "(V) Verbose mode " )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:3) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( "(B) Change to baseline weights      " )
      CALL ADDSTR_F ( "(G) Change to global weights        " )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL ADDSTR_F ( "(D) Display current weights         " )
      CALL ADDSTR_F ( "(O) Return back to previous program " )
      CALL NL_MN()
      CALL NL_MN()
!
      CALL SETCR_MN ( 1, 7 )
!
! --- Awaiting for entering information
!
 920  CONTINUE
!
! --- Awaiting for user response
!
      CALL SENKR_MN ( IX, IY, CC4 )
      SIM = CC4(4:4)
!
! --- Transforming cursor coordinates in to the letter of the item to which it
! --- points
!
      IF ( SIM .EQ. ' ' .OR. SIM .EQ. CHAR(13) ) THEN
           IF ( IY .EQ.  5  .AND.  IX .LT. 36 ) SIM='H'
           IF ( IY .EQ.  7  .AND.  IX .LT. 36 ) SIM='I'
           IF ( IY .EQ.  7  .AND.  IX .GE. 36 ) SIM='M'
           IF ( IY .EQ.  9  .AND.  IX .LT. 36 ) SIM='U'
           IF ( IY .EQ.  9  .AND.  IX .GE. 36 ) SIM='T'
           IF ( IY .EQ. 11  .AND.  IX .LT. 36 ) SIM='N'
           IF ( IY .EQ. 11  .AND.  IX .GE. 36 ) SIM='E'
           IF ( IY .EQ. 13  .AND.  IX .LT. 36 ) SIM='L'
           IF ( IY .EQ. 13  .AND.  IX .GE. 36 ) SIM='V'
           IF ( IY .EQ. 15  .AND.  IX .LT. 36 ) SIM='B'
           IF ( IY .EQ. 15  .AND.  IX .GE. 36 ) SIM='G'
           IF ( IY .EQ. 17  .AND.  IX .LT. 36 ) SIM='D'
           IF ( IY .EQ. 17  .AND.  IX .GE. 36 ) SIM='O'
      END IF
!
! --- Making actions
!     ~~~~~~~~~~~~~~
!
      IF ( SIM .EQ. 'H' ) THEN
           CALL UPWEI_HELP ( VER_UPWEI )
         ELSE IF ( SIM .EQ. 'I' ) THEN
           IACT = 1
           GOTO 810
         ELSE IF ( SIM .EQ. 'M' ) THEN
           IP = 7
 930       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter the maximal number of iterations ( > 0 ): ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  INTEGER*4
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) IVAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of INTEGER type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 930
           END IF
           IF ( IVAL .LT. 1  .OR. IVAL .GT. 32767  ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of the range [1, 32767].  Hit any '// &
     &                          'key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 930
           END IF
!
! -------- Substitution to the new value
!
           REWAY_MAXIT = INT2(IVAL)
           GOTO 910
         ELSE IF ( SIM .EQ. 'T' ) THEN
           IP = 9
 940       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter tolerance factor in the range [0.0001, 1.0] ')
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  REAL*8
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of REAL*8 type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 940
           END IF
           IF ( VAL .LT. 0.0001  .OR.  VAL .GT. 1.0  ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of the range [0.0001, 1.0] '// &
     &                          ' Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 940
           END IF
!
! -------- Substitution to the new value
!
           REWAY_CHITOL = VAL
         ELSE IF ( SIM .EQ. 'U' ) THEN
           IACT = 2
           GOTO 810
         ELSE IF ( SIM .EQ. 'N' ) THEN
!
! -------- Reading weights from NAMFIL and putting them into CHIOBJ data structure
!
           CALL ERR_PASS ( IUER, IER )
           CALL IO_WGT   ( 1, IDB2, DBOBJ, CHIOBJ%WEIGR_BAS, CHIOBJ%WEIPH_BAS, &
     &                     IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6521, IUER, 'UPWEI_MENU', 'Error duiring '// &
     &              'getting weights from NAMFIL while database '// &
     &               DBOBJ%NAME//' was processing' )
                RETURN
           END IF
           CHIOBJ%WEIGR_GLO  = CHIOBJ%WEIGR_BAS(1)
           CHIOBJ%WEIPH_GLO  = CHIOBJ%WEIPH_BAS(1)
           CHIOBJ%LAST_FIELD = 1
           F_CHI = .TRUE.
         ELSE IF ( SIM .EQ. 'E' ) THEN
           IP = 11
 960       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter initial weights for all baselines (psec) ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  REAL*8
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of REAL*8 type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 960
           END IF
           IF ( VAL .LT. 1.D-15 ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is less 0.  Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 960
           END IF
           IF ( VAL .GT. 1.D6 ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is more than 10**6 psec.  Hit any key to '// &
     &                          'proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 960
           END IF
!
! -------- Substitution to the new value
!
           DO 410 J1=1,DBOBJ%L_BAS
              IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                   CHIOBJ%WEIPH_BAS(J1) = 1.D12/VAL
                 ELSE
                   CHIOBJ%WEIGR_BAS(J1) = 1.D12/VAL
              END IF
 410       CONTINUE
           F_CHI = .FALSE.
         ELSE IF ( SIM .EQ. 'L' ) THEN
           IP = 13
 970       CONTINUE
           CALL SETCR_MN ( 0, IP )
!
! -------- Clearing the line
!
           CALL ADDSTR_F ( "                                        "// &
     &                     "                                        " )
!
! -------- Printout of prompt
!
           CALL SETCR_MN ( 0, IP )
           CALL ADDSTR_F ( 'Enter the weight floor in the range '// &
     &                     '[0.0, 300.0] psec ' )
!
! -------- Entering the string STR
!
           CALL CLRCH ( STR )
           CALL GETSTR_F ( STR )
           IF ( ILEN(STR) .EQ. 0 ) GOTO 910
!
! -------- Type transformation CHARACTER  -->  REAL*8
!
           READ ( STR(1:I_LEN(STR)), FMT=*, IOSTAT=I5 ) VAL
           IF ( I5 .NE. 0 ) THEN
!
! ------------- Error in type transformation
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   "'//STR(1:I_LEN(STR))//'" is not the '// &
     &              'variable of REAL*8 type. Hit any key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 970
           END IF
!
           IF ( VAL .LT. 0.0  .OR.  VAL .GT. 300.0 ) THEN
!
! ------------- Value out of range
!
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( "                                        "// &
     &                          "                                       " )
                CALL SETCR_MN ( 0, IP )
                CALL ADDSTR_F ( '$$$   '//STR(1:I_LEN(STR))// &
     &                          ' is out of the range [0.0, 300.0]. Hit any '// &
     &                          'key to proceed. ' )
                CALL SENKR_MN ( IX, IY, CC4 )
                GOTO 970
           END IF
!
! -------- Substitution to the new value
!
           REWAY_FLODEL = VAL
!
! -------- Check: do any baselines have weights less then floor? If yes then
! -------- we change these weights
!
           DO 420 J2=1,DBOBJ%L_BAS
              IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
                   IF ( 1.D0/CHIOBJ%WEIPH_BAS(J2) .LT. REWAY_FLODEL*1.D-12 ) THEN
                        CHIOBJ%WEIPH_BAS(J2) = 1.D0/(REWAY_FLODEL*1.D-12)
                        F_CHI = .FALSE.
                   END IF
                 ELSE 
                   IF ( 1.D0/CHIOBJ%WEIGR_BAS(J2) .LT. REWAY_FLODEL*1.D-12 ) THEN
                        CHIOBJ%WEIGR_BAS(J2) = 1.D0/(REWAY_FLODEL*1.D-12)
                        F_CHI = .FALSE.
                   END IF
              END IF
 420       CONTINUE
         ELSE IF ( SIM .EQ. 'V' ) THEN
           REWAY_VERBOSE = .NOT. REWAY_VERBOSE
         ELSE IF ( SIM .EQ. 'B' ) THEN
           REWAY_TYPE = 'BA'
         ELSE IF ( SIM .EQ. 'G' ) THEN
           REWAY_TYPE = 'GL'
         ELSE IF ( SIM .EQ. 'D' ) THEN
           IACT = 3
           GOTO 810
         ELSE IF ( SIM .EQ. 'O' ) THEN
           IACT = 4
           GOTO 810
      END IF
      GOTO 910
 810  CONTINUE
      IF ( .NOT. F_CHI ) THEN
!
! --------- Putting weights in the CHIOBJ data structure
!
            CALL ERR_PASS ( IUER, IER )
            CALL IO_WGT   ( 2, IDB2, DBOBJ, CHIOBJ%WEIGR_BAS, CHIOBJ%WEIPH_BAS, &
     &                      IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6522, IUER, 'UPWEI_MENU', 'Error duiring '// &
     &               'putting weights to NAMFIL while database '// &
     &                DBOBJ%NAME//' was processing' )
                RETURN
            END IF
!
! --------- Refresh the weights in the data structure OBSBAS
!
            CALL ERR_PASS    ( IUER, IER )
            CALL REFRESH_WEI ( OBSHLD, DBOBJ, OBSBAS, CHIOBJ%WEIGR_BAS, &
     &                         CHIOBJ%WEIPH_BAS, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 6523, IUER, 'UPWEI_MENU', 'Error duiring '// &
     &               'refreshing weights in data structure OBSBAS while '// &
     &               'database '//DBOBJ%NAME//' was processing' )
                RETURN
            END IF
!
! --------- Refreshing weights in the data astructure OBSHLD
!
            CALL COPY_V ( MO_BAS, CHIOBJ%WEIGR_BAS, OBSHLD%WEIGR_BAS )
            CALL COPY_V ( MO_BAS, CHIOBJ%WEIPH_BAS, OBSHLD%WEIPH_BAS )
      END IF
!
! --- Terminating curses and clearing the screen before leaving
!
      CALL CLEAR_MN ()
      CALL END_MN()
      CALL UN_CURSES ()    !  Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  UPWEI_MENU  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UPWEI_HELP ( VER_UPWEI )
! ************************************************************************
! *                                                                      *
! *   Ancillary siutine UPWEI_HELP print on the screen on-line help      *
! *   information about UPWEI routine.                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * VER_UPWEI ( CHARACTER ) -- String with UPWEI-identifier and number   *
! *                            of the current version.                   *
! *                                                                      *
! *  ###  30-JAN-98   UPWEI_HELP   v1.0  (c)  L. Petrov  30-JAN-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE
      INCLUDE   'solve.i'
      INCLUDE   'help.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, MBUF, NBUF, ISIM, J1, IMEN, ICOL, ILIN
      PARAMETER  ( MBUF = 1024 )
      CHARACTER  VER_UPWEI*(*), BUF(MBUF)*160, ASIM*1, FINAM*255
      CHARACTER  PRE_INIT*32, POST_INIT*32, PREF*32, ESC*1, &
     &           SOLVE_PS_VIEWER_USE*128
      INTEGER*4  IT, IG, IP, IST, IM
      LOGICAL*4, EXTERNAL :: USE_TERM_COLOR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, SYSTEM, MAKE_HELP_FINAM
!
      ESC = CHAR(27)
!
      CALL GETENVAR ( 'SOLVE_PS_VIEWER', SOLVE_PS_VIEWER_USE )
      IF ( ILEN(SOLVE_PS_VIEWER_USE) == 0 ) THEN
           SOLVE_PS_VIEWER_USE = SOLVE_PS_VIEWER
      END IF
!
! --- Stopping curses
!
      CALL END_MN()
!
! --- And elimination of the influence of curses
!
      CALL UN_CURSES() ! Elimination of the influence of curses
      CALL CLEAR ( 0, 0 )
!
! --- Make filename with help menu
!
      IM = MAKE_HELP_FINAM ( UPWEI_HELP_00, FINAM )
      IF ( IM.NE.0 ) THEN
           CALL ERR_LOG ( 6781, -1, 'UPWEI_HELP', 'Help file '// &
     &          UPWEI_HELP_00//' is not found. Check directory '// &
     &          SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
           CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), %VAL(0) )
           GOTO 810
      END IF
!
! --- Reading file with help menu
!
      IUER = -1
      CALL RD_TEXT ( FINAM, MBUF, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6782, -1, 'UPWEI_HELP', 'Error during openning '// &
     &         'file '//FINAM(1:I_LEN(FINAM))//' with help information' )
           CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), %VAL(0) )
           GOTO 810
      END IF
!
! --- Getting terminal size
!
      CALL TERM_SIZE ( ILIN, ICOL )
!
! --- Setting esc-sequences changing the colour of the terminal (in hpterm mode)
!
      CALL CLRCH ( PRE_INIT  )
      CALL CLRCH ( POST_INIT )
      CALL CLRCH ( PREF      )
!!      CALL SHOW_IO ( IT, IG, IP, IST, %VAL(0) )
      IT = 6
      IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR() ) THEN
           PRE_INIT  = ESC//'&v0m0.41x0.76y0.39z2I'
           POST_INIT = ESC//'&v0m1b1x1y1z2I'
           PREF = ESC//'&v2S'
      END IF
      IF ( USE_TERM_COLOR() ) CALL PRCH ( PRE_INIT )
!
! --- Dusplaying help-menu
!
      DO 410 J1=1,NBUF
         IF ( J1 .EQ. 1 ) THEN
!
! ----------- Adding version date
!
              BUF(1)(ICOL-ILEN(VER_UPWEI):) = VER_UPWEI
            ELSE
         END IF
         IF ( IT.EQ.6 .OR. IT.EQ.7 ) THEN
              CALL ADR_CURSOR ( J1, 1 )
              IF ( USE_TERM_COLOR() ) THEN
                   CALL PRCH  ( PREF )
              END IF
              CALL PRCH ( BUF(J1)(1:ICOL-1)//CHAR(13) )
           ELSE
              WRITE ( 6, FMT='(A)' ) BUF(J1)(1:I_LEN(BUF(J1)))
         END IF
 410  CONTINUE
      IF ( IT .EQ. 6  .AND.  USE_TERM_COLOR() ) THEN
           CALL PRCH ( PREF(1:ILEN(PREF))//CHAR(13)//CHAR(10) )
      END IF
!
! --- Awaiting user action
!
      CALL INSIM ( ASIM, ISIM )
!
! --- Unsetting clour changes
!
      IF ( USE_TERM_COLOR() ) THEN
           CALL PRCH ( POST_INIT )
      END IF
      IMEN = 1
      IF ( ASIM .EQ. '2' ) IMEN = 2
      IF ( ASIM .EQ. '3' ) IMEN = 3
      IF ( ASIM .EQ. '4' ) IMEN = 4
!
! --- Clearing display ...
!
      CALL CLEAR ( 0, 0 )
!
! --- And different actions
!
      IF ( IMEN .EQ. 1 ) THEN
!
! -------- Displaying 1-st menu item
!
           IM = MAKE_HELP_FINAM ( UPWEI_HELP_01, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6783, -1, 'UPWEI_HELP', 'Help file '// &
     &               UPWEI_HELP_01//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'description of UPWEI menu items', &
     &                               1, IUER )
!
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6784, -1, 'UPWEI_HELP', 'Error during '// &
     &              'openning file '//FINAM(1:I_LEN(FINAM))// &
     &              ' with help information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                           %VAL(0) )
                GOTO 810
           END IF
         ELSE IF ( IMEN .EQ. 2 ) THEN
!
! -------- Displaying 2-nd menu item (in PostScript mode)
!
           IM = MAKE_HELP_FINAM ( UPWEI_HELP_02, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6785, -1, 'UPWEI_HELP', 'Help file '// &
     &               UPWEI_HELP_02//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
! -------- Launching Postscript previewer
!
           WRITE ( 6, FMT='(A)' ) 'Scheduling '// &
     &            SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))//' '// &
     &            FINAM(1:I_LEN(FINAM))//' ...'
           IP = SYSTEM ( SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))//' '// &
     &                   FINAM(1:I_LEN(FINAM))//CHAR(0) )
           IF ( IP .EQ. 32512 ) THEN
                CALL ERR_LOG ( 6786, -1, 'UPWEI_HELP', 'Environment '// &
     &              'variable SHELL has wrong value. Error in running Shell '// &
     &              'command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
             ELSE IF ( IP .NE. 0 ) THEN
                CALL ERR_LOG ( 6787, -1, 'UPWEI_HELP', 'Error in running Shell'// &
     &              ' command: '//SOLVE_PS_VIEWER_USE(1:I_LEN(SOLVE_PS_VIEWER_USE))// &
     &              ' '//FINAM )
           END IF
           CALL HIT_CONT ( %VAL(0), %VAL(0) )
        ELSE IF ( IMEN .EQ. 3 ) THEN
!
! -------- Displaying 3-rd menu item
!
           IM = MAKE_HELP_FINAM ( UPWEI_HELP_03, FINAM )
           IF ( IM.NE.0 ) THEN
                CALL ERR_LOG ( 6788, -1, 'UPWEI_HELP', 'Help file '// &
     &               UPWEI_HELP_03//' is not found. Check directory '// &
     &               SOLVE_HELP_DIR//' and environment variable SOLVE_HELP_DIR' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
!
           IUER = -1
           CALL SHOW_TEXT_FILE_COL ( FINAM, 'UPWEI release note', 1, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 6789, -1, 'UPWEI_HELP', 'Error during '// &
     &              'opening file '//FINAM(1:I_LEN(FINAM))//' with help '// &
     &              'information' )
                CALL HIT_CONT ( '--- Hit any key to proceed --- '//CHAR(1), &
     &                          %VAL(0) )
                GOTO 810
           END IF
      END IF
!
! --- Good bye
!
 810  CONTINUE
!
! --- Clearing display
!
      CALL CLEAR ( 0, 0 )
!
! --- Starting curses again
!
      CALL START_MN()
      RETURN
      END  !#!  UPWEI_HELP  #!#
