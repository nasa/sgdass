      SUBROUTINE RUN_PROG ( INPROG, STATE, PATH )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  RUN_PROG PROGRAM SPECIFICATION
!
! 1.1 Schedule the requested program and either wait for it
!     to complete or pass control on.  When a program is
!     scheduled with RUN_PROG, it must either terminate by
!     calling END_PROG or pass control to another program with
!     a call to RUN_PROG ( ..., 'PASS', ... ).
!
! 1.2 REFERENCES:
!
! 2.  RUN_PROG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER INPROG*(*) 
      CHARACTER STATE*4 
      INTEGER*2 path
!
! INPROG - Standard program name
! STATE - 'WAIT' to wait for completion of scheduled program
!         'PASS' to pass control to scheduled program
!         'PASS' will terminate and not return to caller.
! PATH - If = 1, interpret INPROG as entire path/filename
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: fc_exit,susp,fatal_file,sigmes,fc_write,
!                           unpack_rmpar,execute_d
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IPROGX(3), LEN_STR, LENGTH, NTRY
      INTEGER*2 TRIMLEN, ILF, I, IT, IRETBF(8), IABEND(8)
      CHARACTER CPROGX*6, INTTODECIMAL*6, CRETBF*16, CABEND*6, PROG*64, DEV*16
      INTEGER*4    ISGN
      PARAMETER  ( ISGN=34 )  ! The length of the signature of the end of
!                             ! SOLMOD file
      CHARACTER CIB*80, CIB_FOUND*80, SGN*(ISGN), STR*10, STR1*10
      PARAMETER( SGN = 'SOLMOD SOLMOD SOLMOD SOLMOD SOLMOD' ) ! SOLMOD signature
      LOGICAL*2 KBIT, FOUND, KPASSED, KFIRST, FOPEN_FAILURE, FREAD_FAILURE
      LOGICAL*4 LEX
      INTEGER*4 IERR, STATUS, NBYTES, PTRDEV, ARGC
      PARAMETER ( ARGC=7 )
      INTEGER*4 ARGV(ARGC)
      CHARACTER FILENAME*(NAME_SIZE), ME*8, ERRSTR*255, &
     &          FNAME*(NAME_SIZE+4*7+4), CMD*(NAME_SIZE+4*7+4)
      INTEGER*2 IBUF(40)
      CHARACTER BUFSTR*80, CBUF*80, TOKEN*40, PDIR*160, SOLMOD_NAME*128, &
     &          DBGTTY*3
      EQUIVALENCE (IBUF(1),CBUF)
      INTEGER*4   MAX_TRY, IS, J1, J2, J3, IOS
      PARAMETER ( MAX_TRY = 32 ) ! Maximal number of trying for
!                                ! 1) Opening SOLMOD
!                                ! 2) Reading SOLMOD
      SAVE KFIRST,KPASSED
!
      EQUIVALENCE (CPROGX,IPROGX(1))
      EQUIVALENCE (IABEND,CABEND),(IRETBF,CRETBF)
!
      DATA &
     &       CABEND / 'ABEND '   /, &
     &       KFIRST / .TRUE.     /, &
     &       NBYTES / 16         /, &
     &           ME / 'run_prog' /
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, STRERROR 
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!   CMA  850427  Early version
!   GLL  860201  Modified to give the scheduled program the same
!                priority as the host program
!   WEH  871231  Created RUN_PROG
!   AEE  910515  Enhanced error messages written to the error file.
!   MWH  910927  Don't suspend after successful execution
!   AEE  920204  Removed hard coded path for fclib.i
!   JMG  960803  Time tag feature.
!   jmg  961004  Modified run_prog to call timetag only in batch mode.
!   kdb  961004  Comment out time tag feature.
!   pet  971030  Logic was substantially changed in order to trace
!                1) does SOLMOD file has been read to the end;
!                2) does only one entry for substitution of the exec-module
!                   name exist in SOLMOD.
!                3) increased the length of the variable PDIR from 40 to
!                   160 symbols
!                Comment added
!   pet  980623  Corrected some coding errors for setting path to SOLVE
!                executables
!   pet  2000.02.04  Added initialization of DBGTTY
!   pet  2001.05.04  Added check: if the first symbol of PRG is "/" then no
!                    attempts to add the path is done
!   pet  2003.10.30  Got rid of octal constants in parsing the status. 
!                    Intrinsic function MVBITS is used instead of.
!   pet  2004.11.25  Improved diagonostic of error in child process
!
! 5.  RUN_PROG PROGRAM STRUCTURE
!
! --- Remember our condition on first call, were we a pass?
!
      IF ( KFIRST ) THEN
           KFIRST=.FALSE.
           KPASSED=KBIT ( PRE_IP(2), INT2(4) )
      ENDIF
!
! --- Initialization of DBGTTY
!
      DBGTTY = '   '
!
! --- Make local copies of input values
!
      PROG=INPROG
!
! --- Check to see if this is a pass
!
      IF ( KPASSED  .AND.  STATE(1:4) .EQ. 'PASS' ) THEN
           CRETBF(1:6)=PROG
           DO I=1,5
              IRETBF(3+I)=PRE_IP(I)
           ENDDO
           IERR = FC_WRITE ( PIPE_IDS(2), PTR_NC(IRETBF), NBYTES )
           IF ( IERR.LT.0 ) CALL FERR ( INT2(210), 'Error on passing '// &
     &                          'write to parent', INT2(0), INT2(0) )
!
! -------- Silent termination of the current process
!
           CALL FC_EXIT ( 0 )
      ENDIF
 910  CONTINUE
!
! --- If test check for special version
!
      CPROGX   =  PROG(1:3)//PRE_LETRS
      FOUND    = .FALSE.
      IF ( KTESTV ) THEN
!
! ------ We make MAX_TRY attempts to read SOLMOD file
!
         FREAD_FAILURE = .FALSE.
         DO 410 J1=1,MAX_TRY
            IF ( J1 .EQ. 2 ) THEN
                 FREAD_FAILURE = .TRUE.
                 WRITE ( 6, * ) ' Trying to read SOLMOD once more'
            END IF
!
! --------- We make MAX_TRY attempts to open SOLMOD file
!
            FOPEN_FAILURE = .FALSE.
            DO 420 J2=1,MAX_TRY
               IF ( J1 .EQ. 1  .AND. J2 .EQ. 2 ) THEN
                    FOPEN_FAILURE = .TRUE.
                    WRITE ( 6, * ) ' Trying to open SOLMOD once more'
               END IF
!
! ------------ Opening SOLMOD file
!
               SOLMOD_NAME = PRE_SAV_DIR(1:PRE_SV_LEN)//'SOLMOD'
               OPEN ( UNIT=10, FILE=SOLMOD_NAME, IOSTAT=IOS )
               IF ( IOS .EQ. 0 ) THEN
                    GOTO 810  ! success
                 ELSE
!
! ----------------- Error of opening SOLMOD file occurred. Sleep 1 second and
! ----------------- and then try again
!
                    CALL SUSP ( INT2(2), INT2(1) )
               END IF
 420        CONTINUE
!
! --------- SOLMOD file has not been opened. Preparing error message
!
            CALL CLRCH ( STR    ) 
            CALL CLRCH ( STR1   ) 
            CALL CLRCH ( ERRSTR ) 
            WRITE ( STR,  FMT='(I10)'  ) MAX_TRY
            WRITE ( STR1, FMT='(I10)'  ) IOS
            ERRSTR = 'Failure to open file '//PRE_SAV_DIR(:PRE_SV_LEN)// &
     &               'SOLMOD  ocurred '//STR(1:TRIMLEN(STR))// &
     &               ' consecutive times. IOSTAT = '//STR1(1:TRIMLEN(STR1))
            CALL FERR ( INT2(221), ERRSTR(1:TRIMLEN(ERRSTR)), INT2(0), &
     &                  INT2(0) )
            CALL FC_EXIT ( 1 )
!
 810        CONTINUE
!
! --------- SOLMOD file has been opened successfilly. Now read it.
!
            FOUND = .FALSE.
            DO 430 J3=1,1048576
               READ ( 10, '(A)', IOSTAT= IOS ) CIB
!
! ------------ Check: does the current line contain the signature of the
! ------------ attribute of the end of file
!
               IF ( CIB(1:ISGN) .EQ. SGN ) GOTO 820 ! Yes, contained. It is OK.
               IF ( IOS .EQ. -1 ) THEN
!
! --------------- The end of SOLMOD file has been reached but the
! --------------- signature-termninator has not been found. Sleep 1 second and
! --------------- then try again to open SOLMOD and to read it ince more. Such
! --------------- a case may happen if somebody writes SOLMOD on disk just
! --------------- at this moment.
!
                  CLOSE ( UNIT = 10 )
                  CALL SUSP ( INT2(2), INT2(1) )
                  GOTO 410
               END IF
               IF ( IOS .NE. 0 ) THEN
!
! --------------- Some reading error occur
!
                  STR(1:LEN(STR))       = ' '
                  ERRSTR(1:LEN(ERRSTR)) = ' '
                  WRITE ( STR, FMT='(I10)'  ) IOS
                  ERRSTR = 'Failure in reading file '// &
     &                      PRE_SAV_DIR(:PRE_SV_LEN)//'SOLMOD'// &
     &                     ' IOSTAT='//STR(1:TRIMLEN(STR))
                  CALL FERR ( INT2(222), ERRSTR(1:TRIMLEN(ERRSTR)), &
     &                        INT2(0), INT2(0) )
                  CALL FC_EXIT ( 1 )
               END IF
!
               IF ( CIB(1:5) .EQ. CPROGX(1:5) ) THEN
!
! ----------------- Substitute version of the SOLVE executable has been found!
!
                    IF ( .NOT. FOUND ) THEN
                         FOUND = .TRUE.
                         CIB_FOUND = CIB
                      ELSE
                         CALL FERR ( INT2(240), PRE_SAV_DIR(:PRE_SV_LEN)// &
     &                       'SOLMOD'// &
     &                       ' contains more than one line starting from '// &
     &                        CPROGX(1:5)//' -- please edit it and leave only '// &
     &                       'one line starting from '//CPROGX(1:5), INT2(0), &
     &                        INT2(0) )
                         CALL FC_EXIT ( 1 )
                    END IF
               END IF
 430        CONTINUE
 410     CONTINUE
!
         STR(1:LEN(STR))       = ' '
         ERRSTR(1:LEN(ERRSTR)) = ' '
         WRITE ( STR, FMT='(I10)'  ) MAX_TRY
         ERRSTR = 'Failure in reading file '//PRE_SAV_DIR(:PRE_SV_LEN)// &
     &            'SOLMOD'//' ocurred '//STR(1:TRIMLEN(STR))// &
     &            ' consecutive times: termination signature '//SGN// &
     &            ' has not been found'
         CALL FERR ( INT2(223), ERRSTR(1:TRIMLEN(ERRSTR)), INT2(0), INT2(0) )
         CALL FC_EXIT ( 1 )
!
 820     CONTINUE
         CLOSE ( UNIT = 10 )
         IF ( FOPEN_FAILURE  .OR.  FREAD_FAILURE ) THEN
!
! ----------- We print this message to calm user
!
              WRITE ( 6, * ) ' SOLMOD has been read successfully at last'
         END IF
      END IF  ! KTESTV
!
! --- Construct the file name
!
      IF ( FOUND ) THEN
!
! -------- Substitution name of the exe-,module has been found. Split the
! -------- string at the tokens.
!
           CALL SPLITSTRING ( CIB_FOUND, TOKEN, CIB_FOUND )
           CALL SPLITSTRING ( CIB_FOUND, TOKEN, CIB_FOUND )
           IF ( TOKEN(1:1) .EQ. '/' ) THEN
                PDIR = TOKEN
                DBGTTY = '   '
              ELSE
                DBGTTY = TOKEN(1:3)
                CALL SPLITSTRING ( CIB_FOUND, TOKEN, CIB_FOUND )
                PDIR = TOKEN
           ENDIF
      ENDIF
!
      LEN_STR=MAX ( INT2(1), TRIMLEN(PROG) )
      IF ( KTESTV .AND. FOUND ) THEN
!
! -------- Adding path-name part of the filename if nesssary
!
           IF ( PROG(1:1) .NE. '/' ) THEN
                LENGTH = MAX ( TRIMLEN(PDIR), INT2(1) )
                IF ( PDIR(1:1) .NE. ' ' ) THEN
                    FILENAME = PDIR(:LENGTH)//PROG(1:LEN_STR)
                  ELSE
                    FILENAME=SOLVE_PROG_DIR//PROG(1:LEN_STR)
                ENDIF
              ELSE
                FILENAME = PROG
           END IF
!
           IF ( DBGTTY(1:1) .NE. ' ' ) THEN
                IF ( DBGTTY(1:1) .NE. 's' ) THEN
                     IERR = NULL_TERM ( DEV, '/dev/tty'//DBGTTY )
                  ELSE
                     IERR = NULL_TERM ( DEV, '/dev/pty/tty'//dbgtty )
                ENDIF
!
                CALL FATAL_FILE ( IERR, 'Terminating dbg tty', DBGTTY, ME )
                PTRDEV = PTR_CH ( DEV )
                CALL SBIT ( PRE_IP(2), INT2(5), INT2(1) )
              ELSE
                PTRDEV=0
                CALL SBIT ( PRE_IP(2), INT2(5), INT2(0) )
           ENDIF
         ELSE  ! No substitution version has been found
           IF ( PATH .EQ. 1  .OR.  PROG(1:1) .EQ. '/' ) THEN
!
! ------------- Path has been specified
!
                FILENAME=PROG(1:LEN_STR)
              ELSE
!
! ------------- Path has not been specified. Try environment variable 
! ------------- SOLVE_DIR
!
                IERR = FC_GETENV ( PTR_CH( 'SOLVE_DIR'//CHAR(0) ), &
     &                             PTR_NC( IBUF ) )
                IF ( IERR .GT. 0 ) THEN
!
! ------------------ Yes, it was set up
!
                     IF ( CBUF(IERR:IERR) .EQ. '/' ) IERR=IERR-1
                     FILENAME = CBUF(1:IERR)//'/'//PROG(1:LEN_STR)
                   ELSE
!
! ------------------ It was not set up -- use SOLVE_PROG_DIR
!
                     FILENAME = SOLVE_PROG_DIR//PROG(1:LEN_STR)
                ENDIF
           ENDIF
!
           PTRDEV=0
           CALL SBIT ( PRE_IP(2), INT2(5), INT2(0) )
      ENDIF  ! Solmod
!
! --- Check: does exe-file exist?
!
      IF ( TRIMLEN(FILENAME) .EQ. 0 ) THEN
           ERRSTR = 'RUN_PROG: Empty name of the executable was '// &
     &              'passed to run_prog'
           CALL FERR ( INT2(181), ERRSTR, INT2(0), INT2(0) )
      END IF
      INQUIRE ( FILE=FILENAME, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           WRITE ( ERRSTR, '("RUN_PROG Executable file ",A," not found")') &
     &             FILENAME(:TRIMLEN(FILENAME) )
           CALL FERR ( INT2(111), ERRSTR(1:I_LEN(ERRSTR)), INT2(0), INT2(0) )
      ENDIF
!
! --- Schedule the program
!
      IF ( STATE .EQ. 'PASS' ) THEN
             CALL SBIT ( PRE_IP(2), INT2(4), INT2(1) )
          ELSE IF ( STATE .EQ. 'WAIT' ) THEN
             CALL SBIT ( PRE_IP(2), INT2(4), INT2(0) )
          ELSE
             WRITE ( BUFSTR, '("pre_ip(2) = ",i5)' ) PRE_IP(2)
             CALL FERR ( INT2(211), 'Illegal state in RUN_PROG', INT2(0), &
     &            INT2(0) )
      ENDIF
!
! --- Build the character string given to execute
!
      ILF = TRIMLEN ( FILENAME )
      FNAME = FILENAME(:ILF)//' '//INTTODECIMAL(PRE_IP(1))
      ILF = TRIMLEN ( FNAME )
      FNAME = FNAME(:ILF)//' '//INTTODECIMAL(PRE_IP(2))
      ILF=TRIMLEN(FNAME)
      FNAME = FNAME(:ILF)//' '//INTTODECIMAL(PRE_IP(3))
      ILF=TRIMLEN(FNAME)
      FNAME = FNAME(:ILF)//' '//INTTODECIMAL(PRE_IP(4))
      ILF=TRIMLEN(FNAME)
      FNAME = FNAME(:ILF)//' '//PRE_LETRS
      CALL CLRCH ( CMD )
      IERR=NULL_TERM ( CMD, FNAME )
!
      IF ( IERR .NE. 0 ) THEN
           CALL FERR ( INT2(212), 'Error: null_term in run_prog', &
     &                 INT2(0), INT2(0) )
      END IF
!
! --- Run the command
!
      WRITE ( BUFSTR, '("Running command in run_prog")' )
      IERR = -5
      NTRY = 0
!
! --- Now launch the process
!
      IERR = EXECUTE_D ( PTR_CH(CMD), ARGC, ARGV, STATUS, PTRDEV )
!
      IF ( IERR .NE. 0 ) THEN
!
! -------- Failure to launch exec-module
!
           IF ( IERR .EQ. -1 ) THEN
                WRITE ( 6, * ) 'RUN_PROG: Bad args to execute'
              ELSE IF ( IERR .EQ. -2 ) THEN
                WRITE ( 6, * ) 'RUN_PROG: Fork failed in execute'
              ELSE IF ( IERR .EQ. -3 ) THEN
                WRITE ( 6, * ) 'RUN_PROG: Wait failed in execute'
              ELSE IF ( IERR .EQ. -4 ) THEN
                WRITE ( 6, * ) 'RUN_PROG: xdb fork failed'
              ELSE IF ( IERR .EQ. -5 ) THEN
                WRITE ( 6, * ) 'RUN_PROG: Not enough core'
              ELSE
                WRITE ( *, * ) 'RUN_PROG: Error', IERR
           ENDIF
!
           WRITE ( *, '("Trying: ",A)') FNAME(:TRIMLEN(FNAME) )
           CALL FC_EXIT ( 1 )
      ENDIF
!
      IF ( STATUS .GT. 10000 ) THEN
           WRITE ( 6, * ) 'RUN_PROG: process '//CMD(1:I_LEN(CMD))// &
     &                    ' was stopped by signal'
           CALL SIGMES ( STATUS-10000 )
         ELSE IF ( STATUS .NE. 0 ) THEN
           CALL CLRCH ( ERRSTR )
           CALL INCH  ( STATUS, ERRSTR )
           WRITE ( 6, * ) 'RUN_PROG: process '//CMD(1:I_LEN(CMD))// &
     &                    ' abnormally terminated with status code '// &
     &                      ERRSTR(1:I_LEN(ERRSTR))
           CALL EXIT ( 1 )
      END IF
!
! --- Reading status termination of the program which has been launched and
! --- terminated to this moment.
!
      IERR = FC_READ ( PIPE_IDS(1), PTR_NC(IRETBF), NBYTES )
!
      IF ( IERR .NE. NBYTES ) THEN
           IT = TRIMLEN(FILENAME)
           IF ( IT .LT. 1 ) IT = 1
           WRITE ( 6, 9945 ) FILENAME(1:IT)
 9945      FORMAT ( 'Program ', A, ' abnormally terminated' )
           IERR = FC_WRITE ( PIPE_IDS(2), PTR_NC(IABEND), NBYTES )
           CALL FC_EXIT ( 1 )
         ELSE IF ( CRETBF(1:5) .EQ. CABEND(1:5) ) THEN
           IERR = FC_WRITE ( PIPE_IDS(2), PTR_NC(IABEND), NBYTES )
           CALL FC_EXIT ( 1 )
         ELSE IF ( STATE(:4) .EQ. 'WAIT' ) THEN
           CALL UNPACK_RMPAR ( IRETBF(4) )
           RETURN
         ELSE IF ( CRETBF(1:1) .EQ. ' ' ) THEN
           IERR = FC_WRITE ( PIPE_IDS(2), PTR_NC(IRETBF), NBYTES )
           CALL FC_EXIT ( 0 )
         ELSE ! State is PASS
           CALL UNPACK_RMPAR ( IRETBF(4) )
           PROG = CRETBF(1:5)
      ENDIF
!
! --- And going back to the infinte loop
!
      GOTO 910
!
      END  !#!  RUN_PROG  #!#
