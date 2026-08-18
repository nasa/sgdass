      PROGRAM  ENTER
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  enter PROGRAM SPECIFICATION
!
! 1.1 enter provides an alternate pathway to SOLVE.  It gets the
!     user's initials and makes sure that all the scratch files
!     can be opened.   Finally it calls OPTIN.
!
! 1.2 REFERENCES:
!
! 2.  enter INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: chksc,getspool,utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER LETT*2
      CHARACTER TOKEN*128, APPEND*128, NEWSTR*128, ARG_STR(7)*128
      CHARACTER RUNST*120
      CHARACTER NEW_WORK_DIR*255, CBUF*255, BUFSTR*255
      INTEGER*2 TRIMLEN, LETRX, NEWBUF(64), IPR
      LOGICAL*2 OKSCR,LETOK,CHKSC,BATCH, TEST, PRNT
      INTEGER*4 FDES,MAKE_SEMA
      INTEGER*2 TRM(9), NWORDS, WDLEN, IERR, IBUF(40), I
      LOGICAL*2 POST_EXIST
      EQUIVALENCE (IBUF(1),CBUF)
      CHARACTER  FNAME*63
      LOGICAL*2  KEXIST
      LOGICAL*4  LEX
      CHARACTER  GET_VERSION*54
      CHARACTER  STR*80, USER_NAME*80, USER_REALNAME*80, USER_E_ADDRESS*80
      INTEGER*4  LUN, IS, IOS, NARG
!
      EQUIVALENCE (LETRX,LETT), (NEWBUF,NEWSTR)
!
      DATA        OKSCR  / .FALSE. /
      INTEGER*2   M_PROC_I2, I_PROC_I2
      INTEGER*4   J1, M_PROC_I4, I_PROC_I4
      INTEGER*4,  EXTERNAL :: GET_UNIT, I_LEN, ILEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE  910614  Added background/foreground detection. If IRM(4) is zero,
!                background is assumed and if it is f(oreground) or anything
!                else, foreground is assumed. For background jobs & must be
!                entered as usual.
!   AEE  920102  enter now is used for foreground jobs only. New enter program
!                now handles background jobs.
!   AEE  920220  set bit 6 of word #2 of pre_ip (from precm.i) to 1 to indicate
!                foreground. No longer using use_common.
!   jlr  921215  replaced 0J with I4P0
!   kdb  960506  Make sure that solve code isn't being installed and
!                that it's okay to run the solve programs.
!   kdb  970723  Check for second post file which only locks out batch runs.
!   pet  970128  Added initialization of variable FAST_MODE from glbc4.i
!   pet  980615  Increased the lenght of batch control file from  36 to 128
!                symbols. Increased the length of the buffer for communication
!                with batch from 72 to 128 bytes.
!   pet  1999.05.31.  Enabled handling the 4-th argument. If the 4-th arguemnt
!                     is b then the process is treated by SOVLE as background.
!                     Otherwise it treats as forground. If the and error
!                     condition was detected by ferr then SOLVE chesks its
!                     status. If SOLVE is in foreground mode than it asks user
!                     to hit  <CR>  and proceed. If SOLVE is in backround mode
!                     than it terminates. For example,
!                       enter pe $Ts/job71 0 b -1  -- causes SOLVE to terminate
!                     if an error was deteceted by ferr.
!                       enter pe $Ts/job71
!                       enter pe $Ts/job71 0 f -1 -- WILL NOT cause SOLVE to
!                     terminate if an error was detected by ferr.
!
!   pet  2000.10.06   Improved error messages. Forced enter not to clear
!                     screen if user entered user initials. Enter returns
!                     status code 1 if it failed to launch OPTIN or BATCH.
!                     Added a check whether user scratch files are too old
!                     and not compatible with solve any more. If yes, then
!                     enter prints a descriptive error message and asks
!                     user to reset scratch files. It even reminds the old
!                     parameters of the scratch files!
!
!   pet  2000.12.28   Fixed a bug: ther previous version tried to save
!                     terminal parameters in TERMxx instead of termxx
!
!   pet  2001.08.09   Moved make_pipes upward and put it before an attempt
!                     to start curses. Otherwise under enigmatic curcumstances
!                     Solve didn't work
!
!   pet  2002.03.18   Restore the line CALL GETSPOOL ( IPR ) which was somehow
!                     lost and resulted to loowing spool file after restoring
!                     batch runs
!
!   pet  2004.11.16   Forced enter generate a clear error message in the case
!                     if scratch files are missing
!
!   pet  2007.07.19   Added support of 
!                           the 6-th argument: processor index and
!                           the 7-th argument: total number of processors
!   pet  2007.08.10   Added support of the 40the arument:
!                     "a" -- to start in autorecovery mode. If recovery is 
!                            possible BATCH will recover without dialogue with
!                            a user
!                     "n" -- to start in norecovery mode. Even if recovery is 
!                            possible BATCH will start solution anew without 
!                            dialogue with a user
!   pet  2024.01.17   Improved hadnling PSOLVE_WORK_DIR environment variable &
!                     and handling error messages
!
! 5.  enter PROGRAM STRUCTURE
!
!   GET RMPAR PARAMETERS
!
!
! --- Get run-tine arguments
!
      NARG = IARGC ()
      DO 410 J1=1,7
         CALL CLRCH ( ARG_STR(J1) )
         IF ( J1 .LE. NARG ) THEN
              CALL GETARG ( J1, ARG_STR(J1) )
         END IF
 410  CONTINUE
      CALL TRAN ( 11, ARG_STR(1), LETT )
!
      IF ( ARG_STR(3)(1:1) .NE. '0' ) THEN
           CALL POST_CHECK ( POST_EXIST )
           IF ( POST_EXIST ) THEN
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Determine whether it is batch mode or interactive
!
      IF ( ILEN(ARG_STR(2)) .GT. 0   .AND.ARG_STR(2)(1:2) .NE. '0 ')         &
     &     THEN
           BATCH = .TRUE.
         ELSE
           BATCH = .FALSE.
      END IF
!
      IF ( BATCH ) THEN
!
! -------- Check for post file that precludes batch runs, but not
! -------- interactive ones
!
           FNAME = SOLVE_SAVE_DIR//SOLVE_POST_FILNAMB
           CALL BIN_EXIST ( FNAME, KEXIST )
           IF ( KEXIST)  THEN
                WRITE ( 6, "('***Batch runs are currently unavailable, ',/, &
     &                       'probably due to the posting of code. ',/, &
     &                       'Interactive solve is still available.***')" )
                CALL EXIT ( 1 )
           ENDIF
      ENDIF
!
      DO I=1,9
         TRM(I) = 0
      ENDDO
      CALL SAVE_TERM(TRM )
!
! --- Make pipes. Keep in mind: pipes should be opened BEFORE starting curses!!
!
      CALL MAKE_PIPES()
!
      IF ( .NOT. BATCH ) THEN
           CALL START_MN()
           STR = GET_VERSION ()
           CALL ADDSTR_F ( STR )
           CALL SET_SIGNAL_CTRLC ( 3 )
         ELSE 
           CALL SET_SIGNAL_CTRLC ( 2 )
      END IF
!
 100  CONTINUE
      IF ( ILEN(ARG_STR(1)) .LE. 0  .AND.  .NOT. BATCH ) THEN
!
! -------- Interactive mode. Ask user to enter user initials
!
           CALL NL_MN()
           CALL ADDSTR_F ( "User initials, please? " )
!
           CALL GETSTR_F ( BUFSTR )
           CALL CASEFOLD ( BUFSTR )
           READ ( BUFSTR, '(A2)' ) LETT
           IF ( LETT .EQ. '::' ) THEN
                CALL ADDSTR_F ( "Solve terminated. Good bye!" )
                CALL REFRESH_MN()
                CALL END_MN()
                CALL EXIT ( 1 )
           ENDIF
           CALL NL_MN()
      END IF
!
! --- Check for valid initials
!
  200 CONTINUE
      IF ( .NOT. LETOK(LETT) ) THEN
           IF ( .NOT. BATCH ) THEN
!
! ------------- Interactive mode. Ask user to repeat
!
                CALL ADDSTR_F ( "No good, try again, or :: to terminate" )
                CALL CLRCH ( ARG_STR(1) )
                CALL NL_MN()
                GOTO 100
             ELSE
!
! ------------- Non-interactive mode. Send a message and terminate
!
                WRITE ( 6, '(A)' ) 'Intials '//LETT//' are not supported'
                WRITE ( 6, '(A)' ) 'enter: abnormal termination'
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( .NOT. BATCH ) THEN
!
! -------- Interactive mode: close curses
!
           CALL END_MN()
      END IF
!
!---- Set up prelude common
!
      CALL SAVE_TERM ( TRM )
      READ ( UNIT=LETT, FMT='(A)' ) LETRX
      CALL SETUP_PRELUDE(LETRX )
!
      CALL SBIT ( PRE_IP(2), INT2(6), INT2(1) ) ! Set bit 6 of word 2 to 1 to indicate foreground.
      CALL SBIT ( PRE_IP(2), INT2(7), INT2(0) ) 
      CALL SBIT ( PRE_IP(2), INT2(8), INT2(0) ) 
!
! --- Examine the 4-th argument
!
      IF ( ARG_STR(4)(1:1) .EQ. 'b'  .OR.  ARG_STR(4)(1:1) .EQ. 'B' ) THEN
           CALL SBIT ( PRE_IP(2), INT2(6), INT2(0) ) ! Set bit 6 of word 2 to 0
!                                        ! to indicate background
      END IF
      IF ( ARG_STR(4)(1:1) .EQ. 'a'  .OR.  ARG_STR(4)(1:1) .EQ. 'A' ) THEN
           CALL SBIT ( PRE_IP(2), INT2(6), INT2(0) ) ! Set bit 6 of word 2 to 0
!                                        ! to indicate background
           CALL SBIT ( PRE_IP(2), INT2(7), INT2(1) ) ! Set bit 7 of word 2 to 1
!                            ! to indicate that Solve will start in recovery mode
      END IF
!
      IF ( ARG_STR(4)(1:1) .EQ. 'n'  .OR.  ARG_STR(4)(1:1) .EQ. 'N' ) THEN
           CALL SBIT ( PRE_IP(2), INT2(6), INT2(0) ) ! Set bit 6 of word 2 to 0
!                                        ! to indicate background
           CALL SBIT ( PRE_IP(2), INT2(8), INT2(1) ) ! Set bit 8 of word 2 to 1
!                            ! to indicate that Solve will start from the very
!                            ! beginning
      END IF
!
! --- Save terminal properties
!
      CALL W_TRM ( PTR_CH(PRE_SCR_DIR(:PRE_SD_LEN)//'term'//LETT//CHAR(0)), TRM )
!
! --- Set procedure for catching signals
!
      IS = FC_CATCHSIGS ()
!
! --- Check whether to use a test version of Solve
!
      IF ( ARG_STR(5)(1:2) .EQ. '-1' ) THEN
           TEST = .TRUE.
           CALL SET_TESTV ( TEST )
         ELSE
           TEST = .FALSE.
      END IF
      IF ( ILEN(ARG_STR(7)) > 0 ) THEN
           CALL CHIN ( ARG_STR(7), M_PROC_I4 )
           IF ( M_PROC_I4 > 128 .OR. M_PROC_I4 < 1 ) THEN
                WRITE ( 6, '(A)' ) 'enter: wrong 7-th argument: '//ARG_STR(7)
                CALL EXIT ( 1 )
           END IF
           CALL CHIN ( ARG_STR(7), I_PROC_I4 )
           IF ( I_PROC_I4 > M_PROC_I4 .OR. I_PROC_I4 < 1 ) THEN
                WRITE ( 6, '(A)' ) 'enter: wrong 6-th argument: '//ARG_STR(6)
                WRITE ( 6, '(A)' ) 'enter: it should be in the range [1,M_PROC]'
                CALL EXIT ( 1 )
           END IF
           I_PROC_I2 = I_PROC_I4
           M_PROC_I2 = M_PROC_I4
         ELSE 
           ARG_STR(6) = '1'
           ARG_STR(7) = '1'
           I_PROC_I2 = 1
           M_PROC_I2 = 1
      END IF
!
! --- Open all scratch files
!
      CALL CLRCH ( CBUF )
      IERR=FC_GETENV ( PTR_CH ( 'PSOLVE_WORK_DIR'//CHAR(0)), PTR_NC(IBUF) )
      IF ( IERR .GT. 0 ) THEN
           NEW_WORK_DIR=CBUF(1:IERR)
         ELSE
           NEW_WORK_DIR=SOLVE_WORK_DIR
      ENDIF
      WDLEN=TRIMLEN(NEW_WORK_DIR)
      IF ( NEW_WORK_DIR(WDLEN:WDLEN) .NE. '/' ) THEN
           WDLEN=WDLEN+1
           NEW_WORK_DIR(WDLEN:WDLEN) = '/'
      ENDIF
!
! --- Get user name
!
      CALL GETINFO_USER   ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      IF ( ILEN(USER_REALNAME) .EQ. 0 ) THEN
           USER_REALNAME = 'user '//USER_NAME
      END IF
!
! --- Check configuration file
!
      INQUIRE ( FILE=NEW_WORK_DIR(1:I_LEN(NEW_WORK_DIR))//'CONF'//LETT, &
     &          EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) 'Dear '//USER_REALNAME(1:I_LEN(USER_REALNAME))// &
     &                        ', Solve cannot find your scratch files '
           WRITE ( 6, '(A)' ) 'which correspond to initials '//LETT
           WRITE ( 6, '(A)' ) 'for instance, '//NEW_WORK_DIR(1:I_LEN(NEW_WORK_DIR))//'CONF'//LETT
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) 'You need to run solve_reset command first' 
           IF ( ILEN(CBUF) > 0 ) THEN
                WRITE ( 6, '(A)' ) 'then check PSOLVE_WORK_DIR environment variable'
                WRITE ( 6, '(A)' ) 'Please, make sure you run psovle and psolve_reset with '
                WRITE ( 6, '(A)' ) 'the same setting of PSOLVE_WORK_DIR: '//TRIM(CBUF)
              ELSE
                WRITE ( 6, '(A)' ) 'Please, check that psovle and psolve_reset are launched'
                WRITE ( 6, '(A)' ) 'without setting PSOLVE_WORK_DIR environment variable'
           END IF
           WRITE ( 6, '(A)' ) ' '
           CALL EXIT ( 1 ) 
      END IF
!
! --- Read configuration file
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=NEW_WORK_DIR(1:I_LEN(NEW_WORK_DIR))//'CONF'//LETT, &
     &       STATUS='OLD', IOSTAT=IOS )
      IF ( LEX .AND. IOS .NE. 0 ) THEN
           WRITE ( 6, '(A)'    ) 'enter: Error in opening '//NEW_WORK_DIR(1:I_LEN(NEW_WORK_DIR))//'CONF'//LETT
           WRITE ( 6, '(A,I8)' ) 'enter: Trap of internal control IOS=',IOS
           WRITE ( 6, '(A)'    ) 'enter: Abnormal termination'
           CALL EXIT ( 1 )
      END IF
!
! --- Reading a line from configuration file
!
      READ ( LUN, '(A)', IOSTAT=IOS ) STR
      IF ( LEX  .AND.  IOS .NE. 0 ) THEN
           WRITE ( 6, '(A)'    ) 'enter: Error in reading '//NEW_WORK_DIR(1:I_LEN(NEW_WORK_DIR))//'CONF'//LETT
           WRITE ( 6, '(A,I6)' ) 'enter: Trap of internal control IOS=',IOS
           WRITE ( 6, '(A)'    ) 'enter: Abnormal termination'
           CALL EXIT ( 1 )
         ELSE IF ( .NOT. LEX ) THEN
           STR = 'Creation_date: 1979.08.03-20:00:00'
      END IF
!
! --- Check date of scratch file creation as written in configuration file
! --- CONFxx with the latest compatible date defined in solve.i
!
      IF ( STR(16:25) .LT. SCRATCH_DATE ) THEN
!
! -------- Date ismatch: configuration file is incompatible!
! -------- Generate an error message
!
           WRITE ( 6, '(A)' ) 'Dear '//USER_REALNAME(1:I_LEN(USER_REALNAME))// &
     &                        '! Your scratch files are too old and '
           WRITE ( 6, '(A)' ) 'do not correspond to the current version of '// &
     &                        'Solve'
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) 'Please reset your scratch files for user '// &
     &                        'initials '//LETT//' using program psolve_reset'
           IF ( LEX ) THEN
                WRITE ( 6, '(A)' ) ' '
                WRITE ( 6, '(A)' ) 'Just remind: your old parameters '// &
     &                             'of scratch files were:'
!
                CALL CLRCH ( STR )
                READ ( LUN, '(A)', IOSTAT=IOS ) STR
                WRITE ( 6, '(A)' ) STR(1:I_LEN(STR))
!
                CALL CLRCH ( STR )
                READ ( LUN, '(A)', IOSTAT=IOS ) STR
                WRITE ( 6, '(A)' ) STR(1:I_LEN(STR))
                WRITE ( 6, '(A)'   ) 'enter: Abnormal termination'
           END IF
           CALL EXIT ( 1 )
      END IF
      CLOSE ( UNIT=LUN )
!
! --- Set spooling state as per SOCOM
!
      CALL GETSPOOL ( IPR )
      PRNT = IPR .EQ. 23
      CALL SET_SPOOL ( PRNT )
!
! --- Save run string to write later to spool file
!
      CALL USE_GLBFIL ( 'OR' )
      RUN_STRING = ' '
      RUNST = 'ENTER'
      DO I=1,5
         TOKEN = ' '
         CALL RCPAR(I,TOKEN )
         RUNST = RUNST(1:TRIMLEN(RUNST))//' '//TOKEN
      ENDDO
      RUN_STRING(1:50) = RUNST(1:50)
      CALL USE_GLBFIL ( 'WC' )
!@!
!@! --- Try to lock user initials; quit if already locked
!@!
!@      FDES = MAKE_SEMA(NEW_WORK_DIR(:WDLEN)//'LOCK'//PRE_LETRS, 'W', 'N' )
!@      IF ( FDES .EQ. -1 ) THEN
!@           WRITE ( *, '(A)' ) 'Letters '//LETT//' already in use'
!@           WRITE ( *, '(A)' ) 'Abnormal termination'
!@           CALL EXIT ( 1 )
!@      ENDIF
!
! --- Check, where Solve user initials are in use
!
      CALL CHECK_SOLVE_LOCK()
!
! --- If we are here, then the Solve user initials are NOT in use.
! --- Then, let us lock it!
!
      CALL SOLVE_LOCK()  
!
      OKSCR = .TRUE.
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'PARF'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'CORF'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'NRMF'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'PLTF'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'RESF'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'OBSF'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'COMM'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'NAMF'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'GLBF'//LETT)
      OKSCR=OKSCR.AND.CHKSC(NEW_WORK_DIR(:WDLEN)//'SARF'//LETT)
!
! --- Terminate if not all scratch files are there
!
      IF ( .NOT. OKSCR ) THEN
           WRITE ( *, '(A)' ) 'Not all scratch files were found'
           WRITE ( *, '(A)' ) 'Abnormal termination'
           CALL EXIT ( 1 )
      END IF
      CALL USE_GLBFIL ( 'OR' )
      IF ( BATCH ) KUSER_PART = .FALSE.
      CALL USE_GLBFIL ( 'WC' )
!
      IF ( .NOT. BATCH ) THEN
!
! -------- Interactive more: Call OPTIN
!
           CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
           GOTO 99999
        ELSE
!
! -------- Prepare stuff for launching BATCH
!
! -------- Make the run string for batch  in newstr
!
           NEWSTR = 'RU,BAT'//LETT//','//LETT
           IF ( ILEN(TOKEN) == 0 ) TOKEN = '0'
           NEWSTR = APPEND ( NEWSTR, ','//TOKEN )
!
! -------- Appened the processor index
!
           NEWSTR = APPEND ( NEWSTR, ','//ARG_STR(6) )
!
! -------- Appened the total number of processors
!
           NEWSTR = APPEND ( NEWSTR, ','//ARG_STR(7) )
!
! -------- Control file NAMR
!
           CALL RCPAR ( INT2(2), TOKEN )
           NEWSTR = APPEND ( NEWSTR, ','//TOKEN )
!
! -------- Proceed flag
!
           CALL RCPAR ( INT2(3), TOKEN )
           NEWSTR = APPEND ( NEWSTR, ','//TOKEN )
!
! -------- Find length and schedule BATCH
!
           NWORDS=(TRIMLEN(NEWSTR)+1)/2
           CALL USE_BUFFER ( NEWBUF, INT2(64), 'OWC' )
           CALL RUN_PROG ( 'BATCH', 'PASS', INT2(0) )
    write ( 6, * ) 'ENTER-494 ' ; call flush ( 6 ) ; call exit ( 0 ) ! %%%
      ENDIF
!
! --- LAST STATEMENT
!
99999 CONTINUE
      CALL REMOVE_SOLVE_LOCK()  
      END  !#!  ENTER  #!#
