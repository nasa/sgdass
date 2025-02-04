      PROGRAM GLOBL
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GLOBL PROGRAM SPECIFICATION
!
! 1.1 Perform least_squares solution on one arc of a global solution.
!
! 1.2 REFERENCES:
!
! 2.  GLOBL INTERFACE
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
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'glocm.i'
      INCLUDE 'buff2.i'
      INCLUDE 'socom.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: setup,state,add_things
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2  KBIT
      LOGICAL*4  SKIP_ARC
      CHARACTER  HFFILE*(NAME_SIZE), FNAME*(NAME_SIZE), &
     &           STR*80, CDUM*80, TOKEN*16, GET_VERSION*54
      INTEGER*2  IERR, I, DECIMALTOINT, &
     &           IBUFF(40), IBUFF_PROC, IBUFF_ARCPE
      INTEGER*4  IOS, I4P55, I4P60, I4P0
      INTEGER*4   I_LEN
      INTEGER*2  INT2_ARG
      DATA I4P60, I4P55, I4P0 / 60, 55, 0 /
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR  921215  replaced nJ's with I4Pn's
!   kdb  951130  03/05/97 21:18:25 accidentally replaced with an actual sccs
!                version. Restore it.
!   pet  970823  Added special logic for running GAMB in batch mode.
!   pet  980707  Added logic for skipping arc if PROC returned status
!                "skip this arc"
!   jmg  990126  Fixed bug in user partial routine.  In interactive mode now
!                sets USRGxx file to 0. This is left over from previous global run.
!   pet  990405  Added call of GET_TIM
!   pet  2001.05.02  Changed interpretation os names of user partials/programs:
!                    first it is tried as the name with absolute path,
!                    then it is tried as the name in the current directory,
!                    then it is tried as the name in the directory SOLVE_PATH.
!
! 5. GLOBL PROGRAM STRUCTURE
!
! Pick up RMPAR variables and set up prelude common
!
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      CALL PRE_PROG()
      INCLUDE 'globl_version.i' ! Set revision date of the current version
!
! --- Initialize terminal screen if necessary
!
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
          CALL CLEAR_MN()
          STR = GET_VERSION()
          CALL SETCR_MN ( 79-I_LEN(STR), 0 )
          CALL REVERSE_ON_MN()
          CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
          CALL REVERSE_OFF_MN()
      ENDIF
      CALL SET_SIGNAL_CTRLC ( 3 )
      CALL TIM_INIT() ! Start timer
!
! --- Set up flags for flow of control
!
      CALL USE_GLBFIL   ( 'OR'  )
      CALL USE_GLBFIL_4 ( 'R'   )
      CALL USE_COMMON   ( 'ORC' )
      CALL HOL2CHAR ( HFEOPF, INT2(1), NAME_SIZE, HFFILE )
      CALL SETUP()
      CALL STATE()
      CALL USE_GLBFIL('WC' )
!
! --- Run user-specified program, if any
!
      IF ( USER_PRO .NE. ' ' ) THEN
           IF ( USER_BUF(1:1) .NE. ' ' ) THEN
                CALL CHAR2HOL   ( USER_BUF, IBUFF, INT2(1), INT2(80) )
                CALL USE_BUFFER ( IBUFF, INT2(40), 'OWC' )
           ENDIF
!
           CALL RUN_PROG ( USER_PRO, 'WAIT', INT2(0) )
      ENDIF
!
! --- Run user_specified partial program, if any  (interactive only)
!
      IF ( KUSER_PART  .AND.  .NOT. KBATCH ) THEN
           CALL RUN_PROG ( USER_PART_PROG, 'WAIT', INT2(0) )
           CALL USE_GLBFIL ( 'OR' )
           NUM_USER_GLOB = 0
!
! -------- Added JMGipson  99Jan27.  Write down USRGxx file with 0 number
! -------- of GLOBAL user parameters, since global user parameters are
! -------- forbidden in interactive mode
!
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRG'//PRE_LETRS
           OPEN  ( 66, FILE=FNAME, IOSTAT=IOS )
           WRITE ( 66, *) 0
           CLOSE ( 66   )
!
! -------- End JMGipson  99Jan27.
!
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'USRP'//PRE_LETRS
           OPEN ( 66, FILE=FNAME, IOSTAT=IOS )
           CALL FERR ( INT2(IOS), "Opening user partial file", INT2(0), INT2(0) )
           CDUM = '*'
           DO WHILE ( CDUM(1:1) .EQ. '*' )
              READ ( 66, '(A)', IOSTAT=IOS ) CDUM
              CALL FERR ( INT2(IOS), "Reading user partial file", INT2(0), &
     &             INT2(0) )
           ENDDO
           UPT_FLAG = UPT__UND
           CALL SPLITSTRING(CDUM,TOKEN,CDUM )
           NUM_USER_PART = DECIMALTOINT(TOKEN,IERR)
           ARC_USER_PART = DECIMALTOINT(TOKEN,IERR)
           CALL SPLITSTRING ( CDUM, TOKEN, CDUM )
           IF ( TOKEN(1:1) .NE. ' ' ) THEN
                READ ( UNIT=TOKEN, FMT='(I6)', IOSTAT=IOS ) UPT_FLAG
                IF ( IOS .NE. 0 ) THEN
                     CALL FERR ( INT2(680), "GLBL(globl) unsupported "// &
     &                   "second word "//TOKEN(1:16)// &
     &                   " in the header of the user partial file", INT2(0), &
     &                    INT2(0) )
                     STOP "GLOBL Abnormal termination"
                END IF
                IF ( UPT_FLAG .NE. UPT__FUL  .AND. &
     &               UPT_FLAG .NE. UPT__DEL  .AND. &
     &               UPT_FLAG .NE. UPT__CMP        ) THEN
                     CALL FERR ( INT2(690), "GLOBL(globl) unsupported "// &
     &                   "second word "//TOKEN(1:16)// &
     &                   " in the header of the user partial file", INT2(0), &
     &                    INT2(0) )
                     STOP "GLOBL Abnormal termination"
                 END IF
           END IF
!
           DO I=1,NUM_USER_PART
              CDUM = '*'
              DO WHILE (CDUM(1:1).EQ.'*')
                 READ ( 66, '(A)', IOSTAT=IOS ) CDUM
                 CALL FERR ( INT2(IOS), "Reading user partial file", &
     &                       INT2(0), INT2(0) )
                 ENDDO
                 IF ( CDUM(22:22) .EQ. 'G' ) THEN
                      NUM_USER_GLOB = NUM_USER_GLOB + 1
                 ENDIF
           ENDDO
!
           CALL USE_GLBFIL ( 'WC' )
           CLOSE ( UNIT=66)
           CALL USE_COMMON ( 'OR' )
           CALL PARCN()
           CALL USE_COMMON ( 'WC' )
      ENDIF
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Timing printout
!
           CALL TIM_GET ( 'GLOBL' )
      END IF
      IF ( GAMB_BATCH .AND. KBATCH ) THEN
!
! -------- Special logic for launching
!
           CALL RUN_PROG ( 'GAMB', 'WAIT', INT2(0) )
           CALL END_PROG()
      END IF
!
! --- Proceed with least squares chain as per flow control flags
!
      IBUFF_PROC = 0
      SKIP_ARC   = .FALSE.
!
! --- PROC (building normal system)
!
      IF ( MKNRMEQ ) THEN
           CALL RUN_PROG ( 'PROC', 'WAIT', INT2(0) )
           CALL USE_BUFFER ( IBUFF_PROC, INT2(1), 'ORC' )
           IF ( IBUFF_PROC .EQ. 1 ) SKIP_ARC = .TRUE.
      ENDIF
      IF ( SKIP_ARC ) THEN
           IBUFF_ARCPE = 1
         ELSE
           IBUFF_ARCPE = 0
      END IF
!
! --- ARCPE (arc parameters elimination)
!
      IF ( ELARCPM ) THEN
           CALL USE_BUFFER ( IBUFF_ARCPE, INT2(1), 'OWC' )
           CALL RUN_PROG   ( 'ARCPE', 'WAIT', INT2(0) )
      ENDIF
!
! --- ADDER (CGM update in non-fast modes)
!
      IF ( .NOT. SKIP_ARC ) THEN
           IF ( ADMKCGM ) THEN
                IF ( SUBTRACT_ARC ) THEN
                     CALL SUB_THINGS()
                  ELSE
                     CALL ADD_THINGS()
                ENDIF
          ENDIF
      ENDIF
!
! --- NORML (solving equations of conditions)
!
      IF ( INF2COV ) THEN
           CALL RUN_PROG ( 'NORML', 'WAIT', INT2(0) )
      ENDIF
!
      IF ( .NOT. SKIP_ARC ) THEN
           IF ( ARCCALC .AND.  .NOT. SUBTRACT_ARC ) THEN
                CALL RUN_PROG ( 'BACK', 'WAIT', INT2(0) )
                IF ( PURGARC ) CALL ACS_ARCFIL ( SAVAF, KBIT( PRE_IBATCH, &
     &               INT2(8)), 'P' )
           ENDIF
      ENDIF
      IF ( KOUTPUT ) THEN
           IF ( .NOT. SKIP_ARC ) THEN
                IF ( .NOT. KMINOUT  .OR.  KSCREEN ) THEN
                     IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6))) &
     &                    CALL END_MN
                     CALL RUN_PROG ( 'COVP', 'WAIT', INT2(0) )
                     IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6))) &
     &                    CALL START_MN
                ENDIF
           ENDIF
!
           IF ( KGLBOUT ) THEN
                CALL USE_BUFFER ( LCHAO, BUFF2_WORDS, 'OWC' )
                CALL SBIT ( PRE_IP(3), INT2(10), INT2(1) )
                CALL RUN_PROG ( 'ADJST', 'WAIT', INT2(0) )
                CALL SBIT ( PRE_IP(3), INT2(10), INT2(0) )
                IF ( .NOT. SKIP_ARC ) THEN
                     CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC' )
                     IF ( .NOT. SUBTRACT_ARC ) THEN
                          CALL RUN_PROG ( 'CRES', 'WAIT', INT2(0) )
                    END IF
                END IF
              ELSE
                IF ( .NOT. SKIP_ARC ) THEN
                     CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC' )
                     CALL RUN_PROG   ( 'CRES', 'PASS', INT2(0) )
                ENDIF
           ENDIF
      ENDIF
!
      CALL END_PROG()
      END  !#!  GLOBL  #!#
