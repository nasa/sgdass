      PROGRAM CRES_LAUNCH
      INCLUDE 'solve.i'
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      CHARACTER    STR*54
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL ERR_MODE ( 'NO_PROBE' ) 
      CALL CRES_HEAD()
      END  PROGRAM  CRES_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  CRES_HEAD
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CRES PROGRAM SPECIFICATION
!
! 1.1 CRES is the program which will read the data from OBSFIL and
!     the B array from NRMFIL and will calculate the postfit residuals
!     by a Taylor series expansion.  CRES will write these residuals
!     into the residual file, identified by run code.
!
! 1.2 REFERENCES:
!
! 2.  CRES INTERFACE
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
      INCLUDE 'oborg.i'
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'crecm.i'
      INCLUDE 'buff2.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
      INCLUDE 'fast.i'
      INCLUDE 'heo.i'
      INCLUDE 'cnstr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: first,secnd,third
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 CMENU
      LOGICAL*2 KBIT
      INTEGER*4 IPTR, PAGEWID, PAGELEN, IUER
      CHARACTER LBUF(CRES_BUF_LEN)*120, STR*54
      ADDRESS__TYPE :: ADDR_ARR
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4  I_LEN, INT4_ARG
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*8    MATSIZE, STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      CHARACTER  GET_VERSION*54
      INTEGER*4, EXTERNAL :: MAT_E, ILEN
      ADDRESS__TYPE :: MAT_E4
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  880609  15 ps delay fit logic added
!   JWR  880610  Test removed which suppressed sending baseline info
!                to SARFIL when no good obs for baseline
!   JWR  880611  Logic added to make certain that site no good
!                observations on any baseline produces no output
!   JWR  880619  Logic for printing baseline stats improved.  Baseline
!                order of listing will be affected
!   JRR  890113  Inserted flag passed by OPTIN which indicates whether
!                or not to write out CRES menu (normally not)
! WEH,CEK 890113 Elevation cutoff logic implemented by site
!   jwr  931214  Source pointer arrays intialized so that the list
!                of source observation fits come out in ra order.
!   pet  970712  Changed messages in the mode when CRES called by REWAY
!   pet  980505  Put call of CREMU here instead of SECND
!   pet  980706  Added call of SET_PATHS in order to allow to set SOLVE_HELP_DIR
!                environment variable if it was not set up before
!   pet  980920  Added calculation actual and nominal duration of the session
!   pet  990315  Re-arranged code. Moved some code to cres_do. Added call
!                of program ADJST.
!   pet  990403  Added an additional argument to call of cres_do
!   pet  2002.12.26  Fixed the error: the previous version skipped CRES_DO
!                    in the case of batch global solution
!   pet  2003.12.09  Replaced fc_gwinw, fc_gwinsz  calls with GET_TERMSIZE
!   pet  2004.11.09  Added reading HEO file if needed.
!   pet  2018.03.22  Added support of PSOVLE_MINOUTPUT environment variable.
!                    Umless it set, full output is set
!
! 5.  CRES PROGRAM STRUCTURE
!
! Get input parameters
!
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
!
      CALL PRE_PROG()
      CALL SET_PATHS() ! Setting environment variables to PGPLOT and SOLVE_HELP_DIR
      INCLUDE 'cres_version.i' ! Set revision date of the current version
!
! --- Get value for flag indicating whether to write menu. CMENU
! --- is set in OPTIN; value passed to IMENU which lives in CRECM.
!
      CALL USE_BUFFER ( CMENU, INT2(1), 'ORC' )
      CALL SET_SIGNAL_CTRLC ( 3 )
      IMENU   = CMENU
      IPTR    = 0
      KFULLOUT = .TRUE.
      KMINOUT  = .FALSE.
      CALL GETENVAR ( 'PSOLVE_MINOUTPUT', STR )
      IF ( STR == 'YES' ) THEN
           KFULLOUT = .FALSE.
           KMINOUT  = .TRUE.
      END IF
!
! --- Determine width of the screen
!
      CALL GET_TERMSIZE ( PAGELEN, PAGEWID )
      PAGEWID = PAGEWID - 1 
      IF ( PAGEWID .GT. 120 ) PAGEWID = 120
      IF ( PAGEWID .LT. 79  ) PAGEWID = 79
!
! --- Send version message to user
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 ( 'OR' )
      VTD_ADR = 0
      CALL USE_GLBFIL_4 ( 'W' )
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
           CALL START_MN()
           STR = GET_VERSION()
           CALL SETCR_MN ( 79-I_LEN(STR), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
           IPTR=IPTR+1
           WRITE ( LBUF(IPTR), '(A)' ) STR(1:I_LEN(STR))
!
           if ( kbit ( pre_ip(3), INT2(12)) ) then
                call clrch ( str )
                call inch  ( int4(reway_itcou), str )
                CALL SETCR_MN ( 0, 0 )
                call addstr_f ( '  REWAY --> CRES      Iteration '// &
     &                         str(1:i_len(str)) )
                call setcr_mn ( 50, 0 )
              else
                call nl_mn()
           end if
          call refresh_mn()
      END IF
!
      CALL USE_COMMON ( 'OR' )
!
! --- Extension of socom to socom_plus
!
      CALL SOCOM_EXT()
!
      IF ( KSPOOL ) CALL USE_SPOOL ( 'OS' )
!
!@      MATSIZE = 256*( (MAT_E(MAX_PAR,NPARAM)+255 )/ 256 )
      MATSIZE = 8*MAT_E4 ( M_GPA, NPARAM )
      CALL GET_MEM ( MATSIZE, ADDR_ARR )
!
! --- Read covariance matrix saved by NORML or BACK
!
      CALL USE_NRMFIL ( %VAL(ADDR_ARR), NPARAM, 'ORC' )
!
! --- Initialization of B3DOBJ, B1B3DOBJ
!
      CALL F__CLR_IND ( 4, FAST_MODE, %VAL(0), B3DOBJ, B1B3DOBJ  )
!
      IF ( ILEN(FINAM_HEO) .GT. 0 ) THEN
           IUER = -1
           CALL GHEO ( FINAM_HEO, NAME_HEO, L_HEO, STAT_HEO, &
     &                 ADR_HEO, HEO_EPOCH_SEC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7451, -1, 'CRES_HEAD', 'Error '// &
     &              'during attempt to read the file with harmonic Earth '// &
     &              'orientation parameters '//FINAM_HEO )
                STOP 'CRES(proc_head) Abnormal termination'
           END IF
           STAT_HEO  = HEO__READ
           CALL USE_GLBFIL_4 ( 'OWC' )
      END IF
!
      IUER = -1
      CALL READ_CNSTR ( CNSTROBJ, CNI__LOC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7452, -1, 'CRES_HEAD', 'Error in an '// &
     &         'attempt to read constraints imposed on local parameters' )
           STOP 'CRES(cres_heah) Abnormal termination'
      END IF
!
      CALL CRES_DO ( IMENU, NPARAM, %VAL(ADDR_ARR), B3DOBJ, CNSTROBJ, &
     &               CRES_BUF_LEN, LBUF, IPTR, PAGEWID )
      IF ( KSPOOL ) THEN
           CALL USE_SPOOL  ( 'C' )
           CALL USE_COMMON ( 'OWC' )
      END IF
!
! --- Pass control to ADJST
!
      IF ( .NOT. KBATCH ) THEN
          CALL USE_GLBFIL_4 ( 'OR' )
          FL_VTD_SES = .FALSE.
          CALL USE_GLBFIL_4 ( 'WC' )
      END IF
      BUFF2_WORDS = (   LOC(BUFF2_I2_FILLER) - LOC(LCHAO) &
     &                + SIZEOF(BUFF2_I2_FILLER) )/2
      IF ( .NOT. KSCREEN ) THEN
           ISCREEN = ISCREEN_NO
         ELSE
           ISCREEN = 0
      END IF
      CALL USE_BUFFER ( LCHAO, BUFF2_WORDS, 'OWC' )
      CALL RUN_PROG   ( 'ADJST', 'PASS', INT2(0) )
!
      END  SUBROUTINE  CRES_HEAD  !#!#
