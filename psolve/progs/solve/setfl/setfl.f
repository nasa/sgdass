      PROGRAM SETFL
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETFL PROGRAM SPECIFICATION
!
! 1.1
!     SETFL is the program which will set the parameter flags
!     which determine whether or not a parameter is to be
!     estimated. Also see STFLG.
!
! 1.2 REFERENCES:
!
! 2.  SETFL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: None
!       CALLED SUBROUTINES: sldb,stflg,srflg,rmflg
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2    IKONT, IFLAG, KBITN, ICOUNT, ICHR
      INTEGER*4    NPMAX, IX, IY, IDUM, MAX_GCLOCK_DEG
      CHARACTER    BUFSTR*79, STR*80
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- IDBSEL needed for D.B. selection
! --- IDCSEL for DB selection in CRES
!
!
! 4.  HISTORY
!
!     MODIFICATIONS:
!     81-9-31 JWR: Modified to use year, month, day, hour, minute
!     82-2-16 MAP: Added options to select data bases and to select
!                  baselines (previously done in PROC).
!     82-3-22 MAP: To skip baseline brecords inNAMFIL (subr. SLDB)
!     82 0412 CMA: Use 18th element of IDBEND for DB selection for CRES
!     83-4-14    : Inserted logic to decided if SETFL is being scheduled
!                  by SDBH. If SO , then buffer IPASS is read.
!                  IPASS(3) was added(IMPAR(5).EQ. 99 IF SDBH SCHEDULES)
!     84-1-1   AM: Enabled to choose group, phase or N.B.
!     84-2-1   AM: Direct route between baseline and data base selection
!     84-7-1   AM: Made compatible with multi-user SOLVE
!     85-5-21 JWR: Delete solar corona flags.
!     85-9-1  JWR: Nutation flag logic added.
!     86-4-16 JWR: New style NAMFIL coded.
!     86-6-4  JWR: Nutation time series logic restored.
!     86-6-4  JWR: GLBCM info display and reset put on 'last page'.
!     87-1-25 JWR: 'Last Page' cleaned up and bug in data type selection
!                  logic fixed.
!     87-5-27 KDB: Uniform PARFIL access
!     87-9-24 JWR: Cursor seting for turning of input and output cgm mod.
!     88-5-05 JWR: Implicit none.
!     90-11-26 MWH: Corrected test on IKONT (changed AND to OR)
!     96-04-16 KDB: Convert hardcoded date to sccs-based date.
!                   Add sccsid parameter for version tracking.
!                   Pass general setfl program date to stflg routine, which
!                   had its own date.
!     97-07-17 PET: Forced all calls to use_common to close and open file
!     97-12-04 PET: Removed initialization of IBLSEL array (this work is done
!                   sdbh or gtsup).
!     98-07-06 PET: Added call of SET_PATHS in order to allow to set SOLVE_HELP_DIR
!                   environment variable if it was not set up before
!
! 5.  SETFL PROGRAM STRUCTURE
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
      INCLUDE 'setfl_version.i' ! Set revision date of the current version
      CALL USE_BUFFER ( IKONT, INT2(1), 'ORC' )
      CALL SET_SIGNAL_CTRLC ( 3 )
!
      CALL START_MN()
      CALL SETCR_MN ( 0, 0 )
!
      IF ( IKONT.GE.1 .AND. IKONT.LE.9 ) THEN
           CONTINUE
        ELSE
           IF ( IKONT.GT.-1 .OR. IKONT.LT.-6 ) IKONT = 1
      ENDIF
!
! --- Turn off  OPTIN
!
      ICOUNT=0
!
!     Get spool file information
!
!     Open and read SOCOM from disk.
!
      CALL USE_COMMON ( 'ORC' )
      CGM_TYPE = .FALSE.  !  Set once more session-type of socom
      CALL SOCOM_EXT()
!
! --- Open and read GLBCM. Set up GLBFIL.
!
      CALL USE_GLBFIL   ( 'OR'  )
      CALL USE_GLBFIL_3 (  'R'  )
      CALL USE_GLBFIL_4 (  'RC' )
!
! --- Open PARFIL and READ site and star names.
!
      CALL USE_PARFIL ( 'ORC' )
!
! --- If data bases were just picked up, pass their OBSFIL pointer into common.
!
      IFLAG = KBITN ( IDBSEL, INT2(16) )
      IF ( IFLAG.EQ.1) THEN  !
           CALL SLDB  ( IKONT,  ICHR )
           CALL SWBIT ( IDBSEL, INT2(16) )
           IFLAG = 0
!
! -------- Set all CRES selection bits on
!
           IDCSEL = -1
      END IF  !
!
! --- Set the maximum number of parameters
!
      NPMAX = NRMFL_PARMS
!
! --- Get the maximu global clock polynomial degree.
!
      CALL GET_GCLOCK_DEG ( MAX_GCLOCK_DEG )
!
! --- Skip to setting the correct flags according to IKONT.
!
      IF(IKONT.EQ.-1) GO TO 51
      IF(IKONT.EQ.-2) GO TO 55
      IF(IKONT.EQ.-5) GO TO 55
      IF(IKONT.EQ.-6) GO TO 55
!
! --- Schedule the site flag setting subprogram.
!
   50 CONTINUE
      CALL STFLG ( IKONT, NPMAX, MAX_GCLOCK_DEG )
!
      IF ( IKONT .EQ. -4 ) THEN  !terminate SOLVE
!
! ------- Count the parameter flags
!
          ICLMAX = 0
          CALL PARCN()
          CALL USE_COMMON ( 'OWC' )
          CALL RUN_PROG   ( 'SLEND', 'PASS', INT2(0) )
          END IF  !terminate SOLVE
      IF(IKONT.EQ.-3) GO TO 60
      IF(IKONT.EQ.-7) GO TO 60
      IF(IKONT.EQ.-1) GO TO 51
      IF(IKONT.EQ.-2) GO TO 55
      IF(IKONT.EQ.-5) GO TO 55
      IF(IKONT.EQ.-6) GO TO 55
!
!     Display and set source flags.
!
   51 CONTINUE
      CALL SRFLG ( IKONT )
      IF(IKONT.EQ.-3) GO TO 60
      IF(IKONT.EQ.-7) GO TO 60
      IF(IKONT.EQ.-2) GO TO 55
      IF(IKONT.GE.0 ) GO TO 50
!
! --- Display and set remaining flags.
!
   55 CONTINUE
      CALL RMFLG ( IKONT, NPMAX )
      IF ( IKONT .EQ. -3 ) GOTO 60
      IF ( IKONT .EQ. -1 ) GOTO 51
      IF ( IKONT.EQ.-15 ) THEN
           CALL RUN_PROG ( 'SLEND', 'PASS', INT2(0) )
      ENDIF
      GOTO 50
!
!     Count the parameter flags.
!
  60  ICLMAX = 0
      CALL PARCN()
      IF(NPARAM .LE. NPMAX) GO TO 800
!
      CALL USE_COMMON ( 'OWC' )
      WRITE(bufstr,700) NPARAM,PRE_ILETRS
      call addstr_f(bufstr )
      call nl_mn()
      WRITE(bufstr,701) NPMAX
      call addstr_f(bufstr )
      call nl_mn()
      call addstr_f(" Return to continue" )
      call nl_mn()
  700 FORMAT(I5," Parameters have been selected, but your ", &
     &       'normal equations file NRMF',A2)
  701 FORMAT(' was only dimensioned to ',I6 )
      CALL SENKR_MN ( IX, IY, IDUM )
!
      GO TO 50
!
!     Write out COMMON and close the file.
!
  800 CONTINUE
      CALL USE_COMMON ( 'OWC' )
!
! --- Turn on and schedule GLOBL or OPTIN and terminate
!
      CALL END_MN()
      IF ( IKONT.EQ.-7 ) GOTO 65
      CALL RUN_PROG( 'GLOBL', 'PASS', INT2(0) )
!
   65 CONTINUE
      CALL RUN_PROG( 'OPTIN', 'PASS', INT2(0) )
!
      END  !#!  SETFL  #!#
