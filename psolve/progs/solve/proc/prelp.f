      SUBROUTINE PRELP ( ARR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PRELP PROGRAM SPECIFICATION
!
! 1.1 This is the pre-loop, which sets things up for the main loop.
!
! 1.2 REFERENCES:
!
! 2.  PRELP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
      real*8 ARR(*)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: proc
!       CALLED SUBROUTINES: nrmst
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 JA, JB
      INTEGER*8 NELEM
      INTEGER*2 LDBNAM(5,15), IDBVER(15), NUMDD
      LOGICAL*2 KBIT
      CHARACTER STR*54, GET_VERSION*54
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN
!
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  940201  Implement dynamic memory allocation for normal matrix
!   kdb  951207  Integer*4 number of observations.
!   pet  970712  Change banner for the case when PROC called from REWAY
!   pet  980720  Moved reading of GLBFIL from prelp to proc
!   pet  990104  Fixed a bug: there was an attempt to initilize array ARR
!                in fast modes
!   pet  990108  Added more initialization
!
! PRELP PROGRAM STRUCTURE
!
! --- Display initial message to screen
!
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
          CALL CLEAR_MN()
          STR = GET_VERSION()
          CALL SETCR_MN ( 79-I_LEN(STR), 0 )
          CALL REVERSE_ON_MN()
          CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
          CALL REVERSE_OFF_MN()
!
          if ( kbit ( pre_ip(3), INT2(12)) ) then
               call clrch ( str )
               call inch  ( int4(reway_itcou), str )
               CALL SETCR_MN ( 0, 0 )
               call addstr_f ( '  REWAY --> PROC      Iteration '// &
     &                         str(1:i_len(str)) )
               call setcr_mn ( 50, 0 )
          end if
          call refresh_mn()
      ENDIF
!
      IF ( TRAIN ) THEN
!
! -------- Get info from PARFIL
!
           CALL USE_PARFIL ( 'ORC' )
         ELSE
!
! -------- No train mode. Read socom once more
!
           CALL USE_COMMON ( 'ORC' )
           SOCOM_PLUS_FIRST = SPL__UNDF
           CALL SOCOM_EXT()
!
! -------- ... and compute the number of parameters
!
           CALL PARCN()
!
! -------- Write info in PARFIL
!
           CALL USE_PARFIL ( 'OWC' )
      END IF
      STATUS_HFE = HFE__UNDF
!
! --- Get info from NAMFIL
!
      CALL DBPOX ( NUMDD, LDBNAM, IDBVER, IDBEND )
!
! --- Set status
!
      IF ( TRAIN ) CALL STATUS_SET ( 'PROC', STA__BEG )
!
      JB=1 + 2*M_GPA
      JA=1 + 3*M_GPA
      NELEM = (INT8(NPARAM)*INT8(NPARAM+1))/2
!
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__UND  .OR. &
     &     FAST_MODE .EQ. F__PRD                                     ) THEN
!
! -------- Zero out the normal equations matrix
!
           CALL NRMST ( ARR(JA), ARR(JB), NPARAM, NELEM )
      END IF
!
! --- Get flyby a prioris and modify PARFL common accordingly
!
      CALL FLYBY_APRIOR()
!
      RETURN
      END  !#!  PRELP  #!#
