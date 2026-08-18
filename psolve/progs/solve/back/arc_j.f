      SUBROUTINE ARC_J()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ARC_J PROGRAM SPECIFICATION
!
! 1.1 Review JARC
!
! 1.2 REFERENCES:
!
! 2.  ARC_J INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'baccm.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'prfil.i'
      INTEGER*2 IPARM1(10,M_GPA),IPARM2(10,M_GPA)
      INTEGER*2 IPARM3(10,M_GPA)
      character*20 lparm1(M_GPA),lparm2(M_GPA),lparm3(M_GPA)
      equivalence (lparm1,iparm1),(lparm2,iparm2),(lparm3,iparm3)
      LOGICAL*2 KBIT
      COMMON /PARAM/IPARM1,IPARM2,IPARM3
      real*8 aprior(M_GPA)
      COMMON /APRIORI/aprior
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: cvrnc
!       CALLED SUBROUTINES: indx_parm,reorder_x
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  IWDS, LDBNAM(5), IDBVER
      INTEGER*4  DBEND
      PARAMETER (IWDS=10)
!
      INTEGER*2  I, TRIMLEN, J, K, IT, NUMD
      INTEGER*4  J1
      CHARACTER  INTTODECIMAL*6, DUMCH*10
      LOGICAL*2  EQUAL
      LOGICAL*4  CHECK_STABIT
      INTEGER*4  I_LEN
!
! 4.  HISTORY
!   WHO   WHEN      WHAT
!   kdb     951207  Integer*4 number of observations.
!   kdb     960820  Fix arc_j range error by changing aprior declaration from
!                   max_sta to M_GPA.
!   jmg     960610  Remove holleriths.
!   jmg,kdb 960610  Fix range error.
!   pet     971202  Added logic for bypassing deselected station
!
! 5.  ARC_J PROGRAM STRUCTURE
!
!   get name of j-arc
!
      CALL USE_GLBFIL_4 ( 'ORC' )
      CALL FLYBY_APRIOR()
      NUMD = 1
      CALL DBPOX ( NUMD, LDBNAM, IDBVER, DBEND )
      CALL HOL2CHAR ( LDBNAM(1), INT2(2), INT2(10), DUMCH )
      J_ARCNAME=DUMCH(1:I_LEN(DUMCH))//'.'//INTTODECIMAL(IDBVER)
!
! --- Determine the indices of any parms specified in JCOV &
! --- NOTE:  only want to look at parms in this arc, so only review
! --- IARCS+IGLBLS parms in JPARM.
!
      CALL INDX_PARM ( JCOV, IND_JCOV, LPARM1, IARCS+IGLBLS, M_GPA, &
     &                 PERMUT, IARCS, IGLBLS, END_JDX )
      DO J1=1,END_JDX
         APRIOR(J1) = 0.D0
         IT=IND_JCOV(J1)
         DO J=1,NUMSTA
!
! --------- Check STABIT_P or STABIT_G bit fields to bypass deselcted station
!
            IF ( .NOT. CHECK_STABIT ( J ) ) GOTO 810
!
            IF ( ISITN_CHR(J) .EQ. LPARM1(IT)(1:8) ) THEN
                 K=0
                 IF ( LPARM1(IT)(9:10) .eq. " X" ) K=1
                 IF ( LPARM1(IT)(9:10) .eq. " Y" ) K=2
                 IF ( LPARM1(IT)(9:10) .eq. " Z" ) K=3
                 IF ( K .NE. 0) APRIOR(J1) = VSITEC(K,J)
            ENDIF
 810        CONTINUE
         ENDDO
      ENDDO
!
! --- Create cross reference list for later matrix multiplication, and
! --- reorder global parameters in the index array IND_JCOV to match
! --- CGM order
!
      CALL REORDER_X ( LPARM1, IARCS, LPARM2, IX2T3 )
      RETURN
      END  !#!  ARC_J  #!#
