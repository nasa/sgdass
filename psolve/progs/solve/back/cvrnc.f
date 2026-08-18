      SUBROUTINE CVRNC(M1,M2)
      IMPLICIT NONE
!
! 1.  CVRNC PROGRAM SPECIFICATION
!
! 1.1 Control production of covariance elements within arcs and
!     between arcs.  Uses info in I_ARCNAME to decide which sub
!     to run.  Assumes existence of certain matrices and parm
!     lists.  The j_arc matrix is in A.
!
! 1.2 REFERENCES:
!
! 2.  CVRNC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 M1(*),M2(*)
!
! M1 - ARC matrix
! M2 - CGM matrix
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'baccm.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INTEGER*2 IPARM1(10,M_GPA),IPARM2(10,M_GPA)
      INTEGER*2 IPARM3(10,M_GPA)
      COMMON /PARAM/IPARM1,IPARM2,IPARM3
      character*20 lparm1(M_GPA),lparm2(M_GPA),lparm3(M_GPA)
      equivalence (lparm1,iparm1),(lparm2,iparm2),(lparm3,iparm3)
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: back
!       CALLED SUBROUTINES: arc_i,arc_j, outfl_back, covmm
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 JS,JB,JA
      INTEGER*2 I
      LOGICAL*2 RSTRT
      INTEGER*4 I4P1
      DATA I4P1 /1/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   MWH  900418  Added call to indx_parm to allow STA or SOU covariance option
!   jmg  960610 Remove holleriths.
!
!
! 5.  CVRNC PROGRAM STRUCTURE
!
      JS = 1
      JB = 1 + 2*M_GPA
      JA = 1 + 3*M_GPA
      RSTRT=.FALSE.
      IF((IARCNM.EQ.1).AND.(I_ARC.LE.1)) RSTRT=.TRUE.
      CALL OUTFL_BACK ( 'O', RSTRT )
!
      IF(I_ARCNAME(1:3).EQ.'ALL') THEN
       CALL ARC_J()
       DO I=IARCNM,TARCS
        I_ARC=I
        IF(I_ARC.NE.IARCNM) CALL ARC_I(M2)
        CALL COVMM(M1(JA),M2(JA),M1(JB),M2(JB),M1(JS),M2(JS))
       ENDDO
      ELSE IF(I_ARCNAME(1:1).EQ.'$') THEN  !I ARC is a database
       CALL ARC_J()
       DO I=1,TARCS
        I_ARC=I
        IF(I_ARC.NE.IARCNM) CALL ARC_I(M2)
        CALL COVMM(M1(JA),M2(JA),M1(JB),M2(JB),M1(JS),M2(JS))
       ENDDO
      ELSE IF(I_ARCNAME(1:6).EQ.'BY_ARC') THEN
       I_ARC=IARCNM
       CALL ARC_J()
       CALL COVMM(M1(JA),M2(JA),M1(JB),M2(JB),M1(JS),M2(JS))
      ELSE IF(I_ARCNAME(1:3).EQ.'CGM') THEN
       I_ARC=0
       call indx_parm(icov,ind_icov,lparm2,nparm2,M_GPA, &
     &     permut,i_arc_arcs,i_arc_glbs,end_idx)
       CALL COVMM(M1(JA),M2(JA),M1(JB),M2(JB),M1(JS),M2(JS))
      ENDIF
      CALL OUTFL_BACK ( 'C', RSTRT )
      RETURN
      END  !#!  CVRNC  #!#
