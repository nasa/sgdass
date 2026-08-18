      SUBROUTINE ELIMIN(SCALE,SIG,B,A,IARCS,IGLBLS)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ELIMIN PROGRAM SPECIFICATION
!
! 1.1 Invert the arc parameters.
!
! 1.2 REFERENCES:
!
! 2.  ELIMIN INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 IARCS,IGLBLS
      REAL*8 SIG(*),B(*),A(*)
      INTEGER*4  IARR(2), IP
!
! A - Matrix
! B - Vector
! IARCS - Number of arc parameters
! IGLBLS - Number of global parameters
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SCALE(*)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcpe
!       CALLED SUBROUTINES: global
!
! 3.  LOCAL VARIABLES
!
      REAL*8    Z(M_GPA)
      INTEGER*8, EXTERNAL :: INDX8
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  ELIMIN PROGRAM STRUCTURE
!
      CALL SCALER ( A, B, SCALE, IARCS )
!
      CALL DPPCO ( A, IARCS, RCOND, Z )
      IF ( RCOND < 0.0D0 ) THEN
           CALL ERR_LOG ( 521, -2, 'ELIMIN', 'Error in attempt to perform '// &
     &         'Cholesky decomposition' )
           IP = 3
           IARR(IP) = IP
           RETURN 
      END IF
      CALL USE_GLBFIL_4 ( 'OWC' )
      CALL DPPSL ( A, B, IARCS )
      CALL DPPIN ( A, IARCS )
!
      CALL UNSCALER ( A, B, SCALE, IARCS )
!
      CALL GLOBAL ( A, B, IGLBLS, IARCS, IGLBLS+IARCS, &
     &              A(INDX8( IARCS+IGLBLS, IARCS+IGLBLS)+1), SIG )
!
      RETURN
      END  !#!  ELIMIN  #!#
