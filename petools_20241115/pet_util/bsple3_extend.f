      SUBROUTINE BSPLE3_EXTEND ( N, ARG )
! ************************************************************************
! *                                                                      *
! *   Routine BSPLE3_EXTEND fills values of extended B-spline sequence.  *
! *   Array ARG is sized [-2,N+3], but it is assumed that at entry       *
! *   elements [1,N] are filled. Elements [-2,0] and [N+1,N+3] are in    *
! *   fact placeholders for highly optimized B-spline routines.          *
! *   The step of placeholders at teh beginning of the sequence is       *
! *   eps*(arg[2] - arg[1]), where eps=1.0e-5. Analogously, the step     *
! *   at the end of the sequence is eps*(arg[n] - arg[n-1]).             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   N ( INTEGER*4 ) -- Dimension of the sequence.                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * ARG ( REAL*8    ) -- Sequence of knots. Dimension: [-2,N+3].         *
! *                      Input: knots [1:N] are filled. Output:          *
! *                      knots [-2,0] and [N+1,N+3] are filled.          *
! *                                                                      *
! * ### 20-SEP-2014  BSPLE3_EXTEND  v2.0 (c) L. Petrov  16-AUG-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      INTEGER*4  DEG
      REAL*8     EPS
      PARAMETER  ( DEG = 3 )
      PARAMETER  ( EPS = 1.0D-12 )
      REAL*8     ARG(1-DEG:N+DEG), STEP
      INTEGER*4  J1, J2
!
      STEP = MAX ( ABS(ARG(1)), ABS(ARG(2)) )
      DO 410 J1=0,-2,-1
         ARG(J1) = ARG(1) + (J1-1)*EPS*STEP
 410  CONTINUE 
!
      STEP = MAX ( ABS(ARG(N-1)), ABS(ARG(N)) )
      DO 420 J2=N+1,N+3
         ARG(J2) = ARG(N) + (J2-N)*EPS*STEP
 420  CONTINUE 
      RETURN
      END  SUBROUTINE BSPLE3_EXTEND  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE BSPLE3_EXTEND_R4 ( N, ARG )
! ************************************************************************
! *                                                                      *
! *   Routine BSPLE3_EXTEND_R4 fills values of extended B-spline         *
! *   sequence. Array ARG is sized [-2,N+3], but it is assumed that at   *
! *   entry, elements [1,N] are filled. Elements [-2,0] and [N+1,N+3]    *
! *   are in fact placeholders for highly optimized B-spline routines.   *
! *   The step of placeholders at teh beginning of the sequence is       *
! *   eps*(arg[2] - arg[1]), where eps=1.0e-5. Analogously, the step     *
! *   at the end of the sequence is eps*(arg[n] - arg[n-1]).             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   N ( INTEGER*4 ) -- Dimension of the sequence.                      *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * ARG ( REAL*8    ) -- Sequence of knots. Dimension: [-2,N+3].         *
! *                      Input: knots [1:N] are filled. Output:          *
! *                      knots [-2,0] and [N+1,N+3] are filled.          *
! *                                                                      *
! * ### 20-SEP-2014 BSPLE3_EXTEND_R4 v2.0 (c) L. Petrov  16-AUG-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      INTEGER*4  DEG
      REAL*4     EPS
      PARAMETER  ( DEG = 3 )
      PARAMETER  ( EPS = 1.0E-6 )
      REAL*4     ARG(1-DEG:N+DEG), STEP
      INTEGER*4  J1, J2
!
      STEP = MAX ( ABS(ARG(1)), ABS(ARG(2)) )
      DO 410 J1=0,-2,-1
         ARG(J1) = ARG(1) + (J1-1)*EPS*STEP
 410  CONTINUE 
!
      STEP = MAX ( ABS(ARG(N-1)), ABS(ARG(N)) )
      DO 420 J2=N+1,N+3
         ARG(J2) = ARG(N) + (J2-N)*EPS*STEP
 420  CONTINUE 
      RETURN
      END  SUBROUTINE BSPLE3_EXTEND_R4  !#!  
