      SUBROUTINE TREND_ITE ( NP, T, X, ITE_REC, SHIFT, DRIFT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  TREND_ITE  Determines parameters of the linear trend:     *
! *   shift and drift iteratively by LSQ. It is assumed that TREND_ITE   *
! *   is called consequently with incrementing parameter NP.             *
! *                                                                      *
! *   Linear trend is parametrized as X(t) = SHIFT + DRIFT*(t-t(1))      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      NP ( INTEGER*4 ) -- Index of the point to be added for          *
! *                          computation of linear trend. It is assumed  *
! *                          that TREND_ITE was called before with NP-1  *
! *                          unless NP=1                                 *
! *       T ( REAL*8    ) -- Argument of the point to be bee added for   *
! *                          computation of linear trend.                *
! *       X ( REAL*8    ) -- Value of the point to be bee added for      *
! *                          computation of linear trend.                *
! * ITE_REC ( RECORD    ) -- Data struture whcih keeps results of the    *
! *                          previous computations.                      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  SHIFT  ( REAL*8    ) -- Shift of the linear trend -- value of linear*
! *                          trend in the point T=t(1)                   *
! *  DRIFT  ( REAL*8    ) -- Drift of the linear trend -- inclination of *
! *                          a linear line.                              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error handler.                *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  23-AUG-99   TREND_ITE    v1.0  (c)  L. Petrov  23-AUG-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'trend_ite.i'
      INTEGER*4  NP, IUER
      REAL*8     T, X, SHIFT, DRIFT
      TYPE ( ITE_STRU ) ::  ITE_REC
      REAL*8     EPS, DET, TT
      PARAMETER  ( EPS = 1.D-12 )
!
      IF ( NP .EQ. 1 ) THEN
!
! -------- This point is the first -- initialize accumulators
!
           ITE_REC%T0  = T
           ITE_REC%ST  = 0.0
           ITE_REC%STT = 0.0
           ITE_REC%STX = 0.0
           ITE_REC%SX  = X
           ITE_REC%NP_LAST = 1
!
           SHIFT = 0.0
           DRIFT = 0.0D0
         ELSE
!
! -------- Check: wasn't a sequence violated?
!
           IF ( ITE_REC%NP_LAST + 1 .NE. NP ) THEN
                WRITE ( 6, * ) ' NP =',NP,' NP(last call)=',ITE_REC%NP_LAST
                CALL ERR_LOG ( 601, IUER, 'TREND_ITE', 'Wrong value of NP: '// &
     &              'the value NP used in the previous call of TREND_ITE '// &
     &              'plus 1 was expected' )
                RETURN
           END IF
!
! -------- Update accumulators
!
           ITE_REC%NP_LAST = NP
           TT = T - ITE_REC%T0
           ITE_REC%ST  = ITE_REC%ST  + TT
           ITE_REC%STT = ITE_REC%STT + TT**2
           ITE_REC%STX = ITE_REC%STX + X*TT
           ITE_REC%SX  = ITE_REC%SX  + X
!
! -------- Compute determinant
!
           DET=NP*ITE_REC%STT-ITE_REC%ST**2
           IF ( DET .LT. EPS ) THEN
                WRITE ( 6, * ) 'DET =',DET
                CALL ERR_LOG ( 602, IUER, 'TREND_ITE', 'Singularity was '// &
     &              'detected: determinant is too small' )
                RETURN
           END IF
!
! -------- Compute shift and drift
!
           SHIFT = (ITE_REC%STT*ITE_REC%SX - ITE_REC%ST*ITE_REC%STX)/DET
           DRIFT = (NP*ITE_REC%STX - ITE_REC%ST*ITE_REC%SX )/DET
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  TREND_ITE  #!#
