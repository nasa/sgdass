      SUBROUTINE REPA_MINMAX ( N_POI, ARG, VAL, ERR, ARG_MIN, ARG_MAX, &
     &                         VAL_MIN, VAL_MAX )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine REPA_MINMAX finds the mininal and maximal       *
! *   element among ARG, VAL-ERR, and VAL+ERR.                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   N_POI ( INTEGER*4 ) -- The number of points.                       *
! *     ARG ( REAL*8    ) -- Array of arguments. Dimenstion: N_POI.      *
! *     VAL ( REAL*8    ) -- Array of value.  Dimenstion: N_POI.         *
! *     ERR ( REAL*8    ) -- Array of errors. Dimenstion: N_POI.         *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * ARG_MIN ( REAL*8    ) -- Minimal value of the argument.              *
! * ARG_MAX ( REAL*8    ) -- Maximal value of the arument.               *
! * VAL_MIN ( REAL*8    ) -- Mininal value of VAL(k)-ERR(k).             *
! * VAL_MAX ( REAL*8    ) -- Maximal value of VAL(k)+ERR(k).             *
! *                                                                      *
! * ### 03-DEC-2004   REPA_MINMAX   v1.0 (c)  L. Petrov  03-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N_POI
      REAL*8     ARG(N_POI), VAL(N_POI), ERR(N_POI), ARG_MIN, ARG_MAX, &
     &           VAL_MIN, VAL_MAX
      INTEGER*4  J1
!
      IF ( N_POI .LT. 1 ) RETURN 
      ARG_MIN = ARG(1)
      ARG_MAX = ARG(1)
      VAL_MIN = VAL(1) - DABS(ERR(1))
      VAL_MAX = VAL(1) + DABS(ERR(1))
!
      DO 410 J1=1,N_POI
         IF ( ARG(J1) .LT. ARG_MIN ) THEN
              ARG_MIN = ARG(J1)
         END IF
         IF ( ARG(J1) .GT. ARG_MAX ) THEN
              ARG_MAX = ARG(J1)
         END IF
!
         IF ( VAL(J1) - DABS(ERR(J1)) .LT. VAL_MIN ) THEN
              VAL_MIN = VAL(J1) - DABS(ERR(J1)) 
         END IF
         IF ( VAL(J1) + DABS(ERR(J1)) .GT. VAL_MAX ) THEN
              VAL_MAX = VAL(J1) + DABS(ERR(J1)) 
         END IF
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  REPA_MINMAX

