      SUBROUTINE BOXCAR_FILTER ( NB, N, ARG, VAL1, VAL2 )
! ************************************************************************
! *                                                                      *
! *   Auxillry routine BOXCAR_FILTER
! *                                                                      *
! * ### 22-AUG-2002  BOXCAR_FILTER v1.0 (c)  L. Petrov  22-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  NB, N
      REAL*8     ARG(N), VAL1(N), VAL2(N)
      INTEGER*4  J1, J2, KP
!
      DO 410 J1=1,N
         KP = 0
         VAL2(J1) = 0.0D0
         DO 420 J2=J1-NB/2,J1+NB/2
            IF ( J2 .GE. 1   .AND.   J2 .LE. N ) THEN
                 KP = KP+1
                 VAL2(J1) = VAL2(J1) + VAL1(J2)
            END IF
 420    CONTINUE
        VAL2(J1) = VAL2(J1)/KP
 410  CONTINUE
      RETURN
      END  !#!  BOXCAR_FILTER  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAUSSIAN_FILTER ( SIG, N, ARG, VAL1, VAL2, VAL_LIM )
! ************************************************************************
! *                                                                      *
! *   Auxillry routine GAUSSIAN_FILTER
! *                                                                      *
! * ### 22-AUG-2002  GAUSSIAN_FILTER v1.1 (c) L. Petrov  31-JUL-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N
      REAL*8     SIG, ARG(N), VAL1(N), VAL2(N), WEI, EXPR, VAL_LIM
      REAL*8     VAL_LIM_DEF, VAL_LIM_USED
      PARAMETER  ( VAL_LIM_DEF = 1.0D307 )
      INTEGER*4  J1, J2, KP
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS
!
      IF ( PROBE_READ_ADDRESS(VAL_LIM) ) THEN
           VAL_LIM_USED = VAL_LIM
         ELSE
           VAL_LIM_USED = VAL_LIM_DEF
      END IF     
!
      DO 410 J1=1,N
         KP = 0
         VAL2(J1) = 0.0D0
         WEI = 0.0D0
         DO 420 J2=1,N
            EXPR = -(ARG(J2) - ARG(J1))**2/SIG**2
            IF ( EXPR > -20.0 .AND. VAL1(J2) < VAL_LIM_USED ) THEN
                 VAL2(J1) = VAL2(J1) + DEXP(EXPR)*VAL1(J2)
                 WEI = WEI + DEXP(EXPR)
            END IF
 420     CONTINUE
         VAL2(J1) = VAL2(J1)/WEI
 410  CONTINUE
      RETURN
      END  !#!  GAUSSIAN_FILTER #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAUSSIAN_FILTER_WEI ( M, HW, TIM, VAL_IN, ERR_IN, VAL_OUT )
! ************************************************************************
! *                                                                      *
! *   Auxillry routine GAUSSIAN_FILTER_WEI
! *                                                                      *
! * # 22-AUG-2002  GAUSSIAN_FILTER_WEI v1.0 (c) L. Petrov  22-AUG-2002 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M
      REAL*8     HW, TIM(M), VAL_IN(M), ERR_IN(M), VAL_OUT(M)
      REAL*8     WIN, WIN_SUM, EXP_VAR
      INTEGER*4  J1, J2
!
      DO 410 J1=1,M
         WIN_SUM = 0.0D0
         VAL_OUT(J1) = 0.0D0
         DO 420 J2=1,M
            EXP_VAR = -( (TIM(J2) - TIM(J1))/HW )**2
            IF ( EXP_VAR .LT. -40.0D0 ) EXP_VAR = -40.0D0
            WIN_SUM = WIN_SUM + DEXP(EXP_VAR)/ERR_IN(J2)
            VAL_OUT(J1) = VAL_OUT(J1) + VAL_IN(J2)*DEXP(EXP_VAR)/ERR_IN(J2)
 420     CONTINUE 
         VAL_OUT(J1) = VAL_OUT(J1)/WIN_SUM
 410  CONTINUE 
!      
      RETURN
      END  SUBROUTINE  GAUSSIAN_FILTER_WEI  !#!#
