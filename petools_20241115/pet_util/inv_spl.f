      SUBROUTINE INV_SPL ( M, VAL, ARG_ARR, VAL_ARR, SPL_ARR, EPS, &
     &                     MI, NI, ARG_INT, IND ) 
! ************************************************************************
! *                                                                      *
! *   Routine INV_SPL 
! *                                                                      *
! *  ### 02-AUG-2017   INV_SPL  v1.0 (c)  L. Petrov  02-AUG-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M, MI, NI, IND(M)
      REAL*8     VAL, ARG_ARR(M), VAL_ARR(M), SPL_ARR(M), ARG_INT(MI), EPS
      REAL*8     DH, D, C, B, H
      INTEGER*4  J1, J2, SGN(M)
      REAL*8     D_ARG, VAL_TRY, FSPL8
!
      NI = 0
      ARG_INT = 0.0D0
!
      DO 410 J1=2,M
         IF ( VAL > VAL_ARR(J1-1) .AND. VAL < VAL_ARR(J1) ) THEN
              NI = NI + 1
              IND(NI) = J1-1
              SGN(NI) = -1
         END IF
         IF ( VAL < VAL_ARR(J1-1) .AND. VAL > VAL_ARR(J1) ) THEN
              NI = NI + 1
              IND(NI) = J1-1
              SGN(NI) = 1
         END IF 
 410  CONTINUE 
      DO 420 J2=1,NI
         DH =    ARG_ARR(IND(J2)+1) - ARG_ARR(IND(J2))
         D  =  ( SPL_ARR(IND(J2)+1) - SPL_ARR(IND(J2)) ) / (3.D0*DH)
         C  =    SPL_ARR(IND(J2))
         B  =  ( VAL_ARR(IND(J2)+1) - VAL_ARR(IND(J2)) ) / DH  - D*DH**2 - C*DH
         H  =    DH/2
!
         D_ARG = -B/(2.0D0*C) - SGN(J2)*DSQRT ( B**2 + 4.D0*C*(VAL - VAL_ARR(IND(J2))))/(2.0D0*C)
         H  = D_ARG
         VAL_TRY = VAL_ARR(IND(J2)) + B*H + C*H**2 + D*H**3
         D_ARG = -B/(2.0D0*C) - SGN(J2)* DSQRT ( B**2 + 4.D0*C*(VAL + D*D_ARG**3 - VAL_ARR(IND(J2))))/(2.0D0*C)
         ARG_INT(J2) = ARG_ARR(IND(J2)) + D_ARG
 420  CONTINUE 
!
      RETURN
      END  SUBROUTINE  INV_SPL  !#!  
