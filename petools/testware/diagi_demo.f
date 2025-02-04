      PROGRAM    DIAGI_DEMO
! ************************************************************************
! *                                                                      *
! *   Demonstarion program draws plots of three functions using DiaGI    *
! *   interface.                                                         *
! *                                                                      *
! *  ###  22-OCT-97   DIAGI_DEMO  v1.0  (c)  L. Petrov  22-OCT-1997 ###  *
! *                                                                      *
! ************************************************************************
      PARAMETER  (M=32, M2=48, M3=40 )
      REAL*8     T1(M),  X1(M),  E1(M), XMIN, XMAX, YMIN, YMAX
      REAL*8     T2(M2), X2(M2), E2(M2)
      REAL*8     T3(M3), X3(M3), E3(M3)
      CHARACTER  ZAG*80
      CALL CLRCH ( ZAG )
      ZAG = 'Example of using DiaGI (Dialogue Graphic Interface)'
!
      DO 410 J1=1,M
         T1(J1) = 2.D0 + 0.1*(J1-1)
         X1(J1) = SIN(T1(J1)) + 0.5*SIN(12.*T1(J1))
         E1(J1) = 0.1
 410  CONTINUE
!
      DO 420 J2=1,M2
         T2(J2) = 0.D0 + 0.15*(J2-1)
         X2(J2) = COS(T2(J2)) + 0.2*SIN(30.*T2(J2)) - 1.0
         E2(J2) = 0.02*J2
 420  CONTINUE
!
      DO 430 J3=1,M3
         T3(J3) = 0.D0 + 0.2*(J3-1)
         X3(J3) = SIN(T3(J3)) + 0.3*SIN(3.*T3(J3)) - 2.5
         E3(J3) = 0.02*X3(J3)
 430  CONTINUE
!
      XMIN = -0.5
      XMAX =  4.5
      YMIN = -3.0
      YMAX =  2.0
      IBST = 0
      ILST = 3
      IPST = 2
      IWST = 2
      ICLR = 1
      IUER = -1
!
!!      CALL DIAGI_1  ( M, T1, X1, IUER )
!!      CALL DIAGI_1E ( M, T1, X1, E1, IUER )
!!      CALL DIAGI_2  ( M, T1, X1, M2, T2, X2, IUER )
!!      CALL DIAGI_2E ( M, T1, X1, E1, M2, T2, X2, E2, IUER )
!!      CALL DIAGI_3  ( M, T1, X1, M2, T2, X2, M3, T3, X3, IUER )
      CALL DIAGI_3E ( M, T1, X1, E1, M2, T2, X2, E2, M3, T3, X3, E3, IUER )
      WRITE ( 6, * ) ' IUER = ' ,IUER
      END  !#!  DIAGI_MAIN
