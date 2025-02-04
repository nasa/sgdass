      PROGRAM    DIAGI_BATCH
! ************************************************************************
! *                                                                      *
! *   This is a demo progrm for batch DiaGI.                             *
! *                                                                      *
! *  ### 06-AUG-2002  DIAGI_BATCH  v1.0 (c)  L. Petrov  06-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'diagi.i'
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  M, M2, M3
      PARAMETER  (M=32, M2=48, M3=40 )
      INTEGER*4  J1, J2, J3, IUER
      REAL*8     T1(M),  X1(M),  E1(M)
      REAL*8     T2(M2), X2(M2), E2(M2)
      REAL*8     T3(M3), X3(M3), E3(M3)
      INTEGER*4  DIAGI_LEN
      CHARACTER  ZAG*80
      INTEGER*4  I_LEN
!
      CALL CLRCH ( ZAG )
      ZAG = 'Example of using DiaGI (Dialogue Graphic Interface)'
!
! --- Clear DIAGI_S object
!
      DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      CALL NOUT ( DIAGI_LEN, DIAGI_S )
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
      DIAGI_S%IDEV      = 7
      DIAGI_S%NPOI(1)   = M
      DIAGI_S%ADR_X8(1) = LOC(T1)
      DIAGI_S%ADR_Y8(1) = LOC(X1)
      DIAGI_S%ADR_E8(1) = LOC(E1)
      DIAGI_S%LER(1)    = .TRUE.
!
      DIAGI_S%ICOL(1)   = 1
      DIAGI_S%IBST(1)   = 4
      DIAGI_S%ILST(1)   = 1
      DIAGI_S%IOST(1)   = 1
      DIAGI_S%IPST(1)   = 3
      DIAGI_S%IWST(1)   = 1
      DIAGI_S%NPOI(1)   = M
!C
      DIAGI_S%NPOI(2)   = M2
      DIAGI_S%ADR_X8(2) = LOC(T2)
      DIAGI_S%ADR_Y8(2) = LOC(X2)
      DIAGI_S%ADR_E8(2) = LOC(E2)
      DIAGI_S%LER(2)    = .TRUE.
!
      DIAGI_S%ICOL(2)   = 2
      DIAGI_S%IBST(2)   = 0
      DIAGI_S%ILST(2)   = 2
      DIAGI_S%IOST(2)   = 1
      DIAGI_S%IPST(2)   = 4
      DIAGI_S%IWST(2)   = 1
      DIAGI_S%NPOI(2)   = M
!C
      DIAGI_S%NPOI(3)   = M
      DIAGI_S%ADR_X8(3) = LOC(T3)
      DIAGI_S%ADR_Y8(3) = LOC(X3)
      DIAGI_S%ADR_E8(3) = LOC(E3)
      DIAGI_S%LER(3)    = .TRUE.
!
      DIAGI_S%ICOL(3)   = 3
      DIAGI_S%IBST(3)   = 1
      DIAGI_S%ILST(3)   = 3
      DIAGI_S%IOST(3)   = 1
      DIAGI_S%IPST(3)   = 5
      DIAGI_S%IWST(3)   = 1
!
      DIAGI_S%NCLR      = 3
      DIAGI_S%ICLR      = 1
      DIAGI_S%ZAG       = ZAG
      DIAGI_S%ARG_UNITS = 'Arg'
      DIAGI_S%NAME      = '/tmp/batch_diagi.gif'
      DIAGI_S%ITRM      = 0
      DIAGI_S%IBATCH    = 1
      DIAGI_S%XMIN      = -0.1
      DIAGI_S%XMAX      =  6.3
      DIAGI_S%YMIN      = -3.0
      DIAGI_S%YMAX      =  0.0
      DIAGI_S%STATUS   = DIA__DEF
!
      IUER  = -1
      CALL DIAGI ( DIAGI_S, IUER )
      WRITE ( 6, * ) ' IUER = ' ,IUER
      WRITE ( 6, * ) 'File created: '//DIAGI_S%NAME(1:I_LEN(DIAGI_S%NAME))
      END  !#!  DIAGI_BATCH  #!#
