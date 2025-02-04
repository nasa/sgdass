      PROGRAM    B3D_STAND
! ************************************************************************
! *                                                                      *
! *     Subroutine B3D_STAND  --  is the stand program for verifying     *
! *     the B3D algorithm for solving conditional equations by LSQ.      *
! *     The normal matrix is square, symmetric, bordered,                *
! *     block-tridiagonal.                                               *
! *                                                                      *
! *  ###  01-NOV-95    B3D_STAND    v3.0  (c)  Petrov L. 30-NOV-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  GA, LA, LXA
      INTEGER*4  N, G, L, LX, M, L_EQ, LX_EQ, MEQ, MA, IS, IUER
      INTEGER*8  MEM_LEN
      ADDRESS__TYPE :: MEM_ADR, AF, BF, EF, DSP_F, COV_F
      INTEGER*4  B0, B, BX, C, CX, D, DX, Z0, ZL, ZLX, E0, EL, ELX
      INTEGER*4  WM_GG, WM_LG1, WM_LG2, WM_LLS, WM_LLF, WV_L, WV_G
      REAL*8     RK, RC
!
      IS=2392187
      RK=3.0
!
      N  = 4
      G  = 6
      L  = 5
      LX = 3
!
      M  = G + (N-1)*L + LX  !  total number of parameters
      L_EQ  = L*RK     !  number of equations in one local group
      LX_EQ = LX*RK
      MEQ   = (N-2)*L_EQ + LX_EQ !  total number of equations
      MA  = (M*(M+1))/2
      GA  = (G*(G+1))/2
      LA  = (L*(L+1))/2
      LXA = (LX*(LX+1))/2
!
      CALL GRAB_MEM ( -1, MEM_LEN, MEM_ADR,        25, &
     &                             8*INT8(M*MEQ),        AF, &
     &                             8*INT8(MEQ),          BF, &
     &                             8*INT8(M),            EF, &
     &                             8*INT8(M),            DSP_F, &
     &                             8*INT8(MA),           COV_F, &
     &                             8*GA,                 B0, &
     &                             8*INT8((N-1)*L*G),    B, &
     &                             8*INT8(LX*G),         BX, &
     &                             8*INT8((N-1)*LA),     C, &
     &                             8*LXA,                CX, &
     &                             8*INT8(N*L*L),        D, &
     &                             8*INT8(L*LX),         DX, &
     &                             8*INT8(G),            Z0, &
     &                             8*INT8((N-1)*L),      ZL, &
     &                             8*INT8(LX),           ZLX, &
     &                             8*INT8(G),            E0, &
     &                             8*INT8((N-1)*L),      EL, &
     &                             8*INT8(LX),           ELX, &
     &                             8*GA,                 WM_GG, &
     &                             8*INT8(L*G),          WM_LG1, &
     &                             8*INT8(L*G),          WM_LG2, &
     &                             8*LA,                 WM_LLS, &
     &                             8*INT8(L*L),          WM_LLF, &
     &                             8*INT8(L),            WV_L, &
     &                             8*INT8(G),            WV_G )
!
      write ( 6, * ) ' m=',m,' meq=',meq
      CALL MAKE_B3D ( N, G, L, LX, M, GA, LA, LXA, L_EQ, LX_EQ, MEQ, &
     &                %VAL(AF), %VAL(BF), %VAL(EF), IS, &
     &                %VAL(B0), %VAL(B),  %VAL(BX), %VAL(C),  %VAL(CX), &
     &                %VAL(D),  %VAL(DX), %VAL(Z0), %VAL(ZL), %VAL(ZLX) )
      IUER = -1
!!        call matview_1 ( m, meq, %val(af), -3 ) ! %%
      CALL LSQ_SOL  ( M, MEQ, %VAL(AF), %VAL(BF), %VAL(EF), %VAL(DSP_F), &
     &                %VAL(COV_F), RC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      IUER = -1
      CALL B3D_SOL_X ( N, G, L, LA, LX, LXA, %VAL(B0), %VAL(Z0), %VAL(E0), &
     &                 %VAL(B), %VAL(C), %VAL(D), %VAL(ZL), %VAL(EL), &
     &                 %VAL(BX), %VAL(CX), %VAL(DX), %VAL(ZLX), &
     &                 %VAL(ELX), %VAL(WM_GG), %VAL(WM_LG1), %VAL(WM_LG2), &
     &                 %VAL(WM_LLS), %VAL(WM_LLF), %VAL(WV_L), %VAL(WV_G), &
     &                 RC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      CALL LSQ_SHOW ( M, %VAL(EF), N, G, L, LX, %VAL(E0), %VAL(EL), %VAL(ELX) )
      END  !#!  B3D_STAND   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAKE_B3D ( N, G, L, LX, M, GA, LA, LXA, L_EQ, LX_EQ, M_EQ, &
     &                      AF, BF, EF, IS, &
     &                      B0, B, BX, C, CX, D, DX, Z0, ZL, ZLX )
      IMPLICIT   NONE
      INTEGER*8  GA, LA, LXA
      INTEGER*4  N, G, L, LX, M, L_EQ, LX_EQ, M_EQ, IS
      REAL*8     AF(M,M_EQ), BF(M_EQ), EF(M)
      REAL*8     B0(*), Z0(G), &
     &           B(L,G,N), C(LA,N), D(L,L,N), ZL(L,N), &
     &           BX(LX,G), CX(LXA), DX(LX,L), ZLX(LX)
      REAL*8     RGAUSS
      INTEGER*4  IND(64*1024)
      INTEGER*4  IEQ, LC, LN, LC_EQ, IP, IG, IL1, IL2, J1, J2, J3, J4, J5, J6
!
      CALL NOUT ( 8*M_EQ*M, AF )
!      GA  = (G*(G+1))/2
!      LA  = (L*(L+1))/2
!      LXA = (LX*(LX+1))/2
!
      DO 410 J1=1,M
         EF(J1) = J1
         IND(J1) = J1
 410  CONTINUE
!
      CALL NOUT ( 8*GA, B0 )
      CALL NOUT ( 8*G,  Z0 )
!
      DO 510 J1=1,N-1
         CALL NOUT ( 8*G*L, B(1,1,J1) )
         CALL NOUT ( 8*LA,  C(1,J1) )
         CALL NOUT ( 8*L*L, D(1,1,J1) )
         CALL NOUT ( 8*L,   ZL(1,J1) )
 510  CONTINUE
!
      CALL NOUT ( 8*G*LX, BX  )
      CALL NOUT ( 8*LXA,  CX  )
      CALL NOUT ( 8*L*LX, DX  )
      CALL NOUT ( 8*LX,   ZLX )
!
      IEQ = 0
      DO 420 J2=1,N-1
         IF ( J2 .EQ. N-1 ) THEN
              LC_EQ = LX_EQ
              LC    = L
              LN    = LX
            ELSE
              LC_EQ = L_EQ
              LC    = L
              LN    = L
         END IF
!
         DO 430 J3=1,LC_EQ
            IEQ = IEQ + 1
            BF(IEQ) = RGAUSS ( IS, 0.001D0 )
            IG = 1
            DO 440 J4=1,G
               AF(J4,IEQ) = RGAUSS ( IS, 1.0D0 )
               BF(IEQ) = BF(IEQ) + AF(J4,IEQ)*EF(J4)
 440        CONTINUE
!
            IL1 = G + (J2-1)*L + 1
            DO 450 J5=1,LC
               IP = IL1 + J5-1
               AF(IP,IEQ) = RGAUSS ( IS, 1.0D0 )
               BF(IEQ) = BF(IEQ) + AF(IP,IEQ)*EF(IP)
 450        CONTINUE
!
            IL2 = G + J2*L + 1
            DO 460 J6=1,LN
               IP = IL2 + J6-1
               AF(IP,IEQ) = RGAUSS ( IS, 1.0D0 )
               BF(IEQ) = BF(IEQ) + AF(IP,IEQ)*EF(IP)
 460        CONTINUE
!
            CALL ADD_TRG ( BF(IEQ), 1.0D0, G, IND, AF(IG,IEQ), &
     &                     G, Z0, B0 )
            CALL ADD_TRG ( BF(IEQ), 1.0D0, LC, IND, AF(IL1,IEQ), &
     &                     LC, ZL(1,J2), C(1,J2) )
            CALL ADD_RCT ( BF(IEQ), 1.0D0, LC, IND, AF(IL1,IEQ), &
     &                     G, IND, AF(IG,IEQ), LC, G, B(1,1,J2) )
            IF ( J2 .EQ. N-1 ) THEN
                 CALL ADD_TRG ( BF(IEQ), 1.0D0, LN, IND, AF(IL2,IEQ), &
     &                          LN, ZLX, CX )
                 CALL ADD_RCT ( BF(IEQ), 1.0D0, LN, IND, AF(IL2,IEQ), &
     &                          G, IND, AF(IG,IEQ), LN, G, BX )
                 CALL ADD_RCT ( BF(IEQ), 1.0D0, LN, IND, AF(IL2,IEQ), &
     &                          LC, IND, AF(IL1,IEQ), LN, LC, DX )
               ELSE
                 CALL ADD_TRG ( BF(IEQ), 1.0D0, LN, IND, AF(IL2,IEQ), &
     &                          LN, ZL(1,J2+1), C(1,J2+1) )
                 CALL ADD_RCT ( BF(IEQ), 1.0D0, LN, IND, AF(IL2,IEQ), &
     &                          G, IND, AF(IG,IEQ), LN, G, B(1,1,J2+1) )
                 CALL ADD_RCT ( BF(IEQ), 1.0D0, LN, IND, AF(IL2,IEQ), &
     &                          LN, IND, AF(IL1,IEQ), LN, LC, D(1,1,J2+1) )
            END IF
 430     CONTINUE
 420  CONTINUE
!        call matview_2 ( g, b0, -3 ) ! %%
!        call matview_2 ( l, c(1,1), -3 ) ! %%
!        call matview_2 ( l, c(1,2), -3 ) ! %%
!        call matview_1 ( l, l, d(1,1,1), -3 ) ! %%
!        call matview_1 ( l, l, d(1,1,2), -3 ) ! %%
!
      RETURN
      END  !#!  MAKE_B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LSQ_SOL ( N, M, A, B, X, DISP, COV, RC, IUER )
      IMPLICIT   NONE
      INTEGER*4  M, N, IUER
      REAL*8     A(N,M), B(M), X(N), DISP(N), COV(*), RC
!
      REAL*8     SUSQ, DP_VV_V
      INTEGER*4  IER
!
      IER=-1
      CALL MUL_MM_IT_S ( N, M, A, N, M, A, N, COV, IER )
!!        call matview_2 ( n, cov, -3 ) ! %%
!
      IER=-1
      CALL MUL_MV_IV_V ( N, M, A, M, B, N, DISP, IER )
!
      SUSQ = DP_VV_V ( M, B, B )
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( N, COV, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 12, IUER, 'LSQ_SOL', 'Error during inversion '// &
     &         'of the normal matrix' )
           RETURN
      END IF
      IER=-1
      CALL MUL_MV_SV_V ( N, COV, N, DISP, N, X, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  LSQ_SOL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LSQ_SHOW ( M, EF, N, G, L, LX, E0, EL, ELX )
      IMPLICIT   NONE
      INTEGER*4  M, N, G, L, LX
      REAL*8     EF(M), E0(G), EL(L,N), ELX(LX)
      INTEGER*4  IEQ, J1, J2, J3, J4
!
      IEQ = 0
      DO 410 J1=1,G
         IEQ = IEQ + 1
         WRITE ( 6, 110 ) J1, EF(IEQ), E0(J1)
 110     FORMAT ( 'I=',I3,' Ef=',1PD15.7,' Eg=',1PD15.7 )
 410  CONTINUE
!
      DO 420 J2=1,N-1
         DO 430 J3=1,L
            IEQ = IEQ + 1
            WRITE ( 6, 120 ) IEQ, EF(IEQ), J2, J3, EL(J3,J2)
 120        FORMAT ( 'I=',I3,' Ef=',1PD15.7,' block: ',I3,' par: ',I3, &
     &               ' El= ',1PD15.7 )
 430     CONTINUE
 420  CONTINUE
!
      DO 440 J4=1,LX
         IEQ = IEQ + 1
         WRITE ( 6, 130 ) IEQ, EF(IEQ), J4, ELX(J4)
 130        FORMAT ( 'I=',I3,' Ef=',1PD15.7,' last block. par: ',I3, &
     &               ' Elx= ',1PD15.7 )
 440  CONTINUE
!
      RETURN
      END  !#!  LSQ_SHOW  #!#
