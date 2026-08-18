      PROGRAM    E152
      IMPLICIT   NONE
      INTEGER*8  GA, LA, SA, SXA, MEM_LEN
      INTEGER*4  NL, NS, G, L, S, SX, M, S_EQ, SX_EQ, &
     &           MEQ, MA, IS, IUER
      ADDRESS__TYPE :: MEM_ADR, AF, BF, EF, DSP_F, COV_F
      INTEGER*4  W00, WI0, WIJ, BI0, BIJ, CIJ, DIJ, Z00, ZI0, ZIJ, &
     &           E00, EI0, EIJ
      INTEGER*4  WM_GG, WM_LG, WM_LL, WM_SG1, WM_SG2, WM_SL1, &
     &           WM_SL2, WM_SSF, WM_SSS, WV_G, WV_L, WV_S
      INTEGER*4  WI0_ADR, WIJ_ADR, BI0_ADR, BIJ_ADR, CIJ_ADR, DIJ_ADR, &
     &           ZI0_ADR, ZIJ_ADR, EI0_ADR, EIJ_ADR
      INTEGER*4  J1, J2
      REAL*8     RK, RC
!
      IS=2392187
      RK=3.0
!
      G  = 4
      NL  = 2
      NS  = 4
      G  = 6
      L  = 5
      S  = 5
      SX = 3
!
      M  = G + NL*(L + (NS-1)*S + SX)  !  total number of parameters
      S_EQ  = S*RK     !  number of equations in one local group
      SX_EQ = SX*RK
      MEQ   = NL*((NS-2)*S_EQ + SX_EQ) !  total number of equations
      MA  = (M*(M+1))/2
      GA  = (G*(G+1))/2
      LA  = (L*(L+1))/2
      SA  = (S*(S+1))/2
      SXA = (SX*(SX+1))/2
!
      CALL GRAB_MEM ( -1, MEM_LEN, MEM_ADR,        30, &
     &                             8*INT8(M*MEQ),        AF, &
     &                             8*INT8(MEQ),          BF, &
     &                             8*INT8(M),            EF, &
     &                             8*INT8(M),            DSP_F, &
     &                             8*INT8(MA),           COV_F, &
     &                             8*GA,                 W00, &
     &                             8*INT8(NL*L*G),       WI0, &
     &                             8*NL*LA,              BI0, &
     &                             8*INT8(NL*NS*L*S),    BIJ, &
     &                             8*INT8(NL*NS*G*S),    WIJ, &
     &                             8*INT8(NL*NS*SA),     CIJ, &
     &                             8*INT8(NL*NS*S*S),    DIJ, &
     &                             8*INT8(G),            Z00, &
     &                             8*INT8(NL*L),         ZI0, &
     &                             8*INT8(NL*NS*S),      ZIJ, &
     &                             8*INT8(G),            E00, &
     &                             8*INT8(NL*L),         EI0, &
     &                             8*INT8(NL*NS*S),      EIJ, &
     &                             8*GA,                 WM_GG, &
     &                             8*INT8(L*G),          WM_LG, &
     &                             8*LA,                 WM_LL, &
     &                             8*INT8(S*G),          WM_SG1, &
     &                             8*INT8(S*G),          WM_SG2, &
     &                             8*INT8(S*L),          WM_SL1, &
     &                             8*INT8(S*L),          WM_SL2, &
     &                             8*INT8(S*S),          WM_SSF, &
     &                             8*SA,                 WM_SSS, &
     &                             8*INT8(G),            WV_G, &
     &                             8*INT8(L),            WV_L, &
     &                             8*INT8(S),            WV_S    )
!
      write ( 6, * ) ' m=',m,' meq=',meq
      CALL MAKE_B3D ( NL, NS, G, L, S, SX, M, GA, LA, SA, SXA, S_EQ, SX_EQ, MEQ, &
     &                %VAL(AF), %VAL(BF), %VAL(EF), IS, &
     &                %VAL(W00), %VAL(WI0), %VAL(WIJ), %VAL(BI0), %VAL(BIJ), &
     &                %VAL(CIJ), %VAL(DIJ), %VAL(Z00), %VAL(ZI0), %VAL(ZIJ) )
      IUER = -1
!!        call matview_1 ( m, meq, %val(af), -3 ) ! %%
      CALL LSQ_SOL  ( M, MEQ, %VAL(AF), %VAL(BF), %VAL(EF), %VAL(DSP_F), &
     &                %VAL(COV_F), RC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      DO 410 J1=1,NL
         WI0_ADR = WI0 + 8*(J1-1)*L*G
         WIJ_ADR = WIJ + 8*(J1-1)*NS*G*S
         BI0_ADR = BI0 + 8*(J1-1)*LA
         BIJ_ADR = BIJ + 8*(J1-1)*L*S*NS
         CIJ_ADR = CIJ + 8*(J1-1)*NS*SA
         DIJ_ADR = DIJ + 8*(J1-1)*NS*S*S
         ZI0_ADR = ZI0 + 8*(J1-1)*L
         ZIJ_ADR = ZIJ + 8*(J1-1)*NS*S
!
         IUER = -1
         CALL B1B3D_FRW ( G, L, S, SX, GA, LA, SA, NS, &
     &                    %VAL(W00), %VAL(WI0_ADR), &
     &                    %VAL(WIJ_ADR), %VAL(BI0_ADR), %VAL(BIJ_ADR), &
     &                    %VAL(CIJ_ADR), %VAL(DIJ_ADR), %VAL(Z00), &
     &                    %VAL(ZI0_ADR), %VAL(ZIJ_ADR), &
     &           %VAL(WM_GG), %VAL(WM_LG), %VAL(WM_LL), %VAL(WM_SG1), &
     &           %VAL(WM_SG2), %VAL(WM_SL1), %VAL(WM_SL2), %VAL(WM_SSF), &
     &           %VAL(WM_SSS), %VAL(WV_G), %VAL(WV_L), %VAL(WV_S), RC, IUER )
         IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
 410  CONTINUE
!
      IUER = -1
      CALL INVS ( G, %VAL(W00), RC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      CALL MUL_MV_SV_V ( G, %VAL(W00), G, %VAL(Z00), G, %VAL(E00), -3 )
!
      DO 420 J2=1,NL
         WI0_ADR = WI0 + 8*(J2-1)*L*G
         WIJ_ADR = WIJ + 8*(J2-1)*NS*G*S
         BI0_ADR = BI0 + 8*(J2-1)*LA
         BIJ_ADR = BIJ + 8*(J2-1)*L*S*NS
         CIJ_ADR = CIJ + 8*(J2-1)*NS*SA
         DIJ_ADR = DIJ + 8*(J2-1)*NS*S*S
         ZI0_ADR = ZI0 + 8*(J2-1)*L
         ZIJ_ADR = ZIJ + 8*(J2-1)*NS*S
         EI0_ADR = EI0 + 8*(J2-1)*L
         EIJ_ADR = EIJ + 8*(J2-1)*NS*S
!
!      SUBROUTINE B1B3D_BCK ( G, L, S, SX, GA, LA, SA, NS, &
!     &           W00, WI0, WIJ, BI0, BIJ, CIJ, DIJ, ZI0, ZIJ, &
!     &           E00, EI0, EIJ, WV_G, WV_L, WV_S )
         CALL B1B3D_BCK ( G, L, S, SX, GA, LA, SA, NS, &
     &                    %VAL(W00), %VAL(WI0_ADR), &
     &                    %VAL(WIJ_ADR), %VAL(BI0_ADR), %VAL(BIJ_ADR), &
     &                    %VAL(CIJ_ADR), %VAL(DIJ_ADR), &
     &                    %VAL(ZI0_ADR), %VAL(ZIJ_ADR), &
     &                    %VAL(E00), %VAL(EI0_ADR), %VAL(EIJ_ADR), &
     &                    %VAL(WV_G), %VAL(WV_L), %VAL(WV_S) )
 420  CONTINUE
!
      CALL LSQ_SHOW ( M, %VAL(EF), NL, NS, G, L, S, SX, %VAL(E00), %VAL(EI0), &
     &                %VAL(EIJ) )
      END  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MAKE_B3D ( NL, NS, G, L, S, SX, M, GA, LA, SA, SXA, &
     &                      S_EQ, SX_EQ, MEQ, AF, BF, EF, IS, &
     &                      W00, WI0, WIJ, BI0, BIJ, CIJ, DIJ, Z00, ZI0, ZIJ )
      IMPLICIT   NONE
      INTEGER*8  GA, LA, SA, SXA
      INTEGER*4  NL, NS, G, L, S, SX, M, S_EQ, SX_EQ, MEQ, IS
      REAL*8     AF(M,MEQ), BF(MEQ), EF(M)
      REAL*8     W00(GA), WI0(L,G,NL), WIJ(S,G,NS,NL), BI0(LA,NL), &
     &           BIJ(S,L,NS,NL), CIJ(SA,NS,NL), DIJ(S,S,NS,NL), &
     &           Z00(G), ZI0(L,NL), ZIJ(S,NS,NL)
      REAL*8     RGAUSS
      INTEGER*4  IND(64*1024)
      INTEGER*4  IEQ, SC, SN, SC_EQ, IP, IG, IL, IS1, IS2, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10
!
      CALL NOUT ( 8*MEQ*M, AF )
!
      DO 410 J1=1,M
         EF(J1) = J1
         IND(J1) = J1
 410  CONTINUE
!
      CALL NOUT ( 8*GA, W00 )
      CALL NOUT ( 8*G,  Z00 )
!
      DO 420 J2=1,NL
         CALL NOUT ( 8*G*L, WI0(1,1,J2) )
         CALL NOUT ( 8*LA,  BI0(1,J2)   )
         CALL NOUT ( 8*G,   ZI0(1,J2)   )
         DO 430 J3=1,NS
            CALL NOUT ( 8*G*S,  WIJ(1,1,J3,J2) )
            CALL NOUT ( 8*L*S,  BIJ(1,1,J3,J2) )
            CALL NOUT ( 8*SA,   CIJ(1,J3,J2)   )
            CALL NOUT ( 8*S*S,  DIJ(1,1,J3,J2) )
            CALL NOUT ( 8*S,    ZIJ(1,J3,J2)   )
 430     CONTINUE
 420  CONTINUE
!
      IEQ = 0
      DO 440 J4=1,NL
         DO 450 J5=1,NS-1
            IF ( J5 .EQ. NS-1 ) THEN
                 SC_EQ = SX_EQ
                 SC    = S
                 SN    = SX
               ELSE
                 SC_EQ = S_EQ
                 SC    = S
                 SN    = S
            END IF
!
            DO 460 J6=1,SC_EQ
               IEQ = IEQ + 1
               BF(IEQ) = RGAUSS ( IS, 0.001D0 )
!
               IG = 1
               DO 470 J7=1,G
                  IP = IG + (J7-1)
                  AF(IP,IEQ) = RGAUSS ( IS, 1.0D0 )
                  BF(IEQ) = BF(IEQ) + AF(IP,IEQ)*EF(IP)
 470           CONTINUE
!
               IL = G + (J4-1)*(L + (NS-1)*S + SX) + 1
               DO 480 J8=1,L
                  IP = IL + (J8-1)
                  AF(IP,IEQ) = RGAUSS ( IS, 1.0D0 )
                  BF(IEQ) = BF(IEQ) + AF(IP,IEQ)*EF(IP)
 480           CONTINUE
!
               IS1 = G + (J4-1)*(L + (NS-1)*S + SX) + L + (J5-1)*S + 1
               DO 490 J9=1,SC
                  IP = IS1 + (J9-1)
                  AF(IP,IEQ) = RGAUSS ( IS, 1.0D0 )
                  BF(IEQ) = BF(IEQ) + AF(IP,IEQ)*EF(IP)
 490           CONTINUE
!
               IS2 = G + (J4-1)*(L + (NS-1)*S + SX) + L + J5*S + 1
               DO 4100 J10=1,SN
                  IP = IS2 + (J10-1)
                  AF(IP,IEQ) = RGAUSS ( IS, 1.0D0 )
                  BF(IEQ) = BF(IEQ) + AF(IP,IEQ)*EF(IP)
 4100          CONTINUE
!
               CALL ADD_TRG ( BF(IEQ), 1.0D0, G, IND, AF(IG,IEQ), &
     &                        G, Z00, W00 )
               CALL ADD_RCT ( BF(IEQ), 1.0D0, L, IND, AF(IL,IEQ), &
     &                        G, IND, AF(IG,IEQ), L, G, WI0(1,1,J4) )
               CALL ADD_TRG ( BF(IEQ), 1.0D0, L, IND, AF(IL,IEQ), &
     &                        L, ZI0(1,J4), BI0(1,J4) )
!
               CALL ADD_TRG ( BF(IEQ), 1.0D0, SC, IND, AF(IS1,IEQ), &
     &                        SC, ZIJ(1,J5,J4), CIJ(1,J5,J4) )
               CALL ADD_RCT ( BF(IEQ), 1.0D0, SC, IND, AF(IS1,IEQ), &
     &                        L, IND, AF(IL,IEQ), SC, L, BIJ(1,1,J5,J4) )
               CALL ADD_RCT ( BF(IEQ), 1.0D0, SC, IND, AF(IS1,IEQ), &
     &                        G, IND, AF(IG,IEQ), SC, G, WIJ(1,1,J5,J4) )
!
               CALL ADD_TRG ( BF(IEQ), 1.0D0, SN, IND, AF(IS2,IEQ), &
     &                        SN, ZIJ(1,J5+1,J4), CIJ(1,J5+1,J4) )
               CALL ADD_RCT ( BF(IEQ), 1.0D0, SN, IND, AF(IS2,IEQ), &
     &                        L, IND, AF(IL,IEQ), SN, L, BIJ(1,1,J5+1,J4) )
               CALL ADD_RCT ( BF(IEQ), 1.0D0, SN, IND, AF(IS2,IEQ), &
     &                        G, IND, AF(IG,IEQ), SN, G, WIJ(1,1,J5+1,J4) )
               CALL ADD_RCT ( BF(IEQ), 1.0D0, SN, IND, AF(IS2,IEQ), &
     &                        SC, IND, AF(IS1,IEQ), SN, SC, DIJ(1,1,J5+1,J4) )
 460        CONTINUE
 450     CONTINUE
 440  CONTINUE
!        call matview_2 ( g, b0, -3 ) ! %%
!        call matview_2 ( l, c(1,1), -3 ) ! %%
!        call matview_2 ( l, c(1,2), -3 ) ! %%
!        call matview_1 ( l, l, d(1,1,1), -3 ) ! %%
!        call matview_1 ( l, l, d(1,1,2), -3 ) ! %%
!        call matview_1 ( m, meq, af, -3 ) ! %%
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
      SUBROUTINE LSQ_SHOW ( M, EF, NL, NS, G, L, S, SX, E00, EI0, EIJ )
      IMPLICIT   NONE
      INTEGER*4  M, NL, NS, G, L, S, SX
      REAL*8     EF(M), E00(G), EI0(L,NL), EIJ(S,NS,NL)
      INTEGER*4  IEQ, J1, J2, J3, J4, J5, J6
!
      IEQ = 0
      DO 410 J1=1,G
         IEQ = IEQ + 1
         WRITE ( 6, 110 ) J1, EF(IEQ), E00(J1)
 110     FORMAT ( 'I=',I3,' Ef=',1PD15.7,' E00=',1PD15.7 )
 410  CONTINUE
!
      DO 420 J2=1,NL
         DO 430 J3=1,L
            IEQ = IEQ + 1
            WRITE ( 6, 120 ) IEQ, EF(IEQ), J2, J3, EI0(J3,J2)
 120        FORMAT ( 'I=',I3,' Ef=',1PD15.7,' L-block: ',I3,' par: ',I3, &
     &               ' Eio= ',1PD15.7 )
 430     CONTINUE
!
         DO 440 J4=1,NS-1
            DO 450 J5=1,S
               IEQ = IEQ + 1
               WRITE ( 6, 130 ) IEQ, EF(IEQ), J2, J4, J5, EIJ(J5,J4,J2)
 130           FORMAT ( 'I=',I3,' Ef=',1PD15.7,' Lb: ',I3, &
     &                  ' Sb: ',I3,' par:',I3,' Eij= ',1PD15.7 )
 450        CONTINUE
 440     CONTINUE
!
         DO 460 J6=1,SX
            IEQ = IEQ + 1
            WRITE ( 6, 140 ) IEQ, EF(IEQ), J2, J6, EIJ(J6,NS,J2)
 140        FORMAT ( 'I=',I3,' Ef=',1PD15.7,' Lb: ',I3,' Sb: last', &
     &               ' par: ',I3,' Elx= ',1PD15.7 )
 460     CONTINUE
 420  CONTINUE
!
      RETURN
      END  !#!  LSQ_SHOW  #!#
