        SUBROUTINE B3D_SOL_X ( N, G, L, LA, LX, LXA, B0, Z0, E0, &
     &             B, C, D, ZL, EL, BX, CX, DX, ZLX, ELX, &
     &             WM_GG, WM_LG1, WM_LG2, WM_LLS, WM_LLF, WV_L, WV_G, &
     &             RCOND, IUER )
! ************************************************************************
! *                                                                      *
! *     Subroutine  B3D_SOL_X  solves system of normal equations using   *
! *     B3D algorithm. Assumed that all blocks of local parameters       *
! *     but the last contain the same number of parameters. Assumed that *
! *     the last block has equal or less number of parametrers. Last     *
! *     block should contain less or equal number of parameters.         *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- number of group of local parameters.       *
! *        G ( INTEGER*4 ) -- number of global parameters.               *
! *        L ( INTEGER*4 ) -- number of local parameters at one block    *
! *                           for blocks (1,N-1) except the last one.    *
! *       LA ( INTEGER*8 ) -- (L*(L+1))/2                                *
! *       LX ( INTEGER*4 ) -- number of local parameters at the last     *
! *                           block.                                     *
! *      LXA ( INTEGER*8 ) -- (LX*(LX+1))/2                              *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *       B0 ( REAL*8    ) -- matrix of global-global block. Dimension G *
! *       Z0 ( REAL*8    ) -- vector of right parts of normal equations  *
! *                           for global parameters.                     *
! *        B ( REAL*8    ) -- array of N-1 matrices local-global blocks. *
! *                           dimension: L*G*(N-1).                      *
! *        C ( REAL*8    ) -- array of N-1 diagonal local-local blocks.  *
! *                           dimension: LA*N.                           *
! *        D ( REAL*8    ) -- array of N-1 rectangular down-diagonal     *
! *                           local-local blocks. dimension: L*L*(N-1).  *
! *                           Comment: the first matrix doesn't have any *
! *                           physical sence and isn't used. It only     *
! *                           occupies place for the convinience of      *
! *                           index calculations.                        *
! *       ZL ( REAL*8    ) -- array of N vectors of rights parts of      *
! *                           normal equations for local parameters.     *
! *       BX ( REAL*8    ) -- last, N-th matrix of local-global block.   *
! *                           dimension: LX*G.                           *
! *       CX ( REAL*8    ) -- last, N-th diagonal local-local block.     *
! *                           dimension: LXA.                            *
! *       DX ( REAL*8    ) -- last, N-th rectangular down-diagonal       *
! *                           local-local block. Dimension: LX*L.        *
! *      ZLX ( REAL*8    ) -- last, N-th vector of right parts of normal *
! *                           equations for local parameters.            *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *       E0 ( REAL*8    ) -- vector of the estimates of global          *
! *                           parameters. dimension: G                   *
! *       EL ( REAL*8    ) -- array of N-1 vectors of the estimates of   *
! *                           local parameters. Dimension: L*N-1         *
! *      ELX ( REAL*8    ) -- last, N-th vector of the estimates of      *
! *                           local parameters. Dimension: LX            *
! *    RCOND ( REAL*8    ) -- condition number for combined              *
! *                           global-global matrix.                      *
! *                                                                      *
! * ________________________ WORKING PARAMETERS: _______________________ *
! *                                                                      *
! *     WM_GG  ( REAL*8    ) -- working vector. Dimension: (G*(G+1))/2   *
! *     WM_LG1 ( REAL*8    ) -- working vector. Dimension: L*G           *
! *     WM_LG2 ( REAL*8    ) -- working vector. Dimension: L*G           *
! *     WM_LLS ( REAL*8    ) -- working vector. Dimension: (L*(L+1))/2   *
! *     WV_L   ( REAL*8    ) -- working vector. Dimension: L             *
! *     WV_G   ( REAL*8    ) -- working vector. Dimension: G             *
! *                                                                      *
! *  ###  01-NOV-95     B3D_SOL_X   V2.2 (c)  L. Petrov  10-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT  NONE
        INTEGER*4 N, G, L, LX, IUER
        ADDRESS__TYPE :: LA, LXA
        REAL*8    B0(*), E0(G), Z0(G), &
     &            B(L,G,N), C(LA,N), D(L,L,N), ZL(L,N), EL(L,N), &
     &            BX(LX,G), CX(LXA), DX(LX,L), ZLX(LX), ELX(LX)
        REAL*8    WM_GG(*), WM_LG1(L,G), WM_LG2(L,G), WM_LLS(LA), &
     &            WM_LLF(L,L), WV_L(L), WV_G(G)
        REAL*8    RCOND
        INTEGER*4 J1, J2, IER
        INTEGER*4 LP, LC
        ADDRESS__TYPE :: GA, LPA
        ADDRESS__TYPE :: AD_BP, AD_CP, AD_DP, AD_ZLP, AD_ELP, &
     &                   AD_BC, AD_CC, AD_DC, AD_ZLC, AD_ELC 
        CHARACTER STR*20, STR1*20
        INTEGER*4, EXTERNAL :: I_LEN
!
        GA = (INT8(G)*INT8(G+1))/2
!
        IF ( LX .GT. L ) THEN
             CALL CLRCH   (     STR  )
             CALL INCH    ( LX, STR  )
             CALL CLRCH   (     STR1 )
             CALL INCH    ( L,  STR1 )
             CALL ERR_LOG ( 8101, IUER, 'B3D_SOL_X', 'Parameter LX is too '// &
     &           'large:  LX='//STR(1:I_LEN(STR))//'  LX > L (L='// &
     &            STR1(1:I_LEN(STR1))//') ' )
             RETURN
        END IF
!
! ----- II. Matrix decomposition.
!       ~~~~~~~~~~~~~~~~~~~~~~~~
!
        DO 410 J1=N,1,-1  !  (in backward direction)
!
! -------- Calculation addresses for previous block.
!
           IF ( J1 .GT. 1 ) THEN
                AD_BP = LOC(  B(1,1,J1-1) )
                AD_CP = LOC(  C(1,  J1-1) )
                AD_DP = LOC(  D(1,1,J1-1) )
                AD_ZLP= LOC( ZL(1,  J1-1) )
                LP    = L
                LPA   = LA
             ELSE IF ( J1 .EQ. 1 ) THEN
                AD_BP = -1
                AD_CP = -1
                AD_DP = -1
                AD_ZLP= -1
                LP    = -1
                LPA   = -1
           END IF
!
! -------- Calculation addresses for current block. We should trace the case
! -------- when the current block is last, reduced one
!
           IF ( J1 .EQ. N ) THEN
                AD_BC = LOC( BX )
                AD_CC = LOC( CX )
                AD_DC = LOC( DX )
                AD_ZLC= LOC( ZLX)
                LC    = LX
             ELSE IF ( J1 .LT. N ) THEN
                AD_BC = LOC(  B(1,1,J1) )
                AD_CC = LOC(  C(1,  J1) )
                AD_DC = LOC(  D(1,1,J1) )
                AD_ZLC= LOC( ZL(1,  J1) )
                LC    = L
           END IF
!
! -------- 2.0 Inversion martix of local-local parameters
!
! -------- C(J1) := C(J1)^(-1)
!
           CALL ERR_PASS ( IUER, IER )
           CALL INVS     ( LC, %VAL(AD_CC), RCOND, IER )
           IF ( IER.NE.0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( J1, STR )
                CALL CLRCH ( STR1 )
                WRITE ( UNIT=STR1(1:12), FMT='(1PD12.4)' ) RCOND
                CALL ERR_LOG ( 8102, IUER, 'B3D_SOL_X', 'Error during '// &
     &              'inversion matrix of local-local parameters of '// &
     &               STR(1:I_LEN(STR))//'-th block.  Conditional number ='// &
     &               STR1 )
                RETURN
           END IF
!
! -------- 2.1 Excluding block B(T)(j1)
!
! -------- WM_LG1 := C^(-1)(j1) * B(j1)
!
           CALL MUL_MM_SI_I ( LC, %VAL(AD_CC), LC, G, %VAL(AD_BC), &
     &                                         LC, G, WM_LG1, -3 )
!
! -------- WV_G := B(T)(j1) * C^(-1)(j1) * ZL(j1)
!
           CALL MUL_MV_TV_V ( LC, G, WM_LG1, LC, %VAL(AD_ZLC), G, WV_G, -3 )
!
! -------- Update vector of right parts for global parameters
! -------- Z0 = Z0 - B(T)(j1) * C^(-1)(j1) * ZL(j1)
!
           CALL SUB_VV ( G, Z0, WV_G )
!
! -------- WM_GG := B(T)(j1) * C^(-1)(j1) * B(j1)
!
           CALL MUL_MM_TI_S ( LC, G, %VAL(AD_BC), LC, G, WM_LG1, G, WM_GG, -3 )
!
! -------- Update matrix of global-global parameters
! -------- B0 := B0 - B(T)(j1) * C^(-1)(j1) * B(j1)
!
           CALL SUB_VV8 ( GA, B0, WM_GG )
!
           IF ( J1.NE.1 ) THEN
!
! ------------- 2.2 Excluding block D'(j1)
!
! ------------- WM_LG2 := D(T)(j1) * C^(-1)(j1) * B(j1)
!
                CALL MUL_MM_TI_I ( LC, LP, %VAL(AD_DC), LC, G, WM_LG1, &
     &                             LP, G, WM_LG2, -3 )
!
! ------------- 2.3 Updating previous block of global-local parameters
! ------------- B(j1-1) := B(j1-1) - D(T)(j1) * C^(-1)(j1) * B(j1)
!
                CALL SUB_VV ( LP*G, %VAL(AD_BP), WM_LG2 )
!
! ------------- WM_LLS := D(T)(j1) * C^(-1)(j1) * D(j1)
! ------------- Calculation are being done in two steps
!
                CALL MUL_MM_SI_I ( LC, %VAL(AD_CC), LC, LP, %VAL(AD_DC), &
     &                             LC, LP, WM_LLF, -3 )
                CALL MUL_MM_TI_S ( LC, LP, %VAL(AD_DC), LC, LP, WM_LLF, &
     &                                                      LP, WM_LLS, -3 )
!
! ------------- 2.4 Updating previous down-diagonal block of local-local param.
! ------------- C(j1-1) := C(j1-1) - D(T)(j1) * C^(-1)(j1) * D(j1)
!
                CALL SUB_VV8 ( LPA,  %VAL(AD_CP), WM_LLS )
!
! ------------- Updating previous vector of rights parts for local parameters
! ------------- WV_L := D(T)(j1) * C^(-1)(j1) * ZL(j1)
!
                CALL MUL_MV_TV_V ( LC, LP, WM_LLF, LC, %VAL(AD_ZLC), &
     &                                             LP, WV_L, -3 )
!
! ------------- ZL(j1-1) := ZL(j1-1) - D * C^(-1)(j1) * ZL(j1)
!
                CALL SUB_VV  ( LP, %VAL(AD_ZLP), WV_L )
!
! ------------- Substitution D(j1) := C^(-1)(j1) * D(j1)
!
                CALL COPY_V  ( LC*LP, WM_LLF, %VAL(AD_DC) )
           END IF
!
! -------- Substitution B(j1) := C^(-1)(j1) * B(j1)
!
           CALL COPY_V8  ( INT8(LC)*INT8(G), WM_LG1, %VAL(AD_BC) )
  410   CONTINUE
!
! ----- III. Inversion of updated (combined) global-global matrix
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        CALL ERR_PASS ( IUER, IER )
        CALL INVS     ( G, B0, RCOND, IER )
        IF ( IER.NE.0 ) THEN
             CALL CLRCH ( STR1 )
             WRITE ( UNIT=STR1(1:12), FMT='(1PD12.4)' ) RCOND
             CALL ERR_LOG ( 8103, IUER, 'B3D_SOL_X', 'Error during '// &
     &           'inversion matrix of global-global parameters. '// &
     &           'Conditional number ='//STR1 )
             RETURN
        END IF
!
! ----- Obtaining estimates of global parameters
!
        CALL MUL_MV_SV_V ( G, B0, G, Z0, G, E0, -3 )
!
! ----- IV. Determination local parameters
!       ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
        DO 420 J2=1,N  !  (in forward direction)
!
! -------- Calculation addresses for previous block
!
           IF ( J2 .GT. 1 ) THEN
                AD_BP = LOC( B(1,1,J2-1) )
                AD_CP = LOC( C(1,J2-1)   )
                AD_DP = LOC( D(1,1,J2-1) )
                AD_ZLP= LOC( ZL(1,J2-1)  )
                AD_ELP= LOC( EL(1,J2-1)  )
                LP    = L
             ELSE IF ( J2 .EQ. 1 ) THEN
                AD_BP = -1
                AD_CP = -1
                AD_DP = -1
                AD_ZLP= -1
                AD_ELP= -1
                LP    = -1
           END IF
!
! -------- Calculation addresses for current block. We should trace the case
! -------- when the current block is last, reduced one.
!
           IF ( J2 .EQ. N ) THEN
                AD_BC = LOC( BX )
                AD_CC = LOC( CX )
                AD_DC = LOC( DX )
                AD_ZLC= LOC( ZLX)
                AD_ELC= LOC( ELX)
                LC    = LX
             ELSE IF ( J2 .LT. N ) THEN
                AD_BC = LOC(  B(1,1,J2) )
                AD_CC = LOC(  C(1,  J2) )
                AD_DC = LOC(  D(1,1,J2) )
                AD_ZLC= LOC( ZL(1,  J2) )
                AD_ELC= LOC( EL(1,  J2) )
                LC    = L
           END IF
!
! -------- Calculation estimates of local parameters
! -------- EL(J2) := C^(-1)(j2) * ZL(j2)
!
           CALL MUL_MV_SV_V ( LC, %VAL(AD_CC), LC, %VAL(AD_ZLC), &
     &                                         LC, %VAL(AD_ELC), -3 )
!
! -------- Calculate correction for presence global parameters
! -------- WV_L := C^(-1)(j2) * B(j2) * E0
!
           CALL MUL_MV_IV_V ( LC, G, %VAL(AD_BC), G, E0, LC, WV_L, IER )
!
! -------- Update estimates due to this correstion
! -------- EL(j2) := EL(j2) - C^(-1)(j2) * B(j2) * E0
!
           CALL SUB_VV  ( LC, %VAL(AD_ELC), WV_L )
           IF ( J2.NE.1 ) THEN
!
! ------------- Calculate correction for presence local parameters previous
! ------------- block.
! ------------- WV_L := C^(-1)(j2) * D(j2) * EL(j2-1)
!
                CALL MUL_MV_IV_V ( LC, LP, %VAL(AD_DC), LP, %VAL(AD_ELP), &
     &                             LC, WV_L, IER )
!
! ------------- Correct estimates
! ------------- EL(j2) := EL(j2) - C^(-1)(j2) * D(j2) * EL(j2-1)
!
                CALL SUB_VV  ( LC, %VAL(AD_ELC), WV_L )
           END IF
  420   CONTINUE
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  B3D_SOL_X  #!#
