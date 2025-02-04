      SUBROUTINE B3D_COV_F ( N, G, L, LA, LX, LXA, NK, B0, CB, C, CD, &
     &                       CBX, CX, CDX, COVM, &
     &                       W0_LG, W1_LG, W2_LLS, W3_LL, W4_LLS, W5_LL )
! ************************************************************************
! *                                                                      *
! *       Subroutine  B3D_COV_F  calculates full set of covariance       *
! *     submatrices of the estimates obtained by solving LSQ problem by  *
! *     B3D algorithm.                                                   *
! *       Assumed that all blocks of local parameters but the last       *
! *     contain the same number of parameters. Last block should contain *
! *     less or equal number of parameters.                              *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- number of group of local parameters.       *
! *        G ( INTEGER*4 ) -- number of global parameters.               *
! *        L ( INTEGER*4 ) -- number of local parameters at one block    *
! *                           for blocks (1,N-1) except the last one.    *
! *       LA ( INTEGER*4 ) -- (L*(L+1))/2                                *
! *       LX ( INTEGER*4 ) -- number of local parameters at the last     *
! *                           block.                                     *
! *      LXA ( INTEGER*4 ) -- (LX*(LX+1))/2                              *
! *       NK ( INTEGER*4 ) -- (N-1)*(N-2)/2 -- the number of frames of   *
! *                           off diagonal blovks of the covariance      *
! *                           matrix. Control parameter.                 *
! *       B0 ( REAL*8    ) -- Left-corner block of decomposed normal     *
! *                           matrix. It is unscaled covariance matrix   *
! *                           of global parameters.                      *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *       CB ( REAL*8    ) -- Input: array of N-1 modified left-border   *
! *                           blocks of the decomposed normal matrix.    *
! *                           CB(i) = C^(-1) * B(i). Output: array of N-1*
! *                           unscaled covariance local-global matrices. *
! *                           Dimension: L*G*(N-1).                      *
! *        C ( REAL*8    ) -- Input: array of N-1 modified diagonal      *
! *                           blocks of the decomposed normal matrix.    *
! *                           Output: array of N-1 unscaled local-local  *
! *                           covariance matrices occuping diagonal band *
! *                           on the full matrix.                        *
! *       CD ( REAL*8    ) -- Input: Array of N-1 modified down-diagonal *
! *                           blocks of the decomposed normal matrix.    *
! *                           CD(i) = C^(-1)(i) * D(i) Dimension:        *
! *                           L*L*(N-1). Output: array of N unscaled     *
! *                           local-local cross covariance matrices of   *
! *                           adjacent blocks occuping down-diagonal     *
! *                           band. Comment: the first matrix doesn't    *
! *                           have any physical sence and isn't used. It *
! *                           only occupies place for the convinience of *
! *                           index calculations.                        *
! *       BX ( REAL*8    ) -- Input: last, N-th modified local-global    *
! *                           block of decomposed matrix. Dimension:     *
! *                           LX*G. Output: last, N-th unscaled          *
! *                           local-global block of covariance matrix.   *
! *       CX ( REAL*8    ) -- Input: last, N-th modified local-local     *
! *                           block of decomposed matrix. Dimension:     *
! *                           LXA. Output: last, N-th unscaled           *
! *                           local-local block of covariance matrix.    *
! *       DX ( REAL*8    ) -- Input: last, N-th rectangular modified     *
! *                           block of decomposed matrix. Dimension:     *
! *                           LX*L. Output: last, N-th unscaled          *
! *                           cross local-local block of covariance      *
! *                           matrix for adjacent segments.              *
! *                                                                      *
! * ________________________ OUTPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      CVM ( REAL*8    ) -- Array of off-diagonal blocks of covariance *
! *                           matrix. Array consist of NK L*L frames     *
! *                           (It is assumed that the last block has     *
! *                            less or the same dimension and it is      *
! *                            enough room for allocation it in one      *
! *                            frame). Frames are numbered by columns    *
! *                           starting from j+2,j;  j+3,j; j+4,j; ...    *
! *                           j+3,j+1; j+4,j+1  etc.                     *
! *                                                                      *
! * ________________________ WORKING PARAMETERS: _______________________ *
! *                                                                      *
! *     W0_LG  ( REAL*8    ) -- working vector. Dimension: L*G           *
! *     W1_LG  ( REAL*8    ) -- working vector. Dimension: L*G           *
! *     W2_LLS ( REAL*8    ) -- working vector. Dimension: LA            *
! *     W3_LL  ( REAL*8    ) -- working vector. Dimension: L*L           *
! *     W4_LLS ( REAL*8    ) -- working vector. Dimension: LA            *
! *     W5_LL  ( REAL*8    ) -- working vector. Dimension: L*L           *
! *                                                                      *
! *  ###  08-SEP-97   B3D_COV_F    v1.1 (c) L. Petrov  10-JUL-2003  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, G, L, LA, LX, LXA, NK
      REAL*8     B0(*), CB(L,G,N), C(LA,N), CD(L,L,N), &
     &                  CBX(LX,G), CX(LXA), CDX(LX,L), COVM(L,L,*), &
     &           W0_LG(L,G), W1_LG(L,G), W2_LLS(LA), W3_LL(L,L), &
     &           W4_LLS(LA), W5_LL(L,L)
      INTEGER*4  LP, LPA, LC, LCA, LN, LNA, LK, LKA
      ADDRESS__TYPE :: AD_BP, AD_CP, AD_DP, &
     &                 AD_BC, AD_CC, AD_DC, &
     &                 AD_BN, AD_CN, AD_DN, &
     &                 AD_BK, AD_CK, AD_DK
      INTEGER*4  J1, J2, IK, I_LEN
      CHARACTER  STR*20
!
! --- Calculation local-global and local-local covariance matrices
!
      IK=1
      DO 410 J1=1,N
!
! ------ Calculation addresses for previous block
!
         IF ( J1 .GT. 1 ) THEN
              AD_BP = LOC( CB(1,1,J1-1) )
              AD_CP = LOC(  C(1,  J1-1) )
              AD_DP = LOC( CD(1,1,J1-1) )
              LP    = L
              LPA   = LA
           ELSE IF ( J1 .EQ. 1 ) THEN
              AD_BP = -1
              AD_CP = -1
              AD_DP = -1
              LP    = -1
              LPA   = -1
         END IF
!
! ------ Calculation addresses for current block. We should trace the case
! ------ when the current block is last, reduced one.
!
         IF ( J1 .EQ. N ) THEN
              AD_BC = LOC( CBX )
              AD_CC = LOC( CX  )
              AD_DC = LOC( CDX )
              LC    = LX
              LCA   = LXA
           ELSE IF ( J1 .LT. N ) THEN
              AD_BC = LOC( CB(1,1,J1) )
              AD_CC = LOC(  C(1,  J1) )
              AD_DC = LOC( CD(1,1,J1) )
              LC    = L
              LCA   = LA
         END IF
!
! ------ Calculation addresses for current block. We should trace the case
! ------ when the current block is last, reduced one.
!
         IF ( J1 .EQ. N-1 ) THEN
              AD_BN = LOC( CBX )
              AD_CN = LOC( CX  )
              AD_DN = LOC( CDX )
              LN    = LX
              LNA   = LXA
           ELSE IF ( J1 .LT. N-1) THEN
              AD_BN = LOC( CB(1,1,J1+1) )
              AD_CN = LOC(  C(1,  J1+1) )
              AD_DN = LOC( CD(1,1,J1+1) )
              LN    = L
              LNA   = LA
           ELSE
              AD_BN = -1
              AD_CN = -1
              AD_DN = -1
         END IF
!
! ------ Calculation of Cov(j1,o)
!        ~~~~~~~~~~~~~~~~~~~~~~~~
!
! ------ W0 := - CB(j1) * B0
!
         CALL MUL_MM_IS_I ( LC, G, %VAL(AD_BC), G, B0, LC, G, W0_LG, -3 )
         CALL VEC_MULT_CONSTANT ( W0_LG, LC*G, -1.D0, W0_LG )
!C
         IF ( J1.GT.1 ) THEN
!
! ----------- Updating Cov(j1,o)
!
! ----------- W1 := CD(j1) * Cov(j1-1,o)
!
              CALL MUL_MM_II_I ( LC, LP, %VAL(AD_DC), LP, G, %VAL(AD_BP), &
     &                           LC, G, W1_LG, -3 )
!
! ----------- W0 := CB(j1) * B0  -  CD(j1) * Cov(j1-1,o) = Cov(j1,o)
!
              CALL SUB_VV      ( LC*G, W0_LG, W1_LG )
         END IF
!
! ------ Calculation Cov(j1,j1)
!        ~~~~~~~~~~~~~~~~~~~~~~
!
! ------ W2 := CB(T)(j1) * Cov(j1,o)
!
         CALL MUL_MM_IT_S ( LC, G, %VAL(AD_BC), LC, G, W0_LG, LC, W2_LLS, -3 )
!
! ------ W4 := C^(-1)(j1) - Cov(T)(j1,o) * CB(j1)
!
         CALL SUB_VV_V ( LCA, %VAL(AD_CC), W2_LLS, W4_LLS )
         IF ( J1 .GT. 1 ) THEN
!
! ----------- W2 := CD(j1) * Cov(T)(j1,j1-1)
!
              CALL MUL_MM_IT_S ( LC, LP, %VAL(AD_DC), LC, LP, W5_LL, &
     &                           LC,     W2_LLS, -3 )
!
! ----------- W4 := Cov(j1,j1)
!
              CALL SUB_VV ( LCA,   W4_LLS, W2_LLS )
              CALL COPY_V ( LC*LP, W5_LL,  %VAL(AD_DC) )  !  W5 ==> AD_DC
         END IF
!C
         IF ( J1 .LT. N ) THEN
!
! ----------- Calculation Cov(j1+1,j1)
!             ~~~~~~~~~~~~~~~~~~~~~~~~
!
! ----------- W5 := CD(j1+1) * Cov(j1,j1)
!
              CALL MUL_MM_IS_I ( LN, LC, %VAL(AD_DN), LC, W4_LLS, &
     &                           LN, LC, W5_LL, -3 )
!
! ----------- W3 := B(j1+1) * Cov(T)(j1,o)
!
              CALL MUL_MM_IT_I ( LN, G, %VAL(AD_BN), LC, G, W0_LG, &
     &                           LN, LC, W3_LL, -3 )
!
! ----------- W5 : = CD(j1+1) * Cov(j1,j1) + CB(j1+1) * Cov(j1,o)
!
              CALL ADD_VV ( LN*LC, W5_LL, W3_LL )
!
! ----------- W5 = Cov(j1+1,j1)
!
              CALL VEC_MULT_CONSTANT ( W5_LL, LN*LC, -1.D0, W5_LL )
         END IF
         CALL COPY_V8 ( INT8(LC)*INT8(G), W0_LG,  %VAL(AD_BC) )  ! W0 ==> AD_BC
         CALL COPY_V  ( LCA,  W4_LLS, %VAL(AD_CC) )  ! W4 ==> AD_CC
!
! ------ Now cycle for making off-diagonal blocks which are located down
! ------ down-diagonal band.
!
         IF ( J1+2 .LE. N ) THEN
              DO 420 J2=J1+2,N
                 IF ( J2 .EQ. N ) THEN
                      AD_BK = LOC( CBX )
                      AD_CK = LOC( CX  )
                      AD_DK = LOC( CDX )
                      LK    = LX
                      LKA   = LXA
                   ELSE IF ( J2 .LT. N ) THEN
                      AD_BK = LOC( CB(1,1,J2) )
                      AD_CK = LOC(  C(1,  J2) )
                      AD_DK = LOC( CD(1,1,J2) )
                      LK    = L
                      LKA   = LA
                   ELSE
                      AD_BK = -1
                      AD_CK = -1
                      AD_DK = -1
                 END IF
                 IF ( IK .GT. NK ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( NK, STR )
                      CALL ERR_LOG ( 2801, -1, 'B3D_COV_F', 'Parameter NK ('// &
     &                     STR(1:I_LEN(STR))//') is too small.' )
                      RETURN
                 END IF
!
! -------------- Cov(j2,j1) := CD(j2) * Cov(j2-1,j1)
!
                 IF ( J2 .EQ. J1+2 ) THEN
                      CALL MUL_MM_II_I ( LK, L, %VAL(AD_DK), L, L, W5_LL, &
     &                     LK, L, COVM(1,1,IK), -3 )
                    ELSE
                      CALL MUL_MM_II_I ( LK, L, %VAL(AD_DK), &
     &                     L, L, COVM(1,1,IK-1), LK, L, COVM(1,1,IK), -3 )
                 END IF
!
! -------------- W3 := CB(j2) * Cov(j1,o)
!
                 CALL MUL_MM_IT_I ( LK, G, %VAL(AD_BK), L, G, %VAL(AD_BC), &
     &                              LK, L, W3_LL, -3 )
!
! -------------- Cov(j2,j1) := CD(j2) * Cov(j2-1,j1) + CB(j2) * Cov(j1,o)
!
                 CALL ADD_VV ( LK*L, COVM(1,1,IK), W3_LL )
!
! -------------- Cov(j2,j1) := -( CD(j2) * Cov(j2-1,j1) + CB(j2) * Cov(j1,o) )
!
                 CALL VEC_MULT_CONSTANT ( COVM(1,1,IK), LK*L, -1.D0, &
     &                                    COVM(1,1,IK) )
!
                 IK=IK+1
 420          CONTINUE
         END IF
  410 CONTINUE
!
      RETURN
      END  !#!  B3D_COV_F  #!#
