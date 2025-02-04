      SUBROUTINE B1B3D_COV1 ( G, L, S, SX, GA, LA, SA, NS, &
     &                        W00, WI0, BI0, ZI0, &
     &                        WIJ, BIJ, CIJ, DIJ, ZIJ, &
     &                        WM_GG, WM_LG, WM_LL, &
     &                        IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  B1B3D_COV1  calculates local-local and local-global     *
! *   covariance matrices of the parameter estimates for the certain     *
! *   block of the local parameters produced by solving LSQ problem      *
! *   using B1B3D algorithm. This routine is assumed to be called during *
! *   back run of solving normal system.                                 *
! *                                                                      *
! *     Routine  B1B3D_COV1  puts into vector  ZI0  estimates of the     *
! *   variances of the adjustments of the local parameters.              *
! *                                                                      *
! *     NB: covariance matrices connected with segmented parameters are  *
! *   not calcuated.                                                     *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      G ( INTEGER*4 ) -- number of global parameters.                 *
! *      L ( INTEGER*4 ) -- number of local parameters at one block.     *
! *      S ( INTEGER*4 ) -- number of segmented parameters at one block  *
! *                         except the last segment.                     *
! *     SX ( INTEGER*4 ) -- number of segmented parameters at the last   *
! *                         segment. Note: SX should be no less them S   *
! *     GA ( INTEGER*4 ) -- (G*(G+1))/2                                  *
! *     LA ( INTEGER*4 ) -- (L*(L+1))/2                                  *
! *     SA ( INTEGER*4 ) -- (S*(S+1))/2                                  *
! *     NS ( INTEGER*4 ) -- number of blocks of segmented parameters in  *
! *                         current group of local parameters.           *
! *    W00 ( REAL*8    ) -- global-global covariance matrix. Dimension:  *
! *                         GA.                                          *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *    WI0 ( REAL*8    ) -- Input: modified local-global block of normal *
! *                         matrix. Output: local-global covariance      *
! *                         matrix. Dimension: L*G                       *
! *    BI0 ( REAL*8    ) -- Input: local-local block of normal matrix.   *
! *                         Output: local-local covariance matrix.       *
! *                         Dimension: LA                                *
! *    WIJ ( REAL*8    ) -- Input: a set of NS segmented-global          *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Output: zeroed matrices.                     *
! *                         Dimension: S*G*NS . Note: the last submatrix *
! *                         has actual dimension SX*G. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array WIJ.                               *
! *    BIJ ( REAL*8    ) -- Input: a set of NS modified segmented-local  *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Output: zeroed matrices.                     *
! *                         Dimension: S*L*NS . Note: the last submatrix *
! *                         has actual dimension SX*L. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array BIJ.                               *
! *    CIJ ( REAL*8    ) -- Input: a set of NS segmented-segmented       *
! *                         submatrices of the blocks of normal matrix   *
! *                         located on the main diagonal. Output:        *
! *                         a set of unity matrices. Dimension: SA*NS    *
! *                         Note: the last submatrix has actual          *
! *                         dimension SX*(SX-1)/2. In the case SX<S some *
! *                         unused place occurs at the bottom of the     *
! *                         array CIJ.                                   *
! *    DIJ ( REAL*8    ) -- Input: a set of NS segmented-segmented       *
! *                         submatrices of the blocks of normal matrix   *
! *                         located down the main diagonal. Output:      *
! *                         zeroed matrices. Dimension: S*S*NS  Note:    *
! *                         the last submatrix has actuual dimension     *
! *                         SX*S. In the case SX<S some unused place     *
! *                         occurs at the bottom of the array DIJ.       *
! *  IUER ( INTEGER*4  ) -- Universal error handler.                     *
! *         Input:  swicth IUER=0 -- no error messages generated even in *
! *                 the case of error. IUER=-1 -- in the case of error   *
! *                 the message will pe put on stdout.                   *
! *         Output: 0 in the case of successfull completion and non-zero *
! *                 in the case of error.                                *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *                                                                      *
! *    ZI0 ( REAL*8    ) -- Vector of variances of the adjustments of    *
! *                         the local parameters. Dimension: L.          *
! *                                                                      *
! * _________________________ WORKING ARRAYS: __________________________ *
! *                                                                      *
! *  WM_GG ( REAL*8    ) -- Working array. Dimension: GA.                *
! *  WM_LG ( REAL*8    ) -- Working array. Dimension: L*G                *
! *  WM_LL ( REAL*8    ) -- Working array. Dimension: LA                 *
! *                                                                      *
! *  ###  20-FEB-97   B1B3D_COV1   v1.2  (c)  L. Petrov 10-JUL-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  G, L, S, SX, NS, IUER
      ADDRESS__TYPE :: GA, LA, SA
      REAL*8     W00(GA), WI0(L,G), BI0(LA), ZI0(L), &
     &           WIJ(S,G,NS), BIJ(S,L,NS), CIJ(SA,NS), DIJ(S,S,NS), &
     &           ZIJ(S,NS)
      REAL*8     WM_GG(GA),  WM_LG(L,G), WM_LL(LA)
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-28 )  ! min acceptable diagonal element
      CHARACTER  STR*20, STR1*20, STR2*20
      INTEGER*4  J1, J2, J3, SC, IER
      INTEGER*4  I, J
      ADDRESS__TYPE :: LOCC
      INTEGER*4, EXTERNAL :: I_LEN
      LOCC(I,J)=MIN(I,J) + (INT8(MAX(I,J))*INT8(MAX(I,J)-1))/2
!
      IF ( SX .GT. S ) THEN
           CALL CLRCH   (     STR  )
           CALL INCH    ( SX, STR  )
           CALL CLRCH   (     STR1 )
           CALL INCH    ( S,  STR1 )
           CALL ERR_LOG ( 8221, IUER, 'B1B3D_COV1', 'Parameter SX is too '// &
     &         'large:  SX='//STR(1:I_LEN(STR))//'  SX > S (S='// &
     &          STR1(1:I_LEN(STR1))//') ' )
           RETURN
      END IF
!
! --- Beginning calculation of Cov(o)
!
! --- WM_LG := - Bio * Wio * Cov(oo,oo)
!
      IER = -2
      CALL MUL_MM_IS_I ( L, G, WI0, G, W00, L, G, WM_LG, IER )
      CALL VEC_MULT_CONSTANT ( WM_LG, L*G, -1.D0, WM_LG )
!
! --- WM_LL:=Bio * Wio(T) * Cov(oo,oo) * Bio * Wio * Bio
!
      IER = -2
      CALL MUL_MM_IT_S ( L, G, WI0, L, G, WM_LG, L, WM_LL, IER )
!
! --- Cov(o,o) := Bio^(-1) - WM_LL
!
      CALL SUB_VV8  ( LA, BI0, WM_LL )
!
! --- Putting to Cov(o,oo)  WI0
!
      CALL COPY_V8  ( INT8(L)*INT8(G), WM_LG, WI0 )
!
! --- Calculating variances of local parameters
!
      DO 410 J1=1,L
         IF ( BI0(LOCC(J1,J1)) .LT. EPS ) THEN
              CALL CLRCH   ( STR2     )
              CALL INCH    ( J1, STR2 )
              CALL CLRCH   ( STR1     )
              WRITE ( UNIT=STR1, FMT='(1PE15.7)' ) BI0(LOCC(J1,J1))
              CALL CHASHL  ( STR1     )
              CALL ERR_LOG ( 8222, IUER, 'B1B3D_COV1', STR2(1:I_LEN(STR2))// &
     &            '-th diagonal element of covariance local-local '// &
     &            'matrix is too small: '//STR1 )
              RETURN
         END IF
         ZI0(J1) = SQRT ( BI0(LOCC(J1,J1)) )
 410  CONTINUE
!
! --- Zeroing covariance matrices of segmented parameters. And Putting 1.0 on
! --- the main diagonal as well as putting 1.0 for varianes of global
! --- parameters
!
      DO 420 J2=1,NS
         SC = S
         IF ( J2 .EQ. NS ) SC = SX
!
         CALL NOUT_R8  ( S*G,  WIJ(1,1,J2) )
         CALL NOUT_R8  ( S*L,  BIJ(1,1,J2) )
         CALL NOUT8_R8 ( SA,   CIJ(1,J2) )
         CALL NOUT_R8  ( S*SC, DIJ(1,1,J2) )
!
         DO 430 J3=1,SC
            CIJ( (LOCC(J3,J3)), J2 ) = 1.D0
            ZIJ(J3,J2)   = 1.D0
 430     CONTINUE
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B1B3D_COV1  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE B1B3D_COV2 ( G, L, S, SX, GA, LA, SA, NS, &
     &           W00, WI0, BI0, ZI0, WIJ, BIJ, CIJ, DIJ, ZIJ, &
     &           WM_GG, WM_LG, WM_LL, &
     &           W1_SG, W2_SG, W3_SL, W4_SL, W5_SSS, W6_SSS, W7_SS, W8_SS, &
     &           IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  B1B3D_COV2  calculates local-global, local-local,       *
! *   segmented-global, segmented-local, segmented(current)-             *
! *   segmented(current), segmented(current)-segmented(previuos)         *
! *   covariance matrices of the parameter estimates produced by solving *
! *   LSQ problem using B1B3D algorithm. All blocks of covariance        *
! *   matrices which corresponds to the non-zero blocks of normal matrix *
! *   are calculated. This routine is assumed to be called during back   *
! *   run of solving normal system.                                      *
! *                                                                      *
! *       Assumed that all blocks of segmented parameters but the last   *
! *   has the same dimension: S. The last block has dimsension SX and    *
! *   SX =< S .                                                          *
! *                                                                      *
! *     Routine  B1B3D_COV2  puts into vector  ZI0  estimates of the     *
! *   variances of the adjustments of the local parameters. It puts in   *
! *   array ZIJ estimates of the variances of the adjustments of the     *
! *   segmented parameters as well.                                      *
! *                                                                      *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      G ( INTEGER*4 ) -- number of global parameters.                 *
! *      L ( INTEGER*4 ) -- number of local parameters at one block.     *
! *      S ( INTEGER*4 ) -- number of segmented parameters at one block  *
! *                         except the last segment.                     *
! *     SX ( INTEGER*4 ) -- number of segmented parameters at the last   *
! *                         segment. Note: SX should be no less them S   *
! *     GA ( INTEGER*4 ) -- (G*(G+1))/2                                  *
! *     LA ( INTEGER*4 ) -- (L*(L+1))/2                                  *
! *     SA ( INTEGER*4 ) -- (S*(S+1))/2                                  *
! *     NS ( INTEGER*4 ) -- number of blocks of segmented parameters in  *
! *                         current group of local parameters.           *
! *    W00 ( REAL*8    ) -- global-global covariance matrix. Dimension:  *
! *                         GA.                                          *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *    WI0 ( REAL*8    ) -- Input: modified local-global block of normal *
! *                         matrix. Output: local-global covariance      *
! *                         matrix. Dimension: L*G                       *
! *    BI0 ( REAL*8    ) -- Input: modified local-local block of normal  *
! *                         matrix. Output: local-local covariance       *
! *                         matrix. Dimension: LA                        *
! *    WIJ ( REAL*8    ) -- Input: a set of NS segmented-global          *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Output: a set of segmented-global covariance *
! *                         matrices. Dimension: S*G*NS . Note: the last *
! *                         submatrix has actual dimension SX*G. In the  *
! *                         case SX<S some unused place occurs at the    *
! *                         bottom of the array WIJ.                     *
! *    BIJ ( REAL*8    ) -- Input: a set of NS modified segmented-local  *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Output: a set of segemented-local covariance *
! *                         matrices. Dimension: S*L*NS . Note: the last *
! *                         submatrix has actual dimension SX*L. In the  *
! *                         case SX<S some unused place occurs at the    *
! *                         bottom of the array BIJ.                     *
! *    CIJ ( REAL*8    ) -- Input: a set of NS modified                  *
! *                         segmented-segmented submatrices of the       *
! *                         blocks of normal matrix located on the main  *
! *                         diagonal. Output: a set segmented(current)-  *
! *                         segmented(current) covariance matrices.      *
! *                         Dimension: SA*NS Note: the last submatrix    *
! *                         has actual dimension SX*(SX-1)/2. In the     *
! *                         case SX<S some unused place occurs at the    *
! *                         bottom of the array CIJ.                     *
! *    DIJ ( REAL*8    ) -- Input: a set of NS modified                  *
! *                         segmented-segmented submatrices of the       *
! *                         blocks of normal matrix located down the     *
! *                         main diagonal. Output: a set of              *
! *                         segmented(current)-segmented(previous)       *
! *                         covariance matrices. Dimension: S*S*NS Note: *
! *                         the last submatrix has actual dimension      *
! *                         SX*S. In the case SX<S some unused place     *
! *                         occurs at the bottom of the array DIJ.       *
! *  IUER ( INTEGER*4  ) -- Universal error handler.                     *
! *         Input:  swicth IUER=0 -- no error messages generated even in *
! *                 the case of error. IUER=-1 -- in the case of error   *
! *                 the message will pe put on stdout.                   *
! *         Output: 0 in the case of successfull completion and non-zero *
! *                 in the case of error.                                *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *                                                                      *
! *    ZI0 ( REAL*8    ) -- Vector of variances of the adjustments of    *
! *                         the local parameters. Dimension: L.          *
! *    ZIJ ( REAL*8    ) -- Vector of variances of the adjustments of    *
! *                         the segmented parameters. Dimension: S*NS.   *
! *                         Note: the last subvector has actual          *
! *                         dimension SX. In the case SX<S some unused   *
! *                         place occurs at the bottom of the array ZIJ. *
! *                                                                      *
! * _________________________ WORKING ARRAYS: __________________________ *
! *                                                                      *
! *  WM_GG  ( REAL*8    ) -- Working array. Dimension: GA                *
! *  WM_LG  ( REAL*8    ) -- Working array. Dimension: L*G               *
! *  WM_LL  ( REAL*8    ) -- Working array. Dimension: LA                *
! *  W1_SG  ( REAL*8    ) -- Working array. Dimension: S*G               *
! *  W2_SG  ( REAL*8    ) -- Working array. Dimension: S*G               *
! *  W3_SL  ( REAL*8    ) -- Working array. Dimension: S*L               *
! *  W4_SL  ( REAL*8    ) -- Working array. Dimension: S*L               *
! *  W5_SSS ( REAL*8    ) -- Working array. Dimension: SA                *
! *  W6_SSS ( REAL*8    ) -- Working array. Dimension: SA                *
! *  W7_SS  ( REAL*8    ) -- Working array. Dimension: S*S               *
! *  W8_SS  ( REAL*8    ) -- Working array. Dimension: S*S               *
! *                                                                      *
! *  ###  11-MAR-97   B1B3D_COV2   v1.4 (c) L. Petrov  10-JUL-2003  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  G, L, S, SX, NS, IER, IUER
      ADDRESS__TYPE :: GA, LA, SA
      REAL*8     W00(GA), WI0(L,G), BI0(LA), ZI0(L), &
     &           WIJ(S,G,NS), BIJ(S,L,NS), CIJ(SA,NS), DIJ(S,S,NS), ZIJ(S,NS)
      REAL*8     WM_GG(GA),  WM_LG(L,G), WM_LL(LA), &
     &           W1_SG(S,G), W2_SG(S,G), &
     &           W3_SL(S,L), W4_SL(S,L), &
     &           W5_SSS(SA), W6_SSS(SA), &
     &           W7_SS(S,S), W8_SS(S,S)
      REAL*8     EPS
      PARAMETER  ( EPS = 1.D-28 )  ! min acceptable diagonal element
      INTEGER*4  SP, SC, SN
      ADDRESS__TYPE :: SPA, SCA, SNA
      INTEGER*4  J1, J2, J3, J4
      CHARACTER  STR*20, STR1*20, STR2*20, STR3*20
      INTEGER*4  I, J
      ADDRESS__TYPE :: LOCC
      INTEGER*4, EXTERNAL :: I_LEN
      LOCC(I,J)=MIN(I,J) + (INT8(MAX(I,J))*INT8(MAX(I,J)-1))/2
!
      IF ( SX .GT. S ) THEN
           CALL CLRCH   (     STR  )
           CALL INCH    ( SX, STR  )
           CALL CLRCH   (     STR1 )
           CALL INCH    ( S,  STR1 )
           CALL ERR_LOG ( 8231, IUER, 'B1B3D_COV2', 'Parameter SX is too '// &
     &         'large:  SX='//STR(1:I_LEN(STR))//'  SX > S (S='// &
     &          STR1(1:I_LEN(STR1))//') ' )
           RETURN
      END IF
!
! --- Calculation covariance matrices of local parameters
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! --- Calculation Cov(io,oo)
!
! --- WM_LG := - Bio * Wio * Cov(oo,oo)
!
      IER = -1
      CALL MUL_MM_IS_I ( L, G, WI0, G, W00, L, G, WM_LG, IER )
      CALL VEC_MULT_CONSTANT ( WM_LG, L*G, -1.D0, WM_LG )
!
! --- Calculation Cov(io,io)
!
! --- WM_LL: = - Wio * Cov(oo,oo) * Wio(T)
!
      IER = -1
      CALL MUL_MM_IT_S ( L, G, WI0, L, G, WM_LG, L, WM_LL, IER )
!
! --- Cov(io,io) := Bio^(-1) - WM_LL
!
      CALL SUB_VV8  ( LA, BI0, WM_LL )
!
      CALL COPY_V8  ( INT8(L)*INT8(G), WM_LG, WI0 )   !   WM_LG ==> WI0
!
! --- Calculation covariance matrices of segmented parameters
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      DO 410 J1=1,NS
         SP  =  S
         SPA = (S*(S+1))/2
         IF ( J1 .EQ. NS ) THEN
              SC =  SX
              SCA= (SX*(SX+1))/2
           ELSE
              SC =  S
              SCA= (S*(S+1))/2
         END IF
         IF ( J1+1 .EQ. NS ) THEN
              SN  =  SX
              SNA = (SX*(SX+1))/2
           ELSE
              SN =  S
              SNA= (S*(S+1))/2
         END IF
!
! ------ Calculation Cov(ij,oo)
!        ~~~~~~~~~~~~~~~~~~~~~~
!
! ------ W1 = Wij * Cov(oo,oo)
!
         IER = -1
         CALL MUL_MM_IS_I ( SC, G, WIJ(1,1,J1), G, W00, SC, G, W1_SG, IER )
!
! ------ W2 = Bij * Cov(io,oo)
!
         IER = -1
         CALL MUL_MM_II_I ( SC, L, BIJ(1,1,J1), L, G, WI0, SC, G, W2_SG, IER )
!
! ------ W1 := Wij * Cov(oo,oo) + Bij * Cov(io,oo)
!
         CALL ADD_VV ( SC*G, W1_SG, W2_SG )
         IF ( J1 .GT. 1 ) THEN
!
! --------- W2 = Dij * Cov(ij-1,oo)
!
            IER = -1
            CALL MUL_MM_II_I ( SC, SP, DIJ(1,1,J1), &
     &                         SP, G,  WIJ(1,1,J1-1), SC, G, W2_SG, IER )
!
! --------- W1 := Wij * Cov(oo,oo) + Bij * Cov(io,oo) + Dij * Cov(ij-1,oo)
!
            CALL ADD_VV ( SC*G, W1_SG, W2_SG )
         END IF
!
! ------ W1 := Cov(ij,io)
!
         CALL VEC_MULT_CONSTANT ( W1_SG, SC*G, -1.D0, W1_SG )
!
! ------ Calculation Cov(ij,io)
!        ~~~~~~~~~~~~~~~~~~~~~~
!
! ------ W3 = Wij * Cov(T)(io,oo)
!
         IER = -1
         CALL MUL_MM_IT_I ( SC, G, WIJ(1,1,J1), L, G, WI0, SC, L, W3_SL, IER )
!
! ------ W4 = Bij * Cov(io,io)
!
         IER = -1
         CALL MUL_MM_IS_I ( SC, L, BIJ(1,1,J1), L, BI0, SC, L, W4_SL, IER )
!
! ------ W3 := Wij * Cov(T)(io,oo) + Bij * Cov(io,io)
!
         CALL ADD_VV ( SC*L, W3_SL, W4_SL )
         IF ( J1 .GT. 1 ) THEN
!
! --------- W4 = Dij * Cov(ij-1,io)
!
            IER = -1
            CALL MUL_MM_II_I ( SC, SP, DIJ(1,1,J1), &
     &                         SP, L,  BIJ(1,1,J1-1), SC, L, W4_SL, IER )
!
! --------- W3 := Wij * Cov(T)(io,oo) + Bij * Cov(io,io) + Dij * Cov(ij-1,io)
!
            CALL ADD_VV ( SC*L, W3_SL, W4_SL )
         END IF
!
! ------ W3 := Cov(ij,io)
!
         CALL VEC_MULT_CONSTANT ( W3_SL, SC*L, -1.D0, W3_SL )
!
! ------ Calculation Cov(ij,ij)
!        ~~~~~~~~~~~~~~~~~~~~~~
!
! ------ W5 = Wij * Cov(T)(ij,oo)
!
         IER = -1
         CALL MUL_MM_IT_S ( SC, G, WIJ(1,1,J1), SC, G, W1_SG, SC, W5_SSS, IER )
!
! ------ W5 := Wij * Cov(T)(ij,oo) - C(-1)(ij)
!
         CALL SUB_VV8 ( SCA, W5_SSS, CIJ(1,J1) )
!
! ------ W6 = Bij * Cov(T)(ij,io)
!
         IER = -1
         CALL MUL_MM_IT_S ( SC, L, BIJ(1,1,J1), SC, L, W3_SL, SC, W6_SSS, IER )
!
! ------ W5 := -C(-1)(ij) + Wij * Cov(T)(ij,oo) + Bij * Cov(ij,io)
!
         CALL ADD_VV ( SCA, W5_SSS, W6_SSS )
         IF ( J1 .GT. 1 ) THEN
!
! --------- W6 = Dij * Cov(T)(ij,ij-1)
!
            IER = -1
            CALL MUL_MM_IT_S ( SC, SP, DIJ(1,1,J1), SC, SP, W7_SS, SC, &
     &                         W6_SSS, IER )
!
! --------- W5 := -C(-1)(ij) + Wij * Cov(T)(ij,oo) + Bij * Cov(T)(ij,io) +
! ---------                    Dij * Cov(T)(ij,ij-1)
!
            CALL ADD_VV ( SCA, W5_SSS, W6_SSS )
            CALL COPY_V ( SC*SP, W7_SS, DIJ(1,1,J1) ) ! W7_SS ==> DIJ(1,1,J1)
         END IF
!
! ------ W5 := Cov(ij,ij)
!
         CALL VEC_MULT_CONSTANT ( W5_SSS, SCA, -1.D0, W5_SSS )
         IF ( J1 .LT. NS ) THEN
!
! --------- Calculation Cov(ij+1,ij)
!           ~~~~~~~~~~~~~~~~~~~~~~
!
! --------- W7 = Wij+1 * Cov(T)(ij,oo)
!
            IER = -1
            CALL MUL_MM_IT_I ( SN, G, WIJ(1,1,J1+1), SC, G, W1_SG, &
     &                         SN, SC, W7_SS, IER )
!
! --------- W8 = Bij+1 * Cov(ij,io)
!
            IER = -1
            CALL MUL_MM_IT_I ( SN, L, BIJ(1,1,J1+1), SC, L, W3_SL, &
     &                         SN, SC, W8_SS, IER )
!
! --------- W7 := Wij+1 * Cov(T)(ij,oo) + Bij+1 * Cov(ij,io)
!
            CALL ADD_VV ( SN*SC, W7_SS, W8_SS )
!
! --------- W8 = Dij+1 * Cov(ij,ij)
!
            IER = -1
            CALL MUL_MM_IS_I ( SN, SC, DIJ(1,1,J1+1), SC, W5_SSS, &
     &                         SN, SC, W8_SS, IER )
!
! --------- W7 := Wij+1 *Cov(T)(ij,oo) + Bij+1 *Cov(ij,io) + Dij+1 *Cov(ij,ij)
!
            CALL ADD_VV ( SN*SC, W7_SS, W8_SS )
!
! --------- W7 := Cov(ij+1,ij)
!
            CALL VEC_MULT_CONSTANT ( W7_SS, SN*SC, -1.D0, W7_SS )
         END IF
!
         CALL COPY_V  ( SC*G, W1_SG,  WIJ(1,1,J1) )  !  W1_SG  ==> WIJ(1,1,J1)
         CALL COPY_V  ( SC*L, W3_SL,  BIJ(1,1,J1) )  !  W3_SL  ==> BIJ(1,1,J1)
         CALL COPY_V8 ( SCA,  W5_SSS, CIJ(1,J1)   )  !  W5_SSS ==> CIJ(1,1,J1)
 410  CONTINUE
!
!
! --- Calculating variances of local parameters
!
      DO 420 J2=1,L
         IF ( BI0(LOCC(J2,J2)) .LT. EPS ) THEN
              CALL CLRCH   ( STR2     )
              CALL INCH    ( J2, STR2 )
              CALL CLRCH   ( STR1     )
              WRITE ( UNIT=STR1, FMT='(1PE15.7)' ) BI0(LOCC(J2,J2))
              CALL CHASHL  ( STR1     )
              CALL ERR_LOG ( 8232, IUER, 'B1B3D_COV2', STR2(1:I_LEN(STR2))// &
     &            '-th diagonal element of covariance local-local '// &
     &            'matrix is too small: '//STR1 )
              RETURN
         END IF
         ZI0(J2) = SQRT ( BI0(LOCC(J2,J2)) )
 420  CONTINUE
!
! --- Calcilating variances of segmented parameters
!
      DO 430 J3=1,NS
         SC = S
         IF ( J3 .EQ. NS ) SC = SX
         DO 440 J4=1,SC
            IF ( CIJ( (LOCC(J4,J4)), J3) .LT. EPS ) THEN
                 CALL CLRCH   ( STR3     )
                 CALL INCH    ( J3, STR3 )
                 CALL CLRCH   ( STR2     )
                 CALL INCH    ( J4, STR2 )
                 CALL CLRCH   ( STR1     )
                 WRITE ( UNIT=STR1, FMT='(1PE15.7)' ) CIJ( (LOCC(J4,J4)), J3)
                 CALL CHASHL  ( STR1     )
                 CALL ERR_LOG ( 8233, IUER, 'B1B3D_COV1', STR2(1:I_LEN(STR2))// &
     &               '-th diagonal element of '//STR3(1:I_LEN(STR3))// &
     &               '-th block of covariance segmenteed-segmented '// &
     &               'matrix is too small: '//STR1 )
                 RETURN
            END IF
            ZIJ(J4,J3) = SQRT ( CIJ( (LOCC(J4,J4)), J3) )
 440     CONTINUE
 430  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B1B3D_COV2  #!#
