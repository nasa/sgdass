      SUBROUTINE B1B3D_FRW ( G, L, S, SX, GA, LA, SA, NS, &
     &           W00, WI0, WIJ, BI0, BIJ, CIJ, DIJ, Z00, ZI0, ZIJ, &
     &           WM_GG, WM_LG, WM_LL, WM_SG1, WM_SG2, WM_SL1, &
     &           WM_SL2, WM_SSF, WM_SSS, WV_G, WV_L, WV_S, RCOND, IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  B1B3D_FRW  makes caculation for forward run of the LSQ  *
! *   solution of the normal system for the new block of the parameters  *
! *   realted with the next group of the local data using B1B3D          *
! *   algorithm. For VLBI solution B1B3D_FRW should be called for the    *
! *   each new arc. It is assumed that the whole normal matrix is the    *
! *   square, symmetric, bordered block-diagonal and each diagonal block *
! *   is in turn a symmetric, square, bordered block-threediagonal       *
! *   matrix. As input parameters a set of submatrices which forms       *
! *   a normal matrix and a set of subvectors of right parts is supplied.*
! *                                                                      *
! *     B1B3D_FRW "updates" these submatrices and subvectors.            *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      G ( INTEGER*4 ) -- number of global parameters.                 *
! *      L ( INTEGER*4 ) -- number of local parameters at one block.     *
! *      S ( INTEGER*4 ) -- number of segmented parameters at one block  *
! *                         except the last segment.                     *
! *     SX ( INTEGER*4 ) -- number of segmented parameters at the last   *
! *                         segment. Note: SX should be no less them S   *
! *     GA ( INTEGER*8 ) -- (G*(G+1))/2                                  *
! *     LA ( INTEGER*8 ) -- (L*(L+1))/2                                  *
! *     SA ( INTEGER*8 ) -- (S*(S+1))/2                                  *
! *     NS ( INTEGER*4 ) -- number of blocks of segmented parameters in  *
! *                         current group of local parameters.           *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *    W00 ( REAL*8    ) -- Input: contribution to global-global block   *
! *                         of normal matrix. Dimension: GA              *
! *    WI0 ( REAL*8    ) -- Input: local-global block of normal matrix.  *
! *                         Output: BI0^(-1) * WI0. Dimension: L*G       *
! *    WIJ ( REAL*8    ) -- Input: a set of NS segmented-global          *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Output: CIJ^(-1) * WIJ.                      *
! *                         Dimension: S*G*NS . Note: the last submatrix *
! *                         has actual dimension SX*G. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array WIJ.                               *
! *    BI0 ( REAL*8    ) -- Input: local-local block of normal matrix    *
! *                         Output: BIJ^(-1). Dimension: LA              *
! *    BIJ ( REAL*8    ) -- Input: a set of NS modified segmented-local  *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Output: CIJ^(-1) * BIJ.                      *
! *                         Dimension: S*L*NS . Note: the last submatrix *
! *                         has actual dimension SX*L. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array BIJ.                               *
! *    CIJ ( REAL*8    ) -- Input: a set of NS segmented-segmented       *
! *                         submatrices of the blocks of normal matrix   *
! *                         located on the main diagonal. Output:        *
! *                         CIJ^(-1). Dimension: SA*NS  Note: the last   *
! *                         submatrix has actual dimension SX*(SX-1)/2.  *
! *                         In the case SX<S some unused place occurs at *
! *                         the bottom of the array CIJ.                 *
! *    DIJ ( REAL*8    ) -- Input: a set of NS segmented-segmented       *
! *                         submatrices of the blocks of normal matrix   *
! *                         located down the main diagonal. Output:      *
! *                         CIJ^(-1)*DIJ . Dimension: S*S*NS  Note:      *
! *                         the last submatrix has actuual dimension     *
! *                         SX*S. In the case SX<S some unused place     *
! *                         occurs at the bottom of the array DIJ.       *
! *    Z00 ( REAL*8    ) -- Input: contribution of the current group of  *
! *                         local   parameters to the global-global      *
! *                         block of normal vector. Dimension: G         *
! *    ZI0 ( REAL*8    ) -- Input: local block of normal vector.         *
! *                         Dimension: L                                 *
! *    ZIJ ( REAL*8    ) -- Input: a set of NS segmented-global          *
! *                         subvectors of the blocks of normal vector.   *
! *                         Dimension: S*NS . Note: the last submatrix   *
! *                         has actual dimension S. In the case SX<S     *
! *                         some unused place occurs at the bottom of    *
! *                         the array ZIJ.                               *
! *  IUER ( INTEGER*4  ) -- Universal error handler.                     *
! *         Input:  swicth IUER=0 -- no error messages generated even in *
! *                 the case of error. IUER=-1 -- in the case of error   *
! *                 the message will pe put on stdout.                   *
! *         Output: 0 in the case of successfull completion and non-zero *
! *                 in the case of error.                                *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *    RCOND ( REAL*8    ) -- condition number for the local-local       *
! *                           normal matrix.                             *
! *                                                                      *
! * _________________________ WORKING ARRAYS: __________________________ *
! *                                                                      *
! *  WM_GG ( REAL*8    ) -- Working array. Dimension: GA.                *
! *  WM_LG ( REAL*8    ) -- Working array. Dimension: L*G                *
! *  WM_LL ( REAL*8    ) -- Working array. Dimension: LA                 *
! * WM_SG1 ( REAL*8    ) -- Working array. Dimension: S*G                *
! * WM_SG2 ( REAL*8    ) -- Working array. Dimension: S*G                *
! * WM_SL1 ( REAL*8    ) -- Working array. Dimension: S*L                *
! * WM_SL2 ( REAL*8    ) -- Working array. Dimension: S*L                *
! * WM_SSF ( REAL*8    ) -- Working array. Dimension: S*S                *
! * WM_SSS ( REAL*8    ) -- Working array. Dimension: SA                 *
! *   WV_G ( REAL*8    ) -- Working array. Dimension: G                  *
! *   WV_L ( REAL*8    ) -- Working array. Dimension: L                  *
! *   WV_S ( REAL*8    ) -- Working array. Dimension: S                  *
! *                                                                      *
! *  ###  20-FEB-97   B1B3D_FRW    v1.0  (c)  L. Petrov  24-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  G, L, S, SX, NS, IUER
      ADDRESS__TYPE :: GA, LA, SA
      REAL*8     W00(GA), WI0(L,G), WIJ(S,G,NS), BI0(LA), &
     &           BIJ(S,L,NS), CIJ(SA,NS), DIJ(S,S,NS), &
     &           Z00(G), ZI0(L), ZIJ(S,NS)
      REAL*8     WM_GG(GA),  WM_LG(L,G), &
     &           WM_LL(LA),  WM_SG1(S,G), WM_SG2(S,G), &
     &                       WM_SL1(S,L), WM_SL2(S,L), WM_SSF(S,S), &
     &           WM_SSS(SA), WV_G(G),     WV_L(L),     WV_S(S)
      REAL*8     RCOND
      INTEGER*4  J1, k1, k2
      INTEGER*4  SP, SC, IER, I_LEN
      ADDRESS__TYPE :: SXA, SPA
      CHARACTER STR*20, STR1*20
!
      SXA = (SX*(SX+1))/2
!
      IF ( SX .GT. S ) THEN
           CALL CLRCH   (     STR  )
           CALL INCH    ( SX, STR  )
           CALL CLRCH   (     STR1 )
           CALL INCH    ( S,  STR1 )
           CALL ERR_LOG ( 8241, IUER, 'B1B3D_FRW', 'Parameter SX is too '// &
     &         'large:  SX='//STR(1:I_LEN(STR))//'  SX > S (S='// &
     &          STR1(1:I_LEN(STR1))//') ' )
           RETURN
      END IF
!
! --- II.Elimination influence of segmented parameters on local and global ones
!
      DO 410 J1=NS,1,-1  !  (in backward direction)
         IF ( J1 .EQ. NS ) THEN
              SP  = S
              SPA = SA
              SC  = SX
            ELSE IF ( J1 .LT. NS ) THEN
              SP  = S
              SPA = SA
              SC  = S
         END IF
!
! ------ 2.0 Inversion of the martix of segmented-segmented parameters
!
! ------ Cij(j1) := Cij(j1)^(-1)
!
         CALL ERR_PASS ( IUER, IER )
         CALL INVS     ( SC, CIJ(1,J1), RCOND, IER )
         IF ( IER.NE.0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              WRITE ( UNIT=STR1(1:12), FMT='(1PD12.4)' ) RCOND
              CALL ERR_LOG ( 8242, IUER, 'B1B3D_FRW', 'Error during '// &
     &            'inversion '//STR(1:I_LEN(STR))//'-th matrix of '// &
     &            'segmented-segmented parameters. Conditional number ='// &
     &             STR1 )
              RETURN
         END IF
!
! ------ 2.1 Excluding block Wij(T)(j1)
!           ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ------ WM_SG1 := Cij^(-1)(j1) * Wij(j1)
!
         CALL MUL_MM_SI_I ( SC, CIJ(1,J1), SC, G, WIJ(1,1,J1), &
     &                      SC, G, WM_SG1, -3 )
!
! ------ WV_G := Wij(T)(j1) * Cij^(-1)(j1) * Zij(j1)
!
         CALL MUL_MV_TV_V ( SC, G, WM_SG1, SC, ZIJ(1,J1), G, WV_G, -3 )
!
! ------ Update vector of right parts for global parameters
! ------ Zoo = Zoo - Wij(T)(j1) * Cij^(-1)(j1) * Zij(j1)
!
         CALL SUB_VV ( G, Z00, WV_G )
!
! ------ WM_GG := Wij(T)(j1) * Cij^(-1)(j1) * Wij(j1)
!
         CALL MUL_MM_TI_S ( SC, G, WIJ(1,1,J1), SC, G, WM_SG1, &
     &                      G, WM_GG, -3 )
!
! ------ Update matrix of global-global parameters
! ------ Woo := Woo - Wij(T)(j1) * Cij^(-1)(j1) * Wij(j1)
!
         CALL SUB_VV8 ( GA, W00, WM_GG )
!
! ------ 2.2 Excluding block Bij(T)(j1)
!        ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! ------ WM_SL1 := Cij^(-1)(j1) * Bij(j1)
!
         CALL MUL_MM_SI_I ( SC, CIJ(1,J1), SC, L, BIJ(1,1,J1), &
     &                      SC, L, WM_SL1, -3 )
!
! ------ WV_L := Bij(T)(j1) * Cij^(-1)(j1) * Zij(j1)
!
         CALL MUL_MV_TV_V ( SC, L, WM_SL1, SC, ZIJ(1,J1), L, WV_L, -3 )
!
! ------ Update vector of right parts for local parameters
! ------ Zio(j1) = Zio(j1) - Bij(T)(j1) * Cij^(-1)(j1) * Zij(j1)
!
         CALL SUB_VV ( L, ZI0, WV_L )
!
! ------ WM_LL := Bij(T)(j1) * Cij^(-1)(j1) * Bij(j1)
!
         CALL MUL_MM_TI_S ( SC, L, BIJ(1,1,J1), SC, L, WM_SL1, L, WM_LL, -3 )
!
! ------ Update matrix of local-local parameters
! ------ Bio := Bio - Bij(T)(j1) * Cij^(-1)(j1) * Bij(j1)
!
         CALL SUB_VV8 ( LA, BI0, WM_LL )
!
! ------ WM_LG := Bij(T)(j1) * Cij^(-1)(j1) * W(j1)
!
         CALL MUL_MM_TI_I ( SC, L, WM_SL1, SC, G, WIJ(1,1,J1), &
     &                      L, G, WM_LG, -3 )
!
! ------ Update matrix of local-global parameters
! ------ Wio := Wio - Bij(T)(j1) * Cij^(-1)(j1) * Wij(j1)
!
         CALL SUB_VV8 ( INT8(L)*INT8(G), WI0, WM_LG )
!
         IF ( J1 .NE. 1 ) THEN
!
! ----------- WM_SG2 := Dij(T)(j1) * Cij^(-1)(j1) * Wij(j1)
!
              CALL MUL_MM_TI_I ( SC, SP, DIJ(1,1,J1), SC, G, WM_SG1, &
     &                           SP, G, WM_SG2, -3 )
!
! ----------- 2.3.1 Updating previous block of global-segmented parameters
! ----------- Wij(j1-1) := Wij(j1-1) - Dij(T)(j1)*Cij^(-1)(j1)*Wij(j1)
!
              CALL SUB_VV8 ( INT8(SP)*INT8(G), WIJ(1,1,J1-1), WM_SG2 )
!
! ----------- WM_SL2 := Dij(T)(j1) * Cij^(-1)(j1) * Bij(j1)
!
              CALL MUL_MM_TI_I ( SC, SP, DIJ(1,1,J1), SC, L, WM_SL1, &
     &                           SP, L, WM_SL2, -3 )
!
! ----------- 2.3.2 Updating previous block of local-segmented parameters
! ----------- Bij(j1-1) := Bij(j1-1) - Dij(T)(j1)*Cij^(-1)(j1)*Bij(j1)
!
              CALL SUB_VV ( SP*L, BIJ(1,1,J1-1), WM_SL2 )
!
! ----------- WM_LLS := Dij(T)(j1) * Cij^(-1)(j1) * Dij(j1)
! ----------- Calculation are being done in two steps
!
              CALL MUL_MM_SI_I ( SC, CIJ(1,J1), SC, SP, DIJ(1,1,J1), &
     &                                          SC, SP, WM_SSF, -3 )
              CALL MUL_MM_TI_S ( SC, SP, DIJ(1,1,J1), SC, SP, WM_SSF, &
     &                                                    SP, WM_SSS, -3 )
!
! ----------- 2.4 Updating previous down-diagonal block of segment-segmented
! ----------- parameters
! ----------- Cij(j1-1) := Cij(j1-1) - Dij(T)(j1)*Cij^(-1)(j1)*Dij(j1)
!
              CALL SUB_VV8 ( SPA, CIJ(1,J1-1), WM_SSS )
!
! ----------- Updating previous vector of rights parts for segmented parameters
! ----------- WV_L := Dij(T)(j1) * Cij^(-1)(j1) * Zij(j1)
!
              CALL MUL_MV_TV_V ( SC, SP, WM_SSF, SC, ZIJ(1,J1), SP, WV_S, -3 )
!
! ----------- Zij(j1-1) := Zij(j1-1) - Dij(T)(j1)*Cij^(-1)(j1)*Zij(j1)
!
              CALL SUB_VV  ( SP, ZIJ(1,J1-1), WV_S )
!
! ----------- Substitution Dij(j1) := Cij^(-1)(j1) * D(j1)
!
              CALL COPY_V  ( SC*SP, WM_SSF, DIJ(1,1,J1) )
         END IF
!
! ------ Substitution Bij(j1) := Cij^(-1)(j1) * Bij(j1)
!
         CALL COPY_V  ( SC*L, WM_SL1, BIJ(1,1,J1) )
!
! ------ Substitution Wij(j1) := Cij^(-1)(j1) * Wij(j1)
!
         CALL COPY_V  ( SC*G, WM_SG1, WIJ(1,1,J1) )
 410  CONTINUE
!
! --- Elimination influence of local parameters on global one
!
! --- 2.4.1 Inversion martix of local-local parameters
!
! --- Bio := Bio^(-1)
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( L, BI0, RCOND, IER )
      IF ( IER.NE.0 ) THEN
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR1(1:12), FMT='(1PD12.4)' ) RCOND
           CALL ERR_LOG ( 8203, IUER, 'B1B3D_SOL', 'Error during '// &
     &         'inversion matrix of local-local parameters, '// &
     &         ' Conditional number ='//STR1 )
           RETURN
      END IF
!
! --- 2.4.2 Excluding block Wio(T)
!
! --- WM_LG := Bio^(-1) * Wio
!
      CALL MUL_MM_SI_I ( L, BI0, L, G, WI0, L, G, WM_LG, -3 )
!
! --- WV_G := Wio(T) * Bio^(-1) * Zio
!
      CALL MUL_MV_TV_V ( L, G, WM_LG, L, ZI0, G, WV_G, -3 )
!
! --- Update vector of right parts for global parameters
! --- Zoo := Zoo - Wio(T) * Bio^(-1) * Zi0
!
      CALL SUB_VV ( G, Z00, WV_G )
!
! --- WM_GG := Wio(T) * Bio^(-1) * Wio
!
      CALL MUL_MM_TI_S ( L, G, WI0, L, G, WM_LG, G, WM_GG, -3 )
!
! --- Update matrix of global-global parameters
! --- Woo := Woo - Wio(T) * Bio^(-1) * Wio
!
      CALL SUB_VV8 ( GA, W00, WM_GG )
!
! --- Substitution Wio := Bio^(-1) * Wio
!
      CALL COPY_V8  ( INT8(L)*INT8(G), WM_LG, WI0 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B1B3D_FRW  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE B1B3D_BCK ( G, L, S, SX, GA, LA, SA, NS, &
     &           W00, WI0, WIJ, BI0, BIJ, CIJ, DIJ, ZI0, ZIJ, &
     &           E00, EI0, EIJ, WV_G, WV_L, WV_S )
! ************************************************************************
! *                                                                      *
! *     Routine  B1B3D_BCK  makes caculation for the back run of the LSQ *
! *   solution of the normal system for the new block of the parameters  *
! *   realted with the next group of the local data using B1B3D          *
! *   algorithm. For VLBI solution B1B3D_BCK should be called for each   *
! *   arc. It is assumed that the whole normal matrix was the square,    *
! *   symmetric, bordered block-diagonal and each diagonal block is in   *
! *   turn a symmetric, square, bordered block-threediagonal matrix.     *
! *   As input parameters a set of modified submatrices and subvectors   *
! *   which formed a normal matrix and normal vectors and further were   *
! *   modified by B1B3D_FRW is supplied.                                 *
! *                                                                      *
! *     B1B3D_BCK produces the adjustments for the local parameters and  *
! *   for the segmented parameters related to these group of the local   *
! *   data.                                                              *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      G ( INTEGER*4 ) -- number of global parameters.                 *
! *      L ( INTEGER*4 ) -- number of local parameters at one block.     *
! *      S ( INTEGER*4 ) -- number of segmented parameters at one block  *
! *                         except the last segment.                     *
! *     SX ( INTEGER*4 ) -- number of segmented parameters at the last   *
! *                         segment. Note: SX should be no less them S   *
! *     GA ( INTEGER*8 ) -- (G*(G+1))/2                                  *
! *     LA ( INTEGER*8 ) -- (L*(L+1))/2                                  *
! *     SA ( INTEGER*8 ) -- (S*(S+1))/2                                  *
! *     NS ( INTEGER*4 ) -- number of blocks of segmented parameters in  *
! *                         current group of local parameters.           *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *    W00 ( REAL*8    ) -- covariance matrix of global-global           *
! *                         parameters. Dimension: GA                    *
! *    WI0 ( REAL*8    ) -- modified local-global block of normal matrix.*
! *                         Dimension: L*G                               *
! *    WIJ ( REAL*8    ) -- a set of NS modified segmented-global        *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Dimension: S*G*NS . Note: the last submatrix *
! *                         has actual dimension SX*G. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array WIJ.                               *
! *    BI0 ( REAL*8    ) -- modifeid local-local block of normal matrix  *
! *                         Dimension: LA                                *
! *    BIJ ( REAL*8    ) -- a set of NS modified segmented-local         *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Dimension: S*L*NS . Note: the last submatrix *
! *                         has actual dimension SX*L. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array BIJ.                               *
! *    CIJ ( REAL*8    ) -- a set of NS modified segmented-segmented     *
! *                         submatrices of the blocks of normal matrix   *
! *                         located on the main diagonal. Dimension:     *
! *                         SA*NS  Note: the last submatrix has actual   *
! *                         dimension SX*(SX-1)/2. In the case SX<S some *
! *                         unused place occurs at the bottom of the     *
! *                         array CIJ.                                   *
! *    DIJ ( REAL*8    ) -- a set of NS modified segmented-segmented     *
! *                         submatrices of the blocks of normal matrix   *
! *                         located down the main diagonal. Dimension:   *
! *                         S*S*NS  Note: the last submatrix has actual  *
! *                         dimension SX*S. In the case SX<S some unused *
! *                         place occurs at the bottom of the array DIJ. *
! *    ZI0 ( REAL*8    ) -- modified localblock of normal vector.        *
! *                         Dimension: L                                 *
! *    ZIJ ( REAL*8    ) -- a set of NS mododied segmented-global        *
! *                         subvectors of the blocks of normal vector.   *
! *                         Dimension: S*NS . Note: the last submatrix   *
! *                         has actual dimension S. In the case SX<S     *
! *                         some unused place occurs at the bottom of    *
! *                         the array ZIJ.                               *
! *    E00 ( REAL*8    ) -- Estimates of hte global parameters.          *
! *                         Dimension: G                                 *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *    EI0 ( REAL*8    ) -- Estimates of the local parameters.           *
! *                         Dimension: L                                 *
! *    EIJ ( REAL*8    ) -- a set of sub NS vectors of the estimates of  *
! *                         segmented parameters. Dimension: S*NS .      *
! *                         Note: the last subvector has actual          *
! *                         dimension S. In the case SX<S unused place   *
! *                         occurs at the bottom of the array ZIJ.       *
! *                                                                      *
! * _________________________ WORKING ARRAYS: __________________________ *
! *                                                                      *
! *   WV_G ( REAL*8    ) -- Working array. Dimension: G                  *
! *   WV_L ( REAL*8    ) -- Working array. Dimension: L                  *
! *   WV_S ( REAL*8    ) -- Working array. Dimension: S                  *
! *                                                                      *
! *  ###  20-FEB-1997  B1B3D_BCK   v1.1  (c)  L. Petrov 07-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  G, L, S, SX, NS, IUER
      ADDRESS__TYPE :: GA, LA, SA
      REAL*8     W00(GA), WI0(L,G), WIJ(S,G,NS), BI0(LA), &
     &           BIJ(S,L,NS), CIJ(SA,NS), DIJ(S,S,NS), &
     &           ZI0(L), ZIJ(S,NS), &
     &           E00(G), EI0(L), EIJ(S,NS)
      REAL*8     WV_G(G), WV_L(L), WV_S(S)
!
      INTEGER*4  J1, IER
      INTEGER*4  SP, SC
      ADDRESS__TYPE :: SPA
!
! --- IV. Determination local and segmented parameters
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! --- 4.1 Calculation preliminary estimates of the local parameters
! --- Eio := Bio^(-1) * Zio
!
      CALL MUL_MV_SV_V ( L, BI0, L, ZI0, L, EI0, -3 )
!
! --- Calculate correction for presence global parameters
! --- WV_L := Bio^(-1) * Wio * Eoo
!
      CALL MUL_MV_IV_V ( L, G, WI0, G, E00, L, WV_L, IER )
!
! --- Update estimates due to this correstion
! --- Eio := Eio - Bio^(-1) * Wio * Eoo
!
      CALL SUB_VV  ( L, EI0, WV_L )
!
! --- 4.2 Calculation estimates of the segmented parameteres
!
      DO 410 J1=1,NS
         IF ( J1 .EQ. NS ) THEN
              SP  = S
              SPA = SA
              SC  = SX
            ELSE IF ( J1 .LT. NS ) THEN
              SP  = S
              SPA = SA
              SC  = S
         END IF
!
! ------ Calculation prelimnary estimates of the segmeneted parameters
! ------ Eij(j1) := Cij^(-1)(j1) * Zij(j1)
!
         CALL MUL_MV_SV_V ( SC, CIJ(1,J1), SC, ZIJ(1,J1), SC, EIJ(1,J1), -3 )
!
! ------ Calculate correction for the presence of global parameters
! ------ WV_S := Cij^(-1)(j1) * Wij(j1) * Eoo
!
         CALL MUL_MV_IV_V ( SC, G, WIJ(1,1,J1), G, E00, SC, WV_S, IER )
!
! ------ Update estimates due to this correstion
! ------ Eij(j1) := Eij(j1) - Cij^(-1)(j1) * Wij(j1) * Eoo
!
         CALL SUB_VV  ( SC, EIJ(1,J1), WV_S )
!
! ------ Calculate correction for the presence of local parameters
! ------ WV_S := Cij^(-1)(j1) * Bij(j1) * Eio
!
         CALL MUL_MV_IV_V ( SC, L, BIJ(1,1,J1), L, EI0, SC, WV_S, IER )
!
! ------ Update estimates due to this correstion
! ------ Eij(j1) := Eij(j1) - Cij^(-1)(j1) * Bij(j1) * Eio
!
         CALL SUB_VV  ( SC, EIJ(1,J1), WV_S )
!C
         IF ( J1.NE.1 ) THEN
!
! ----------- Calculate correction for the presence of segmented parameters
! ----------- of the previous block.
! ----------- WV_S := Cij^(-1)(j1) * Dij(j1) * Eij(j1-1)
!
              CALL MUL_MV_IV_V ( SC, SP, DIJ(1,1,J1), SP, &
     &                                   EIJ(1,J1-1), SC, WV_S, IER )
!
! ----------- Update estimates due to this correstion
! ----------- Eij(j1) := Eij(j1) - Cij^(-1)(j1) * Dij(j1) * Eio(j1-1)
!
              CALL SUB_VV  ( SC, EIJ(1,J1), WV_S )
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B1B3D_BCK  #!#
