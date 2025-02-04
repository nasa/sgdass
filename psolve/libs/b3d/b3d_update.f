      SUBROUTINE B3D_UPDATE ( IP, ISCL, N, G, L, LX, LA, LXA, NK, N_OUT, &
     &           A0, A1, A2, Y, WEI, S0, SL, SLX, &
     &           B0, B, C, D, BX, CX, DX, CVM, &
     &           E0, EL, ELX, &
     &           Q0, QL, QLX, VG0, VL1, VL2, &
     &           IUER )
! ************************************************************************
! *                                                                      *
! *     Routine  B3D_UPDATE  makes update of the solution of LSQ problem *
! *   using B3D method for adding or subtracting one observation.        *
! *                                                                      *
! *    It is assumed that 1) All elements of the covariance matrix has   *
! *   been calculated; 2) Normal matrix and normal vectors have been     *
! *   scaled by B3D_SCL (if ISCL .NE. 0 ); 3) All blocks of local        *
! *   parameters have the same size except the last one: it has the size *
! *   not greater than the size of others blocks.                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       IP ( INTEGER*4 ) -- Switch of the mode of work:                *
! *                        IP = 1 -- adding new observation.             *
! *                        IP =-1 -- subtracting one observation.        *
! *     ISCL ( INTEGER*4 ) -- Switch of scaling.                         *
! *                        ISCL = 0 -- No scaling of the normal matrix   *
! *                                    has been applied.                 *
! *                        ISCL = 1 -- Scaling of the normal matrix has  *
! *                                    been applied: normal matrix and   *
! *                                    normal vector have been           *
! *                                    multiplied by diagonal matrix to  *
! *                                    be reciprocal to the matrix       *
! *                                    produced from the main diagonal   *
! *                                    of the initial normal matrix.     *
! *                                    Thus, the main diagonal is unit.  *
! *        N ( INTEGER*4 ) -- number of group of local parameters.       *
! *        G ( INTEGER*4 ) -- number of global parameters.               *
! *        L ( INTEGER*4 ) -- number of local parameters at one block    *
! *                           for blocks (1,N-1) except the last one.    *
! *       LA ( INTEGER*8 ) -- (L*(L+1))/2                                *
! *       LX ( INTEGER*4 ) -- number of local parameters at the last     *
! *                           block.                                     *
! *      LXA ( INTEGER*8 ) -- (LX*(LX+1))/2                              *
! *       NK ( INTEGER*4 ) -- (N-1)*(N-2)/2 -- the number of frames of   *
! *                           off diagonal blocks of the covariance      *
! *                           matrix. Control parameter.                 *
! *    N_OUT ( INTEGER*4 ) -- The number of the block where the          *
! *                           equation which will be added or removed    *
! *                           is located.                                *
! *        Y ( REAL*8    ) -- Right part of the equation of conditions.  *
! *      WEI ( REAL*8    ) -- Weight of the observation. It has the      *
! *                           dimension to be reciprocal to dimension    *
! *                           of the right part (1/sigma).               *
! *       S0 ( REAL*8    ) -- vector of the scales for global            *
! *                           parameters. Dimension: G                   *
! *       SL ( REAL*8    ) -- array of N-1 vectors of the scales for the *
! *                           local parameters. Dimension: L*(N-1)       *
! *      SLX ( REAL*8    ) -- last, N-th vector of the scales for the    *
! *                           local parameters. Dimension: LX            *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *       A0 ( REAL*8    ) -- Subvector of global parameters for the     *
! *                           equation of conditions.                    *
! *       A1 ( REAL*8    ) -- Subvector of local current parameters for  *
! *                           the equation of conditions.                *
! *       A2 ( REAL*8    ) -- Subvector of local next parameters for     *
! *                           the equation of conditions.                *
! *       B0 ( REAL*8    ) -- matrix of global-global block. Dimension G *
! *        B ( REAL*8    ) -- array of N-1 matrices local-global blocks. *
! *                           dimension: L*G*(N-1).                      *
! *        C ( REAL*8    ) -- array of N-1 diagonal local-local blocks.  *
! *                           dimension: LA*N.                           *
! *        D ( REAL*8    ) -- array of N-1 rectangular down-diagonal     *
! *                           local-local blocks. dimension: L*L*(N-1).  *
! *                           Comment: the first matrix doesn't have any *
! *                           physical sense and isn't used. It only     *
! *                           occupies place for the convenience of      *
! *                           index calculations.                        *
! *       BX ( REAL*8    ) -- last, N-th matrix of local-global block.   *
! *                           dimension: LX*G.                           *
! *       CX ( REAL*8    ) -- last, N-th diagonal local-local block.     *
! *                           dimension: LXA.                            *
! *       DX ( REAL*8    ) -- last, N-th rectangular down-diagonal       *
! *                           local-local block. Dimension: LX*L.        *
! *      CVM ( REAL*8    ) -- Array of off-diagonal blocks of covariance *
! *                           matrix. Array consist of NK L*L frames     *
! *                           (It is assumed that the last block has     *
! *                            less or the same dimension and it is      *
! *                            enough room for allocation it in one      *
! *                            frame). Frames are numbered by columns    *
! *                           starting from j+2,j;  j+3,j; j+4,j; ...    *
! *       E0 ( REAL*8    ) -- vector of the estimates of global          *
! *                           parameters. Dimension: G                   *
! *       EL ( REAL*8    ) -- array of N-1 vectors of the estimates of   *
! *                           local parameters. Dimension: L*(N-1)       *
! *      ELX ( REAL*8    ) -- last, N-th vector of the estimates of the  *
! *                           local parameters. Dimension: LX            *
! *       Q0 ( REAL*8    ) -- Working vector dimension of G.             *
! *       QL ( REAL*8    ) -- Array of working vectors. Dimension L*N    *
! *      QLX ( REAL*8    ) -- Working vector dimension of LX.            *
! *      VG0 ( REAL*8    ) -- Working vector dimension of G.             *
! *      VL1 ( REAL*8    ) -- Working vector dimension of L.             *
! *      VL2 ( REAL*8    ) -- Working vector dimension of L.             *
! * IUER ( INTEGER*4, OPT) -- Universal error handler.                   *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  05-SEP-97   B3D_UPDATE   v1.3 (c) L. Petrov  10-JUL-2003  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IP, ISCL, N, G, L, LX, NK, N_OUT, IUER
      ADDRESS__TYPE :: LA, LXA
      REAL*8     A0(G), A1(L), A2(*), Y
      REAL*8     B0(*), E0(G), Q0(G), &
     &           B(L,G,N), C(LA,N), D(L,L,N), EL(L,N), QL(L,N), &
     &           BX(LX,G), CX(LXA), DX(LX,L), ELX(LX), QLX(LX), &
     &           CVM(L,L,NK), &
     &           VG0(G), VL1(L), VL2(L), S0(G), SL(L,N), SLX(LX), &
     &           WEI
      REAL*8     GAIN, UNGAIN, UNGAIN_LIMIT
      PARAMETER  ( UNGAIN_LIMIT = 1.D0/1.D6 )
      CHARACTER  STR*20
      INTEGER*4  LC2                       
      INTEGER*4  LC3                       
      INTEGER*4  LC, L2, IK, J1, J2, J3
      ADDRESS__TYPE :: IQ,  IB,       ID, IE
      ADDRESS__TYPE :: IQ2, IB2, IC2      
      ADDRESS__TYPE :: IQ3,          ID3
      INTEGER*4  I, J, IER
      INTEGER*8  LOC_CVM
      REAL*8     CQ, CE
      INTEGER*4, EXTERNAL :: I_LEN
      REAL*8,    EXTERNAL :: DP_VV_V
!
! --- Statment function for calculation the index of the frame for the block
! --- I,J of full covariance matrix in the array CVM
!
      LOC_CVM(I,J,N) = INT8(min(I,J)-1)*INT8(2*N-min(I,J)-2)/2 + INT8(max(I,J)-min(I,J)-1)
!
      IF ( N_OUT .LE. 0   .OR.   N_OUT+1 .GT. N ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( N_OUT, STR )
           CALL ERR_LOG ( 7401, IUER, 'B3D_UPDATE', 'Parameter N_OUT '// &
     &         '(index of the current segment) has wrong value: '// &
     &         STR(1:I_LEN(STR)) )
           RETURN
      END IF
!
      IF ( ISCL .NE. 0  .AND.  ISCL .NE. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( ISCL, STR )
           CALL ERR_LOG ( 7402, IUER, 'B3D_UPDATE', 'Parameter ISCL has '// &
     &         'wrong value: '//STR(1:I_LEN(STR)) )
           RETURN
      END IF
!
      IF ( IP .NE. -1  .AND.  IP .NE. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IP, STR )
           CALL ERR_LOG ( 7403, IUER, 'B3D_UPDATE', 'Parameter IP has '// &
     &         'wrong value: '//STR(1:I_LEN(STR)) )
           RETURN
      END IF
!
! --- If scaling was applied we should multiply element-by-element all three
! --- parts of an equation of conditions by the vectors of scales.
!
      IF ( ISCL .EQ. 1 ) THEN
           CALL VEC_MULT_VECTOR   ( A0, S0,          G, A0 )
           CALL VEC_MULT_VECTOR   ( A1, SL(1,N_OUT), L, A1 )
           IF ( N_OUT+1 .LT. N ) THEN
                CALL VEC_MULT_VECTOR   ( A2, SL(1,N_OUT+1),  L, A2 )
              ELSE IF ( N_OUT+1 .EQ. N ) THEN
                CALL VEC_MULT_VECTOR   ( A2, SLX,           LX, A2 )
           END IF
      END IF
!
! --- We should also multiply vectors of the euqtions by the weight ( regardless
! --- of whether or not scaling has been applied)
!
      CALL VEC_MULT_CONSTANT      ( A0, G, WEI,  A0 )
      CALL VEC_MULT_CONSTANT      ( A1, L, WEI,  A1 )
      IF ( N_OUT+1 .LT. N ) THEN
           CALL VEC_MULT_CONSTANT ( A2,  L, WEI, A2 )
         ELSE
           CALL VEC_MULT_CONSTANT ( A2, LX, WEI, A2 )
      END IF
!
! --- Now we calculate the subset of vectors Q ( Q = N^(-1)* A * wei )
! --- Whis is necessary for calcualtion of GAIN
!
! --- Calculation of Q0
!
      CALL MUL_MV_SV_V ( G, B0, G, A0, G, Q0, -3 )
      CALL MUL_MV_TV_V ( L, G, B(1,1,N_OUT), L, A1, G, VG0, -3 )
      CALL ADD_VV      ( G, Q0, VG0 )
      IF ( N_OUT+1 .LT. N ) THEN
           CALL MUL_MV_TV_V ( L, G, B(1,1,N_OUT+1), L, A2, G, VG0, -3 )
         ELSE IF ( N_OUT+1 .EQ. N ) THEN
           CALL MUL_MV_TV_V ( LX, G, BX, LX, A2, G, VG0, -3 )
      END IF
      CALL ADD_VV      ( G, Q0, VG0 )
!
! --- Calculation of QL(NZ_OUT)
!
      CALL MUL_MV_IV_V ( L, G, B(1,1,N_OUT), G, A0, L, QL(1,N_OUT), -3 )
      CALL MUL_MV_SV_V ( L, C(1,N_OUT), L, A1, L, VL1, -3 )
      CALL ADD_VV      ( L, QL(1,N_OUT), VL1 )
      IF ( N_OUT+1 .LT. N ) THEN
           CALL MUL_MV_TV_V ( L, L, D(1,1,N_OUT+1), L, A2, L, VL1, -3 )
        ELSE IF ( N_OUT+1 .EQ. N ) THEN
           CALL MUL_MV_TV_V ( LX, L, DX, LX, A2, L, VL1, -3 )
      END IF
      CALL ADD_VV ( L, QL(1,N_OUT), VL1 )
!
! --- Calculation of QL(NZ_OUT+1)
!
      IF ( N_OUT+1 .LT. N ) THEN
           CALL MUL_MV_IV_V ( L, G, B(1,1,N_OUT+1), G, A0, L, QL(1,N_OUT+1), &
     &                        IER )
           CALL MUL_MV_IV_V ( L, L, D(1,1,N_OUT+1), L, A1, L, VL2, -3 )
           CALL ADD_VV      ( L, QL(1,N_OUT+1), VL2 )
           CALL MUL_MV_SV_V ( L,    C(1,N_OUT+1),   L, A2, L, VL2, -3 )
           CALL ADD_VV      ( L, QL(1,N_OUT+1), VL2 )
        ELSE IF ( N_OUT+1 .EQ. N ) THEN
           CALL MUL_MV_IV_V ( LX, G, BX, G,  A0, LX, QLX, -3 )
           CALL MUL_MV_IV_V ( LX, L, DX, L,  A1, LX, VL2, -3 )
           CALL ADD_VV      ( LX, QLX, VL2 )
           CALL MUL_MV_SV_V ( LX,    CX, LX, A2, LX, VL2, -3 )
           CALL ADD_VV      ( LX, QLX, VL2 )
      END IF
!
! --- Calcultation of contributors CQ and CE
!
      IF ( N_OUT+1 .LT. N ) THEN
           CQ = DP_VV_V ( G, A0, Q0 ) + DP_VV_V ( L, A1, QL(1,N_OUT  ) ) + &
     &                                  DP_VV_V ( L, A2, QL(1,N_OUT+1) )
           CE = DP_VV_V ( G, A0, E0 ) + DP_VV_V ( L, A1, EL(1,N_OUT  ) ) + &
     &                                  DP_VV_V ( L, A2, EL(1,N_OUT+1) )
        ELSE IF ( N_OUT+1 .EQ. N ) THEN
           CQ = DP_VV_V ( G, A0, Q0 ) + DP_VV_V ( L,  A1, QL(1,N_OUT) ) + &
     &                                  DP_VV_V ( LX, A2, QLX         )
           CE = DP_VV_V ( G, A0, E0 ) + DP_VV_V ( L,  A1, EL(1,N_OUT) ) + &
     &                                  DP_VV_V ( LX, A2, ELX         )
      END IF
!
! --- Calculation of GAIN
!
      IF ( IP .EQ. 1 ) THEN
           GAIN = -1.D0/(1.D0 + CQ)
         ELSE IF ( IP .EQ. -1 ) THEN
!
! -------- Such a rare situation is possible: since residuals have not
! -------- been updated after correction solution for the previous
! -------- outlier, gain may appear too large. So large that computational
! -------- problems may occur. To prevent the loss of precision we make
! -------- a test: isn't the gain too large?
!
           UNGAIN = ( 1.D0 - CQ )
           IF ( DABS(UNGAIN) .LT. UNGAIN_LIMIT ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR, FMT='(1PE15.7)' ) UNGAIN
                CALL CHASHL ( STR )
!                CALL ERR_PASS ( 7404, IUER )
                CALL ERR_LOG  ( 7404, IUER, 'B3D_UPDATE', 'Internal error: '// &
     &                         'GAIN appeared too large: 1/GAIN ='//STR )
                RETURN
           END IF
!
           GAIN = 1.D0/UNGAIN
      END IF
!
! --- Making correction of the global parameters
!
      CALL ADDC_VV ( G, 1.D0, E0, GAIN*(CE-Y*WEI), Q0, E0 )
!
! --- Making correction for local parameters
!
      DO 410 J1=1,N
!
! ------ In order to faciliate handling special case of the last short block
! ------ we do such a trick: we keep the addresses of some necessary arrays
!
         IF ( J1 .LT. N ) THEN
              IQ = LOC( QL(1,J1)   )
              IE = LOC( EL(1,J1)   )
              IB = LOC(  B(1,1,J1) )
              ID = LOC(  D(1,1,J1) )
              LC = L
            ELSE IF ( J1 .EQ. N ) THEN
              IQ = LOC( QLX )
              IE = LOC( ELX )
              IB = LOC(  BX )
              ID = LOC(  DX )
              LC = LX
         END IF
         IF ( N_OUT+1 .LT. N ) THEN
              L2 = L
           ELSE IF ( N_OUT+1 .EQ. N ) THEN
              L2 = LX
         END IF
!
! ------ Caculation all others subvectors QL (unless J1 is N_OUT or N_OUT+1 --
! ------ they have been calculated previously). 4 cases should be handled
! ------ separately
!
         IF ( J1 .LT. N_OUT-1 ) THEN
!
             CALL MUL_MV_IV_V ( L, G, B(1,1,J1), G, A0, L, %VAL(IQ), -3 )
             IK = LOC_CVM ( J1, N_OUT, N )
             CALL MUL_MV_TV_V ( L, L, CVM(1,1,IK), L, A1, L, VL1, -3 )
             CALL ADD_VV  ( L, %VAL(IQ), VL1 )
             IK = LOC_CVM ( J1, N_OUT+1, N )
             CALL MUL_MV_TV_V ( L2, L, CVM(1,1,IK), L2, A2, L, VL2, -3 )
             CALL ADD_VV  ( L, %VAL(IQ), VL2 )
           ELSE IF ( J1 .EQ. N_OUT-1 ) THEN
!
             CALL MUL_MV_IV_V ( L, G, B(1,1,J1), G, A0, L, %VAL(IQ), -3 )
             CALL MUL_MV_TV_V ( L, L, D(1,1,J1+1), L, A1, L, VL1, -3 )
             CALL ADD_VV  ( L, %VAL(IQ), VL1 )
             IK = LOC_CVM ( J1, N_OUT+1, N )
             CALL MUL_MV_TV_V ( L2, L, CVM(1,1,IK), L2, A2, L, VL2, -3 )
             CALL ADD_VV  ( L, %VAL(IQ), VL2 )
           ELSE IF ( J1 .EQ. N_OUT+2 ) THEN
!
             CALL MUL_MV_IV_V ( LC, G, %VAL(IB), G, A0, LC, %VAL(IQ), -3 )
             IK = LOC_CVM ( J1, N_OUT, N )
             CALL MUL_MV_IV_V ( LC, L, CVM(1,1,IK), L, A1, LC, VL1, -3 )
             CALL ADD_VV  ( LC, %VAL(IQ), VL1 )
             CALL MUL_MV_IV_V ( LC, L2, %VAL(ID), L2, A2, LC, VL2, -3 )
             CALL ADD_VV  ( LC, %VAL(IQ), VL2 )
           ELSE IF ( J1 .GT. N_OUT+2 ) THEN
!
             CALL MUL_MV_IV_V ( LC, G, %VAL(IB), G, A0, LC, %VAL(IQ), -3 )
             IK = LOC_CVM ( J1, N_OUT, N )
             CALL MUL_MV_IV_V ( LC, L, CVM(1,1,IK), L, A1, LC, VL1, -3 )
             CALL ADD_VV  ( LC, %VAL(IQ), VL1 )
             IK = LOC_CVM ( J1, N_OUT+1, N )
             CALL MUL_MV_IV_V ( LC, L2, CVM(1,1,IK), L2, A2, LC, VL2, -3 )
             CALL ADD_VV  ( LC, %VAL(IQ), VL2 )
         END IF
!
! ------ Making correction to the estimates of the local parameters
!
         CALL ADDC_VV ( LC, 1.D0, %VAL(IE), GAIN*(CE-Y*WEI), %VAL(IQ), %VAL(IE))
 410  CONTINUE
!
! --- Update of global covariance matrix
!
      CALL DIAD_CVT_S ( GAIN, G, Q0, Q0, B0 )
!
! --- Update of local-global and local-local covariance matrices
!
      IK = 0
      DO 420 J2=1,N
         IF ( J2 .LT. N ) THEN
              IQ2 = LOC( QL(1,J2)   )
              IB2 = LOC(  B(1,1,J2) )
              IC2 = LOC(  C(1,J2)   )
              LC2 = L
            ELSE IF ( J2 .EQ. N ) THEN
              IQ2 = LOC( QLX )
              IB2 = LOC(  BX )
              IC2 = LOC(  CX )
              LC2 = LX
         END IF
!
! ------ Update local-global block of covariance matrix
!
         CALL DIAD_CVT ( GAIN, LC2, %VAL(IQ2), G, Q0, %VAL(IB2) )
!
         DO 430 J3=J2,N
            IF ( J3 .LT. N ) THEN
                 IQ3 = LOC( QL(1,J3)   )
                 ID3 = LOC(  D(1,1,J3) )
                 LC3 = L
               ELSE IF ( J3 .EQ. N ) THEN
                 IQ3 = LOC( QLX )
                 ID3 = LOC(  DX )
                 LC3 = LX
            END IF
            IF ( J3 .EQ. J2 ) THEN
!
! -------------- Update of digonal local-local block of covariance matrix
!
                 CALL DIAD_CVT_S ( GAIN, LC2, %VAL(IQ2), %VAL(IQ2), %VAL(IC2) )
              ELSE IF ( J3 .EQ. J2+1 ) THEN
!
! -------------- Update of down-diagonal local-local block of covariance matrix
!
                 CALL DIAD_CVT ( GAIN, LC3, %VAL(IQ3), LC2, %VAL(IQ2), &
     &                                                      %VAL(ID3)  )
              ELSE IF ( J3 .GT. J2+1 ) THEN
!
! -------------- Update of digonal, off-diagonal and off-downdiagonal
! -------------- local-local (j3,j2) block of covariance matrix
!
                 IK = LOC_CVM ( J2, J3, N )
                 CALL DIAD_CVT ( GAIN, LC3, %VAL(IQ3), LC2, %VAL(IQ2), &
     &                                                      CVM(1,1,IK) )
            END IF
 430     CONTINUE
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B3D_UPDATE  #!#
