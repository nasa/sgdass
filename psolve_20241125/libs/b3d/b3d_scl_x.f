      SUBROUTINE B3D_SCL_X ( N, G, GA, L, LA, LX, LXA, B0, Z0, S0, &
     &                       B, C, D, ZL, SL, BX, CX, DX, ZLX, SLX, &
     &                       IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  B3D_SCL  makes scaling all submatrices and subvectors of  *
! *   the system of normal equation for the case B3D algorithm.          *
! *   Vectors of scale are saved in S0, SL. Scaling is the operation of  *
! *   multiplying normal matrix and vector on such a matrix that         *
! *   resulting matrix will have unit main diagonal. Assumed that all   *
! *   blocks of local parameters but the last contain the same number of *
! *   parameters.                                                        *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- number of group of local parameters.       *
! *        G ( INTEGER*4 ) -- number of global parameters.               *
! *       GA ( INTEGER*8 ) -- (G*(G+1))/2                                *
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
! *                           physical sense and isn't used. It only     *
! *                           occupies place for the convenience of      *
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
! *       S0 ( REAL*8    ) -- vector of the scales for global            *
! *                           parameters. Dimension: G                   *
! *       SL ( REAL*8    ) -- array of N-1 vectors of the scales for the *
! *                           local parameters. Dimension: L*(N-1)       *
! *      SLX ( REAL*8    ) -- last, N-th vector of the scales for the    *
! *                           local parameters. Dimension: LX            *
! *                                                                      *
! *   NB:  B3D_SCL_X  makes the same dirty trick as routine SCALER in    *
! *        SOLVE: if diagonal element is very small (1.D-14) it is being *
! *        substituted by 1.0                                            *
! *                                                                      *
! *  ###  03-JAN-1997   B3D_SCL_X  v2.5  (c)  L. Petrov 20-MAY-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*8  GA, LA, LXA
      INTEGER*4  N, G, L, LX, IUER
      REAL*8     B0(*), Z0(G), S0(G), &
     &           B(L,G,N), C(LA,N), D(L,L,N), ZL(L,N), SL(L,N), &
     &           BX(LX,G), CX(LXA), DX(LX,L), ZLX(LX), SLX(LX)
      REAL*8     EPS
      CHARACTER  STR*20, STR1*20, STR2*20
      PARAMETER  ( EPS = 1.D-14 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, I_LEN
      INTEGER*8  LU, LD
      LOGICAL    DIRTY_TRICK
!
! --- Scaling global parameters
!
      LD = 0
      DO 410 J1=1,G
         LD = LD + J1     ! address of diagonal element
         LU = LD - (J1-1) ! address of element B0(1,J1) (upper element of col)
         IF ( B0(LD) .LT. 0.D0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL CLRCH ( STR1 )
              WRITE ( UNIT=STR1(1:12), FMT='(1PG12.4)' ) B0(LD)
              CALL CHASHL  ( STR1 )
              CALL ERR_LOG ( 6411, IUER, 'B3D_SCL_X', STR(1:I_LEN(STR))// &
     &            '-th diagonal element of combined global-global '// &
     &            'matrix is negative: '//STR1(1:I_LEN(STR1)) )
              RETURN
         END IF
!
! ------ Test: is it necessary to make "dirty trick" ?
!
         IF ( B0(LD) .LT. EPS ) THEN
              S0(J1) = 0.D0
              DIRTY_TRICK = .TRUE.
            ELSE
              S0(J1) = 1.D0 / SQRT ( B0(LD) )
              DIRTY_TRICK = .FALSE.
         END IF
!
! ------ Multiplunig whole J1-th column by the (diagonal j1-th element)**-1/2
!
         CALL VEC_MULT_CONSTANT ( B0(LU), J1, S0(J1), B0(LU) )
!
! ------ Multiply element by element J1-th column by accumulated vector S0
!
         CALL VEC_MULT_VECTOR   ( B0(LU), S0, J1, B0(LU) )
         IF ( DIRTY_TRICK ) B0(LD) = 1.0D0
 410  CONTINUE
!
! --- Multiply element by element normal vector by vector S0
!
      CALL VEC_MULT_VECTOR   ( Z0, S0, G, Z0 )
!
! --- The same thing for all local blocks
!
      DO 420 J2=1,N-1
         LD = 0
         DO 430 J3=1,L
            LD = LD + J3     ! address of diagonal element
            LU = LD - (J3-1) ! arrdess of element of column for J2-th block
            IF ( C(LD,J2) .LT. 0.0D0 ) THEN
               CALL CLRCH ( STR )
               CALL INCH  ( J3, STR )
               CALL CLRCH ( STR1 )
               CALL INCH  ( J2, STR1 )
               CALL CLRCH ( STR2 )
               WRITE ( UNIT=STR2(1:12), FMT='(1PG12.4)' ) C(LD,J2)
               CALL CHASHL  ( STR2 )
               CALL ERR_LOG ( 6412, IUER, 'B3D_SCL_X', STR(1:I_LEN(STR))// &
     &            '-th diagonal element of '//STR1(1:I_LEN(STR1))// &
     &            ' local-local matrix is negative: '//STR2 )
               RETURN
            END IF
!
! --------- Test: is it necessary to make "dirty trick" ?
!
            IF ( C(LD,J2) .LT. EPS ) THEN
                 SL(J3,J2) = 0.D0
                 DIRTY_TRICK = .TRUE.
               ELSE
                 SL(J3,J2) = 1.D0 / SQRT ( C(LD,J2) )
                 DIRTY_TRICK = .FALSE.
            END IF
!
! --------- Multiplying all J3-th column by the (diagonal j3-th element)**-1/2
!
            CALL VEC_MULT_CONSTANT ( C(LU,J2), J3, SL(J3,J2), C(LU,J2) )
!
! --------- Multiply element by element J3-th column and accumulated vector SL
!
            CALL VEC_MULT_VECTOR   ( C(LU,J2), SL(1,J2), J3, C(LU,J2) )
!
! --------- Multiply J2-th column of matrix B by S0
!
            CALL VEC_MULT_VECTOR_I ( B(J3,1,J2), L, S0, 1, G, B(J3,1,J2), L )
            IF ( DIRTY_TRICK ) C(LD,J2) = 1.0D0
 430     CONTINUE
!
         DO 440 J4=1,G
!
! --------- Multiply J4-th row of B(J2) by SL(J2) vector
!
            CALL VEC_MULT_VECTOR   ( B(1,J4,J2), SL(1,J2), L, B(1,J4,J2) )
 440     CONTINUE
!
         IF ( J2 .GT. 1 ) THEN
            DO 450 J5=1,L
!
! ------------ Multiply J5-th column of D(J2) by SL(J2) vector
!
               CALL VEC_MULT_VECTOR   ( D(1,J5,J2), SL(1,J2), L, &
     &                                  D(1,J5,J2), L  )
 450        CONTINUE
         END IF
!
         IF ( J2+1 .LT. N ) THEN
            DO 460 J6=1,L
!
! ------------ Multiply J6-th row of D(J2+1) by SL(J2) vector
!
               CALL VEC_MULT_VECTOR_I ( D(J6,1,J2+1), L, SL(1,J2), 1, L, &
     &                                  D(J6,1,J2+1), L )
 460        CONTINUE
           ELSE IF ( J2+1 .EQ. N ) THEN
            DO 470 J7=1,LX
!
! ------------ Multiply J7-th row of DX by SL(J2) vector
!
               CALL VEC_MULT_VECTOR_I ( DX(J7,1), LX, SL(1,J2), 1, L, &
     &                                  DX(J7,1), LX )
 470        CONTINUE
         END IF
!
! ------ Multiply element by element vector ZL(J2) by vector SL(J2)
!
         CALL VEC_MULT_VECTOR ( ZL(1,J2), SL(1,J2), L, ZL(1,J2) )
 420  CONTINUE
!
! --- TREATING THE LAST LONG BLOCK
!
      LD = 0
      DO 480 J8=1,LX
         LD = LD + J8     ! address of diagonal element
         LU = LD - (J8-1) ! arrdess of element of column for last block
         IF ( CX(LD) .LT. 0.D0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J8, STR )
              CALL CLRCH ( STR1 )
              WRITE ( UNIT=STR1(1:12), FMT='(1PG12.4)' ) CX(LD)
              CALL CHASHL  ( STR1 )
              CALL ERR_LOG ( 6413, IUER, 'B3D_SCL_X', STR(1:I_LEN(STR))// &
     &            '-th diagonal element of last local-local '// &
     &            'matrix is negative: '//STR1(1:I_LEN(STR1)) )
              RETURN
         END IF
!
! ------ Test: is it necessary to make "dirty trick" ?
!
         IF ( CX(LD) .LT. EPS ) THEN
              SLX(J8) = 0.D0
              DIRTY_TRICK = .TRUE.
           ELSE
              SLX(J8) = 1.D0 / SQRT ( CX(LD) )
              DIRTY_TRICK = .FALSE.
         END IF
!
! ------ Multiplunig all J8-th column by the (diagonal j8-th element)**-1/2
!
         CALL VEC_MULT_CONSTANT ( CX(LU), J8, SLX(J8), CX(LU) )
!
! ------ Multiply element by element J8-th column and accumulated vector SLX
!
         CALL VEC_MULT_VECTOR   ( CX(LU), SLX, J8, CX(LU) )
!
! ------ Multiply J8-th column of matrix BX by S0
!
         CALL VEC_MULT_VECTOR_I ( BX(J8,1), LX, S0, 1, G, BX(J8,1), LX )
         IF ( DIRTY_TRICK ) CX(LD) = 1.0D0
 480  CONTINUE
!
      IF ( LX > 0 ) THEN
           DO 490 J9=1,G
!
! ----------- Multiply J9-th row of BX by SLX vector
!
              CALL VEC_MULT_VECTOR   ( BX(1,J9), SLX, LX, BX(1,J9) )
 490       CONTINUE
      END IF
!
      DO 4100 J10=1,L
!
! ------ Multiply J10-th column of DX by SLX vector
!
         CALL VEC_MULT_VECTOR   ( DX(1,J10), SLX, LX, DX(1,J10), LX )
 4100 CONTINUE
!
! --- Multiply element by element vector ZLX by vector SLX
!
      CALL VEC_MULT_VECTOR ( ZLX, SLX, LX, ZLX )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B3D_SCL_X  #!#
