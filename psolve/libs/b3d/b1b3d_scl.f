      SUBROUTINE B1B3D_SCL ( G, L, S, SX, GA, LA, SA, NS, &
     &                       WI0, WIJ, BI0, BIJ, CIJ, DIJ, ZI0, ZIJ, &
     &                       SI0, SIJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  B1B3D_SCL  makes scaling all submatrices and subvectors   *
! *   of the submatrices of the system of normal equations related to    *
! *   the sertain block of local parameters for the case B1B3D algorithm.*
! *   Vectors of scales are saved in SI0, SIJ. Scaling is the operation  *
! *   of multiplying normal matrix and vector on such a matrix that      *
! *   resulting matrix will have unuit main diagonal. Assumed that all   *
! *   blocks of segmented parameters but the last contain the same       *
! *   number of parameters.                                              *
! *                                                                      *
! *   NB:  B1B3D_SCL   makes the same dirty trick as routine SCALER in   *
! *        SOLVE: if diagonal element is very small (1.D-14) it is being *
! *        sibstituted by 1.0                                            *
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
! *    WI0 ( REAL*8    ) -- Local-global block of normal matrix.         *
! *                         Dimension: L*G                               *
! *    WIJ ( REAL*8    ) -- A set of NS segmented-global submatrices of  *
! *                         the blocks of normal matrix. Dimension:      *
! *                         S*G*NS . Note: the last submatrix            *
! *                         has actual dimension SX*G. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array WIJ.                               *
! *    BI0 ( REAL*8    ) -- Local-local block of normal matrix.          *
! *                         Dimension: LA                                *
! *    BIJ ( REAL*8    ) -- A set of NS modified segmented-local         *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Dimension: S*L*NS . Note: the last submatrix *
! *                         has actual dimension SX*L. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array BIJ.                               *
! *    CIJ ( REAL*8    ) -- a set of NS segmented-segmented submatrices  *
! *                         of the blocks of normal matrix located       *
! *                         on the main diagonal.                        *
! *    DIJ ( REAL*8    ) -- A set of NS segmented-segmented submatrices  *
! *                         of the blocks of normal matrix located down  *
! *                         the main diagonal. Dimension: S*S*NS . Note: *
! *                         the last submatrix has actual dimension SX*S.*
! *                         In the case SX<S some unused place occurs at *
! *                         the bottom of the array DIJ.                 *
! *    ZI0 ( REAL*8    ) -- Local block of normal vector. Dimension: L   *
! *    ZIJ ( REAL*8    ) -- A set of NS segmented-global subvectors of   *
! *                         the blocks of normal vector. Dimension: S*NS *
! *                         Note: the last submatrix has actual          *
! *                         dimension S. In the case SX<S some unused    *
! *                         place occurs at the bottom of the array ZIJ. *
! *  IUER ( INTEGER*4  ) -- Universal error handler.                     *
! *         Input:  swicth IUER=0 -- no error messages generated even in *
! *                 the case of error. IUER=-1 -- in the case of error   *
! *                 the message will pe put on stdout.                   *
! *         Output: 0 in the case of successfull completion and non-zero *
! *                 in the case of error.                                *
! *                                                                      *
! * ________________________ OUTPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    SI0 ( REAL*8    ) -- Vector of scales of local parameters.        *
! *                         Dimension: L.                                *
! *    SIJ ( REAL*8    ) -- Set of vectors of scales of segmented        *
! *                         parameters. Dimension S*S*NS.                *
! *                                                                      *
! *  ###  20-FEB-1997  B1B3D_SCL   v1.2  (c)  L. Petrov 01-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  G, L, S, SX, NS, IUER
      INTEGER*8  GA, LA, SA
      REAL*8     WI0(L,G), WIJ(S,G,NS), BI0(LA), &
     &           BIJ(S,L,NS), CIJ(SA,NS), DIJ(S,S,NS), &
     &           ZI0(L), ZIJ(S,NS), &
     &           SI0(L), SIJ(S,NS)
      REAL*8     EPS
      CHARACTER  STR*20, STR1*20, STR2*20
      PARAMETER  ( EPS = 1.D-14 )
      INTEGER*4  J2, J3, J4, J5, J6, J7, J8, J9, SC, SF, KN1, KN2
      INTEGER*8  LU, LD
      INTEGER*4, EXTERNAL :: I_LEN
      LOGICAL    DIRTY_TRICK
!
! --- Scaling local parameters
!
      LD = 0
      DO 420 J2=1,L
         LD = LD + J2     ! address of diagonal element
         LU = LD - (J2-1) ! address of element BI0(1,J2) (upper element of col)
         IF ( BI0(LD) .LT. 0.D0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J2, STR )
              CALL CLRCH ( STR1 )
              WRITE ( UNIT=STR1(1:12), FMT='(1PG12.4)' ) BI0(LD)
              CALL CHASHL  ( STR1 )
              CALL ERR_LOG ( 6411, IUER, 'B1B3D_SCL', STR(1:I_LEN(STR))// &
     &            '-th diagonal element of combined global-global '// &
     &            'matrix is negative: '//STR1(1:I_LEN(STR1)) )
              RETURN
         END IF
!
! ------ Test: is it necessary to make "dirty trick" ?
!
         IF ( BI0(LD) .LT. EPS ) THEN
              SI0(J2) = 0.D0
              DIRTY_TRICK = .TRUE.
            ELSE
              SI0(J2) = 1.D0 / SQRT ( BI0(LD) )
              DIRTY_TRICK = .FALSE.
         END IF
!
! ------ Multiplunig whole J2-th column by the (diagonal J2-th element)**-1/2
!
         CALL VEC_MULT_CONSTANT ( BI0(LU), J2, SI0(J2), BI0(LU) )
!
! ------ Multiply element by element J2-th column by accumulated vector SI0
!
         CALL VEC_MULT_VECTOR   ( BI0(LU), SI0, J2, BI0(LU) )
!
         IF ( DIRTY_TRICK ) BI0(LD) = 1.0D0
 420  CONTINUE
!
      DO 430 J3=1,G
!
! ------ Multiply J3-th column of Wio(j3) by Sio(j3) vector
!
         CALL VEC_MULT_VECTOR ( WI0(1,J3), SI0, L, &
     &                            WI0(1,J3)                )
 430  CONTINUE
!
! --- Multiply element by element normal vector by vector SI0
!
      CALL VEC_MULT_VECTOR   ( ZI0, SI0, L, ZI0 )
!
! --- Scalling all blocks of segmented parameters
!
      DO 440 J4=1,NS
         LD = 0
         SC=S
         IF ( J4 .EQ. NS ) SC=SX
         DO 450 J5=1,SC
            LD = LD + J5     ! address of diagonal element
            LU = LD - (J5-1) ! arrdess of element of column for J4-th block
            IF ( CIJ(LD,J4) .LT. 0.0D0 ) THEN
               CALL CLRCH ( STR )
               CALL INCH  ( J5, STR )
               CALL CLRCH ( STR )
               CALL INCH  ( J4, STR )
               CALL CLRCH ( STR2 )
               WRITE ( UNIT=STR2(1:12), FMT='(1PG12.4)' ) CIJ(LD,J4)
               CALL CHASHL  ( STR1 )
               CALL ERR_LOG ( 6412, IUER, 'B1B3D_SCL', STR(1:I_LEN(STR))// &
     &            '-th diagonal element of '//STR1(1:I_LEN(STR1))// &
     &            ' local-local matrix is negative: '//STR2 )
               RETURN
            END IF
!
! --------- Test: is it necessary to make "dirty trick" ?
!
            IF ( CIJ(LD,J4) .LT. EPS ) THEN
                 SIJ(J5,J4) = 0.D0
                 DIRTY_TRICK = .TRUE.
               ELSE
                 SIJ(J5,J4) = 1.D0 / SQRT ( CIJ(LD,J4) )
                 DIRTY_TRICK = .FALSE.
            END IF
!
! --------- Multiply all J5-th column by the (diagonal j5-th element)**-1/2
!
            CALL VEC_MULT_CONSTANT ( CIJ(LU,J4), J5, SIJ(J5,J4), CIJ(LU,J4) )
!
! --------- Multiply element by element J5-th column and accumulated vector SL
!
            CALL VEC_MULT_VECTOR   ( CIJ(LU,J4), SIJ(1,J4), J5, CIJ(LU,J4) )
!
! --------- Multiply J5-th column of matrix BIJ by SI0
!
! --------- Here we make the first time the trick which we'll make further:
! --------- we find new indeces of the BIJ: (KN1,KN2) for the matrix with
! --------- the first dimension S, which corresponds the indeces (J5,1) for
! --------- the matrix with dimension SC. It stems from the fact that elements
! --------- in the last submatricres has been put assuming that the dimension
! --------- of the matrix is SX, but BIJ were delcatred in B1B3D_SCL with S
! --------- as the first dimension.
!
            CALL REDIM_MAT ( SC, J5, 1, S, KN1, KN2 )
            CALL VEC_MULT_VECTOR_I ( BIJ(KN1,KN2,J4), SC, SI0, 1, L, &
     &                               BIJ(KN1,KN2,J4), SC )
            IF ( DIRTY_TRICK ) CIJ(LD,J4) = 1.0D0
 450     CONTINUE
!
         DO 460 J6=1,G
!
! --------- Multiply J6-th column of Wij(J4) by Sij(J4) vector
!
            CALL REDIM_MAT ( SC, 1, J6, S, KN1, KN2 )
            CALL VEC_MULT_VECTOR   ( WIJ(KN1,KN2,J4), SIJ(1,J4), SC, &
     &                               WIJ(KN1,KN2,J4)                )
 460     CONTINUE
!
         DO 470 J7=1,L
!
! --------- Multiply J7-th column of Bij(J4) by Sij(J4) vector
!
            CALL REDIM_MAT ( SC, 1, J7, S, KN1, KN2 )
            CALL VEC_MULT_VECTOR   ( BIJ(KN1,KN2,J4), SIJ(1,J4), SC, &
     &                               BIJ(KN1,KN2,J4)                )
 470     CONTINUE
!
         IF ( J4 .GT. 1 ) THEN
            DO 480 J8=1,S
!
! ------------ Multiply J8-th column of D(J4) by SL(J4) vector
!
               CALL REDIM_MAT ( SC, 1, J8, S, KN1, KN2 )
               CALL VEC_MULT_VECTOR   ( DIJ(KN1,KN2,J4), SIJ(1,J4), SC, &
     &                                    DIJ(KN1,KN2,J4)                )
 480        CONTINUE
         END IF
!
         IF ( J4 .LT. NS ) THEN
            SF = S
            IF ( J4+1 .EQ. NS ) SF = SX
            DO 490 J9=1,SF
!
! ------------ Multiply J9-th row of DIJ(J4+1) by SIJ(J4) vector
!
               CALL REDIM_MAT ( SC, J9, 1, S, KN1, KN2 )
               CALL VEC_MULT_VECTOR_I ( DIJ(KN1,KN2,J4+1), SF, SIJ(1,J4), 1, &
     &                                 S, DIJ(KN1,KN2,J4+1), SF )
 490        CONTINUE
         END IF
!
! ------ Multiply element by element vector ZIJ(J4) by vector SIJ(J4)
!
         CALL VEC_MULT_VECTOR ( ZIJ(1,J4), SIJ(1,J4), SC, ZIJ(1,J4) )
 440  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B1B3D_SCL  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE B1B3D_USC ( G, L, S, SX, GA, LA, SA, NS, &
     &                       WI0, WIJ, BI0, BIJ, CIJ, DIJ, &
     &                       SI0, SIJ, EI0, EIJ, ZI0, ZIJ )
! ************************************************************************
! *                                                                      *
! *   Routine  B1B3D_USCL  unscales all submatrices and subvectors       *
! *   of the submatrices of the system of normal equations related to    *
! *   the sertain block of local parameters for the case B1B3D algorithm.*
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      G ( INTEGER*4 ) -- number of global parameters.                 *
! *      L ( INTEGER*4 ) -- number of local parameters at one block.     *
! *      S ( INTEGER*4 ) -- number of segmented parameters at one block  *
! *                         except the last segment.                     *
! *     SX ( INTEGER*4 ) -- number of segmented parameters at the last   *
! *                         segment. Note: SX should be no less them S   *
! *     G8 ( INTEGER*4 ) -- (G*(G+1))/2                                  *
! *     L8 ( INTEGER*4 ) -- (L*(L+1))/2                                  *
! *     S8 ( INTEGER*4 ) -- (S*(S+1))/2                                  *
! *     NS ( INTEGER*4 ) -- number of blocks of segmented parameters in  *
! *                         current group of local parameters.           *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *    WI0 ( REAL*8    ) -- Local-global block of normal matrix.         *
! *                         Dimension: L*G                               *
! *    WIJ ( REAL*8    ) -- A set of NS segmented-global submatrices of  *
! *                         the blocks of normal matrix. Dimension:      *
! *                         S*G*NS . Note: the last submatrix            *
! *                         has actual dimension SX*G. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array WIJ.                               *
! *    BI0 ( REAL*8    ) -- Local-local block of normal matrix.          *
! *                         Dimension: LA                                *
! *    BIJ ( REAL*8    ) -- A set of NS modified segmented-local         *
! *                         submatrices of the blocks of normal matrix.  *
! *                         Dimension: S*L*NS . Note: the last submatrix *
! *                         has actual dimension SX*L. In the case SX<S  *
! *                         some unused place occurs at the bottom of    *
! *                         the array BIJ.                               *
! *    CIJ ( REAL*8    ) -- a set of NS segmented-segmented submatrices  *
! *                         of the blocks of normal matrix located       *
! *                         on the main diagonal.                        *
! *    DIJ ( REAL*8    ) -- A set of NS segmented-segmented submatrices  *
! *                         of the blocks of normal matrix located down  *
! *                         the main diagonal. Dimension: S*S*NS . Note: *
! *                         the last submatrix has actual dimension SX*S.*
! *                         In the case SX<S some unused place occurs at *
! *                         the bottom of the array DIJ.                 *
! *    ZI0 ( REAL*8    ) -- Local block of normal vector. Dimension: L   *
! *    ZIJ ( REAL*8    ) -- A set of NS segmented-global subvectors of   *
! *                         the blocks of normal vector. Dimension: S*NS *
! *                         Note: the last submatrix has actual          *
! *                         dimension S. In the case SX<S some unused    *
! *                         place occurs at the bottom of the array ZIJ. *
! *  IUER ( INTEGER*4  ) -- Universal error handler.                     *
! *         Input:  swicth IUER=0 -- no error messages generated even in *
! *                 the case of error. IUER=-1 -- in the case of error   *
! *                 the message will pe put on stdout.                   *
! *         Output: 0 in the case of successfull completion and non-zero *
! *                 in the case of error.                                *
! *                                                                      *
! * ________________________ OUTPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    SI0 ( REAL*8    ) -- Vector of scales of local parameters.        *
! *                         Dimension: L.                                *
! *    SIJ ( REAL*8    ) -- Set of vectors of scales of segmented        *
! *                         parameters. Dimension S*S*NS.                *
! *                                                                      *
! *  ###  20-FEB-97    B1B3D_USC   v1.2  (c)  L. Petrov 10-JUL-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  G, L, S, SX, NS
      ADDRESS__TYPE :: GA, LA, SA
      REAL*8     WI0(L,G), WIJ(S,G,NS), BI0(LA), &
     &           BIJ(S,L,NS), CIJ(SA,NS), DIJ(S,S,NS), &
     &           SI0(L), SIJ(S,NS), &
     &           EI0(L), EIJ(S,NS), &
     &           ZI0(L), ZIJ(S,NS)
      INTEGER*4  J2, J3, J4, J5, J6, J7, J8, J9, SC, SF, KN1, KN2
      INTEGER*8  LU, LD
!
! --- The text of B1B3D_USC is almosty identical to B1B3D_SCL except that scale
! --- vectors are not calculated (they were calculated in B1B3D_SCL). Thus,
! --- see the source code B1B3D_SCL for more details.
!
! --- Unscaling local parameters
!
      LD = 0
      DO 420 J2=1,L
         LD = LD + J2     ! address of diagonal element
         LU = LD - (J2-1) ! address of element BI0(1,J2) (upper element of col)
!
         CALL VEC_MULT_CONSTANT ( BI0(LU), J2, SI0(J2), BI0(LU) )
!
         CALL VEC_MULT_VECTOR   ( BI0(LU), SI0, J2, BI0(LU) )
 420  CONTINUE
!
      DO 430 J3=1,G
         CALL VEC_MULT_VECTOR   ( WI0(1,J3), SI0, L, WI0(1,J3) )
 430  CONTINUE
!
      CALL VEC_MULT_VECTOR      ( ZI0, SI0, L, ZI0 )
      CALL VEC_MULT_VECTOR      ( EI0, SI0, L, EI0 )
!
! --- The same thing for all segmented blocks
!
      DO 440 J4=1,NS
         LD = 0
         SC=S
         IF ( J4 .EQ. NS ) SC=SX
         DO 450 J5=1,SC
            LD = LD + J5     ! address of diagonal element
            LU = LD - (J5-1) ! arrdess of element of column for J4-th block
!
            CALL VEC_MULT_CONSTANT ( CIJ(LU,J4), J5, SIJ(J5,J4), CIJ(LU,J4) )
!
            CALL VEC_MULT_VECTOR   ( CIJ(LU,J4), SIJ(1,J4), J5, CIJ(LU,J4) )
!
            CALL REDIM_MAT ( SC, J5, 1, S, KN1, KN2 )
            CALL VEC_MULT_VECTOR_I ( BIJ(KN1,KN2,J4), SC, SI0, 1, L, &
     &                               BIJ(J5,1,J4), SC )
 450     CONTINUE
!
         DO 460 J6=1,G
            CALL REDIM_MAT ( SC, 1, J6, S, KN1, KN2 )
            CALL VEC_MULT_VECTOR   ( WIJ(KN1,KN2,J4), SIJ(1,J4), SC, &
     &                               WIJ(KN1,KN2,J4)                )
 460     CONTINUE
!
         DO 470 J7=1,L
            CALL REDIM_MAT ( SC, 1, J7, S, KN1, KN2 )
            CALL VEC_MULT_VECTOR   ( BIJ(KN1,KN2,J4), SIJ(1,J4), SC, &
     &                               BIJ(KN1,KN2,J4)                )
 470     CONTINUE
!
         IF ( J4 .GT. 1 ) THEN
              DO 480 J8=1,S
                 CALL REDIM_MAT ( SC, 1, J8, S, KN1, KN2 )
                 CALL VEC_MULT_VECTOR   ( DIJ(KN1,KN2,J4), SIJ(1,J4), SC, &
     &                                    DIJ(KN1,KN2,J4)  )
 480        CONTINUE
         END IF
!
         IF ( J4 .LT. NS ) THEN
              SF = S
              IF ( J4+1 .EQ. NS ) SF = SX
              DO 490 J9=1,SF
                 CALL REDIM_MAT ( SC, J9, 1, S, KN1, KN2 )
                 CALL VEC_MULT_VECTOR_I ( DIJ(KN1,KN2,J4+1), SF, SIJ(1,J4), 1, &
     &                                    S, DIJ(KN1,KN2,J4+1), SF )
 490          CONTINUE
         END IF
         CALL VEC_MULT_VECTOR ( ZIJ(1,J4), SIJ(1,J4), SC, ZIJ(1,J4) )
         CALL VEC_MULT_VECTOR ( EIJ(1,J4), SIJ(1,J4), SC, EIJ(1,J4) )
 440  CONTINUE
!
      RETURN
      END  !#!  B1B3D_USC  #!#
