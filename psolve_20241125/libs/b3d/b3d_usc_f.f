      SUBROUTINE B3D_USC_F ( F_MAT, F_VEC, N, G, GA, L, LA, LX, LXA, NK, B0, &
     &           Z0, S0, B, C, D, ZL, SL, BX, CX, DX, ZLX, SLX, CVM )
! ************************************************************************
! *                                                                      *
! *   Routine  B3D_USC  makes unscaling all submatrices and subvectors   *
! *   of  the system of normal equation for the case B3D algorithm. This *
! *   operation is reciprocal to  B3D_SCL. It is assumed that 1) full    *
! *   covaraiance matrix has been calculated; 2) Normal matriz and       *
! *   normal vectors have been scaled by B3D_SCL; 3) All blocks of local *
! *   parameters have the same size except the last one: it has size not *
! *   greater than the size of others blocks.                            *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    F_MAT ( LOGICAL*4 ) -- whether to make unscaling for the normal   *
! *                           matrix (before estimation) or the          *
! *                           covariance matrix (after estiamtion).      *
! *    F_VEC ( LOGICAL*4 ) -- whether to make unscaling for the vectors  *
! *                           of right parts of the normal system        *
! *                           (before estimation) or the vectors of the  *
! *                           estiamtes (after estiamtion).              *
! *        N ( INTEGER*4 ) -- number of group of local parameters.       *
! *        G ( INTEGER*4 ) -- number of global parameters.               *
! *        L ( INTEGER*4 ) -- number of local parameters at one block    *
! *                           for blocks (1,N-1) except the last one.    *
! *       LA ( INTEGER*8 ) -- (L*(L+1))/2                                *
! *       LX ( INTEGER*4 ) -- number of local parameters at the last     *
! *                           block.                                     *
! *      LXA ( INTEGER*8 ) -- (LX*(LX+1))/2                              *
! *       NK ( INTEGER*4 ) -- (N-1)*(N-2)/2 -- the number of frames of   *
! *                           off diagonal blovks of the covariance      *
! *                           matrix. Control parameter.                 *
! *                                                                      *
! * _______________________ MODIFIED PARAMETERS: _______________________ *
! *                                                                      *
! *       B0 ( REAL*8    ) -- matrix of global-global block. Dimension G *
! *       Z0 ( REAL*8    ) -- vector of right parts of normal equations  *
! *                           for global parameters.                     *
! *       S0 ( REAL*8    ) -- vector of the scales for global            *
! *                           parameters. Dimension: G                   *
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
! *       SL ( REAL*8    ) -- array of N-1 vectors of the scales for the *
! *                           local parameters. Dimension: L*(N-1)       *
! *       BX ( REAL*8    ) -- last, N-th matrix of local-global block.   *
! *                           dimension: LX*G.                           *
! *       CX ( REAL*8    ) -- last, N-th diagonal local-local block.     *
! *                           dimension: LXA.                            *
! *       DX ( REAL*8    ) -- last, N-th rectangular down-diagonal       *
! *                           local-local block. Dimension: LX*L.        *
! *      ZLX ( REAL*8    ) -- last, N-th vector of right parts of normal *
! *                           equations for local parameters.            *
! *      SLX ( REAL*8    ) -- last, N-th vector of the scales for the    *
! *                           local parameters. Dimension: LX            *
! *      CVM ( REAL*8    ) -- Array of off-diagonal blocks of covariance *
! *                           matrix. Array consist of NK L*L frames     *
! *                           (It is assumed that the last block has     *
! *                            less or the same dimension and it is      *
! *                            enough room for allocation it in one      *
! *                            frame). Frames are numbered by columns    *
! *                           starting from j+2,j;  j+3,j; j+4,j; ...    *
! *                           j+3,j+1; j+4,j+1  etc.                     *
! *                                                                      *
! *  ###  09-SEP-97   B3D_USC_F   v2.1 (c)  L. Petrov  10-JUL-2003  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  N, G, L, LX, NK
      ADDRESS__TYPE :: GA, LA, LXA
      LOGICAL*4  F_VEC, F_MAT
      REAL*8     B0(*), Z0(G), S0(G), &
     &           B(L,G,N), C(LA,N), D(L,L,N), ZL(L,N), SL(L,N), &
     &           BX(LX,G), CX(LXA), DX(LX,L), ZLX(LX), SLX(LX), &
     &           CVM(L,L,*)
      ADDRESS__TYPE :: LU, LD
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10
      INTEGER*4  K1, K2, K3, K4, K5, ITP, K, IK
!
! --- Calculations are the same as in B3D_USC except the thing that vectors
! --- S0, SL are not calculated -- they have been already calculated earlier
! --- See commentrs for the subroutine B3D_SCL_X
!
      LD = 0
      DO 410 J1=1,G
         LD = LD + J1
         LU = LD - (J1-1)
!
         IF ( F_MAT ) THEN
              CALL VEC_MULT_CONSTANT ( B0(LU), J1, S0(J1), B0(LU) )
              CALL VEC_MULT_VECTOR   ( B0(LU), S0, J1,     B0(LU) )
         END IF
 410  CONTINUE
      IF ( F_VEC ) THEN
           CALL VEC_MULT_VECTOR   ( Z0, S0, G, Z0 )
      END IF
!
      IK=1
      DO 420 J2=1,N-1
         LD = 0
         DO 430 J3=1,L
            LD = LD + J3
            LU = LD - (J3-1)
!
            IF ( F_MAT ) THEN
               CALL VEC_MULT_CONSTANT ( C(LU,J2), J3, SL(J3,J2), C(LU,J2) )
               CALL VEC_MULT_VECTOR   ( C(LU,J2), SL(1,J2),  J3, C(LU,J2) )
!
               CALL VEC_MULT_VECTOR_I ( B(J3,1,J2), L, S0, 1, G, B(J3,1,J2), &
     &                                    L )
            END IF
 430     CONTINUE
!
         DO 440 J4=1,G
            IF ( F_MAT ) THEN
               CALL VEC_MULT_VECTOR   ( B(1,J4,J2), SL(1,J2), L, B(1,J4,J2) )
            END IF
 440     CONTINUE
         IF ( J2 .GT. 1 ) THEN
            DO 450 J5=1,L
               IF ( F_MAT ) THEN
                  CALL VEC_MULT_VECTOR ( D(1,J5,J2), SL(1,J2), L, D(1,J5,J2) )
               END IF
 450        CONTINUE
         END IF
         IF ( J2+1 .LT. N ) THEN
            DO 460 J6=1,L
               IF ( F_MAT ) THEN
                    CALL VEC_MULT_VECTOR_I ( D(J6,1,J2+1), L, SL(1,J2), 1, L, &
     &                                       D(J6,1,J2+1), L )
               END IF
 460        CONTINUE
           ELSE IF ( J2+1 .EQ. N ) THEN
            DO 470 J7=1,LX
               IF ( F_MAT ) THEN
                    CALL VEC_MULT_VECTOR_I ( DX(J7,1), LX, SL(1,J2), 1, L, &
     &                                       DX(J7,1), LX )
               END IF
 470        CONTINUE
         END IF
!
! ------ Unscalling of off-diafonal blocks (apart from main-diagonal and from
! ------ down-diagonal)
!
         IF ( F_MAT  .AND. J2+2 .LE. N ) THEN
!
! ----------- Cycle over the columns except the last one
!
              DO 510 K1=J2+2,N-1
                 IF ( IK .GT. NK ) THEN
                      WRITE ( 6, * ) ' N=',N,' NK=',NK
                      CALL ERR_LOG ( 7044, -1, 'B3D_USC_F', 'parameter NK '// &
     &                    'appeared to be too small and there is no room for '// &
     &                    'off-diagonal terms of covariance matrix' )
                      STOP 'B3D_USC_F: Fatal internal error'
                 END IF
                 DO 520 K2=1,L
                    CALL VEC_MULT_VECTOR ( CVM(1,K2,IK), SL(1,J2), L, &
     &                                     CVM(1,K2,IK) )
 520             CONTINUE
!
                 DO 530 K3=1,L
                    CALL VEC_MULT_VECTOR_I ( CVM(K3,1,IK), L, SL(1,J2), 1, L, &
     &                                       CVM(K3,1,IK), L  )
 530             CONTINUE
                 IK=IK+1
 510          CONTINUE
!
! ----------- Now the blosk in the last column
!
              DO 540 K4=1,L
                 CALL VEC_MULT_VECTOR ( CVM(1,K4,IK), SLX, LX, CVM(1,K4,IK) )
 540          CONTINUE
!
              DO 550 K5=1,LX
                 CALL VEC_MULT_VECTOR_I ( CVM(K5,1,IK), LX, SL(1,J2), 1, L, &
     &                                    CVM(K5,1,IK), LX  )
 550          CONTINUE
              IK=IK+1
         END IF
         IF ( F_VEC ) THEN
              CALL VEC_MULT_VECTOR ( ZL(1,J2), SL(1,J2), L, ZL(1,J2) )
         END IF
 420  CONTINUE
!
! --- Treating the last long block
!
      LD = 0
      DO 480 J8=1,LX
         LD = LD + J8     ! address of diagonal element
         LU = LD - (J8-1) ! arrdess of element of column for last block
!
         IF ( F_MAT ) THEN
            CALL VEC_MULT_CONSTANT ( CX(LU), J8,  SLX(J8), CX(LU) )
            CALL VEC_MULT_VECTOR   ( CX(LU), SLX, J8,      CX(LU) )
!
            CALL VEC_MULT_VECTOR_I ( BX(J8,1), LX, S0, 1, G, BX(J8,1), LX )
         END IF
 480  CONTINUE
!
      DO 490 J9=1,G
         IF ( F_MAT ) THEN
              CALL VEC_MULT_VECTOR   ( BX(1,J9), SLX, LX, BX(1,J9) )
         END IF
 490  CONTINUE
!
      DO 4100 J10=1,L
         IF ( F_MAT ) THEN
              CALL VEC_MULT_VECTOR   ( DX(1,J10), SLX, LX, DX(1,J10) )
         END IF
 4100 CONTINUE
!
      IF ( F_VEC ) THEN
         CALL VEC_MULT_VECTOR ( ZLX, SLX, LX, ZLX )
      END IF
!
! --- Testing section  (this section is not used for normal calculations)
!     ---------------
!
      IF ( L .NE. -233434 ) GOTO 810 ! Ignore testing section for normal life
!
          WRITE ( 6, * ) ' n=',n,' l=',l,' lx=',lx,' la=',la,' LXA=',LXA  ! %%%%%%
  910     CONTINUE
          WRITE ( 6, * ) '# B3D_USC:  Enter (0-B0, 1-B, 2-C, 3-D, ', &
     &             '4-Z0, 5-ZL, 6-S0, 7-SL),   K '
          READ ( 5, * )  ITP, K
         IF ( ITP.EQ.0 ) THEN
              CALL  MATVIEW_2 ( G, B0 )
            ELSE IF ( ITP.EQ.1 ) THEN
              IF ( K .GT. 0    .AND.    K .LT. N ) THEN
                   CALL MATVIEW ( 2, L, G, B(1,1,K), 'B (transp)', &
     &                            '()', 1, 1, -3 )
                ELSE IF ( K .EQ. N ) THEN
                   CALL MATVIEW ( 2, LX, G, BX, 'BX (transp)', &
     &                            '()', 1, 1, -3 )
              END IF
            ELSE IF ( ITP.EQ.2 ) THEN
              IF ( K .GT. 0    .AND.    K .LT. N ) THEN
                   CALL  MATVIEW_2 ( L, C(1,K) )
                ELSE IF ( K .EQ. N ) THEN
                   CALL  MATVIEW_2 ( LX, CX )
              END IF
            ELSE IF ( ITP.EQ.3 ) THEN
              IF ( K .GT. 0    .AND.    K .LT. N ) THEN
                   CALL MATVIEW ( 2, L, L, D(1,1,K), 'D (trnasp)', &
     &                           '()', 1, 1, -3 )
                ELSE IF ( K .EQ. N ) THEN
                   CALL MATVIEW ( 2, LX, L, DX, 'DX (trnasp)', &
     &                           '()', 1, 1, -3 )
              END IF
            ELSE IF ( ITP.EQ.4 ) THEN
              CALL  MATVIEW_1 ( G, 1, Z0           )
            ELSE IF ( ITP.EQ.5 ) THEN
              IF ( K .GT. 0    .AND.    K .LT. N  ) THEN
                   CALL  MATVIEW_1 ( L, 1, ZL(1,K) )
                ELSE IF ( K .EQ. N ) THEN
                   CALL  MATVIEW_1 ( LX, 1, ZLX    )
              END IF
            ELSE IF ( ITP.EQ.6 ) THEN
              CALL  MATVIEW_1 ( G, 1, S0           )
            ELSE IF ( ITP.EQ.7 ) THEN
              IF ( K .GT. 0    .AND.    K .LT. N  ) THEN
                   CALL  MATVIEW_1 ( L, 1, SL(1,K) )
                ELSE IF ( K .EQ. N ) THEN
                   CALL  MATVIEW_1 ( LX, 1, SLX    )
              END IF
            ELSE IF ( ITP .LT. 0 ) THEN
              GOTO 810
          END IF
         GOTO 910
 810  CONTINUE  ! End of testing section
      RETURN
      END  !#!  B3D_USC_F  #!#
