      SUBROUTINE B3D_DSP_X ( FAST_COV, N, G, GA, L, LA, LX, LXA, B0, D0, &
     &                       C, DL, CX, DLX, SIG, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  B3D_DSP_X  puts into vectors  D0, DL, DLX  variances of   *
! *   the estimates of the parameters for the case of B3D algorithm.     *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! * FAST_COV ( INTEGER*4 ) -- Mode switch for calculation covariance     *
! *                           matrices.                                  *
! *            FAST_COV = F__GLO -- Only covariance matrix for global    *
! *                                 parameters will be calculated.       *
! *            FAST_COV = F__SEG -- All blocks of covariance matrix      *
! *                                 which correspond to non-zero blocks  *
! *                                 of normal matrix will be calculated. *
! *            FAST_COV = F__LOC -- the same as F__SEG.                  *
! *            FAST_COV = F__FUL -- All elements of covariance matrix,   *
! *                                 includinng off-diagonal have been    *
! *                                 calculated.                          *
! *        N ( INTEGER*4 ) -- number of group of local parameters.       *
! *        G ( INTEGER*4 ) -- number of global parameters.               *
! *       GA ( INTEGER*8 ) -- (G*(G+1))/2                                *
! *        L ( INTEGER*4 ) -- number of local parameters at one block    *
! *                           for blocks (1,N-1) except the last one.    *
! *       LA ( INTEGER*8 ) -- (L*(L+1))/2                                *
! *       LX ( INTEGER*4 ) -- number of local parameters at the last     *
! *                           block.                                     *
! *      LXA ( INTEGER*8 ) -- (LX*(LX+1))/2                              *
! *       B0 ( REAL*8    ) -- covariance matrix for global-global block. *
! *                           Dimension G .                              *
! *        C ( REAL*8    ) -- array of N-1 covariance martices for the   *
! *                           local-local segments. Dimension: LA*N.     *
! *       CX ( REAL*8    ) -- last, N-th diagonal covariance matrix for  *
! *                           the local-local segment. Dimension: LXA.   *
! *      SIG ( REAL*8    ) -- scaling factor. Variance is square root    *
! *                           from diagonal block of covariance matrix,  *
! *                           which after multiplied by SIG.             *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *       D0 ( REAL*8    ) -- vector of the variances for global         *
! *                           parameters. Dimension: G                   *
! *       DL ( REAL*8    ) -- array of N-1 vectors of the variances for  *
! *                           the local parameters. Dimension: L*(N-1)   *
! *      DLX ( REAL*8    ) -- last, N-th vector of the variances for the *
! *                           local parameters. Dimension: LX            *
! *                                                                      *
! *  ###  09-JAN-97    B3D_DSP_X   v1.5  (c)  L. Petrov  25-SEP-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      INTEGER*4  FAST_COV, N, G, L, LX, IUER
      INTEGER*8  GA, LA, LXA
      REAL*8     B0(*), D0(G), C(LA,N), DL(L,N), &
     &                         CX(LXA), DLX(LX), SIG
      INTEGER*4  J1, J3, J4, J5, K
!
! --- Calculation variances of global parameters
!
      K=0
      DO 410 J1=1,G
         K=K+J1   !  index of diagonal element
         D0(J1)=SQRT(B0(K))*SIG
 410  CONTINUE
!
! --- Calculation variances of the estimates for the local parameters
!
      DO 430 J3=1,N-1
         K=0
         DO 440 J4=1,L
            K=K+J4   !  index of diagonal element
            IF ( FAST_COV .EQ. F__LOC  .OR.  FAST_COV .EQ. F__SEG  .OR. &
     &           FAST_COV .EQ. F__FUL                                   ) THEN
                 DL(J4,J3)=SQRT( C(K,J3) )*SIG
              ELSE
                 DL(J4,J3)=SIG
            END IF
 440     CONTINUE
 430  CONTINUE
!
! --- Calculation variances of the estimates for the local parameters for the
! --- last block
!
      K=0
      DO 450 J5=1,LX
         K=K+J5      !  index of diagonal element
         DLX(J5)=SQRT( CX(K) )*SIG
            IF ( FAST_COV .EQ. F__LOC  .OR.  FAST_COV .EQ. F__SEG  .OR. &
     &           FAST_COV .EQ. F__FUL                                   ) THEN
              DLX(J5)=SQRT( CX(K) )*SIG
            ELSE
              DLX(J5)=SIG
         END IF
 450  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B3D_DSP_X  #!#
