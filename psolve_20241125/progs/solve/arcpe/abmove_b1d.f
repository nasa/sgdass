      SUBROUTINE ABMOVE_B1D ( L, G, IORDER, AMAT, BVEC, A_LL, A_LG, &
     &                        A_GG, B_L, B_G )
! ************************************************************************
! *                                                                      *
! *   Routine  ABMOVE separates global and local parameters. Normal      *
! *   matrix AMAT and normal vector BVEC contains a mixture of local     *
! *   and global parameters in arbitrary order. Vector IORDER contains   *
! *   indeces of the local and global parameters. If the value of the    *
! *   index is more than L that means that this parameters has index     *
! *   to be equal value-l. Otherwise it is local parmater with the index *
! *   to be equl value.                                                  *
! *                                                                      *
! *  ###  13-FEB-1997   ABMOVE_B1D  v1.1  (c) L. Petrov 13-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  L, G, IORDER(*)
      REAL*8     AMAT(*), BVEC(*), A_LL(*), A_LG(L,G), A_GG(*), &
     &           B_L(L), B_G(G)
      INTEGER*4  J1, J2, IP1, IP2
      INTEGER*4, EXTERNAL :: I_LEN
      INTEGER*8, EXTERNAL :: INDX8
!%
!%      DO I = 1, NPARM
!%        B1(IORDER(I))=B2(I)
!%        DO J = 1, I
!%          A1( INDX4(IORDER(I),IORDER(J)) )=A2( INDX4(I,J) )
!%        END DO
!%      END DO
!
      DO 410 J1=1,L+G
         IP1=IORDER(J1)
         IF ( IP1 .LE. L ) THEN
              B_L( IP1 ) = BVEC(J1)
           ELSE
              B_G( IP1-L ) = BVEC(J1)
         END IF
         DO 420 J2=1,J1
            IP2=IORDER(J2)
            IF (        IP1 .LE. L  .AND.  IP2 .LE. L  ) THEN
                 A_LL( INDX8(IP1, IP2) ) = AMAT( INDX8(J1,J2) )
              ELSE IF ( IP1 .LE. L  .AND.  IP2 .GT. L  ) THEN
                 A_LG( IP1, IP2-L ) = AMAT( INDX8(J1,J2) )
              ELSE IF ( IP1 .GT. L  .AND.  IP2 .LE. L  ) THEN
                 A_LG( IP2, IP1-L ) = AMAT( INDX8(J2,J1) )
              ELSE IF ( IP1 .GT. L  .AND.  IP2 .GT. L   ) THEN
                 A_GG( INDX8(IP1-L, IP2-L) ) = AMAT( INDX8(J1,J2) )
            END IF
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END   !#!  ABMOVE_B1D  #!#
