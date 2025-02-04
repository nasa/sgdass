      SUBROUTINE CVMML ( A, SPAR, ISITE1, ISITE2, BSIG, CMT, LSCRD )
      IMPLICIT NONE
!
! 1.  CVMML PROGRAM SPECIFICATION
!
! 1.1 Calculate the product SPAR X A X SPARTRANS.
!
! 1.2 REFERENCES:
!
! 2.  CVMML INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      REAL*8 A(*)
      REAL*8 SPAR(6,11)
      INTEGER*2 LSCRD(3,MAX_STA), ISITE1, ISITE2
!
! A - Site coordinates covariance matrix
! ISITE1,ISITE2 - Site numbers of sites one and two
! LSCRD - Array of site coordinate parameter numbers for each component
!          of each station
! SPAR - Vector containing the partials
!
! 2.3 OUTPUT Variables:
!
      REAL*8 BSIG(11),CMT(3,3)
!
! BSIG - Baseline formal errors
! CMT - Covariance matrix for cartesian components
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: bwork
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 K,I,J,KPARAM,LPARAM,IX,ISP1,ISP2,LOCV(6),LOCP(6)
      INTEGER*4 IA,INDX4
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   pet   04-APR-99  Improved comments
!
! 5.  CVMML PROGRAM STRUCTURE
!
!CCCCC
!
!---- ZERO OUT THE OUTPUT ARRAYS
!
      DO K=1,11
         BSIG(K)=0.0D0
      ENDDO
!
      KPARAM = 0
      LPARAM = 0
!
! --- Run over sites 1 and 2
!
      DO 300 I=1,MAX(ISITE1,ISITE2)
!
! ------ See if I is pointing at either of the site we`re intereted in.
!
         IF ( I.NE.ISITE1 .AND. I.NE.ISITE2 ) GOTO 300
!
! ------ Run over x,y,z station components
!
         DO 200 J=1,3
!
! --------- OK, it is one of the sites we want. KPARAM counts the interesting
! --------- parameters
!
            KPARAM=KPARAM+1
!
! --------- See if this interesting parameter was adjusted
!
            IF ( LSCRD(J,I) .EQ. 0 ) GOTO 200
!
! --------- It was adjusted, so count the number of interesting adjusted
! --------- parameters
!
            LPARAM=LPARAM+1
!
! --------- LOC gives the correspondance between PARAMETERS and LPARAM
!
            LOCP(LPARAM) = LSCRD(J,I)
            LOCV(LPARAM) = KPARAM
            ISP2 = LOCV(LPARAM)
!
            DO 100 IX=1,LPARAM-1
               IA   = INDX4(LOCP(IX),LOCP(LPARAM))
               ISP1 = LOCV(IX)
!
               DO 50 K=1,11
                  BSIG(K) = BSIG(K) + 2.0D0*A(IA)*SPAR(ISP1,K)*SPAR(ISP2,K)
   50          CONTINUE
  100       CONTINUE
!
            IA = INDX4(LOCP(LPARAM),LOCP(LPARAM))
            DO 120 K=1,11
               BSIG(K)=BSIG(K)+A(IA)*SPAR(ISP2,K)**2
  120       CONTINUE
  200    CONTINUE
  300 CONTINUE
!
! --- Convert the variances to formal errors
!
      DO 400 I=1,11
         BSIG(I) = DSQRT(BSIG(I))
  400 CONTINUE
!
      RETURN
      END  !#!  CVMML  #!#
