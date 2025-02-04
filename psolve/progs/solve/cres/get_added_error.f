      SUBROUTINE get_added_noise &
     &(isite,et,itt,ittb,del_error,rat_error)
!
!     get_added_noise loops up the reweight constants for a given
!     baseline
!
      IMPLICIT NONE
      INCLUDE 'solve.i'
!
      INTEGER*2 ISITE(2), ITTB(*),ISTA1, ISTA2, I, N, J, JJ
      INTEGER*2 ITT(*)
      REAL*8 ET(2,*), DEL_ERROR, RAT_ERROR
      LOGICAL*4  IS_R8_NAN
!
      ISTA1 = ITT(ISITE(1))
      ISTA2 = ITT(ISITE(2))
!
      IF (ISTA1 .GT. ISTA2) THEN     ! swap stations
        I = ISTA1
        ISTA1 = ISTA2
        ISTA2 = I
      END IF
!
!   Calculate index in packed table which points to error for this
!   baseline. N is (max # stations per database - 2) = 16 - 2 = 14
!
      N = MAX_ARC_STA - 2
      J = ((ISTA1-1) * N) - (((ISTA1-1) * (ISTA1-2))/2) + ISTA2-1
      JJ = ITTB(J)
!
      IF ( IS_R8_NAN(ET(1,JJ)) ) ET(1,JJ) = 1.D-12
      IF ( IS_R8_NAN(ET(2,JJ)) ) ET(2,JJ) = 1.D-15
      del_error  =  ET(1, JJ)*1.e12
      rat_error  =  ET(2, JJ)*1.d15
!
      RETURN
      END
