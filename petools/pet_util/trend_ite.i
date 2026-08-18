!
! >>>>> INCLUDE-BLOCK with description of data structures used by
!       ../pet_util/trend_ite.f
!
!       trend_ite.i  23-AUG-99 
!
      TYPE     ITE_STRU
         REAL*8     ST  ! accumulator of time summs
         REAL*8     STT ! accumulator of summs of squares of time
         REAL*8     STX ! accumulator multiplications time and O-C
         REAL*8     SX  ! accumulators of squares of O-C
         REAL*8     T0  ! 
         INTEGER*4  NP_LAST
      END TYPE ITE_STRU
!
! <<<<< end of INCLUDE-BLOCK  trend_ite
!
