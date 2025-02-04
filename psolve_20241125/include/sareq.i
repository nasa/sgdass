!@This is the start of file &SAREQ
!
!     Some important equivalence + dimension statements:
!     Note that DABF is REAL*8 to insure double precision even in
!          programs which use extended precision.
!     JABF is equivalenced to IABF to prevent doubleword boundary error. (??)
!                                             ( no such thing! )
!     2021.06.01  pet  Increased JABF from 24 to 29 to accommodate space for JNAME_SARS
!     2024.07.09  pet  Increased IABF from 26 to 38 to accommodate space for NSCA_TOT_SARS, NSCA_USED_SARS
!
      CHARACTER   QABF(5)*8, JNAME_SARS*10
      INTEGER*4   N4BF(5)
      INTEGER*2   IABF(38), JABF(37), NABF(37), ITPR
      INTEGER*4   NSCA_TOT_SARS, NSCA_USED_SARS
      REAL*4      RABF(8)
      REAL*8      DABF(4), WEIGHTED_EPOCH_SARS, WEIGHT_SUM_SARS
!
      COMMON / TURF / JABF
      EQUIVALENCE ( JABF(4),    IABF(1), ITPR )
      EQUIVALENCE ( JABF(5),    NABF(1), QABF(1), N4BF(1) )
      EQUIVALENCE ( JABF(9),    DABF(1), RABF(1) )
      EQUIVALENCE ( JNAME_SARS,          IABF(22) )
      EQUIVALENCE ( NSCA_TOT_SARS,       IABF(27) )
      EQUIVALENCE ( NSCA_USED_SARS,      IABF(29) )
      EQUIVALENCE ( WEIGHTED_EPOCH_SARS, IABF(31) )
      EQUIVALENCE ( WEIGHT_SUM_SARS,     IABF(35) )
!
