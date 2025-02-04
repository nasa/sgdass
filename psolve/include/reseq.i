!
!     The following equivalence statement is required to decode
!     the array IRSITE into which the records from RESFIL are read.
!
      REAL*8 RFJD,RFRCT,RFAMB
      INTEGER*2 IRSITE(35)
      EQUIVALENCE(IRSITE(3),IRSTAR),(IRSITE(4),RDOC),(IRSITE(6),RROC  ), &
     &        (IRSITE( 8),RDERR),(IRSITE(10),RRERR),(IRSITE(12),RFJD  ), &
     &        (IRSITE(16),RFRCT),(IRSITE(20),IRUNW),(IRSITE(21),RELEV ), &
     &        (IRSITE(25),RFAMB),(IRSITE(29),NAMB ),(IRSITE(30),IRPNTR)
!
