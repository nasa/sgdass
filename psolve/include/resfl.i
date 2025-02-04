!@This is the start of file &RESFL
!
      INTEGER*2  IRSITE(2), IRSTAR, IRUNW, NAMB, IRPNTR, SUPSTAT_RES(2), &
     &           UACSUP_RES
      INTEGER*4  AUTO_SUP_RES, USER_SUP_RES, USER_REC_RES
!
!           JMGipson added PDERR, PRERR =post_fit errors.
! 01-MAY-98    pet      added SUPSTAT
! 26-JUL-99    pet      added UACSUP
! 06-JUN-2007  pet      added AUTO_SUP, USER_SUP, USER_REC
! 2021.03.20   pet      Added TAU_C, RATE_C
!
      INTEGER*2 IRESCM(JRESREC_WORDS)
      REAL*8    RDOC, RROC, RDERR, RRERR, PDERR, PRERR, RELEV(2)
      REAL*8    RFJD, RFRCT, RFAMB, TAU_C, RATE_C
      INTEGER*1 FILLER_RESFIL(3)
      INTEGER*1 LAST_BYTE_RESFIL
!
      COMMON / RESFL / RDOC, RROC, RDERR, RRERR, PDERR, PRERR, RFJD, &
     &                 RFRCT, RELEV, RFAMB, TAU_C, RATE_C, &
     &                 IRSITE, IRSTAR, IRUNW, NAMB, IRPNTR, SUPSTAT_RES, &
     &                 UACSUP_RES, AUTO_SUP_RES, USER_SUP_RES, USER_REC_RES, &
     &                 FILLER_RESFIL, LAST_BYTE_RESFIL
!
      EQUIVALENCE (RDOC,IRESCM(1))
