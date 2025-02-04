      SUBROUTINE PIMA_PARSE_VLBA_LOG ( NBUF, BUF, M_TIM, M_CHN, N_TSYS, N_FRQ, &
     &           MJD_TSYS, UTC_TSYS, IF_FRQ, LO_FRQ, POL_FRQ, SCAN_NAME, &
     &           SOURCE_NAME, TSYS,  STA_NAM, YEAR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_PARSE_VLBA_LOG
! *                                                                      *
! * ## 11-NOV-2016 PIMA_PARSE_VLBA_LOG v1.0 (c) L. Petrov 11-NOV-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  NBUF, M_TIM, M_FRQ, N_TSYS, N_FRQ, YEAR, IVRB, IUER
      CHARACTER  BUF(NBUF)*(*), SCAN_NAME(M_TIM)*(*), POL_FRQ(M_FRQ)*(*), &
     &           SOURCE_TSYS(M_TIM)*(*), STA_NAM*(*)
      INTEGER*4  MJD_TSYS(M_TIM)
      REAL*8     UTC_TSYS(M_TIM), UTC_ONS(2,M_TIM), IF_FRQ(M_FRQ), &
     &           LO_FRQ(M_FRQ), TSYS(M_FRQ,M_TIM)
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_PARSE_VLBA_LOG  !#!#
