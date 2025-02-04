      SUBROUTINE PIMA_GACO_INIT ( PIM, VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_GACO_INIT
! *                                                                      *
! * ### 24-FEB-2016   PIMA_GACO_INIT  v1.1 (c) L. Petrov 30-JUL-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      REAL*8     VAL
      INTEGER*4  IUER
      INTEGER*4  J1, J2, IFRQ
!
      DO 410 J1=1,PIM%NSTA
         IFRQ = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            PIM%GACO(J1)%GACO_ERR(IFRQ)  = -1.0
            PIM%GACO(J1)%GACO_FRQ(IFRQ)  = PIM%FRQ(J2,PIM%CONF%FRQ_GRP)%FREQ
            PIM%GACO(J1)%IND_FREQ(IFRQ)  = J2
            PIM%GACO(J1)%NVIS(IFRQ)      = 0
 420     CONTINUE 
         PIM%GACO(J1)%GAIN_CORR = VAL
         PIM%GACO(J1)%NFRQ = IFRQ
 410  CONTINUE 
!
      PIM%GACO_STATUS = PIMA__INIT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GACO_INIT  !#!#
