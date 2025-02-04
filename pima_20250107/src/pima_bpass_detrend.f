      SUBROUTINE PIMA_BPASS_DETREND ( PIM, BPS_CMPL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_BPASS_DETREND 
! *                                                                      *
! * ### 17-NOV-2020 PIMA_BPASS_DETREND v1.0 (c) L. Petrov 17-NOV-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE       ) :: PIM
      INTEGER*4  IUER
      CHARACTER  STR*128
      COMPLEX*8  BPS_CMPL(PIM%NCHN,PIM%NFRQ)
      REAL*8,    ALLOCATABLE :: PHAS(:), AMPL(:)
      REAL*8     PHS_AVR(PIM__MFRQ), GR_DEL(PIM__MFRQ), GR_SBD_ALL, PHS_AVR_ALL, &
     &           PHS_ADD, PHAS_SUM
      COMPLEX*8  CMPLX_SUM
      LOGICAL*1  FL_USED(PIM__MFRQ)
      INTEGER*4  J1, J2, J3, J4, J5, J6, IFRQ, KFRQ, IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!
      ALLOCATE ( PHAS(PIM%NCHN), STAT=IER )
      ALLOCATE ( AMPL(PIM%NCHN), STAT=IER )
!
      GR_SBD_ALL  = 0.0D0
      PHS_AVR_ALL = 0.0D0
      KFRQ = 0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         FL_USED(J1) = .FALSE.
         DO 420 J2=1,PIM%NCHN
            PHAS(J2) = PHAS_CMPL_R4 ( BPS_CMPL(J2,J1) )
            AMPL(J2) = ABS ( BPS_CMPL(J2,J1) )
            IF ( AMPL(J2) > PIM%CONF%BPS_AMP_MIN ) THEN
                 FL_USED(J1) = .TRUE.
            END IF
 420     CONTINUE 
         IF ( FL_USED(J1) ) THEN
              KFRQ = KFRQ + 1
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_AMB_RES_R8 ( PIM%NCHN, PIM%FREQ_ARR(1,J1,PIM%CONF%FRQ_GRP), &
     &                               PHAS, AMPL, PHS_AVR(J1), GR_DEL(J1), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 7721, IUER, 'PIMA_BPASS_DETREND', 'Error '// &
     &                 'in ambiguity resolution at IF '//STR )
                   DEALLOCATE ( PHAS )
                   DEALLOCATE ( AMPL )
                   RETURN 
              END IF
         END IF
         GR_SBD_ALL  = GR_SBD_ALL  + GR_DEL(J1)
         PHS_AVR_ALL = PHS_AVR_ALL + PHS_AVR(J1)
 410  CONTINUE 
      IF ( KFRQ > 0 ) THEN
           GR_SBD_ALL = GR_SBD_ALL/KFRQ
      END IF
      CMPLX_SUM = 0.0
      DO 430 J3=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IF ( FL_USED(J3) ) THEN
              DO 440 J4=1,PIM%NCHN
                 PHS_ADD = PI2*GR_SBD_ALL*(PIM%FREQ_ARR(J4,J3,PIM%CONF%FRQ_GRP) - &
     &                                     PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) ) 
                 CMPLX_SUM = CMPLX_SUM + BPS_CMPL(J4,J3)/ABS(BPS_CMPL(J4,J3))/ &
     &                                                   CMPLX( COS(PHS_ADD), SIN(PHS_ADD) )
 440          CONTINUE 
         END IF
 430  CONTINUE 
      IF ( KFRQ > 0 ) THEN
           PHS_AVR_ALL = PHAS_CMPL_R4 ( CMPLX_SUM )
         ELSE
           PHS_AVR_ALL = 0.0
      END IF
!
      DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         DO 460 J6=1,PIM%NCHN
            PHS_ADD = 0.0*PHS_AVR_ALL + PI2*GR_SBD_ALL*(PIM%FREQ_ARR(J6,J5,PIM%CONF%FRQ_GRP) - &
     &                                              PIM%FREQ_ARR(1,PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP) ) 
            BPS_CMPL(J6,J5) = BPS_CMPL(J6,J5)/CMPLX( COS(PHS_ADD), SIN(PHS_ADD) )
 460     CONTINUE 
 450  CONTINUE 
!
      DEALLOCATE ( PHAS )
      DEALLOCATE ( AMPL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_BPASS_DETREND  !#!#
