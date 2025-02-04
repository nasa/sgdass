      SUBROUTINE BPASS_PHAS_RATE ( IND_STA, LFRQ, SGN, &
     &                             PIM, BPS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  BPASS_PHAS_RATE  is for determing the rate of change of   *
! *   the phase calibration phase with frequency after resolving         *
! *   ambiguity in phase calibration phases                              *
! *                                                                      *
! * ### 29-JAN-2009  BPASS_PHAS_RATE v1.0 (c)  L. Petrov 30-JAN-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INTEGER*4  IND_STA
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      INTEGER*1  SGN
      INTEGER*4  LFRQ, IUER
      REAL*4     FREQ_BPS(PIM__MCHN), PHAS_BPS(PIM__MCHN), &
     &           AMPL_BPS(PIM__MCHN), PHAS_RAT, PHAS_PRED
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, IFRQ, IAMB, IND_PCAL, &
     &           IND_PCAL_BEG,  IER
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  if ( iuer .ne. -1231 ) then; call err_log ( 0, iuer ); return; end if
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL NOUT_R4 ( PIM%NFRQ, PIM%BPASS(IND_STA)%PHS_RATE )
!
      IFRQ = 0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 420 J2=1,PIM%NCHN ! cycle over spectral channels
            PHAS_BPS(J2) = PHAS_CMPL_R4( BPS%CMPL(J2,J1,IND_STA) )
            AMPL_BPS(J2) = ABS ( BPS%CMPL(J2,J1,IND_STA) )
            FREQ_BPS(J2) = PIM%FREQ_ARR(J2,J1,PIM%CONF%FRQ_GRP)
            IF ( PHAS_BPS(J2) > PI__NUM ) PHAS_BPS(J2) = PHAS_BPS(J2) - PI2
            IF ( J2 > 1 ) THEN
                 IAMB = NINT( (PHAS_BPS(J2) - PHAS_BPS(J2-1))/PI2 )
                 PHAS_BPS(J2) = PHAS_BPS(J2) - IAMB*PI2
            END IF
 420     CONTINUE 
         CALL ERR_PASS ( IUER, IER )
         CALL BPASS_MOD_POLY ( 1, PIM%CONF%BPS_AMP_MIN,  -1, 1, &
     &                         PIM%NCHN, FREQ_BPS, PHAS_BPS, AMPL_BPS, &
     &                                             PHAS_BPS, AMPL_BPS, &
     &                         PIM%NCHN, FREQ_BPS, PHAS_BPS, AMPL_BPS, &
     &                         IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG( 4082, IUER, 'BPASS_PHAS_RATE', 'Failure '// &
     &            'to compute interpolating polynomial for badnpass '// &
     &            'phases for station '//PIM%C_STA(IND_STA) )
              RETURN 
         END IF
!
! ------ NB! Pay attention to the sign
!
         IF ( SGN == -1 ) THEN
!
! ----------- This means that the first station is the 
! ----------- bandpass-reference station
!
              PIM%BPASS(IND_STA)%PHS_RATE(J1) = &
     &                 (PHAS_BPS(PIM%NCHN) - PHAS_BPS(1))/ &
     &                 (FREQ_BPS(PIM%NCHN) - FREQ_BPS(1))
            ELSE
!
! ----------- This means that the second station is the 
! ----------- bandpass-reference station. NB: the sign of the residual phase
! ----------- has been inverted by BPASS_STA_INT
!
              PIM%BPASS(IND_STA)%PHS_RATE(J1) = &
     &               - (PHAS_BPS(PIM%NCHN) - PHAS_BPS(1))/ &
     &                 (FREQ_BPS(PIM%NCHN) - FREQ_BPS(1))
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  BPASS_PHAS_RATE  !#!#
