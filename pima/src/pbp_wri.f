      SUBROUTINE PBP_WRI ( PIM, BPS, SNR_MIN, SEFD_USE, DEG_AMP, DEG_PHS, &
     &                     FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PBP_WRI 
! *                                                                      *
! *  ###  19-AUG-2010     PBP_WRI    v1.1 (c) L. Petrov 03-MAR-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE  
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      REAL*8     SNR_MIN 
      CHARACTER  SEFD_USE*(*), FILOUT*(*)
      INTEGER*4  DEG_AMP, DEG_PHS, IUER
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, IFRQ, IER, LUN
      REAL*4     AMPL, PHAS
      REAL*8     PHAS_AVR, PHAS_RMS, AMPL_AVR, AMPL_RMS
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LTM_DIF
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 6371, IUER, 'PBP_WRI', 'Failure to open '// &
     &         'the output file '//FILOUT(1:I_LEN(FILOUT))//' for writing '// &
     &         'the bandpass. Error code: '//STR )
           RETURN 
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__POL_BPASS_LABEL
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 6371, IUER, 'PBP_WRI', 'Failure to write '// &
     &         'the file label in the output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' for the bandpass. Error code: '//STR )
           RETURN 
      END IF
!
      STR = GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Created at '//STR(1:19)
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A, I8)' ) 'PBP INFO '// &
     &                                  'GENERATED_BY_PIMA_VERSION: ', PIM%PBP(1)%PIMA_VERS
      WRITE ( UNIT=LUN, FMT='(A, I3)' ) 'PBP INFO '// &
     &                                  'NUMBER_OF_STATIONS:             ', PIM%NSTA
      WRITE ( UNIT=LUN, FMT='(A, I3)' ) 'PBP INFO '// &
     &                                  'FREQUENCY_GROUP:                ', BPS%IFRG
      WRITE ( UNIT=LUN, FMT='(A, A)'  ) 'PBP INFO '// &
     &                                  'REFERENCE_STATION:         ', &
              &                                   PIM%C_STA(BPS%IND_STA_REF)
      WRITE ( UNIT=LUN, FMT='(A, F8.2)' ) 'PBP INFO SNR_MIN:                   ', &
              &                            SNR_MIN
      WRITE ( UNIT=LUN, FMT='(A, I2)'   ) 'PBP INFO AMPL_POLYNOMIAL_DEGREE:          ', &
              &                            DEG_AMP
      WRITE ( UNIT=LUN, FMT='(A, I2)'   ) 'PBP INFO PHAS_POLYNOMIAL_DEGREE:          ', &
     &                                     DEG_PHS
      WRITE ( UNIT=LUN, FMT='(A, A)'    ) 'PBP INFO METHOD:                        ', &
     &                                     BPS%STATUS
      WRITE ( UNIT=LUN, FMT='(A, A)'    ) 'PBP INFO PCAL_CODE:                  ', &
     &                                     PIM%CONF%PHAS_CAL_CODE 
      WRITE ( UNIT=LUN, FMT='(A, A)'    ) 'PBP INFO SEFD_USE:                        ', &
     &                                     SEFD_USE
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      DO 410 J1=1,PIM%NSTA
         WRITE ( UNIT=LUN, FMT='(A,A)' ) 'PBP INFO STATION: ', PIM%C_STA(J1)
         WRITE ( UNIT=LUN, FMT='(A)'   ) '                  ########'
         WRITE ( UNIT=LUN, FMT='(A,A)' ) 'PBP INFO BANDPASS_TYPE:                       ', &
     &                                    PIM%PBP(J1)%TYP
         WRITE ( UNIT=LUN, FMT='(A,I8)' ) 'PBP INFO NUMBER_OF_FREQUENCY_CHANNELS: ', &
     &                                    PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
         WRITE ( UNIT=LUN, FMT='(A,I8)' ) 'PBP INFO NUMBER_OF_SPECTRAL_CHANNELS:  ', &
     &                                    PIM%NCHN
         IF ( PIM%CONF%BPS_MODE == PIMA__BPASS_INIT  .OR. &
     &        PIM%CONF%BPS_MODE == PIMA__BPASS_ACCUM      ) THEN
              WRITE ( UNIT=LUN, FMT='(A,I8,2X,I8)' ) 'PBP INFO NUMBER_OF_USED_OBSERVATIONS:  ', &
     &                                                BPS%NUM_OBS_ACCUM(J1,1), &
     &                                                BPS%NUM_OBS_ACCUM(J1,2)
            ELSE IF ( PIM%CONF%BPS_MODE == PIMA__BPASS_FINE ) THEN
              WRITE ( UNIT=LUN, FMT='(A,I8,2X,I8)' ) 'PBP INFO NUMBER_OF_USED_OBSERVATIONS:  ', &
     &                                                BPS%NUM_OBS_FINE(J1,1), &
     &                                                BPS%NUM_OBS_FINE(J1,2)
         END IF
!
         IFRQ = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
            IFRQ = IFRQ + 1
            AMPL_AVR = 0.0
            PHAS_AVR = 0.0
            DO 430 J3=1,PIM%NCHN
               IF ( ASSOCIATED ( PIM%PBP(J1)%CMPL ) ) THEN
                    PHAS = PHAS_CMPL_R4 ( PIM%PBP(J1)%CMPL(J3,J2) )
                    AMPL = ABS(PIM%PBP(J1)%CMPL(J3,J2))
                  ELSE
                    PHAS = 0.0
                    AMPL = 1.0
               END IF
               IF ( PHAS > PI__NUM ) PHAS = PHAS - PI2
               PHAS_AVR = PHAS_AVR + PHAS 
               AMPL_AVR = AMPL_AVR + AMPL 
 430        CONTINUE 
!
            AMPL_AVR = AMPL_AVR/PIM%NCHN
            PHAS_AVR = PHAS_AVR/PIM%NCHN
            AMPL_RMS = 0.0
            PHAS_RMS = 0.0
            DO 440 J4=1,PIM%NCHN
               IF ( ASSOCIATED ( PIM%PBP(J1)%CMPL ) ) THEN
                    PHAS = PHAS_CMPL_R4 ( PIM%PBP(J1)%CMPL(J4,J2) )
                    AMPL = ABS(PIM%PBP(J1)%CMPL(J4,J2))
                  ELSE
                    PHAS = 0.0
                    AMPL = 1.0
               END IF
               IF ( PHAS > PI__NUM ) PHAS = PHAS - PI2
               PHAS_RMS = PHAS_RMS + (PHAS - PHAS_AVR)**2
               AMPL_RMS = AMPL_RMS + (AMPL - AMPL_AVR)**2
 440        CONTINUE 
            PHAS_RMS = DSQRT ( PHAS_RMS/PIM%NCHN )
            AMPL_RMS = DSQRT ( AMPL_RMS/PIM%NCHN )
!
            IF ( AMPL_RMS > PIMA__BPASS_AMP_MAX ) AMPL_RMS = PIMA__BPASS_AMP_MAX 
            WRITE ( LUN, 130 ) J2, AMPL_RMS, PHAS_RMS
 130        FORMAT ( 'PBP INFO RMS_RESIDUALS_FREQUENCY_CHANNEL: ',I2, &
     &               ' AMPL: ', F7.5, ' PHAS: ', F8.5 )
 420     CONTINUE 
!
         IFRQ = 0
         DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
            IFRQ = IFRQ + 1
            WRITE ( UNIT=LUN, FMT='(A)' ) '#'
            WRITE ( UNIT=LUN, FMT='(A)' ) '#       Stat     #Chn #frq  Freq. MHz    Ampl          Phas (rad)'
            WRITE ( UNIT=LUN, FMT='(A)' ) '#'
            DO 460 J6=1,PIM%NCHN
               IF ( ASSOCIATED ( PIM%PBP(J1)%CMPL ) ) THEN
                    PHAS = PHAS_CMPL_R4 ( PIM%PBP(J1)%CMPL(J6,J5) )
                    IF ( PHAS > PI__NUM ) PHAS = PHAS - PI2
                    AMPL = MIN ( ABS(PIM%PBP(J1)%CMPL(J6,J5)), PIMA__BPASS_AMP_MAX )
                  ELSE
                    PHAS = 0.0
                    AMPL = 1.0
               END IF
               WRITE ( LUN, 140 ) PIM%C_STA(J1), J6, J5, &
     &                 PIM%FREQ_ARR(J6,J5,PIM%CONF%FRQ_GRP)*1.D-6, &
     &                 AMPL, PHAS
 140           FORMAT ( 'PBP VAL ', A, 1X, I5, 1X, I3, 1X, F13.6, &
     &                           ' AMPL: ', F7.5, ' PHAS: ', F8.5 )
 460        CONTINUE 
 450     CONTINUE 
         WRITE ( UNIT=LUN, FMT='(A)' ) '#'
 410  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__POL_BPASS_LABEL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PBP_WRI  !#!#
