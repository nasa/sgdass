      SUBROUTINE PIMA_WRITE_BPASS ( PIM, BPS, SNR_MIN, SEFD_USE, &
     &                              DEG_AMP, DEG_PHS, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_WRITE_BPASS 
! *                                                                      *
! * ### 26-JAN-2009 PIMA_WRITE_BPASS v1.4  (c) L. Petrov 03-DEC-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE  
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE         ) :: PIM
      TYPE     ( PIM_BPS_STA__TYPE  ) :: BPS
      REAL*8     SNR_MIN, BPS_AMP_MAX
      CHARACTER  SEFD_USE*(*), FILOUT*(*)
      INTEGER*4  DEG_AMP, DEG_PHS, IUER
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, IFRQ, IER, LUN
      REAL*4     PHAS
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LTM_DIF
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 6371, IUER, 'PIMA_WRITE_BPASS', 'Failure to open '// &
     &         'the output file '//FILOUT(1:I_LEN(FILOUT))//' for writing '// &
     &         'the bandpass. Error code: '//STR )
           RETURN 
      END IF
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__BPASS_STA_LABEL
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 6371, IUER, 'PIMA_WRITE_BPASS', 'Failure to write '// &
     &         'the file label in the output file '//FILOUT(1:I_LEN(FILOUT))// &
     &         ' for the bandpass. Error code: '//STR )
           RETURN 
      END IF
!
      STR = GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Created at '//STR(1:19)
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A, I8)' ) 'BPASS_STA INFO '// &
     &                                  'GENERATED_BY_PIMA_VERSION: ', BPS%PIMA_VERS
      WRITE ( UNIT=LUN, FMT='(A, I3)' ) 'BPASS_STA INFO '// &
     &                                  'NUMBER_OF_STATIONS:             ', PIM%NSTA
      WRITE ( UNIT=LUN, FMT='(A, I3)' ) 'BPASS_STA INFO '// &
     &                                  'FREQUENCY_GROUP:                ', BPS%IFRG
      WRITE ( UNIT=LUN, FMT='(A, A8)' ) 'BPASS_STA INFO '// &
     &                                  'POLARIZATION:                    ', BPS%POLAR
      WRITE ( UNIT=LUN, FMT='(A, A)'  ) 'BPASS_STA INFO REFERENCE_STATION:         ', &
     &                                   PIM%C_STA(BPS%IND_STA_REF)
      WRITE ( UNIT=LUN, FMT='(A, F8.2)' ) 'BPASS_STA INFO SNR_MIN:                   ', &
     &                                        SNR_MIN
      WRITE ( UNIT=LUN, FMT='(A, I2)'   ) 'BPASS_STA INFO AMPL_POLYNOMIAL_DEGREE:          ', &
     &                                     DEG_AMP
      WRITE ( UNIT=LUN, FMT='(A, I2)'   ) 'BPASS_STA INFO PHAS_POLYNOMIAL_DEGREE:          ', &
     &                                     DEG_PHS
      WRITE ( UNIT=LUN, FMT='(A, A)'    ) 'BPASS_STA INFO METHOD:                        ', &
     &                                     BPS%STATUS
      WRITE ( UNIT=LUN, FMT='(A, A)'    ) 'BPASS_STA INFO PCAL_CODE:                  ', &
     &                                     PIM%CONF%PHAS_CAL_CODE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      DO 410 J1=1,PIM%NSTA
         WRITE ( UNIT=LUN, FMT='(A,A)' ) 'BPASS_STA INFO STATION: ', PIM%C_STA(J1)
         WRITE ( UNIT=LUN, FMT='(A)'   ) '                        ########'
         WRITE ( UNIT=LUN, FMT='(A,I8)' ) 'BPASS_STA INFO NUMBER_OF_FREQUENCY_CHANNELS: ', &
     &                                    PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
         WRITE ( UNIT=LUN, FMT='(A,I8)' ) 'BPASS_STA INFO NUMBER_OF_SPECTRAL_CHANNELS:  ', &
     &                                    PIM%NCHN
         IF ( PIM%CONF%BPS_MODE == PIMA__BPASS_INIT  .OR. &
     &        PIM%CONF%BPS_MODE == PIMA__BPASS_ACCUM      ) THEN
              WRITE ( UNIT=LUN, FMT='(A,I8,1X,I8)' ) 'BPASS_STA INFO NUMBER_OF_USED_OBSERVATIONS:  ', &
     &                                                BPS%NUM_OBS_ACCUM(J1,1), &
     &                                                BPS%NUM_OBS_ACCUM(J1,2)
            ELSE IF ( PIM%CONF%BPS_MODE == PIMA__BPASS_FINE ) THEN
              WRITE ( UNIT=LUN, FMT='(A,I8,1X,I8)' ) 'BPASS_STA INFO NUMBER_OF_USED_OBSERVATIONS:  ', &
     &                                                BPS%NUM_OBS_FINE(J1,1), &
     &                                                BPS%NUM_OBS_FINE(J1,2)
         END IF
         WRITE ( UNIT=LUN, FMT='(A)' ) '#'
         WRITE ( UNIT=LUN, FMT='(A,F8.5)' ) 'BPASS_STA INFO INTEGRAL_FREQ_CHAN_AMPLITUDE: ', BPS%AMPL_INTEGRAL(J1)
         WRITE ( UNIT=LUN, FMT='(A,F8.5)' ) 'BPASS_STA INFO TOTAL_AMPLITUDE_RMS:          ', BPS%AMPL_TOT_RMS(J1)
         WRITE ( UNIT=LUN, FMT='(A,F8.5)' ) 'BPASS_STA INFO TOTAL_PHASE RMS:              ', BPS%PHAS_TOT_RMS(J1)
         WRITE ( UNIT=LUN, FMT='(A,1PD13.6)' ) 'BPASS_STA INFO BANDPASS_GROUP_DELAY:         ', PIM%BPASS(J1)%BPS_MB_GRDEL
         WRITE ( UNIT=LUN, FMT='(A,1PD13.6)' ) 'BPASS_STA INFO PCAL_MB_GROUP_DELAY:          ', BPS%PCAL_MB_GRDEL(J1)
!
         IFRQ = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
            IFRQ = IFRQ + 1
            WRITE ( LUN, 110 ) J2, PIM%FREQ_ARR(1,J2,PIM%CONF%FRQ_GRP)*1.D-6, &
     &                         PIM%BPASS(J1)%PHS_RATE(J2) 
 110        FORMAT ( 'BPASS_STA INFO PHAS_RATE_WITH_FREQUENCY: ',7X, I2, &
     &               ' FREQ: ', F13.6, 1X, ' RATE: ', 1PD12.5 )
 420     CONTINUE 
!
         IFRQ = 0
         DO 430 J3=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
            IFRQ = IFRQ + 1
            WRITE ( LUN, 120 ) J3, PIM%FREQ_ARR(1,J3,PIM%CONF%FRQ_GRP)*1.D-6, &
     &                             BPS%PCAL_SB_GRDEL(J3,J1)
 120        FORMAT ( 'BPASS_STA INFO PCAL_SB_GROUP_DELAY:      ', 7X, I2, &
     &               ' FREQ: ', F13.6, 1X, ' DEL:  ', 1PD12.5 )
 430     CONTINUE 
!
         IFRQ = 0
         DO 440 J4=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
            IFRQ = IFRQ + 1
            WRITE ( LUN, 130 ) J4, BPS%AMPL_FRQ_AVR(J4,J1), &
     &                             BPS%PHAS_FRQ_AVR(J4,J1)
 130        FORMAT ( 'BPASS_STA INFO AVR_RESIDUALS_FREQUENCY_CHANNEL: ',I2, &
     &               ' AMPL: ', F7.5, ' PHAS: ', F8.5 )
 440     CONTINUE 
!
         IFRQ = 0
         DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
            IFRQ = IFRQ + 1
            IF ( BPS%AMPL_FRQ_RMS(J5,J1) > PIMA__BPASS_AMP_MAX ) BPS%AMPL_FRQ_RMS(J5,J1) = PIMA__BPASS_AMP_MAX 
            IF ( BPS%PHAS_FRQ_RMS(J5,J1) > PIMA__BPASS_AMP_MAX ) BPS%PHAS_FRQ_RMS(J5,J1) = PIMA__BPASS_AMP_MAX
            WRITE ( LUN, 140 ) J5, &
     &              MIN( BPS%AMPL_FRQ_RMS(J5,J1), PIMA__BPASS_AMP_MAX ), &
     &              BPS%PHAS_FRQ_RMS(J5,J1)
 140        FORMAT ( 'BPASS_STA INFO RMS_RESIDUALS_FREQUENCY_CHANNEL: ',I2, &
     &               ' AMPL: ', F7.5, ' PHAS: ', F8.5 )
 450     CONTINUE 
         WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
         IFRQ = 0
         DO 460 J6=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ 
            IFRQ = IFRQ + 1
            WRITE ( UNIT=LUN, FMT='(A)' ) '#'
            WRITE ( UNIT=LUN, FMT='(A)' ) '#             Stat     #Chn #frq  Freq. MHz    Ampl          Phas (rad)'
            WRITE ( UNIT=LUN, FMT='(A)' ) '#'
            BPS_AMP_MAX = -1.D8
            DO 470 J7=1,PIM%NCHN
               BPS_AMP_MAX = MAX ( BPS_AMP_MAX, ABS( BPS%CMPL(J7,J6,J1) ) )
 470        CONTINUE 
!
            DO 480 J8=1,PIM%NCHN
               IF ( BPS_AMP_MAX < PIMA__BPS_AMP_SPLN_MIN ) THEN
                    BPS%CMPL(J8,J6,J1) = 1.0
               END IF
               PHAS = PHAS_CMPL_R4 ( BPS%CMPL(J8,J6,J1) )
               IF ( PHAS > PI__NUM ) PHAS = PHAS - PI2
               WRITE ( LUN, 150 ) PIM%C_STA(J1), J8, J6, &
     &                 PIM%FREQ_ARR(J8,J6,PIM%CONF%FRQ_GRP)*1.D-6, &
     &                 MIN ( ABS(BPS%CMPL(J8,J6,J1)), PIMA__BPASS_AMP_MAX ), &
     &                 PHAS
 150           FORMAT ( 'BPASS_STA VAL ', A, 1X, I5, 1X, I3, 1X, F13.6, &
     &                           ' AMPL: ', F7.5, ' PHAS: ', F8.5 )
 480        CONTINUE 
 460     CONTINUE 
 410  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__BPASS_STA_LABEL
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PIMA_WRITE_BPASS  !#!#
