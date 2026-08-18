      PROGRAM    STP_FIL_PARSER_TEST
      IMPLICIT   NONE
      INCLUDE    'stp.i'
      INCLUDE    'astro_constants.i'
      TYPE ( STP__STA_TYPE ) :: STP_STA
      INTEGER*4  IUER, I,I2, J1
      INTEGER*4  N_HOR, N_TSYS, N_GAIN, N_BPSL
      CHARACTER  FIL_STP*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      FIL_STP = '/f1/home/nhabana/data/stp/hn-vlba.stp'     
      STP_STA%STATUS = STP__UNDF
!
      IUER = -1
      CALL STP_FIL_PARSER ( STP_STA, FIL_STP, IUER )
!%%%%%!      CALL STP_COUNT ( N_HOR, N_TSYS, N_GAIN, N_BPSL, FIL_STP, IUER )
!
      WRITE (6,*) '______________________________________' 
      WRITE (6,*) 'Reading from: '
      WRITE (6,*) FIL_STP
      WRITE (6,*) ' '
      WRITE (6,*) 'We counted: '
      WRITE (6,*) 'NHOR:           ', STP_STA%NHOR
      WRITE (6,*) 'NTSYS:          ', STP_STA%NTSYS
      WRITE (6,*) 'NGAIN:          ', STP_STA%NGAIN
      WRITE (6,*) 'NBPSL:          ', STP_STA%NBPSL
      WRITE (6,*) '______________________________________' 
      WRITE (6,*) ' '
      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  From Slew Section  |'
      WRITE (6,*) '_______________________'
      WRITE (6,*) ' '
      WRITE (6,*) 'STATUS:      ', STP_STA%STATUS
      WRITE (6,*) 'STN_NAME:    ', STP_STA%NAME
      WRITE (6,*) 'SHORT_NAME:  ', STP_STA%SHORT_NAME
      WRITE (6,*) 'LAST_UPDATE: ', STP_STA%LAST_UPDATE
      WRITE (6,*) 'COORD:       ', STP_STA%COO(:)
      WRITE (6,*) 'MOUNT_TYPE:  ', STP_STA%MOUNT_TYPE
      WRITE (6,*) 'SLEW_EL:     ', STP_STA%SLEW_RATE_EL/DEG__TO__RAD
      WRITE (6,*) 'SLEW_AZ:     ', STP_STA%SLEW_RATE_AZ/DEG__TO__RAD
      WRITE (6,*) 'ACCL_EL:     ', STP_STA%SLEW_ACCL_EL/DEG__TO__RAD
      WRITE (6,*) 'ACCL_AZ:     ', STP_STA%SLEW_ACCL_AZ/DEG__TO__RAD
      WRITE (6,*) 'TSETTLE_EL:  ', STP_STA%TIME_SETTLE_EL
      WRITE (6,*) 'TSETTLE_AZ:  ', STP_STA%TIME_SETTLE_AZ
      WRITE (6,*) 'EL_MIN:      ', STP_STA%EL_MIN/DEG__TO__RAD
      WRITE (6,*) 'EL_MAX:      ', STP_STA%EL_MAX/DEG__TO__RAD
      WRITE (6,*) 'AZ_RANGE:    ', STP_STA%AZ_RANGE(:)/DEG__TO__RAD
      WRITE (6,*) 'RECORDER:    ', STP_STA%RECORDER
      WRITE (6,*) 'PREOB:       ', STP_STA%PREOB
      WRITE (6,*) 'POSTOB:      ', STP_STA%POSTOB
      WRITE (6,*) 'HOR_EL:  ',(STP_STA%HOR_EL(I)/DEG__TO__RAD,I=1,STP_STA%NHOR)
      WRITE (6,*) 'HOR_AZ:  ',(STP_STA%HOR_AZ(I)/DEG__TO__RAD,I=1,STP_STA%NHOR)
      WRITE (6,*) ' '
      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  From Tsys Section  |'
      WRITE (6,*) '_______________________'
      WRITE (6,*) ' '

      DO 410  J1 = 1, STP_STA%NTSYS
         WRITE (6,*)   ' '
         WRITE (6,*)   'TSYS #:  ', J1
!%%%%%%!         WRITE (6,*)   'MJD_RANGE: ', STP_STA%TSYS(J1)%MJD_RANGE(:)
!%%%%%%!         WRITE (6,*) 'TAI_RANGE: ', STP_STA%TSYS(J1)%TAI_RANGE(:)
         WRITE (6,*)   'DATE(1):  ', MJDSEC_TO_DATE (                   &
     &                               STP_STA%TSYS(J1)%MJD_RANGE(1),         &
     &                               STP_STA%TSYS(J1)%TAI_RANGE(1), IUER )
         WRITE (6,*)   'DATE(2):  ', MJDSEC_TO_DATE (                   &
     &                               STP_STA%TSYS(J1)%MJD_RANGE(2),         &
     &                               STP_STA%TSYS(J1)%TAI_RANGE(2), IUER )
         WRITE (6,*)   'NEL:      ', STP_STA%TSYS(J1)%NEL
         WRITE (6,*)   'TSYS_ELEVS: '
         WRITE (6,102) ( STP_STA%TSYS(J1)%TSYS_ELEV(I)/DEG__TO__RAD,        &
     &                   I=1,STP_STA%TSYS(J1)%NEL )
         WRITE (6,104) 'TSYS_FREQS: ', STP_STA%TSYS(J1)%FRQ_RANGE(:)/1.0D9
!%%%%%%!         WRITE (6,*)   'NPOL:      ', STP_STA%TSYS(J1)%NPOL
         WRITE (6,*)   'POLARIZATIONS:  ', STP_STA%TSYS(J1)%POLS(:)
         WRITE (6,104) 'TCAL_POLVAL:    ', STP_STA%TSYS(J1)%TCAL(:)
         WRITE (6,*)   'TSYS_POLVALS:  '
         DO 420 I = 1,2
            WRITE (6,102) ( STP_STA%TSYS(J1)%TSYS_VALS(I,I2),               &
     &                     I2=1,STP_STA%TSYS(J1)%NPOL )
!%%%            WRITE (6,*) ' '
 420     CONTINUE
         WRITE (6,*)   ' '
 410  CONTINUE
      WRITE (6,*) ' '
      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  From Gain Section  |'
      WRITE (6,*) '_______________________'
      WRITE (6,*) ' '
      DO 411  J1 = 1, STP_STA%NGAIN
         WRITE (6,*) ' '
         WRITE (6,*) 'GAIN #:   ', J1
!%%%%%%!         WRITE (6,*)   'MJD_RANGE:   ', STP_STA%GAIN(J1)%MJD_RANGE(:)
!%%%%%%!         WRITE (6,104) 'TAI_RANGE:   ', STP_STA%GAIN(J1)%TAI_RANGE(:)
         WRITE (6,*) 'DATE(1): ', MJDSEC_TO_DATE(                       &
     &                            STP_STA%GAIN(J1)%MJD_RANGE(1),            &
     &                            STP_STA%GAIN(J1)%TAI_RANGE(1), IUER )
         WRITE (6,*) 'DATE(2): ', MJDSEC_TO_DATE(                       &
     &                            STP_STA%GAIN(J1)%MJD_RANGE(2),            &
     &                            STP_STA%GAIN(J1)%TAI_RANGE(2), IUER )
         WRITE (6,104) 'GAIN_FREQS: ', STP_STA%GAIN(J1)%FRQ_RANGE(:)/1.0D9
!%%%%%%!         WRITE (6,*)   'NEL:         ', STP_STA%GAIN(J1)%NEL
         WRITE (6,*) 'GAIN_ELEVS: '
         WRITE (6,102) ( STP_STA%GAIN(J1)%GAIN_ELEV(I)/DEG__TO__RAD,        &
     &              I=1,STP_STA%GAIN(J1)%NEL )
!%%%%%%!         WRITE (6,*)   'NPOL:      ', STP_STA%GAIN(J1)%NPOL
         WRITE (6,*) 'POLARIZATIONS: ', STP_STA%GAIN(J1)%POLS(:)
         WRITE (6,*) 'GAIN_POLVALS:  '
         DO 421 I = 1,2
            WRITE (6,102) ( STP_STA%GAIN(J1)%GAIN_VALS(I,I2),               &
     &                     I2=1,STP_STA%GAIN(J1)%NPOL )
!%%%            WRITE (6,*) ' '
 421     CONTINUE
 411  CONTINUE
      WRITE (6,*) ' '
      WRITE (6,*) '_______________________'
      WRITE (6,*) '|  From BPSL Section  |'
      WRITE (6,*) '_______________________'
      WRITE (6,*) ' '
      DO 412  J1 = 1, STP_STA%NBPSL
         WRITE (6,*) ' '
         WRITE (6,*) 'BPSL #:  ', J1
!%%%%%%!         WRITE (6,*)   'MJD_RANGE:   ', STP_STA%BPSL(J1)%MJD_RANGE(:)
!%%%%%%!         WRITE (6,104) 'TAI_RANGE:   ', STP_STA%BPSL(J1)%TAI_RANGE(:)
         WRITE (6,*) 'DATE1: ',  MJDSEC_TO_DATE(                        &
     &                           STP_STA%BPSL(J1)%MJD_RANGE(:),             &
     &                           STP_STA%BPSL(J1)%TAI_RANGE(1), IUER )
         WRITE (6,*) 'DATE_2: ',  MJDSEC_TO_DATE(                       &
     &                            STP_STA%BPSL(J1)%MJD_RANGE(2),            &
     &                            STP_STA%BPSL(J1)%TAI_RANGE(2), IUER )
         WRITE (6,*) 'IF_BWIDTH: ', STP_STA%BPSL(J1)%IF_BANDWIDTH/1.0D6
         WRITE (6,104) 'BETA:    ', STP_STA%BPSL(J1)%BETA
 412  CONTINUE
! ----------------------------------------------------------------
 101  FORMAT (10F15.4)
 102  FORMAT (10F9.4)
 103  FORMAT (1P5E15.4)
 104  FORMAT (A,10F17.4)
 105  FORMAT (A5)
 106  FORMAT (A,10F8.3)

      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      END PROGRAM
