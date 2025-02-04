      PROGRAM GPS_IONO
! ************************************************************************
! *                                                                      *
! *   Program GPS_IONO
! *                                                                      *
! *  ### 17-MAY-2010    GPS_IONO   v1.0 (c)  L. Petrov  17-MAY-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'socom.i'
      INCLUDE    'oborg.i'
      INCLUDE    'prfil.i'
      INCLUDE    'vtd.i'
      LOGICAL*4   LEX
      CHARACTER   STR*80, VTD_CONF_FILE*128
      INTEGER*2   LDBNAM(5,15), IDBV(15)
      CHARACTER   CDBNAM(15)*10, C_STA(MAX_ARC_STA)*8
      EQUIVALENCE (CDBNAM,LDBNAM(1,1))
      TYPE      ( VTD__TYPE     ) :: VTD
      TYPE      ( VTD__OBS_TYPE ) :: OBS_TYP
      LOGICAL*1   FL_IONO_GPS
      REAL*8      PRES_VAL, TEMP_VAL, UTC_OBS, TAI_OBS, UTC_MINUS_TAI, &
     &            DELAY_THR, RATE_THR, DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), &
     &            VTD_IONO_SCALE
      INTEGER*4   UMC_SLOT, IDBE(15), NN, NOBS, MJD_UTC_OBS, MJD_TAI_OBS, &
     &            J1, J2, J3, ISTA, IUER
      INTEGER*4,  EXTERNAL :: ILEN, I_LEN
!
      CALL PRE_PROG()
!
! --- Reading common area
!
      CALL USE_PARFIL   ( 'ORC' )  ! Reading  parfil
      CALL USE_COMMON   ( 'ORC' )  ! Reading  socom.i
      CALL SOCOM_EXT()
!
! --- Getting database name
!
      CALL DBPOX  ( NUMDB, LDBNAM, IDBV, IDBE )
      NOBS = IDBE(1)
!!  write ( 6, * ) ' DBNAME_CH = ', DBNAME_CH
!!  write ( 6, * ) ' NOBS= ', NOBS
      CALL GETENVAR ( 'GPS_IONO_VTD', VTD_CONF_FILE )
      IF ( ILEN(VTD_CONF_FILE) == 0 ) THEN 
           WRITE ( 6, '(A)'  ) 'Environoment variable GPS_IONO_VTD is not set up...'
           WRITE ( 6, '(A$)' ) 'Name of the VTD file >> '
           READ  ( 5, '(A)'  ) VTD_CONF_FILE
      END IF
      INQUIRE ( FILE=VTD_CONF_FILE, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1601, -2, 'GPS_IONO', 'Cannot find VTD file '// &
     &                    VTD_CONF_FILE )
           CALL EXIT ( 1 )
      END IF
      IUER = -1
      CALL PRE_VTD ( VTD, VTD%STATUS, VTD_CONF_FILE, FL_IONO_GPS, VTD_IONO_SCALE, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1602, -2, 'GPS_IONO', 'Failure in an attempt '// &
     &         'to initialize VTD' )
           CALL EXIT ( 1 )
      END IF
      DO ISTA = 1, NUMSTA
         C_STA(ISTA) = ISITN_CHR(ISTA)
         CALL VTD_NAME_REPAIR ( C_STA(ISTA) )
      END DO
!
! --- Getting a slot for User Mode Calibrations and modify NAMFIL if necessary
!
      IUER = -1
      CALL GET_UMC_SLOT ( DBNAME_CH, UMC_SLOT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1603, -2, 'GPS_IONO', 'Error in getting a slot for '// &
     &         'user mode calibration when superfile '//DBNAME_CH// &
     &         ' was processed' )
           CLOSE ( UNIT=11 )
           STOP 'USERMCAL'
      END IF
!
! --- Open OBS-file
!
      IDATYP = GX__DTP 
      CALL ACS_OBSFIL ( 'O' )
      DO 410 J1=1,NOBS ! cycle on observations
!
! ------ Reading oborg
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
         CALL JD_TO_MJD_SEC  ( FJD, MJD_UTC_OBS, UTC_OBS )
         UTC_OBS = UTC_OBS + FRACT*86400.0D0
!
! ------ Load meteorological parameters of the first station
! ------ into the VTD record
!
         PRES_VAL = ATMPR(1)*100.0D0
         TEMP_VAL = TEMPC(1) + 273.16D0
         IUER = -1
         CALL VTD_METEO_IN ( VTD, C_STA(ISITE(1)), PRES_VAL, TEMP_VAL, &
     &                       TEMP_VAL, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 1604, -2, 'GPS_IONO', 'Error in an '// &
     &            'attempt to load meteorological parameters '// &
     &            'for station '//C_STA(ISITE(1)) )
              CALL EXIT ( 1 )
         END IF
!
! ------ Load meteorologial parameters for the second station
!
         PRES_VAL = ATMPR(2)*100.0D0
         TEMP_VAL = TEMPC(2) + 273.16D0
         IUER = -1
         CALL VTD_METEO_IN ( VTD, C_STA(ISITE(2)), PRES_VAL, TEMP_VAL, &
     &                       TEMP_VAL, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 1605, -2, 'GPS_IONO',  'Error in an '// &
     &            'attempt to load meteorological parameters '// &
     &            'for station '//C_STA(ISITE(2)) )
              CALL EXIT ( 1 )
         END IF
!
! ------ NB: we need to transform FJD+FRACTC by parts, otherwise
! ------ we will lose precision whcih will result in an additional
! ------ noise in delay with rms 10-15 ps!!!
!
         IF ( J1 == 1 ) THEN
!
! ----------- This trick is done since VLBI formatter stores pseudo-UTC.
! ----------- We need to record UTC-TAI(t) at the beginning of the
! ----------- experiment and apply it to all observations, regardless
! ----------- whether the new clock jump took place duing the experiment
!
              CALL VTD_UTC_TO_TAI ( VTD, MJD_UTC_OBS, UTC_OBS, TAI_OBS, IUER )
              IF ( IUER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1606, IUER, 'GPS_IONO', 'Error in an '// &
     &                         'attempt to get UTC-minus TAI' )
                   CALL EXIT ( 1 )
              END IF
              UTC_MINUS_TAI = UTC_OBS - TAI_OBS
              IF ( UTC_MINUS_TAI >  43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI - 86400.0D0
              IF ( UTC_MINUS_TAI < -43200.0D0 ) UTC_MINUS_TAI = UTC_MINUS_TAI + 86400.0D0
         END IF
         TAI_OBS = UTC_OBS - UTC_M_TAI
         IF ( TAI_OBS < 0.0D0 ) THEN
              TAI_OBS = TAI_OBS + 86400.0D0
            MJD_TAI_OBS = MJD_UTC_OBS - 1
              ELSE
              MJD_TAI_OBS = MJD_UTC_OBS
         END IF
!
! ------ Set fields of OBS_TYP
!
         CALL SET_OBSTYP ( OBS_TYP )
!
         IUER = -1
         CALL VTD_DELAY ( ISTRN_CHR(ISTAR), C_STA(ISITE(1)), &
     &                    C_STA(ISITE(2)), MJD_TAI_OBS, TAI_OBS, &
     &                    OBS_TYP, VTD, DELAY_THR, &
     &                    RATE_THR, DER_DEL, DER_RAT, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 1607, IUER, 'GPS_IONO', 'Error in an '// &
     &            'attempt to compute VLBI time delay' )
              CALL EXIT ( 1 )
         END IF
!
! ------ Putting ionosphere path delay calibration in the appropriate
! ------ slot ot CALIBM
!
         CALIBM(MCL__GRX,UMC_SLOT) = -DER_DEL(VTD__IONO1) + DER_DEL(VTD__IONO2)
         CALIBM(MCL__RTX,UMC_SLOT) = -DER_RAT(VTD__IONO1) + DER_RAT(VTD__IONO2)
         CALIBM(MCL__PHX,UMC_SLOT) = -CALIBM(MCL__GRX,UMC_SLOT)*(EFFREQ/PHEFFREQ)**2
         IF ( EFFREQ_S > 1.0D0 ) THEN
              CALIBM(MCL__GRS,UMC_SLOT) =  CALIBM(MCL__GRX,UMC_SLOT)*(EFFREQ/EFFREQ_S)**2
              CALIBM(MCL__RTS,UMC_SLOT) =  CALIBM(MCL__GRX,UMC_SLOT)*(EFFREQ/PHEFFREQ_S)**2
              CALIBM(MCL__PHS,UMC_SLOT) = -CALIBM(MCL__GRX,UMC_SLOT)*(REFFREQ/REFFREQ_S)**2
            ELSE 
              CALIBM(MCL__GRS,UMC_SLOT) = 0.0D0
              CALIBM(MCL__RTS,UMC_SLOT) = 0.0D0
              CALIBM(MCL__PHS,UMC_SLOT) = 0.0D0
         END IF 
!
! ------ Writing oborg
!
         CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410  CONTINUE
!
! --- Closing OBS-file
!
      CALL ACS_OBSFIL ( 'C' )
!
      CALL END_PROG()
      END  PROGRAM   GPS_IONO  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_UMC_SLOT ( DBNAME, UMC_SLOT, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiallry routine  GET_UMC_SLOT  examines calibration status,      *
! *   return a slot where user mode calibrations can be put. It set bit  *
! *   avialable and applied to user mode calibrations and then write     *
! *   calibration status back to NAMFIL.                                 *
! *                                                                      *
! *  ###  19-NOV-99  GET_UMC_SLOT  v1.0  (c)  L. Petrov  19-NOV-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'cals.i'
      INTEGER*4  UMC_SLOT, IUER
      CHARACTER  DBNAME*(*)
!
      TYPE ( CALS_STRU ) ::  CAL
      INTEGER*4  IVRB, J1, IER
!
      IVRB = 0
!
! --- Reading calibration status
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R ( INT2(1), IVRB, 0, CAL, IER)
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6511, IUER, 'GET_UMC_SLOT ', 'Error in reading '// &
     &         'calibration information while database '//DBNAME// &
     &         ' was processed' )
           RETURN
      END IF
!
! --- Learn the index of "User mode Calibration"
!
      UMC_SLOT = 0
      IF ( CAL%L_MCAL .GT. 0 ) THEN
           DO 410 J1=1,CAL%L_MCAL
              IF ( CAL%MCAL(J1) .EQ. 'GPS_Iono' ) UMC_SLOT = J1
 410       CONTINUE
      END IF
!
      IF ( UMC_SLOT .EQ. 0 ) THEN
!
! -------- User Mode Calibration has not been found
!
           IF ( CAL%L_MCAL .EQ. M_CLM ) THEN
                CALL ERR_LOG ( 6512, IUER, 'GET_UMC_SLOT ', 'There is no '// &
     &              'free slot for User Mode Calibrations for the database '// &
     &              DBNAME )
                RETURN
           END IF
!
! -------- Create it
!
           CAL%L_MCAL = CAL%L_MCAL + 1
           UMC_SLOT = CAL%L_MCAL
           CAL%MCAL(UMC_SLOT) = 'GPS_Iono'
           CAL%MCAL_LCODE(UMC_SLOT) = '        '
      END IF
!
! --- Setting status "available" and "applied" for user mode calibration
!
      CAL%MCAL_AVL(UMC_SLOT) = .TRUE.
      CAL%MCAL_APL(UMC_SLOT) = .TRUE.
!
! --- Writing calibration status
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_W ( INT2(1), CAL, IER)
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6513, IUER, 'GET_UMC_SLOT ', 'Error in writing '// &
     &         'calibration information while database '//DBNAME// &
     &         ' was processed' )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_UMC_SLOT  #!#
