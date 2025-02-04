      SUBROUTINE VTD_READ_ANTEX ( VTD, ANTENNA_TYPES, NUM_ANTENNAS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_READ_ANTEX parses an IGS ANTEX file                   *
! *   to import PCO/PCV data for a given GNSS antenna                    *
! *                                                                      *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 26-MAR-2024  VTD_READ_ANTEX   v1.0 (c)  J. Skeens  26-MAR-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      CHARACTER*512, ALLOCATABLE :: BUF(:)
      LOGICAL*1, ALLOCATABLE :: FOUND_TYPES(:)
      CHARACTER  STR*80, START_ANTENNA_STR*16, TYPE_SERNUM_STR*16, METHOD_STR*20, &
     &           DAZI_STR*4, ZENITH_STR*18, NUMFREQ_STR*16, VALID_FROM_STR*10, &
     &           VALID_UNTIL_STR*11, SINEX_CODE_STR*10, COMMENT_STR*7, &
     &           START_FREQ_STR*18, NEU_FREQ_STR*17, END_FREQ_STR*16, NOAZI_STR*5, &
     &           START_FREQRMS_STR*17, END_FREQRMS_STR*15, END_ANTENNA_STR*14, &
     &           DATETIME_STR*26, FMT_STR*20, FREQ*3
      PARAMETER  ( START_ANTENNA_STR = 'START OF ANTENNA' )
      PARAMETER  ( TYPE_SERNUM_STR   = 'TYPE / SERIAL NO' )
      PARAMETER  ( METHOD_STR        = 'METH / BY / # / DATE' )
      PARAMETER  ( DAZI_STR          = 'DAZI' )
      PARAMETER  ( NOAZI_STR         = 'NOAZI' )
      PARAMETER  ( ZENITH_STR        = 'ZEN1 / ZEN2 / DZEN' )
      PARAMETER  ( NUMFREQ_STR       = '# OF FREQUENCIES' )
      PARAMETER  ( VALID_FROM_STR    = 'VALID FROM' )
      PARAMETER  ( VALID_UNTIL_STR   = 'VALID UNTIL' )
      PARAMETER  ( SINEX_CODE_STR    = 'SINEX CODE' )
      PARAMETER  ( COMMENT_STR       = 'COMMENT' )
      PARAMETER  ( START_FREQ_STR    = 'START OF FREQUENCY' )
      PARAMETER  ( NEU_FREQ_STR      = 'NORTH / EAST / UP' )
      PARAMETER  ( END_FREQ_STR      = 'END OF FREQUENCY' )
      PARAMETER  ( START_FREQRMS_STR = 'START OF FREQ RMS' )
      PARAMETER  ( END_FREQRMS_STR   = 'END OF FREQ RMS' )
      PARAMETER  ( END_ANTENNA_STR   = 'END OF ANTENNA' )
      CHARACTER*20  ANTENNA_TYPES(VTD__M_PCO)
      LOGICAL*1  LEX, IN_RECORD, IN_TABLE, TRIGGER_ERROR
      INTEGER*4  POS
      INTEGER*4  NBUF, IOS, J1, J2, J3, J4, IER, YMD(5), MJD, IFRQ, ITYPE, &
     &           TABLE_LINE_NUM, N_ELEV, N_AZI, NUM_FREQS, &
     &           NUM_ANTENNAS
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN, LTM_DIF, ADD_CLIST
      REAL*8     SEC, SEC_MJD, NORTH, EAST, UP, DAZI, DELEV, BOUND_LOW, &
     &           BOUND_HIGH
        
!
! --- Check whether the file really exists
!
      INQUIRE ( FILE=VTD%CONF%FINAM_PHASE_OFFS, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2901, IUER, 'VTD_READ_ANTEX', 'Control file '// &
     &         'for source structure definition '// &
     &         VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Allocate memory for parsing the file
!
      ALLOCATE ( BUF(VTD__M_ATX), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( VTD__M_ATX*VTD__M_STR, STR )
           CALL ERR_LOG ( 2902, IUER, 'VTD_READ_ANTEX', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory' )
           RETURN 
      END IF
!
! --- Allocate memory for PCO objects
!
      ALLOCATE ( VTD%PCO(NUM_ANTENNAS), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 2903, IUER, 'VTD_READ_ANTEX', 'Error in an '// &
     &         'attempt to allocate dynamic memory for PCO object.' )
           RETURN 
      END IF
!
      ALLOCATE ( FOUND_TYPES(NUM_ANTENNAS), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 2904, IUER, 'VTD_READ_ANTEX', 'Error in an '// &
     &         'attempt to allocate dynamic memory for logical array.' )
           RETURN 
      END IF
!
! --- Read the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_PHASE_OFFS, VTD__M_ATX, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2905, IUER, 'VTD_READ_ANTEX', 'Error in an '// &
     &         'attempt to read input file with phase offset and variation '// &
     &         'coeffs '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS)) )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Initializtion
!
      VTD%L_STR = 0
!
! --- Now time came to start parsing this file
!
      IN_RECORD = .FALSE.
      IN_TABLE = .FALSE.
      TABLE_LINE_NUM = 1
      FREQ = '0'
      ITYPE = 0
      DO 410 J1=1,NBUF
!
         IF ( .NOT. IN_RECORD ) THEN
             DO 420 J2=1,NUM_ANTENNAS
                 IF ( INDEX ( BUF(J1)(1:24), TRIM ( ANTENNA_TYPES(J2) ) ) /= 0 .AND. &
                      INDEX ( BUF(J1), TYPE_SERNUM_STR ) /= 0 ) THEN
!
! ------------------ Entering a heading for the right antenna type
!
                     IN_RECORD = .TRUE.            
                     ITYPE = J2
                     VTD%PCO(ITYPE)%ANTENNA_TYPE = ANTENNA_TYPES(J2)
                 ENDIF
 420         CONTINUE

         ELSE IF ( IN_RECORD .EQV. .TRUE. ) THEN
!
! ----------- Continue parsing the record
!
             IF ( INDEX ( BUF(J1), VALID_FROM_STR ) /= 0 ) THEN 
!
! -------------- If satellite, check if this is the PCO for the correct timerange
!           
                 READ ( UNIT=BUF(J1), FMT='(5I6, F13.7, 17X)', IOSTAT=IOS) YMD, SEC
                 WRITE ( DATETIME_STR, '(I4.4, ".", I2.2, ".", I2.2, "-", I2.2, ":", I2.2, ":", F9.6)') &
     &                   YMD(1), YMD(2), YMD(3), YMD(4), YMD(5), SEC
                 IF ( IOS .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2906, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))// &
     &                   '. Cannot read start date of record.')
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF

                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( DATETIME_STR, MJD, SEC_MJD, IER ) 
                 IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2907, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'an attempt to convert record begin datetime to MJD '// &
     &                   'for line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS)))
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF

                 IF ( MJD > VTD%MJD_BEG ) THEN
!
! -------------- Not the right record, continue on
!       
                     IN_RECORD = .FALSE.
                 END IF
                 FOUND_TYPES(ITYPE) = .TRUE.
!
             ELSE IF ( INDEX ( BUF(J1), VALID_UNTIL_STR ) /= 0 ) THEN
                 READ ( UNIT=BUF(J1), FMT='(5I6, F13.7, 17X)', IOSTAT=IOS) YMD, SEC
                 WRITE ( DATETIME_STR, '(I4.4, ".", I2.2, ".", I2.2, "-", I2.2, ":", I2.2, ":", F9.6)') &
     &                   YMD(1), YMD(2), YMD(3), YMD(4), YMD(5), SEC
                 IF ( IOS .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2908, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))// &
     &                   '. Cannot read end date of record.')
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( DATETIME_STR, MJD, SEC_MJD, IER)
                 IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2909, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'an attempt to convert record end datetime to MJD '// &
     &                   'for line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS)))
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
!
                 IF ( MJD < VTD%MJD_BEG ) THEN
!
! -------------- Not the right record, continue on
!       
                     IN_RECORD = .FALSE.
                 END IF
             ELSE IF ( INDEX ( BUF(J1), DAZI_STR ) /= 0 ) THEN
!
! -------------- Record azimuth increment for PCV array
!             
                 READ ( UNIT=BUF(J1), FMT='(2X, F6.1, 52X)', IOSTAT=IOS) DAZI
                 IF ( IOS .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2910, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))//&
     &                   ' for frequency '//FREQ//' and type '//TRIM(ANTENNA_TYPES(ITYPE))// &
     &                   ' -- wrong format of the field DAZI' )
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 VTD%PCO(ITYPE)%DAZI = DAZI
             ELSE IF ( INDEX ( BUF(J1), ZENITH_STR ) /= 0 ) THEN
!
! -------------- Record elevation increment and bounds for PCV array
!           
                 READ ( UNIT=BUF(J1), FMT='(2X, 3F6.1, 52X)', IOSTAT=IOS) BOUND_LOW, BOUND_HIGH, DELEV
                 IF ( IOS .NE. 0 ) THEN
                     CALL ERR_LOG ( 2911, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))// &
     &                   ' for frequency '//FREQ//' and type '//TRIM(ANTENNA_TYPES(ITYPE))// &
     &                   ' -- wrong format of the field DAZI' )
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 VTD%PCO(ITYPE)%DELEV = DELEV
                 VTD%PCO(ITYPE)%BOUND_LOW_ELEV = BOUND_LOW
                 VTD%PCO(ITYPE)%BOUND_HIGH_ELEV = BOUND_HIGH
             ELSE IF ( INDEX ( BUF(J1), NUMFREQ_STR ) /= 0 ) THEN
!
! -------------- Read how many frequencies will be specified, then allocate accordingly
!             
                 READ ( UNIT=BUF(J1), FMT='(I6,54X)', IOSTAT=IOS) NUM_FREQS
                 IF ( IOS .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2912, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))// &
     &                   '. Cannot recognize number of RINEX frequencies')
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
!
                 ALLOCATE ( VTD%PCO(ITYPE)%RINEX_FREQ(NUM_FREQS), STAT=IER )
                 VTD%PCO(ITYPE)%NFREQ = NUM_FREQS
                 IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2913, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'an attempt to allocate memory for RINEX_FREQ array '// &
     &                   'while processing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS)))
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 IFRQ = 0
             ELSE IF ( INDEX ( BUF(J1), START_FREQ_STR ) /= 0 ) THEN
!
! -------------- Reading start of frequency, which frequency?
!             
                 READ ( UNIT=BUF(J1), FMT='(3X, A3, 54X)', IOSTAT=IOS) FREQ
                 IF ( IOS .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2914, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))//&
     &                   '. Cannot recognize start frequency format')
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 IFRQ = IFRQ + 1
                 VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%FREQ_NAME = FREQ
             ELSE IF ( INDEX ( BUF(J1), NEU_FREQ_STR ) /= 0 ) THEN
!
! -------------- Record PCO for frequency (North, East, Up frame)
!             
                 READ ( UNIT=BUF(J1)(1:30), FMT='(3F10.2)', IOSTAT=IOS) NORTH, EAST, UP
                 IF ( IOS .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2915, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))//&
     &                   ' for frequency '//FREQ//' and type '//TRIM(ANTENNA_TYPES(ITYPE))//&
     &                   ' -- wrong format of the field North East Up' )
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%UEN(1) = UP
                 VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%UEN(2) = EAST
                 VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%UEN(3) = NORTH
             ELSE IF ( INDEX ( BUF(J1), NOAZI_STR ) /= 0 .AND. DAZI > 0.0D0 ) THEN
!
! -------------- This is the non-azimuth-dependent line (dont care) 
! -------------- However, next line will be the start of the azimuth/elevation table
!           
                 IN_TABLE = .TRUE.
                 TABLE_LINE_NUM = 1
                 N_ELEV = (BOUND_HIGH - BOUND_LOW) / DELEV + 1
                 N_AZI = 360.0D0 / DAZI + 1
                 CALL ERR_PASS ( IUER, IER )
                 ALLOCATE ( VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%PHASE_PAT(N_AZI, N_ELEV), STAT=IER )
                 VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%NEL = N_ELEV
                 VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%NAZ = N_AZI
                 IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2916, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'an attempt to allocate memory for PCV array '// &
     &                   'while processing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS)))
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 WRITE ( FMT_STR, FMT='(A, I2, A)') '(8X,', N_ELEV, 'F8.2)'
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL VTD_SET_FREQ ( VTD, FREQ, ITYPE, IFRQ, IER )
                 IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2917, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'failure to map RINEX frequency code '//FREQ//' to sky frequency')
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
             ELSE IF ( INDEX ( BUF(J1), NOAZI_STR ) /= 0 .AND. DAZI == 0.0D0 ) THEN
!
! -------------- This is the non-azimuth-dependent line and we do care 
! -------------- because there is no azimuth/elevation table--this is all we get
!            
                 N_ELEV = (BOUND_HIGH - BOUND_LOW) / DELEV
                 WRITE ( FMT_STR, FMT='(A, I2, A)') '(8X,', N_ELEV, 'F8.2)'
                 ALLOCATE ( VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%PHASE_PAT(1,N_ELEV), STAT=IER )
                 VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%NEL = N_ELEV
                 VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%NAZ = 1
                 IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2918, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'an attempt to allocate memory for 1D PCV array '// &
     &                   'while processing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS)))
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 READ ( UNIT=BUF(J1), FMT=FMT_STR, IOSTAT=IOS) &
     &                  VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%PHASE_PAT
                 IF ( IOS .NE. 0 ) THEN
                     CALL ERR_LOG ( 2919, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))// &
     &                   'for frequency '//FREQ//' and type '//TRIM(ANTENNA_TYPES(ITYPE))//&
     &                   ' -- Cannot read NOAZI array' )
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF

                 CALL ERR_PASS ( IUER, IER )
                 CALL VTD_SET_FREQ ( VTD, FREQ, ITYPE, IFRQ, IER )
                 IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2920, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'failure to map RINEX frequency code to sky frequency')
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 TABLE_LINE_NUM = 1
                 N_AZI = 0
!
             ELSE IF ( INDEX ( BUF(J1), END_FREQ_STR ) /= 0  ) THEN
!
! -------------- End of azimuth/elevation table for this frequency 
!           
                 IN_TABLE = .FALSE.
                 IF ( TABLE_LINE_NUM /= N_AZI+1 ) THEN
                     CALL CLRCH ( STR ) 
                     CALL INCH  ( J1, STR )
                     CALL ERR_LOG ( 2921, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))//&
     &                   ' for frequency '//FREQ//' and type '//TRIM(ANTENNA_TYPES(ITYPE))//&
     &                   ' PCV array not filled correctly--skipped or missed a line ' )
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
             ELSE IF ( IN_TABLE .EQV. .TRUE. ) THEN
!
! -------------- Currently reading an azimuth/elevation PCV table 
!           
                 READ ( UNIT=BUF(J1), FMT=FMT_STR, IOSTAT=IOS) &
     &                  VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%PHASE_PAT(TABLE_LINE_NUM,:)
                 IF ( IOS .NE. 0 ) THEN
                     CALL ERR_LOG ( 2922, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &                   'parsing line '//STR(1:I_LEN(STR))//' of the input '//  &
     &                   'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))// &
     &                   'for frequency '//FREQ//' and type '//TRIM(ANTENNA_TYPES(ITYPE))//&
     &                   ' -- Cannot read phase pattern array' )
                     DEALLOCATE ( BUF )
                     RETURN 
                 END IF
                 TABLE_LINE_NUM = TABLE_LINE_NUM + 1
             ELSE IF ( INDEX ( BUF(J1), END_ANTENNA_STR ) /= 0 ) THEN
!
! -------------- End of antenna record, search for new antenna
!           
                 IN_RECORD = .FALSE.
             END IF
         END IF
 410  CONTINUE 
      DO 430 J3=1,NUM_ANTENNAS
         TRIGGER_ERROR = .FALSE.
         IF ( FOUND_TYPES(J3) .NEQV. .TRUE. ) THEN
             CALL ERR_LOG ( 2923, IUER, 'VTD_READ_ANTEX', 'Error in '//  &
     &           'finding antenna PCO tables for the input '//  &
     &           'ANTEX file '//VTD%CONF%FINAM_PHASE_OFFS(1:I_LEN(VTD%CONF%FINAM_PHASE_OFFS))//  &
     &           ' Could not find antenna type '//ANTENNA_TYPES(J3) )
             TRIGGER_ERROR = .TRUE.
         END IF 
 430  CONTINUE

      DEALLOCATE ( BUF )
      IF ( TRIGGER_ERROR .EQV. .TRUE. ) THEN
          RETURN
      ENDIF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_READ_ANTEX  !#!#

      SUBROUTINE VTD_SET_FREQ ( VTD, FREQ_CHAR, ITYPE, IFRQ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_SET_FREQ assigns a frequency in HZ                    *
! *   from a RINEX frequency string                                      *
! *                                                                      *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 26-MAR-2024  VTD_READ_ANTEX   v1.0 (c)  J. Skeens  26-MAR-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      CHARACTER  FREQ_CHAR*3
      LOGICAL*4  LEX, IN_RECORD, IN_TABLE
      INTEGER*4  ITYPE, IFRQ
      REAL*8     FREQ_HZ

      IF ( FREQ_CHAR == 'G01' .OR. FREQ_CHAR == 'E01' .OR. FREQ_CHAR == 'J01' &
     &     .OR. FREQ_CHAR == 'S01' .OR. FREQ_CHAR == 'C01' .OR. FREQ_CHAR == 'C02' ) THEN
          FREQ_HZ = 1.57542D9
      ELSE IF ( FREQ_CHAR == 'G02' .OR. FREQ_CHAR == 'J02' ) THEN
          FREQ_HZ = 1.22760D9
      ELSE IF ( FREQ_CHAR == 'G05' .OR. FREQ_CHAR == 'E05' .OR. FREQ_CHAR == 'J05' &
     &     .OR. FREQ_CHAR == 'C05' .OR. FREQ_CHAR == 'S05' .OR. FREQ_CHAR == 'I05' ) THEN
          FREQ_HZ = 1.117645D9 
      ELSE IF ( FREQ_CHAR == 'R01' .OR. FREQ_CHAR == 'R04' ) THEN
          FREQ_HZ = 1.602D9
      ELSE IF ( FREQ_CHAR == 'R02' .OR. FREQ_CHAR == 'R06' ) THEN
          FREQ_HZ = 1.246D9
      ELSE IF ( FREQ_CHAR == 'E06' .OR. FREQ_CHAR == 'J06' .OR. FREQ_CHAR == 'C06' ) THEN
          FREQ_HZ = 1.27875D9
      ELSE IF ( FREQ_CHAR == 'E07' .OR. FREQ_CHAR == 'C07' ) THEN
          FREQ_HZ = 1.20714D9
      ELSE IF ( FREQ_CHAR == 'E08' .OR. FREQ_CHAR == 'C08' ) THEN
          FREQ_HZ = 1.191795D9
      ELSE
          CALL ERR_LOG ( 2924, IUER, 'VTD_SET_FREQ', 'Error in '//  &
     &        ' assigning RINEX frequency to sky frequency '//  &
     &        ' unknown ANTEX frequency code ' //FREQ_CHAR )
          RETURN 
      END IF

      VTD%PCO(ITYPE)%RINEX_FREQ(IFRQ)%FREQ = FREQ_HZ
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_SET_FREQ  !#!#
