      SUBROUTINE  STP_PARSER ( STP, FIL_STP, IUER )
!
! ***************************************************************************
! *                                                                         *
! *   Routine STP_PARSER parses the VLBI station parameters                 *
! *                                                                         *
! *   INPUT:                                                                *
! *            FIL_STP    =  Station Parameter File      { CHAR }           *
! *                          N.B: The formating of this file is not         *
! *                               subject to international standardization  *
! *                               and the parsing is based on out format    *
! *                               preferences.                              *
! *                               See an example of a .stp file here to     *
! *                               see our formating.                        *
! *                                                                         *
! *            IUER      =  Error Handler                { INT, OPT }       *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                         Default, IUER = -1                              *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            STP       =  Parsed Object                { DERIVED TYPE }   *
! *                         For more on the parsed data, see stp.i, and     *
! *                         edit it accordingly to include more data        *
! *                         blocks.                                         *
! *                                                                         *
! *  ### 29-JUL-2020  STP_PARSER   v1.0 (c)  N. Habana  29-JUL-2020 ###     *
! *                                                                         *
! *  ### 20-OCT-2020  STP_PARSER   v2.0 (c)  N. Habana  20-OCT-2020 ###     *      
! *    - Deleted the provisions that for each file there is only            *
! *      one date for all Gain inputs.                                      *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT    NONE
      INCLUDE     'stp.i'
      INCLUDE     'astro_constants.i'
      TYPE ( STP__TYPE ) :: STP
      CHARACTER   FIL_STP*(*)                   
      CHARACTER   DELIM*5                       ! Deliminator
      INTEGER*4   MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4   MAXL_STRING                   ! Max. String length
!***!      INTEGER*4  N_SECTS                       ! No. of Sections
!***!      PARAMETER  ( N_SECTS = 4 )
      PARAMETER   ( MAXL_STRING = 256 )           
      PARAMETER   ( MP = 128*1024 )             
      PARAMETER   ( MIND = 128 )                 
      PARAMETER   ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'='//':' ) 
      CHARACTER   BUF(MP)*(MAXL_STRING)         ! Read File
!***!      CHARACTER SECTS(N_SECTS)*32
      INTEGER*4   NP, LIND, IND(2,MIND), LN
      INTEGER*4   IUER, IER
      INTEGER*4   I0, I1
      INTEGER*4   J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10
      INTEGER*4   NDATES(3), NUMI(2)
      REAL*8      NUMR(2)
      CHARACTER   STR(2)*32
      LOGICAL*1   FL_IVS, FL_ID, FL_UD, FL_COO, FL_MNT, FL_SAZ, FL_SEL
      LOGICAL*1   FL_AAZ, FL_AEL, FL_DAZ, FL_DEL, FL_TAZ, FL_TEL
      LOGICAL*1   FL_EMIN, FL_EMAX, FL_AZR, FL_REC, FL_PRE, FL_POST
      LOGICAL*1   FL_HAZ, FL_HEL
      INTEGER*4,  EXTERNAL :: ILEN
#ifdef GNU
      INTEGER*4,  EXTERNAL :: STP_COMPAR_TSYS, STP_COMPAR_GAIN
#else
      INTEGER*2,  EXTERNAL :: STP_COMPAR_TSYS, STP_COMPAR_GAIN
#endif
!
! --- Reading the Station Parameter File to variable, BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_STP, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5001, IUER, 'STP_PARSER', 'Error in reading ' &
     &          //'input file: '//FIL_STP )
           RETURN
      END IF
      IF ( STP%STATUS .NE. STP__INIT ) CALL STP_CLEAN ( STP )
!
! --- Counting the No. of horizontal masks, TSYS, GAIN, and BPSL
!
      CALL ERR_PASS ( IUER, IER )
      CALL STP_COUNT ( STP%NHOR, STP%NTSYS, STP%NGAIN, STP%NBPSL,       &
     &                 FIL_STP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5002, IUER, 'STP_PARSER', 'Error in counting '// &
     &         'parameters in the input file: '//FIL_STP )
           RETURN
      END IF
!
! --- Allocating the (null pointed) structures found in STP__TYPE to 
!     variables.
!     Array sizes based on the counts derived from above
!
      ALLOCATE ( STP%TSYS(STP%NTSYS),  STAT = IER )
      ALLOCATE ( STP%GAIN(STP%NGAIN),  STAT = IER ) 
      ALLOCATE ( STP%BPSL(STP%NBPSL),  STAT = IER )
      ALLOCATE ( STP%HOR_AZ(STP%NHOR), STAT = IER )
      ALLOCATE ( STP%HOR_EL(STP%NHOR), STAT = IER )
! ---
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 5003, IUER, 'STP_PARSER', 'Failed to allocate ' &
     &              //'the following STP pointers: STP%TSYS, STP%GAIN,' &
     &              //' STP%BPSL, STP%HOR_AZ, and STP%HOR_EL.' )
      END IF
! ---
      DO I0 = 1, STP%NTSYS
!
! ------ Allocate the TSYS_VALS pointer
!
         ALLOCATE ( STP%TSYS(I0)%TSYS_VALS(2,STP__MPOL), STAT=IER )
      END DO
! ---
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 5004, IUER, 'STP_PARSER', 'Failed to allocate ' &
     &              //'STP%TSYS(:)%TSYS_VALS.' )
      END IF
! ---
      DO I0 = 1, STP%NGAIN
!
! ------ Allocate the GAIN_VALS pointer.
!
         ALLOCATE ( STP%GAIN(I0)%GAIN_VALS(2,STP__MPOL), STAT=IER )
      END DO
! ---
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 5005, IUER, 'STP_PARSER', 'Failed to allocate ' &
     &              //'the STP%GAIN(:)%GAIN_VALS.' )
      END IF
!***!!
!***!! --- Section List in the prefered order
!***!! --- N.B: There is no interdependency between blocks, therefore the 
!***!!          order does not matter as much, but we would still like to
!***!!          parse, as if it does.
!***!!
!***!      SECTS(1)  = 'GENERAL'
!***!      SECTS(2)  = 'TSYS'
!***!      SECTS(3)  = 'BPSL'
!***!      SECTS(4)  = 'GAIN'
!***!!***!!***!!***!!***!!***!!***!!***!!***!!***!!***!!***!!***!!***!!***!
!
!
! --- Initialise the flags
!
      FL_IVS   = .FALSE.        ! IVS SHORT AND ID NAME
      FL_UD    = .FALSE.        ! Last update
      FL_COO   = .FALSE.        ! Stn Coordinates
      FL_MNT   = .FALSE.        ! Mount type
      FL_SAZ   = .FALSE.        ! Antenna Slew Az.
      FL_SEL   = .FALSE.        ! Antenna Slew El.
      FL_AAZ   = .FALSE.        ! Antenna Accel. Az.
      FL_AEL   = .FALSE.        ! Antenna Accel. El.
      FL_DAZ   = .FALSE.        ! Antenna Decel. Az.
      FL_DEL   = .FALSE.        ! Antenna Decel. El.
      FL_TAZ   = .FALSE.        ! Antenna settle time Az.
      FL_TEL   = .FALSE.        ! Antenna settle time El.
      FL_EMIN  = .FALSE.        ! Min. Elevation angle
      FL_EMAX  = .FALSE.        ! Max. Elevation angle
      FL_AZR   = .FALSE.        ! Az. Range
      FL_REC   = .FALSE.        ! Recorder type
      FL_PRE   = .FALSE.        ! Preob allotted time 
      FL_POST  = .FALSE.        ! Postob allotted time
      FL_HAZ   = .FALSE.        ! Elevation Mask Az.
      FL_HEL   = .FALSE.        ! Elevation Mask El.
!
! --- Initialise some counters
!
      J6 = 0                    ! Gain Counter
! ---      
      DO 410 J1 = 1, NP
!
! ------ Bypass empty lines
!
         IF ( ILEN(BUF(J1)) == 0 )  GOTO 410
!
! ------ Bypass comment lines.
!
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
!
! ------ Extract words from the read line
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! ------ Handle the "General" Section
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SHORT_NAME' ) THEN
            FL_IVS = .TRUE. 
!
! --------- Get IVS name (in Caps)
!
            CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)), STP%NAME )
!
! --------- Get Station ID (in lower case)
!
            CALL TRAN ( 12, BUF(J1)(IND(1,4):IND(2,4)), STP%SHORT_NAME )
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LAST_UPDATE' ) THEN
            FL_UD = .TRUE.
!
! --------- Get date of last modification [YYYY.MM.DD]
! --------- N.B: We have ":" as a deliminator, therefore to avoid this 
!                being a actor in the date collection, we just use the 
!                word from the first "Y"+9 characters.
!
            STP%LAST_UPDATE = BUF(J1)(IND(1,4):IND(1,4)+9)
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'COORD' ) THEN
            FL_COO = .TRUE.
!
! --------- Get the Station Coordinates
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F15.6)',      &
     &             IOSTAT=IER )  STP%COO(1)                ! X [m]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F15.6)',      &
     &             IOSTAT=IER )  STP%COO(2)                ! Y [m]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F15.6)',      &
     &             IOSTAT=IER )  STP%COO(3)                ! Z [m]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MOUNT' ) THEN
            FL_MNT = .TRUE.
!
! --------- Get the Mount type Code
!
            STP%MOUNT_TYPE = BUF(J1)(IND(1,4):IND(2,4))
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SLEW_AZ' ) THEN
            FL_SAZ = .TRUE.
!
! --------- Get the Antenna slewing rate on azimuth axis
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%SLEW_RATE_AZ              ! [deg/s]
            STP%SLEW_RATE_AZ = STP%SLEW_RATE_AZ*DEG__TO__RAD   ! [rad/s]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SLEW_EL' ) THEN
            FL_SEL = .TRUE.
!
! --------- Get the Antenna slewing rate on elevation axis
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%SLEW_RATE_EL              ! [deg/s]
            STP%SLEW_RATE_EL = STP%SLEW_RATE_EL*DEG__TO__RAD   ! [rad/s]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ACCL_AZ' ) THEN
            FL_AAZ = .TRUE.
!
! --------- Get the Antenna slewing acceleration on azimuth axis
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%SLEW_ACCL_AZ              ! [deg/s^2]
            STP%SLEW_ACCL_AZ = STP%SLEW_ACCL_AZ*DEG__TO__RAD   ! [rad/s^2]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ACCL_EL' ) THEN
            FL_AEL = .TRUE.
!
! --------- Get the Antenna slewing acceleration on elevation axis
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%SLEW_ACCL_EL              ! [deg/s^2]
            STP%SLEW_ACCL_EL = STP%SLEW_ACCL_EL*DEG__TO__RAD   ! [rad/s^2]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DECE_AZ' ) THEN
            FL_DAZ = .TRUE.
!
! --------- Get the Antenna slewing deceleration on azimuth axis
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%SLEW_DECE_AZ              ! [deg/s^2]
            STP%SLEW_DECE_AZ = STP%SLEW_DECE_AZ*DEG__TO__RAD   ! [rad/s^2]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DECE_EL' ) THEN
            FL_DEL = .TRUE.
!
! --------- Get the Antenna slewing deceleration on elevation axis
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%SLEW_DECE_EL              ! [deg/s^2]
            STP%SLEW_DECE_EL = STP%SLEW_DECE_EL*DEG__TO__RAD   ! [rad/s^2]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSETTLE_AZ' ) THEN
            FL_TAZ = .TRUE.
!
! --------- Get the Antenna settle time on azimuth axis
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%TIME_SETTLE_AZ            ! [s]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSETTLE_EL' ) THEN
            FL_TEL = .TRUE.
!
! --------- Get the Antenna settle time on elevation axis
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%TIME_SETTLE_EL            ! [s]
         END IF
! ------          
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EL_MIN' ) THEN
            FL_EMIN = .TRUE.
!
! --------- Get the minimum elevation angle
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%EL_MIN                   ! [deg]
            STP%EL_MIN = STP%EL_MIN*DEG__TO__RAD              ! [rad]
         END IF
! ------          
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EL_MAX' ) THEN
            FL_EMAX = .TRUE.
!
! --------- Get the maximum elevation angle
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%EL_MAX                   ! [deg]
            STP%EL_MAX = STP%EL_MAX*DEG__TO__RAD              ! [rad]
         END IF
! ------          
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'AZ_RANGE' ) THEN
            FL_AZR  = .TRUE.
!
! --------- Get the Azimuth sectors
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%AZ_RANGE(1)              ! [deg]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%AZ_RANGE(2)              ! [deg]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%AZ_RANGE(3)              ! [deg]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,7):IND(2,7)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%AZ_RANGE(4)              ! [deg]
!
! --------- Convert to radians
!
            STP%AZ_RANGE(1) =  STP%AZ_RANGE(1)*DEG__TO__RAD
            STP%AZ_RANGE(2) =  STP%AZ_RANGE(2)*DEG__TO__RAD
            STP%AZ_RANGE(3) =  STP%AZ_RANGE(3)*DEG__TO__RAD
            STP%AZ_RANGE(4) =  STP%AZ_RANGE(4)*DEG__TO__RAD
         END IF
! ------ 
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'RECORDER' ) THEN
            FL_REC  = .TRUE.
!
! --------- Get the Recorder type
!     
            STP%RECORDER = BUF(J1)(IND(1,4):IND(2,4))
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'PREOB' ) THEN
            FL_PRE  = .TRUE.
!
! --------- Get the time allocated for the PREOB procedure: when antenna
!           is pointing at source, before recording.
!      
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%PREOB                  ! [s]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'POSTOB' ) THEN
            FL_POST  = .TRUE.
!
! --------- Get the time allocated for the POSTOB procedure: when antenna
!           is pointing at source, after recording.
!      
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%POSTOB                 ! [s]
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HOR_AZIM' ) THEN
            FL_HAZ  = .TRUE.
!
! --------- Get the array of azimuths defining the elevation mask
!           and convert it to radians.
!      
            DO 427 J10 = 1, STP%NHOR
               READ ( UNIT=BUF(J1)(IND(1,J10+3):IND(2,J10+3)),          &
     &                FMT='(F6.3)', IOSTAT=IER ) STP%HOR_AZ(J10) ! [deg]
               STP%HOR_AZ(J10) = STP%HOR_AZ(J10)*DEG__TO__RAD    ! [rad]
 427        CONTINUE
         ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HOR_ELEV' ) THEN
            FL_HEL  = .TRUE.
!
! --------- Get the array of elevations defining the elevation mask
!           and convert it to radians.
!
            DO 428 J10 = 1, STP%NHOR
               READ ( UNIT=BUF(J1)(IND(1,J10+3):IND(2,J10+3)),          &
     &                FMT='(F6.3)', IOSTAT=IER ) STP%HOR_EL(J10) ! [deg]
               STP%HOR_EL(J10) = STP%HOR_EL(J10)*DEG__TO__RAD    ! [rad]
 428        CONTINUE
         END IF
!
! ------ Handle the TSYS Section
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_DATES' ) THEN
!     
! --------- Get the Date Range for TSYS
! --------- Since the date contains no time add midnight.
!
            J2  =  0
            STR(1) = BUF(J1)(IND(1,4):IND(2,4))//'-00:00:00.00'
            STR(2) = BUF(J1)(IND(1,5):IND(2,5))//'-00:00:00.00'
!     
! --------- Convert to (MJD, TAI)
!
            CALL DATE_TO_TIME ( STR(1), STP%TSYS(1)%MJD_RANGE(1),       &
     &             STP%TSYS(1)%TAI_RANGE(1), IER ) 
            CALL DATE_TO_TIME ( STR(2), STP%TSYS(1)%MJD_RANGE(2),       &
     &             STP%TSYS(1)%TAI_RANGE(2), IER ) 
!
! --------- The second date should come after the first one. If that's
!           not the case, we swap them around.
!
            IF ( STP%TSYS(1)%MJD_RANGE(2) .GT.                          &
     &           STP%TSYS(1)%MJD_RANGE(1) ) THEN
               GO TO 310
            ELSE IF  ( STP%TSYS(1)%MJD_RANGE(2) .LT.                    &
     &                 STP%TSYS(1)%MJD_RANGE(1) ) THEN
!
! ------------ Hold the dates in the temp number holders, NUMI, and
!              NUMR
!
               NUMI(1) = STP%TSYS(1)%MJD_RANGE(1)
               NUMR(1) = STP%TSYS(1)%TAI_RANGE(1)
               NUMI(2) = STP%TSYS(1)%MJD_RANGE(2)
               NUMR(2) = STP%TSYS(1)%TAI_RANGE(2)
!
! ------------ Swap the dates
!
               STP%TSYS(1)%MJD_RANGE(1) = NUMI(2)
               STP%TSYS(1)%TAI_RANGE(1) = NUMR(2)
               STP%TSYS(1)%MJD_RANGE(2) = NUMI(1)
               STP%TSYS(1)%TAI_RANGE(2) = NUMR(1)
            ELSE
!
! ------------ If the MJD are equal, then check the TAI
!
               IF ( STP%TSYS(1)%TAI_RANGE(2) .GT.                       &
     &              STP%TSYS(1)%TAI_RANGE(1) ) THEN
                  GO TO 310
               ELSE IF  ( STP%TSYS(1)%TAI_RANGE(2) .LT.                 &
     &                    STP%TSYS(1)%TAI_RANGE(1) ) THEN
!
! --------------- Hold the dates in the temp number holder,  NUMR, 
!                 since MJD is equal, no need to swap that.
!
                  NUMR(1) = STP%TSYS(1)%TAI_RANGE(1)
                  NUMR(2) = STP%TSYS(1)%TAI_RANGE(2)
!
! --------------- Swap the dates
!
                  STP%TSYS(1)%TAI_RANGE(1) = NUMR(2)
                  STP%TSYS(1)%TAI_RANGE(2) = NUMR(1)
               ELSE
!
! --------------- Both dates are equal
!
                  GO TO 310
               END IF
            END IF
! ---------
 310        CONTINUE
! ---------
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_ELEVS' ) THEN
!     
! --------- Get the Elevations in TSYS
!
            STP%TSYS(1)%NEL = LIND - 3
!
! --------- Allocate the TSYS_ELEV pointer.
!
            ALLOCATE (STP%TSYS(1)%TSYS_ELEV(STP%TSYS(1)%NEL), STAT=IER)
! ---------
            DO 421 J3 = 1, STP%TSYS(1)%NEL
!     
! ------------ Populate the elevations in TSYS
!
               READ ( UNIT=BUF(J1)(IND(1,J3+3):IND(2,J3+3)),            &
     &                FMT='(F6.3)', IOSTAT=IER )                        &
     &              STP%TSYS(1)%TSYS_ELEV(J3)                   ! [deg]
               STP%TSYS(1)%TSYS_ELEV(J3) =                              &
                    STP%TSYS(1)%TSYS_ELEV(J3)*DEG__TO__RAD      ! [rad]
 421        CONTINUE
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_FREQS' ) THEN
!
! --------- Get the Frequency ranges
!
            J2 = J2 + 1
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%TSYS(J2)%FRQ_RANGE(1)       ! [GHz]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%TSYS(J2)%FRQ_RANGE(2)       ! [GHz]
!
! --------- Convert to Hz
!
            STP%TSYS(J2)%FRQ_RANGE(1) = 1.D9*STP%TSYS(J2)%FRQ_RANGE(1)
            STP%TSYS(J2)%FRQ_RANGE(2) = 1.D9*STP%TSYS(J2)%FRQ_RANGE(2)
!
! --------- The second Frequency should be larger that the first. If 
!           that's not the case, we swap them around.
!
            IF ( STP%TSYS(J2)%FRQ_RANGE(2) .GE.                         &
     &           STP%TSYS(J2)%FRQ_RANGE(1) ) THEN
               GO TO 311
            ELSE
! ------------ Hold the frequencies in the temp number holder, NUMR
!
               NUMR(1) = STP%TSYS(J2)%FRQ_RANGE(1)
               NUMR(2) = STP%TSYS(J2)%FRQ_RANGE(2)
!
! ------------ Swap the frequencies.
!
               STP%TSYS(J2)%FRQ_RANGE(1) = NUMR(2)
               STP%TSYS(J2)%FRQ_RANGE(2) = NUMR(1)
            END IF
! ---------
 311        CONTINUE
! ------
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_POLVALS' ) THEN
!
! --------- Get the TSYS values in K
! --------- Note that each Freq. will have two Polarizations.
!           If above "TSYS_POLVALS" is not another "TSYS_POLVAS", then 
!           that is the 1st polarization, if it's another "TSYS_POLVALS" 
!           then we are in the 2nd one.
!
            STP%TSYS(J2)%NPOL  =  LIND - 4 ! No. of Polarizations
! ---------
            IF ( BUF(J1-1)(1:12) .NE. 'TSYS_POLVALS' ) THEN
!
! ------------ Populate the first Polarization's name and values
!
               STP%TSYS(J2)%POLS(1) =  BUF(J1)(IND(1,4):IND(1,4))
! ------------
               DO 422 J4 = 1, STP%TSYS(J2)%NPOL
                  READ ( UNIT=BUF(J1)(IND(1,J4+4):IND(2,J4+4)),         &
     &                   FMT='(F6.3)', IOSTAT=IER )                     &
     &                 STP%TSYS(J2)%TSYS_VALS(1,J4)              ! [K]
 422           CONTINUE
               
            ELSEIF ( BUF(J1-1)(1:12) == 'TSYS_POLVALS' ) THEN
!
! ------------ Populate the second Polarization's name and values
!
               STP%TSYS(J2)%POLS(2) =  BUF(J1)(IND(1,4):IND(1,4))
! ------------
               DO 423 J4 = 1, STP%TSYS(J2)%NPOL
                  READ ( UNIT=BUF(J1)(IND(1,J4+4):IND(2,J4+4)),         &
     &                   FMT='(F6.3)', IOSTAT=IER )                     &
     &                 STP%TSYS(J2)%TSYS_VALS(2,J4)              ! [K]
 423           CONTINUE
            END IF   
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TCAL_POLVAL' ) THEN
!
! --------- Get the calibration load temperatures for each polarization 
! --------- N.B: Analaogous to the Freq., the calibrations also
!                come in two polarizations.
!                If above "TCAL_POLVAL" is not another "TCAL_POLVAL", 
!                then that is the 1st polarization, if it's another 
!                "TCAL_POLVAL" then we are in the 2nd one.
!
            IF ( BUF(J1-1)(1:11) .NE. 'TCAL_POLVAL' ) THEN
!
! ------------ Populate the 1st Polarization's value [K]
!
               READ ( UNIT=BUF(J1)(IND(1,5):IND(1,5)),  FMT='(F6.3)',   &
     &                IOSTAT=IER )   STP%TSYS(J2)%TCAL(1)
            ELSEIF ( BUF(J1-1)(1:11) == 'TCAL_POLVAL') THEN
!
! ------------ Populate the 2nd Polarization's value [K]
!
               READ ( UNIT=BUF(J1)(IND(1,5):IND(1,5)),  FMT='(F6.3)',   &
     &                IOSTAT=IER )   STP%TSYS(J2)%TCAL(2)
            END IF
         END IF
!
! ------ Handle the BPSL Section
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPSS_DATES' ) THEN
!     
! --------- Get the Date Range for BPSL
! --------- Since the date contains no time add midnight.
!
            J5  =  0
            STR(1) = BUF(J1)(IND(1,4):IND(2,4))//'-00:00:00.00'
            STR(2) = BUF(J1)(IND(1,5):IND(2,5))//'-00:00:00.00'
!     
! --------- Convert to (MJD, TAI)
!
            CALL DATE_TO_TIME ( STR(1), STP%BPSL(1)%MJD_RANGE(1),       &
     &             STP%BPSL(1)%TAI_RANGE(1), IER ) 
            CALL DATE_TO_TIME ( STR(2), STP%BPSL(1)%MJD_RANGE(2),       &
     &             STP%BPSL(1)%TAI_RANGE(2), IER )
!
! --------- The second date should come after the first one. If that's
!           not the case, we swap them around.
!
            IF ( STP%BPSL(1)%MJD_RANGE(2) .GT.                          &
     &           STP%BPSL(1)%MJD_RANGE(1) ) THEN
               GO TO 312
            ELSE IF  ( STP%BPSL(1)%MJD_RANGE(2) .LT.                    &
     &                 STP%BPSL(1)%MJD_RANGE(1) ) THEN
!
! ------------ Hold the dates in the temp number holders, NUMI, and
!              NUMR
!
               NUMI(1) = STP%BPSL(1)%MJD_RANGE(1)
               NUMR(1) = STP%BPSL(1)%TAI_RANGE(1)
               NUMI(2) = STP%BPSL(1)%MJD_RANGE(2)
               NUMR(2) = STP%BPSL(1)%TAI_RANGE(2)
!
! ------------ Swap the dates
!
               STP%BPSL(1)%MJD_RANGE(1) = NUMI(2)
               STP%BPSL(1)%TAI_RANGE(1) = NUMR(2)
               STP%BPSL(1)%MJD_RANGE(2) = NUMI(1)
               STP%BPSL(1)%TAI_RANGE(2) = NUMR(1)
            ELSE
!
! ------------ If the MJD are equal, then check the TAI
!
               IF ( STP%BPSL(1)%TAI_RANGE(2) .GT.                       &
     &              STP%BPSL(1)%TAI_RANGE(1) ) THEN
                  GO TO 312
               ELSE IF  ( STP%BPSL(1)%TAI_RANGE(2) .LT.                 &
     &                    STP%BPSL(1)%TAI_RANGE(1) ) THEN
!
! --------------- Hold the dates in the temp number holder,  NUMR,
!                 since MJD is equal, no need to swap that.
!
                  NUMR(1) = STP%BPSL(1)%TAI_RANGE(1)
                  NUMR(2) = STP%BPSL(1)%TAI_RANGE(2)
!
! --------------- Swap the dates
!
                  STP%BPSL(1)%TAI_RANGE(1) = NUMR(2)
                  STP%BPSL(1)%TAI_RANGE(2) = NUMR(1)
               ELSE
!
! --------------- Both dates are equal
!
                  GO TO 312
               END IF
            END IF
! ---------
 312        CONTINUE
! ---------
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPSS_BWIDTH' ) THEN
            J5  =  J5 + 1
!
! --------- Get the IF Bandwith [MHz]
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)),  FMT='(I5)',      &
     &             IOSTAT=IER )   STP%BPSL(J5)%IF_BANDWIDTH
!
! --------- Convert to Hz
!
            STP%BPSL(J5)%IF_BANDWIDTH = STP%BPSL(J5)%IF_BANDWIDTH*1.D6
        ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPSS_BETA' ) THEN
!
! --------- Get the Bandpass Loss  ??[d/l]??
!
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)),  FMT='(F6.3)',      &
     &             IOSTAT=IER )   STP%BPSL(J5)%BETA
         END IF
!###################################################################
!#############THIS IS WHERE WE ARE IN EDITING ######################
!###################################################################
!     
! ------ Handle the GAIN Section
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_DATES' ) THEN
!     
! --------- Get the Date Range for GAIN
! --------- Since the date contains no time add midnight.
!
            J6  =  J6 + 1
            STR(1) = BUF(J1)(IND(1,4):IND(2,4))//'-00:00:00.00'
            STR(2) = BUF(J1)(IND(1,5):IND(2,5))//'-00:00:00.00'
!     
! --------- Convert to (MJD, TAI)
!
            CALL DATE_TO_TIME ( STR(1), STP%GAIN(J6)%MJD_RANGE(1),       &
     &             STP%GAIN(J6)%TAI_RANGE(1), IER ) 
            CALL DATE_TO_TIME ( STR(2), STP%GAIN(J6)%MJD_RANGE(2),       &
     &             STP%GAIN(J6)%TAI_RANGE(2), IER )
!
! --------- The second date should come after the first one. If that's
!           not the case, we swap them around.
!
            IF ( STP%GAIN(J6)%MJD_RANGE(2) .GT.                          &
     &           STP%GAIN(J6)%MJD_RANGE(1) ) THEN
               GO TO 313
            ELSE IF  ( STP%GAIN(J6)%MJD_RANGE(2) .LT.                    &
     &                 STP%GAIN(J6)%MJD_RANGE(1) ) THEN
!
! ------------ Hold the dates in the temp number holders, NUMI, and
!              NUMR
!
               NUMI(1) = STP%GAIN(J6)%MJD_RANGE(1)
               NUMR(1) = STP%GAIN(J6)%TAI_RANGE(1)
               NUMI(2) = STP%GAIN(J6)%MJD_RANGE(2)
               NUMR(2) = STP%GAIN(J6)%TAI_RANGE(2)
!
! ------------ Swap the dates
!
               STP%GAIN(J6)%MJD_RANGE(1) = NUMI(2)
               STP%GAIN(J6)%TAI_RANGE(1) = NUMR(2)
               STP%GAIN(J6)%MJD_RANGE(2) = NUMI(1)
               STP%GAIN(J6)%TAI_RANGE(2) = NUMR(1)
            ELSE
!
! ------------ If the MJD are equal, then check the TAI
!
               IF ( STP%GAIN(J6)%TAI_RANGE(2) .GT.                       &
     &              STP%GAIN(J6)%TAI_RANGE(1) ) THEN
                  GO TO 313
               ELSE IF  ( STP%GAIN(J6)%TAI_RANGE(2) .LT.                 &
     &                    STP%GAIN(J6)%TAI_RANGE(1) ) THEN
!
! --------------- Hold the dates in the temp number holder,  NUMR,
!                 since MJD is equal, no need to swap that.
!
                  NUMR(1) = STP%GAIN(J6)%TAI_RANGE(1)
                  NUMR(2) = STP%GAIN(J6)%TAI_RANGE(2)
!
! --------------- Swap the dates
!
                  STP%GAIN(J6)%TAI_RANGE(1) = NUMR(2)
                  STP%GAIN(J6)%TAI_RANGE(2) = NUMR(1)
               ELSE
!
! --------------- Both dates are equal
!
                  GO TO 313
               END IF
            END IF
! ---------
 313        CONTINUE
! ---------
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_FREQS' ) THEN
!
! --------- Get the Frequency ranges
!
!%%NOKH%%!            J6 = J6 + 1
            READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%GAIN(J6)%FRQ_RANGE(1)       ! [GHz]
! ---------
            READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F6.3)',       &
     &             IOSTAT=IER )  STP%GAIN(J6)%FRQ_RANGE(2)       ! [GHz]
!
! --------- Convert to Hz
!
            STP%GAIN(J6)%FRQ_RANGE(1) = 1.D9*STP%GAIN(J6)%FRQ_RANGE(1)
            STP%GAIN(J6)%FRQ_RANGE(2) = 1.D9*STP%GAIN(J6)%FRQ_RANGE(2)
!
! --------- The second Frequency should be larger that the first. If 
!           that's not the case, we swap them around.
!
            IF ( STP%GAIN(J6)%FRQ_RANGE(2) .GE.                         &
     &           STP%GAIN(J6)%FRQ_RANGE(1) ) THEN
               GO TO 314
            ELSE
! ------------ Hold the frequencies in the temp number holder, NUMR
!
               NUMR(1) = STP%GAIN(J6)%FRQ_RANGE(1)
               NUMR(2) = STP%GAIN(J6)%FRQ_RANGE(2)
!
! ------------ Swap the frequencies.
!
               STP%GAIN(J6)%FRQ_RANGE(1) = NUMR(2)
               STP%GAIN(J6)%FRQ_RANGE(2) = NUMR(1)
            END IF
! ---------
 314        CONTINUE
         END IF
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_ELEVS' ) THEN
!     
! --------- Get the Elevations in GAIN
!
            STP%GAIN(J6)%NEL = LIND - 3
!
! --------- Allocate the GAIN_ELEV pointer.
!
            ALLOCATE (STP%GAIN(J6)%GAIN_ELEV(STP%GAIN(J6)%NEL),STAT=IER)
! ---------
            DO 424 J7 = 1, STP%GAIN(J6)%NEL
!
! ------------ Popolulate the elevations in GAIN
!
               READ ( UNIT=BUF(J1)(IND(1,J7+3):IND(2,J7+3)),            &
     &                FMT='(F6.3)', IOSTAT=IER )                        &
     &              STP%GAIN(J6)%GAIN_ELEV(J7)                   ! [deg]
               STP%GAIN(J6)%GAIN_ELEV(J7) =                              &
                    STP%GAIN(J6)%GAIN_ELEV(J7)*DEG__TO__RAD      ! [rad]
 424        CONTINUE
         END IF         
! ------
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_POLVALS' ) THEN
!
! --------- Get the GAIN values in K/Jy
! --------- Note that each Freq. will have two Polarizations.
!           If above "GAIN_POLVALS" is not "GAIN_POLVALS", that's the 
!           1st polarization, if it's another "GAIN_POLVALS" then we
!           are in the 2nd one.
!
            STP%GAIN(J6)%NPOL  =  LIND - 4 ! No. of Polarizations
! ---------            
            IF ( BUF(J1-1)(1:12) .NE. 'GAIN_POLVALS' ) THEN
!
! ------------ Populate the first Polarization's name and values
!
               STP%GAIN(J6)%POLS(1) =  BUF(J1)(IND(1,4):IND(2,4))
! ------------
               DO 425 J8 = 1, STP%GAIN(J6)%NPOL
                  READ ( UNIT=BUF(J1)(IND(1,J8+4):IND(2,J8+4)),         &
     &                   FMT='(F6.3)', IOSTAT=IER )                     &
     &                 STP%GAIN(J6)%GAIN_VALS(1,J8)             ! [K]
 425           CONTINUE
               
            ELSEIF ( BUF(J1-1)(1:12) == 'GAIN_POLVALS' ) THEN
!
! ------------ Populate the second Polarization's name and values
!
               STP%GAIN(J6)%POLS(2) =  BUF(J1)(IND(1,4):IND(1,4))
! ------------
               DO 426 J9 = 1, STP%GAIN(J6)%NPOL
                  READ ( UNIT=BUF(J1)(IND(1,J9+4):IND(2,J9+4)),         &
     &                   FMT='(F5.2)', IOSTAT=IER )                     &
     &                 STP%GAIN(J6)%GAIN_VALS(2,J9)              ! [K]
 426           CONTINUE
            END IF   
         END IF
 410  CONTINUE
!
! --- N.B: - The dates for TSYS, and BPSL are assumed to appear 
!            once per file. This assumption affects the sorting because 
!            the other dates are not filled. To counter this, we will
!            repeat the date accross all date fields in each section.
!          - In future should we encounter a file with multiple dates 
!            and elevations, then these shall be edited and the
!            pointers in STP can be changed to 2-D.
!
      DO 431 I1 = 1, STP%NTSYS
         STP%TSYS(I1)%MJD_RANGE(1) = STP%TSYS(1)%MJD_RANGE(1)
         STP%TSYS(I1)%TAI_RANGE(1) = STP%TSYS(1)%TAI_RANGE(1)
         STP%TSYS(I1)%MJD_RANGE(2) = STP%TSYS(1)%MJD_RANGE(2)
         STP%TSYS(I1)%TAI_RANGE(2) = STP%TSYS(1)%TAI_RANGE(2)
 431  CONTINUE
! ---
      DO 432 I1 = 1, STP%NBPSL
         STP%BPSL(I1)%MJD_RANGE(1) = STP%BPSL(1)%MJD_RANGE(1)
         STP%BPSL(I1)%TAI_RANGE(1) = STP%BPSL(1)%TAI_RANGE(1)
         STP%BPSL(I1)%MJD_RANGE(2) = STP%BPSL(1)%MJD_RANGE(2)
         STP%BPSL(I1)%TAI_RANGE(2) = STP%BPSL(1)%TAI_RANGE(2)
 432  CONTINUE
!
! --- Sort the TSYS according to the dates and frequencies
!
      CALL FOR_QSORT ( STP%TSYS, STP%NTSYS, SIZEOF(STP%TSYS(1)),        &
     &          STP_COMPAR_TSYS )
!
! --- Sort the GAIN according to the dates and frequencies
!
      CALL FOR_QSORT ( STP%GAIN, STP%NGAIN, SIZEOF(STP%GAIN(1)),        &
     &          STP_COMPAR_GAIN )
!
! --- Variable Parsing Errors
!
      IF ( .NOT. FL_IVS ) THEN
         CALL ERR_LOG ( 5006, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter a name for the Station in '          &
     &        //FIL_STP// ' ,hence STP%NAME and STP%SHORT_NAME could '  &
     &        //'could not be parsed.' )
         RETURN
      END IF
! --- 
      IF ( .NOT. FL_UD ) THEN
         CALL ERR_LOG ( 5007, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter an update date for the Station in '  &
     &        //FIL_STP//' ,hence STP%LAST_UPDATE could not be '        &
     &        //'parsed.' )
         RETURN
      END IF
! --- 
      IF ( .NOT. FL_COO ) THEN
         CALL ERR_LOG ( 5008, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter Station coordinates in '             &
     &        //FIL_STP//' ,hence STP%COO could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_MNT ) THEN
         CALL ERR_LOG ( 5009, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the mount type code in '             &
     &        //FIL_STP//' ,hence STP%MNT could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_SAZ ) THEN
         CALL ERR_LOG ( 5010, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter antenna slewing rate (azimuth) in '  &
     &        //FIL_STP//' ,hence STP%SLEW_RATE_AZ couldn"t be parsed.')
         RETURN
      END IF
! ---
      IF ( .NOT. FL_SEL ) THEN
         CALL ERR_LOG ( 5011, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter antenna slewing rate (elevation) in '&
     &        //FIL_STP//' ,hence STP%SLEW_RATE_EL couldn"t be parsed.')
         RETURN
      END IF
! ---
      IF ( .NOT. FL_AAZ ) THEN
         CALL ERR_LOG ( 5012, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter antenna slewing acc. (azimuth) in '  &
     &        //FIL_STP//' ,hence STP%SLEW_ACCL_AZ couldn"t be parsed.')
         RETURN
      END IF
! ---
      IF ( .NOT. FL_AEL ) THEN
         CALL ERR_LOG ( 5013, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter antenna slewing acc. (elevation) in '&
     &        //FIL_STP//' ,hence STP%SLEW_ACCL_EL couldn"t be parsed.')
         RETURN
      END IF
! ---
      IF ( .NOT. FL_TAZ ) THEN
         CALL ERR_LOG ( 5014, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter antenna settle time (azimuth) in '   &
     &        //FIL_STP//' ,hence STP%TIME_SETTLE_AZ could not be'      &
     &        //' parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_TEL ) THEN
         CALL ERR_LOG ( 5015, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter antenna settle time (elevation) in ' &
     &        //FIL_STP//' ,hence STP%TIME_SETTLE_EL could not be'      &
     &        //' parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_EMIN ) THEN
         CALL ERR_LOG ( 5016, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the minimum elevation angle in '     &
     &        //FIL_STP//' ,hence STP%EMIN could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_EMAX ) THEN
         CALL ERR_LOG ( 5017, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the maximum elevation angle in '     &
     &        //FIL_STP//' ,hence STP%EMAX could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_AZR ) THEN
         CALL ERR_LOG ( 5018, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the azimuth range  in '              &
     &        //FIL_STP//' ,hence STP%AZ_RANGE could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_REC ) THEN
         CALL ERR_LOG ( 5019, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the recorder type  in '              &
     &        //FIL_STP//' ,hence STP%REC could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_PRE ) THEN
         CALL ERR_LOG ( 5020, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the allocated PREOB time in '        &
     &        //FIL_STP//' ,hence STP%PREOB could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_POST ) THEN
         CALL ERR_LOG ( 5021, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the allocated POSTOB time in '        &
     &        //FIL_STP//' ,hence STP%POSTOB could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_HAZ ) THEN
         CALL ERR_LOG ( 5022, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the horizon mask (azimuth) in '      &
     &        //FIL_STP//' ,hence STP%HOR_AZ could not be parsed.' )
         RETURN
      END IF
! ---
      IF ( .NOT. FL_HEL ) THEN
         CALL ERR_LOG ( 5023, IUER, 'STP_PARSER', 'Wrong File Format: ' &
     &        //'Did not encounter the horizon mask (elevation) in '      &
     &        //FIL_STP//' ,hence STP%HOR_EL could not be parsed.' )
         RETURN
      END IF
! ---
      RETURN
      END SUBROUTINE ! STP_PARSER  !#!
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE STP_CLEAN ( STP )
      IMPLICIT   NONE
      INCLUDE   'stp.i'
      TYPE ( STP__TYPE ) :: STP
!
      IF ( ASSOCIATED ( STP%TSYS ) ) DEALLOCATE ( STP%TSYS )
      IF ( ASSOCIATED ( STP%GAIN ) ) DEALLOCATE ( STP%GAIN )
      IF ( ASSOCIATED ( STP%BPSL ) ) DEALLOCATE ( STP%BPSL )
      IF ( ASSOCIATED ( STP%HOR_AZ ) ) DEALLOCATE ( STP%HOR_AZ )
      IF ( ASSOCIATED ( STP%HOR_EL ) ) DEALLOCATE ( STP%HOR_EL )
!
      CALL CLRCH ( STP%NAME )
      CALL CLRCH ( STP%SHORT_NAME )
      CALL CLRCH ( STP%LAST_UPDATE )
      STP%COO(1)         = 0.D0
      STP%COO(2)         = 0.D0
      STP%COO(3)         = 0.D0
      STP%SLEW_RATE_EL   = 0.D0
      STP%SLEW_RATE_AZ   = 0.D0
      STP%SLEW_ACCL_EL   = 0.D0
      STP%SLEW_ACCL_AZ   = 0.D0
      STP%TIME_SETTLE_EL = 0.D0
      STP%TIME_SETTLE_AZ = 0.D0
      STP%EL_MIN         = 0.D0
      STP%EL_MAX         = 0.D0
      STP%AZ_RANGE(1)    = 0.D0
      STP%AZ_RANGE(2)    = 0.D0
      STP%AZ_RANGE(3)    = 0.D0
      STP%AZ_RANGE(4)    = 0.D0
      STP%PREOB          = 0.D0
      STP%POSTOB         = 0.D0
      CALL CLRCH ( STP%MOUNT_TYPE )
      CALL CLRCH ( STP%RECORDER )
      STP%NHOR    = 0
      STP%NTSYS   = 0
      STP%NGAIN   = 0
      STP%NBPSL   = 0
      STP%STATUS  = STP__INIT
!
      RETURN
      END SUBROUTINE ! STP_CLEAN    !#!#!
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE STP_COUNT ( N_HOR, N_TSYS, N_GAIN, N_BPSL, FIL_STP,    &
     &                       IUER )
!
! ***************************************************************************
! *                                                                         *
! *   Routine STP_COUNT counts the number of: horizontal masks, system      *
! *   temperatures, gains, and bandpass losses in a given station           *
! *   parameters file.                                                      *
! *   INPUT:                                                                *
! *            FIL_STP   =  Station Parameter file           { CHAR }       *
! *                         N.B: The formating of this file is not          *
! *                              subject to international standardization   *
! *                              and the parsing is based on out format     *
! *                              preferences.                               *
! *                              See an example of a .stp file here to      *
! *                              see our formating.                         *
! *                                                                         *
! *            IUER      =  Error Handler                    { INT, OPT }   *
! *                         If IUER=0 no error message will be printed,     *
! *                         even in the event of an error. However, for     *
! *                         other possible values, i.e. IUER=-1,-2, & -3,   *
! *                         the error message will print to screen. For     *
! *                         the latter case, i.e. IUER=-3, after printing   *
! *                         the program will terminate.                     *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            N_HOR     =  Number of Horizontal Masks       { INT }        *
! *            N_TSYS    =  Number of System Temperatures    { INT }        *
! *            N_GAIN    =  Number of Gains                  { INT }        *
! *            N_BPSL    =  Number of Band Pass Losses       { INT }        *
! *                                                                         *
! *  ### 30-JUL-2020  STP_COUNT   v1.0 (c)    N. Habana    30-JUL-2020 ###  *
! *                                                                         *
! *  ### 20-OCT-2020  STP_COUNT   v1.1 (c)    N. Habana    20-OCT-2020 ###  *
! *    - Changed the gains and Tsys counters to use the number of dates     *
! *      as their basis.                                                    *
! *                                                                         *
! ***************************************************************************
!
      IMPLICIT   NONE
      INTEGER*4  N_HOR, N_TSYS, N_GAIN, N_BPSL, IUER
      INTEGER*4  H_CHECKA, H_CHECKB
      CHARACTER  FIL_STP*(*)
      CHARACTER  DELIM*5
      INTEGER*4  MP, MIND, MAXL_STRING
      PARAMETER  ( MAXL_STRING=256 )
      PARAMETER  ( MP = 128*1024 )
      PARAMETER  ( MIND = 128 )
      CHARACTER  BUF(MP)*(MAXL_STRING), SECT_ID*32      ! Read File, File Sec
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'='//':' )
      INTEGER*4  J0, J1, NP, LIND, IND(2,MIND), IER
      LOGICAL*1  FL_HORA, FL_HORB, FL_HOR, FL_TSYS, FL_GAIN, FL_BPSL
      INTEGER*4, EXTERNAL :: ILEN
!
! --- Reading the Station Parameter File to variable, BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_STP, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5201, IUER, 'STP_COUNT', 'Error in reading '  &
     &          //'input file: '//FIL_STP )
           RETURN
      END IF
!
! --- N.B: Some of the counts below are based on the assumption that
!          there won't be any comments that follow the parameters.     
! --- Flags:
!
      FL_HORA  = .FALSE.
      FL_HORB  = .FALSE.
      FL_HOR   = .FALSE.
      FL_TSYS  = .FALSE.
      FL_GAIN  = .FALSE.
      FL_BPSL  = .FALSE.
!
! --- Preallocate some counts    
!     
      N_GAIN = 0
! ---
      DO 420 J1 = 1, NP
!
! --------- Bypass empty lines
!
            IF ( ILEN(BUF(J1)) == 0 )  GOTO 420
!
! --------- Bypass comment lines.
!
            IF ( BUF(J1)(1:1) == '#' ) GOTO 420
!
! --------- Extract words from the read line
!
            CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! --------- Count the number of Horizontal Masks 
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HOR_AZIM' ) THEN
               H_CHECKA   =  LIND - 3
               FL_HORA    =  .TRUE.
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HOR_ELEV' ) THEN
               H_CHECKB   =  LIND - 3
               FL_HORB    =  .TRUE.
            END IF
! ---------
            IF ( FL_HORA .AND. FL_HORB ) THEN
               N_HOR   =  H_CHECKA
               FL_HOR  =  .TRUE.
            END IF
!
! --------- Count the number of System Temperatures
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_DATES' ) THEN
               FL_TSYS  =  .TRUE.
               SECT_ID  =  'TSYS'
               N_TSYS   =  0
            END IF
! ---------
            IF ( SECT_ID == 'TSYS' ) THEN
               IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_FREQS' ) THEN
                  N_TSYS = N_TSYS + 1
               END IF
            END IF
!
! --------- Count the number of Band Pass Losses
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPSS_DATES' ) THEN
               FL_BPSL  =  .TRUE.
               SECT_ID  =  'BPSL'
               N_BPSL   =  0
            END IF
! ---------
            IF ( SECT_ID == 'BPSL' ) THEN
               IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPSS_BWIDTH' ) THEN
                  N_BPSL = N_BPSL + 1
               END IF
            END IF
!
! --------- Count the number of Gains
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_DATES' ) THEN
               FL_GAIN  =  .TRUE.
               SECT_ID  =  'GAIN'
!%%NOKH%%!               N_GAIN   =  0
               N_GAIN = N_GAIN + 1
            END IF
! ---------
!%%NOKH%%!            IF ( SECT_ID == 'GAIN' ) THEN
!%%NOKH%%!               IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_FREQS' ) THEN
!%%NOKH%%!                  N_GAIN = N_GAIN + 1
!%%NOKH%%!               END IF
!%%NOKH%%!            END IF
 420     CONTINUE
! ---
      IF ( .NOT. FL_HOR ) THEN
         CALL ERR_LOG ( 5202, IUER, 'STP_COUNT', 'Mismatch in number '  &
     &        //'of horizontal mask found in azimuth, and those in '    &
     &        //'elevation.' )
           RETURN 
      END IF
! ---
      IF ( .NOT. FL_TSYS ) THEN
         CALL ERR_LOG ( 5203, IUER, 'STP_COUNT', 'Wrong File Format: '  &
     &          //'TSYS Block missing date, therefore we stopped the '  &
     &          //'counter.' )
           RETURN 
      END IF
! ---
      IF ( .NOT. FL_BPSL ) THEN
         CALL ERR_LOG ( 5204, IUER, 'STP_COUNT', 'Wrong File Format: '  &
     &          //'BPSL Block missing date, therefore we stopped the '  &
     &          //'counter.' )
           RETURN 
      END IF
! ---
      IF ( .NOT. FL_GAIN ) THEN
         CALL ERR_LOG ( 5205, IUER, 'STP_COUNT', 'Wrong File Format: '  &
     &          //'GAIN Block missing date, therefore we stopped the '  &
     &          //'counter.' )
           RETURN 
      END IF
! ---
      CALL ERR_LOG (0, IUER)
      RETURN
      END SUBROUTINE ! STP_COUNT !#!#!#!      
