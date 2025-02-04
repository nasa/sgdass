      SUBROUTINE  STP_DIR_PARSER ( DIR_STP, STP, IUER )
! ***************************************************************************
! *                                                                         *
! *   Routine STP_DIR_PARSER parses the VLBI station parameters files in a  *
! *   given directory.                                                      *
! *   N.B: - See also stp_parser.f                                          *
! *                                                                         *
! *   INPUT:                                                                *
! *            DIR_STP    =  STP File Directory          { CHAR }           *
! *                                                                         *
! *   OUTPUT:                                                               *
! *            STP       =  Parsed Object                { DERIVED TYPE }   *
! *                         For more on the parsed data, see stp.i, and     *
! *                         edit it accordingly to include more data        *
! *                         blocks.                                         *
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
! *  ### 16-NOV-2020  STP_DIR_PARSER  v1.0 (c)  N. Habana  16-NOV-2020 ###  *
! *                                                                         *
! ***************************************************************************
      IMPLICIT    NONE
      INCLUDE     'stp.i'
      INCLUDE     'astro_constants.i'
      TYPE        ( STP__TYPE ) :: STP
      CHARACTER   DIR_STP*(*)
      CHARACTER   FIL_STP*128, STR*128
      INTEGER*4   L_FIL, L_STP, LEV, IS
      INTEGER*4   DIR_DESC(16)
      CHARACTER   DELIM*5                       ! Deliminator for STP file
      CHARACTER   DELIM_DIR*4                       ! Deliminator for STP directory
      INTEGER*4   MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4   MAXL_STRING                   ! Max. String length
      PARAMETER   ( MAXL_STRING = 256 )           
      PARAMETER   ( MP = 128*1024 )             
      PARAMETER   ( MIND = 128 )                 
      PARAMETER   ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'='//':' ) 
      PARAMETER   ( DELIM_DIR =  CHAR(0)//CHAR(32)//CHAR(9)//'/' ) 
      CHARACTER   BUF(MP)*(MAXL_STRING)         ! Read File
      INTEGER*4   NP, LIND, IND(2,MIND), LN
      INTEGER*4   IUER, IER
      INTEGER*4   I0, I1, I2
      INTEGER*4   J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10
      INTEGER*4   K0, K1, K2
      INTEGER*4   NDATES(3), NUMI(2)
      REAL*8      NUMR(2)
      CHARACTER   STR2(2)*32
      LOGICAL*1   FL_IVS, FL_ID, FL_UD, FL_COO, FL_MNT, FL_SAZ, FL_SEL
      LOGICAL*1   FL_AAZ, FL_AEL, FL_DAZ, FL_DEL, FL_TAZ, FL_TEL
      LOGICAL*1   FL_EMIN, FL_EMAX, FL_AZR, FL_REC, FL_PRE, FL_POST
      LOGICAL*1   FL_HAZ, FL_HEL
      INTEGER*4,  EXTERNAL :: ILEN, GET_FILE_FROM_DIR
#ifdef GNU
      INTEGER*4,  EXTERNAL :: STP_COMPAR_TSYS, STP_COMPAR_GAIN
#else
      INTEGER*2,  EXTERNAL :: STP_COMPAR_TSYS, STP_COMPAR_GAIN
#endif
!
! --- Clean STP
!
      IF ( STP%STATUS .NE. STP__INIT ) CALL STP_CLEAN ( STP )
      STP%DIR_NAM = DIR_STP
!
! --- Initialise the file count and directory level
!
      L_FIL = 0
      LEV   = 0
      L_STP = 0
!
! --- Read the files in the directory
!
      DO 210 K1 = 1, STP__MFIL
! ------
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_STP, FIL_STP )
         IF ( IS .NE. 0 ) THEN
            IUER = -2
            CALL ERR_LOG ( 5501, IUER, 'STP_DIR_PARSER',                &
     &              'Error in reading input directory '//               &
     &              TRIM(DIR_STP)//' '//FIL_STP )
            CALL EXIT ( 1 )
         END IF
! ------
         IF ( LEV == 0 ) GOTO 220 ! Have all the files in the directory
! ------
!
! ------ Bypass temporary files created by emacs
!
         IF ( INDEX ( FIL_STP, '#' ) > 0 ) GOTO 210
         IF ( INDEX ( FIL_STP, '~' ) > 0 ) GOTO 210
!
         L_FIL = L_FIL + 1
         IF ( L_FIL > STP__MFIL ) THEN
            IUER = -2
            CALL CLRCH ( STR )
            CALL INCH  ( STP__MFIL, STR )
            CALL ERR_LOG ( 5502, IUER, 'STP_DIR_PARSER',                &
     &              'Too many files were found in the input directory'  &
     &              //TRIM(DIR_STP)//' more than STP__MFIL '            &
     &              //TRIM(STR)//'. Please raise STP__MFIL' )
             CALL EXIT ( 1 )
         END IF
!
! ------ Check if the file is an stp file.  
! ------ N.B: - Such files will have extension ".stp" and thus be
!               longer than 4 characters in name.
!
         CALL EXWORD ( FIL_STP, MIND, LIND, IND, DELIM_DIR, IUER )
         LN = IND(2,LIND) - IND(1,LIND)
         IF ( (LN .GT. 4)                                        .AND.  &
     &        (FIL_STP(IND(2,LIND)-3:IND(2,LIND)) .EQ. '.stp') ) THEN
            L_STP = L_STP + 1
            STP%C_FIL(L_STP) = FIL_STP
         END IF
 210  CONTINUE
!
 220  CONTINUE
!
! --- Sort the files and allocate them, given the directory contains stp 
!     files
!
      STP%NSTA = L_STP
      IF ( STP%NSTA > 0 ) THEN
         CALL SORT_FAST_CH ( STP%NSTA, STP%C_FIL ) 
!
         ALLOCATE ( STP%STA(STP%NSTA),  STAT = IER )
!
! ------ Parse the stp parameters for all files.
!
         DO 510 K2 = 1, STP%NSTA
!
! --------- Reading the Station Parameter File to variable, BUF
!
            CALL CLRCH ( BUF )
            CALL ERR_PASS ( IUER, IER )
            CALL RD_TEXT  ( STP%C_FIL(K2), MP, BUF, NP, IER )
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5503, IUER, 'STP_DIR_PARSER',             &
     &                 'Error in reading input file: '//STP%C_FIL(K2) )
               RETURN
            END IF
            IF ( STP%STA(K2)%STATUS .NE. STP__INIT ) THEN
                 CALL STP_STA_CLEAN ( STP%STA(K2) )
            END IF
!
! --------- Counting the No. of horizontal masks, TSYS, GAIN, and BPSL
!
            CALL ERR_PASS ( IUER, IER )
            CALL STP_COUNT ( STP%STA(K2)%NHOR, STP%STA(K2)%NTSYS,       &
     &              STP%STA(K2)%NGAIN, STP%STA(K2)%NBPSL,               &
     &              STP%C_FIL(K2), IER )
!
!---------- Allocating the (null pointed) structures found in STP__TYPE
!           to variables.
!           Array sizes based on the counts derived from above
!
            ALLOCATE (STP%STA(K2)%TSYS(STP%STA(K2)%NTSYS),  STAT = IER)
            ALLOCATE (STP%STA(K2)%GAIN(STP%STA(K2)%NGAIN),  STAT = IER) 
            ALLOCATE (STP%STA(K2)%BPSL(STP%STA(K2)%NBPSL),  STAT = IER)
            ALLOCATE (STP%STA(K2)%HOR_AZ(STP%STA(K2)%NHOR), STAT = IER)
            ALLOCATE (STP%STA(K2)%HOR_EL(STP%STA(K2)%NHOR), STAT = IER)
! ---------
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5504, IUER, 'STP_DIR_PARSER',             &
     &                 'Failed to allocate the following STP pointers:' &
     &                 //'STP%STA(K2)%TSYS, STP%STA(K2)%GAIN, '         &
     &                 //'STP%STA(K2)%BPSL, STP%STA(K2)%HOR_AZ, and '   &
     &                 //'STP%STA(K2)%HOR_EL.' )
            END IF
! ---------
            DO I0 = 1, STP%STA(K2)%NTSYS
!
! ------------ Allocate the TSYS_VALS pointer
!
               ALLOCATE ( STP%STA(K2)%TSYS(I0)%TSYS_VALS(2,STP__MPOL),  &
     &                    STAT=IER )
            END DO
! ---------
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5505, IUER, 'STP_DIR_PARSER',             &
     &                 'Failed to allocate '                            &
     &                 //'STP%STA(K2)%TSYS(:)%TSYS_VALS.' )
            END IF
! ---------
            DO I0 = 1, STP%STA(K2)%NGAIN
!
! ------------ Allocate the GAIN_VALS pointer.
!
               ALLOCATE ( STP%STA(K2)%GAIN(I0)%GAIN_VALS(2,STP__MPOL),  &
     &                    STAT=IER )
            END DO
! ---------
            IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 5506, IUER, 'STP_DIR_PARSER',             &
     &                 'Failed to allocate '                            &
     &                //'the STP%STA(K2)%GAIN(:)%GAIN_VALS.' )
            END IF
!
! --------- Initialise the flags
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
! --------- Initialise some counters
!
            J6 = 0                    ! Gain Counter
! ---------      
            DO 410 J1 = 1, NP
!
! --------- Bypass empty lines
!
            IF ( ILEN(BUF(J1)) == 0 )  GOTO 410
!
! --------- Bypass comment lines.
!
            IF ( BUF(J1)(1:1) == '#' ) GOTO 410
!
! --------- Extract words from the read line
!
            CALL EXWORD ( BUF(J1), MIND, LIND, IND, DELIM, IER )
!
! --------- Handle the "General" Section
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SHORT_NAME' ) THEN
               FL_IVS = .TRUE. 
!
! ------------ Get IVS name (in Caps)
!
               CALL TRAN ( 11, BUF(J1)(IND(1,2):IND(2,2)),              &
     &                  STP%STA(K2)%NAME )
!
! ------------ Get Station ID (in lower case)
!
               CALL TRAN ( 12, BUF(J1)(IND(1,4):IND(2,4)),              &
     &                  STP%STA(K2)%SHORT_NAME )
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'LAST_UPDATE' ) THEN
               FL_UD = .TRUE.
!
! ------------ Get date of last modification [YYYY.MM.DD]
! ------------ N.B: We have ":" as a deliminator, therefore to avoid this 
!                   being a actor in the date collection, we just use the 
!                   word from the first "Y"+9 characters.
!
               STP%STA(K2)%LAST_UPDATE = BUF(J1)(IND(1,4):IND(1,4)+9)
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'COORD' ) THEN
               FL_COO = .TRUE.
!
! ------------ Get the Station Coordinates
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F15.6)',   &
     &                IOSTAT=IER )  STP%STA(K2)%COO(1)                ! X [m]
! ------------
               READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F15.6)',   &
     &                IOSTAT=IER )  STP%STA(K2)%COO(2)                ! Y [m]
! ------------
               READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F15.6)',   &
     &                IOSTAT=IER )  STP%STA(K2)%COO(3)                ! Z [m]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'MOUNT' ) THEN
               FL_MNT = .TRUE.
!
! ------------ Get the Mount type Code
!
               STP%STA(K2)%MOUNT_TYPE = BUF(J1)(IND(1,4):IND(2,4))
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SLEW_AZ' ) THEN
               FL_SAZ = .TRUE.
!
! ------------ Get the Antenna slewing rate on azimuth axis
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%SLEW_RATE_AZ              ! [deg/s]
               STP%STA(K2)%SLEW_RATE_AZ =                               &
     &            STP%STA(K2)%SLEW_RATE_AZ*DEG__TO__RAD   ! [rad/s]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SLEW_EL' ) THEN
               FL_SEL = .TRUE.
!
! ------------ Get the Antenna slewing rate on elevation axis
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%SLEW_RATE_EL              ! [deg/s]
               STP%STA(K2)%SLEW_RATE_EL =                               &
     &            STP%STA(K2)%SLEW_RATE_EL*DEG__TO__RAD   ! [rad/s]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ACCL_AZ' ) THEN
               FL_AAZ = .TRUE.
!
! ------------ Get the Antenna slewing acceleration on azimuth axis
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%SLEW_ACCL_AZ              ! [deg/s^2]
               STP%STA(K2)%SLEW_ACCL_AZ =                               &
     &            STP%STA(K2)%SLEW_ACCL_AZ*DEG__TO__RAD   ! [rad/s^2]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ACCL_EL' ) THEN
               FL_AEL = .TRUE.
!
! ------------ Get the Antenna slewing acceleration on elevation axis
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%SLEW_ACCL_EL              ! [deg/s^2]
               STP%STA(K2)%SLEW_ACCL_EL =                               &
     &            STP%STA(K2)%SLEW_ACCL_EL*DEG__TO__RAD   ! [rad/s^2]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DECE_AZ' ) THEN
               FL_DAZ = .TRUE.
!
! ------------ Get the Antenna slewing deceleration on azimuth axis
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%SLEW_DECE_AZ              ! [deg/s^2]
               STP%STA(K2)%SLEW_DECE_AZ =                               &
     &            STP%STA(K2)%SLEW_DECE_AZ*DEG__TO__RAD   ! [rad/s^2]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DECE_EL' ) THEN
               FL_DEL = .TRUE.
!
! ------------ Get the Antenna slewing deceleration on elevation axis
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%SLEW_DECE_EL              ! [deg/s^2]
               STP%STA(K2)%SLEW_DECE_EL =                               &
     &            STP%STA(K2)%SLEW_DECE_EL*DEG__TO__RAD   ! [rad/s^2]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSETTLE_AZ' ) THEN
               FL_TAZ = .TRUE.
!
! ------------ Get the Antenna settle time on azimuth axis
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%TIME_SETTLE_AZ            ! [s]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSETTLE_EL' ) THEN
               FL_TEL = .TRUE.
!
! ------------ Get the Antenna settle time on elevation axis
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%TIME_SETTLE_EL            ! [s]
            END IF
! ---------          
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EL_MIN' ) THEN
               FL_EMIN = .TRUE.
!
! ------------ Get the minimum elevation angle
!
                READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &                 IOSTAT=IER )  STP%STA(K2)%EL_MIN                   ! [deg]
                STP%STA(K2)%EL_MIN = STP%STA(K2)%EL_MIN*DEG__TO__RAD              ! [rad]
            END IF
! ---------          
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'EL_MAX' ) THEN
               FL_EMAX = .TRUE.
!
! ------------ Get the maximum elevation angle
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%EL_MAX                   ! [deg]
               STP%STA(K2)%EL_MAX = STP%STA(K2)%EL_MAX*DEG__TO__RAD              ! [rad]
            END IF
! ---------          
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'AZ_RANGE' ) THEN
               FL_AZR  = .TRUE.
!
! ------------ Get the Azimuth sectors
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%AZ_RANGE(1)              ! [deg]
! ------------
               READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%AZ_RANGE(2)              ! [deg]
! ------------
               READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%AZ_RANGE(3)              ! [deg]
! -----------
               READ ( UNIT=BUF(J1)(IND(1,7):IND(2,7)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%AZ_RANGE(4)              ! [deg]
!
! ------------ Convert to radians
!
               STP%STA(K2)%AZ_RANGE(1) =  STP%STA(K2)%AZ_RANGE(1)*DEG__TO__RAD
               STP%STA(K2)%AZ_RANGE(2) =  STP%STA(K2)%AZ_RANGE(2)*DEG__TO__RAD
               STP%STA(K2)%AZ_RANGE(3) =  STP%STA(K2)%AZ_RANGE(3)*DEG__TO__RAD
               STP%STA(K2)%AZ_RANGE(4) =  STP%STA(K2)%AZ_RANGE(4)*DEG__TO__RAD
            END IF
! --------- 
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'RECORDER' ) THEN
               FL_REC  = .TRUE.
!
! ------------ Get the Recorder type
!     
               STP%STA(K2)%RECORDER = BUF(J1)(IND(1,4):IND(2,4))
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'PREOB' ) THEN
               FL_PRE  = .TRUE.
!
! ------------ Get the time allocated for the PREOB procedure: when antenna
!              is pointing at source, before recording.
!      
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%PREOB                  ! [s]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'POSTOB' ) THEN
               FL_POST  = .TRUE.
!
! ------------ Get the time allocated for the POSTOB procedure: when antenna
!              is pointing at source, after recording.
!      
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',       &
     &                IOSTAT=IER )  STP%STA(K2)%POSTOB                 ! [s]
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HOR_AZIM' ) THEN
               FL_HAZ  = .TRUE.
!
! ------------ Get the array of azimuths defining the elevation mask
!              and convert it to radians.
!      
               DO 427 J10 = 1, STP%STA(K2)%NHOR
                  READ ( UNIT=BUF(J1)(IND(1,J10+3):IND(2,J10+3)),       &
     &                   FMT='(F6.3)', IOSTAT=IER )                     &
     &                 STP%STA(K2)%HOR_AZ(J10) ! [deg]
                  STP%STA(K2)%HOR_AZ(J10) =                             &
     &               STP%STA(K2)%HOR_AZ(J10)*DEG__TO__RAD    ! [rad]
 427           CONTINUE
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'HOR_ELEV' ) THEN
               FL_HEL  = .TRUE.
!
! ------------ Get the array of elevations defining the elevation mask
!              and convert it to radians.
!
               DO 428 J10 = 1, STP%STA(K2)%NHOR
                  READ ( UNIT=BUF(J1)(IND(1,J10+3):IND(2,J10+3)),       &
     &                   FMT='(F6.3)', IOSTAT=IER )                     &
     &                 STP%STA(K2)%HOR_EL(J10) ! [deg]
                  STP%STA(K2)%HOR_EL(J10) =                             &
     &               STP%STA(K2)%HOR_EL(J10)*DEG__TO__RAD    ! [rad]
 428           CONTINUE
            END IF
!
! --------- Handle the TSYS Section
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_DATES' ) THEN
!     
! ------------ Get the Date Range for TSYS
! ------------ Since the date contains no time add midnight.
!
               J2  =  0
               STR2(1) = BUF(J1)(IND(1,4):IND(2,4))//'-00:00:00.00'
               STR2(2) = BUF(J1)(IND(1,5):IND(2,5))//'-00:00:00.00'
!     
! ------------ Convert to (MJD, TAI)
!
               CALL DATE_TO_TIME ( STR2(1),                             &
     &                  STP%STA(K2)%TSYS(1)%MJD_RANGE(1),               &
     &                  STP%STA(K2)%TSYS(1)%TAI_RANGE(1), IER )
               CALL DATE_TO_TIME ( STR2(2),                             &
     &                  STP%STA(K2)%TSYS(1)%MJD_RANGE(2),               &
     &                  STP%STA(K2)%TSYS(1)%TAI_RANGE(2), IER ) 
!
! ------------ The second date should come after the first one. If that's
!              not the case, we swap them around.
!
               IF ( STP%STA(K2)%TSYS(1)%MJD_RANGE(2) .GT.               &
     &              STP%STA(K2)%TSYS(1)%MJD_RANGE(1) ) THEN
                  GO TO 310
               ELSE IF  ( STP%STA(K2)%TSYS(1)%MJD_RANGE(2) .LT.         &
     &                    STP%STA(K2)%TSYS(1)%MJD_RANGE(1) ) THEN
!
! --------------- Hold the dates in the temp number holders, NUMI, and
!                 NUMR
!
                  NUMI(1) = STP%STA(K2)%TSYS(1)%MJD_RANGE(1)
                  NUMR(1) = STP%STA(K2)%TSYS(1)%TAI_RANGE(1)
                  NUMI(2) = STP%STA(K2)%TSYS(1)%MJD_RANGE(2)
                  NUMR(2) = STP%STA(K2)%TSYS(1)%TAI_RANGE(2)
!
! --------------- Swap the dates
!
                  STP%STA(K2)%TSYS(1)%MJD_RANGE(1) = NUMI(2)
                  STP%STA(K2)%TSYS(1)%TAI_RANGE(1) = NUMR(2)
                  STP%STA(K2)%TSYS(1)%MJD_RANGE(2) = NUMI(1)
                  STP%STA(K2)%TSYS(1)%TAI_RANGE(2) = NUMR(1)
               ELSE
!
! --------------- If the MJD are equal, then check the TAI
!
                  IF ( STP%STA(K2)%TSYS(1)%TAI_RANGE(2) .GT.                       &
     &                 STP%STA(K2)%TSYS(1)%TAI_RANGE(1) ) THEN
                     GO TO 310
                  ELSE IF  ( STP%STA(K2)%TSYS(1)%TAI_RANGE(2) .LT.                 &
     &                       STP%STA(K2)%TSYS(1)%TAI_RANGE(1) ) THEN
!
! ------------------ Hold the dates in the temp number holder,  NUMR, 
!                    since MJD is equal, no need to swap that.
!   
                     NUMR(1) = STP%STA(K2)%TSYS(1)%TAI_RANGE(1)
                     NUMR(2) = STP%STA(K2)%TSYS(1)%TAI_RANGE(2)
!
! ------------------ Swap the dates
!
                     STP%STA(K2)%TSYS(1)%TAI_RANGE(1) = NUMR(2)
                     STP%STA(K2)%TSYS(1)%TAI_RANGE(2) = NUMR(1)
                  ELSE
!
! ------------------ Both dates are equal
!
                     GO TO 310
                  END IF
               END IF
! ------------
 310           CONTINUE
! ------------
            END IF
! ----------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_ELEVS' ) THEN
!     
! ------------ Get the Elevations in TSYS
!   
               STP%STA(K2)%TSYS(1)%NEL = LIND - 3
!
! ------------ Allocate the TSYS_ELEV pointer.
!
               ALLOCATE (                                               &
     &           STP%STA(K2)%TSYS(1)%TSYS_ELEV(STP%STA(K2)%TSYS(1)%NEL),&
     &           STAT=IER )
! ------------
               DO 421 J3 = 1, STP%STA(K2)%TSYS(1)%NEL
!     
! --------------- Popolulate the elevations in TSYS
!
                  READ ( UNIT=BUF(J1)(IND(1,J3+3):IND(2,J3+3)),         &
     &                   FMT='(F6.3)', IOSTAT=IER )                     &
     &                 STP%STA(K2)%TSYS(1)%TSYS_ELEV(J3)                   ! [deg]
                  STP%STA(K2)%TSYS(1)%TSYS_ELEV(J3) =                   &
                       STP%STA(K2)%TSYS(1)%TSYS_ELEV(J3)*DEG__TO__RAD      ! [rad]
 421           CONTINUE
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_FREQS' ) THEN
!
! ------------ Get the Frequency ranges
!
               J2 = J2 + 1
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%TSYS(J2)%FRQ_RANGE(1)       ! [GHz]
! ------------
               READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%TSYS(J2)%FRQ_RANGE(2)       ! [GHz]
!
! ------------ Convert to Hz
!
               STP%STA(K2)%TSYS(J2)%FRQ_RANGE(1) =                      &
     &            1.D9*STP%STA(K2)%TSYS(J2)%FRQ_RANGE(1)
               STP%STA(K2)%TSYS(J2)%FRQ_RANGE(2) =                      &
     &            1.D9*STP%STA(K2)%TSYS(J2)%FRQ_RANGE(2)
!
! ------------ The second Frequency should be larger that the first. If 
!              that's not the case, we swap them around.
!
               IF ( STP%STA(K2)%TSYS(J2)%FRQ_RANGE(2) .GE.              &
     &              STP%STA(K2)%TSYS(J2)%FRQ_RANGE(1) ) THEN
                     GO TO 311
               ELSE
! --------------- Hold the frequencies in the temp number holder, NUMR
!
                  NUMR(1) = STP%STA(K2)%TSYS(J2)%FRQ_RANGE(1)
                  NUMR(2) = STP%STA(K2)%TSYS(J2)%FRQ_RANGE(2)
!
! --------------- Swap the frequencies.
!
                  STP%STA(K2)%TSYS(J2)%FRQ_RANGE(1) = NUMR(2)
                  STP%STA(K2)%TSYS(J2)%FRQ_RANGE(2) = NUMR(1)
               END IF
! ------------
 311           CONTINUE
! ------------
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TSYS_POLVALS' ) THEN
!
! ------------ Get the TSYS values in K
! ------------ Note that each Freq. will have two Polarizations.
!              If above "TSYS_POLVALS" is not another "TSYS_POLVAS", then 
!              that is the 1st polarization, if it's another "TSYS_POLVALS" 
!              then we are in the 2nd one.
!
               STP%STA(K2)%TSYS(J2)%NPOL  =  LIND - 4 ! No. of Polarizations
! ------------
               IF ( BUF(J1-1)(1:12) .NE. 'TSYS_POLVALS' ) THEN
!
! --------------- Populate the first Polarization's name and values
!
                  STP%STA(K2)%TSYS(J2)%POLS(1) =                        &
     &               BUF(J1)(IND(1,4):IND(1,4))
! ---------------
                  DO 422 J4 = 1, STP%STA(K2)%TSYS(J2)%NPOL
                     READ ( UNIT=BUF(J1)(IND(1,J4+4):IND(2,J4+4)),      &
     &                      FMT='(F6.3)', IOSTAT=IER )                  &
     &                    STP%STA(K2)%TSYS(J2)%TSYS_VALS(1,J4)              ! [K]
 422              CONTINUE
               
               ELSEIF ( BUF(J1-1)(1:12) == 'TSYS_POLVALS' ) THEN
!
! --------------- Populate the second Polarization's name and values
!
                  STP%STA(K2)%TSYS(J2)%POLS(2) =                        &
     &                BUF(J1)(IND(1,4):IND(1,4))
! ------------
                  DO 423 J4 = 1, STP%STA(K2)%TSYS(J2)%NPOL
                     READ ( UNIT=BUF(J1)(IND(1,J4+4):IND(2,J4+4)),      &
     &                      FMT='(F6.3)', IOSTAT=IER )                  &
     &                    STP%STA(K2)%TSYS(J2)%TSYS_VALS(2,J4)              ! [K]
 423              CONTINUE
               END IF   
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'TCAL_POLVAL' ) THEN
!
! ------------ Get the calibration load temperatures for each polarization 
! ------------ N.B: Analaogous to the Freq., the calibrations also
!                   come in two polarizations.
!                   If above "TCAL_POLVAL" is not another "TCAL_POLVAL", 
!                   then that is the 1st polarization, if it's another 
!                   "TCAL_POLVAL" then we are in the 2nd one.
!
               IF ( BUF(J1-1)(1:11) .NE. 'TCAL_POLVAL' ) THEN
!
! --------------- Populate the 1st Polarization's value [K]
!
                  READ ( UNIT=BUF(J1)(IND(1,5):IND(1,5)),  FMT='(F6.3)',&
     &                   IOSTAT=IER )   STP%STA(K2)%TSYS(J2)%TCAL(1)
               ELSEIF ( BUF(J1-1)(1:11) == 'TCAL_POLVAL') THEN
!
! --------------- Populate the 2nd Polarization's value [K]
!
                  READ ( UNIT=BUF(J1)(IND(1,5):IND(1,5)),  FMT='(F6.3)',&
     &                   IOSTAT=IER )   STP%STA(K2)%TSYS(J2)%TCAL(2)
               END IF
            END IF
!
! --------- Handle the BPSL Section
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPSS_DATES' ) THEN
!     
! ------------ Get the Date Range for BPSL
! ------------ Since the date contains no time add midnight.
!
               J5  =  0
               STR2(1) = BUF(J1)(IND(1,4):IND(2,4))//'-00:00:00.00'
               STR2(2) = BUF(J1)(IND(1,5):IND(2,5))//'-00:00:00.00'
!     
! ------------ Convert to (MJD, TAI)
!
               CALL DATE_TO_TIME ( STR2(1),                             &
     &                  STP%STA(K2)%BPSL(1)%MJD_RANGE(1),               &
     &                  STP%STA(K2)%BPSL(1)%TAI_RANGE(1), IER ) 
               CALL DATE_TO_TIME ( STR2(2),                             &
     &                  STP%STA(K2)%BPSL(1)%MJD_RANGE(2),               &
     &                  STP%STA(K2)%BPSL(1)%TAI_RANGE(2), IER )
!
! ------------ The second date should come after the first one. If that's
!              not the case, we swap them around.
!
               IF ( STP%STA(K2)%BPSL(1)%MJD_RANGE(2) .GT.               &
     &              STP%STA(K2)%BPSL(1)%MJD_RANGE(1) ) THEN
                  GO TO 312
               ELSE IF  ( STP%STA(K2)%BPSL(1)%MJD_RANGE(2) .LT.         &
     &                    STP%STA(K2)%BPSL(1)%MJD_RANGE(1) ) THEN
!
! --------------- Hold the dates in the temp number holders, NUMI, and
!                 NUMR
!
                  NUMI(1) = STP%STA(K2)%BPSL(1)%MJD_RANGE(1)
                  NUMR(1) = STP%STA(K2)%BPSL(1)%TAI_RANGE(1)
                  NUMI(2) = STP%STA(K2)%BPSL(1)%MJD_RANGE(2)
                  NUMR(2) = STP%STA(K2)%BPSL(1)%TAI_RANGE(2)
!
! --------------- Swap the dates
!
                  STP%STA(K2)%BPSL(1)%MJD_RANGE(1) = NUMI(2)
                  STP%STA(K2)%BPSL(1)%TAI_RANGE(1) = NUMR(2)
                  STP%STA(K2)%BPSL(1)%MJD_RANGE(2) = NUMI(1)
                  STP%STA(K2)%BPSL(1)%TAI_RANGE(2) = NUMR(1)
               ELSE
!
! --------------- If the MJD are equal, then check the TAI
!
                  IF ( STP%STA(K2)%BPSL(1)%TAI_RANGE(2) .GT.            &
     &                 STP%STA(K2)%BPSL(1)%TAI_RANGE(1) ) THEN
                     GO TO 312
                  ELSE IF  ( STP%STA(K2)%BPSL(1)%TAI_RANGE(2) .LT.      &
     &                       STP%STA(K2)%BPSL(1)%TAI_RANGE(1) ) THEN
!
! ------------------ Hold the dates in the temp number holder,  NUMR,
!                    since MJD is equal, no need to swap that.
!
                     NUMR(1) = STP%STA(K2)%BPSL(1)%TAI_RANGE(1)
                     NUMR(2) = STP%STA(K2)%BPSL(1)%TAI_RANGE(2)
!
! ------------------ Swap the dates
!
                     STP%STA(K2)%BPSL(1)%TAI_RANGE(1) = NUMR(2)
                     STP%STA(K2)%BPSL(1)%TAI_RANGE(2) = NUMR(1)
                  ELSE
!
! ------------------ Both dates are equal
!
                     GO TO 312
                  END IF
               END IF
! ------------
 312           CONTINUE
! ------------
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPSS_BWIDTH' ) THEN
               J5  =  J5 + 1
!
! ------------ Get the IF Bandwith [MHz]
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)),  FMT='(I5)',     &
     &                IOSTAT=IER )   STP%STA(K2)%BPSL(J5)%IF_BANDWIDTH
!
! ------------ Convert to Hz
!
               STP%STA(K2)%BPSL(J5)%IF_BANDWIDTH =                      &
     &            STP%STA(K2)%BPSL(J5)%IF_BANDWIDTH*1.D6
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BPSS_BETA' ) THEN
!
! ----------- Get the Bandpass Loss  ??[d/l]??
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)),  FMT='(F6.3)',   &
     &               IOSTAT=IER )   STP%STA(K2)%BPSL(J5)%BETA
            END IF
!     
! --------- Handle the GAIN Section
!
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_DATES' ) THEN
!     
! ------------ Get the Date Range for GAIN
! ------------ Since the date contains no time add midnight.
!
               J6  =  J6 + 1
               STR2(1) = BUF(J1)(IND(1,4):IND(2,4))//'-00:00:00.00'
               STR2(2) = BUF(J1)(IND(1,5):IND(2,5))//'-00:00:00.00'
!     
! ------------ Convert to (MJD, TAI)
!
               CALL DATE_TO_TIME ( STR2(1),                             &
     &                  STP%STA(K2)%GAIN(J6)%MJD_RANGE(1),              &
     &                  STP%STA(K2)%GAIN(J6)%TAI_RANGE(1), IER ) 
               CALL DATE_TO_TIME ( STR2(2),                             &
     &                  STP%STA(K2)%GAIN(J6)%MJD_RANGE(2),              &
     &                  STP%STA(K2)%GAIN(J6)%TAI_RANGE(2), IER )
!
! ------------ The second date should come after the first one. If that's
!              not the case, we swap them around.
!
               IF ( STP%STA(K2)%GAIN(J6)%MJD_RANGE(2) .GT.              &
     &              STP%STA(K2)%GAIN(J6)%MJD_RANGE(1) ) THEN
                  GO TO 313
               ELSE IF  ( STP%STA(K2)%GAIN(J6)%MJD_RANGE(2) .LT.        &
     &                    STP%STA(K2)%GAIN(J6)%MJD_RANGE(1) ) THEN
!
! --------------- Hold the dates in the temp number holders, NUMI, and
!                 NUMR
!
                  NUMI(1) = STP%STA(K2)%GAIN(J6)%MJD_RANGE(1)
                  NUMR(1) = STP%STA(K2)%GAIN(J6)%TAI_RANGE(1)
                  NUMI(2) = STP%STA(K2)%GAIN(J6)%MJD_RANGE(2)
                  NUMR(2) = STP%STA(K2)%GAIN(J6)%TAI_RANGE(2)
!
! --------------- Swap the dates
!
                  STP%STA(K2)%GAIN(J6)%MJD_RANGE(1) = NUMI(2)
                  STP%STA(K2)%GAIN(J6)%TAI_RANGE(1) = NUMR(2)
                  STP%STA(K2)%GAIN(J6)%MJD_RANGE(2) = NUMI(1)
                  STP%STA(K2)%GAIN(J6)%TAI_RANGE(2) = NUMR(1)
               ELSE
!
! --------------- If the MJD are equal, then check the TAI
!
                  IF ( STP%STA(K2)%GAIN(J6)%TAI_RANGE(2) .GT.           &
     &                 STP%STA(K2)%GAIN(J6)%TAI_RANGE(1) ) THEN
                     GO TO 313
                  ELSE IF  ( STP%STA(K2)%GAIN(J6)%TAI_RANGE(2) .LT.     &
     &                       STP%STA(K2)%GAIN(J6)%TAI_RANGE(1) ) THEN
!
! ------------------ Hold the dates in the temp number holder,  NUMR,
!                    since MJD is equal, no need to swap that.
!
                     NUMR(1) = STP%STA(K2)%GAIN(J6)%TAI_RANGE(1)
                     NUMR(2) = STP%STA(K2)%GAIN(J6)%TAI_RANGE(2)
!
! ------------------ Swap the dates
!
                     STP%STA(K2)%GAIN(J6)%TAI_RANGE(1) = NUMR(2)
                     STP%STA(K2)%GAIN(J6)%TAI_RANGE(2) = NUMR(1)
                  ELSE
!
! ------------------ Both dates are equal
!
                     GO TO 313
                  END IF
               END IF
! ------------
 313           CONTINUE
! ------------
            END IF
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_FREQS' ) THEN
!
! ------------ Get the Frequency ranges
!
               READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%GAIN(J6)%FRQ_RANGE(1)       ! [GHz]
! ------------
               READ ( UNIT=BUF(J1)(IND(1,5):IND(2,5)), FMT='(F6.3)',    &
     &                IOSTAT=IER )  STP%STA(K2)%GAIN(J6)%FRQ_RANGE(2)       ! [GHz]
!
! ------------ Convert to Hz
!
               STP%STA(K2)%GAIN(J6)%FRQ_RANGE(1) =                      &
     &            1.D9*STP%STA(K2)%GAIN(J6)%FRQ_RANGE(1)
               STP%STA(K2)%GAIN(J6)%FRQ_RANGE(2) =                      &
     &            1.D9*STP%STA(K2)%GAIN(J6)%FRQ_RANGE(2)
!
! ------------ The second Frequency should be larger that the first. If 
!              that's not the case, we swap them around.
!
               IF ( STP%STA(K2)%GAIN(J6)%FRQ_RANGE(2) .GE.              &
     &              STP%STA(K2)%GAIN(J6)%FRQ_RANGE(1) ) THEN
                  GO TO 314
               ELSE
! --------------- Hold the frequencies in the temp number holder, NUMR
!
                  NUMR(1) = STP%STA(K2)%GAIN(J6)%FRQ_RANGE(1)
                  NUMR(2) = STP%STA(K2)%GAIN(J6)%FRQ_RANGE(2)
!
! --------------- Swap the frequencies.
!
                  STP%STA(K2)%GAIN(J6)%FRQ_RANGE(1) = NUMR(2)
                  STP%STA(K2)%GAIN(J6)%FRQ_RANGE(2) = NUMR(1)
               END IF
! ------------
 314           CONTINUE
            END IF
! ----------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_ELEVS' ) THEN
!     
! ------------ Get the Elevations in GAIN
!
               STP%STA(K2)%GAIN(J6)%NEL = LIND - 3
!
! ------------ Allocate the GAIN_ELEV pointer.
!
               ALLOCATE (STP%STA(K2)%GAIN(J6)%GAIN_ELEV(STP%STA(K2)%GAIN(J6)%NEL),STAT=IER)
! ------------
               DO 424 J7 = 1, STP%STA(K2)%GAIN(J6)%NEL
!
! --------------- Popolulate the elevations in GAIN
!
                  READ ( UNIT=BUF(J1)(IND(1,J7+3):IND(2,J7+3)),         &
     &                   FMT='(F6.3)', IOSTAT=IER )                     &
     &                 STP%STA(K2)%GAIN(J6)%GAIN_ELEV(J7)                   ! [deg]
                  STP%STA(K2)%GAIN(J6)%GAIN_ELEV(J7) =                  &
     &                  STP%STA(K2)%GAIN(J6)%GAIN_ELEV(J7)*DEG__TO__RAD      ! [rad]
 424           CONTINUE
            END IF         
! ---------
            IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'GAIN_POLVALS' ) THEN
!
! ------------ Get the GAIN values in K/Jy
! ------------ Note that each Freq. will have two Polarizations.
!              If above "GAIN_POLVALS" is not "GAIN_POLVALS", that's the 
!              1st polarization, if it's another "GAIN_POLVALS" then we
!              are in the 2nd one.
!
               STP%STA(K2)%GAIN(J6)%NPOL  =  LIND - 4 ! No. of Polarizations
! -------------            
               IF ( BUF(J1-1)(1:12) .NE. 'GAIN_POLVALS' ) THEN
!
! --------------- Populate the first Polarization's name and values
!
                  STP%STA(K2)%GAIN(J6)%POLS(1) =                        &
     &                BUF(J1)(IND(1,4):IND(2,4))
! ---------------
                  DO 425 J8 = 1, STP%STA(K2)%GAIN(J6)%NPOL
                     READ ( UNIT=BUF(J1)(IND(1,J8+4):IND(2,J8+4)),      &
     &                      FMT='(F6.3)', IOSTAT=IER )                  &
     &                    STP%STA(K2)%GAIN(J6)%GAIN_VALS(1,J8)             ! [K]
 425              CONTINUE
!               
               ELSEIF ( BUF(J1-1)(1:12) == 'GAIN_POLVALS' ) THEN
!
! --------------- Populate the second Polarization's name and values
!
                  STP%STA(K2)%GAIN(J6)%POLS(2) =                        &
     &                BUF(J1)(IND(1,4):IND(1,4))
! ---------------
                  DO 426 J9 = 1, STP%STA(K2)%GAIN(J6)%NPOL
                     READ ( UNIT=BUF(J1)(IND(1,J9+4):IND(2,J9+4)),      &
     &                      FMT='(F5.2)', IOSTAT=IER )                  &
     &                    STP%STA(K2)%GAIN(J6)%GAIN_VALS(2,J9)              ! [K]
 426              CONTINUE
               END IF   
            END IF
 410        CONTINUE

!
! --------- N.B: - The dates for TSYS, and BPSL are assumed to appear 
!                  once per file. This assumption affects the sorting because 
!                  the other dates are not filled. To counter this, we will
!                  repeat the date accross all date fields in each section.
!                - In future should we encounter a file with multiple dates 
!                  and elevations, then these shall be edited and the
!                  pointers in STP can be changed to 2-D.
!                - So do the no. of elevations & those elevation values.
!
            DO 431 I1 = 1, STP%STA(K2)%NTSYS
               STP%STA(K2)%TSYS(I1)%MJD_RANGE(1) =                      &
     &            STP%STA(K2)%TSYS(1)%MJD_RANGE(1)
               STP%STA(K2)%TSYS(I1)%TAI_RANGE(1) =                      &
     &            STP%STA(K2)%TSYS(1)%TAI_RANGE(1)
               STP%STA(K2)%TSYS(I1)%MJD_RANGE(2) =                      &
     &            STP%STA(K2)%TSYS(1)%MJD_RANGE(2)
               STP%STA(K2)%TSYS(I1)%TAI_RANGE(2) =                      &
     &            STP%STA(K2)%TSYS(1)%TAI_RANGE(2)
!
!
!
               STP%STA(K2)%TSYS(I1)%NEL =  STP%STA(K2)%TSYS(1)%NEL
               ALLOCATE (                                               &
     &           STP%STA(K2)%TSYS(I1)%TSYS_ELEV(STP%STA(K2)%TSYS(1)%NEL)&
     &           , STAT=IER)
               DO 433 I2 = 1, STP%STA(K2)%TSYS(1)%NEL
                  STP%STA(K2)%TSYS(I1)%TSYS_ELEV(I2)  =                 &
     &               STP%STA(K2)%TSYS(1)%TSYS_ELEV(I2)
 433           CONTINUE
 431        CONTINUE
! ---------
            DO 432 I1 = 1, STP%STA(K2)%NBPSL
               STP%STA(K2)%BPSL(I1)%MJD_RANGE(1) =                      &
     &            STP%STA(K2)%BPSL(1)%MJD_RANGE(1)
               STP%STA(K2)%BPSL(I1)%TAI_RANGE(1) =                      &
     &            STP%STA(K2)%BPSL(1)%TAI_RANGE(1)
               STP%STA(K2)%BPSL(I1)%MJD_RANGE(2) =                      &
     &            STP%STA(K2)%BPSL(1)%MJD_RANGE(2)
               STP%STA(K2)%BPSL(I1)%TAI_RANGE(2) =                      &
     &            STP%STA(K2)%BPSL(1)%TAI_RANGE(2)
 432        CONTINUE
!
! --------- Sort the TSYS according to the dates and frequencies
!
            CALL FOR_QSORT ( STP%STA(K2)%TSYS, STP%STA(K2)%NTSYS,       &
     &              SIZEOF(STP%STA(K2)%TSYS(1)), STP_COMPAR_TSYS )
!
! --------- Sort the GAIN according to the dates and frequencies
!
            CALL FOR_QSORT ( STP%STA(K2)%GAIN, STP%STA(K2)%NGAIN,       &
     &              SIZEOF(STP%STA(K2)%GAIN(1)), STP_COMPAR_GAIN )
!
! --------- Variable Parsing Errors
!
            IF ( .NOT. FL_IVS ) THEN
               CALL ERR_LOG ( 5507, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: Did not encounter a name'//  &
     &                 ' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%NAME and STP%STA(K2)%SHORT_NAME'//  &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! --- 
            IF ( .NOT. FL_UD ) THEN
               CALL ERR_LOG ( 5508, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: Did not encounter update '// &
     &                 ' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &                //'STP%STA(K2)%LAST_UPDATE'//                     &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! --- 
            IF ( .NOT. FL_COO ) THEN
               CALL ERR_LOG ( 5509, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: Did not encounter coord.'//  &
     &                 ' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%COO'//                              &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_MNT ) THEN
               CALL ERR_LOG ( 5510, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: Did not encounter mount code'&
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%MNT'//                              &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_SAZ ) THEN
               CALL ERR_LOG ( 5511, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: Didnt find antenna slew rate'&
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%SLEW_RATE_AZ'//                     &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_SEL ) THEN
               CALL ERR_LOG ( 5512, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: Didnt find antenna slew rate'&
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%SLEW_RATE_EL'//                     &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_AAZ ) THEN
               CALL ERR_LOG ( 5513, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No antenna slw acc. (az)'    &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%SLEW_ACCL_AZ'//                     &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_AEL ) THEN
               CALL ERR_LOG ( 5514, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No antenna slw acc. (elev)'  &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%SLEW_ACCL_EL'//                     &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_TAZ ) THEN
               CALL ERR_LOG ( 5515, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No antenna settle time (az)' &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%TIME_SETTLE_AZ'//                   &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_TEL ) THEN
               CALL ERR_LOG ( 5516, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No antenna settle time(elev)'&
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%SLEW_ACCL_EL'//                     &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_EMIN ) THEN
               CALL ERR_LOG ( 5517, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No min. elev. angle'         &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%EMIN'//                             &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_EMAX ) THEN
               CALL ERR_LOG ( 5518, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No max. elev. angle'         &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%EMAX'//                             &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_AZR ) THEN
               CALL ERR_LOG ( 5519, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No azimuth range'            &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%AZ_RANGE'//                         &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_REC ) THEN
               CALL ERR_LOG ( 5520, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No recorder type '           &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%REC'//                              &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_PRE ) THEN
               CALL ERR_LOG ( 5521, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No pre-orbital time '        &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%PREOB'//                            &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_POST ) THEN
               CALL ERR_LOG ( 5521, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No post-orbital time'        &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%POSTOB'//                           &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_HAZ ) THEN
               CALL ERR_LOG ( 5522, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No horizon mask (azimuth)'   &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%HOR_AZ'//                           &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ---
            IF ( .NOT. FL_HEL ) THEN
               CALL ERR_LOG ( 5523, IUER, 'STP_DIR_PARSER',             &
     &                 'Wrong File Format: No horizon mask (elev)'      &
     &               //' for the Station in '//STP%C_FIL(K2)//' ,hence '&
     &               //'STP%STA(K2)%HOR_EL'//                           &
     &                 ' could not be parsed.' )
               RETURN
            END IF
! ------
 510     CONTINUE
      ELSE
         CALL ERR_LOG ( 5524, IUER, 'STP_DIR_PARSER',                   &
     &            'There are no STP files in '//DIR_STP//'.' )
         RETURN
      END IF
!
      DO 430 J3=1,STP%NSTA
         STP%C_STA(J3) = STP%STA(J3)%NAME
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE ! STP_DIR_PARSER  !#!
!
! ---------------------------------------------------------------------------
!
      SUBROUTINE STP_CLEAN ( STP )
      IMPLICIT   NONE
      INCLUDE   'stp.i'
      TYPE     ( STP__TYPE ) :: STP
      INTEGER*4  J1
!
      IF ( STP%NSTA > 0 ) THEN
           DO 410 J1=1,STP%NSTA
              IF ( ASSOCIATED ( STP%STA(J1)%HOR_AZ ) ) DEALLOCATE ( STP%STA(J1)%HOR_AZ )
              IF ( ASSOCIATED ( STP%STA(J1)%HOR_EL ) ) DEALLOCATE ( STP%STA(J1)%HOR_EL )
              IF ( ASSOCIATED ( STP%STA(J1)%TSYS   ) ) DEALLOCATE ( STP%STA(J1)%TSYS   )
              IF ( ASSOCIATED ( STP%STA(J1)%GAIN   ) ) DEALLOCATE ( STP%STA(J1)%GAIN   )
              IF ( ASSOCIATED ( STP%STA(J1)%BPSL   ) ) DEALLOCATE ( STP%STA(J1)%BPSL   )
 410       CONTINUE 
      END IF
!
      IF ( ASSOCIATED ( STP%STA ) ) DEALLOCATE ( STP%STA )
!
      CALL CLRCH ( STP%C_FIL )
      STP%NSTA   = 0
      STP%STATUS  = STP__INIT
!
      RETURN
      END SUBROUTINE  STP_CLEAN    !#!#!
