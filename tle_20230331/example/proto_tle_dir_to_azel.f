     PROGRAM   TLE_DIR_TO_AZEL
!
!     Go through a directory of SDE's.
!     Compute azel at given times, write each to file.
!     Point out the times of visibility
!
!
      
      IMPLICIT   NONE
      INCLUDE    'ners.i'
      INCLUDE    'ners_local.i'
      INCLUDE    'astro_constants.i'
      INCLUDE    'tle_sgp4.i'
      TYPE       ( NERS__TYPE ) :: NERS
      TYPE       ( EPH__TYPE )  :: EPH(EPH__MFIL)
      CHARACTER  DIR_TLE*64, DIROUT*64, FIL_TLE*128, FILOUT(EPH__MFIL)*128
      CHARACTER  STR*128, SAT_NAM*32
      INTEGER*4  IUER, IER, IMOD, NTLE
      INTEGER*4  DIR_DESC(16)
      INTEGER*4  L_FIL, L_TLE, IS, LEV
      REAL*8     TSTART, TFINISH
      INTEGER*4  J1, J2, J3, J4, J5
      INTEGER*4  K1, K2, K3, K4
      INTEGER*8  STACK_SIZE_IN_BYTES, GB
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4)*GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
      CHARACTER  TLEDATE*30, DATE_BEG*21
      CHARACTER  NERS_CONFIG*128, HOME_DIR*128
      REAL*8     UTC, DTIM, UTC_BEG, EPOCH
      REAL*8     TAI_BEG, UTC_MTAI, TIM_TAI, TAI
      REAL*8     X_TRS(3), XDOT_TRS(3), STA_POS(3)
      INTEGER*4  MJD, MJD_BEG
      CHARACTER  DATE_STR*32, REFR_MODE*5, C_PAR*16
      CHARACTER  CDATE*22
      INTEGER*4  M_PAR, L_PAR, ICNT
      PARAMETER  ( M_PAR = NERS__MPAR )
      REAL*8     PARS(M_PAR), AZ, EL
      LOGICAL*1  LEX
      CHARACTER   DELIM_DIR*4                   ! Deliminator for TLE directory
      INTEGER*4   MP, MIND                      ! Max. No. of lines, Max. Index 
      INTEGER*4   MAXL_STRING                   ! Max. String length
      PARAMETER   ( MAXL_STRING = 256 )           
      PARAMETER   ( MP = 128*1024 )             
      PARAMETER   ( MIND = 128 )                 
      PARAMETER   ( DELIM_DIR =  CHAR(0)//CHAR(32)//CHAR(9)//'/' ) ! Null, Space, Tab, forward slash
      CHARACTER   BUF(MP)*(MAXL_STRING)         ! Read File
      INTEGER*4   NP, LIND, IND(2,MIND), LN
      INTEGER*4   NEPC
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, GET_FILE_FROM_DIR
!
! --- Ground Station
!
!      STA_POS(1)  =  1130730.331D0   !GGAO12M
 !     STA_POS(2)  = -4831246.540D0   !GGAO12M
  !    STA_POS(3)  =  3994228.904D0   !GGAO12M

      STA_POS(1) =  1130769.3342D0   ! CEA
      STA_POS(2) = -4831224.9743D0   ! CEA
      STA_POS(3) =  3994243.8465D0   ! CEA

!      STA_POS(1) =  -1330788.244D0   ! MACGO12M
 !     STA_POS(2) =  -5328106.522D0   ! MACGO12M
  !    STA_POS(3) =   3236427.559D0   ! MACGO12M
     
!
! --- Conigure NERS
!
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( NERS_CONFIG == ' ' ) THEN
!
! ------ Second, check $HOME/.ners_config file
!
         CALL GETENVAR ( 'HOME', HOME_DIR )
         NERS_CONFIG = TRIM(HOME_DIR)//'/.ners_config'
         INQUIRE ( FILE = NERS_CONFIG, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
!
! --------- Third, check for the system-wide ners configuration file 
!
            NERS_CONFIG = NERS__CONFIG
         END IF
      END IF
!
! --- Initialise NERS
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 2101, IUER, 'TLE_EXAMPLE_TO_AZEL',              &
     &                  'Error in initializing NERS data structure' )
         RETURN
      END IF
!
! --- Get intrinsic date and time
!
      CALL CPU_TIME ( TSTART )
!
! --- Input and output files directory
!
      DIR_TLE  = '/home/nhabana/data/tle_con/20230705'
      DIROUT   = '/home/nhabana/data/tle_azel/computed_20230705_cea'
      
!@@@!      FILIN  = TRIM(DIR)//'/'//'gnss_act.tle'

!
! --- Define time epoch to compute for the start 
!
      IUER = -1
      DATE_BEG = '2023.07.05_12:00:00'
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, UTC_BEG, IUER ) 
!
! --- Time steps
!
      DTIM = 60.D0             ! [1 min]
      NEPC = 360           ! [6 hrs]
!     
! --- convert time to TAI
!
      CALL NERS_GET_UTCMTAI ( NERS,                                     &
     &                        (MJD_BEG - J2000__MJD)*86400 + UTC_BEG,   &
     &                        UTC_MTAI, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 2102, IUER, 'TLE_EXAMPLE_TO_AZEL',              &
     &                  'Error in extracting UTC minus TAI function' )
         RETURN
      END IF
!
      TAI_BEG =  UTC_BEG - UTC_MTAI
!
! --- Initial Time in TAI
!
      TIM_TAI = (MJD_BEG - J2000__MJD)*86400.0D0 + TAI_BEG
!
! --- Load NERS
!
      IUER = -1
      CALL NERS_LOAD ( NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
         CALL ERR_LOG ( 2103, IUER, 'TLE_EXAMPLE_TO_AZEL',              &
     &           'Error in an attempt to retrieve NERS forecast '//     &
     &           'parameters from the remote server' )
         CALL EXIT ( 1 )
      END IF
!
! --- Initialise the file count and directory level
!
      L_FIL = 0
      LEV   = 0
      L_TLE = 0
!     
! --- Go through the TLE files in the folder
!
      DO 410 K1 = 1, EPH__MFIL
!
! ------ Is there a file in the directory? 
!
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_TLE, FIL_TLE )
         IF ( IS .NE. 0 ) THEN
            IUER = -2
            CALL ERR_LOG ( 2104, IUER, 'TLE_EXAMPLE_TO_AZEL',           &
     &              'Error in reading input directory '//               &
     &              TRIM(DIR_TLE)//' '//FIL_TLE )
            CALL EXIT ( 1 )
         END IF
!
! ------ If we have all the files in the directory
!
         IF ( LEV == 0 ) GOTO 420
!
! ------ Bypass temporary files created by emacs
!
         IF ( INDEX ( FIL_TLE, '#' ) > 0 ) GOTO 410
         IF ( INDEX ( FIL_TLE, '~' ) > 0 ) GOTO 410
!
         L_FIL = L_FIL + 1
!
! ------ More files in directory than expected
!
         IF ( L_FIL > EPH__MFIL ) THEN
            IUER = -2
            CALL CLRCH ( STR )
            CALL INCH  ( EPH__MFIL, STR )
            CALL ERR_LOG ( 2105, IUER, 'TLE_EXAMPLE_TO_AZEL',           &
     &              'Too many files were found in the input directory'  &
     &              //TRIM(DIR_TLE)//' more than EPH__MFIL '            &
     &              //TRIM(STR)//'. Please raise EPH__MFIL' )
             CALL EXIT ( 1 )
         END IF
!
! ------ Check if the file is a TLE file.  
! ------ If it is, go through the file TLE file and compute azel and 
!
! ------ N.B: - Such files will have extension ".tle" and thus be
!               longer than 4 characters in name.
!
         CALL EXWORD ( FIL_TLE, MIND, LIND, IND, DELIM_DIR, IUER )
         LN = IND(2,LIND) - IND(1,LIND)
! ------
         IF ( (LN .GT. 4)                                        .AND.  &
     &        (FIL_TLE(IND(2,LIND)-3:IND(2,LIND)) .EQ. '.tle') ) THEN
!
! --------- TLE file count
!
            L_TLE = L_TLE + 1
! ---------
            CALL CLRCH ( SAT_NAM )
            SAT_NAM = FIL_TLE( IND(1,LIND):IND(2,LIND)-4 )
!
! --------- Open file with the sateli na me
!
            FILOUT(L_TLE) = TRIM(DIROUT)//'/'//TRIM(SAT_NAM)//'.eph'
!@@@!            PRINT *, L_TLE, FILOUT
            OPEN ( UNIT = 11, FILE = TRIM(FILOUT(L_TLE)),               &
     &             STATUS = 'UNKNOWN' )
!     
! --------- Parse the file to derived type.
!
            IUER = -1
            CALL TLE_PARSER ( FIL_TLE, EPH(L_TLE), IUER )
!             
            DO 430 K2 = 1, NEPC
!
! ------------
!
               X_TRS    = 0.D0
               XDOT_TRS = 0.D0
! ------------
               MJD = MJD_BEG
               TAI = TAI_BEG + (K2 - 1)*DTIM
               UTC = TAI + UTC_MTAI
!     
! ------------ compute the ECEF coordinates
!
               IUER = -1
               CALL TLE_TO_TRS ( EPH(L_TLE), MJD, TAI, STA_POS,         &
     &                           X_TRS, XDOT_TRS, AZ, EL, IUER  )
!
! ------------
!
               IUER  = -1
               CDATE = MJDSEC_TO_DATE( MJD, UTC, IUER )
!
! ------------ If visible write to file
!
               IF ( EL/DEG__TO__RAD .GT. 15.D0 ) THEN

                  WRITE ( 11, 112 ) SAT_NAM, CDATE, AZ/DEG__TO__RAD , EL/DEG__TO__RAD
!
!                  ICNT = ICNT + 1
 !           PRINT *, ICNT, " ", TRIM(FILOUT(J1)), " ", EL/DEG__TO__RAD, " ", AZ/DEG__TO__RAD
               END IF
 430        CONTINUE
            CLOSE ( UNIT = 11 )
            
         END IF




 410  CONTINUE

 420  CONTINUE

     
      CALL CPU_TIME ( TFINISH )
!
      WRITE ( 6, 111 ) TFINISH - TSTART  
!
 101  FORMAT ( 'Processing Sta:  ', A, ' ', I4, ' of ', I4 )
!
 111  FORMAT( 'Runtime = ', F12.2, ' Secs' )
!
 112  FORMAT( 'Sat: ', A16, ' Date: ', A24, ' Az: ', F6.2, ' El:', F6.2 )
!     
      END PROGRAM !#! TLE_DIR_TO_AZEL
