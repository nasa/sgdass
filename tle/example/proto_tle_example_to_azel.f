     PROGRAM   TLE_EXAMPLE_TO_AZEL
      IMPLICIT   NONE
      INCLUDE     'ners.i'
      INCLUDE     'ners_local.i'
      INCLUDE     'astro_constants.i'
      INCLUDE     'tle_sgp4.i'
      TYPE       ( NERS__TYPE ) :: NERS
      TYPE       ( EPH__TYPE )  :: EPH(1024)
      CHARACTER  DIR*64, FILIN*128, FILOUT(1024)*128, STR*128
      INTEGER*4  IUER, IER, IMOD, NTLE
      REAL*8     TSTART, TFINISH
      INTEGER*4  J1, J2, J3, J4, J5
      INTEGER*8  STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4)*GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
      CHARACTER   TLEDATE*30, DATE_BEG*21
      CHARACTER   NERS_CONFIG*128, HOME_DIR*128
      REAL*8      UTC, DT, UTC_BEG, EPOCH
      REAL*8      TAI_BEG, UTC_MTAI, TIM_TAI
      REAL*8      X_TRS(3), XDOT_TRS(3), STA_POS(3)
      INTEGER*4   MJD, MJD_BEG
      CHARACTER  DATE_STR*32, REFR_MODE*5, C_PAR*16
      CHARACTER  CDATE*22
      INTEGER*4  M_PAR, L_PAR, ICNT
      PARAMETER  ( M_PAR = NERS__MPAR )
      REAL*8     PARS(M_PAR), AZ, EL
      LOGICAL*1   LEX
      CHARACTER,  EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN
! ---
      CALL GETENVAR ( 'NERS_CONFIG', NERS_CONFIG )
      IF ( NERS_CONFIG == ' ' ) THEN
!
! ------ Second, check $HOME/.ners_config file
!
         CALL GETENVAR ( 'HOME', HOME_DIR )
         NERS_CONFIG = TRIM(HOME_DIR)//'/.ners_config'
         INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
!
! --------- Third, check for the system-wide ners configuration file 
!
            NERS_CONFIG = NERS__CONFIG
         END IF
      END IF
! ---
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 2102, IUER, 'TLE_EXAMPLE_TO_AZEL',                    &
     &                  'Error in initializing NERS data structure' )
         RETURN
      END IF

!
! --- Get intrinsic date and time
!
      CALL CPU_TIME ( TSTART )
!
! --- Month and directories
!
      DIR  =  '/progs/tle_20230331/share'
      FILIN  = TRIM(DIR)//'/'//'gnss_act.tle'
!
! --- Extract TLE's from the main file
!
      CALL TLE_BREAK( FILIN, FILOUT, NTLE, IUER )
!
! --- Define time epoch to compute for the stat 
!
      IUER = -1
      DATE_BEG = '2022.05.04_20:15:00'
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, UTC_BEG, IUER ) 
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
      TAI_BEG = UTC_BEG - UTC_MTAI
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
         CALL ERR_LOG ( 1930, IUER, 'TLE_EXAMPLE_TO_AZEL',           &
     &           'Error in an attempt to retrieve NERS forecast '//     &
     &           'parameters from the remote server' )
         CALL EXIT ( 1 )
      END IF
!
! --- Ground Station
!
      STA_POS(1)  =  1130730.331D0   !GGAO12M
      STA_POS(2)  = -4831246.540D0   !GGAO12M
      STA_POS(3)  =  3994228.904D0   !GGAO12M
!
! --- Go through the TLE files and 
!
      ICNT = 0
      DO 310 J1 = 1, NTLE
         X_TRS = 0.D0
         XDOT_TRS = 0.D0


            PRINT *, "%%%%%%%%%%% TLE_EXAMPLE_TO_AZEL - 117 %%%%%%%%%"
            PRINT *, "NTLE=",J1, " ", TRIM(FILOUT(J1))
!
! ------ Parse TLE file to derived type
!
         IUER = -1
         CALL TLE_PARSER ( FILOUT(J1), EPH(J1), IUER )     
!
! ------ compute the ECEF coordinates
!       
         IUER = -1
         CALL TLE_TO_TRS ( EPH(J1), MJD_BEG, TAI_BEG, STA_POS,          &
     &                     X_TRS, XDOT_TRS, AZ, EL, IUER )

!
! ------ List all visible satellites
!
         IF ( EL/DEG__TO__RAD .GT. 10.D0 ) THEN
            ICNT = ICNT + 1
            PRINT *, ICNT, " ", TRIM(FILOUT(J1)), " ", EL/DEG__TO__RAD, " ", AZ/DEG__TO__RAD
         END IF

 310  CONTINUE


!
! ----Compute 
!
      CALL CPU_TIME ( TFINISH )
!
      WRITE ( 6, 111 ) TFINISH - TSTART  
!
 101  FORMAT ( 'Processing Sta:  ', A, ' ', I4, ' of ', I4 )
!
 111  FORMAT( 'Runtime = ', F12.2, ' Secs' )
!
      END PROGRAM
!
! ------------------------------------------------------------------------
!
      SUBROUTINE TLE_BREAK( FILIN, FILOUT, NTLE, IUER )
!
! *  Break file with multiple TLE's into indivudal TLE's
!
      IMPLICIT   NONE
      CHARACTER  FILIN*128, FILOUT(1024)*128, STR*128, DIR*64
      CHARACTER  OUT(3)*256
      INTEGER*4  IUER, IER, NTLE
      REAL*8     TSTART, TFINISH
      INTEGER*4  J1, J2, J3, J4, J5, J10, J11, J12
      INTEGER*4  MIND, MP, MAXL_STRING, NP                   ! Max. String length
      PARAMETER  ( MAXL_STRING = 1024 )
      PARAMETER  ( MP = 128*1024 )
      PARAMETER  ( MIND = 512 )
      CHARACTER  DELIM*5
      PARAMETER  ( DELIM =  CHAR(0)//CHAR(32)//CHAR(9)//'='//':' ) ! Null, Space, Tab, Equal-sign, Colon
      CHARACTER  BUF(MP)*(MAXL_STRING), SECT_ID*32                 ! Read File, File Section
!
! --- 
!
      DIR  =  '/progs/tle_20230331/share/gnss_tle'
!
! --- open the input file
!
      CALL RD_TEXT  ( FILIN, MP, BUF, NP, IER ) 
      IF ( IER .NE. 0 ) THEN                       ! i.e. if there is error
           CALL ERR_LOG ( 8001, IUER, 'TLE_BREAK',                      &
     &            'Error in reading input TLE file '//FILIN )                  
           RETURN 
      END IF
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
! --- Fix this after you have done this computation
!
      NTLE = NP/3
!
! --- Go through file writing each TLE to an individual file
!
      
      DO 310 J1 = 1, NTLE
         J10 = J1*3 - 2
         J11 = J1*3 - 1
         J12 = J1*3
!
! ---
!     
         CALL CLRCH (OUT)
         OUT(1) = BUF(J10)  
         OUT(2) = BUF(J11)          
         OUT(3) = BUF(J12)  

         FILOUT(J1) = TRIM(DIR)//'/'//TRIM(OUT(1))//'.tle'

         IUER = -1
         CALL WR_TEXT ( 3, OUT, FILOUT(J1), IUER ) 
!
! ------
!   
 310  CONTINUE


      RETURN
      END SUBROUTINE
