      PROGRAM    REFINE_GNSS_KEY
! ************************************************************************
! *                                                                      *
! *   Program  REFINE_GNSS_KEY
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 10-JAN-2022  REFINE_GNSS_KEY  v2.3 (d) L. Petrov 15-MAY-2026 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      INTEGER*4  MP, MIND, MS, MO, MT
      TYPE       ( NERS__TYPE ) :: NERS
      PARAMETER  ( MP   = 4*1024*1024 )
      PARAMETER  ( MIND =          32 )
      PARAMETER  ( MS   =         256 )
      PARAMETER  ( MO   =     128*1024 )
      PARAMETER  ( MT   =    1024*1024 )
      CHARACTER  FILIN_KEY*128, FILIN_VEX*128, FILOUT_KEY*128, FILOUT_VEX*128, &
     &           FIL_SPIND*128, FILSTA*128, FILLOC*128, FILLPS*128
      CHARACTER  BUFK(MO)*512, BUFV(MO)*256, BUFS(MP)*256, OUT(MP)*256, &
     &           STR*128, C_STA(MS)*8, C_DB(MS)*8, REF_STA*8, &
     &           SOU_NAM*8, TIM_STR*12, RA_TPC_STR*13, DEC_TPC_STR*11, &
     &           RA_GCN_STR*13, DEC_GCN_STR*11, &
     &           NERS_CONFIG*128, VEX_DATE_STR*19, DATE_STR*19, MODE*12, &
     &           TLE_INP_DIR*128, TLE_EXP_DIR*128, TLE_LIST(MT)*128, &
     &           TLE_INP_FILE*128, TLE_EXP_FILE*128
      INTEGER*4  NI, NV, NO, NS, IP, J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           ID, LIND, IND(2,MIND), MJD_BEG, L_STA, I_STA, K_STA, &
     &           IND_TPC_STA, L_TLE, I_TLE, IUER
      LOGICAL*1  LEX, FL_READ_STA_COO
      REAL*8     VAL, TAI_BEG, TAI_MID, DUR, UTC_M_TAI, RA, DEC
      REAL*8     AZ_MID, EL_MID, HA, AZ_RATE, EL_RATE, HA_RATE, UTC_BEG, DST, &
     &           STA_COO(3,MS), AZ(MS), EL(MS)
      CHARACTER  REFINE_GNSS_KEY__LABEL*36
      PARAMETER  ( REFINE_GNSS_KEY__LABEL = 'refine_gnss_key v 2.2  of 2025.09.30' )
      INTEGER*4, EXTERNAL :: IFIND_SORT_CH, I_LEN, ILEN, LINDEX, LTM_DIF
      CHARACTER, EXTERNAL :: VEX_TO_DATE*19, MJDSEC_TO_DATE*30, GET_CDATE*19
!
      IF ( IARGC() < 7 ) THEN
           WRITE ( 6, '(A)' ) 'refine_gnss_key prelim_key prelim_vex mode ref_sta highres_spind fine_key fine_vex'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN_KEY  )
           CALL GETARG ( 2, FILIN_VEX  )
           CALL GETARG ( 3, MODE       )
           CALL GETARG ( 4, REF_STA    )
           CALL GETARG ( 5, FIL_SPIND  )
           CALL GETARG ( 6, FILOUT_KEY )
           CALL GETARG ( 7, FILOUT_VEX )
      END IF
!
      IF ( MODE(1:7) == 'key_tpc' .OR. MODE == 'key_tle' .OR. MODE == 'azel' ) THEN
           CONTINUE 
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 2711, IUER, 'REFINE_GNSS_KEY', 'Unsupported value of the 3rd '// &
     &         'argument '//TRIM(MODE)//' : only key_tpc or key_tle, or azel are suported' )
           CALL EXIT ( 1 ) 
      END IF
!
      CALL GETENVAR ( 'HOME', NERS_CONFIG )
      NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
      INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           NERS_CONFIG = NERS__CONFIG
      END IF
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2712, IUER, 'REFINE_GNSS_KEY', 'Error in initializing '// &
     &         'NERS data structure' )
           RETURN 
      END IF
!
      IUER = -1
      CALL NERS_LOAD ( NERS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2713, IUER, 'REFINE_GNSS_KEY', 'Error in '// &
     &         'an attempt to retrieve NERS forecast parameters '// &
     &         'form the remote server' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILIN_KEY, MO, BUFK, NI, IUER )
      IF ( IUER .NE. 0 ) CALL  EXIT ( 1 )
!
      IUER = -1
      CALL RD_TEXT ( FILIN_VEX, MO, BUFV, NV, IUER )
      IF ( IUER .NE. 0 ) CALL  EXIT ( 1 )
!
      IUER = -1
      CALL RD_TEXT ( FIL_SPIND, MP, BUFS, NS, IUER )
      IF ( IUER .NE. 0 ) CALL  EXIT ( 1 )
!
      IND_TPC_STA = 0
      UTC_M_TAI = 0.0
      FL_READ_STA_COO = .FALSE.
      CALL CLRCH ( FILSTA )
      CALL CLRCH ( FILLOC )
      DO 410 J1=1,NI ! input key file
         IF ( ILEN(BUFK(J1)) == 0 ) GOTO 410
         CALL EXWORD ( BUFK(J1), MIND, LIND, IND, '=,"'//CHAR(32), IUER )
         IF ( LIND .GE. 7 ) THEN
              IF ( BUFK(J1)(IND(1,6):IND(2,6)) == 'UTC_M_TAI:' ) THEN
                   READ ( UNIT=BUFK(J1)(IND(1,7):IND(2,7)), FMT='(F5.1)' ) UTC_M_TAI
              END IF             
         END IF
!
         IF ( BUFK(J1)(1:1) == '!' ) THEN
              IF ( BUFK(J1)(1:12) == "!leapsecfile" ) THEN
                   FILLPS = BUFK(J1)(IND(1,2):IND(2,2))
              END IF
              IF ( BUFK(J1)(1:12) == "!tleinpdir" ) THEN
                   TLE_INP_DIR = BUFK(J1)(IND(1,2):IND(2,2))
              END IF
              IF ( BUFK(J1)(1:12) == "!tleexpdir" ) THEN
                   TLE_EXP_DIR = BUFK(J1)(IND(1,2):IND(2,2))
              END IF
!
              IF ( INDEX ( BUFK(J1), '__@REF_PROGNAME@__' ) > 0 ) THEN
                   IP = INDEX ( BUFK(J1), '__@REF_PROGNAME@__' )
                   BUFK(J1)(IP:IP+LEN(REFINE_GNSS_KEY__LABEL)-1) = REFINE_GNSS_KEY__LABEL
              END IF
              IF ( INDEX ( BUFK(J1), '__@REF_COMMAND@__' ) > 0 ) THEN
                   IP = INDEX ( BUFK(J1), '__@REF_COMMAND@__' )
                   CALL GET_COMMAND ( STR ) 
                   BUFK(J1)(IP:IP+LEN(STR)-1) = STR
              END IF
              IF ( INDEX ( BUFK(J1), '__@REF_DATE@__' ) > 0 ) THEN
                   IP = INDEX ( BUFK(J1), '__@REF_DATE@__' )
                   STR = GET_CDATE()
                   BUFK(J1)(IP:IP+LEN(STR)-1) = STR
              END IF
              CONTINUE 
           ELSE IF ( INDEX ( BUFK(J1), 'stafile' ) > 0 ) THEN
              FILSTA = BUFK(J1)(IND(1,2):IND(2,2))
           ELSE IF ( INDEX ( BUFK(J1), 'locfile'  ) > 0 ) THEN
              FILLOC = BUFK(J1)(IND(1,2):IND(2,2))
           ELSE IF  ( BUFK(J1)(IND(1,1):IND(2,1)) == 'satinit' ) THEN
              IF ( MODE == 'key_tpc' ) THEN
                   BUFK(J1) = '@ '//BUFK(J1) 
              END IF
           ELSE IF  ( BUFK(J1)(IND(1,1):IND(2,1)) == 'satname' ) THEN
              IF ( MODE == 'key_tpc' ) THEN
                   BUFK(J1) = '@ '//BUFK(J1) 
              END IF
           ELSE IF  ( BUFK(J1)(IND(1,1):IND(2,1)) == 'endsat' ) THEN
              IF ( MODE == 'key_tpc' ) THEN
                   BUFK(J1) = '@ '//BUFK(J1) 
              END IF
!
              IUER = -1
              CALL GET_TLE_LIST ( TLE_INP_DIR, MP, L_TLE, TLE_LIST, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 2714, IUER, 'REFINE_GNSS_KEY', 'Error '// &
     &                 'in an attempt to get the list of satellite '// &
     &                 'orbit files from the input directory '//TLE_INP_DIR )
                   CALL EXIT ( 1 )
              END IF
           ELSE IF  ( BUFK(J1)(IND(1,1):IND(2,1)) == 'srccat' ) THEN
              IF ( MODE == 'key_tle' ) THEN
                   BUFK(J1) = '@ '//BUFK(J1) 
              END IF
           ELSE IF  ( BUFK(J1)(IND(1,1):IND(2,1)) == 'source' ) THEN
              IF ( MODE == 'key_tle' ) THEN
                   BUFK(J1) = '@ '//BUFK(J1) 
              END IF
           ELSE IF  ( BUFK(J1)(IND(1,1):IND(2,1)) == 'endcat' ) THEN
              IF ( MODE == 'key_tle' ) THEN
                   BUFK(J1) = '@ '//BUFK(J1) 
              END IF
           ELSE IF  ( BUFK(J1)(IND(1,1):IND(2,1)) == 'kerfile' ) THEN
              IF ( ILEN(FILLPS) > 0 ) THEN
                   BUFK(J1)(IND(1,2):) = FILLPS
              END IF
           ELSE IF ( INDEX ( BUFK(J1), "STATIONS" ) > 0 ) THEN
              IF ( .NOT. FL_READ_STA_COO ) THEN
                   IF ( ILEN(FILSTA) == 0 )  THEN
                        IUER = -1
                        CALL ERR_LOG ( 2715, IUER, 'REFINE_GNSS_KEY', 'Error '// &
     &                      'in parsing the key file '//TRIM(FILIN_KEY)//' -- '// &
     &                      'station file was not found' )
                        CALL EXIT ( 1 )
                   END IF
                   IUER = -1
                   CALL READ_STA_COO ( FILSTA, FILLOC, MP, L_STA, &
     &                                 C_STA, C_DB, STA_COO, IUER )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 2716, IUER, 'REFINE_GNSS_KEY', 'Error '// &
     &                      'in an attempt to get the station list by '// &
     &                      'processing input files '//TRIM(FILSTA)//' and '// &
     &                       FILLOC )
                        CALL EXIT ( 1 )
                   END IF
                   FL_READ_STA_COO = .TRUE.
              END IF
              K_STA = LIND - 1
              DO 430 J3=2,LIND
                 I_STA = LTM_DIF ( 0, L_STA, C_STA, &
     &                             BUFK(J1)(IND(1,J3):IND(2,J3)) )
                 IF ( I_STA < 1 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J1, STR )
                      IUER = -1
                      CALL ERR_LOG ( 2717, IUER, 'REFINE_GNSS_KEY', 'Error in '// &
     &                    'processing line '//TRIM(STR)//' of the key file '// &
     &                     TRIM(FILIN_KEY)//' : station '// &
     &                     BUFK(J1)(IND(1,J3):IND(2,J3))// &
     &                    ' was not defined in the station file '//TRIM(FILSTA)// &
     &                    ' or location file '//TRIM(FILLOC) )
                      write ( 6, * ) 'C_STA= ', C_STA(1:L_STA)//' ' 
                      write ( 6, * ) 'C_DB = ', C_DB(1:L_STA)//' ' 
                      write ( 6, * ) 'J3= ', J3
                      CALL EXIT ( 1 )
                 END IF
 430          CONTINUE 
              IF ( MODE == 'key_tpc' .OR. MODE == 'azel' ) THEN
                   IND_TPC_STA = LTM_DIF ( 0, L_STA, C_STA, REF_STA )
                   IF ( IND_TPC_STA < 1 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 2718, IUER, 'REFINE_GNSS_KEY', 'Reference '// &
     &                      'statin '//REF_STA//' was not defined in the location '// &
     &                      'file '//FILLOC )
                        CALL EXIT ( 1 )
                   END IF
                 ELSE
                   IND_TPC_STA = 1
              END IF
           ELSE IF ( INDEX ( BUFK(J1), "dur" ) > 0 ) THEN
              VEX_DATE_STR = BUFK(J1)(IND(1,2):IND(2,2))//'y'// &
     &                       BUFK(J1)(IND(1,4):IND(2,4))//'d'// &
     &                       BUFK(J1)(IND(1,6):IND(2,6))//'s'
              VEX_DATE_STR(12:12) = 'h'
              VEX_DATE_STR(15:15) = 'm'
              IUER = -1
              DATE_STR = VEX_TO_DATE ( VEX_DATE_STR, IUER )
              IUER = -1
              CALL DATE_TO_TIME ( DATE_STR, MJD_BEG, UTC_BEG, IUER ) 
              TAI_BEG = UTC_BEG - UTC_M_TAI
!
              READ ( UNIT=BUFK(J1)(IND(1,10):IND(2,10)), FMT='(F10.2)' ) DUR
              TAI_MID = NINT(TAI_BEG + DUR/2.0D0)
              CALL SR_TAT ( TAI_MID, VAL )
              CALL RH_TAT ( VAL, 1, TIM_STR, IUER  )
              CALL CHASHL ( TIM_STR )
              TIM_STR(3:3) = ':'
              TIM_STR(6:6) = ':'
              CALL CLRCH ( RA_GCN_STR  )
              CALL CLRCH ( DEC_GCN_STR )
!
              SOU_NAM = BUFK(J1)(IND(1,8):IND(2,8))
              DO 440 J4=1,NS
                 IF ( SOU_NAM(1:3)      == BUFS(J4)(81:83) .AND. &
     &                BUFS(J4)(159:166) == TIM_STR(1:8)          ) THEN
                      RA_GCN_STR  = BUFS(J4)(13:23)
                      DEC_GCN_STR = BUFS(J4)(26:36)
                      READ ( UNIT=BUFS(J4)(127:139), FMT='(F13.2)' ) DST
                 END IF
 440          CONTINUE 
!
              IF ( ILEN(RA_GCN_STR) == 0 ) THEN
                   IUER = -1
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2719, IUER, 'REFINE_GNSS_KEY', 'Cannot '// &
     &                 'find a record in high-res spind for line '//STR )
                   CALL EXIT ( 1 )
              END IF
!
              DO 450 J5=1,K_STA
                 IUER = -1
                 CALL HR_TAT ( RA_GCN_STR,  RA, IUER )
!
                 IUER = -1
                 CALL GR_TAT ( DEC_GCN_STR, DEC, IUER )
!
                 IUER = -1
                 CALL UPDATE_RADEC ( NERS, MJD_BEG, TAI_MID, STA_COO(1,IND_TPC_STA), &
     &                               RA, DEC, DST, AZ, EL, IUER  )
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 2720, IUER, 'REFINE_GNSS_KEY', 'Error in '// &
     &                    'an attempt to update ra/dec for the finite distance' )
                      CALL EXIT ( 1 )
                 END IF
!
                 IUER = -1
                 CALL RH_TAT ( RA,  3, RA_TPC_STR,  IUER )
                 IUER = -1
                 CALL RG_TAT ( DEC, 2, DEC_TPC_STR, IUER )
                 IF ( DEC > 0 ) THEN
                      DEC_TPC_STR(1:1) = '+'
                 END IF
 450          CONTINUE 
!
              IF ( MODE(1:7) == 'key_tpc' ) THEN
                   DO 460 J6=1,NI
                      IF ( BUFK(J6)(1:7) == 'source=' .AND. BUFK(J6)(9:16) == SOU_NAM ) THEN
                           BUFK(J6)(23:35) = RA_TPC_STR(2:)
                           BUFK(J6)(42:52) = DEC_TPC_STR
                           BUFK(J6)(25:25) = ':'
                           BUFK(J6)(28:28) = ':'
                           BUFK(J6)(45:45) = ':'
                           BUFK(J6)(48:48) = ':'
                           BUFK(J6)(81:)   = 'GPS satellite '//SOU_NAM(1:3)//' at station '//C_STA(IND_TPC_STA)
                      END IF
 460               CONTINUE 
                 ELSE IF ( MODE == 'key_tle' ) THEN
                   CALL CLRCH ( BUFK(J1)(IND(1,8)+3:IND(2,8)) )
              END IF
         END IF
 410  CONTINUE 
!
      DO 470 J7=1,NV ! input vex file
         IF ( ILEN(BUFV(J7)) == 0 ) GOTO 470
         CALL EXWORD ( BUFV(J7), MIND, LIND, IND, CHAR(32)//';', IUER )
         IF ( BUFV(J7)(IND(1,1):IND(2,1)) == 'source_name' ) THEN
              CALL EXWORD ( BUFV(J7+2), MIND, LIND, IND, CHAR(32)//';', IUER )
              RA_GCN_STR = BUFV(J7+2)(IND(1,3):IND(1,3)+1)//'_'// &
     &                 BUFV(J7+2)(IND(1,3)+3:IND(1,3)+4)//'_'// &
     &                 BUFV(J7+2)(IND(1,3)+6:IND(1,3)+14)
              CALL EXWORD ( BUFV(J7+3), MIND, LIND, IND, CHAR(32)//';', IUER )
              DEC_GCN_STR = BUFV(J7+3)(IND(1,3):IND(1,3)+2)//'_'// &
     &                  BUFV(J7+3)(IND(1,3)+4:IND(1,3)+5)//'_'// &
     &                  BUFV(J7+3)(IND(1,3)+7:IND(1,3)+14)
!
              IUER = -1
              CALL HR_TAT ( RA_GCN_STR,  RA, IUER )
!
              IUER = -1
              CALL GR_TAT ( DEC_GCN_STR, DEC, IUER )
!
              IF ( MODE .NE. 'key_tle' ) THEN
                   IUER = -1
                   CALL UPDATE_RADEC ( NERS, MJD_BEG, TAI_MID, STA_COO(1,IND_TPC_STA), &
     &                                 RA, DEC, DST, AZ, EL, IUER  )
                   IF ( IUER .NE. 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 2721, IUER, 'REFINE_GNSS_KEY', 'Error in '// &
     &                      'an attempt to update ra/dec for the finite distance' )
                        CALL EXIT ( 1 )
                   END IF
              END IF
!
              IUER = -1
              CALL RH_TAT ( RA,  3, RA_TPC_STR,  IUER )
              CALL CHASHL ( RA_TPC_STR )
              IUER = -1
              CALL RG_TAT ( DEC, 2, DEC_TPC_STR, IUER )
              IF ( DEC > 0 ) THEN
                   DEC_TPC_STR(1:1) = '+'
              END IF
!
              CALL EXWORD ( BUFV(J7+2), MIND, LIND, IND, CHAR(32)//';', IUER )
              IF ( MODE == 'key_tpc' ) THEN
                   BUFV(J7+2)(IND(1,3):) = RA_TPC_STR(1:2)//'h'//RA_TPC_STR(4:5)//'m'//RA_TPC_STR(7:13)//'00s;'
                 ELSE
                   BUFV(J7+2)(IND(1,3):) = RA_GCN_STR(1:2)//'h'//RA_GCN_STR(4:5)//'m'//RA_GCN_STR(7:13)//'00s;'
              END IF
!
              CALL EXWORD ( BUFV(J7+3), MIND, LIND, IND, CHAR(32)//';', IUER )
              IF ( MODE == 'key_tpc' ) THEN
                   BUFV(J7+3)(IND(1,3):) = DEC_TPC_STR(1:3)//'d'//DEC_TPC_STR(5:6)//"'"//DEC_TPC_STR(8:11)//'0000";'
                 ELSE
                   BUFV(J7+3)(IND(1,3):) = DEC_GCN_STR(1:3)//'d'//DEC_GCN_STR(5:6)//"'"//DEC_GCN_STR(8:11)//'0000";'
              END IF
         END IF
 470  CONTINUE 
!
      NO = 0 
      DO 480 J8=1,NI
         IF ( BUFK(J8)(1:1) == '@' ) GOTO 480
         CALL EXWORD ( BUFK(J8), MIND, LIND, IND, '=,"'//CHAR(32), IUER )
         IF  ( BUFK(J8)(IND(1,1):IND(2,1)) == 'satname' .AND. &
     &         MODE == 'key_tle' ) THEN
               TLE_INP_FILE = BUFK(J8)(IND(1,6):IND(2,6)) 
               ID = LINDEX ( TLE_INP_FILE, '/' ) + 1
               I_TLE = 0
               DO 490 J9=1,L_TLE
                  IF ( INDEX ( TRIM(TLE_LIST(J9)), TRIM(TLE_INP_FILE(ID:)) ) > 0 ) THEN
                       I_TLE = J9
               END IF
 490           CONTINUE 
               IF ( I_TLE < 1 ) THEN
                    IUER = -1
                    WRITE ( 6, * ) 'L_TLE = ', L_TLE
                    WRITE ( 6, * ) 'TLE_LIST(1) = ', TRIM(TLE_LIST(1))
                    WRITE ( 6, * ) 'TLE_LIST(2) = ', TRIM(TLE_LIST(3))
                    WRITE ( 6, * ) 'TLE_LIST(3) = ', TRIM(TLE_LIST(2))
                    CALL ERR_LOG ( 2722, IUER, 'REFINE_GNSS_KEY', 'Trap '// &
     &                  'of internal control: tle file '//TRIM(TLE_INP_FILE)// &
     &                  ' was not found in directory '//TLE_INP_DIR )
                    CALL EXIT ( 1 )
               END IF
!
               IUER = -1
               CALL EXPORT_TLE_FILE ( TLE_LIST(I_TLE), TLE_EXP_DIR, TLE_EXP_FILE, IUER )
               IF ( IUER .NE. 0 ) THEN
                    IUER = -1
                    CALL ERR_LOG ( 2723, IUER, 'REFINE_GNSS_KEY', 'Error '// &
     &                  'in an attempt to export the satellite '// &
     &                  'orbit files to the output directory '//TLE_EXP_DIR )
                    CALL EXIT ( 1 )
               END IF
               BUFK(J8)(IND(1,6):) = TRIM(TLE_EXP_FILE)//' /'
         END IF
         NO = NO + 1 
         OUT(NO) = BUFK(J8)
 480  CONTINUE 
!
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FILOUT_KEY, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2724, IUER, 'REFINE_GNSS_KEY', 'Error '// &
     &         'in writing the output key file '//FILOUT_KEY )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Output file: '//TRIM(FILOUT_KEY)
!
      IUER = -1
      CALL WR_TEXT ( NV, BUFV, FILOUT_VEX, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 2725, IUER, 'REFINE_GNSS_KEY', 'Error '// &
     &         'in writing the output key file '//FILOUT_VEX )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Output file: '//TRIM(FILOUT_VEX)
!
      END  PROGRAM  REFINE_GNSS_KEY  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UPDATE_RADEC ( NERS, MJD, TAI, COO_TRS, RA, DEC, DST, &
     &                          AZ, EL, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine  UPDATE_RADEC computes topocentric right ascensions and    *
! *   declinations for an object with geocentric right ascensions,       *
! *   declination and distance DST.                                      *
! *                                                                      *
! *  ### 15-JAN-2022 UPDATE_RADEC  v1.0 (c)  L. Petrov  15-JAN-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      TYPE       ( NERS__TYPE ) :: NERS
      INTEGER*4  MJD, IUER
      REAL*8     TAI, COO_TRS(3), RA, DEC, DST
      REAL*8     AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE, UTC_BEG
      REAL*8     RA_USE, DEC_USE, RA_DELTA, DEC_DELTA
      REAL*8     AZ_RA, AZ_DEC, EL_RA, EL_DEC, AZ1, EL1, RD, EPS
      REAL*8     ZEN_VEN(3), S_VEN(3), RAD, AZ_NZO, EL_NZO, MAT(2,2)
      CHARACTER  STR*30
      INTEGER*4  MIT
      PARAMETER  ( MIT = 8 ) 
      INTEGER*4  J1, J2, J3, IER 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      EPS = 1.D-8
!
      RAD = DSQRT ( COO_TRS(1)**2 + COO_TRS(2)**2 + COO_TRS(3)**2 )
!
      CALL ERR_PASS ( IUER, IER )
      CALL NERS_AZELHA_COMP ( NERS, (MJD - J2000__MJD)*86400.0D0 + TAI, &
     &                        COO_TRS, RA, DEC, 'radio', &
     &                        AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2726, IUER, 'UPDATE_RADEC', 'Error in computation '// &
     &         'of az/el' )
           RETURN
      END IF
!!     write ( 6, * ) 'ORIG: AZO/EL= ', sngl(az/deg__to__rad), sngl(el/deg__to__rad) ! %%%%%%%%%
!!     write ( 6, * ) 'ORIG: RA/DEC= ', sngl(ra/deg__to__rad), sngl(dec/deg__to__rad) ! %%%%%%%%%
!
      ZEN_VEN(1) = 0.0D0
      ZEN_VEN(2) = 0.0D0
      ZEN_VEN(3) = RAD
      S_VEN(1) = DCOS(EL)*DCOS(AZ)
      S_VEN(2) = DCOS(EL)*DSIN(AZ)
      S_VEN(3) = DSIN(EL)
      S_VEN    = DST*S_VEN - ZEN_VEN
      CALL DECPOL ( 3, S_VEN, RD, AZ_NZO, EL_NZO, IER )
!
      STR = MJDSEC_TO_DATE ( MJD, TAI, IER )
      RA_USE  = RA
      DEC_USE = DEC
      RA_DELTA  = 0.05D0
      DEC_DELTA = 0.05D0
!
      DO 410 J1=1,MIT
         CALL ERR_PASS ( IUER, IER )
         CALL NERS_AZELHA_COMP ( NERS, (MJD - J2000__MJD)*86400.0D0 + TAI, &
     &                           COO_TRS, RA_USE, DEC_USE, 'radio', &
     &                           AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2721, IUER, 'UPDATE_RADEC', 'Error in computation '// &
     &            'of az/el' )
              RETURN
         END IF
!
! ------ Compute partial derivatives
!
         CALL NERS_AZELHA_COMP ( NERS, (MJD - J2000__MJD)*86400.0D0 + TAI, &
     &                           COO_TRS, RA_USE + RA_DELTA, DEC_USE, 'radio', &
     &                           AZ1, EL1, HA, AZ_RATE, EL_RATE, HA_RATE, IER )
         AZ_RA = (AZ1 - AZ)/RA_DELTA
         EL_RA = (EL1 - EL)/RA_DELTA
!
         CALL NERS_AZELHA_COMP ( NERS, (MJD - J2000__MJD)*86400.0D0 + TAI, &
     &                           COO_TRS, RA_USE, DEC_USE + DEC_DELTA, 'radio', &
     &                           AZ1, EL1, HA, AZ_RATE, EL_RATE, HA_RATE, IER )
         AZ_DEC = (AZ1 - AZ)/DEC_DELTA
         EL_DEC = (EL1 - EL)/DEC_DELTA
!         
         MAT(1,1) = AZ_RA
         MAT(1,2) = AZ_DEC
         MAT(2,1) = EL_RA
         MAT(2,2) = EL_DEC
!
         CALL ERR_PASS ( IUER, IER )
         CALL INVA ( 2, MAT, EPS, IER )
         RA_USE  = RA_USE  + ( MAT(1,1)*(AZ_NZO - AZ) + MAT(1,2)*(EL_NZO - EL) )
         DEC_USE = DEC_USE + ( MAT(2,1)*(AZ_NZO - AZ) + MAT(2,2)*(EL_NZO - EL) )
!         write ( 6, * ) 'Iter: ', int2(j1), ' ra/dec= ', sngl(ra_use), sngl(dec_use), ' Dra= ', sngl( mat(1,1)*(az_nzo - az) + mat(1,2)*(el_nzo - el) ), ' Dde= ', sngl( mat(2,1)*(az_nzo - az) + mat(2,2)*(el_nzo - el) )
!@         write ( 6, * ) 'ITER: ', int2(j1), ' az_dif= ', sngl(az_nzo-az), sngl(el_nzo-el) ! %%%
 410  CONTINUE 
!
      RA  = RA_USE
      DEC = DEC_USE
      CALL NERS_AZELHA_COMP ( NERS, (MJD - J2000__MJD)*86400.0D0 + TAI, &
     &                        COO_TRS, RA, DEC, 'radio', &
     &                        AZ, EL, HA, AZ_RATE, EL_RATE, HA_RATE, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE UPDATE_RADEC  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_STA_COO ( FILSTA, FILLOC, M_STA, L_STA, C_STA, &
     &                          C_DB, STA_COO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_STA_COO  reads station names and station position     *
! *   from NRAO-style station and location files.                        *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 25-MAR-2025  READ_STA_COO  v1.0 (d)  L. Petrov  25-MAR-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M_STA, L_STA, IUER 
      CHARACTER  FILSTA*128, FILLOC*128, C_STA(M_STA)*(*), C_DB(M_STA)*(*)
      REAL*8     STA_COO(3,M_STA)
      INTEGER*4  MP, MIND
      PARAMETER  ( MP    = 1024 )
      PARAMETER  ( MIND  =   64 )
      CHARACTER  BUF(MP)*256
      LOGICAL*1  FL_EXIST, FL_STA_DEF(MP)
      CHARACTER  STR*128, STA_NAM*8, STR1*4096
      INTEGER*4  J1, J2, J3, NSTA, NLOC, LIND, IND(2,MIND), I_STA, &
     &           N_MIS, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      INQUIRE ( FILE=FILSTA, EXIST=FL_EXIST )
      IF ( .NOT. FL_EXIST ) THEN
           CALL ERR_LOG ( 2741, IUER, 'READ_STA_COO', 'Did not find '// &
     &         'station file '//FILSTA )
           RETURN
      END IF
!
      INQUIRE ( FILE=FILLOC, EXIST=FL_EXIST )
      IF ( .NOT. FL_EXIST ) THEN
           CALL ERR_LOG ( 2742, IUER, 'READ_STA_COO', 'Did not find '// &
     &         'location file '//FILLOC )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILLOC, MP, BUF, NLOC, IER )
      IF ( .NOT. FL_EXIST ) THEN
           CALL ERR_LOG ( 2743, IUER, 'READ_STA_COO', 'Error in reading '// &
     &         'location file '//FILLOC )
           RETURN
      END IF
!
      L_STA = 0
      DO 410 J1=1,NLOC
         IF ( BUF(J1)(1:1) == '!' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(32)//CHAR(9)//'=', IER )
         IF ( LIND < 2 ) GOTO 410
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DBNAME' ) THEN
              L_STA = L_STA + 1
              IF ( L_STA > M_STA ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_STA, STR )
                   CALL ERR_LOG ( 2744, IUER, 'READ_STA_COO', 'Too many '// &
     &                 'stations in the location file file '//TRIM(FILLOC)// &
     &                 ' Please update parameter M_STA '//STR )
                   RETURN
              END IF
              C_DB(L_STA) = BUF(J1)(IND(1,2):IND(2,2))
              FL_STA_DEF(L_STA) = .FALSE.
           ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'X' ) THEN
              READ ( UNIT=BUF(J1)(IND(1,2):IND(2,2)), FMT='(F13.4)' ) STA_COO(1,L_STA)
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F13.4)' ) STA_COO(2,L_STA)
              READ ( UNIT=BUF(J1)(IND(1,6):IND(2,6)), FMT='(F13.4)' ) STA_COO(3,L_STA)
         END IF
 410  CONTINUE 
!
      IF ( L_STA == 0 ) THEN
           CALL ERR_LOG ( 2743, IUER, 'READ_STA_COO', 'No stations were found '// &
     &         'in location file '//FILLOC )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILSTA, MP, BUF, NSTA, IER )
      IF ( .NOT. FL_EXIST ) THEN
           CALL ERR_LOG ( 2744, IUER, 'READ_STA_COO', 'Error in reading '// &
     &         'location file '//FILLOC )
           RETURN
      END IF
!
      DO 420 J2=1,NSTA
         IF ( BUF(J2)(1:1) == '!' ) GOTO 420
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, CHAR(32)//CHAR(9)//'=', IER )
         IF ( LIND < 2 ) GOTO 420
         IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'STATION' ) THEN
              STA_NAM = BUF(J2)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'DBNAME' ) THEN
              I_STA = LTM_DIF ( 0, L_STA, C_DB, BUF(J2)(IND(1,2):IND(2,2)) )
              IF ( I_STA > 0 ) THEN
                   C_STA(I_STA) = STA_NAM
                   FL_STA_DEF(I_STA) = .TRUE.
              END IF
         END IF
 420  CONTINUE 
!
      CALL CLRCH ( STR1 )
      N_MIS = 0
      DO 430 J3=1,L_STA
         IF ( .NOT. FL_STA_DEF(J3) ) THEN
              N_MIS = N_MIS + 1
              STR1(ILEN(STR1)+1:) = ', '//C_DB(J3)
         ENDIF
 430  CONTINUE 
      IF ( N_MIS > 0 ) THEN
           CALL CLRCH (        STR )
           CALL INCH  ( N_MIS, STR )
           CALL ERR_LOG ( 2745, IUER, 'READ_STA_COO', 'There are '// &
     &          TRIM(STR)//' stations in the location file '//TRIM(FILLOC)// &
     &         ' that are missing in the station file '//TRIM(FILSTA)// &
     &         ' : '//STR1(3:) )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_STA_COO  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GET_TLE_LIST ( TLE_INP_DIR, M_TLE, L_TLE, TLE_LIST, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_TLE_LIST
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 25-MAR-2025  GET_TLE_LIST  v1.0 (d) L. Petrov  25-MAR-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M_TLE, L_TLE, IUER
      CHARACTER  TLE_INP_DIR*(*), TLE_LIST(M_TLE)*(*)
      INTEGER*8  DIR_DESC(16)
      CHARACTER  FILNAM*128, STR*128
      INTEGER*4  J1, J2, J3, IS, LEV
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR 
!
      LEV   = 0
      L_TLE = 0
      DO 410 J1=1,M_TLE
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, TLE_INP_DIR, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 2761, IUER, 'GET_TLE_LIST', 'Error in '// &
     &            'attempt to find satellite orbits in directory '// &
     &            TRIM(TLE_INP_DIR)//' : '//FILNAM ) 
              RETURN 
         END IF
         IF ( LEV == 0 ) GOTO 810
         IF ( INDEX ( FILNAM, '~' ) > 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '#' ) > 0 ) GOTO 410
!
         IF ( INDEX ( FILNAM, '.tle' ) > 0 ) THEN
              L_TLE = L_TLE + 1
              IF ( L_TLE > M_TLE ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_TLE, STR )
                   CALL ERR_LOG ( 2762, IUER, 'GET_TLE_LIST', 'Too many '// &
     &                 'satellite orbits in directory '//TRIM(TLE_INP_DIR)// &
     &                 ' -- please increase parameter M_TLE '//STR )
                   RETURN
              END IF
              TLE_LIST(L_TLE) = FILNAM
         END IF
 410  CONTINUE 
      IF ( M_TLE == 0 ) THEN 
           CALL CLRCH ( STR )
           CALL INCH  ( M_TLE, STR )
           CALL ERR_LOG ( 2763, IUER, 'GET_TLE_LIST', 'Too many '// &
     &         'satellite orbits in directory '//TRIM(TLE_INP_DIR)// &
     &         ' -- please increase parameter M_TLE '//STR )
           RETURN
      END IF 
 810  CONTINUE 
      IF ( L_TLE == 0 ) THEN 
           CALL ERR_LOG ( 2764, IUER, 'GET_TLE_LIST', 'No satellite orbits '// &
     &         'were found in directory '//TLE_INP_DIR )
           RETURN
      END IF 
!
      CALL SORT_FAST_CH ( L_TLE, TLE_LIST )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_TLE_LIST  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EXPORT_TLE_FILE ( TLE_INP_FILE, TLE_EXP_DIR, TLE_EXP_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EXPORT_TLE_LIST
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 25-MAR-2025  EXPORT_TLE_FILE  v1.0 (d) L. Petrov 25-MAR-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  TLE_INP_FILE*(*), TLE_EXP_DIR*(*), TLE_EXP_FILE*(*)
      INTEGER*4  MP
      PARAMETER  ( MP = 4096 )
      CHARACTER  BUF(MP)*128, OUT(MP)*128
      INTEGER*4  IUER
      INTEGER*4  J1, NP, NO, ID, IER 
      INTEGER*4, EXTERNAL :: LINDEX 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( TLE_INP_FILE, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2771, IUER, 'EXPORT_TLE_FILE', 'Error in an '// &
     &         'attempt to read input satellite orbit file '//TLE_INP_FILE )
           RETURN 
      END IF
!
      NO = 0 
      DO 410 J1=1,NP
         IF ( BUF(J1)(1:2) == '1 ' .OR. BUF(J1)(1:2) == '2 ' ) THEN
              NO = NO + 1
              OUT(NO) = BUF(J1)
         END IF 
 410  CONTINUE 
!
      IF ( NO .NE. 2 ) THEN
           CALL ERR_LOG ( 2772, IUER, 'EXPORT_TLE_FILE', 'Error in '// &
     &         'parsing input satellite orbit file '//TRIM(TLE_INP_FILE)// &
     &         ' : a valid file should have lines that start from 1 and 2' )
           RETURN 
      END IF
      ID = LINDEX ( TLE_INP_FILE, '/' ) + 1
      TLE_EXP_FILE = TRIM(TLE_EXP_DIR)//'/'//TLE_INP_FILE(ID:)
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( NO, OUT, TLE_EXP_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2773, IUER, 'EXPORT_TLE_FILE', 'Error in '// &
     &         'an attempt to write the satellite orbit file '//TLE_EXP_FILE )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EXPORT_TLE_FILE  !#!#
