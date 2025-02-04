       PROGRAM    PD_IONO_MAIN
       IMPLICIT   NONE 
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL PD_IONO()
       END  PROGRAM  PD_IONO_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE PD_IONO()
! ************************************************************************
! *                                                                      *
! *   Program PD_IONO
! *                                                                      *
! *  ### 10-SEP-2016    PD_IONO    v1.0 (c)  L. Petrov  14-SEP-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INTEGER*4  M_DEL
      PARAMETER  ( M_DEL = 128*1024 )
      TYPE     ( VTD_2P__TYPE  ) :: VTD_2P(M_DEL)
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  VTD_CONF_FIL*128, PD_FIL*128, STR*128, STR1*128, STR2*128, &
     &           C_STA(1)*8, C_SOU(1)*10
      REAL*8     TIM, TIM_MIN, TIM_MAX, TAI_BEG, TAI_END, UP_UEN(3), EMI_RD, &
     &           UP_TRS(3), UP_CRS(3), TEC, IONO_MAP, IONO_DEL, IONO_RATE
      INTEGER*4  MJD_BEG, MJD_END
      INTEGER*4  J1, J2, J3, IDAY, IVRB, N_DEL, L_STA, L_SOU, IONO_STATUS, IUER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: DP_VV_V 
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = 1.0D9
      OBS_TYP%FRQ_REF(2) = 1.0D9
      OBS_TYP%N_BND      = 2
      OBS_TYP%DELAY_TYPE = VTD__PH__DTP
      OBS_TYP%FRQ_ION_EFF(1) = 1.0D9
      OBS_TYP%FRQ_ION_EFF(2) = 1.0D9
      OBS_TYP%STATUS     = VTD__SPC 
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: pd_iono vtd_config_file pd_file [ivrb]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, VTD_CONF_FIL )
           CALL GETARG ( 2, PD_FIL   )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, STR )
                CALL CHIN ( STR, IVRB )
              ELSE
                IVRB = 0
           END IF
      END IF
!
      IUER = -1
      CALL READ_PD_IONO_DAT ( PD_FIL, M_DEL, VTD_2P, N_DEL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5301, IUER, 'PD_IONO', 'Error in reading '// &
     &         'input file '//PD_FIL )
           RETURN 
      END IF
!
      DO 410 J1=1,N_DEL
         TIM = (VTD_2P(J1)%MJD - J2000__MJD)*86400.0D0 + VTD_2P(J1)%TAI
         IF ( J1 == 1 ) THEN
              TIM_MIN = TIM
              TIM_MAX = TIM
            ELSE
              IF ( TIM < TIM_MIN ) TIM_MIN = TIM 
              IF ( TIM > TIM_MAX ) TIM_MAX = TIM 
         END IF
 410  CONTINUE 
!
      IDAY    = (TIM_MIN - VTD__IONO_TIM_MAR)/86400D0 
      MJD_BEG = J2000__MJD + IDAY
      TAI_BEG = (TIM_MIN - VTD__IONO_TIM_MAR) - IDAY*86400.0D0
!
      IDAY    = (TIM_MAX + VTD__IONO_TIM_MAR)/86400D0
      MJD_END = J2000__MJD + IDAY
      TAI_END = (TIM_MAX + VTD__IONO_TIM_MAR) - IDAY*86400.0D0
!
      IUER = -1
      STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
!
      IUER = -1
      STR2 = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A,I9)' ) 'Number of points: ', N_DEL
           WRITE ( 6, '(A)'    ) 'Dates: '//STR1(1:21)//' , '//STR2(1:21)
      END IF
!
      IUER = -1
      CALL VTD_INIT ( VTD,  IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5302, IUER, 'PD_IONO', 'Error in an attempt to '// &
     &        'initialize VTD oibject' )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Read and parse configuration file
!
      IUER = -1
      CALL VTD_CONF ( VTD_CONF_FIL, VTD, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5303, IUER, 'PD_IONO', 'Error in an attempt '// &
     &         'to read configuration file '//VTD_CONF_FIL )
           CALL  EXIT ( 1 ) 
      END IF
!
      VTD%CONF%IVRB = 0
      L_STA = 1
      C_STA = 'GEOCENTR'
      L_SOU = 1
      C_SOU = 'GALCENTR'
!
! --- Load catalogues, ephemerides, EOP series and other data files
!
      IUER = -1
      CALL VTD_LOAD  ( VTD, L_STA, C_STA, L_SOU, C_SOU, MJD_BEG, TAI_BEG, &
     &                 MJD_END, TAI_END, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5304, IUER, 'PD_IONO', 'Error in an '// &
     &         'attempt to load the data into VTD data structure' )
           CALL EXIT ( 1 ) 
      END IF
!
      DO 420 J2=1,N_DEL
!
! ------ Compute the vector of local zenith in CRS and then
! ------ rate of its change
!
         UP_UEN(1) = 1.0D0
         UP_UEN(2) = 0.0D0
         UP_UEN(3) = 0.0D0
         CALL MUL_MV_IV_V ( 3, 3, VTD%STA(1)%UEN_TO_TRS,  &
     &                         3, UP_UEN, 3, UP_TRS, IUER )
         CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                         3, UP_TRS, 3, UP_CRS, IUER )
         IUER = -1
         CALL VTD_MOMENT ( C_SOU(1), VTD_2P(J2)%MJD, VTD_2P(J2)%TAI, VTD, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5305, IUER, 'PD_IONO', 'Error in computing '// &
     &            'transformation from TRS to CRS' )
              CALL EXIT ( 1 ) 
         END IF
!
         VTD%STA(1)%BEG_TRS(1:3)   = VTD_2P(J2)%COO_REC(1:3)
         VTD%STA(1)%COO_TRS(1:3,1) = VTD_2P(J2)%COO_REC(1:3)
         CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                         3, VTD%STA(1)%COO_TRS(1:3,1), &
     &                         3, VTD%STA(1)%COO_CRS, IUER )
         CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                         3, VTD_2P(J2)%COO_EMI, &
     &                         3, VTD%SOU(1)%S_CRS, IUER )
         CALL NORM_VEC ( 3, VTD%SOU(1)%S_CRS, EMI_RD )
!
! ------ Compute geometric elevation angle (no refraction, no aberration)
!
         VTD%STA(1)%ELEV = DASIN ( DP_VV_V ( 3, UP_CRS, VTD%SOU(1)%S_CRS ) )
         CALL INCH ( VTD_2P(J2)%IND_PT, STR )
         VTD%STA(1)%IVS_NAME = STR(1:8)
!
         IUER = -1
         CALL VTD_IONO_DELAY ( VTD, OBS_TYP, 1, 1, TEC, IONO_MAP, &
     &                         IONO_DEL, IONO_RATE, IONO_STATUS, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5305, IUER, 'PD_IONO', 'Error in computing '// &
     &            'ionospheric path delay for point ID '//VTD%STA(1)%IVS_NAME )
              CALL EXIT ( 1 ) 
         END IF
         VTD_2P(J2)%DEL = IONO_DEL
 420  CONTINUE 
!
      IUER = -1
      CALL WRI_PD_IONO_DAT ( PD_FIL, N_DEL, VTD_2P, MJD_BEG, &
     &                       TAI_BEG, MJD_END, TAI_END, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5306, IUER, 'PD_IONO', 'Error in writing '// &
     &         'ionospheric path delay results in the output file '// &
     &          PD_FIL )
           CALL EXIT ( 1 ) 
      END IF
      IF ( IVRB > 0 ) THEN
           WRITE ( 6, '(A)' ) 'Updated ionosphere file '//TRIM(PD_FIL)
      END IF
!
      END  SUBROUTINE  PD_IONO  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_PD_IONO_DAT ( PD_FIL, M_DEL, VTD_2P, N_DEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_PD_IONO_DAT  reads the input file with with          *
! *   data for the PD_IONO program.                                      *
! *                                                                      *
! * ### 10-SEP-2016 READ_PD_IONO_DAT v1.0 (c) L. Petrov 14-SEP-2016  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INTEGER*4  M_DEL, N_DEL, IUER
      TYPE     ( VTD_2P__TYPE  ) :: VTD_2P(M_DEL)
      INTEGER*4  MAX__HDR
      PARAMETER  ( MAX__HDR = 64 ) ! maximum number of header lines
      CHARACTER  PD_FIL*(*)
      CHARACTER*256, ALLOCATABLE :: BUF(:)
      CHARACTER  STR*128
      INTEGER*4  NBUF, J1, IER, IER1, IER2, IER3, IER4, IER5, IER6, &
     &           IER7, IER8, IER9
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ALLOCATE ( BUF(M_DEL+MAX__HDR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (M_DEL+MAX__HDR)*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 5311, IUER, 'READ_PD_IONO_DAT', 'Failure '// &
     &         'to allocate '//TRIM(STR)//' bytes of dynamic memory for '// &
     &         'array BUF' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( PD_FIL, M_DEL+MAX__HDR, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5312, IUER, 'READ_PD_IONO_DAT', 'Error in '// &
     &         'reading ioput file '//PD_FIL )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      N_DEL = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
!
         N_DEL = N_DEL + 1
         IF ( N_DEL > M_DEL ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( N_DEL, STR )
              CALL ERR_LOG ( 5313, IUER, 'READ_PD_IONO_DAT', 'Error in '// &
     &            'reading ioput file '//TRIM(PD_FIL)//' -- to many records, '// &
     &            'more than M_DEL= '//STR )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
         READ ( UNIT=BUF(J1)(1:9),     FMT='(I9)',    IOSTAT=IER1 ) VTD_2P(N_DEL)%IND_PT
         READ ( UNIT=BUF(J1)(11:15),   FMT='(I5)',    IOSTAT=IER1 ) VTD_2P(N_DEL)%MJD
         READ ( UNIT=BUF(J1)(17:23),   FMT='(F7.1)',  IOSTAT=IER2 ) VTD_2P(N_DEL)%TAI
         READ ( UNIT=BUF(J1)(26:38),   FMT='(F13.4)', IOSTAT=IER3 ) VTD_2P(N_DEL)%COO_EMI(1)
         READ ( UNIT=BUF(J1)(40:52),   FMT='(F13.4)', IOSTAT=IER4 ) VTD_2P(N_DEL)%COO_EMI(2)
         READ ( UNIT=BUF(J1)(54:66),   FMT='(F13.4)', IOSTAT=IER5 ) VTD_2P(N_DEL)%COO_EMI(3)
         READ ( UNIT=BUF(J1)(69:81),   FMT='(F13.4)', IOSTAT=IER6 ) VTD_2P(N_DEL)%COO_REC(1)
         READ ( UNIT=BUF(J1)(83:95),   FMT='(F13.4)', IOSTAT=IER7 ) VTD_2P(N_DEL)%COO_REC(2)
         READ ( UNIT=BUF(J1)(97:109),  FMT='(F13.4)', IOSTAT=IER8 ) VTD_2P(N_DEL)%COO_REC(3)
         READ ( UNIT=BUF(J1)(112:122), FMT='(D11.4)', IOSTAT=IER9 ) VTD_2P(N_DEL)%DEL
         IF ( IER1 .NE. 0 .OR. IER2 .NE. 0 .OR. IER3 .NE. 0 .OR. IER4 .NE. 0 .OR. &
     &        IER5 .NE. 0 .OR. IER6 .NE. 0 .OR. IER7 .NE. 0 .OR. IER8 .NE. 0 .OR. &
     &        IER9 .NE. 0                                                         ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5314, IUER, 'READ_PD_IONO_DAT', 'Error in '// &
     &            'parsing line '//TRIM(STR)//' of the input file '//TRIM(PD_FIL)// &
     &            ' -- '//TRIM(BUF(J1)) )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_PD_IONO_DAT  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRI_PD_IONO_DAT ( PD_FIL, N_DEL, VTD_2P, MJD_BEG, &
     &                             TAI_BEG, MJD_END, TAI_END, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_PD_IONO_DAT  reads the input file with with          *
! *   data for the PD_IONO program.                                      *
! *                                                                      *
! * ### 10-SEP-2016 READ_PD_IONO_DAT v1.0 (c) L. Petrov 14-SEP-2016  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INTEGER*4  N_DEL, MJD_BEG, MJD_END, IUER
      REAL*8     TAI_BEG, TAI_END
      TYPE     ( VTD_2P__TYPE  ) :: VTD_2P(N_DEL)
      CHARACTER  PD_FIL*(*)
      CHARACTER  STR*128, STR1*32, STR2*32
      INTEGER*4  NBUF, LUN, J1, IER, IER1, IER2, IER3, IER4, IER5, IER6, &
     &           IER7, IER8, IER9
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=PD_FIL, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5321, IUER, 'WRI_PD_IONO_DAT', 'Error in '// &
     &        'openng output file '//PD_FIL )
           RETURN 
      END IF
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) VTD__IONO_PD_FMT
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5322, IUER, 'WRI_PD_IONO_DAT', 'Error in '// &
     &         'writing the first line inthe output file '//PD_FIL )
           RETURN 
      END IF
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Generated on '//GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '# by '//VTD__LABEL
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      IER  = 0
      STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG+VTD__IONO_TIM_MAR, IER )
      IER  = 0
      STR2 = MJDSEC_TO_DATE ( MJD_END, TAI_END-VTD__IONO_TIM_MAR, IUER )
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Start time: '//STR1(1:21)
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Stop  time: '//STR2(1:21)
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# ----------------------------------------------------------------------------------------------------------------------------------------------------'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Format description:'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Filed      Position  Format   Units          Meaning'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# -----      --------  ------   -----          -------'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Poi ID     1:9       I9       n/a            A unique integer number that identifies the point'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# MJD        11:15     I5       day            Integer modified Julian date at  the midnight'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# TAI        17:23     F7.1     sec            Time of the emission in TAI scale'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# X_emi      26:38     F13.4    meter          X-coordinate of the emitter  in the crust fixed system'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Y_emi      40:52     F13.4    meter          Y-coordinate of the emitter  in the crust fixed system'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Z_emi      54:66     F13.4    meter          Z-coordinate of the emitter  in the crust fixed system'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# X_bou      69:81     F13.4    meter          X-coordinate of the receiver in the crust fixed system'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Y_bou      83:95     F13.4    meter          Y-coordinate of the receiver in the crust fixed system'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Z_bou      97:109    F13.4    meter          Z-coordinate of the receiver in the crust fixed system'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Del_iono  112:122    1PD11.4  sec            Phase ionospheric delay at frequency 1GHz'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# ----------------------------------------------------------------------------------------------------------------------------------------------------'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#                           Position of the emitter                     Position of the bouncing point'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Poi ID   MJD   TAI     X-coordinate  Y-coordinate  Z-coordinate    X-coordinate  Y-coordinate  Z-coordinate Delay at 1GHz'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#                                                                                                              sec'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      DO 410 J1=1,N_DEL
         CALL CLRCH ( STR )
         WRITE ( UNIT=STR(1:9),     FMT='(I9)',    IOSTAT=IER1 ) VTD_2P(J1)%IND_PT
         WRITE ( UNIT=STR(11:15),   FMT='(I5)',    IOSTAT=IER1 ) VTD_2P(J1)%MJD
         WRITE ( UNIT=STR(17:23),   FMT='(F7.1)',  IOSTAT=IER2 ) VTD_2P(J1)%TAI
         WRITE ( UNIT=STR(26:38),   FMT='(F13.4)', IOSTAT=IER3 ) VTD_2P(J1)%COO_EMI(1)
         WRITE ( UNIT=STR(40:52),   FMT='(F13.4)', IOSTAT=IER4 ) VTD_2P(J1)%COO_EMI(2)
         WRITE ( UNIT=STR(54:66),   FMT='(F13.4)', IOSTAT=IER5 ) VTD_2P(J1)%COO_EMI(3)
         WRITE ( UNIT=STR(69:81),   FMT='(F13.4)', IOSTAT=IER6 ) VTD_2P(J1)%COO_REC(1)
         WRITE ( UNIT=STR(83:95),   FMT='(F13.4)', IOSTAT=IER7 ) VTD_2P(J1)%COO_REC(2)
         WRITE ( UNIT=STR(97:109),  FMT='(F13.4)', IOSTAT=IER8 ) VTD_2P(J1)%COO_REC(3)
         WRITE ( UNIT=STR(112:122), FMT='(1PD11.4)', IOSTAT=IER9 ) VTD_2P(J1)%DEL
         IF ( IER1 .NE. 0 .OR. IER2 .NE. 0 .OR. IER3 .NE. 0 .OR. IER4 .NE. 0 .OR. &
     &        IER5 .NE. 0 .OR. IER6 .NE. 0 .OR. IER7 .NE. 0 .OR. IER8 .NE. 0 .OR. &
     &        IER9 .NE. 0                                                         ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5323, IUER, 'WRI_PD_IONO_DAT', 'Error in '// &
     &            'writing line '//TRIM(STR)//' of the output file '//PD_FIL )
              RETURN 
         END IF
!
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) TRIM(STR)
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5324, IUER, 'WRI_PD_IONO_DAT', 'Error in '// &
     &            'writing line '//TRIM(STR)//' of the output file '//PD_FIL )
              RETURN 
         END IF
 410  CONTINUE 
!
      CLOSE ( UNIT=LUN )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRI_PD_IONO_DAT  !#!  
