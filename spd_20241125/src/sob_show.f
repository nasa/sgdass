#include <mk5_preprocessor_directives.inc>
      PROGRAM    SOB_SHOW
! ************************************************************************
! *                                                                      *
! *   Program SOB_SHOW  computes atmospheric opacity and atmospheric     *
! *   radiative temperature for the list of specified stations, for      *
! *   the specified elevation angles, azimuths, moments of time, and     *
! *   frequncies. It uses the results of SPD_3D with gridded opacities   *
! *   and brightness temperatures stored in the specified directory.     *
! *   It expands the gridded opacities and brithness temperatures into   *
! *   3D B-spline basis for each frequency. After that it interpolates   *
! *   these quantities.                                                  *
! *                                                                      *
! *  ###  13-SEP-2014    SOB_SHOW   v1.0 (c) L. Petrov  15-SEP-2014  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  M_OBS, M_STA, M_FIL
      REAL*8     TIM_STEP_MIN, EPS_MAR
      PARAMETER  ( M_OBS = 128*1024 )
      PARAMETER  ( M_STA =     1024 )
      PARAMETER  ( M_FIL =  32*1024 )
      PARAMETER  ( TIM_STEP_MIN = 1800.0D0 )
      PARAMETER  ( EPS_MAR = 1.0E-5 )
      TYPE     ( SPD__ASCII__TYPE ), POINTER :: SAT(:)
      TYPE     ( SPD_DEL__TYPE    ), POINTER :: SPD_RES(:)
      INTEGER*8  DIR_DESC(16)
      CHARACTER  SOB_DIR*128, C_STA(M_STA)*8, C_FIL(M_FIL)*128, FIL*128, &
     &           FINAM*128, STR*128, DATE_STR*21
      REAL*4     ARGS(3)
      REAL*8     TAI_BEG, TAI_END, TAI_EPC, TAI_OBS(M_OBS), AZ(M_OBS), &
     &           EL(M_OBS), FRQ_OBS(M_OBS), TIM_FIL(M_FIL), TIM_STEP, &
     &           TIM_STEP_THIS, MAP_VAL, TIM_VAL, OPA_VAL, TAT_VAL, FRQ_DIF
      INTEGER*4  L_STA, MJD_BEG, MJD_END, MJD_EPC, MJD_OBS(M_OBS), &
     &           L_OBS, IND_STA(M_OBS), IS, L_FIL, LEV, IP, IL, &
     &           MJD_FIL(M_FIL), J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           J10, J11, J12, J13, J14, J15, J16, J17, J18, INDS(3), DIMS(3), &
     &           IND_BEG, IND_END, N_EPC, IND_EPC, IND_FRQ, ISTA, IUER
!
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4
      REAL*8,    EXTERNAL :: DEL_ISA 
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, CLOSEDIR, GET_FILE_FROM_DIR, IXMN4 
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: sob_show control_file'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FIL )
      END IF
!
! --- Parse the input file
!
      IUER = -1
      CALL SOH_PARSE ( FIL, M_STA, M_OBS, SOB_DIR, L_STA, C_STA, MJD_BEG, &
     &                 TAI_BEG, MJD_END, TAI_END, L_OBS, MJD_OBS, TAI_OBS, &
     &                 IND_STA, AZ, EL, FRQ_OBS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4001, IUER, 'SOB_SHOW', 'Error in parsing '// &
     &         'input file '//FIL )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Check whether the input directory exists
!
      DIR_DESC(1) = FUNC_OPENDIR ( SOB_DIR(1:I_LEN(SOB_DIR))//CHAR(0) )
      IF ( DIR_DESC(1) .EQ. 0 ) THEN
!
! -------- Does not exist? No show.
!
           IUER = -1
           CALL ERR_LOG ( 4002, IUER, 'SOB_SHOW', 'Cannot find SOB'// &
     &         'directory '//SOB_DIR(1:I_LEN(SOB_DIR))//' specified '// &
     &         'in the control file '//FIL )
           CALL EXIT ( 1 )
        ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC(1)) )
      END IF
!
! --- Travel the directory tree and collect relevant files.
!
      L_FIL = 0
      LEV = 0
      DO 410 J1=1,M_FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, SOB_DIR, FINAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4003, IUER, 'SOB_SHOW', 'Error in '// &
     &                 'reading input directory '//SOB_DIR )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FINAM, '#' ) .GT. 0 ) GOTO 410
         IL = ILEN(FINAM)
         IF ( IL < 18 ) GOTO 410
         IF ( FINAM(IL-3:IL) == '.spd' .AND. FINAM(IL-8:IL-8) == '_' .AND. &
     &        FINAM(IL-17:IL-17) == '_' ) THEN
              L_FIL = L_FIL + 1
              IF ( L_FIL > M_FIL) THEN
                   IUER = -2
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_FIL, STR )
                   CALL ERR_LOG ( 4004, IUER, 'SOB_SHOW', 'Too many files '// &
     &                 'in directory '//SOB_DIR(1:I_LEN(SOB_DIR))//' -- '// &
     &                 'more than '//STR )
                   CALL EXIT ( 1 )
              END IF
              C_FIL(L_FIL) = FINAM
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL < 1 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4005, IUER, 'SOB_SHOW', 'No valid data files with '// &
     &         'atmosphere opatcity/brightness were found in the input '// &
     &         'directory '//SOB_DIR )
           CALL EXIT ( 1 )
      END IF
      IF ( L_FIL == 1 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4006, IUER, 'SOB_SHOW', 'Only one valid data '// &
     &         'files with atmosphere opatcity/brightness were found '// &
     &         'in the input directory '//SOB_DIR(1:I_LEN(SOB_DIR))// &
     &         ' while at least two data files are needed' )
           CALL EXIT ( 1 )
      END IF
!
! --- Sort data files in alphabetic order which is equivalent to time order
!
      CALL SORT_FAST_CH ( L_FIL, C_FIL )
!
! --- Extract the dates of data files and get the time step
!
      TIM_STEP = 1.D9
      DO 420 J2=1,L_FIL
         IL = ILEN(C_FIL(J2))
         DATE_STR = C_FIL(J2)(IL-16:IL-13)//'.'//C_FIL(J2)(IL-12:IL-11)//'.'// &
     &              C_FIL(J2)(IL-10:IL-6)//':'//C_FIL(J2)(IL-5:IL-4)//':00.0'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_STR, MJD_FIL(J2), TIM_FIL(J2), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4007, IUER, 'SOB_SHOW', 'Wrong format of the '// &
     &            'data file name '//C_FIL(J2)(1:I_LEN(C_FIL(J2)))// &
     &            ' -- it should have yyyymmdd_HHMM part inside that is '// &
     &            'a valid calendar date' )
              CALL EXIT ( 1 )
         END IF
         IF ( J2 > 1 ) THEN
              TIM_STEP_THIS = (MJD_FIL(J2)*86400.0D0   + TIM_FIL(J2)) - &
     &                        (MJD_FIL(J2-1)*86400.0D0 + TIM_FIL(J2-1)) 
              IF ( TIM_STEP_THIS < TIM_STEP_MIN ) THEN
                   IUER = -2
                   CALL ERR_LOG ( 4008, IUER, 'SOB_SHOW', 'Too short interval '// &
     &                 'between data files '//C_FIL(J2-1)(1:I_LEN(C_FIL(J2-1)))// &
     &                 ' and '//C_FIL(J2)(1:I_LEN(C_FIL(J2)))//' -- please '// &
     &                 'check whether the files in the input directory '// &
     &                 SOB_DIR(1:I_LEN(SOB_DIR))//' satisfy to the naming '// &
     &                 'convention' )
                   CALL EXIT ( 1 )
              END IF
              TIM_STEP = MIN ( TIM_STEP, TIM_STEP_THIS )
         END IF
 420  CONTINUE 
!
! --- Find the index of the first file in L_FIL/C_FIL list and
! --- the last file that fits the interval. NB: to alleviate the
! --- errors of interpolation at the end of the interval, the
! --- interpolation interval is increased in both ends by 1-2 epochs
!
      IND_BEG = 0
      IND_END = 0
      DO 430 J3=1,L_FIL
         IF ( IND_BEG == 0 ) THEN
              IF ( ( MJD_FIL(J3)*86400.0D0 + TIM_FIL(J3) ) - &
     &             ( MJD_BEG*86400.0D0 + TAI_BEG ) > -2.0D0*TIM_STEP ) THEN
                   IND_BEG = J3
              END IF
         END IF
         IF ( IND_BEG == 0 ) THEN
              IF ( ( MJD_FIL(J3)*86400.0D0 + TIM_FIL(J3) ) - &
     &             ( MJD_BEG*86400.0D0 + TAI_BEG ) > -1.0D0*TIM_STEP ) THEN
                   IND_BEG = J3
              END IF
         END IF
         IF ( ( MJD_FIL(J3)*86400.0D0 + TIM_FIL(J3) ) - &
     &        ( MJD_END*86400.0D0 + TAI_END ) > 1.0D0*TIM_STEP ) THEN
              IF ( IND_END == 0 ) IND_END = J3
         END IF
         IF ( ( MJD_FIL(J3)*86400.0D0 + TIM_FIL(J3) ) - &
     &        ( MJD_END*86400.0D0 + TAI_END ) > 2.0D0*TIM_STEP ) THEN
              IF ( IND_END == J3-1 ) IND_END = J3
         END IF
 430  CONTINUE 
!
! --- Check, whether we found data files for in requested interval
!
      IF ( IND_BEG == 0 ) THEN
           DATE_STR = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, IUER )
           IUER = -2
           CALL ERR_LOG ( 4009, IUER, 'SOB_SHOW', 'Input directory '// &
     &          SOB_DIR(1:I_LEN(SOB_DIR))//' does not have data files '// &
     &          'for epochs on and before the beginning the interval '// &
     &          DATE_STR )
           CALL EXIT ( 1 )
      END IF
      IF ( IND_END == 0 ) THEN
           DATE_STR = MJDSEC_TO_DATE ( MJD_END, TAI_END, IUER )
           IUER = -2
           CALL ERR_LOG ( 4010, IUER, 'SOB_SHOW', 'Input directory '// &
     &          SOB_DIR(1:I_LEN(SOB_DIR))//' does not have data files '// &
     &          'for epochs on and after the end of the interval '// &
     &          DATE_STR )
           CALL EXIT ( 1 )
      END IF
!
! --- Allocate memory for the ascii representation of data files
!
      N_EPC = IND_END - IND_BEG + 1
      ALLOCATE ( SAT(N_EPC), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( N_EPC*SIZEOF(SAT(1)), STR )
           IUER = -2
           CALL ERR_LOG ( 4011, IUER, 'SOB_SHOW', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes if dynamic memory for array '// &
     &         'SAT' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read ascii data files and copy its contents into array SAT
!
      DO 440 J4=IND_BEG,IND_END
         IND_EPC = J4 - IND_BEG + 1
         IUER = -1
         CALL SPD_3D_READ_ASCII ( C_FIL(J4), SAT(IND_EPC), IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 4012, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to read input file with atmosphere opacity '// &
     &            'and radiative temperature '//C_FIL(J4) )
              CALL EXIT ( 1 )
         END IF
 440  CONTINUE 
!
! --- Allocate memory for SPD_RES data structure
!
      ALLOCATE ( SPD_RES(L_STA), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( L_STA*SIZEOF(SPD_RES(1)), STR )
           IUER = -2
           CALL ERR_LOG ( 4013, IUER, 'SOB_SHOW', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes if dynamic memory for array '// &
     &         'SPD_RES' )
           CALL EXIT ( 1 )
      END IF
!
! --- Now cycle over stations. Read SAT data structures with results of SPD_3D 
! --- in ascii representation, decode them in binary form, and put into
! --- SPD_RES data structures
!
      DO 450 J5=1,L_STA
         IUER = -1
         CALL SPD_DEL_INIT ( SPD_RES(J5), IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4014, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to initialize SPD_RES object' )
              CALL EXIT ( 1 )
         END IF
!
! ------ Find ISTA -- index of the J5-th station from the L_STA/C_STA list
! ------ in SAT station list
!
         ISTA = 0
         DO 460 J6=1,SAT(1)%NS
            IF ( SAT(1)%SLINE(J6)%STA_NAME == C_STA(J5) ) ISTA =J6
 460     CONTINUE 
         IF ( ISTA == 0 ) THEN
              CALL ERR_LOG ( 4015, IUER, 'SOB_SHOW', 'Opacity and '// &
     &            'radiative temperature were not computed for station '// &
     &            C_STA(J5) )
              CALL EXIT ( 1 )
         END IF
!
         SPD_RES(J5)%ELV%N_EL = SAT(1)%NE
         SPD_RES(J5)%AZM%N_AZ = SAT(1)%NA
         SPD_RES(J5)%N_TIM    = N_EPC
         SPD_RES(J5)%N_FRQ    = SAT(1)%NF
!
         DIMS(1) = SPD_RES(J5)%ELV%N_EL
         DIMS(2) = SPD_RES(J5)%AZM%N_AZ
         DIMS(3) = SPD_RES(J5)%N_TIM
!
! ------ Alocate dynamic memory in SPD_RES data structures fir various fields
!
         ALLOCATE ( SPD_RES(J5)%TIM_ARR(SPD_RES(J5)%N_TIM), STAT=IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4016, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to allocate memory for array TIM_ARR' )
              CALL EXIT ( 1 )
         END IF
!
         ALLOCATE ( SPD_RES(J5)%ELV%ELEV(SPD_RES(J5)%ELV%N_EL), STAT=IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4017, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to allocate memory for array SPD_RES(J5)%ELV%ELEV' )
              CALL EXIT ( 1 )
         END IF
!
         ALLOCATE ( SPD_RES(J5)%ELV%MAP(SPD_RES(J5)%ELV%N_EL), STAT=IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4018, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to allocate memory for array SPD_RES(J5)%ELV%MAP' )
              CALL EXIT ( 1 )
         END IF
!
         ALLOCATE ( SPD_RES(J5)%AZM%AZIM(SPD_RES(J5)%AZM%N_AZ), STAT=IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4019, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to allocate memory for array SPD_RES(J5)%AZM%AZ' )
              CALL EXIT ( 1 )
         END IF
!
         ALLOCATE ( SPD_RES(J5)%FRQ_ARR(SPD_RES(J5)%N_FRQ), STAT=IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 4020, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to allocate memory for array SPD_RES(J5)%FRQ_ARR' )
              CALL EXIT ( 1 )
         END IF
!
! ------ Generate the date array TIM_ARR -- time from the the first epoch
! ------ of data that are used for epxansion into 3D B-spline basis
!
         DO 470 J7=1,SPD_RES(J5)%N_TIM
            IUER = -1
            CALL DATE_TO_TIME ( SAT(J7)%TLINE%DATE_STR, MJD_EPC, TAI_EPC, IUER )
            IF ( IUER .NE. 0 ) THEN
                 IUER = -2
                 CALL ERR_LOG ( 4021, IUER, 'SOB_SHOW', 'Trap of internal '// &
     &               'control: wrong date format: '//SAT(J7)%TLINE%DATE_STR )
                 CALL EXIT ( 1 )
            END IF
            IF ( J7 == 1 ) THEN
                 SPD_RES(J5)%TIM%MJD_BEG  = MJD_EPC
                 SPD_RES(J5)%TIM%TAI_BEG  = TAI_EPC
                 SPD_RES(J5)%TIM%TIM_STEP = TIM_STEP
               ELSE 
                 SPD_RES(J5)%TIM%MJD_END  = MJD_EPC
                 SPD_RES(J5)%TIM%TAI_END  = TAI_EPC
            END IF
            SPD_RES(J5)%TIM_ARR(J7) = (MJD_EPC - SPD_RES(J5)%TIM%MJD_BEG)*86400.0D0 + &
     &                                (TAI_EPC - SPD_RES(J5)%TIM%TAI_BEG)
 470     CONTINUE 
!
! ------ Generate the array of elevation angles and mapping function
!
         DO 480 J8=1,SPD_RES(J5)%ELV%N_EL
            READ ( UNIT=SAT(1)%ELINE(J8)%ANG, FMT='(F10.6)', IOSTAT=IUER ) SPD_RES(J5)%ELV%ELEV(J8)
            IF ( IUER .NE. 0 ) THEN
                 IUER = -2
                 CALL ERR_LOG ( 4022, IUER, 'SOB_SHOW', 'Trap of internal '// &
     &               'control: wrong elevation angle format: '//SAT(1)%ELINE(J8)%ANG )
                 CALL EXIT ( 1 )
            END IF
            SPD_RES(J5)%ELV%ELEV(J8) = SPD_RES(J5)%ELV%ELEV(J8)*DEG__TO__RAD
!
            SPD_RES(J5)%ELV%MAP(J8) = DEL_ISA ( DBLE(SPD_RES(J5)%ELV%ELEV(J8)) )/ &
     &                                DEL_ISA ( P2I )
 480     CONTINUE 
!
! ------ Generate the rray of aximuth angles
!
         DO 490 J9=1,SPD_RES(J5)%AZM%N_AZ
            READ ( UNIT=SAT(1)%ALINE(J9)%ANG, FMT='(F10.6)', IOSTAT=IUER ) SPD_RES(J5)%AZM%AZIM(J9)
            IF ( IUER .NE. 0 ) THEN
                 IUER = -2
                 CALL ERR_LOG ( 4023, IUER, 'SOB_SHOW', 'Trap of internal '// &
     &               'control: wrong azimuth angle format: '//SAT(1)%ELINE(J9)%ANG )
                 CALL EXIT ( 1 )
            END IF
            SPD_RES(J5)%AZM%AZIM(J9) = SPD_RES(J5)%AZM%AZIM(J9)*DEG__TO__RAD
 490     CONTINUE 
!
! ------ Generate the frequency array
!
         DO 4100 J10=1,SPD_RES(J5)%N_FRQ
            READ ( UNIT=SAT(1)%FLINE(J10)%FRQ, FMT='(F15.6)', IOSTAT=IUER ) SPD_RES(J5)%FRQ_ARR(J10)
            IF ( IUER .NE. 0 ) THEN
                 IUER = -2
                 CALL ERR_LOG ( 4024, IUER, 'SOB_SHOW', 'Trap of internal '// &
     &               'control: wrong format of the frequency: '//SAT(1)%FLINE(J10)%FRQ )
                 CALL EXIT ( 1 )
            END IF
 4100    CONTINUE 
!
! ------ Allocate memory for 4D arrays that will keep expansion coefficients
! ------ in the 3D B-spline basis. The 4th dimension is over frequency.
! ------ NB: remember: dimensions start from 1-SPD__MDEG.
!
         ALLOCATE ( SPD_RES(J5)%OPA(1-SPD__MDEG:SPD_RES(J5)%ELV%N_EL, &
     &                              1-SPD__MDEG:SPD_RES(J5)%AZM%N_AZ, &
     &                              1-SPD__MDEG:SPD_RES(J5)%N_TIM,    &
     &                              1:SPD_RES(J5)%N_FRQ), STAT=IUER   )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL IINCH ( 4*(SPD_RES(J5)%ELV%N_EL + SPD__MDEG)* &
     &                       (SPD_RES(J5)%AZM%N_AZ + SPD__MDEG)* &
     &                       (SPD_RES(J5)%N_TIM    + SPD__MDEG)* &
     &                        SPD_RES(J5)%N_FRQ, STR ) 
              IUER = -2
              CALL ERR_LOG ( 4025, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'dynamic memory for array SPD_RES(J5)%OPA' )
              CALL EXIT ( 1 )
         END IF
!
         ALLOCATE ( SPD_RES(J5)%TAT(1-SPD__MDEG:SPD_RES(J5)%ELV%N_EL, &
     &                              1-SPD__MDEG:SPD_RES(J5)%AZM%N_AZ, &
     &                              1-SPD__MDEG:SPD_RES(J5)%N_TIM,    &
     &                              1:SPD_RES(J5)%N_FRQ), STAT=IUER   )
         IF ( IUER .NE. 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL IINCH ( 4*(SPD_RES(J5)%ELV%N_EL  + SPD__MDEG)* &
     &                       (SPD_RES(J5)%AZM%N_AZ  + SPD__MDEG)* &
     &                       (SPD_RES(J5)%N_TIM + SPD__MDEG)* &
     &                        SPD_RES(J5)%N_FRQ, STR ) 
              IUER = -2
              CALL ERR_LOG ( 4026, IUER, 'SOB_SHOW', 'Failure in an '// &
     &            'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'dynamic memory for array SPD_RES(J5)%TAT' )
              CALL EXIT ( 1 )
         END IF
!
! ------ Cycle over frequency
!
         DO 4110 J11=1,SPD_RES(J5)%N_FRQ
!
! --------- Populate arrays 
! --------- OPA -- atmosphere optical depth and 
! --------- TAT -- atmosphere brightness temperature at the station.
!
            DO 4120 J12=1,SPD_RES(J5)%N_TIM
               DO 4130 J13=1,SPD_RES(J5)%AZM%N_AZ
                  DO 4140 J14=1,SPD_RES(J5)%ELV%N_EL
                     READ ( UNIT=SAT(J12)%OLINE(J11,J14,J13,ISTA)%OPA, FMT='(F6.4)', IOSTAT=IUER ) &
     &                      SPD_RES(J5)%OPA(J14,J13,J12,J11)
                     IF ( IUER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4027, IUER, 'SOB_SHOW', 'Trap of internal '// &
     &                        'control: error in reading opacity' )
                          CALL EXIT ( 1 )
                     END IF
                     READ ( UNIT=SAT(J12)%OLINE(J11,J14,J13,ISTA)%TAT, FMT='(F6.4)', IOSTAT=IUER ) &
     &                      SPD_RES(J5)%TAT(J14,J13,J12,J11)
                     IF ( IUER .NE. 0 ) THEN
                          IUER = -2
                          CALL ERR_LOG ( 4028, IUER, 'SOB_SHOW', 'Trap of internal '// &
     &                        'control: error in reading atmosphere radiative '// &
     &                        'temperature' )
                          CALL EXIT ( 1 )
                     END IF
 4140              CONTINUE 
 4130           CONTINUE 
 4120        CONTINUE 
!
! ---------- Expand atmosphere optical depth and atmosphere brightness temperature
! ---------- into 3D B-spline basus. Dimensions of the expansion:
! ---------- mapping function, azimuth angle, and time
!
             IUER = -1
             CALL BSPL4_3D_CMP ( SPD__MDEG, 10, DIMS, SPD_RES(J5)%ELV%MAP, SPD_RES(J5)%AZM%AZIM, &
     &                           SPD_RES(J5)%TIM_ARR, SPD_RES(J5)%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J11), &
     &                           IUER )
             IF ( IUER .NE. 0 ) THEN
                  IUER = -2
                  CALL ERR_LOG ( 4029, IUER, 'SOB_SHOW', 'Failure in an '// &
     &                'attempt to expand opacity in 3D B-spline basis' )
                  CALL EXIT ( 1 )
             END IF
!
             IUER = -1
             CALL BSPL4_3D_CMP ( SPD__MDEG, 10, DIMS, SPD_RES(J5)%ELV%MAP, SPD_RES(J5)%AZM%AZIM, &
     &                           SPD_RES(J5)%TIM_ARR, SPD_RES(J5)%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J11), &
     &                           IUER )
             IF ( IUER .NE. 0 ) THEN
                  IUER = -2
                  CALL ERR_LOG ( 4030, IUER, 'SOB_SHOW', 'Failure in an '// &
     &                'attempt to expand atmospere radiative temperature '// &
     &                'in 3D B-spline basis' )
                  CALL EXIT ( 1 )
             END IF
 4110    CONTINUE 
 450  CONTINUE 
!
! === Good! WE read input data files, parsed their contents and expanded &
! === optical depth and atmosphere brightness into 3D B-spline basus.   
! === Now we can compute opacity and atmosphere atmosphere brightness 
! === for specified stations, elevation anglers, azimuths, and frequencies
! === by B-spline interpolation
!
      DO 4150 J15=1,L_OBS
         FRQ_DIF = 1.D30
         ISTA = IND_STA(J15)
!
! ------ Search for the frequency in FRQ_ARR that is the closest to the 
! ------ user specified frequency FRQ_OBS
!
         DO 4160 J16=1,SPD_RES(ISTA)%N_FRQ
            IF ( DABS ( SPD_RES(ISTA)%FRQ_ARR(J16) - FRQ_OBS(J15) ) < FRQ_DIF ) THEN
                 IND_FRQ = J16
                 FRQ_DIF = DABS ( SPD_RES(ISTA)%FRQ_ARR(J16) - FRQ_OBS(J15) ) 
            END IF
 4160    CONTINUE 
!
! ------ Gather dimensions of the expansion
!
         DIMS(1) = SPD_RES(ISTA)%ELV%N_EL
         DIMS(2) = SPD_RES(ISTA)%AZM%N_AZ
         DIMS(3) = SPD_RES(ISTA)%N_TIM
!
! ------ Compute the mapping function and time since the begining of the expansion
! ------ interval
!
         MAP_VAL = DEL_ISA ( EL(J15) )/DEL_ISA ( P2I )
         TIM_VAL = (MJD_OBS(J15) - SPD_RES(ISTA)%TIM%MJD_BEG)*86400.0D0 + &
     &             (TAI_OBS(J15) - SPD_RES(ISTA)%TIM%TAI_BEG)
!
! ------ Gather the vector of arguments of the 3D B-spline
!
         ARGS(1) = MAP_VAL
         ARGS(2) = AZ(J15)
         ARGS(3) = TIM_VAL
!
! ------ Find the pivotal indices
!
         INDS(1) = IXMN4 ( DIMS(1), SPD_RES(ISTA)%ELV%MAP,  ARGS(1) )
         INDS(2) = IXMN4 ( DIMS(2), SPD_RES(ISTA)%AZM%AZIM, ARGS(2) )
         INDS(3) = IXMN4 ( DIMS(3), SPD_RES(ISTA)%TIM_ARR,  ARGS(3) )
!
         IF ( INDS(1) < 1 .AND. &
     &        ABS(ARGS(1) - SPD_RES(ISTA)%ELV%MAP(1)) <        &
     &        EPS_MAR*ABS(ARGS(1) - SPD_RES(ISTA)%ELV%MAP(1))  ) THEN
              INDS(1) = 1
         END IF         
         IF ( INDS(2) < 1 .AND. &
     &        ABS( ARGS(1) - SPD_RES(ISTA)%AZM%AZIM(1) ) <     &
     &        EPS_MAR*ABS(ARGS(2) - SPD_RES(ISTA)%AZM%AZIM(1)) ) THEN
              INDS(2) = 1
         END IF         
         IF ( INDS(3) < 1 .AND. &
     &        ABS( ARGS(1) - SPD_RES(ISTA)%TIM_ARR(1)  ) <     &
     &        EPS_MAR*ABS(ARGS(3) - SPD_RES(ISTA)%TIM_ARR(1))  ) THEN
              INDS(3) = 1
         END IF         
!
! ------ Finally, compute the atmosphere optical depth and atmosphere brightness
! ------ for the specified station
!
         OPA_VAL = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                            SPD_RES(ISTA)%ELV%MAP, &
     &                            SPD_RES(ISTA)%AZM%AZIM, &
     &                            SPD_RES(ISTA)%TIM_ARR, &
     &                            SPD_RES(ISTA)%OPA(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,IND_FRQ) )
         TAT_VAL = VAL_3D_BSPL4 ( ARGS, SPD__MDEG, DIMS, INDS, &
     &                            SPD_RES(ISTA)%ELV%MAP, &
     &                            SPD_RES(ISTA)%AZM%AZIM, &
     &                            SPD_RES(ISTA)%TIM_ARR, &
     &                            SPD_RES(ISTA)%TAT(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,IND_FRQ) )
!
! ------ Print results
!
         IUER = -1
         DATE_STR = MJDSEC_TO_DATE ( MJD_OBS(J15), TAI_OBS(J15), IUER )
         WRITE ( 6, 110 ) C_STA(ISTA), DATE_STR, SPD_RES(ISTA)%FRQ_ARR(IND_FRQ), &
     &                    EL(J15)/DEG__TO__RAD, AZ(J15)/DEG__TO__RAD, &
     &                    OPA_VAL, TAT_VAL
 110     FORMAT ( 'Sta: ', A,  ' Date: ', A, ' Frq: ', 1PD15.7, &
     &            ' El: ',0PF5.2, ' Az: ', 0PF6.2, ' Opa: ', 0PF6.4, &
     &            ' Tat: ', 0PF6.2 )
 4150 CONTINUE 
!
! --- Just fot the sake of completness, deallocate dynamic memory
!
      DO 4170 J17=1,L_STA
         IUER = -1
         CALL SPD_DEL_QUIT ( SPD_RES(J17), IUER )
 4170 CONTINUE 
!
      DO 4180 J18=1,N_EPC
         IUER = -1
         CALL SAT_QUIT ( SAT(J18) )
 4180 CONTINUE 
!
      CALL EXIT ( 0 )
      END  PROGRAM  SOB_SHOW  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SOH_PARSE ( FIL, M_STA, M_OBS, SOB_DIR, L_STA, C_STA, &
     &                       MJD_BEG, TAI_BEG, MJD_END, TAI_END, L_OBS, &
     &                       MJD_OBS, TAI_OBS, IND_STA, AZ, EL, FRQ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SOH_PARSE  parses input file FIL in SFS format.            *
! *   It extracts the following parameters:                              *
! *                                                                      *
! *   1) Name of the directory with binary slant path delays SOB_DIR     *
! *   2) Number of stations L_STA                                        *
! *   3) Number of observations L_OBS                                    *
! *   4) List of station names C_STA                                     *
! *   5) Begin TAI date of the interval in a pair MJD_BEG, TAI_BEG       *
! *   6) End date of the interval in a pair MJD_END, TAI_END             *
! *   7) Array of TAI epochs of observations MJD_OBS, TAI_OBS            *
! *   8) Array of station indices IND_STA                                *
! *   9) Array of azimuth angles AZ                                      *
! *  10) Array of elevation angles                                       *
! *  11) Array of azimuth angle rates AZ_RATE                            *
! *  12) Array of elevation angle rates EL_RATE                          *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      FIL ( CHARACTER ) -- Input file in SHS (SHow Slantad path delay *
! *                           format.                                    *
! *    M_STA ( INTEGER*4 ) -- Maximum number of stations.                *
! *    M_OBS ( INTEGER*4 ) -- Maximum number of observations.            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  SOB_DIR ( CHARACTER ) -- Name of the directory with atmosphere      *
! *                           opacity and radiative sky temperature.     *
! *    L_STA ( INTEGER*4 ) -- The number of stations.                    *
! *    C_STA ( CHARACTER ) -- Array of 8-characters long station names.  *
! *                           It is assumed the path delay has been      *
! *                           computed for all these stations.           *
! *                           Dimension: L_STA.                          *
! *  MJD_BEG ( INTEGER*4 ) -- Modified Julian date at the midnight of    *
! *                           beginning the interval. Units: days.       *
! *  TAI_BEG ( INTEGER*4 ) -- TAI time past the midnight of the          *
! *                           beginning the interval. Units: seconds.    *
! *  MJD_END ( INTEGER*4 ) -- Modified Julian date at the midnight of    *
! *                           beginning the interval. Units: days.       *
! *  TAI_END ( INTEGER*4 ) -- TAI time past the midnight of the end of   *
! *                           the interval. Units: seconds.              *
! *    L_OBS ( INTEGER*4 ) -- The number of observations.                *
! *  MJD_OBS ( INTEGER*4 ) -- Modified Julian date at the midnight of    *
! *                           the observatin interval. Units: days.      *
! *                           Dimension: L_OBS.                          *
! *  TAI_OBS ( INTEGER*4 ) -- TAI time past the midnight of the          *
! *                           observation. Units: seconds.               *
! *                           Dimension: L_OBS.                          *
! *  IND_STA ( INTEGER*4 ) -- Array of station index in L_STA/C_STA list *
! *                           for a given observation. Dimension: L_STA. *
! *       AZ ( REAL*8    ) -- Array of azimuth for a given observation.  *
! *                           Unit: rad. Dimension: L_OBS.               *
! *       EL ( REAL*8    ) -- Array of elevation angle for a given       *
! *                           observation. Unit: rad. Dimension: L_OBS.  *
! *      FRQ ( REAL*8    ) -- Array of centeral frequencies for which    *
! *                           the opacity and sky radiative temperature  *
! *                           are computed. Unit: Hz. Dimension: L_OBS.  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *  ### 13-SEP-2014   SOH_PARSE   v1.0 (c)  L. Petrov  13-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  M_OBS, M_STA, L_STA, MJD_BEG, MJD_END, MJD_OBS(M_OBS), &
     &           L_OBS, IND_STA(M_OBS), IUER
      CHARACTER  FIL*(*), SOB_DIR*(*), C_STA(M_STA)*(*)
      REAL*8     TAI_BEG, TAI_END, TAI_OBS(M_OBS), AZ(M_OBS), EL(M_OBS), &
     &           FRQ(M_OBS)
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*128, STR1*128
      INTEGER*4  M_OVR, MIND
      PARAMETER  ( M_OVR = 256 )
      PARAMETER  ( MIND = 128 )
      INTEGER*4  LIND, IND(2,MIND), J1, J2, J3, J4, J5, IP, NP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, ADD_CLIST
!
! --- Get memory for the buffer with file contents
!
      ALLOCATE ( BUF(M_OBS+M_STA+M_OVR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LEN(BUF(1))*(M_OBS+M_STA+M_OVR), STR )
           CALL ERR_LOG ( 4211, IUER, 'SOH_PARSE', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array BUF' )
           RETURN 
      END IF
!
! --- Read the file FIL into the buffer BUF. 
! --- NP -- is the number of lines in the file
!
      IER = IUER
      CALL RD_TEXT ( FIL, M_OBS+M_STA+M_OVR, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( LEN(BUF(1))*(M_OBS+M_STA+M_OVR), STR )
           CALL ERR_LOG ( 4212, IUER, 'SOH_PARSE', 'Error in an attempt '// &
     &         'to read input configuration file '//FIL )
           RETURN 
      END IF
!
! --- Check the first line of the file. It should have format label
!
      IF ( BUF(1)(1:LEN(SOB_SHOW__LABEL)) .NE. SOB_SHOW__LABEL ) THEN
           STR = BUF(1)
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 4213, IUER, 'SOH_PARSE', 'Unsupported format '// &
     &         'of input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &         'its first line is '//STR(1:I_LEN(STR))//' while '// &
     &         'the file label '//SOB_SHOW__LABEL//' was expected' )
           RETURN 
      END IF
!
! --- Make the first run through the file contents
!
      L_STA = 0
      L_OBS = 0
      MJD_BEG = 0.0
      TAI_BEG = 0.0D0
      MJD_END = 0.0
      TAI_END = 0.0D0
      DO 410 J1=1,NP
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
!
! ------ Split the J1-th line into words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND < 2 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4214, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &            'the '//STR(1:I_LEN(STR))//' th line of '// &
     &            ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &            ' -- it should have atg least two words' )
              RETURN 
         END IF
!
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'SOB_DIR:' ) THEN
              SOB_DIR = BUF(J1)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DATE_TAI_BEG:' ) THEN
              IER = IUER 
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,2):IND(2,2)), MJD_BEG, TAI_BEG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4215, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse date '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' NB: supported format: YYYY.MM.DD_hh:mm:ss.s' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DATE_TAI_END:' ) THEN
              IER = IUER 
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,2):IND(2,2)), MJD_END, TAI_END, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4216, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse date '//BUF(J1)(IND(1,2):IND(2,2))// &
     &                 ' NB: supported format: YYYY.MM.DD_hh:mm:ss.s' )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'OBS:' ) THEN
              L_OBS = L_OBS + 1
              IF ( L_OBS > M_OBS ) THEN
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( M_OBS, STR1 )
                   CALL ERR_LOG ( 4217, IUER, 'SOH_PARSE', 'Failure in '// &
     &                 'processing the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- to many observations, more than '//STR1 )
                   RETURN 
              END IF
              IF ( LIND < 8 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4218, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- to few words, less than than 8' )
                   RETURN 
              END IF
!
! ----------- Add the station name to the list L_STA/C_STA
!
              IER = IUER
              IP = ADD_CLIST ( M_STA, L_STA, C_STA, BUF(J1)(IND(1,2):IND(2,2)), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( M_STA, STR1 )
                   CALL ERR_LOG ( 4219, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- to many stations, more than '//STR1 )
                   RETURN 
              END IF
            ELSE 
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4220, IUER, 'SOH_PARSE', 'Unsupported record '// &
     &            'definition at the '//STR(1:I_LEN(STR))//' th line of '// &
     &            ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &            BUF(J1)(IND(1,1):IND(2,1))//' . Supported words: '// &
     &            ' SOB_DIR, DATE_TAI_BEG, DATE_TAI_END, OBS' )
              RETURN 
         END IF
 410  CONTINUE 
!
! --- The second run. Parse observation records
!
      L_OBS = 0
      DO 420 J2=1,NP
         IF ( BUF(J2)(1:1) == '#' ) GOTO 420
!
! ------ Splie the J2-th line into words
!
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J2)(IND(1,1):IND(2,1)) == 'OBS:' ) THEN
!
! ----------- Get the station index
!
              L_OBS = L_OBS + 1
              IND_STA(L_OBS) = LTM_DIF ( 1, L_STA, C_STA, BUF(J2)(IND(1,2):IND(2,2)) ) 
!
! ----------- Get the observation epoch
!
              IER = IUER 
              CALL DATE_TO_TIME ( BUF(J2)(IND(1,3):IND(2,3)), MJD_OBS(L_OBS), &
     &                            TAI_OBS(L_OBS), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4221, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse date '//BUF(J2)(IND(1,3):IND(2,3))// &
     &                 ' NB: supported format: YYYY.MM.DD_hh:mm:ss.s' )
                   RETURN 
              END IF
!
! ----------- Get Frequency
!
              READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F20.5)', IOSTAT=IER ) FRQ(L_OBS)
              IF ( BUF(J2)(IND(1,5):IND(2,5)) == 'Hz' ) THEN
                   CONTINUE 
                 ELSE IF ( BUF(J2)(IND(1,5):IND(2,5)) == 'kHz' ) THEN
                   FRQ(L_OBS) = 1.0D3*FRQ(L_OBS)
                 ELSE IF ( BUF(J2)(IND(1,5):IND(2,5)) == 'MHz' ) THEN
                   FRQ(L_OBS) = 1.0D6*FRQ(L_OBS)
                 ELSE IF ( BUF(J2)(IND(1,5):IND(2,5)) == 'GHz' ) THEN
                   FRQ(L_OBS) = 1.0D9*FRQ(L_OBS)
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4222, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse the 5th word '//BUF(J2)(IND(1,5):IND(2,5))// &
     &                 ' -- Hz, kHz, MHz, GHz' )
                   RETURN 
              END IF
!
! ----------- Get AZ,  EL
!
              READ ( UNIT=BUF(J2)(IND(1,6):IND(2,6)), FMT='(F10.5)', IOSTAT=IER ) AZ(L_OBS)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4223, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse azimuth '//BUF(J2)(IND(1,6):IND(2,6))// &
     &                 ' -- a real number was expected' )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J2)(IND(1,7):IND(2,7)), FMT='(F10.5)', IOSTAT=IER ) EL(L_OBS)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4224, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse elevation '//BUF(J2)(IND(1,7):IND(2,7))// &
     &                 ' -- a real number was expected' )
                   RETURN 
              END IF
!
! ----------- Units transform if needed
!
              IF ( BUF(J2)(IND(1,8):IND(2,8))  == 'deg' ) THEN
                   AZ(L_OBS) = AZ(L_OBS)*DEG__TO__RAD
                   EL(L_OBS) = EL(L_OBS)*DEG__TO__RAD
                ELSE IF ( BUF(J2)(IND(1,8):IND(2,8))  == 'rad' ) THEN
                   CONTINUE 
                ELSE
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4225, IUER, 'SOH_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse the 8th word '//BUF(J2)(IND(1,8):IND(2,8))// &
     &                 ' -- rad or deg were expected' )
                   RETURN 
              END IF
         END IF
 420  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SOH_PARSE  !#!  
