      PROGRAM    SPR_MODEL
! ************************************************************************
! *                                                                      *
! *   Program SPR_MODEL computes parameters of a parameteric model       *
! *   that describes surface pressure. The model includes the average    *
! *   field on a specified epoch, the secular trend, cosine and sine     *
! *   components at specified frequencies (so-called atmospheric tides). *
! *                                                                      *
! *  ### 22-FEB-2013    SPR_MODEL  v4.0 (c)  L. Petrov  24-FEB-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_IN, HEB_OUT
      TYPE     ( MALO__TYPE ), POINTER :: MALO(:)
      INTEGER*4    M_SPR, M_PAR
      PARAMETER  ( M_SPR = 4*1024*1024 )
      PARAMETER  ( M_PAR = MALO__MWAV  )
      CHARACTER, ALLOCATABLE :: FIL_SPR(:)*128
      CHARACTER  DIR_NAM*128, FILNAM*128, &
     &           FIL_OUT*128, DATE_BEG*32, DATE_END*32, STR*128, &
     &           STR1*128, PAR_NAM*8, DATE_FIL*21
      INTEGER*8  DIR_DESC(32)
      REAL*4     TIM_R4(M_SPR), VAL_R4(M_SPR)
      REAL*8     TIM_R8(M_SPR), VAL_R8(M_SPR)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, IS, IL, LEV, L_FIL, &
     &           IP, IND_LAT, IND_LON, IND_PAR, MJD_BEG, MJD_END, MJD_MID, &
     &           MJD_FIL, IDAYS, MOD, L_PAR, L_JMP, IVRB, IUER
      REAL*8     EQU_VEC(M_PAR), NOR_MAT((M_PAR*(M_PAR+1))/2), &
     &           EST_R8(M_PAR), RC, T_MID, T_RAN, TAI_BEG, TAI_END, TAI_MID, &
     &           TAI_FIL, TIM_STEP, EPS, CNS_SIG
      LOGICAL*1  FL_ONLY_JUMPS
      PARAMETER  ( EPS = 500.0D0 )
      PARAMETER  ( CNS_SIG = 1.D0 )
      REAL*8,    ALLOCATABLE :: NOR_VEC(:,:,:)
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL :: GET_CDATE*19, MJDSEC_TO_DATE*30
!
! /progs/malo_20170320/bin_static/spr_model /imsl/heb/geosfpit      twland   2001.01.01_00:00 2017.02.20_00:00 3 /s1/temp/lws_geosfpit_model_2016_2017.heb
! /progs/malo_20170320/bin_static/spr_model /t0/imsl/spr/lws/merra2 merra2   1980.01.01_00:00 2017.02.28_21:00 4 /progs/malo_20170320/share/lws_merra2_model_1980_2017_d2699.heb
! /progs/malo_20180831/bin_static/spr_model /t0/spr_10799/geosfpit  geosfpit 2016.09.01_00:00 2017.01.01_00:00 7 /t0/imsl_devel_model/spr_atm_geosfpit_model_201609_201701_d10799.heb
! /progs/malo_20180831/bin_static/spr_model /t0/spr_2699/geosfpit   geosfpit 2016.09.01_00:00 2017.01.01_00:00 7 /t0/imsl_devel_model/spr_atm_geosfpit_model_201609_201701_d2699.heb
! spr_model /t0/heb/mpiom07/ mpiom07 1980.01.01 2023.01.01 2    /a1/imsl/oper_model/spr_nto_mpiom07_model_d2699_1980_2013.heb
!
      IVRB = 3
      ALLOCATE ( FIL_SPR(M_SPR), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(M_SPR)*INT8(SIZEOF(FIL_SPR(1))), STR )
           IUER = -1
           CALL ERR_LOG ( 6001, IUER, 'SPR_MODEL', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for array FIL_SPR' )
           CALL EXIT ( 1 )
      END IF
!
! --- Allocate memory for MALO object
!
      ALLOCATE ( MALO(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6002, IUER, 'SPR_MODEL', 'Error in an attempt '// &
     &         'to allocate memory for two objects MALO' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialize malo object
!
      IUER = -1
      CALL MALO_INIT ( MALO(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6003, IUER, 'SPR_MODEL', 'Error in an attempt '// &
     &         'to initialize object MALO' )
           CALL EXIT ( 1 )
      END IF
      MALO(1)%MJD_DEF = J2000__MJD
      MALO(1)%TAI_DEF = 43200.0D0
!
! --- Get argumnets
!
      IF ( IARGC() < 6 ) THEN
           WRITE ( 6, * ) 'Usage: spr_model dir_nam par_nam date_beg date_end mod filout [only_jumps]'
           WRITE ( 6, * ) 'mod is 1,2,3,4'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, DIR_NAM  )
           CALL GETARG ( 2, PAR_NAM  )
           CALL GETARG ( 3, DATE_BEG )
           CALL GETARG ( 4, DATE_END )
           CALL GETARG ( 5, STR      )
           CALL CHIN   ( STR, MOD )
           CALL GETARG ( 6, FIL_OUT  )
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, STR )
                IF ( STR == 'only_jumps' ) THEN
                     FL_ONLY_JUMPS = .TRUE.
                   ELSE 
                     IUER = -1
                     CALL ERR_LOG ( 6001, IUER, 'SPR_MODEL', 'Unsupported '// &
     &                   'the 7th argument '//TRIM(STR)//' while only_jump '// &
     &                   'was supported' )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                FL_ONLY_JUMPS = .FALSE.
           END IF
      END IF 
      IF ( MOD < 0 .OR. MOD > MALO__MFS ) THEN
           IUER = -1
           CALL CLRCH ( STR1 )
           CALL INCH  ( MALO__MFS, STR1 )
           CALL ERR_LOG ( 6004, IUER, 'SPR_MODEL', 'Wrong MOD: '// &
     &          STR(1:I_LEN(STR))//' it is out of range [1, '//STR1 )
           CALL EXIT ( 1 )
      END IF
!
! --- Parse the regression model code
!
      MALO(1)%CONF%MODEL_CODE = MOD
      IUER = - 1
      CALL MALO_MODC_PARSE ( MALO(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6005, IUER, 'SPR_MODEL', 'Error in parsing '// &
     &         'the model code' )
           CALL EXIT ( 1 )
      END IF
      L_PAR = MALO_EFS(MOD)
!
! --- Parse start and stop dates
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TAI_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6006, IUER, 'SPR_MODEL', 'Wrong begin date '//DATE_BEG )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD_END, TAI_END, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6007, IUER, 'SPR_MODEL', 'Wrong end date '//DATE_END )
           CALL EXIT ( 1 )
      END IF
!
! --- Collect files in the loop
!
      L_FIL = 0
      LEV   = 0
      DO 410 J1=1,M_SPR
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_NAM, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6008, IUER, 'SPR_MODEL', 'Error in '// &
     &           'reading input directory '//DIR_NAM(1:I_LEN(DIR_NAM))// &
     &           '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IL = ILEN(FILNAM)
         IF ( IL < 8 ) GOTO 410
         IF ( FILNAM(IL-3:IL) == '.heb'     .OR. &
     &        FILNAM(IL-7:IL) == '.heb.bz2'      ) THEN
!
              IP = LINDEX ( FILNAM, '.heb' )
              DATE_FIL = FILNAM(IP-13:IP-10)//'.'//FILNAM(IP-9:IP-8)//'.'// &
     &                   FILNAM(IP-7:IP-3)//':'//FILNAM(IP-2:IP-1)//'00.0'
!
              IUER = -1
              CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TAI_FIL, IUER ) 
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   CALL ERR_LOG ( 6009, IUER, 'SPR_MODEL', 'Wrong date '// &
     &                  DATE_FIL//' in file '//FILNAM ) 
                   CALL EXIT ( 1 )
              END IF
              IF ( (MJD_FIL*86400.0D0 + TAI_FIL) - (MJD_BEG*86400.0D0 + TAI_BEG) > -300.0 .AND. &
     &             (MJD_END*86400.0D0 + TAI_END) - (MJD_FIL*86400.0D0 + TAI_FIL) > -300.0       ) THEN
                   IF ( INDEX ( FILNAM, PAR_NAM(1:I_LEN(PAR_NAM)) ) > 0 ) THEN
                        L_FIL = L_FIL + 1
                        FIL_SPR(L_FIL) = FILNAM 
                   END IF
              END IF
         END IF
 410  CONTINUE 
      CALL CLRCH ( STR )
      CALL INCH  ( M_SPR, STR )
      IUER = -1
      CALL ERR_LOG ( 6010, IUER, 'SPR_MODEL', 'Not all files have been '// &
     &    'checked: parameter M_SPR = '//STR(1:I_LEN(STR))//' is too small' )
      CALL EXIT ( 1 )
!      
 810  CONTINUE 
      IF ( L_FIL == 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 6011, IUER, 'SPR_MODEL', 'Cannot find any target '// &
     &         'files in the input directory '//DIR_NAM(1:I_LEN(DIR_NAM))// &
     &         ' for parameter '//PAR_NAM )
           CALL EXIT ( 1 )
      END IF
!
! --- Sort file names
!
      CALL SORT_FAST_CH ( L_FIL, FIL_SPR )
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, 210 ) L_FIL
 210       FORMAT ( I7, ' files with surface pressure will be processed' )
      END IF
!
! --- Read the first file with surface pressure 
!
      IUER = -1
      CALL READ_HEB ( FIL_SPR(1), HEB_IN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6012, IUER, 'SPR_MODEL', 'Failure in attempt '// &
     &         'to read the first HEB file with surface pressure '// &
     &          FIL_SPR(1) )
           RETURN 
      END IF
      MJD_BEG = HEB_IN%MJD
      TAI_BEG = HEB_IN%UTC
!
! --- Read the last file with surface pressure 
!
      IUER = -1
      CALL READ_HEB ( FIL_SPR(L_FIL), HEB_IN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6013, IUER, 'SPR_MODEL', 'Failure in attempt '// &
     &         'to read the last HEB file with surface pressure '// &
     &          FIL_SPR(L_FIL) )
           RETURN 
      END IF
      MJD_END = HEB_IN%MJD
      TAI_END = HEB_IN%UTC
!
! --- Compute the middle epoch and the half time span. 
! --- We will need these parameters for secular drift normalization
!
      T_MID = ( (MJD_END - J2000__MJD)*86400.0D0 + (TAI_END  - 43200.0D0) + &
     &          (MJD_BEG - J2000__MJD)*86400.0D0 + (TAI_BEG  - 43200.0D0) )/2.0D0
      T_RAN = ( (MJD_END - J2000__MJD)*86400.0D0 + (TAI_END  - 43200.0D0) - &
     &          (MJD_BEG - J2000__MJD)*86400.0D0 - (TAI_BEG  - 43200.0D0) )/2.0D0
      TIM_STEP = 2.0D0*T_RAN/(L_FIL-1)
!
      TAI_MID = TAI_BEG + T_MID
      IDAYS = TAI_MID/86400.D0
      MJD_MID = J2000__MJD + IDAYS
      TAI_MID = TAI_MID    - IDAYS*86400.D0
      IF ( TAI_MID < 0.0 ) THEN
           MJD_MID = MJD_MID - 1
           TAI_MID = TAI_MID + 86400.0D0
      END IF
      IF ( IVRB .GE. 2 ) THEN
           write ( 6, * ) ' mjd_mid= ', mjd_mid, ' tai_mid= ', sngl(tai_mid), ' tim_step= ', sngl(tim_step)
           write ( 6, * ) ' mjd_beg= ', mjd_beg, ' tai_beg= ', tai_beg
           write ( 6, * ) ' mjd_end= ', mjd_end, ' tai_end= ', tai_end
           write ( 6, * ) ' t_ran = ', t_ran, ' total_range= ', 2.0d0*t_ran
           write ( 6, * ) ' t_mid = ', t_mid, ' t_ran = ', t_ran
           write ( 6, * ) ' l_par= ', l_par, ' m_par= ', m_par
      END IF
!
! --- Initialize HEB_OUT object and grab dynamic memory
!
      HEB_OUT%DIMS(1) = HEB_IN%DIMS(1)
      HEB_OUT%DIMS(2) = HEB_IN%DIMS(2)
      HEB_OUT%DIMS(3) = MALO_NFS(MOD)
      HEB_OUT%DIMS(4) = 1
!
      IUER = -1
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH   ( STR ) 
           CALL IINCH8  ( INT8(4)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3)*HEB_OUT%DIMS(4), &
     &                    STR )
           IUER = -1
           CALL ERR_LOG ( 6014, IUER, 'SPR_MODEL', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array NORM_VEC' )
           CALL EXIT ( 1 )
      END IF
      HEB_OUT%VAL = 0.0
!
      IUER = -1
      CALL SPR_MODEL_EST_JUMPS ( M_SPR, L_FIL, FIL_SPR, MALO(1), &
     &                           L_JMP, HEB_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6015, IUER, 'SPR_MODEL', 'Error in an attempt '// &
     &         'to estimate jumps in the surface pressure field' )
           CALL EXIT ( 1 )
      END IF 
      IF ( FL_ONLY_JUMPS ) THEN
           GOTO 710
      END IF
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, * ) ' Found ', L_JMP, ' jumps'
           WRITE ( 6, * ) ' The total number of parameters: ', L_PAR
           WRITE ( 6, * ) ' HEB_OUT%DIMS= ', HEB_OUT%DIMS
      END IF
!
      IUER = -1
      ALLOCATE ( NOR_VEC(L_PAR,HEB_IN%DIMS(1),HEB_IN%DIMS(2)), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL CLRCH  ( STR ) 
           CALL IINCH8 ( INT8(8)*INT8(L_PAR)*HEB_IN%DIMS(1)*HEB_IN%DIMS(2), STR )
           CALL ERR_LOG ( 6016, IUER, 'SPR_MODEL', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of memory for '// &
     &         'array NORM_VEC' )
           CALL EXIT ( 1 )
      END IF
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
!
! --- Cycle over epochs
!
      CALL WALL_TIMER ( %VAL(0) )
      DO 420 J2=1,L_FIL
!
! ------ Read surface pressure
!
         IUER = -1
         CALL READ_HEB ( FIL_SPR(J2), HEB_IN, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6017, IUER, 'SPR_MODEL', 'Failure in '// &
     &            'an attempt to read the HEB file with surface '// &
     &            'pressure '//FIL_SPR(J2) )
              CALL EXIT ( 1 )
         END IF
         TIM_R8(J2) = (HEB_IN%MJD - MALO(1)%MJD_DEF)*86400.0D0 + &
     &                (HEB_IN%UTC - MALO(1)%TAI_DEF)
!
! ------ Build observation equation
!
         EQU_VEC = 0.0D0
         DO 430 J3=1,L_PAR
            IF ( MALO(1)%MODC(J3)%TYP == MALO__CNST ) THEN
                 EQU_VEC(J3) = 1.0D0
              ELSE IF ( MALO(1)%MODC(J3)%TYP == MALO__DRFT ) THEN
                 EQU_VEC(J3) = (TIM_R8(J2) - T_MID)/T_RAN
              ELSE IF ( MALO(1)%MODC(J3)%TYP == MALO__COS  ) THEN
                 EQU_VEC(J3) = DCOS(MALO(1)%MODC(J3)%FRQ*TIM_R8(J2) + MALO(1)%MODC(J3)%PHS)
              ELSE IF ( MALO(1)%MODC(J3)%TYP == MALO__SIN  ) THEN
                 EQU_VEC(J3) = DSIN(MALO(1)%MODC(J3)%FRQ*TIM_R8(J2) + MALO(1)%MODC(J3)%PHS)
            END IF
 430     CONTINUE 
!%   write ( 6, * ) ' equ_vec= ', equ_vec(1:l_par)  ! %%%%
!
! ------ Subtract jumps
!
         DO 440 J4=1,MALO(1)%NMDC
            IF ( MALO(1)%MODC(J4)%TYP == MALO__JMP ) THEN
                 IF ( TIM_R8(J2) .GE. MALO(1)%MODC(J4)%TIM ) THEN
                      HEB_IN%VAL(1:HEB_IN%DIMS(1),1:HEB_IN%DIMS(2),1,1) = &
     &                       HEB_IN%VAL(1:HEB_IN%DIMS(1),1:HEB_IN%DIMS(2),1,1)   &
     &                     - HEB_OUT%VAL(1:HEB_IN%DIMS(1),1:HEB_IN%DIMS(2),J4,1) 
                 END IF
           END IF
 440    CONTINUE 
!
! ------ Update normal matrux
!
         CALL DIAD_CVT_S ( 1.0D0, L_PAR, EQU_VEC, EQU_VEC, NOR_MAT )
!
! ------ Update normal vectors
!
!$OMP PARALLEL DO PRIVATE ( J5, J6 ), SCHEDULE ( DYNAMIC, 1 )
         DO 450 J5=1,HEB_IN%DIMS(2)
            DO 460 J6=1,HEB_IN%DIMS(1)
               CALL NORVEC_UPD ( L_PAR, 1.0D0, DBLE(HEB_IN%VAL(J6,J5,1,1)), &
     &                           EQU_VEC, NOR_VEC(1,J6,J5) )
 460        CONTINUE 
 450     CONTINUE 
!$OMP END PARALLEL DO
         IF ( ASSOCIATED ( HEB_IN%VAL ) ) DEALLOCATE ( HEB_IN%VAL )
         IF ( IVRB .GE. 2 ) THEN
              CALL WALL_TIMER ( STR )
              WRITE ( 6, '("  Epoch  ", I6, " ( ", I6, " )  ",A,A$)' )  J2, &
     &                        L_FIL, STR(1:27), CHAR(13)
              CALL FLUSH ( 6 )
              CALL WALL_TIMER ( %VAL(0) )
         END IF
 420  CONTINUE 
!
      IF ( IVRB .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) '                                    '
           WRITE ( 6, '(I6,A)' ) L_FIL, ' epocs are processed'
           WRITE ( 6, '(A)'   ) 'Matrix inversion'
      END IF
!
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, * ) 'NOR_MAT= ', NOR_MAT 
      END IF
!%      CALL MATVIEW_2 ( L_PAR, NOR_MAT )
!
! --- Invert normal matrix
!
      IUER = -1
      CALL INVS ( L_PAR, NOR_MAT, RC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6018, IUER, 'SPR_MODEL', 'Failure in attempt '// &
     &         'to invert matrix NOR_MAT' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVRB .GE. 3 ) THEN
           WRITE ( 6, * ) ' RC = ', RC
      END IF
!
! --- Compute parameters of the regression model using the inverse 
! --- of the normal matrix and normal vectors
!
      DO 470 J7=1,HEB_IN%DIMS(2)
!$OMP PARALLEL DO DEFAULT ( NONE ) &
!$OMP    PRIVATE ( J8, J9, EST_R8, IUER ) &
!$OMP    SHARED  ( J7, L_PAR, NOR_MAT, NOR_VEC, T_RAN, HEB_OUT, MALO ) &
!$OMP    SCHEDULE ( DYNAMIC )
         DO 480 J8=1,HEB_IN%DIMS(1)
            CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC(1,J8,J7), &
     &                         L_PAR, EST_R8, IUER )
            HEB_OUT%VAL(J8,J7,1:L_PAR,1) = EST_R8(1:L_PAR)
            DO 490 J9=1,L_PAR
               IF ( MALO(1)%MODC(J9)%TYP == MALO__DRFT ) THEN
!
! ----------------- Unscale drift
!
                    HEB_OUT%VAL(J8,J7,J9,1) = HEB_OUT%VAL(J8,J7,J9,1)/T_RAN*YEAR__TO__SEC 
               END IF
 490        CONTINUE 
 480     CONTINUE 
!$OMP END PARALLEL DO
 470  CONTINUE 
 710  CONTINUE 
!
! --- Prepare other fields of the output
!
      HEB_OUT%UTC  = TAI_MID
      HEB_OUT%TAI  = TAI_MID
      HEB_OUT%MJD  = MJD_MID
      HEB_OUT%DATA_OFFSET = HEB__HDS
      HEB_OUT%ENDIAN      = HEB__LE
      HEB_OUT%DATA_TRANSFORM = HEB__NONE
      HEB_OUT%FILL_VALUE     = 1.0E15
      HEB_OUT%OFFSET         = 0.0
      HEB_OUT%SCALE_FACTOR   = 1.0
      HEB_OUT%DATA_COMPRESSION = HEB__NONE
      IF ( HEB_IN%SDS_NAME(1:20) == 'Atmospheric pressure' ) THEN
           HEB_OUT%SDS_NAME  = 'Regression model of the surface atmospheric pressure'
         ELSE IF ( HEB_IN%SDS_NAME(1:11) == 'Total water' ) THEN
           HEB_OUT%SDS_NAME       = 'Land water storage pressure'
         ELSE IF ( HEB_IN%SDS_NAME(1:15) == 'Bottom pressure' ) THEN
           HEB_OUT%SDS_NAME       = 'Bottom pressure'
      END IF
!
      HEB_OUT%PROD_NAME      = HEB_OUT%SDS_NAME       
      HEB_OUT%TITLE          = HEB_OUT%SDS_NAME
      HEB_OUT%SOURCE         = HEB_IN%SOURCE
      HEB_OUT%UNITS          = 'Pa'
      HEB_OUT%SOURCE         = HEB_IN%SOURCE
      HEB_OUT%DATA_FORMAT    = HEB__R4
      HEB_OUT%MIN_VALUE      = MINVAL(HEB_OUT%VAL)
      HEB_OUT%MAX_VALUE      = MAXVAL(HEB_OUT%VAL)
      HEB_OUT%VALID_RANGE(1) = MINVAL(HEB_OUT%VAL)
      HEB_OUT%VALID_RANGE(2) = MAXVAL(HEB_OUT%VAL)
      HEB_OUT%PROD_DATE_TIME = GET_CDATE()
      HEB_OUT%FILE_NAME      = FIL_OUT
      HEB_OUT%HISTORY        = HEB_IN%HISTORY
      HEB_OUT%VERSION_ID     = '1'
      HEB_OUT%INSTITUTION    = 'Astrogeo Center'
      HEB_OUT%REFERENCES = 'Petrov, L., (2015) http://arxiv.org/abs/1503.00191'
      HEB_OUT%COMMENT(1) = 'The regression was computed over interval of time '// &
     &                      DATE_BEG(1:10)//' -- '//DATE_END(1:10)
      CALL CLRCH ( STR )
      CALL INCH  ( MOD, STR )
      HEB_OUT%COMMENT(2) = 'Regression model code: '//STR(1:I_LEN(STR))
      STR = MJDSEC_TO_DATE ( MALO(1)%MJD_DEF, MALO(1)%TAI_DEF, IUER )
      HEB_OUT%COMMENT(3) = 'Model reference epoch: '//STR(1:21)
!
! --- Write the output file
!
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      WRITE ( 6, * ) 'Output file: '//FIL_OUT(1:I_LEN(FIL_OUT))
!
      END  PROGRAM  SPR_MODEL  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SPR_MODEL_EST_JUMPS ( M_SPR, L_FIL, FIL_SPR, MALO, &
     &                                 L_JMP, HEB_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPR_MODEL_EST_JUMPS 
! *                                                                      *
! * ## 21-FEB-2017 SPR_MODEL_EST_JUMPS v1.0 (c) L. Petrov 21-FEB-2017 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE  ) :: HEB_OUT
      INTEGER*4  M_SPR, L_FIL, L_JMP, IUER
      CHARACTER  FIL_SPR(L_FIL)*(*)
      TYPE     ( HEB__TYPE ) :: HEB_BEF(2), HEB_AFT(2)
      CHARACTER  DATE_FIL*128, STR*128
      INTEGER*4  J1, J2, J3, J4, J5, MJD_FIL, IND_JMP, L_PAR, IP, IER
      REAL*8     TAI_FIL, TIM_FIL, TIM_R8(M_SPR), TIM_BEF(2), TIM_AFT(2), &
     &           TB1, TB2, TA1, TA2
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX
!
      L_JMP = MALO_NFS(MALO%CONF%MODEL_CODE) - MALO_EFS(MALO%CONF%MODEL_CODE) 
      IF ( L_JMP == 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      L_JMP = 0 
      DO 410 J1=1,MALO%NMDC
         IF ( MALO%MODC(J1)%TYP == MALO__JMP ) THEN
              L_JMP = L_JMP + 1
              IND_JMP = 0
              DO 420 J2=1,L_FIL
                 IP = LINDEX ( FIL_SPR(J2), '.heb' )
                 DATE_FIL = FIL_SPR(J2)(IP-13:IP-10)//'.'//FIL_SPR(J2)(IP-9:IP-8)//'.'// &
     &                      FIL_SPR(J2)(IP-7:IP-3)//':'//FIL_SPR(J2)(IP-2:IP-1)//'00.0'
!
                 CALL ERR_PASS ( IUER, IER )
                 CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TAI_FIL, IUER ) 
                 IF ( IUER .NE. 0 ) THEN
                      IUER = -1
                      CALL ERR_LOG ( 6041, IUER, 'SPR_MODEL_EST_JUMPS', 'Wrong date '// &
     &                     DATE_FIL//' in file '//FIL_SPR(J2) ) 
                      RETURN
                 END IF
                 TIM_R8(J2) = (MJD_FIL - MALO%MJD_DEF)*86400.0D0 + (TAI_FIL - MALO%TAI_DEF)
                 IF ( J2 > 1 ) THEN
                      IF ( TIM_R8(J2)   .GE. MALO%MODC(J1)%TIM .AND. &
     &                     TIM_R8(J2-1) .LT. MALO%MODC(J1)%TIM       ) THEN
                           IND_JMP = J2
                      END IF
                 END IF
 420          CONTINUE 
              IF ( IND_JMP == 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( L_JMP, STR )
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 6042, IUER, 'SPR_MODEL_EST_JUMPS', 'Did not find '// &
     &                 'jump epoch '//STR )
                   RETURN
              END IF
!
! ----------- Two epochs preceedings the jump
!
              DO 430 J3=1,2
                 CALL ERR_PASS ( IUER, IER )
                 CALL READ_HEB ( FIL_SPR(IND_JMP-3+J3), HEB_BEF(J3), IER )
                 IF ( IER .NE. 0  ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ERR_LOG ( 6043, IUER, 'SPR_MODEL_EST_JUMPS', 'Error in '// &
     &                    'reading surface pressure file '//FIL_SPR(IND_JMP+1-J3) )
                      RETURN
                 END IF
                 TIM_BEF(J3) = (HEB_BEF(J3)%MJD - MALO%MJD_DEF)*86400.0D0 + &
     &                         (HEB_BEF(J3)%UTC - MALO%TAI_DEF)
 430          CONTINUE 
!
! ----------- Two epochs following the jump
!
              DO 440 J4=1,2
                 CALL ERR_PASS ( IUER, IER )
                 CALL READ_HEB ( FIL_SPR(IND_JMP-1+J4), HEB_AFT(J4), IER )
                 IF ( IER .NE. 0  ) THEN
                      CALL ERR_PASS ( IUER, IER )
                      CALL ERR_LOG ( 6044, IUER, 'SPR_MODEL_EST_JUMPS', 'Error in '// &
     &                    'reading surface pressure file '//FIL_SPR(IND_JMP+J4-1) )
                      RETURN
                 END IF
                 TIM_AFT(J4) = (HEB_AFT(J4)%MJD - MALO%MJD_DEF)*86400.0D0 + &
     &                         (HEB_AFT(J4)%UTC - MALO%TAI_DEF)
 440          CONTINUE 
              TB1 =  (MALO%MODC(J1)%TIM - TIM_BEF(2))/(TIM_BEF(2) - TIM_BEF(1))
              TB2 = -(MALO%MODC(J1)%TIM - TIM_BEF(1))/(TIM_BEF(2) - TIM_BEF(1))
              TA1 = -(MALO%MODC(J1)%TIM - TIM_AFT(2))/(TIM_AFT(2) - TIM_AFT(1))
              TA2 =  (MALO%MODC(J1)%TIM - TIM_AFT(1))/(TIM_AFT(2) - TIM_AFT(1))
              HEB_OUT%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),J1,1) = &
     &                TB1*HEB_BEF(1)%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),1,1) + &
     &                TB2*HEB_BEF(2)%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),1,1) + &
     &                TA1*HEB_AFT(1)%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),1,1) + &
     &                TA2*HEB_AFT(2)%VAL(1:HEB_OUT%DIMS(1),1:HEB_OUT%DIMS(2),1,1)
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPR_MODEL_EST_JUMPS  !#!#
