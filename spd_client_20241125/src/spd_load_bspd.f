      SUBROUTINE SPD_LOAD_BSPD ( L_DIR, C_DIR, SPD_BIAS_FILE, APD_NAME, &
     &                           LAYER_HEIGHT, LAYER_FWHM, L_STA, C_STA, &
     &                           MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                           SPD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_LOAD_BSPD  reads a list of directories with slant     *
! *   path delays in binary format, finds files for the specified list   *
! *   of stations ad the specified date range [MJD_BEG/TAI_BEG,          *
! *   MJD_END/TAI_END].  It reads the path delays and for each station   *
! *   computes te coefficients of the 3D B-spline expansion of path      *
! *   delay over mapping value, azimuth and time. Mapping values is      *
! *   a function of elevation angle. It is the ratio of path delay at    *
! *   a given elevation angle to the path delay at zenith direction for  *
! *   the ISA 1976 Standard Atmosphere. The coefficients are written in  *
! *   fields of array of objects SPD.                                    *
! *                                                                      *
! *   Slant path delay for a given station may be present in several     *
! *   directories. If so, the definition in the first directories will   *
! *   take the precedence. That mean, if the data for a given station    *
! *   present in i-th directory, the i+1, i+2, etc will not be sought.   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *         L_DIR ( INTEGER*4 ) -- The number of directories with        *
! *                                slant path delays in BSPD format.     *
! *         C_DIR ( CHARACTER ) -- List of directories with slant path   *
! *                                delay in BSPD format.                 *
! *                                Dimension: L_DIR.                     *
! * SPD_BIAS_FILE ( CHARACTER ) -- File with biases of slant path delay. *
! *                                NONE means no biases to slant path    *
! *                                delay will be applied.                *
! *  APD_NAME ( CHARACTER ) -- Model of partial derivativies with        *
! *                            respect to atmosphere path delay in       *
! *                            zenith direction. Supported models:       *
! *                    NMFW -- Niell (1996) mapping function (wet).      *
! *                    NMFH -- Niell (1996) mapping function             *
! *                            (hydrostatic) on the mean epoch.          *
! *             TOTAL_SCALE -- mapping function for the case when        *
! *                            residual atmosphere is considered         *
! *                            proportional to the total atmosphere.     *
! *                            The partial derivative is defined as      *
! *                            a ratio of the total slant path delay to  *
! *                            the total path delay in the zenith        *
! *                            direction.                                *
! *             WATER_SCALE -- mapping function for the case when        *
! *                            residual atmosphere is considered         *
! *                            proportional to the water vapor           *
! *                            contribution to the atmosphere.           *
! *                            The partial derivative is defined as a    *
! *                            ratio of the water vapor contribution of  *
! *                            slant path delay to the water vapor       *
! *                            contribution to path delay in the zenith  *
! *                             direction.                               *
! *          GAUSSIAN_LAYER -- mapping function for the case when the    *
! *                            dependence of concentration of the        *
! *                            residual atmosphere is described with     *
! *                            the Gaussian model with the specified     *
! *                            height and the specified full width half  *
! *                            maximum (FWHM).                           *
! * LAYER_HEIGHT ( REAL*8 ) -- If the partial derivative model is        *
! *                            GAUSSIAN_LAYER, then APD_PAR1 is the      *
! *                            layer height in meters. Otherwise, it is  *
! *                            zero.                                     *
! * LAYER_FWHM   ( REAL*8 ) -- If the partial derivative model is        *
! *                            GAUSSIAN_LAYER, then APD_PAR1 is the      *
! *                            layer FWHM in meters. Otherwise, it is    *
! *                            zero.                                     *
! *         L_STA ( INTEGER*4 ) -- The number of stations for which      *
! *                                slant path delay will be interpolated.*
! *         C_STA ( CHARACTER ) -- A list of station names for which     *
! *                                slant path delay will be interpolated.*
! *                                The name should be in the list of     *
! *                                stations for which the path delay     *
! *                                have been computed. Dimension: L_STA. *
! *       MJD_BEG ( INTEGER*4 ) -- Integer Modified Julian Date of the   *
! *                                beginning of the interval of          *
! *                                interpolation. Units: days.           *
! *       TAI_BEG ( REAL*8    ) -- TAI time since the midnight of the    *
! *                                beginning of the interval of          *
! *                                interpolation. Units: sec.            *
! *       MJD_END ( INTEGER*4 ) -- Integer Modified Julian Date of the   *
! *                                end of the interval of interpolation. *
! *                                Units: days.                          *
! *       TAI_END ( REAL*8    ) -- TAI time since the midnight of the    *
! *                                end of the interval of                *
! *                                interpolation. Units: sec.            *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      SPD ( SPD_DEL__TYPE ) -- Array of objects with Slant Path       *
! *                               Delays. Routine SPD_LOAD_BSPD will     *
! *                               populate internal fields of SPD        *
! *                               array. Dimension: L_STA.               *
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
! * ### 21-FEB-2009  SPD_LOAD_BSPD   v3.1 (c) L. Petrov 05-JAN-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      TYPE     ( SPD_DEL__TYPE ) :: SPD(L_STA)
      INTEGER*4  L_DIR, L_STA, MJD_BEG, MJD_END, IUER
      CHARACTER  C_STA(L_STA)*(*), C_DIR(L_DIR)*(*), SPD_BIAS_FILE*(*), &
     &           APD_NAME*(*)
      REAL*8     TAI_BEG, TAI_END, LAYER_HEIGHT, LAYER_FWHM
      CHARACTER, ALLOCATABLE :: FIL_SPD(:,:)*128
      CHARACTER  FILNAM*128, OUT*1024, OUT1*1024, STA_FIL*8, STR*32, STR1*32
      ADDRESS__TYPE :: DIR_DESC
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, LEV, &
     &           M_FIL, L_FIL, IS, IL, ILO, NM_STA, IND_BEG, IND_END, &
     &           DIMS(3), IND_STA, IER
      LOGICAL*1  FL_LET, FL_LAST_DIR
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, I_LEN, ILEN, LTM_DIF
!
! --- Allocate memory for file names
!
      ALLOCATE ( FIL_SPD(L_STA,L_DIR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL IINCH  ( L_STA*L_DIR*LEN(FIL_SPD(1,1)), STR )
           CALL ERR_LOG ( 3811, IUER, 'SPD_LOAD_BSPD', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array FIL_SPD' )
           RETURN 
      END IF
!
! --- Initialization of SPD objects
!
      DO 410 J1=1,L_DIR
         DO 420 J2=1,L_STA
            CALL CLRCH ( FIL_SPD(J2,J1) )
            IF ( J1 == 1 ) THEN
                 IF ( ASSOCIATED ( SPD(J2)%SUR_PRS ) ) THEN
                      DEALLOCATE ( SPD(J2)%SUR_PRS )
                 END IF
                 IF ( ASSOCIATED ( SPD(J2)%SUR_PWP ) ) THEN
                      DEALLOCATE ( SPD(J2)%SUR_PWP )
                 END IF
                 IF ( ASSOCIATED ( SPD(J2)%SUR_TEM ) ) THEN
                      DEALLOCATE ( SPD(J2)%SUR_TEM )
                 END IF
                 IF ( ASSOCIATED ( SPD(J2)%DELS ) ) THEN
                      DEALLOCATE ( SPD(J2)%DELS )
                 END IF
                 IF ( ASSOCIATED ( SPD(J2)%MAP_ARR ) ) THEN
                      DEALLOCATE ( SPD(J2)%MAP_ARR )
                 END IF
                 IF ( ASSOCIATED ( SPD(J2)%TIM_ARR ) ) THEN
                      DEALLOCATE ( SPD(J2)%TIM_ARR )
                 END IF
                 IF ( ASSOCIATED ( SPD(J2)%ZEN_DEL ) ) THEN
                      DEALLOCATE ( SPD(J2)%ZEN_DEL )
                 END IF
                 SPD(J2)%STATUS = SPD__INIT
            END IF
 420    CONTINUE 
 410  CONTINUE 
!
      CALL CLRCH ( OUT1 )
      DO 430 J3=1,L_DIR
         IF ( ILEN(C_DIR(J3)) > 0 ) THEN
              OUT1 = OUT1(1:ILEN(OUT1)+2)//C_DIR(J3)
!
! ----------- OK, the J3-th directory name is not empty
! ----------- Cycle over all files of the directory
!
              LEV = 0
              DO 440 J4=1,1024*1024*1024
                 IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, C_DIR(J3), FILNAM )
                 IF ( LEV == 0 ) GOTO 840
                 IF ( IS .NE. 0 ) THEN
                      CALL ERR_LOG ( 3812, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &                    'reading directory '//C_DIR(J3)(1:I_LEN(C_DIR(J3)))// &
     &                    ' -- '//FILNAM(1:I_LEN(FILNAM))//' where files '// &
     &                    'with slant path delay are supposed to reside' )
                      RETURN 
                 END IF
!
! -------------- Filter unwanated file names
!
                 IL = ILEN(FILNAM) 
                 IF ( IL < 13 ) GOTO 440
                 IF ( FILNAM(IL-4:IL) .NE. '.bspd' ) GOTO 440
!
! -------------- Build the station name from the file name
!
                 FL_LET = .FALSE.
!
! -------------- Condition the file name
!
                 DO 450 J5=8,1,-1
                    STA_FIL(J5:J5) = FILNAM(IL-13+J5:IL-13+J5) 
!
! ----------------- Replace trailing underscores with blanks
!
                    IF ( .NOT. FL_LET .AND. STA_FIL(J5:J5) == '_' ) THEN
                          STA_FIL(J5:J5) = ' ' 
                    END IF
                    IF ( STA_FIL(J5:J5) .NE. '_' .AND. &
     &                   STA_FIL(J5:J5) .NE. ' '       ) THEN
                         FL_LET = .TRUE.
                    END IF
!
! ----------------- Replace letters of small register to letters of big register
!
                    IF ( ICHAR(STA_FIL(J5:J5)) .GE. ICHAR('a') .AND. &
     &                   ICHAR(STA_FIL(J5:J5)) .LE. ICHAR('z')       ) THEN
                         STA_FIL(J5:J5) = CHAR(ICHAR(STA_FIL(J5:J5))-32)
                    END IF
 450             CONTINUE 
!
! -------------- NB: station name in C_STA is in the upper register, 
! -------------- but station name in the external file ids in the lower register
!
                 DO 460 J6=1,L_STA
                    IF ( C_STA(J6) == STA_FIL ) THEN
                         FIL_SPD(J6,J3) = FILNAM
                    END IF
 460             CONTINUE 
 440         CONTINUE 
 840         CONTINUE 
         END IF
 430  CONTINUE 
!
! --- Check whether we have found data for all the stations. 
! --- Put the name of stations not found in OUT list.
! --- NM_STA -- the number of missing stations
!
      CALL CLRCH  ( OUT )
      ILO = 1
      NM_STA = 0
      DO 470 J7=1,L_STA
         DO 480 J8=1,L_DIR
            IF ( ILEN(FIL_SPD(J7,J8)) > 0 ) GOTO 470
 480     CONTINUE 
!
         OUT(ILO:ILO+7) = C_STA(J7)
         OUT(ILO+8:ILO+8) =  ' ' 
         ILO = ILO + 9
         NM_STA = NM_STA + 1
 470  CONTINUE 
!
      IF ( NM_STA > 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NM_STA, STR )
           IF ( L_DIR == 1 ) THEN
                OUT1 = 'y '//OUT1(2:)
              ELSE 
                OUT1 = 'ies '//OUT1(2:)
           END IF
           CALL ERR_LOG ( 3813, IUER, 'SPD_LOAD_BSPD', 'Cannot find '// &
     &         'files with slant path delays for '//STR(1:I_LEN(STR))// &
     &         ' stations: '//OUT(1:I_LEN(OUT))//' in director'// &
     &          OUT1 )
           RETURN 
      END IF
!
! --- Cycle over stations
!
      DO 490 J9=1,L_STA
         CALL SPD_DEL_INIT ( SPD(J9), IER )
!
! ------ Cycle over directories
!
         DO 4100 J10=1,L_DIR
!
! --------- Of the data for station J9 have alread been parsed -- end of cycle
!
            IF ( SPD(J9)%STATUS == SPD__READ ) GOTO 490
            IF ( ILEN(FIL_SPD(J9,J10)) == 0 ) GOTO 4100
            IF ( J10 == L_DIR ) THEN
                 FL_LAST_DIR = .TRUE.
               ELSE 
                 IF ( ILEN(FIL_SPD(J9,J10)) == 0 ) THEN
                     FL_LAST_DIR = .TRUE.
                   ELSE
                     FL_LAST_DIR = .FALSE.
                 END IF
            END IF
!
! --------- Read the header of the file
!
            CALL ERR_PASS ( IUER, IER )
            CALL SPD_3D_BIN_READ_HEAD ( FIL_SPD(J9,J10), SPD(J9), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3814, IUER, 'SPD_LOAD_BSPD', 'Error in an '// &
     &               'attempt to parse the header of the slant path delay '// &
     &               'file '//FIL_SPD(J9,J10)(1:I_LEN(FIL_SPD(J9,J10)))// &
     &               ' for station '//C_STA(J9) )
                 RETURN 
            END IF
!
! --------- Determine indexes of slant path delay record for the beginning and 
! --------- the end of observations interval
!
            IND_BEG = ( (MJD_BEG*86400.0 + TAI_BEG) - &
     &          (SPD(J9)%TIM%MJD_BEG*86400.0 + SPD(J9)%TIM%TAI_BEG) )/ &
     &           SPD(J9)%TIM%TIM_STEP
!
            IND_END = ( (MJD_END*86400.0 + TAI_END) - &
     &          (SPD(J9)%TIM%MJD_BEG*86400.0 + SPD(J9)%TIM%TAI_BEG) )/ &
     &           SPD(J9)%TIM%TIM_STEP
!
            IF ( (MJD_END*86400.0 + TAI_END) >  &
     &           (SPD(J9)%TIM%MJD_BEG*86400.0 + SPD(J9)%TIM%TAI_BEG) + &
     &           IND_END*SPD(J9)%TIM%TIM_STEP ) THEN
                 IND_END = IND_END + 1
            END IF
!
            IF ( IND_BEG < 1 ) THEN
!
! -------------- Beginning the interval date is out of range
!
                 IF ( FL_LAST_DIR ) THEN
                      CALL CLRCH ( STR  )
                      CALL CLRCH ( STR1 )
                      STR  = MJDSEC_TO_DATE ( SPD(J9)%TIM%MJD_BEG, &
     &                                          SPD(J9)%TIM%TAI_BEG, -2 )
                      STR1 = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, -2 )
                      CALL ERR_LOG ( 3815, IUER, 'SPD_LOAD_BSPD', 'The file with '// &
     &                    'slant path delay for '// &
     &                     FIL_SPD(J9,J10)(1:I_LEN(FIL_SPD(J9,J10)))//' has the first '// &
     &                    'epoch '//STR(1:21)//', which is later than the first '// &
     &                    'epoch of observations '//STR1 )
                      RETURN 
                    ELSE
!
! ------------------- Since this is not the last directory, we do not lose hope
!
                      GOTO 4100
                 END IF
            END IF
!
            IF ( IND_END > SPD(J9)%LAB%TOT_NUM_DEL ) THEN
!
! -------------- End of the interval date is out of range
!
                 IF ( FL_LAST_DIR ) THEN
                      CALL CLRCH ( STR  )
                      CALL CLRCH ( STR1 )
                      STR  = MJDSEC_TO_DATE ( SPD(J9)%TIM%MJD_END, &
     &                                        SPD(J9)%TIM%TAI_END, -2 )
                      STR1 = MJDSEC_TO_DATE ( MJD_END, TAI_END, -2 )
                      WRITE ( 6, * ) ' SPD(J9)%LAB%TOT_NUM_DEL = ', SPD(J9)%LAB%TOT_NUM_DEL 
                      CALL ERR_LOG ( 3816, IUER, 'SPD_LOAD_BSPD', 'The file with '// &
     &                    'slant path delay for '// &
     &                     FIL_SPD(J9,J10)(1:I_LEN(FIL_SPD(J9,J10)))//' has the last '// &
     &                    'epoch '//STR(1:21)//', which is earlier than the first '// &
     &                    'epoch of observations '//STR1 )
                      RETURN 
                   ELSE 
!
! ------------------- Since this is not the last directory, we do not lose hope
!
                      GOTO 4100
                 END IF
            END IF
!
            IF ( IND_BEG - M_SPD__OVR > 1 ) THEN
!
! -------------- If there is room we shift the first epoch backward to 
! -------------- reduce effect of loss of accuracy of interpolation at the
! -------------- edge of the interval
!
                 IND_BEG = IND_BEG - M_SPD__OVR
                ELSE 
                 IND_BEG = 1
            END IF
!
            IF ( IND_END + M_SPD__OVR < SPD(J9)%LAB%TOT_NUM_DEL ) THEN
!
! -------------- If there is room we shift the last epoch forward to 
! -------------- reduce effect of loss of accuracy of interpolation at the
! -------------- edge of the interval
!
                 IND_END = IND_END + M_SPD__OVR  
               ELSE 
                 IND_END = SPD(J9)%LAB%TOT_NUM_DEL
            END IF
!
! --------- Store the beginning interpolation interval
!
            SPD(J9)%TIM_BEG = SPD(J9)%TIM%MJD_BEG*86400.0D0 + &
     &                               SPD(J9)%TIM%TAI_BEG + &
     &                               (IND_BEG-1)*SPD(J9)%TIM%TIM_STEP
!
! --------- Get the two arrays of slant path delay at the 2D grid
!
            CALL ERR_PASS ( IUER, IER )
            CALL SPD_3D_BIN_READ_DEL ( FIL_SPD(J9,J10), SPD(J9), &
     &                                 IND_BEG, IND_END, 1-SPD__MDEG, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3817, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &               'an attempt to read delay records from the slant '// &
     &               'path delay file '//FIL_SPD(J9,J10)(1:I_LEN(FIL_SPD(J9,J10)))// &
     &               ' for station '//C_STA(J9) )
                 RETURN 
            END IF
!
! --------- Store dimensions
!
            DIMS(1) = SPD(J9)%ELV%N_EL
            DIMS(2) = SPD(J9)%AZM%N_AZ
            DIMS(3) = SPD(J9)%N_TIM
!
! --------- Cycle over total slant path delay and wet path delay
!
            DO 4110 J11=1,SPD(J9)%MOD%N_RFR
!
! ------------ Compute the B-spline expansion for slant path delay
!
               CALL ERR_PASS ( IUER, IER )
               CALL BSPL4_3D_CMP ( SPD__MDEG, 0, DIMS, &
     &                             SPD(J9)%MAP_ARR, &
     &                             SPD(J9)%AZM%AZIM, &
     &                             SPD(J9)%TIM_ARR, &
     &              SPD(J9)%DELS(1-SPD__MDEG,1-SPD__MDEG,1-SPD__MDEG,J11), &
     &              IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 3818, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &                  'an attempt to compute B-spline expansion for the '// &
     &                  'first component of the slant path delay for '// &
     &                  'station '//C_STA(J9) )
                    RETURN 
               END IF
!
! ------------ Compute the B-spline expansion for the path delay in zenith direction
!
               CALL ERR_PASS ( IUER, IER )
               CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD(J9)%N_TIM, &
     &                             SPD(J9)%TIM_ARR, &
     &                             SPD(J9)%ZEN_DEL(1-SPD__MDEG,J11), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 3819, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &                  'an attempt to compute B-spline expansion for the '// &
     &                  'path delay in zenith direction at station '// &
     &                   C_STA(J9) )
                    RETURN 
               END IF
 4110       CONTINUE 
!
! --------- Compute the B-spline expansion for surface pressure
!
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD(J9)%N_TIM, &
     &                          SPD(J9)%TIM_ARR, &
     &                          SPD(J9)%SUR_PRS, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3820, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &               'an attempt to compute B-spline expansion for the '// &
     &               'surface atmosperic pressire at station '// &
     &                C_STA(J9) )
                 RETURN 
            END IF
!
! --------- Compute the B-spline expansion for surface pressure
!
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD(J9)%N_TIM, &
     &                          SPD(J9)%TIM_ARR, &
     &                          SPD(J9)%SUR_PWP, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3821, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &               'an attempt to compute B-spline expansion for the '// &
     &               'surface atmosperic pressire at station '// &
     &                C_STA(J9) )
                 RETURN 
            END IF
!
! --------- Compute the B-spline expansion for surface temperature
!
            CALL ERR_PASS ( IUER, IER )
            CALL BSPL4_1D_CMP ( SPD__MDEG, 0, SPD(J9)%N_TIM, &
     &                          SPD(J9)%TIM_ARR, &
     &                          SPD(J9)%SUR_TEM, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3822, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &               'an attempt to compute B-spline expansion for the '// &
     &               'surface air temperature at station '//C_STA(J9) )
                 RETURN 
            END IF
!
            SPD(J9)%ZEN_BIAS  = 0.0D0
            SPD(J9)%ZEN_SCALE = 1.0D0
            SPD(J9)%STATUS = SPD__READ
 4100    CONTINUE 
!
         IF ( APD_NAME == SPD__NMFW_STR .OR. &
     &        APD_NAME == SPD__NMFH_STR .OR. &
     &        APD_NAME == SPD__GL_STR        ) THEN
!
              CALL ERR_PASS ( IUER, IER )
              CALL SPD_LOAD_MF ( SPD, L_STA, J9, APD_NAME, LAYER_HEIGHT, &
     &                           LAYER_FWHM, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 3823, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &                 'an attempt to load mapping function' )
                   RETURN 
              END IF
            ELSE IF ( APD_NAME  == SPD__TOTS_STR ) THEN
              SPD(J9)%MF%MF_NAME = SPD__TOTS_STR
              SPD(J9)%MF%STATUS  = SPD__INIT
            ELSE IF ( APD_NAME  == SPD__WATS_STR ) THEN
              SPD(J9)%MF%MF_NAME = SPD__WATS_STR
              SPD(J9)%MF%STATUS  = SPD__INIT
            ELSE 
              CALL ERR_LOG ( 3824, IUER, 'SPD_LOAD_BSPD', 'Unsupported '// &
     &            'model of partial derivativies with respect to '// &
     &            'atmosphere path delay in zenith direction: '// &
     &             APD_NAME(1:I_LEN(APD_NAME))//'.  Supported '// &
     &            ' models: GAUSSIAN_LAYER, WATER_SCALE, TOTAL_WATER, '// &
     &            'NMFW, or NMFH' )
              RETURN 
         END IF
 490  CONTINUE 
!
      IF ( ILEN(SPD_BIAS_FILE) > 0  .AND.  SPD_BIAS_FILE .NE. 'NONE' ) THEN
!
! -------- Now read the the file with SPD biases.
! -------- At the moment ( 2014.07.14 ) SPD biases are not supported
!
           M_FIL = 16384
!
! -------- Allocate memory for a temporary buffer
!
           ALLOCATE ( BUF(M_FIL), STAT=IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( M_FIL, STR )
                CALL ERR_LOG ( 3825, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &            'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'of dynamic memory for SPD path delay biases' )
                RETURN 
           END IF
!
! -------- Read the file with SPD biases
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT ( SPD_BIAS_FILE, M_FIL, BUF, L_FIL, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 3826, IUER, 'SPD_LOAD_BSPD', 'Error in '// &
     &            'an attempt to read the buffer with the SPD path delay '// &
     &            'biases' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
!
! -------- Check the header and trailing labels
!
           IF ( BUF(1) .NE. SPD_3D_BIAS__LABEL ) THEN
                CALL CLRCH ( STR )
                CALL TRAN  ( 13, BUF(1), STR )
                CALL ERR_LOG ( 3827, IUER, 'SPD_LOAD_BSPD', 'Unrecogized '// &
     &              'format of the the SPD path delay bias file '// &
     &               SPD_BIAS_FILE(1:I_LEN(SPD_BIAS_FILE))// &
     &              ' : the first line is '//STR(1:I_LEN(STR))//' while '// &
     &              'the label '//SPD_3D_BIAS__LABEL//' was expected' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
           IF ( BUF(L_FIL) .NE. SPD_3D_BIAS__LABEL ) THEN
                CALL ERR_LOG ( 3828, IUER, 'SPD_LOAD_BSPD', 'The SPD path '// &
     &              'delay bias file '// &
     &               SPD_BIAS_FILE(1:I_LEN(SPD_BIAS_FILE))// &
     &              ' : was not read up to the end: the last line is not '// &
     &              'the label '//SPD_3D_BIAS__LABEL//' as was expected' )
                DEALLOCATE ( BUF )
                RETURN 
           END IF
!
! -------- Parse the file with SPD biases
!
           DO 4120 J12=1,L_FIL
              IF ( BUF(J12)(1:1) == 'B' ) THEN
!
! ---------------- Search whether this station will be used
!
                   IND_STA = LTM_DIF ( 1, L_STA, C_STA, BUF(J12)(12:19) )
                   IF ( IND_STA > 0 ) THEN
!
! --------------------- Decode the value of the bias
!
                        READ ( UNIT=BUF(J12)(23:34), FMT='(1PD12.5)', IOSTAT=IER ) &
     &                         SPD(IND_STA)%ZEN_BIAS 
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J12, STR )
                             CALL ERR_LOG ( 3829, IUER, 'SPD_LOAD_BSPD', &
     &                           'Error in decoding SPD path delay bias '// &
     &                           'at line '//STR(1:I_LEN(STR))// &
     &                           ' of the file '// &
     &                           SPD_BIAS_FILE(1:I_LEN(SPD_BIAS_FILE))// &
     &                           ' : '//BUF(J12)(23:34) )
                             DEALLOCATE ( BUF )
                             RETURN 
                        END IF
!
                        READ ( UNIT=BUF(J12)(38:44), FMT='(F7.4)', IOSTAT=IER ) &
     &                         SPD(IND_STA)%ZEN_SCALE
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( J12, STR )
                             CALL ERR_LOG ( 3830, IUER, 'SPD_LOAD_BSPD', &
     &                           'Error in decoding SPD path delay scale '// &
     &                           'at line '//STR(1:I_LEN(STR))// &
     &                           ' of the file '// &
     &                           SPD_BIAS_FILE(1:I_LEN(SPD_BIAS_FILE))// &
     &                           ' : '//BUF(J12)(23:34) )
                             DEALLOCATE ( BUF )
                             RETURN 
                        END IF
                   END IF
              END IF
 4120      CONTINUE 
!
! -------- Deallocate temporary buffer
!
           DEALLOCATE ( BUF )
      END IF
!
      DEALLOCATE ( FIL_SPD )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE SPD_LOAD_BSPD  !#!#
