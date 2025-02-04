      PROGRAM    SPD_SHOW
! ************************************************************************
! *                                                                      *
! *   Program SPD_SHOW
! *                                                                      *
! *  ###  11-JUL-2014    SPD_SHOW   v2.1 (c) L. Petrov  24-AUG-2014  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  M_OBS, M_STA 
      PARAMETER  ( M_OBS = 128*1024 )
      PARAMETER  ( M_STA =     1024 )
      TYPE     ( SPD_DEL__TYPE ) :: SPD(M_STA)
      INTEGER*4  L_STA, MJD_BEG, MJD_END, MJD_OBS(M_OBS), &
     &           L_OBS, IND_STA(M_OBS), J1, J2, IUER
      REAL*8     TAI_BEG, TAI_END, TAI_OBS(M_OBS), AZ(M_OBS), EL(M_OBS), &
     &           AZ_RATE(M_OBS), EL_RATE(M_OBS), APD_PAR1, APD_PAR2
      REAL*8     SPD_DELAY, SPD_RATE, SPD_DELAY_DER_ZEN, SPD_RATE_DER_ZEN
      CHARACTER  BSPD_DIR*128, C_STA(M_STA)*8, SPD_BIAS_FILE*128, &
     &           APD_NAME*16, FIL*128, STR*128
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      SPD_BIAS_FILE = 'NONE'
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: spd_show_delay control_file'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FIL )
      END IF
!
! --- Parse the input file
!
      IUER = -1
      CALL SHS_PARSE ( FIL, M_STA, M_OBS, BSPD_DIR, APD_NAME, &
     &                 APD_PAR1, APD_PAR2, L_STA, C_STA, MJD_BEG, &
     &                 TAI_BEG, MJD_END, TAI_END, L_OBS, MJD_OBS, TAI_OBS, &
     &                 IND_STA, AZ, EL, AZ_RATE, EL_RATE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4001, IUER, 'SPD_SHOW', 'Error in parsing '// &
     &         'input file '//FIL )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Load file binary displacements, perform B-spline expansion 
! --- slant path delay for the the list of stations L_STA/C_STA
! --- in the interval of dates [MJD_BEG,TAI_BEG -- MJD_END,TAI_END]
!
      IUER = -1
      CALL SPD_LOAD_BSPD ( 1, BSPD_DIR, SPD_BIAS_FILE, APD_NAME, &
     &                     APD_PAR1, APD_PAR2, L_STA, C_STA, &
     &                     MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                     SPD, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4002, IUER, 'SPD_SHOW', 'Error in loading '// &
     &         'data with slant path delay expansion coefficients '// &
     &         'from directory '//BSPD_DIR )
           CALL EXIT ( 1 ) 
      END IF
!
      DO 410 J1=1,L_OBS
!
! ------ Compute path delay, path delay rate and its rate of change
!
         IUER = -1
         CALL SPD_INTRP_DELAY ( C_STA(IND_STA(J1)), L_STA, SPD, EL(J1), AZ(J1), &
     &                          EL_RATE(J1), AZ_RATE(J1), MJD_OBS(J1), TAI_OBS(J1), &
     &                          SPD_DELAY, SPD_RATE, SPD_DELAY_DER_ZEN, &
     &                          SPD_RATE_DER_ZEN, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 4002, IUER, 'SPD_SHOW', 'Error in computing '// &
     &            'slant path delay for station '//C_STA(IND_STA(J1)) )
              CALL EXIT ( 1 ) 
         END IF
         IUER = -1
         STR = MJDSEC_TO_DATE ( MJD_OBS(J1), TAI_OBS(J1), IUER )
         WRITE ( 6, 120 ) STR(1:24), C_STA(IND_STA(J1)), &
     &                    AZ(J1)/DEG__TO__RAD, EL(J1)/DEG__TO__RAD, &
     &                    SPD_DELAY, SPD_RATE, SPD_DELAY_DER_ZEN, SPD_RATE_DER_ZEN, &
     &                    APD_NAME
 120     FORMAT ( A, 1X, A, ' Az: ', F9.5, ' El: ', F7.4, ' deg  ', &
     &            'Delay: ', 1PD12.5, ' sec Rate: ', 1PD12.5, &
     &            ' Delay_der_zen: ', 0PF8.5,' Rate_der_zen: ', 1PD12.5, &
     &            ' Partial_derivative_type: ', A )
 410  CONTINUE 
!
      DO 420 J2=1,L_STA
         CALL SPD_DEL_QUIT ( SPD(J2), IUER )
 420  CONTINUE 
!
      CALL EXIT ( 0 )
      END  PROGRAM  SPD_SHOW  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SHS_PARSE ( FIL, M_STA, M_OBS, BSPD_DIR, APD_NAME, &
     &                       APD_PAR1, APD_PAR2, L_STA, C_STA, &
     &                       MJD_BEG, TAI_BEG, MJD_END, TAI_END, L_OBS, &
     &                       MJD_OBS, TAI_OBS, IND_STA, AZ, EL, AZ_RATE, &
     &                       EL_RATE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SHS_PARSE  parses input file FIL in SFS format.            *
! *   It extracts the following parameters:                              *
! *                                                                      *
! *   1) Name of the directory with binary slant path delays BSPD_DIR    *
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
! * BSPD_DIR ( CHARACTER ) -- Name of the directory with slant path      *
! *                           delay in BSPD format.                      *
! * APD_NAME ( CHARACTER ) -- Model of partial derivativies with respect *
! *                           to atmosphere path delay in zenith         *
! *                           direction. Supported models:               *
! *                   NMFW -- Niell (1996) mapping function (wet).       *
! *                   NMFH -- Niell (1996) mapping function              *
! *                           (hydrostatic) on the mean epoch.           *
! *            TOTAL_SCALE -- mapping function for the case when         *
! *                           residual atmosphere is considered          *
! *                           proportional to the total atmosphere.      *
! *                           The partial derivative is defined as       *
! *                           a ratio of the total slant path delay to   *
! *                           the total path delay in the zenith         *
! *                           direction.                                 *
! *            WATER_SCALE -- mapping function for the case when         *
! *                           residual atmosphere is considered          *
! *                           proportional to the water vapor            *
! *                           contribution to the atmosphere.            *
! *                           The partial derivative is defined as a     *
! *                           ratio of the water vapor contribution of   *
! *                           slant path delay to the water vapor        *
! *                           contribution to path delay in the zenith   *
! *                           direction.                                 *
! *         GAUSSIAN_LAYER -- mapping function for the case when the     *
! *                           dependence of concentration of the         *
! *                           residual atmosphere is described with      *
! *                           the Gaussian model with the specified      *
! *                           height and the specified full width half   *
! *                           maximum (FWHM).                            *
! * APD_PAR1 ( REAL*8    ) -- If the partial derivative model is         *
! *                           GAUSSIAN_LAYER, then APD_PAR1 is the layer *
! *                           height in meters. Otherwise, it is zero.   *
! * APD_PAR2 ( REAL*8    ) -- If the partial derivative model is         *
! *                           GAUSSIAN_LAYER, then APD_PAR1 is the layer *
! *                           FWHM in meters. Otherwise, it is zero.     *
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
! *  AZ_RATE ( REAL*8    ) -- Array of azimuth rate of changes for       *
! *                           a given observation. Unit: rad/s.          *
! *                           Dimension: L_OBS.                          *
! *  EL_RATE ( REAL*8    ) -- Array of elevation angle rate for a given  *
! *                           observation. Unit: rad/s.                  *
! *                           Dimension: L_OBS.                          *
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
! *  ### 11-JUL-2014   SHS_PARSE   v2.0 (c)  L. Petrov  19-AUG-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INTEGER*4  M_OBS, M_STA, L_STA, MJD_BEG, MJD_END, MJD_OBS(M_OBS), &
     &           L_OBS, IND_STA(M_OBS), IUER
      CHARACTER  FIL*(*), BSPD_DIR*(*), C_STA(M_STA)*(*), APD_NAME*(*)
      REAL*8     TAI_BEG, TAI_END, TAI_OBS(M_OBS), AZ(M_OBS), EL(M_OBS), &
     &           AZ_RATE(M_OBS), EL_RATE(M_OBS), APD_PAR1, APD_PAR2
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
           CALL ERR_LOG ( 4101, IUER, 'SHS_PARSE', 'Error in an attempt '// &
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
           CALL ERR_LOG ( 4102, IUER, 'SHS_PARSE', 'Error in an attempt '// &
     &         'to read input configuration file '//FIL )
           RETURN 
      END IF
!
! --- Check the first line of the file. It should have format label
!
      IF ( BUF(1)(1:LEN(SPD_SHOW__LABEL)) .NE. SPD_SHOW__LABEL ) THEN
           STR = BUF(1)
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 4103, IUER, 'SHS_PARSE', 'Unsupported format '// &
     &         'of input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &         'its first line is '//STR(1:I_LEN(STR))//' while '// &
     &         'the file label '//SPD_SHOW__LABEL//' was expected' )
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
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'BSPD_DIR:' ) THEN
              BSPD_DIR = BUF(J1)(IND(1,2):IND(2,2)) 
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'ATMOSPHERE_PATH_DELAY_PARTIAL:' ) THEN
              IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NONE' ) THEN
                   APD_NAME = SPD__NONE_STR
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NMFW' ) THEN
                   APD_NAME = SPD__NMFW_STR
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'NMFH' ) THEN
                   APD_NAME = SPD__NMFH_STR
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'TOTAL_SCALE' ) THEN
                   APD_NAME = SPD__TOTS_STR
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'WATER_SCALE' ) THEN
                   APD_NAME = SPD__WATS_STR
                 ELSE IF ( BUF(J1)(IND(1,2):IND(2,2)) .EQ. 'GAUSSIAN_LAYER' ) THEN
                   APD_NAME = SPD__GL_STR
                   IF ( LIND < 4 ) THEN
                        CALL ERR_LOG ( 4104, IUER, 'SHS_PARSE', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '//FIL(1:I_LEN(FIL))// &
     &                      ' : three qualifiers after the keyword '// &
     &                      'ATMOSPHERE_PATH_DELAY_PARTIAL were expected' )
                        RETURN
                   END IF
!
                   READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F12.6)', &
     &                    IOSTAT=IER ) APD_PAR1 
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4105, IUER, 'SHS_PARSE', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '//FIL(1:I_LEN(FIL))// &
     &                      ' : failure in parsing the 3rd qualifier '// &
     &                      BUF(J1)(IND(1,3):IND(2,3))//' -- the layer height '// &
     &                      'in meters. A real number was expected' )
                        RETURN
                   END IF
!
                   READ ( UNIT=BUF(J1)(IND(1,3):IND(2,3)), FMT='(F12.6)', &
     &                    IOSTAT=IER ) APD_PAR2
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4106, IUER, 'SHS_PARSE', 'Error in '// &
     &                      'parsing the '//STR(1:I_LEN(STR))//'-th line '// &
     &                      'in the configuration file '//FIL(1:I_LEN(FIL))// &
     &                      ' : failure in parsing the 4th qualifier '// &
     &                      BUF(J1)(IND(1,3):IND(2,3))//' -- the layer FWHM '// &
     &                      'thickness in meters. A real number was expected' )
                        RETURN
                   END IF
              END IF
            ELSE IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'DATE_TAI_BEG:' ) THEN
              IER = IUER 
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,2):IND(2,2)), MJD_BEG, TAI_BEG, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 4107, IUER, 'SHS_PARSE', 'Failure in processing '// &
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
                   CALL ERR_LOG ( 4108, IUER, 'SHS_PARSE', 'Failure in processing '// &
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
                   CALL ERR_LOG ( 4109, IUER, 'SHS_PARSE', 'Failure in '// &
     &                 'processing the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- to many observations, more than '//STR1 )
                   RETURN 
              END IF
              IF ( LIND < 9 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL CLRCH ( STR1 )
                   CALL INCH  ( LIND, STR1 )
                   CALL ERR_LOG ( 4110, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- to few words, less than than '//STR1 )
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
                   CALL ERR_LOG ( 4111, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- to many stations, more than '//STR1 )
                   RETURN 
              END IF
            ELSE 
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 4112, IUER, 'SHS_PARSE', 'Unsupported record '// &
     &            'definition at the '//STR(1:I_LEN(STR))//' th line of '// &
     &            ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &            BUF(J1)(IND(1,1):IND(2,1))//' . Supported words: '// &
     &            ' BSPD_DIR, DATE_TAI_BEG, DATE_TAI_END, OBS' )
              RETURN 
         END IF
 410  CONTINUE 
!
! --- The second run. Parse oservation records
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
                   CALL ERR_LOG ( 4113, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse date '//BUF(J2)(IND(1,3):IND(2,3))// &
     &                 ' NB: supported format: YYYY.MM.DD_hh:mm:ss.s' )
                   RETURN 
              END IF
!
! ----------- Get AZ,  EL
!
              READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F10.5)', IOSTAT=IER ) AZ(L_OBS)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4114, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse azimuth '//BUF(J2)(IND(1,4):IND(2,4))// &
     &                 ' -- a real number was expected' )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J2)(IND(1,5):IND(2,5)), FMT='(F10.5)', IOSTAT=IER ) EL(L_OBS)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4115, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse elevation '//BUF(J2)(IND(1,5):IND(2,5))// &
     &                 ' -- a real number was expected' )
                   RETURN 
              END IF
!
! ----------- Units ftansform if needed
!
              IF ( BUF(J2)(IND(1,6):IND(2,6))  == 'deg' ) THEN
                   AZ(L_OBS) = AZ(L_OBS)*DEG__TO__RAD
                   EL(L_OBS) = EL(L_OBS)*DEG__TO__RAD
                ELSE IF ( BUF(J2)(IND(1,6):IND(2,6))  == 'rad' ) THEN
                   CONTINUE 
                ELSE
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4116, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse the 6th word '//BUF(J2)(IND(1,6):IND(2,6))// &
     &                 ' -- rad or deg were expected' )
                   RETURN 
              END IF
!
! ----------- Get AZ_rate, EL_rate
!
              READ ( UNIT=BUF(J2)(IND(1,7):IND(2,7)), FMT='(F10.5)', IOSTAT=IER ) AZ_RATE(L_OBS)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4117, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse azimuth rate '//BUF(J2)(IND(1,7):IND(2,7))// &
     &                 ' -- a real number was expected' )
                   RETURN 
              END IF
              READ ( UNIT=BUF(J2)(IND(1,8):IND(2,8)), FMT='(F10.5)', IOSTAT=IER ) EL_RATE(L_OBS)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4118, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse elevation rate '//BUF(J2)(IND(1,8):IND(2,8))// &
     &                 ' -- a real number was expected' )
                   RETURN 
              END IF
!
! ----------- Perform unit transform, if needed
!
              IF ( BUF(J2)(IND(1,9):IND(2,9))  == 'deg/s' ) THEN
                   AZ_RATE(L_OBS) = AZ_RATE(L_OBS)*DEG__TO__RAD
                   EL_RATE(L_OBS) = EL_RATE(L_OBS)*DEG__TO__RAD
                ELSE IF ( BUF(J2)(IND(1,9):IND(2,9))  == 'rad/s' ) THEN
                   CONTINUE 
                ELSE
                   CALL CLRCH ( STR )
                   CALL INCH  ( J2, STR )
                   CALL ERR_LOG ( 4119, IUER, 'SHS_PARSE', 'Failure in processing '// &
     &                 'the '//STR(1:I_LEN(STR))//' th line of '// &
     &                 ' the input file '//FIL(1:I_LEN(FIL))//' -- '// &
     &                 ' -- cannot parse the 9th word '//BUF(J2)(IND(1,5):IND(2,5))// &
     &                 ' -- rad/s or deg/s were expected' )
                   RETURN 
              END IF
         END IF
 420  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SHS_PARSE  !#!  
