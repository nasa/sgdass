      SUBROUTINE GEN_MAPPLOT ( MAP, IDEV, ICLR, IMA_WIN, SIGLEV, ISIZE, &
     &                         IBEAM, ITHICK, FILIN, FILOUT, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine  GEN_MAPPLOT  generates the contour map of the image       *
! *   produced from analysis of VLBI observations. The first contour     *
! *   level is selected as the image rms noise multiplied by some        *
! *   factor. The contour levels sequence is the geometric progression   *
! *   with the base of 2.                                                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MAP ( MAP__TYPE ) -- object which keeps variables related to the  *
! *                         source image.                                *
! *   IDEV ( INTEGER*4 ) -- Device code:                                 *
! *                         IDEV == 1 -- the plot will be printed        *
! *                                      on the screen.                  *
! *                         IDEV == 2 -- the plot will be in Postscript  *
! *                                      format.                         *
! *                         IDEV == 3 -- the plot will be in gif format. *
! *                         IDEV == 4 -- the plot will be in ong format. *
! *                         IDEV == 5 -- no plot: a table is put in the  *
! *                                      output file.                    *
! *   ICLR ( INTEGER*4 ) -- color index according to DiaGI.              *
! *                         Valid range -1, 0, or [1, 64].               *
! *                         Color index -1 indicates that the color of   *
! *                         the plot will be selected in accordance      *
! *                         with the observing frequency.                *
! *                         Color index 0 means black.                   *
! * IMA_WIN ( REAL*8   ) -- This parameter determines the area of the    *
! *                         map to be printed.                           *
! *                         IMA_WIN == 0 -- to print the entire map.     *
! *                         IMA_WIN > 0  -- determines the size of the   *
! *                                         image coordinate box in      *
! *                                         mas -- the distance from     *
! *                                         the center to the edge of    *
! *                                         the map.                     *
! *                         IMA_WIN = -1.0  The image size is determined *
! *                                         automatically in order to    *
! *                                         show all details with        *
! *                                         amplitude the greater than   *
! *                                         SIGLEV*Noise.                *
! *                         IMA_WIN = -2.0  The image size will be set   *
! *                                         to the value which depends   *
! *                                         only on frequency. However,  *
! *                                         if there are details in the  *
! *                                         image with amplitude >       *
! *                                         SIGLEV*Noise beyond that     *
! *                                         predefined window, the       *
! *                                         window will be increased in  *
! *                                         order to show these details. *
! * SIGLEV ( REAL*8    ) -- The factor used for computing the first      *
! *                         contour level. The first level is determine  *
! *                         as  the product of this factor and the rms   *
! *                         image noise.                                 *
! *  ISIZE ( INTEGER*4 ) -- size code:                                   *
! *                         0 -- 44x50 mm, large font                    *
! *                         1 -- 45x45 mm                                *
! *                         2 -- 80x80 mm                                *
! *                         3 -- 160x160 mm                              *
! *                         4 -- 270x270 mm                              *
! *                         5 -- 360x360 mm                              *
! *  IBEAM ( INTEGER*4 ) -- Code which determines the way who the image  *
! *                         is displayed.                                *
! *                         IBEAM = 0 -- image is drawn using the pixel  *
! *                                      matrix stored in the fits-file. *
! *                         IBEAM = 1 -- image is drawn using sequence   *
! *                                      of delta functions convolved    *
! *                                      with the circular beam with     *
! *                                      the width equal to the          *
! *                                      semi-minor axes of the clean    *
! *                                      beam stored in the fits-file.   *
! *                         IBEAM = 11 - The same as BEAM = 1, but       *
! *                                      an additional vertical line and *
! *                                      smal horizontal lines are drawn.*
! *                         IBEAM = 2 -- image is drawn using sequence   *
! *                                      of delta functions convolved    *
! *                                      with the circular beam with     *
! *                                      the width equal to the          *
! *                                      semi-major axes of the clean    *
! *                                      beam stored in the fits-file.   *
! *                         IBEAM = 3 -- image is drawn using sequence   *
! *                                      of delta functions convolved    *
! *                                      with the beam stored in the     *
! *                                      fits-file.                      *
! *                         IBEAM = 5 -- image is drawn using sequence   *
! * ITHICK ( INTEGER*4 ) -- Thickness of countors as an integer number.  *
! * FILIN  ( CHARACTER ) -- Name of the input file with map.             *
! * FILOUT ( CHARACTER ) -- Name of the output file.                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! *  ### 26-FEB-2004  GEN_MAPPLOT   v3.0 (c) L. Petrov  13-OCT-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'diagi.i'
      INCLUDE    'sou_map.i'
      TYPE     ( SOUMAP__TYPE ) :: MAP
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  FILIN*(*), FILOUT*(*)
      INTEGER*4  IDEV, ICLR, ISIZE, IBEAM, ITHICK, IUER
      REAL*8     IMA_WIN, SIGLEV
      INTEGER*4  MC
      PARAMETER  ( MC = 64 ) 
      REAL*4     LEV_CONT(MC), XC, YC, AX_LEN, ASP, SC, PHI, LAM, PAP_SIZE
      REAL*4     XP, YP, X_MAR, X_BL, X_TR, Y_BL, Y_TR, TR(6), TICK_STEP, &
     &           X2(2), Y2(2)
      REAL*8     XC_BEAM, YC_BEAM, ARG, AX_FACTOR, BD_FACTOR, &
     &           BD_FACTOR_DEF, BEAM_SIZE 
      PARAMETER  ( BD_FACTOR_DEF = 5.0 )
      CHARACTER  CH*1, STR*256, COM_STR*256, BAND*1
      INTEGER*4  ID_XW, ISG, J1, J2, J3, J4, J5, J6, IL, NC, GREY_COL, &
     &           ICLR_BAND, QUAL_CODE, LUN, IER
      CHARACTER  OUT*4096
      REAL*4,    ALLOCATABLE :: IMAGE(:,:)
      REAL*4      PI, PI2, P2I, RAD_TO_MAS
      PARAMETER ( PI=3.1415926, PI2=2.0*PI, P2I=PI/2.0 ) ! Pi number
      PARAMETER ( RAD_TO_MAS = 180.0*3600.0*1000.0/PI )
      PARAMETER ( AX_FACTOR  = 1.25D0 )
      LOGICAL*4   FL_RA_POS, FL_CM
      REAL*4      X_DAT_SHIFT, Y_HEA_FACTOR, X_HEA_CENTER, XFREQ_CENTER, &
     &            Y_RA_FACTOR, Y_BEAM_FACTOR, Y_LEVEL_FACTOR, JET_ANG_R4
      REAL*4      CX, CY, MX, MY, FT, FM, PA
      LOGICAL*4   FL_ANG_UPDATE 
      REAL*8      IMA_WIN_RAD, JET_ANG_R8
      REAL*4,     EXTERNAL :: ATAN_CS_R4
      CHARACTER,  EXTERNAL :: MJDSEC_TO_DATE*30, GET_BAND*1
      INTEGER*4,  EXTERNAL :: I_LEN, ILEN, PGOPEN
      LOGICAL,    EXTERNAL :: PGNOTO
!
! --- Flag: if .TRUE., than the right ascension increases from the left to 
! --- right at the plot
!
      FL_RA_POS = .FALSE.
      FL_CM = .FALSE.
      CALL GETENVAR ( 'FITS_MAP_CM_PRINT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) FL_CM = .TRUE.
      CALL GETENVAR ( 'FITS_MAP_BD_FACTOR', STR )
      IF ( ILEN(STR) == 0 ) THEN
           BD_FACTOR = BD_FACTOR_DEF
         ELSE
           READ ( UNIT=STR, FMT=* ) BD_FACTOR
      END IF
      CX = 0.0
      CY = 0.0
      MX = 0.0
      MY = 0.0
      FT = 0.0
      FM = -100.0
!
      IF ( IBEAM == 12  .OR.  IBEAM == 13 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL IO_JET_ANG ( 1, FILIN, JET_ANG_R8, IMA_WIN_RAD, QUAL_CODE, IER ) 
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6262, IUER, 'GEN_MAPPLOT', 'Error in '// &
     &              'an attempt to get jet direction for the sources '// &
     &              'related to the file '//FILOUT )
                RETURN
           END IF
           JET_ANG_R4 = JET_ANG_R8
           IMA_WIN = IMA_WIN_RAD*RAD_TO_MAS
         ELSE 
           JET_ANG_R4 = 0.0
           QUAL_CODE = 0
      END IF
!
! --- Setting plotting parameters
!
      CALL DIAGI_SET ( 1, DIAGI_S )
      GREY_COL = 127
!
! --- Openning the plotting device
!
      IF ( IDEV .EQ. 1 ) THEN
           ID_XW = PGOPEN ( DIAGI_S%DEVICE )
         ELSE IF ( IDEV == 2 ) THEN
           ID_XW = PGOPEN ( FILOUT(1:I_LEN(FILOUT))//'/VCPS' )
         ELSE IF ( IDEV == 3 ) THEN
           ID_XW = PGOPEN ( FILOUT(1:I_LEN(FILOUT))//'/GIF' )
         ELSE IF ( IDEV == 4 ) THEN
           ID_XW = PGOPEN ( FILOUT(1:I_LEN(FILOUT))//'/PNG' )
      END IF
      IF ( ID_XW .LE. 0 ) THEN
           CALL CLRCH   (        STR )
           CALL INCH    ( ID_XW, STR )
           CALL ERR_LOG ( 6261, IUER, 'GEN_MAPPLOT', 'Error in openning '// &
     &         'the graphic device '//DIAGI_S%DEVICE//' IER='//STR )
           RETURN
      END IF
!
! --- Get the band name
!
      BAND = GET_BAND ( MAP%FREQ )
      IF ( BAND == 'W' ) THEN
           ICLR_BAND = 10
        ELSE IF ( BAND == 'Q' ) THEN
           ICLR_BAND = 7
        ELSE IF ( BAND == 'A' ) THEN
           ICLR_BAND = 8
        ELSE IF ( BAND == 'K' ) THEN
           ICLR_BAND = 5
        ELSE IF ( BAND == 'U' ) THEN
           ICLR_BAND = 9
        ELSE IF ( BAND == 'X' ) THEN
           ICLR_BAND = 1
        ELSE IF ( BAND == 'C' ) THEN
           ICLR_BAND = 3
        ELSE IF ( BAND == 'S' ) THEN
           ICLR_BAND = 2
        ELSE IF ( BAND == 'L' ) THEN
           ICLR_BAND = 6
        ELSE IF ( BAND == '?' ) THEN
           ICLR_BAND = 13
      END IF
!
! --- Setting colours
!
      DIAGI_S%NCLR  = 1
      IF ( ICLR .EQ. -1 ) THEN
!
! -------- the color is determined by the sky frequency of the image
!
           DIAGI_S%ICOL(1) = ICLR_BAND
         ELSE 
           DIAGI_S%ICOL(1) = ICLR
      END IF
      IF ( DIAGI_S%ICOL(1) .LE. 0 .OR. DIAGI_S%ICOL(1) > MCLR ) THEN
!
! -------- Default color
!
           DIAGI_S%ICOL(1) = 13 ! Default color
      END IF
      CALL DIAGI_CLS ( DIAGI_S, IER )
      IF ( IDEV .NE. 1 ) THEN
           CALL PGCOL_RGB ( 0, 256, 256, 256 )
      END IF
      CALL PGCOL_RGB ( GREY_COL, 128, 128, 128 )
      CALL PGSCI ( 1 )
!
! --- Setting default font type
!
      CALL PGSCF  ( 2 )
!
! --- Setting new paper size
!
      IF ( ISIZE == 0 ) THEN
           PAP_SIZE = 50.0
         ELSE IF ( ISIZE == 1 ) THEN
           PAP_SIZE = 50.0
         ELSE IF ( ISIZE == 2 ) THEN
           PAP_SIZE = 100.0
         ELSE IF ( ISIZE == 3 ) THEN
           PAP_SIZE = 176.0
         ELSE IF ( ISIZE == 4 ) THEN
           PAP_SIZE = 270.0
         ELSE IF ( ISIZE == 5 ) THEN
           PAP_SIZE = 360.0
      END IF
      IF ( IDEV == 1 ) THEN
           CALL PGERAS()       ! Erase the screen
      END IF
!
! --- Compute the image noise 
!
      CALL FIND_IMAGE_NOISE ( MAP )
 910  CONTINUE 
!
! --- Determine the world coordinates of the image
!
      IF ( DABS(IMA_WIN) < 1.E-6 ) THEN
           AX_LEN = ABS(RAD_TO_MAS*MAP%STEP_RA*MAP%DIM2/2)
         ELSE IF ( IMA_WIN > 1.E-6 ) THEN
           AX_LEN = IMA_WIN
         ELSE IF ( DABS(IMA_WIN + 1.0D0) < 1.E-4  .OR.  &
     &             DABS(IMA_WIN + 2.0D0) < 1.E-4  ) THEN
           AX_LEN = 0.0
           DO 410 J1=1,MAP%DIM1
              XC = MAP%STEP_RA*(J1 - MAP%DIM1/2)
              DO 420 J2=1,MAP%DIM2 
                 YC = MAP%STEP_DL*(J2 - MAP%DIM2/2)
                 IF ( MAP%IMAGE(J1,J2)**2 > (SIGLEV*MAP%NOISE)**2  ) THEN
                      AX_LEN = MAX ( AX_LEN, ABS(XC) )
                      AX_LEN = MAX ( AX_LEN, ABS(YC) )
                 END IF
 420          CONTINUE 
 410       CONTINUE 
           AX_LEN = RAD_TO_MAS*AX_LEN
           IF ( AX_LEN < 1.0E-6 ) AX_LEN = ABS(RAD_TO_MAS*MAP%STEP_RA*MAP%DIM2/2)
           IF ( AX_LEN*AX_FACTOR < ABS(RAD_TO_MAS*MAP%STEP_RA*MAP%DIM2/2) ) THEN
                AX_LEN = AX_LEN*AX_FACTOR
           END IF
           IF ( DABS(IMA_WIN + 2.0D0) < 1.E-4 ) THEN
                IF ( BAND == 'W' ) THEN
                     IF ( AX_LEN <  1.5 ) AX_LEN =  1.5
                  ELSE IF ( BAND == 'Q' ) THEN
                     IF ( AX_LEN <  3.0 ) AX_LEN =  3.0
                  ELSE IF ( BAND == 'A' ) THEN
                     IF ( AX_LEN <  5.0 ) AX_LEN =  5.0
                  ELSE IF ( BAND == 'K' ) THEN
                     IF ( AX_LEN <  5.0 ) AX_LEN =  5.0
                  ELSE IF ( BAND == 'U' ) THEN
                     IF ( AX_LEN <  8.0 ) AX_LEN =  8.0
                  ELSE IF ( BAND == 'X' ) THEN
                     IF ( AX_LEN < 15.0 ) AX_LEN = 15.0
                  ELSE IF ( BAND == 'C' ) THEN
                     IF ( AX_LEN < 30.0 ) AX_LEN = 30.0
                  ELSE IF ( BAND == 'S' ) THEN
                     IF ( AX_LEN < 50.0 ) AX_LEN = 50.0
                  ELSE IF ( BAND == 'L' ) THEN
                     IF ( AX_LEN < 100.0 ) AX_LEN = 100.0
                  ELSE IF ( BAND == '?' ) THEN
                     IF ( AX_LEN < 100.0 ) AX_LEN = ABS(RAD_TO_MAS*MAP%STEP_RA*MAP%DIM2/2)
                END IF
           END IF
         ELSE 
           AX_LEN = ABS(RAD_TO_MAS*MAP%STEP_RA*MAP%DIM2/2)
      END IF
!
! --- Deterimne the window world coordinates
!
      X_BL =  AX_LEN
      X_TR = -AX_LEN
      IF ( FL_RA_POS ) THEN
           X_BL = -AX_LEN
           X_TR =  AX_LEN
      END IF
      Y_BL = -AX_LEN
      Y_TR =  AX_LEN
!
! --- Determine the size of the tick step
!
      TICK_STEP = 1.0*NINT( AX_LEN/2 )
      IF ( TICK_STEP < 0.1 ) THEN
           CONTINUE 
         ELSE IF ( TICK_STEP < 0.5 ) THEN
           TICK_STEP = 0.1
         ELSE IF ( TICK_STEP < 1.0 ) THEN
           TICK_STEP = 0.2
         ELSE IF ( TICK_STEP < 2.0 ) THEN
           TICK_STEP = 0.5
         ELSE IF ( TICK_STEP < 5.0 ) THEN
           TICK_STEP = 2.0
         ELSE IF ( TICK_STEP < 10.0 ) THEN
           TICK_STEP = 5.0
         ELSE IF ( TICK_STEP < 15.0 ) THEN
           TICK_STEP = 10.0
         ELSE IF ( TICK_STEP < 20.0 ) THEN
           TICK_STEP = 15.0
         ELSE IF ( TICK_STEP < 25.0 ) THEN
           TICK_STEP = 20.0
         ELSE IF ( TICK_STEP < 30.0 ) THEN
           TICK_STEP = 25.0
         ELSE IF ( TICK_STEP < 40.0 ) THEN
           TICK_STEP = 30.0
         ELSE IF ( TICK_STEP < 50.0 ) THEN
           TICK_STEP = 40.0
         ELSE IF ( TICK_STEP < 60.0 ) THEN
           TICK_STEP = 50.0
      END IF
!
! --- Set the first contour level 
!
      LEV_CONT(1) = SIGLEV*MAP%NOISE
!
! --- Set the size of the image
!
      IF ( ISIZE == 0 ) THEN
           CALL PGPAP  ( PAP_SIZE/25.4,  1.4 )
         ELSE
           CALL PGPAP  ( PAP_SIZE/25.4,  1.0 )
      END IF
      IF ( ISIZE == 0 ) THEN
           CALL PGSVP  (  0.20, 0.90,  0.40, 0.90 )
         ELSE IF ( ISIZE == 1 ) THEN
           CALL PGSVP  (  0.25, 0.90,  0.25, 0.90 )
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSVP  (  0.20, 0.90,  0.20, 0.90 )
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSVP  (  0.14, 0.94,  0.14, 0.94 )
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSVP  (  0.14, 0.94,  0.14, 0.94 )
         ELSE IF ( ISIZE == 5 ) THEN
           CALL PGSVP  (  0.18, 0.88,  0.18, 0.88 )
      END IF
!
! --- Set the world coordinates of the image
!
      CALL PGSWIN ( X_BL, X_TR, Y_BL, Y_TR )
!
      IF ( ISIZE == 0 ) THEN
           CALL PGSCH   ( 1.6 )
           CALL PGSLW   ( 1 )
         ELSE IF ( ISIZE == 1 ) THEN
           CALL PGSCH   ( 1.2 )
           CALL PGSLW   ( 1 )
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSCH   ( 1.1 )
           CALL PGSLW   ( 1 )
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSCH   ( 1.0 )
           CALL PGSLW   ( 1 )
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSCH   ( 1.0 )
           CALL PGSLW   ( 2 )
         ELSE IF ( ISIZE == 5 ) THEN
           CALL PGSCH   ( 1.0 )
           CALL PGSLW   ( 2 )
      END IF
!
! --- Set the coordiante box of the image
!
      CALL PGBOX  ( 'bicnts', TICK_STEP, 1, 'bicnts', TICK_STEP, 1 )
      IF ( ISIZE == 0 ) THEN
           CALL PGSCH   ( 1.6 )
           CALL PGSLW   ( 3 )
           X_DAT_SHIFT  =  0.20*X_BL
           X_HEA_CENTER =  0.55
           Y_HEA_FACTOR =  0.066
         ELSE IF ( ISIZE == 1 ) THEN
           CALL PGSCH   ( 1.6 )
           CALL PGSLW   ( 3 )
           X_DAT_SHIFT  =  0.25*X_BL
           X_HEA_CENTER =  0.55
           Y_HEA_FACTOR =  0.050
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSCH   ( 1.6 )
           CALL PGSLW   ( 4 )
           X_DAT_SHIFT  =  0.25*X_BL
           X_HEA_CENTER =  0.7
           Y_HEA_FACTOR =  0.040
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSCH   ( 1.4 )
           CALL PGSLW   ( 4 )
           X_DAT_SHIFT  = 0.20*X_BL
           X_HEA_CENTER = 0.75
           Y_HEA_FACTOR = 0.030
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSCH   ( 1.4 )
           CALL PGSLW   ( 4 )
           X_DAT_SHIFT  = 0.20*X_BL
           X_HEA_CENTER = 0.8
           Y_HEA_FACTOR = 0.030
         ELSE IF ( ISIZE == 5 ) THEN
           CALL PGSCH   ( 1.6 )
           CALL PGSLW   ( 5 )
           X_DAT_SHIFT  = 0.25*X_BL
           X_HEA_CENTER = 0.70
           Y_HEA_FACTOR = 0.030
      END IF
!
! --- Draw the image header: source name
!
      CALL GETENVAR ( 'MAP_NAME', STR ) 
      IF ( ILEN(STR) == 0 ) THEN
           CALL PGPTXT  ( 0.0, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, X_HEA_CENTER, &
     &                    MAP%SOU_NAME )
         ELSE
           WRITE ( 6, * ) 'Set source name according to the environment '// &
     &                    'variable MAP_NAME = '//TRIM(STR)
           CALL PGPTXT  ( 0.0, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, X_HEA_CENTER, &
     &                    STR )
      END IF
!
! --- Printing the date in the upper left corner
!
      STR = MJDSEC_TO_DATE ( MAP%MJD, MAP%TAI, -2 )
      CALL PGSLW   ( 1   )
      CALL PGSCH   ( 1.6 )
      CALL CLRCH ( STR(11:) )
      IF ( MAP%NUM_SEG == 1 ) THEN
           CONTINUE 
         ELSE 
!
! -------- If the map was created using more than one epoch, put
! -------- the number of epochs
!
           STR(11:11) = '('
           CALL INCH ( MAP%NUM_SEG, STR(12:20) )
           CALL CHASHL ( STR(12:20) )
           STR(ILEN(STR)+1:ILEN(STR)+1) = ')'
      END IF 
      IF ( ISIZE == 4 .OR. ISIZE == 5 ) THEN
           CALL PGSCH   ( 1.3 )
           CALL PGSLW   ( 2 )
      END IF
      CALL PGPTXT  ( X_BL + X_DAT_SHIFT, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), &
     &               0.0, 0.0, STR(1:I_LEN(STR)) )
!
! --- Printing the image sky frequency in the upper right corner of the image
!
      WRITE ( UNIT=STR, FMT='("Freq: ",F4.1," GHz")' ) 1.D-9*MAP%FREQ
      IF ( STR(7:7) == ' ' ) CALL CHASHL ( STR(7:) )
      IF ( ISIZE == 0 .OR.  ISIZE == 1  .OR. ISIZE == 2 ) THEN
           STR = STR(1:1)//STR(5:)
      END IF
      CALL PGSCI   ( ITAB_CLR(1,1) )
      IF ( ISIZE == 0 ) THEN
           XFREQ_CENTER = 0.9
         ELSE IF ( ISIZE == 1 ) THEN
           XFREQ_CENTER = 0.8
         ELSE
           XFREQ_CENTER = 1.0
      END IF
      CALL PGPTXT  ( X_TR, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, &
     &               STR(1:I_LEN(STR)) )
!
! --- Put a strut
!
      CALL PGSCI   ( 0 )
      IF ( ISIZE == 5 ) THEN
           CALL PGPTXT  ( 1.25*X_BL, Y_TR + 1.40*Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, '|' )
         ELSE 
           CALL PGPTXT  ( 1.25*X_BL, Y_TR + 1.10*Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, '|' )
      END IF 
!
! --- Draw the axes annoation
!
      CALL PGSCI   ( 1 )
      CALL PGSCH   ( 1.2 )
      IER = 0
      CALL RG_TAT ( MAP%DELTA, 3, STR, IER )
      IF ( IER == 0 ) THEN
           IF ( STR(1:1) == ' ' ) STR(1:1) = '+'
           STR(4:4) = ':'
           STR(7:7) = ':'
           IF ( ISIZE == 0 ) THEN
                CALL PGSCH   ( 1.6 )
                STR = 'Decl. (mas) relative to '//STR
                X_MAR = X_BL*1.27
              ELSE 
                IF ( ISIZE == 4 ) THEN
                     CALL PGSLW   ( 2 )
                  ELSE IF ( ISIZE == 5 ) THEN
                     CALL PGSLW   ( 2 )
                END IF
                STR = 'Declination (mas) relative to '//STR
                X_MAR = X_BL*1.18
           END IF
         ELSE 
           STR = 'Relative declination'
           X_MAR = X_BL*1.18
      END IF
      CALL PGPTXT  ( X_MAR, (Y_TR+Y_BL)/2.0, 90.0, 0.5, STR(1:I_LEN(STR)) )
!
      IER = 0
      CALL RH_TAT ( MAP%ALPHA, 4, STR, IER )
      IF ( IER == 0 ) THEN
           CALL CHASHL ( STR )
           STR(3:3) = ':'
           STR(6:6) = ':'
           IF ( ISIZE == 0 ) THEN
                STR = 'Rt. asc. (mas) relative to '//STR
              ELSE
                IF ( ISIZE == 4 ) THEN
                     CALL PGSLW   ( 2 )
                  ELSE IF ( ISIZE == 5 ) THEN
                     CALL PGSLW   ( 2 )
                END IF
                STR = 'Right ascension (mas) relative to '//STR
           END IF
         ELSE 
           STR = 'Relative right ascension'
      END IF
      IF ( ISIZE == 0 ) THEN
           Y_RA_FACTOR    = 1.30
           Y_BEAM_FACTOR  = 1.30
           Y_LEVEL_FACTOR = 1.38
         ELSE IF ( ISIZE == 1 ) THEN
           Y_RA_FACTOR    = 1.22
           Y_BEAM_FACTOR  = 1.30
           Y_LEVEL_FACTOR = 1.38
         ELSE IF ( ISIZE == 2 ) THEN
           Y_RA_FACTOR    = 1.20
           Y_BEAM_FACTOR  = 1.27
           Y_LEVEL_FACTOR = 1.34
         ELSE IF ( ISIZE == 3 ) THEN
           Y_RA_FACTOR    = 1.18
           Y_BEAM_FACTOR  = 1.25
           Y_LEVEL_FACTOR = 1.32
         ELSE IF ( ISIZE == 4 ) THEN
           Y_RA_FACTOR    = 1.18
           Y_BEAM_FACTOR  = 1.25
           Y_LEVEL_FACTOR = 1.32
         ELSE IF ( ISIZE == 5 ) THEN
           Y_RA_FACTOR    = 1.18
           Y_BEAM_FACTOR  = 1.25
           Y_LEVEL_FACTOR = 1.32
      END IF
      CALL PGPTXT  ( (X_TR+X_BL)/2.0, Y_RA_FACTOR*Y_BL, 0.0, 0.55, &
     &               STR(1:I_LEN(STR)) )
!
! --- Draw the image sky frequency
!
      WRITE ( UNIT=STR, FMT='("Freq: ",F4.1," GHz")' ) 1.D-9*MAP%FREQ
!
! --- Print the peak level of the correlated flux density
!
      WRITE ( UNIT=STR, FMT='("Peak_lev= ",F6.3," Jy/beam")' ) MAP%FLUX_MAX
      IF ( STR(12:12) .EQ. ' ' ) THEN
           CALL CHASHL ( STR(12:) )
      END IF

      IF ( IDEV == 2 ) THEN
           IF ( ISIZE == 0 ) THEN
                CALL PGSLW ( 2 )
                CALL PGSCH   ( 1.4 )
              ELSE IF ( ISIZE == 1 ) THEN
                CALL PGSLW ( 2 )
                CALL PGSCH   ( 1.0 )
              ELSE IF ( ISIZE == 2 ) THEN
                CALL PGSLW ( 2 )
                CALL PGSCH   ( 1.0 )
              ELSE IF ( ISIZE == 3 ) THEN
                CALL PGSLW ( 2 )
                CALL PGSCH ( 1.0 )
              ELSE IF ( ISIZE == 4 ) THEN
                CALL PGSLW ( 3 )
                CALL PGSCH   ( 1.0 )
              ELSE IF ( ISIZE == 5 ) THEN
                CALL PGSLW ( 3 )
                CALL PGSCH ( 0.95 )
           END IF
      END IF
      CALL PGSCI   ( ITAB_CLR(1,1) )
      IF ( ISIZE == 0 ) THEN
           CALL PGPTXT  ( 1.32*X_BL, 1.54*Y_BL, 0.0, 0.0, STR(1:I_LEN(STR)) )
         ELSE 
           CALL PGPTXT  ( 1.24*X_BL, Y_BEAM_FACTOR*Y_BL, 0.0, 0.0, STR(1:I_LEN(STR)) )
      END IF
      CALL PGSCI   ( 1 )
!
! --- Printing the correlated flux density first level
!
      IF ( MAP%NOISE < 0.0001 ) THEN
           WRITE ( UNIT=STR, FMT='("Rms_noise= ",F4.2," mJy/beam")' ) 1.D3*MAP%NOISE
         ELSE IF ( MAP%NOISE < 0.01 ) THEN
           WRITE ( UNIT=STR, FMT='("Rms_noise= ",F4.1," mJy/beam")' ) 1.D3*MAP%NOISE
         ELSE IF ( MAP%NOISE < 0.1 ) THEN
           WRITE ( UNIT=STR, FMT='("Rms_noise= ",F4.0," mJy/beam")' ) 1.D3*MAP%NOISE
         ELSE 
           WRITE ( UNIT=STR, FMT='("Rms_noise= ",F6.3," Jy/beam")'  ) MAP%NOISE
      END IF
!
      CALL PGSLW   (   1 )
      IF ( ISIZE == 0 ) THEN
           CALL PGPTXT  ( 1.32*X_BL, 1.42*Y_BL, 0.0, 0.0, STR(1:I_LEN(STR)) )
         ELSE 
           IF ( ISIZE == 4 .OR. ISIZE == 5 ) THEN
                CALL PGSLW   ( 3 )
           END IF
           CALL PGPTXT  ( 0.02*X_BL, Y_BEAM_FACTOR*Y_BL, 0.0, 0.0, STR(1:I_LEN(STR)) )
      END IF
!
! --- Store the first contour level
!
      NC = 1
      CALL CLRCH ( STR )
      IF ( LEV_CONT(1) < 0.001 ) THEN
           WRITE ( UNIT=STR, FMT='("Levels: ", F3.1)' ) 1.E3*LEV_CONT(1)
         ELSE IF ( LEV_CONT(1) < 1.0 ) THEN
           WRITE ( UNIT=STR, FMT='("Levels: ", I3)' ) NINT(1.E3*LEV_CONT(1))
         ELSE IF ( LEV_CONT(1) < 10.0 ) THEN
           WRITE ( UNIT=STR, FMT='("Levels: ", I4)' ) NINT(1.E3*LEV_CONT(1))
         ELSE IF ( LEV_CONT(1) < 100.0 ) THEN
           WRITE ( UNIT=STR, FMT='("Levels: ", I5)' ) NINT(1.E3*LEV_CONT(1))
         ELSE 
           WRITE ( UNIT=STR, FMT='("Levels: ", I5)' ) 99999
           WRITE ( 6, * ) ' '
           WRITE ( 6, * ) 'Big level MAP%SOU_NAME = ', MAP%SOU_NAME
           WRITE ( 6, * ) 'Big level LEV_CONT(1) = ', LEV_CONT(1)
           WRITE ( 6, * ) ' '
      END IF
!
! --- Compute other contour levels
!
      DO 430 J3=2,MC
         NC = NC + 1
         LEV_CONT(J3) = 2.0D0*LEV_CONT(J3-1)
         IF ( LEV_CONT(J3) > MAP%FLUX_MAX ) THEN
!
! ----------- ... till the reach the peak level
!
              NC = J3 - 1
              GOTO 830
         END IF
!
! ------ Format the string with values of the contour levels
!
         IL = ILEN(STR)
         IF ( ISIZE == 0 .AND. J3 > 5 ) GOTO 430
         
         IF ( LEV_CONT(J3) < 0.001 ) THEN
              WRITE ( UNIT=STR(IL+1:), FMT='(", ", F3.1)' ) 1.E3*LEV_CONT(J3)
            ELSE IF ( LEV_CONT(J3) < 1.0 ) THEN
              WRITE ( UNIT=STR(IL+1:), FMT='(", ", I3)' ) NINT(1.E3*LEV_CONT(J3))
            ELSE IF ( LEV_CONT(J3) < 10.0 ) THEN
              WRITE ( UNIT=STR(IL+1:), FMT='(", ", I4)' ) NINT(1.E3*LEV_CONT(J3))
            ELSE 
              WRITE ( UNIT=STR(IL+1:), FMT='(", ", I5)' ) NINT(1.E3*LEV_CONT(J3))
         END IF
 430  CONTINUE 
 830  CONTINUE 
      IL = ILEN(STR)
      STR(IL+2:) = 'mJy/beam'
      IF ( ISIZE == 0 ) THEN
           CALL PGSCH   ( 1.50 )
        ELSE IF ( ISIZE == 1 ) THEN
           CALL PGSCH   ( 1.00 )
        ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSCH   ( 1.00 )
        ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSCH   ( 0.80 )
        ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSCH   ( 1.00 )
           CALL PGSLW   ( 2 )
        ELSE IF ( ISIZE == 5 ) THEN
           CALL PGSCH   ( 0.95 )
      END IF
      IF ( ISIZE == 0 ) THEN
           CALL PGPTXT  ( 1.32*X_BL, 1.66*Y_BL, 0.0, 0.0, STR(1:I_LEN(STR)) )
         ELSE
           CALL PGPTXT  ( 1.24*X_BL, Y_LEVEL_FACTOR*Y_BL, 0.0, 0.0, STR(1:I_LEN(STR)) )
      END IF
      CALL PGSCI   ( 0 ) 
      IF ( ISIZE == 1 ) THEN
           CALL PGPTXT  ( X_TR, 1.01*Y_LEVEL_FACTOR*Y_BL, 0.0, 0.0, '||||||||' )
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGPTXT  ( X_TR, 1.01*Y_LEVEL_FACTOR*Y_BL, 0.0, 0.0, '|||||||' )
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGPTXT  ( X_TR, 1.01*Y_LEVEL_FACTOR*Y_BL, 0.0, 0.0, '||||||' )
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGPTXT  ( X_TR, 1.01*Y_LEVEL_FACTOR*Y_BL, 0.0, 0.0, '|||||' )
         ELSE IF ( ISIZE == 5 ) THEN
           CALL PGPTXT  ( X_TR, 1.01*Y_LEVEL_FACTOR*Y_BL, 0.0, 0.0, '|||||' )
      ENDIF
      CALL PGSCI   ( 1 ) 
!
! --- Determine location of the clean beam
!
      XC_BEAM = X_BL - AX_FACTOR*RAD_TO_MAS*MAP%BEAM_MAJ
      IF ( FL_RA_POS ) THEN
           XC_BEAM = X_BL - AX_FACTOR*RAD_TO_MAS*MAP%BEAM_MAJ/2.0D0
      END IF
      YC_BEAM = Y_BL + AX_FACTOR*RAD_TO_MAS*MAP%BEAM_MAJ/2.0D0
!
! --- Draw the beam
!
      IF ( IBEAM == 0  .OR. IBEAM == 3 ) THEN
!
! -------- First, fill the area of the beam with grey color
!
           CALL PGSCI ( GREY_COL ) 
           CALL PGSFS ( 1 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*MAP%BEAM_MIN/2.0D0, &
     &                      RAD_TO_MAS*MAP%BEAM_MAJ/2.0D0, -MAP%BEAM_POS_ANG )
!
! -------- Then the contour of the beam using black color
!
           CALL PGSCI ( 1 ) 
           CALL PGSFS ( 2 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*MAP%BEAM_MIN/2.0D0, &
     &                      RAD_TO_MAS*MAP%BEAM_MAJ/2.0D0, -MAP%BEAM_POS_ANG )
         ELSE IF ( IBEAM == 1  .OR.  &
     &             IBEAM == 11 .OR.  &
     &             IBEAM == 12 .OR.  &
     &             IBEAM == 13 .OR.  &
     &             IBEAM == 14       ) THEN
           CALL PGSCI ( GREY_COL ) 
           CALL PGSFS ( 1 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*MAP%BEAM_MIN/2.0D0, &
     &                      RAD_TO_MAS*MAP%BEAM_MIN/2.0D0, 0.0D0 )
           CALL PGSCI ( 1 ) 
           CALL PGSFS ( 3 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*MAP%BEAM_MIN/2.0D0, &
     &                      RAD_TO_MAS*MAP%BEAM_MIN/2.0D0, 0.0D0 )
         ELSE IF ( IBEAM == 21 ) THEN
           CALL PGSCI ( GREY_COL ) 
           CALL PGSFS ( 1 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*BEAM_SIZE/2.0D0, &
     &                      RAD_TO_MAS*BEAM_SIZE/2.0D0, 0.0D0 )
           CALL PGSCI ( 1 ) 
           CALL PGSFS ( 3 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*BEAM_SIZE/2.0D0, &
     &                      RAD_TO_MAS*BEAM_SIZE/2.0D0, 0.0D0 )
         ELSE IF ( IBEAM == 2  ) THEN
           CALL PGSCI ( GREY_COL ) 
           CALL PGSFS ( 1 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*MAP%BEAM_MAJ/2.0D0, &
     &                      RAD_TO_MAS*MAP%BEAM_MAJ/2.0D0, 0.0D0 )
           CALL PGSCI ( 1 ) 
           CALL PGSFS ( 4 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*MAP%BEAM_MAJ/2.0D0, &
     &                      RAD_TO_MAS*MAP%BEAM_MAJ/2.0D0, 0.0D0 )
         ELSE IF ( IBEAM == 5  ) THEN
           CALL PGSCI ( GREY_COL ) 
           CALL PGSFS ( 1 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*MAP%BEAM_MIN/2.0D0/BD_FACTOR, &
     &                      RAD_TO_MAS*MAP%BEAM_MIN/2.0D0/BD_FACTOR, 0.0D0 )
           CALL PGSCI ( 1 ) 
           CALL PGSFS ( 4 )
           CALL DRAW_BEAM ( XC_BEAM, YC_BEAM, RAD_TO_MAS*MAP%BEAM_MIN/2.0D0/BD_FACTOR, &
     &                      RAD_TO_MAS*MAP%BEAM_MIN/2.0D0/BD_FACTOR, 0.0D0 )
      END IF
!
! --- Set parameters for pgplot routine which draw the contours
!
      TR(1) = -RAD_TO_MAS*MAP%STEP_RA*MAP%DIM1/2.0
      TR(2) =  RAD_TO_MAS*MAP%STEP_RA
      TR(3) =  0.0
      TR(4) = -RAD_TO_MAS*MAP%STEP_DL*MAP%DIM2/2.0
      TR(5) =  0.0
      TR(6) =  RAD_TO_MAS*MAP%STEP_DL
!
      CALL PGSCI  ( ITAB_CLR(1,1) )
      CALL PGSLW  ( 1 )
!
      IF ( IBEAM > 0 ) THEN
!
! ======== Create the pixel image array using the set of delta functions &
! -------- which represent the image
!
! -------- Allocate dynamic memory for the image
!
           ALLOCATE ( IMAGE(MAP%DIM1,MAP%DIM2), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*MAP%DIM1*MAP%DIM2, STR )
                CALL ERR_LOG ( 4621, IUER, 'GEN_MAPPLOT', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for the image' )
                RETURN 
           END IF
           CALL NOUT_R4 ( MAP%DIM1*MAP%DIM2, IMAGE )
!
           DO 440 J4=1,MAP%DIM1
              XC = MAP%STEP_RA*(J4 - MAP%DIM1/2)
              DO 450 J5=1,MAP%DIM2 
                 YC = MAP%STEP_DL*(J5 - MAP%DIM2/2)
                 DO 460 J6=1,MAP%NUM_CC
!
! ----------------- Using different beams for convolution
!
                    IF ( IBEAM == 1  .OR. &
     &                   IBEAM == 11 .OR. &
     &                   IBEAM == 12 .OR. &
     &                   IBEAM == 13 .OR. &
     &                   IBEAM == 14      ) THEN
!
! ---------------------- Circular beam with the semi-minor axis of the 
! ---------------------- initial image
!
                         ARG = -( (XC - MAP%COOR_CC(1,J6))*COS(-MAP%BEAM_POS_ANG) &
     &                         -(  YC - MAP%COOR_CC(2,J6))*SIN(-MAP%BEAM_POS_ANG) )**2/ &
     &                             (MAP%BEAM_MIN/2.0D0)**2 &
     &                         -( (XC - MAP%COOR_CC(1,J6))*SIN(-MAP%BEAM_POS_ANG) &
     &                           +(YC - MAP%COOR_CC(2,J6))*COS(-MAP%BEAM_POS_ANG) )**2/ &
     &                             (MAP%BEAM_MIN/2.0D0)**2
                      ELSE IF ( IBEAM == 21 ) THEN
                         ARG = -( (XC - MAP%COOR_CC(1,J6)) &
     &                         -(  YC - MAP%COOR_CC(2,J6)) )**2/ &
     &                             (BEAM_SIZE/2.0D0)**2 &
     &                         -( (XC - MAP%COOR_CC(1,J6)) &
     &                           +(YC - MAP%COOR_CC(2,J6)) )**2/ &
     &                             (BEAM_SIZE/2.0D0)**2
                      ELSE IF ( IBEAM == 2 ) THEN
!
! ---------------------- Circular beam with the semi-major axis of the initial 
! ---------------------- image
!
                         ARG = -( (XC - MAP%COOR_CC(1,J6))*COS(-MAP%BEAM_POS_ANG) &
     &                         -(  YC - MAP%COOR_CC(2,J6))*SIN(-MAP%BEAM_POS_ANG) )**2/ &
     &                             (MAP%BEAM_MAJ/2.0D0)**2 &
     &                         -( (XC - MAP%COOR_CC(1,J6))*SIN(-MAP%BEAM_POS_ANG) &
     &                           +(YC - MAP%COOR_CC(2,J6))*COS(-MAP%BEAM_POS_ANG) )**2/ &
     &                             (MAP%BEAM_MAJ/2.0D0)**2
                      ELSE IF ( IBEAM == 3 ) THEN
!
! ---------------------- Elliptical "clean" beam
!
                         ARG = -( (XC - MAP%COOR_CC(1,J6))*COS(-MAP%BEAM_POS_ANG) &
     &                         -(  YC - MAP%COOR_CC(2,J6))*SIN(-MAP%BEAM_POS_ANG) )**2/ &
     &                             (MAP%BEAM_MIN/2.0D0)**2 &
     &                         -( (XC - MAP%COOR_CC(1,J6))*SIN(-MAP%BEAM_POS_ANG) &
     &                           +(YC - MAP%COOR_CC(2,J6))*COS(-MAP%BEAM_POS_ANG) )**2/ &
     &                             (MAP%BEAM_MAJ/2.0D0)**2
                      ELSE IF ( IBEAM == 6 ) THEN
                         IF ( ABS(XC - MAP%COOR_CC(1,J6)) < ABS(MAP%STEP_RA/2.0) .AND. & 
     &                        ABS(YC - MAP%COOR_CC(2,J6)) < ABS(MAP%STEP_DL/2.0)       ) THEN
                              ARG = 0.0D0
                            ELSE
                              ARG = -100.0D0
                         END IF
                      ELSE IF ( IBEAM == 5 ) THEN
!
! ---------------------- Circular beam with the semi-minor axis of the 
! ---------------------- initial image **DIVIDED** by BD_FACTOR
!
                         ARG = -( (XC - MAP%COOR_CC(1,J6))*COS(-MAP%BEAM_POS_ANG) &
     &                         -(  YC - MAP%COOR_CC(2,J6))*SIN(-MAP%BEAM_POS_ANG) )**2/ &
     &                             (MAP%BEAM_MIN/(2.0D0*BD_FACTOR))**2 &
     &                         -( (XC - MAP%COOR_CC(1,J6))*SIN(-MAP%BEAM_POS_ANG) &
     &                           +(YC - MAP%COOR_CC(2,J6))*COS(-MAP%BEAM_POS_ANG) )**2/ &
     &                             (MAP%BEAM_MIN/(2.0D0*BD_FACTOR))**2
                    END IF
!
                    IF ( IBEAM == 4 ) THEN
                         IMAGE(J4,J5) = IMAGE(J4,J5) + MAP%FLUX_CC(J6)
                       ELSE 
                         IF ( ARG > -15.0D0 ) THEN
                              IMAGE(J4,J5) = IMAGE(J4,J5) + MAP%FLUX_CC(J6)*EXP( ARG )
                         END IF
                    END IF
                    IF ( J4 == 1 .AND. J5 == 1 ) THEN
                         CX = CX + MAP%FLUX_CC(J6)*MAP%COOR_CC(1,J6)
                         CY = CY + MAP%FLUX_CC(J6)*MAP%COOR_CC(2,J6)
                         FT = FT + MAP%FLUX_CC(J6)
                         IF ( MAP%FLUX_CC(J6) > FM ) THEN
                              FM = MAP%FLUX_CC(J6) 
                              MX = MAP%COOR_CC(1,J6)
                              MY = MAP%COOR_CC(2,J6)
                         END IF
                    END IF
 460             CONTINUE 
 450          CONTINUE 
 440       CONTINUE 
           CX = CX/FT
           CY = CY/FT
           IF ( FL_CM ) THEN
                PA = ATAN_CS_R4 ( CY, CX )
                IF ( PA > PI ) PA = PA - PI2
                WRITE ( 6, 110 ) MAP%SOU_NAME, MAP%DATE_OBS, CX*RAD_TO_MAS, CY*RAD_TO_MAS, &
     &                           SQRT( CX**2 + CY**2)*RAD_TO_MAS, PA*180.0/PI, FM, FT
 110            FORMAT ( 'Sou: ', A, ' Date: ', A, ' Cx= ', F8.3, ' Cy= ', F8.3, &
     &                   ' arc= ', F8.3, ' mas  C_PA: ', F6.1, ' Flux max/tot= ', &
     &                   F8.4, 1X, F8.4, ' Jy' ) 
           END IF
!
! -------- Draw the contours
!
           CALL PGCONT ( IMAGE, MAP%DIM1, MAP%DIM2, 1, MAP%DIM1, &
     &                   1, MAP%DIM2, LEV_CONT, NC, TR )
           DEALLOCATE ( IMAGE )
         ELSE
!
! -------- Draw contours
!
           CALL PGSLW  ( ITHICK ) 
           CALL PGCONT ( MAP%IMAGE, MAP%DIM1, MAP%DIM2, 1, MAP%DIM1, &
     &                   1, MAP%DIM2, LEV_CONT, NC, TR )
      END IF
!
      FL_ANG_UPDATE = .FALSE.
      XC = 0.0
      YC = 0.0
 920  CONTINUE 
      CALL PGSLW ( 1 ) 
      CALL PGSCI ( 1 )
!
      IF ( IBEAM == 11 ) THEN
           X2(1) = 0.0
           X2(2) = 0.0
           Y2(1) = Y_BL
           Y2(2) = Y_TR
           CALL PGSLW  ( 1 )
           CALL PGLINE ( 2, X2, Y2 )
           X2(1) = 0.05*X_BL
           X2(2) = 0.05*X_TR
           Y2(1) = 0.0
           Y2(2) = 0.0
           CALL PGLINE ( 2, X2, Y2 )
         ELSE IF ( IBEAM == 12  .OR.  IBEAM == 13 ) THEN
           X2(1) = 0.0
           Y2(1) = 0.0 
           X2(2) = -Y_BL*SQRT(2.0)*SIN(-JET_ANG_R4)
           Y2(2) = -Y_BL*SQRT(2.0)*COS(-JET_ANG_R4)
           CALL PGSLW  ( 1 )
           CALL PGLINE ( 2, X2, Y2 )
      END IF
!
      IF ( IDEV .EQ. 1 ) THEN
!
! -------- Asking the user input
!
           CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
           IF ( CH == 'X' ) THEN
                CALL PGCLOQ()
                IF ( FL_ANG_UPDATE .AND. IBEAM == 13 ) THEN
                     IBEAM = -13
                END IF
              ELSE IF ( IBEAM .GE. 11  .AND.  &
     &                  IBEAM .LE. 14  .AND.  &
     &                  ( CH == '>'  .OR.  CH == '.' ) ) THEN
                IMA_WIN = IMA_WIN/DSQRT(2.0D0)
                GOTO 910
              ELSE IF ( IBEAM .GE. 11  .AND.  &
     &                  IBEAM .LE. 14  .AND.  &
     &                  ( CH == '<'  .OR.  CH == ',' ) ) THEN
                IMA_WIN = IMA_WIN*DSQRT(2.0D0)
                GOTO 910
              ELSE IF ( IBEAM == 21  .AND. &
     &                  ( CH == '>'  .OR.  CH == '.' ) ) THEN
                BEAM_SIZE = BEAM_SIZE/DSQRT(2.0D0)
                GOTO 910
              ELSE IF ( IBEAM .GE. 11  .AND.  &
     &                  IBEAM .LE. 14  .AND.  &
     &                  ( CH == '<'  .OR.  CH == ',' ) ) THEN
                IMA_WIN = IMA_WIN*DSQRT(2.0D0)
                GOTO 910
              ELSE IF ( IBEAM == 21  .AND. &
     &                  ( CH == '<'  .OR.  CH == ',' ) ) THEN
                BEAM_SIZE = BEAM_SIZE*DSQRT(2.0D0)
                GOTO 910
              ELSE IF ( IBEAM .GE. 11  .AND.  &
     &                  IBEAM .LE. 14  .AND.  &
     &                  CH == 'D' ) THEN
                CALL PGCLOQ()
                IF ( FL_ANG_UPDATE ) THEN
                     JET_ANG_R8 = JET_ANG_R4
                     IMA_WIN_RAD = IMA_WIN/RAD_TO_MAS
                     CALL ERR_PASS ( IUER, IER )
                     CALL IO_JET_ANG ( 2, FILIN, JET_ANG_R8, IMA_WIN_RAD, &
     &                                 QUAL_CODE, IER ) 
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 6263, IUER, 'GEN_MAPPLOT', 'Error '// &
     &                        'in an attempt to put jet direction for the '// &
     &                        'sources related to the file '//FILOUT )
                          RETURN
                     END IF
                     IBEAM = -13
                 END IF
              ELSE
                IF ( IBEAM == 12 ) THEN
                     JET_ANG_R4 = P2I - ATAN_CS_R4 ( -XC, YC )
                     IF ( JET_ANG_R4 < 0.0 ) JET_ANG_R4 = JET_ANG_R4 + PI2
                     IF ( JET_ANG_R4 < 0.0 ) JET_ANG_R4 = JET_ANG_R4 + PI2
                     CALL PGSCI  ( 0 )
                     CALL PGLINE ( 2, X2, Y2 )
                     CALL PGSCI  ( 1 )
                     FL_ANG_UPDATE = .TRUE.
                     GOTO 920
                END IF
           END IF
         ELSE
           CALL PGCLOQ()
           IF ( ( IDEV == 2 .AND. ISIZE == 0 ) .OR.  &
     &          ( IDEV == 2 .AND. ISIZE == 1 ) .OR.  &
     &          ( IDEV == 2 .AND. ISIZE == 2 )       ) THEN
!
! ------------- A special trick: make lines in postscript file thinner
!
                COM_STR = 'sed -i '// &
     &                '"s@/LW {5 mul setlinewidth} bind def'// &
     &                  '@/LW {1 mul setlinewidth} bind def@g" '//FILOUT
                CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           END IF
!@           WRITE ( 6, * ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GEN_MAPPLOT !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DRAW_BEAM ( XCENT, YCENT, XRADIUS, YRADIUS, ANG )
! ************************************************************************
! *                                                                      *
! *   Drawing the circle using PGPLOT. PGPLOT routine PGCIRC draws       *
! *   rather ellipses than circles.                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     XCENT ( REAL*8    ) -- X-World cordinates of the center.         *
! *     YCENT ( REAL*8    ) -- Y-World cordinates of the center.         *
! *   XRADIUS ( REAL*8    ) -- X-radius (world coordinates).             *
! *   YRADIUS ( REAL*8    ) -- Y-radius (world coordinates).             *
! *       ANG ( REAL*8    ) -- position angle.                           *
! *                                                                      *
! *  ###  26-JAN-2007   DRAW_BEAM v1.1  (c)  L. Petrov  26-JAN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     PI__NUM, P2I, PI2
      PARAMETER  ( PI__NUM = 3.141592653589793D0 )
      PARAMETER  ( PI2     = 2.0D0*PI__NUM       )
      PARAMETER  ( P2I     = PI__NUM/2.0D0       )
      REAL*8      XCENT, YCENT, XRADIUS, YRADIUS, ANG, XC, YC
      INTEGER*4   NPTS
      PARAMETER ( NPTS=1024 )
!
      REAL*8    ANGLE
      REAL*4    X(NPTS),Y(NPTS)
      INTEGER*4 J1
!
      IF ( XRADIUS .LT. 1.D-30 ) XRADIUS = 1.D-30 ! In order to prevent
      IF ( YRADIUS .LT. 1.D-30 ) YRADIUS = 1.D-30 ! underrflow
      DO 410 J1=1,NPTS
         ANGLE = (J1-1)*PI2/REAL(NPTS-1)
         XC = XRADIUS*DCOS(ANGLE)
         YC = YRADIUS*DSIN(ANGLE)
         X(J1) = XCENT + XC*DCOS(ANG) - YC*DSIN(ANG)
         Y(J1) = YCENT + XC*DSIN(ANG) + YC*DCOS(ANG)
 410  CONTINUE
      CALL PGPOLY ( NPTS, X, Y )
!
      END  SUBROUTINE  DRAW_BEAM  !#!#
