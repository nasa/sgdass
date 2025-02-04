      PROGRAM    VIO_MAP
! ************************************************************************
! *                                                                      *
! *   Program  VIO_MAP  reads the file with the maps of total electron   *
! *   contents if the ionosphere and displays a color plot of map at     *
! *   the specified epoch.                                               *
! *                                                                      *
! *   Usage: vio_map vio_file date_map proj dev_code                     *
! *          where                                                       *
! *                vio_file -- file with TEC maps in VIONO format.       *
! *                date_map -- Date of the map in ISO8601 format.        *
! *                proj     -- Proection code. One of                    *
! *                            1 -- latitide-longitude project           *
! *                            2 -- whole Earth Hammer projection.       *
! *                dev_code -- device code. One of                       *
! *                            1 -- XW -- the plot is displayed at the   *
! *                                 terminal.                            *
! *                            2 -- plot in gif will be written in the   *
! *                                 output file with name                *
! *                                 /tmp/plot_map_yyyy_mm_dd_hh.gif      *
! *                            3 -- plot in postscript format will be    *
! *                                 written in theoutput file with name  *
! *                                 /tmp/plot_map_yyyy_mm_dd_hh.ps       *
! *                                                                      *
! *  ### 12-MAY-2010    VIO_MAP    v1.0 (c)  L. Petrov  18-MAY-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( IONO__TYPE ) :: VIO
      INTEGER*4  M_LON, M_LAT
      PARAMETER  ( M_LON = 1000, M_LAT = 501 )
      REAL*4     TEC(M_LON,M_LAT), LON(M_LON), LAT(M_LAT), RVAL_USE_MAX
      CHARACTER  VIO_FILE*128, PROJ_STR*3, DATE_MAP*21, STR*32, TITLE*64, &
     &           DEV_STR*32
      INTEGER*4  MJD_MAP, MJD_VIO_BEG, MJD_VIO_END, IDAY, IPRC, LEPC, &
     &           IDEV, IPAL, J1, J2, IUER
      REAL*8     UTC_MAP, UTC_VIO_BEG, UTC_VIO_END, UTC_BEG_USED
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IPAL = 7
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage vio_map vio_file date_map proj dev_code'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, VIO_FILE )
           CALL GETARG ( 2, DATE_MAP )
           CALL GETARG ( 3, PROJ_STR )
           CALL GETARG ( 4, DEV_STR  )
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_MAP, MJD_MAP, UTC_MAP, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 4401, -2, 'VIO_MAP', 'Failure to parse '// &
     &              'the start date' )
                CALL EXIT ( 1 )
           END IF
!
           CALL CHIN  ( PROJ_STR, IPRC )
           IF ( IPRC < 1  .OR.  IPRC > 3 ) THEN
                CALL ERR_LOG ( 4402, -2, 'VIO_MAP', 'Wrong projection '// &
     &              'code: '//PROJ_STR(1:I_LEN(PROJ_STR))// &
     &              ' an integer in range [1, 3] was expected' ) 
                CALL EXIT ( 1 )
           END IF
!
           CALL CHIN  ( DEV_STR, IDEV )
           IF ( IDEV < 1  .OR.  IDEV > 3 ) THEN
                CALL ERR_LOG ( 4403, -2, 'VIO_MAP', 'Wrong device '// &
     &              'code: '//DEV_STR(1:I_LEN(DEV_STR))// &
     &              ' an integer in range [1, 3] was expected' ) 
                CALL EXIT ( 1 )
           END IF
      END IF
!
! --- Initialization
!
      CALL NOUT ( SIZEOF(VIO), VIO )
!
! --- Read the file header
!
      IUER = -1
      CALL VIO_GET_HEADER ( VIO_FILE, VIO, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4403, -2, 'VIO_MAP', 'Failure to read the '// &
     &         'header of the ionosphere TEC maps file '//VIO_FILE )
           CALL EXIT ( 1 )
      END IF
      MJD_VIO_BEG = VIO%HEADER%MJD_BEG
      UTC_VIO_BEG = VIO%HEADER%UTC_BEG
      UTC_VIO_END = VIO%HEADER%UTC_BEG + (VIO%HEADER%NEPC-1)*VIO%HEADER%TIM_STEP
      IDAY = (UTC_VIO_END + 0.01D0*VIO%HEADER%TIM_STEP)/86400.0D0
      MJD_VIO_END = MJD_VIO_BEG + IDAY
      UTC_VIO_END = UTC_VIO_END - IDAY*86400.0D0
      LEPC = 7
!
      UTC_BEG_USED = UTC_MAP - 3*VIO%HEADER%TIM_STEP
!
!      write ( 6, * ) ' mjd_map= ', mjd_map, ' utc_map= ', utc_map ! %%%
!      write ( 6, * ) ' mjd_vio_beg= ', mjd_vio_beg, ' utc_vio_beg= ', utc_vio_beg ! %%%
!      write ( 6, * ) ' mjd_vio_end= ', mjd_vio_end, ' utc_vio_end= ', utc_vio_end ! %%%
!      write ( 6, * ) ' lat_min = ', vio%header%lat_min/deg__to__rad
!      write ( 6, * ) ' lat_max = ', (vio%header%lat_min + (vio%header%nlat-1)*vio%header%lat_step)/deg__to__rad
!      write ( 6, * ) ' nlon, nlat= ', vio%header%nlon, vio%header%nlat
!      write ( 6, * ) ' hei = ', vio%header%height  ! %%%%%%%%%%%%%%%%%%%%%%
!
! --- Read global TEC maps for LEPC epochs for the dates starting from
! --- MJD_MAP/UTC_BEG_USED. Results are put in VIO.
!
      IUER = -1
      CALL VTD_GET_IONO ( VIO_FILE, MJD_MAP, UTC_BEG_USED, LEPC, VIO, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4404, -2, 'VIO_MAP', 'Error in reading the body '// &
     &         'of the ionosphere TEC model data from the input file '//VIO_FILE )
           CALL EXIT ( 1 )
      END IF
!
! --- Compute coefficients of interpolating 3D spline that approximate the 
! --- field of TEC
!
      IUER = -1
      CALL COMP_IONO_SPL ( VIO, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4405, -2, 'VIO_MAP', 'Error in an attempt to compute '// &
     &         'coefficients of the 3D spline that interpolates ionosphere TEC map' )
           CALL EXIT ( 1 )
      END IF
!
! --- Compute the 2D map of TEC at the gloval grid with resolution 
! --- M_LON x M_AT at the epoch MJD_MAP/UTC_MAP
!
      IUER = -1
      CALL COMP_IONO_FRAME ( VIO, M_LON, M_LAT, MJD_MAP, UTC_MAP, TEC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4406, -2, 'VIO_MAP', 'Error in an attempt '// &
     &         'to compute the frame of the global TEC map' )
           CALL EXIT ( 1 )
      END IF
!
! --- Comput values of longitude and latitude axes
!
      IUER = -1
      DO 410 J1=1,M_LON
         LON(J1) = (-PI__NUM +  (J1-1)*PI2/M_LON )/DEG__TO__RAD
 410  CONTINUE 
      DO 420 J2=1,M_LAT
         LAT(J2) = (-P2I + (J2-1)*PI__NUM/(M_LAT-1))/DEG__TO__RAD
 420  CONTINUE 
!
      RVAL_USE_MAX = -1.0E+10
      IUER = -1
      STR = MJDSEC_TO_DATE ( MJD_MAP, UTC_MAP, IUER )
      TITLE = 'GPS TEC map at '//STR(1:21)
!
! --- Generate the plot
!
      IUER = -1
      CALL VIONO_GRID_PLOT ( IDEV, IPAL, IPRC, M_LON, M_LAT, MJD_MAP, UTC_MAP, &
     &                       LON, LAT, TEC, -1.0, RVAL_USE_MAX, TITLE, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4406, -2, 'VIO_MAP', 'Failure to make the '// &
     &         'plot of the global TEC map' )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  VIO_MAP  !#!#

! ------------------------------------------------------------------------
!
      SUBROUTINE COMP_IONO_FRAME ( VIO, M_LON, M_LAT, MJD_MAP, UTC_MAP, &
     &                             TEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  COMP_IONO_FRAME  computes the total electron contents     *
! *   ionosphere map at the global grid with resolution M_LON x M_LAT    *
! *   at the epoch MJD_MAP/UTC_MAP using coefficients of the expansion   *
! *   of TEC over 3D B-splines. It is assumed that the coefficients have *
! *   previosly been computed.                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VIO ( IONO__TYPE ) -- Data structure that keeps the data        *
! *                            related to ionosphere TEC maps.           *
! *                            It contains the header that describes     *
! *                            the dataset and the data.                 *
! *    M_LON ( INTEGER*4  ) -- Dimention the ouptu TEC array over        *
! *                            longitude.                                *
! *    M_LAT ( INTEGER*4  ) -- Dimention the ouptu TEC array over        *
! *                            latitude.                                 *
! *  MJD_MAP ( INTEGER*4  ) -- MJD epoch to which the TEC map will be    *
! *                            computed.                                 *
! *  UTC_MAP ( REAL*8     ) -- UTC epoch to which the TEC map will be    *
! *                            computed.                                 *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     TEC ( REAL*8     ) -- Array of the total electron content on     *
! *                           a grid M_LON x M_LAN. Units: TECU.         *
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
! * ### 12-MAY-2010  COMP_IONO_FRAME  v1.0 (c) L. Petrov 12-MAY-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( IONO__TYPE ) :: VIO
      INTEGER*4  M_LON, M_LAT, MJD_MAP, IUER 
      REAL*8     UTC_MAP
      REAL*4     TEC(M_LON,M_LAT)
      INTEGER*4  L_EPC, J1, J2, J3, J4, DIMS(3), INDS(3), IER
      REAL*4     ARGS(3), LAT_CELL, LON_CELL, EPS
      PARAMETER  ( EPS = 1.E-5 )
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      DIMS(1) = VIO%HEADER%NLON
      DIMS(2) = VIO%HEADER%NLAT
      DIMS(3) = VIO%HEADER%NEPC
!
      ARGS(3) = ( (MJD_MAP - VIO%HEADER%MJD_BEG)*86400.0D0 + &
     &            (UTC_MAP - VIO%HEADER%UTC_BEG) )/VIO%HEADER%TIM_STEP 
      INDS(3) = INT(ARGS(3)) + 1
!
! --- Correct possible overshot/undershot due to rounding
!
      IF ( INDS(3) .LE. 0 ) INDS(3) = 1
      IF ( INDS(3) .GE. VIO%HEADER%NEPC-1 ) INDS(3) = VIO%HEADER%NEPC - 1
      DO 410 J1=1,M_LAT
         LAT_CELL = -P2I + (J1-1)*PI__NUM/(M_LAT-1)
         ARGS(2) = LAT_CELL
         INDS(2) = INT ( (LAT_CELL - VIO%HEADER%LAT_MIN)/VIO%HEADER%LAT_STEP ) + 1
!
! ------ Correct possible overshot/undershot due to rounding
!
         IF ( INDS(2) .LE. 0 ) INDS(2) = 1
         IF ( INDS(2) .GE. VIO%HEADER%NLAT-1 ) INDS(2) = VIO%HEADER%NLAT - 1
         DO 420 J2=1,M_LON
            LON_CELL = -PI__NUM + (J2-1)*PI2/(M_LON-1)
            ARGS(1) = LON_CELL
            IF ( J2 == 1 ) THEN
                 ARGS(1) = ARGS(1) + EPS
               ELSE IF ( J2 == M_LON ) THEN
                 ARGS(1) = ARGS(1) - EPS
            END IF
            INDS(1) = INT ( (LON_CELL - VIO%HEADER%LON_MIN)/VIO%HEADER%LON_STEP ) + 1
!
! --------- Correct possible overshot/undershot due to rounding
!
            IF ( INDS(1) .LE. 0 ) INDS(1) = 1
            IF ( INDS(1) .GE. VIO%HEADER%NLON-1 ) INDS(1) = VIO%HEADER%NLON - 1
!
            CALL ERR_PASS ( IUER, IER ) 
            TEC(J2,J1) = VAL_3D_BSPL4 ( ARGS, VIO__M_DEG, DIMS, INDS, &
     &                                  VIO%LON_VAL, VIO%LAT_VAL, &
     &                                  VIO%TIM_VAL, VIO%TEC_SPL )
 420     CONTINUE 
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  COMP_IONO_FRAME  !#!#  
