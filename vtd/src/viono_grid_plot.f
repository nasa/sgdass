      SUBROUTINE VIONO_GRID_PLOT ( IDEV, IPAL, IPRC, NLON, NLAT, MJD, TAI, &
     &                             LON, LAT, R4_2D_MAP, FILL, &
     &                             RVAL_USE_MAX, TITLE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VIONO_GRID_PLOT  prepars the plot of the total electron    *
! *   contents ionosphere maps.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *         IDEV ( INTEGER*4 ) -- device code. One of                    *
! *                            1 -- XW -- the plot is displayed at the   *
! *                                 terminal.                            *
! *                            2 -- plot in gif will be written in the   *
! *                                 output file with name                *
! *                                 /tmp/plot_map_yyyy_mm_dd_hh.gif      *
! *                            3 -- plot in postscript format will be    *
! *                                 written in theoutput file with name  *
! *                                 /tmp/plot_map_yyyy_mm_dd_hh.ps       *
! *         IPAL ( INTEGER*4 ) -- Palitra code in range 1 to 7.          *
! *         IPRC ( INTEGER*4 ) -- Proection code. One of                 *
! *                            1 -- latitide-longitude projection.       *
! *                            2 -- whole Earth Hammer projection.       *
! *         NLON ( INTEGER*4 ) -- Longitude dimension.                   *
! *         NLAT ( INTEGER*4 ) -- Latitude dimension.                    *
! *          MJD ( INTEGER*4 ) -- MJD date of the map.                   *
! *          TAI ( REAL*8    ) -- TAI time of the map.                   *
! *          LON ( REAL*4    ) -- Array of longitude axis.               *
! *                               Dimension: NLON.                       *
! *          LAT ( REAL*4    ) -- Array of latitude axis.                *
! *                               Dimension: NLAT.                       *
! *    R4_2D_MAP ( REAL*4    ) -- Array of the TEC map. Dimension:       *
! *                               NLON,NLAT.                             *
! *         FILL ( REAL*4    ) -- Fill value.                            *
! * RVAL_USE_MAX ( REAL*4    ) -- Maximim value of hte TEC. Elements of  *
! *                               trhe TEC mapo R3_2D_MAP with values    *
! *                               greater than RVAL_USE_MAX are not      *
! *                               plotted.                               *
! *        TITLE ( CHARACTER ) -- Title of the plot.                     *
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
! * ### 10-MAY-2010 VIONO_GRID_PLOT  v1.0 (c)  L. Petrov 13-MAY-2008 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'diagi.i'
      CHARACTER  TITLE*(*)
      INTEGER*4  IPAL, IDEV, IPRC, NLON, NLAT, MJD, IUER
      REAL*8     TAI
      REAL*4     LON(NLON), LAT(NLAT), R4_2D_MAP(NLON,NLAT), FILL, &
     &           RVAL_USE_MAX
      CHARACTER  FINAM*128, STR*180, CH*1, STR_START*32, STR_END*23, &
     &           STR_DATE*30
      REAL*4     XMIN, XMAX, YMIN, YMAX, EPS
      INTEGER*4  M1, M2
      PARAMETER  ( M1 = 8192 )
      PARAMETER  ( M2 = 1024*1024 )
      PARAMETER  ( EPS = 1.E-5 )
      REAL*4     WHITE_CLR, XC, YC
      REAL*4     LONB_R4, LONE_R4, LATB_R4, LATE_R4, XL, YB, XR, YT, VAL_R4
      REAL*4     XPR, YPR, XCOEF_IMA, XCOEF_PAP, PHI, LAM, RVAL_MIN, &
     &           RVAL_MAX, ARR_X(M1), ARR_Y(M1), PAP_SIZE
      INTEGER*4  NCOL, INIT_COL, CI1, CI2, MAP_I4(M2), &
     &           IVAL_MIN, IVAL_MAX, IEPH, J1, J2, J3, J4, J5, J6, J7, J8, &
     &           IP, IX, IY, NLAB, IER
      INTEGER*2  IVAL_I2
      REAL*8     JD, SUM
      LOGICAL*4  FL_PLOT
!
      INTEGER*4  I, J, M, LOCC
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: PGBEG, I_LEN, ILEN
      LOCC(I,J,M) = I + (J-1)*M
!
! --- Initialization
!
      CALL CLRCH ( FINAM )
      STR_DATE = MJDSEC_TO_DATE ( MJD, TAI + 1.0, IER ) 
      FINAM = '/tmp/plot_map_'//STR_DATE(1:4)//'_'//STR_DATE(6:7)//'_'// &
     &        STR_DATE(9:10)
      FL_PLOT = .FALSE.
      LONB_R4 = LON(1)
      LONE_R4 = LON(NLON)
      LATB_R4 = LAT(1)
      LATE_R4 = LAT(NLAT)
      INIT_COL = 20
      NCOL = 200
      NLAB = 10
!
      IVAL_MIN =  1.0E9
      IVAL_MAX = -1.0E9
      RVAL_MIN =  1.0E9
      RVAL_MAX = -1.0E9
      IF ( IPRC == 1 ) THEN
           XCOEF_IMA = 0.75
           XCOEF_PAP = 0.75
         ELSE
           XCOEF_IMA = 0.5
           XCOEF_PAP = 0.75
      END IF
 910  CONTINUE
!
! --- Extract the map of the specified parameter on the specified epoch
!
      SUM = 0.0D0
      DO 410 J1=1,NLON
         IP = J1
         DO 420 J2=1,NLAT
            IF ( ABS(R4_2D_MAP(J1,NLAT+1-J2) - FILL) < ABS(FILL)*EPS ) THEN
                 MAP_I4(LOCC(J1,J2,NLON)) = 0
                 GOTO 420
            END IF
!
! --------- Find minimal and maximal value
!
            IF ( MAP_I4(LOCC(IP,J2,NLON)) .LT. IVAL_MIN ) THEN
                 IVAL_MIN = MAP_I4(LOCC(IP,J2,NLON))
            END IF
!
            IF ( MAP_I4(LOCC(IP,J2,NLON)) .GT. IVAL_MAX ) THEN
                 IVAL_MAX = MAP_I4(LOCC(IP,J2,NLON))
            END IF
!
            IF ( R4_2D_MAP(J1,NLAT+1-J2) .GT. RVAL_MAX ) THEN
                 RVAL_MAX = R4_2D_MAP(J1,NLAT+1-J2)
            END IF
            IF ( R4_2D_MAP(J1,NLAT+1-J2) .LT. RVAL_MIN ) THEN
                 RVAL_MIN = R4_2D_MAP(J1,NLAT+1-J2)
            END IF
 420     CONTINUE
 410  CONTINUE
!
      RVAL_MIN = 0.0
      IF ( RVAL_USE_MAX > -0.5D0 ) RVAL_MAX = RVAL_USE_MAX 
!!  write ( 6, * ) 'rval_min= ', rval_min, ' rval_max= ', rval_max
      IVAL_MAX = RVAL_MAX*100
!
! --- Open plotting device and set up coordinate system.
!
      IF ( IDEV .EQ. 1 ) THEN
           IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( IDEV .EQ. 2 ) THEN
           FINAM = FINAM(1:I_LEN(FINAM))//PS_DIAGI//'/CPS'
           IF ( PGBEG ( 0, FINAM, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( IDEV .EQ. 3 ) THEN
           FINAM = FINAM(1:I_LEN(FINAM))//GIF_DIAGI//'/GIF'
           IF ( PGBEG ( 0, FINAM, 1, 1 ) .NE. 1 ) STOP
      END IF
!
      IF ( IDEV .EQ. 1 ) THEN
           PAP_SIZE = 360.0
         ELSE IF ( IDEV .EQ. 2 ) THEN
           PAP_SIZE = 270.0
         ELSE IF ( IDEV .EQ. 4 ) THEN
           PAP_SIZE = 150.0
         ELSE
           PAP_SIZE = 240.0
      END IF
      CALL PGPAP  ( PAP_SIZE/25.4, XCOEF_PAP )
!
! --- Set parameters of the plotting window
!
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN  ( -180.0, 180.0, -90.0, 90.0 )
           CALL PGSVP   ( 0.18, 0.97, 0.10, 0.90  ) ! makes fields for labels
         ELSE 
           CALL PGSVP   (  0.05, 0.95, 0.15, 0.90 ) ! makes fields for labels
           CALL PGSWIN  ( -1.0,  1.0, -1.0,  1.0  )
      END IF
      CALL PGSCR   ( 0, 1.0, 1.0, 1.0 ) ! pure white background
!
! --- Learn the number of colors available for this device
!
      CALL PGQCOL  ( CI1, CI2 )
      IF ( CI2.LT. INIT_COL+NCOL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INIT_COL+NCOL, STR )
           CALL ERR_LOG ( 6358, IUER, 'ATM3_PLOT', 'This program requires '// &
     &         'a device with at least '//STR(1:I_LEN(STR))//' colors' )
           RETURN
      END IF
!
! --- Set white color
!
      IF ( IDEV .EQ. 1 ) THEN
!
! -------- ... it is a little bit dimer for the interactive device
!
           CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
           WHITE_CLR = BCG_CLR(1)
         ELSE
           WHITE_CLR = 255.0
           CALL PGCOL_RGB ( BCG_CLRI, 255, 255, 255 )
      END IF
!
! --- Set up a color palette using NCOL indices from INIT_COL to INIT_COL+NCOL.
!
      CALL PGCOL_RGB ( FRG_CLRI, FRG_CLR(1), FRG_CLR(2), FRG_CLR(3) )
      CALL DEFINE_PALETTE ( IPAL, IDEV, NCOL, INIT_COL )
!
! --- Transform the map from [min, max] range to [1, NCOL] range
!
      CALL NOUT ( SIZEOF(MAP_I4), MAP_I4 ) 
      DO 430 J3=1,NLON
         IF ( IPRC == 2 ) THEN
              XPR = -1.0 + (J3-1)*2.0/(NLON-1)
         END IF
         DO 440 J4=1,NLAT
            IF ( IPRC == 1 ) THEN
                 IF ( ABS(R4_2D_MAP(J3,NLAT+1-J4) - FILL) < ABS(FILL)*EPS ) THEN
                      MAP_I4(LOCC(J3,J4,NLON)) = BCG_CLRI
                    ELSE 
                      MAP_I4(LOCC(J3,J4,NLON)) = &
     &                       ( R4_2D_MAP(J3,NLAT+1-J4) - RVAL_MIN )/ &
     &                       (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                 END IF
               ELSE IF ( IPRC == 2 ) THEN
                 MAP_I4(LOCC(J3,J4,NLON)) = BCG_CLRI
                 YPR = -1.0 + (NLAT+1-J4)*2.0/(NLAT-1)
                 CALL INV_HAMMER_TRANS ( XPR, YPR, XCOEF_IMA, PHI, LAM )
                 PHI = PHI*180.0/PI__NUM
                 LAM = LAM*180.0/PI__NUM + 180.0
                 IF ( LAM > 360.0 ) LAM = LAM - 360.0
                 IF ( LAM < 0.0   ) LAM = LAM + 360.0
                 IF ( PHI < -99.9 ) GOTO 440
!
                 IX = INT ( (LAM/360.0)*NLON + 1.E-5 ) + 1
                 IF ( IX == 0  ) IX = NLON
                 IY = INT ( (PHI+90.0)/180.0*(NLAT-1) + 1.E-5 ) + 1
!
                 IF ( IY == 0  ) IY = 1
!
                 IF ( ABS(R4_2D_MAP(IX,IY) - FILL) < ABS(FILL)*EPS ) THEN
                      MAP_I4(LOCC(J3,J4,NLON)) = BCG_CLRI
                      GOTO 440
                    ELSE 
                      IF ( (RVAL_MAX - RVAL_MIN) < 1.E-7 ) THEN
                           MAP_I4(LOCC(J3,J4,NLON)) = INIT_COL
                        ELSE 
                           MAP_I4(LOCC(J3,J4,NLON)) = &
     &                           ( R4_2D_MAP(IX,IY) - RVAL_MIN )/ &
     &                           (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                      END IF

                      IF ( MAP_I4(LOCC(J3,J4,NLON)) < INIT_COL ) THEN
                           MAP_I4(LOCC(J3,J4,NLON)) = INIT_COL  
                      END IF
                      IF ( MAP_I4(LOCC(J3,J4,NLON)) > INIT_COL + NCOL ) THEN
!!                           MAP_I4(LOCC(J3,J4,NLON)) = INIT_COL + NCOL
                           MAP_I4(LOCC(J3,J4,NLON)) = INIT_COL + NCOL - 1
                      END IF
                END IF
            END IF
 440     CONTINUE
 430  CONTINUE
!
! --- Plot the 2D image
!
      IF ( IPRC == 1 ) THEN
           CALL PGPIXL ( MAP_I4, NLON, NLAT, 1, NLON, 1, NLAT, &
     &                   LONB_R4, LONE_R4, LATB_R4, LATE_R4 )
         ELSE IF ( IPRC == 2 ) THEN
           CALL PGPIXL ( MAP_I4, NLON, NLAT, 1, NLON, 1, NLAT, &
     &                   -1.0, 1.0, -1.0, 1.0 )
!
           DO 450 J5=1,NLAT
              PHI = -P2I + (J5-1)*PI__NUM/(NLAT-1)
              LAM = PI__NUM
              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J5), ARR_Y(J5) )
              ARR_Y(J5) = ARR_Y(J5) + 1.0/NLAT
              IF ( ARR_Y(J5) > 1.0 ) ARR_Y(J5) = 1.0
 450       CONTINUE 
           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
!
           DO 460 J6=1,NLAT
              PHI = -P2I + (J6-1)*PI__NUM/(NLAT-1)
              LAM = -PI__NUM
              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J6), ARR_Y(J6) )
              ARR_Y(J6) = ARR_Y(J6) + 1.0/NLAT
              IF ( ARR_Y(J6) > 1.0 ) ARR_Y(J6) = 1.0
 460       CONTINUE 
           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
      END IF
!
! --- Print title
!
      CALL PGSCH  ( 2.0 )
      CALL PGSCF  ( 2   )
      CALL PGSLW  ( 5 )
      CALL PGMTXT ( 't', 0.5, 0.5, 0.5, TITLE(1:I_LEN(TITLE)) )
      CALL PGSCH  ( 1.0 )
      CALL PGSCF  ( 1   )
      CALL PGSLW  ( 1 )
      IF ( IPRC == 1 ) THEN
!
! -------- Plot the box around the image
!
           CALL PGSCI  ( 1 )
           CALL PGBOX  ( 'bicnts', 30.0, 3, 'bicnts', 20.0, 2 )
!
! -------- Print the annotation
!
           CALL PGSLW  ( 4 )
           CALL PGSCH  ( 1.5 )
           CALL PGMTXT ( 'b', 1.9, 1.0, 1.0, 'longitude' )
           CALL PGMTXT ( 'l', 1.5, 1.0, 1.0, 'latitude' )
      END IF
!
! --- Printing the left column with scale
!
      IF ( IPRC == 1 ) THEN
           XMIN = 0.08
           XMAX = 0.11
           YMIN = 0.08
           YMAX = 0.92
         ELSE IF ( IPRC == 2 ) THEN
           XMIN = 0.08
           XMAX = 0.92
           YMIN = 0.06
           YMAX = 0.10
      END IF
      CALL PGSVP  ( XMIN, XMAX, YMIN, YMAX  )
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*NCOL )
         ELSE  IF ( IPRC == 2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*NCOL, YMIN, YMAX )
      END IF
      IF ( IDEV .EQ. 1 ) THEN
           CALL PGSCH  ( 1.0 )
         ELSE
           CALL PGSCH  ( 0.7 )
      END IF
      CALL PGSFS  ( 1   )
      CALL PGSLW  ( 2   )
!
      DO 470 J7=1,NCOL
         IF ( IPRC == 1 ) THEN
              XL = XMIN
              YB = J7
              XR = XMAX
              YT = (J7-1)
            ELSE IF ( IPRC == 2 ) THEN
              XL = J7-1 
              YB = YMIN
              XR = J7 
              YT = YMAX
         END IF
!
! ------ Make a box
!
         CALL PGSCI  ( J7-1+INIT_COL )
         CALL PGSFS  ( 1 )
         CALL PGRECT ( XL, XR, YB, YT )
 470  CONTINUE
!
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*(NLAB-1) )
         ELSE IF ( IPRC == 2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
      END IF
!
      DO 480 J8=1,NLAB
         WRITE ( UNIT=STR(1:5), FMT='(F5.1)' ) &
     &           RVAL_MIN + (RVAL_MAX-RVAL_MIN)/FLOAT(NLAB-1)*(J8-1)
         CALL CHASHR ( STR(1:5) )
!
! ------- Write annotation near the box
!
         CALL PGSCI  ( 1 )
         IF ( IPRC == 1 ) THEN
              XL = XMIN
              YB = FLOAT(J8-1)
              CALL PGPTXT ( XL*0.9, YB, 0.0, 1.0, STR(1:5) )
            ELSE IF ( IPRC == 2 ) THEN
              XL = FLOAT(J8-1) 
              YB = YMAX
              CALL PGPTXT ( XL, YB*1.1, 0.0, 0.5, STR(1:5) )
         END IF
 480  CONTINUE 
!
! --- Write units of the annotation
!
      CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*(NLAB-1) )
!
! --- ... and the box around the scale
!
      CALL PGSCI ( 1 )
      CALL PGSFS ( 2 )
      CALL PGSLW ( 2 )
      CALL PGRECT ( XMIN+0.0001, XMAX-0.0001, 0.0, 1.0*(NLAB-1)-0.001 )
!
      CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0 )
      CALL PGSLW  ( 4 )
      CALL PGSCH  ( 1.4 )
      CALL PGMTXT ( 'b', 0.9, 1.0, 1.0, 'TECU' )
!
      IF ( IDEV .EQ. 1 ) THEN
           CALL PGSCH  ( 1.0 )
           CALL PGPTXT ( 0.01, -1.4, 0.0, 0.0, 'Hit <CNTRL/P> for printing, '// &
     &         'P - for making ps-file, G - for gif-file, other key to exit' )
      END IF
!
      XC = 0.95
      YC = 0.0
!
      IF ( IDEV .EQ. 1 ) THEN
!
! -------- Waith for a user to hit a key
!
           CALL PGCURS ( XC, YC, CH )
           CALL TRAN ( 11, CH, CH )
           CALL PGCLOQ()
!
! -------- Check the value of the key
!
           IF ( CH .EQ. CHAR(16) ) THEN
!
! ------------- Make the plot in ps format and then print
!
                IDEV = 2
                FL_PLOT = .TRUE.
                GOTO 910
              ELSE IF ( CH .EQ. 'P' ) THEN
!
! ------------- Make the polt in postscript format
!
                IDEV = 2
                GOTO 910
              ELSE IF ( CH .EQ. 'G' ) THEN
!
! ------------- Make the plot in gif format
!
                IDEV = 3
                GOTO 910
           END IF
         ELSE
           CALL PGCLOQ()  ! quit pgplot
           WRITE ( 6, * ) 'Plot is written in file '//FINAM(1:I_LEN(FINAM)-4)
           IF ( FL_PLOT ) THEN
!
! ------------- Aga. A user wants the plot to be printed
!
                CALL GETENVAR ( 'DIAGI_PRICOM', STR )
                IF ( ILEN(STR) .GT. 0 ) THEN
!
! ------------------ Set the command for printing
!
                     STR = STR(1:I_LEN(STR))//' '// &
     &                      FINAM(1:I_LEN(FINAM)-4)//' &'
                     WRITE ( 6, * ) 'Plot is being sent to printer ...'
                     CALL SYSTEM ( STR(1:I_LEN(STR))//CHAR(0) )
                     WRITE ( 6, * ) ' STR >>',STR(1:I_LEN(STR)),'<< '
                     WRITE ( 6, * ) 'Plot is sent to printer'
                END IF
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VIONO_GRID_PLOT  !#!#
