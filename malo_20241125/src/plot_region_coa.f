      SUBROUTINE PLOT_REGION_COA_R4 ( IDEV, IPAL, IPRC, ILAND, NLON, NLAT, &
     &                ARR_R4, ARR_COA_R4, ARR_COA_I1, TITLE, UNIT, FILOUT, &
     &                LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &                VAL_MIN, VAL_MAX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PLOT_REGION_COA_R4 
! *                                                                      *
! * ## 09-OCT-2012 PLOT_REGION_COA_R4 v2.1 (c) L. Petrov 25-JAN-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'diagi.i'
      INTEGER*4  IDEV, IPAL, IPRC, ILAND, NLON, NLAT, IUER
      REAL*4     LON_MIN, LON_MAX, LAT_MIN, LAT_MAX
      REAL*4     VAL_MIN, VAL_MAX
      CHARACTER  TITLE*(*), UNIT*(*), FILOUT*(*)
      CHARACTER  UFILOUT*128, STR_RVAL_MIN*20, STR_RVAL_MAX*20
      REAL*4     ARR_R4(NLON,NLAT), ARR_COA_R4(NLON,NLAT)
      INTEGER*1  ARR_COA_I1(NLON,NLAT)
      INTEGER*4  INIT_COL, NCOL, NLAB, IVAL_MIN, IVAL_MAX
      INTEGER*4  MLON, MLAT
!      PARAMETER  ( MLON = 2048 )
!      PARAMETER  ( MLAT = 2048 )
      PARAMETER  ( MLON = 4096 )
      PARAMETER  ( MLAT = 4096 )
      INTEGER*4, ALLOCATABLE :: MAP_I4(:,:)
      CHARACTER  STR*128, FINAM*128, CH*4
      REAL*4     XCOEF_IMA, XCOEF_PAP, LONB_R4, LONE_R4, LATB_R4, LATE_R4, &
     &           PAP_SIZE, WHITE_CLR, XC, YC, XPR, YPR, &
     &           PHI, LAM, &
     &           XMIN, XMAX, YMIN, YMAX, XL, XR, YB, YT, &
     &           LON_DEG, LAT_DEG, LON_TICK, LAT_TICK, VAL_MAX_ALLOWED
      REAL*4     RVAL_MIN, RVAL_MAX, VAL_R4, ARR_X(8192), ARR_Y(8192)
      INTEGER*4  NLAT_TICKS_MIN, NLON_TICKS_MIN, NLON_TICKS_MAX, NLAT_TICKS_MAX
      REAL*4     EPS, DIST_MODE_TICK
      PARAMETER  ( EPS = 1.E-6 )
      PARAMETER  ( NLAT_TICKS_MIN =  3   )
      PARAMETER  ( NLON_TICKS_MIN =  3   )
      PARAMETER  ( NLAT_TICKS_MAX =  5   )
      PARAMETER  ( NLON_TICKS_MAX = 10   )
      PARAMETER  ( DIST_MODE_TICK =  5.0 )
      LOGICAL*4  FL_PLOT
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, NUM, IP, CI1, CI2, &
     &           IX, IY, ID, IND_LON, IND_LAT, ILON, UDEV
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: PGBEG, I_LEN, ILEN, LINDEX
!
      VAL_MAX_ALLOWED = 0.99E+14
      UDEV = IDEV
      INIT_COL = 20
      NCOL = 200
      IF ( IPRC == 1 ) THEN
           NLAB = 16
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           NLAB =  5
      END IF 
!
      LONB_R4 = LON_MIN
      LONE_R4 = LON_MAX
      LATB_R4 = LAT_MIN
      LATE_R4 = LAT_MAX
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
      ALLOCATE ( MAP_I4(MLON,MLAT) )
      MAP_I4 = -9999
      DO 410 J1=1,NLAT
         IP = J1
         DO 420 J2=1,NLON
!
! --------- Find minimal and maximal value
!
            IF ( ABS(ARR_R4(J2,J1)) > VAL_MAX_ALLOWED ) GOTO 420
            IF ( ARR_R4(J2,J1) < RVAL_MIN ) THEN
                 RVAL_MIN = ARR_R4(J2,J1) 
            END IF
!
            IF ( ARR_R4(J2,J1) > RVAL_MAX ) THEN
                 RVAL_MAX = ARR_R4(J2,J1) 
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( VAL_MAX > VAL_MIN ) THEN
           RVAL_MIN = VAL_MIN
           RVAL_MAX = VAL_MAX
      END IF
      IF ( ( RVAL_MAX - RVAL_MIN) < EPS ) THEN
             RVAL_MAX = RVAL_MIN + MAX ( EPS, EPS*RVAL_MIN )
      END IF
!
      CALL GETENVAR ( 'MALO_RVAL_MIN', STR_RVAL_MIN )
      IF ( ILEN(STR_RVAL_MIN) > 0 ) THEN
           IF ( INDEX ( STR_RVAL_MIN, '.' ) .LE. 0 ) THEN
                STR_RVAL_MIN = STR_RVAL_MIN(1:I_LEN(STR_RVAL_MIN))//'.0'
           END IF
           READ ( UNIT=STR_RVAL_MIN, FMT='(F20.10)' ) RVAL_MIN
      END IF
!
      CALL GETENVAR ( 'MALO_RVAL_MAX', STR_RVAL_MAX )
      IF ( ILEN(STR_RVAL_MAX) > 0 ) THEN
           IF ( INDEX ( STR_RVAL_MAX, '.' ) .LE. 0 ) THEN
                STR_RVAL_MAX = STR_RVAL_MAX(1:I_LEN(STR_RVAL_MAX))//'.0'
           END IF
           READ ( UNIT=STR_RVAL_MAX, FMT='(F20.10)' ) RVAL_MAX
      END IF
 910  CONTINUE
!
! --- Open plotting device and set up coordinate system.
!
      UFILOUT = FILOUT
      IF ( UDEV .EQ. 1 ) THEN
           IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 2 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//PS_DIAGI//'/CPS'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 3 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//GIF_DIAGI//'/GIF'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 4 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//PS_DIAGI//'/VCPS'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
      END IF
!
      IF ( UDEV .EQ. 1 ) THEN
           PAP_SIZE = 400.0 
         ELSE IF ( UDEV .EQ. 2 ) THEN
           PAP_SIZE = 270.0
         ELSE IF ( UDEV .EQ. 3 ) THEN
           PAP_SIZE = 400.0
         ELSE IF ( UDEV .EQ. 4 ) THEN
           PAP_SIZE = 600.0
         ELSE
           PAP_SIZE = 240.0
      END IF
      CALL PGPAP  ( PAP_SIZE/25.4, XCOEF_PAP )
!
! --- Set parameters of the plotting window
!
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN  ( LONB_R4, LONE_R4, LATB_R4, LATE_R4 )
           CALL PGSVP   ( 0.17, 0.97, 0.10, 0.90  ) ! makes fields for labels
         ELSE 
           CALL PGSVP   (  0.05, 0.95, 0.15, 0.90 ) ! makes fields for labels
           CALL PGSWIN  ( -1.0,  1.0, -1.1,  1.0  )
      END IF
      CALL PGSCR   ( 0, 1.0, 1.0, 1.0 ) ! pure white background
!
! --- Learn the number of colors available for this device
!
      CALL PGQCOL  ( CI1, CI2 )
      IF ( CI2 .LT. INIT_COL+NCOL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INIT_COL+NCOL, STR )
           CALL ERR_LOG ( 6230, IUER, 'PLOT_REGION_R4', 'This program '// &
     &         'requires a device with at least '// &
     &          STR(1:I_LEN(STR))//' colors' )
           RETURN
      END IF
!
! --- Set white color
!
      IF ( UDEV .EQ. 1 ) THEN
!
! -------- ... it is a little bit dimmer for the interactive device
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
      CALL DEFINE_PALETTE ( IPAL, UDEV, NCOL, INIT_COL )
!
! --- Transform the map from [min, max] range to [1, NCOL] range
!
      CALL NOUT ( SIZEOF(MAP_I4), MAP_I4 ) 
      NUM = 0
!
      DO 430 J3=1,MLON
         IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
              XPR = -1.0 + (J3-1)*2.0/(NLON-1)
         END IF
         LON_DEG = LONB_R4 + (J3-1)*(LONE_R4 - LONB_R4)/(MLON-1)
         IND_LON = 1 + NLON*(LON_DEG/360.0)
         IF ( IND_LON > NLON ) IND_LON = IND_LON - NLON
         IF ( IND_LON <    1 ) IND_LON = IND_LON + NLON
         DO 440 J4=1,MLAT
            LAT_DEG = LATB_R4 + (J4-1)*(LATE_R4 - LATB_R4)/(MLAT-1)
            IND_LAT = 1 + (NLAT-1)*(LAT_DEG + 90.0)/180.0
            IF ( IND_LAT < 1    ) IND_LAT = 1
            IF ( IND_LAT > NLAT ) IND_LAT = NLAT
            MAP_I4(J3,J4) = BCG_CLRI
            IF ( IPRC == 1 ) THEN
                 VAL_R4 = ARR_R4(IND_LON,IND_LAT)
                 IF ( VAL_R4 < RVAL_MIN ) VAL_R4 = RVAL_MIN
                 IF ( VAL_R4 > RVAL_MAX ) VAL_R4 = RVAL_MAX
                 MAP_I4(J3,J4) = (VAL_R4 - RVAL_MIN)/(RVAL_MAX - RVAL_MIN)* &
     &                           (NCOL-1) + INIT_COL
                 IF ( LOC(ARR_COA_R4) .NE. 0 ) THEN
                      IF ( ILAND == 0 ) THEN
                           IF ( ARR_COA_R4(IND_LON,IND_LAT) > 0.1 ) THEN
!!                                MAP_I4(J3,J4) = BCG_CLRI
                                MAP_I4(J3,J4) = FRG_CLRI
                           END IF
                         ELSE IF ( ILAND == 1 ) THEN
                           IF ( ARR_COA_R4(IND_LON,IND_LAT) < 0.5 ) THEN
                                MAP_I4(J3,J4) = BCG_CLRI
                           END IF
                      END IF
                    ELSE 
                      IF ( ILAND == 0 ) THEN
                           IF ( ARR_COA_I1(IND_LON,IND_LAT) == 1 ) THEN
                                MAP_I4(J3,J4) = BCG_CLRI
                           END IF
                         ELSE IF ( ILAND == 1 ) THEN
                           IF ( ARR_COA_I1(IND_LON,IND_LAT) == 0 ) THEN
                                MAP_I4(J3,J4) = BCG_CLRI
                           END IF
                      END IF
                 END IF
               ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
                 YPR = -1.0 + J4*2.0/(NLAT-1)
                 CALL INV_HAMMER_TRANS ( XPR, YPR, XCOEF_IMA, PHI, LAM )
                 PHI = PHI*180.0/PI__NUM
                 IF ( IPRC == 2 ) THEN
                      LAM = LAM*180.0/PI__NUM + 180.0
                    ELSE IF ( IPRC == -2 ) THEN
                      LAM = LAM*180.0/PI__NUM
                 END IF
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
                 VAL_R4 = ARR_R4(IX,IY)
                 IF ( VAL_R4 < RVAL_MIN ) VAL_R4 = RVAL_MIN
                 IF ( VAL_R4 > RVAL_MAX ) VAL_R4 = RVAL_MAX
                 
                 MAP_I4(J3,J4) = ( VAL_R4 - RVAL_MIN )/ &
     &                           (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                 IF ( LOC(ARR_COA_R4) .NE. 0 ) THEN
                      IF ( ARR_COA_R4(IX,IY) > 0.5 ) THEN
                           MAP_I4(J3,J4) = BCG_CLRI
                      END IF
                    ELSE
                      IF ( ARR_COA_I1(IX,IY) == 1 ) THEN
                           MAP_I4(J3,J4) = BCG_CLRI
                      END IF
                 END IF
            END IF
 440     CONTINUE 
 430  CONTINUE 
      IF ( IPRC == 1 ) THEN
           CALL PGPIXL ( MAP_I4, MLON, MLAT, 1, MLON, 1, MLAT, &
     &                   LONB_R4, LONE_R4, LATB_R4, LATE_R4 )
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGPIXL ( MAP_I4, MLON, MLAT, 1, MLON, 1, MLAT, &
     &                   -1.0, 1.0, -1.0, 1.0 )
!
           DO 450 J5=1,NLAT
              PHI = -P2I + (J5-1)*PI__NUM/(NLAT-1)
              LAM = PI__NUM
              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J5), ARR_Y(J5) )
              ARR_Y(J5) = ARR_Y(J5) - 2.0/(NLAT-1)
              IF ( ARR_Y(J5) > 1.0 ) ARR_Y(J5) = 1.0
 450       CONTINUE 
           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
!
           DO 460 J6=1,NLAT
              PHI = -P2I + (J6-1)*PI__NUM/(NLAT-1)
              LAM = -PI__NUM
              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J6), ARR_Y(J6) )
              ARR_Y(J6) = ARR_Y(J6) - 2.0/(NLAT-1)
              IF ( ARR_Y(J6) > 1.0 ) ARR_Y(J6) = 1.0
 460       CONTINUE 
           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
      END IF
!
      CALL PGSCF  ( 2   )
      CALL PGSCH  ( 2.4 )
      CALL PGSLW  ( 10 )
      CALL PGSCI  ( 2 )
!
! --- Print title
!
      IF ( ILEN(TITLE) > 70 ) THEN
           CALL PGSCH  ( 0.9  )
         ELSE 
           CALL PGSCH  ( 1.44 )
      END IF
      CALL PGSCF  ( 2    )
      IF ( UDEV == 2 .OR. UDEV == 4 ) THEN
           CALL PGSLW  ( 2 )
         ELSE 
           CALL PGSLW  ( 5 )
      END IF
      CALL PGSCI  ( 1 )
!
      CALL PGSCH  ( 1.2 )
      CALL PGMTXT ( 't', 1.5, 0.5, 0.5, TITLE(1:I_LEN(TITLE)) )
      CALL PGSCH  ( 1.0 )
      CALL PGSCF  ( 1   )
      CALL PGSLW  ( 1 )
      IF ( IPRC == 1 ) THEN
!
! -------- Plot the box around the image
!
           CALL PGSCI  ( 1 )
           LON_TICK = 0.125
           DO 470 J7=1,16
              IF ( (LON_MAX - LON_MIN) > DIST_MODE_TICK ) THEN
                   IF ( (LON_MAX - LON_MIN)/LON_TICK > NLON_TICKS_MAX ) LON_TICK = 2*LON_TICK
                 ELSE 
                   IF ( (LON_MAX - LON_MIN)/LON_TICK > NLON_TICKS_MIN ) LON_TICK = 2*LON_TICK
              END IF
 470       CONTINUE 
           IF ( LON_TICK > 10.0 ) THEN
                LON_TICK = 10*NINT(LON_TICK/10.0)
             ELSE IF ( LON_TICK >  5.0 ) THEN
                LON_TICK = 5*NINT(LON_TICK/5.0)
           END IF
           LAT_TICK = 0.125
           DO 480 J8=1,16
              IF ( (LAT_MAX - LAT_MIN) > DIST_MODE_TICK ) THEN
                   IF ( (LAT_MAX - LAT_MIN)/LAT_TICK > NLON_TICKS_MAX ) LAT_TICK = 2*LAT_TICK
                 ELSE 
                   IF ( (LAT_MAX - LAT_MIN)/LAT_TICK > NLON_TICKS_MIN ) LAT_TICK = 2*LAT_TICK
              END IF
 480       CONTINUE 
           IF ( LAT_TICK > 10.0 ) THEN
                LAT_TICK = 10*NINT(LAT_TICK/10.0)
              ELSE IF ( LAT_TICK >  5.0 ) THEN
                LAT_TICK =  5*NINT(LAT_TICK/5.0)
           END IF
           CALL PGBOX  ( 'bicnts', LON_TICK, 2, 'bicnts', LAT_TICK, 2 )
!
! -------- Print the annotation
!
           CALL PGSLW  ( 4 )
           CALL PGSCH  ( 1.5 )
           CALL PGMTXT ( 'b', 1.90, 1.0, 1.0, 'longitude' )
           CALL PGMTXT ( 'l', 1.33, 1.0, 1.0, 'latitude' )
      END IF
!
! --- Printing the left column with scale
!
      IF ( IPRC == 1 ) THEN
           XMIN = 0.08
           XMAX = 0.11
           YMIN = 0.08
           YMAX = 0.92
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           XMIN = 0.08
           XMAX = 0.92
           YMIN = 0.06
           YMAX = 0.10
      END IF
      CALL PGSVP  ( XMIN, XMAX, YMIN, YMAX  ) !
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*NCOL )
         ELSE  IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*NCOL, YMIN, YMAX )
      END IF
      IF ( UDEV .EQ. 1 ) THEN
           CALL PGSCH  ( 1.0 )
         ELSE
           CALL PGSCH  ( 0.7 )
      END IF
      CALL PGSFS  ( 1   )
      CALL PGSLW  ( 2   )
!
      DO 490 J9=1,NCOL
         IF ( IPRC == 1 ) THEN
              XL = XMIN
              YB = J9
              XR = XMAX
              YT = (J9-1)
            ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
              XL = J9-1 
              YB = YMIN
              XR = J9 
              YT = YMAX
         END IF
!
! ------ Make a box
!
         CALL PGSCI  ( J9-1+INIT_COL )
         CALL PGSFS  ( 1 )
         CALL PGRECT ( XL, XR, YB, YT )
 490  CONTINUE
!
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*(NLAB-1) )
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
      END IF
!
      DO 4100 J10=1,NLAB
         VAL_R4 = RVAL_MIN + (RVAL_MAX-RVAL_MIN)/FLOAT(NLAB-1)*(J10-1)
         IF ( VAL_R4 .GE. 10000.0 .OR. VAL_R4 < -1000.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.0)' ) VAL_R4
            ELSE IF ( VAL_R4 .GE. 1000.0 .OR. VAL_R4 < -100.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.1)' ) VAL_R4
            ELSE IF ( VAL_R4 .GE. 100.0 .OR. VAL_R4 < -10.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.2)' ) VAL_R4
            ELSE 
              WRITE ( UNIT=STR(1:7), FMT='(F7.3)' ) VAL_R4
         END IF
         CALL CHASHR ( STR(1:7) )
!
! ------ Write annotation near the box
!
         CALL PGSCI  ( 1 )
         IF ( IPRC == 1 ) THEN
              XL = XMIN
              YB = FLOAT(J10-1)
              CALL PGPTXT ( XL*0.9, YB, 0.0, 1.0, STR(1:7) )
            ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
              XL = FLOAT(J10-1) 
              YB = YMAX
              CALL PGPTXT ( XL, YB*1.1, 0.0, 0.5, STR(1:7) )
         END IF
 4100 CONTINUE 
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
      IF ( UDEV .NE. 3 ) THEN
           CALL PGRECT ( XMIN+0.0001, XMAX-0.0001, 0.0, 1.0*(NLAB-1)-0.001 )
      END IF
!
      CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0 )
      CALL PGSLW  ( 4 )
      CALL PGSCH  ( 1.4 )
      CALL PGMTXT ( 'b', 0.9, 1.0, 1.0, UNIT(1:I_LEN(UNIT)) )
!
      IF ( UDEV .EQ. 1 ) THEN
           CALL PGSCH  ( 1.0 )
           CALL PGPTXT ( 0.01, -1.4, 0.0, 0.0, 'Hit <CNTRL/P> for printing, '// &
     &         'P - for making ps-file, G - for gif-file, other key to exit' )
      END IF
!
      XC = 0.95
      YC = 0.0
!
      IF ( UDEV .EQ. 1 ) THEN
!
! -------- Wait for a user to hit a key
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
                UDEV = 2
                FL_PLOT = .TRUE.
                GOTO 910
              ELSE IF ( CH .EQ. 'P' ) THEN
!
! ------------- Make the polt in postscript format
!
                UDEV = 2
                GOTO 910
              ELSE IF ( CH .EQ. 'G' ) THEN
!
! ------------- Make the plot in gif format
!
                UDEV = 3
                GOTO 910
           END IF
         ELSE
           CALL PGCLOQ()  ! quit pgplot
           ID = LINDEX ( UFILOUT, '/' ) - 1
           IF ( ID < 0 ) ID = ILEN(UFILOUT)
           WRITE ( 6, * ) 'Plot is written in file '//UFILOUT(1:ID)
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
     &                      UFILOUT(1:I_LEN(UFILOUT)-4)//' &'
                     WRITE ( 6, * ) 'Plot is being sent to printer ...'
                     CALL SYSTEM ( STR(1:I_LEN(STR))//CHAR(0) )
                     WRITE ( 6, * ) ' STR >>',STR(1:I_LEN(STR)),'<< '
                     WRITE ( 6, * ) 'Plot is sent to printer'
                END IF
           END IF
      END IF
!
      DEALLOCATE ( MAP_I4 ) 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PLOT_REGION_COA_R4  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PLOT_GRID_COA_R4 ( IDEV, IPAL, ISCL, IPRC, NLON, NLAT, &
     &                              ARR_R4, ARR_COA, TITLE, UNIT, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PLOT_GRID_COA_R4 
! *                                                                      *
! * ### 09-OCT-2012   PLOT_GRID_R4  v1.4 (c)  L. Petrov 11-FEB-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'diagi.i'
      INTEGER*4  IDEV, IPAL, ISCL, IPRC, NLON, NLAT, IUER
      CHARACTER  TITLE*(*), UNIT*(*), FILOUT*(*)
      CHARACTER  UFILOUT*128
      REAL*4     ARR_R4(NLON,NLAT), ARR_COA(NLON,NLAT)
      INTEGER*4  INIT_COL, NCOL, NLAB, IVAL_MIN, IVAL_MAX
      INTEGER*4, ALLOCATABLE :: MAP_I4(:,:)
      CHARACTER  STR*128, FINAM*128, CH*4, STR_RVAL_MIN*20, STR_RVAL_MAX*20
      REAL*4     XCOEF_IMA, XCOEF_PAP, LONB_R4, LONE_R4, LATB_R4, LATE_R4, &
     &           PAP_SIZE, WHITE_CLR, XC, YC, XPR, YPR, LON, LAT, &
     &           PHI, LAM, LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, &
     &           XMIN, XMAX, YMIN, YMAX, XL, XR, YB, YT, RD
      REAL*4     RVAL_MIN, RVAL_MAX, VAL_R4, ARR_X(8192), ARR_Y(8192)
      REAL*4     EPS
      REAL*4     RANGE(2)
      DATA       RANGE / -1.0E10, 1.0E10 /
      PARAMETER  ( EPS = 1.E-6 )
      LOGICAL*4  FL_PLOT
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, NUM, IP, CI1, CI2, &
     &           IX, IY, ID, ILON, UDEV, NR
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: PGBEG, I_LEN, ILEN, LINDEX
!
      UDEV = IDEV
      INIT_COL = 20
      NCOL = 200
      IF ( IPRC == 1 ) THEN
           NLAB = 16
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           NLAB =  5
         ELSE IF ( IPRC == 3 ) THEN
!
! -------- Southern pole
!
           LAT_MIN =  -P2I
           LAT_MAX = (-P2I + 30.0D0*DEG__TO__RAD )
           RD = COS(PI__NUM/4.0 - LAT_MAX/2.0)
!!           NR = 30.0D0*DEG__TO__RAD*(NLAT-1)*0.6225 ! Coefficient 0.6225 was found empirically to avoid mosiac
           NR = 30.0D0*DEG__TO__RAD*(NLAT-1)*0.58 ! Coefficient 0.6225 was found empirically to avoid mosiac
           NLAB =  7
         ELSE IF ( IPRC == 4 ) THEN
!
! -------- Northern pole
!
           LAT_MIN = (P2I - 30.0D0*DEG__TO__RAD )
           LAT_MAX =  P2I
           RD = SIN(PI__NUM/4.0 - LAT_MIN/2.0)
           NR = 30.0D0*DEG__TO__RAD*(NLAT-1)*0.6225 ! Coefficient 0.6225 was found empirically to avoid mosiac
           NLAB =  7
      END IF 
!
      LONB_R4 =    0.0
      LONE_R4 =  360.0
      LATB_R4 =  -90.0
      LATE_R4 =   90.0
!
      IVAL_MIN =  1.0E9
      IVAL_MAX = -1.0E9
      RVAL_MIN =  1.0E9
      RVAL_MAX = -1.0E9
      IF ( IPRC == 1 ) THEN
           XCOEF_IMA = 0.75
           XCOEF_PAP = 0.75
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           XCOEF_IMA = 0.5
           XCOEF_PAP = 0.75
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           XCOEF_IMA = 1.0
           XCOEF_PAP = 1.0
      END IF
      IF ( IPRC == 1 .OR. IPRC == 2 .OR. IPRC == 2 ) THEN
           ALLOCATE ( MAP_I4(NLON,NLAT) )
         ELSE IF ( IPRC == 3 .OR. IPRC == 4 ) THEN
           ALLOCATE ( MAP_I4(NR,NR) )
      END IF
      MAP_I4 = -9999
      DO 410 J1=1,NLAT
         IP = J1
         DO 420 J2=1,NLON
!
! --------- Find minimal and maximal value
!
            IF ( ARR_R4(J2,J1) > RANGE(1) .AND. ARR_R4(J2,J1) < RANGE(2) ) THEN
                 IF ( ARR_R4(J2,J1) < RVAL_MIN ) THEN
                      RVAL_MIN = ARR_R4(J2,J1) 
                 END IF
!
                 IF ( ARR_R4(J2,J1) > RVAL_MAX ) THEN
                      RVAL_MAX = ARR_R4(J2,J1) 
                 END IF
            END IF
 420     CONTINUE 
 410  CONTINUE 
!
      IF ( ISCL == 0 ) THEN
           WRITE ( 6, * ) 'PGR RVAL_MIN/RVAL_MAX = ', RVAL_MIN, RVAL_MAX
         ELSE IF ( ISCL == 1 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 1.0
         ELSE IF ( ISCL == 2 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 1.5
         ELSE IF ( ISCL == 3 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 2.0
         ELSE IF ( ISCL == 4 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 3.0
         ELSE IF ( ISCL == 5 ) THEN
           RVAL_MIN = 0.0
           RVAL_MAX = 4.0
         ELSE IF ( ISCL == 6 ) THEN
           RVAL_MIN =  0.0
           RVAL_MAX =  1.0
         ELSE IF ( ISCL == 7 ) THEN
           RVAL_MIN =   5.0
           RVAL_MAX = 120.0
         ELSE IF ( ISCL == 8 ) THEN
           RVAL_MIN =    0.0
           RVAL_MAX =  180.0
         ELSE IF ( ISCL == 10 ) THEN
           RVAL_MIN = -1.0
           RVAL_MAX =  1.0
         ELSE IF ( ISCL == 11 ) THEN
           RVAL_MIN =  0.0
           RVAL_MAX = 10.0
         ELSE IF ( ISCL == 12 ) THEN
           RVAL_MIN = -0.1
           RVAL_MAX =  0.1
         ELSE IF ( ISCL == 13 ) THEN
           RVAL_MIN =   0.0
           RVAL_MAX = 100.0
         ELSE IF ( ISCL == 14 ) THEN
           RVAL_MIN =   0.0
           RVAL_MAX = 1000.0
         ELSE IF ( ISCL == 15 ) THEN
           RVAL_MIN =   0.0
           RVAL_MAX =   0.2
         ELSE IF ( ISCL == 16 ) THEN
           RVAL_MIN =   0.0
           RVAL_MAX =   1.D9
         ELSE IF ( ISCL == 17 ) THEN
           RVAL_MIN =    0.0
           RVAL_MAX =   90.0
         ELSE IF ( ISCL == 18 ) THEN
           RVAL_MIN =  -180.0
           RVAL_MAX =   180.0
         ELSE IF ( ISCL == 19 ) THEN
           RVAL_MIN =     0.0
           RVAL_MAX =     0.5
         ELSE IF ( ISCL == 20 ) THEN
           RVAL_MIN =      480.
           RVAL_MAX =     1080.0
         ELSE IF ( ISCL == 21 ) THEN
           RVAL_MIN =     0.
           RVAL_MAX =    30.
         ELSE IF ( ISCL == 22 ) THEN
           RVAL_MIN =    240.0
           RVAL_MAX =    320.0
        ELSE IF ( ISCL == 23 ) THEN
           RVAL_MIN =    -3.0
           RVAL_MAX =     5.0
        ELSE IF ( ISCL == 24 ) THEN
           RVAL_MIN =     0.0
           RVAL_MAX =     7.0
        ELSE IF ( ISCL == 25 ) THEN
           RVAL_MIN =      0.0
           RVAL_MAX =     30.0
         ELSE IF ( ISCL == 26 ) THEN
           RVAL_MIN =    50000.0
           RVAL_MAX =   110000.0
         ELSE IF ( ISCL == 27 ) THEN
           RVAL_MIN =  -10000.0
           RVAL_MAX =   10000.0
         ELSE IF ( ISCL == 28 ) THEN
           RVAL_MIN =  -5000.0
           RVAL_MAX =   5000.0
         ELSE IF ( ISCL == 29 ) THEN
           RVAL_MIN =  -300.0
           RVAL_MAX =   1000.0
         ELSE IF ( ISCL == 30 ) THEN
           RVAL_MIN =  -30.0
           RVAL_MAX =   30.0
         ELSE IF ( ISCL == 31 ) THEN
           RVAL_MIN =  -10.0
           RVAL_MAX =   10.0
         ELSE IF ( ISCL == 32 ) THEN
           RVAL_MIN =  -0.05
           RVAL_MAX =   0.15
         ELSE IF ( ISCL == 33 ) THEN
           RVAL_MIN =  280.0
           RVAL_MAX =  300.0
         ELSE IF ( ISCL == 34 ) THEN
           RVAL_MIN =    0
           RVAL_MAX =  500.0
         ELSE IF ( ISCL == 35 ) THEN
           RVAL_MIN =    0.0
           RVAL_MAX =    25000.0
         ELSE IF ( ISCL == 36 ) THEN
           RVAL_MIN =    -15.
           RVAL_MAX =     15.
         ELSE IF ( ISCL == 37 ) THEN
           RVAL_MIN =    -20.
           RVAL_MAX =     20.
         ELSE IF ( ISCL == 38 ) THEN
           RVAL_MIN =    -25.
           RVAL_MAX =     25.
         ELSE IF ( ISCL == 39 ) THEN
           RVAL_MIN =    -200.
           RVAL_MAX =     200.
         ELSE IF ( ISCL == 40 ) THEN
           RVAL_MIN =    -2000.
           RVAL_MAX =     2000.
         ELSE IF ( ISCL == 41 ) THEN
           RVAL_MIN =    -100.
           RVAL_MAX =     100.
         ELSE IF ( ISCL == 42 ) THEN
           RVAL_MIN =    -50.
           RVAL_MAX =     50.
      END IF
      IF ( ( RVAL_MAX - RVAL_MIN) < EPS ) THEN
             RVAL_MAX = RVAL_MIN + MAX ( EPS, EPS*RVAL_MIN )
      END IF
!
      CALL GETENVAR ( 'MALO_RVAL_MIN', STR_RVAL_MIN )
      IF ( ILEN(STR_RVAL_MIN) > 0 ) THEN
           IF ( INDEX ( STR_RVAL_MIN, '.' ) .LE. 0 ) THEN
                STR_RVAL_MIN = STR_RVAL_MIN(1:I_LEN(STR_RVAL_MIN))//'.0'
           END IF
           READ ( UNIT=STR_RVAL_MIN, FMT='(F20.10)' ) RVAL_MIN
      END IF
!
      CALL GETENVAR ( 'MALO_RVAL_MAX', STR_RVAL_MAX )
      IF ( ILEN(STR_RVAL_MAX) > 0 ) THEN
           IF ( INDEX ( STR_RVAL_MAX, '.' ) .LE. 0 ) THEN
                STR_RVAL_MAX = STR_RVAL_MAX(1:I_LEN(STR_RVAL_MAX))//'.0'
           END IF
           READ ( UNIT=STR_RVAL_MAX, FMT='(F20.10)' ) RVAL_MAX
      END IF
 910  CONTINUE
!
! --- Open plotting device and set up coordinate system.
!
      UFILOUT = FILOUT
      IF ( UDEV .EQ. 1 ) THEN
           IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 2 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//PS_DIAGI//'/CPS'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 3 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//GIF_DIAGI//'/GIF'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 4 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//PS_DIAGI//'/VCPS'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
      END IF
!
      IF ( UDEV .EQ. 1 ) THEN
           IF ( XCOEF_PAP < 1 ) THEN
                PAP_SIZE = 400.0 
              ELSE 
                PAP_SIZE = 300.0 
           END IF
         ELSE IF ( UDEV .EQ. 2 ) THEN
           PAP_SIZE = 270.0
         ELSE IF ( UDEV .EQ. 3 ) THEN
           PAP_SIZE = 400.0
         ELSE IF ( UDEV .EQ. 4 ) THEN
           PAP_SIZE = 600.0
         ELSE
           PAP_SIZE = 240.0
      END IF
      CALL PGPAP  ( PAP_SIZE/25.4, XCOEF_PAP )
!
! --- Set parameters of the plotting window
!
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN  ( LONB_R4, LONE_R4, LATB_R4, LATE_R4 )
           CALL PGSVP   ( 0.17, 0.97, 0.10, 0.90  ) ! makes fields for labels
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSVP   (  0.05, 0.95, 0.15, 0.90 ) ! makes fields for labels
           CALL PGSWIN  ( -1.0,  1.0, -1.1,  1.0  )
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           CALL PGSVP   (  0.15, 0.90, 0.15, 0.90 ) ! makes fields for labels
           CALL PGSWIN  ( -1.1,  1.0, -1.1,  1.0  )
      END IF
      CALL PGSCR   ( 0, 1.0, 1.0, 1.0 ) ! pure white background
!
! --- Learn the number of colors available for this device
!
      CALL PGQCOL  ( CI1, CI2 )
      IF ( CI2 .LT. INIT_COL+NCOL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INIT_COL+NCOL, STR )
           CALL ERR_LOG ( 6232, IUER, 'POLOT_GRID_R4', 'This program requires '// &
     &         'a device with at least '//STR(1:I_LEN(STR))//' colors' )
           RETURN
      END IF
!
! --- Set white color
!
      IF ( UDEV .EQ. 1 ) THEN
!
! -------- ... it is a little bit dimmer for the interactive device
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
      CALL DEFINE_PALETTE ( IPAL, UDEV, NCOL, INIT_COL )
!
! --- Transform the map from [min, max] range to [1, NCOL] range
!
      CALL NOUT ( SIZEOF(MAP_I4), MAP_I4 ) 
      NUM = 0
!
      DO 430 J3=1,NLON
         LON = (J3-1)*PI2/(NLON-1)
         IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
              XPR = -1.0 + (J3-1)*2.0/(NLON-1)
         END IF
         DO 440 J4=1,NLAT
            LAT = -P2I + (J4-1)*PI__NUM/(NLAT-1)
            IF ( IPRC == 1 ) THEN
                 VAL_R4 = ARR_R4(J3,J4)
                 IF ( VAL_R4 < RANGE(1) .OR. VAL_R4 > RANGE(2) ) THEN
                      MAP_I4(J3,J4) = BCG_CLRI
                    ELSE 
                      IF ( VAL_R4 < RVAL_MIN ) VAL_R4 = RVAL_MIN
                      IF ( VAL_R4 > RVAL_MAX ) VAL_R4 = RVAL_MAX
                      MAP_I4(J3,J4) = (VAL_R4 - RVAL_MIN)/(RVAL_MAX - RVAL_MIN)* &
     &                                (NCOL-1) + INIT_COL
                  END IF
                  IF ( ARR_COA(J3,J4) > 0.5 ) THEN
                       MAP_I4(J3,J4) = BCG_CLRI
                  END IF
               ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
                 MAP_I4(J3,J4) = BCG_CLRI
                 YPR = -1.0 + J4*2.0/(NLAT-1)
                 CALL INV_HAMMER_TRANS ( XPR, YPR, XCOEF_IMA, PHI, LAM )
                 PHI = PHI*180.0/PI__NUM
                 IF ( IPRC == 2 ) THEN
                      LAM = LAM*180.0/PI__NUM + 180.0
                    ELSE IF ( IPRC == -2 ) THEN
                      LAM = LAM*180.0/PI__NUM
                 END IF 
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
                 VAL_R4 = ARR_R4(IX,IY)
                 IF ( VAL_R4 < RANGE(1) .OR. VAL_R4 > RANGE(2) ) THEN
                      MAP_I4(J3,J4) = BCG_CLRI
                    ELSE 
                      IF ( VAL_R4 < RVAL_MIN ) VAL_R4 = RVAL_MIN
                      IF ( VAL_R4 > RVAL_MAX ) VAL_R4 = RVAL_MAX
                      MAP_I4(J3,J4) = ( VAL_R4 - RVAL_MIN )/ &
     &                                (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                 END IF
                 IF ( ARR_COA(IX,IY) > 0.5 ) THEN
                      MAP_I4(J3,J4) = BCG_CLRI
                 END IF
               ELSE IF ( IPRC == 3 ) THEN
                 VAL_R4 = ARR_R4(J3,J4)
                 IF ( LAT < LAT_MAX ) THEN
                      XPR = COS(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                      YPR = COS(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
                      IX = 1 + (XPR+1.0)/2*(NR-1)
                      IY = 1 + (YPR+1.0)/2*(NR-1)
                      IF ( IX < 1 .OR. IX > NR .OR. IY < 1 .OR. IY > NR ) GOTO 440
                      MAP_I4(IX,IY) = (  VAL_R4 - RVAL_MIN)/ &
     &                                (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                      IF ( ARR_COA(IX,IY) > 0.5 ) THEN
                           MAP_I4(J3,J4) = BCG_CLRI
                      END IF
                 END IF
               ELSE IF ( IPRC == 4 ) THEN
                 VAL_R4 = ARR_R4(J3,J4)
                 IF ( LAT > LAT_MIN ) THEN
                      XPR = SIN(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                      YPR = SIN(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
                      IX = 1 + (XPR+1.0)/2*(NR-1)
                      IY = 1 + (YPR+1.0)/2*(NR-1)
                      MAP_I4(IX,IY) = (  VAL_R4 - RVAL_MIN )/ &
     &                                (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
                      IF ( ARR_COA(IX,IY) > 0.5 ) THEN
                           MAP_I4(J3,J4) = BCG_CLRI
                      END IF
                 END IF
            END IF
 440     CONTINUE 
 430  CONTINUE 
      IF ( IPRC == 1 ) THEN
           CALL PGPIXL ( MAP_I4, NLON, NLAT, 1, NLON, 1, NLAT, &
     &                   LONB_R4, LONE_R4, LATB_R4, LATE_R4 )
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGPIXL ( MAP_I4, NLON, NLAT, 1, NLON, 1, NLAT, &
     &                   -1.0, 1.0, -1.0, 1.0 )
!
           DO 450 J5=1,NLAT
              PHI = -P2I + (J5-1)*PI__NUM/(NLAT-1)
              LAM = PI__NUM
              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J5), ARR_Y(J5) )
              ARR_Y(J5) = ARR_Y(J5) - 2.0/(NLAT-1)
              IF ( ARR_Y(J5) > 1.0 ) ARR_Y(J5) = 1.0
 450       CONTINUE 
           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
!
           DO 460 J6=1,NLAT
              PHI = -P2I + (J6-1)*PI__NUM/(NLAT-1)
              LAM = -PI__NUM
              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J6), ARR_Y(J6) )
              ARR_Y(J6) = ARR_Y(J6) - 2.0/(NLAT-1)
              IF ( ARR_Y(J6) > 1.0 ) ARR_Y(J6) = 1.0
 460       CONTINUE 
           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
         ELSE IF ( IPRC == 3 .OR. IPRC == 4 ) THEN
           CALL PGPIXL ( MAP_I4, NR, NR, 1, NR, 1, NR, -1.0, 1.0, -1.0, 1.0 )
      END IF
!
!
! --- Print title
!
      IF ( ILEN(TITLE) > 70 ) THEN
           CALL PGSCH  ( 0.9  )
         ELSE 
           CALL PGSCH  ( 1.44 )
      END IF
      CALL PGSCF  ( 2    )
      IF ( UDEV == 2 .OR. UDEV == 4 ) THEN
           CALL PGSLW  ( 2 )
         ELSE 
           CALL PGSLW  ( 5 )
      END IF
!
      CALL PGSCH  ( 1.2 )
      CALL PGMTXT ( 't', 1.5, 0.5, 0.5, TITLE(1:I_LEN(TITLE)) )
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
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           XMIN = 0.08
           XMAX = 0.92
           YMIN = 0.06
           YMAX = 0.10
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4      ) THEN
           XMIN = 0.08
           XMAX = 0.92
           YMIN = 0.06
           YMAX = 0.10
      END IF
      CALL PGSVP  ( XMIN, XMAX, YMIN, YMAX  ) !
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*NCOL )
         ELSE  IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*NCOL, YMIN, YMAX )
         ELSE  IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           CALL PGSWIN ( 0.0, 1.0*NCOL, YMIN, YMAX )
      END IF
      IF ( UDEV .EQ. 1 ) THEN
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
            ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
              XL = J7-1 
              YB = YMIN
              XR = J7 
              YT = YMAX
            ELSE IF ( IPRC == 3 .OR. IPRC == 4 ) THEN
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
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
      END IF
!
      DO 480 J8=1,NLAB
         VAL_R4 = RVAL_MIN + (RVAL_MAX-RVAL_MIN)/FLOAT(NLAB-1)*(J8-1)
         IF ( VAL_R4 .GE. 10000.0 .OR. VAL_R4 < -1000.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.0)' ) VAL_R4
            ELSE IF ( VAL_R4 .GE. 1000.0 .OR. VAL_R4 < -100.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.1)' ) VAL_R4
            ELSE IF ( VAL_R4 .GE. 100.0 .OR. VAL_R4 < -10.0 ) THEN
              WRITE ( UNIT=STR(1:7), FMT='(F7.2)' ) VAL_R4
            ELSE 
              WRITE ( UNIT=STR(1:7), FMT='(F7.3)' ) VAL_R4
         END IF
         CALL CHASHR ( STR(1:7) )
!
! ------- Write annotation near the box
!
         CALL PGSCI  ( 1 )
         IF ( IPRC == 1 ) THEN
              XL = XMIN
              YB = FLOAT(J8-1)
              CALL PGPTXT ( XL*0.9, YB, 0.0, 1.0, STR(1:6) )
            ELSE IF ( IPRC == 2 .OR. IPRC == -2 .OR. &
     &                IPRc == 3 .OR. IPRC ==  4      ) THEN
              XL = FLOAT(J8-1) 
              YB = YMAX
              CALL PGPTXT ( XL, YB*1.1, 0.0, 0.5, STR(1:6) )
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
      IF ( UDEV .NE. 3 ) THEN
           CALL PGRECT ( XMIN+0.0001, XMAX-0.0001, 0.0, 1.0*(NLAB-1)-0.001 )
      END IF
!
      CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0 )
      CALL PGSLW  ( 4 )
      CALL PGSCH  ( 1.4 )
      CALL PGMTXT ( 'b', 0.9, 1.0, 1.0, UNIT(1:I_LEN(UNIT)) )
!
      IF ( UDEV .EQ. 1 ) THEN
           CALL PGSCH  ( 1.0 )
           CALL PGPTXT ( 0.01, -1.4, 0.0, 0.0, 'Hit <CNTRL/P> for printing, '// &
     &         'P - for making ps-file, G - for gif-file, other key to exit' )
      END IF
!
      XC = 0.95
      YC = 0.0
!
      IF ( UDEV .EQ. 1 ) THEN
!
! -------- Wait for a user to hit a key
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
                UDEV = 2
                FL_PLOT = .TRUE.
                GOTO 910
              ELSE IF ( CH .EQ. 'P' ) THEN
!
! ------------- Make the polt in postscript format
!
                UDEV = 2
                GOTO 910
              ELSE IF ( CH .EQ. 'G' ) THEN
!
! ------------- Make the plot in gif format
!
                UDEV = 3
                GOTO 910
           END IF
         ELSE
           CALL PGCLOQ()  ! quit pgplot
           ID = LINDEX ( UFILOUT, '/' ) - 1
           IF ( ID < 0 ) ID = ILEN(UFILOUT)
           WRITE ( 6, * ) 'Plot is written in file '//UFILOUT(1:ID)
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
     &                      UFILOUT(1:I_LEN(UFILOUT)-4)//' &'
                     WRITE ( 6, * ) 'Plot is being sent to printer ...'
                     CALL SYSTEM ( STR(1:I_LEN(STR))//CHAR(0) )
                     WRITE ( 6, * ) ' STR >>',STR(1:I_LEN(STR)),'<< '
                     WRITE ( 6, * ) 'Plot is sent to printer'
                END IF
           END IF
      END IF
!
      DEALLOCATE ( MAP_I4 ) 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PLOT_GRID_COA_R4  !#!#
