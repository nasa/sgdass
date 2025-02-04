      SUBROUTINE PLOT_LEAP_R4 ( IDEV, IPAL, NLON, NLAT, ARR_R4, &
     &                          TITLE, UNIT, FILOUT, &
     &                          LAT_POLE, LAT_CIRC, &
     &                          VAL_MIN, VAL_MAX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PLOT_LEAP_R4
! *                                                                      *
! * ###  02-JUL-2013 PLOT_LEAP_R4   v1.0 (c)  L. Petrov 09-OCT-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'diagi.i'
      INTEGER*4  IDEV, IPAL, NLON, NLAT, NR, IUER
      REAL*4     LAT_POLE, LAT_CIRC
      REAL*4     VAL_MIN, VAL_MAX
      CHARACTER  TITLE*(*), UNIT*(*), FILOUT*(*)
      CHARACTER  UFILOUT*128
      REAL*4     ARR_R4(NLON,NLAT)
      INTEGER*4  INIT_COL, NCOL, NLAB, IVAL_MIN, IVAL_MAX
      INTEGER*4, ALLOCATABLE :: MAP_I4(:,:)
      CHARACTER  STR*128, FINAM*128, CH*4
      REAL*4     XCOEF_PAP, XMIN, XMAX, YMIN, YMAX, &
     &           PAP_SIZE, WHITE_CLR, XC, YC, XPR, YPR, &
     &           PHI, LAM, RD, XL, XR, YB, YT, &
     &           LON, LAT, LON_TICK, LAT_TICK, LAT_MIN, LAT_MAX
      REAL*4     RVAL_MIN, RVAL_MAX, VAL_R4, ARR_X(8192), ARR_Y(8192)
      INTEGER*4  NLON_TICKS_MAX, NLAT_TICKS_MAX
      REAL*4     EPS
      PARAMETER  ( EPS = 1.E-5 )
      PARAMETER  ( NLAT_TICKS_MAX =  5 )
      PARAMETER  ( NLON_TICKS_MAX = 10 )
      LOGICAL*4  FL_PLOT
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, NUM, IP, CI1, CI2, &
     &           IX, IY, ID, IND_LON, IND_LAT, ILON, UDEV, IPOL
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: PGBEG, I_LEN, ILEN, LINDEX
!
      UDEV = IDEV
      INIT_COL = 20
      NCOL = 200
      NLAB =  6
      NR = LAT_CIRC*NLAT*0.6225 ! Coefficient 0.6225 was found empirically to avoid mosiac
!!      NR = 445*LAT_CIRC/DEG__TO__RAD/10.0*NLAT/4096
!!           write ( 6, * ) ' k3= ', 445/DEG__TO__RAD/10.0/4096
!
      IF ( ABS(LAT_POLE - P2I) < EPS ) THEN
!
! -------- Northen pole 
!
           LAT_MIN = (P2I - LAT_CIRC)
           LAT_MAX =  P2I
           RD = SIN(PI__NUM/4.0 - LAT_MIN/2.0)
           IPOL =  1
         ELSE IF ( ABS(LAT_POLE + P2I) < EPS ) THEN
           LAT_MIN =  -P2I 
           LAT_MAX = (-P2I + LAT_CIRC)
           RD = COS(PI__NUM/4.0 - LAT_MAX/2.0)
           IPOL = -1
         ELSE 
           WRITE ( UNIT=STR(1:12), FMT='(1PE11.4)' ) LAT_CIRC
           CALL ERR_LOG ( 6371, IUER, 'PLOT_LEAP_R4', 'Wrong '// &
     &         'value of LAT_CIRC: either -pi/2 or pi/2 are expected' )
           RETURN
      END IF
!
      IVAL_MIN =  1.0E9
      IVAL_MAX = -1.0E9
      RVAL_MIN =  1.0E9
      RVAL_MAX = -1.0E9
      XCOEF_PAP = 1.0
      ALLOCATE ( MAP_I4(NR,NR) )
      MAP_I4 = -9999
      DO 410 J1=1,NLAT
         LAT = -P2I + (J1-1)*PI__NUM/(NLAT-1)
         IF ( IPOL == 1 ) THEN
              IF ( LAT < LAT_MIN ) GOTO 410
            ELSE IF ( IPOL == -1 ) THEN
              IF ( LAT > LAT_MAX ) GOTO 410
         END IF
         IP = J1
         DO 420 J2=1,NLON
!
! --------- Find minimal and maximal value
!
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
           PAP_SIZE = 300.0 
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
      CALL PGSWIN  ( -1.0, 1.0, -1.0, 1.0 )
      CALL PGSVP   ( 0.10, 0.90, 0.15, 0.95  ) ! makes fields for labels
      CALL PGSCR   ( 0, 1.0, 1.0, 1.0 ) ! pure white background
!
! --- Learn the number of colors available for this device
!
      CALL PGQCOL  ( CI1, CI2 )
      IF ( CI2 .LT. INIT_COL+NCOL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INIT_COL+NCOL, STR )
           CALL ERR_LOG ( 6372, IUER, 'PLOT_LEAP_R4', 'This program '// &
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
      DO 430 J3=1,NLON
         LON = (J3-1)*PI2/(NLON-1)
         DO 440 J4=1,NLAT
            LAT = -P2I + (J4-1)*PI__NUM/(NLAT-1)
            IF ( IPOL == 1 ) THEN
                 IF ( LAT < LAT_MIN ) GOTO 440
                 XPR = SIN(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                 YPR = SIN(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
               ELSE IF ( IPOL == -1 ) THEN
                 IF ( LAT > LAT_MAX ) GOTO 440
                 XPR = COS(PI__NUM/4.0 - LAT/2.0)*SIN(LON)/RD
                 YPR = COS(PI__NUM/4.0 - LAT/2.0)*COS(LON)/RD
            END IF
!
            IX = 1 + (XPR+1.0)/2*(NR-1)
            IY = 1 + (YPR+1.0)/2*(NR-1)
            VAL_R4 = ARR_R4(J3,J4)
            IF ( VAL_R4 < RVAL_MIN ) VAL_R4 = RVAL_MIN
            IF ( VAL_R4 > RVAL_MAX ) VAL_R4 = RVAL_MAX
!                 
            MAP_I4(IX,IY) = ( VAL_R4 - RVAL_MIN )/ &
     &                      (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
 440     CONTINUE 
 430  CONTINUE 
      CALL PGPIXL ( MAP_I4, NR, NR, 1, NR, 1, NR, &
     &              -1.0, 1.0, -1.0, 1.0 )
!
!           DO 450 J5=1,NLAT
!              PHI = -P2I + (J5-1)*PI__NUM/(NLAT-1)
!              LAM = PI__NUM
!              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J5), ARR_Y(J5) )
!              ARR_Y(J5) = ARR_Y(J5) - 2.0/(NLAT-1)
!              IF ( ARR_Y(J5) > 1.0 ) ARR_Y(J5) = 1.0
! 450       CONTINUE 
!           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
!!
!           DO 460 J6=1,NLAT
!              PHI = -P2I + (J6-1)*PI__NUM/(NLAT-1)
!!              LAM = -PI__NUM
!              CALL HAMMER_TRANS ( PHI, LAM, XCOEF_IMA, ARR_X(J6), ARR_Y(J6) )
!              ARR_Y(J6) = ARR_Y(J6) - 2.0/(NLAT-1)
!              IF ( ARR_Y(J6) > 1.0 ) ARR_Y(J6) = 1.0
! 460       CONTINUE 
!           CALL PGLINE ( NLAT, ARR_X, ARR_Y ) 
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
      CALL PGMTXT ( 't', 0.5, 0.5, 0.5, TITLE(1:I_LEN(TITLE)) )
      CALL PGSCH  ( 1.0 )
      CALL PGSCF  ( 1   )
      CALL PGSLW  ( 1 )
!@!
!@! -------- Plot the box around the image
!@!
!@           CALL PGSCI  ( 1 )
!@           LON_TICK = 0.1171875
!@           DO 470 J7=1,16
!@              IF ( (LON_MAX - LON_MIN)/LON_TICK > NLON_TICKS_MAX ) LON_TICK = 2*LON_TICK
!@ 470       CONTINUE 
!@           LAT_TICK = 0.1171875
!@           DO 480 J8=1,16
!@              IF ( (LAT_MAX - LAT_MIN)/LAT_TICK > NLON_TICKS_MAX ) LAT_TICK = 2*LAT_TICK
!@ 480       CONTINUE 
!@           CALL PGBOX  ( 'bicnts', LON_TICK, 2, 'bicnts', LAT_TICK, 2 )
!@!
!@! -------- Print the annotation
!@!
!@           CALL PGSLW  ( 4 )
!@           CALL PGSCH  ( 1.5 )
!@           CALL PGMTXT ( 'b', 1.90, 1.0, 1.0, 'longitude' )
!@           CALL PGMTXT ( 'l', 1.33, 1.0, 1.0, 'latitude' )
!@      END IF
!
! --- Printing the the bottom bar with scale
!
      XMIN = 0.08
      XMAX = 0.92
      YMIN = 0.06
      YMAX = 0.10
      CALL PGSVP  ( XMIN, XMAX, YMIN, YMAX  ) !
      CALL PGSWIN ( 0.0, 1.0*NCOL, YMIN, YMAX )
      IF ( UDEV .EQ. 1 ) THEN
           CALL PGSCH  ( 1.0 )
         ELSE
           CALL PGSCH  ( 0.7 )
      END IF
      CALL PGSFS  ( 1   )
      CALL PGSLW  ( 2   )
!
      DO 490 J9=1,NCOL
         XL = J9-1 
         YB = YMIN
         XR = J9 
         YT = YMAX
!
! ------ Make a box
!
         CALL PGSCI  ( J9-1+INIT_COL )
         CALL PGSFS  ( 1 )
         CALL PGRECT ( XL, XR, YB, YT )
 490  CONTINUE
!
      CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
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
         XL = FLOAT(J10-1) 
         YB = YMAX
         CALL PGPTXT ( XL, YB*1.1, 0.0, 0.5, STR(1:7) )
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
!
      RETURN
      END  SUBROUTINE  PLOT_LEAP_R4  !#!#
