      SUBROUTINE PIMA_2D_MAP_PLOT ( FINAM_PLOT, PIM, MAP, TITLE, &
     &                              IND_X, IND_Y, MAP_RES, PLOT_RES, MAP_SCL, &
     &                              MODE, IPAR, ISCL, IPAL, IDEV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_2D_MAP_PLOT 
! *                                                                      *
! * ### 01-JAN-2012 PIMA_2D_MAP_PLOT v1.1 (c) L. Petrov  05-APR-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      CHARACTER  FINAM_PLOT*(*), TITLE*(*)
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_X, IND_Y, MAP_RES, PLOT_RES, MODE, IPAR, ISCL, &
     &           IPAL, IDEV, IUER
      COMPLEX*8  MAP(MAP_RES,MAP_RES)
      REAL*8     MAP_SCL
      REAL*4     XCOEF_PAP, PAP_SIZE, XC, YC, XL, YB, XR, YT, &
     &           XMIN, XMAX, YMIN, YMAX, VAL, VAL_MAX, VAL_MIN, EPS
      REAL*4,    ALLOCATABLE :: MAP_R4(:,:)
      INTEGER*4, ALLOCATABLE :: MAP_I4(:,:)
      PARAMETER  ( EPS = 1.0E-7 )
      INTEGER*4  PLO_CLRI 
      PARAMETER  (  PLO_CLRI = 19 )
      CHARACTER  OUT1*128, OUT2*128, OUT3*128, OUT4*128, &
     &           OUT5*128, OUT6*128, OUT7*128, OUT8*128, &
     &           STR*128,  CH*1,     FINAM*128
      CHARACTER  NOW*128
      INTEGER*4  IU, IV, J1, J2, J3, J4, J5, J6, NLAB, NCOL, &
     &           INIT_COL, CI1, CI2, ID, WHITE_CLR, IER
      LOGICAL*4  FL_SMALL, FL_PLOT
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: PGOPEN, I_LEN, ILEN, LINDEX
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
!
#ifdef NO_PLOT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_2D_MAP_PLOT  !#!#
#else
!
      FINAM = FINAM_PLOT
      FL_SMALL = .FALSE.
      IF ( MODE == 1 ) THEN
           ALLOCATE ( MAP_R4(PLOT_RES,PLOT_RES), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9411, IUER, 'PIMA_2D_MAP_PLOT', &
     &              'Failure to allocate memory for temporary '// &
     &              'array MAP_R4' ) 
                RETURN 
           END IF
        ELSE IF ( MODE == 2 ) THEN
           ALLOCATE ( MAP_I4(PLOT_RES,PLOT_RES), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9412, IUER, 'PIMA_2D_MAP_PLOT', &
     &              'Failure to allocate memory for temporary '// &
     &              'array MAP_I4' ) 
                RETURN 
           END IF
      END IF
 910  CONTINUE 
!
      IF ( IDEV .EQ. 1 ) THEN
!@           IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
           IF ( PGOPEN ( '/XW' ) .NE. 1 ) STOP
         ELSE IF ( IDEV .EQ. 2 ) THEN
           FINAM = FINAM(1:I_LEN(FINAM))//PS_DIAGI//'/CPS'
!@           IF ( PGBEG ( 0, FINAM, 1, 1 ) .NE. 1 ) STOP
           IF ( PGOPEN ( FINAM ) .NE. 1 ) STOP
         ELSE IF ( IDEV .EQ. 3 ) THEN
           FINAM = FINAM(1:I_LEN(FINAM))//GIF_DIAGI//'/GIF'
!@           IF ( PGBEG ( 0, FINAM, 1, 1 ) .NE. 1 ) STOP
           IF ( PGOPEN ( FINAM ) .NE. 1 ) STOP
         ELSE IF ( IDEV .EQ. 4 ) THEN
           FINAM = FINAM(1:I_LEN(FINAM))//PS_DIAGI//'/VCPS'
!@           IF ( PGBEG ( 0, FINAM, 1, 1 ) .NE. 1 ) STOP
           IF ( PGOPEN ( FINAM ) .NE. 1 ) STOP
      END IF
!
      INIT_COL = 20
      NCOL = 200
      NLAB = 16
      XCOEF_PAP = 1.0
      IF ( IDEV .EQ. 1 ) THEN
           PAP_SIZE = 300.0 
         ELSE IF ( IDEV .EQ. 2 ) THEN
           PAP_SIZE = 210.0
         ELSE IF ( IDEV .EQ. 3 ) THEN
           PAP_SIZE = 1000.0
         ELSE IF ( IDEV .EQ. 4 ) THEN
           PAP_SIZE = 600.0
         ELSE
           PAP_SIZE = 240.0
      END IF
      CALL PGPAP  ( PAP_SIZE/25.4, XCOEF_PAP )
!
      CALL PGCOL_RGB ( PLO_CLRI, 38, 67, 180 )
!
! --- Set parameters of the plotting window
!
      IF ( MODE == 1 ) THEN
           CALL PGSVP   (  0.01, 0.99, 0.13, 0.99 )
         ELSE IF ( MODE == 2 ) THEN
           CALL PGSWIN  ( -PLOT_RES*SNGL(MAP_SCL), PLOT_RES*SNGL(MAP_SCL), &
     &                    -PLOT_RES*SNGL(MAP_SCL), PLOT_RES*SNGL(MAP_SCL)  )
           CALL PGSVP   ( 0.17, 0.97, 0.10, 0.90  ) ! makes fields for labels
           CALL PGSCR   ( 0, 1.0, 1.0, 1.0 ) ! pure white background
      END IF
!
! --- Learn the number of colors available for this device
!
      CALL PGQCOL  ( CI1, CI2 )
      IF ( CI2 .LT. INIT_COL+NCOL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( INIT_COL+NCOL, STR )
           DEALLOCATE ( MAP_I4 )
           CALL ERR_LOG ( 6358, IUER, 'AS3_GRID_PLOT', 'This program requires '// &
     &         'a device with at least '//STR(1:I_LEN(STR))//' colors' )
           RETURN
      END IF
!
! --- Set white color
!
      IF ( IDEV .EQ. 1 ) THEN
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
      CALL DEFINE_PALETTE ( IPAL, IDEV, NCOL, INIT_COL )
!
      CALL PGCOL_RGB ( FRG_CLRI, FRG_CLR(1), FRG_CLR(2), FRG_CLR(3) )
!
      CALL PGERAS() ! Erase the screen
      CALL PGSCI ( PLO_CLRI )
      CALL CLRCH ( OUT2 )
      CALL CLRCH ( OUT3 )
      CALL CLRCH ( OUT4 )
      CALL CLRCH ( OUT5 )
      CALL CLRCH ( OUT6 )
      CALL CLRCH ( OUT7 )
      CALL CLRCH ( OUT8 )
!
      DO 410 J1=1,PLOT_RES
         IV = IND_Y - PLOT_RES/2 + J1-1
         IF ( IV < 1       ) IV = IV + MAP_RES
         IF ( IV > MAP_RES ) IV = IV - MAP_RES
         DO 420 J2=1,PLOT_RES
            IU = IND_X - PLOT_RES/2 + J2-1
            IF ( IU < 1       ) IU = IU + MAP_RES
            IF ( IU > MAP_RES ) IU = IU - MAP_RES
            IF ( IPAR == 1 ) THEN
                 VAL = ABS(MAP(IU,IV))
              ELSE IF ( IPAR == 2 ) THEN
                 VAL = PHAS_CMPL_R4(MAP(IU,IV))
                 IF ( ABS(MAP(IU,IV)) < EPS ) VAL = 0.0
              ELSE IF ( IPAR == 3 ) THEN
                 VAL = REAL(MAP(IU,IV))
              ELSE IF ( IPAR == 4 ) THEN
                 VAL = IMAG(MAP(IU,IV))
            END IF
!
            IF ( J1 == 1 .AND. J2 == 1 ) THEN
                 VAL_MIN = VAL
                 VAL_MAX = VAL
               ELSE 
                 VAL_MIN = MIN ( VAL_MIN, VAL )
                 VAL_MAX = MAX ( VAL_MAX, VAL )
            END IF
 420     CONTINUE 
 410  CONTINUE
      IF ( ISCL == 1 ) THEN
           VAL_MIN = 0.0
           VAL_MAX = 1.0
      END IF
!
!%  write ( 6, * ) 'PIMA_2D_MAP_PLOT: val_min = ', val_min, ' val_max= ', val_max, ' ncol= ', ncol ! %%%%%%%%
      DO 430 J3=1,PLOT_RES
         IV = IND_Y - PLOT_RES/2 + J3-1
         IF ( IV < 1       ) IV = IV + MAP_RES
         IF ( IV > MAP_RES ) IV = IV - MAP_RES
         DO 440 J4=1,PLOT_RES
            IU = IND_X - PLOT_RES/2 + J4-1
            IF ( IU < 1       ) IU = IU + MAP_RES
            IF ( IU > MAP_RES ) IU = IU - MAP_RES
            IF ( IPAR == 1 ) THEN
                 VAL = ABS(MAP(IU,IV))
              ELSE IF ( IPAR == 2 ) THEN
                 VAL = PHAS_CMPL_R4(MAP(IU,IV))
                 IF ( ABS(MAP(IU,IV)) < EPS ) VAL = 0.0
              ELSE IF ( IPAR == 3 ) THEN
                 VAL = REAL(MAP(IU,IV))
              ELSE IF ( IPAR == 4 ) THEN
                 VAL = IMAG(MAP(IU,IV))
            END IF
            IF ( MODE == 1 ) THEN
                 MAP_R4(J4,J3) = VAL
              ELSE IF ( MODE == 2 ) THEN
                 MAP_I4(J4,J3) = ( VAL - VAL_MIN )/ &
     &                           ( VAL_MAX - VAL_MIN )*(NCOL-1) + INIT_COL
!% write ( 6, * ) ' j4= ', int2(j4), ' j3= ', int2(j3), ' val= ', val, ' abs= ', abs(map(iu,iv)), ' map= ', map(iu,iv) ! %%%%%%%%%%%%%%%%%%
            END IF
 440     CONTINUE 
 430  CONTINUE 
      NOW = GET_CDATE()
      IF ( MODE == 1 ) THEN
           CALL FREDDY  ( MAP_R4, PLOT_RES, PLOT_RES, 1.0, 30.0, &
     &                    'u', 'v', TITLE, OUT2, OUT3, OUT4, &
     &                    OUT5, OUT6, OUT7, OUT8, NOW, FL_SMALL, PIMA__LABEL )
!
           IF ( IDEV == 1 ) THEN
                XC = 1.0
                YC = 1.0
                CALL PGBAND ( 0, 1, XC, YC, XC, YC, STR )
           END IF
           CALL PGCLOQ()
        ELSE IF ( MODE == 2 ) THEN
           CALL PGPIXL ( MAP_I4, PLOT_RES, PLOT_RES, 1, PLOT_RES, 1, PLOT_RES, &
     &                   -PLOT_RES*SNGL(MAP_SCL), PLOT_RES*SNGL(MAP_SCL), &
     &                   -PLOT_RES*SNGL(MAP_SCL), PLOT_RES*SNGL(MAP_SCL)  )
!
! -------- Print title
!
           CALL PGSCF  ( 2 )
           IF ( IDEV == 2 .OR. IDEV == 4 ) THEN
                CALL PGSLW  ( 2 )
              ELSE 
                CALL PGSLW  ( 6 )
           END IF
!
           CALL PGSCH  ( 1.6 )
           CALL PGSCF  ( 2  )
           CALL PGMTXT ( 't', 0.5, 0.5, 0.5, TITLE(1:I_LEN(TITLE)) )
           CALL PGSCF  ( 1   )
           CALL PGSLW  ( 4 )
           CALL PGSCH  ( 1.0 )
           CALL PGSCI  ( 1 )
           CALL PGMTXT ( 't', 2.5, 1.0, 1.0, NOW )
           CALL PGSLW  ( 1 )
!
! -------- Plot the box around the image
!
           CALL PGSCI  ( 1 )
           CALL PGSCH  ( 0.8 )
           CALL PGBOX  ( 'bicnts', PLOT_RES*SNGL(MAP_SCL)/2.0, 5, &
     &                   'bicnts', PLOT_RES*SNGL(MAP_SCL)/2.0, 5 )
!
! -------- Print the annotation
!
           CALL PGSLW  ( 4 )
           CALL PGSCH  ( 1.0 )
           CALL PGMTXT ( 'b', 2.5, 1.0, 1.0, 'Right Ascension (mas)' )
           CALL PGMTXT ( 'l', 1.5, 1.0, 1.0, 'Declination (mas)' )
!
! -------- Printing the left column with scale
!
           XMIN = 0.08
           XMAX = 0.11
           YMIN = 0.08
           YMAX = 0.92
           CALL PGSVP  ( XMIN, XMAX, YMIN, YMAX  ) !
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*NCOL )
           IF ( IDEV .EQ. 1 ) THEN
                CALL PGSCH  ( 1.0 )
              ELSE
                CALL PGSCH  ( 0.7 )
           END IF
           CALL PGSFS  ( 1   )
           CALL PGSLW  ( 2   )
!
           DO 450 J5=1,NCOL
              XL = XMIN
              YB = J5
              XR = XMAX
              YT = (J5-1)
!
! ----------- Make a box
!
              CALL PGSCI  ( J5-1+INIT_COL )
              CALL PGSFS  ( 1 )
              CALL PGRECT ( XL, XR, YB, YT )
 450       CONTINUE
!
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*(NLAB-1) )
!
           DO 460 J6=1,NLAB
              IF ( VAL_MAX < 0.01 ) THEN
                    WRITE ( UNIT=STR(1:7), FMT='(F7.5)' ) &
     &                      VAL_MIN + (VAL_MAX-VAL_MIN)/FLOAT(NLAB-1)*(J6-1)*1000.0
                  ELSE 
                    WRITE ( UNIT=STR(1:7), FMT='(F7.3)' ) &
     &                      VAL_MIN + (VAL_MAX-VAL_MIN)/FLOAT(NLAB-1)*(J6-1)
              END IF
              CALL CHASHR ( STR(1:7) )
!
! ----------- Write annotation near the box
!
              CALL PGSCI  ( 1 )
              XL = XMIN
              YB = FLOAT(J6-1)
              CALL PGPTXT ( XL*0.9, YB, 0.0, 1.0, STR(1:7) )
 460       CONTINUE 
!
! -------- Write units of the annotation
!
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*(NLAB-1) )
!
! --------- ... and the box around the scale
!
           CALL PGSCI ( 1 )
           CALL PGSFS ( 2 )
           CALL PGSLW ( 2 )
           IF ( IDEV .NE. 3 ) THEN
                CALL PGRECT ( XMIN+0.0001, XMAX-0.0001, 0.0, 1.0*(NLAB-1)-0.001 )
           END IF
!
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0 )
           CALL PGSLW  ( 5 )
           CALL PGSCH  ( 0.75 )
           IF ( IPAR == 2 ) THEN
                CALL PGMTXT ( 'b', 1.0, 1.0, 1.0, 'Phase (rad)' )
             ELSE
                IF ( VAL_MAX < 0.01 ) THEN
                     CALL PGMTXT ( 'b', 1.0, 1.0, 1.0, 'Flux d. (mJy)' )
                  ELSE 
                     CALL PGMTXT ( 'b', 1.0, 1.0, 1.0, 'Flux d. (Jy)' )
                END IF
           END IF
!
           IF ( IDEV .EQ. 1 ) THEN
                CALL PGSCH  ( 1.0 )
                CALL PGPTXT ( 0.01, -1.4, 0.0, 0.0, 'Hit <CNTRL/P> for printing, '// &
     &              'P - for making ps-file, G - for gif-file, other key to exit' )
           END IF
!
           XC = 0.95
           YC = 0.0
!
           IF ( IDEV .EQ. 1 ) THEN
!
! ------------- Wait for a user to hit a key
!
                CALL PGCURS ( XC, YC, CH )
                CALL TRAN ( 11, CH, CH )
                CALL PGCLOQ()
!
! ------------- Check the value of the key
!
                IF ( CH .EQ. CHAR(16) ) THEN
!
! ------------------ Make the plot in ps format and then print
!
                     IDEV = 2
                     FL_PLOT = .TRUE.
                     GOTO 910
                   ELSE IF ( CH .EQ. 'P' ) THEN
!
! ------------------ Make the polt in postscript format
!
                     IDEV = 2
                     GOTO 910
                   ELSE IF ( CH .EQ. 'G' ) THEN
!
! ------------------ Make the plot in gif format
!
                     IDEV = 3
                     GOTO 910
                END IF
              ELSE
                CALL PGCLOQ()  ! quit pgplot
                ID = LINDEX ( FINAM, '/' ) - 1
                IF ( ID < 0 ) ID = ILEN(FINAM)
                WRITE ( 6, * ) 'Plot is written in file '//FINAM(1:ID)
                IF ( FL_PLOT ) THEN
!
! ------------------ Aga. A user wants the plot to be printed
!
                     CALL GETENVAR ( 'DIAGI_PRICOM', STR )
                     IF ( ILEN(STR) .GT. 0 ) THEN
!
! ------------------ Set the command for printing
!
                          STR = STR(1:I_LEN(STR))//' '// &
     &                           FINAM(1:I_LEN(FINAM)-4)//' &'
                          WRITE ( 6, * ) 'Plot is being sent to printer ...'
                          CALL SYSTEM ( STR(1:I_LEN(STR))//CHAR(0) )
                          WRITE ( 6, * ) ' STR >>',STR(1:I_LEN(STR)),'<< '
                          WRITE ( 6, * ) 'Plot is sent to printer'
                     END IF
                END IF
           END IF
      END IF
!
      IF ( MODE == 1 ) THEN
           DEALLOCATE ( MAP_R4 )
         ELSE IF ( MODE == 2 ) THEN
           DEALLOCATE ( MAP_I4 )
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_2D_MAP_PLOT  !#!#
#endif
