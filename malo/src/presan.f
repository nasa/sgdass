      PROGRAM    PRESAN
! ************************************************************************
! *                                                                      *
! *   Program PREASN reads tghe input file with the atmospheric pressure *
! *   time, and coordinates of a the ground station and computes the     *
! *   atmospheric pressure for the same time and location from the       *
! *   output of the numeric model.                                       *
! *                                                                      *
! *  ### 14-FEB-2018    PRESAN     v1.0 (c)  L. Petrov  14-FEB-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_MASK
      INTEGER*4  MP
      PARAMETER  ( MP = 1024 )
      CHARACTER  FILPRE*128, FILMASK*128, STR*128, BUF(MP)*256, C_STA(MP)*3
      LOGICAL*1  LEX
      REAL*8     LAT(MP), LON(MP), HEI(MP), PRES(6,MP), PD(6,MP), VAL_MIN, VAL_MAX
      INTEGER*4  J1, J2, J3, J4, NP, MODE, IDEV, IPRC, L_STA, ISTA, IUER 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_CLIST, LTM_DIF
!
      IDEV = 1
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, '(A)' ) 'presan pressure_stat_file mask_file ipar val_min val_max'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILPRE  )
           CALL GETARG ( 2, FILMASK )
           CALL GETARG ( 3, STR     )
           CALL CHIN   ( STR, MODE  )
           CALL GETARG ( 4, STR     )
           READ ( UNIT=STR, FMT=*   ) VAL_MIN
           CALL GETARG ( 5, STR     )
           READ ( UNIT=STR, FMT=*   ) VAL_MAX
      END IF
!
      INQUIRE ( FILE=FILPRE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -2
           CALL ERR_LOG ( 4901, IUER, 'PRESAN', 'Input file '// &
     &          FILPRE(1:I_LEN(FILPRE))//' is not found' )
           CALL EXIT ( 1 )
      END IF
      INQUIRE ( FILE=FILMASK, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -2
           CALL ERR_LOG ( 4902, IUER, 'PRESAN', 'Input file '// &
     &          FILMASK(1:I_LEN(FILMASK))//' is not found' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILMASK, HEB_MASK, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4903, IUER, 'PRESAN', 'Error '// &
     &         'in an attempt to read heb-file with '// &
     &         'mask '//FILMASK )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FILPRE, MP, BUF, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 4904, IUER, 'PRESAN', 'Error '// &
     &         'in an attempt to read pressure statistics file '// &
     &         FILPRE )
           CALL EXIT ( 1 )
      END IF
!
      L_STA = 0
      DO 410 J1=1,NP
         CALL BLANK_TO_ZERO ( BUF(J1)(1:3) )
         IF ( BUF(J1)(1:4) == 'Sta:' ) THEN
              ISTA = ADD_CLIST ( MP, L_STA, C_STA, BUF(J1)(6:8), IUER )
              READ ( UNIT=BUF(J1)(10:17),  FMT='(F7.2)' ) HEI(ISTA)
              READ ( UNIT=BUF(J1)(18:24),  FMT='(F7.2)' ) LAT(ISTA)
              LAT(ISTA) = LAT(ISTA)*DEG__TO__RAD
              READ ( UNIT=BUF(J1)(26:33),  FMT='(F8.2)' ) LON(ISTA)
              LON(ISTA) = LON(ISTA)*DEG__TO__RAD
              IF ( LON(ISTA) < 0.0 ) LON(ISTA) = LON(ISTA) + PI2
              READ ( UNIT=BUF(J1)(94:99),    FMT='(F6.2)' ) PRES(1,ISTA)
              READ ( UNIT=BUF(J1)(101:106),  FMT='(F6.2)' ) PRES(2,ISTA)
              READ ( UNIT=BUF(J1)(108:113),  FMT='(F6.2)' ) PRES(3,ISTA)
              READ ( UNIT=BUF(J1)(115:120),  FMT='(F6.2)' ) PRES(4,ISTA)
              READ ( UNIT=BUF(J1)(128:132),  FMT='(F6.2)' ) PRES(5,ISTA)
              READ ( UNIT=BUF(J1)(156:161),  FMT='(F6.1)' ) PRES(6,ISTA)
              PD(1:5,ISTA) = 2.41*PRES(1:5,ISTA)
!!              PD(1,ISTA) = -PD(1,ISTA)
!              PD(3,ISTA) = -PD(3,ISTA)
!              PD(4,ISTA) = -PD(4,ISTA)
         END IF
 410  CONTINUE 
!
      IF ( INDEX ( FILMASK, 'anta' ) > 0 ) THEN
           IPRC = 3 
         ELSE 
           IPRC = 4
      END IF
      IF ( MODE == 0 ) THEN
           DO 420 J2=1,L_STA
              WRITE ( 6, 110 ) C_STA(J2), HEI(J2), LAT(J2)/DEG__TO__RAD, &
     &                         LON(J2)/DEG__TO__RAD, PD(1:5,J2)
 110          FORMAT ( A, ') hlp: ', F6.1, 1X, F7.2, 1X, F8.2, ' Pd: ', 5(F7.2, 1X) )
 420       CONTINUE 
         ELSE
!!   write ( 6, * ) ' HEB_MASK%DIMS= ', HEB_MASK%DIMS  ! %%%%%%%%%%%%%
           IUER = -1
           CALL PRESAN_PLOT ( MODE, IDEV, IPRC, L_STA, C_STA, LAT, LON, &
     &                        PD, HEB_MASK, VAL_MIN, VAL_MAX, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM    PRESAN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PRESAN_PLOT ( MODE, IDEV, IPRC, L_STA, C_STA, LAT, LON, &
     &                         PD, HEB_MASK, VAL_MIN, VAL_MAX, IUER  )
! ************************************************************************
! *                                                                      *
! *   Roujtine PRESAN_PLOT
! *                                                                      *
! *  ### 22-FEB-2018  PRESAN_PLOT  v1.0 (c)  L. Petrov  22-FEB-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'diagi.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB_MASK
      INTEGER*4  MODE, IDEV, IPRC, L_STA, IUER
      CHARACTER  C_STA(*)
      REAL*8     LAT(L_STA), LON(L_STA), PD(6,L_STA), VAL_MIN, VAL_MAX 
!
      INTEGER*4  IPAL, ISCL, NLON, NLAT
      CHARACTER  FILOUT*128
      INTEGER*4  MP, ML
      PARAMETER  ( MP = 512 ) 
      PARAMETER  ( ML = 4 ) 
      INTEGER*4  INIT_COL, NCOL, NLAB, IVAL_MIN, IVAL_MAX
      INTEGER*4, ALLOCATABLE :: MAP_I4(:,:)
      CHARACTER  STR*128, FINAM*128, CH*4, STR_RVAL_MIN*20, STR_RVAL_MAX*20, &
     &           UNIT*6
      REAL*4     XCOEF_IMA, XCOEF_PAP, LONB_R4, LONE_R4, LATB_R4, LATE_R4, &
     &           PAP_SIZE, WHITE_CLR, XC, YC, XPR, YPR, &
     &           PHI, LAM, LON_MIN, LON_MAX, LAT_MIN, LAT_MAX, LAT_VAL, LON_VAL, &
     &           XMIN, XMAX, YMIN, YMAX, XL, XR, YB, YT, RD, VAL_MAX_ALLOWED, RAD_CIRC
      REAL*4     RVAL_MIN, RVAL_MAX, VAL_R4, ARR_X(8192), ARR_Y(8192), &
     &           LAT_ARR(MP), LON_ARR(MP)
      REAL*4     EPS
      REAL*4     RANGE(2)
      DATA       RANGE / -1.0E10, 1.0E10 /
      PARAMETER  ( EPS = 1.E-6 )
      LOGICAL*4  FL_PLOT
      INTEGER*4  BCG_GREY
      PARAMETER  ( BCG_GREY = 8 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           NUM, IP, CI1, CI2, IX, IY, ID, UDEV, ICLR, ILAT, ILON, NR
      INTEGER*4, EXTERNAL :: PGBEG, I_LEN, ILEN, LINDEX
!
      FILOUT = '/tmp/foo'
      IPAL   = 7
      UNIT   = 'mm'
      RAD_CIRC = 0.015
!
      NLON = HEB_MASK%DIMS(1)
      NLAT = HEB_MASK%DIMS(2)
      IF ( IPRC == 3 ) THEN
!
! -------- Southern pole
!
           LAT_MIN =  -P2I
           LAT_MAX = (-P2I + 30.0D0*DEG__TO__RAD )
           RD = COS(PI__NUM/4.0 - LAT_MAX/2.0)
           NR = 30.0D0*DEG__TO__RAD*(NLAT-1)*0.60 ! Coefficient 0.6225 was found empirically to avoid mosiac
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
      UDEV = IDEV
      XCOEF_IMA = 1.0
      XCOEF_PAP = 1.0
      INIT_COL = 20
      NCOL = 200
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
!
      ALLOCATE ( MAP_I4(NR,NR) )
      MAP_I4 = -9999
!
      IF ( VAL_MAX < VAL_MIN ) THEN
           DO 410 J1=1,L_STA
              IF ( PD(MODE,J1) < RVAL_MIN ) RVAL_MIN = PD(MODE,J1) 
              IF ( PD(MODE,J1) > RVAL_MAX ) RVAL_MAX = PD(MODE,J1) 
 410       CONTINUE 
         ELSE 
           RVAL_MIN = VAL_MIN
           RVAL_MAX = VAL_MAX
      END IF
 910  CONTINUE
!
! --- Open plotting device and set up coordinate system.
!
      IF ( UDEV .EQ. 1 ) THEN
           IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 2 ) THEN
           FILOUT = FILOUT(1:I_LEN(FILOUT))//PS_DIAGI//'/CPS'
           IF ( PGBEG ( 0, FILOUT, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 3 ) THEN
           FILOUT = FILOUT(1:I_LEN(FILOUT))//GIF_DIAGI//'/GIF'
           IF ( PGBEG ( 0, FILOUT, 1, 1 ) .NE. 1 ) STOP
         ELSE IF ( UDEV .EQ. 4 ) THEN
           FILOUT = FILOUT(1:I_LEN(FILOUT))//PS_DIAGI//'/VCPS'
           IF ( PGBEG ( 0, FILOUT, 1, 1 ) .NE. 1 ) STOP
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
           CALL ERR_LOG ( 6351, IUER, 'POLOT_GRID_R4', 'This program requires '// &
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
      CALL PGCOL_RGB ( BCG_GREY, 196, 196, 196 )
!
      MAP_I4 = BCG_CLRI
      DO 420 J2=1,NLON
         LON_VAL = (J2-1)*PI2/(NLON-1)
         DO 430 J3=1,NLAT
            LAT_VAL = -P2I + (J3-1)*PI__NUM/(NLAT-1)
            IF ( IPRC == 3 ) THEN
                 IF ( LAT_VAL < LAT_MAX ) THEN
                      XPR = COS(PI__NUM/4.0 - LAT_VAL/2.0)*SIN(LON_VAL)/RD
                      YPR = COS(PI__NUM/4.0 - LAT_VAL/2.0)*COS(LON_VAL)/RD
                      IX = 1 + (XPR+1.0)/2*(NR-1)
                      IY = 1 + (YPR+1.0)/2*(NR-1)
                      IF ( IX < 1 .OR. IX > NR .OR. IY < 1 .OR. IY > NR ) GOTO 430
                      IF ( HEB_MASK%VAL1(J2,J3,1,1) == 1 ) THEN
                           MAP_I4(IX,IY) = BCG_GREY
                      END IF
                 END IF
               ELSE IF ( IPRC == 4 ) THEN
                 IF ( LAT_VAL > LAT_MIN ) THEN
                      XPR = SIN(PI__NUM/4.0 - LAT_VAL/2.0)*SIN(LON_VAL)/RD
                      YPR = SIN(PI__NUM/4.0 - LAT_VAL/2.0)*COS(LON_VAL)/RD
                      IX = 1 + (XPR+1.0)/2*(NR-1)
                      IY = 1 + (YPR+1.0)/2*(NR-1)
                      IF ( HEB_MASK%VAL1(J2,J3,1,1) == 1 ) THEN
                           MAP_I4(IX,IY) = BCG_GREY
                      END IF
                 END IF
            END IF
 430     CONTINUE 
 420  CONTINUE 
      CALL PGPIXL ( MAP_I4, NR, NR, 1, NR, 1, NR, -1.0, 1.0, -1.0, 1.0 )
      IF ( IPRC == 3 ) THEN
           LAT_ARR(1) = -90.0*PI__NUM/180.0 
           LAT_ARR(2) = -80.0*PI__NUM/180.0 
           LAT_ARR(3) = -70.0*PI__NUM/180.0 
           LAT_ARR(4) = -60.0*PI__NUM/180.0 
         ELSE IF ( IPRC == 4 ) THEN
           LAT_ARR(1) =  60.0*PI__NUM/180.0 
           LAT_ARR(2) =  70.0*PI__NUM/180.0 
           LAT_ARR(3) =  80.0*PI__NUM/180.0 
           LAT_ARR(4) =  90.0*PI__NUM/180.0 
       END IF
!
! === Now putting coordinate grid around the picture
!
      DO 440 J4=1,ML
         LON_VAL = PI2*(J4-1)/(1.0*ML)
         DO 450 J5=1,MP
            LAT_VAL = LAT_ARR(1) + (J5-1)*(LAT_ARR(ML) - LAT_ARR(1))/(MP-1)
            IF ( IPRC == 3 ) THEN
                 ARR_X(J5) = COS(PI__NUM/4.0 - LAT_VAL/2.0)*SIN(LON_VAL)/RD
                 ARR_Y(J5) = COS(PI__NUM/4.0 - LAT_VAL/2.0)*COS(LON_VAL)/RD
               ELSE IF ( IPRC == 4 ) THEN
                 ARR_X(J5) = SIN(PI__NUM/4.0 - LAT_VAL/2.0)*SIN(LON_VAL)/RD
                 ARR_Y(J5) = SIN(PI__NUM/4.0 - LAT_VAL/2.0)*COS(LON_VAL)/RD
            END IF
 450      CONTINUE 
          CALL PGLINE ( MP, ARR_X, ARR_Y ) 
          CALL CLRCH  ( STR )
          WRITE ( UNIT=STR, FMT='(F4.0)' ) LON_VAL/PI2*360.
          STR(4:4) = ' '
          CALL CHASHL ( STR )
          IF ( IPRC == 3 ) THEN 
               CALL PGPTXT ( ARR_X(MP), ARR_Y(MP), 0.0, 0.0, &
     &                       STR(1:I_LEN(STR))//'\u\(0902)\d' )
             ELSE IF ( IPRC == 4 ) THEN
               CALL PGPTXT ( ARR_X(1), ARR_Y(1), 0.0, 0.0, &
     &                       STR(1:I_LEN(STR))//'\u\(0902)\d' )
          END IF
 440  CONTINUE 
!
      DO 460 J6=1,ML
         CALL PGSCI  ( 1   )
         LAT_VAL = LAT_ARR(J6)
         DO 470 J7=1,MP
            IF ( IPRC == 3 ) THEN
                 LON_VAL = PI2*(J7-1)/(MP-1.0)
               ELSE IF ( IPRC == 4 ) THEN
                 LON_VAL = PI2*(J7-1)/(MP-1.0)
            END IF
            IF ( IPRC == 3 ) THEN
                 ARR_X(J7) = COS(PI__NUM/4.0 - LAT_VAL/2.0)*SIN(LON_VAL)/RD
                 ARR_Y(J7) = COS(PI__NUM/4.0 - LAT_VAL/2.0)*COS(LON_VAL)/RD
               ELSE IF ( IPRC == 4 ) THEN
                 ARR_X(J7) = SIN(PI__NUM/4.0 - LAT_VAL/2.0)*SIN(LON_VAL)/RD
                 ARR_Y(J7) = SIN(PI__NUM/4.0 - LAT_VAL/2.0)*COS(LON_VAL)/RD
            END IF
 470     CONTINUE 
         CALL PGLINE ( MP, ARR_X, ARR_Y ) 
         IF ( IPRC == 3 .AND. J6 == ML  ) GOTO 460
         IF ( IPRC == 4 .AND. J6 == 1   ) GOTO 460
         CALL CLRCH  ( STR )
         WRITE ( UNIT=STR, FMT='(F4.0)' ) LAT_VAL/PI__NUM*180.
         STR(4:4) = ' '
         CALL CHASHL ( STR )
         CALL PGPTXT ( ARR_X(MP/4), ARR_Y(MP/4), 0.0, 0.0, &
     &                 STR(1:I_LEN(STR))//'\u\(0902)\d' )
 460  CONTINUE 
!
      DO 480 J8=1,L_STA
         ILAT = NLAT*((LAT(J8) + P2I)/PI__NUM) + 1
         ILON = NLON*(LON(J8)/PI2) + 1
         IF ( ILON > NLON ) ILON = 1
         IF ( HEB_MASK%VAL1(ILON,ILAT,1,1) == 1 ) THEN
              IF ( IPRC == 3 ) THEN
                   XPR = COS(PI__NUM/4.0 - LAT(J8)/2.0)*SIN(LON(J8))/RD
                   YPR = COS(PI__NUM/4.0 - LAT(J8)/2.0)*COS(LON(J8))/RD
                ELSE IF ( IPRC == 4 ) THEN
                   XPR = SIN(PI__NUM/4.0 - LAT(J8)/2.0)*SIN(LON(J8))/RD
                   YPR = SIN(PI__NUM/4.0 - LAT(J8)/2.0)*COS(LON(J8))/RD
              END IF
!
              ICLR = (  PD(MODE,J8) - RVAL_MIN)/ &
     &               (RVAL_MAX - RVAL_MIN)*(NCOL-1) + INIT_COL
              IF ( ICLR > NCOL ) ICLR = INIT_COL + NCOL - 1 
              IF ( ICLR <    1 ) ICLR = INIT_COL 
              CALL PGSCI  ( ICLR )
              CALL PGCIRC_PET ( 72, XPR, YPR, XCOEF_IMA*RAD_CIRC, RAD_CIRC )
         END IF
 480  CONTINUE 
      CALL PGSCI  ( 1 )
!
! --- Printing the bottom scale slider
!
      XMIN = 0.08
      XMAX = 0.92
      YMIN = 0.06
      YMAX = 0.10
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
      DO 4110 J11=1,NCOL
         IF ( IPRC == 1 ) THEN
              XL = XMIN
              YB = J11
              XR = XMAX
              YT = (J11-1)
            ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
              XL = J11-1 
              YB = YMIN
              XR = J11 
              YT = YMAX
            ELSE IF ( IPRC == 3 .OR. IPRC == 4 ) THEN
              XL = J11-1 
              YB = YMIN
              XR = J11 
              YT = YMAX
         END IF
!
! ------ Make a box
!
         CALL PGSCI  ( J11-1+INIT_COL )
         CALL PGSFS  ( 1 )
         CALL PGRECT ( XL, XR, YB, YT )
 4110 CONTINUE
!
      IF ( IPRC == 1 ) THEN
           CALL PGSWIN ( XMIN, XMAX, 0.0, 1.0*(NLAB-1) )
         ELSE IF ( IPRC == 2 .OR. IPRC == -2 ) THEN
           CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
         ELSE IF ( IPRC == 3 .OR. IPRC ==  4 ) THEN
           CALL PGSWIN ( 0.0, 1.0*(NLAB-1), YMIN, YMAX )
      END IF
!
      DO 4120 J12=1,NLAB
         VAL_R4 = RVAL_MIN + (RVAL_MAX-RVAL_MIN)/FLOAT(NLAB-1)*(J12-1)
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
              YB = FLOAT(J12-1)
              CALL PGPTXT ( XL*0.9, YB, 0.0, 1.0, STR(1:6) )
            ELSE IF ( IPRC == 2 .OR. IPRC == -2 .OR. &
     &                IPRc == 3 .OR. IPRC ==  4      ) THEN
              XL = FLOAT(J12-1) 
              YB = YMAX
              CALL PGPTXT ( XL, YB*1.1, 0.0, 0.5, STR(1:6) )
         END IF
 4120 CONTINUE 
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
           ID = LINDEX ( FILOUT, '/' ) - 1
           IF ( ID < 0 ) ID = ILEN(FILOUT)
           WRITE ( 6, * ) 'Plot is written in file '//FILOUT(1:ID)
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
     &                      FILOUT(1:I_LEN(FILOUT)-4)//' &'
                     WRITE ( 6, * ) 'Plot is being sent to printer ...'
                     CALL SYSTEM ( STR(1:I_LEN(STR))//CHAR(0) )
                     WRITE ( 6, * ) ' STR >>',STR(1:I_LEN(STR)),'<< '
                     WRITE ( 6, * ) 'Plot is sent to printer'
                END IF
           END IF
      END IF
!
      DEALLOCATE ( MAP_I4 ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PRESAN_PLOT  !#!  
