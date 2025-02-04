      SUBROUTINE GEN_RADPLOT ( VIS, IDEV, ICLR, ISIZE, GAP_SCAN, FL_WEI_USE, &
     &                         FL_AUTO, CUTOFF_NERR, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GEN_RADPLOT generates the plot of dependence of the        *
! *   fringe amplitude on the length of the baseline projection to the   *
! *   tangential plane of the source.                                    *
! *   Supports  variables  PIMAVAR_X_TR and PIMAVAR_TICK_STEP_X for      *
! *   overriding automatic selection of the plot size.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    VIS ( VIS__TYPE ) -- object which keeps variables related to the  *
! *                         visibility data for this source.             *
! *   IDEV ( INTEGER*4 ) -- Device code:                                 *
! *                         IDEV == 1 -- the plot will be printed        *
! *                                      on the screen.                  *
! *                         IDEV == 2 -- the plot will be in PostScript  *
! *                                      format.                         *
! *                         IDEV == 3 -- the plot will be in gif format. *
! *                         IDEV == 4 -- No plot, text table will be     *
! *                                      generated.                      *
! *   ICLR ( INTEGER*4 ) -- color index according to DiaGI.              *
! *                         Valid range -1, 0, or [1, 64].               *
! *                         Color index -1 indicates that the color of   *
! *                         the plot will be selected in accordance      *
! *                         with the observing frequency.                *
! *                         Color index 0 means black.                   *
! *  ISIZE ( INTEGER*4 ) -- size code:                                   *
! *                         0 -- Special mode. The table of correlated   *
! *                              flux density versus baseline length     *
! *                              will be written in the file opened      *
! *                              at unit 80.                             *
! *                         1 -- 36x44 mm                                *
! *                         2 -- 72x90 mm                                *
! *                         3 -- 130x172 mm                              *
! *                         4 -- 200x265 mm                              *
! *                         5 -- 270x360 mm                              *
! * GAP_SIZE ( REAL*8  ) -- Maximum gap in the observations of the same  *
! *                         scan. Observations with gaps longer than     *
! *                         GAP_SIZE are considered to belong to         *
! *                         different scans.                             *
! * FL_WEI_USE  ( LOGICAL*4 ) -- If .TRUE., then weights from the        *
! *                              file with visibilities will be used     *
! *                              for averaging and computing the         *
! *                              statistical uncertainties. If .FALSE.,  *
! *                              then the weights 1.0 will used for      *
! *                              computations, provided the weight in    *
! *                              the visibility data is positive (and    *
! *                              the point will be discarded if weight   *
! *                              is negative).                           *
! * FL_AUTO    ( LOGICAL*4 )  -- If .FALSE.,  then GEN_RADPLOT makes a   *
! *                              plot of calibrated amplitude versus     *
! *                              baseline length. IF .TRUE. then         *
! *                              GEN_RADPLOT makes a plot versus the     *
! *                              length of the baseline projection to    *
! *                              the direction which makes the scatter   *
! *                              of the amplitude with respect to        *
! *                              a smoothed curve minimal.               *
! *                                                                      *
! * CUTOFF_NERR ( REAL*8    ) -- The points with the normalized          *
! *                              statistical uncertainties determined    *
! *                              as Err(Amp)/Amp exceeding CUTOFF_NERR   *
! *                              are flagged out and removed from the    *
! *                              output array. Here Err(Amp) is the      *
! *                              statistical error of the averaged       *
! *                              amplitude determined on the basis of    *
! *                              the scatter with respect to average and *
! *                              Amp is the averaged amplitude.          *
! * FILOUT ( CHARACTER ) -- Name of the output file. The name is ignored *
! *                         if IDEV == 1 .                               *
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
! *  ### 30-JAN-2007  GEN_RADPLOT  v2.1 (c)  L. Petrov 13-OCT-2024 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'astro_constants.i'
      INCLUDE    'diagi.i'
      INCLUDE    'sou_map.i'
      INTEGER*4  IDEV, ICLR, ISIZE, IUER
      LOGICAL*4  FL_WEI_USE, FL_AUTO
      CHARACTER  FILOUT*(*)
      REAL*8     GAP_SCAN, CUTOFF_NERR
      TYPE     ( VIS__TYPE  ) :: VIS
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      INTEGER*4  M_POI
      PARAMETER  ( M_POI = 64*1024 )
      REAL*8     C__VEL
      PARAMETER  ( C__VEL = 299792458.0D0  )
      REAL*8     UV_LEN(M_POI), AMP(M_POI), ERR_AMP(M_POI), TAI_AVR(M_POI), &
     &           AMP_MAX, UV_LEN_MAX, PHI_MIN
      REAL*4     PAP_SIZE
      REAL*4     X_BL, X_TR, Y_BL, Y_TR, TICK_STEP_X, TICK_STEP_Y, XC, YC, &
     &           XRAD, YRAD, X_AR2(2), Y_AR2(2), X_CP, Y_CP, X_DAT, &
     &           X_HEA_CENTER, Y_HEA_FACTOR, XFREQ_CENTER, X_DATE_CENTER
      REAL*4     AMP_FUDGE, RAD, ASPECT, AMP_MAX__DEFAULT
      PARAMETER  ( AMP_FUDGE = 1.1  )
      PARAMETER  ( ASPECT    = 0.75 )
      PARAMETER  ( AMP_MAX__DEFAULT = 0.01 ) ! Jy
      REAL*8     FRQ_AVR, UVLEN_FCT
      INTEGER*4  J1, J2, J3, J4, J5, L_POI, L_SCA, ID_XW, IND_AMP_MAX, &
     &           IND_UVL_MAX, ICLR_BAND, BAS_IND(M_POI), IND_STA(2), IVRB, IER
      CHARACTER  STR*256, STR1*256, CH*1, COM_STR*256, BAND*1
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_BAND*1 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, MAX_LIST_R8, PGOPEN
!
      IF ( VIS%STATUS .NE. SMP__LOAD ) THEN
           CALL ERR_LOG ( 1611, IUER, 'GEN_RADPLOT', 'The visibililty data '// &
     &         'were not loaded into the object VIS' )
           RETURN
      END IF
!
      CALL GETENVAR ( 'UVLEN_FCT', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F10.5)' ) UVLEN_FCT
         ELSE 
           UVLEN_FCT = 1.0
      END IF      
!
! --- Compute visibilities averaged over intermediate frequencies and over
! --- time within a scan separately for each baseline 
!
      IF ( IDEV == 4 ) THEN
           IVRB = 2
         ELSE 
           IVRB = 0
      END IF
      CALL ERR_PASS ( IUER, IER )
      CALL GET_UVLEN_AMP ( VIS, GAP_SCAN, FL_WEI_USE, FL_AUTO, CUTOFF_NERR, &
     &                     M_POI, L_POI, L_SCA, UV_LEN, AMP, ERR_AMP, &
     &                     TAI_AVR, BAS_IND, IVRB, PHI_MIN, FILOUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1612, IUER, 'GEN_RADPLOT', 'Error in an attempt '// &
     &         'to compute the avereged UV length and correlated flux '// &
     &         'density for source '//VIS%SOU_NAME )
           RETURN
      END IF
      IF ( IDEV == 4 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IND_AMP_MAX = MAX_LIST_R8 ( L_POI, AMP )
      IND_UVL_MAX = MAX_LIST_R8 ( L_POI, UV_LEN )
      AMP_MAX = AMP(IND_AMP_MAX) + ERR_AMP(IND_AMP_MAX)
      UV_LEN_MAX = UV_LEN(IND_UVL_MAX)
      IF ( L_POI == 0 ) THEN
           AMP_MAX = AMP_MAX__DEFAULT
      END IF
!
! --- Set sizes
!
      TICK_STEP_Y = 1 ! default
      X_BL = 0.0
      Y_BL = 0.0
      IF ( AMP_MAX < 0.02 ) THEN
           Y_TR = 1000.0*INT( AMP_MAX*AMP_FUDGE*10000.0 )/10000.0 + 0.0001
           TICK_STEP_Y = 1.0* INT( Y_TR/5.0 )
           IF ( TICK_STEP_Y == 0.0 ) TICK_STEP_Y = 10.0* INT( Y_TR/0.5 )
           IF ( TICK_STEP_Y == 0.0 ) TICK_STEP_Y = 100.0* INT( Y_TR/0.05 )
           IF ( TICK_STEP_Y == 0.0 ) TICK_STEP_Y = 1000.0* INT( Y_TR/0.005 )
           IF ( TICK_STEP_Y == 0.0 ) TICK_STEP_Y = 10000.0* INT( Y_TR/0.0005 )
           IF ( TICK_STEP_Y == 0.0 ) TICK_STEP_Y = 100000.0* INT( Y_TR/0.00005 )
      END IF
      IF ( AMP_MAX > 0.02 ) THEN
           Y_TR = INT( AMP_MAX*AMP_FUDGE*1000.0 )/1000.0 + 0.001
           TICK_STEP_Y = 0.01
      END IF
      IF ( AMP_MAX > 0.2 ) THEN
           Y_TR = INT( AMP_MAX*AMP_FUDGE*100.0 )/100.0 + 0.01
           TICK_STEP_Y = 0.1
      END IF
      IF ( AMP_MAX > 2.0 ) THEN
           Y_TR = INT( AMP_MAX*AMP_FUDGE*10.0 )/10.0 + 0.1
           TICK_STEP_Y = 1.0
      END IF
      IF ( Y_TR/TICK_STEP_Y > 5.0 ) TICK_STEP_Y = 2.0*TICK_STEP_Y 
      IF ( Y_TR/TICK_STEP_Y > 5.0 ) TICK_STEP_Y = 2.0*TICK_STEP_Y 
!
! --- Setting plotting parameters
!
      CALL DIAGI_SET ( 1, DIAGI_S )
!
! --- Get the band name
!
      BAND = GET_BAND ( VIS%FRQ_LO )
      IF ( BAND == 'W' ) THEN
           X_TR = 4000.0
           TICK_STEP_X = 1000.0
           ICLR_BAND = 10
        ELSE IF ( BAND == 'Q' ) THEN
           X_TR = 2000.0
           TICK_STEP_X = 500.0
           ICLR_BAND = 7
        ELSE IF ( BAND == 'A' ) THEN
           X_TR = 1500.0
           TICK_STEP_X = 500.0
           ICLR_BAND = 8
        ELSE IF ( BAND == 'K' ) THEN
           X_TR = 1000.0
           TICK_STEP_X = 200.0
           ICLR_BAND = 5
        ELSE IF ( BAND == 'U' ) THEN
           X_TR = 600.0
           TICK_STEP_X = 200.0
           ICLR_BAND = 9
        ELSE IF ( BAND == 'X' ) THEN
           X_TR = 350.0
           TICK_STEP_X = 50.0
           ICLR_BAND = 1
        ELSE IF ( BAND == 'C' ) THEN
           X_TR = 200.0
           TICK_STEP_X = 50.0
           ICLR_BAND = 3
        ELSE IF ( BAND == 'S' ) THEN
           X_TR = 90.0
           TICK_STEP_X = 20.0
           ICLR_BAND = 2
        ELSE IF ( BAND == 'L' ) THEN
           X_TR = 50.0
           TICK_STEP_X = 10.0
           ICLR_BAND = 6
        ELSE IF ( BAND == '?' ) THEN
           X_TR = 1.D7/C__VEL*VIS%FRQ_LO 
           TICK_STEP_X = X_TR/5
           ICLR_BAND = 13
      END IF
!
      CALL GETENVAR ( 'PIMAVAR_X_TR', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F8.3)' ) X_TR
      END IF
      CALL GETENVAR ( 'PIMAVAR_TICK_STEP_X', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F8.3)' ) TICK_STEP_X
      END IF
      CALL GETENVAR ( 'PIMAVAR_Y_TR', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F8.3)' ) Y_TR
      END IF
      CALL GETENVAR ( 'PIMAVAR_TICK_STEP_Y', STR )
      IF ( ILEN(STR) > 0 ) THEN
           READ ( UNIT=STR, FMT='(F8.3)' ) TICK_STEP_Y
      END IF
!
       X_TR = UVLEN_FCT*X_TR
       TICK_STEP_X = UVLEN_FCT*TICK_STEP_X
!
! --- Openning plotting device
!
      IF ( IDEV .EQ. 1 ) THEN
           ID_XW = PGOPEN ( DIAGI_S%DEVICE )
           STR1 = '/XS'
         ELSE IF ( IDEV == 2 ) THEN
           ID_XW = PGOPEN ( FILOUT(1:I_LEN(FILOUT))//'/VCPS' )
           STR1 = FILOUT
         ELSE IF ( IDEV == 3 ) THEN
           ID_XW = PGOPEN ( FILOUT(1:I_LEN(FILOUT))//'/GIF' )
           STR1 = FILOUT
      END IF
      IF ( ID_XW .LE. 0 ) THEN
           CALL CLRCH   (        STR )
           CALL INCH    ( ID_XW, STR )
           CALL ERR_LOG ( 1613, IUER, 'GEN_RADPLOT', 'Error in openning '// &
     &         'the graphic device '//STR1(1:I_LEN(STR1))//' IER='//STR )
           RETURN
      END IF
!
! --- Setting new paper size
!
      IF ( ISIZE == 1  .OR. ISIZE == 0 ) THEN
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
!
! --- Set world coordinates for plotting
!
      CALL PGPAP  ( PAP_SIZE/25.4, ASPECT )
      IF ( ISIZE == 1  .OR.  ISIZE == 0 ) THEN
           CALL PGSVP  (  0.16, 0.90,  0.16, 0.90 )
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSVP  (  0.12, 0.90,  0.13, 0.91 )
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSVP  (  0.09, 0.97,  0.12, 0.92 )
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSVP  (  0.09, 0.97,  0.11, 0.92 )
         ELSE IF ( ISIZE == 5 ) THEN
           CALL PGSVP  (  0.09, 0.97,  0.12, 0.92 )
      END IF
      CALL PGSWIN ( X_BL, X_TR, Y_BL, Y_TR )
!
! --- Setting colours
!
      DIAGI_S%NCLR  = 1
      IF ( ICLR .EQ. -1 ) THEN
!
! -------- The color was picked in accordance with the frequency
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
!
! --- Set default color
!
      CALL DIAGI_CLS ( DIAGI_S, IER )
      IF ( IDEV .NE. 1 ) THEN
           CALL PGCOL_RGB ( 0, 256, 256, 256 )
      END IF
!
      CALL PGSCI ( 1 )
!
! --- Setting default font type
!
      CALL PGSCF  ( 2 )
      IF ( IDEV == 1 ) THEN
           CALL PGERAS()       ! Erase the screen
      END IF
      IF ( ISIZE == 1  .OR.  ISIZE == 0 ) THEN
           CALL PGSCH  ( 1.8 )
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSCH  ( 1.6 )
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSCH  ( 1.4 )
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSCH  ( 1.2 )
         ELSE IF ( ISIZE == 5 ) THEN
           CALL PGSCH  ( 1.3 )
           CALL PGSLW  (   2 )
      END IF
!
! --- Draw the coordinate box
!
      CALL PGBOX  ( 'bicnts', TICK_STEP_X, 5, 'bicnts', TICK_STEP_Y, 5 )
      IF ( ISIZE == 1  .OR.  ISIZE == 0 ) THEN
           RAD = 0.015
         ELSE IF ( ISIZE == 2 ) THEN
           RAD = 0.010
         ELSE IF ( ISIZE == 3 ) THEN
           RAD = 0.008
         ELSE IF ( ISIZE == 4 ) THEN
           RAD = 0.006
         ELSE IF ( ISIZE == 5 ) THEN
           RAD = 0.009
      END IF
!
      XRAD = RAD*X_TR*ASPECT
      YRAD = RAD*Y_TR
      CALL PGSCI   ( ITAB_CLR(1,1) )
      CALL PGSLW ( 1 )
!
! --- Plot error bar
!
      DO 410 J1=1,L_POI
         X_AR2(1) = UV_LEN(J1)*1.D-6
         X_AR2(2) = UV_LEN(J1)*1.D-6
         IF ( AMP_MAX > 0.02 ) THEN
              Y_AR2(1) = AMP(J1) - ERR_AMP(J1)
              Y_AR2(2) = AMP(J1) + ERR_AMP(J1)
           ELSE 
              Y_AR2(1) = (AMP(J1) - ERR_AMP(J1))*1000.0
              Y_AR2(2) = (AMP(J1) + ERR_AMP(J1))*1000.0
         END IF
         CALL PGLINE ( 2, X_AR2, Y_AR2 )
         IF ( ISIZE == 0 ) THEN
              STR = MJDSEC_TO_DATE ( VIS%MJD_REF, TAI_AVR(J1), -2 )                  
              IND_STA(1) = BAS_IND(J1)/256
              IND_STA(2) = BAS_IND(J1) - IND_STA(1)*256
              WRITE  ( 80, 110 ) BAND, VIS%SOU_NAME, STR(1:21), &               
          &                      VIS%C_STA(IND_STA(1)), VIS%C_STA(IND_STA(2)), &
     &                           UV_LEN(J1)*299792458.0D0/VIS%FRQ_LO, &     
          &                      AMP(J1), ERR_AMP(J1)
              WRITE  ( 6, 110 ) BAND, VIS%SOU_NAME, STR(1:21), &               
          &                      VIS%C_STA(IND_STA(1)), VIS%C_STA(IND_STA(2)), &
     &                           UV_LEN(J1)*299792458.0D0/VIS%FRQ_LO, &     
          &                      AMP(J1), ERR_AMP(J1)
 110          FORMAT ( 'CORR_AMP', 2X, A, 2X, A, 2X, A, 2X, A, ' / ', A, &
     &                 F11.1, 1X, F7.4, 2X, F7.5 )                                   
          END IF
 410  CONTINUE 
      DO 420 J2=1,L_POI
         IF ( ISIZE == 1  .OR.  ISIZE == 0 ) THEN
              CALL PGSLW ( 1 )
            ELSE IF ( ISIZE == 2 ) THEN
              CALL PGSLW ( 2 )
            ELSE IF ( ISIZE == 3 ) THEN
              CALL PGSLW ( 3 )
            ELSE IF ( ISIZE == 4 ) THEN
              CALL PGSLW ( 3 )
            ELSE IF ( ISIZE == 5 ) THEN
              CALL PGSLW ( 3 )
         END IF
!
! ------ First plot the white disk in order to erase that 
! ------ what has been already drawn
!
         CALL PGSCI ( 0 )
         CALL PGSFS ( 1 )
         IF ( AMP_MAX > 0.02 ) THEN
              CALL PGCIRC_PET ( 12, SNGL(UV_LEN(J2)*1.D-6), SNGL(AMP(J2)), &
     &                          XRAD, YRAD )
            ELSE 
              CALL PGCIRC_PET ( 12, SNGL(UV_LEN(J2)*1.D-6), 1000.0*SNGL(AMP(J2)), &
     &                          XRAD, YRAD )
         END IF
!
! ------ Then draw the circle
!
         CALL PGSCI ( ITAB_CLR(1,1) )
         CALL PGSFS ( 2 )
         IF ( AMP_MAX > 0.02 ) THEN
              CALL PGCIRC_PET ( 12, SNGL(UV_LEN(J2)*1.D-6), SNGL(AMP(J2)), &
     &                          XRAD, YRAD )
            ELSE
              CALL PGCIRC_PET ( 12, SNGL(UV_LEN(J2)*1.D-6), 1000.0*SNGL(AMP(J2)), &
     &                          XRAD, YRAD )
         END IF
 420  CONTINUE 
!
! --- Set sizes for drawing the annotation
!
      CALL PGSLW  ( 1 )
      IF ( ISIZE == 1  .OR.  ISIZE == 0 ) THEN
           X_CP = X_BL - 0.10*(X_TR-X_BL)
           Y_CP = Y_BL - 0.18*(Y_TR-Y_BL)
           CALL PGSCH  ( 2.4 )
        ELSE IF ( ISIZE == 2 ) THEN
           X_CP = X_BL - 0.09*(X_TR-X_BL)
           Y_CP = Y_BL - 0.15*(Y_TR-Y_BL)
           CALL PGSCH  ( 2.0 )
        ELSE IF ( ISIZE == 3 ) THEN
           X_CP = X_BL - 0.072*(X_TR-X_BL)
           Y_CP = Y_BL - 0.13*(Y_TR-Y_BL)
           CALL PGSCH  ( 2.0 )
        ELSE IF ( ISIZE == 4 ) THEN
           X_CP = X_BL - 0.06*(X_TR-X_BL)
           Y_CP = Y_BL - 0.12*(Y_TR-Y_BL)
           CALL PGSCH  ( 2.2 )
        ELSE IF ( ISIZE == 5 ) THEN
           X_CP = X_BL - 0.06*(X_TR-X_BL)
           Y_CP = Y_BL - 0.12*(Y_TR-Y_BL)
           CALL PGSCH  ( 2.0 )
           CALL PGSLW  ( 3   )
      END IF
!
! --- Draw the annotation
!
      CALL PGSCI  ( 1 )
      IF ( AMP_MAX > 0.02 ) THEN
           CALL PGPTXT ( X_CP, (Y_TR+Y_BL)/2.0, 90.0, 0.5, &
     &                   'Correlated flux density (Jy)' )
         ELSE 
           CALL PGPTXT ( X_CP, (Y_TR+Y_BL)/2.0, 90.0, 0.5, &
     &                   'Correlated flux density (mJy)' )
      END IF
!
      IF ( FL_AUTO ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:4), FMT='(F4.0)' ) PHI_MIN/DEG__TO__RAD
           CALL CLRCH  ( STR(4:4) )
           CALL CHASHL ( STR )
           CALL PGPTXT ( (X_TR+X_BL)/2.0, Y_CP, 0.0, 0.55, &
     &                   'Baseline projection length at angle '// &
     &                    STR(1:I_LEN(STR))//'\u\m22\d (M\gl)' )
         ELSE
           CALL PGPTXT ( (X_TR+X_BL)/2.0, Y_CP, 0.0, 0.5, &
     &                   'Baseline length projection (M\gl)' )
      END IF
!
! --- Set size for drawing the header
!
      CALL PGSLW   ( 1   )
      IF ( ISIZE == 1  .OR.  ISIZE == 0 ) THEN
           CALL PGSCH   ( 3.0 )
           CALL PGSLW   ( 3 )
           X_DAT        =  0.0
           X_HEA_CENTER =  0.5
           Y_HEA_FACTOR =  0.05
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSCH   ( 2.4 )
           CALL PGSLW   ( 4 )
           X_DAT        =  0.0
           X_HEA_CENTER =  0.5
           Y_HEA_FACTOR =  0.05
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSCH   ( 2.4 )
           CALL PGSLW   ( 4 )
           X_DAT        = 0.0
           X_HEA_CENTER = 0.5
           Y_HEA_FACTOR = 0.05
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSCH   ( 2.4 )
           CALL PGSLW   ( 6 )
           X_DAT        = 0.20
           X_HEA_CENTER = 0.5
           Y_HEA_FACTOR = 0.04
         ELSE IF ( ISIZE == 5 ) THEN
           CALL PGSCH   ( 2.4 )
           CALL PGSLW   ( 6 )
           X_DAT        = 0.20
           X_HEA_CENTER = 0.5
           Y_HEA_FACTOR = 0.04
      END IF
!
! --- Print the source name
!
      CALL PGPTXT  ( (X_TR+X_BL)/2.0, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), &
     &               0.0, X_HEA_CENTER, VIS%SOU_NAME )
      CALL PGSCI ( 1 )
      IF ( ISIZE == 5 ) THEN
           CALL PGSLW   ( 2 )
         ELSE 
           CALL PGSLW   ( 1 )
      END IF
!
! --- Printing the date in the upper left corner
!
      STR = MJDSEC_TO_DATE ( VIS%MJD(1), VIS%TAI(1), -2 )
      IF ( ISIZE == 1  .OR.  ISIZE == 0 ) THEN
           CALL PGSCH   ( 1.8 )
           X_DATE_CENTER = 0.1
         ELSE 
           CALL PGSCH   ( 1.6 )
           X_DATE_CENTER = 0.0
      END IF
      CALL CLRCH ( STR(11:) )
      IF ( VIS%NUM_SEG == 1 ) THEN
           CONTINUE 
         ELSE 
!
! -------- If the map was created using more than one epoch, put
! -------- the number of epochs
!
           STR(11:11) = '('
           CALL INCH ( VIS%NUM_SEG, STR(12:20) )
           CALL CHASHL ( STR(12:20) )
           STR(ILEN(STR)+1:ILEN(STR)+1) = ')'
      END IF 
      IF ( ISIZE == 5 ) THEN
           CALL PGSCH   ( 1.8 )
      END IF
      CALL PGPTXT  ( X_BL, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), &
     &               0.0, X_DATE_CENTER, STR(1:I_LEN(STR)) )
!
! --- Compute the averaged frequency
!
      FRQ_AVR = 0.0D0
      DO 430 J3=1,VIS%NFRQ
         FRQ_AVR = FRQ_AVR + VIS%SKY_FRQ(J3)
 430  CONTINUE 
!
! --- Draw the sky frequency at the upper right corner
!
      FRQ_AVR = FRQ_AVR/VIS%NFRQ
      WRITE ( UNIT=STR, FMT='("Freq: ",F4.1," GHz")' ) 1.D-9*FRQ_AVR
      IF ( STR(7:7) == ' ' ) CALL CHASHL ( STR(7:) )
      IF ( ISIZE == 1 ) THEN
           STR = STR(1:1)//STR(5:)
      END IF
      IF ( ISIZE == 5 ) THEN
           CALL PGSLW ( 2 )
      END IF
      CALL PGSCI   ( ITAB_CLR(1,1) )
      XFREQ_CENTER = 1.0
      CALL PGPTXT  ( X_TR, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, &
     &               STR(1:I_LEN(STR)) )
!
! --- Put a strut
!
      CALL PGSCI   ( 0 )
      IF ( ISIZE == 5 ) THEN
           CALL PGPTXT  ( 1.05*X_TR, Y_TR + 1.50*Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, '|o' )
           CALL PGPTXT  ( 1.05*X_TR, Y_BL - 3.30*Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, '|o' )
           CALL PGPTXT  ( X_BL - 0.07*(X_TR-X_BL), Y_BL - 3.30*Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, 'o|' )
         ELSE 
           CALL PGPTXT  ( 1.05*X_TR, Y_TR + 1.10*Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, '|o' )
      END IF 
      CALL PGSCI ( 1 )
!
      XC = 0.0
      YC = 0.0
!
      IF ( IDEV .EQ. 1 ) THEN
!
! -------- Asking the user input
!
           CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
           CALL PGCLOQ()
         ELSE
           CALL PGCLOQ()
           IF ( IDEV == 2 .AND. ISIZE == 1  .OR.  &
     &          IDEV == 2 .AND. ISIZE == 2        ) THEN
!
! ------------- A special trick: make lines in postscript file thinner
!
                COM_STR = 'sed -i '// &
     &                '"s@/LW {5 mul setlinewidth} bind def'// &
     &                  '@/LW {2 mul setlinewidth} bind def@g" '//FILOUT
                CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
           END IF
!@           WRITE ( 6, * ) 'Output file: '//FILOUT(1:I_LEN(FILOUT))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GEN_RADPLOT !#!  
