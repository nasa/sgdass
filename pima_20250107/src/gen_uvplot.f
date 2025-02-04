      SUBROUTINE GEN_UVPLOT ( VIS, IDEV, ICLR, ISIZE, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GEN_UVPLOT  generates the plot of the UV coverage for     *
! *   observations of a source with VLBI. The information about the      *
! *   uv coordiantes is taken from the visibility data. Teh data which   *
! *   have been flagged out are not shown. The data are averaged in      *
! *   frequency within the band and in time within each scan at each     *
! *   baseline.                                                          *
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
! *   ICLR ( INTEGER*4 ) -- color index according to DiaGI.              *
! *                         Valid rnage -1, 0, or [1, 64].               *
! *                         Color index -1 indicates that the color of   *
! *                         the plot will be selected in accordance      *
! *                         with the observing frequency.                *
! *                         Color index 0 means black.                   *
! *  ISIZE ( INTEGER*4 ) -- size code:                                   *
! *                         1 -- 36x44 mm                                *
! *                         2 -- 72x90 mm                                *
! *                         3 -- 130x172 mm                              *
! *                         4 -- 200x265 mm                              *
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
! *  ### 30-JAN-2007  GEN_UVPLOT   v1.0 (c)  L. Petrov 30-JAN-2007  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'astro_constants.i'
      INCLUDE    'diagi.i'
      INCLUDE    'sou_map.i'
      INTEGER*4  IDEV, ICLR, ISIZE, IUER
      CHARACTER  FILOUT*(*)
      TYPE     ( VIS__TYPE  ) :: VIS
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      INTEGER*4  M_POI
      PARAMETER  ( M_POI = 64*1024 )
      REAL*8     C__VEL
      PARAMETER  ( C__VEL = 299792458.0D0  )
      REAL*4     PAP_SIZE
      REAL*4     X_BL, X_TR, Y_BL, Y_TR, TICK_STEP, XC, YC, &
     &           XRAD, YRAD, X_CP, Y_CP, &
     &           X_HEA_CENTER, Y_HEA_FACTOR, XFREQ_CENTER, X_DATE_CENTER
      REAL*4     RAD, UV_ARR(M_POI,2)
      REAL*8     FRQ_AVR, GAP_SCAN
      INTEGER*4  J1, J2, J3, J4, J5, L_POI, L_SCA, ID_XW, MODE, ICLR_BAND, IER
      CHARACTER  STR*256, CH*1, COM_STR*256, BAND*1
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_BAND*1
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, PGOPEN
!
      MODE = 2
      GAP_SCAN = 300.0D0
!
      IF ( VIS%STATUS .NE. SMP__LOAD ) THEN
           CALL ERR_LOG ( 1661, IUER, 'GEN_UVPLOT', 'The visibililty data '// &
     &         'were not loaded into the object VIS' )
           RETURN
      END IF
!
! --- Set world coordinates for plotting
!
      BAND = GET_BAND ( VIS%FRQ_LO )
      IF ( BAND == 'W' ) THEN
           X_BL = -4000.0
           TICK_STEP = 2000.0
           ICLR_BAND = 10
        ELSE IF ( BAND == 'Q' ) THEN
           X_BL = -2000.0
           TICK_STEP = 500.0
           ICLR_BAND = 7
        ELSE IF ( BAND == 'A' ) THEN
           X_BL = -1500.0
           TICK_STEP = 500.0
           ICLR_BAND = 8
        ELSE IF ( BAND == 'K' ) THEN
           X_BL = -1000.0
           TICK_STEP = 200.0
           ICLR_BAND = 5
        ELSE IF ( BAND == 'U' ) THEN
           X_BL = -600.0
           TICK_STEP = 200.0
           ICLR_BAND = 9
        ELSE IF ( BAND == 'X' ) THEN
           X_BL = -350.0
           TICK_STEP = 100.0
           ICLR_BAND = 1
        ELSE IF ( BAND == 'C' ) THEN
           X_BL = -200.0
           TICK_STEP = 50.0
           ICLR_BAND = 3
        ELSE IF ( BAND == 'S' ) THEN
           X_BL = -90.0
           TICK_STEP = 20.0
           ICLR_BAND = 2
        ELSE IF ( BAND == 'L' ) THEN
           X_BL = -50.0
           TICK_STEP = 20.0
           ICLR_BAND = 6
        ELSE IF ( BAND == '?' ) THEN
           X_BL = -1.D7/C__VEL*VIS%FRQ_LO 
           TICK_STEP = -X_BL/5
           ICLR_BAND = 13
      END IF
      X_TR =  -X_BL
      Y_BL =   X_BL
      Y_TR =  -X_BL
!
! --- Setting plotting parameters
!
      CALL DIAGI_SET ( 1, DIAGI_S )
!
! --- Openning plotting device
!
      IF ( IDEV .EQ. 1 ) THEN
           ID_XW = PGOPEN ( DIAGI_S%DEVICE )
         ELSE IF ( IDEV == 2 ) THEN
           ID_XW = PGOPEN ( FILOUT(1:I_LEN(FILOUT))//'/VCPS' )
         ELSE IF ( IDEV == 3 ) THEN
           ID_XW = PGOPEN ( FILOUT(1:I_LEN(FILOUT))//'/GIF' )
      END IF
      IF ( ID_XW .LE. 0 ) THEN
           CALL CLRCH   (        STR )
           CALL INCH    ( ID_XW, STR )
           CALL ERR_LOG ( 6262, IUER, 'GEN_UVPLOT', 'Error in openning '// &
     &         'the graphic device '//DIAGI_S%DEVICE//' IER='//STR )
           RETURN
      END IF
!
! --- Setting new paper size
!
      IF ( ISIZE == 1 ) THEN
           PAP_SIZE = 50.0
         ELSE IF ( ISIZE == 2 ) THEN
           PAP_SIZE = 100.0
         ELSE IF ( ISIZE == 3 ) THEN
           PAP_SIZE = 176.0
         ELSE IF ( ISIZE == 4 ) THEN
           PAP_SIZE = 270.0
      END IF
!
      CALL PGPAP  ( PAP_SIZE/25.4, 1.0 )
      IF ( ISIZE == 1 ) THEN
           CALL PGSVP  (  0.25, 0.95,  0.20, 0.90 )
!@           CALL PGSVP  (  0.16, 0.90,  0.16, 0.90 )
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSVP  (  0.25, 0.95,  0.20, 0.90 )
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSVP  (  0.12, 0.97,  0.12, 0.92 )
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSVP  (  0.11, 0.97,  0.11, 0.92 )
      END IF
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
           DIAGI_S%ICOL(1) = 13 ! Default color
      END IF
      CALL DIAGI_CLS ( DIAGI_S, IER )
      IF ( IDEV .NE. 1 ) THEN
           CALL PGCOL_RGB ( 0, 256, 256, 256 )
      END IF
      CALL PGCOL_RGB ( 100, 128, 128, 128 )
      CALL PGSCI ( 1 )
!
! --- Setting default font type
!
      CALL PGSCF  ( 2 )
      IF ( IDEV == 1 ) THEN
           CALL PGERAS()       ! Erase the screen
      END IF
      CALL PGSWIN ( X_BL, X_TR, Y_BL, Y_TR )
      IF ( ISIZE == 1 ) THEN
           CALL PGSCH  ( 1.6 )
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSCH  ( 1.4 )
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSCH  ( 1.2 )
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSCH  ( 1.2 )
      END IF
!
      CALL PGBOX  ( 'bicnts', TICK_STEP, 5, 'bicnts', TICK_STEP, 5 )
      IF ( ISIZE == 1 ) THEN
           RAD = 0.018
         ELSE IF ( ISIZE == 2 ) THEN
           RAD = 0.015
         ELSE IF ( ISIZE == 3 ) THEN
           RAD = 0.011
         ELSE IF ( ISIZE == 4 ) THEN
           RAD = 0.008
      END IF
!
      XRAD = RAD*X_TR
      YRAD = RAD*Y_TR
      CALL PGSCI   ( ITAB_CLR(1,1) )
      CALL PGSLW ( 1 )
      IF ( MODE == 1 ) THEN
           DO 410 J1=1,VIS%NP
              DO 420 J2=1,4 ! VIS%NFRQ
                 CALL PGSCI ( ITAB_CLR(1,1) )
                 CALL PGSFS ( 1 )
                 CALL PGCIRC_PET ( 8,  VIS%UV(1,J2,J1)*1.E-6, &
     &                                 VIS%UV(2,J2,J1)*1.E-6, XRAD, YRAD )
                 CALL PGCIRC_PET ( 8, -VIS%UV(1,J2,J1)*1.E-6, &
     &                                -VIS%UV(2,J2,J1)*1.E-6, XRAD, YRAD )
 420          CONTINUE 
 410       CONTINUE 
         ELSE 
           CALL ERR_PASS   ( IUER, IER )
           CALL GET_UV_AVR ( VIS, GAP_SCAN, M_POI, L_POI, L_SCA, UV_ARR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6262, IUER, 'GEN_UVPLOT', 'Error in '// &
     &              'attempt to compute veraged UV coordinates' )
                RETURN
           END IF
!
           DO 430 J3=1,L_POI
              CALL PGSCI ( ITAB_CLR(1,1) )
              CALL PGSFS ( 1 )
!@  write ( 6, * ) ' poi=', int2(j3),' uv=',UV_ARR(J3,1)*1.E-6,  UV_ARR(J3,2)*1.E-6 ! %%%%%%%
              CALL PGCIRC_PET ( 8,  UV_ARR(J3,1)*1.E-6,  UV_ARR(J3,2)*1.E-6, &
     &                              XRAD, YRAD )
              CALL PGCIRC_PET ( 8, -UV_ARR(J3,1)*1.E-6, -UV_ARR(J3,2)*1.E-6, &
     &                              XRAD, YRAD )
 430       CONTINUE 
      END IF
!  write ( 6, * ) ' l_poi = ', l_poi, ' kk_poi = ', vis%np*vis%nfrq ! %%
!  write ( 6, * ) ' x_bl=',x_bl, ' x_tr=', x_tr,' y_bl=', y_bl,' y_tr=',y_tr ! %%
!  write ( 6, * ) ' xrad=',xrad,' yrad=',yrad ! %%%
      CALL PGSCI  ( 1 )
      IF ( ISIZE == 1 ) THEN
           X_CP = X_BL - 0.15*(X_TR-X_BL)
           Y_CP = Y_BL - 0.18*(Y_TR-Y_BL)
           CALL PGSCH  ( 2.2 )
        ELSE IF ( ISIZE == 2 ) THEN
           X_CP = X_BL - 0.10*(X_TR-X_BL)
           Y_CP = Y_BL - 0.14*(Y_TR-Y_BL)
           CALL PGSCH  ( 1.4 )
        ELSE IF ( ISIZE == 3 ) THEN
           X_CP = X_BL - 0.10*(X_TR-X_BL)
           Y_CP = Y_BL - 0.135*(Y_TR-Y_BL)
           CALL PGSCH  ( 2.0 )
        ELSE IF ( ISIZE == 4 ) THEN
           X_CP = X_BL - 0.09*(X_TR-X_BL)
           Y_CP = Y_BL - 0.12*(Y_TR-Y_BL)
           CALL PGSCH  ( 2.0 )
      END IF
!
      CALL PGSLW  ( 1 )
      CALL PGPTXT ( X_CP, (Y_TR+Y_BL)/2.0, 90.0, 0.5, &
     &              'U Baseline projection (M\gl)' )
!
      CALL PGPTXT ( (X_TR+X_BL)/2.0, Y_CP, 0.0, 0.5, &
     &              'V Baseline projection (M\gl)' )
      IF ( ISIZE == 1 ) THEN
           CALL PGSCH   ( 2.4 )
           CALL PGSLW   ( 3 )
           X_HEA_CENTER =  0.55
           Y_HEA_FACTOR =  0.05
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSCH   ( 2.0 )
           CALL PGSLW   ( 4 )
           X_HEA_CENTER =  0.6
           Y_HEA_FACTOR =  0.05
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSCH   ( 2.4 )
           CALL PGSLW   ( 4 )
           X_HEA_CENTER = 0.667
           Y_HEA_FACTOR = 0.04
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSCH   ( 2.4 )
           CALL PGSLW   ( 6 )
           X_HEA_CENTER = 0.667
           Y_HEA_FACTOR = 0.04
      END IF
!
! --- Print the source name
!
      CALL PGPTXT  ( (X_TR+X_BL)/2.0, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), &
     &               0.0, X_HEA_CENTER, VIS%SOU_NAME )
      CALL PGSCI ( 1 )
!
! --- Printing the date in the upper left corner
!
      STR = MJDSEC_TO_DATE ( VIS%MJD, VIS%TAI, -2 )
      CALL PGSLW   ( 1   )
      IF ( ISIZE == 1 ) THEN
           CALL PGSCH   ( 1.4 )
           X_DATE_CENTER = 0.4
         ELSE IF ( ISIZE == 2 ) THEN
           CALL PGSCH   ( 1.4 )
           X_DATE_CENTER = 0.3
         ELSE IF ( ISIZE == 3 ) THEN
           CALL PGSCH   ( 1.4 )
           X_DATE_CENTER = 0.3
         ELSE IF ( ISIZE == 4 ) THEN
           CALL PGSCH   ( 1.4 )
           X_DATE_CENTER = 0.3
      END IF
      CALL PGPTXT  ( X_BL, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), &
     &               0.0, X_DATE_CENTER, STR(1:10) )
!
      FRQ_AVR = 0.0D0
      DO 440 J4=1,VIS%NFRQ
         FRQ_AVR = FRQ_AVR + VIS%SKY_FRQ(J4)
 440  CONTINUE 
!
      FRQ_AVR = FRQ_AVR/VIS%NFRQ
      WRITE ( UNIT=STR, FMT='("Freq: ",F4.1," GHz")' ) 1.D-9*FRQ_AVR
      IF ( STR(7:7) == ' ' ) CALL CHASHL ( STR(7:) )
      IF ( ISIZE == 1 .OR. ISIZE == 2 ) THEN
           STR = STR(1:1)//STR(5:)
           XFREQ_CENTER = 0.85
         ELSE 
           XFREQ_CENTER = 1.0
      END IF
      CALL PGSCI   ( ITAB_CLR(1,1) )
      CALL PGPTXT  ( X_TR, Y_TR + Y_HEA_FACTOR*(Y_TR-Y_BL), 0.0, XFREQ_CENTER, &
     &               STR(1:I_LEN(STR)) )
!
! --- Asking the user input
!
      XC = 0.0
      YC = 0.0
      CALL PGSCI ( 1 )
      IF ( IDEV .EQ. 1 ) THEN
           CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
           CALL PGCLOQ()
         ELSE
           CALL PGCLOQ()
           IF ( IDEV == 2 .AND. ISIZE == 1  .OR.  &
     &          IDEV == 2 .AND. ISIZE == 2        ) THEN
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
      END  SUBROUTINE  GEN_UVPLOT  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE REMOVE_KEY ( IND_KEY, L_KEY, KEYS )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine REMOVE_KEY removes a key from the table KEYS.   *
! *                                                                      *
! *  ### 18-JAN-2007  REMOVE_KEY   v1.0 (c)  L. Petrov  18-JAN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND_KEY, L_KEY
      CHARACTER  KEYS(*)*(*)
      INTEGER*4  J1
!
      IF ( IND_KEY == L_KEY ) THEN
           L_KEY = L_KEY - 1
         ELSE
           DO 410 J1=IND_KEY,L_KEY-1
              KEYS(J1) = KEYS(J1+1)
 410       CONTINUE 
           L_KEY = L_KEY - 1
      END IF
      IF ( L_KEY < 0 ) L_KEY = 0
      RETURN
      END  SUBROUTINE  REMOVE_KEY  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADD_KEY ( IND_KEY, MKEY, L_KEY, KEYS, REC )
! ************************************************************************
! *                                                                      *
! *   Auxilliary routine REMOVE_KEY adds a key to the table KEYS just    *
! *   after IND_KEY -th key.                                             *
! *                                                                      *
! *  ### 18-JAN-2007    ADD_KEY    v1.0 (c)  L. Petrov  18-JAN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IND_KEY, MKEY, L_KEY
      CHARACTER  KEYS(MKEY)*(*), REC*(*)
      INTEGER*4  J1
!
      IF ( IND_KEY == 0 ) THEN
           L_KEY = L_KEY + 1
           KEYS(L_KEY) = REC
         ELSE
           L_KEY = L_KEY + 1
           DO 410 J1=L_KEY,IND_KEY+2,-1
              KEYS(J1) = KEYS(J1-1)
 410       CONTINUE 
           KEYS(IND_KEY+1) = REC
      END IF
      RETURN
      END  SUBROUTINE ADD_KEY  !#!#
