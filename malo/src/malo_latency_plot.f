      PROGRAM    MALO_LATENCY_PLOT_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL MALO_LATENCY_PLOT()
      END  PROGRAM  MALO_LATENCY_PLOT_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  MALO_LATENCY_PLOT()
! ************************************************************************
! *                                                                      *
! *   Program  MALO_LATENCY_PLOT generates plots of latencies of MALO    *
! *   services.                                                          *
! *                                                                      *
! * ## 24-JUL-2014 MALO_LATENCY_PLOT v2.1 (c) L. Petrov  03-MAR-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MP, MC, ML, MLL, MIND
      PARAMETER  ( MP  = 1024*1024 )
      PARAMETER  ( MC  = 1024      )
      PARAMETER  ( ML  = 64*1024   )
      PARAMETER  ( MLL  = 60       )
      PARAMETER  ( MIND = 32       )
      CHARACTER  FIL_CONF*128, FIL_LAT*128, DIR_PLOT*128, DATE_STR*16, &
     &           FILOUT*128, TITLE*128, UNITS*8, SUF*1, STR*2
      CHARACTER  BUFC(MC)*256, BUFL(MP)*256
      REAL*8     TIM_EPOCH, TIM_UPDATE, TIM_LAST, TIM_STEP, TIM_ARR(ML), &
     &           LAT_AVR(ML), LAT_MIN(ML), LAT_MAX(ML), LAT_LONG, LAT_SHORT
      INTEGER*4  J1, J2, J3, NC, NL, KL, NF, IS, MODE, IN, MJD_EPOCH, &
     &           MJD_UPDATE, MJD_LAST, NE(MP), LIND, IND(2,MIND), &
     &           LJIND, JND(2,MIND), IVAL, IND_SUF, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, * ) 'Usage conf_file latency_file directory_plot'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_CONF )
           CALL GETARG ( 2, FIL_LAT  )
           CALL GETARG ( 3, DIR_PLOT )
      END IF
!
! --- Read configuraton file
!
      IUER = -1
      CALL RD_TEXT ( FIL_CONF, MC, BUFC, NC, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IS = 0
      DO 410 J1=1,NC
!
! ------ Parse a line of the configuration file
!
         CALL EXWORD ( BUFC(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( BUFC(J1)(IND(1,1):IND(2,1)) == 'short_latency:' ) THEN
              CALL CHIN ( BUFC(J1)(IND(1,2):IND(2,2)), IVAL )
              IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'hour' ) THEN
                   LAT_SHORT = 3600.0D0*IVAL
                 ELSE IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'Hour' ) THEN
                   LAT_SHORT = 3600.0D0*IVAL
                 ELSE IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'day' ) THEN
                   LAT_SHORT = 86400.0D0*IVAL
                 ELSE 
                   LAT_SHORT = 1.0D0*IVAL
              END IF
            ELSE IF ( BUFC(J1)(IND(1,1):IND(2,1)) == 'long_latency:' ) THEN
              CALL CHIN ( BUFC(J1)(IND(1,2):IND(2,2)), IVAL )
              IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'hour' ) THEN
                   LAT_LONG = 3600.0D0*IVAL
                 ELSE IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'Hour' ) THEN
                   LAT_LONG = 3600.0D0*IVAL
                 ELSE IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'day' ) THEN
                   LAT_LONG  = 86400.0D0*IVAL
                 ELSE 
                   LAT_LONG  = 1.0D0*IVAL
              END IF
            ELSE IF ( BUFC(J1)(IND(1,1):IND(2,1)) == 'tim_step:' ) THEN
              IF ( INDEX ( BUFC(J1)(IND(1,2):IND(2,2)), '.' ) == 0 ) THEN
                   CALL CHIN ( BUFC(J1)(IND(1,2):IND(2,2)), IVAL )
                   TIM_STEP = IVAL
                 ELSE 
                   READ ( UNIT=BUFC(J1)(IND(1,2):IND(2,2)), FMT='(F10.5)' ) TIM_STEP
              END IF
              IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'day' ) THEN
                   TIM_STEP = 86400.0D0*TIM_STEP
                ELSE IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'hour' ) THEN
                   TIM_STEP = 3600.0D0*TIM_STEP
                ELSE IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'Hour' ) THEN
                   TIM_STEP = 3600.0D0*TIM_STEP
                ELSE IF ( BUFC(J1)(IND(1,3):IND(2,3)) == 'min' ) THEN
                   TIM_STEP = 60.0D0*TIM_STEP
                ELSE
                   CONTINUE 
              END IF
            ELSE IF ( BUFC(J1)(IND(1,1):IND(2,1)) == 'service:' ) THEN
              SUF = BUFC(J1)(IND(2,2):IND(2,2)) 
!
! ----------- Incement IS -- service counter
!
              IS = IS + 1
!
! ----------- Read the file with latencies
!
              IUER = -1
              CALL RD_TEXT ( FIL_LAT, MP, BUFL, NL, IUER )
              IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
! ----------- Get the last date for ith latenceis are availble
!
              IUER = -1
              CALL DATE_TO_TIME ( BUFL(NL)(1:16), MJD_LAST, TIM_LAST, IUER )
              IF ( IUER .NE. 0 ) THEN
                   IUER = -1
                   WRITE ( 6, * ) 'NL = ', NL
                   CALL ERR_LOG ( 5601, IUER, 'MALO_LATENCY_PLOT', 'Wrong date: '// &
     &                  BUFL(NL)(1:16)//' in the latechny file '//FIL_LAT )
                   CALL EXIT ( 1 )
              END IF 
              UNITS   = BUFC(J1)(IND(1,3):IND(2,3))//'s'
!
! ----------- Cycle over two modes: 
! ----------- the first  mode plots actual latencies for last five days. 
! ----------- the second mode plots min, max and mean latencies averaged over one day
!
              DO 420 J2=1,2
                 KL = 0
                 NE = 0
                 MODE = J2
!
                 DO 430 J3=NL,1,-1
                    IF ( INDEX( BUFL(J3), 'a' ) > 0 .OR. INDEX( BUFL(J3), 'c' ) > 0 ) THEN
                         CALL EXWORD ( BUFL(J3), MIND, LJIND, JND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
                         IND_SUF = INDEX ( BUFL(J3)(JND(1,2):JND(2,2)), SUF )
                         IF ( IND_SUF < 1 ) GOTO 430
!
! ---------------------- Extract the date of the latency recording
!
                         IUER = -1
                         CALL DATE_TO_TIME ( BUFL(J3)(1:16), MJD_EPOCH, TIM_EPOCH, IUER )
                         IF ( IUER .NE. 0 ) THEN
                              IUER = -1
                              WRITE ( 6, * ) 'NL = ', NL, ' J3= ',J3
                              WRITE ( 6, * ) 'BUFL = ', TRIM(BUFL(J3))
                              CALL ERR_LOG ( 5602, IUER, 'MALO_LATENCY_PLOT', 'Wrong date: '// &
     &                             BUFL(J3)(1:16)//' in the latency file '//FIL_LAT )
                              CALL EXIT ( 1 )
                         END IF 
!
                         DATE_STR = BUFL(J3)(JND(1,2+IND_SUF):JND(2,2+IND_SUF))
                       ELSE
                         IUER = -1
                         IF ( ILEN(BUFL(J3)(IS*14+4:IS*14+16)) == 0 ) GOTO 430 ! Bypass empty field
!
! ---------------------- Extract the date of the latency recording
!
                         IUER = -1
                         CALL DATE_TO_TIME ( BUFL(J3)(1:16), MJD_EPOCH, TIM_EPOCH, IUER )
                         IF ( IUER .NE. 0 ) THEN
                              IUER = -1
                              WRITE ( 6, * ) 'NL = ', NL, ' J3= ',J3
                              CALL ERR_LOG ( 5603, IUER, 'MALO_LATENCY_PLOT', 'Wrong date: '// &
     &                             BUFL(J3)(1:16)//' in the latechny file '//FIL_LAT )
                              CALL EXIT ( 1 )
                         END IF 
!
                         DATE_STR = BUFL(J3)(IS*14+4:IS*14+7)//'.'// &
     &                              BUFL(J3)(IS*14+8:IS*14+9)//'.'// &
     &                              BUFL(J3)(IS*14+10:IS*14+14)//':'// &
     &                              BUFL(J3)(IS*14+15:IS*14+16)
                    END IF
!
! ----------------- Extract the date of results availability
!
                    IUER = -1
                    CALL DATE_TO_TIME ( DATE_STR, MJD_UPDATE, TIM_UPDATE, IUER )
                    IF ( IUER .NE. 0 ) THEN
                         IUER = -1
                         WRITE ( 6, * ) 'NL = ', NL, ' J3= ',J3
                         CALL ERR_LOG ( 5604, IUER, 'MALO_LATENCY_PLOT', 'Wrong date: '// &
     &                        DATE_STR//' in the latechny file '//FIL_LAT )
                         CALL EXIT ( 1 )
                    END IF 
                    IF ( MODE == 1 ) THEN
!
! ---------------------- Just raw latencies
!
                         KL = KL + 1
                         TIM_ARR(KL) = (MJD_EPOCH - MJD_LAST)   + (TIM_EPOCH - TIM_LAST)/86400.0D0
                         LAT_AVR(KL) = ( (MJD_EPOCH - MJD_UPDATE) + &
     &                                  (TIM_EPOCH - TIM_UPDATE)/86400.0D0 )
                       ELSE 
!
! ---------------------- Compute min, max and mean latencies
!
                         IN = (MJD_LAST - MJD_EPOCH) + 1
                         NE(IN) = NE(IN) + 1
                         IF ( NE(IN) == 1 ) THEN
                              KL = KL + 1
                              LAT_MIN(KL) = ( (MJD_EPOCH - MJD_UPDATE) + &
     &                                        (TIM_EPOCH - TIM_UPDATE)/86400.0D0 )
                              LAT_MAX(KL) = ( (MJD_EPOCH - MJD_UPDATE) + &
     &                                        (TIM_EPOCH - TIM_UPDATE)/86400.0D0 )
                              LAT_AVR(KL) = ( (MJD_EPOCH - MJD_UPDATE) + &
     &                                        (TIM_EPOCH - TIM_UPDATE)/86400.0D0 )
                              TIM_ARR(KL) = (MJD_EPOCH - MJD_LAST) + 0.5
                            ELSE 
                              LAT_MIN(KL) = MIN ( LAT_MIN(KL), ( (MJD_EPOCH - MJD_UPDATE) + &
     &                                                           (TIM_EPOCH - TIM_UPDATE)/86400.0D0 ) )
                              LAT_MAX(KL) = MAX ( LAT_MAX(KL), ( (MJD_EPOCH - MJD_UPDATE) + &
     &                                                           (TIM_EPOCH - TIM_UPDATE)/86400.0D0 ) )
                              LAT_AVR(KL) = ( LAT_AVR(KL)*(NE(IN)-1) + &
     &                                       (MJD_EPOCH - MJD_UPDATE) + &
     &                                       (TIM_EPOCH - TIM_UPDATE)/86400.0D0 )/NE(IN)
                         END IF
                    END IF
                    IF ( MODE .EQ. 1 .AND. DABS(TIM_ARR(KL) - TIM_ARR(1))*86400.0D0 > LAT_SHORT ) GOTO 830
                    IF ( MODE .EQ. 2 .AND. DABS(TIM_ARR(KL) - TIM_ARR(1))*86400.0D0 > LAT_LONG  ) GOTO 830
 430             CONTINUE 
 830             CONTINUE 
                 IF ( KL < 1 ) GOTO 420
                 IF ( UNITS == 'hours' .OR. UNITS == 'Hours' ) THEN
!
! ------------------- Units are hours
!
                      LAT_MIN = 24*LAT_MIN
                      LAT_MAX = 24*LAT_MAX
                      LAT_AVR = 24*LAT_AVR
                 END IF
!
! -------------- Generate the title
!
                 TITLE  = 'Latency of '//BUFC(J1)(IND(1,6):IND(2,LIND))
!
! -------------- Generate the output file name
!
                 CALL INCH ( J2, STR )
                 FILOUT = DIR_PLOT(1:I_LEN(DIR_PLOT))//'/latency_'// &
     &                    BUFC(J1)(IND(1,2):IND(2,2))// &
     &                    '_'//STR(1:1)//'.gif'
!
! -------------- Generate the plot of latencies in mode MODE
!
                 IUER  = -1
                 CALL PLOT_LATENCY ( MODE, UNITS, MJD_LAST, TIM_LAST, TIM_STEP, &
     &                               KL, TIM_ARR, LAT_MIN, LAT_MAX, LAT_AVR, &
     &                               TITLE, FILOUT, IUER )
                 IF ( IUER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J1, STR )
                      WRITE ( 6, * ) 'J2= ', J2
                      CALL ERR_LOG ( 5605, IUER, 'MALO_LATENCY_PLOT', 'Error in '// &
     &                    'making plot for consituent '//STR )
                      CALL EXIT ( 1 )
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
!
      END  SUBROUTINE  MALO_LATENCY_PLOT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PLOT_LATENCY ( MODE, UNITS, MJD_LAST, TIM_LAST, TIM_STEP, &
     &                          MP, TIM_ARR, LAT_MIN, LAT_MAX, LAT_AVR, TITLE, &
     &                          FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PLOT_LATENCY  generates the plot of MALO service          *
! *   latenceis.                                                         *
!
! ----------------- Extract the date of results availability
!
! *                                                                      *
! *  ### 25-JUL-2014  PLOT_LATENCY v2.1 (c)  L. Petrov  14-AUG-2015 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'diagi.i'
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4  MODE, MP, MJD_LAST, IUER
      REAL*8     TIM_LAST, TIM_STEP, TIM_ARR(MP), LAT_AVR(MP), &
     &           LAT_MIN(MP), LAT_MAX(MP)
      CHARACTER  UNITS*(*), TITLE*(*), FILOUT*(*)
      CHARACTER  UFILOUT*128, STR*128, CH*1
      REAL*4     XCOEF_IMA, XCOEF_PAP, PAP_SIZE, WHITE_CLR, &
     &           XC, YC, XPR, YPR, XL, XR, YB, YT, RD, VAL_R4, XA(2), YA(2)
      REAL*4     XMIN, XMAX, YMIN, YMAX, XMIN_U, XMAX_U, YMIN_U, YMAX_U, &
     &           OVR, RAD, RAD_X, RAD_Y, DIF
      LOGICAL*1  FL_TERM 
      INTEGER*4  RED, GREEN
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, IER
      INTEGER*4, EXTERNAL :: PGBEG, ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      XCOEF_PAP = 0.75
      RAD = 0.003
      GREEN = 3
      RED   = 4
      OVR   = 0.02
!
! --- Open plotting device and set up coordinate system.
!
      IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
           UFILOUT = FILOUT(1:I_LEN(FILOUT))//'/GIF'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
           PAP_SIZE = 270.0
           FL_TERM = .FALSE.
           CALL PGCOL_RGB ( BCG_CLRI, 255, 255, 255 )
         ELSE IF ( INDEX ( FILOUT, '.ps' ) > 0 ) THEN
           UFILOUT = UFILOUT(1:I_LEN(UFILOUT))//'/VCPS'
           IF ( PGBEG ( 0, UFILOUT, 1, 1 ) .NE. 1 ) STOP
           PAP_SIZE = 400.0
           FL_TERM = .FALSE.
           CALL PGCOL_RGB ( BCG_CLRI, 255, 255, 255 )
         ELSE 
           IF ( PGBEG ( 0, '/XW', 1, 1 ) .NE. 1 ) STOP
           IF ( XCOEF_PAP < 1 ) THEN
                PAP_SIZE = 400.0 
              ELSE 
                PAP_SIZE = 300.0 
           END IF
           FL_TERM = .TRUE.
!
! -------- While color is a little bit dimmer for the interactive device
!
           CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
           WHITE_CLR = BCG_CLR(1)
      END IF
!
! --- Setting colours
!
      DIAGI_S%NCLR  = 3
      CALL DIAGI_CLS ( DIAGI_S, IER )
      WHITE_CLR = BCG_CLRI
!
! --- Setting black background
!
      CALL PGCOL_RGB ( FRG_CLRI, FRG_CLR(1), FRG_CLR(2), FRG_CLR(3) )
!
! --- Set up a color palette using NCOL indices from INIT_COL to INIT_COL+NCOL.
!
      CALL PGCOL_RGB ( GREEN,   0,  80,   0 )
      CALL PGCOL_RGB ( RED,   180,  38,  67 )
!
      CALL PGPAP  ( PAP_SIZE/25.4, XCOEF_PAP )
      XMIN = MINVAL ( TIM_ARR )
      XMAX = MAXVAL ( TIM_ARR )
      XMIN_U = XMIN - OVR*(XMAX - XMIN)
      XMAX_U = XMAX + OVR*(XMAX - XMIN)
      IF ( XMAX_U - XMIN_U < OVR ) THEN
           XMIN_U = XMIN_U - OVR
           XMAX_U = XMAX_U + OVR
      END IF
      XMIN = XMIN_U
      XMAX = XMAX_U
      IF ( MODE == 1 ) THEN
           YMIN = MINVAL ( LAT_AVR )
           YMAX = MAXVAL ( LAT_AVR )
         ELSE IF ( MODE == 2 ) THEN
           YMIN = MIN ( MINVAL ( LAT_AVR ), MINVAL ( LAT_MIN ) )
           YMAX = MAX ( MAXVAL ( LAT_AVR ), MAXVAL ( LAT_MAX ) )
      END IF
!!   write ( 6, * ) 'MX1 ymin= ', ymin, ' ymax= ', ymax ! %%%
!!   write ( 6, * ) 'MX1 lat_avr= ', sngl(lat_avr(1:mp)) ! %%%%
      IF ( YMIN > 0.0 ) THEN
           YMIN = 0.0
         ELSE 
           YMIN = (INT(YMIN/10.0) - 1)*10.0
      END IF
      YMAX = (INT(YMAX/10.0) + 1)*10.0
!!   ymax = 100.0 ! %%%%%%%%%%%%%
!!   write ( 6, * ) 'MX2 ymin= ', ymin, ' ymax= ', ymax ! %%%
!
! --- Set parameters of the plotting window
!
      CALL PGSWIN  ( XMIN, XMAX, YMIN, YMAX )
      CALL PGSVP   ( 0.07, 0.93, 0.07, 0.93  ) ! makes fields for labels
      CALL PGSCR   ( 0, 1.0, 1.0, 1.0 ) ! pure white background
!
! --- Plot the box around the image
!
      CALL PGSCI  ( 1 )
      CALL PGERAS() ! Erase the screen
      CALL PGBOX  ( 'bc', 1.0, 2, 'bctns', 10.0, 2 )
!
      RAD_X = RAD*(XMAX - XMIN)*XCOEF_PAP
      RAD_Y = RAD*(YMAX - YMIN)
      DO 410 J1=1,MP
         CALL PGSCI  ( ITAB_CLR(1,1) )
         CALL PGCIRC_PET ( 72, SNGL(TIM_ARR(J1)), SNGL(LAT_AVR(J1)), &
     &                     RAD_X, RAD_Y )
         IF ( MODE == 2 ) THEN
!
! ----------- Min latency
!
              CALL PGSCI      ( ITAB_CLR(2,1) )
              CALL PGCIRC_PET ( 72, SNGL(TIM_ARR(J1)), SNGL(LAT_MAX(J1)), &
     &                         RAD_X, RAD_Y )
!
! ----------- Max latency
!
              CALL PGSCI      ( ITAB_CLR(3,1) )
              CALL PGCIRC_PET ( 72, SNGL(TIM_ARR(J1)), SNGL(LAT_MIN(J1)), &
     &                          RAD_X, RAD_Y )
         END IF
410   CONTINUE 
      CALL PGSLW  ( 5 ) 
      CALL PGSCI  ( ITAB_CLR(1,1) )
      CALL PGLINE ( MP, SNGL(TIM_ARR), SNGL(LAT_AVR) )
!
      IF ( MODE == 2 ) THEN
!
! -------- Min latency
!
           CALL PGSCI  ( ITAB_CLR(2,1) )
           CALL PGLINE ( MP, SNGL(TIM_ARR), SNGL(LAT_MAX) )
!
! -------- Max latency
!
           CALL PGSCI  ( ITAB_CLR(3,1) )
           CALL PGLINE ( MP, SNGL(TIM_ARR), SNGL(LAT_MIN) )
      END IF
      CALL PGSLW  ( 1 ) 
!
! --- Print title
!
      CALL PGSCI  ( 1 )
      IF ( ILEN(TITLE) > 70 ) THEN
           CALL PGSCH  ( 0.9  )
         ELSE 
           CALL PGSCH  ( 1.44 )
      END IF
!
      IF ( FL_TERM ) THEN
           CALL PGSLW  ( 5 )
         ELSE 
           CALL PGSLW  ( 2 )
      END IF
      CALL PGSCF ( 2 )
      CALL PGMTXT ( 't', 0.5, 0.5, 0.5, TITLE(1:I_LEN(TITLE)) )
!
! --- Labels
!
      CALL PGSLW  ( 1 )
      CALL PGMTXT ( 'l', 1.5, 1.0, 1.0, 'Latency in '//UNITS(1:I_LEN(UNITS)) )
!
      CALL PGSCH  ( 0.8 )
!
! --- Generate time labels
!
      IF ( MODE == 1 ) THEN
           DO 420 J2=1,MP
              DIF = TIM_ARR(J2) + TIM_LAST/86400.0
              IF ( ABS(DIF - INT(DIF)) < 0.99*TIM_STEP/86400.0D0  ) THEN
                   IER = IUER
                   STR = MJDSEC_TO_DATE ( MJD_LAST + INT(DIF), 0.0D0, IER )
                   CALL PGMTXT ( 'b', 1.25, (MP-J2)/(MP-1.0), &
     &                           0.5, STR(1:10) )
                   XA(1) = TIM_ARR(J2)
                   YA(1) = YMIN
                   XA(2) = TIM_ARR(J2)
                   YA(2) = YMIN + 0.010*(YMAX-YMIN)
                   CALL PGLINE ( 2, XA, YA )
              END IF
              IF ( ABS(DIF - INT(DIF) + 0.5 ) < 0.99*TIM_STEP/86400.0D0 ) THEN
                   XA(1) = TIM_ARR(J2)
                   YA(1) = YMIN
                   XA(2) = TIM_ARR(J2)
                   YA(2) = YMIN + 0.005*(YMAX-YMIN)
                   CALL PGLINE ( 2, XA, YA )
              END IF
 420       CONTINUE 
         ELSE IF ( MODE == 2 ) THEN
           DO 430 J3=1,MP
              IF ( MOD(J3,10) == 1 ) THEN
                   STR = MJDSEC_TO_DATE ( MJD_LAST, TIM_ARR(J3)*86400.D0, IER )
                   IF ( MP > 1 ) THEN
                        CALL PGMTXT ( 'b', 1.25, (MP-J3)/(MP-1.0), &
     &                                0.5, STR(1:10) )
                      ELSE
                        CALL PGMTXT ( 'b', 1.25, 0.5, &
     &                                0.5, STR(1:10) )
                   END IF
                   XA(1) = TIM_ARR(J3)
                   YA(1) = YMIN
                   XA(2) = TIM_ARR(J3)
                   YA(2) = YMIN + 0.010*(YMAX-YMIN)
                 ELSE
                   CALL PGLINE ( 2, XA, YA )
                   XA(1) = TIM_ARR(J3)
                   YA(1) = YMIN
                   XA(2) = TIM_ARR(J3)
                   YA(2) = YMIN + 0.005*(YMAX-YMIN)
                   CALL PGLINE ( 2, XA, YA )
              END IF
 430       CONTINUE 
!
           CALL PGSCH  ( 1.1 )
           CALL PGSCI  ( ITAB_CLR(3,1) )
           CALL PGMTXT ( 'b', 2.0, 0.0, 0.0, 'Red - minimal latency' )
!
           CALL PGSCI  ( ITAB_CLR(1,1) )
           CALL PGMTXT ( 'b', 2.0, 0.33, 0.0, 'Green - average latency' )
!
           CALL PGSCI  ( ITAB_CLR(2,1) )
           CALL PGMTXT ( 'b', 2.0, 0.70, 0., 'Blue - maximal latency' )
      END IF
!
      IF ( FL_TERM ) THEN
           XC = XMIN + (XMAX - XMIN)/2.0D0
           YC = YMIN + (YMAX - YMIN)/2.0D0
!
! -------- Wait for a user to hit a key
!
           CALL PGCURS ( XC, YC, CH )
           CALL TRAN ( 11, CH, CH )
           CALL PGCLOQ()
         ELSE
           CALL PGCLOQ()  ! quit pgplot
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PLOT_LATENCY  !#!#
