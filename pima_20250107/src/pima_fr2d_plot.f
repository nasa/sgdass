      SUBROUTINE PIMA_FR2D_PLOT ( FINAM_PLOT, PIM, IND_OBS, &
     &                            WG_L, WG_H, WR_L, WR_H, &
     &                            DRF_AMPL, AP_LEN, &
     &                            DEL_INIT, DEL_WIN, RAT_INIT, RAT_WIN, &
     &                            AMPL, SNR, GR_DEL, PH_RAT, IDEV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FR2D_PLOT
! *                                                                      *
! * ### 13-JAN-2006  PIMA_FR2D_PLOT  v1.4 (c)  L. Petrov  03-MAY-2020 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      CHARACTER  FINAM_PLOT*(*)
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, WG_L, WG_H, WR_L, WR_H, IDEV, IUER
      REAL*4     DRF_AMPL(WG_L:WG_H,WR_L:WR_H), XC, YC
      REAL*8     DEL_INIT, DEL_WIN, RAT_INIT, RAT_WIN
      REAL*8     AP_LEN, AMPL, SNR, GR_DEL, PH_RAT
      INTEGER*4  KP, J1, J2, J3, NO, ID_PGPLOT, FRG_IND, IER
      INTEGER*4  PLO_CLRI 
      PARAMETER  (  PLO_CLRI = 101 )
      CHARACTER  STR*128, OUT1*128, OUT2*128, OUT3*128, OUT4*128, &
     &           OUT5*128, OUT6*128, OUT7*128, OUT8*128, FINAM*128, &
     &           NOW*128, FINAM_TXT_PLOT*128
      CHARACTER*128, ALLOCATABLE :: OUT(:)
      LOGICAL*4  FL_SMALL
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, PGOPEN 
      REAL*4,    EXTERNAL :: R4_ARRAY_MIN, R4_ARRAY_MAX 
!
#ifdef NO_PLOT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FR2D_PLOT  !#!  
#else
      FL_SMALL = .FALSE.
!
      CALL CLRCH ( OUT1 )
      CALL CLRCH ( OUT2 )
      OUT1 = 'St1: '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%IVS_NAME//' '// &
     &       'St2: '//PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%IVS_NAME//' '// &
     &       'Sou: '//PIM%SOU(PIM%OBS(IND_OBS)%SOU_IND)%IVS_NAME//' '// &
     &       'Exp: '//PIM%OBS_CODE
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      STR(1:30)  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                              PIM%TIM_R8(PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND), -3 )
      WRITE ( UNIT=OUT2, FMT=110, IOSTAT=IER ) &
     &        IND_OBS, PIM%SCA(PIM%OBS(IND_OBS)%SCA_IND)%SCAN_NAME(1:10), &
     &        STR(1:23), PIM%OBS(IND_OBS)%NUM_EPC(PIM%CONF%FRQ_GRP)*AP_LEN
 110  FORMAT ( 'Obs: ',I6,' Scan: ', A, ' Start: ',A, &
     &         ' Duration: ', F8.3, ' sec' )
!
      WRITE ( UNIT=OUT3, FMT=120 ) AMPL, SNR
 120  FORMAT ( 'Ampl: ',F9.7,' SNR: ', F8.2 )
!
      WRITE ( UNIT=OUT4, FMT=130 ) GR_DEL, PH_RAT
 130  FORMAT ( 'Gr_del: ',1PD13.6,' sec   Ph_rat: ', 1PD13.6, ' s/s' )
!
      CALL CLRCH ( OUT5 )
      CALL CLRCH ( OUT6 )
      CALL CLRCH ( OUT7 )
      CALL CLRCH ( OUT8 )
      OUT5   = 'Plotting window:'
      WRITE  ( UNIT=OUT6, FMT=140 ) DEL_INIT - DEL_WIN, DEL_INIT + DEL_WIN
 140  FORMAT ( 'Delay window: [ ', 1PD12.5, ', ', 1PD12.5, ']   s' )
      WRITE  ( UNIT=OUT7, FMT=150 ) RAT_INIT - RAT_WIN, RAT_INIT + RAT_WIN
 150  FORMAT ( 'Rate window:  [ ', 1PD12.5, ', ', 1PD12.5, '] s/s' )
      WRITE  ( UNIT=OUT8, FMT=160 ) DEL_WIN/WG_H, RAT_WIN/WR_H
 160  FORMAT ( 'Step: ',1PD12.5, ' s             ', 1PD12.5, ' s/s' )
!
      IF ( IDEV == 1 ) THEN
!!OLD           CALL PGBEG ( 0, '/XW', 1, 1 ) 
           ID_PGPLOT = PGOPEN ( '/XW' )
           CALL PGPAP   ( 350.0/25.4, 0.75 )
           CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
         ELSE IF ( IDEV == 2 ) THEN
           FINAM = FINAM_PLOT(1:I_LEN(FINAM_PLOT))//GIF_DIAGI//'/GIF'
!!OLD           CALL PGBEG ( 0, FINAM, 1, 1 ) 
           ID_PGPLOT = PGOPEN ( FINAM )
           CALL PGPAP ( 350.0/25.4, 0.75 )
           CALL PGCOL_RGB ( BCG_CLRI, 256, 256, 256 )
         ELSE IF ( IDEV == 3 ) THEN
           FINAM = FINAM_PLOT(1:I_LEN(FINAM_PLOT))//PS_DIAGI//'/CPS'
!!OLD           CALL PGBEG ( 0, FINAM, 1, 1 ) 
           ID_PGPLOT = PGOPEN ( FINAM )
           IF ( FL_SMALL ) THEN
                CALL PGPAP ( 90.0/25.4, 0.75 )
              ELSE 
                CALL PGPAP ( 270.0/25.4, 0.75 )
           END IF
           CALL PGCOL_RGB ( PLO_CLRI, 1, 1, 1 )
      END IF
      CALL PGUPDT()
      NOW = GET_CDATE()
      IF ( IDEV == -1 ) THEN
!
! -------- Plot is written as a text file. In fact, we write two text
!
           FINAM_TXT_PLOT = FINAM_PLOT(1:I_LEN(FINAM_PLOT))//'.txt'
           KP = (WG_H+WG_L-1)*(WR_H+WR_L-1)
!
! -------- Allocate memory for temporary buffer
!
           ALLOCATE   ( OUT(KP+32), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7571, IUER, 'PIMA_FR2D_PLOT', 'Failure '// &
     &                        'to allocate memory for internal buffer '// &
     &                        'of the output text file' )
                DEALLOCATE ( OUT )
                RETURN 
           END IF
!
! -------- Prepare table preamble
!
           OUT(1) = PIMA__TXT2D_LABEL
           OUT(2) = '#'
           OUT(2) = '# Fringe amplitude as a function of group delay and phase delay rate'
           OUT(2) = '#'
           OUT(3) = '# Generated on '//NOW
           OUT(4) = '# Generated by '//PIMA__LABEL
           OUT(5) = '#'
!
! -------- Write into the table auxiliary information
!
           NO = 5
           NO = NO + 1; OUT(NO) = 'PLOT_TITLE:    Fringe amplitude at '//OUT1
           NO = NO + 1; OUT(NO) = 'SUBTITLE:      Mseg: '//OUT2
           CALL INCH ( WG_H+WG_L-1, STR )
           NO = NO + 1; OUT(NO) = 'AXIS1_NUM_POI: '//STR
           NO = NO + 1; OUT(NO) = 'AXIS1_TITLE:   Group delay'
           NO = NO + 1; OUT(NO) = 'AXIS1_UNITS:   s'
!
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) DEL_INIT - DEL_WIN
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS1_MIN:     '//STR(1:15)
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) DEL_INIT + DEL_WIN
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS1_MAX:     '//STR(1:15)
!
           CALL INCH ( WR_H+WR_L-1, STR )
           NO = NO + 1; OUT(NO) = 'AXIS2_NUM_POI: '//STR
           NO = NO + 1; OUT(NO) = 'AXIS2_TITLE:   Delay rate'
           NO = NO + 1; OUT(NO) = 'AXIS2_UNITS:   dimensionlesss'
!
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) RAT_INIT - RAT_WIN
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS2_MIN:     '//STR(1:15)
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) RAT_INIT + RAT_WIN
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS2_MAX:     '//STR(1:15)
!
           CALL INCH ( (WG_H+WG_L-1)*(WR_H+WR_L-1), STR )
           NO = NO + 1; OUT(NO) = 'AXIS3_NUM_POI: '//STR
           NO = NO + 1; OUT(NO) = 'AXIS3_TITLE:   Fringe amplitude'
           NO = NO + 1; OUT(NO) = 'AXIS3_UNITS:   dimensionlesss'

           WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) R4_ARRAY_MIN ( KP, DRF_AMPL )
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS3_MIN:     '//STR(1:15)
           WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) R4_ARRAY_MAX ( KP, DRF_AMPL )
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS3_MAX:     '//STR(1:15)
           DO 410 J1=WR_L,WR_H
              DO 420 J2=WG_L,WG_H  
                 NO = NO + 1
                 WRITE  ( OUT(NO), 170 ) J2, J1, &
     &                                   DEL_INIT - DEL_WIN + (J2-1)*2*DEL_WIN/WG_H, &
     &                                   RAT_INIT - RAT_WIN + (J1-1)*2*RAT_WIN/WR_H, &
     &                                   DRF_AMPL(J2,J1)
 170             FORMAT ( 'POINT: ', I5, 1X, I5, 1X, 1PD15.7, 1X, 1PD15.7, 1X, 0PF10.7 )
 420          CONTINUE 
 410       CONTINUE 
           NO = NO + 1; OUT(NO) = '#'
           NO = NO + 1; OUT(NO) = PIMA__TXT2D_LABEL
!
! -------- Write the buffer into file
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT  ( NO, OUT, FINAM_TXT_PLOT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7572, IUER, 'PIMA_FR2D_PLOT', 'Failure '// &
     &                        'to write into the output table file '// &
     &                         FINAM_PLOT )
                DEALLOCATE ( OUT )
                RETURN 
           END IF
           WRITE ( 6, * ) 'File written '//FINAM_TXT_PLOT(1:I_LEN(FINAM_TXT_PLOT))
!
! -------- Finally, do not forget to deallocate the buffer
!
           DEALLOCATE ( OUT )
         ELSE IF ( IDEV > 0 ) THEN
           IF ( SNR .LT. 5.0 ) THEN
                CALL PGCOL_RGB ( PLO_CLRI, 180,  38,  67 )
              ELSE IF ( SNR .GE. 5.0  .AND.  SNR .LT. 6.0 ) THEN
                CALL PGCOL_RGB ( PLO_CLRI, 180,  151,  38 )
              ELSE IF ( SNR .GE. 6.0  .AND.  SNR .LT. 7.5 ) THEN
                CALL PGCOL_RGB ( PLO_CLRI, 38,  67, 180 )
              ELSE 
                CALL PGCOL_RGB ( PLO_CLRI, 67, 180,  38 )
           END IF
!
           CALL PGCOL_RGB ( FRG_CLRI, FRG_CLR(1), FRG_CLR(2), FRG_CLR(3) )
           CALL PGERAS() ! Erase the screen
           CALL PGSVP   (  0.01, 0.99, 0.13, 0.99 )
           CALL PGSCI   ( PLO_CLRI )
!
           CALL FREDDY  ( DRF_AMPL, WG_H-WG_L+1, WR_H-WR_L+1, 1.0, 30.0, &
     &                    'Rate', 'Delay', OUT1, OUT2, OUT3, OUT4, &
     &                    OUT5, OUT6, OUT7, OUT8, NOW, FL_SMALL, PIMA__LABEL )
           IF ( IDEV == 1 ) THEN
                XC = 1.0
                YC = 1.0
                CALL PGBAND ( 0, 1, XC, YC, XC, YC, STR )
           END IF
           CALL PGCLOQ()
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FR2D_PLOT  !#!#
!
! ------------------------------------------------------------------------
!
     SUBROUTINE FREDDY ( ARRAY, KX, NY, SIZE, ANGLE, STRX, STRY, &
     &                   STR1, STR2, STR3, STR4, STR5, STR6, STR7, STR8, &
     &                   NOW, FL_SMALL, LABEL )
      INTEGER KX, NY
      CHARACTER  STRX*(*), STRY*(*), STR1*(*), STR2*(*), STR3*(*), STR4*(*), &
     &           STR5*(*), STR6*(*), STR7*(*), STR8*(*), NOW*(*), LABEL*(*)
      REAL ARRAY(KX,NY), SIZE, ANGLE
      LOGICAL*4  FL_SMALL
!
! Draws isometric plot of array
!
      REAL FMAX,FMIN,DELTAX,DELTAY,DELTAV,SINE,PEAK,X,DX,HEIGHT
      INTEGER*4  ICLR_SAVE
      REAL*4     ANG
      INTEGER I,J,KI,KJ,NX,MX,MY,STEP,LEFT,RIGHT,IT,MN,INCX
      LOGICAL VISBLE
      COMMON /FREDCM/ DELTAX,X,STEP,LEFT,RIGHT,IT,NX,VISBLE
      INTEGER*4, EXTERNAL :: ILEN,I_LEN
!
      MN = KX*NY
      NX = KX
!     Check array size:
      IF(NX.LT.2 .OR. NY.LT.2) RETURN
      FMAX = ARRAY(1,1)
      FMIN = FMAX
      DO 20 J=1,NY
          DO 10 I=1,NX
              FMIN = AMIN1(ARRAY(I,J),FMIN)
              FMAX = AMAX1(ARRAY(I,J),FMAX)
   10     CONTINUE
   20 CONTINUE
      DELTAX = SIZE/(NX+NY)
      SINE = SIN(ANGLE/57.2925)
      DELTAY = DELTAX*SINE
      HEIGHT = SIZE*(1.-ABS(SINE))
      DELTAV = HEIGHT
      FMAX = FMAX-FMIN
      IF ( FMAX .LT. 1.e-10 ) FMAX = DELTAV
      DELTAV = DELTAV/FMAX*1.33
      MX = NX+1
      MY = NY+1
      STEP = MX
!
! Start PGPLOT buffering.
!
      CALL PGBBUF()
!
! Work our way down the Y axis, then up the X axis,
! calculating the Y plotter coordinates for each
! column of the plot, doing the hidden-line suppression
! at the same time.
!
      DO 50 J=1,NY
          KJ = MY-J
          KI = 1
!               ( KI,KJ are coordinates of bottom of column)
          ARRAY(KI,KJ) = DELTAY*(KI+KJ) + DELTAV*(ARRAY(KI,KJ)-FMIN)
   30     PEAK = ARRAY(KI,KJ)
   40     KI = KI+1
          KJ = KJ+1
          IF(KI.GT.NX .OR. KJ.GT.NY) GOTO 50
          ARRAY(KI,KJ) = DELTAY*(KI+KJ) + DELTAV*(ARRAY(KI,KJ)-FMIN)
          IF(ARRAY(KI,KJ).GT.PEAK) GOTO 30
          IF(ARRAY(KI,KJ).LE.PEAK) ARRAY(KI,KJ) = -ABS(ARRAY(KI,KJ))
          GOTO 40
   50 CONTINUE
!
! Now to work our way up the X axis
!
      DO 80 I=2,NX
          KI = I
          KJ = 1
          ARRAY(KI,KJ) = DELTAY*(KI+KJ)+DELTAV*(ARRAY(KI,KJ)-FMIN)
   60     PEAK = ARRAY(KI,KJ)
   70     KI = KI+1
          KJ = KJ+1
          IF(KI.GT.NX .OR. KJ.GT.NY) GOTO 80
          ARRAY(KI,KJ) = DELTAY*(KI+KJ)+DELTAV*(ARRAY(KI,KJ)-FMIN)
          IF(ARRAY(KI,KJ).GT.PEAK) GOTO 60
          IF(ARRAY(KI,KJ).LE.PEAK) ARRAY(KI,KJ) = -ABS(ARRAY(KI,KJ))
          GOTO 70
   80 CONTINUE
!
! Draw a line along the bottom of the vertical faces
!
      CALL PGMOVE(DELTAX*(NX+NY-2), DELTAY*(MX))
      CALL PGDRAW(DELTAX*(NY-1),    DELTAY*2)
      CALL PGDRAW(0.0,              DELTAY*MY)
!
! Array is now ready for plotting.  If a point is
! positive, then it is to be plotted at that Y
! coordinate; if it is negative, then it is
! invisible, but at minus that Y coordinate (the point
! where the line heading towards it disappears has to
! be determined by finding the intersection of it and
! the cresting line).
!
! Plot rows:
!
      DO 110 J=1,NY,2
          KJ = MY-J
          DX = DELTAX*(J-2)
          X = DX+DELTAX
          CALL PGMOVE(X,DELTAY*(KJ+1))
          CALL PGDRAW(X,ARRAY(1,KJ))
          VISBLE = .TRUE.
          DO 90 I=2,NX
              RIGHT = I+NX*(KJ-1)
              LEFT = RIGHT-1
              IT = RIGHT
              X = DX+DELTAX*I
              CALL FREDGO(ARRAY,MN)
   90     CONTINUE
!
! Now at far end of row so come back
!
          KJ = KJ-1
          IF(KJ.LE.0) GOTO 170
          VISBLE = ARRAY(NX,KJ).GE.0.0
          DX = DELTAX*(NX+J)
          IF(VISBLE) CALL PGMOVE(DX-DELTAX,ARRAY(NX,KJ))
          DELTAX = -DELTAX
          DO 100 I=2,NX
              KI = MX-I
              LEFT = KI+NX*(KJ-1)
              RIGHT = LEFT+1
              IT = LEFT
              X = DX+DELTAX*I
              CALL FREDGO(ARRAY,MN)
  100     CONTINUE
!
          X = DX+DELTAX*NX
          IF(.NOT.VISBLE) CALL PGMOVE(X,ARRAY(1,KJ))
          CALL PGDRAW(X,DELTAY*(KJ+1))
!               (set DELTAX positive for return trip)
          DELTAX = -DELTAX
  110 CONTINUE
!
! Now do the columns:
! as we fell out of the last DO-loop we do the
! columns in ascending-X order
!
      INCX = 1
      KI = 1
!               (set DELTAX -ve since scanning R to L)
  120 DX = DELTAX*(KI+NY-1)
      DELTAX = -DELTAX
      X = DX+DELTAX
      CALL PGMOVE(X,ARRAY(1,1))
  130 VISBLE = .TRUE.
      DO 140 J=2,NY
          LEFT = KI+NX*(J-1)
          RIGHT = LEFT-NX
          IT = LEFT
          X = DX+DELTAX*J
          CALL FREDGO(ARRAY,MN)
  140 CONTINUE
!
! At far end, increment X and check still inside array
!
      KI = KI+INCX
      IF(KI.LE.0 .OR. KI.GT.NX) GOTO 180
      VISBLE = ARRAY(KI,NY).GE.0.0
      DELTAX = -DELTAX
      DX = DELTAX*(KI-2)
      X = DX+DELTAX
      IF(VISBLE) CALL PGMOVE(X,ARRAY(KI,NY))
      DO 150 J=2,NY
          KJ = MY-J
          RIGHT = KI+NX*(KJ-1)
          LEFT = RIGHT+NX
          IT = RIGHT
          X = DX+DELTAX*J
          CALL FREDGO(ARRAY,MN)
  150 CONTINUE
!
      X = DX+DELTAX*NY
      IF(.NOT.VISBLE) CALL PGMOVE(X,ARRAY(KI,1))
      IF(KI.EQ.1) GOTO 180
      CALL PGDRAW(X,DELTAY*(KI+1))
      KI = KI+INCX
      IF(KI.GT.NX) GOTO 180
      IF(KI.EQ.1) GOTO 120
  160 CONTINUE 
      DELTAX = -DELTAX
      DX = DELTAX*(1-KI-NY)
      X = DX+DELTAX
      CALL PGMOVE(X,DELTAY*(KI+1))
      CALL PGDRAW(X,ARRAY(KI,1))
      GOTO 130
!
! Do columns backwards because ended rows at far end of X
!
  170 KI = NX
      INCX = -1
      DX = DELTAX*(KI+NY)
      GOTO 160
!
  180 CONTINUE 
!
      CALL PGQCI ( ICLR_SAVE )
      CALL PGSCI ( 1   )
      CALL PGSCF ( 2 )
      IF ( FL_SMALL ) THEN
           CALL PGSLW ( 1   )
           CALL PGSCH ( 1.0 )
         ELSE 
           CALL PGSLW ( 4   )
           CALL PGSCH ( 1.0 )
      END IF
!
      ANG = -ATAN2(  ABS(DELTAY*(MX-2)), ABS(DELTAX*(NX+1)) )*57.23*0.65
      CALL PGPTXT( (NY-1)*SIZE/(NX+NY)/2-0.015, ABS(DELTAY)*(MY-2)/2-0.015, &
     &             ANG, 0.5, STRX )
!
      ANG =  ATAN2(  DELTAY*(MX-2)/2, (NX+2*NY-3)*SIZE/(NX+NY)/2 )*57.23*1.1
      CALL PGPTXT( (NX+2*NY-3)*SIZE/(NX+NY)/2-0.05, &
     &              DELTAY*(MX+2)/2-0.05, ANG, 0.0, STRY )
!
!      CALL PGPTXT( SIZE/(NX+NY)*(NX+NY-2), DELTAY*MX, ANGLE, 0.0, 'A' )
!      CALL PGPTXT( SIZE/(NX+NY)*(NY-1),    DELTAY*2,  ANGLE, 0.0, 'B' )
!      CALL PGPTXT( 0.0,                    DELTAY*MY, ANGLE, 0.0, 'C' )
!
      CALL PGSCF ( 2 )
      IF ( FL_SMALL ) THEN
           CALL PGSLW ( 1 )
           CALL PGSCH ( 0.83 )
         ELSE 
           CALL PGSLW ( 3 )
           CALL PGSCH ( 0.8 )
      END IF
      CALL PGPTXT( 0.0, -0.02, 0.0, 0.0, STR1(1:I_LEN(STR1)) )
      CALL PGPTXT( 0.0, -0.05, 0.0, 0.0, STR2(1:I_LEN(STR2)) )
      CALL PGPTXT( 0.0, -0.08, 0.0, 0.0, STR3(1:I_LEN(STR3)) )
      CALL PGPTXT( 0.0, -0.11, 0.0, 0.0, STR4(1:I_LEN(STR4)) )
!
      CALL PGPTXT( 0.57,  0.07, 0.0, 0.0, STR5(1:I_LEN(STR5)) )
      CALL PGPTXT( 0.57,  0.04, 0.0, 0.0, STR6(1:I_LEN(STR6)) )
      CALL PGPTXT( 0.57,  0.01, 0.0, 0.0, STR7(1:I_LEN(STR7)) )
      CALL PGPTXT( 0.57, -0.02, 0.0, 0.0, STR8(1:I_LEN(STR8)) )
!
      CALL PGPTXT( 0.99, -0.08, 0.0, 1.0, 'Processed on: '// &
     &                                     NOW(1:I_LEN(NOW)) )
      CALL PGSCF ( 3 )
      CALL PGPTXT( 0.99, -0.11, 0.0, 1.0, LABEL(1:I_LEN(LABEL)) )
!
      CALL PGSLW ( 1   )
      CALL PGSCI ( ICLR_SAVE )
!
      CALL PGEBUF
      END
!
!-----------------------------------------------------------------------
!
      SUBROUTINE FREDGO(ARRAY,MN)
      INTEGER MN
      REAL ARRAY(MN)
!
      INTEGER STEP,LEFT,RIGHT,IT,NX
      LOGICAL VISBLE
      REAL AL,AR,BL,EM,XX,X,Y,DELTAX
      COMMON /FREDCM/ DELTAX,X,STEP,LEFT,RIGHT,IT,NX,VISBLE
!
! Test visibility
!
      IF(ARRAY(IT).LT.0.0) GOTO 80
!
! This point is visible - was last?
!
      IF(VISBLE) GOTO 50
!
! No: calculate point where this line vanishes
!
   10 IF(LEFT.LE.NX .OR. MOD(LEFT-1,NX).EQ.0 .OR. &
     &     RIGHT.LE.NX .OR. MOD(RIGHT-1,NX).EQ.0) GOTO 100
      AL = ABS(ARRAY(LEFT))
      AR = ABS(ARRAY(RIGHT))
      IF(ARRAY(LEFT).LT.0.0) GOTO 70
!               Right-hand point is crested
   20 RIGHT = RIGHT-STEP
      IF(ARRAY(RIGHT).LT.0.0) GOTO 20
!               Left-hand end of cresting line is either
!               RIGHT+NX or RIGHT-1
      LEFT = RIGHT+NX
      IF(ARRAY(LEFT).LT.0.0) LEFT = RIGHT-1
!
!               RIGHT and LEFT index into the endpoints of the
!               cresting line
   30 BL = ABS(ARRAY(LEFT))
      EM = ABS(ARRAY(RIGHT))-BL
      XX = EM-AR+AL
      IF(ABS(XX).LT.0.0001) GOTO 60
      XX = (AL-BL)/XX
   40 Y = EM*XX+BL
      IF(DELTAX.GT.0.0) XX = 1.0-XX
      XX = X-XX*DELTAX
      IF(VISBLE) GOTO 90
!               Drawing a line from an invisible point
!               to a visible one
      CALL PGMOVE(XX,Y)
      VISBLE = .TRUE.
   50 CALL PGDRAW(X,ARRAY(IT))
      RETURN
!
   60 XX = 0.5
      GOTO 40
!
! Left-hand point crested
!
   70 LEFT = LEFT-STEP
      IF(ARRAY(LEFT).LT.0.0) GOTO 70
!
! Right-hand end of cresting line is either LEFT+1 or LEFT-NX
!
      RIGHT = LEFT+1
      IF(ARRAY(RIGHT).LT.0.0) RIGHT = LEFT-NX
      GOTO 30
!
! This point is invisible; if last one was too, then forget it;
! else draw a line towards it
!
   80 IF(.NOT.VISBLE) RETURN
      GOTO 10
!
   90 CALL PGDRAW(XX,Y)
  100 VISBLE = .FALSE.
      RETURN
      END
#endif
!
! ------------------------------------------------------------------------
!
      FUNCTION   R4_ARRAY_MIN ( N, ARR )
! ************************************************************************
! *                                                                      *
! *   Aucilliary routine R4_ARRAY_MIN 
! *                                                                      *
! *  ### 31-DEC-2012  R4_ARRAY_MIN  v1.0 (c) L. Petrov  31-DEC-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*4     R4_ARRAY_MIN, ARR(N)
      INTEGER*4  J1
!
      R4_ARRAY_MIN = ARR(1)
      IF ( N > 1 ) THEN
           DO 410 J1=2,N
              R4_ARRAY_MIN = MIN ( R4_ARRAY_MIN, ARR(J1) )
 410       CONTINUE 
      END IF
      RETURN
      END  FUNCTION   R4_ARRAY_MIN  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   R4_ARRAY_MAX ( N, ARR )
! ************************************************************************
! *                                                                      *
! *   Aucilliary routine R4_ARRAY_MAX
! *                                                                      *
! *  ### 31-DEC-2012  R4_ARRAY_MAX  v1.0 (c) L. Petrov  31-DEC-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*4     R4_ARRAY_MAX, ARR(N)
      INTEGER*4  J1
!
      R4_ARRAY_MAX = ARR(1)
      IF ( N > 1 ) THEN
           DO 410 J1=2,N
              R4_ARRAY_MAX = MAX ( R4_ARRAY_MAX, ARR(J1) )
 410       CONTINUE 
      END IF
      RETURN
      END  FUNCTION   R4_ARRAY_MAX   !#!#
