      PROGRAM    SMOOTH_EOP
! ************************************************************************
! *                                                                      *
! *   Propgram  SMOOTH_EOP performs Gaussian smoothing of the input      *
! *   EOP file in NERS EOP series format.                                *
! *                                                                      *
! *  ### 11-OCT-2023  SMOOTH_EOP   v1.3 (c)  L. Petrov  07-NOV-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE      ( VTD__TYPE ) :: VTD
      INTEGER*4  MP
      PARAMETER  ( MP  = 128*1024 )
      CHARACTER  FIL_IN_EOP*128, FIL_OUT_EOP*128, EOP_TYPE*6
      CHARACTER  BUFI(MP)*256, OUT(MP)*256, STR_SIG*12, STR*128
      REAL*8     COEF(MP), WORK(MP), D8(MP), DSPL8, D2SPL8 
      REAL*8     SIG_DAYS, SIG, EOP(MP,3), TAI_SEC, E3, E3_DOT, E3_DT2
      INTEGER*4  J1, J2, J3, J4, J5, IVRB, NP, NO, MJD, IUER
      LOGICAL*1  FL_UZT, FL_PLOT
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23, GET_CDATE*19
!
      FL_PLOT = .FALSE.
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: smooth_eop input_eop_file smoothing_int_days polu/poluz output_eop_file [ivrb]' 
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_IN_EOP )
           CALL GETARG ( 2, STR_SIG    )
           IF ( INDEX ( STR_SIG, '.' ) < 1 ) THEN
                STR_SIG = TRIM(STR_SIG)//'.0'
           END IF
           READ ( UNIT=STR_SIG, FMT='(F10.5)' ) SIG_DAYS
           SIG = 86400.0D0*SIG_DAYS
           CALL GETARG ( 3, EOP_TYPE )
           IF ( EOP_TYPE == 'polu'  .OR. &
     &          EOP_TYPE == 'poluz'      ) THEN
                CONTINUE 
              ELSE
                IUER = -1
                CALL ERR_LOG ( 8401, IUER, 'Incorrect 3rd argument '//EOP_TYPE// &
     &                         'while popu or poluz were expected' )
                CALL EXIT ( 1 )
           END IF
           CALL GETARG ( 4, FIL_OUT_EOP )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5,   STR )
                CALL CHIN   ( STR, IVRB )
           END IF
      END IF
!
      IUER = -1
      CALL VTD_INIT ( VTD, IUER )
!
! --- VTD%UEOP will have t he contriubution of zonal tides removed
!
      VTD%CONF%EOP_TIME_SCALE = VTD__TAI
      VTD%CONF%UZT_USE   = UZT__INTERPOLATE
      VTD%CONF%UZT_MODEL = UZT__RE2014
!
      IUER = -1
      CALL VTD_UEOP_INIT ( VTD, FIL_IN_EOP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8402, IUER, 'Error in reading input '// &
     &                   'EOP file '//FIL_IN_EOP )
           CALL EXIT ( 1 )
      END IF

      IUER = -1
      CALL RD_TEXT ( FIL_IN_EOP, MP, BUFI, NP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8403, IUER, 'Error in reading input '// &
     &                   'EOP file '//FIL_IN_EOP )
           CALL EXIT ( 1 )
      END IF
!
      EOP = 0.0D0
      DO 410 J1=1,3
         CALL GAUSSIAN_FILTER ( SIG, VTD%UEOP%NP, VTD%UEOP%TIM, VTD%UEOP%VAL(1,J1), &
     &                          EOP(1,J1), 1.D10 )
 410  CONTINUE 
!
      CALL GETARG ( 0, STR )
      NO = 0
      NO = NO + 1 ; WRITE ( OUT(NO), '(A)' ) NERS__SER_FMT
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#'
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '# Output type: '//EOP_TYPE
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#'
      DO 420 J2=7,NP
         NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) TRIM(BUFI(J2))
         IF ( BUFI(J2)(1:15) == '# Command line:' ) THEN
              GOTO 820
         END IF
 420  CONTINUE 
 820  CONTINUE 
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#'
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '# Smoothed with the Gaussian fileter by smooth_eop'
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '# '//TRIM(STR)//' '//TRIM(FIL_IN_EOP)//' '// &
     &                                        TRIM(STR_SIG)//' '//TRIM(FIL_OUT_EOP)
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#'
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#    1:7    I7       Ind   ---  row index'
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#    9:20   F12.1    Tim   s    Time since 2000.01.01_00:00:00.0 in TAI time scale'
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#   22:44   A23      Dat   ---  TAI Calendar date in format YYYY.MM.DD-hh:mm:ss.sss'
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#   51:65   1PD15.8  XPol  rad  X pole coordinate'
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#   67:81   1PD15.8  YPol  rad  Y pole coordinate'
      NO = NO + 1 ;  
      IF ( EOP_TYPE == 'poluz' ) THEN
           WRITE ( OUT(NO), '(A)' ) '#   83:97   1PD15.8  UT1   s    UT1R minus TAI'
         ELSE
           WRITE ( OUT(NO), '(A)' ) '#   83:97   1PD15.8  UT1   s    UT1 minus TAI'
      END IF
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#'
      NO = NO + 1 ;  
      IF ( EOP_TYPE == 'poluz' ) THEN
           WRITE ( OUT(NO), '(A)' ) '#   Ind  TAI time    Calendar time                 X-pole (arcsec) Y-pole (arcsec) UT1R-TAI (sec)'
         ELSE
           WRITE ( OUT(NO), '(A)' ) '#   Ind  TAI time    Calendar time                 X-pole (arcsec) Y-pole (arcsec) UT1-TAI (sec)'
      END IF
      NO = NO + 1 ;  WRITE ( OUT(NO), '(A)' ) '#'
!
      DO 430 J3=1,VTD%UEOP%NP
         NO = NO + 1
         VTD%UEOP%TIM(J3) = VTD%UEOP%TIM(J3) + 43200.0D0
         MJD     = VTD%UEOP%TIM(J3)/86400.0D0 + J2000__MJD
         TAI_SEC = VTD%UEOP%TIM(J3) - (MJD - J2000__MJD)*86400.0D0
         CALL E3ZT_RE2014 ( 0, MJD, TAI_SEC, E3, E3_DOT, E3_DT2 )
         IF ( EOP_TYPE == 'polu' ) THEN
              EOP(J3,UEOP__UT1_M_TAI)  = EOP(J3,UEOP__UT1_M_TAI) + E3/UT1__TO__E3
         END IF
         IF ( IVRB .GE. 2 ) THEN
              VTD%UEOP%VAL(J3,1) = RAD__TO__ARCSEC*VTD%UEOP%VAL(J3,1) 
              VTD%UEOP%VAL(J3,2) = RAD__TO__ARCSEC*VTD%UEOP%VAL(J3,2) 
              VTD%UEOP%VAL(J3,3) = VTD%UEOP%VAL(J3,3)
              IF ( EOP_TYPE == 'polu' ) THEN
                   VTD%UEOP%VAL(J3,3) = VTD%UEOP%VAL(J3,3) + E3/UT1__TO__E3
             END IF
         END IF
         EOP(J3,UEOP__XPL) = RAD__TO__ARCSEC*EOP(J3,UEOP__XPL)
         EOP(J3,UEOP__YPL) = RAD__TO__ARCSEC*EOP(J3,UEOP__YPL)
         WRITE ( OUT(NO), 110 ) J3, VTD%UEOP%TIM(J3), &
     &                          TIM_TO_DATE ( VTD%UEOP%TIM(J3), IUER ), EOP(J3,1:3)
 110     FORMAT ( I7, 1X, F12.1, 1X, A, ' EOP= ', 3(1PD15.8,1X) )
 430  CONTINUE 
!
      IF ( IVRB .GE. 2 ) THEN
           IUER = -1
           CALL MAKE_SPLINE ( 1, VTD%UEOP%NP, VTD%UEOP%TIM, VTD%UEOP%VAL(1,3), &
     &                        0.0D0, 0.0D0, COEF, WORK, IUER )
!           CALL MAKE_SPLINE ( 1, VTD%UEOP%NP, VTD%UEOP%TIM, EOP(1,3), &
!     &                        0.0D0, 0.0D0, COEF, WORK, IUER )
           DO 440 J4=1,VTD%UEOP%NP
              IF ( J4 == 1 ) THEN
                   D8(J4) = D2SPL8 ( VTD%UEOP%TIM(J4)+0.1, VTD%UEOP%NP, VTD%UEOP%TIM, VTD%UEOP%VAL(1,3), J4, COEF )
!!                   D8(J4) = D2SPL8 ( VTD%UEOP%TIM(J4)+0.1, VTD%UEOP%NP, VTD%UEOP%TIM, EOP(1,3), J4, COEF )
                ELSE IF ( J4 == NP ) THEN
                   D8(J4) = D2SPL8 ( VTD%UEOP%TIM(J4)-0.1, VTD%UEOP%NP, VTD%UEOP%TIM, VTD%UEOP%VAL(1,3), VTD%UEOP%NP-1, COEF )
!!                   D8(J4) = D2SPL8 ( VTD%UEOP%TIM(J4)-0.1, VTD%UEOP%NP, VTD%UEOP%TIM, EOP(1,3), VTD%UEOP%NP-1, COEF )
                ELSE
                   D8(J4) = D2SPL8 ( VTD%UEOP%TIM(J4), VTD%UEOP%NP, VTD%UEOP%TIM, VTD%UEOP%VAL(1,3), J4, COEF )
!!                   D8(J4) = D2SPL8 ( VTD%UEOP%TIM(J4), VTD%UEOP%NP, VTD%UEOP%TIM, EOP(1,3), J4, COEF )
              END IF
              D8(J4) = D8(J4)*SEC__TO__RAD
!!           write ( 6, * )'j4= ', j4, ' d8 = ', d8(j4) ! %%%%
 440       CONTINUE 
           CALL DIAGI_1 ( VTD%UEOP%NP-1, VTD%UEOP%TIM, D8, IUER )
!
           DO 450 J5=1,3
              CALL DIAGI_2 ( VTD%UEOP%NP, VTD%UEOP%TIM, VTD%UEOP%VAL(1,J5), &
     &                       VTD%UEOP%NP, VTD%UEOP%TIM, EOP(1,J5), IUER )
 450       CONTINUE 
      END IF
!
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FIL_OUT_EOP, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 8403, IUER, 'Error in writing into the output '// &
     &                   'EOP file '//FIL_OUT_EOP )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM    SMOOTH_EOP  !#!#
