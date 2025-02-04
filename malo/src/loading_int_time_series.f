      PROGRAM    LOADING_INT_TIME_SERIES
! ************************************************************************
! *                                                                      *
! *   Program  LOADING_INT_TIME_SERIES
! *                                                                      *
! * # 13-MAY-2015 LOADING_INT_TIME_SERIES v1.1 (c) L. Petrov 14-MAR-2016 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      INCLUDE   'diagi.i'
      TYPE     ( DIAGI_STRU ) ::  DIAGI_S
      CHARACTER  C_FIL(MALO__FIL)*128, FILNAM*128, FIL_MASK*128
      INTEGER*8  DIR_DESC(16), IP8
      CHARACTER  DIRIN*128, FILOUT*128, EXT*4, DATE_BEG*32, DATE_END*32, &
     &           DATE_FIL*32, STR*512, MODE*16, COMMENT*128, ZAG*128, &
     &           UNIT*128, OUT(MALO__FIL)*256
      REAL*8     TIM_BEG, TIM_END, TIM_FIL, TAI_DATA, TIM(MALO__FIL), &
     &           TIM_PRC(MALO__FIL), TS
      REAL*8     LOA_INT(MALO__FIL,4), LOA_PRC(MALO__FIL,4)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3, &
     &           DIAGI_LEN, IER
      INTEGER*4  IVRB, MJD_BEG, MJD_END, MJD_FIL, MJD_DATA, K_FIL, L_FIL, &
     &           LEV, IS, IP, NP, J1, J2, J3, J4, J5, J6, J7, IL, ID, IUER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*32, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, LINDEX, &
     &                       MKDIR, OPENDIR, CLOSEDIR
!      
! /imsl/load_int/atm/geosfpit/atm_geosfpit_20150412_2130.txt
!
      IF ( IARGC() < 6 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: loading_int_time_series dirin date_beg date_end method interval filout [comment]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIRIN ) 
           CALL GETARG ( 2, DATE_BEG ) 
           CALL GETARG ( 3, DATE_END ) 
           CALL GETARG ( 4, MODE     ) 
           CALL GETARG ( 5, STR      ) 
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.0'
           READ ( UNIT=STR, FMT='(F10.5)' ) TS
           TS = 86400.0D0*TS
           CALL GETARG ( 6, FILOUT   ) 
           IF ( IARGC() .GE. 7 ) THEN
                CALL GETARG ( 7, COMMENT  ) 
              ELSE
                CALL CLRCH  (    COMMENT  )
           END IF
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TIM_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6701, IUER, 'LOADING_INT_TIME_SERIES', &
     &                    'Wrong begin date '//DATE_BEG )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD_END, TIM_END, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6702, IUER, 'LOADING_INT_TIME_SERIES', &
     &                    'Wrong end date '//DATE_END )
           CALL EXIT ( 1 )
      END IF
!
      EXT = '.txt'
      L_FIL = 0
      LEV = 0
      DO 410 J1=1,MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRIN, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -2
              CALL ERR_LOG ( 6704, IUER, 'LOADING_INT_TIME_SERIES', 'Error in '// &
     &            'reading input directory '//DIRIN(1:I_LEN(DIRIN))// &
     &            '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
!!    write ( 6, * ) ' j1= ', j1, ' file: ', filnam(1:i_len(filnam)) ! %%%
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 410
!
         IL = ILEN(FILNAM)
         IF ( IL < 20 ) GOTO 410
         DATE_FIL = FILNAM(IL-16:IL-13)//'_'//FILNAM(IL-12:IL-11)//'_'// &
     &              FILNAM(IL-10:IL-6)//':'//FILNAM(IL-5:IL-4)//':00.0'
         IUER = -1
         CALL DATE_TO_TIME ( DATE_FIL, MJD_FIL, TIM_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6704, IUER, 'LOADING_INT_TIME_SERIES', &
     &             'Unexpected format of file name '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( MJD_FIL*86400.0D0 + TIM_FIL < MJD_BEG*86400.0D0 +  TIM_BEG ) GOTO 410
         IF ( MJD_FIL*86400.0D0 + TIM_FIL > MJD_END*86400.0D0 +  TIM_END ) GOTO 410
         L_FIL = L_FIL + 1
         C_FIL(L_FIL) = FILNAM
!!   write ( 6, * ) 'j1= ', int2(j1), ' c_fil= '//c_fil(l_fil)(1:i_len(c_fil(l_fil))) ! %%%5
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6704, IUER, 'LOADING_INT_TIME_SERIES', &
     &         'No files with extension '//EXT(1:I_LEN(EXT))// &
     &         ' were found in the input directory '//DIRIN )
           CALL EXIT ( 1 )
      END IF
      CALL SORT_FAST_CH ( L_FIL, C_FIL )
      DO 420 J2=1,L_FIL
         IUER = -1
         CALL RD_TEXT ( C_FIL(J2), 1, STR, NP, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6705, IUER, 'LOADING_INT_TIME_SERIES', &
     &            'Error in reading input file '//C_FIL(J2) )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL DATE_TO_TIME ( STR(7:25), MJD_FIL, TIM_FIL, IUER )
         IF ( IUER .NE. 0 ) THEN
              IUER = -1
              WRITE ( 6, * ) 'STR(7:25) = ', STR(7:25)
              CALL ERR_LOG ( 6706, IUER, 'LOADING_INT_TIME_SERIES', &
     &            'Trap of internal control: wrong data in file '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         TIM(J2) = (MJD_FIL - J2000__MJD)*86400.0D0 + TIM_FIL
         IF ( STR(53:58)   == '******' ) STR(53:58)   = '99.999'
         IF ( STR(71:76)   == '******' ) STR(71:76)   = '99.999'
         IF ( STR(88:93)   == '******' ) STR(88:93)   = '99.999'
         IF ( STR(111:116) == '******' ) STR(111:116) = '99.999'
         READ ( UNIT=STR(53:58),   FMT='(F6.3)' ) LOA_INT(J2,1)
         READ ( UNIT=STR(71:76),   FMT='(F6.3)' ) LOA_INT(J2,2)
         READ ( UNIT=STR(88:93),   FMT='(F6.3)' ) LOA_INT(J2,3)
         READ ( UNIT=STR(111:116), FMT='(F6.3)' ) LOA_INT(J2,4)
 420  CONTINUE 
      DO 430 J3=1,4
         CALL DETREND ( L_FIL, TIM, LOA_INT(1,J3) )
         IF ( MODE == 'GF-DENSE' ) THEN
              CALL GAUSS_FILTER ( 1, L_FIL, TS, TIM, LOA_INT(1,J3), &
     &                               K_FIL, TIM_PRC, LOA_PRC(1,J3) )
              LOA_INT(1:L_FIL,J3) = LOA_PRC(1:L_FIL,J3)
           ELSE IF ( MODE == 'GF-SPARSE' ) THEN
              CALL GAUSS_FILTER ( 2, L_FIL, TS, TIM, LOA_INT(1,J3), &
     &                               K_FIL, TIM_PRC, LOA_PRC(1,J3) )
              LOA_INT(1:K_FIL,J3) = LOA_PRC(1:K_FIL,J3)
           ELSE IF ( MODE == 'RAW' ) THEN
              TIM_PRC = TIM
              LOA_PRC = LOA_INT
              K_FIL = L_FIL
              CONTINUE 
           ELSE 
              IUER = -1
              CALL ERR_LOG ( 6707, IUER, 'LOADING_INT_TIME_SERIES', &
     &            'Unsupported mode '//MODE(1:I_LEN(MODE))//'. The list '// &
     &            'of supported modes: GF-DENSE, GF-SPARSE, RAW' )
              CALL EXIT ( 1 )
         END IF
 430  CONTINUE 
      IF ( MODE == 'GF-SPARSE' ) THEN
           TIM(1:K_FIL) = TIM_PRC(1:K_FIL)
           L_FIL = K_FIL
      END IF 
!
      DO 440 J4=1,L_FIL
         TIM(J4) = 2000.0D0 + TIM(J4)/(86400.0D0*365.25D0)
 440  CONTINUE 
      IF ( INDEX ( FILOUT, 'XW' ) > 0 ) THEN
           CALL DIAGI_SETDEF ( IUER, 'DIAGI_ICL1', 3 )
           IF ( ILEN(COMMENT) > 0 ) THEN
                CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', COMMENT )
                CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Time in years' )
           END IF
           CALL DIAGI_1 ( L_FIL, TIM, LOA_PRC(1,2), IUER )
         ELSE IF ( INDEX ( FILOUT, '.gif' ) > 0 ) THEN
!
! -------- Clear DIAGI_S object
!
           DIAGI_LEN = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
           CALL NOUT ( DIAGI_LEN, DIAGI_S )
!
! -------- Setting default values of the plotting parameters
!
           CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, UNIT, &
     &                       ICL1, ICL2, ICL3, IUER )
!
! -------- Setting up the values of the DIAGI internal data structure for the further
! -------- plotting
!
           DO 450 J5=1,4
              DIAGI_S%IDEV      = 9
              DIAGI_S%NCLR      = 1
              DIAGI_S%NPOI(1)   = L_FIL
              DIAGI_S%LER(1)    = .FALSE.
              DIAGI_S%ICOL(1)   = ICL1
              DIAGI_S%IBST(1)   = 0
              DIAGI_S%ILST(1)   = ILST
              DIAGI_S%IOST(1)   = IOST
              DIAGI_S%IPST(1)   = IPST
              DIAGI_S%IWST(1)   = IWST
              DIAGI_S%ICLR      = 1
              DIAGI_S%XMIN      = 1.0
              DIAGI_S%XMAX      = 0.0
              DIAGI_S%YMIN      = 1.0
              DIAGI_S%YMAX      = 0.0
              DIAGI_S%ARG_UNITS = UNIT
              DIAGI_S%ITRM      = 0
              DIAGI_S%IBATCH    = 1
              DIAGI_S%STATUS    = DIA__DEF
              DIAGI_S%ADR_X8(1) = LOC(TIM)
              ID = LINDEX ( FILOUT, '.' ) - 1
              IF ( ID < 1 ) ID = 1
              IF ( J5 == 1 ) THEN
                   DIAGI_S%ZAG  = COMMENT(1:I_LEN(COMMENT))//' total'
                   DIAGI_S%NAME = FILOUT(1:ID)//'_total.gif'
                 ELSE IF ( J5 == 2 ) THEN
                   DIAGI_S%ZAG  = COMMENT(1:I_LEN(COMMENT))//' land'
                   DIAGI_S%NAME = FILOUT(1:ID)//'_land.gif'
                 ELSE IF ( J5 == 3 ) THEN
                   DIAGI_S%ZAG  = COMMENT(1:I_LEN(COMMENT))//' ocean'
                   DIAGI_S%NAME = FILOUT(1:ID)//'_ocean.gif'
                 ELSE IF ( J5 == 4 ) THEN
                   DIAGI_S%ZAG  = COMMENT(1:I_LEN(COMMENT))//' ocean66'
                   DIAGI_S%NAME = FILOUT(1:ID)//'_ocean66.gif'
              END IF
              DIAGI_S%ADR_Y8(1) = LOC(LOA_PRC(1,J5))
!
! ----------- Calling the main routine of DiaGI
!
              CALL DIAGI     ( DIAGI_S, IUER )
 450       CONTINUE 
         ELSE IF ( INDEX ( FILOUT, '.txt' ) > 0 ) THEN
           DO 460 J6=1,L_FIL
              OUT(J6) = 'Date:                                   Whole_dspl:        Land_dspl:        Sea_dspl:        Sea_66deg_dspl:        mm'
              MJD_FIL = IDINT((TIM(J6)-2000.0D0)*365.25D0) + J2000__MJD
              TIM_FIL =      ((TIM(J6)-2000.0D0)*365.25D0)*86400.D0 - &
     &                  IDINT((TIM(J6)-2000.0D0)*365.25D0)*86400.D0
              OUT(J6)(7:25) = MJDSEC_TO_DATE ( MJD_FIL, TIM_FIL, IUER )
              WRITE ( UNIT=OUT(J6)(27:39),   FMT='(F13.8)' ) TIM(J6)
              WRITE ( UNIT=OUT(J6)(53:58),   FMT='(F6.3)'  ) LOA_PRC(J6,1)
              WRITE ( UNIT=OUT(J6)(71:76),   FMT='(F6.3)'  ) LOA_PRC(J6,2)
              WRITE ( UNIT=OUT(J6)(88:93),   FMT='(F6.3)'  ) LOA_PRC(J6,3)
              WRITE ( UNIT=OUT(J6)(111:116), FMT='(F6.3)'  ) LOA_PRC(J6,4)
 460       CONTINUE 
!
           IUER = -1
           CALL WR_TEXT ( L_FIL, OUT, FILOUT, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
           WRITE ( 6, '(A)' ) 'Output file '//FILOUT(1:I_LEN(FILOUT))
      END IF
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
      RETURN
      END  PROGRAM   LOADING_INT_TIME_SERIES   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GAUSS_FILTER ( MODE, M, HW, TIM, VAL_IN, NO, TIM_OUT, VAL_OUT )
      IMPLICIT   NONE 
      INTEGER*4  MODE, M, NO
      REAL*8     HW, TIM(M), VAL_IN(M), TIM_OUT(M), VAL_OUT(M)
      REAL*8     WIN, WIN_SUM, EXP_VAR, STEP
      INTEGER*4  J1, J2, J3, J4, IBEG, IEND, IND, IS
      IS = IDNINT( HW/(TIM(2) - TIM(1)) ) + 1
!
      IF ( MODE == 1 ) THEN
           DO 410 J1=1,M
              WIN_SUM = 0.0D0
              VAL_OUT(J1) = 0.0D0
              IBEG = J1 - IS
              IEND = J1 + IS
              IF ( IBEG < 1 ) IBEG = 1
              IF ( IEND > M ) IEND = M
              DO 420 J2=IBEG,IEND
                 EXP_VAR = -( (TIM(J2) - TIM(J1))/HW )**2
                 IF ( EXP_VAR .LT. -40.0D0 ) EXP_VAR = -40.0D0
                 WIN_SUM = WIN_SUM + DEXP(EXP_VAR)
                 VAL_OUT(J1) = VAL_OUT(J1) + VAL_IN(J2)*DEXP(EXP_VAR)
 420          CONTINUE 
              TIM_OUT(J1) = TIM(J1)
              VAL_OUT(J1) = VAL_OUT(J1)/WIN_SUM
 410       CONTINUE 
           NO = M
         ELSE IF ( MODE == 2 ) THEN
           STEP = TIM(2) - TIM(1)
           NO = 0
           DO 430 J3=1,M
              IND = IDNINT(J3*HW/STEP)
              IF ( IND < 1 ) IND = 1 
              IF ( IND > M ) GOTO 830
              NO = NO + 1
              WIN_SUM = 0.0D0
              VAL_OUT(J3) = 0.0D0
              IBEG = IND - IS
              IEND = IND + IS
              IF ( IBEG < 1 ) IBEG = 1
              IF ( IEND > M ) IEND = M
              DO 440 J4=IBEG,IEND
                 EXP_VAR = -( (TIM(J4) - TIM(IND))/HW )**2
                 IF ( EXP_VAR .LT. -40.0D0 ) EXP_VAR = -40.0D0
                 WIN_SUM = WIN_SUM + DEXP(EXP_VAR)
                 VAL_OUT(NO) = VAL_OUT(NO) + VAL_IN(J4)*DEXP(EXP_VAR)
 440          CONTINUE 
              TIM_OUT(NO) = TIM(IND)
              VAL_OUT(NO) = VAL_OUT(NO)/WIN_SUM
 430       CONTINUE
 830       CONTINUE 
      END IF
!      
      RETURN
      END  SUBROUTINE  GAUSS_FILTER 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE DETREND ( NP, ARG, VAL )
      IMPLICIT   NONE 
      INTEGER*4  NP
      REAL*8     ARG(NP), VAL(NP), AVR
      INTEGER*4  J1, J2
!
      AVR = 0.D0
      DO 410 J1=1,NP
         AVR = AVR + VAL(J1)
 410  CONTINUE 
      AVR = AVR/NP
      DO 420 J2=1,NP
         VAL(J2) = VAL(J2) - AVR
 420  CONTINUE 
!
      RETURN
      END  !#!  
