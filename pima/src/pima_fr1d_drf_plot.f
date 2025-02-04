      SUBROUTINE PIMA_FR1D_DRF_PLOT ( PIM, IND_OBS, FINAM_PLOT, LTIM, LCHN, &
     &                                LFRQ, FREQ_ARR, FREQ_REF, WEI, &
     &                                UV, TIME_FRT, AP_LEN, GRPAMBSP, &
     &                                GR_DEL, PH_RAT, GR_RAT, PH_ACC, PHAS, &
     &                                AMPL, SNR, IDEV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_FR1D_DRF_PLOT 
! *                                                                      *
! * ## 15-JAN-2006  PIMA_FR1D_DRF_PLOT v1.9 (c) L. Petrov 07-AUG-2016 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'diagi.i'
      CHARACTER  FINAM_PLOT*(*)
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( DIAGI_STRU  ) :: DIA
      INTEGER*4  IND_OBS, LTIM, LCHN, LFRQ, IDEV, IUER
      REAL*8     FREQ_ARR(LCHN,LFRQ), FREQ_REF, TIME_FRT, AP_LEN, &
     &           GRPAMBSP, GR_DEL, PH_RAT, GR_RAT, PH_ACC, PHAS, &
     &           AMPL, SNR
      COMPLEX*8  UV(PIM%NCHN,LFRQ,LTIM)
      REAL*4     WEI(LTIM)
      INTEGER*4  MARR, MPB
!!      PARAMETER  ( MARR = 4097 )
      PARAMETER  ( MARR = 257 )
      PARAMETER  ( MPB  =   4 )
      REAL*4     PHS_MOD
      REAL*8     GRDEL_R8(MARR), DRF_R8(MARR), WPOI, MD_MIN, MD_MAX, &
     &           MD_STEP, DRF_MIN, DRF_MAX, GRPAMBSP__DEF
      PARAMETER  ( GRPAMBSP__DEF = 3.D-8 )
      COMPLEX*8  DRF, DRF_IF(PIM__MFRQ)
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, ICL1, ICL2, ICL3, &
     &           IND_STA(2), IND_SOU, J1, J2, J3, J4, J5, J6, J7, UV_IND, &
     &           ICODE, NC, NR, IV8(MARR), ID, IP, NN, IDEV_DEF, IFRQ, &
     &           FRG_IND, NO, MODE_DRF3, IER
      INTEGER*4  I1, I2
      CHARACTER  COMMON_TIT*80, TITS*128, BUTTON_NAME(MPB)*24, &
     &           BUTTON_LET(MPB)*2, PREF_NAME*128, ZAG*128, UNIT*128, STR*128, &
     &           STR1*32, FINAM_TXT_PLOT*128
      CHARACTER*128, ALLOCATABLE :: OUT(:)
      LOGICAL*1  FL_FRINGE_DFT, FL_OPT
      REAL*8,    ALLOCATABLE :: CFRQ_REF(:,:), TIM_ARR(:)
      DATA      ( BUTTON_LET(NN), BUTTON_NAME(NN), NN=1,MPB ) &
     &          / &
     &          'Xx', 'Exit plot               ',  &
     &          'Qq', 'Quit                    ',  &
     &          '  ', '                        ',  &
     &          '  ', '                        '   &
     &          /
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4 
      LOGICAL*4, EXTERNAL :: IS_R4_NAN 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL :: GET_CDATE*19
#ifdef NO_PLOT
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FR1D_PLOT  !#!  
#else
      CALL GETENVAR ( 'PIMAVAR_FRINGE_DFT', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_FRINGE_DFT = .TRUE.
           FL_OPT = .FALSE.
         ELSE 
           FL_FRINGE_DFT = .FALSE.
           FL_OPT = .TRUE.
      END IF
!
! --- Setting defaults values of the plotting parameters
!
      CALL ERR_PASS   ( IUER, IER )
      CALL DIAGI_DEF  ( IBST, ILST, IOST, IPST, IWST, IDEV_DEF, ZAG, UNIT, &
     &                  ICL1, ICL2, ICL3, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7581, IUER, 'PIMA_FR1D_DRF_PLOT', 'Error in '// &
     &                    'setting default values for the plot' )
           RETURN
      END IF
      IF ( IDEV == 0 ) IDEV = IDEV_DEF
      PREF_NAME = '/tmp/'
!
! --- Allocate memory for intermediate frequency arrays needed for
! --- speeding up calculations
!
      ALLOCATE ( CFRQ_REF(PIM%NCHN,LFRQ), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*LTIM, STR )
           CALL ERR_LOG ( 7582, IUER, 'PIMA_FR1D_DRF_PLOT', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array CFRQ_REF' )
           RETURN
      END IF
!
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      IF ( FRG_IND == 0 ) THEN
           WRITE ( 6, * ) 'No uv data for obs ', IND_OBS, ' frequency group ', &
     &                     PIM%CONF%FRQ_GRP
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IFRQ = 0
      DO 410 J1=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
         IFRQ = IFRQ + 1
         DO 420 J2=1,PIM%NCHN
            CFRQ_REF(J2,IFRQ) = PI2* ( FREQ_ARR(J2,IFRQ) - FREQ_REF )
 420     CONTINUE 
 410  CONTINUE 
!
      ALLOCATE ( TIM_ARR(LTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*LTIM, STR )
           CALL ERR_LOG ( 7584, IUER, 'PIMA_FR1D_DRF_PLOT', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for array TIM_ARR' )
           RETURN
      END IF
      DO 430 J3=1,LTIM
         UV_IND  = PIM%OBS(IND_OBS)%UV_IND(J3,FRG_IND)
         TIM_ARR(J3) = (PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                  PIM%OBS(IND_OBS)%TIM_BEG)
 430  CONTINUE 
!
      IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_BIN .OR. &
     &     PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_LSQ      ) THEN
           MODE_DRF3 = PIMA__GRAT
         ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_PAR ) THEN
           MODE_DRF3 = PIMA__ACC  
         ELSE IF ( PIM%CONF%FRIB_FINE_SEARCH == PIMA__FINE_SEARCH_ACC ) THEN
           MODE_DRF3 = PIMA__ACC  
      END IF
!
      IF ( GRPAMBSP < PIMA__MD_ERR_LIM ) THEN
           MD_MIN  = -GRPAMBSP__DEF*PIM%CONF%FRIB_1D_DRF_SPAN
           MD_MAX  =  GRPAMBSP__DEF*PIM%CONF%FRIB_1D_DRF_SPAN
         ELSE 
           MD_MIN  = -GRPAMBSP*PIM%CONF%FRIB_1D_DRF_SPAN
           MD_MAX  =  GRPAMBSP*PIM%CONF%FRIB_1D_DRF_SPAN
      END IF
      MD_STEP =  (MD_MAX - MD_MIN)/(MARR-1)
      DRF_MAX = 0.0D0
      DO 450 J5=1,MARR
         GRDEL_R8(J5) = MD_MIN + MD_STEP*(J5-1)
         CALL PIMA_UV_DRF3 ( MODE_DRF3, FL_OPT, LTIM, PIM%NCHN, LFRQ, WEI, TIM_ARR, &
     &                       PIM%CONF%FRIB_WEIGHTS_THRESHOLD, &
     &                       TIME_FRT, PH_RAT, GR_DEL + GRDEL_R8(J5), &
     &                       GR_RAT, PH_ACC, FREQ_ARR, FREQ_REF, CFRQ_REF, UV, &
     &                       DRF, DRF_IF )
         DRF_R8(J5) = ABS(DRF)
         IF ( DRF_R8(J5) > DRF_MAX ) THEN
              DRF_MAX = DRF_R8(J5)
         END IF
 450  CONTINUE 
!
      DO 460 J6=1,MARR
         DRF_R8(J6) = DRF_R8(J6)/DRF_MAX
         IF ( J6 == 1 ) THEN
              DRF_MIN = DRF_R8(J6) 
            ELSE 
              DRF_MIN = MIN ( DRF_MIN, DRF_R8(J6) )
         END IF
 460  CONTINUE 
!
      IND_STA(1) = PIM%OBS(IND_OBS)%STA_IND(1)
      IND_STA(2) = PIM%OBS(IND_OBS)%STA_IND(2)
      IND_SOU    = PIM%OBS(IND_OBS)%SOU_IND
!
      CALL NOUT ( SIZEOF(DIA), DIA )
      DIA%IDEV = IDEV
      DIA%NCLR = 1
      DIA%NPOI(1)   = MARR
      DIA%ADR_X8(1) = LOC(GRDEL_R8)
      DIA%ADR_Y8(1) = LOC(DRF_R8)
      DIA%ADR_E8(1) = 0
      DIA%LER(1)    = .FALSE.
      DIA%IPST(1)   = 1
      DIA%ICOL(1)   = 1
      DIA%IBST(1)   = 0
      DIA%ILST(1)   = 3
      DIA%IOST(1)   = 1
      DIA%IWST(1)   = 2
      DIA%ICLR      = 1
      DIA%XMIN   =  GRDEL_R8(1)    - (GRDEL_R8(MARR) - GRDEL_R8(1))*DIAGI_FIE 
      DIA%XMAX   =  GRDEL_R8(MARR) + (GRDEL_R8(MARR) - GRDEL_R8(1))*DIAGI_FIE 
      DIA%YMIN   =  0.0D0
      DIA%YMAX   =  1.0D0 + DIAGI_FIE 
      DIA%ARG_UNITS = 'Group delay: sec'
      DIA%NAME   = FINAM_PLOT
      DIA%ITRM   = 0
      IF ( IDEV == 1  .OR. &
     &     IDEV == 2  .OR. &
     &     IDEV == 3  .OR. &
     &     IDEV == 4  .OR. &
     &     IDEV == 5  .OR. &
     &     IDEV == 6       ) THEN
           DIA%IBATCH = 0
         ELSE IF ( IDEV == 100 ) THEN
           IP = ILEN(FINAM_PLOT)
           FINAM_PLOT = FINAM_PLOT(1:IP-1)//'.sav'
           DIA%NAME   = FINAM_PLOT
           DIA%IBATCH = 2
         ELSE 
           DIA%IBATCH = 1
      END IF
      DIA%STATUS = DIA__DEF
      TITS = PIM%CONF%BAND//'-band delay resolution function for '// &
     &       PIM%SOU(IND_SOU)%IVS_NAME//' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &       '/'//PIM%STA(IND_STA(2))%IVS_NAME// &
     &       ' in '//PIM%OBS_CODE
      DIA%ZAG  = PIM%CONF%BAND//'-band delay resolution function for '// &
     &           PIM%SOU(IND_SOU)%IVS_NAME// &
     &           ' at '//PIM%STA(IND_STA(1))%IVS_NAME// &
     &           '/'//PIM%STA(IND_STA(2))%IVS_NAME
      NC = 1
      NR = 1
!     
      CALL CLRCH ( STR1 ) 
      CALL INCH  ( IND_OBS, STR1 )
      COMMON_TIT = 'Delay resolution function for obs #'//STR1(1:I_LEN(STR1))// &
     &             ' Exp '//PIM%OBS_CODE
!
      IF ( IDEV == -1 ) THEN
           FINAM_TXT_PLOT = FINAM_PLOT(1:I_LEN(FINAM_PLOT))//'drf.txt'
!
! -------- Allocate memory for temporary buffer
!
           ALLOCATE   ( OUT(MARR+32), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7585, IUER, 'PIMA_FR1D_DRF_PLOT', 'Failure '// &
     &                        'to allocate memory for internal buffer '// &
     &                        'of the output text file' )
                DEALLOCATE ( OUT )
                RETURN 
           END IF
!
! -------- Prepare table preamble
!
           OUT(1) = PIMA__TXT1D_LABEL
           OUT(2) = '#'
           OUT(2) = '# Plot of delay resolution function group delay frequency'
           OUT(2) = '#'
           OUT(3) = '# Generated on '//GET_CDATE()
           OUT(4) = '# Generated by '//PIMA__LABEL
           OUT(5) = '#'
!
           NO = 5
           NO = NO + 1; OUT(NO) = 'PLOT_TITLE:  '//TITS
           NO = NO + 1; OUT(NO) = 'SUBTITLE:    '
           CALL INCH  ( MARR, STR )
           NO = NO + 1; OUT(NO) = 'NUM_POINTS:  '//STR
           NO = NO + 1; OUT(NO) = 'AXIS1_TITLE: Group delay'
           NO = NO + 1; OUT(NO) = 'AXIS1_UNITS: s'
!
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) GRDEL_R8(1)
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS1_MIN:   '//STR(1:15)
           WRITE ( UNIT=STR(1:15), FMT='(1PD15.7)' ) GRDEL_R8(MARR)
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS1_MAX:   '//STR(1:15)
!
           NO = NO + 1; OUT(NO) = 'AXIS2_NAME:  Normalized fringe amplitude'
           NO = NO + 1; OUT(NO) = 'AXIS2_UNITS: dimensionless'
           WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) DRF_MIN
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS2_MIN:   '//STR(1:15)
           WRITE ( UNIT=STR(1:15), FMT='(F10.7)' ) DRF_MAX
           CALL CHASHL ( STR )
           NO = NO + 1; OUT(NO) = 'AXIS2_MAX:   '//STR(1:15)
           NO = NO + 1; OUT(NO) = 'AXIS2_ERRS:  no'
!
! -------- Write the agrguments and values of plotting variables
!
           DO 470 J7=1,MARR
              NO = NO + 1
              WRITE  ( OUT(NO), 110 ) J7, GRDEL_R8(J7), DRF_R8(J7)
 110          FORMAT ( 'POINT: ', I5, 1X, 1PD15.7, 1X, 0PF10.7 )
 470       CONTINUE 
           NO = NO + 1; OUT(NO) = '#'
           NO = NO + 1; OUT(NO) = PIMA__TXT1D_LABEL
!
! -------- Write the buffer into file
!
           CALL ERR_PASS ( IUER, IER )
           CALL WR_TEXT  ( NO, OUT, FINAM_TXT_PLOT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7586, IUER, 'PIMA_FR1D_DRF_PLOT', 'Failure '// &
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
         ELSE IF ( IDEV == 10 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL DIAGI ( DIA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7587, IUER, 'PIMA_FR1D_DRF_PLOT', 'Failure to '// &
     &              'make a plot of delay resolution function' )
                RETURN 
           END IF
         ELSE IF ( IDEV >  0 ) THEN
           ICODE = 0
           ID = LINDEX ( FINAM_PLOT, '/' ) 
           IF ( ID < 2 ) ID = 2
           CALL ERR_PASS ( IUER, IER )
           CALL MULTI_DIAGI ( COMMON_TIT, 1, NC, NR, TITS, MPB, & 
     &                        BUTTON_NAME, BUTTON_LET, PREF_NAME, DIA, &
     &                        ICODE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7588, IUER, 'PIMA_FR1D_DRF_PLOT', 'Failure to '// &
     &              'make a plot of delay resolution function' )
                RETURN 
           END IF
      END IF
!
      DEALLOCATE ( CFRQ_REF )
      DEALLOCATE ( TIM_ARR  )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FR1D_DRF_PLOT  !#!#
#endif
