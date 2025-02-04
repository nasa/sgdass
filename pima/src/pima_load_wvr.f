      SUBROUTINE PIMA_LOAD_WVR ( PIM, PARAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LOAD_WVR reads the input file with WVR data, parses   *
! *   it and puts its contents into appropriate PIMA data structure.     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   PIM ( PIMA__TYP ) -- Object with information related to program    *
! *                        PIMA.                                         *
! * PARAM ( CHARACTER ) -- One of load, plot1, or plot2.                 *
! *                        load  -- just load the data without plotting. *
! *                        plot1 -- load the data and plot WVR path      *
! *                                 delay versus time.                   *
! *                        plot2 -- load the data and make a plot of     *
! *                                 two functions versus time: WVR path  *
! *                                 delay and scaled elevation.          *
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
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! * ### 03-SEP-2015   PIMA_LOAD_WVR   v1.0 (c) L. Petrov 10-SEP-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      CHARACTER  PARAM*(*)
      TYPE     ( PIMA__TYPE     ) :: PIM
      INTEGER*4    MBUF, MIND
      PARAMETER  ( MBUF = 32*1024 )
      PARAMETER  ( MIND =     256 )
      INTEGER*4    PIMA__WVR_MINPT, PIMA__WVR_MAXPT, PIMA__WVR_MPLOT
      PARAMETER  ( PIMA__WVR_MINPT = 5 )
      PARAMETER  ( PIMA__WVR_MAXPT = 32*1024 )
      PARAMETER  ( PIMA__WVR_MPLOT = 4096 )
      CHARACTER  FMT__LABEL_1*41
      PARAMETER  ( FMT__LABEL_1 = '* WVR_EFL data. Format version 2014.07.24' )
      INTEGER*4  IUER
      REAL*8     HEI_WVR
      CHARACTER  FIL*128, STR*128, STR1*128, STA_NAM*8, BUF(MBUF)*256, &
     &           DATE_STR*23
      REAL*8     TIM_WVR(MBUF), DEL_WVR(MBUF), ELEV_WVR(MBUF), AZ_WVR(MBUF), &
     &           TAI_WVR, UTC_WVR, TIM_STEP, &
     &           TIM_PLOT(PIMA__WVR_MPLOT), DEL_PLOT(PIMA__WVR_MPLOT)
      REAL*8     PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC 
      INTEGER*4  J1, J2, J3, J4, IFMT, IND_STA, K_WVR, L_WVR, NBUF, IDAY, &
     &           MJD_WVR, LIND, IND(2,MIND), IER
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8, NMF_W
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LTM_DIF
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      PIM%WVR_STATUS = PIMA__UNDF
      IF ( PIM%CONF%L_WVR == 0 ) THEN
           WRITE ( 6, '(A)' ) 'No WVR files were specified. Nothinbg to do. Quitting'
           CALL EXIT ( 0 ) 
      END IF
!
! --- Check parameter
!
      IF ( PARAM == 'load' ) THEN
           CONTINUE 
         ELSE IF ( PARAM == 'plot1' ) THEN
           CONTINUE 
         ELSE IF ( PARAM == 'plot2' ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 7611, IUER, 'PIMA_LOAD_WVR', 'Unrecognized '// &
     &         'value of the keyword wvr: '//PARAM(1:I_LEN(PARAM))// &
     &         ': load, plot1, plot2, or plot3  were expected' )
           RETURN 
      END IF
!
! --- Cycle over WVR files
!
      DO 410 J1=1,PIM%CONF%L_WVR
!
! ------ Read the WVR file
!
         CALL RD_TEXT ( PIM%CONF%WVR_FILE(J1), MBUF, BUF, NBUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7611, IUER, 'PIMA_LOAD_WVR', &
     &            'Error in reading WVR file '//PIM%CONF%WVR_FILE(J1) )
              RETURN 
         END IF
!
! ------ Determine its format
!
         IF ( BUF(1)(1:LEN(FMT__LABEL_1)) == FMT__LABEL_1 ) THEN
              IFMT = 1
            ELSE 
              CALL CLRCH ( STR )
              STR = FMT__LABEL_1 
              CALL TRAN ( 13, STR )
              CALL ERR_LOG ( 7612, IUER, 'PIMA_LOAD_WVR', &
     &            'Error in parsing WVR file '// &
     &             PIM%CONF%WVR_FILE(J1)(1:I_LEN(PIM%CONF%WVR_FILE(J1)))// &
     &            ' -- the first line is '//STR(1:I_LEN(STR))// &
     &            ' which is not the recognizable WVR label' )
              RETURN 
         END IF
!
         IND_STA = 0
         K_WVR   = 0
         L_WVR   = 0
!
! ------ Parse the WVR file,line by line
!
         DO 420 J2=2,NBUF
            CALL EXWORD ( BUF(J2), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
            IF ( BUF(J2)(1:15) == '* STATION_NAME:' ) THEN
!
! -------------- Get station name
!
                 STR = BUF(J2)(16:) 
                 CALL CHASHL ( STR )
                 STA_NAM = STR
                 IND_STA = LTM_DIF ( 1, PIM%NSTA, PIM%C_STA, STA_NAM )
                 IF ( IND_STA < 1 ) THEN
                      CALL ERR_LOG ( 7613, IUER, 'PIMA_LOAD_WVR', &
     &                    'Error in parsing WVR file '// &
     &                     PIM%CONF%WVR_FILE(J1)(1:I_LEN(PIM%CONF%WVR_FILE(J1)))// &
     &                    ' -- station '//STA_NAM//' did not '// &
     &                    'observe in experiment '//PIM%CONF%SESS_CODE )
                      RETURN 
                 END IF
               ELSE IF ( BUF(J2)(1:31) == '* WVR_HEIGHT_ABOVE_THE_STATION:' ) THEN
!
! -------------- Get the height of the WVR wrt the station
!
                 STR = BUF(J2)(32:) 
                 CALL CHASHL ( STR )
                 READ ( UNIT=STR, FMT='(F10.5)', IOSTAT=IER ) HEI_WVR
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 7613, IUER, 'PIMA_LOAD_WVR', &
     &                    'Error in parsing WVR file '// &
     &                     PIM%CONF%WVR_FILE(J1)(1:I_LEN(PIM%CONF%WVR_FILE(J1)))// &
     &                    ' -- a valid real number was expected in '// &
     &                    'line '//BUF(J2) )
                      RETURN 
                 END IF
               ELSE IF ( BUF(J2)(1:1) .NE. '*' ) THEN
!
! -------------- Parse the data line
!
                 IF ( K_WVR == 0 .AND. IND_STA == 0 ) THEN
                      CALL ERR_LOG ( 7614, IUER, 'PIMA_LOAD_WVR', &
     &                    'Error in parsing WVR file '// &
     &                     PIM%CONF%WVR_FILE(J1)(1:I_LEN(PIM%CONF%WVR_FILE(J1)))// &
     &                    ' -- station name was not defined' ) 
                      RETURN 
                   ELSE IF ( K_WVR == 0 ) THEN
                      L_WVR = NBUF - J2 + 1
                      IF ( L_WVR > MBUF ) THEN
                           CALL ERR_LOG ( 7615, IUER, 'PIMA_LOAD_WVR', &
     &                         'Trap of internal control: WVR file '// &
     &                          PIM%CONF%WVR_FILE(J1)(1:I_LEN(PIM%CONF%WVR_FILE(J1)))// &
     &                         ' is too long. Please change variable MBUF' )
                           RETURN 
                      END IF
                 END IF
                 K_WVR = K_WVR + 1
!
! -------------- Parse the date
!
                 DATE_STR = BUF(J2)(1:4)//'.01.01_'//BUF(J2)(10:17)//'.000'
                 CALL CHIN ( BUF(J2)(6:8), IDAY )
                 CALL DATE_TO_TIME ( DATE_STR, MJD_WVR, UTC_WVR, IER )
                 MJD_WVR = MJD_WVR + IDAY - 1
                 TAI_WVR = UTC_WVR - PIM%UTC_MTAI
!
! -------------- Transform  the data to the time elapsed since the nominal experiment 
! -------------- start
!
                 TIM_WVR(K_WVR) = (MJD_WVR - PIM%MJD_0)*86400.0D0 + (TAI_WVR - PIM%TAI_0)
!
! -------------- Read delay, elevation angle, azimuth angle
!
                 READ ( UNIT=BUF(J2)(IND(1,7):IND(2,7)), FMT='(F7.2)' ) DEL_WVR(K_WVR)
                 READ ( UNIT=BUF(J2)(IND(1,4):IND(2,4)), FMT='(F7.2)' ) ELEV_WVR(K_WVR)
                 READ ( UNIT=BUF(J2)(IND(1,3):IND(2,3)), FMT='(F7.2)' ) AZ_WVR(K_WVR)
!
! -------------- ... and transform them to SI units 
!
                 AZ_WVR(K_WVR) = AZ_WVR(K_WVR)*DEG__TO__RAD
                 DEL_WVR(K_WVR) = 1.D-3*DEL_WVR(K_WVR)/VTD__C
                 ELEV_WVR(K_WVR) = ELEV_WVR(K_WVR)*DEG__TO__RAD
            END IF
 420     CONTINUE 
!
! ------ Check whether file covers entire date range of the experiment
!
         IF ( TIM_WVR(K_WVR) < PIM%TIM_R8(PIM%NOBS) ) THEN
              IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 .OR. PIM%CONF%WARNING ) THEN
                   DATE_STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_WVR(K_WVR), IER )
                   STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(PIM%NOBS), IER )
                   WRITE ( 6, 110 ) PIM%C_STA(IND_STA), DATE_STR, STR(1:23)
 110               FORMAT ( 'PIMA_LOAD_WVR: WVR file for station ', A, ' is too short: '/ &
     &                      '     it has data that end at   ', A/ &
     &                      '     earlier than session end: ', A  )
              END IF
              IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                   CALL ERR_PASS ( 7616, IUER )
                   RETURN 
              END IF
         END IF
         IF ( TIM_WVR(1) > 0.0D0 ) THEN
              IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 .OR. PIM%CONF%WARNING ) THEN
                   DATE_STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_WVR(1), IER )
                   STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0, IER )
                   WRITE ( 6, 120 ) PIM%C_STA(IND_STA), DATE_STR, STR(1:23)
 120               FORMAT ( 'PIMA_LOAD_WVR: WVR file for station ', A, ' is too short: '/ &
     &                      '     it has data that start at ', A/ &
     &                      '     later than session start: ', A  )
              END IF
              IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                   CALL ERR_PASS ( 7617, IUER )
                   RETURN 
              END IF
         END IF
!
! ------ Check whether the file has enough data points
!
         IF ( K_WVR < 3 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( K_WVR, STR )
              CALL ERR_LOG ( 7616, IUER, 'PIMA_LOAD_WVR', 'Trap of '// &
     &            'internal control: too few points were found in WVR file '// &
     &             PIM%CONF%WVR_FILE(J1)(1:I_LEN(PIM%CONF%WVR_FILE(J1)))// &
     &             ' -- only '//STR )
              RETURN 
         END IF
!
! ------ Allocate memory in WVR data strucuture of PIMA
!
         PIM%STA(IND_STA)%L_WVR = K_WVR
         IF ( ASSOCIATED ( PIM%STA(IND_STA)%WVR%TIM_ARR ) ) THEN
              DEALLOCATE ( PIM%STA(IND_STA)%WVR%TIM_ARR )
         END IF
         IF ( ASSOCIATED ( PIM%STA(IND_STA)%WVR%DEL_ARR ) ) THEN
              DEALLOCATE ( PIM%STA(IND_STA)%WVR%DEL_ARR )
         END IF
         IF ( ASSOCIATED ( PIM%STA(IND_STA)%WVR%EL_ARR ) ) THEN
              DEALLOCATE ( PIM%STA(IND_STA)%WVR%EL_ARR )
         END IF
         IF ( ASSOCIATED ( PIM%STA(IND_STA)%WVR%AZ_ARR ) ) THEN
              DEALLOCATE ( PIM%STA(IND_STA)%WVR%AZ_ARR )
         END IF
!
         PIM%STA(IND_STA)%WVR%HEI_WVR = HEI_WVR 
         ALLOCATE ( PIM%STA(IND_STA)%WVR%TIM_ARR(PIM%STA(IND_STA)%L_WVR), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7619, IUER, 'PIMA_LOAD_WVR', 'Trap of '// &
     &            'internal control: failure to allocate object WVR%TIM_ARR '// &
     &            'for station '//PIM%C_STA(IND_STA) )
              RETURN 
         END IF
!
         ALLOCATE ( PIM%STA(IND_STA)%WVR%DEL_ARR(PIM%STA(IND_STA)%L_WVR), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7619, IUER, 'PIMA_LOAD_WVR', 'Trap of '// &
     &            'internal control: failure to allocate object WVR%DEL_ARR '// &
     &            'for station '//PIM%C_STA(IND_STA) )
              RETURN 
         END IF
!
         ALLOCATE ( PIM%STA(IND_STA)%WVR%DEL_ERR(PIM%STA(IND_STA)%L_WVR), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7619, IUER, 'PIMA_LOAD_WVR', 'Trap of '// &
     &            'internal control: failure to allocate object WVR%ERR_ARR '// &
     &            'for station '//PIM%C_STA(IND_STA) )
              RETURN 
         END IF
!
         ALLOCATE ( PIM%STA(IND_STA)%WVR%EL_ARR(PIM%STA(IND_STA)%L_WVR), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7619, IUER, 'PIMA_LOAD_WVR', 'Trap of '// &
     &            'internal control: failure to allocate object WVR%EL_ARR '// &
     &            'for station '//PIM%C_STA(IND_STA) )
              RETURN 
         END IF
!
         ALLOCATE ( PIM%STA(IND_STA)%WVR%AZ_ARR(PIM%STA(IND_STA)%L_WVR), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 7619, IUER, 'PIMA_LOAD_WVR', 'Trap of '// &
     &            'internal control: failure to allocate object WVR%AZ_ARR '// &
     &            'for station '//PIM%C_STA(IND_STA) )
              RETURN 
         END IF
!
! ------ Get station geocentric latitude
!
         CALL REF_ELL ( 1, PIM%STA(IND_STA)%COO, PHI_GCN, PHI_GDT, LAMBDA, &
     &                  H_ELL, RD, G_ACC )
!
! ------ Copy the data into tyhe WVR data strucutre
!
         DO 430 J3=1,PIM%STA(IND_STA)%L_WVR
            PIM%STA(IND_STA)%WVR%TIM_ARR(J3) = TIM_WVR(J3)
            PIM%STA(IND_STA)%WVR%DEL_ARR(J3) = DEL_WVR(J3)
            PIM%STA(IND_STA)%WVR%DEL_ERR(J3) = 3.0D-12/NMF_W ( PHI_GCN, ELEV_WVR(J3) )
            PIM%STA(IND_STA)%WVR%EL_ARR(J3)  = ELEV_WVR(J3)
            PIM%STA(IND_STA)%WVR%AZ_ARR(J3)  = AZ_WVR(J3)
 430     CONTINUE 
         IF ( PARAM == 'plot1' .OR. PARAM == 'plot2' ) THEN
!
! ----------- Make a plot
!
              STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + TIM_WVR(1) , IER )
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'WVR data for '// &
     &                            PIM%C_STA(IND_STA)//' wrt '//STR(1:19) )
              CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', &
     &                           'Time in seconds wrt nominal start of '// &
     &                           PIM%CONF%SESS_CODE )
              IF ( PARAM == 'plot1' ) then
                   CALL DIAGI_1 ( K_WVR, TIM_WVR, DEL_WVR, IER )
                 ELSE IF ( PARAM == 'plot2' ) THEN
                   ELEV_WVR = ELEV_WVR/90.0D0*DEL_WVR(K_WVR)
                   CALL DIAGI_2 ( K_WVR, TIM_WVR, DEL_WVR, &
     &                            K_WVR, TIM_WVR, ELEV_WVR, IER )
              END IF
         END IF
 410  CONTINUE 
!
      PIM%WVR_STATUS = PIMA__LOADED
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_LOAD_WVR  !#!  
