      SUBROUTINE PIMA_WVR_TO_OBS ( PIM, IND_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_WVR_TO_OBS computes for the given observation the     *
! *   path delay in the atmosphere from measurements by the Water Vapor  *
! *   Radiometer(s) (WVR) using a requested method of interpolation and  *
! *   smoothing.                                                         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     PIM ( PIMA__TYP ) -- Object with information related to program  *
! *                          PIMA.                                       *
! * IND_OBS ( INTEGER*4 ) -- index of the observation.                   *
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
! * ### 06-SEP-2015  PIMA_WVR_TO_OBS v1.1 (c)  L. Petrov 31-AUG-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, IUER 
      INTEGER*4  M_WVR 
      PARAMETER  ( M_WVR = 32*1024 )
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, SGN_STA(2), IND_BEG, IND_END, &
     &           KP, LP, FRG_IND, DEG, IER
      REAL*8     EPS, SCL
      PARAMETER  ( EPS = 0.05  )
      PARAMETER  ( SCL = 1.D11 )
      LOGICAL*1  FL_WVR_PLOT
      REAL*8     WRMS, WVR_TIM(M_WVR), WVR_SPL(M_WVR), TIM(M_WVR), VAL(M_WVR), &
     &           TIM_ARG(M_WVR), DEL_ARG(M_WVR), WEI_ARG(M_WVR)
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8
      INTEGER*4, EXTERNAL :: IXMN8, ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      SGN_STA(1) = -1
      SGN_STA(2) =  1
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP) 
!
      FL_WVR_PLOT = .FALSE.
      CALL GETENVAR ( 'PIMAVAR_WVR_PLOT_ONLY', STR )
      IF ( STR == 'YES' ) FL_WVR_PLOT = .TRUE.
!
      IF ( PIM%CONF%L_WVR > 0 ) THEN
!
! -------- Allocate memory for WVR delay for a given observation
!
           IF ( ASSOCIATED ( PIM%OBS(IND_OBS)%WVR_DELAY ) ) THEN
                DEALLOCATE ( PIM%OBS(IND_OBS)%WVR_DELAY )
           END IF 
           ALLOCATE ( PIM%OBS(IND_OBS)%WVR_DELAY(PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND)), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IND_OBS, STR )
                CALL ERR_LOG ( 4711, IUER, 'PIMA_WVR_TO_OBS', 'Failure in an '// &
     &              'attempt to allocate dynamic memory for array WVR_DELAY '// &
     &              'for observation '//STR )
                RETURN 
           END IF
           PIM%OBS(IND_OBS)%WVR_DELAY = 0.0D0
!
! -------- Cycle over stations of the observation
!
           DO 410 J1=1,2
              IF ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%L_WVR > 0 ) THEN
!
! ---------------- Find the first epoch of WVR data just preceeding the nominal
! ---------------- observation start
!
                   IND_BEG = IXMN8 ( PIM%STA( PIM%OBS(IND_OBS)%STA_IND(J1))%L_WVR, &
     &                               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%TIM_ARR, &
     &                               PIM%OBS(IND_OBS)%TIM_BEG )
                   IF ( IND_BEG == -1 ) THEN
                        IF ( PIM%CONF%WARNING .AND. PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                             WRITE ( 6, 110 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1))
 110                         FORMAT ( 'Observation ', I6, ' at station '// &
     &                                 A, ' starter earlier than the first '// &
     &                                'WVR data' )
                        END IF
                        IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( IND_OBS ) 
                             CALL ERR_LOG ( IUER, 4712, 'PIMA_WVR_TO_OBS', &
     &                           'Observation '//STR(1:I_LEN(STR))//' at station '// &
     &                            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1))// &
     &                           ' started earlier than the first WVR data' )
                        END IF
                   END IF
!
                   IND_BEG = IND_BEG + 1 ! the first epoch within the observation
!
! ---------------- Find the last epoch of WVR data just preceeding the nominal
! ---------------- observation stop
!
                   IND_END = IXMN8 ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%L_WVR, &
     &                               PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%TIM_ARR, &
     &                               PIM%OBS(IND_OBS)%TIM_END )
                   IF ( IND_END == -2 ) THEN
                        IF ( PIM%CONF%WARNING .AND. PIM%CONF%CHECK_SEVERITY < 2 ) THEN
                             WRITE ( 6, 120 ) IND_OBS, PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1))
 120                         FORMAT ( 'Observation ', I6, ' at station '// &
     &                                 A, ' ended later than the last '// &
     &                                'WVR data' )
                        END IF
                        IF ( PIM%CONF%CHECK_SEVERITY .GE. 2 ) THEN
                             CALL CLRCH ( STR )
                             CALL INCH  ( IND_OBS ) 
                             CALL ERR_LOG ( IUER, 4713, 'PIMA_WVR_TO_OBS', &
     &                           'Observation '//STR(1:I_LEN(STR))//' at station '// &
     &                            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1))// &
     &                           ' ended later than the last WVR data' )
                             RETURN 
                        END IF
                   END IF
                   LP = IND_END - IND_BEG + 1 ! The number of epochs within the observation
!
! ---------------- Compute KP -- the number of B-spline knots
!
                   KP = IDNINT ( (PIM%OBS(IND_OBS)%TIM_END - PIM%OBS(IND_OBS)%TIM_BEG)/ &
     &                            PIM%CONF%WVR_SMOOTHING_INTERVAL )
!
! ---------------- Set the B-spline degree
!
                   IF ( PIM%CONF%WVR_USE == PIMA__WVR_SPLINE_3RD ) THEN
                        DEG = 3
                      ELSE IF ( PIM%CONF%WVR_USE == PIMA__WVR_SPLINE_LIN ) THEN
                        DEG = 1
                      ELSE IF ( PIM%CONF%WVR_USE == PIMA__WVR_SPLINE_AVR ) THEN
                        DEG = 0
                   END IF
!
! ---------------- Compute the scan averaged path delay
!
                   PIM%OBS(IND_OBS)%WVR_DEL_AVR = 0.0D0
                   DO 420 J2=IND_BEG,IND_END
                      PIM%OBS(IND_OBS)%WVR_DEL_AVR = PIM%OBS(IND_OBS)%WVR_DEL_AVR + PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%DEL_ARR(J2)
 420               CONTINUE 
                   PIM%OBS(IND_OBS)%WVR_DEL_AVR = PIM%OBS(IND_OBS)%WVR_DEL_AVR/ &
     &                                            (IND_END - IND_BEG + 1)
!
! ---------------- Extract an array of time epochs, delays and weights
! ---------------- We also scale delays and weights in order to avoid
! ---------------- a catatrophic precision loss
!
                   DO 430 J3=1,LP
                      TIM_ARG(J3) = PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%TIM_ARR(IND_BEG+J3-1)
                      DEL_ARG(J3) = ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%DEL_ARR(IND_BEG+J3-1) - &
     &                                PIM%OBS(IND_OBS)%WVR_DEL_AVR )*SCL
                      WEI_ARG(J3) = 1.D0/(SCL*PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%DEL_ERR(IND_BEG+J3-1))
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                           WRITE ( 6, 210 ) PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1)), &
     &                                      IND_OBS, IND_BEG+J3-1, &
     &                                      MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                                PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%TIM_ARR(IND_BEG+J3-1), IER ), &
     &                                      PIM%STA(PIM%OBS(IND_OBS)%STA_IND(J1))%WVR%DEL_ARR(IND_BEG+J3-1)*VTD__C*1.D3
 210                       FORMAT ( 'PIMA_WVR_TO_OBS sta: ', A, 1X, ' Obs: ', I5, 1X, &
     &                              ' Pt: ', I5, ' Tai: ', A23, ' Del: ', F6.1, ' mm ' )
                      END IF
 430               CONTINUE 
!
                   IF ( KP < 3 ) THEN
!
! --------------------- Too few points for a meaningful interpolation?
! --------------------- Let us use the scan-average phase delay
!
                        DO 440 J4=1,PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND)
                           PIM%OBS(IND_OBS)%WVR_DELAY(J4) = &
     &                         PIM%OBS(IND_OBS)%WVR_DELAY(J4) + &
     &                         SGN_STA(J1)*PIM%OBS(IND_OBS)%WVR_DEL_AVR
 440                    CONTINUE 
                      ELSE
!
! --------------------- Compute tghe array of time epochs for interpolation knots
!
                        DO 450 J5=1,KP
                           WVR_TIM(J5) = PIM%OBS(IND_OBS)%TIM_BEG + &
     &                                   (J5-1)*(PIM%OBS(IND_OBS)%TIM_END - PIM%OBS(IND_OBS)%TIM_BEG)/ &
     &                                   (KP-1)
 450                    CONTINUE 
!
! --------------------- Slightly adjust the time epochs at the edges of the 
! --------------------- interpolation interaval
!
                        WVR_TIM(1)  = WVR_TIM(1)  - EPS*(WVR_TIM(2)  - WVR_TIM(1))
                        WVR_TIM(KP) = WVR_TIM(KP) + EPS*(WVR_TIM(KP) - WVR_TIM(KP-1))
!
! --------------------- Compute the smooting B-splines of the WVR path delay 
! --------------------- with applying constraints on the value of the B-spline
!
                        CALL ERR_PASS  ( IUER, IER )
                        CALL EBSPL_WLSQ_CNS3  ( LP, TIM_ARG, DEL_ARG, WEI_ARG, &
     &                                          KP, DEG, WVR_TIM, WVR_SPL, &
     &                                          SCL*PIM%CONF%WVR_SMOOTHING_SIGMA, &
     &                                          0.0D0, 0.0D0, WRMS, IER )
                        IF ( IER .NE. 0 ) THEN
                             WRITE ( 6, * ) ' KP= ', KP, ' LP= ', LP, ' DEG= ', DEG
                             CALL CLRCH ( STR )
                             CALL INCH  ( IND_OBS, STR )
                             CALL ERR_LOG ( 4714, IUER, 'PIMA_WVR_TO_OBS', &
     &                           'Failure in attempt to expand path delay '// &
     &                           'in the atmsphere in B-spline basis '// &
     &                           'for station '//PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1))// &
     &                           ' observation '//STR )
                             DEALLOCATE ( PIM%OBS(IND_OBS)%WVR_DELAY )
                             RETURN 
                        END IF
!
! --------------------- Now computer the path delay for each AP of the observation
!
                        DO 460 J6=1,PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND)
                           TIM(J6) = PIM%OBS(IND_OBS)%TIM_BEG + (J6-1)*PIM%OBS(IND_OBS)%AP_LEN
                           PIM%OBS(IND_OBS)%WVR_DELAY(J6) = &
     &                        PIM%OBS(IND_OBS)%WVR_DELAY(J6) + SGN_STA(J1)* &
     &                        ( EBSPL_VAL_R8 ( KP, DEG, TIM(J6), WVR_TIM, WVR_SPL )/SCL + &
     &                          PIM%OBS(IND_OBS)%WVR_DEL_AVR )
!
! ------------------------ This array is for plotting
!
                           VAL(J6) = EBSPL_VAL_R8 ( KP, DEG, TIM(J6), WVR_TIM, WVR_SPL )/SCL + &
     &                               PIM%OBS(IND_OBS)%WVR_DEL_AVR
 460                    CONTINUE 
                   END IF
!
                   IF ( FL_WVR_PLOT ) THEN
!
! --------------------- Make a plot of path delay if requested
!
                        DEL_ARG = DEL_ARG/SCL + PIM%OBS(IND_OBS)%WVR_DEL_AVR 
                        CALL CLRCH ( STR )
                        CALL INCH  ( IND_OBS, STR )
                        CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', 'WVR Delay at '// &
     &                             PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(J1))// &
     &                             ' Obs: #'//STR(1:I_LEN(STR)) )
                        CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', &
     &                                     'Time wrt the observation start in seconds' )
                        CALL DIAGI_2 ( PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND), TIM, VAL, &
     &                                 LP, TIM_ARG, DEL_ARG, IER )
                   END IF
!
! ---------------- Store the scan-averaged WVR path delay
!
                   PIM%OBS(IND_OBS)%WVR_DEL_AVR = SGN_STA(J1)*PIM%OBS(IND_OBS)%WVR_DEL_AVR
!
! ---------------- Set the flag that the WVR phases are available for this observation
!
                   PIM%OBS(IND_OBS)%WVR_FLAG = PIMA__WVR_AVL 
              END IF
 410       CONTINUE 
         ELSE
           CALL ERR_LOG ( 0, IUER )
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_WVR_TO_OBS  !#!  
