      SUBROUTINE PIMA_ONOF_PROC ( PIM, IND_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_ONOF_PROC  computes ON-OFF flags.                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     PIM ( PIMA__TYP ) -- Object with information related to program  *
! *                          PIMA.                                       *
! * IND_OBS ( INTEGER*4 ) -- Observation index.                          *
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
! * ### 18-NOV-2014  PIMA_ONOF_PROC  v2.1 (c)  L. Petrov 01-DEC-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  IND_OBS, IUER 
      CHARACTER  STR*128, STR1*128
      REAL*8     TIM_ARR(PIM__MUV), TIM_BEG, SUMVIS_WEISQ
      REAL*8     TIM_GOOD(PIM__MUV), AMP_GOOD(PIM__MUV), &
     &           TIM_BAD(PIM__MUV),  AMP_BAD(PIM__MUV)
      REAL*4     WEI_SUM, WEI_TOT_SUM, AMPL_KER, WRMS_AMPL, AMPL, WRMS_SEG
      REAL*4     EPS
      PARAMETER  ( EPS = 0.0001 ) 
      COMPLEX*8  DRF
      LOGICAL*1  FL_KER(PIM__MUV), FL_LOW(PIM__MUV), FL_NSIG, FL_AMPL
      INTEGER*4  LTIM, J1, J2, J3, J4, J5, J6, J7, NSEG,IND_BND, FRG_IND, &
     &           IND_BEG, UV_IND, IND_END, K, IND_FIRST, IND_LAST, &
     &           FIRST_BAD, LAST_BAD, NGOOD, NBAD, LPOL, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check whether IND_OBS in a range
!
      IF ( IND_OBS < 1 .OR. IND_OBS > PIM%NOBS ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( IND_OBS,  STR  )
           CALL INCH  ( PIM%NOBS, STR1 )
           CALL ERR_LOG ( 9141, IUER, 'PIMA_ONOF_PROC', 'Wrong observation '// &
     &         'index: '//STR(1:I_LEN(STR))//' -- it should be in range '// &
     &         '[1, '//STR1(1:I_LEN(STR1))//']' )
           RETURN 
      END IF
!
! --- Check whether the observation was read and the UV data are available
!
      IF ( .NOT. ASSOCIATED ( PIM%OBS(IND_OBS)%UV      ) .OR. &
     &     .NOT. ASSOCIATED ( PIM%OBS(IND_OBS)%UV_IF   ) .OR. &
     &     .NOT. ASSOCIATED ( PIM%OBS(IND_OBS)%UV_BAND )      ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IND_OBS,  STR  )
           CALL ERR_LOG ( 9142, IUER, 'PIMA_ONOF_PROC', 'Trap of internal '// &
     &         'control: uv data for observation '//STR(1:I_LEN(STR))// &
     &         ' are not processed with PIMA_GET_OBS' )
           RETURN 
      END IF
!
      IND_BND = 1
      FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
      LTIM = PIM%OBS(IND_OBS)%NUM_EPC(FRG_IND)
!
! --- Set the index of the first and last acummulation period for
! --- the kernel of the observation
!
      IND_BEG = 1 + IDNINT((LTIM-1)*PIM%CONF%ONOF_KERNEL_START_SHARE)
      IF ( IND_BEG < 1 ) IND_BEG = 1
      IND_END = 1 + IDNINT((LTIM-1)*PIM%CONF%ONOF_KERNEL_END_SHARE)
      IF ( IND_END > LTIM ) IND_END = LTIM
!
! --- Compute the average amplitude and within the observation kernel
!
      SUMVIS_WEISQ = 0.0D0
      WEI_TOT_SUM  = 0.0
      WEI_SUM = 0.0
      DRF = (0.0, 0.0)
      TIM_BEG = -1.D9
      DO 410 J1=1,LTIM 
         FL_KER(J1) = .FALSE.
         UV_IND = PIM%OBS(IND_OBS)%UV_IND(J1,FRG_IND)
         TIM_ARR(J1) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                 PIM%OBS(IND_OBS)%TIM_BEG
         IF ( J1 .GE. IND_BEG .AND. J1 .LE. IND_END ) THEN
!
! ----------- This AP belong to the kernel. Let us update accumulators
!
              IF ( TIM_BEG < 0.0 ) TIM_BEG = TIM_ARR(J1)
              IF ( TIM_ARR(J1) - TIM_BEG .LE. PIM%CONF%ONOF_COHERENT_INTERVAL ) THEN
                   DRF = DRF + PIM%OBS(IND_OBS)%WEI_1D(J1,1)* &
     &                         PIM%OBS(IND_OBS)%UV_BAND(J1,1)
                   WEI_SUM = WEI_SUM + PIM%OBS(IND_OBS)%WEI_1D(J1,1)
                   FL_KER(J1) = .TRUE.
              END IF
         END IF
         WEI_TOT_SUM = WEI_TOT_SUM + PIM%OBS(IND_OBS)%WEI_1D(J1,1)
 410  CONTINUE 
      IF ( WEI_SUM > PIMA__WEI_MIN ) THEN
           AMPL_KER = ABS(DRF)/WEI_SUM
         ELSE 
           AMPL_KER = 0.0
           IF ( PIM%CONF%DEBUG_LEVEL == 23 ) THEN
                WRITE ( 6, * ) 'PIMA_ONOF_PROC: no valid AP was found for observation ', IND_OBS
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Compute the WRMS of an individual AP over the kernel
!
      WRMS_AMPL = 0.0
      IF ( WEI_SUM > PIMA__WEI_MIN ) THEN
           DO 420 J2=1,LTIM 
              UV_IND = PIM%OBS(IND_OBS)%UV_IND(J2,FRG_IND)
              TIM_ARR(J2) = PIM%TIM_R8(PIM%UV_IND(UV_IND)%TIM_IND) - &
     &                      PIM%OBS(IND_OBS)%TIM_BEG
              IF ( FL_KER(J2) ) THEN
                   WRMS_AMPL = WRMS_AMPL + PIM%OBS(IND_OBS)%WEI_1D(J2,1)* &
     &                                     (ABS(PIM%OBS(IND_OBS)%UV_BAND(J2,1)) - AMPL_KER)**2
              END IF
 420       CONTINUE 
           WRMS_AMPL = SQRT( WRMS_AMPL/WEI_TOT_SUM )
      END IF
!
! --- Compute the number of APs in a segment for computing the trial
! --- amplitude
!
      IF ( AMPL_KER > PIMA__AMP_MIN ) THEN
           NSEG = NINT( ( WRMS_AMPL/AMPL_KER * PIM%CONF%ONOF_NSIG_THRESHOLD )**2 )
         ELSE 
           NSEG = 1
      END IF
      IF ( NSEG < 1 ) NSEG = 1
!
! --- Initialize
!
      FL_LOW = .FALSE.
!
! --- Investigate the start part of the UV array backward
!
      FIRST_BAD = 0
      DO 430 J3=IND_BEG,1,-1
!
! ------ K -- is the first point of the segment, J3 is the last point of the segment
!
         K = J3 - NSEG + 1
         IF ( K < 1 ) K = 1
         IF ( J3-K > 1 ) THEN
              DRF = (0.0, 0.0)
              WEI_SUM = 0.0
!
! ----------- Get the amplitude that is coherently averaged over the segment
! ----------- and its wrms
!
              DO 440 J4=J3,K,-1
                 DRF = DRF + PIM%OBS(IND_OBS)%WEI_1D(J4,1)* &
     &                       PIM%OBS(IND_OBS)%UV_BAND(J4,1)
                 WEI_SUM = WEI_SUM + PIM%OBS(IND_OBS)%WEI_1D(J4,1)
 440          CONTINUE 
              IF ( WEI_SUM > PIMA__WEI_MIN ) THEN
                   AMPL = ABS(DRF)/WEI_SUM
                   WRMS_SEG = WRMS_AMPL/SQRT(WEI_SUM)
                 ELSE 
                   AMPL = 0.0
                   WRMS_SEG = 0.0D0
              END IF
            ELSE 
              AMPL = ABS ( PIM%OBS(IND_OBS)%UV_BAND(J3,1) )
              WRMS_SEG = WRMS_AMPL
         END IF
!
         FL_NSIG = .FALSE.
         FL_AMPL = .FALSE.
         IF ( PIM%CONF%ONOF_NSIG_THRESHOLD > 0.001 ) THEN
!
! ----------- Check whether the ampliude deviates from the kernel average by 
! ----------- more than N-sigma
!
              IF ( AMPL < AMPL_KER - WRMS_SEG*PIM%CONF%ONOF_NSIG_THRESHOLD ) FL_NSIG = .TRUE.
         END IF
         IF ( PIM%CONF%ONOF_AMPL_THRESHOLD*AMPL_KER > EPS .AND. NSEG == 1 ) THEN
!
! ----------- Check whether the ampliude is lower than threshold level
!
              IF ( AMPL < PIM%CONF%ONOF_AMPL_THRESHOLD*AMPL_KER ) FL_AMPL = .TRUE.
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL == 6 .OR. PIM%CONF%DEBUG_LEVEL == 23 ) THEN
              WRITE ( 6, 210 ) J3, TIM_ARR(J3), AMPL_KER, AMPL, WRMS_SEG, FL_NSIG, FL_AMPL
 210          FORMAT ( 'PIMA_ONOF_PROC J3= ', I4, ' Tim= ', F7.2, ' Ampl_ker= ', 1PD9.3, &
     &                 ' Ampl_seg= ', 1PD9.3, ' Wrms_seg= ', 1PD9.3, &
     &                 ' Fl_nsig= ', L1, ' Fl_ampl: ', L1 )
         END IF
!
         IF ( FL_NSIG .OR. FL_AMPL ) THEN
              FL_LOW(J3) = .TRUE.
              IF ( FIRST_BAD == 0 ) THEN
                   FIRST_BAD = J3
                 ELSE IF ( (FIRST_BAD - J3) .GE. PIM%CONF%ONOF_MIN_LOW_AP )  THEN
                   FL_LOW(1:FIRST_BAD) = .TRUE.
                   GOTO 830
              END IF
           ELSE 
              FIRST_BAD = 0
         END IF
 430  CONTINUE 
 830  CONTINUE 
!
! --- Investigate the end part of the UV array forward
!
      LAST_BAD = 0
      DO 450 J5=IND_END,LTIM
!
! ------ J5 -- is the first point of the segment, K is the last point of the segment
!
         K = J5 + NSEG - 1
         IF ( K > LTIM ) K = LTIM
         IF ( K - J5 > 1 ) THEN
              DRF = (0.0, 0.0)
              WEI_SUM = 0.0
!
! ----------- Get the amlitude of coherently averaged over the segment
!
              DO 460 J6=J5,K
                 DRF = DRF + PIM%OBS(IND_OBS)%WEI_1D(J6,1)* &
     &                       PIM%OBS(IND_OBS)%UV_BAND(J6,1)
                 WEI_SUM = WEI_SUM + PIM%OBS(IND_OBS)%WEI_1D(J6,1)
 460          CONTINUE 
              IF ( WEI_SUM > PIMA__WEI_MIN ) THEN
                   AMPL = ABS(DRF)/WEI_SUM
                   WRMS_SEG = WRMS_AMPL/SQRT(WEI_SUM)
                 ELSE 
                   AMPL = 0.0
                   WRMS_SEG = 0.0D0
              END IF
            ELSE 
              AMPL = ABS ( PIM%OBS(IND_OBS)%UV_BAND(J5,1) )
              WRMS_SEG = WRMS_AMPL
         END IF
!
         FL_NSIG = .FALSE.
         FL_AMPL = .FALSE.
         IF ( PIM%CONF%ONOF_NSIG_THRESHOLD > 0.001 ) THEN
!
! ----------- Check whether the ampliude deviates from the kernel average by 
! ----------- more than N-sigma
!
              IF ( AMPL < AMPL_KER - WRMS_SEG*PIM%CONF%ONOF_NSIG_THRESHOLD ) FL_NSIG = .TRUE.
         END IF
         IF ( PIM%CONF%ONOF_AMPL_THRESHOLD*AMPL_KER > EPS .AND. NSEG == 1 ) THEN
!
! ----------- Check whether the ampliude is lower than threshold level
!
              IF ( AMPL < PIM%CONF%ONOF_AMPL_THRESHOLD*AMPL_KER ) FL_AMPL = .TRUE.
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL == 6 .OR. PIM%CONF%DEBUG_LEVEL == 23 ) THEN
              WRITE ( 6, 210 ) J5, TIM_ARR(J5), AMPL_KER, AMPL, WRMS_SEG, FL_NSIG, FL_AMPL
         END IF
         IF ( FL_NSIG .OR. FL_AMPL ) THEN
              FL_LOW(J5) = .TRUE.
              IF ( LAST_BAD == 0 ) THEN
                   LAST_BAD = J5
                ELSE IF ( (J5 - LAST_BAD) .GE. PIM%CONF%ONOF_MIN_LOW_AP )  THEN
                   FL_LOW(LAST_BAD:LTIM) = .TRUE.
                   GOTO 850
              END IF
           ELSE 
              FIRST_BAD = 0
         END IF
 450  CONTINUE 
 850  CONTINUE 
      IF ( PIM%CONF%DEBUG_LEVEL == 6 .OR. PIM%CONF%DEBUG_LEVEL == 23 ) THEN
           WRITE ( 6, 220 ) IND_OBS, IND_BEG, IND_END, LTIM, NSEG, FIRST_BAD, LAST_BAD, &
     &                      AMPL_KER, WRMS_AMPL, NSEG, FL_LOW(1:LTIM)
 220       FORMAT ( 'PIMA_OBOF_PROC:  IND_OBS= ', I5, ' IND_BEG= ', I4, ' IND_END= ', I4, &
     &              ' LTIM= ', I4, ' NSEG= ', I4 / &
     &              'FIRST_BAD: ', I4, ' LAST_BAD: ', I4, ' AMPL_KER= ', 1PD9.3, &
     &              ' WRMS_AMPL = ', 1PD9.3, ' NSEG= ', I4 / &
     &              'Flags: ', 4096L1 )
      END IF
!
! --- Allocate memory for flags, if necessary
!
      IF ( .NOT. ASSOCIATED ( PIM%OBS(IND_OBS)%USER_FLAG ) ) THEN
           ALLOCATE ( PIM%OBS(IND_OBS)%USER_FLAG(LTIM) )
           PIM%OBS(IND_OBS)%USER_FLAG = 1.0
      END IF      
!
      NGOOD = 0
      NBAD  = 0
      DO 470 J7=1,LTIM
         IF ( FL_LOW(J7) ) THEN
!
! ----------- Update the flag
!
              PIM%OBS(IND_OBS)%USER_FLAG(J7) = 0.0
         END IF
!
! ------ Update arrays of good and bad points
!
         IF ( PIM%OBS(IND_OBS)%USER_FLAG(J7) > 0.5 ) THEN
              NGOOD = NGOOD + 1
              TIM_GOOD(NGOOD) = TIM_ARR(J7)
              AMP_GOOD(NGOOD) = PIM%OBS(IND_OBS)%WEI_1D(J7,1)* &
     &                          ABS(PIM%OBS(IND_OBS)%UV_BAND(J7,1))
           ELSE 
              NBAD = NBAD + 1
              TIM_BAD(NBAD) = TIM_ARR(J7)
              AMP_BAD(NBAD) = PIM%OBS(IND_OBS)%WEI_1D(J7,1)* &
     &                            ABS(PIM%OBS(IND_OBS)%UV_BAND(J7,1))
         END IF
 470  CONTINUE 
      IF ( PIM%CONF%DEBUG_LEVEL == 23 ) THEN
!
! -------- Make a plot of amplitudes
!
           CALL CLRCH ( STR )
           CALL INCH  ( IND_OBS, STR )
           STR = 'Fringe amplitude. Obs. #'//STR(1:I_LEN(STR))//' of '// &
     &            PIM%C_SOU(PIM%OBS(IND_OBS)%SOU_IND)//' at '// &
     &            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1))//'/'// &
     &            PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))//' '// &
     &            ' Exp. '//PIM%CONF%SESS_CODE
           CALL DIAGI_SETDEF ( IER, 'DIAGI_IPST', 3 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_ICL2', 3 )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', STR )
           CALL DIAGI_SETDEF ( IER, 'DIAGI_UNIT', 'Time (s)' )
           IF ( NBAD == 0 ) THEN
                CALL DIAGI_1 ( NGOOD, TIM_GOOD, AMP_GOOD, IER )
              ELSE 
                CALL DIAGI_2 ( NGOOD, TIM_GOOD, AMP_GOOD, &
     &                         NBAD,  TIM_BAD,  AMP_BAD,  IER )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   SUBROUTINE  PIMA_ONOF_PROC  !#!#
