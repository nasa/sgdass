      SUBROUTINE PIMA_SPLT_FITSTA ( PIM, VTD, IND_SOU, NSCA_USED, NOBS_IN_USED, &
     &                              NOBS_OUT_USED, OLD_GR_DEL, OLD_PH_RAT, &
     &                              OLD_GR_RAT, OLD_PH_ACC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_SPLT_FITSTA recomputes phase delay rate, group delay, *
! *   and group delay rate from baseline based to station based.         *
! *   Since PIMA performs fringe fitting for each baseline               *
! *   independently, in general, the closure of phase delay rate, group  *
! *   delay, and group delay rate is not zero. PIMA_SPLT_FITSTA uses     *
! *   the input estimates of baseline-dependent phase delay rate, group  *
! *   delay, and group delay rate, computes station based quantities,    *
! *   and then using them it re-computes phase delay rate, group delay,  *
! *   and group delay rate for each observation in such a way that       *
! *   the closure along any closed triangle is to be zero. Such          *
! *   computations are done for all scans of a given source.             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   IND_SOU ( INTEGER*4 ) -- Source index.                             *
! *       VTD ( VTD__TYP  ) -- Object with information related to        *
! *                            package VTD for computed apriori path     *
! *                            delay.                                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     NSCA_USED ( INTEGER*4 ) -- The number of used scans.             *
! *  NOBS_IN_USED ( INTEGER*4 ) -- The number of observations used for   *
! *                                computation of station-dependent      *
! *                                group delay and phase delay rates.    *
! * NOBS_OUT_USED ( INTEGER*4 ) -- The number of observations that will  *
! *                                be used for task splt.                *
! * OLD_GR_DEL    ( REAL*8    ) -- Array of original group delays before *
! *                                transformation. Dimension: PIM%NOBS.  *
! * OLD_PH_RAT    ( REAL*8    ) -- Array of original phase delay rates   *
! *                                before transformation. Dimension:     *
! *                                PIM%NOBS.                             *
! * OLD_GR_RAT    ( REAL*8    ) -- Array of original group delay rates   *
! *                                before transformation. Dimension:     *
! *                                PIM%NOBS.                             *
! * OLD_PH_ACC    ( REAL*8    ) -- Array of original phase accelerations *
! *                                before transformation. Dimension:     *
! *                                PIM%NOBS.                             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
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
! * ### 05-SEP-2011  PIMA_SPLT_FITSTA v6.1 (c) L. Petrov 30-MAY-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      CHARACTER  STR*128, STR1*128
      TYPE     ( PIMA__TYPE  ) :: PIM
      TYPE     ( VTD__TYPE   ) :: VTD
      INTEGER*4  IND_SOU, NOBS_IN_USED, NOBS_OUT_USED, NSCA_USED, IUER
      REAL*8     OLD_GR_DEL(PIM__MOBS), OLD_PH_RAT(PIM__MOBS), &
     &           OLD_GR_RAT(PIM__MOBS), OLD_PH_ACC(PIM__MOBS)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, IS, IND_OBS, IND_SCA, L_SCA, &
     &           LIS_SCA(PIM__MSCA), L_STA, LIS_STA(PIM__MSTA), &
     &           KIS_STA(PIM__MSTA), LIS_OBS(PIM__MOBS), I_REF_STA, &
     &           REF_STA, L_PAR, M_PAR, I_STA, IND_FRA, IND_BND, K_BAS, &
     &           N_SUB, J_BAS, IS1, IS2, NOBS_SCA(PIM__MSCA), &
     &           NOBS_SA, IND_SUB, IND_SUB_STA(2), KUS_OBS, FRG_IND, &
     &           K_OBS, IER
      PARAMETER  ( M_PAR = 3*PIM__MBAS )
      REAL*8     SNR, NOR_MAT((M_PAR*(M_PAR+1))/2), NOR_VEC(M_PAR), &
     &           EST(M_PAR), RC, TIM_SCA, &
     &           WEI_VAL, WEI_SUM, TIM_DIF
      REAL*8,    ALLOCATABLE :: EQU_OBS(:,:), RH(:), WEI(:)
      REAL*8     MAXDEV_GR_DEL, MAXDEV_PH_RAT, MAXDEV_GR_RAT, MAXDEV_PH_ACC, &
     &           RMS_GR_DEL,    RMS_PH_RAT,    RMS_GR_RAT,    RMS_PH_ACC, &
     &           WRMS_PRAT, WRMS_GDEL, WRMS_GRAT, WRMS_ACCL, &
     &           WEI_PRAT,  WEI_GDEL,  WEI_GRAT,  WEI_ACCL
      REAL*8     DEL_APSO(PIM__MBAS), RAT_APSO(PIM__MBAS), ACC_APSO(PIM__MBAS), &
     &           DEL_QUAD_MAX(PIM__MBAS)
      REAL*8     PHR_SCL, GDL_SCL, GRT_SCL, ACC_SCL
      PARAMETER  ( PHR_SCL = 1.D-13  )
      PARAMETER  ( GDL_SCL = 1.D-12  )
      PARAMETER  ( GRT_SCL = 1.D-13  )
      PARAMETER  ( ACC_SCL = 1.D-13  )
      LOGICAL*1  FL_USE_OBS(PIM__MOBS), FL_USE_SCA(PIM__MSCA), FL_GRRAT, &
     &           FL_USE_OBS_INIT(PIM__MOBS), FL_KEEP_FRI
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_LIS, ADC_LIS, IFIND_PL, &
     &                       LTM_DIF, MAX_LIST_I4
      REAL*8,    EXTERNAL :: DP_VV_V
!
      CALL GETENV ( 'PIMAVAR_SPLT_KEEP_FRI', STR )
      IF ( STR == 'YES' .OR. STR == 'yes' ) THEN
           FL_KEEP_FRI = .TRUE.
           WRITE ( 6, * ) 'PIMAVAR_SPLT_KEEP_FRI= '//TRIM(STR)
         ELSE 
           FL_KEEP_FRI = .FALSE.
      END IF
!
      IF ( IND_SOU < 1  .OR.  IND_SOU > PIM%NSOU ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( IND_SOU, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM%NSOU, STR1 )
           CALL ERR_LOG ( 8711, IUER, 'PIMA_SPLT_FITSTA', 'Wrong '// &
     &         'source index '//STR(1:I_LEN(STR))//' -- should be '// &
     &         'in range [1, '//STR1(1:I_LEN(STR1))//' ]' )
           RETURN
      END IF
!
! --- Get the index of the fine fringe fittling algorithm
!
      IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_DRF ) THEN
           IND_FRA = PIMA__DRF
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_LSQ ) THEN
           IND_FRA = PIMA__LSQ
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_MUL ) THEN
           IND_FRA = PIMA__MUL
         ELSE IF ( PIM%CONF%MKDB_FRINGE_ALGORITHM == PIMA__FRA_ADD ) THEN
           IND_FRA = PIMA__ADD
      END IF
      IND_BND = 1
!
! --- Get a list of scans L_SCA/LIS_SCA that observe this source, bypassing
! --- not used observations
!
      L_SCA = 0
      DO 410 J1=1,PIM%CONF%FRIB_NOBS
         IND_OBS = PIM%CONF%FRIB_OBS(J1)
         IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 410 ! Bypass deselected observation
!
! ------ Bypass obervations with wrong index, 
! ------ or non-detections.
! ------ or observations with the SNR below the detection limit,
! ------ or observations without system temperature
!
         IF ( IND_OBS < 1  .OR.  IND_OBS > PIM%NOBS ) GOTO 410
         IF ( PIM%OBS(IND_OBS)%SOU_IND .NE. IND_SOU ) GOTO 410
         SNR = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,1)/PIM%OBS(IND_OBS)%NOISE(1)
         IF ( SNR < PIM%CONF%FRIB_SNR_DETECTION ) GOTO 410
         IF ( PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP) .LE. 0 ) GOTO 410
         IF ( PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP) .LE. 0 ) GOTO 410
!
         IS = ADD_LIS ( PIM__MSCA, L_SCA, LIS_SCA, &
     &                  INT(PIM%OBS(IND_OBS)%SCA_IND,KIND=4), IER )
 410  CONTINUE
!
! --- Allocate memory
!
      IF ( PIM%SUB%STATUS == PIMA__ALLOCATED ) THEN
           IF ( ASSOCIATED ( PIM%SUB%OBS_IND_SUB ) ) DEALLOCATE ( PIM%SUB%OBS_IND_SUB )
           IF ( ASSOCIATED ( PIM%SUB%TIM_SRT     ) ) DEALLOCATE ( PIM%SUB%TIM_SRT     )
      END IF
      CALL NOUT ( SIZEOF(PIM%SUB), PIM%SUB )
!
      ALLOCATE  ( PIM%SUB%OBS_IND_SUB(PIM%NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH ( 4*PIM%NOBS, STR )
           CALL ERR_LOG ( 8712, IUER, 'PIMA_SPLT_FITSTA', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array PIM%SUB%OBS_IND_SUB' )
           RETURN 
      END IF
!
      ALLOCATE  ( PIM%SUB%TIM_SRT(PIM__MSUB,PIM__MSCA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*PIM__MSUB*PIM__MSCA, STR )
           CALL ERR_LOG ( 8713, IUER, 'PIMA_SPLT_FITSTA', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array PIM%SUB%TIM_SRT' )
           RETURN 
      END IF
!
      PIM%SUB%OBS_IND_SUB = 0
      PIM%SUB%TIM_SRT     = 0.0D0
      PIM%SUB%STATUS = PIMA__ALLOCATED
!
! --- Initialization
!
      PIM%SUB%IND_SOU = IND_SOU
      FL_USE_SCA = .FALSE.
      FL_GRRAT   = .TRUE.
      PIM%SUB%OBS_IND_SUB = 0
      PIM%SUB%L_STA = 0
      NOBS_IN_USED  = 0
      NOBS_OUT_USED = 0
      K_BAS     = 0
      L_STA     = 0
      NOBS_SCA  = 0
      NSCA_USED = 0
!
      FL_USE_OBS = .FALSE.
      DO 420 J2=1,L_SCA
         L_STA = 0
         FL_USE_OBS = .FALSE.
         DO 430 J3=1,PIM%SCA(LIS_SCA(J2))%NBAS
!
! --------- This cycle runs over all observations of this scan
!
            IND_OBS = PIM%SCA(LIS_SCA(J2))%OBS_IND(J3)
            SNR = PIM%OBS(IND_OBS)%AMPL(PIMA__DRF,1)/PIM%OBS(IND_OBS)%NOISE(1)
!
! --------- Bypass deselected observations
!
            FL_USE_OBS_INIT(IND_OBS) = PIM%USE_OBS(IND_OBS) 
            IF ( SNR < PIM%CONF%FRIB_SNR_DETECTION ) FL_USE_OBS_INIT(IND_OBS) = .FALSE. 
            IF ( .NOT. PIM%USE_OBS(IND_OBS) ) GOTO 430
!
! --------- Bypass a pathological case when no information about an observation is available
!
            IF ( PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) < -0.5 .AND. &
     &           PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) < -0.5       ) GOTO 430
            IF ( PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) < -0.5 ) THEN
                 FL_GRRAT = .FALSE.
            END IF
!
! --------- Bypass useless observations
!
            IF ( SNR < PIM%CONF%FRIB_SNR_DETECTION ) GOTO 430
            IF ( PIM%OBS(IND_OBS)%SOU_IND .NE. IND_SOU ) GOTO 430
            IF ( PIM%OBS(IND_OBS)%TSYS_IND(1,PIM%CONF%FRQ_GRP) .LE. 0 ) GOTO 430
            IF ( PIM%OBS(IND_OBS)%TSYS_IND(2,PIM%CONF%FRQ_GRP) .LE. 0 ) GOTO 430
            FL_USE_OBS(IND_OBS) = .TRUE.
            NOBS_SCA(J2) = NOBS_SCA(J2) + 1
            NOBS_IN_USED = NOBS_IN_USED + 1
 430     CONTINUE 
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, 205 ) J2, PIM%C_SOU(IND_SOU), NOBS_IN_USED
 205          FORMAT ( ' PIMA_SPLT_FITSTA Scan: ', I5, ' Source: ', A, &
     &                 ' NOBS_IN_USED: ', I4  )
         END IF
         IF ( PIM%CONF%SPLT_STA_BASED .EQ. PIMA__STA_BASED_YES ) THEN
              IF ( NOBS_IN_USED .GE. 3 ) THEN
                   FL_USE_SCA(J2) = .TRUE.
                   NSCA_USED = NSCA_USED + 1
                ELSE IF ( NOBS_IN_USED < 3 ) THEN
!
! ---------------- Discard scans with less than three stations
!
                   FL_USE_SCA(J2) = .FALSE.
                   GOTO 420
              END IF
            ELSE 
              IF ( NOBS_SCA(J2) > 0 ) THEN
                   FL_USE_SCA(J2) = .TRUE.
                   NSCA_USED = NSCA_USED + 1
                 ELSE 
                   GOTO 420
              END IF
         END IF
!
! ------ Split the data into subarrays
!
         CALL ERR_PASS ( IUER, IER )
         CALL PIMA_FIND_SUB ( PIM, J2, FL_USE_OBS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8713, IUER, 'PIMA_SPLT_FITSTA', 'Failure '// &
     &            'to split the observations onto subarrays' )
              RETURN 
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
              WRITE ( 6, 210 ) J2, PIM%L_SUB
 210          FORMAT ( ' PIMA_SPLT_FITSTA Scan: ', I5, ' The total number of subarrays: ', I2 )
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   DO 450 J5=1,PIM%L_SUB
                      WRITE ( 6, '(18X,A,I2)' ) 'Number of stations:  ', PIM%SUB%L_STA(J5)
                      DO 460 J6=1,PIM%SUB%L_STA(J5)
                         WRITE ( 6, 220 ) J5, J6, PIM%SUB%L_STA(J5), &
     &                                    PIM%C_STA(PIM%SUB%LIS_STA(J6,J5)), &
     &                                    PIM%SUB%LIS_STA(J6,J5)
  220                    FORMAT ( 25X,'Subarray ', I2, ' Sta: ', I2, ' ( ', I2, ' )  ', A, 2X, I2 )
  460                 CONTINUE 
                      WRITE ( 6, '(A)' ) ' '
  450              CONTINUE 
              END IF
         END IF
!
! ------ This cycle runs over subarrays of the J2-th scan
!
         DO 470 J7=1,PIM%L_SUB
!
! --------- Less than three stations in a given subarray? Nothing to do.
!
            IF ( PIM%CONF%SPLT_STA_BASED == PIMA__STA_BASED_YES ) THEN
                 IF ( PIM%SUB%L_STA(J7) < 3 ) GOTO 470
               ELSE IF ( PIM%CONF%SPLT_STA_BASED == PIMA__STA_BASED_NO ) THEN
                 IF ( PIM%SUB%L_STA(J7) < 2 ) GOTO 470
            END IF
!
            TIM_SCA = 0.0D0
            WEI_SUM = 0.0D0
            L_STA = 0
            KIS_STA = 0
            KUS_OBS = 0
            DO 480 J8=1,PIM%SCA(LIS_SCA(J2))%NBAS
!
! ------------ This cycle runs over all observations of this scan
!
               IND_OBS = PIM%SCA(LIS_SCA(J2))%OBS_IND(J8)
               IF ( .NOT. FL_USE_OBS(IND_OBS)            ) GOTO 480
               IF ( PIM%SUB%OBS_IND_SUB(IND_OBS) .NE. J7 ) GOTO 480
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 3 ) THEN
                    WRITE ( 6, 230 ) J7, LIS_SCA(J2), PIM%C_SOU(IND_SOU), &
     &                               IND_OBS, PIM%USE_OBS(IND_OBS), &
     &                               PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(1)), &
     &                               PIM%C_STA(PIM%OBS(IND_OBS)%STA_IND(2))
 230                FORMAT ( ' PIMA_SPLT_FITSTA  Sa: ', I2, ' Sca: ', I4, &
     &                       ' Sou: ',A, ' Ind_obs: ', I12, &
     &                       ' Use_obs: ', L1, ' Bas: ', A, ' / ', A )
                    CALL FLUSH ( 6  )
               END IF
!
! ------------ Update list of stations for this subarray
!
               IS1 = ADD_LIS ( PIM__MSTA, L_STA, LIS_STA, &
     &                         INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4), IER )
               IS2 = ADD_LIS ( PIM__MSTA, L_STA, LIS_STA, &
     &                         INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4), IER )
               KIS_STA(IS1) = KIS_STA(IS1) + 1
               KIS_STA(IS2) = KIS_STA(IS2) + 1
!
! ------------ Set the weight
!
               WEI_VAL = 1.D0/SNR
!
! ------------ Update the accumulator for the weighted sub-array epoch
!
               TIM_SCA = TIM_SCA + &
     &                   (PIM%OBS(IND_OBS)%TIM_BEG + PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND))* &
     &                   WEI_VAL
               WEI_SUM = WEI_SUM + WEI_VAL
               KUS_OBS = KUS_OBS + 1
  480       CONTINUE
!
! --------- NB: here I_REF_STA is an index of the reference station
! --------- in array LIS_STA
!
            IF ( L_STA == 0 ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                      WRITE ( 6, 232 ) J2, PIM%C_SOU(IND_SOU)
  232                 FORMAT ( 'No useable observations at scan ',I4, &
     &                         ' of source ', A )
                 END IF
                 GOTO 470
            END IF
            I_REF_STA = MAX_LIST_I4 ( L_STA, KIS_STA ) ! The index in list KIS_STA
            IF ( I_REF_STA < 1          ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE ( 6, '(A)' ) 'PIMA_SPLT_FITSTA: No reference station found :-('
                 END IF 
                 GOTO 470
            END IF
!
            IF ( KIS_STA(I_REF_STA) < 1 ) THEN
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE ( 6, '(A)' ) 'PIMA_SPLT_FITSTA: reference station '// &
     &                                PIM%C_STA(LIS_STA(I_REF_STA))// &
     &                               ' does not have obs :-(' 
                      WRITE ( 6 ,* ) ' KIS_STA= ', INT2(KIS_STA(1:L_STA))
                 END IF
                 GOTO 470
            END IF
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 5 ) THEN
                 WRITE ( 6, 234 ) J7, LIS_SCA(J2), PIM%C_SOU(IND_SOU), L_STA, KUS_OBS
 234             FORMAT ( ' PIMA_SPLT_FITSTA  SA: ', I2, ' Sca: ', I4, &
     &                       ' Sou: ',A, ' Num_sta: ', I2, ' Num_used_obs: ', I3 )
                 CALL FLUSH ( 6  )
            END IF
!
! --------- Sort the station list L_STA/LIS_STA
!
            REF_STA = LIS_STA(I_REF_STA)
            CALL SORT_I ( L_STA, LIS_STA )
            I_REF_STA = IFIND_PL ( L_STA, LIS_STA, REF_STA )
!
! --------- Get TIM_SCA -- scan reference time. 
! --------- Round it to the nearest integer second
!
            TIM_SCA = IDNINT ( TIM_SCA/WEI_SUM )
!
! --------- Set L_PAR -- the number of observations
!
            L_PAR = 3*(L_STA - 1)
!
            ALLOCATE ( EQU_OBS(L_PAR,3*KUS_OBS), RH(3*KUS_OBS), WEI(3*KUS_OBS), STAT=IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 2832, 'PIMA_SPLT_FITSTA', 'Error in attempt to allocate '// &
     &               'dynamic memory for arrays EQU_OBS, RH, WEI' )
                 RETURN 
            END IF 
!
! --------- Initializaton
!
            NOR_MAT = 0.0D0
            NOR_VEC = 0.0D0
            EST     = 0.0D0
            RH      = 0.0D0
            WEI     = 0.0D0
            DEL_APSO     = 0.0D0
            RAT_APSO     = 0.0D0
            ACC_APSO     = 0.0D0
            DEL_QUAD_MAX = 0.0D0
!
! --------- Cycle over used observation of the baseline
!
            K_OBS = 0
            DO 490 J9=1,PIM%SCA(LIS_SCA(J2))%NBAS
               IND_OBS = PIM%SCA(LIS_SCA(J2))%OBS_IND(J9)
               IF ( .NOT. FL_USE_OBS(IND_OBS)            ) GOTO 490
               IF ( PIM%SUB%OBS_IND_SUB(IND_OBS) .NE. J7 ) GOTO 490
!
! ------------ Compute the contribution to delay, delay rate and acceleration
! ------------ due to the differences between the a priori source
! ------------ positions specified in the PIMA control file and the
! ------------ positions used during correlation for observation
!
               CALL ERR_PASS ( IUER, IER )
               CALL PIMA_APSO_COMP ( PIM, VTD, IND_OBS, DEL_APSO(J9), &
     &                               RAT_APSO(J9), ACC_APSO(J9), &
     &                               DEL_QUAD_MAX(J9), IER )
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IND_OBS, STR ) 
                    CALL ERR_LOG ( 8713, IUER, 'PIMA_SPLT_FITSTA', 'Failure '// &
     &                  'to compute delay, delay rate, and delay acceleration '// &
     &                  'due to the differences between the a priori source '// &
     &                  'positions specified in the PIMA control file and the '// &
     &                  'positions used during correlation for observation '// &
     &                   TRIM(STR) )
                     RETURN 
               END IF
!
               PIM%OBS(IND_OBS)%TOT_PHS_GC(IND_FRA,IND_BND) = 0.0D0
               FRG_IND = PIM%OBS(IND_OBS)%REF_FRG_INDS(PIM%CONF%FRQ_GRP)
               TIM_DIF = (TIM_SCA - ( PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND ) + &
     &                                PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND) ) )
               IF ( FL_GRRAT ) THEN
!
! ================= Case when group delay rate was evaluated
!
! ----------------- Cycle over 
! ----------------- 0) phase delay rate
! ----------------- 1) group delay 
! ----------------- 2) group delay rate
!
                    DO 4100 J10=0,2
                       K_OBS = K_OBS + 1
                       EQU_OBS(1:L_PAR,K_OBS) = 0.0D0
                       IF ( J10 == 0 ) THEN
!
! ------------------------- Phase delay rate
!
                            RH(K_OBS) = PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) &
     &                                  + ACC_APSO(J9) * TIM_DIF
                            WEI(K_OBS) = PHR_SCL/PIM%OBS(IND_OBS)%PH_RAT_ERR(IND_FRA,IND_BND)
                          ELSE IF ( J10 == 1 ) THEN
!
! ------------------------- Group delay
!
                            RH(K_OBS) = PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) &
     &                                  + ACC_APSO(J9) * TIM_DIF**2/2.D0
                            WEI(K_OBS) = GDL_SCL/PIM%OBS(IND_OBS)%MB_DEL_ERR(IND_FRA,IND_BND)
                          ELSE IF ( J10 == 2 ) THEN
!
! ------------------------- Group delay rate
!
                            RH(K_OBS) = PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)
                            WEI(K_OBS) = GRT_SCL/PIM%OBS(IND_OBS)%GR_RAT_ERR(IND_BND)
                       END IF
!
! -------------------- find the index of the first station in the array of stations 
! -------------------- of this subarray and update the equation of conditions
!
                       I_STA = IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4) )
                       IF ( I_STA .NE. I_REF_STA ) THEN
                            IF ( I_STA > I_REF_STA ) I_STA = I_STA - 1
                            EQU_OBS(I_STA + J10*(L_STA-1),K_OBS) = -1.0D0
                            IF ( J10 == 1 ) THEN
                                 EQU_OBS(I_STA + 0*(L_STA-1),K_OBS) =  TIM_DIF
                            END IF
                       END IF
!
! -------------------- find the index of the second station in the array of stations 
! -------------------- of this subarray and udpate the equation of conditions
!
                       I_STA = IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4) )
                       IF ( I_STA .NE. I_REF_STA ) THEN
                            IF ( I_STA > I_REF_STA ) I_STA = I_STA - 1
                            EQU_OBS(I_STA + J10*(L_STA-1),K_OBS) = 1.0D0
                            IF ( J10 == 1 ) THEN
                                 EQU_OBS(I_STA + 0*(L_STA-1),K_OBS) = -TIM_DIF
                            END IF
                       END IF
!
! -------------------- Update the normal matrix and normal vector
!
                       CALL DIAD_CVT_S ( WEI(K_OBS)**2, L_PAR, EQU_OBS(1,K_OBS), EQU_OBS(1,K_OBS), NOR_MAT )
                       CALL NORVEC_UPD ( L_PAR, WEI(K_OBS), RH(K_OBS), EQU_OBS(1,K_OBS), NOR_VEC )
 4100               CONTINUE
                  ELSE
!
! ================= Case when phase delay acceleration was evaluated
!
! ----------------- Cycle over 
! ----------------- 0) phase delay rate
! ----------------- 1) group delay 
! ----------------- 2) phase delay acceleration
!
                    DO 4110 J11=0,2
                       K_OBS = K_OBS + 1
                       EQU_OBS(1:L_PAR,K_OBS) = 0.0D0
                       IF ( J11 == 0 ) THEN
                            RH(K_OBS) = PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND)
                            WEI = PHR_SCL/PIM%OBS(IND_OBS)%PH_RAT_ERR(IND_FRA,IND_BND)
                          ELSE IF ( J11 == 1 ) THEN
                                 RH(K_OBS) = PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND)
                            WEI = GDL_SCL/PIM%OBS(IND_OBS)%MB_DEL_ERR(IND_FRA,IND_BND)
                          ELSE IF ( J11 == 2 ) THEN
                            RH(K_OBS) = PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND)
                            WEI = ACC_SCL/PIM%OBS(IND_OBS)%PH_ACC_ERR(IND_BND)
                       END IF
!
! -------------------- find the index of the 1st station in the array of stations 
! -------------------- of this subarray and update the equation of conditions
!
                       I_STA = IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4) )
                       IF ( I_STA .NE. I_REF_STA ) THEN
                            IF ( I_STA > I_REF_STA ) I_STA = I_STA - 1
                            EQU_OBS(I_STA + J11*(L_STA-1),K_OBS) = -1.0D0
                       END IF
!
! -------------------- find the index of the 2nd station in the array of stations 
! -------------------- of this subarray and update the equation of conditions
!
                       I_STA = IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4) )
                       IF ( I_STA .NE. I_REF_STA ) THEN
                            IF ( I_STA > I_REF_STA ) I_STA = I_STA - 1
                            EQU_OBS(I_STA + J11*(L_STA-1),K_OBS) = 1.0D0
                       END IF
!
! -------------------- Update the normal matrix and normal vector
!
                       CALL DIAD_CVT_S ( WEI(K_OBS)**2, L_PAR, EQU_OBS(1,K_OBS), EQU_OBS(1,K_OBS), NOR_MAT )
                       CALL NORVEC_UPD ( L_PAR, WEI(K_OBS), RH(K_OBS), EQU_OBS(1,K_OBS), NOR_VEC )
 4110               CONTINUE
               END IF
 490        CONTINUE
!
! ========= Solve the system of LSQ equations
!
            CALL ERR_PASS ( IUER, IER )
            CALL INVS ( L_PAR, NOR_MAT, RC, IER )
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 WRITE ( 6, * ) 'PIMA_SPLT_FITSTA  RC= ', RC 
            END IF
!
! --------- Find vector of the estimates of the parameters
!
            IER = -1
            CALL MUL_MV_SV_V ( L_PAR, NOR_MAT, L_PAR, NOR_VEC, L_PAR, &
     &                         EST, IER )
            WRMS_PRAT = 0.0
            WRMS_GDEL = 0.0
            WRMS_GRAT = 0.0
            WRMS_ACCL = 0.0
!
            WEI_PRAT = 0.0
            WEI_GDEL = 0.0
            WEI_GRAT = 0.0
            WEI_ACCL = 0.0
            DO 4120 J12=1,K_OBS
               IF ( MOD(J12,3) == 1 )  THEN
                    WRMS_PRAT = WRMS_PRAT + WEI(J12)*( DP_VV_V ( L_PAR, EQU_OBS(1,J12), EST ) - RH(J12) )**2
                    WEI_PRAT  = WEI_PRAT  + WEI(J12)
                  ELSE IF ( MOD(J12,3) == 2 )  THEN
                    WRMS_GDEL = WRMS_GDEL + WEI(J12)*( DP_VV_V ( L_PAR, EQU_OBS(1,J12), EST )  - RH(J12) )**2
                    WEI_GDEL  = WEI_GDEL  + WEI(J12)
                  ELSE IF ( MOD(J12,3) == 0 )  THEN
                    IF ( FL_GRRAT ) THEN
                         WRMS_GRAT = WRMS_GRAT + WEI(J12)*( DP_VV_V ( L_PAR, EQU_OBS(1,J12), EST ) - RH(J12) )**2 
                         WEI_GRAT  = WEI_GRAT  + WEI(J12)
                       ELSE 
                         WRMS_ACCL = WRMS_ACCL + WEI(J12)*( DP_VV_V ( L_PAR, EQU_OBS(1,J12), EST ) - RH(J12) )**2
                         WEI_ACCL  = WEI_ACCL  + WEI(J12)
                    END IF
               END IF
 4120       CONTINUE 
!
            WRMS_PRAT = DSQRT ( WRMS_PRAT/WEI_PRAT )
            WRMS_GDEL = DSQRT ( WRMS_GDEL/WEI_GDEL )
            IF ( FL_GRRAT ) THEN
                 WRMS_GRAT = DSQRT ( WRMS_GRAT/WEI_GRAT )
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                      WRITE  ( 6, 295 ) 1.0D9*WRMS_GDEL,  1.0D12*WRMS_PRAT, &
     &                                  1.0D12*WRMS_GRAT, K_OBS/3
 295                  FORMAT ( 'RMS_gdel: ', F9.3, ' ns ', &
     &                         'RMS_prat: ', F9.3, ' ps/s  RMS_grat: ', F9.3, &
     &                         ' ps/s ', ' Nu_obs: ', I3 )
                 END IF
               ELSE
                 WRMS_ACCL = DSQRT ( WRMS_ACCL/WEI_ACCL )
            ENDIF
!
            IF ( IER .NE. 0 ) THEN
                 WRITE ( 6, * ) ' L_STA= ', L_STA, ' NOBS_IN_USED= ', INT2(NOBS_IN_USED), &
     &                          ' I_REF_STA_IND= ', INT2(I_REF_STA), ' L_PAR= ', INT2(L_PAR), &
     &                          ' FL_GRRAT= ', FL_GRRAT
                 WRITE ( 6, * ) ' LIS_STA= ', INT2(LIS_STA(1:L_STA))
                 DO 4130 J13=1,PIM%SCA(LIS_SCA(J2))%NBAS
                    IND_OBS = PIM%SCA(LIS_SCA(J2))%OBS_IND(J13)
                    IF ( .NOT. FL_USE_OBS(IND_OBS)            ) GOTO 4130
                    IF ( PIM%SUB%OBS_IND_SUB(IND_OBS) .NE. J7 ) GOTO 4130
                    WRITE ( 6, 2110 ) J13, IND_OBS, PIM%OBS(IND_OBS)%STA_IND, &
     &                                IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4) ), &
     &                                IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4) )
 2110               FORMAT ( 'i= ', I4, ' Ind_obs: ', I5, ' Sta_ind: ', I2, 1X, I2, &
     &                       ' Lis_sta_ind: ', I2, 1X, I2 )
 4130            CONTINUE 
                 CALL CLRCH ( STR )
                 CALL INCH  ( LIS_SCA(J2), STR )
                 IF ( PIM%CONF%WARNING ) THEN
                      WRITE ( 6, 240 ) STR(1:I_LEN(STR)), PIM%C_SOU(IND_SOU)
 240                  FORMAT (  'PIMA_SPLT_FITSTA Failure in an attempt to ', &
     &                          ' solve for station based fringe '/ &
     &                          '     fitting parameters for scan ',A,'  source ',A/ &
     &                          '    Nevertheless, continue' )
                      GOTO 470
                 END IF
            END IF
!
! --------- Finally, we make a run for updating phase dalay rate, 
! --------- group delay, and group delay rate
!
            MAXDEV_PH_RAT = 0.0D0
            MAXDEV_GR_DEL = 0.0D0
            MAXDEV_GR_RAT = 0.0D0
            MAXDEV_PH_ACC = 0.0D0
            RMS_PH_RAT = 0.0D0
            RMS_GR_DEL = 0.0D0
            RMS_GR_RAT = 0.0D0
            RMS_PH_ACC = 0.0D0
            J_BAS = 0
!
            NOBS_SA = 0
            DO 4140 J14=1,PIM%SCA(LIS_SCA(J2))%NBAS
               IND_OBS = PIM%SCA(LIS_SCA(J2))%OBS_IND(J14)
               IF ( PIM%CONF%SPLT_STA_BASED == PIMA__STA_BASED_ALL ) THEN
!
! ----------------- In this mode we set all observations that are with
! ----------------- the both stations that participated in computation of
! ----------------- station-based group delay and phase delay rate are 
! ----------------- eligible
!
                    IF ( IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4) ) > 0 .AND. &
     &                   IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4) ) > 0       ) THEN
                         FL_USE_OBS(IND_OBS)  = .TRUE.
                         PIM%SUB%OBS_IND_SUB(IND_OBS) = J7
                         PIM%USE_OBS(IND_OBS) = FL_USE_OBS(IND_OBS)
                       ELSE
                         IF ( PIM%SUB%OBS_IND_SUB(IND_OBS) .NE. J7 ) GOTO 4140
                    END IF
                  ELSE
                    IF ( PIM%SUB%OBS_IND_SUB(IND_OBS) .NE. J7 ) GOTO 4140
               END IF
               IF ( .NOT. FL_USE_OBS(IND_OBS) ) GOTO 4140
               NOBS_OUT_USED = NOBS_OUT_USED + 1
!
               OLD_PH_RAT(IND_OBS) = PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) 
               OLD_GR_DEL(IND_OBS) = PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) 
               IF ( FL_GRRAT ) THEN
                    OLD_GR_RAT(IND_OBS) = PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) 
                  ELSE 
                    OLD_PH_ACC(IND_OBS) = PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND) 
               END IF
!
               TIM_DIF = (TIM_SCA - ( PIM%TIM_R8( PIM%UV_IND(PIM%OBS(IND_OBS)%UV_IND(1,FRG_IND))%TIM_IND ) + &
     &                                PIM%OBS(IND_OBS)%FRT_OFFSET(IND_BND) ) )
!
               PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) = -ACC_APSO(J14) * TIM_DIF
               PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) = -ACC_APSO(J14) * TIM_DIF**2/2.D0
               PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)         = 0.0D0
               PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND)         = 0.0D0
!
               I_STA = IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(1),KIND=4) )
               IF ( I_STA .NE. I_REF_STA ) THEN
                    IF ( I_STA > I_REF_STA ) I_STA = I_STA - 1
!
                    PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) = &
     &                   PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) &
     &                       -EST(I_STA + 0*(L_STA-1))
                    PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) = &
     &                   PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) &
     &                       -EST(I_STA + 1*(L_STA-1)) + 1.0*TIM_DIF*EST(I_STA)
                    IF ( FL_GRRAT ) THEN
                         PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)    = -EST(I_STA + 2*(L_STA-1)) 
                       ELSE 
                         PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND)    = -EST(I_STA + 2*(L_STA-1))
                    END IF
               END IF
!
               I_STA = IFIND_PL ( L_STA, LIS_STA, INT(PIM%OBS(IND_OBS)%STA_IND(2),KIND=4) )
               IF ( I_STA .NE. I_REF_STA ) THEN
                    IF ( I_STA > I_REF_STA ) I_STA = I_STA - 1
!
                    PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) = &
     &                  PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) + &
     &                      EST(I_STA+ 0*(L_STA-1)) 
                    PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) = &
     &                  PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) + &
     &                      EST(I_STA + 1*(L_STA-1)) - 1.0*TIM_DIF*EST(I_STA)
                    IF ( FL_GRRAT ) THEN
                         PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) = &
     &                       PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) + &
     &                           EST(I_STA + 2*(L_STA-1))
                       ELSE 
                         PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND) = &
     &                       PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND) + &
     &                           EST(I_STA + 2*(L_STA-1))
                    END IF
               END IF
!
! ------------ Compute an additional phase that stems from the 
! ------------ inequality of fringe reference time at different baselines
!
               K_BAS = K_BAS + 1
               J_BAS = J_BAS + 1
!
! ------------ ... though as of 2020.05.09, TOT_PHS_GC is not used
!
               PIM%OBS(IND_OBS)%TOT_PHS_GC(IND_FRA,IND_BND) = &
     &               -PI2*PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND)* &
     &                PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%FREQ*TIM_DIF
               IF ( FL_GRRAT ) THEN
                    PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) = &
     &                  PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) - &
     &                  PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)*TIM_DIF
               END IF
               IF ( FL_USE_OBS_INIT(IND_OBS) ) THEN
                    MAXDEV_PH_RAT = MAX ( MAXDEV_PH_RAT, &
     &                                    DABS( PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) - &
     &                                          OLD_PH_RAT(IND_OBS) ) )
                    MAXDEV_GR_DEL = MAX ( MAXDEV_GR_DEL, &
     &                                    DABS(   PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) &
     &                                          - OLD_GR_DEL(IND_OBS) ) )
                    IF ( FL_GRRAT ) THEN
                         MAXDEV_GR_RAT = MAX ( MAXDEV_GR_RAT, &
     &                                         DABS(   PIM%OBS(IND_OBS)%RES_GR_RAT(1) &
     &                                               - OLD_GR_RAT(IND_OBS) ) )
                      ELSE 
                         MAXDEV_PH_ACC = MAX ( MAXDEV_GR_RAT, &
     &                                         DABS(   PIM%OBS(IND_OBS)%RES_PH_ACC(1) &
     &                                               - OLD_PH_ACC(IND_OBS) ) )
                    END IF
!
                    RMS_PH_RAT = RMS_PH_RAT + ( PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND) - &
     &                                          OLD_PH_RAT(IND_OBS) )**2
                    RMS_GR_DEL = RMS_GR_DEL + (   PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND) &
     &                                          - OLD_GR_DEL(IND_OBS) )**2
                    IF ( FL_GRRAT ) THEN
                         RMS_GR_RAT = RMS_GR_RAT + (   PIM%OBS(IND_OBS)%RES_GR_RAT(1) &
     &                                               - OLD_GR_RAT(IND_OBS) )**2
                       ELSE 
                         RMS_PH_ACC = RMS_PH_ACC + (   PIM%OBS(IND_OBS)%RES_PH_ACC(1) &
     &                                               - OLD_PH_ACC(IND_OBS) )**2
                    END IF
               END IF 
               NOBS_SA = NOBS_SA + 1
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                    IF ( FL_USE_OBS_INIT(IND_OBS) ) THEN
                         STR1(1:1) = ' ' 
                       ELSE
                         STR1(1:1) = '@' 
                    END IF
                    WRITE ( 6, 250 ) PIM%C_SOU(IND_SOU), IND_OBS, STR1(1:1),                 &
     &                               1.D9*(   PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,IND_BND)   &
     &                                      - OLD_GR_DEL(IND_OBS) ),                         &
     &                               1.D12*(PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,IND_BND)     &
     &                                      - OLD_PH_RAT(IND_OBS) ), &
     &                               1.D12*(PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND)             &
     &                                       - OLD_GR_RAT(IND_OBS) )
 250                FORMAT ( ' PIMA_SPLT_FITSTA  Sou: ',A, ' Obs: ', I5, 1X, A, 1X, &
     &                       ' Gr_del_diff: ', F10.3, ' ns ',   &
     &                       ' Ph_rat_diff: ', F10.3, ' ps/s ', &
     &                       ' Gr_rat_diff: ', F10.3, ' ps/s ' )
               END IF
 4140       CONTINUE
!
            DO 4150 J15=1,PIM%SCA(LIS_SCA(J2))%NBAS
               IND_OBS = PIM%SCA(LIS_SCA(J2))%OBS_IND(J15)
               IF ( .NOT. FL_USE_OBS(IND_OBS) ) GOTO 4150
               IF ( PIM%SUB%OBS_IND_SUB(IND_OBS) .NE. J7 ) GOTO 4150
               IF ( FL_GRRAT ) THEN
                    PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND) = -1.0D0
                  ELSE 
                    PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) = -1.0D0
               END IF
 4150       CONTINUE 
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 .AND. J_BAS > 0 ) THEN
                 RMS_PH_RAT = DSQRT ( RMS_PH_RAT/J_BAS )
                 RMS_GR_DEL = DSQRT ( RMS_GR_DEL/J_BAS )
                 IF ( FL_GRRAT ) THEN
                      RMS_GR_RAT = DSQRT ( RMS_GR_RAT/J_BAS )
                    ELSE 
                      RMS_PH_ACC = DSQRT ( RMS_PH_ACC/J_BAS )
                      RMS_GR_RAT = 0.0D0
                 END IF
                 WRITE ( 6, 260 ) PIM%C_SOU(IND_SOU), LIS_SCA(J2), J7, NOBS_SA, TIM_SCA
 260             FORMAT ( ' PIMA_SPLT_FITSTA  Sou: ', A, ' Sca: ', I4, &
     &                    '  Suba_ind: ', I2, ' Nobs_Sa: ', I4, ' Tim_sca: ', F8.1, ' s' )
                 WRITE ( 6, 270 ) PIM%C_SOU(IND_SOU), MAXDEV_GR_DEL*1.D9,  RMS_GR_DEL*1.D9
 270             FORMAT ( ' PIMA_SPLT_FITSTA  Sou: ', A, &
     &                    ' MaxDev_Gr_Del: ', F8.3, ' Rms_Gr_Del: ', F8.3, ' ns ' )
                 WRITE ( 6, 280 ) PIM%C_SOU(IND_SOU), MAXDEV_PH_RAT*1.D12, RMS_PH_RAT*1.D12
 280             FORMAT ( ' PIMA_SPLT_FITSTA  Sou: ', A,  &
     &                    ' MaxDev_Ph_Rat: ', F8.3, ' Rms_Ph_Rat: ', F8.3, ' ps/s '  )
                 WRITE ( 6, 290 ) PIM%C_SOU(IND_SOU), MAXDEV_GR_RAT*1.D12, RMS_GR_RAT*1.D12
 290             FORMAT ( ' PIMA_SPLT_FITSTA  Sou: ', A,  &
     &                    ' MaxDev_Gr_Rat: ', F8.3, ' Rms_Gr_Rat: ', F8.3, ' ps/s '  )
                 CALL FLUSH ( 6 )
            END IF
!
! --------- Store scan reference time for the J7-thj subarray for posterity
!
            PIM%SUB%TIM_SRT(J7,LIS_SCA(J2)) =  TIM_SCA
            IF ( ALLOCATED  ( EQU_OBS ) ) THEN
                 DEALLOCATE ( RH      )
                 DEALLOCATE ( WEI     )
                 DEALLOCATE ( EQU_OBS )
            END IF
 470     CONTINUE
!
! ------ Check for internal consistency
!
         DO 4160 J16=1,PIM%NOBS
            IF ( FL_USE_OBS(J16) ) THEN
                 IND_SUB = PIM%SUB%OBS_IND_SUB(J16)
                 IF ( IND_SUB < 1 .OR. IND_SUB > PIM%L_SUB ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J16, STR )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( IND_SUB, STR1 )
                      CALL ERR_LOG ( 8714, IUER, 'PIMA_SPLT_FITSTA', &
     &                    'Trap of internal control: observation '// &
     &                     STR(1:I_LEN(STR))//' is assigned to '// &
     &                    'to subarray with index '//STR1 )
                      RETURN
                 END IF
                 IF ( PIM%SUB%L_STA(IND_SUB) < 2 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IND_SUB, STR )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( PIM%SUB%L_STA(IND_SUB), STR1 )
                      CALL ERR_LOG ( 8715, IUER, 'PIMA_SPLT_FITSTA', &
     &                    'Trap of internal control: subarray '// &
     &                     STR(1:I_LEN(STR))//' has only '// &
     &                     STR1(1:I_LEN(STR1))//' stations' )
                      RETURN
                 END IF
                 IND_SUB_STA(1) = IFIND_PL ( PIM%SUB%L_STA(IND_SUB), &
     &                                       PIM%SUB%LIS_STA(1,IND_SUB), &
     &                                       INT(PIM%OBS(J16)%STA_IND(1),KIND=4) )
                 IND_SUB_STA(2) = IFIND_PL ( PIM%SUB%L_STA(IND_SUB), &
     &                                       PIM%SUB%LIS_STA(1,IND_SUB), &
     &                                       INT(PIM%OBS(J16)%STA_IND(2),KIND=4) )
                 IF ( IND_SUB_STA(1) < 1 .OR. IND_SUB_STA(2) < 1 ) THEN
                      WRITE ( 6,  * ) 'Number of stations in the subarray: ', PIM%SUB%L_STA(IND_SUB)
                      WRITE ( 6,  * ) 'Subarray station list: ', PIM%SUB%LIS_STA(1:PIM%SUB%L_STA(IND_SUB),IND_SUB)
                      CALL CLRCH ( STR )
                      CALL INCH  ( J16, STR )
                      CALL CLRCH ( STR1 )
                      CALL INCH  ( IND_SUB, STR1 )
                      CALL ERR_LOG ( 8716, IUER, 'PIMA_SPLT_FITSTA', &
     &                    'Trap of internal control: observation '// &
     &                     STR(1:I_LEN(STR))//' assigned to '// &
     &                    'to subarray with index '//STR1(1:I_LEN(STR1))// &
     &                    ' that does not have stations that were used'// &
     &                    ' in this observation' )
                      RETURN
                 END IF
                 IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                      WRITE ( 6, 2100 ) J16, IND_SUB, PIM%OBS(J16)%STA_IND(1:2), &
     &                                  IND_SUB_STA(1:2)
 2100                 FORMAT ( 'PIMA_SPLT_FITSTA: Ind_obs: ', I5, ' Ind_sub: ', I2, &
     &                         ' IND_STA: ', I2, 1X, I2, ' IND_SUB_STA: ', I2, 1X, I2 )
                      WRITE ( 6,  * ) 'Subarray station list: ', INT2(PIM%SUB%LIS_STA(1:PIM%SUB%L_STA(IND_SUB),IND_SUB))
                 END IF
            END IF
 4160    CONTINUE 
         IF ( FL_KEEP_FRI ) THEN
              DO 4170 J17=1,PIM%SCA(LIS_SCA(J2))%NBAS
                 IND_OBS = PIM%SCA(LIS_SCA(J2))%OBS_IND(J17)
                 PIM%OBS(IND_OBS)%RES_MB_DEL(IND_FRA,1) = OLD_GR_DEL(IND_OBS)
                 PIM%OBS(IND_OBS)%RES_PH_RAT(IND_FRA,1) = OLD_PH_RAT(IND_OBS)
                 IF ( FL_GRRAT ) THEN
                      PIM%OBS(IND_OBS)%RES_GR_RAT(IND_BND) = OLD_GR_RAT(IND_OBS) 
                   ELSE 
                      PIM%OBS(IND_OBS)%RES_PH_ACC(IND_BND) = OLD_PH_ACC(IND_OBS) 
                 END IF
 4170         CONTINUE 
         END IF
 420  CONTINUE
      IF ( NOBS_IN_USED  == 0 ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, '(A)' ) ' PIMA_SPLT_FITSTA: found no useful '// &
     &                             'oservations of source '//PIM%C_SOU(IND_SOU)
           END IF
           CALL ERR_PASS ( 8714, IUER )
           RETURN 
        ELSE IF ( PIM%CONF%SPLT_STA_BASED .EQ. PIMA__STA_BASED_YES  .AND.  &
     &            K_BAS == 0 ) THEN
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_SCA, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_STA, STR1 )
           WRITE ( 6, * ) ' K_BAS = ', K_BAS
           CALL ERR_LOG ( 8717, IUER, 'PIMA_SPLT_FITSTA', 'Cannot '// &
     &         'update station-based fringe parameters because the '// &
     &         'source '//PIM%C_SOU(IND_SOU)//' was observed in '// &
     &          STR(1:I_LEN(STR))//' scans at disconnected sub-arrays. '// &
     &         'Maximum number of stations in a subarray that observed '// &
     &         'the source: '//STR1 )
           RETURN
      END IF       
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
           WRITE ( 6, '(A)' ) 'PIMA_SPLT_FITSTA: end of work for source '//PIM%C_SOU(IND_SOU)
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SPLT_FITSTA  !#!
