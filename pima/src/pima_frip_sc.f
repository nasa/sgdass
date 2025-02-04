      SUBROUTINE PIMA_FRIP_SC ( PIM, SCA_TYP, MODE, MAP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_FRIP_SC  perfroms the first of model calibration     *
! *   for the current scan of phase referenced source of the type        *
! *   SCA_TYP.
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  SCA_TYP ( INTEGER*4 ) -- Phase referencing scan type PIMA__TAG or   *
! *                           PIMA__CAL.                                 *
! *  MODE    ( INTEGER*4 ) -- Mode of work:                              *
! *                        PIMA__SC_PHS -- model calibration for phase.  *
! *                        PIMA__SC_AMP -- model calibration for         *
! *                                        amplitude.                    *
! *  MAP  ( SOUMAP__TYPE ) -- External map in the form of a set of       *
! *                           Clean components.                          *
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
! * ###  07-JAN-2012  PIMA_FRIP_SC  v1.0 (c)  L. Petrov  10-JAN-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'vtd.i'
      TYPE     ( PIMA__TYPE   ) :: PIM
      TYPE     ( SOUMAP__TYPE ) :: MAP
      INTEGER*4  SCA_TYP, MODE, IUER
      REAL*8,    ALLOCATABLE :: EQU_VEC(:), NOR_MAT(:), NOR_VEC(:), EST_VEC(:)
      REAL*8     OC, SIGMA, SIG_CNS, RC, RES, RES_WRMS, RES_COH_WRMS, &
     &           VIS_WRMS, MOD_WRMS, &
     &           WW, BP, WEI_SUM
      INTEGER*4, ALLOCATABLE :: IND_EQU(:)
      CHARACTER  STR*256
      INTEGER*4  LFRQ, LEQU, LPAR, LPA2, LSTA, IND_OBS, L_STA, &
     &           IND, IND_STA(2), ISTA_ARR(PIM__MSTA), IPAR, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, IER
      COMPLEX*8  AVR_RES, VIS_MAP, VIS_OBS, VIS_EST
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*4,    EXTERNAL :: ATAN_CS_R4, PHAS_CMPL_R4
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, IFIND_PL
!
      LSTA = PIM%FRIP(SCA_TYP)%L_STA
      LFRQ = PIM%CONF%END_FRQ - PIM%CONF%BEG_FRQ + 1
!
! --- Set LPAR -- the number of parameters
! ---     LEQU -- the number of equations
!
      IF ( MODE == PIMA__SC_PHS ) THEN
           LPAR =  (LSTA-1)*LFRQ
           LEQU = 2
         ELSE IF ( MODE == PIMA__SC_AMP ) THEN
           LPAR =  LSTA*LFRQ
           LEQU = 2
      END IF
      LPA2 = (LPAR*(LPAR+1))/2
!
! --- Set reciprocal weights for contraints
!
      SIG_CNS = 1.D3
!
! --- Allocate memory for intermediate parameters needed
! --- for the LSQ solution
!
      ALLOCATE ( IND_EQU(LEQU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9151, IUER, 'PIMA_FRIP_SC', 'Failure in '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic mmeory for IND_EQU array' )
           RETURN
      END IF
!
      ALLOCATE ( EQU_VEC(LEQU), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9152, IUER, 'PIMA_FRIP_SC', 'Failure in '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic mmeory for EQU_VEC array' )
           RETURN
      END IF
!
      ALLOCATE ( NOR_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9153, IUER, 'PIMA_FRIP_SC', 'Failure in '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic mmeory for NOR_VEC array' )
           RETURN
      END IF
!
      ALLOCATE ( NOR_MAT(LPA2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9154, IUER, 'PIMA_FRIP_SC', 'Failure in '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic mmeory for NOR_MAT array' )
           RETURN
      END IF
!
      ALLOCATE ( EST_VEC(LPAR), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9155, IUER, 'PIMA_FRIP_SC', 'Failure in '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic mmeory for EST_VEC array' )
           RETURN
      END IF
!
! --- Build the cross-reference table ISTA_ARR from station index
! --- to the list of stations used in parameter estimation.
! --- NB: during pahse model-calibration the reference station is bypassed
!
      L_STA = 0
      DO 410 J1=1,PIM%FRIP(SCA_TYP)%L_STA
         IF ( MODE == PIMA__SC_PHS  .AND. &
     &        J1 == PIM%FRIP(SCA_TYP)%IND_STA_REF ) GOTO 410
!
         L_STA = L_STA + 1
         ISTA_ARR(L_STA) = J1
 410  CONTINUE
!
! --- Run the least square solution
!
      NOR_VEC = 0.0D0
      NOR_MAT = 0.0D0
      VIS_WRMS = 0.0D0
      WW       = 0.0D0
      DO 420 J2=1,PIM%FRIP(SCA_TYP)%NOBS
!
! ------ Cycle of used observations
!
         IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J2) ) GOTO 420
         IND_OBS = PIM%FRIP(SCA_TYP)%OBS(J2)%IND_OBS
         DO 430 J3=1,LFRQ
            EQU_VEC = 0.0D0
!
! --------- Cycle over used IFs
!
            IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J3,J2) < PIMA__WEI_MIN ) GOTO 430
!
! --------- Get the index of stations that participate in the parameter
! --------- estimation for the baseline reference antenna
!
            IND_STA(1) = IFIND_PL ( L_STA, ISTA_ARR, &
     &                              PIM%FRIP(SCA_TYP)%IND_STA(1,J2) )
            IF ( IND_STA(1) > 0 ) THEN
                 IND_EQU(1) = (IND_STA(1)-1)*LFRQ + J3
                 IF ( MODE == PIMA__SC_PHS ) THEN
                      EQU_VEC(1) = -1.0D0
                    ELSE IF ( MODE == PIMA__SC_AMP ) THEN
                      EQU_VEC(1) =  1.0D0
                 END IF
               ELSE
!
! -------------- This antenna is the phase reference antenna
!
                 IND_EQU(1) = 1
                 EQU_VEC(1) = 0.0D0
            END IF
!
! --------- Get the index of stations that participate in the parameter
! --------- estimation for the baseline remote antenna
!
            IND_STA(2) = IFIND_PL ( L_STA, ISTA_ARR, &
     &                              PIM%FRIP(SCA_TYP)%IND_STA(2,J2) )
            IF ( IND_STA(2) > 0 ) THEN
                 IND_EQU(2) = (IND_STA(2)-1)*LFRQ + J3
                 EQU_VEC(2) = 1.0D0
               ELSE
!
! -------------- This antenna is the phase reference antenna
!
                 IND_EQU(2) = 1
                 EQU_VEC(2) = 0.0D0
            END IF
!
! --------- Compute the visibility for the model map 
! --------- using the clean components
!
            VIS_MAP = (0.0, 0.0)
            DO 440 J4=1,MAP%NUM_CC
               BP = ( PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__UC,J3,J2)*MAP%COOR_CC(1,J4) + &
     &                PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__VC,J3,J2)*MAP%COOR_CC(2,J4) )
               VIS_MAP = VIS_MAP + MAP%FLUX_CC(J4)* &
     &                   CMPLX( COS( SNGL(PI2*BP)), SIN( SNGL(PI2*BP))  )
 440        CONTINUE
!
! --------- Apply apriori gains
!
            VIS_OBS = PIM%FRIP(SCA_TYP)%VIS_AF(J3,J2)* &
     &                CONJG(PIM%FRIP(SCA_TYP)%GAIN(J3,PIM%FRIP(SCA_TYP)%IND_STA(1,J2))) * &
     &                      PIM%FRIP(SCA_TYP)%GAIN(J3,PIM%FRIP(SCA_TYP)%IND_STA(2,J2))
!
! --------- Bypass observations with too low amplitude
!
            IF ( ABS(VIS_OBS) < PIMA__AMP_MIN ) GOTO 430
            IF ( MODE == PIMA__SC_PHS ) THEN
!
! -------------- Phase mode: set reciprocal weight
!
                 IF ( ABS( VIS_OBS ) > PIMA__AMP_MIN ) THEN
                      SIGMA = 1.0D0/ABS( VIS_OBS )
                   ELSE
                      SIGMA = 1.0D15
                 END IF
!
! -------------- Compute the O-C taking into account ambiguties in IF
!
                 OC =   PHAS_CMPL_R4 ( VIS_MAP ) &
                      - PHAS_CMPL_R4 ( VIS_OBS ) &
     &                - PI2*PIM%FRIP(SCA_TYP)%IAMB_AF(J3,J2)
              ELSE IF ( MODE == PIMA__SC_AMP ) THEN
!
! -------------- Amplitude mode: set reciprocal weight
!
                 IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J3,J2) > PIMA__WEI_MIN ) THEN
                      SIGMA = 1.D0/PIM%FRIP(SCA_TYP)%WEI_AF(J3,J2)
                    ELSE
                      SIGMA = 1.D15
                 END IF
!
! -------------- Compute O-C using logarighms of amplitudes
!
                 OC = LOG( ABS( VIS_MAP ) ) - LOG( ABS( VIS_OBS ) )
!
! -------------- Update the wrms of observed visibilities 
!
                 VIS_WRMS = VIS_WRMS + ABS( VIS_OBS )**2/SIGMA**2
!
! -------------- Update the accumulator for squares of reciprocal wegiths 
!
                 WW = WW +  1.0D0/SIGMA**2
            END IF
!
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
!@                 WRITE ( 6, 210 ) J3, J2, &
!@     &                            PIM%FRIP(SCA_TYP)%IND_STA(1,J2), &
!@     &                            PIM%FRIP(SCA_TYP)%IND_STA(2,J2), &
!@     &                            OC, SIGMA, &
!@     &                            PIM%FRIP(SCA_TYP)%IAMB_AF(J3,J2), &
!@     &                            PHAS_CMPL_R4 ( VIS_MAP ), &
!@     &                            ABS ( VIS_MAP ), VIS_AMP
!@ 210             FORMAT ( 'PIMA_FRIP_SC  Ifrq: ', I2, ' Iobs: ', I3, &
!@     &                    ' Sta: ', I2, ' / ', I2, ' Phs: ', 0PF8.5, &
!@     &                    ' Wei: ', 1PD11.4, ' Iamb: ', I3, &
!@     &                    ' Phs_map: ', 0PF8.5, ' Amp_map: ', 0PF7.4, &
!@     &                    ' Amp_vis: ', 0PF7.4, ' UV: ', 0PF12.1, ', ', F12.1 )
!
                 WRITE ( 6, 216 ) J3, J2, &
     &                            PIM%FRIP(SCA_TYP)%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(1,J2)), &
     &                            PIM%FRIP(SCA_TYP)%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(2,J2)), &
     &                            PIM%FRIP(SCA_TYP)%UVW_AF(1:2,J3,J2)
 216             FORMAT ( 'PIMA_FRIP_SC  Ifrq: ', I2, ' Iobs: ', I3, &
     &                    ' Sta: ', A, ' / ', A, ' UV: ', F12.1, ', ', F12.1 )
            END IF
!
! --------- Normal matrix and normal vector update
!
            CALL ADD_TRG ( OC, SIGMA, LEQU, IND_EQU, EQU_VEC, &
     &                     LPAR, NOR_VEC, NOR_MAT )
 430     CONTINUE
 420  CONTINUE
      IF ( MODE == PIMA__SC_AMP ) THEN
!
! -------- Compute the WRMS of visibility ampltides
!
           IF ( WW > PIMA__WEI_MIN ) THEN
                VIS_WRMS = DSQRT ( VIS_WRMS/WW )
              ELSE
                VIS_WRMS = PIMA__WEI_MIN
           END IF
      END IF
!
!!  call matview_2 ( lpar, nor_mat ) ! %%%
!
! --- Impose weak constraint to support a case when one or
! --- more IFs at some stations are missing
!
      IND = 0
      DO 450 J5=1,LPAR
         IND = IND + J5
         NOR_MAT(IND) = NOR_MAT(IND) + 1.D0/SIG_CNS**2
 450  CONTINUE
!
! --- Invert normal matrix
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS     ( LPAR, NOR_MAT, RC, IER )
      IF ( IER > 0 ) THEN
           WRITE ( 6, * ) ' RC = ', RC
           CALL ERR_LOG ( 9156, IUER, 'PIMA_FRIP_SC', 'Error during '// &
     &         'normal matrix inversion' )
           RETURN
      END IF
!
! --- Find vector of the estimates of the parameters
!
      IER=-1
      CALL MUL_MV_SV_V ( LPAR, NOR_MAT, LPAR, NOR_VEC, LPAR, EST_VEC, IER )
!
! --- Collect gains from the estimates
!
      IPAR = 0
      DO 460 J6=1,PIM%FRIP(SCA_TYP)%L_STA
         IF ( MODE == PIMA__SC_PHS .AND. &
     &        J6 == PIM%FRIP(SCA_TYP)%IND_STA_REF ) GOTO 460
         DO 470 J7=1,LFRQ
            IPAR = IPAR + 1
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                 WRITE ( 6, 220 ) J7, J6, IPAR, EST_VEC(IPAR), &
     &                            DSQRT( NOR_MAT((IPAR*(IPAR+1))/2) )
 220             FORMAT ( 'PIMA_FRIP_SC  Ifrq: ', I2, ' Ista: ', I2, &
     &                    ' Ipar: ', I4, &
     &                    ' Est: ', F8.5, ' Sig= ', 1PE12.5 )
            END IF
            IF ( MODE == PIMA__SC_PHS ) THEN
!
! -------------- Phase mode
!
                 PIM%FRIP(SCA_TYP)%GAIN(J7,J6) = &
     &               PIM%FRIP(SCA_TYP)%GAIN(J7,J6)* &
     &               CMPLX ( COS(SNGL(EST_VEC(IPAR))), SIN(SNGL(EST_VEC(IPAR))) )
              ELSE IF ( MODE == PIMA__SC_AMP ) THEN
!
! -------------- Amplitude mode
!
                 PIM%FRIP(SCA_TYP)%GAIN(J7,J6) = &
     &               PIM%FRIP(SCA_TYP)%GAIN(J7,J6)* DEXP ( EST_VEC(IPAR) )
            END IF
 470     CONTINUE
 460  CONTINUE
!
! --- A run for compiting residuals
!
      AVR_RES  = (0.0, 0.0)
      RES_WRMS = 0.0D0
      RES_COH_WRMS = 0.0D0
      MOD_WRMS = 0.0D0
      WW       = 0.0D0
      WEI_SUM  = 0.0
!
      DO 480 J8=1,PIM%FRIP(SCA_TYP)%NOBS
         IF ( .NOT. PIM%FRIP(SCA_TYP)%USED(J8) ) GOTO 480
         IND_OBS = PIM%FRIP(SCA_TYP)%OBS(J8)%IND_OBS
         DO 490 J9=1,LFRQ
            EQU_VEC = 0.0D0
            IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8) < PIMA__WEI_MIN ) GOTO 490
!
! --------- Get the index of stations that participate in the parameter
! --------- estimation for the baseline reference antenna
!
            IND_STA(1) = IFIND_PL ( L_STA, ISTA_ARR, PIM%FRIP(SCA_TYP)%IND_STA(1,J8) )
            IF ( IND_STA(1) > 0 ) THEN
                 IND_EQU(1) = (IND_STA(1)-1)*LFRQ + J9
                 IF ( MODE == PIMA__SC_PHS ) THEN
                      EQU_VEC(1) = -1.0D0
                    ELSE IF ( MODE == PIMA__SC_AMP ) THEN
                      EQU_VEC(1) =  1.0D0
                 END IF
               ELSE
                 IND_EQU(1) = 1
                 EQU_VEC(1) = 0.0D0
            END IF
!
! --------- Get the index of stations that participate in the parameter
! --------- estimation for the baseline remote antenna
!
            IND_STA(2) = IFIND_PL ( L_STA, ISTA_ARR, PIM%FRIP(SCA_TYP)%IND_STA(2,J8) )
            IF ( IND_STA(2) > 0 ) THEN
                 IND_EQU(2) = (IND_STA(2)-1)*LFRQ + J9
                 EQU_VEC(2) = 1.0D0
               ELSE
                 IND_EQU(2) = 1
                 EQU_VEC(2) = 0.0D0
            END IF
            VIS_OBS = PIM%FRIP(SCA_TYP)%VIS_AF(J9,J8)
            IF ( ABS(VIS_OBS) < PIMA__AMP_MIN ) GOTO 490
!
! --------- Compute the visibility for the model map 
! --------- using the clean components
!
            VIS_MAP = CMPLX ( 0.0, 0.0 )
            DO 4100 J10=1,MAP%NUM_CC
               BP = ( PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__U,J9,J8)*MAP%COOR_CC(1,J10) + &
     &                PIM%FRIP(SCA_TYP)%UVW_AF(PIMA__V,J9,J8)*MAP%COOR_CC(2,J10) )
               VIS_MAP = VIS_MAP + MAP%FLUX_CC(J10)* &
     &                   CMPLX( COS( SNGL(PI2*BP)), SIN( SNGL(PI2*BP))  )
 4100       CONTINUE
!
            IF ( MODE == PIMA__SC_PHS ) THEN
!
! -------------- Phase mode: compute reciprocal weights and O-C
!
                 IF ( ABS(VIS_OBS) > PIMA__AMP_MIN ) THEN
                      SIGMA = 1.0D0/ABS(VIS_OBS)
                   ELSE
                      SIGMA = 1.0D15
                 END IF
                 OC =   PHAS_CMPL_R4 ( VIS_MAP ) &
     &                - PHAS_CMPL_R4 ( VIS_OBS ) &
     &                - PI2*PIM%FRIP(SCA_TYP)%IAMB_AF(J9,J8)
                 RES = OC - EQU_VEC(1)*EST_VEC(IND_EQU(1)) &
     &                    - EQU_VEC(2)*EST_VEC(IND_EQU(2))
                 RES_WRMS = RES_WRMS + RES**2/SIGMA**2
                 WW       = WW + 1.0D0/SIGMA**2
              ELSE IF ( MODE == PIMA__SC_AMP ) THEN
!
! -------------- Amplitude mode: compute reciprocal weights and O-C
!
                 IF ( PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8) > PIMA__WEI_MIN ) THEN
                      SIGMA = 1.0D0/PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8)
                    ELSE
                      SIGMA = 1.0D15
                 END IF
                 VIS_EST = PIM%FRIP(SCA_TYP)%VIS_AF(J9,J8)* &
     &                          CONJG(PIM%FRIP(SCA_TYP)%GAIN(J9,IND_STA(1))) * &
     &                                PIM%FRIP(SCA_TYP)%GAIN(J9,IND_STA(2))
                 RES =   LOG( ABS( VIS_MAP ) ) - LOG( ABS( VIS_EST ) )
                 AVR_RES = AVR_RES + ( VIS_MAP - VIS_EST ) / SIGMA
                 WEI_SUM = WEI_SUM + 1.0/SIGMA
!
                 RES_WRMS = RES_WRMS + ( ABS(VIS_EST) - ABS(VIS_MAP) )**2/ SIGMA**2
                 RES_COH_WRMS = RES_COH_WRMS + (ABS(VIS_EST - VIS_MAP))**2/SIGMA**2
                 MOD_WRMS = MOD_WRMS + (ABS( VIS_MAP )/SIGMA)**2
                 WW       = WW + 1.0/SIGMA**2
            END IF
            IF ( PIM%CONF%DEBUG_LEVEL .GE. 16 ) THEN
               IF ( MODE == PIMA__SC_PHS ) THEN
                    WRITE ( 6, 230 ) J9, J8, IND_OBS, &
     &                               PIM%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(1,J8)), &
     &                               PIM%C_STA(PIM%FRIP(SCA_TYP)%IND_STA(2,J8)), &
     &                               RES, PIM%FRIP(SCA_TYP)%WEI_AF(J9,J8), &
     &                                 EQU_VEC(1)*EST_VEC(IND_EQU(1)) &
     &                               + EQU_VEC(2)*EST_VEC(IND_EQU(2)), &
     &                               PHAS_CMPL_R4(PIM%FRIP(SCA_TYP)%VIS_AF(J9,J8)), &
     &                               EQU_VEC(1:2), EST_VEC(IND_EQU(1)), EST_VEC(IND_EQU(2))
 230                FORMAT ( 'PIMA_FRIP_SC  Ifrq: ', I2, ' Lobs: ', I3, &
     &                       ' Ind_obs: ', I5, ' Sta: ', A, ' / ', A, &
     &                       ' Res_phs: ', F8.5, &
     &                       ' Wei: ', 1PD12.5, ' Contr: ', 0pf8.5, ' raw: ', 0pf8.5, &
     &                       ' equ_vec: ', f4.1, 1x, f4.1, &
     &                       ' est_vec: ', 0pf8.5, ' , ', 0pf8.5  )
                  ELSE IF ( MODE == PIMA__SC_AMP ) THEN
                    WRITE ( 6, 240 ) J9, J8, &
     &                         PIM%FRIP(SCA_TYP)%IND_STA(1,J8), &
     &                         PIM%FRIP(SCA_TYP)%IND_STA(2,J8), &
     &                         RES, &
     &                         ABS( PIM%FRIP(SCA_TYP)%VIS_AF(J9,J8) ), &
     &                         ABS( VIS_EST ), &
     &                         ABS( VIS_MAP ), &
     &                         ABS( VIS_MAP ) - ABS( VIS_EST )
 240                FORMAT ( 'PIMA_FRIP_SC  Ifrq: ', I2, ' Iobs: ', I3, &
     &                       ' Sta: ', I2, ' / ', I2, ' Log_res_amp: ', F8.5, &
     &                       ' Raw_vis_amp: ', 0PF7.4, &
     &                       ' Vis_amp_gain: ', 0PF7.4, &
     &                       ' Mod_amp: ', 0PF7.4, &
     &                       ' Res_amp: ', 0PF7.4  )
               END IF
            END IF
 490     CONTINUE
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 16 ) THEN
              CALL CLRCH ( STR )
              WRITE ( 6, '(A)' ) STR(1:186)
         END IF
 480  CONTINUE
!
! --- Compute statistics: coherent (complex) wrms of residuals
! --- and incoherent (scalar) wrms of residuals
!
      IF ( WW > PIMA__WEI_MIN**2 ) THEN
           RES_WRMS = DSQRT ( RES_WRMS/WW )
           RES_COH_WRMS = DSQRT ( RES_COH_WRMS/WW )
           MOD_WRMS = DSQRT ( MOD_WRMS/WW )
         ELSE
           RES_WRMS = PIMA__WEI_MIN
           RES_COH_WRMS = PIMA__WEI_MIN
           MOD_WRMS = PIMA__WEI_MIN
      END IF
      IF ( MODE == PIMA__SC_AMP ) THEN
           IF ( WEI_SUM > PIMA__WEI_MIN ) THEN
                AVR_RES = AVR_RES/WEI_SUM
           END IF
      END IF
!
      DEALLOCATE ( IND_EQU )
      DEALLOCATE ( EQU_VEC )
      DEALLOCATE ( EST_VEC )
      DEALLOCATE ( NOR_VEC )
      DEALLOCATE ( NOR_MAT )
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2  .AND.  MODE == PIMA__SC_PHS ) THEN
           WRITE ( 6, 250 ) PIMA__FRIP_SCATYP(SCA_TYP), &
     &                      PIM%SOU(PIM%SCA(PIM%FRIP(SCA_TYP)%IND_SCA)%SOU_IND)%J2000_NAME, &
     &                      RES_WRMS
 250       FORMAT ( 'PIMA_FRIP_SC  ',A, ' Source ', A, &
     &              ' phs_wrms: ', F7.4, ' rad' )
        ELSE IF ( PIM%CONF%DEBUG_LEVEL .GE. 2  .AND.  MODE == PIMA__SC_AMP ) THEN
           WRITE ( 6, 260 ) PIMA__FRIP_SCATYP(SCA_TYP), &
     &                      PIM%SOU(PIM%SCA(PIM%FRIP(SCA_TYP)%IND_SCA)%SOU_IND)%J2000_NAME, &
     &                      VIS_WRMS, MOD_WRMS, RES_WRMS, RES_COH_WRMS, AVR_RES
 260       FORMAT ( 'PIMA_FRIP_SC  ',A, ' Source ', A, &
     &              ' vis_map_wrms: ', F7.4/ &
     &              14X, 'mod_map_wrms: ', F7.4, &
     &              ' res_wrms: ', F7.4/ &
     &              14X,'res_coh_wrms: ', F7.4, &
     &              ' avr_res: ( ', F7.4, ', ', F7.4, ' )' )
      END IF
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4  ) THEN
           DO 4120 J12=1,LSTA
              DO 4130 J13=1,LFRQ
                 WRITE ( 6, 270 ) J12, PIM%FRIP(SCA_TYP)%C_STA(J12), &
     &                            PIMA__FRIP_SCATYP(SCA_TYP), &
     &                            J13, ABS(PIM%FRIP(SCA_TYP)%GAIN(J13,J12)), &
     &                                 PHAS_CMPL_R4 ( PIM%FRIP(SCA_TYP)%GAIN(J13,J12) )
 270             FORMAT ( 'PIMA_FRIP_SC  Ista: ', I2, 2X, A, 2X, A, 2X, &
     &                    ' Ifrq: ', I2, &
     &                    ' Gain_ampl: ', F7.4, ' Gain_phas: ', F7.4, ' rad' )
 4130         CONTINUE
 4120      CONTINUE
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 7  ) THEN
           WRITE ( 6, 280 ) PIM%FRIP(SCA_TYP)%TIM_EPC, &
     &                      MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%FRIP(SCA_TYP)%TIM_EPC, -2 )
 280       FORMAT ( 'PIMA_FRIP_SC  TIM_EPC: ', F9.2, ' DATE_EPC: ', A )
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_FRIP_SC  !#!
