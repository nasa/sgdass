      SUBROUTINE REPA_SETFUNC ( REP, SUPMET, ARG_IND, VAL_IND, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_SETFUNC sets the argument and values of the function  *
! *   which is to be plotted. It create dynamic arrays for each baseline *
! *   and each category (good, bad, unrecoverable) and fills these       *
! *   arrays with arguments, values, errors and observation indexes.     *
! *   In addition to that it fills REP with extended names of the        *
! *   argument and the function.                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     SUPMET ( INTEGER*2 ) -- Suppression method.                      *
! *    ARG_IND ( INTEGER*4 ) -- Index of the argument of the function    *
! *                             to be plotted.                           *
! *    VAL_IND ( INTEGER*4 ) -- Index of the vaalue of the function      *
! *                             to be plotted.                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        REP ( RECORD    ) -- Object which keeps internal parameters   *
! *                             for program REPA (REsiduals Plots and    *
! *                             Ambiguities).                            *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 02-DEC-2004  REPA_SETFUNC  v1.2 (c)  L. Petrov  20-MAR-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i' 
      INCLUDE   'solve.i' 
      INCLUDE   'diagi.i'
      INCLUDE   'repa.i' 
      INCLUDE   'cals.i' 
      TYPE     ( REP__TYPE ) :: REP
      TYPE     ( CALS_STRU ) :: CALS
      INTEGER*2  SUPMET
      INTEGER*4  ARG_IND, VAL_IND, IUER
      CHARACTER  STR*32, STR1*32
      INTEGER*4  IND_BAS, IND_PLT, J1, J2, J3, J4, J5, J6, ICAL, ISTA, IER
      LOGICAL*4  FL_USED, FL_RECO
      LOGICAL*4, EXTERNAL :: SUPR_INQ, META_SUPR_INQ
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check validity of the argument index
!
      IF ( ARG_IND .LE. 0  .OR.  ARG_IND .GT. REP__M_ARG ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( ARG_IND, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( REP__M_ARG, STR1 )
           CALL ERR_LOG ( 7761, IUER, 'REPA_SETFUNC', 'Wrong index of '// &
     &         'the argument: '//STR(1:I_LEN(STR))//' -- this is out of '// &
     &         'range [1, '//STR1(1:I_LEN(STR1))//'] ' )
           RETURN 
      END IF
      REP%CNF%ARG_IND = ARG_IND
!
! --- Check validity of the value index
!
      IF ( VAL_IND .LE. 0  .OR.  VAL_IND .GT. REP__M_VAL ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( VAL_IND, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( REP__M_VAL, STR1 )
           CALL ERR_LOG ( 7762, IUER, 'REPA_SETFUNC', 'Wrong index of '// &
     &         'the value: '//STR(1:I_LEN(STR))//' -- this is out of '// &
     &         'range [1, '//STR1(1:I_LEN(STR1))//'] ' )
           RETURN 
      END IF
      REP%CNF%VAL_IND = VAL_IND
!
! --- Initialization. Dealloacate memory if it was previously allocated
!
      IF ( ASSOCIATED ( REP%PLT ) ) THEN
           DO 410 J1=1,REP%N_BAS
              IF ( ASSOCIATED( REP%PLT(J1)%ARG_GOO ) ) THEN
                   DEALLOCATE( REP%PLT(J1)%ARG_GOO )
                   DEALLOCATE( REP%PLT(J1)%VAL_GOO )
                   DEALLOCATE( REP%PLT(J1)%ERR_GOO )
                   DEALLOCATE( REP%PLT(J1)%IND_GOO )
!
                   DEALLOCATE( REP%PLT(J1)%ARG_BAD )
                   DEALLOCATE( REP%PLT(J1)%VAL_BAD )
                   DEALLOCATE( REP%PLT(J1)%ERR_BAD )
                   DEALLOCATE( REP%PLT(J1)%IND_BAD )
!
                   DEALLOCATE( REP%PLT(J1)%ARG_UNR )
                   DEALLOCATE( REP%PLT(J1)%VAL_UNR )
                   DEALLOCATE( REP%PLT(J1)%ERR_UNR )
                   DEALLOCATE( REP%PLT(J1)%IND_UNR )
!                 
                   REP%PLT(J1)%N_GOO = 0
                   REP%PLT(J1)%N_BAD = 0
                   REP%PLT(J1)%N_UNR = 0
              END IF
 410       CONTINUE 
         ELSE 
!
! -------- Allocate memory for pointers in PLT
!
           ALLOCATE ( REP%PLT(REP%N_BAS) )
      END IF
!
! --- Allocate memory for arrays with informatio about points of each
! --- category at baseline per baseline basis.
!
      DO 420 J2=1,REP%N_BAS
         ALLOCATE ( REP%PLT(J2)%ARG_GOO ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%VAL_GOO ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%ERR_GOO ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%IND_GOO ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
!        
         ALLOCATE ( REP%PLT(J2)%ARG_BAD ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%VAL_BAD ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%ERR_BAD ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%IND_BAD ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
!        
         ALLOCATE ( REP%PLT(J2)%ARG_UNR ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%VAL_UNR ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%ERR_UNR ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
         ALLOCATE ( REP%PLT(J2)%IND_UNR ( MAX(1,REP%LIS%KA_BAS(J2)) ) )
!
         REP%PLT(J2)%N_GOO = 0
         REP%PLT(J2)%N_BAD = 0
         REP%PLT(J2)%N_UNR = 0
 420  CONTINUE 
!
! --- Allocate memory for the command stack
!
      IF ( .NOT. ASSOCIATED ( REP%COM ) ) THEN
           ALLOCATE ( REP%COM(REPA__M_COM) )
      END IF
!
! --- Fill arrays with arguments, values and indexes
!
      DO 430 J3=1,REP%N_OBS
         IND_BAS = REP%OBS(J3)%IND_BAS
         IF ( SUPMET == SUPMET__META ) THEN
              FL_USED = META_SUPR_INQ ( REP%OBS(J3)%AUTO_SUP, &
     &                                  REP%OBS(J3)%USER_SUP, &
     &                                  REP%OBS(J3)%USER_REC, USED__SPS ) 
              FL_RECO = META_SUPR_INQ ( REP%OBS(J3)%AUTO_SUP, &
     &                                  REP%OBS(J3)%USER_SUP, &
     &                                  REP%OBS(J3)%USER_REC, RECO__SPS ) 
            ELSE 
              FL_USED = SUPR_INQ ( REP%OBS(J3)%SUPSTAT, REP%OBS(J3)%UACSUP, USED__SPS ) 
              FL_RECO = SUPR_INQ ( REP%OBS(J3)%SUPSTAT, REP%OBS(J3)%UACSUP, RECO__SPS ) 
         END IF
         IF ( FL_USED ) THEN
!
! ----------- This is a good observation
!
              REP%PLT(IND_BAS)%N_GOO =  REP%PLT(IND_BAS)%N_GOO + 1
              IND_PLT = REP%PLT(IND_BAS)%N_GOO 
              IF ( REP__CH_ARG(ARG_IND) .EQ. 'Time    ' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%TIM_SEC
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Del_Err ' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%RES(J3)%ERR_DEL
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Delay   ' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%RES(J3)%TAU_C
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Rate    ' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%RES(J3)%RATE_C
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Elev_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%EL(1)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Elev_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%EL(2)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Azim_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%AZ(1)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Azim_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%AZ(2)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Feed_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%FEED_ANG(1)
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Feed_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%FEED_ANG(2)
!!   write  ( 6,  * ) ' j3 = ',j3, ' feed= ', REP%OBS(J3)%FEED_ANG(2); call pause ( 'repad_setfunc 180' ) ! %%%%
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Temp_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%AIR_TEMP(1) - 273.15D0
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Temp_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_GOO(IND_PLT) = REP%OBS(J3)%AIR_TEMP(2) - 273.15D0
              END IF
              IF ( REP__CH_VAL(VAL_IND) .EQ. 'Delay   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%RES(J3)%RES_DEL
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = REP%RES(J3)%ERR_DEL
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Rate    ' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%RES(J3)%RES_RAT
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = REP%RES(J3)%ERR_RAT
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SNR_X   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%SNR_X
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SNR_S   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%SNR_S
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Temp_St1' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%AIR_TEMP(1) - 273.15D0
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Temp_St2' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%AIR_TEMP(2) - 273.15D0
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Pres_St1' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%AIR_PRES(1)
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Pres_St2' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%AIR_PRES(2)
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrIon_Dl' ) THEN
                   CALL REPA_GET_IONO ( 'GrIon_Dl', J3, REP, &
     &                                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT), &
     &                                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT)  )
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'PhIon_Dl' ) THEN
                   CALL REPA_GET_IONO ( 'PhIon_Dl', J3, REP, &
     &                                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT), &
     &                                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT)  )
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Gx' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%SPAMB_GR_X
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Gs' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%SPAMB_GR_S
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Px' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%SPAMB_PH_X
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Ps' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%SPAMB_PH_S
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrAmb_Gx' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%NAMB_GR_X* &
     &                                                 REP%OBS(J3)%SPAMB_GR_X
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-12
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrAmb_Ps' ) THEN
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%NAMB_GR_S* &
     &                                                 REP%OBS(J3)%SPAMB_GR_S
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-12
                 ELSE IF ( REP__CH_VAL(VAL_IND)(1:3) .EQ. 'Cal' .AND. &
     &                     REP__CH_VAL(VAL_IND)(5:7) .EQ. '_St'       ) THEN
                   CALL CHIN ( REP__CH_VAL(VAL_IND)(4:4), ICAL )
                   CALL CHIN ( REP__CH_VAL(VAL_IND)(8:8), ISTA )
                   REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%SCAL(ICAL,ISTA)
                   REP%PLT(IND_BAS)%ERR_GOO(IND_PLT) = 1.D-15
                   IF ( ICAL == 2 ) THEN
                        REP%PLT(IND_BAS)%VAL_GOO(IND_PLT) = REP%OBS(J3)%TAU_GR_X - REP%OBS(J3)%TAU_SB_X
                   END IF
              END IF
              REP%PLT(IND_BAS)%IND_GOO(IND_PLT) = J3
           ELSE IF ( .NOT. FL_USED .AND.  FL_RECO ) THEN
!
! ----------- This is a conditionally bad observation
!
              REP%PLT(IND_BAS)%N_BAD =  REP%PLT(IND_BAS)%N_BAD + 1
              IND_PLT = REP%PLT(IND_BAS)%N_BAD 
              IF ( REP__CH_ARG(ARG_IND) .EQ. 'Time    ' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%TIM_SEC
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Del_Err ' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%RES(J3)%ERR_DEL
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Delay   ' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%RES(J3)%TAU_C
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Rate    ' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%RES(J3)%RATE_C 
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Elev_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%EL(1)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Elev_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%EL(2)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Azim_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%AZ(1)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Azim_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%AZ(2)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Feed_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%FEED_ANG(1)
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Feed_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%FEED_ANG(2)
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Temp_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%AIR_TEMP(1) - 273.15D0
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Temp_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_BAD(IND_PLT) = REP%OBS(J3)%AIR_TEMP(2) - 273.15D0
              END IF
              IF ( REP__CH_VAL(VAL_IND) .EQ. 'Delay   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%RES(J3)%RES_DEL
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = REP%RES(J3)%ERR_DEL
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Rate    ' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%RES(J3)%RES_RAT
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = REP%RES(J3)%ERR_RAT
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SNR_X   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%SNR_X
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SNR_S   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%SNR_S
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Temp_St1' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%AIR_TEMP(1) - 273.15D0
!@                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%EL(1)/DEG__TO__RAD
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Temp_St2' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%AIR_TEMP(2) - 273.15D0
!@                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%EL(2)/DEG__TO__RAD
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Pres_St1' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%AIR_PRES(1)
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Pres_St2' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%AIR_PRES(2)
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrIon_Dl' ) THEN
                   CALL REPA_GET_IONO ( 'GrIon_Dl', J3, REP, &
     &                                  REP%PLT(IND_BAS)%VAL_BAD(IND_PLT), &
     &                                  REP%PLT(IND_BAS)%ERR_BAD(IND_PLT)  )
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'PhIon_Dl' ) THEN
                   CALL REPA_GET_IONO ( 'PhIon_Dl', J3, REP, &
     &                                  REP%PLT(IND_BAS)%VAL_BAD(IND_PLT), &
     &                                  REP%PLT(IND_BAS)%ERR_BAD(IND_PLT)  )
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Gx' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%SPAMB_GR_X
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Gs' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%SPAMB_GR_S
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Px' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%SPAMB_PH_X
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Ps' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%SPAMB_PH_S
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrAmb_Gx' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%NAMB_GR_X* &
     &                                                 REP%OBS(J3)%SPAMB_GR_X
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrAmb_Gs' ) THEN
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%NAMB_GR_S* &
     &                                                 REP%OBS(J3)%SPAMB_GR_S
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND)(1:3) .EQ. 'Cal' .AND. &
     &                     REP__CH_VAL(VAL_IND)(5:7) .EQ. '_St'       ) THEN
                   CALL CHIN ( REP__CH_VAL(VAL_IND)(4:4), ICAL )
                   CALL CHIN ( REP__CH_VAL(VAL_IND)(8:8), ISTA )
                   REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%SCAL(ICAL,ISTA)
                   REP%PLT(IND_BAS)%ERR_BAD(IND_PLT) = 1.D-15
                   IF ( ICAL == 2 ) THEN
                        REP%PLT(IND_BAS)%VAL_BAD(IND_PLT) = REP%OBS(J3)%TAU_GR_X - REP%OBS(J3)%TAU_SB_X
                   END IF
              END IF
              REP%PLT(IND_BAS)%IND_BAD(IND_PLT) = J3
            ELSE 
!
! ----------- This is an unrecoverable observation
!
              REP%PLT(IND_BAS)%N_UNR =  REP%PLT(IND_BAS)%N_UNR + 1
              IND_PLT = REP%PLT(IND_BAS)%N_UNR 
              IF ( REP__CH_ARG(ARG_IND) .EQ. 'Time    ' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%TIM_SEC
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Del_Err ' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%RES(J3)%ERR_DEL
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Delay   ' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%RES(J3)%TAU_C
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Rate    ' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%RES(J3)%RATE_C
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Elev_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%EL(1)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Elev_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%EL(2)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Azim_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%AZ(1)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Azim_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%AZ(2)/DEG__TO__RAD
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Feed_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%FEED_ANG(1)
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Feed_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%FEED_ANG(2)
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Temp_St1' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%AIR_TEMP(1) - 273.15D0
                 ELSE IF ( REP__CH_ARG(ARG_IND) .EQ. 'Temp_St2' ) THEN
                   REP%PLT(IND_BAS)%ARG_UNR(IND_PLT) = REP%OBS(J3)%AIR_TEMP(2) - 273.15D0
              END IF
              IF ( REP__CH_VAL(VAL_IND) .EQ. 'Delay   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%RES(J3)%RES_DEL
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = REP%RES(J3)%ERR_DEL
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Rate    ' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%RES(J3)%RES_RAT
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = REP%RES(J3)%ERR_RAT
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SNR_X   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%SNR_X
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SNR_S   ' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%SNR_S
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Temp_St1' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%AIR_TEMP(1) - 273.15D0
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Temp_St2' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%AIR_TEMP(2) - 273.15D0
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Pres_St1' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%AIR_PRES(1)
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'Pres_St2' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%AIR_PRES(2)
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 0.01
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrIon_Dl' ) THEN
                   CALL REPA_GET_IONO ( 'GrIon_Dl', J3, REP, &
     &                                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT), &
     &                                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT)  )
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'PhIon_Dl' ) THEN
                   CALL REPA_GET_IONO ( 'PhIon_Dl', J3, REP, &
     &                                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT), &
     &                                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT)  )
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Gx' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%SPAMB_GR_X
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Gs' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%SPAMB_GR_S
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Px' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%SPAMB_PH_X
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'SpAmb_Ps' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%SPAMB_PH_S
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-15
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrAmb_Gx' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%NAMB_GR_X* &
     &                                                 REP%OBS(J3)%SPAMB_GR_X 
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-12
                 ELSE IF ( REP__CH_VAL(VAL_IND) .EQ. 'GrAmb_Gs' ) THEN
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%NAMB_GR_S* &
     &                                                 REP%OBS(J3)%SPAMB_GR_S 
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-12
                 ELSE IF ( REP__CH_VAL(VAL_IND)(1:3) .EQ. 'Cal' .AND. &
     &                     REP__CH_VAL(VAL_IND)(5:7) .EQ. '_St'       ) THEN
                   CALL CHIN ( REP__CH_VAL(VAL_IND)(4:4), ICAL )
                   CALL CHIN ( REP__CH_VAL(VAL_IND)(8:8), ISTA )
                   REP%PLT(IND_BAS)%VAL_UNR(IND_PLT) = REP%OBS(J3)%SCAL(ICAL,ISTA)
                   REP%PLT(IND_BAS)%ERR_UNR(IND_PLT) = 1.D-15
              END IF
              REP%PLT(IND_BAS)%IND_UNR(IND_PLT) = J3
         END IF
 430  CONTINUE 
!
! --- Sorting plotting arrays 
!
      IF ( .NOT. ASSOCIATED ( REP%DIAGI ) ) ALLOCATE ( REP%DIAGI(REP%N_BAS) )
      DO 440 J4=1,REP%N_BAS
         CALL SORT83I ( REP%PLT(J4)%N_GOO,   REP%PLT(J4)%ARG_GOO, &
     &                  REP%PLT(J4)%VAL_GOO, REP%PLT(J4)%ERR_GOO, &
     &                  REP%PLT(J4)%IND_GOO )
         CALL SORT83I ( REP%PLT(J4)%N_BAD,   REP%PLT(J4)%ARG_BAD, &
     &                  REP%PLT(J4)%VAL_BAD, REP%PLT(J4)%ERR_BAD, &
     &                  REP%PLT(J4)%IND_BAD )
         CALL SORT83I ( REP%PLT(J4)%N_UNR,   REP%PLT(J4)%ARG_UNR, &
     &                  REP%PLT(J4)%VAL_UNR, REP%PLT(J4)%ERR_UNR, &
     &                  REP%PLT(J4)%IND_UNR )
!
! ------ Nullify the DiaGi data structure
!
         CALL NOUT ( SIZEOF(REP%DIAGI(J4)), REP%DIAGI(J4) )
         REP%DIAGI(J4)%STATUS = DIA__UND 
 440  CONTINUE 
!
! --- Store extended name of each argument type
!
      DO 450 J5=1,REP__M_ARG
         CALL CLRCH ( REP%CH_ARG(J5) )
         IF ( REP__CH_ARG(J5) .EQ. 'Time    ' ) THEN
              REP%CH_ARG(J5) = 'Time'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Del_Err ' ) THEN
              REP%CH_ARG(J5) = 'Delay Error'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Delay   ' ) THEN
              REP%CH_ARG(J5) = 'Total delay'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Rate    ' ) THEN
              REP%CH_ARG(J5) = 'Total rate '
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Elev_St1' ) THEN
              REP%CH_ARG(J5) = 'Elevation angle station 1'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Elev_St2' ) THEN
              REP%CH_ARG(J5) = 'Elevation angle station 2'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Azim_St1' ) THEN
              REP%CH_ARG(J5) = 'Azimuth at station 1'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Azim_St2' ) THEN
              REP%CH_ARG(J5) = 'Azimuth at station 2'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Feed_St1' ) THEN
              REP%CH_ARG(J5) = 'Feed angle at station 1'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Feed_St2' ) THEN
              REP%CH_ARG(J5) = 'Feed angle at station 2'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Temp_St1' ) THEN
              REP%CH_ARG(J5) = 'Air temperature at station 1'
            ELSE IF ( REP__CH_ARG(J5) .EQ. 'Temp_St2' ) THEN
              REP%CH_ARG(J5) = 'Air temperature at station 2'
         END IF
 450  CONTINUE 
!
! --- Read NAMFIL file and store information from there in CALS
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R ( INT2(1), 0, 1, CALS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7763, IUER, 'REPA_SETFUNC', 'Error in an attempt '// &
     &         'to extract information from NAMFIL' ) 
           RETURN 
      END IF
!
! --- Store extended name of each value type
!
      DO 460 J6=1,REP__M_VAL
         CALL CLRCH ( REP%CH_VAL(J6) )
         IF ( REP__CH_VAL(J6) .EQ. 'Delay   ' ) THEN
              REP%CH_VAL(J6) = 'Delay'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'Rate    ' ) THEN
              REP%CH_VAL(J6) = 'Phase delay rate'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'SNR_X   ' ) THEN
              REP%CH_VAL(J6) = 'SNR at X-band'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'SNR_S   ' ) THEN
              REP%CH_VAL(J6) = 'SNR at S-band'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'Temp_St1' ) THEN
              REP%CH_VAL(J6) = 'Air temperature at station 1'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'Temp_St2' ) THEN
              REP%CH_VAL(J6) = 'Air temperature at station 2'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'Pres_St1' ) THEN
              REP%CH_VAL(J6) = 'Air pressure at station 1'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'Pres_St2' ) THEN
              REP%CH_VAL(J6) = 'Air pressure at station 2'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'GrIon_Dl' ) THEN
              REP%CH_VAL(J6) = 'Group ionospheric delay'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'PhIon_Dl' ) THEN
              REP%CH_VAL(J6) = 'Phase ionospheric delay'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'SpAmb_Gx' ) THEN
              REP%CH_VAL(J6) = 'X-band group delay amb. spacing'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'SpAmb_Gs' ) THEN
              REP%CH_VAL(J6) = 'S-band group delay amb. spacing'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'GrAmb_Gx' ) THEN
              REP%CH_VAL(J6) = 'X-band group delay ambiguity'
            ELSE IF ( REP__CH_VAL(J6) .EQ. 'GrAmb_Gs' ) THEN
              REP%CH_VAL(J6) = 'S-band group delay ambiguity'
            ELSE IF ( REP__CH_VAL(J6)(1:3) == 'Cal' .OR. &
     &                REP__CH_VAL(J6)(5:7) == '_St'      ) THEN
!
! ----------- Special case of station calibration: 
! ----------- extract the name of the calibration and station index
!
              CALL CHIN ( REP__CH_VAL(J6)(4:4), ICAL )
              CALL CHIN ( REP__CH_VAL(J6)(8:8), ISTA )
              IF ( ILEN(CALS%SCAL(ICAL)) .GT. 0 ) THEN
                   REP%CH_VAL(J6) = CALS%SCAL(ICAL)(1:I_LEN(CALS%SCAL(ICAL)))// &
     &                              ' station '//REP__CH_VAL(J6)(8:8)
                 ELSE 
                   REP%CH_VAL(J6) = 'Calibration #'//REP__CH_VAL(J6)(4:4)// &
     &                              ' station '//REP__CH_VAL(J6)(8:8)
              END IF
         END IF
 460  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  REPA_SETFUNC 
