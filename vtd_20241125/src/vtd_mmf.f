      SUBROUTINE VTD_MMF ( VTD, TYP, ISTA, MMF, MMF_RATE, IUER )
! ************************************************************************
! *                                                                      *
! *   Function VTD_MMF
! *                                                                      *
! *  ### 27-SEP-2008    VTD_MMF    v1.0 (c)  L. Petrov  15-OCT-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      REAL*8     MMF, MMF_RATE
      CHARACTER  STR*128
      REAL*8     MMF_ZEN, TIM_ARG, CHEB_COEF(0:MMF__N_POL), &
     &           ARG, ARG_MIN, ARG_MAX, ETA, EPS, DEL_ISA_ZEN 
      INTEGER*4  TYP, ISTA, J1, IUER
      REAL*8,    EXTERNAL :: DEL_ISA, DEL_ISA_DER, CHEB_VAL, CHEB_DER
!
      IF ( TYP .NE. HYD__IND  .AND.  &
     &     TYP .NE. NHY__IND         ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( TYP, STR )
           CALL ERR_LOG ( 2471, IUER, 'VTD_MMF_ZEN', 'Wrong parameter TYP: '// &
     &          STR )
           RETURN 
      END IF
!
      TIM_ARG = ( VTD%MOM%MJD - J2000__MJD)*86400.0D0 + &
     &          ( VTD%MOM%TAI - 43200.0D0 )
      ETA = &
     &      VTD%MMF(ISTA)%COEF(MMF__AVR,MMF__DER_NOR,TYP) + &
     &      VTD%MMF(ISTA)%COEF(MMF__SA_COS, MMF__DER_NOR,TYP)*DCOS(SA_FRQ__TRP*TIM_ARG)  + &
     &      VTD%MMF(ISTA)%COEF(MMF__SA_SIN, MMF__DER_NOR,TYP)*DSIN(SA_FRQ__TRP*TIM_ARG)  + &
     &      VTD%MMF(ISTA)%COEF(MMF__SSA_COS,MMF__DER_NOR,TYP)*DCOS(SSA_FRQ__TRP*TIM_ARG) + &
     &      VTD%MMF(ISTA)%COEF(MMF__SSA_SIN,MMF__DER_NOR,TYP)*DSIN(SSA_FRQ__TRP*TIM_ARG) + &
     &      VTD%MMF(ISTA)%COEF(MMF__STA_COS,MMF__DER_NOR,TYP)*DCOS(STA_FRQ__TRP*TIM_ARG) + &
     &      VTD%MMF(ISTA)%COEF(MMF__STA_SIN,MMF__DER_NOR,TYP)*DSIN(STA_FRQ__TRP*TIM_ARG) + &
     &      VTD%MMF(ISTA)%COEF(MMF__S1_COS, MMF__DER_NOR,TYP)*DCOS(S1_FRQ__TRP*TIM_ARG)  + &
     &      VTD%MMF(ISTA)%COEF(MMF__S1_SIN, MMF__DER_NOR,TYP)*DSIN(S1_FRQ__TRP*TIM_ARG)
!
      EPS = &
     &      VTD%MMF(ISTA)%COEF(MMF__AVR,MMF__DER_EAS,TYP) + &
     &      VTD%MMF(ISTA)%COEF(MMF__SA_COS, MMF__DER_EAS,TYP)*DCOS(SA_FRQ__TRP*TIM_ARG)  + &
     &      VTD%MMF(ISTA)%COEF(MMF__SA_SIN, MMF__DER_EAS,TYP)*DSIN(SA_FRQ__TRP*TIM_ARG)  + &
     &      VTD%MMF(ISTA)%COEF(MMF__SSA_COS,MMF__DER_EAS,TYP)*DCOS(SSA_FRQ__TRP*TIM_ARG) + &
     &      VTD%MMF(ISTA)%COEF(MMF__SSA_SIN,MMF__DER_EAS,TYP)*DSIN(SSA_FRQ__TRP*TIM_ARG) + &
     &      VTD%MMF(ISTA)%COEF(MMF__STA_COS,MMF__DER_EAS,TYP)*DCOS(STA_FRQ__TRP*TIM_ARG) + &
     &      VTD%MMF(ISTA)%COEF(MMF__STA_SIN,MMF__DER_EAS,TYP)*DSIN(STA_FRQ__TRP*TIM_ARG) + &
     &      VTD%MMF(ISTA)%COEF(MMF__S1_COS, MMF__DER_EAS,TYP)*DCOS(S1_FRQ__TRP*TIM_ARG)  + &
     &      VTD%MMF(ISTA)%COEF(MMF__S1_SIN, MMF__DER_EAS,TYP)*DSIN(S1_FRQ__TRP*TIM_ARG)
      DO 410 J1=0,MMF__N_POL
         CHEB_COEF(J1) = VTD%MMF(ISTA)%COEF(MMF__AVR,MMF__CHE0+J1,TYP) + &
     &        VTD%MMF(ISTA)%COEF(MMF__SA_COS, MMF__CHE0+J1,TYP)*DCOS(SA_FRQ__TRP*TIM_ARG)  + &
     &        VTD%MMF(ISTA)%COEF(MMF__SA_SIN, MMF__CHE0+J1,TYP)*DSIN(SA_FRQ__TRP*TIM_ARG)  + &
     &        VTD%MMF(ISTA)%COEF(MMF__SSA_COS,MMF__CHE0+J1,TYP)*DCOS(SSA_FRQ__TRP*TIM_ARG) + &
     &        VTD%MMF(ISTA)%COEF(MMF__SSA_SIN,MMF__CHE0+J1,TYP)*DSIN(SSA_FRQ__TRP*TIM_ARG) + &
     &        VTD%MMF(ISTA)%COEF(MMF__STA_COS,MMF__CHE0+J1,TYP)*DCOS(STA_FRQ__TRP*TIM_ARG) + &
     &        VTD%MMF(ISTA)%COEF(MMF__STA_SIN,MMF__CHE0+J1,TYP)*DSIN(STA_FRQ__TRP*TIM_ARG) + &
     &        VTD%MMF(ISTA)%COEF(MMF__S1_COS, MMF__CHE0+J1,TYP)*DCOS(S1_FRQ__TRP*TIM_ARG)  + &
     &        VTD%MMF(ISTA)%COEF(MMF__S1_SIN, MMF__CHE0+J1,TYP)*DSIN(S1_FRQ__TRP*TIM_ARG)
 410  CONTINUE 
!
      DEL_ISA_ZEN = DEL_ISA(P2I)
      ARG_MIN = DEL_ISA ( VTD%MMF(ISTA)%EL_MAX )/DEL_ISA_ZEN
      ARG_MAX = DEL_ISA ( VTD%MMF(ISTA)%EL_MIN )/DEL_ISA_ZEN
      ARG = DEL_ISA(VTD%STA(ISTA)%ELEV)/DEL_ISA_ZEN
      MMF = DEL_ISA ( VTD%STA(ISTA)%ELEV + &
     &                ETA*DCOS(VTD%STA(ISTA)%AZ) + &
     &                EPS*DSIN(VTD%STA(ISTA)%AZ) )/DEL_ISA_ZEN * &
     &      CHEB_VAL ( MMF__N_POL, ARG_MIN, ARG_MAX, ARG, CHEB_COEF )/ &
     &      CHEB_VAL ( MMF__N_POL, ARG_MIN, ARG_MAX, ARG_MIN, CHEB_COEF )
!@   write ( 6, * ) ' mmf = ', mmf, ' mmf_zen = ', & ! %%%%%%%%%%%%%%%%%%%%%%%%%%
!@     &      cheb_val ( mmf__n_pol, arg_min, arg_max, arg, cheb_coef )/ &  ! %%%
!@     &      cheb_val ( mmf__n_pol, arg_min, arg_max, arg_min, cheb_coef ) ! %%%
      MMF_RATE = DEL_ISA_DER ( VTD%STA(ISTA)%ELEV + &
     &                         ETA*DCOS(VTD%STA(ISTA)%AZ) + &
     &                         EPS*DSIN(VTD%STA(ISTA)%AZ) )/DEL_ISA_ZEN * &
     &      ( CHEB_VAL ( MMF__N_POL, ARG_MIN, ARG_MAX, ARG, CHEB_COEF ) + &
     &        DEL_ISA ( VTD%STA(ISTA)%ELEV + &
     &                  ETA*DCOS(VTD%STA(ISTA)%AZ) + &
     &                  EPS*DSIN(VTD%STA(ISTA)%AZ) )* &
     &        CHEB_DER ( MMF__N_POL, ARG_MIN, ARG_MAX, ARG, CHEB_COEF ) &
     &      )/ &
     &      CHEB_VAL ( MMF__N_POL, ARG_MIN, ARG_MAX, ARG_MIN, CHEB_COEF )* &
     &      VTD%STA(ISTA)%ELEV_DER
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_MMF  !#!  
