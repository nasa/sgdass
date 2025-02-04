      FUNCTION   VTD_MMF_ZEN ( VTD, TYP, ISTA, IUER )
! ************************************************************************
! *                                                                      *
! *   Function VTD_MMF_ZEN 
! *                                                                      *
! *  ### 27-SEP-2008  VTD_MMF_ZEN  v1.0 (c)  L. Petrov  27-SEP-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      REAL*8     VTD_MMF_ZEN 
      CHARACTER  STR*128
      INTEGER*4  TYP, ISTA, IUER
!
      REAL*8     TIM_ARG, CHEB_COEF(0:MMF__N_POL), DEL_ISA_ZEN, ARG_MIN, ARG_MAX
      INTEGER*4  J1
      REAL*8,    EXTERNAL :: CHEB_VAL, DEL_ISA
!
      IF ( TYP .NE. HYD__IND  .AND.  &
     &     TYP .NE. NHY__IND         ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( TYP, STR )
           CALL ERR_LOG ( 2461, IUER, 'VTD_MMF_ZEN', 'Wrong parameter TYP: '// &
     &          STR )
           RETURN 
      END IF
!
      TIM_ARG = ( VTD%MOM%MJD - J2000__MJD)*86400.0D0 + &
     &          ( VTD%MOM%TAI - 43200.0D0 )
!
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
!
      VTD_MMF_ZEN = CHEB_VAL ( MMF__N_POL, ARG_MIN, ARG_MAX, ARG_MIN, CHEB_COEF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  FUNCTION  VTD_MMF_ZEN  !#!  
