      FUNCTION REPA_GET_AMBSP ( REP, IND_OBS )
! ************************************************************************
! *                                                                      *
! *   Function REPA_GET_AMBSP returns teh ambigity spacings for the      *
! *   current observable type. The output value 0.0 means that no        *
! *   ambiguity spacing is associated wiht the points of the current     *
! *   observable type, and the operation of ambiguity shigt is not       *
! *   valid.                                                             *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *  IND_OBS ( RECORD    ) -- Index of the observation of interest in    *
! *                           the experiment.                            *
! *                                                                      *
! * ### 09-DEC-2004  REPA_GET_AMBSP  v1.0 (c) L. Petrov  09-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  IND_OBS
      REAL*8     REPA_GET_AMBSP 
!
      IF ( REP%DATYP_I2 == GRPRAT__DTP ) THEN
!
! -------- Group delay and rate
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_GR_X*REP%OBS(IND_OBS)%FRQEFF_GR_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 - &
     &        REP%OBS(IND_OBS)%FRQEFF_GR_S**2   )
         ELSE IF ( REP%DATYP_I2 == PHSRAT__DTP ) THEN
!
! -------- Phase delay and phase delay rate
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_PH_X*REP%OBS(IND_OBS)%FRQEFF_PH_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_PH_X**2 - &
     &        REP%OBS(IND_OBS)%FRQEFF_PH_S**2   )
         ELSE IF ( REP%DATYP_I2 == SNBRAT__DTP ) THEN
!
! -------- Single band delay and rate
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_GR_X*REP%OBS(IND_OBS)%FRQEFF_GR_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 - &
     &        REP%OBS(IND_OBS)%FRQEFF_GR_S**2   )
         ELSE IF ( REP%DATYP_I2 == GRPONL__DTP ) THEN
!
! -------- Group delay only
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_GR_X*REP%OBS(IND_OBS)%FRQEFF_GR_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 - &
     &        REP%OBS(IND_OBS)%FRQEFF_GR_S**2   )
         ELSE IF ( REP%DATYP_I2 == PHSONL__DTP ) THEN
!
! -------- Phase delay only
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_PH_X*REP%OBS(IND_OBS)%FRQEFF_PH_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_PH_X**2 - &
     &        REP%OBS(IND_OBS)%FRQEFF_PH_S**2   )
         ELSE IF ( REP%DATYP_I2 == SNBONL__DTP ) THEN
!
! -------- Single band delay only. NB: strctly speaking these frequencies
! -------- are wrong for this case
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_GR_X*REP%OBS(IND_OBS)%FRQEFF_GR_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 - &
     &        REP%OBS(IND_OBS)%FRQEFF_GR_S**2   )
         ELSE IF ( REP%DATYP_I2 == RATONL__DTP ) THEN
!
! -------- Phase delay rate
!
           REPA_GET_AMBSP = 0.0D0
         ELSE IF ( REP%DATYP_I2 == G_GXS__DTP  ) THEN
!
! -------- Ionosphere-free linear combination of GX and GS observables
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_GR_X*REP%OBS(IND_OBS)%FRQEFF_GR_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 - &
     &        REP%OBS(IND_OBS)%FRQEFF_GR_S**2   )
         ELSE IF ( REP%DATYP_I2 == PX_GXS__DTP ) THEN
!
! -------- Phase delay at X-band only
!
           REPA_GET_AMBSP = REP%OBS(IND_OBS)%SPAMB_PH_X
         ELSE IF ( REP%DATYP_I2 == PS_GXS__DTP ) THEN
!
! -------- Phase delay at S-band only
!
           REPA_GET_AMBSP = REP%OBS(IND_OBS)%SPAMB_PH_S
         ELSE IF ( REP%DATYP_I2 == PX_GX__DTP  ) THEN
!
! -------- Ionosphere-free linear combination of PX and GX observables
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_PH_X*REP%OBS(IND_OBS)%FRQEFF_PH_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 + &
     &        REP%OBS(IND_OBS)%FRQEFF_PH_X**2   )
         ELSE IF ( REP%DATYP_I2 == PX_GS__DTP  ) THEN
!
! -------- Ionosphere-free linear combination of PX and GS observables
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_PH_X*REP%OBS(IND_OBS)%FRQEFF_PH_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_S**2 + &
     &        REP%OBS(IND_OBS)%FRQEFF_PH_X**2   )
         ELSE IF ( REP%DATYP_I2 == PS_GX__DTP  ) THEN
!
! -------- Ionosphere-free linear combination of PS and GX observables
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_PH_S*REP%OBS(IND_OBS)%FRQEFF_PH_S**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_X**2 + &
     &        REP%OBS(IND_OBS)%FRQEFF_PH_S**2   )
         ELSE IF ( REP%DATYP_I2 == PS_GS__DTP  ) THEN
!
! -------- Ionosphere-free linear combination of PS and GS observables
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_PH_S*REP%OBS(IND_OBS)%FRQEFF_PH_S**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_GR_S**2 + &
     &        REP%OBS(IND_OBS)%FRQEFF_PH_S**2   )
         ELSE IF ( REP%DATYP_I2 == P_PXS__DTP  ) THEN
!
! -------- Ionosphere-free linear combination of PX and PS observables
!
           REPA_GET_AMBSP = &
     &        REP%OBS(IND_OBS)%SPAMB_PH_X*REP%OBS(IND_OBS)%FRQEFF_PH_X**2/ &
     &      ( REP%OBS(IND_OBS)%FRQEFF_PH_X**2 - &
     &        REP%OBS(IND_OBS)%FRQEFF_PH_S**2   )
         ELSE IF ( REP%DATYP_I2 == GX__DTP     ) THEN
!
! -------- Group delay at X-band only
!
           REPA_GET_AMBSP = REP%OBS(IND_OBS)%SPAMB_GR_X
         ELSE IF ( REP%DATYP_I2 == GS__DTP     ) THEN
!
! -------- Group delay at S-band only
!
           REPA_GET_AMBSP = REP%OBS(IND_OBS)%SPAMB_GR_S
         ELSE IF ( REP%DATYP_I2 == PX__DTP     ) THEN
!
! -------- Phase delay at X-band only
!
           REPA_GET_AMBSP = REP%OBS(IND_OBS)%SPAMB_PH_X
         ELSE IF ( REP%DATYP_I2 == PS__DTP     ) THEN
!
! -------- Phase delay at S-band only
!
           REPA_GET_AMBSP = REP%OBS(IND_OBS)%SPAMB_PH_S
         ELSE IF ( REP%DATYP_I2 == SNG_X__DTP  ) THEN
!
! -------- Narrow band delay at X-band only
!
           REPA_GET_AMBSP = 0.0D0
         ELSE IF ( REP%DATYP_I2 == SNG_S__DTP  ) THEN
!
! -------- Narrow band delay at S-band only
!
           REPA_GET_AMBSP = 0.0D0
         ELSE IF ( REP%DATYP_I2 == FUSED__DTP  ) THEN
!
! -------- Fused data type
!
           REPA_GET_AMBSP = 1.D0 ! Current version does not support zero ambiguity spasing -- it crashes with NaN
      END IF
!
      RETURN
      END  FUNCTION  REPA_GET_AMBSP
