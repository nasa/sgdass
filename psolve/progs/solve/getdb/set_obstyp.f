      SUBROUTINE SET_OBSTYP ( OBS_TYP )
! ************************************************************************
! *                                                                      *
! *   Routine SET_OBSTYP  sets fields of the object OBS_TYP in           *
! *   accrodance with the solution tye variable IDATYP defined in        *
! *   socom.                                                             *
! *                                                                      *
! *  ### 20-MAR-2007   SET_OBSTYP  v1.1 (c)  L. Petrov  31-DEC-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'gvh.i'
      INCLUDE   'vtd.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      REAL*8     REFFR_X, REFFR_S
      TYPE    ( VTD__OBS_TYPE ) :: OBS_TYP
      REAL*8       FRQ_DIF_LIM
      PARAMETER  ( FRQ_DIF_LIM = 0.1D0 ) ! MHz
!
      IF ( DABS(PHAMI8_S) > 1.D-12 ) THEN
           REFFR_S = 1.D0/PHAMI8_S
         ELSE 
           REFFR_S = FRQ_DIF_LIM 
      END IF
      REFFR_X = FREQ_SKY
!
      IF ( IDATYP == GRPRAT__DTP  .OR. &
     &     IDATYP == SNBRAT__DTP  .OR. &
     &     IDATYP == GRPONL__DTP  .OR. &
     &     IDATYP == G_GXS__DTP   .OR. &
     &     IDATYP == FUSED__DTP        ) THEN
!
           OBS_TYP%PLRZ    = 'RR'     
           OBS_TYP%FRQ_REF(1) = REFFR_S*1.D6
           OBS_TYP%FRQ_REF(2) = REFFR_X*1.D6
           OBS_TYP%N_BND      = 2
           OBS_TYP%DELAY_TYPE  = VTD__MLMH__DTP 
           OBS_TYP%FRQ_ION_EFF(1) = EFFREQ_S*1.D6
           OBS_TYP%FRQ_ION_EFF(2) = EFFREQ*1.D6
           OBS_TYP%STATUS  = VTD__BND 
        ELSE IF ( IDATYP == GS__DTP ) THEN
           OBS_TYP%PLRZ    = 'RR'     
           OBS_TYP%FRQ_REF(1) = REFFR_S*1.D6
           OBS_TYP%FRQ_REF(2) = 0.0D0
           OBS_TYP%N_BND      = 1
           OBS_TYP%DELAY_TYPE  = VTD__ML__DTP 
           OBS_TYP%FRQ_ION_EFF(1) = EFFREQ_S*1.D6
           OBS_TYP%FRQ_ION_EFF(2) = 0.D0
           OBS_TYP%STATUS  = VTD__BND 
        ELSE IF ( IDATYP == GX__DTP ) THEN
           OBS_TYP%PLRZ    = 'RR'     
           OBS_TYP%FRQ_REF(1) = REFFR_X*1.D6
           OBS_TYP%FRQ_REF(2) = 0.D0
           OBS_TYP%N_BND      = 1
           OBS_TYP%DELAY_TYPE  = VTD__ML__DTP 
           OBS_TYP%FRQ_ION_EFF(1) = EFFREQ*1.D6
           OBS_TYP%FRQ_ION_EFF(2) = 0.D0
           OBS_TYP%STATUS  = VTD__BND 
        ELSE IF ( IDATYP == PX__DTP ) THEN
           OBS_TYP%PLRZ    = 'RR'     
           OBS_TYP%FRQ_REF(1) = REFFR_X*1.D6
           OBS_TYP%FRQ_REF(2) = 0.0D0
           OBS_TYP%N_BND      = 1
           OBS_TYP%DELAY_TYPE  = VTD__PH__DTP 
           OBS_TYP%FRQ_ION_EFF(1) = PHEFFREQ*1.D6 
           OBS_TYP%FRQ_ION_EFF(2) = 0.D0
           OBS_TYP%STATUS  = VTD__BND 
        ELSE IF ( IDATYP == PS__DTP ) THEN
           OBS_TYP%PLRZ    = 'RR'     
           OBS_TYP%FRQ_REF(1) = REFFR_S*1.D6
           OBS_TYP%FRQ_REF(2) = 0.D0
           OBS_TYP%N_BND      = 1
           OBS_TYP%DELAY_TYPE  = VTD__PL__DTP 
           OBS_TYP%FRQ_ION_EFF(1) = PHEFFREQ_S*1.D6
           OBS_TYP%FRQ_ION_EFF(2) = 0.D0
           OBS_TYP%STATUS  = VTD__BND 
        ELSE IF ( IDATYP == PX_GS__DTP ) THEN
           OBS_TYP%PLRZ    = 'RR'     
           OBS_TYP%FRQ_REF(1) = REFFR_S*1.D6
           OBS_TYP%FRQ_REF(2) = REFFR_X*1.D6
           OBS_TYP%N_BND      = 2
           OBS_TYP%DELAY_TYPE  = VTD__PHML__DTP 
           OBS_TYP%FRQ_ION_EFF(1) = EFFREQ_S*1.D6  
           OBS_TYP%FRQ_ION_EFF(2) = PHEFFREQ*1.D6
           OBS_TYP%STATUS  = VTD__BND 
        ELSE IF ( IDATYP == PX_GX__DTP ) THEN
           OBS_TYP%PLRZ    = 'RR'     
           OBS_TYP%FRQ_REF(1) = REFFR_X*1.D6
           OBS_TYP%FRQ_REF(2) = REFFR_X*1.D6
           OBS_TYP%N_BND      = 2
           OBS_TYP%DELAY_TYPE  = VTD__PHMH__DTP 
           OBS_TYP%FRQ_ION_EFF(1) = EFFREQ*1.D6 
           OBS_TYP%FRQ_ION_EFF(2) = PHEFFREQ*1.D6
           OBS_TYP%STATUS  = VTD__BND 
        ELSE 
!
! -------- Some default
!
           OBS_TYP%PLRZ    = 'RR'     
           OBS_TYP%FRQ_REF(1) = REFFR_X*1.D6
           OBS_TYP%FRQ_REF(2) = 0.D0
           OBS_TYP%N_BND      = 1
           OBS_TYP%DELAY_TYPE  = VTD__PL__DTP 
           OBS_TYP%FRQ_ION_EFF(1) = EFFREQ*1.D6
           OBS_TYP%FRQ_ION_EFF(2) = 0.D0
           OBS_TYP%STATUS  = VTD__BND 
      END IF
!
      RETURN
      END  SUBROUTINE  SET_OBSTYP  !#!#
