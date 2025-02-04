      SUBROUTINE GET_PLANET_COO ( PLA_NAM, VTD, MJD_OBS, TAI_OBS, &
     &                            PLA_COO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_PLANET_COO
! *                                                                      *
! *  ### 27-NOV-2012 GET_PLANET_COO v1.0 (c)  L. Petrov 27-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  PLA_NAM*(*)
      INTEGER*4  MJD_OBS, IUER
      REAL*8     TAI_OBS, PLA_COO(3)
      REAL*8     COO_PLA(3), VEL_PLA(3), &
     &           ACC_PLA(3),  COO_EA(3), VEL_EA(3), ACC_EA(3), &
     &           COO_CFS(3), COO_BRS(3), VEL_BRS(3), DIST_PLA, &
     &           RD, ALPHA, DELTA, S_APP(3), SV, COO_TRS(3), &
     &           STA_POS_CRS(3), STA_VEL_CRS(3)
      CHARACTER  DATE_OBS*24
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      INTEGER*4  IND_PLA, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF 
      REAL*8,    EXTERNAL :: DP_VV_V
!
      IND_PLA = LTM_DIF ( 0, VTD__M_PLA, VTD__PLANAM, PLA_NAM )
      IF ( IND_PLA < 1 ) THEN
           CALL ERR_LOG ( 3311, -2, 'GET_PLANET_COO', 'Wrong planet '// &
     &                   'name: '//PLA_NAM ) 
           RETURN 
      END IF
!
      VTD%CONF%PREC_EXP = PREC__CAPITAINE2003
      VTD%CONF%NUT_EXP  = NUT__MHB2000
      VTD%L_SOU = 1
      VTD%SOU(1)%IVS_NAME = 'OJ287   ' 
!
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_OBS, TAI_OBS, &
     &                      'EARTH', COO_EA, VEL_EA, ACC_EA, IER )
      IF ( IER .NE. 0 ) THEN
           DATE_OBS = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
           CALL ERR_LOG ( 3312, IUER, 'GET_PLANET_COO', 'Error in an '// &
     &         'attempt to compute position of the planet Earth on '// &
     &          DATE_OBS(1:24) )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_OBS, TAI_OBS, &
     &                      VTD__PLANAM(IND_PLA), COO_PLA, VEL_PLA, &
     &                      ACC_PLA, IER )
      IF ( IER .NE. 0 ) THEN
           DATE_OBS = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
           CALL ERR_LOG ( 3313, IUER, 'GET_PLANET_COO', 'Error in an '// &
     &         'attempt to compute position of the planet '// &
     &         VTD__PLANAM(IND_PLA)//' on '//DATE_OBS(1:24) )
           RETURN 
      END IF
!
      COO_BRS = COO_PLA - COO_EA
      VEL_BRS = VEL_PLA - VEL_EA
      CALL NORM_VEC ( 3, COO_BRS, DIST_PLA )
!
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, MJD_OBS, TAI_OBS - DIST_PLA/VTD__C, &
     &                      VTD__PLANAM(IND_PLA), COO_PLA, VEL_PLA, &
     &                      ACC_PLA, IER )
      IF ( IER .NE. 0 ) THEN
           DATE_OBS = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
           CALL ERR_LOG ( 3314, IUER, 'GET_PLANET_COO', 'Error in an '// &
     &         'attempt to compute position of the planet '// &
     &         VTD__PLANAM(IND_PLA)//' on '//DATE_OBS(1:24) )
           RETURN 
      END IF
!     
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_MOMENT ( VTD%SOU(1)%IVS_NAME, MJD_OBS, TAI_OBS, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           DATE_OBS = MJDSEC_TO_DATE ( MJD_OBS, TAI_OBS, -2 )
           CALL ERR_LOG ( 3315, IUER, 'GET_PLANET_COO', 'Error in an '// &
     &         'attempt to compute reductions on moment of observations '// &
     &         'on '//DATE_OBS  )
           RETURN 
      END IF
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS,      &
     &                      3, VTD%STA(1)%BEG_TRS(1:3), &
     &                      3, STA_POS_CRS, IUER )
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                      3, VTD%STA(1)%BEG_TRS(1:3), &
     &                      3, STA_VEL_CRS, IUER )
!
      COO_BRS =    COO_PLA - COO_EA - STA_POS_CRS
      VEL_BRS = -( VEL_PLA - VEL_EA - STA_VEL_CRS )
!
      CALL NORM_VEC ( 3, COO_BRS, RD )
      SV = DP_VV_V  ( 3, COO_BRS, VEL_BRS )
      CALL ADDC_VV  ( 3, 1.0D0/VTD__C, VEL_BRS, (1.0D0 - SV/VTD__C), &
     &                COO_BRS, PLA_COO )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_PLANET_COO  !#!  
