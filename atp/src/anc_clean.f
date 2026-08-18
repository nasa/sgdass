      SUBROUTINE ANC_CLEAN ( ANC )
      IMPLICIT   NONE 
      INCLUDE   'atp.i'
      TYPE     ( ANC__TYP   ) :: ANC      
!
! --- 
!
      ANC%STATUS  = ATP__UNDF
      CALL CLRCH ( ANC%ANTCAL_FMT  )
      CALL CLRCH ( ANC%STA_NAM     )
      CALL CLRCH ( ANC%EXP_CODE    )
      ANC%UTC_MTAI    =   0.D0
      CALL CLRCH ( ANC%FILLER_CH   )
      ANC%FILLER_I4   = -99
      ANC%FILLER_R8   = -99.9D0
      CALL CLRCH ( ANC%FILLER_DATE )
      ANC%MJD_FILLER  =  -1
      ANC%TAI_FILLER  =   0.D0
      CALL CLRCH ( ANC%FILIN       )
      ANC%NUM_PRV     =   0
      ANC%NUM_DOO     =   0
      ANC%NUM_MET     =   0
      ANC%NUM_TPS     =   0
      ANC%NUM_TTO     =   0
      ANC%NUM_TSYS    =   0
      ANC%NUM_TATM    =   0
      ANC%NUM_OPA     =   0 
      ANC%NUM_TPI     =   0
      ANC%NUM_PCS     =   0
      ANC%NUM_PCAL    =   0
      ANC%NUM_TGPS    =   0
      ANC%NUM_GPS     =   0
      ANC%NUM_SEFD    =   0
      ANC%NUM_CBL     =   0
      ANC%NUM_EPO_TTO  =   0
      ANC%NUM_EPO_TPI  =   0
      ANC%NUM_EPO_PCAL =   0
      ANC%NUM_EPO_GPS  =   0
      ANC%NUM_EPO_SEFD =   0
      ANC%MJD_DOO     =  -1 
      ANC%MJD_TTO     =  -1
      ANC%MJD_MET     =  -1
      ANC%MJD_PCAL    =  -1
      ANC%MJD_SEFD    =  -1
      ANC%MJD_GPS     =  -1
      ANC%MJD_CBL     =  -1
      ANC%MJD_TPI     =  -1
      ANC%TAI_DOO     =   0.D0
      ANC%TAI_TTO     =   0.D0
      ANC%TAI_MET     =   0.D0
      ANC%TAI_PCAL    =   0.D0
      ANC%TAI_SEFD    =   0.D0
      ANC%TAI_GPS     =   0.D0
      ANC%TAI_CBL     =   0.D0
      ANC%TAI_TPI     =   0.D0
      CALL CLRCH ( ANC%TPS_TAG    )
      CALL CLRCH ( ANC%PCS_TAG    )
      CALL CLRCH ( ANC%TGPS_TAG   )
      CALL CLRCH ( ANC%CNF        )
      CALL CLRCH ( ANC%PRV        )
!     
! --- Deallocate all the pointers
!
      IF ( ASSOCIATED ( ANC%NEP_ARR ) )  DEALLOCATE ( ANC%NEP_ARR )
      IF ( ASSOCIATED ( ANC%DOO     ) )  DEALLOCATE ( ANC%DOO     )
      IF ( ASSOCIATED ( ANC%MET     ) )  DEALLOCATE ( ANC%MET     )
      IF ( ASSOCIATED ( ANC%TPS     ) )  DEALLOCATE ( ANC%TPS     )
      IF ( ASSOCIATED ( ANC%TTO     ) )  DEALLOCATE ( ANC%TTO     )
      IF ( ASSOCIATED ( ANC%PCS     ) )  DEALLOCATE ( ANC%PCS     )
      IF ( ASSOCIATED ( ANC%PCAL    ) )  DEALLOCATE ( ANC%PCAL    )
      IF ( ASSOCIATED ( ANC%SEFD    ) )  DEALLOCATE ( ANC%SEFD    )
      IF ( ASSOCIATED ( ANC%TGPS    ) )  DEALLOCATE ( ANC%TGPS    )
      IF ( ASSOCIATED ( ANC%GPS     ) )  DEALLOCATE ( ANC%GPS     )
      IF ( ASSOCIATED ( ANC%CBL     ) )  DEALLOCATE ( ANC%CBL     )
      IF ( ASSOCIATED ( ANC%TPI     ) )  DEALLOCATE ( ANC%TPI     )
      ANC%STATUS = ATP__INIT      
!
      RETURN
      END  SUBROUTINE ANC_CLEAN  !#!#
