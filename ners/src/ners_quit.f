      SUBROUTINE NERS_QUIT ( IPAR, NERS )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_QUIT  releases dynamic memory allocated by NERS.      *
! *                                                                      *
! *  ### 20-JUN-2016   NERS_QUIT   v2.1 (c)  L. Petrov  02-JUN-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  IPAR, FCS_STATIC_SIZE
!
      IF ( IPAR == NERS__FCS .OR. IPAR == NERS__ALL ) THEN
           IF ( ASSOCIATED ( NERS%FCS%ARG_12         ) ) DEALLOCATE ( NERS%FCS%ARG_12    )
           IF ( ASSOCIATED ( NERS%FCS%ARG_3          ) ) DEALLOCATE ( NERS%FCS%ARG_3     )
           IF ( ASSOCIATED ( NERS%FCS%ARG_C          ) ) DEALLOCATE ( NERS%FCS%ARG_C     )
           IF ( ASSOCIATED ( NERS%FCS%ARG_L          ) ) DEALLOCATE ( NERS%FCS%ARG_L     )
           IF ( ASSOCIATED ( NERS%FCS%HEO_ARG        ) ) DEALLOCATE ( NERS%FCS%HEO_ARG   )
           IF ( ASSOCIATED ( NERS%FCS%HEOR_ARG       ) ) DEALLOCATE ( NERS%FCS%HEOR_ARG  )
           IF ( ASSOCIATED ( NERS%FCS%ARG_UTC_M_TAI  ) ) DEALLOCATE ( NERS%FCS%ARG_UTC_M_TAI )
!
           IF ( ASSOCIATED ( NERS%FCS%BSPL_E12       ) ) DEALLOCATE ( NERS%FCS%BSPL_E12  )
           IF ( ASSOCIATED ( NERS%FCS%BSPL_E3        ) ) DEALLOCATE ( NERS%FCS%BSPL_E3   )
           IF ( ASSOCIATED ( NERS%FCS%BSPL_C         ) ) DEALLOCATE ( NERS%FCS%BSPL_C    )
           IF ( ASSOCIATED ( NERS%FCS%BSPL_L         ) ) DEALLOCATE ( NERS%FCS%BSPL_L    )
           IF ( ASSOCIATED ( NERS%FCS%HEO_AMP        ) ) DEALLOCATE ( NERS%FCS%HEO_AMP   )
           IF ( ASSOCIATED ( NERS%FCS%HEOR_AMP       ) ) DEALLOCATE ( NERS%FCS%HEOR_AMP  )
           IF ( ASSOCIATED ( NERS%FCS%BSPL_UTC_M_TAI ) ) DEALLOCATE ( NERS%FCS%BSPL_UTC_M_TAI )
!
           NERS%FCS%ARG_12         => NULL()
           NERS%FCS%ARG_3          => NULL()
           NERS%FCS%ARG_C          => NULL()
           NERS%FCS%ARG_L          => NULL()
           NERS%FCS%HEO_ARG        => NULL()
           NERS%FCS%HEOR_ARG       => NULL()
           NERS%FCS%ARG_UTC_M_TAI  => NULL()
!
           NERS%FCS%BSPL_E12       => NULL()
           NERS%FCS%BSPL_E3        => NULL()
           NERS%FCS%BSPL_C         => NULL()
           NERS%FCS%BSPL_L         => NULL()
           NERS%FCS%HEO_AMP        => NULL()
           NERS%FCS%HEOR_AMP       => NULL()
           NERS%FCS%BSPL_UTC_M_TAI => NULL()
!
           NERS%FCS%NK_12           = 0
           NERS%FCS%NK_3            = 0
           NERS%FCS%NC              = 0
           NERS%FCS%NJ              = 0
           NERS%FCS%L_HEO           = 0
           NERS%FCS%L_HEOR          = 0
           NERS%FCS%NERS_FMT        = NERS__BIN_FMT 
           NERS%FCS%EOP_FCS_VERS    = ' '
           NERS%FCS%NUT_APR_MOD     = ' '
           NERS%FCS%PRC_APR_MOD     = ' '
           NERS%FCS%HEO_MOD         = ' '
           NERS%FCS%HEO_ID          = ' '
           NERS%FCS%EANG_MOD        = ' '
           NERS%FCS%TAI_GEN         = 0.0D0
           NERS%FCS%TAI_LAST_HEO    = 0.0D0
           NERS%FCS%TAI_HEO_EPOCH   = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_C = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_U = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_R = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_I = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_J = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_S = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_F = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_A = 0.0D0
           NERS%FCS%TAI_LAST_EOPS_A_ASS = 0
!
           NERS%FCS_STATUS = NERS__INIT
      END IF
!
      IF ( IPAR == NERS__EXP .OR. IPAR == NERS__ALL ) THEN
           IF ( ASSOCIATED ( NERS%EXP%TIM  ) ) DEALLOCATE ( NERS%EXP%TIM  )
           IF ( ASSOCIATED ( NERS%EXP%VAL  ) ) DEALLOCATE ( NERS%EXP%VAL  )
           IF ( ASSOCIATED ( NERS%EXP%ARG  ) ) DEALLOCATE ( NERS%EXP%ARG  )
           IF ( ASSOCIATED ( NERS%EXP%BSPL ) ) DEALLOCATE ( NERS%EXP%BSPL )
!
           NERS%EXP%L_NOD  = 0
           NERS%EXP%TIM    => NULL()
           NERS%EXP%VAL    => NULL()
           NERS%EXP%ARG    => NULL()
           NERS%EXP%ARG    => NULL()
           NERS%EXP%BSPL   => NULL()
           NERS%EXP_STATUS = NERS__UNDF
      END IF
!
      IF ( IPAR == NERS__ALL ) THEN
           IF ( ASSOCIATED ( NERS%EPH%TIM           ) ) DEALLOCATE ( NERS%EPH%TIM  )
           IF ( ASSOCIATED ( NERS%EPH%COO_EARTH_VAL ) ) DEALLOCATE ( NERS%EPH%COO_EARTH_VAL )
           IF ( ASSOCIATED ( NERS%EPH%COO_EARTH_SPL ) ) DEALLOCATE ( NERS%EPH%COO_EARTH_SPL )
           NERS%EPH%L_TIM = 0
           FCS_STATIC_SIZE = LOC(NERS%FCS%NERS_STATUS) - LOC(NERS%FCS) + &
     &                       SIZEOF(NERS%FCS%NERS_STATUS)
           CALL BZERO ( NERS%FCS, %VAL(SIZEOF(NERS%FCS)) )
           NERS%TIM_START  = 0.0D0
           NERS%TIM_STOP   = 0.0D0
           NERS%UTC_LOAD   = 0.0D0
           NERS%TIM_LOAD   = 0.0D0
           NERS%EPH_STATUS = NERS__UNDF
           NERS%EXP_STATUS = NERS__UNDF
           NERS%FCS_STATUS = NERS__UNDF
           NERS%CNF_STATUS = NERS__UNDF
           NERS%FCS%NERS_FMT  = NERS__BIN_FMT 
      END IF
!
      RETURN
      END  SUBROUTINE  NERS_QUIT  !#!#
