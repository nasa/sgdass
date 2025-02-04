      SUBROUTINE EOP_INIT ( EOP )
! ************************************************************************
! *                                                                      *
! *   Routine  EOP_INIT
! *                                                                      *
! *  ### 05-MAR-2016    EOP_INIT   v1.0 (c)  L. Petrov  05-MAR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INTEGER*4  J1
!
      TYPE     ( MALO__EOP_TYPE ) :: EOP
      CALL NOUT ( SIZEOF(EOP), EOP )
      DO 410 J1=1,M__EOPS
         EOP%EOPS(J1)%NP  = 0
         CALL CLRCH ( EOP%EOPS(J1)%FIL_EOPS )
         EOP%EOPS(J1)%SER => NULL()
 410  CONTINUE 
!
      RETURN
      END  SUBROUTINE EOP_INIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FCS_QUIT ( NERS )
! ************************************************************************
! *                                                                      *
! *   Routine FCS_QUIT 
! *                                                                      *
! *  ### 04-APR-2016    FCS_QUIT   v2.2 (c)  L. Petrov  04-DEC-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
!
      IF ( ASSOCIATED ( NERS%FCS%ARG_12         ) ) DEALLOCATE ( NERS%FCS%ARG_12         )
      IF ( ASSOCIATED ( NERS%FCS%ARG_3          ) ) DEALLOCATE ( NERS%FCS%ARG_3          )
      IF ( ASSOCIATED ( NERS%FCS%ARG_C          ) ) DEALLOCATE ( NERS%FCS%ARG_C          )
      IF ( ASSOCIATED ( NERS%FCS%ARG_L          ) ) DEALLOCATE ( NERS%FCS%ARG_L          )
      IF ( ASSOCIATED ( NERS%FCS%ARG_UTC_M_TAI  ) ) DEALLOCATE ( NERS%FCS%ARG_UTC_M_TAI  )
      IF ( ASSOCIATED ( NERS%FCS%HEO_ARG        ) ) DEALLOCATE ( NERS%FCS%HEO_ARG        )
      IF ( ASSOCIATED ( NERS%FCS%HEOR_ARG       ) ) DEALLOCATE ( NERS%FCS%HEOR_ARG       )
!
      IF ( ASSOCIATED ( NERS%FCS%BSPL_E12       ) ) DEALLOCATE ( NERS%FCS%BSPL_E12       )
      IF ( ASSOCIATED ( NERS%FCS%BSPL_E3        ) ) DEALLOCATE ( NERS%FCS%BSPL_E3        )
      IF ( ASSOCIATED ( NERS%FCS%BSPL_C         ) ) DEALLOCATE ( NERS%FCS%BSPL_C         )
      IF ( ASSOCIATED ( NERS%FCS%BSPL_L         ) ) DEALLOCATE ( NERS%FCS%BSPL_L         )
      IF ( ASSOCIATED ( NERS%FCS%HEO_AMP        ) ) DEALLOCATE ( NERS%FCS%HEO_AMP        )
      IF ( ASSOCIATED ( NERS%FCS%HEOR_AMP       ) ) DEALLOCATE ( NERS%FCS%HEOR_AMP       )
      IF ( ASSOCIATED ( NERS%FCS%BSPL_UTC_M_TAI ) ) DEALLOCATE ( NERS%FCS%BSPL_UTC_M_TAI )
      CALL NOUT ( LOC(NERS%FCS%NERS_STATUS) - LOC(NERS%FCS) + SIZEOF(NERS%FCS%NERS_STATUS), NERS%FCS  )
!
      RETURN
      END  SUBROUTINE  FCS_QUIT  !#!  
