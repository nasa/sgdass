      SUBROUTINE SPD_FREE ( SPD, IPAR ) 
! ************************************************************************
! *                                                                      *
! *   Routine  SPD_FREE 
! *                                                                      *
! *  ### 12-MAR-2008    SPD_FREE   v2.3 (c)  L. Petrov  01-AUG-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  IPAR
      INTEGER*4  J1
!
      IF ( ASSOCIATED(SPD%LON)      ) DEALLOCATE ( SPD%LON )
      IF ( ASSOCIATED(SPD%LAT)      ) DEALLOCATE ( SPD%LAT )
      IF ( ASSOCIATED(SPD%LEV)      ) DEALLOCATE ( SPD%LEV )
      IF ( ASSOCIATED(SPD%FRQ)      ) DEALLOCATE ( SPD%FRQ )
      IF ( ASSOCIATED(SPD%REF_3D)   ) DEALLOCATE ( SPD%REF_3D )
      IF ( ASSOCIATED(SPD%SPR_3D)   ) DEALLOCATE ( SPD%SPR_3D )
      IF ( ASSOCIATED(SPD%STM_3D)   ) DEALLOCATE ( SPD%STM_3D )
      IF ( ASSOCIATED(SPD%SPW_3D)   ) DEALLOCATE ( SPD%SPW_3D )
      IF ( ASSOCIATED (SPD%ELV%ELEV)      ) DEALLOCATE ( SPD%ELV%ELEV )
      IF ( ASSOCIATED (SPD%ELV%MAP )      ) DEALLOCATE ( SPD%ELV%MAP  )
      IF ( ASSOCIATED (SPD%AZM%AZIM)      ) DEALLOCATE ( SPD%AZM%AZIM )
      IF ( ASSOCIATED (SPD%DSPL%DSPL_XYZ) ) DEALLOCATE ( SPD%DSPL%DSPL_XYZ )
      IF ( ASSOCIATED (SPD%DSPL%NAME)     ) DEALLOCATE ( SPD%DSPL%NAME     )
      IF ( IPAR == 2 ) THEN
           IF ( ASSOCIATED(SPD%STA) ) THEN
                IF ( SPD%NSTA > 0 ) THEN
                     DO 410 J1=1,SPD%NSTA
                        IF ( ASSOCIATED ( SPD%STA(J1)%DEL ) ) DEALLOCATE ( SPD%STA(J1)%DEL )
                        IF ( SPD%CONF%N_FRQ > 0 ) THEN
                             IF ( ASSOCIATED ( SPD%STA(J1)%OPA ) ) DEALLOCATE ( SPD%STA(J1)%OPA )
                             IF ( ASSOCIATED ( SPD%STA(J1)%TAT ) ) DEALLOCATE ( SPD%STA(J1)%TAT )
                        END IF
 410                 CONTINUE 
                END IF
                DEALLOCATE ( SPD%STA )
           END IF
      END IF
      IF ( ASSOCIATED(SPD%MASK)      ) DEALLOCATE ( SPD%MASK )
!
      RETURN
      END  SUBROUTINE  SPD_FREE !#!  
