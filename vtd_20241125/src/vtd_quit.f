      SUBROUTINE VTD_QUIT ( VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_QUIT  releases memory allocated by internal data       *
! *   structures of package VLBI Time Delay (VTD).                       *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 14-APR-2006    VTD_QUIT   v1.8 (c)  L. Petrov  27-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7
      LOGICAL*4, EXTERNAL :: PROBE_WRITE_ADDRESS
!
      IF ( .NOT. PROBE_WRITE_ADDRESS ( VTD%STATUS ) ) THEN
           CALL ERR_LOG ( 2991, IUER, 'VTD_QUIT', 'Object VTD is not '// &
     &         'accessible for writing. It is an indication of a very '// &
     &         'serious error. Please check the argument list' )
           RETURN
      END IF
!
      IF ( VTD%STATUS == VTD__LOAD .OR. &
     &     VTD%STATUS == VTD__ALLC      ) THEN
!
           IF ( VTD%L_SOU > 0 ) THEN
                DO 410 J1=1,VTD%L_SOU
                   DO 420 J2=1,VTD__M_BND
                      IF ( ASSOCIATED ( VTD%SOU(J1)%MAP(J2)%IMAGE ) ) THEN
                           DEALLOCATE ( VTD%SOU(J1)%MAP(J2)%IMAGE )
                      END IF
                      IF ( ASSOCIATED ( VTD%SOU(J1)%MAP(J2)%FLUX_CC ) ) THEN
                           DEALLOCATE ( VTD%SOU(J1)%MAP(J2)%FLUX_CC )
                      END IF
                      IF ( ASSOCIATED ( VTD%SOU(J1)%MAP(J2)%COOR_CC ) ) THEN
                           DEALLOCATE ( VTD%SOU(J1)%MAP(J2)%COOR_CC )
                      END IF
 420               CONTINUE 
 410            CONTINUE 
           END IF
!
           IF ( VTD%STATUS_ANTI == VTD__YES ) THEN
                IF ( VTD%ANTI%N_AHM > 0 ) THEN
                     IF ( ASSOCIATED ( VTD%ANTI%AHM_STA_NAM ) ) THEN
                          DEALLOCATE ( VTD%ANTI%AHM_STA_NAM )
                     END IF
                     IF ( ASSOCIATED ( VTD%ANTI%AHM_FIL ) ) THEN
                          DEALLOCATE ( VTD%ANTI%AHM_FIL     )
                     END IF
                END IF
                IF ( VTD%ANTI%N_ANT > 0 ) THEN
                     IF ( ASSOCIATED ( VTD%ANTI%STA_NAM ) ) THEN
                          DEALLOCATE ( VTD%ANTI%STA_NAM )
                     END IF
                     IF ( ASSOCIATED ( VTD%ANTI%INFO ) ) THEN
                          DEALLOCATE ( VTD%ANTI%INFO )
                     END IF
                END IF
                VTD%ANTI%STATUS = ANTI__UNDF 
                VTD%STATUS_ANTI = ANTI__UNDF 
                VTD%STATUS_AHM  = ANTI__UNDF 
!
                VTD%ANTI%N_ANT = 0 
                VTD%ANTI%N_AHM = 0 
           END IF
!
           IF ( VTD%STATUS_AGD == VTD__YES ) THEN
                IF ( VTD%AGD%N_ANT > 0 ) THEN
                     DO 430 J3=1,VTD%AGD%N_ANT
                        IF ( ASSOCIATED ( VTD%AGD%INFO(J3)%FOC_SPL ) ) THEN
                             DEALLOCATE ( VTD%AGD%INFO(J3)%FOC_SPL )
                        END IF
                        IF ( ASSOCIATED ( VTD%AGD%INFO(J3)%FOC_LEN ) ) THEN
                             DEALLOCATE ( VTD%AGD%INFO(J3)%FOC_LEN )
                        END IF
                        IF ( ASSOCIATED ( VTD%AGD%INFO(J3)%ELEV ) ) THEN
                             DEALLOCATE ( VTD%AGD%INFO(J3)%ELEV    )
                        END IF
 430                 CONTINUE 
                     IF ( ASSOCIATED ( VTD%AGD%INFO ) ) THEN
                          DEALLOCATE ( VTD%AGD%INFO )
                     END IF
                     IF ( ASSOCIATED ( VTD%AGD%STA_NAM ) ) THEN
                          DEALLOCATE ( VTD%AGD%STA_NAM )
                     END IF
                END IF
                VTD%AGD%N_ANT = 0
                VTD%AGD%STATUS = VTD__UNDF
           END IF
!
           DO 440 J4=VTD__M_PSF,1,-1
              IF ( VTD%POSVAR(J4)%MEM_LEN > 0 ) THEN
                   CALL FREE ( VTD%POSVAR(J4)%MEM_ADR )
                   VTD%POSVAR(J4)%MEM_ADR = 0
                   VTD%POSVAR(J4)%MEM_LEN = 0
              END IF
              IF ( VTD%POSVAR(J4)%STS_BSPPOS == VTD__LOAD ) THEN
                   IF ( ASSOCIATED ( VTD%POSVAR(J4)%BSP ) ) THEN
                        DEALLOCATE ( VTD%POSVAR(J4)%BSP )
                   END IF
              END IF
              VTD%POSVAR(J4)%STS_BSPPOS = VTD__UNDF
 440       CONTINUE 
!
           DO 450 J5=VTD__MEM,1,-1
              IF ( VTD%MEM_LEN(J5) > 0 ) THEN
                   CALL FREE ( VTD%MEM_ADR(J5) )
                   VTD%MEM_ADR(J5) = 0
                   VTD%MEM_LEN(J5) = 0
              END IF
 450       CONTINUE 
!
           IF ( VTD%IONO%STATUS_VAL == VIO__ALLO .OR. &
     &          VTD%IONO%STATUS_VAL == VIO__READ .OR. &
     &          VTD%IONO%STATUS_VAL == VIO__COMP      ) THEN
                IF ( ASSOCIATED ( VTD%IONO%TEC_VAL ) ) THEN
                     DEALLOCATE ( VTD%IONO%TEC_VAL )
                END IF
                VTD%IONO%STATUS_VAL = VTD__UNDF
           END IF 
           IF ( VTD%IONO%STATUS_SPL == VIO__ALLO .OR. &
     &          VTD%IONO%STATUS_SPL == VIO__READ .OR. &
     &          VTD%IONO%STATUS_SPL == VIO__COMP      ) THEN
                IF ( ASSOCIATED ( VTD%IONO%TEC_SPL ) ) THEN
                     DEALLOCATE ( VTD%IONO%TEC_SPL )
                END IF
                IF ( ASSOCIATED ( VTD%IONO%LON_VAL ) ) THEN
                     DEALLOCATE ( VTD%IONO%LON_VAL )
                END IF
                IF ( ASSOCIATED ( VTD%IONO%LAT_VAL ) ) THEN
                     DEALLOCATE ( VTD%IONO%LAT_VAL )
                END IF
                IF ( ASSOCIATED ( VTD%IONO%TIM_VAL ) ) THEN
                     DEALLOCATE ( VTD%IONO%TIM_VAL )
                END IF
                VTD%IONO%STATUS_SPL = VTD__UNDF
           END IF 
!
           DO 460 J6=1,VTD%L_STA
               IF ( VTD%SPD_3D(J6)%STATUS .NE. SPD__UNDF ) THEN
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%SUR_PRS) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%SUR_PRS  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%SUR_TEM) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%SUR_TEM  )
                    END IF 
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%DELS) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%DELS  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%MAP_ARR) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%MAP_ARR  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%TIM_ARR) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%TIM_ARR  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%ZEN_DEL) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%ZEN_DEL  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%MOD%TEXT) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%MOD%TEXT  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%MET%TEXT) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%MET%TEXT  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%MF%EL_ARG) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%MF%EL_ARG )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%MF%MF_SPL) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%MF%MF_SPL  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%MF%MF_ARG) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%MF%MF_ARG  )
                    END IF
                    IF ( ASSOCIATED ( VTD%SPD_3D(J6)%MF%EL_SPL) ) THEN
                         DEALLOCATE ( VTD%SPD_3D(J6)%MF%EL_SPL  )
                    END IF
                    VTD%SPD_3D(J6)%MF%STATUS = SPD__UNDF 
                    VTD%SPD_3D(J6)%STATUS    = SPD__UNDF 
               END IF
 460       CONTINUE 
!
           IF ( VTD%MF%STATUS .NE. SPD__UNDF ) THEN
                IF ( ASSOCIATED ( VTD%MF%EL_ARG ) ) THEN
                     DEALLOCATE ( VTD%MF%EL_ARG )
                END IF
                IF ( ASSOCIATED ( VTD%MF%MF_SPL ) ) THEN
                     DEALLOCATE ( VTD%MF%MF_SPL )
                END IF
                IF ( ASSOCIATED ( VTD%MF%MF_ARG ) ) THEN
                     DEALLOCATE ( VTD%MF%MF_ARG )
                END IF
                IF ( ASSOCIATED ( VTD%MF%EL_SPL ) ) THEN
                     DEALLOCATE ( VTD%MF%EL_SPL )
                END IF
                VTD%MF%STATUS = SPD__UNDF
           END IF
!
           IF ( VTD%L_NZO > 0 ) THEN
                DO 470 J7=1,VTD%L_NZO
                   IF ( ASSOCIATED ( VTD%NZO(J7)%TIM_ARR ) ) THEN
                        DEALLOCATE ( VTD%NZO(J7)%TIM_ARR )
                   END IF
                   IF ( ASSOCIATED ( VTD%NZO(J7)%SPL_ARR ) ) THEN
                        DEALLOCATE ( VTD%NZO(J7)%SPL_ARR )
                   END IF
                   IF ( ASSOCIATED ( VTD%NZO(J7)%SPL_RLT_ARR ) ) THEN
                        DEALLOCATE ( VTD%NZO(J7)%SPL_RLT_ARR )
                   END IF
 470            CONTINUE 
            END IF
            VTD%STATUS = VTD__UNDF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_QUIT
