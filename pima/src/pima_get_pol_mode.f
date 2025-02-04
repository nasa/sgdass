      SUBROUTINE PIMA_GET_POL_MODE ( PIM, POLAR, IND_OBS, POL_MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PIMA_GET_POL_MODE
! *                                                                      *
! * ### 15-DEC-2018 PIMA_GET_POL_MODE v1.4 (c) L. Petrov 15-DEC-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      CHARACTER  POLAR*(*)
      INTEGER*4  POL_MODE, IND_OBS, IUER
!
      IF ( PIM%NSTK == 1 ) THEN
           IF ( POLAR == PIMA__POLAR_RR ) THEN
                POL_MODE = PIMA__RRCC
              ELSE IF ( POLAR == PIMA__POLAR_LL ) THEN
                POL_MODE = PIMA__LLCC
              ELSE
                CALL ERR_LOG ( 7991, IUER, 'PIMA_GET_POL_MODE', 'Cannot use '// &
     &              TRIM(POLAR)//' because the visibility file '// &
     &              'does not contain this polarization' )
                RETURN
           END IF
         ELSE IF ( PIM%NSTK == 2 ) THEN
           IF ( POLAR == PIMA__POLAR_RR ) THEN
                POL_MODE = PIMA__RRCC
              ELSE IF ( POLAR == PIMA__POLAR_LL ) THEN
                POL_MODE = PIMA__LLCC
              ELSE IF ( POLAR == PIMA__POLAR_I ) THEN
                POL_MODE = PIMA__IPCC
              ELSE 
                CALL ERR_LOG ( 7992, IUER, 'PIMA_GET_POL_MODE', 'Cannot use '// &
     &               TRIM(POLAR)//' because the visibility file '// &
     &               'does not contain this polarization' )
                RETURN
           END IF
         ELSE IF ( PIM%NSTK == 4 ) THEN
           IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_R .OR. &
     &            PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_L      ) .AND. &
     &          ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_R .OR. &
     &            PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_L      )       ) THEN
!
! ============= Cir-cir case
!
                IF ( POLAR == PIMA__POLAR_RR ) THEN
                     POL_MODE = PIMA__RRCC
                   ELSE IF ( POLAR == PIMA__POLAR_LL ) THEN
                     POL_MODE = PIMA__LLCC
                   ELSE IF ( POLAR == PIMA__POLAR_RL ) THEN
                     POL_MODE = PIMA__RLCC
                   ELSE IF ( POLAR == PIMA__POLAR_LR ) THEN
                     POL_MODE = PIMA__LRCC
                   ELSE IF ( POLAR == PIMA__POLAR_I ) THEN
                     POL_MODE = PIMA__IPCC
                   ELSE IF ( POLAR == PIMA__POLAR_Q ) THEN
                     POL_MODE = PIMA__QPCC
                   ELSE IF ( POLAR == PIMA__POLAR_U ) THEN
                     POL_MODE = PIMA__UPCC
                   ELSE IF ( POLAR == PIMA__POLAR_V ) THEN
                     POL_MODE = PIMA__VPCC
                   ELSE IF ( POLAR == PIMA__POLAR_ALL  ) THEN
                     POL_MODE = PIMA__PALL_XY
                   ELSE IF ( POLAR == PIMA__POLAR_ORIG ) THEN
                     POL_MODE = PIMA__PALL_NOR
                   ELSE IF ( POLAR == PIMA__POLAR_1ST  ) THEN
                     POL_MODE = PIMA__PALL_1ST
                   ELSE IF ( POLAR == PIMA__POLAR_2ND  ) THEN
                     POL_MODE = PIMA__PALL_2ND
                   ELSE 
                     CALL ERR_LOG ( 7993, IUER, 'PIMA_GET_POL_MODE', 'Cannot use '// &
     &                    TRIM(POLAR)//' because the visibility file '// &
     &                   'does not contain this polarization' )
                     RETURN
                END IF
              ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_V      ) .AND. &
     &                  ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_V      )     ) THEN
!
! ============= Lin-lin case
!
                IF ( POLAR == PIMA__POLAR_HH ) THEN
                     POL_MODE = PIMA__HHLL
                   ELSE IF ( POLAR == PIMA__POLAR_VV ) THEN
                     POL_MODE = PIMA__VVLL
                   ELSE IF ( POLAR == PIMA__POLAR_HV ) THEN
                     POL_MODE = PIMA__HVLL
                   ELSE IF ( POLAR == PIMA__POLAR_VH ) THEN
                     POL_MODE = PIMA__VHLL
                   ELSE IF ( POLAR == PIMA__POLAR_XX ) THEN
                     POL_MODE = PIMA__XXLL
                   ELSE IF ( POLAR == PIMA__POLAR_YY ) THEN
                     POL_MODE = PIMA__YYLL
                   ELSE IF ( POLAR == PIMA__POLAR_XY ) THEN
                     POL_MODE = PIMA__XYLL
                   ELSE IF ( POLAR == PIMA__POLAR_YX ) THEN
                     POL_MODE = PIMA__YXLL
                   ELSE IF ( POLAR == PIMA__POLAR_RR ) THEN
                     POL_MODE = PIMA__RRLL
                   ELSE IF ( POLAR == PIMA__POLAR_LL ) THEN
                     POL_MODE = PIMA__LLLL
                   ELSE IF ( POLAR == PIMA__POLAR_RL ) THEN
                     POL_MODE = PIMA__RLLL
                   ELSE IF ( POLAR == PIMA__POLAR_LR ) THEN
                     POL_MODE = PIMA__LRLL
                   ELSE IF ( POLAR == PIMA__POLAR_I ) THEN
                     POL_MODE = PIMA__IPLL
                   ELSE IF ( POLAR == PIMA__POLAR_Q ) THEN
                     POL_MODE = PIMA__QPLL
                   ELSE IF ( POLAR == PIMA__POLAR_U ) THEN
                     POL_MODE = PIMA__UPLL
                   ELSE IF ( POLAR == PIMA__POLAR_V ) THEN
                     POL_MODE = PIMA__VPLL
                   ELSE IF ( POLAR == PIMA__POLAR_ALL ) THEN
                     POL_MODE = PIMA__PALL_XY
                   ELSE IF ( POLAR == PIMA__POLAR_ORIG   ) THEN
                     POL_MODE = PIMA__PALL_NOR
                   ELSE IF ( POLAR == PIMA__POLAR_1ST    ) THEN
                     POL_MODE = PIMA__PALL_1ST
                   ELSE IF ( POLAR == PIMA__POLAR_2ND    ) THEN
                     POL_MODE = PIMA__PALL_2ND
                   ELSE 
                     CALL ERR_LOG ( 7994, IUER, 'PIMA_GET_POL_MODE',  'Cannot use '// &
     &                    TRIM(POLAR)//' because the visibility file '// &
     &                   'does not contain this polarization' )
                     RETURN
                END IF
              ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_V      ) .AND. &
     &                  ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_R .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_L      )     ) THEN
!
! ============= Lin-cir case
!
                IF ( POLAR == PIMA__POLAR_HR ) THEN
                     POL_MODE = PIMA__HRLC
                   ELSE IF ( POLAR == PIMA__POLAR_HL ) THEN
                     POL_MODE = PIMA__HLLC
                   ELSE IF ( POLAR == PIMA__POLAR_VR ) THEN
                     POL_MODE = PIMA__VRLC
                   ELSE IF ( POLAR == PIMA__POLAR_VL ) THEN
                     POL_MODE = PIMA__VLLC
                   ELSE IF ( POLAR == PIMA__POLAR_XX ) THEN
                     POL_MODE = PIMA__XXLC
                   ELSE IF ( POLAR == PIMA__POLAR_YY ) THEN
                     POL_MODE = PIMA__YYLC
                   ELSE IF ( POLAR == PIMA__POLAR_XY ) THEN
                     POL_MODE = PIMA__XYLC
                   ELSE IF ( POLAR == PIMA__POLAR_YX ) THEN
                     POL_MODE = PIMA__YXLC
                   ELSE IF ( POLAR == PIMA__POLAR_RR ) THEN
                     POL_MODE = PIMA__RRLC
                   ELSE IF ( POLAR == PIMA__POLAR_LL ) THEN
                     POL_MODE = PIMA__LLLC
                   ELSE IF ( POLAR == PIMA__POLAR_RL ) THEN
                     POL_MODE = PIMA__RLLC
                   ELSE IF ( POLAR == PIMA__POLAR_LR ) THEN
                     POL_MODE = PIMA__LRLC
                   ELSE IF ( POLAR == PIMA__POLAR_I ) THEN
                     POL_MODE = PIMA__IPLC
                   ELSE IF ( POLAR == PIMA__POLAR_Q ) THEN
                     POL_MODE = PIMA__QPLC
                   ELSE IF ( POLAR == PIMA__POLAR_U ) THEN
                     POL_MODE = PIMA__UPLC
                   ELSE IF ( POLAR == PIMA__POLAR_V ) THEN
                     POL_MODE = PIMA__VPLC
                   ELSE IF ( POLAR == PIMA__POLAR_ALL  ) THEN
                     POL_MODE = PIMA__PALL_XY
                   ELSE IF ( POLAR == PIMA__POLAR_ORIG ) THEN
                     POL_MODE = PIMA__PALL_NOR
                   ELSE IF ( POLAR == PIMA__POLAR_1ST  ) THEN
                     POL_MODE = PIMA__PALL_1ST
                   ELSE IF ( POLAR == PIMA__POLAR_2ND  ) THEN
                     POL_MODE = PIMA__PALL_2ND
                   ELSE 
                     CALL ERR_LOG ( 7995, IUER, 'PIMA_GET_POL_MODE', 'Cannot use '// &
     &                    TRIM(POLAR)//' because the visibility file '// &
     &                   'does not contain this polarization' )
                     RETURN
                END IF
              ELSE IF ( ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_R .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(1))%POL_TYP(1) == PIMA__POL_L      ) .AND. &
     &                  ( PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_H .OR. &
     &                    PIM%STA(PIM%OBS(IND_OBS)%STA_IND(2))%POL_TYP(1) == PIMA__POL_V      )     ) THEN
!
! ============= Cir-lin case
!
                IF ( POLAR == PIMA__POLAR_RH ) THEN
                     POL_MODE = PIMA__RHCL
                   ELSE IF ( POLAR == PIMA__POLAR_LH ) THEN
                     POL_MODE = PIMA__LHCL
                   ELSE IF ( POLAR == PIMA__POLAR_RV ) THEN
                     POL_MODE = PIMA__RVCL
                   ELSE IF ( POLAR == PIMA__POLAR_LV ) THEN
                     POL_MODE = PIMA__LVCL
                   ELSE IF ( POLAR == PIMA__POLAR_XX ) THEN
                     POL_MODE = PIMA__XXCL
                   ELSE IF ( POLAR == PIMA__POLAR_YY ) THEN
                     POL_MODE = PIMA__YYCL
                   ELSE IF ( POLAR == PIMA__POLAR_XY ) THEN
                     POL_MODE = PIMA__XYCL
                   ELSE IF ( POLAR == PIMA__POLAR_YX ) THEN
                     POL_MODE = PIMA__YXCL
                   ELSE IF ( POLAR == PIMA__POLAR_RR ) THEN
                     POL_MODE = PIMA__RRCL
                   ELSE IF ( POLAR == PIMA__POLAR_LL ) THEN
                     POL_MODE = PIMA__LLCL
                   ELSE IF ( POLAR == PIMA__POLAR_RL ) THEN
                     POL_MODE = PIMA__RLCL
                   ELSE IF ( POLAR == PIMA__POLAR_LR ) THEN
                     POL_MODE = PIMA__LRCL
                   ELSE IF ( POLAR == PIMA__POLAR_I ) THEN
                     POL_MODE = PIMA__IPCL
                   ELSE IF ( POLAR == PIMA__POLAR_Q ) THEN
                     POL_MODE = PIMA__QPCL
                   ELSE IF ( POLAR == PIMA__POLAR_U ) THEN
                     POL_MODE = PIMA__UPCL
                   ELSE IF ( POLAR == PIMA__POLAR_V ) THEN
                     POL_MODE = PIMA__VPCL
                   ELSE IF ( POLAR == PIMA__POLAR_ALL  ) THEN
                     POL_MODE = PIMA__PALL_XY
                   ELSE IF ( POLAR == PIMA__POLAR_ORIG ) THEN
                     POL_MODE = PIMA__PALL_NOR
                   ELSE IF ( POLAR == PIMA__POLAR_1ST  ) THEN
                     POL_MODE = PIMA__PALL_1ST
                   ELSE IF ( POLAR == PIMA__POLAR_2ND  ) THEN
                     POL_MODE = PIMA__PALL_2ND
                   ELSE 
                     CALL ERR_LOG ( 7996, IUER, 'PIMA_GET_POL_MODE', 'Cannot use '// &
     &                    TRIM(POLAR)//' because the visibility file '// &
     &                   'does not contain this polarization' )
                     RETURN
                END IF
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_GET_POL_MODE  !#!#
