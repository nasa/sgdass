      SUBROUTINE NERS_GET_EVEC ( NERS, TIM, EVEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_GET_EVEC
! *                                                                      *
! *  ### 05-APR-2016  NERS_GET_EVEC  v2.2 (c)  L. Petrov 05-DEC-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIM, EVEC(3,0:2)
      INTEGER*4  IUER 
      INTEGER*4  J1, J2, J3, MJD, IER
      REAL*8     TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR 
      CHARACTER  STR*22, STR1*23, STR2*23, STR3*23, STR4*23
      REAL*8,    EXTERNAL :: EBSPL_VAL_R8, EBSPL_DER_R8, EBSPL_DR2_R8
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
      EVEC = 0.0D0
!
! --- Check the date
!
      IF ( TIM .GE. NERS%FCS%ARG_C(1) .AND. TIM .LE. NERS%FCS%ARG_C(NERS%FCS%NC-NERS__EDG_NODES) ) THEN
!
! -------- Use C04 series
!
           DO 410 J1=1,3
              EVEC(J1,0) = EBSPL_VAL_R8 ( NERS%FCS%NC, NERS__MDEG, TIM, NERS%FCS%ARG_C, &
     &                                    NERS%FCS%BSPL_C(1-NERS__MDEG,J1) )
              EVEC(J1,1) = EBSPL_DER_R8 ( NERS%FCS%NC, NERS__MDEG, TIM, NERS%FCS%ARG_C, &
     &                                    NERS%FCS%BSPL_C(1-NERS__MDEG,J1) )
              EVEC(J1,2) = EBSPL_DR2_R8 ( NERS%FCS%NC, NERS__MDEG, TIM, NERS%FCS%ARG_C, &
     &                                    NERS%FCS%BSPL_C(1-NERS__MDEG,J1) )
 410       CONTINUE 
         ELSE IF ( TIM .LE. NERS%FCS%ARG_3(NERS%FCS%NK_3) ) THEN
!
! -------- Use the forecast series
!
           DO 420 J2=1,2
              EVEC(J2,0) = EBSPL_VAL_R8 ( NERS%FCS%NK_12, NERS__MDEG, TIM, NERS%FCS%ARG_12, &
     &                                    NERS%FCS%BSPL_E12(1-NERS__MDEG,J2) )
              EVEC(J2,1) = EBSPL_DER_R8 ( NERS%FCS%NK_12, NERS__MDEG, TIM, NERS%FCS%ARG_12, &
     &                                    NERS%FCS%BSPL_E12(1-NERS__MDEG,J2) )
              EVEC(J2,2) = EBSPL_DR2_R8 ( NERS%FCS%NK_12, NERS__MDEG, TIM, NERS%FCS%ARG_12, &
     &                                    NERS%FCS%BSPL_E12(1-NERS__MDEG,J2) )
 420       CONTINUE 
!
           EVEC(3,0) = EBSPL_VAL_R8 ( NERS%FCS%NK_3, NERS__MDEG, TIM, NERS%FCS%ARG_3, &
     &                                NERS%FCS%BSPL_E3 )
           EVEC(3,1) = EBSPL_DER_R8 ( NERS%FCS%NK_3, NERS__MDEG, TIM, NERS%FCS%ARG_3, &
     &                                NERS%FCS%BSPL_E3 )
           EVEC(3,2) = EBSPL_DR2_R8 ( NERS%FCS%NK_3, NERS__MDEG, TIM, NERS%FCS%ARG_3, &
     &                                NERS%FCS%BSPL_E3 )
           MJD = J2000__MJD  + INT(TIM/86400.0D0)
           TAI = TIM - 86400.0D0*INT(TIM/86400.0D0)
           IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_D93 ) THEN
                CALL NERS_E3ZT_DICKMAN1993 ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
              ELSE IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_RE2014  ) THEN
                CALL NERS_E3ZT_RE2014      ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
              ELSE
                 E3_HAR     = 0.0D0
                 E3_DOT_HAR = 0.0D0
                 E3_DT2_HAR = 0.0D0
           END IF
           EVEC(3,0) = EVEC(3,0) + E3_HAR
           EVEC(3,1) = EVEC(3,1) + E3_DOT_HAR
           EVEC(3,2) = EVEC(3,2) + E3_DT2_HAR
         ELSE 
           IF ( NERS%FCS%NL .LE. 0 ) THEN
                CALL ERR_LOG ( 5711, IUER, 'NERS_GET_EVEC', 'Argument TIM '// &
     &              'is out of range' )
                RETURN 
           END IF
           IF ( TIM .LE. NERS%FCS%ARG_L(NERS%FCS%NL) .AND. &
     &          TIM .GE. NERS%FCS%ARG_C(1)           .AND. &
     &          NERS%CNF%LTP_USAGE .NE. NERS__STOP         ) THEN 
!
                IF ( NERS%CNF%LTP_USAGE .EQ. NERS__WARNING .AND. .NOT. NERS%WARN_LTP ) THEN
                     WRITE ( UNIT=STR, FMT='(1PD22.15)' ) TIM
                     STR1 = TIM_TO_DATE ( TIM,            IER )
                     STR3 = TIM_TO_DATE ( NERS%FCS%ARG_3(NERS%FCS%NK_3), IER )
                     WRITE ( 6, '(A)' ) 'WARNING! NERS: Epoch TIM= '//STR(1:22)//' ( '// &
     &                       STR1(1:23)//' ) is beyond the last epoch of the '// &
     &                      'EOP forecast '//STR3(1:23)//'. Long-term prediction '// &
     &                      'is used. Accuracy of EOP is substantially impaired. '// &
     &                      'Please check Internet connection to the NERS servers.'
                     NERS%WARN_LTP = .TRUE.
                END IF
!
! ------------- Use the long-term prediction series. NB: the LTP spline keeps E3 
! ------------- with the conqtribution due to zonal tides subtracted
!
                DO 430 J3=1,3
                   EVEC(J3,0) = EBSPL_VAL_R8 ( NERS%FCS%NL, NERS__MDEG, TIM, NERS%FCS%ARG_L, &
     &                                         NERS%FCS%BSPL_L(1-NERS__MDEG,J3) )
                   EVEC(J3,1) = EBSPL_DER_R8 ( NERS%FCS%NL, NERS__MDEG, TIM, NERS%FCS%ARG_L, &
     &                                         NERS%FCS%BSPL_L(1-NERS__MDEG,J3) )
                   EVEC(J3,2) = EBSPL_DR2_R8 ( NERS%FCS%NL, NERS__MDEG, TIM, NERS%FCS%ARG_L, &
     &                                         NERS%FCS%BSPL_L(1-NERS__MDEG,J3) )
 430            CONTINUE 
!
! ------------- Now add the contribution to E3 due to zonal tides added back
!
                MJD = J2000__MJD  + INT(TIM/86400.0D0)
                TAI = TIM - 86400.0D0*INT(TIM/86400.0D0)
                IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_D93 ) THEN
                     CALL NERS_E3ZT_DICKMAN1993 ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
                   ELSE IF ( NERS%FCS%E3Z_APR_MOD == NERS__E3Z_RE2014  ) THEN
                     CALL NERS_E3ZT_RE2014      ( 0, MJD, TAI, E3_HAR, E3_DOT_HAR, E3_DT2_HAR ) 
                   ELSE
                     E3_HAR     = 0.0D0
                     E3_DOT_HAR = 0.0D0
                     E3_DT2_HAR = 0.0D0
                END IF
                EVEC(3,0) = EVEC(3,0) + E3_HAR
                EVEC(3,1) = EVEC(3,1) + E3_DOT_HAR
                EVEC(3,2) = EVEC(3,2) + E3_DT2_HAR
              ELSE
                WRITE ( UNIT=STR, FMT='(1PD22.15)' ) TIM
                STR1 = TIM_TO_DATE ( TIM,            IER )
                STR2 = TIM_TO_DATE ( NERS%FCS%ARG_C(1), IER )
                STR3 = TIM_TO_DATE ( NERS%FCS%ARG_3(NERS%FCS%NK_3), IER )
                IF ( NERS%FCS%NL > 0 ) THEN
                     STR4 = TIM_TO_DATE ( NERS%FCS%ARG_L(NERS%FCS%NL), IER )
                   ELSE 
                     STR4 = TIM_TO_DATE ( NERS%FCS%ARG_3(NERS%FCS%NK_3), IER )
                END IF
                CALL ERR_LOG ( 5712, IUER, 'NERS_GET_EVEC', 'Argument TIM='//TRIM(STR)// &
     &              ' '//TRIM(STR1)//' is out of range. Valid forecast range: '// &
     &              '[ '//STR2//', '//STR3//' ]; valid long-term prediction range [ '// &
     &              STR2//', '//STR4//' ]' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_GET_EVEC  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EOP_NERS_UTCMTAI ( TIM, NERS, UTC_M_TAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EOP_NERS_UTCMTAI
! *                                                                      *
! * ### 05-APR-2016  EOP_NERS_UTCMTAI v2.1 (c) L. Petrov 15-NOV-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     TIM, UTC_M_TAI, TAI
      CHARACTER  STR_DATE*32, REQ_DATE*19
      INTEGER*4  IUER
      INTEGER*4  MJD, J1, IER
      CHARACTER  MJDSEC_TO_DATE*30, TIM_TO_DATE*23
!
      IF ( TIM .LE. NERS%FCS%ARG_UTC_M_TAI(1) ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_UTC_M_TAI(1)/86400.0D0)
           TAI = NERS%FCS%ARG_UTC_M_TAI(1) - &
     &           86400.0D0*INT(NERS%FCS%ARG_UTC_M_TAI(1)/86400.0D0)
           IER = IUER
           STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IER ) 
           IER = IUER
           REQ_DATE = TIM_TO_DATE    ( TIM, IER )
           CALL ERR_LOG ( 5731, IUER, 'EOP_NERS_UTCMTAI', 'Requested '// &
     &         'time epoch '//REQ_DATE(1:19)//' is too early. '// &
     &         'The first supported epoch is '//STR_DATE(1:19) )
           RETURN 
      END IF
!
      IF ( TIM > NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ) ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ)/86400.0D0)
           TAI = NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ) - &
     &           86400.0D0*INT(NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ)/86400.0D0)
           IER = IUER
           STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IER ) 
           IER = IUER
           REQ_DATE = TIM_TO_DATE    ( TIM, IER )
           CALL ERR_LOG ( 5732, IUER, 'EOP_NERS_UTCMTAI', 'Requested '// &
     &         'time epoch '//REQ_DATE(1:19)//' is too far in the future. '// &
     &         'The last supported epoch is '//STR_DATE(1:19) )
           RETURN 
      END IF
!
      UTC_M_TAI = 0.0D0
      DO 410 J1=1,NERS%FCS%NJ
         IF ( TIM .GE. NERS%FCS%ARG_UTC_M_TAI(J1) ) THEN
              UTC_M_TAI = NERS%FCS%BSPL_UTC_M_TAI(J1) 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EOP_NERS_UTCMTAI  !#!#
