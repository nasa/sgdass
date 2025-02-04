      SUBROUTINE NERS_GET_UTCMTAI ( NERS, UTC, UTC_M_TAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_GET_UTCMTAI
! *                                                                      *
! * ## 16-JUN-2016  NERS_GET_UTCMTAI  v1.3 (c) L. Petrov  16-OCT-2019 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( NERS__TYPE ) :: NERS
      REAL*8     UTC, TAI, UTC_M_TAI
      CHARACTER  STR_DATE*32, STR*32
      INTEGER*4  IUER
      INTEGER*4  MJD, J1, IER
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( NERS%FCS_STATUS .NE. NERS__INIT  .AND.  &
     &     NERS%FCS_STATUS .NE. NERS__LOAD         ) THEN
           CALL ERR_LOG ( 5731, IUER, 'NERS_GET_UTCMTAI', 'NERS object '// &
     &         'is not initialized. Please execute routine ners_init' )
           RETURN 
      END IF
!
      IF ( NERS%FCS%NJ < 1 ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:10), FMT='(I10)' ) NERS%FCS%NJ 
!$OMP      CRITICAL (NERS_GET_UTCMTAI_01)
           CALL ERR_LOG ( 5732, IUER, 'NERS_GET_UTCMTAI', 'Trap of '// &
     &         'internal control: NERS%FCS%NJ = '//STR )
!$OMP      END CRITICAL (NERS_GET_UTCMTAI_01)
           RETURN 
      END IF
!
      IF ( .NOT. ASSOCIATED ( NERS%FCS%ARG_UTC_M_TAI ) ) THEN
!$OMP      CRITICAL (NERS_GET_UTCMTAI_02)
           CALL ERR_LOG ( 5733, IUER, 'NERS_GET_UTCMTAI', 'Trap of '// &
     &         'internal control: NERS%FCS%ARG_UTC_M_TAI is not associated' )
!$OMP      END CRITICAL (NERS_GET_UTCMTAI_02)
           RETURN 
      END IF
      IF ( UTC .LE. NERS%FCS%ARG_UTC_M_TAI(1) ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_UTC_M_TAI(1)/86400.0D0)
           TAI = NERS%FCS%ARG_UTC_M_TAI(1) - 86400.0D0*INT(NERS%FCS%ARG_UTC_M_TAI(1)/86400.0D0)
           IER = IUER 
           STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IER ) 
!$OMP      CRITICAL (NERS_GET_UTCMTAI_03)
           CALL ERR_LOG ( 5733, IUER, 'NERS_GET_UTCMTAI', 'Time epoch is '// &
     &         'too early. The first supported epoch is '//STR_DATE(1:19) )
!$OMP      END CRITICAL (NERS_GET_UTCMTAI_03)
           RETURN 
      END IF
!
      IF ( UTC > NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ) ) THEN
           MJD = J2000__MJD  + INT(NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ)/86400.0D0)
           TAI = NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ) - 86400.0D0*INT(NERS%FCS%ARG_UTC_M_TAI(NERS%FCS%NJ)/86400.0D0)
           IER = IUER 
!$OMP      CRITICAL (NERS_GET_UTCMTAI_04)
           write ( 6, * ) 'utc=  ', utc ! %%%%
           write ( 6, * ) 'NERS%FCS%NJ = ', NERS%FCS%NJ ! %%%%
           write ( 6, * ) 'NERS%FCS%ARG_UTC_M_TAI = ', NERS%FCS%ARG_UTC_M_TAI ! %%%%
           STR_DATE = MJDSEC_TO_DATE ( MJD, TAI, IER ) 
           CALL ERR_LOG ( 5733, IUER, 'NERS_GET_UTCMTAI', 'Time epoch is '// &
     &        'too far in the future. The last supported epoch is '//STR_DATE(1:19) )
!$OMP      END CRITICAL (NERS_GET_UTCMTAI_04)
           RETURN 
      END IF
!
      UTC_M_TAI = 0.0D0
      DO 410 J1=1,NERS%FCS%NJ
         IF ( UTC .GE. NERS%FCS%ARG_UTC_M_TAI(J1) ) THEN
              UTC_M_TAI = NERS%FCS%BSPL_UTC_M_TAI(J1)
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_GET_UTCMTAI  !#!#
