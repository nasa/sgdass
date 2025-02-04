      PROGRAM    UT1MTAI
! ************************************************************************
! *                                                                      *
! *   Routine  UT1MTAI
! *                                                                      *
! *  ### 22-JUN-2016    UT1MTAI    v1.0 (c)  L. Petrov  22-JUN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( NERS__TYPE ) :: NERS
      CHARACTER  NERS_CONFIG*128, CPARM*16
      REAL*8     PARS(NERS__MPAR), TAI_EOP, UTC_EOP
      LOGICAL*1  FL_CUR, LEX
      INTEGER*4  J1, J2, J3, NS, IVRB, MJD, NOPT, L_PAR, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, TIME
      CHARACTER, EXTERNAL :: TIM_TO_DATE*23
!
      CALL GETENVAR ( 'HOME', NERS_CONFIG )
      NERS_CONFIG = TRIM(NERS_CONFIG)//'/.ners_config'
      INQUIRE ( FILE=NERS_CONFIG, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           NERS_CONFIG = NERS__CONFIG
      END IF
!
      IUER = -1
      CALL NERS_INIT ( NERS_CONFIG, NERS, -1.0D0, -1.0D0, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4001, IUER, 'UT1MTAI', 'Error in initializing '// &
     &         'NERS data structure' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL NERS_GET_EOP ( NERS, -1.0D15, 'ut1mtai', NERS__MPAR, L_PAR, PARS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 4003, IUER, 'UT1MTAI', 'Error evaluating the '// &
     &         'Earth orientation parameter' )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, 110 ) PARS(1)
 110  FORMAT ( F10.6 )
      END  PROGRAM    UT1MTAI  !#!  
