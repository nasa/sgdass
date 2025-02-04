      SUBROUTINE SOLVE_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation general parameters for SOLVE defined in glbc4.i     *
! *   SOLVE_DEFAULT firstly makes system-wide initialization and then    *
! *   looks for environment variable. If corresponding environment       *
! *   variables are set up their values will override system-wide        *
! *   defaults.                                                          *
! *                                                                      *
! *  ### 18-AUG-1997  SOLVE_DEFAULT  v2.10 (c) L. Petrov 23-DEC-2018 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, IER, IVAL
      REAL*8     DVAL
      CHARACTER  STR*80, STR1*80
      INTEGER*4, EXTERNAL ::ILEN, I_LEN
!
! --- Set sigmas of constraints
!
      CALL CNS_DEFAULT()
!
      KBSL_CONST = .TRUE.  ! Constraints on baseline dependent clocks
!
! --- Then: system-wide defaults
!
      QUALCODE_GOOD_LIM = QUALCODE_GOOD_LIM__DEF
      SOLVE_EDITOR      = SOLVE_EDITOR__DEF
!@      SOLVE_PS_VIEWER   = SOLVE_PS_VIEWER__DEF
      SOLVE_EMULATION   = SOLVE_EMULATION__DEF
      SEG_OUTPUT        = SEG_OUTPUT__DEF
      COND_WARNING      = COND_WARNING__DEF
      TRAIN             = TRAIN__DEF
      NORATE_FLAG       = NORATE__DEF
      APRIORI_ZENDEL    = APRIORI_ZENDEL__DEF
      SRC_COO_SIGMA     = SRC_COO_SIGMA__DEF
      ERR_FUDGE_FACTOR  = ERR_FUDGE_FACTOR__DEF
!
      ESTIMATE_NUTATION_FIRST = ESTIMATE_NUTATION_FIRST__DEF
      ESTIMATE_STATION_FIRST  = ESTIMATE_STATION_FIRST__DEF
      ESTIMATE_EOP_FIRST      = ESTIMATE_EOP_FIRST__DEF
      ESTIMATE_UT1_RATE_FIRST = ESTIMATE_UT1_RATE_FIRST__DEF
!
      FL_NRD_TABLE = FL_NRD_TABLE__DEF
      FL_CHI_TABLE = FL_CHI_TABLE__DEF
      SRC_LISTING_STYLE = SRC_LISTING_STYLE__DEF
      SEG_LISTING_STYLE = SEG_LISTING_STYLE__DEF
      CALL CLRCH ( VTD_CONF_SES )
      FL_VTD_SES = .FALSE.
      IONO_ERR_FCT = 0.0D0
!
! --- Examining QUALCODE_GOOD_LIM
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'QUALCODE_GOOD_LIM', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           CALL IFOR_MEN ( STR, IVAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6861, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable QUALCODE_GOOD_LIM has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( IVAL .LT. 1  .OR.  IVAL .GT. 9  ) THEN
                CALL ERR_LOG ( 6862, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable QUALCODE_GOOD_LIM '//STR//' is out of range '// &
     &              '[1, 9]' )
                RETURN
           END IF
           QUALCODE_GOOD_LIM = IVAL
      END IF
!
! --- Examining SOLVE_EDITOR
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'SOLVE_EDITOR', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           IF ( ILEN(STR) .GT. LEN(SOLVE_EDITOR) ) THEN
                CALL CLRCH ( STR1 )
                CALL INCH  ( LEN(SOLVE_EDITOR), STR1 )
                CALL ERR_LOG ( 6863, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable SOLVE_EDITOR "'//STR//'" is too long: longer '// &
     &              'than '//STR1(1:I_LEN(STR1))//' characters' )
                RETURN
           END IF
           SOLVE_EDITOR = STR
      END IF
!@!
!@! --- Examining SOLVE_PS_VIEWER
!@!
!@      CALL CLRCH ( STR )
!@      CALL GETENVAR ( 'SOLVE_PS_VIEWER', STR )
!@      IF ( ILEN(STR) .NE. 0 ) THEN
!@           IF ( ILEN(STR) .GT. LEN(SOLVE_PS_VIEWER) ) THEN
!@                CALL CLRCH ( STR1 )
!@                CALL INCH  ( LEN(SOLVE_PS_VIEWER), STR1 )
!@                CALL ERR_LOG ( 6864, IUER, 'SOLVE_DEFAULT', 'Environment '// &
!@     &              'variable SOLVE_PS_VIEWER "'//STR//'" is too long: '// &
!@     &              'longer than '//STR1(1:I_LEN(STR1))//' characters' )
!@                RETURN
!@           END IF
!@           SOLVE_PS_VIEWER = STR
!@      END IF
!
! --- Examining SOLVE_EMULATION
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'SOLVE_EMULATION', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:2) .EQ. 'NO' ) STR = '0       '
           CALL IFOR_MEN ( STR, IVAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6865, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  SOLVE_EMULATION  has wrong value: '//STR )
                RETURN
           END IF
!
           IF ( IVAL .NE. 0  .AND.  IVAL .NE. 9612  ) THEN
                CALL ERR_LOG ( 6866, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  SOLVE_EMULATION has wrong value:  '//STR// &
     &              ' not 0 or 9612' )
                RETURN
           END IF
           SOLVE_EMULATION = IVAL
      END IF
!
! --- Examining SEG_OUTPUT
!
      CALL CLRCH ( STR )
      CALL GETENVAR ( 'SEG_OUTPUT', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'YES'  .OR.  STR(1:2) .EQ. 'ON' ) THEN
                SEG_OUTPUT = .TRUE.
             ELSE IF ( STR(1:2) .EQ. 'NO'  .OR.  STR(1:3) .EQ. 'OFF' ) THEN
                SEG_OUTPUT = .FALSE.
             ELSE
                CALL ERR_LOG ( 6867, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  SEG_OUTPUT has wrong value:  '//STR//' not'// &
     &              ' YES or ON or NO or OFF' )
                RETURN
           END IF
      END IF
!
! --- Examining COND_WARNING
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'COND_WARNING', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, DVAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6868, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  COND_WARNING  has wrong value: '//STR )
                RETURN
           END IF
           COND_WARNING = DVAL
      END IF
!
! --- Examining ESTIMATE_NUTATION_FIRST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ESTIMATE_NUTATION_FIRST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'YES'  .OR.  STR(1:2) .EQ. 'ON' ) THEN
                ESTIMATE_NUTATION_FIRST = .TRUE.
             ELSE IF ( STR(1:2) .EQ. 'NO'  .OR.  STR(1:3) .EQ. 'OFF' ) THEN
                ESTIMATE_NUTATION_FIRST = .FALSE.
             ELSE
                CALL ERR_LOG ( 6869, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  ESTIMATE_NUTATION_FIRST  has wrong value: '// &
     &               STR//' not YES or ON or NO or OFF' )
                RETURN
           END IF
      END IF
!
! --- Examining ESTIMATE_UT1_RATE_FIRST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ESTIMATE_UT1_RATE_FIRST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'YES'  .OR.  STR(1:2) .EQ. 'ON' ) THEN
                ESTIMATE_UT1_RATE_FIRST = .TRUE.
             ELSE IF ( STR(1:2) .EQ. 'NO'  .OR.  STR(1:3) .EQ. 'OFF' ) THEN
                ESTIMATE_UT1_RATE_FIRST = .FALSE.
             ELSE
                CALL ERR_LOG ( 6870, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  ESTIMATE_UT1_RATE_FIRST  has wrong value: '// &
     &               STR//' not YES or ON or NO or OFF' )
                RETURN
           END IF
      END IF
!
! --- Examining ESTIMATE_STATION_FIRST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ESTIMATE_STATION_FIRST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'YES'  .OR.  STR(1:2) .EQ. 'ON' ) THEN
                ESTIMATE_STATION_FIRST = .TRUE.
             ELSE IF ( STR(1:2) .EQ. 'NO'  .OR.  STR(1:3) .EQ. 'OFF' ) THEN
                ESTIMATE_STATION_FIRST = .FALSE.
             ELSE
                CALL ERR_LOG ( 6871, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  ESTIMATE_STATION_FIRST  has wrong value: '// &
     &               STR//' not YES or ON or NO or OFF' )
                RETURN
           END IF
      END IF
!
! --- Examining ESTIMATE_EOP_FIRST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'ESTIMATE_EOP_FIRST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'YES'  .OR.  STR(1:2) .EQ. 'ON' ) THEN
                ESTIMATE_EOP_FIRST = .TRUE.
             ELSE IF ( STR(1:2) .EQ. 'NO'  .OR.  STR(1:3) .EQ. 'OFF' ) THEN
                ESTIMATE_EOP_FIRST = .FALSE.
             ELSE
                CALL ERR_LOG ( 6872, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  ESTIMATE_EOP_FIRST  has wrong value: '// &
     &               STR//' not YES or ON or NO or OFF' )
                RETURN
           END IF
      END IF
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'SRC_COO_SIGMA', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           IF ( INDEX ( STR, '.' ) == 0 ) STR = STR(1:I_LEN(STR))//'.0'
           READ ( UNIT=STR, FMT='(F12.6)' ) SRC_COO_SIGMA
           IF ( SRC_COO_SIGMA > 0.0D0 ) THEN
                KSRC_CONST = .TRUE.
              ELSE
                KSRC_CONST = .FALSE.
           END IF
         ELSE 
           KSRC_CONST = .FALSE.
      END IF
!
! --- Examining NORATE_FLAG
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'NORATE_FLAG', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'YES'  .OR.  STR(1:2) .EQ. 'ON' ) THEN
                NORATE_FLAG = .TRUE.
             ELSE IF ( STR(1:2) .EQ. 'NO'  .OR.  STR(1:3) .EQ. 'OFF' ) THEN
                NORATE_FLAG = .FALSE.
             ELSE
                CALL ERR_LOG ( 6873, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  NORATE_FLAG  has wrong value: '// &
     &               STR//' not YES or ON or NO or OFF' )
                RETURN
           END IF
      END IF
!
! --- Examining EQUMEM_FLAG
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'EQUMEM_FLAG', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:3) .EQ. 'YES'  .OR.  STR(1:2) .EQ. 'ON' ) THEN
                EQUMEM_FLAG = .TRUE.
             ELSE IF ( STR(1:2) .EQ. 'NO'  .OR.  STR(1:3) .EQ. 'OFF' ) THEN
                EQUMEM_FLAG = .FALSE.
             ELSE
                CALL ERR_LOG ( 6874, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  EQUMEM_FLAG  has wrong value: '// &
     &               STR//' not YES or ON or NO or OFF' )
                RETURN
           END IF
      END IF
!
! --- Examining LISTING_SRC_STAT
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'LISTING_SRC_STAT', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR .EQ. 'PRE2004' ) THEN
                SRC_LISTING_STYLE = SRC_PRE2004_SPOOL__FMT 
             ELSE IF ( STR .EQ. 'SHORT' ) THEN
                SRC_LISTING_STYLE = SRC_SHORT_SPOOL__FMT 
             ELSE IF ( STR .EQ. 'LONG' ) THEN
                SRC_LISTING_STYLE = SRC_LONG_SPOOL__FMT 
             ELSE IF ( STR .EQ. 'POST2021' ) THEN
                SRC_LISTING_STYLE = SRC_POST2021_SPOOL__FMT
             ELSE IF ( STR .EQ. 'POST2024' ) THEN
                SRC_LISTING_STYLE = SRC_POST2024_SPOOL__FMT
             ELSE
                CALL ERR_LOG ( 6875, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  LISTING_SRC_STAT  has wrong value: '// &
     &               STR//' -- only PRE2004, SHORT and LONG are supported' )
                RETURN
           END IF
      END IF
!
! --- Examining LISTING_SEG_STYLE variable
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'LISTING_SEG_STYLE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR .EQ. 'SEG_PRE2005' ) THEN
                SEG_LISTING_STYLE = SEG_PRE2005_SPOOL__FMT 
             ELSE IF ( STR .EQ. 'SEG_POST2005' ) THEN
                SEG_LISTING_STYLE = SEG_PRE2005_SPOOL__FMT 
             ELSE
                CALL ERR_LOG ( 6876, IUER, 'SOLVE_DEFAULT', 'Environment '// &
     &              'variable  LISTING_SEG_STYLE has wrong value: '// &
     &               STR//' -- only SEG_PRE2005 and SEG_POST2005 are supported' )
                RETURN
           END IF
      END IF
!
      CALL CLRCH ( BATCH_CNF_FINAM )
      CALL CLRCH ( VTD_CONF_SES    )
      CALL CLRCH ( VTD_CONF_GLB    )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  SUBROUTINE  SOLVE_DEFAULT  !#!#
