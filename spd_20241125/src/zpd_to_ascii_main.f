#include <mk5_preprocessor_directives.inc>
      PROGRAM    ZPD_TO_ASCII_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = SPD__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL ZPD_TO_ASCII_MAIN()
      END  PROGRAM  ZPD_TO_ASCII_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ZPD_TO_ASCII_MAIN()
! ************************************************************************
! *                                                                      *
! *   Program  ZPD_TO_ASCII
! *                                                                      *
! * # 10-OCT-2021  ZPD_TO_ASCII_MAIN v1.0 (c) L. Petrov 10-OCT-2021 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'heb.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( HEB__TYPE    ) :: HEB_GEOID_BSPL, HEB_ZPD
      REAL*8     EDGE_SEC
      PARAMETER  ( EDGE_SEC = 0.0D0 ) 
      INTEGER*4     M_MOD, M_INP
      PARAMETER  (  M_MOD = 128 ) 
      PARAMETER  (  M_INP = 128 ) 
      CHARACTER  FIL_CONF*128, FIL_ZPD*128, OUT_PREF*128, STR*128
      CHARACTER  MOD_TEXT(M_MOD)*128, INP_TEXT(M_INP)*128
      CHARACTER  BUF_STA(SPD__M_STA)*128, OUT(SPD__M_STA)*128
      INTEGER*4  J1, J2, J3, J4, IVRB, NS, N_MOD, N_INP,  IUER
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: zpd_to_ephedisp {fil_conf} {fil_zpd} {out_pref} '// &
     &                        '[verbosity]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_CONF )
           CALL GETARG ( 2, FIL_ZPD  )
           CALL GETARG ( 3, OUT_PREF )
           IF ( IARGC() > 3 ) THEN
                CALL GETARG ( 4, STR )
                CALL CHIN   ( STR, IVRB )
              ELSE
                IVRB = 0
           END IF
      END IF
!
! --- Read and parse station file
!
      IUER = -1
      CALL SPD_3D_CONF ( FIL_CONF, SPD, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5301, -2, 'ZPD_TO_ASCII_MAIN', 'Failure '// &
     &         'in parsing input configuration file '//FIL_CONF )
           CALL EXIT ( 1 )
      END IF
!
! --- Read and parse station file
!
      IUER = -1
      CALL SPD_3D_INP_STA ( SPD, EDGE_SEC, HEB_GEOID_BSPL, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5302, -2, 'ZPD_TO_ASCII_MAIN',  'Failure '// &
     &         'in loading the list of stations' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_ZPD, HEB_ZPD, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5303, -2, 'ZPD_TO_ASCII_MAIN',  'Failure '// &
     &         'in reading input file '//FIL_ZPD )
           CALL EXIT ( 1 )
      END IF
!
! --- Compute zenith path delay at positions of the stations
!
      CALL ZPD_TO_ASCII ( SPD, HEB_ZPD, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5304, -2, 'ZPD_TO_ASCII_MAIN',  'Failure '// &
     &         'in computation of the zenith path delay for requested '// &
     &         'stations' )
           CALL EXIT ( 1 )
      END IF
!
! --- Write down results
!
      N_MOD = 0
      N_MOD = N_MOD + 1; MOD_TEXT(N_MOD) = &
    &        'Zenith path delays interpolated from the global grid'
!
      N_INP = 0
      N_INP = N_INP + 1; INP_TEXT(N_INP) = 'Name of the input atmospheric '// &
     &       'data: '
      N_INP = N_INP + 1; INP_TEXT(N_INP) = '  '//HEB_ZPD%TITLE
      N_INP = N_INP + 1; INP_TEXT(N_INP) = 'Institution that provided '// &
     &                 'atmospheric data: '
      N_INP = N_INP + 1; INP_TEXT(N_INP) = '  '//HEB_ZPD%INSTITUTION
      N_INP = N_INP + 1; INP_TEXT(N_INP) = 'Reference to the source of '// &
     &                 'atmospheric data: '
      N_INP = N_INP + 1; INP_TEXT(N_INP) = '  '//HEB_ZPD%REFERENCES

      IUER = -1
      CALL SPD_3D_WRITE ( SPD__WRITE_ASC, 1, SPD, OUT_PREF, &
     &                    N_MOD, MOD_TEXT, N_INP, INP_TEXT, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5305, -2, 'ZPD_TO_ASCII_MAIN',  'Failure in '// &
     &         'writing the output file with zenith path delays' )
           CALL EXIT ( 1 )
      END IF
      CALL SPD_FREE ( SPD, 1 ) 
      CALL SPD_FREE ( SPD, 2 ) 
!
      END  SUBROUTINE   ZPD_TO_ASCII_MAIN  !#!#
