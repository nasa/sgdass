#include <mk5_preprocessor_directives.inc>
      PROGRAM    DCM_APPLY
! ************************************************************************
! *                                                                      *
! *   Program EDC APPLY is for creating a directory of output decimation *
! *   files in accordance with the configuration file.                   *
! *                                                                      *
! *  ### 26-OCT-2007   DCM_APPLY   v1.0 (c)  L. Petrov  26-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      TYPE     ( EDC__TYPE     ) :: EDC
      TYPE     ( DCM__TYPE     ) :: DCM
      CHARACTER  DCM_CONFIG*128, DB_NAME*16, EDC_DIR*128
      CHARACTER  BUF(DCM__M_OBJ)*128, STR*128
      LOGICAL*4  FL_OUT_TERM 
      INTEGER*4  NBUF, J1, ID, IP, I_EDC, COUNTER(DCM__M_OBJ), IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
      LOGICAL*4, EXTERNAL :: FUNC_ISATTY
!
      CALL SET_SIGNAL_CTRLC ( 1 )
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: edc_apply {configuration_file}'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, DCM_CONFIG )
      END IF
#ifdef SUN
      FL_OUT_TERM = FUNC_ISATTY ( 0 ) ! Flag whether the unit 6 is a terminal
#else
      FL_OUT_TERM = FUNC_ISATTY ( 6 ) ! Flag whether the unit 6 is a terminal
#endif
!
      IUER = -1
      CALL DCM_PARSE_CONFIG ( DCM_CONFIG, DCM, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6301, -2, 'DCM_APPLY', 'Error in an attempt '// &
     &         'to parse configuration file '//DCM_CONFIG )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DCM_LISTS ( DCM, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6302, -2, 'DCM_APPLY', 'Error in an attempt '// &
     &         'to read lists' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the list of input files
!
      CALL RD_TEXT  ( DCM%FILIN_LIST, DCM__M_OBJ, BUF, NBUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6303, -2, 'DCM_APPLY', 'Error reading '// &
     &         'file template external decimation file list '//DCM%FILIN_LIST )
           CALL EXIT ( 1 )
      END IF
!
      I_EDC = 0
      CALL NOUT_I4 ( DCM__M_OBJ, COUNTER )
      DO 410 J1=1,NBUF
         IF ( DCM%SESS_RESET ) CALL NOUT_I4 ( DCM__M_OBJ, COUNTER )
         CALL CHASHL ( BUF(J1) )
         IF ( BUF(J1)(1:1) == ' ' ) GOTO 410
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
!
         ID = LINDEX ( BUF(J1), '/' ) 
         IF ( ID .LE. 0 ) THEN
              EDC_DIR = './'
              DB_NAME = BUF(J1)
            ELSE IF ( ID == 1 ) THEN
              EDC_DIR = '/'
              DB_NAME = BUF(J1)(2:)
            ELSE 
              EDC_DIR = BUF(J1)(1:ID-1)
              DB_NAME = BUF(J1)(ID+1:)
         END IF
         IP = INDEX ( DB_NAME, '.' )
         IF ( IP > 0 ) CALL CLRCH ( DB_NAME(IP:) )
!
         I_EDC = I_EDC + 1
         IUER = -1
         CALL EDC_READ ( DB_NAME, EDC_DIR, EDC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6304, -2, 'DCM_APPLY', 'Error in reading '// &
     &            'the '//STR(1:I_LEN(STR))//'-th external decimation '// &
     &            'template file '//DB_NAME(1:I_LEN(DB_NAME))// &
     &            ' from input directory '//EDC_DIR )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL DCM_PERFORM ( DCM, EDC, COUNTER, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6305, -2, 'DCM_APPLY', 'Error in performing '// &
     &            'decimation for the '//STR(1:I_LEN(STR))//'-th external '// &
     &            'decimation template file '//DB_NAME(1:I_LEN(DB_NAME))// &
     &            ' from input directory '//EDC_DIR )
              CALL EXIT ( 1 )
         END IF
!
         IUER = -1
         CALL EDC_WRITE ( EDC, DCM%OUTDIR, DCM%EDC_TYP, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6306, -2, 'DCM_APPLY', 'Error in writing '// &
     &            'the '//STR(1:I_LEN(STR))//'-th external decimation '// &
     &            'template file '//DB_NAME(1:I_LEN(DB_NAME))// &
     &            ' from directory '//DCM%OUTDIR )
              CALL EXIT ( 1 )
         END IF
         IF ( FL_OUT_TERM ) THEN
              IF ( MOD(I_EDC ,10) == 0 ) THEN
                   WRITE ( 6, 110 ) I_EDC, ' files processed  '//CHAR(13)
                   CALL FLUSH ( 6 )
 110               FORMAT ( 4X, I5, A $ )
              END IF
         END IF
 410  CONTINUE 
!
      WRITE ( 6, 120 ) I_EDC, DCM%OUTDIR(1:I_LEN(DCM%OUTDIR))
 120  FORMAT ( I4,' external decimation files were created in ', A )
!
      END PROGRAM   DCM_APPLY !#!#
