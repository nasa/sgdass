#include <mk5_preprocessor_directives.inc>
      PROGRAM    SIMUL_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = SIMUL__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL SIMUL_MAIN()
      END  PROGRAM  SIMUL_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  SIMUL_MAIN()
! ************************************************************************
! *                                                                      *
! *   Program  SIMUL_MAIN  creates simulation VLBI database.             *
! *                                                                      *
! *                                                                      *
! *   Usage: -vex|-gvf|-vda input_file -gvf|-vda output_file mode        *
! *                                                                      *
! *   1st argument: input file format. Supported formats:                *
! *      -vex -- VLBI schedule file in vex format.                       *
! *      -gvf -- VLBI database file in GVF format.                       *
! *      -vda -- VLBI database file in VDA format.                       *
! *   2nd argument: input file.                                          *
! *   3rd argument: output file format. Supported formats:               *
! *      -gvf -- VLBI database file in GVF format.                       *
! *      -vda -- VLBI database file in VDA format.                       *
! *   4th argument: output file.                                         *
! *   5th format of the renaming table. -tab is the only supported       *
! *       format.                                                        *
! *   6th file name with station/source renaming table.                  *
! *   7th argument: mode. Supported modes:                               *
! *       1 -- right hand side of the output database is random with     *
! *            rms=10ps.                                                 *
! *                                                                      *
! *   If the input file is database, the output file is also database    *
! *   with COR_TYPE lcode Simul. That is the marker that the database    *
! *   is simulation. Variables GR_DELAY and GRDELERR are replaced with   *
! *   simulated values.                                                  *
! *                                                                      *
! *   If the output file is a database in VDA format, then its name      *
! *   is supposed to be the full path file name.                         *
! *                                                                      *
! *   If the output file is a database in GVF format, then its name      *
! *   can be either in 10-character or 15 character form:                *
! *   YYYYMMDD-S or YYYYMMDD-S_vVVV, where                               *
! *   YYYY -- year, MM -- month of the year as an integer number,        *
! *   DD -- day of the month, S suffix in a range from a to z, VVV --    *
! *   an integer database version. The simulated database is placed in   *
! *   the simulation repository.                                         *
! *                                                                      *
! *  ### 08-JUN-2020  SIMUL_MAIN   v2.0 (c)  L. Petrov  01-DEC-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'simul.i'
      TYPE     ( SIMUL__TYPE ) :: SIMUL
      INTEGER*4  MT, M_ARG
      PARAMETER  ( MT = 1024 )
      PARAMETER  ( M_ARG = 10 )
      CHARACTER  KEYWORD*32, VALUE*128, FMT_IN*6, FIL_IN*128, &
     &           FMT_OUT*6, FIL_OUT*128, SIMUL_CONF_FIL*128, STR_6*16
      CHARACTER  ARGS(M_ARG)*128, RENAME_TAB(MT)*128
      REAL*8     DIL_FACTOR, WHT_NOISE
      LOGICAL*1  LEX
      INTEGER*8  DIR_DESC
      INTEGER*4  J1, LT, ISEED, IVRB, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: OPENDIR 
!
!     simul -i-gvf 20130724_p_v002 -o-gvf 20130724_x -c /sol/qua/bas_2021b/bas_2021b.vtd -s $Vs/rv/empty.tab -m cov_0.0011 -w 10.0 -v 2 -d /t0/7km/zpd_cov_summer -r 28333
!
      ISEED = 20381095
      CALL CLRCH ( FIL_IN  ) 
      CALL CLRCH ( FIL_OUT ) 
!
      IF ( IARGC() < 6 ) THEN 
           WRITE ( 6, '(A)' ) 'Usage: -i-vex|-i-gvf|-i-vda input_file -o-gvf|-o-vda output_file '// &
     &                        '-c config_file [-v verb] [-r seed]'
           CALL EXIT ( 1 ) 
         ELSE
           DO 410 J1=1,2*IARGC()
              CALL GETARG ( 1 + (J1-1)*2, KEYWORD )
              CALL GETARG ( 2 + (J1-1)*2, VALUE   )
              IF ( KEYWORD == '-i-vex' .OR. KEYWORD == '-i-gvf' .OR. KEYWORD == '-i-vda' ) THEN
                   FMT_IN = KEYWORD
                   FIL_IN = VALUE
                   IF ( FMT_IN == '-i-vex' ) THEN
                        CONTINUE 
                      ELSE IF ( FMT_IN == '-i-gvf' ) THEN
                        CONTINUE 
                      ELSE IF ( FMT_IN == '-i-vda' ) THEN
                        CONTINUE 
                      ELSE
                        IUER = -1
                        CALL ERR_LOG ( 1401, IUER, 'SIMUL_MAIN', 'Wrong input data type argument '// &
     &                       TRIM(FMT_IN)//' -- supported formats -i-vex, -i-gvf, -i-vda' )
                        CALL EXIT ( 1 )
                   END IF
!
                   IF ( KEYWORD == '-i-vex' .OR. KEYWORD == '-i-vda' ) THEN
                        INQUIRE ( FILE=FIL_IN, EXIST=LEX )
                        IF ( .NOT. LEX ) THEN
                             IUER = -1
                             CALL ERR_LOG ( 1402, IUER, 'SIMUL_MAIN', 'Cannot find input file '//FIL_IN )
                             CALL EXIT ( 1 )
                        END IF
                   END IF
                ELSE IF ( KEYWORD == '-o-gvf' .OR. KEYWORD == '-o-vda' ) THEN
                   FMT_OUT = KEYWORD
                   FIL_OUT = VALUE
                   IF ( FMT_OUT == '-o-gvf' ) THEN
                        CONTINUE
                      ELSE IF ( FMT_OUT == '-o-vda' ) THEN
                        CONTINUE
                      ELSE
                        IUER = -1
                        CALL ERR_LOG ( 1403, IUER, 'SIMUL_MAIN', 'Wrong output data type argument '// &
     &                       TRIM(FMT_OUT)//' -- supported formats -o-gvf, -o-vda' )
                        CALL EXIT ( 1 )
                   END IF
                ELSE IF ( KEYWORD(1:2) == '-c' ) THEN
                   SIMUL_CONF_FIL = VALUE
                   INQUIRE ( FILE=SIMUL_CONF_FIL, EXIST=LEX )
                   IF ( .NOT. LEX ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 1404, IUER, 'SIMUL_MAIN', 'Cannot find configuration file '// &
     &                       SIMUL_CONF_FIL )
                        CALL EXIT ( 1 )
                   END IF
                ELSE IF ( KEYWORD(1:2) == '-r' ) THEN
                   CALL CHIN ( VALUE, ISEED )
                   IF ( ISEED < 0 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 1405, IUER, 'SIMUL_MAIN', 'Wrong value of random '// &
     &                      'number seed '//TRIM(VALUE)//' -- it should be a positive '// &
     &                      'integer number' )
                        CALL EXIT ( 1 )
                   END IF
                ELSE IF ( KEYWORD(1:2) == '-v' ) THEN
                   CALL CHIN ( VALUE, IVRB )
                   IF ( IVRB < 0 .OR. IVRB > 127 ) THEN
                        IUER = -1
                        CALL ERR_LOG ( 1406, IUER, 'SIMUL_MAIN', 'Wrong value of verbosity '// &
     &                      'parameter '//TRIM(VALUE)//' -- it should be in [0, 127] range' )
                        CALL EXIT ( 1 )
                   END IF
              END IF
 410       CONTINUE
      END IF
      IF ( ILEN(FIL_OUT) == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1407, IUER, 'SIMUL_MAIN', 'The output file has not been '// &
     &         'specified. Please check your command line. Accepted options are '// &
     &         '-o-vda or -o-gvf' )
           CALL EXIT ( 1 )
      END IF
!
! --- Initialization of SIMUL object
!
      CALL NOUT ( SIZEOF(SIMUL), SIMUL )
      SIMUL%COV => NULL()
!
! --- Parse simul configuration file
!
      IUER = -1
      CALL SIMUL_CONF_PARSE ( SIMUL, SIMUL_CONF_FIL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1408, IUER, 'SIMUL_MAIN', 'Error in attempt to '// &
     &         'parse VLBI simulation files '//SIMUL_CONF_FIL )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL SIMUL_DO ( SIMUL, FMT_IN, FIL_IN, FMT_OUT, FIL_OUT, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1409, IUER, 'SIMUL_MAIN', 'Failure in an attempt to '// &
     &         'generate a simulated dataset from input file '//FIL_IN ) 
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, * ) 'Created the output dataset '//TRIM(FIL_OUT)
      CALL EXIT ( 0 ) 
      END  SUBROUTINE  SIMUL_MAIN  !#!#
