      SUBROUTINE SET_VERSION ( PROGNAM, DAT, COMMENT )
! ************************************************************************
! *                                                                      *
! *   Program  SET_VERSION  set a program name, date of the last         *
! *   modification and comment. This information is stored in PVERS      *
! *   common ares, stored on disk in the file VERSxx.                    *
! *                                                                      *
! *   SET_VERSION supports 4 modes                                       *
! *                                                                      *
! *   1) Normal mode (the first two character of variable COMMENT are    *
! *      not "--", "##" or "??") :                                       *
! *      It is assumed that                                              *
! *      a) program name is up to 5 letters of upper case long;          *
! *      b) date is in SOLVE format: yyyy.mm.dd, f.e. 1999.10.19         *
! *      c) comment is ' ' (empty) for a stable, standard version and    *
! *         not   empty (arbitrary text up to 32 letters which doesn't   *
! *         start from ## or ?? or -- ) for test versions.               *
! *                                                                      *
! *   2) External mode.                                                  *
! *      If the first two characters of the argument COMMENT are "--"    *
! *      then the name of the prognam is not sought in pvser.i .         *
! *      The name of the program may be up to 32 scharacters long.       *
! *      PVERS is neither read nor written.                              *
! *                                                                      *
! *   3) Total initilaiszation.                                          *
! *      If the first two symbols of the argument COMMENT are "??" then  *
! *      PVERS is not read. Comments and dates are initialized by blanks *
! *      for all other program names, PVERS is written and a routine     *
! *      executes return. Fields PROGNAM and DAT are ignored.            *
! *                                                                      *
! *   4) Partial initialiuzation.                                        *
! *      If the first two symbols of the argument COMMENT are "##" then  *
! *      versions and comments are initialized by blanks for all other   *
! *      program names.                                                  *
! *                                                                      *
! *   5) Bypass writing PVERS file.                                      *
! *      If the first four symbols of COMMENT are --@@  then PVERS file  *
! *      is not read and written.                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * PROGNAM ( CHARACTER ) -- Name of the program. Up to 5 characters in  *
! *                          normal mode and up to 32 characters in      *
! *                          external mode.                              *
! *     DAT ( CHARACTER ) -- Date of the last modification in internal   *
! *                          SOLVE format: yyyy.mm.dd -- 10 symbols.     *
! * COMMENT ( CHARACTER ) -- Comment -- a 32-symbols string. Should be   *
! *                          empty for a stable versions.                *
! *                                                                      *
! *  ###  11-OCT-1999  SET_VERSION  v2.4  (c) L. Petrov 04-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'pvers.i'
      EXTERNAL   INIT_PVERS
      CHARACTER  PROGNAM*(*), DAT*(*), COMMENT*(*)
      CHARACTER  STR*128
      LOGICAL*4  FL_FOUND, FL_EXTERNAL, FL_PVERS
      INTEGER*4  J1, J2
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get WORK_DIR
!
      CALL GETENVAR ( 'PSOLVE_WORK_DIR', STR )
      IF ( ILEN(STR) .GT. 0 ) THEN
           PRE_SCR_DIR = STR
         ELSE
           PRE_SCR_DIR = SOLVE_WORK_DIR
      END IF
      PRE_SD_LEN = ILEN(PRE_SCR_DIR)
      IF ( PRE_SD_LEN .EQ. 0 ) PRE_SD_LEN = 1
      IF ( PRE_SCR_DIR(PRE_SD_LEN:PRE_SD_LEN) .NE. '/' ) THEN
           PRE_SD_LEN=PRE_SD_LEN+1
           PRE_SCR_DIR(PRE_SD_LEN:PRE_SD_LEN) = '/'
      ENDIF
!
      IF ( LEN(COMMENT) .GE. 2 ) THEN
           IF ( COMMENT(1:2) .EQ. '??'  ) THEN
!
! ------------- Special case: initializing dates and comments
!
                DO 410 J1=1,NUM_PROGS
                   CALL CLRCH ( PROG_DATES(J1)    )
                   CALL CLRCH ( PROG_COMMENTS(J1) )
 410            CONTINUE
                CALL USE_PVERS ( 'OWC' )
                RETURN  ! that's all
           END IF
      END IF
!
      CALL CLRCH ( CUR_PROG      )
      CALL CLRCH ( CUR_PROG_LONG )
      CALL CLRCH ( CUR_DATE      )
      CALL CLRCH ( CUR_COMMENT   )
      CALL CLRCH ( CUR_VERSION   )
      FL_EXTERNAL = .FALSE.
!
      IF ( LEN(COMMENT) .GE. 2 ) THEN
           IF ( COMMENT(1:2) .EQ. '--' ) THEN
                FL_EXTERNAL = .TRUE. ! set flag: external mode on
                IF ( LEN(COMMENT) .GE. 3 ) THEN
                     CUR_VERSION = PROGNAM(1:I_LEN(PROGNAM))//' Ver. '//DAT// &
     &                             ' '//COMMENT(3:)
                     CUR_COMMENT   = COMMENT(3:)
                   ELSE
                     CUR_VERSION = PROGNAM(1:I_LEN(PROGNAM))//' Ver. '//DAT
                END IF
!
                CUR_PROG_LONG = PROGNAM
                CUR_DATE      = DAT
           END IF
      END IF
!
      IF ( .NOT. FL_EXTERNAL ) THEN
!
! -------- Reading PVERS
!
           CALL USE_PVERS ( 'OR' )
           CUR_PROG_LONG = PROGNAM
           CUR_PROG      = PROGNAM
           CUR_DATE      = DAT
           CUR_COMMENT   = COMMENT
!
           FL_FOUND = .FALSE.
!
           DO 420 J2=1,NUM_PROGS
              IF ( LEN(COMMENT) .GE. 2 ) THEN
                   IF ( COMMENT(1:2) .EQ. '##' ) THEN
!
! --------------------- Special case: initialize dates and comments
!
                        CALL CLRCH ( PROG_DATES(J2)    )
                        CALL CLRCH ( PROG_COMMENTS(J2) )
                        CALL CLRCH ( CUR_COMMENT )
                        IF ( LEN(COMMENT) .GE. 3 ) THEN
                             CUR_COMMENT   = COMMENT(3:)
                        END IF
                   END IF
              END IF
!
              IF ( PROG_NAMES(J2) .EQ. PROGNAM ) THEN
!
! ---------------- Usual case: set program date and program comment
!
                   PROG_DATES(J2)    = CUR_DATE
                   PROG_COMMENTS(J2) = CUR_COMMENT
                   FL_FOUND = .TRUE. ! set flag that the program name has been found
              ENDIF
 420       CONTINUE
!
           IF ( .NOT. FL_FOUND ) THEN
                CALL FERR ( INT2(6666), 'SET_VERSION: program '//PROGNAM// &
     &              ' has not been found among program names '// &
     &              'specified in ../include/pvers.i', INT2(0), INT2(0) )
                STOP '(set_version) Abnormal termination'
           END IF
           CUR_VERSION = CUR_PROG//' Ver. '//CUR_DATE//' '//CUR_COMMENT
!
           FL_PVERS = .TRUE.
           IF ( LEN(COMMENT) .GE. 4 ) THEN
                IF ( COMMENT(1:4) .EQ. '--@@' ) FL_PVERS = .FALSE.
           END IF
           IF ( .NOT. FL_PVERS ) THEN
                CALL USE_PVERS ( 'WC' )
              ELSE 
                CALL USE_PVERS ( 'C'  )
           END IF 
      END IF
!
      RETURN
      END  !#!  SET_VERSION  #!#
