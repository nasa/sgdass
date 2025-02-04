      SUBROUTINE SET_PATHS()
! ************************************************************************
! *                                                                      *
! *   Routine SET_PATH sets environment variable PGPLOT_DIR, PGPLOT_FONT *
! *   and SOLVE_HELP_DIR if they have not been set up earlier in order to use  *
! *   library PGPLOT and on-line help files. It sets these values        *
! *   in according with values saved in include-file solve_paths.i which *
! *   is automatically generated during SOLVE-installation by script     *
! *   make_support. It assumed that the tageted directories are not      *
! *   moved since compilation time.                                      *
! *                                                                      *
! *   SET_PATH doesn't set new values of environmant variables if they   *
! *   were already set.                                                  *
! *                                                                      *
! *   Environment variables set up by SET_PATHS are visible only         *
! *   while the program called SET_PATHS is running. Their values are    *
! *   not kept for parents shell or for children processes.              *
! *                                                                      *
! *   It is assumed that SET_PATHS is called before the first usage of   *
! *   pgplot routines or usage of routines calling on-line help.         *
! *                                                                      *
! *  ###  05-JUL-98    SET_PATHS   v1.1  (c)  L. Petrov  06-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'solve_paths.i'
      INTEGER*2  IT, TRIMLEN
!
      CHARACTER  STR*128
      CHARACTER  PGPLOT_DIR_STR*128, PGPLOT_FONT_STR*128, SOLVE_HELP_DIR_STR*128
!
! --- Very important! These variables whould be always saved!!
!
      SAVE       PGPLOT_DIR_STR,  PGPLOT_FONT_STR,  SOLVE_HELP_DIR_STR
!
! --- Examining PGPLOT_DIR
!
      CALL GETENVAR ( 'PGPLOT_DIR', STR )
      IF ( STR(1:1) .EQ. ' ' ) THEN
!
! -------- Environment variable PGPLOT_DIR was not set up.
! -------- Create new environment variable PGPLOT_DIR
!
           PGPLOT_DIR_STR = 'PGPLOT_DIR'//'='//PGPLOT_DIR_DEF
           IT = TRIMLEN( PGPLOT_DIR_STR )
           PGPLOT_DIR_STR(IT+1:IT+1) = CHAR(0)
           CALL PUTENV ( %REF ( PGPLOT_DIR_STR(1:IT+1) ) )
      END IF
!
! --- Examining PGPLOT_FONT
!
      CALL GETENVAR ( 'PGPLOT_FONT', STR )
      IF ( STR(1:1) .EQ. ' ' ) THEN
!
! -------- Environment variable PGPLOT_FONT was not set up.
! -------- Create new environment variable PGPLOT_FONT
!
           PGPLOT_FONT_STR = 'PGPLOT_FONT'//'='//PGPLOT_FONT_DEF
           IT = TRIMLEN( PGPLOT_FONT_STR )
           PGPLOT_FONT_STR(IT+1:IT+1) = CHAR(0)
           CALL PUTENV ( %ref( PGPLOT_FONT_STR(1:IT+1) ) )
      END IF
!
! --- Examining SOLVE_HELP_DIR
!
      CALL GETENVAR ( 'PSOLVE_HELP_DIR', STR )
      IF ( STR(1:1) .EQ. ' ' ) THEN
!
! -------- Environment variable PSOLVE_HELP_DIR was not set up.
! -------- Create new environment variable PSOLVE_HELP_DIR
!
           SOLVE_HELP_DIR_STR = 'PSOLVE_HELP_DIR'//'='//SOLVE_HELP_DIR
           IT = TRIMLEN( SOLVE_HELP_DIR_STR )
           SOLVE_HELP_DIR_STR(IT+1:IT+1) = CHAR(0)
           CALL PUTENV ( %ref( SOLVE_HELP_DIR_STR(1:IT+1) ) )
      END IF
!
      RETURN
      END  !#!  SET_PATHS  #!#
