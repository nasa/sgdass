      SUBROUTINE MDLPL_HLP ( HELP_NAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MDLPL_HLP  prints at the current device PGPLOT the text   *
! *   of the help file for MDLPL_EXT.                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * HELP_NAME ( CHARACTER ) -- Name of the help file. Its path name is   *
! *                            kept in environment variable SOLVE_HELP_DIR.    *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  03-NOV-97    MDLPL_HLP   v2.0  (c)  L. Petrov  28-JUL-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'diagi.i'
      CHARACTER  HELP_NAME*(*)
      INTEGER*4  IUER, IER, NHLP
      CHARACTER  BUF(MHLP)*80, HELP_FIL*255, HELP_TIT*32, SOLVE_HELP_DIR_STR*255, &
     &           CH*1
      REAL*4     XC, YC, XCT, YCT
      INTEGER*4  ILEN
!
      CALL CLRCH ( SOLVE_HELP_DIR_STR )
      CALL GETENVAR ( 'PSOLVE_HELP_DIR', SOLVE_HELP_DIR_STR )
      IF ( ILEN(SOLVE_HELP_DIR_STR) .GT. 0 ) THEN
!
! -------- If the last symbol of the value of environment variable is not
! -------- "/" then adding it to the end
!
           IF ( SOLVE_HELP_DIR_STR(ILEN(SOLVE_HELP_DIR_STR):ILEN(SOLVE_HELP_DIR_STR)) .NE. '/') &
     &          THEN
                SOLVE_HELP_DIR_STR(ILEN(SOLVE_HELP_DIR_STR)+1:) = '/'
           END IF
      END IF
!
! --- Setting file name for help file and the line of its title
!
      CALL CLRCH ( HELP_FIL )
      CALL CLRCH ( HELP_TIT )
      HELP_FIL = HELP_NAME
      IF ( ILEN(SOLVE_HELP_DIR_STR) .GT. 0  .AND. INDEX(HELP_FIL, '/') .EQ. 0 ) THEN
!
! -------- Adding the directory name before the name of help-file
!
           HELP_FIL = SOLVE_HELP_DIR_STR(1:ILEN(SOLVE_HELP_DIR_STR))//HELP_FIL
      END IF
!
! --- Reading the file with help information
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( HELP_FIL, MHLP, BUF, NHLP, IER )
      IF ( IER .NE.0 ) THEN
           CALL ERR_LOG ( 9251, IUER, 'MDLPL_HLP', 'Error during the '// &
     &         'attempt to read help file' )
           RETURN
      END IF
      CALL CHASHL ( BUF(1)   )
      CALL CLRCH  ( HELP_TIT )
      HELP_TIT = BUF(1)
!
 910  CONTINUE
!
! --- Deleting previous window
!
      CALL PGERAS()
!
      CALL PGSAVE() ! 1
!
! --- Setting new world coodrinates
!
      CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0 )
      CALL PGSWIN ( 0.0, 1.0, 0.0, 1.0 )
!
! --- Hard coded colours
!
      CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
      CALL PGCOL_RGB ( FRG_CLRI, FRG_CLR(1), FRG_CLR(2), FRG_CLR(3) )
      CALL PGCOL_RGB ( ERR_CLRI, ERR_CLR(1), ERR_CLR(2), ERR_CLR(3) )
      CALL PGCOL_RGB ( SLG_CLRI, SLG_CLR(1), SLG_CLR(2), SLG_CLR(3) )
      CALL PGCOL_RGB ( DEG_CLRI, DEG_CLR(1), DEG_CLR(2), DEG_CLR(3) )
!
! --- Printing by large letters the title of help information
!
      CALL PGSLW  ( ISLW_HLT )
      CALL PGSCF  ( ISCF_HLT )
      CALL PGSCH  (  SCH_HLT )
      XCT = 0.50
      YCT = 0.96
      CALL PGPTXT ( XCT, YCT, 0.0, 0.5, HELP_TIT )
!
! --- Shrinking a bit the viewing surface to leave the title unchanged when
! --- the text buffer will be printed
!
      XC = XCT
      YC = YCT
      CALL PGSVP  ( 0.0, 1.0, 0.0, 0.948 )
!
! --- Printing the content of help buffer at the graphic window.
!
      CALL PGSLW  ( ISLW_HLP )
      CALL PGSCF  ( ISCF_HLP )
      CALL PGSCH  (  SCH_HLP )
      CALL PGTBF  ( NHLP-2, BUF(3), CH ) ! Skipping first two lines
      CALL PGERAS()
!
! --- Restoring the current colour index and other PGPLOT current parameters
!
      CALL PGSCI  ( 1 )
      CALL PGUNSA() !
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MDLPL_HLP  #!#
