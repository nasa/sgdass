      FUNCTION   READ_LINE ( PROMPT, STR )
! ************************************************************************
! *                                                                      *
! *   Routine READ_LINE provides a Fortran interface to the GNU library  *
! *   readline. It reads a line from the keyboard and stores it in the   *
! *   buffer. It allows to edit the line.                                *
! *                                                                      *
! *   More about readline functionality can be found in                  *
! *   http://tiswww.case.edu/php/chet/readline/rltop.html                *
! *                                                                      *
! *   NB: Please be sure that the readline library is installed at       *
! *   your system. You need to add "-lreadline -ltermcap" to the list    *
! *   of libraries used by linker in your makefile.                      *
! *                                                                      *
! * --------------------------- Input parameters: ---------------------- *
! *                                                                      *
! *      PROMPT ( CHARACTER ) -- Prompt that will be displayed.          *
! *                              SHould be '', if no prompt is needed.   *
! *                                                                      *
! * --------------------------- Output parameters: --------------------- *
! *                                                                      *
! *         STR ( CHARACTER ) -- The line that the user entered.         *
! * <READ_LINE> ( INTEGER*4 ) -- The number of characters the user       *
! *                              entered.                                *
! *                                                                      *
! *  ### 26-NOV-2007    READ_LINE  v1.1 (c)  L. Petrov  11-APR-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  READ_LINE
      CHARACTER  PROMPT*(*), STR*(*)
      INTEGER*4  J1
      LOGICAL*4  FL_EMPTY
      INTEGER*4  LOC__SUN$$_STR
      ADDRESS__TYPE  IADR
      ADDRESS__TYPE, EXTERNAL :: READLINE 
!
! --- Get the address of the line that user typed
!
      IADR = READLINE ( PROMPT//CHAR(0) )
!
! --- Copy the line to the destination
!
#ifdef SUN
      CALL STRNCPY ( %VAL(LOC__SUN$$_STR(STR)), %VAL(IADR), %VAL(LEN(STR)) )
#else
      CALL STRNCPY ( %REF(STR), %VAL(IADR), %VAL(LEN(STR)) )
#endif
!
! --- Include the line into the history buffer
!
      CALL ADD_HISTORY ( %VAL(IADR) )
!
! --- Determine the length of the line. Add padding blanks, if necessary
!
      READ_LINE = LEN(STR)
      FL_EMPTY = .FALSE.
      DO 410 J1=1,LEN(STR)
         IF ( .NOT. FL_EMPTY  .AND. STR(J1:J1) == CHAR(0) ) THEN
              READ_LINE = J1-1
              FL_EMPTY = .TRUE.
         END IF
         IF ( FL_EMPTY ) THEN
              STR(J1:J1) = ' '
         END IF
 410  CONTINUE 
!
! --- Release dynamic memory
!
      CALL FREE ( IADR )
      RETURN
      END  FUNCTION   READ_LINE  !#!#
