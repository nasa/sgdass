      FUNCTION   MAKE_HELP_FINAM ( HELP_FILE, FINAM )
! ************************************************************************
! *                                                                      *
! *   Ancillary routine MAKE_HELP_FINAM makes full name of help-file.    *
! *   It triess 1) to add lgobal variable from glbc4.i SOLVE_HELP_DIR    *
! *   before filename; 2) to add environment variable SOLVE_HELP_DIR before    *
! *   filename; 3) to use HELP_FILE directly. If these attempts leed to  *
! *   resolving actual filename then MAKE_FILE_FINAM returns value 0 and *
! *   FINAM contains actual file name with path. In the case of failure  *
! *   MAKE_HELP_FINAM returns -1.                                        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   HELP_FILE ( CHARACTER ) -- main part of the name of help file.     *
!C *                                                                     *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <MAKE_HELP_FINAM> ( INTEGER*4 ) -- Flag of success: 0 -- success, -1 *
! *                                    failure.                          *
! *            FINAM  ( CHARACTER ) -- Full file name.                   *
! *                                                                      *
! *  ###  24-AUG-97  MAKE_HELP_FINAM   v1.0 (c) L. Petrov 24-AUG-97 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  MAKE_HELP_FINAM
      CHARACTER  HELP_FILE*(*), FINAM*(*), STR*255
      INTEGER*4  ILEN, I_LEN
      LOGICAL*4  L_EXIST
!
! --- Make filename with help menu
!
      CALL CLRCH ( FINAM )
      FINAM = SOLVE_HELP_DIR//HELP_FILE
      INQUIRE ( FILE=FINAM, EXIST=L_EXIST )
      IF ( .NOT. L_EXIST ) THEN
           CALL CLRCH ( STR )
           CALL GETENVAR ( 'PSOLVE_HELP_DIR', STR )
           IF ( ILEN(STR) .GT. 0 ) THEN
               IF ( STR(ILEN(STR):ILEN(STR)) .EQ. '/' ) THEN
                    FINAM = STR(1:I_LEN(STR))//HELP_FILE
                  ELSE
                    FINAM = STR(1:I_LEN(STR))//'/'//HELP_FILE
               END IF
             ELSE
                FINAM = HELP_FILE
           END IF
      END IF
      INQUIRE ( FILE=FINAM, EXIST=L_EXIST )
      IF ( L_EXIST ) THEN
           MAKE_HELP_FINAM =  0
         ELSE
           MAKE_HELP_FINAM = -1
      END IF
      RETURN
      END  !#!  MAKE_HELP_FINAM  #!#
