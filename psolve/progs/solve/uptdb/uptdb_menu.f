      SUBROUTINE UPTDB_MENU ( GVH, VCAT_REPO, DB_NAME, DB_VERS, CODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine UPTDB_MENU 
! *                                                                      *
! *  ### 02-DEC-2005   UPTDB_MENU   v2.0 (c) L. Petrov  22-DEC-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      TYPE     ( GVH__STRU ) :: GVH
      CHARACTER  VCAT_REPO*(*), DB_NAME*(*)
      INTEGER*4  DB_VERS, CODE, IUER
      CHARACTER  STR*54
      INTEGER*4  IX, IY, NL
      CHARACTER  CC4*4, STR_VER*3, STR_NEW_VER*3
      CHARACTER, EXTERNAL :: GET_VERSION*54
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      STR = GET_VERSION()
      CALL INCH ( DB_VERS, STR_VER )
      CALL INCH ( DB_VERS+1, STR_NEW_VER )
!
      CALL START_MN()
 910  CONTINUE
!
! --- Printing the first line: title of the program
!
      CALL CLEAR_MN()
      CALL SETCR_MN (  0, 0 )
      CALL ADDSTR_F ( 'Update GVF database in repostitory '//VCAT_REPO )
      CALL SETCR_MN (  79-ILEN(STR), 0 )
      CALL REVERSE_ON_MN()
      CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
      CALL REVERSE_OFF_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '------------------------' )
      CALL NL_MN()
      CALL ADDSTR_F ( '           Select an option: ' )
      CALL NL_MN()
      CALL NL_MN()
      CALL ADDSTR_F ( '  1. Update the current version         '// &
     &                STR_VER(1:I_LEN(STR_VER))//' of database '// &
     &                DB_NAME(1:I_LEN(DB_NAME)) )
      CALL NL_MN()
      IF ( DB_VERS == 1 ) THEN
           CALL ADDSTR_F ( '  2. Create the new version             '// &
     &                     STR_NEW_VER(1:I_LEN(STR_NEW_VER))//' of database '// &
     &                     DB_NAME(1:I_LEN(DB_NAME)) )
         ELSE
           CALL ADDSTR_F ( '  2. Update keeping the current version '// &
     &                     STR_VER(1:I_LEN(STR_VER))//' of database '// &
     &                     DB_NAME(1:I_LEN(DB_NAME)) )
      END IF
      CALL NL_MN()
      CALL ADDSTR_F ( '  3. Create the new version             '// &
     &                STR_NEW_VER(1:I_LEN(STR_NEW_VER))//' of database '// &
     &                DB_NAME(1:I_LEN(DB_NAME)) )
      CALL NL_MN()
      CALL ADDSTR_F ( '  4. Cancel operation ' )
      CALL NL_MN()
!
!@      NL = 6
      NL = 3
      CALL SETCR_MN ( 0, NL )
      CALL SENKR_MN ( IX, IY, CC4 )
!
      IF ( IY == 4  .OR.  CC4(4:4) == '1' ) THEN
           CODE = 1
         ELSE IF ( IY == 5  .OR.  CC4(4:4) == '2' ) THEN
           CODE = 3
         ELSE IF ( IY == 6  .OR.  CC4(4:4) == '3' ) THEN
           CODE = 2
         ELSE IF ( IY == 7  .OR.  CC4(4:4) == '4' ) THEN
           CODE = -1
         ELSE 
           CODE = 0
      END IF 
      IF ( CODE == 0 ) GOTO 910
!
      CALL END_MN()
!
      CALL UN_CURSES ()
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  UPTDB_MENU  !#!#
