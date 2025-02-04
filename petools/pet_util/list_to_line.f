      SUBROUTINE LIST_TO_LINE ( L_LST, C_LST, SEP, OUT )
! ************************************************************************
! *                                                                      *
! *   Routine  LIST_TO_LINE puts elements of the character list to one   *
! *   line end separte them by one symbol SEP plus one additional blank  *
! *   in the case if SEP is not a blank. It removes trailing blanks from *
! *   the elements of the list.                                          *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   L_LST ( INTEGER*4 ) -- Number of elements in the character array   *
! *   C_LST ( CHARACTER ) -- Character array to be glued.                *
! *     SEP ( CHARACTER ) -- Separator symbols to be put as a delimiter  *
! *                          between two elements of the list in the     *
! *                          resulting string.                           *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *     OUT ( CHARACTER ) -- Resulting string.                           *
! *                                                                      *
! *  ###  17-MAR-98   LIST_TO_LINE  v1.0 (c)  L. Petrov  17-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  L_LST
      CHARACTER  C_LST(L_LST)*(*), SEP*1, OUT*(*)
      INTEGER*4  J1, IE
      INTEGER*4, EXTERNAL :: I_LEN
!
      CALL CLRCH ( OUT )
!
      IE = -1
      DO 410 J1=1,L_LST
         OUT(IE+2:) = C_LST(J1)
         IE = I_LEN(OUT)
         IF ( SEP .NE. ' '  .AND. J1 .NE. L_LST ) THEN
              OUT(IE+1:) = SEP
              IE = IE+1
         END IF
 410  CONTINUE
!
      RETURN
      END  !#!  LIST_TO_LINE  #!#
