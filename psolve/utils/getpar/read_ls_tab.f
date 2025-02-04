      SUBROUTINE READ_LS_TAB ( M_LS, L_LS, BUF_LS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_LS_TAB  reads file with leap second and returns a    *
! *   text buffer with contents of that file.                            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *   M_LS ( INTEGER*4 ) -- Maximal length of the buffer with leap       *
! *                         second. Recomendation: 64 .                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   L_LS ( INTEGER*4 ) -- Actual length of the buffer with leap second.*
! * BUF_LS ( CHARACTER ) -- Text buffer with leap second.                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  29-JUL-99   READ_LS_TAB   v1.0 (c)  L. Petrov  29-JUL-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'param.i'
      INTEGER*4  M_LS, L_LS, IUER
      CHARACTER  BUF_LS(M_LS)*80
      INTEGER*4  IER
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( DFLEAP, M_LS, BUF_LS, L_LS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6401, IUER, 'READ_LS_TAB', 'Error in attempt to '// &
     &         'read leap second file '//DFLEAP )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_LS_TAB  #!#
