      SUBROUTINE CHANGE_WEBDIR ( PREF_NAME )
! ************************************************************************
! *                                                                      *
! *   Auxillary routine  CHANGE_WEBDIR  changes the name of Web_dir --   *
! *   directory name (plus prefix) where hardcopy of the plots will be   *
! *   written.                                                           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    PREF_NAME ( CHARACTER ) -- Prefix string with pathname which will *
! *                               be prepend before the second part of   *
! *                               filename of hardcopies of the plots.   *
! *         IUER ( INTEGER*4, OPT ) -- Universal error handler.          *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  23-JUL-99  CHANGE_WEBDIR v1.1  (c)  L. Petrov 10-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  PREF_NAME*(*), STR*128
      INTEGER*4  IQ, I5
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      WRITE ( 6, 110 ) PREF_NAME(1:I_LEN(PREF_NAME))
 110  FORMAT ( 1X,'Current Web_dir: ',A/ &
     &         1X,'New Web_dir    >>'$ )
      READ ( UNIT=5, FMT='(A)', IOSTAT=I5 ) STR
      IQ = ILEN(STR)
!
      IF ( I5 .NE. 0  .OR.  IQ .LE. 0 ) THEN
           WRITE ( 6, 120 )
 120       FORMAT ( 1X,' Web dir was not changed' )
        ELSE
           CALL CLRCH ( PREF_NAME )
           PREF_NAME = STR(1:IQ)
      END IF
!
      RETURN
      END  !#!  CHANGE_WEBDIR  #!#
