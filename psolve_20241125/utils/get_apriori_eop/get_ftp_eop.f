      SUBROUTINE GET_FTP_EOP ( URLNAM, FILEXT, WGET_EXE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_FTP_EOP  retrieves file with URL URLNAM by using      *
! *   program wget_exe and writes it in the local file FILEXT            *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  URLNAM    ( CHARACTER ) -- URL of the external EOP file.            *
! *  FILEXT    ( CHARACTER ) -- Name of the local temporary file where   *
! *                             the external EOP is to be written.       *
! *  WGET_EXE  ( CHARACTER ) -- Full name with path of program wget for  *
! *                             retrieving external file via ftp.        *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! *  ### 09-NOV-2000   GET_FTP_EOP  v1.0 (c) L. Petrov  09-NOV-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  URLNAM*(*), FILEXT*(*), WGET_EXE*(*)
      INTEGER*4  IUER
      CHARACTER  REMOTE_COMSTR*256, STR*32
      INTEGER*4  IS, ICOD, ISIG
      INTEGER*4  SYSTEM, I_LEN
!
      CALL UNLINK ( FILEXT(1:I_LEN(FILEXT))//CHAR(0) )
!
      CALL CLRCH ( REMOTE_COMSTR )
      REMOTE_COMSTR = 'cd /tmp/  ; '// &
     &              WGET_EXE(1:I_LEN(WGET_EXE))//' -q -O '// &
     &              FILEXT(1:I_LEN(FILEXT))//' '//URLNAM(1:I_LEN(URLNAM))
!
! --- Launch wget for file retrieving and wait
!
      IS = SYSTEM ( REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
           CALL CLRCH ( STR )
           CALL INCH  ( ICOD, STR )
           CALL ERR_LOG ( 8241, IUER, 'GET_FTP_EOP', 'Error in '// &
     &            'attempt to execute a command "'// &
     &             REMOTE_COMSTR(1:I_LEN(REMOTE_COMSTR))// &
     &            '" for downloading master files ICOD='//STR )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GET_FTP_EOP  #!#
