      SUBROUTINE CEX_BUILD_COMMAND ( CEX, DATA_DIR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CEX_BUILD_COMMAND
! *                                                                      *
! * ## 07-APR-2005  CEX_BUILD_COMMAND  v1.1 (c) L. Petrov 06-SEP-2005 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'corel_export.i'
      TYPE      ( CEX__TYPE ) :: CEX
      CHARACTER  DATA_DIR*(*)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 256 )
      CHARACTER  BUF_COM(MBUF)*256, NAME*128
      INTEGER*4  IUER
      INTEGER*4  IL, IP, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      IL = ILEN(DATA_DIR)
      IP = LINDEX ( DATA_DIR(1:IL-1), '/' )
      NAME = DATA_DIR(IP+1:IL-1)
!
      BUF_COM(1)  = '#!/bin/csh -f'
      BUF_COM(2)  = 'set OS = `uname`'
      BUF_COM(3)  = 'switch ( $OS )'
      BUF_COM(4)  = '   case "HP-UX": '
      BUF_COM(5)  = '      setenv ECHO echo'
      BUF_COM(6)  = '      breaksw'
      BUF_COM(7)  = '   case "Linux"'
      BUF_COM(8)  = '      setenv ECHO "/bin/echo -e"'
      BUF_COM(9)  = '      breaksw'
      BUF_COM(10)  = '   default:'
      BUF_COM(11)  = '      setenv ECHO echo'
      BUF_COM(12)  = '      breaksw'
      BUF_COM(13)  = 'endsw'
      BUF_COM(14)  = '#'
      BUF_COM(15)  = 'cd '//DATA_DIR(1:I_LEN(DATA_DIR))
      BUF_COM(16)  = 'cd ../'
      BUF_COM(17)  = '$ECHO "Compress directory tree '// &
     &               DATA_DIR(1:I_LEN(DATA_DIR))//' ... \c"'
      BUF_COM(18)  = 'tar -chf - '//NAME(1:I_LEN(NAME))//'/'// &
     &              ' | '//CEX%BZIP2_EXE(1:I_LEN(CEX%BZIP2_EXE))// &
     &              ' > '//CEX%OUTCOMING_LOCAL_DIR(1:I_LEN(CEX%OUTCOMING_LOCAL_DIR))// &
     &              '/'//NAME(1:I_LEN(NAME))//'_mk4.tar.bz2'
      BUF_COM(19)  = 'set tar_bzip_status = $status'
      BUF_COM(20)  = 'if ( $tar_bzip_status != 0 ) then'
      BUF_COM(21)  = '   echo "error in an attempt to compress the output file"'
      BUF_COM(22)  = '   exit 1'
      BUF_COM(23)  = 'endif'
      BUF_COM(24)  = '$ECHO "done"'
      BUF_COM(25)  = '$ECHO "Send e-mail to '// &
     &                CEX%DSERVER_E_MAIL(1:I_LEN(CEX%DSERVER_E_MAIL))// &
     &               ' ... \c"'
      BUF_COM(26) = CEX%MAIL_COMMAND(1:I_LEN(CEX%MAIL_COMMAND))//' -s "'// &
     &              'Submission of name" '// &
     &               CEX%DSERVER_E_MAIL(1:I_LEN(CEX%DSERVER_E_MAIL))// &
     &              ' <<END_OF_FILE'
      BUF_COM(27) = 'DATA_TYPE:     MK5VLBI'
      BUF_COM(28) = 'URL:          '//CEX%OUTCOMING_URL_DIR(1:I_LEN(CEX%OUTCOMING_URL_DIR))// &
     &               NAME(1:I_LEN(NAME))//'_mk4.tar.bz2'
      BUF_COM(29) = 'CONFIRM_EMAIL: '//CEX%CONFIRM_E_MAIL(1:I_LEN(CEX%CONFIRM_E_MAIL))
      BUF_COM(30) = 'SUCCESS_EMAIL: '//CEX%SUCCESS_E_MAIL(1:I_LEN(CEX%SUCCESS_E_MAIL))
      BUF_COM(31) = 'END_OF_FILE'
      BUF_COM(32) = 'set mail_status = $status'
      BUF_COM(33) = 'if ( $mail_status != 0 ) then'
      BUF_COM(34) = '   echo "error in an attempt to send mail to dserver"'
      BUF_COM(35) = '   exit 1'
      BUF_COM(36) = 'endif'
      BUF_COM(37) = '$ECHO "done"'
!
      CALL ERR_PASS ( IUER, IER )
      CALL WR_TEXT  ( 37, BUF_COM, CEX%TMP_COM_FILE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4851, IUER, 'CEX_BUILD_COMMAND', 'Error in '// &
     &         'an attempt to write the temporary command file' )
           RETURN 
      END IF
!
      CALL SYSTEM ( 'chmod u+rwx,g+rwx,o+rwx '// &
     &               CEX%TMP_COM_FILE(1:I_LEN(CEX%TMP_COM_FILE))//CHAR(0) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CEX_BUILD_COMMAND  
