      SUBROUTINE CEX_SUBMIT ( CEX, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CEX_SUBMIT
! *                                                                      *
! *  ### 07-APR-2005   CEX_SUBMIT  v1.0 (c)  L. Petrov  07-APR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'corel_export.i'
      TYPE      ( CEX__TYPE ) :: CEX
      INTEGER*4  IUER
      CHARACTER  COMSTR*512, STR*128
      INTEGER*4  IS, ISIG, ICOD
      INTEGER*4  SYSTEM, ILEN, I_LEN
!
      COMSTR = '/bin/csh -cf '// &
     &          CEX%TMP_COM_FILE(1:I_LEN(CEX%TMP_COM_FILE))//CHAR(0) 
!
! --- Set default behaviour for handling signals for waiting child
! --- process
!
!      CALL SIGCLD_DFL ()
!
! --- Launch dclient and wait
!
      IS = SYSTEM ( COMSTR(1:I_LEN(COMSTR))//CHAR(0) )
      IF ( IS .NE. 0 ) THEN
           ISIG = 0
           ICOD = 0
           CALL MVBITS ( IS, 0, 8, ISIG, 0 )
           CALL MVBITS ( IS, 8, 8, ICOD, 0 )
           IF ( ICOD .GE. 128 ) ICOD = ICOD-256
           IF ( ICOD .EQ. -64 ) THEN
                CALL ERR_PASS ( 4881, IUER )
                RETURN
           END IF
           CALL CLRCH ( STR )
           CALL INCH  ( ICOD, STR )
           CALL ERR_LOG ( 4882, IUER, 'CEX_SUBMIT', 'Error in '// &
     &         'attempt to execute a command "'//COMSTR(1:I_LEN(COMSTR))// &
     &         '" for database submission ICOD='//STR )
           RETURN
      END IF
!
      CALL UNLINK ( CEX%TMP_COM_FILE(1:I_LEN(CEX%TMP_COM_FILE))//CHAR(0) )
!
      CALL ERR_PASS ( 0, IUER )
      RETURN
      END  SUBROUTINE  CEX_SUBMIT
