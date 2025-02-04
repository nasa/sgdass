      SUBROUTINE TPD_WRITE ( TPD, IUER ) 
! ************************************************************************
! *                                                                      *
! *   Routine TPD_WRITE
! *                                                                      *
! *  ### 07-NOV-2007   TPD_WRITE   v1.0 (c)  L. Petrov  07-NOV-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      TYPE     ( TPD__TYPE     ) :: TPD
      INTEGER*4  IUER
      LOGICAL*4  LEX
      INTEGER*4  LUN, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      INQUIRE ( FILE=TPD%FILE_NAME, EXIST=LEX ) 
      IF ( LEX ) CALL UNLINK ( TPD%FILE_NAME(1:I_LEN(TPD%FILE_NAME))//CHAR(0) )
!
      CALL ERR_PASS  ( IUER,  IER )
      CALL BINF_OPEN ( TPD%FILE_NAME, 'NEW', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7421, IUER, 'TPD_WRITE', 'Error in an attempt '// &
     &         'to open the output TPD file '//TPD%FILE_NAME )
           RETURN 
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, SIZEOF(TPD%HEADER), TPD%HEADER, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7421, IUER, 'TPD_WRITE', 'Error in an attempt '// &
     &         'to write the header of the output TPD file '// &
     &          TPD%FILE_NAME )
           RETURN 
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, TPD%HEADER%NSTA*SIZEOF(TPD%STA(1)), &
     &                    TPD%STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7422, IUER, 'TPD_WRITE', 'Error in an attempt '// &
     &         'to write the array of station objects in the output TPD '// &
     &         'file '//TPD%FILE_NAME )
           RETURN 
      END IF 
      DEALLOCATE ( TPD%STA )
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, TPD%HEADER%NSOU*SIZEOF(TPD%SOU(1)), &
     &                    TPD%SOU, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7423, IUER, 'TPD_WRITE', 'Error in an attempt '// &
     &         'to write the array of source objects in the output TPD '// &
     &         'file '//TPD%FILE_NAME )
           RETURN 
      END IF 
      DEALLOCATE ( TPD%SOU )
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, TPD%HEADER%NOBS*SIZEOF(TPD%PARAM(1)), &
     &                    TPD%PARAM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7424, IUER, 'TPD_WRITE', 'Error in an attempt '// &
     &         'to write the array of param objects in the output TPD file '// &
     &          TPD%FILE_NAME )
           RETURN 
      END IF 
      DEALLOCATE ( TPD%PARAM )
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, TPD%HEADER%NOBS*SIZEOF(TPD%DELAY(1)), &
     &                    TPD%DELAY, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7425, IUER, 'TPD_WRITE', 'Error in an attempt '// &
     &         'to write the array of delay objects in the output TPD file '// &
     &          TPD%FILE_NAME )
           RETURN 
      END IF 
      DEALLOCATE ( TPD%DELAY )
!
      IF ( TPD%HEADER%RATE_USE == SOLVE__YES ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL WRBIN_RECORD ( LUN, TPD%HEADER%NOBS*SIZEOF(TPD%RATE(1)), &
     &                         TPD%RATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG  ( 7426, IUER, 'TPD_WRITE', 'Error in an '// &
     &              'attempt to write the array of rate objects in the '// &
     &              'output TPD file '//TPD%FILE_NAME )
                RETURN 
           END IF 
!
           DEALLOCATE ( TPD%RATE   )
      END IF 
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG  ( 7427, IUER, 'TPD_WRITE', 'Error in an '// &
     &         'attempt to close the output TPD file '//TPD%FILE_NAME )
           RETURN 
      END IF 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TPD_WRITE  !#!#
