      SUBROUTINE MALO_LOAD_LEAPSEC ( MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_LOAD_LEAPSEC parses and loads into MALO the file with *
! *   leap second. This file determines values of the step function      *
! *   UTC-TAI(t). The name of the leap second file is defined in the     *
! *   internal fields of MALO.                                           *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      MBUF ( INTEGER*4 ) -- The length of the buffer in lines.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     MALO ( MALO__TYPE ) -- Object which keeps configuration and data *
! *                            related to MAss LOading (MALO) package.   *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 29-JAN-2004  MALO_LOAD_LEAPSEC  v1.0 (c) L. Petrov 29-JAN-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  IUER
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      CHARACTER  STR*128
      LOGICAL*4  LEX
      CHARACTER  LEAPSEC__LABEL1*42
      PARAMETER  ( LEAPSEC__LABEL1 = &
     &            '# LEAP_SECOND file  Version of 2004.01.29 ' )
      REAL*8     VAL
      INTEGER*4  NBUF, IOS, J1, MBUF, IER
      PARAMETER  ( MBUF = 256 )
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=MALO%LEAPSEC%FINAM_LEAPSEC, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2191, IUER, 'MALO_LOAD_LEAPSEC', 'File with '// &
     &         'leap second '// &
     &          MALO%LEAPSEC%FINAM_LEAPSEC(1:I_LEN(MALO%LEAPSEC%FINAM_LEAPSEC))// &
     &         ' was not found' )
           RETURN 
      END IF
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 2192, IUER, 'MALO_LOAD_LEAPSEC', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for a temporary buffer' )
           RETURN 
      END IF
!
! --- Read the file with leap second
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( MALO%LEAPSEC%FINAM_LEAPSEC, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2193, IUER, 'MALO_LOAD_LEAPSEC', 'Error in an '// &
     &         'attempt to read input file with leap second information '// &
     &          MALO%LEAPSEC%FINAM_LEAPSEC )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Check the label
!
      IF ( BUF(1) .NE. LEAPSEC__LABEL1 ) THEN
           CALL ERR_LOG ( 2194, IUER, 'MALO_LOAD_LEAPSEC', 'Error in an '// &
     &         'attempt to parse input file with leap seconds '// &
     &          MALO%LEAPSEC%FINAM_LEAPSEC(1:I_LEN(MALO%LEAPSEC%FINAM_LEAPSEC))// &
     &         ' -- format of this file was not recognized' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Parse the file
!
      MALO%LEAPSEC%L_LPS = 0
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         MALO%LEAPSEC%L_LPS = MALO%LEAPSEC%L_LPS + 1
!
! ------ Parse the date
!
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( BUF(J1)(7:27), &
     &                       MALO%LEAPSEC%MJD_LPS(MALO%LEAPSEC%L_LPS), &
     &                       MALO%LEAPSEC%TAI_LPS(MALO%LEAPSEC%L_LPS), IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) 'Line ',J1
              CALL ERR_LOG ( 2195, IUER, 'MALO_LOAD_LEAPSEC', 'Error in '// &
     &            'parsing leap second file '// MALO%LEAPSEC%FINAM_LEAPSEC )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
! ------ Parse the value
!
         READ ( UNIT=BUF(J1)(39:43), FMT='(F5.1)', IOSTAT=IOS ) VAL
         IF ( IOS .NE. 0 ) THEN
              CALL ERR_LOG ( 2196, IUER, 'MALO_LOAD_LEAPSEC', 'Error in '// &
     &            'parsing leap second file '// MALO%LEAPSEC%FINAM_LEAPSEC )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
         MALO%LEAPSEC%UTC_M_TAI(MALO%LEAPSEC%L_LPS) = -VAL
 410  CONTINUE 
!
! --- Set the status
!
      MALO%LEAPSEC%STATUS = MALO__LOAD
      DEALLOCATE ( BUF )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_LOAD_LEAPSEC  !#!#
