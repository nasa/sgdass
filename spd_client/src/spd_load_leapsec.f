      SUBROUTINE SPD_LOAD_LEAPSEC ( SPD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_LOAD_LEAPSEC parses and loads into SPD the file with   *
! *   leap second. This file determines values of the step function      *
! *   UTC-TAI(t). The name of the leap second file is defined in the     *
! *   internal fields of SPD.                                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   SPD ( SPD_3D__TYPE   ) -- The object that keeps information        *
! *                             related to computing slanted path delay. *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                          Input:  switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                          Output: 0 in the case of successful         *
! *                                  completion and non-zero in the      *
! *                                  case of error.                      *
! *                                                                      *
! * ## 29-JAN-2004  SPD_LOAD_LEAPSEC  v2.0 (c) L. Petrov 02-JUN-2018 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd.i'
      INCLUDE   'ners.i'
      INCLUDE   'spd_local.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      INTEGER*4  IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 256 )
      CHARACTER  BUF(MBUF)*128, STR*128
      LOGICAL*4  LEX
      REAL*8     VAL
      INTEGER*4  NBUF, IOS, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=SPD%CONF%FIL_LEAPSEC, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2191, IUER, 'SPD_LOAD_LEAPSEC', 'File with '// &
     &         'leap second '//SPD%CONF%FIL_LEAPSEC(1:I_LEN(SPD%CONF%FIL_LEAPSEC))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Read the file with leap second
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( SPD%CONF%FIL_LEAPSEC, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2192, IUER, 'SPD_LOAD_LEAPSEC', 'Error in an '// &
     &         'attempt to read input file with leap second information '// &
     &          SPD%CONF%FIL_LEAPSEC )
           RETURN 
      END IF
!
! --- Check the label
!
      IF ( BUF(1) .NE. SPD__LEAPSEC__LABEL  .AND.  BUF(1) .NE. NERS__LS_FMT ) THEN
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), STR ) 
           CALL ERR_LOG ( 2193, IUER, 'SPD_LOAD_LEAPSEC', 'Error in an '// &
     &         'attempt to parse input file with leap seconds '// &
     &          SPD%CONF%FIL_LEAPSEC(1:I_LEN(SPD%CONF%FIL_LEAPSEC))// &
     &         ' -- format of this file was not recognized '// &
     &         'the first line is '//STR(1:I_LEN(STR))// &
     &         ' while expected '//SPD__LEAPSEC__LABEL )
           RETURN 
      END IF
!
! --- Parse the file
!
      SPD%LEAPSEC%L_LPS = 0
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0  ) GOTO 410
         SPD%LEAPSEC%L_LPS = SPD%LEAPSEC%L_LPS + 1
         IF ( BUF(1) == SPD__LEAPSEC__LABEL ) THEN
!
! ----------- Parse the date
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(7:27), &
          &                       SPD%LEAPSEC%MJD_LPS(SPD%LEAPSEC%L_LPS), &
          &                       SPD%LEAPSEC%TAI_LPS(SPD%LEAPSEC%L_LPS), IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) 'Line ',J1
                   CALL ERR_LOG ( 2194, IUER, 'SPD_LOAD_LEAPSEC', 'Error in '// &
          &            'parsing leap second file '//SPD%CONF%FIL_LEAPSEC )
                   RETURN 
              END IF
!
! ------------Parse the value
!     
              READ ( UNIT=BUF(J1)(39:43), FMT='(F5.1)', IOSTAT=IOS ) VAL
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2195, IUER, 'SPD_LOAD_LEAPSEC', 'Error in '// &
          &            'parsing leap second file '//SPD%CONF%FIL_LEAPSEC )
                   RETURN 
              END IF
              SPD%LEAPSEC%UTC_M_TAI(SPD%LEAPSEC%L_LPS) = -VAL
           ELSE IF ( BUF(1) == NERS__LS_FMT ) THEN
!
! ----------- Parse the date
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(15:33), &
          &                       SPD%LEAPSEC%MJD_LPS(SPD%LEAPSEC%L_LPS), &
          &                       SPD%LEAPSEC%TAI_LPS(SPD%LEAPSEC%L_LPS), IER )
              IF ( IER .NE. 0 ) THEN
                   WRITE ( 6, * ) 'Line ',J1
                   CALL ERR_LOG ( 2196, IUER, 'SPD_LOAD_LEAPSEC', 'Error in '// &
          &            'parsing leap second file '//SPD%CONF%FIL_LEAPSEC )
                   RETURN 
              END IF
!
! ------------Parse the value
!     
              READ ( UNIT=BUF(J1)(36:40), FMT='(F5.1)', IOSTAT=IOS ) VAL
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 2197, IUER, 'SPD_LOAD_LEAPSEC', 'Error in '// &
          &            'parsing leap second file '//SPD%CONF%FIL_LEAPSEC )
                   RETURN 
              END IF
              SPD%LEAPSEC%UTC_M_TAI(SPD%LEAPSEC%L_LPS) = VAL
         END IF
 410  CONTINUE 
!
! --- Set the status
!
      SPD%LEAPSEC%STATUS = SPD__LOAD
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_LOAD_LEAPSEC  !#!#
