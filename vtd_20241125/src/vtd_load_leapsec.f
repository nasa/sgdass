      SUBROUTINE VTD_LOAD_LEAPSEC ( VTD, MBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_LOAD_LEAPSEC parses and loads into VTD the file with   *
! *   leap second. This file determines values of the step function      *
! *   UTC-TAI(t). The name of the leap second file is defined in the     *
! *   internal fields of VTD.                                            *
! *   
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      MBUF ( INTEGER*4 ) -- The length of the buffer in lines.        *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *       BUF ( CHARACTER ) -- The text buffer. Dimension: MBUF.         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
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
! * ## 29-JAN-2004  VTD_LOAD_LEAPSEC  v1.0 (c) L. Petrov 29-JAN-2004 ##  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MBUF, IUER
      CHARACTER  BUF(MBUF)*(*)
      LOGICAL*4  LEX
      CHARACTER  LEAPSEC__LABEL1*42
      PARAMETER  ( LEAPSEC__LABEL1 = &
     &            '# LEAP_SECOND file  Version of 2004.01.29 ' )
      REAL*8     VAL
      INTEGER*4  NBUF, IOS, J1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=VTD%CONF%FINAM_LEAPSEC, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2191, IUER, 'VTD_LOAD_LEAPSEC', 'File with '// &
     &         'leap second '// &
     &          VTD%CONF%FINAM_LEAPSEC(1:I_LEN(VTD%CONF%FINAM_LEAPSEC))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Read the file with leap second
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_LEAPSEC, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2192, IUER, 'VTD_LOAD_LEAPSEC', 'Error in an '// &
     &         'attempt to read input file with leap second information '// &
     &          VTD%CONF%FINAM_LEAPSEC )
           RETURN 
      END IF
!
! --- Check the label
!
      IF ( BUF(1) .NE. LEAPSEC__LABEL1 ) THEN
           CALL ERR_LOG ( 2193, IUER, 'VTD_LOAD_LEAPSEC', 'Error in an '// &
     &         'attempt to parse input file with leap seconds '// &
     &          VTD%CONF%FINAM_LEAPSEC(1:I_LEN(VTD%CONF%FINAM_LEAPSEC))// &
     &         ' -- format of this file was not recognized' )
           RETURN 
      END IF
!
! --- Parse the file
!
      VTD%LEAPSEC%L_LPS = 0
      DO 410 J1=2,NBUF
         IF ( BUF(J1)(1:1)  .EQ. '#' ) GOTO 410
         IF ( ILEN(BUF(J1)) .EQ.  0   ) GOTO 410
         VTD%LEAPSEC%L_LPS = VTD%LEAPSEC%L_LPS + 1
!
! ------ Parse the date
!
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( BUF(J1)(7:27), &
     &                       VTD%LEAPSEC%MJD_LPS(VTD%LEAPSEC%L_LPS), &
     &                       VTD%LEAPSEC%TAI_LPS(VTD%LEAPSEC%L_LPS), IER )
         IF ( IER .NE. 0 ) THEN
              WRITE ( 6, * ) 'Line ',J1
              CALL ERR_LOG ( 2194, IUER, 'VTD_LOAD_LEAPSEC', 'Error in '// &
     &            'parsing leap second file '//VTD%CONF%FINAM_LEAPSEC )
              RETURN 
         END IF
!
! ------ Parse the value
!
         READ ( UNIT=BUF(J1)(39:43), FMT='(F5.1)', IOSTAT=IOS ) VAL
         IF ( IOS .NE. 0 ) THEN
              CALL ERR_LOG ( 2195, IUER, 'VTD_LOAD_LEAPSEC', 'Error in '// &
     &            'parsing leap second file '//VTD%CONF%FINAM_LEAPSEC )
              RETURN 
         END IF
         VTD%LEAPSEC%UTC_M_TAI(VTD%LEAPSEC%L_LPS) = -VAL
 410  CONTINUE 
!
! --- Set the status
!
      VTD%LEAPSEC%STATUS = VTD__LOAD
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_LEAPSEC 
