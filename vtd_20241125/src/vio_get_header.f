      SUBROUTINE VIO_GET_HEADER ( VIO_FILE, VIO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VIO_GET_HEADER  reads the header of the binary file with  *
! *   GPS ionosphere TEC maps, parses it, and writes results of parsing  *
! *   in fields of the data structure VIO.                               *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * VIO_FILE ( CHARACTER  ) -- Binary file with GPS ionosphere TEC maps  *
! *                            in VIONO format.                          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      VIO ( IONO__TYPE ) -- Data structure that keeps the data        *
! *                            related to ionosphere TEC maps.           *
! *                            It contains the header that describes     *
! *                            the dataset and the data.                 *
! *
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
! * ### 09-MAY-2010  VIO_GET_HEADER v1.0 (c) L. Petrov  09-MAY-2010 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'viono.i'
      TYPE     ( IONO__TYPE ) :: VIO
      CHARACTER  VIO_FILE*(*)
      INTEGER*4  IUER
      LOGICAL*4  LEX
      CHARACTER  STR*128
      INTEGER*4  IS, LUN, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ
!
! --- Check whether the binary file with ionosphere TEC maps exists
!
      INQUIRE ( FILE=VIO_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4391, IUER, 'VIO_GET_HEADER', 'Trap of internal '// &
     &         'control: cannot find binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
!
! --- Open ionosphere TEC maps file
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( VIO_FILE, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4392, IUER, 'VIO_GET_HEADER', 'Failure '// &
     &         'to open existing binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
      END IF
!
! --- Read teh header
!
      IS = READ ( %VAL(LUN), VIO%HEADER, %VAL(SIZEOF(VIO%HEADER)) )
      IF ( IS .EQ. -1 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 4393, IUER, 'VIO_GET_HEADER', 'Failure '// &
     &         'to read the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- '//STR )
           RETURN 
        ELSE IF ( IS .NE. SIZEOF(VIO%HEADER) ) THEN
           CALL ERR_LOG ( 4394, IUER, 'VIO_GET_HEADER', 'Failure '// &
     &         'to read  the header into binary ionosphere file '// &
     &          VIO_FILE(1:I_LEN(VIO_FILE))//' -- not all bytes have '// &
     &         'been written: no space left at disk?' )
           RETURN 
      END IF
!
! --- Close the open ionspere TEC maps file
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4395, IUER, 'VIO_GET_HEADER', 'Failure '// &
     &         'to close binary ionosphere file '//VIO_FILE )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VIO_GET_HEADER  !#!  
