      SUBROUTINE VTD_LOAD_STACOO ( VTD, MBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine VTD_LOAD_STACOO reads the file with station      *
! *   coordinates defined in the internal fields of VTD for the stations *
! *   defined in the another fields of VTD. It loads station coordinates *
! *   and the epochs of these coordinates.                               *
! *                                                                      *
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
! * ### 26-JAN-2004  VTD_LOAD_STACOO  v2.4 (c) L. Petrov 11-MAR-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  MBUF, IUER
      CHARACTER  BUF(MBUF)*(*)
      CHARACTER  STR*80, DATE_STR*21
      CHARACTER  STACOO__LABEL1*46, STACOO__LABEL2*46, STACOO__LABEL3*34
      PARAMETER  ( STACOO__LABEL1 = &
     &           '# GETPAR_STA format version 1.1  of 2023.03.11' )
      PARAMETER  ( STACOO__LABEL2 = &
     &           '# GETPAR_STA format version 1.0  of 2001.05.25' )
      PARAMETER  ( STACOO__LABEL3 = &
     &           '$$  SIT-MODFILE Format 2001.09.26 ' )
      LOGICAL*4  LEX
      REAL*8     TAI_REF
      INTEGER*4  NBUF, J1, J2, J3, J4, IOS, IYR, I_STA, USE_STA(VTD__M_STA), &
     &           NUM_ERR, MJD_REF, I_FMT, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( VTD%L_STA == 0 ) THEN
           CALL ERR_LOG ( 2141, IUER, 'VTD_LOAD_STACOO', 'Trap '// &
     &         'of internal control: VTD%L_STA is 0. Have you called '// &
     &         'VTD_STA_DESC before?' )
           RETURN 
      END IF
      INQUIRE ( FILE=VTD%CONF%FINAM_STACOO, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2142, IUER, 'VTD_LOAD_STACOO', 'File with '// &
     &         'station coordinates '// &
     &          VTD%CONF%FINAM_STACOO(1:I_LEN(VTD%CONF%FINAM_STACOO))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Read the file with station coordinates
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_STACOO, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2143, IUER, 'VTD_LOAD_STACOO', 'Error in an '// &
     &         'attempt to read input file with station coordinates'// &
     &          VTD%CONF%FINAM_STACOO )
           RETURN 
      END IF
!
      I_FMT = 0
      IF ( BUF(1)(1:LEN(STACOO__LABEL1)) .EQ. STACOO__LABEL1 ) THEN
           I_FMT = 1
         ELSE IF ( BUF(1)(1:LEN(STACOO__LABEL2)) .EQ. STACOO__LABEL2 ) THEN
           I_FMT = 2
         ELSE IF ( BUF(1)(1:LEN(STACOO__LABEL3)) .EQ. STACOO__LABEL3 ) THEN
           I_FMT = 3
         ELSE
           CALL ERR_LOG ( 2144, IUER, 'VTD_LOAD_STACOO', 'Error in an '// &
     &         'attempt to parse input file with station coordinates '// &
     &          VTD%CONF%FINAM_STACOO(1:I_LEN(VTD%CONF%FINAM_STACOO))// &
     &         ' -- format of this file was not recognized' )
           RETURN 
      END IF
!
      CALL NOUT_I4 ( VTD__M_STA, USE_STA ) 
      MJD_REF = 0
      TAI_REF = 0.0D0
!
! --- Parse station cordinates file
!
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) .EQ. '#' ) GOTO 410
         IF ( I_FMT .EQ. 1  .AND.  BUF(J1)(1:26) .EQ. '# Position reference date:' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(28:46), MJD_REF, TAI_REF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2145, IUER, 'VTD_LOAD_STACOO', 'Error '// &
     &                 'in parsing reference epoch in the file with '// &
     &                 'station coordinates '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF
           ELSE IF ( I_FMT .EQ. 2  .AND.  BUF(J1)(1:6) .EQ. 'EPOCH:' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(11:29), MJD_REF, TAI_REF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2146, IUER, 'VTD_LOAD_STACOO', 'Error '// &
     &                 'in parsing reference epoch in the file with '// &
     &                 'station coordinates '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF
           ELSE IF ( I_FMT .EQ. 3  .AND.  BUF(J1)(5:9) .EQ. 'EPOCH'  ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( BUF(J1)(11:29), MJD_REF, TAI_REF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2147, IUER, 'VTD_LOAD_STACOO', 'Error '// &
     &                 'in parsing reference epoch in the file with '// &
     &                 'station coordinates '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF
         END IF
         IF ( BUF(J1)(1:1) .EQ. '$' ) GOTO 410
!
         IF ( ( I_FMT .EQ. 1 .OR. I_FMT .EQ. 2 ) .AND. &
     &        BUF(J1)(1:8) .EQ. 'STA_GCX:'             ) THEN
!
! ----------- GETPAR format
!
! ----------- Repair names: Replace blank in the name with underscore
!
              CALL VTD_NAME_REPAIR ( BUF(J1)(11:18) )
              I_STA = 0
              DO 420 J2=1,VTD%L_STA
                 IF ( BUF(J1)(11:18) .EQ. VTD%STA(J2)%IVS_NAME ) THEN
                      I_STA = J2
                 END IF
 420          CONTINUE 
              IF ( I_STA .EQ. 0 ) GOTO 410
!
              USE_STA(I_STA) = 1 ! Set the flag "station was found"
!
              VTD%STA(I_STA)%MJD_REF = MJD_REF
              VTD%STA(I_STA)%TAI_REF = TAI_REF
              VTD%STA(I_STA)%N_EPC = VTD%STA(I_STA)%N_EPC + 1
              IF ( VTD%STA(I_STA)%N_EPC .EQ. 1 ) THEN
                   VTD%STA(I_STA)%MJD_EPC = VTD__EARLIEST_MJD
                   VTD%STA(I_STA)%TAI_EPC = VTD__EARLIEST_TAI
              END IF
!
              IF ( ILEN(BUF(J1)(20:25)) .NE. 0 ) THEN
                   DATE_STR = '  '//BUF(J1)(20:21)//'.'//BUF(J1)(22:23)//'.'// &
     &                              BUF(J1)(24:25)//'_00:00:00.0'
                   CALL CHIN ( BUF(J1)(20:21), IYR ) 
                   IF ( IYR .LT. 70 ) THEN
                        STR(1:2) = '20'
                     ELSE
                        STR(1:2) = '19'
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DATE_TO_TIME ( DATE_STR, VTD%STA(I_STA)%MJD_EPC, &
     &                                 VTD%STA(I_STA)%TAI_EPC, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J1, STR ) 
                        CALL ERR_LOG ( 2148, IUER, 'VTD_LOAD_STACOO', &
     &                      'Error in parsing reference epoch at line '// &
     &                       STR(1:I_LEN(STR))//' in the file with '// &
     &                      'station coordinates '//VTD%CONF%FINAM_STACOO )
                       RETURN 
                  END IF
              END IF
!
! ----------- Parsing X-coordinate
!
              READ ( UNIT=BUF(J1)(31:45), FMT='(F15.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%COO_TRS(1,VTD%STA(I_STA)%N_EPC)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2149, IUER, 'VTD_LOAD_STACOO', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'station coordinate file '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm to m
!
              VTD%STA(I_STA)%COO_TRS(1,VTD%STA(I_STA)%N_EPC) = &
     &                VTD%STA(I_STA)%COO_TRS(1,VTD%STA(I_STA)%N_EPC)*0.001D0
!
! ----------- Parsing Y-coordinate
!
              READ ( UNIT=BUF(J1)(65:79), FMT='(F15.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%COO_TRS(2,VTD%STA(I_STA)%N_EPC)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2150, IUER, 'VTD_LOAD_STACOO', 'Error '// &
     &                 'in parsing line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'station coordinate file '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm to m
!
              VTD%STA(I_STA)%COO_TRS(2,VTD%STA(I_STA)%N_EPC) = &
     &                VTD%STA(I_STA)%COO_TRS(2,VTD%STA(I_STA)%N_EPC)*0.001D0
!
! ----------- Parsing Z-coordinate
!
              READ ( UNIT=BUF(J1)(99:113), FMT='(F15.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%COO_TRS(3,VTD%STA(I_STA)%N_EPC)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2151, IUER, 'VTD_LOAD_STACOO', 'Error '// &
     &                 'in parsing line '//STR(1:I_LEN(STR))// &
     &                 ' of the station coordinate file '// &
     &                 VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm to m
!
              VTD%STA(I_STA)%COO_TRS(3,VTD%STA(I_STA)%N_EPC) = &
     &                VTD%STA(I_STA)%COO_TRS(3,VTD%STA(I_STA)%N_EPC)*0.001D0
           ELSE IF ( I_FMT .EQ. 3  .AND.  BUF(J1)(5:5) .NE. ' ' ) THEN
!
! ----------- SITMODE format
!
              CALL VTD_NAME_REPAIR ( BUF(J1)(5:12) )
              I_STA = 0
              DO 430 J3=1,VTD%L_STA
                 IF ( BUF(J1)(5:12) .EQ. VTD%STA(J3)%IVS_NAME ) THEN
                      I_STA = J3
                 END IF
 430          CONTINUE 
              IF ( I_STA .EQ. 0 ) GOTO 410
!
              USE_STA(I_STA) = 1 ! Set the flag "station was found"
!
              VTD%STA(I_STA)%MJD_REF = MJD_REF
              VTD%STA(I_STA)%TAI_REF = TAI_REF
              VTD%STA(I_STA)%N_EPC = VTD%STA(I_STA)%N_EPC + 1
              IF ( VTD%STA(I_STA)%N_EPC .EQ. 1 ) THEN
                   VTD%STA(I_STA)%MJD_EPC = VTD__EARLIEST_MJD
                   VTD%STA(I_STA)%TAI_EPC = VTD__EARLIEST_TAI
              END IF
!
              IF ( BUF(J1)(63:70) .EQ. '00 00 00' ) THEN
                   BUF(J1)(63:70) = '00 01 01'
              END IF
              DATE_STR = '  '//BUF(J1)(63:64)//'.'//BUF(J1)(66:67)//'.'// &
     &                         BUF(J1)(69:70)//'_00:00:00.0'
              CALL CHIN ( BUF(J1)(63:64), IYR ) 
              IF ( IYR .LT. 70 ) THEN
                   DATE_STR(1:2) = '20'
                 ELSE
                   DATE_STR(1:2) = '19'
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL DATE_TO_TIME ( DATE_STR, VTD%STA(I_STA)%MJD_EPC, &
     &                            VTD%STA(I_STA)%TAI_EPC, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR ) 
                   CALL ERR_LOG ( 2152, IUER, 'VTD_LOAD_STACOO', &
     &                 'Error in parsing reference epoch '// &
     &                  DATE_STR//' at line '// &
     &                  STR(1:I_LEN(STR))//' in the file with '// &
     &                 'station coordinates '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF
!
! ----------- Parsing X-coordinate
!
              READ ( UNIT=BUF(J1)(16:27), FMT='(F12.3)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%COO_TRS(1,VTD%STA(I_STA)%N_EPC)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2153, IUER, 'VTD_LOAD_STACOO', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'station coordinate file '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF 
!
! ----------- Parsing Y-coordinate
!
              READ ( UNIT=BUF(J1)(32:43), FMT='(F12.3)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%COO_TRS(2,VTD%STA(I_STA)%N_EPC)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2154, IUER, 'VTD_LOAD_STACOO', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'station coordinate file '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF 
!
! ----------- Parsing Z-coordinate
!
              READ ( UNIT=BUF(J1)(48:59), FMT='(F12.3)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%COO_TRS(3,VTD%STA(I_STA)%N_EPC)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2155, IUER, 'VTD_LOAD_STACOO', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the '// &
     &                 'station coordinate file '//VTD%CONF%FINAM_STACOO )
                   RETURN 
              END IF 
         END IF
 410  CONTINUE 
!
! --- Check whether all stations have been found
!
      NUM_ERR = 0
      DO 440 J4=1,VTD%L_STA
         IF ( USE_STA(J4) .EQ. 0  ) THEN
              IF ( VTD%STA(J4)%STA_TYP == VTD__OR ) THEN
                   VTD%STA(J4)%COO_TRS = 0.0D0
                 ELSE 
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 2156, IER, 'VTD_LOAD_STACOO', 'Station '// &
     &                  VTD%STA(J4)%IVS_NAME//' was not found in the '// &
     &                 'station coordinate catalogue file '// &
     &                  VTD%CONF%FINAM_STACOO )
                   NUM_ERR = NUM_ERR + 1
              END IF
         END IF
 440  CONTINUE 
!
      IF ( NUM_ERR .GT. 0 ) THEN
           WRITE ( 6, * ) ' I_FMT=',I_FMT 
           CALL CLRCH ( STR  )
           CALL INCH  ( NUM_ERR, STR ) 
           CALL ERR_LOG ( 2157, IUER, 'VTD_LOAD_STACOO', STR(1:I_LEN(STR))// &
     &         ' stations were not found in station coodinate catalogue'// &
     &         ' file '//VTD%CONF%FINAM_STACOO )
           RETURN 
      END IF
!
      IF ( MJD_REF == 0 ) THEN
           CALL ERR_LOG ( 2158, IUER, 'VTD_LOAD_STACOO', ' Reference '// &
     &         'epoch for station coordinates was not found in file '// &
     &          VTD%CONF%FINAM_STACOO )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_STACOO  !#!#
