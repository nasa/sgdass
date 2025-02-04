      SUBROUTINE VTD_LOAD_STAVEL ( VTD, MBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine VTD_LOAD_STACOO reads the file with station      *
! *   velocities defined in the internal fields of VTD for the stations  *
! *   defined in the another fields of VTD. It loads velocities to the   *
! *   internal fields of VTD.                                            *
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
! * ### 26-JAN-2004  VTD_LOAD_STAVEL  v2.2 (c) L. Petrov 11-MAR-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  MBUF, IUER
      CHARACTER  BUF(MBUF)*(*)
      CHARACTER  STR*80, DATE_STR*21
      CHARACTER  STAVEL__LABEL1*46, STAVEL__LABEL2*46, STAVEL__LABEL3*34
      PARAMETER  ( STAVEL__LABEL1 = &
     &           '# GETPAR_VEL format version 1.1  of 2023.03.11' )
      PARAMETER  ( STAVEL__LABEL2 = &
     &           '# GETPAR_VEL format version 1.0  of 2001.05.25' )
      PARAMETER  ( STAVEL__LABEL3 = &
     &           '$$  VEL-MODFILE Format 2001.09.26 ' )
      REAL*8       JUL_YEAR__TO__SEC
      PARAMETER  ( JUL_YEAR__TO__SEC = 365.25D0*86400.0D0 ) ! Julian year
      LOGICAL*4  LEX
      REAL*8     TAI_REF
      INTEGER*4  NBUF, J0, J1, J2, J3, J4, I_FMT, IOS, IYR, I_STA, &
     &           USE_STA(VTD__M_STA), NUM_ERR, MJD_REF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( ILEN(VTD%CONF%FINAM_STAVEL) == 0 ) THEN
!
! -------- Nothing to do. Initialize velocities
!
           DO 400 J0=1,VTD%L_STA
              VTD%STA(J0)%VEL_TRS(1) = 0.0D0
              VTD%STA(J0)%VEL_TRS(2) = 0.0D0
              VTD%STA(J0)%VEL_TRS(3) = 0.0D0
 400       CONTINUE 
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
! --- Check whether the file with velocities exists
!
      INQUIRE ( FILE=VTD%CONF%FINAM_STAVEL, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2151, IUER, 'VTD_LOAD_STAVEL', 'File with '// &
     &         'station coordinates '// &
     &          VTD%CONF%FINAM_STAVEL(1:I_LEN(VTD%CONF%FINAM_STAVEL))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Read the file with station velocities
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_STAVEL, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2152, IUER, 'VTD_LOAD_STAVEL', 'Error in an '// &
     &         'attempt to read input file with station velocities '// &
     &          VTD%CONF%FINAM_STAVEL )
           RETURN 
      END IF
!
      I_FMT = 0
      IF ( BUF(1) .EQ. STAVEL__LABEL1 ) THEN
           I_FMT = 1
         ELSE IF ( BUF(1)(1:LEN(STAVEL__LABEL2)) .EQ. STAVEL__LABEL2 ) THEN
           I_FMT = 2
         ELSE IF ( BUF(1)(1:LEN(STAVEL__LABEL3)) .EQ. STAVEL__LABEL3 ) THEN
           I_FMT = 3
         ELSE IF ( BUF(2)(1:LEN(STAVEL__LABEL3)) .EQ. STAVEL__LABEL3 ) THEN
           I_FMT = 3
         ELSE
           CALL ERR_LOG ( 2153, IUER, 'VTD_LOAD_STAVEL', 'Error in an '// &
     &         'attempt to parse input file with station velocities '// &
     &          VTD%CONF%FINAM_STAVEL(1:I_LEN(VTD%CONF%FINAM_STAVEL))// &
     &         ' -- format of this file was not recognized' )
           RETURN 
      END IF
!
! --- Initialization
!
      CALL NOUT_I4 ( VTD__M_STA, USE_STA ) 
      MJD_REF = 0
      TAI_REF = 0.0D0
!
! --- Parse station velocities file
!
      DO 410 J1=1,NBUF
         IF ( J1 .EQ. 1 .AND.  I_FMT .EQ. 3 ) GOTO  410
         IF ( BUF(J1)(1:1) .EQ. '#' ) GOTO 410
         IF ( BUF(J1)(1:1) .EQ. '$' ) GOTO 410
         IF ( ( I_FMT .EQ. 1 .OR. I_FMT .EQ. 2 ) .AND. &
     &        BUF(J1)(1:8) .EQ. 'STA_GVX:'             ) THEN
              I_STA = 0
              CALL VTD_NAME_REPAIR ( BUF(J1)(11:18) )
              DO 420 J2=1,VTD%L_STA
                 IF ( BUF(J1)(11:18) .EQ. VTD%STA(J2)%IVS_NAME ) THEN
                      I_STA = J2
                 END IF
 420          CONTINUE 
!
              IF ( I_STA .EQ. 0 ) GOTO 410
              USE_STA(I_STA) = 1
!
! ----------- Parsing X-component of the velocity
!
              READ ( UNIT=BUF(J1)(24:32), FMT='(F15.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%VEL_TRS(1)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2154, IUER, 'VTD_LOAD_STAVEL', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the station '// &
     &                 'description file '//VTD%CONF%FINAM_STADESC )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm/yr to m/s
!
              VTD%STA(I_STA)%VEL_TRS(1) = VTD%STA(I_STA)%VEL_TRS(1)*0.001D0/ &
     &                                    JUL_YEAR__TO__SEC
!
! ----------- Parsing Y-coordinate
!
              READ ( UNIT=BUF(J1)(50:58), FMT='(F9.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%VEL_TRS(2)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2155, IUER, 'VTD_LOAD_STAVEL', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the station '// &
     &                 'description file '//VTD%CONF%FINAM_STADESC )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm to m
!
              VTD%STA(I_STA)%VEL_TRS(2) = VTD%STA(I_STA)%VEL_TRS(2)*0.001D0/ &
     &                                    JUL_YEAR__TO__SEC
!
! ----------- Parsing Z-coordinate
!
              READ ( UNIT=BUF(J1)(76:84), FMT='(F9.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%VEL_TRS(3)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2156, IUER, 'VTD_LOAD_STAVEL', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the station '// &
     &                 'description file '//VTD%CONF%FINAM_STADESC )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm to m
!
              VTD%STA(I_STA)%VEL_TRS(3) = VTD%STA(I_STA)%VEL_TRS(3)*0.001D0/ &
     &                                    JUL_YEAR__TO__SEC
           ELSE IF ( I_FMT .EQ. 3  .AND.  BUF(J1)(5:5) .NE. ' ' ) THEN
              I_STA = 0
              CALL VTD_NAME_REPAIR ( BUF(J1)(5:12) )
              DO 430 J3=1,VTD%L_STA
                 IF ( BUF(J1)(5:12) .EQ. VTD%STA(J3)%IVS_NAME ) THEN
                      I_STA = J3
                 END IF
 430          CONTINUE 
!
              IF ( I_STA .EQ. 0 ) GOTO 410
              USE_STA(I_STA) = 1
!
! ----------- Parsing X-component of the velocity
!
              READ ( UNIT=BUF(J1)(19:28), FMT='(F10.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%VEL_TRS(1)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2157, IUER, 'VTD_LOAD_STAVEL', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the station '// &
     &                 'description file '//VTD%CONF%FINAM_STADESC )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm/yr to m/s
!
              VTD%STA(I_STA)%VEL_TRS(1) = VTD%STA(I_STA)%VEL_TRS(1)*0.001D0/ &
     &                                    JUL_YEAR__TO__SEC
!
! ----------- Parsing Y-coordinate
!
              READ ( UNIT=BUF(J1)(35:44), FMT='(F10.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%VEL_TRS(2)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2158, IUER, 'VTD_LOAD_STAVEL', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the station '// &
     &                 'description file '//VTD%CONF%FINAM_STADESC )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm to m
!
              VTD%STA(I_STA)%VEL_TRS(2) = VTD%STA(I_STA)%VEL_TRS(2)*0.001D0/ &
     &                                    JUL_YEAR__TO__SEC
!
! ----------- Parsing Z-coordinate
!
              READ ( UNIT=BUF(J1)(51:60), FMT='(F10.2)', IOSTAT=IOS ) &
     &               VTD%STA(I_STA)%VEL_TRS(3)
              IF ( IOS .NE. 0 ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 2159, IUER, 'VTD_LOAD_STAVEL', 'Error in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the station '// &
     &                 'description file '//VTD%CONF%FINAM_STADESC )
                   RETURN 
              END IF 
!
! ----------- Transform coordinate from mm to m
!
              VTD%STA(I_STA)%VEL_TRS(3) = VTD%STA(I_STA)%VEL_TRS(3)*0.001D0/ &
     &                                    JUL_YEAR__TO__SEC
         END IF
 410  CONTINUE 
!
! --- Check whethere all stations have been found in the catalogue
!
      NUM_ERR = 0
      DO 440 J4=1,VTD%L_STA
         IF ( USE_STA(J4) .EQ. 0 ) THEN
              IF ( VTD%STA(J4)%STA_TYP == VTD__OR ) THEN
                   VTD%STA(J4)%VEL_TRS = 0.0D0
                 ELSE 
                   CALL ERR_PASS ( IUER, IER )
                   CALL ERR_LOG ( 2160, IER, 'VTS_LOAD_STAVEL', 'Station '// &
     &                  VTD%STA(J4)%IVS_NAME//' was not found in the '// &
     &                 'station velocity catalogue file '// &
     &                  VTD%CONF%FINAM_STAVEL )
                   NUM_ERR = NUM_ERR + 1
              END IF
         END IF
 440  CONTINUE 
!
      IF ( NUM_ERR .GT. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NUM_ERR, STR ) 
           CALL ERR_LOG ( 2161, IUER, 'VTS_LOAD_STAVEL', STR(1:I_LEN(STR))// &
     &         ' stations were not found in station velocity catalogue'// &
     &         ' file '//VTD%CONF%FINAM_STAVEL )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_STAVEL  !#!#
