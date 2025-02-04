      SUBROUTINE VTD_LOAD_STADESC ( VTD, L_STA, C_STA, MBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine VTD_LOAD_STACOO  reads information from the      *
! *   station description catalogue defined in the internal fields of    *
! *   VTD about the stations defined in the input list. It extracts      *
! *   from the catalogue station name, antenna axis offset, antenna      *
! *   mounting type. It does not load station coordinates and velocities *
! *   Routines VTD_LOAD_STACOO and VTD_LOAD_STAVEL do it.                *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     L_STA ( INTEGER*4 ) -- The number of participating stations.     *
! *     C_STA ( CHARACTER ) -- List of station names participating in    *
! *                            the experiment. Dimension: L_STA.         *
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
! * ### 26-JAN-2004  VTD_LOAD_STADESC v1.6 (c) L. Petrov 18-FEB-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MBUF, L_STA, IUER
      CHARACTER  BUF(MBUF)*(*), C_STA(L_STA)*(*)
      CHARACTER  STR*80, PLT_NAM*4
      CHARACTER    STADESC__LABEL1*52, STADESC__LABEL2*52
      PARAMETER  ( STADESC__LABEL1 = '# STATION DESCRIPTION   Format version of 2023.02.18' )
      PARAMETER  ( STADESC__LABEL2 = '# STATION DESCRIPTION   Format version of 2013.09.18' )
      LOGICAL*4  LEX
      INTEGER*4  NBUF, J1, J2, IOS, NUM_ERR, IFMT, IER
      INTEGER*4  I_LEN
!
! --- Check, whether the file exists
!
      INQUIRE ( FILE=VTD%CONF%FINAM_STADESC, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2131, IUER, 'VTD_LOAD_STADESC', 'File with '// &
     &         'station description '// &
     &          VTD%CONF%FINAM_STADESC(1:I_LEN(VTD%CONF%FINAM_STADESC))// &
     &         ' was not found' )
           RETURN 
      END IF
!
! --- Read the file with station description
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( VTD%CONF%FINAM_STADESC, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2132, IUER, 'VTD_LOAD_STADESC', 'Error in an '// &
     &         'attempt to read input file with station description '// &
     &          VTD%CONF%FINAM_STADESC )
           RETURN 
      END IF
!
      IF ( BUF(1)(1:LEN(STADESC__LABEL1)) .EQ. STADESC__LABEL1 ) THEN
           IFMT = 1
        ELSE IF ( BUF(1)(1:LEN(STADESC__LABEL2)) .EQ. STADESC__LABEL2 ) THEN
           IFMT = 2
        ELSE
           CALL ERR_LOG ( 2133, IUER, 'VTD_LOAD_STADESC', 'Error in an '// &
     &         'attempt to parse input file with station description '// &
     &          VTD%CONF%FINAM_STADESC(1:I_LEN(VTD%CONF%FINAM_STADESC))// &
     &         ' -- format of this file was not recognized' )
           RETURN 
      END IF
!
! --- Parse station description file
!
      VTD%L_STA = 0
      NUM_ERR = 0
      DO 410 J1=1,L_STA
         CALL VTD_NAME_REPAIR ( C_STA(J1) )
         DO 420 J2=1,NBUF
            IF ( BUF(J2)(1:1) .EQ. '#' ) GOTO 420
            IF ( C_STA(J1) .EQ. BUF(J2)(1:8) ) THEN
                 VTD%L_STA = VTD%L_STA + 1
                 VTD%STA(VTD%L_STA)%IVS_NAME   = BUF(J2)(1:8)
                 IF ( IFMT == 1 ) THEN
                      VTD%STA(VTD%L_STA)%MOUNT_TYPE = BUF(J2)(11:14)
                   ELSE IF ( IFMT == 2 ) THEN
                      VTD%STA(VTD%L_STA)%MOUNT_TYPE = BUF(J2)(12:15)
                 END IF
                 IF ( VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'AZEL' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'EQUA' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'X-YN' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'X-YE' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'RICH' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'NASM' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'NASP' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'NASL' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'NASR' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'BWG ' .OR. &
     &                VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'GNSS'      ) THEN
!
                      CONTINUE 
                   ELSE
                      CALL ERR_LOG ( 2134, IUER, 'VTD_LOAD_STADESC', 'Error '// &
     &                    'in parsing line '//BUF(J2)(1:I_LEN(BUF(J2)))//' of '// &
     &                    'station description file '// &
     &                    VTD%CONF%FINAM_STADESC(1:I_LEN(VTD%CONF%FINAM_STADESC))// &
     &                    ' -- mounting type '//BUF(J2)(12:15)//' is not '// &
     &                    'supported. Supported mounting types: AZEL, EQUA, '// &
     &                    'X-YN, X_YE, RICH, NASM, NASP, NASL, NASR, BWG' )
                      RETURN 
                 END IF
!
                 IF ( IFMT == 1 ) THEN
                      READ ( UNIT=BUF(J2)(26:32), FMT='(F7.4)', IOSTAT=IOS ) &
     &                                    VTD%STA(VTD%L_STA)%AXIS_OFFSET 
                   ELSE IF ( IFMT == 2 ) THEN
                      READ ( UNIT=BUF(J2)(18:25), FMT='(F8.5)', IOSTAT=IOS ) &
     &                                    VTD%STA(VTD%L_STA)%AXIS_OFFSET 
                 END IF
                 IF ( IOS .NE. 0 ) THEN
                      CALL CLRCH ( STR ) 
                      CALL INCH  ( J2, STR )
                      CALL ERR_LOG ( 2135, IUER, 'VTD_LOAD_STADESC', 'Error '// &
     &                    'in parsing line '//STR(1:I_LEN(STR))//' of '// &
     &                    'station description file '//VTD%CONF%FINAM_STADESC )
                      RETURN 
                 END IF 
                 IF ( IFMT == 1 ) THEN
                      PLT_NAM = BUF(J2)(45:48)
                   ELSE IF ( IFMT == 2 ) THEN
                      PLT_NAM = BUF(J2)(28:31)
                 END IF
                 IF ( PLT_NAM == 'HELL' ) THEN
                      VTD%STA(VTD%L_STA)%STA_TYP = VTD__GC
                    ELSE IF ( PLT_NAM == 'EAOR' ) THEN
                      VTD%STA(VTD%L_STA)%STA_TYP = VTD__OR
                    ELSE 
                      VTD%STA(VTD%L_STA)%STA_TYP = VTD__GR
                 END IF
                 IF ( IFMT == 1 ) THEN
                      READ ( UNIT=BUF(J2)(51:55), FMT='(F5.1)', IOSTAT=IOS ) VTD%STA(VTD%L_STA)%ANT_DIAM
                   ELSE IF ( IFMT == 2 ) THEN
                      READ ( UNIT=BUF(J2)(34:38), FMT='(F5.1)', IOSTAT=IOS ) VTD%STA(VTD%L_STA)%ANT_DIAM
                 END IF
                 IF ( IOS .NE. 0 ) THEN
                      CALL CLRCH ( STR ) 
                      CALL INCH  ( J2, STR )
                      CALL ERR_LOG ( 2136, IUER, 'VTD_LOAD_STADESC', 'Error '// &
     &                    'in parsing line '//STR(1:I_LEN(STR))//' of '// &
     &                    'station description file '// &
     &                     VTD%CONF%FINAM_STADESC(1:I_LEN(VTD%CONF%FINAM_STADESC))// &
     &                     ' cannot read antenna diameter '//BUF(J2)(34:38) )
                      RETURN 
                 END IF 
                 IF ( VTD%STA(VTD%L_STA)%MOUNT_TYPE  == 'GNSS' ) THEN
!
! ------------------ If GNSS station, get ANTEX type
!
                     READ ( UNIT=BUF(J2)(41:61), FMT='(A20)', IOSTAT=IOS ) VTD%STA(VTD%L_STA)%ANTEX_TYPE
                     IF ( IOS .NE. 0 ) THEN
                          CALL CLRCH ( STR ) 
                          CALL INCH  ( J2, STR )
                          CALL ERR_LOG ( 2137, IUER, 'VTD_LOAD_STADESC', 'Error '// &
     &                        'in parsing line '//STR(1:I_LEN(STR))//' of '// &
     &                        'station description file '// &
     &                         VTD%CONF%FINAM_STADESC(1:I_LEN(VTD%CONF%FINAM_STADESC))// &
     &                         ' cannot read ANTEX type '//BUF(J2)(43:63) )
                          RETURN 
                     END IF 
                 END IF
                 GOTO 410
            END IF 
 420     CONTINUE 
!
         CALL ERR_PASS ( IUER, IER )
         CALL ERR_LOG ( 2137, IER, 'VTD_LOAD_STADESC', 'Station '//C_STA(J1)// &
     &       ' was not found in station description file '// &
     &        VTD%CONF%FINAM_STADESC )
         NUM_ERR = NUM_ERR + 1
 410  CONTINUE 
!
      IF ( NUM_ERR .GT. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NUM_ERR, STR ) 
           CALL ERR_LOG ( 2138, IUER, 'VTD_LOAD_STADESC', STR(1:I_LEN(STR))// &
     &         ' stations were not found in station description file '// &
     &          VTD%CONF%FINAM_STADESC )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_STADESC  !#!#
