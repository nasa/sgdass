      SUBROUTINE VTD_LOAD_SOUCOO   ( VTD, L_SOU, C_SOU, MBUF, BUF, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine VTD_LOAD_SOUCOO  reads information from the      *
! *   source catalogue defined in the internal fields of VTD about the   *
! *   sources defined in the input list. It extracts from the catalogue  *
! *   source names, reference date and coordinates.                      *
! *                                                                      *
! *   If a source is defined in more than one catalogue or defined more  *
! *   than once in the same catalogue, the latest definition will        *
! *   override the previous one.                                         *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     L_SOU ( INTEGER*4 ) -- The number of observed sources.           *
! *     C_SOU ( CHARACTER ) -- List of source names participating in     *
! *                            the experiment. Dimension: L_SOU.         *
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
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 26-JAN-2004  VTD_LOAD_SOUCOO  v3.1 (d) L. Petrov 25-MAR-2026 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      INTEGER*4  MBUF, L_SOU, IUER
      CHARACTER  BUF(MBUF)*(*), C_SOU(L_SOU)*(*)
      CHARACTER  STR*80, ALPHA_STR*17, DELTA_STR*17
      CHARACTER  SOUCOO__LABEL1*64, SOUCOO__LABEL2*32, SOUCOO__LABEL3*76, &
     &           SOUCOO__LABEL4*32, SOUCOO__LABEL5*50, SOUCOO__LABEL6*62, &
     &           SOUCOO__LABEL7*32
      PARAMETER  ( SOUCOO__LABEL1 = &
     & '# VLBI SOURCE POSITION CATALOGUE  Format version of 2004.01.21  ' )
      PARAMETER  ( SOUCOO__LABEL2 = '$$  SOU-MODFILE Format pre-2000 ' )
      PARAMETER  ( SOUCOO__LABEL3 = '# Catalogue of source positions and velocities. Format version of 2005.12.29' )
      PARAMETER  ( SOUCOO__LABEL4 = '# SOURCE-NAMES  v 2.0 2005.09.06' )
      PARAMETER  ( SOUCOO__LABEL5 = '#  Source position file format  1.0  of 2019.08.08' )
      PARAMETER  ( SOUCOO__LABEL6 = '# VLBI SOURCE POSITION CATALOGUE  Format version of 2024.10.14' )
      PARAMETER  ( SOUCOO__LABEL7 = '# SOURCE-NAMES  v 3.0 2025.09.06' )
      LOGICAL*4  LEX, FL_FOUND(VTD__M_SOU)
      CHARACTER  V_SOU(VTD__M_SOU)*8, SAT_NAM*8
      INTEGER*4    MIND
      PARAMETER  ( MIND = 128 )
      REAL*8     SEC_REF, TAI_REF, TAI_SAT_BEG, TIM_SAT_STEP
      INTEGER*4  NUM_ERR, I_FMT(VTD__M_SCC), IBEG_BUF(VTD__M_SCC), &
     &           IEND_BUF(VTD__M_SCC), IND_BUF, IND_LINE, IND_SOU, &
     &           KBUF, NBUF, IB, L_EPO, L_SAT, J1, J2, J3, J4, J5, J6, &
     &           LIND, IND(2,MIND), MJD_REF, NZO_FMT_ID, MJD_SAT_BEG, &
     &           IND_SELF, JND_SOU, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
!
! --- Check whether the file exists
!
      FL_FOUND = .FALSE.
      IB = 1
      DO 410 J1=1,VTD__M_SCC
         IBEG_BUF(J1) = 0
         IEND_BUF(J1) = 0
         IF ( ILEN(VTD%CONF%FINAM_SOUCOO(J1)) == 0 ) GOTO 410
         INQUIRE ( FILE=VTD%CONF%FINAM_SOUCOO(J1), EXIST=LEX ) 
         IF ( .NOT. LEX ) THEN
              CALL ERR_LOG ( 2161, IUER, 'VTD_LOAD_SOUCOO', 'File with '// &
     &            'source coordinates '// &
     &             VTD%CONF%FINAM_SOUCOO(J1)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(J1)))// &
     &            ' was not found' )
              RETURN 
         END IF
!
! ------ Read the file with source coordinates
!
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( VTD%CONF%FINAM_SOUCOO(J1), MBUF, BUF(IB), NBUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2162, IUER, 'VTD_LOAD_SOUCOO', 'Error in an '// &
     &            'attempt to read input file with source coordinates'// &
     &             VTD%CONF%FINAM_SOUCOO(J1) )
              RETURN 
         END IF
!
! ------ Check the lable and determine the format
!
         I_FMT(J1) = 0
         IF ( BUF(IB)(1:LEN(SOUCOO__LABEL1))        .EQ. SOUCOO__LABEL1 ) THEN
              I_FMT(J1) = 1
           ELSE IF ( BUF(IB)(1:LEN(SOUCOO__LABEL2)) .EQ. SOUCOO__LABEL2 ) THEN
              I_FMT(J1) = 2
           ELSE IF ( BUF(IB)(1:LEN(SOUCOO__LABEL3)) .EQ. SOUCOO__LABEL3 ) THEN
              I_FMT(J1) = 3
           ELSE IF ( BUF(IB)(1:LEN(SOUCOO__LABEL4)) .EQ. SOUCOO__LABEL4 ) THEN
              I_FMT(J1) = 4
           ELSE IF ( BUF(IB)(1:LEN(SOUCOO__LABEL5)) .EQ. SOUCOO__LABEL5 ) THEN
              I_FMT(J1) = 5
           ELSE IF ( BUF(IB)(1:LEN(SOUCOO__LABEL6)) .EQ. SOUCOO__LABEL6 ) THEN
              I_FMT(J1) = 6
           ELSE IF ( BUF(IB)(1:LEN(SOUCOO__LABEL7)) .EQ. SOUCOO__LABEL7 ) THEN
              I_FMT(J1) = 7
           ELSE 
              CALL ERR_LOG ( 2163, IUER, 'VTD_LOAD_SOUCOO', 'Error in an '// &
     &            'attempt to parse the input file with source coordinates '// &
     &             VTD%CONF%FINAM_SOUCOO(J1)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(J1)))// &
     &            ' -- format of this file was not recognized' )
              RETURN 
         END IF
         IBEG_BUF(J1) = IB
         IEND_BUF(J1) = IB + NBUF-1
         IB  = IB + NBUF
         KBUF = IB - 1
 410  CONTINUE 
!
! --- Parse source coordinate file
!
      MJD_REF = J2000__MJD  
      TAI_REF = 0.0D0
      VTD%L_SOU = 0
      NUM_ERR = 0
      DO 420 J2=1,KBUF
         IND_BUF = -1
         DO 430 J3=1,VTD__M_SCC
            IF ( J2 .GE. IBEG_BUF(J3) .AND. &
     &           J2 .LE. IEND_BUF(J3)       ) THEN
                 IND_BUF = J3
                 IND_LINE = J2 - IBEG_BUF(J3) + 1
            END IF
 430     CONTINUE 
!
         CALL EXWORD ( BUF(J2), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( LIND .GE. 3 ) THEN
              IF ( BUF(J2)(IND(1,1):IND(2,1)) == '#' .AND.        &
     &             ( BUF(J2)(IND(1,2):IND(2,2)) == 'EPOCH'   .OR. &
     &               BUF(J2)(IND(1,2):IND(2,2)) == 'EPOCH:'  .OR. &
     &               BUF(J2)(IND(1,2):IND(2,2)) == 'Reference_epoch: ' ) ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL DATE_TO_TIME ( BUF(J2)(IND(1,3):IND(2,3)), MJD_REF, TAI_REF, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J2, STR )
                        CALL ERR_LOG ( 2163, IUER, 'VTD_LOAD_SOUCOO', 'Error in '// &
     &                      'parsing the '//TRIM(STR)//'th line of the the input '// &
     &                      'file with source coordinates '//TRIM(VTD%CONF%FINAM_SOUCOO(J1))// &
     &                      ' -- failure to parse the catalogue reference epoch' )
                        RETURN 
                   END IF
              END IF
         END IF
         IF ( BUF(J2)(1:1) .EQ. '#' ) GOTO 420
         IF ( BUF(J2)(1:1) .EQ. '$' ) GOTO 420
         IF ( I_FMT(IND_BUF) .EQ. 1 ) THEN
!
! ----------- Search the source BUF(J2)(1:8) among the sources that 
! ----------- have already been found in this catalgoue. If the 
! ----------- sources has been already processed, the new record
! ----------- superseeds the prevous one
!
              IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(1:8) )
              IF ( IND_SOU > 0 ) THEN
!
! ---------------- Initialization
!
                   DO 440 J5=1,VTD__M_BND
                      VTD%SOU(IND_SOU)%FL_STRUC(J4) = .FALSE. ! Not defined
 440               CONTINUE 
!
! ---------------- Extract the source name
!
                   VTD%SOU(IND_SOU)%IVS_NAME = BUF(J2)(1:8)
                   VTD%SOU(IND_SOU)%J2000_NAME = BUF(J2)(10:19)
!
! ---------------- ... reference date
!
                   VTD%SOU(IND_SOU)%MJD_REF = MJD_REF
                   VTD%SOU(IND_SOU)%TAI_REF = TAI_REF
!
                   CALL CLRCH ( ALPHA_STR ) 
                   CALL CLRCH ( DELTA_STR ) 
                   ALPHA_STR = BUF(J2)(22:23)//'_'//BUF(J2)(25:26)//'_'//BUF(J2)(28:36)
                   DELTA_STR = BUF(J2)(38:40)//'_'//BUF(J2)(42:43)//'_'//BUF(J2)(45:52)
!
! ---------------- Parsing right ascension
!     
                   CALL ERR_PASS ( IUER, IER )
                   CALL HR_TAT ( ALPHA_STR, VTD%SOU(IND_SOU)%ALPHA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( IND_LINE, STR )
                        CALL ERR_LOG ( 2164, IUER, 'VTD_LOAD_SOUCOO', 'Error in '// &
          &                 'parsing line '//STR(1:I_LEN(STR))//' of the source '// &
          &                 'coordinate file '// &
          &                  TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
          &                 ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                       RETURN 
                  END IF 
!
! --------------- Parsing declination
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL GR_TAT ( DELTA_STR, VTD%SOU(IND_SOU)%DELTA, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR ) 
                       CALL INCH  ( IND_LINE, STR )
                       CALL ERR_LOG ( 2165, IUER, 'VTD_LOAD_SOUCOO', 'Error in '// &
          &                'parsing line '//STR(1:I_LEN(STR))//' of the source '// &
          &                'coordinate file '// &
          &                TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
          &                ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                       RETURN 
                  END IF 
!
                  VTD%SOU(IND_SOU)%S_CRS(1) = DCOS(VTD%SOU(IND_SOU)%ALPHA)* &
          &                                   DCOS(VTD%SOU(IND_SOU)%DELTA)
                  VTD%SOU(IND_SOU)%S_CRS(2) = DSIN(VTD%SOU(IND_SOU)%ALPHA)* &
          &                                     DCOS(VTD%SOU(IND_SOU)%DELTA)
                  VTD%SOU(IND_SOU)%S_CRS(3) = DSIN(VTD%SOU(IND_SOU)%DELTA)
                  VTD%SOU(IND_SOU)%OBJ_TYPE = VTD__MG
                  CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_DIR  )
                  CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_FILE )
                  FL_FOUND(IND_SOU) = .TRUE.
              END IF
           ELSE IF ( I_FMT(IND_BUF) .EQ. 2 ) THEN
!
! -------------- Search the source BUF(J2)(1:8) among the sources that 
! -------------- have already been found in this catalgoue. If the 
! -------------- sources has been already processed, the new record
! -------------- superseeds the previous one
!
              IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(5:12) )
              IF ( IND_SOU > 0 ) THEN
                   VTD%SOU(IND_SOU)%IVS_NAME = BUF(J2)(5:12)
                   VTD%SOU(IND_SOU)%J2000_NAME = '??????????'
                   VTD%SOU(IND_SOU)%MJD_REF = MJD_REF
                   VTD%SOU(IND_SOU)%TAI_REF = TAI_REF
!
                   CALL CLRCH ( ALPHA_STR ) 
                   CALL CLRCH ( DELTA_STR ) 
                   ALPHA_STR = BUF(J2)(15:16)//'_'//BUF(J2)(18:19)//'_'//BUF(J2)(21:31)
                   DELTA_STR = BUF(J2)(35:37)//'_'//BUF(J2)(39:40)//'_'//BUF(J2)(42:51)
!
! --------------- Parsing right ascension
!     
                  CALL ERR_PASS ( IUER, IER )
                  CALL HR_TAT ( ALPHA_STR, VTD%SOU(IND_SOU)%ALPHA, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR ) 
                       CALL INCH  ( IND_LINE, STR )
                       CALL ERR_LOG ( 2166, IUER, 'VTD_LOAD_SOUCOO', 'Error in '// &
          &                'parsing line '//STR(1:I_LEN(STR))//' of the source '// &
          &                'coordinate file '// &
          &                TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
          &                ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                       RETURN 
                  END IF 
!
! --------------- Parsing declination
!
                  CALL ERR_PASS ( IUER, IER )
                  CALL GR_TAT ( DELTA_STR, VTD%SOU(IND_SOU)%DELTA, IER )
                  IF ( IER .NE. 0 ) THEN
                       CALL CLRCH ( STR ) 
                       CALL INCH  ( IND_LINE, STR )
                       CALL ERR_LOG ( 2167, IUER, 'VTD_LOAD_SOUCOO', 'Error in '// &
          &                'parsing line '//STR(1:I_LEN(STR))//' of the source '// &
          &                'coordinate file '//VTD%CONF%FINAM_SOUCOO(IND_BUF) )
                       RETURN 
                  END IF 
!
                  VTD%SOU(IND_SOU)%S_CRS(1) = DCOS(VTD%SOU(IND_SOU)%ALPHA)* &
          &                                     DCOS(VTD%SOU(IND_SOU)%DELTA)
                  VTD%SOU(IND_SOU)%S_CRS(2) = DSIN(VTD%SOU(IND_SOU)%ALPHA)* &
          &                                     DCOS(VTD%SOU(IND_SOU)%DELTA)
                  VTD%SOU(IND_SOU)%S_CRS(3) = DSIN(VTD%SOU(IND_SOU)%DELTA)
                  VTD%SOU(IND_SOU)%OBJ_TYPE = VTD__MG
                  CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_DIR  )
                  CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_FILE )
                  FL_FOUND(IND_SOU) = .TRUE.
              END IF
            ELSE IF ( I_FMT(IND_BUF) .EQ. 3 ) THEN
!
! -------------- Search the source BUF(J2)(1:8) among the sources that 
! -------------- have already been found in this catalgoue. If the 
! -------------- sources has been already processed, the new record
! -------------- superseeds the prevous one
!
              IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(41:48) )
              IF ( IND_SOU > 0 ) THEN
                   VTD%SOU(IND_SOU)%IVS_NAME   = BUF(J2)(41:48)
                   VTD%SOU(IND_SOU)%J2000_NAME = '??????????'
                   VTD%SOU(IND_SOU)%NZO_NAME   = BUF(J2)(51:58)
                   VTD%SOU(IND_SOU)%OBJ_TYPE   = BUF(J2)(38:38)
                   VTD%SOU(IND_SOU)%MJD_REF    = MJD_REF
                   VTD%SOU(IND_SOU)%TAI_REF    = TAI_REF
                   CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_DIR  )
                   CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_FILE )
                   FL_FOUND(IND_SOU) = .TRUE.
              END IF
            ELSE IF ( I_FMT(IND_BUF) .EQ. 4 ) THEN
!
! ----------- Search the source BUF(J2)(1:8) among the sources that 
! ----------- have already been found in this catalgoue. If the 
! ----------- sources has been already processed, the new record
! ----------- superseeds the prevous one
!
              IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(1:8) )
              IF ( IND_SOU < 1 ) THEN
                   IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(23:30) )
              END IF
              IF ( IND_SOU < 1 ) THEN
                   IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(33:42) )
              END IF
              IF ( IND_SOU > 0 ) THEN
                   VTD%SOU(IND_SOU)%IVS_NAME   = BUF(J2)(1:8)
                   VTD%SOU(IND_SOU)%J2000_NAME = BUF(J2)(11:20)
                   VTD%SOU(IND_SOU)%NZO_NAME   = BUF(J2)(1:8)
                   VTD%SOU(IND_SOU)%OBJ_TYPE   = VTD__MG
                   VTD%SOU(IND_SOU)%MJD_REF    = MJD_REF
                   VTD%SOU(IND_SOU)%TAI_REF    = TAI_REF
!
                   CALL HR_TAT ( BUF(J2)(46:58), VTD%SOU(IND_SOU)%ALPHA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J2, STR )
                        CALL ERR_LOG ( 2168, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                      'in parsing right ascension of "'// &
     &                       BUF(J2)(46:58)//' at line '//STR(1:I_LEN(STR))// &
     &                      ' of source catalogue file '// &
     &                       TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
     &                      ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                        RETURN 
                   END IF
!
                   CALL GR_TAT ( BUF(J2)(60:72), VTD%SOU(IND_SOU)%DELTA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J2, STR )
                        CALL ERR_LOG ( 2169, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                      'in parsing declination of "'// &
     &                       BUF(J2)(60:72)//' at line '//STR(1:I_LEN(STR))// &
     &                      ' of source catalogue file '// &
     &                       TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
     &                      ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                        RETURN 
                   END IF
!
                   VTD%SOU(IND_SOU)%S_CRS(1) = DCOS(VTD%SOU(IND_SOU)%DELTA)* &
     &                                         DCOS(VTD%SOU(IND_SOU)%ALPHA)
                   VTD%SOU(IND_SOU)%S_CRS(2) = DCOS(VTD%SOU(IND_SOU)%DELTA)* &
     &                                         DSIN(VTD%SOU(IND_SOU)%ALPHA)
                   VTD%SOU(IND_SOU)%S_CRS(3) = DSIN(VTD%SOU(IND_SOU)%DELTA)
                   VTD%SOU(IND_SOU)%OBJ_TYPE = VTD__MG
                   CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_DIR  )
                   CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_FILE )
                   FL_FOUND(IND_SOU) = .TRUE.
                   IF ( IND_SOU < L_SOU ) THEN
                        JND_SOU = LTM_DIF ( 1, L_SOU-IND_SOU, C_SOU(IND_SOU+1), BUF(J2)(33:42) )
                        IF ( JND_SOU > 0 ) THEN
                             FL_FOUND(IND_SOU+JND_SOU) = .TRUE.
                           ELSE 
                             JND_SOU = LTM_DIF ( 1, L_SOU-IND_SOU, C_SOU(IND_SOU+1), BUF(J2)(23:30) )
                             IF ( JND_SOU > 0 ) THEN
                                  FL_FOUND(IND_SOU+JND_SOU) = .TRUE.
                             END IF
                        END IF
                   END IF
              END IF
            ELSE IF ( I_FMT(IND_BUF) .EQ. 5 ) THEN
              IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(4:11) )
              IF ( IND_SOU > 0 ) THEN
!               
                   VTD%SOU(IND_SOU)%IVS_NAME   = BUF(J2)(4:11)
                   VTD%SOU(IND_SOU)%J2000_NAME = BUF(J2)(14:23)
                   VTD%SOU(IND_SOU)%NZO_NAME   = BUF(J2)(4:11)
                   VTD%SOU(IND_SOU)%OBJ_TYPE   = VTD__MG
                   VTD%SOU(IND_SOU)%MJD_REF    = MJD_REF
                   VTD%SOU(IND_SOU)%TAI_REF    = TAI_REF
!
                   CALL HR_TAT ( BUF(J2)(26:40), VTD%SOU(IND_SOU)%ALPHA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J2, STR )
                        CALL ERR_LOG ( 2170, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                      'in parsing right ascension of "'// &
     &                       BUF(J2)(26:40)//' at line '//STR(1:I_LEN(STR))// &
     &                      ' of source catalogue file '// &
     &                       TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
     &                      ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                        RETURN 
                   END IF
!
                   CALL GR_TAT ( BUF(J2)(43:57), VTD%SOU(IND_SOU)%DELTA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J2, STR )
                        CALL ERR_LOG ( 2171, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                      'in parsing declination of "'// &
     &                       BUF(J2)(43:57)//' at line '//STR(1:I_LEN(STR))// &
     &                      ' of source catalogue file '// &
     &                       TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
     &                      ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                        RETURN 
                   END IF
!
                   VTD%SOU(IND_SOU)%S_CRS(1) = DCOS(VTD%SOU(IND_SOU)%DELTA)* &
     &                                         DCOS(VTD%SOU(IND_SOU)%ALPHA)
                   VTD%SOU(IND_SOU)%S_CRS(2) = DCOS(VTD%SOU(IND_SOU)%DELTA)* &
     &                                         DSIN(VTD%SOU(IND_SOU)%ALPHA)
                   VTD%SOU(IND_SOU)%S_CRS(3) = DSIN(VTD%SOU(IND_SOU)%DELTA)
                   VTD%SOU(IND_SOU)%OBJ_TYPE = VTD__MG
                   CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_DIR  )
                   CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_FILE )
                   FL_FOUND(IND_SOU) = .TRUE.
              END IF
            ELSE IF ( I_FMT(IND_BUF) .EQ. 6 ) THEN
              IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(17:24) )
              IF ( IND_SOU > 0 ) THEN
                   VTD%SOU(IND_SOU)%IVS_NAME   = BUF(J2)(17:24)
                   VTD%SOU(IND_SOU)%J2000_NAME = BUF(J2)(5:14)
                   VTD%SOU(IND_SOU)%NZO_NAME   = BUF(J2)(17:24)
                   VTD%SOU(IND_SOU)%OBJ_TYPE   = VTD__MG
                   VTD%SOU(IND_SOU)%MJD_REF    = MJD_REF
                   VTD%SOU(IND_SOU)%TAI_REF    = TAI_REF
!
                   BUF(J2)(29:29) = ':'
                   BUF(J2)(32:32) = ':'
                   CALL HR_TAT ( BUF(J2)(27:41), VTD%SOU(IND_SOU)%ALPHA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J2, STR )
                        CALL ERR_LOG ( 2170, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                      'in parsing right ascension of "'// &
     &                       BUF(J2)(26:40)//' at line '//STR(1:I_LEN(STR))// &
     &                      ' of source catalogue file '// &
     &                       TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
     &                      ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                        RETURN 
                   END IF
!
                   BUF(J2)(46:46) = ':'
                   BUF(J2)(49:49) = ':'
                   CALL GR_TAT ( BUF(J2)(43:57), VTD%SOU(IND_SOU)%DELTA, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR ) 
                        CALL INCH  ( J2, STR )
                        CALL ERR_LOG ( 2171, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                      'in parsing declination of "'// &
     &                       BUF(J2)(43:57)//' at line '//STR(1:I_LEN(STR))// &
     &                      ' of source catalogue file '// &
     &                       TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
     &                      ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                        RETURN 
                   END IF
!
                   VTD%SOU(IND_SOU)%S_CRS(1) = DCOS(VTD%SOU(IND_SOU)%DELTA)* &
     &                                         DCOS(VTD%SOU(IND_SOU)%ALPHA)
                   VTD%SOU(IND_SOU)%S_CRS(2) = DCOS(VTD%SOU(IND_SOU)%DELTA)* &
     &                                         DSIN(VTD%SOU(IND_SOU)%ALPHA)
                   VTD%SOU(IND_SOU)%S_CRS(3) = DSIN(VTD%SOU(IND_SOU)%DELTA)
                   VTD%SOU(IND_SOU)%OBJ_TYPE = VTD__MG
                   CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_DIR  )
                   CALL CLRCH ( VTD%SOU(IND_SOU)%NZO_FILE )
                   FL_FOUND(IND_SOU) = .TRUE.
              END IF
            ELSE IF ( I_FMT(IND_BUF) .EQ. 7 ) THEN
              IF ( BUF(J2)(43:43) == 'D' .OR. BUF(J2)(43:43) == 'F' ) THEN
                   IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(1:8) )
                   IF ( IND_SOU > 0 ) THEN
                        VTD%SOU(IND_SOU)%IVS_NAME   = BUF(J2)(1:8)
                        VTD%SOU(IND_SOU)%J2000_NAME = BUF(J2)(1:8)
                        VTD%SOU(IND_SOU)%NZO_NAME   = BUF(J2)(1:8)
                        VTD%SOU(IND_SOU)%OBJ_TYPE   = BUF(J2)(41:41)
                        IF ( VTD%SOU(IND_SOU)%OBJ_TYPE == VTD__SS .OR. &
     &                       VTD%SOU(IND_SOU)%OBJ_TYPE == VTD__ES .OR. &
     &                       VTD%SOU(IND_SOU)%OBJ_TYPE == VTD__EM      ) THEN
                             CONTINUE 
                          ELSE
                             CALL ERR_LOG ( 2172, IUER, 'VTD_LOAD_SOUCOO', 'Wrong '// &
     &                           'object type '//VTD%SOU(IND_SOU)%OBJ_TYPE// &
     &                           ' expected types: '//VTD__SS//', '// &
     &                           VTD__ES//', '//VTD__EM )
                             RETURN 
                        END IF
                        VTD%SOU(IND_SOU)%SOU_CRS = 0.0D0
                        VTD%SOU(IND_SOU)%S_CRS   = 0.0D0
                        VTD%SOU(IND_SOU)%ALPHA   = 0.0D0
                        VTD%SOU(IND_SOU)%DELTA   = 0.0D0
                        IF ( ILEN(BUF(J2)(11:15)) == 0 ) THEN
                             VTD%SOU(IND_SOU)%NORAD_ID = 0
                           ELSE
                             CALL CHIN ( BUF(J2)(11:15), VTD%SOU(IND_SOU)%NORAD_ID )
                        END IF
                        IF ( VTD%SOU(IND_SOU)%NORAD_ID < 0 ) THEN
                             CALL ERR_LOG ( 2173, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                           'in processing line '//TRIM(BUF(J2))//' of the '// &
     &                           'source position file: error in parsing NORAD id' )
                             RETURN 
                        END IF
                        IF ( BUF(J2)(43:43) == 'D' ) THEN
                             VTD%SOU(IND_SOU)%NZO_DIR  = BUF(J2)(46:)
                             IF ( .NOT. IS_DIR_EXIST ( VTD%SOU(IND_SOU)%NZO_DIR, STR ) ) THEN
                                  CALL ERR_LOG ( 2173, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                                'in processing line '//TRIM(BUF(J2))//' of the '// &
     &                                'source position file: orbit directory '// &
     &                                TRIM(VTD%SOU(IND_SOU)%NZO_DIR)//' does not exist' )
                                  RETURN 
                             END IF
                           ELSE IF ( BUF(J2)(43:43) == 'F' ) THEN
                             VTD%SOU(IND_SOU)%NZO_FILE = BUF(J2)(46:)
                             INQUIRE ( FILE=VTD%SOU(IND_SOU)%NZO_FILE, EXIST=LEX )
                             IF ( .NOT. LEX ) THEN
                                  CALL ERR_LOG ( 2174, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                                'in processing line '//TRIM(BUF(J2))//' of the '// &
     &                                'source position file: orbit file '// &
     &                                TRIM(VTD%SOU(IND_SOU)%NZO_FILE)//' does not exist' )
                                  RETURN 
                             END IF
!
                             CALL ERR_PASS ( IUER, IER )
                             CALL VTD_NZO_INQUIRE ( VTD%SOU(IND_SOU)%NZO_FILE, NZO_FMT_ID, &
     &                                              MJD_SAT_BEG, TAI_SAT_BEG, &
     &                                              TIM_SAT_STEP, L_EPO, L_SAT, SAT_NAM, IER )
                             IF ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 2175, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                                'in an attempt to inquire NZO file '//VTD%SOU(IND_SOU)%NZO_FILE )
                                  RETURN 
                             END IF
                             IF ( NZO_FMT_ID == VTD__TLE ) THEN
                                  IF ( BUF(J2)(11:15) .NE. SAT_NAM(1:5) ) THEN
                                       CALL ERR_LOG ( 2176, IUER, 'VTD_LOAD_SOUCOO', 'Mismatch '// &
     &                                     'between NORAD_ID defined in the source file '// &
     &                                     BUF(J2)(11:15)//' and NORAD_ID '//TRIM(SAT_NAM)// &
     &                                    ' defined in the NZO file '//VTD%SOU(IND_SOU)%NZO_FILE )
                                       RETURN 
                                  END IF
                             END IF
                        END IF
                        FL_FOUND(IND_SOU) = .TRUE.
                   END IF
                ELSE
                   IND_SOU = LTM_DIF ( 1, L_SOU, C_SOU, BUF(J2)(17:24) )
                   IF ( IND_SOU > 0 ) THEN
                        VTD%SOU(IND_SOU)%IVS_NAME   = BUF(J2)(17:24)
                        VTD%SOU(IND_SOU)%J2000_NAME = BUF(J2)(5:14)
                        VTD%SOU(IND_SOU)%NZO_NAME   = BUF(J2)(17:24)
                        VTD%SOU(IND_SOU)%OBJ_TYPE   = VTD__MG
                        VTD%SOU(IND_SOU)%MJD_REF    = MJD_REF
                        VTD%SOU(IND_SOU)%TAI_REF    = TAI_REF
!
                        BUF(J2)(29:29) = ':'
                        BUF(J2)(32:32) = ':'
                        CALL HR_TAT ( BUF(J2)(27:41), VTD%SOU(IND_SOU)%ALPHA, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR ) 
                             CALL INCH  ( J2, STR )
                             CALL ERR_LOG ( 2177, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                           'in parsing right ascension of "'// &
     &                            BUF(J2)(26:40)//' at line '//STR(1:I_LEN(STR))// &
     &                           ' of source catalogue file '// &
     &                            TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
     &                           ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                             RETURN 
                        END IF
!
                        BUF(J2)(46:46) = ':'
                        BUF(J2)(49:49) = ':'
                        CALL GR_TAT ( BUF(J2)(43:57), VTD%SOU(IND_SOU)%DELTA, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL CLRCH ( STR ) 
                             CALL INCH  ( J2, STR )
                             CALL ERR_LOG ( 2178, IUER, 'VTD_LOAD_SOUCOO', 'Error '// &
     &                           'in parsing declination of "'// &
     &                            BUF(J2)(43:57)//' at line '//STR(1:I_LEN(STR))// &
     &                           ' of source catalogue file '// &
     &                            TRIM(VTD%CONF%FINAM_SOUCOO(IND_BUF))// &
     &                           ' -- the line >>'//TRIM(BUF(J2))//'<<' )
                             RETURN 
                        END IF
!
                        VTD%SOU(IND_SOU)%S_CRS(1) = DCOS(VTD%SOU(IND_SOU)%DELTA)* &
     &                                              DCOS(VTD%SOU(IND_SOU)%ALPHA)
                        VTD%SOU(IND_SOU)%S_CRS(2) = DCOS(VTD%SOU(IND_SOU)%DELTA)* &
     &                                              DSIN(VTD%SOU(IND_SOU)%ALPHA)
                        VTD%SOU(IND_SOU)%S_CRS(3) = DSIN(VTD%SOU(IND_SOU)%DELTA)
                        VTD%SOU(IND_SOU)%OBJ_TYPE = VTD__MG
                        FL_FOUND(IND_SOU) = .TRUE.
                   END IF
              END IF
          END IF
 420   CONTINUE 
!
       VTD%L_SOU = 0
       DO 450 J5=1,L_SOU      
          IF ( ILEN(C_SOU(J5)) == 0 ) THEN
               CALL CLRCH (     STR )
               CALL INCH  ( J5, STR )
               CALL ERR_LOG ( 2179, IUER, 'VTD_LOAD_SOUCOO', 'The '// &
     &              STR(1:I_LEN(STR))//' source to be loaded has '// &
     &              'an empty name' )
               NUM_ERR = NUM_ERR + 1
               GOTO 450
          END IF
          IF ( .NOT. FL_FOUND(J5) .AND. VTD%L_NZO > 0 ) THEN
               DO 460 J6=1,VTD%L_NZO
                  IF ( VTD%NZO(J6)%NAME == C_SOU(J5) ) THEN
                       VTD%L_SOU = VTD%L_SOU + 1
                       VTD%SOU(VTD%L_SOU)%IVS_NAME   = C_SOU(J5)
                       VTD%SOU(VTD%L_SOU)%J2000_NAME = 'J_'//C_SOU(J5)
                       VTD%SOU(VTD%L_SOU)%NZO_NAME   = C_SOU(J5)
                       VTD%SOU(VTD%L_SOU)%IND_NZO    = J6
                       FL_FOUND(J5) = .TRUE.
                       VTD%SOU(VTD%L_SOU)%OBJ_TYPE = VTD%NZO(J6)%OBJ_TYPE
                  END IF
 460           CONTINUE 
          END IF
!
! ------- We do this to support the situation when a given source 
! ------- is defined two times
!
          IND_SELF = LTM_DIF ( 1, L_SOU, C_SOU, C_SOU(J5) )
          IF ( .NOT. FL_FOUND(J5) .AND. .NOT. FL_FOUND(IND_SELF) ) THEN
               CALL ERR_PASS ( IUER, IER )
               CALL ERR_LOG ( 2180, IUER, 'VTD_LOAD_SOUCOO', 'Source '// &
     &              C_SOU(J5)//' was not found in source catalogue file '// &
     &              VTD%CONF%FINAM_SOUCOO(IND_BUF) )
               NUM_ERR = NUM_ERR + 1
             ELSE
               VTD%L_SOU = VTD%L_SOU + 1
          END IF
 450  CONTINUE 
!
      IF ( NUM_ERR .GT. 0 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NUM_ERR, STR ) 
           CALL ERR_LOG ( 2181, IUER, 'VTD_LOAD_SOUCOO', STR(1:I_LEN(STR))// &
     &         ' sources were not found in the source catalogue files '//     &
     &          VTD%CONF%FINAM_SOUCOO(1)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(1)))//' '// &
     &          VTD%CONF%FINAM_SOUCOO(2)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(2)))//' '// &
     &          VTD%CONF%FINAM_SOUCOO(3)(1:I_LEN(VTD%CONF%FINAM_SOUCOO(3)))//' '// &
     &          VTD%CONF%FINAM_SOUCOO(4) )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD_SOUCOO  !#!#
