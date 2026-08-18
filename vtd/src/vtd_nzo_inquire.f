      SUBROUTINE VTD_NZO_INQUIRE ( NZO_FIL, NZO_FMT_ID, MJD_BEG, TAI_BEG, &
     &                             TIM_STEP, L_EPO, L_SAT, SAT_NAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_NZO_INQUIRE reads a part of a file with ephemeride     *
! *   of a near zone objects and returns some information from there.    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *                                                                      *
! * NZO_FIL    ( CHARACTER ) -- A file with ephemeride of a near zone    *
! *                             object.                                  *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * NZO_FMT_ID ( INTEGER*4 ) -- Format of the near zone orbit.           *
! *                             Supported formats:                       *
! *                             VTD__NZO -- VTD NZO format.              *
! *                             VTD__SP3 -- IGS SP3 format for GNSS      *
! *                                         orbits.                      *
! *                             VTD__TLE -- two line element format.     *
! *                             VTD__ODM -- CCSDS format.                *
! * MJD_BEG    ( INTEGER*4 ) -- MJD of the ephemeride start.             *
! * TIM_BEG    ( REAL*8    ) -- TAI of the ephemeride start.             *
! * TIM_STEP   ( REAL*8    ) -- Time step of the ephemeride.             *
! * L_EPO      ( INTEGER*4 ) -- The number of epochs of the ephemeride.  *
! * L_SAT      ( INTEGER*4 ) -- The number of satellites in the          *
! *                             ephemeride.                              *
! * SAT_NAM    ( CHARACTER ) -- Satellite id                             *
! *                             VTD__NZO -- satellite name               *
! *                             VTD__ODM -- satellite name               *
! *                             VTD__TLE -- NORAD ID, 5-character long   *
! *                             VTD__SP3 -- ?? (not supported)           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * ### 22-AUG-2025  VTD_NZO_INQUIRE  v1.0 (d) L. Petrov 22-AUG-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      CHARACTER  NZO_FIL*(*), SAT_NAM*(*)
      INTEGER*4  NZO_FMT_ID, MJD_BEG, L_EPO, L_SAT, IUER
      REAL*8     TAI_BEG, TIM_STEP
      CHARACTER  STR*128
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      INTEGER*4  IS, UNIX_DATE, M_BUF, N_BUF, MJD_VAL, DOY, J1, J2, IER
      LOGICAL*1  FL_META
      REAL*8     TAI_VAL
      INTEGER*8  SIZE_I8
      INTEGER*4, EXTERNAL :: FILE_INFO 
!
      SAT_NAM = '??'
!
! --- Get information about the file
!
      IS = FILE_INFO ( TRIM(NZO_FIL)//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 2721, IUER, 'VTD_NZO_INQUIRE', 'Error in '// &
     &         'attempt in get information about the satellite '// &
     &         'ephemeride file '//TRIM(NZO_FIL)//' -- '//STR )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
! --- Aloocate memory for the file contents
!
      M_BUF = SIZE_I8/60 + 256
      ALLOCATE ( BUF(M_BUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR) 
           CALL INCH  ( M_BUF, STR ) 
           CALL ERR_LOG ( 2722, IUER, 'VTD_NZO_INQUIRE', 'Error in '// &
     &         'attempt in allocate '//TRIM(STR)//' bytes of dynamic '// &
     &         'memory for the buffer for the ephemeride file' )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
! --- Read the file 
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( NZO_FIL, M_BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2723, IUER, 'VTD_NZO_INQUIRE', 'Error in '// &
     &         'reading the ephemeride file '//TRIM(NZO_FIL)//' -- '//STR )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
      IF ( N_BUF < 2 ) THEN
           CALL CLRCH ( STR) 
           CALL INCH  ( M_BUF, STR ) 
           CALL ERR_LOG ( 2724, IUER, 'VTD_NZO_INQUIRE', 'Malformed '// &
     &         'ephemeride file '//TRIM(NZO_FIL)//' -- it should have '// &
     &         'at least 2 lines, but has only '//TRIM(STR)//' lines' )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
! --- Determine file format
!
      IF ( BUF(1)(1:LEN(NZO__LABEL)) == NZO__LABEL ) THEN
           NZO_FMT_ID = VTD__NZO
         ELSE IF ( BUF(1)(1:2) .EQ. '#d' .AND. &
     &         BUF(2)(1:2) .EQ. '##' .AND. &
     &         BUF(N_BUF)(1:3) .EQ. 'EOF'  ) THEN
           NZO_FMT_ID = VTD__SP3
         ELSE IF ( BUF(1)(1:14) .EQ. 'CCSDS_OEM_VERS' .AND. &
     &             BUF(2)(1:10) .EQ. 'META_START'           ) THEN
           NZO_FMT_ID = VTD__ODM
         ELSE IF ( N_BUF .LE. 3 ) THEN
           IF ( ( BUF(1)(1:2) == '1 ' .AND. BUF(1)(24:24) == '.' .AND. BUF(2)(1:2) == '2 ' ) .OR. &
     &          ( BUF(2)(1:2) == '1 ' .AND. BUF(2)(24:24) == '.' .AND. BUF(3)(1:2) == '2 ' )      ) THEN
                NZO_FMT_ID = VTD__TLE
           END IF
         ELSE 
           CALL ERR_LOG ( 2725, IUER, 'VTD_NZO_INQUIRE', 'Malformed '// &
     &         'ephemeride file '//TRIM(NZO_FIL)//' the first line is '// &
     &         'expected to start from #d and the last line is expected '// &
     &         'to contain EOF, but the first line was '//BUF(1) )
           DEALLOCATE ( BUF ) 
           RETURN 
      END IF
!
      IF ( NZO_FMT_ID == VTD__NZO .OR. &
     &     NZO_FMT_ID == VTD__SP3 .OR. &
     &     NZO_FMT_ID == VTD__ODM      ) THEN
           IF ( N_BUF < 25 ) THEN
                CALL CLRCH ( STR) 
                CALL INCH  ( M_BUF, STR ) 
                CALL ERR_LOG ( 2726, IUER, 'VTD_NZO_INQUIRE', 'Malformed '// &
     &              'ephemeride file '//TRIM(NZO_FIL)//' -- it should have '// &
     &               'at least 25 lines, but has only '//TRIM(STR)//' lines' )
                DEALLOCATE ( BUF ) 
                RETURN 
           END IF
      END IF
!
      IF ( NZO_FMT_ID == VTD__NZO ) THEN
!
! -------- Derermine the number of epochs and the time step
!
           L_SAT = 1 
           MJD_BEG = -1
           L_EPO   = 0
           DO 410 J1=1,N_BUF
              IF ( BUF(J1)(1:8) == 'TIM_ARG:' ) THEN
                   IF ( MJD_BEG < 0 ) THEN
                        CALL ERR_PASS ( IUER, IER )
                        CALL DATE_TO_TIME ( BUF(J1)(10:38), MJD_BEG, TAI_BEG, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 2727, IUER, 'VTD_NZO_INQUIRE', 'Error '// &
     &                           'in parsing date '//BUF(J1)(10:38) )
                             DEALLOCATE ( BUF ) 
                             RETURN 
                        END IF
                   END IF
                   IF ( L_EPO == 1 ) THEN
                        CALL ERR_PASS ( IUER, IER )
                        CALL DATE_TO_TIME ( BUF(J1)(10:38), MJD_VAL, TAI_VAL, IER )
                        IF ( IER .NE. 0 ) THEN
                             CALL ERR_LOG ( 2728, IUER, 'VTD_NZO_INQUIRE', 'Error '// &
     &                           'in parsing date '//BUF(J1)(10:38) )
                             DEALLOCATE ( BUF ) 
                             RETURN 
                        END IF
                        TIM_STEP = (MJD_VAL - MJD_BEG)*86400.0D0 + (TAI_VAL - TAI_BEG)
                   END IF
                   L_EPO = L_EPO + 1
                 ELSE IF ( BUF(J1)(1:12) == 'OBJECT_NAME:' ) THEN
                   SAT_NAM = BUF(J1)(21:)
              END IF 
 410       CONTINUE 
        ELSE IF ( NZO_FMT_ID == VTD__ODM ) THEN
           FL_META = .TRUE.
           DO 420 J2=1,N_BUF
              IF ( BUF(J2)(1:1) == ' ' ) GOTO 420
              IF ( BUF(J2)(1:15) == 'OBJECT_NAME   =' ) THEN
                   CALL CHASHL ( BUF(J2)(17:) )
                   SAT_NAM = BUF(J2)(17:)
                 ELSE IF ( BUF(J2)(1:15) == 'START_TIME    =' ) THEN
                   CALL CHASHL ( BUF(J2)(17:) )
                   CALL ERR_PASS ( IUER, IER )
                   CALL DATE_TO_TIME ( BUF(J2)(17:45), MJD_BEG, TAI_BEG, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2729, IUER, 'VTD_NZO_INQUIRE', 'Error '// &
     &                      'in parsing date '//BUF(J1)(10:38) )
                        DEALLOCATE ( BUF ) 
                        RETURN 
                   END IF
                 ELSE IF ( BUF(J2)(1:9) == 'META_STOP' ) THEN
                   FL_META = .FALSE.
                 ELSE
                   IF ( .NOT. FL_META ) THEN
                        IF ( L_EPO == 1 ) THEN
                             CALL CHASHL ( BUF(J2)(17:) )
                             CALL ERR_PASS ( IUER, IER )
                             CALL DATE_TO_TIME ( BUF(J2)(17:45), MJD_VAL, TAI_VAL, IER )
                             IF ( IER .NE. 0 ) THEN
                                  CALL ERR_LOG ( 2730, IUER, 'VTD_NZO_INQUIRE', 'Error '// &
     &                                'in parsing date '//BUF(J1)(10:38) )
                                  DEALLOCATE ( BUF ) 
                                  RETURN 
                             END IF
                             TIM_STEP = (MJD_VAL - MJD_BEG)*86400.0D0 + (TAI_VAL - TAI_BEG)
                        END IF
                        L_EPO = L_EPO + 1
                   END IF                  
              END IF
 420       CONTINUE 
        ELSE IF ( NZO_FMT_ID == VTD__SP3 ) THEN
!
! -------- Red some information from the header of a file in SP3 format
!
           CALL CHIN ( BUF(1)(33:39), L_EPO   )
           CALL CHIN ( BUF(2)(40:44), MJD_BEG )
           CALL CHIN ( BUF(3)(4:6),   L_SAT   )
           READ ( UNIT=BUF(2)(25:38), FMT='(F14.8)'  ) TIM_STEP
           READ ( UNIT=BUF(2)(46:60), FMT='(F15.10)' ) TAI_BEG
           TAI_BEG = TAI_BEG - VTD__GPS_M_TAI
           IF ( TAI_BEG .GE. 86400.0D0 ) THEN
                TAI_BEG = TAI_BEG - 86400.0D0
                MJD_BEG = MJD_BEG + 1
           END IF
        ELSE IF ( NZO_FMT_ID == VTD__TLE ) THEN
           IER = 0
           IF ( BUF(1)(1:2) == '1 ' ) THEN
                CALL CHIN ( BUF(1)(21:23), DOY )
                SAT_NAM = BUF(1)(3:7)
                STR = '20'//BUF(1)(19:20)//'.01.01_00:00:00'
                READ ( UNIT=BUF(1)(24:32), FMT='(F9.8)', IOSTAT=IER ) TAI_BEG
              ELSE
                CALL CHIN ( BUF(2)(21:23), DOY )
                STR = '20'//BUF(2)(19:20)//'.01.01_00:00:00'
                READ ( UNIT=BUF(2)(24:32), FMT='(F9.8)', IOSTAT=IER ) TAI_BEG
           END IF
           IF ( DOY < 1 ) THEN
                CALL ERR_LOG ( 2731, IUER, 'VTD_NZO_INQUIRE', 'Malformed '// &
     &              'ephemeride file '//TRIM(NZO_FIL)//' in TLE format: '// &
     &              'wrong date of year' )
                DEALLOCATE ( BUF ) 
                RETURN 
           END IF
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2732, IUER, 'VTD_NZO_INQUIRE', 'Malformed '// &
     &              'ephemeride file '//TRIM(NZO_FIL)//' in TLE format: '// &
     &              'wrong time '//BUF(1)(24:32) )
                DEALLOCATE ( BUF ) 
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL DATE_TO_TIME ( STR(1:19), MJD_BEG, TAI_VAL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2733, IUER, 'VTD_NZO_INQUIRE', 'Error '// &
     &              'in parsing date '//STR(1:19) )
                     DEALLOCATE ( BUF ) 
                RETURN 
           END IF
           MJD_BEG = MJD_BEG + DOY - 1
           TAI_BEG = 86400.0D0*TAI_BEG
           L_EPO = 1
           L_SAT = 1
      END IF
      DEALLOCATE ( BUF ) 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_NZO_INQUIRE  !#!#
