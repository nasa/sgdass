      PROGRAM    MALO_SPR_ZERO
! ************************************************************************
! *                                                                      *
! *   Routine MALO_SPR_ZERO
! *                                                                      *
! * ### 22-OCT-2012   MALO_SPR_ZERO  v1.0 (c) L. Petrov  23-OCT-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  DEG, IVRB, IUER
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ), POINTER :: MALO(:)
      CHARACTER  STR*128, FILIN*128, FILOUT*128
      INTEGER*4  J1, J2, J3, J4, J5, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: malo_spr_zero file_in file_out [verbosity_level]'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, FILIN   )
           CALL GETARG ( 2, FILOUT  )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, STR )
                CALL CHIN   ( STR, IVRB )
                IF ( IVRB < 0 .OR. IVRB > 99  ) THEN
                     IUER = -2
                     CALL ERR_LOG ( 6501, IUER, 'MALO_SPR_ZERO', 'Wrong verbosity '// &
     &                   'level argument: '//STR(1:I_LEN(STR))// &
     &                   ' an integer in range [0, 99] was expected' )
                     CALL EXIT ( 1 )
                END IF
              ELSE 
                IVRB = 1
           END IF
      END IF
!
      ALLOCATE ( MALO(2), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6502, IUER, 'MALO_SPR_ZERO', 'Error in an attempt '// &
     &         'to allocate memory for object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,2
         IUER = -1
         CALL MALO_INIT ( MALO(J1), IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6503, IUER, 'MALO_SPR_ZERO', 'Error in an attempt '// &
     &            'to initialize object MALO' )
              CALL EXIT ( 1 )
         END IF
 410  CONTINUE 
!
      IUER = -1
      CALL SPR_READ_NC ( FILIN, MALO(1), IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6504, IUER, 'MALO_SPR_ZERO', 'Error in an attempt '// &
     &         'to read the surface pressure from the input file '// &
     &          FILIN )
           CALL EXIT ( 1 )
      END IF
!
      MALO(2) = MALO(1)
      MALO(2)%LAT     => NULL()
      MALO(2)%LON     => NULL()
      MALO(2)%MJD_ARR => NULL()
      MALO(2)%TAI_ARR => NULL()
      MALO(2)%SPR     => NULL()
!
      ALLOCATE ( MALO(2)%LON(MALO(2)%NLON), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH  ( 4*MALO(2)%NLON, STR )
           CALL ERR_LOG ( 6505, IUER, 'MALO_SPR_ZERO', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO(2)%LON' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO(2)%LAT(MALO(2)%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO(2)%NLAT, STR )
           CALL ERR_LOG ( 6506, IUER, 'MALO_SPR_ZERO', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO(2)%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO(2)%MJD_ARR(MALO(2)%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO(2)%NTIM, STR )
           CALL ERR_LOG ( 6507, IUER, 'MALO_SPR_ZERO', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO(2)%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO(2)%TAI_ARR(MALO(2)%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO(2)%NTIM, STR )
           CALL ERR_LOG ( 6508, IUER, 'MALO_SPR_ZERO', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO(2)%LAT' )
           RETURN 
      END IF 
!
      ALLOCATE ( MALO(2)%SPR(MALO(2)%NLON,MALO(2)%NLAT,MALO(2)%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO(2)%NLON*MALO(2)%NLAT*MALO(2)%NTIM, STR )
           CALL ERR_LOG ( 6509, IUER, 'MALO_SPR_ZERO', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes dynamic memory for array MALO(2)%LAT' )
           RETURN 
      END IF 
!
      DO 420 J2=1,MALO(2)%NTIM
         DO 430 J3=1,MALO(2)%NLAT
            IF ( J2 == 1 ) THEN
                 MALO(2)%LAT(J3) = MALO(1)%LAT(J3) 
            END IF
            DO 440 J4=1,MALO(1)%NLON
               IF ( J2 == 1 .AND. J3 == 1 ) THEN
                    MALO(2)%LON(J4) = MALO(1)%LON(J4) 
               END IF
               MALO(2)%SPR(J4,J3,J2) = 0.0
 440        CONTINUE 
 430     CONTINUE 
         MALO(2)%MJD_ARR(J2) = MALO(1)%MJD_ARR(J2) 
         MALO(2)%TAI_ARR(J2) = MALO(1)%TAI_ARR(J2) 
 420  CONTINUE 
!
      IUER = -1
      CALL SPR_WRITE_NC ( MALO(2), FILOUT, MALO(2)%MJD_BEG, MALO(2)%UTC_BEG, &
     &                             MALO(2)%MJD_END, MALO(2)%UTC_END, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6510, IUER, 'MALO_SPR_ZERO', 'Error in an attempt '// &
     &         'to write the output surface pressure to the file with generic '// &
     &         'name '//FILOUT )
           CALL EXIT ( 1 )
      END IF 
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Output file '//MALO(2)%FILOUT(1:I_LEN(MALO(2)%FILOUT))
      END IF
!
      END  PROGRAM   MALO_SPR_ZERO  !#!#
