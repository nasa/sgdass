      SUBROUTINE MALO_MODC_PARSE ( MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_MODC_PARSE fills object MALO%MODC that keeps          *
! *   parameters of the regression model for the surface pressure field  *
! *   according to the model code specified in MALO%CONF%MODEL_CODE      *
! *                                                                      *
! * ### 18-FEB-2017 MALO_MODC_PARSE v1.0 (c) L. Petrov  18-FEB-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  IUER
      CHARACTER  WAV_NAM*4, TYP_NAM*3, STR*128, STR1*128
      LOGICAL*1  FL_WAV
      REAL*8     SEC
      INTEGER*4  J1, J2, J3, J4, IND_EPC, MJD, IER
!
      MALO%NMDC            = 0
      MALO%IND_MOD_CNST    = 0
      MALO%MODC_NHAR       = 0
      MALO%MODC_NHAR_SUBTR = 0
      MALO%MDC_STATUS = MALO__UNDF
!
      IF ( MALO%CONF%MODEL_CODE < 1 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( MALO%CONF%MODEL_CODE, STR )
           CALL ERR_LOG ( 2661, IUER, 'MALO_MODC_PARSE', 'Wrong value '// &
     &         '(too low) of the regression model code: '//STR )
           RETURN 
      END IF
!
      IF ( MALO%CONF%MODEL_CODE > MALO__MFS ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( MALO%CONF%MODEL_CODE, STR )
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( MALO__MFS, STR1 )
           CALL ERR_LOG ( 2662, IUER, 'MALO_MODC_PARSE', 'Wrong value '// &
     &         '(too large) of the regression model code: '//TRIM(STR)// &
     &         ' the model code should be in range [ 1, '//TRIM(STR1)//' ]' )
           RETURN 
      END IF
!
      MALO%NMDC = MALO_NFS(MALO%CONF%MODEL_CODE)
      MALO%IND_MOD_CNST = -1
      ALLOCATE ( MALO%MODC(MALO%NMDC), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2663, IUER, 'MALO_MODC_PARSE', 'Failure '// &
     &         'to allocate MALO%MODC object' )
           RETURN 
      END IF
!
      DO 410 J1=1,MALO%NMDC
         WAV_NAM = MALO_WFS(J1,MALO%CONF%MODEL_CODE)(1:4)
         TYP_NAM = MALO_WFS(J1,MALO%CONF%MODEL_CODE)(6:8)
         CALL UNDERSCORE_TO_BLANK ( WAV_NAM )
         FL_WAV = .FALSE.
         DO 420 J2=1,MALO__MFRQ
            IF ( WAV_NAM == MDC_WAVE(J2) ) THEN
                 FL_WAV = .TRUE.
                 MALO%MODC(J1)%PHS = MDC_PHAS(J2)
                 MALO%MODC(J1)%FRQ = MDC_FREQ(J2)
                 MALO%MODC(J1)%ACC = 0.0D0
            END IF 
 420     CONTINUE 
         IF ( .NOT. FL_WAV ) THEN
               CALL ERR_LOG ( 2664, IUER, 'MALO_MODC_PARSE', 'Trap of '// &
     &             'internal control: cannot associate wave name '// &
     &              WAV_NAM//' with that in the MDC_WAVE table' )
               RETURN 
         END IF
         IF ( WAV_NAM(1:3) == 'JMP' ) THEN
              MALO%MODC(J1)%TYP = MALO__JMP
              CALL CHIN ( TYP_NAM(2:3), IND_EPC )
              IF ( IND_EPC .GE. 1 .AND. IND_EPC .LE. MALO__MTIMS ) THEN
                   CALL ERR_LOG ( 0, IER )
                   CALL DATE_TO_TIME ( MALO_TIMS(IND_EPC), MJD, SEC, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2666, IUER, 'MALO_MODC_PARSE', 'Trap of '// &
     &                      'internal control in parsing '//MALO_TIMS(IND_EPC) )
                        RETURN 
                   END IF
                   MALO%MODC(J1)%TIM = (MJD - MALO%MJD_DEF)*86400.0D0 + (SEC - MALO%TAI_DEF)
                 ELSE 
                   CALL ERR_LOG ( 2667, IUER, 'MALO_MODC_PARSE', 'Trap of '// &
     &                 'internal control: cannot associate wave name '// &
     &                  WAV_NAM//' with that in the MDC_WAVE table' )
                   RETURN 
              END IF
            ELSE IF ( WAV_NAM == 'CNST' ) THEN
              MALO%IND_MOD_CNST = J1
              MALO%MODC(J1)%TYP = MALO__CNST
            ELSE IF ( WAV_NAM == 'DRFT' ) THEN
              MALO%MODC(J1)%TYP = MALO__DRFT
            ELSE
              IF ( TYP_NAM == 'cos' ) THEN
                   MALO%MODC(J1)%TYP = MALO__COS
                   MALO%MODC_NHAR    = MALO%MODC_NHAR + 1
                   IF ( J1 .LE. MALO_HFS(MALO%CONF%MODEL_CODE) ) THEN
                        MALO%MODC_NHAR_SUBTR = MALO%MODC_NHAR_SUBTR + 1
                   END IF
                ELSE IF ( TYP_NAM == 'sin' ) THEN
                   MALO%MODC(J1)%TYP = MALO__SIN
                   IF ( J1 .LE. MALO_HFS(MALO%CONF%MODEL_CODE) ) THEN
                        MALO%MODC_NHAR_SUBTR = MALO%MODC_NHAR_SUBTR + 1
                   END IF
                 ELSE 
                   CALL ERR_LOG ( 2668, IUER, 'MALO_MODC_PARSE', 'Trap of '// &
     &                 'internal control: cannot associate wave name '// &
     &                  WAV_NAM//' with that in the MDC_WAVE table' )
                   RETURN 
              END IF
         END IF
 410  CONTINUE 
      MALO%MDC_STATUS = MALO__LOAD
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_MODC_PARSE  !#!#
