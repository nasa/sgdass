      SUBROUTINE READ_PCAL_MASK ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_PCAL_MASK 
! *                                                                      *
! * ### 11-MAY-2015  READ_PCAL_MASK   v2.1 (c) L. Petrov 11-MAY-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      CHARACTER  STR*128, STR1*128
      CHARACTER, ALLOCATABLE :: BUF(:)*80
      INTEGER*4  J1, J2, J3, IND_STA, IND_FRQ, IND_TONE, MBUF, NBUF, IER
      LOGICAL*1  FL_EXIST
      TYPE     ( PCAL_MASK_TEXT__TYPE ) :: PPM
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( PIM%CONF%PCAL_MASK_FILE == 'NO' .OR. &
     &     ILEN(PIM%CONF%PCAL_MASK_FILE) == 0 ) THEN
           CALL ERR_LOG ( 4141, IUER, 'READ_PCAL_MASK', 'The pcal '// &
     &         'mask file has not been defined in PIM%CONF' )
           RETURN 
      END IF
!
      IF ( PIM%NPCT .LE. 2 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( PIM%NPCT, STR )
           CALL ERR_LOG ( 4142, IUER, 'READ_PCAL_MASK', 'Too few pcal tones '// &
     &         'were extracted during processing the experiment: '// &
     &          STR(1:I_LEN(STR))//' -- at least 3 per IF are required' )
           RETURN 
      END IF
!
      INQUIRE ( FILE=PIM%CONF%PCAL_MASK_FILE, EXIST=FL_EXIST )
      IF ( .NOT. FL_EXIST ) THEN
           CALL ERR_LOG ( 4143, IUER, 'READ_PCAL_MASK', 'The pcal '// &
     &         'mask file '// &
     &          PIM%CONF%PCAL_MASK_FILE(1:I_LEN(PIM%CONF%PCAL_MASK_FILE))// &
     &         ' was not found' )
           RETURN 
      END IF
!
      MBUF = 32 + PIM__MSTA*(PIM%NFRQ*(PIM%NPCT+2))
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 4144, IUER, 'READ_PCAL_MASK', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for a temporary buffer' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( PIM%PCAL_MASK ) ) THEN
           DEALLOCATE ( PIM%PCAL_MASK )
      END IF
!
      ALLOCATE ( PIM%PCAL_MASK(PIM%NPCT,PIM%NFRQ,PIM%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NPCT*PIM%NFRQ*PIM%NSTA, STR )
           CALL ERR_LOG ( 4145, IUER, 'READ_PCAL_MASK', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the pcal mask' )
           RETURN 
      END IF
      PIM%PCAL_MASK_STATUS = PIMA__ALLOCATED
!
! --- Initialization
!
      PIM%PCAL_MASK = 1
!
! --- Read the badnpass mask file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIM%CONF%PCAL_MASK_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4146, IUER, 'READ_PCAL_MASK', 'Error '// &
     &         'in an attempt to read the input file with the pcal '// &
     &         'mask '//PIM%CONF%PCAL_MASK_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Check the first line of the file
!
      IF ( BUF(1)(1:LEN(PIMA__PCAL_MASK_LABEL)) == PIMA__PCAL_MASK_LABEL(1:LEN(PIMA__PCAL_MASK_LABEL)) ) THEN
           CONTINUE 
        ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG ( 4147, IUER, 'READ_PCAL_MASK', 'Wrong format '// &
     &         'of the pcal mask file '// &
     &          PIM%CONF%PCAL_MASK_FILE(1:I_LEN(PIM%CONF%PCAL_MASK_FILE))// &
     &         ' the first line is '//STR(1:LEN(PIMA__BPASS_MASK_LABEL))// &
     &         ' while the format identification '//PIMA__BPASS_MASK_LABEL// &
     &         ' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Check the last line of the file
!
      IF ( BUF(NBUF)(1:LEN(PIMA__PCAL_MASK_LABEL)) == PIMA__PCAL_MASK_LABEL(1:LEN(PIMA__PCAL_MASK_LABEL)) ) THEN
           CONTINUE 
        ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG ( 4148, IUER, 'READ_PCAL_MASK', 'Corrupted '// &
     &         'pcal mask file '// &
     &          PIM%CONF%PCAL_MASK_FILE(1:I_LEN(PIM%CONF%PCAL_MASK_FILE))// &
     &         ' the last line is '//STR(1:LEN(PIMA__BPASS_MASK_LABEL))// &
     &         ' while the format identification '//PIMA__BPASS_MASK_LABEL// &
     &         ' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Cycle over lines of the badnpass mask file
!
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         IF ( BUF(J1)(1:1) == ' ' ) GOTO 410
         IF ( BUF(J1)(1:1) == '!' ) GOTO 410
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ Fill PPM with the tempplate
!
         CALL LIB$MOVC3 ( LEN(PCAL_MASK_TEMPLATE), %REF(BUF(J1)), PPM )
!
         IND_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, PPM%STA_NAM )
         IF ( IND_STA .LE. 0 ) GOTO 410
!
         CALL CHIN ( PPM%IND_FRQ, IND_FRQ )
         IF ( IND_FRQ < 1  .OR.  IND_FRQ > PIM%NFRQ ) THEN
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%NFRQ, STR1 )
              CALL ERR_LOG( 4149, IUER, 'READ_PCAL_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th of the pcal '// &
     &            'mask file '// &
     &             PIM%CONF%PCAL_MASK_FILE(1:I_LEN(PIM%CONF%PCAL_MASK_FILE))// & 
     &            ' the frequency index '//PPM%IND_FRQ//' is out of the '// &
     &            'range [1,'//STR1(1:I_LEN(STR1))//'] ' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
         CALL CHIN ( PPM%IND_TONE, IND_TONE )
         IF ( IND_TONE < 1  .OR.  IND_TONE > PIM%NPCT ) THEN
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%NPCT, STR1 )
              CALL ERR_LOG( 4150, IUER, 'READ_PCAL_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th of the pcal '// &
     &            'mask file '// &
     &             PIM%CONF%PCAL_MASK_FILE(1:I_LEN(PIM%CONF%PCAL_MASK_FILE))// & 
     &            ' the pcal tone index '//PPM%IND_TONE//' is out '// &
     &            'of the range [1,'//STR1(1:I_LEN(STR1))//'] ' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
! ------ Get the mask value in AUTC mode
!
         IF ( PPM%MASK == '0' ) THEN
              PIM%PCAL_MASK(IND_TONE,IND_FRQ,IND_STA) = 0 
           ELSE IF ( PPM%MASK == '1' ) THEN
              PIM%PCAL_MASK(IND_TONE,IND_FRQ,IND_STA) = 1
           ELSE 
              CALL ERR_LOG( 4151, IUER, 'READ_PCAL_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the pcal '// &
     &            'mask file '// &
     &             PIM%CONF%PCAL_MASK_FILE(1:I_LEN(PIM%CONF%PCAL_MASK_FILE))// & 
     &            ' the pcal mask value is '//PPM%MASK// &
     &            ' while 0 or 1 were expected' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
 410  CONTINUE 
      PIM%PCAL_MASK_STATUS = PIMA__LOADED
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_PCAL_MASK  !#!#
