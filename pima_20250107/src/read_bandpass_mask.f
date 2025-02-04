      SUBROUTINE READ_BANDPASS_MASK ( PIM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_BANDPASS_MASK 
! *                                                                      *
! * ## 06-FEB-2009 READ_BANDPASS_MASK v2.1 (c) L. Petrov 20-SEP-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      INTEGER*4  IUER
      CHARACTER  STR*128, STR1*128
      CHARACTER, ALLOCATABLE :: BUF(:)*80
      INTEGER*4  J1, J2, J3, IND_STA, IND_FRQ, IND_CHN, MBUF, NBUF, IER
      LOGICAL*1  FL_EXIST, FL_20090506 
      TYPE ( BANDPASS_MASK_TEXT__TYPE ) :: BPM
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      IF ( PIM%CONF%BANDPASS_MASK_FILE == 'NO' .OR. &
     &     ILEN(PIM%CONF%BANDPASS_MASK_FILE) == 0 ) THEN
           CALL ERR_LOG ( 4111, IUER, 'READ_BANDPASS_MASK', 'The bandpass '// &
     &         'mask file has not beenb definded in PIM%CONF' )
           RETURN 
      END IF
!
      INQUIRE ( FILE=PIM%CONF%BANDPASS_MASK_FILE, EXIST=FL_EXIST )
      IF ( .NOT. FL_EXIST ) THEN
           CALL ERR_LOG ( 4112, IUER, 'READ_BANDPASS_MASK', 'The bandpass '// &
     &         'mask file '// &
     &          PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// &
     &         ' was not found' )
           RETURN 
      END IF
!
      MBUF = 32 + PIM__MSTA*(PIM%NFRQ*(PIM%NCHN+2))
!
      ALLOCATE ( BUF(MBUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MBUF*LEN(BUF(1)), STR )
           CALL ERR_LOG ( 4113, IUER, 'READ_BANDPASS_MASK', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for a temporary buffer' )
           RETURN 
      END IF
!
      IF ( ASSOCIATED ( PIM%BANDPASS_MASK ) ) THEN
           DEALLOCATE ( PIM%BANDPASS_MASK )
      END IF
!
      ALLOCATE ( PIM%BANDPASS_MASK(PIM%NCHN,PIM%NFRQ,PIM%NSTA,PIMA__MASKS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PIM%NCHN*PIM%NFRQ*PIM%NSTA*PIMA__MASKS, STR )
           CALL ERR_LOG ( 4113, IUER, 'READ_BANDPASS_MASK', 'Failure '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for the bandpass mask' )
           RETURN 
      END IF
!
! --- Initialization
!
      PIM%BANDPASS_MASK = 1
      PIM%BANDPASS_MASK_STYLE = PIMA__COMBINED
!
! --- Read the badnpass mask file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( PIM%CONF%BANDPASS_MASK_FILE, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4114, IUER, 'READ_BANDPASS_MASK', 'Error '// &
     &         'in an attempt to read the input file with the bandpass '// &
     &         'mask '//PIM%CONF%BANDPASS_MASK_FILE )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Check the first line of the file
!
      IF ( BUF(1)(1:LEN(PIMA__BPASS_MASK_LABEL)) == '# PIMA BPASS_MASK   Format of 2009.05.26' ) THEN
           FL_20090506 = .TRUE.
        ELSE IF ( BUF(1)(1:LEN(PIMA__BPASS_MASK_LABEL)) == PIMA__BPASS_MASK_LABEL ) THEN
           FL_20090506 = .FALSE.
        ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG ( 4115, IUER, 'READ_BANDPASS_MASK', 'Wrong format '// &
     &         'of the bandpass mask file '// &
     &          PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// &
     &         ' the first line is '//STR(1:LEN(PIMA__BPASS_MASK_LABEL))// &
     &         ' while the format identification '//PIMA__BPASS_MASK_LABEL// &
     &         ' was expected' )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
! --- Check the last line of the file
!
      IF ( BUF(NBUF)(1:LEN(PIMA__BPASS_MASK_LABEL)) == BUF(1)(1:LEN(PIMA__BPASS_MASK_LABEL)) ) THEN
           CONTINUE 
        ELSE 
           CALL CLRCH ( STR )
           CALL TRAN  ( 13, BUF(1), BUF(1) )
           CALL ERR_LOG ( 4116, IUER, 'READ_BANDPASS_MASK', 'Corrupted '// &
     &         'bandpass mask file '// &
     &          PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// &
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
! ------ Fill BPM with the tempplate
!
         CALL LIB$MOVC3 ( LEN(BANDPASS_MASK_TEMPLATE), %REF(BUF(J1)), BPM )
!
         IND_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, BPM%STA_NAM )
         IF ( IND_STA .LE. 0 ) GOTO 410
!@         IF ( IND_STA .LE. 0 ) THEN
!@              CALL ERR_LOG( 4117, IUER, 'READ_BANDPASS_MASK', 'Failure in '// &
!@     &            'parsing the '//STR(1:I_LEN(STR))//'-th of the bandpass '// &
!@     &            'mask file '// &
!@     &             PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// & 
!@     &            ' the station '//BPM%STA_NAM//' did not participated '// &
!@     &            'in VLBI experiment '//PIM%CONF%SESS_CODE )
!@              DEALLOCATE ( BUF )
!@              RETURN 
!@         END IF
!
         IF ( FL_20090506 ) THEN
!
! ----------- In this format first value is for AUTC, BPAS, 
! ----------- the second value is for FRNG, SPLT
!
              BPM%MASK_FRNG = BPM%MASK_BPAS
              BPM%MASK_SPLT = BPM%MASK_BPAS
              BPM%MASK_BPAS = BPM%MASK_AUTC
         END IF
         CALL CHIN ( BPM%IND_FRQ, IND_FRQ )
         IF ( IND_FRQ < 1  .OR.  IND_FRQ > PIM%NFRQ ) THEN
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%NFRQ, STR1 )
              CALL ERR_LOG( 4118, IUER, 'READ_BANDPASS_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th of the bandpass '// &
     &            'mask file '// &
     &             PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// & 
     &            ' the frequency index '//BPM%IND_FRQ//' is out of the '// &
     &            'range [1,'//STR1(1:I_LEN(STR1))//'] ' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
         CALL CHIN ( BPM%IND_CHN, IND_CHN )
         IF ( IND_CHN < 1  .OR.  IND_FRQ > PIM%NCHN ) THEN
              CALL CLRCH ( STR1 )
              CALL INCH  ( PIM%NCHN, STR1 )
              CALL ERR_LOG( 4119, IUER, 'READ_BANDPASS_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th of the bandpass '// &
     &            'mask file '// &
     &             PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// & 
     &            ' the spectral channel index '//BPM%IND_CHN//' is out '// &
     &            'of the range [1,'//STR1(1:I_LEN(STR1))//'] ' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
! ------ Get the mask value in AUTC mode
!
         IF ( BPM%MASK_AUTC == '0' ) THEN
              PIM%BANDPASS_MASK(IND_CHN,IND_FRQ,IND_STA,PIMA__MASK_AUTC) = 0 
           ELSE IF ( BPM%MASK_AUTC  == '1' ) THEN
              PIM%BANDPASS_MASK(IND_CHN,IND_FRQ,IND_STA,PIMA__MASK_AUTC) = 1
           ELSE 
              CALL ERR_LOG( 4120, IUER, 'READ_BANDPASS_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the bandpass '// &
     &            'mask file '// &
     &             PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// & 
     &            ' the bandpass mask coaese value is '//BPM%MASK_AUTC// &
     &            ' while 0 or 1 were expected' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
! ------ Get the mask value in BPAS mode
!
         IF ( BPM%MASK_BPAS == '0' ) THEN
              PIM%BANDPASS_MASK(IND_CHN,IND_FRQ,IND_STA,PIMA__MASK_BPAS) = 0 
           ELSE IF ( BPM%MASK_BPAS == '1' ) THEN
              PIM%BANDPASS_MASK(IND_CHN,IND_FRQ,IND_STA,PIMA__MASK_BPAS) = 1
           ELSE 
              CALL ERR_LOG( 4121, IUER, 'READ_BANDPASS_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the bandpass '// &
     &            'mask file '// &
     &             PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// & 
     &            ' the bandpass mask fine value is '//BPM%MASK_BPAS// &
     &            ' while 0 or 1 were expected' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
! ------ Get the mask value in FRNG mode
!
         IF ( BPM%MASK_FRNG == '0' ) THEN
              PIM%BANDPASS_MASK(IND_CHN,IND_FRQ,IND_STA,PIMA__MASK_FRNG) = 0 
           ELSE IF ( BPM%MASK_FRNG == '1' ) THEN
              PIM%BANDPASS_MASK(IND_CHN,IND_FRQ,IND_STA,PIMA__MASK_FRNG) = 1
           ELSE 
              CALL ERR_LOG( 4122, IUER, 'READ_BANDPASS_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the bandpass '// &
     &            'mask file '// &
     &             PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// & 
     &            ' the bandpass mask fine value is '//BPM%MASK_FRNG// &
     &            ' while 0 or 1 were expected' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
!
! ------ Get the mask value in SPLT mode
!
         IF ( BPM%MASK_SPLT == '0' ) THEN
              PIM%BANDPASS_MASK(IND_CHN,IND_FRQ,IND_STA,PIMA__MASK_SPLT) = 0 
           ELSE IF ( BPM%MASK_SPLT == '1' ) THEN
              PIM%BANDPASS_MASK(IND_CHN,IND_FRQ,IND_STA,PIMA__MASK_SPLT) = 1
           ELSE 
              CALL ERR_LOG( 4123, IUER, 'READ_BANDPASS_MASK', 'Failure in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the bandpass '// &
     &            'mask file '// &
     &             PIM%CONF%BANDPASS_MASK_FILE(1:I_LEN(PIM%CONF%BANDPASS_MASK_FILE))// & 
     &            ' the bandpass mask fine value is '//BPM%MASK_SPLT// &
     &            ' while 0 or 1 were expected' )
              DEALLOCATE ( BUF )
              RETURN 
         END IF
 410  CONTINUE 
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_BANDPASS_MASK  !#!#
