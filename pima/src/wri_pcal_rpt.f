      SUBROUTINE WRI_PCAL_RPT ( PIM, FIL_GEN, FAIL_ARR, STA_NAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRI_PCAL_RPT
! *                                                                      *
! * ###  30-JUN-2022  WRI_PCAL_MASK  v1.0 (c) L. Petrov 30-JUN-2022  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  FIL_GEN*(*), STA_NAM*(*)
      INTEGER*4  IUER
      CHARACTER  STR*32, STR1*32, OUT*100, FIL_OUT*70, TRUNC_CONF*70, &
     &           J2_CHAR*3, J3_CHAR*3
      INTEGER*1  MASK_VALUE 
      TYPE     ( PCAL_RPT_TEXT__TYPE ) :: RPT
!
      INTEGER*4  J1, J2, J3, J4, LUN, IND_ABS_CHN, IER, PREFIX, BAND, &
     &           BEG_STA, END_STA, IFRQ, FAIL_AMP, FAIL_CPDIFF, FAIL_Y8DIFF
      INTEGER*4  FAIL_ARR(PIM__MTON,PIM%NFRQ,PIM%NSTA)
! 
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LTM_DIF
      PARAMETER  ( FAIL_AMP = 1 )
      PARAMETER  ( FAIL_CPDIFF = 2 )
      PARAMETER  ( FAIL_Y8DIFF = 3 )
!     
      LUN = GET_UNIT()
!
! --- Open the output file
!
      PREFIX = SCAN( PIM%CONF_FILE, '_' )
      TRUNC_CONF = PIM%CONF_FILE(1:PREFIX)
      BAND = SCAN( PIM%CONF_FILE(PREFIX+1:LEN ( PIM%CONF_FILE )), '_' )
      FIL_OUT = TRIM(TRUNC_CONF) // TRIM(PIM%CONF_FILE(PREFIX+1:PREFIX+BAND-1))
      FIL_OUT = TRIM(FIL_OUT) // TRIM("_pcal_report.gen")
      OPEN ( UNIT=LUN, FILE=FIL_OUT, STATUS='UNKNOWN', IOSTAT=IER)
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4661, IUER, 'WRI_PCAL_RPT', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the output '// &
     &         'file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the labal
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__PCAL_RPT_LABEL(1:LEN(PIMA__PCAL_RPT_LABEL))
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4662, IUER, 'WRI_PCAL_RPT', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the first '// &
     &         'line into the output file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the preamble
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# PCAL_RPT file for experiment '// &
     &                                 PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# created by '//PIMA__PCAL_RPT_GEN// &
     &                              ' on '//GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '# using control file '//FIL_GEN(1:I_LEN(FIL_GEN))
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Cycle over stations
!
      IF ( STA_NAM == 'ALL' .OR. STA_NAM == 'all' ) THEN
           BEG_STA = 1
           END_STA = PIM%NSTA
        ELSE
           BEG_STA = LTM_DIF ( 0, PIM%NSTA, PIM%C_STA, STA_NAM )
           END_STA = BEG_STA
        ENDIF
        
      DO 410 J1=BEG_STA,END_STA
!
! ------ Cycle over channels and frequencies
!
         IND_ABS_CHN = PIM%CONF%BEG_FRQ * PIM%NPCT
         IFRQ = 0
         DO 420 J2=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            DO 430 J3=1,PIM%NPCT
               IND_ABS_CHN = IND_ABS_CHN + 1
               IF ( FAIL_ARR(J3,IFRQ,J1) > 0 .AND. &
     &              ( LTM_DIF ( 0, PIM%CONF%L_PUS, PIM%CONF%PCAL_USE_STA, PIM%C_STA(J1) ) == 0 .OR. &
     &                PIM%CONF%L_PUS == 0                                                         ) ) THEN
!
! ----------------- Fill the RPT-string with the template
!
                    CALL LIB$MOVC3 (  LEN(PCAL_RPT_TEMPLATE), &
     &                                %REF(PCAL_RPT_TEMPLATE), RPT )
                    RPT%STA_NAM = PIM%C_STA(J1)
!
! ----------------- Code indexes
!
                    CALL INCH ( J2, J2_CHAR )
                    RPT%IND_FRQ = TRIM(J2_CHAR) // '-' // TRIM(J2_CHAR)
                    CALL CHASHR   ( RPT%IND_FRQ )
                    CALL INCH ( J3, J3_CHAR )
                    RPT%IND_TONE = TRIM(J3_CHAR) // '-' // TRIM(J3_CHAR)
                    CALL CHASHR   ( RPT%IND_TONE )
                    IF ( FAIL_ARR(J3,IFRQ,J1) == FAIL_AMP .OR. &
                   &     FAIL_ARR(J3,IFRQ,J1) == FAIL_AMP + FAIL_Y8DIFF ) THEN
                          WRITE( UNIT=RPT%RPT_VAL, FMT='(A)' ) 'LOW AMPLITUDE'
                       ELSE IF ( FAIL_ARR(J3,IFRQ,J1) == FAIL_CPDIFF ) THEN
                          WRITE( UNIT=RPT%RPT_VAL, FMT='(A)' ) 'SPURIOUS SIGNAL'
                       ELSE IF ( FAIL_ARR(J3,IFRQ,J1) == FAIL_Y8DIFF ) THEN
                          WRITE( UNIT=RPT%RPT_VAL, FMT='(A)' ) 'PHASE JUMPS' 
                    ENDIF
!
! ----------------- Convert RPT to the string
!
                    CALL LIB$MOVC3 (  LEN(PCAL_RPT_TEMPLATE), RPT, %REF(OUT) )
!
! ----------------- ... and write this string out
!
                    WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) OUT
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( IER, STR )
                         CALL ERR_LOG ( 4665, IUER, 'WRI_PCAL_RPT', 'Error '// &
          &                   STR(1:I_LEN(STR))//' in an attempt to write '// &
          &                  'into the output file '//FIL_OUT )
                         RETURN 
                    END IF
               ENDIF   
 430        CONTINUE 
 420     CONTINUE 
         WRITE ( UNIT=LUN, FMT='(A)' ) '# '
 410  CONTINUE 
!
! --- Write the trailing format label
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__PCAL_RPT_LABEL(1:LEN(PIMA__PCAL_RPT_LABEL))
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4666, IUER, 'WRI_PCAL_RPT', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the last '// &
     &         'line into the output file '//FIL_OUT )
           RETURN 
      END IF
!
      CLOSE ( UNIT=LUN )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'WRI_PCAL_RPT: pcal rpt file is written in '// &
     &                         FIL_OUT(1:I_LEN(FIL_OUT))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRI_PCAL_RPT  !#!#
