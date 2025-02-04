      SUBROUTINE WRI_PCAL_RMS ( PIM, FIL_GEN, RMS_TIME, RMS_FREQ, TIME_AVG, STA_NAM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRI_PCAL_RMS
! *                                                                      *
! * ###  30-JUN-2022  WRI_PCAL_MASK  v1.0 (c) L. Petrov 30-JUN-2022  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  FIL_GEN*(*), STA_NAM*(*)
      INTEGER*4  IUER
      CHARACTER  STR*32, STR1*32, OUT*100, OUT_FREQ*59, FIL_OUT*70, TRUNC_CONF*70
      INTEGER*1  MASK_VALUE 
      TYPE     ( PCAL_RMS_TEXT__TYPE ) :: PPR_TIME
      TYPE     ( PCAL_RMS_FREQ_TEXT__TYPE ) :: PPR_FREQ
      INTEGER*4  J1, J2, J3, J4, J5, LUN, IND_ABS_CHN, IER, PREFIX, BAND, &
     &     BEG_STA, END_STA, IFRQ
      REAL*8     TIME_AVG, RMS_TIME(PIM__MTON,PIM%NFRQ,PIM%NSTA), &
     &           RMS_FREQ(PIM%NFRQ,PIM%NSTA)
! 
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LTM_DIF
!
      LUN = GET_UNIT()
!      T_AVG = (PIM%AP_LEN_MAX + PIM%AP_LEN_MIN) / 2
!
!     --- Open the output file
      PREFIX = SCAN( PIM%CONF_FILE, '_' )
      TRUNC_CONF = PIM%CONF_FILE(1:PREFIX)
      BAND = SCAN( PIM%CONF_FILE(PREFIX+1:LEN ( PIM%CONF_FILE )), '_' )
      FIL_OUT = TRIM(TRUNC_CONF) // TRIM(PIM%CONF_FILE(PREFIX+1:PREFIX+BAND-1))
      FIL_OUT = TRIM(FIL_OUT) // TRIM("_pcal_rms.txt")
      OPEN ( UNIT=LUN, FILE=FIL_OUT, STATUS='UNKNOWN', IOSTAT=IER)
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4661, IUER, 'WRI_PCAL_RMS', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the output '// &
     &         'file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the labal
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__PCAL_RMS_LABEL(1:LEN(PIMA__PCAL_RMS_LABEL))
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4662, IUER, 'WRI_PCAL_RMS', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the first '// &
     &         'line into the output file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the preamble
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# PCAL_RMS file for experiment '// &
     &                                 PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# created by '//PIMA__PCAL_RMS_GEN// &
     &                              ' on '//GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '# using control file '//FIL_GEN(1:I_LEN(FIL_GEN))
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A, F10.3, A)' ) 'AVERAGE TIME SPACING: ', TIME_AVG, ' SEC'
      WRITE ( UNIT=LUN, FMT='(A)' ) 'TIME DIRECTION RMS PHASE CALIBRATION JITTER (RAD) BY CHANNEL:'
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
!
! --- Write time rms section
!
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
               IF  ( PIM%PCAL_MASK(J3,J2,J1) == 1 .AND. &
     &               PIM%STA(J1)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(J3,IFRQ,1) .GT. 0 &
     &              .AND. ( LTM_DIF ( 0, PIM%CONF%L_PUS, PIM%CONF%PCAL_USE_STA, &
     &              PIM%C_STA(J1) ) == 0 .OR. PIM%CONF%L_PUS ==0 ) ) THEN
!
! ----------------- Fill the PPR_TIME-string with the template
!
                    CALL LIB$MOVC3 (  LEN(PCAL_RMS_TEMPLATE), &
     &                                %REF(PCAL_RMS_TEMPLATE), PPR_TIME )
                    
                    PPR_TIME%STA_NAM = PIM%C_STA(J1)
!
! ----------------- Code indexes
!
                    CALL INCH ( J2, PPR_TIME%IND_FRQ )
                    CALL CHASHR   ( PPR_TIME%IND_FRQ )  
                    CALL INCH ( J3, PPR_TIME%IND_TONE )
                    CALL CHASHR   ( PPR_TIME%IND_TONE )   
                    CALL INCH ( IND_ABS_CHN, PPR_TIME%IND_ABS_TONE )
                    CALL CHASHR   ( PPR_TIME%IND_ABS_TONE )
                    WRITE( UNIT=PPR_TIME%RMS_VAL, FMT='(F8.3)' ) RMS_TIME(J3,IFRQ,J1)
!
! ----------------- Convert PPR_TIME to the string
!
                    CALL CLRCH     ( OUT )
                    CALL LIB$MOVC3 (  LEN(PCAL_RMS_TEMPLATE), PPR_TIME, %REF(OUT) )
!
! ---------------- ... and write this string out
!
                    WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) TRIM(OUT)
                    IF ( IER .NE. 0 ) THEN
                         CALL CLRCH ( STR )
                         CALL INCH  ( IER, STR )
                         CALL ERR_LOG ( 4665, IUER, 'WRI_PCAL_RMS', 'Error '// &
          &                   STR(1:I_LEN(STR))//' in an attempt to write '// &
          &                  'into the output file '//FIL_OUT )
                         RETURN 
                    END IF
               ENDIF   
 430        CONTINUE 
            IF ( LTM_DIF ( 0, PIM%CONF%L_PUS, PIM%CONF%PCAL_USE_STA, PIM%C_STA(J1) ) &
           &     == 0 ) WRITE ( UNIT=LUN, FMT='(A)' ) '# '
 420     CONTINUE 
 410  CONTINUE
!
! ------------ Now in the frequency direction
!
      WRITE ( UNIT=LUN, FMT='(A)' ) 'FREQ DIRECTION RMS PHASE CALIBRATION JITTER (RAD) BY IF:'

      DO 440 J4=BEG_STA,END_STA
!
! ------ Cycle over channels and frequencies
!
         IND_ABS_CHN = PIM%CONF%BEG_FRQ * PIM%NPCT
         IFRQ = 0
         DO 450 J5=PIM%CONF%BEG_FRQ,PIM%CONF%END_FRQ
            IFRQ = IFRQ + 1
            IF  ( ANY( PIM%PCAL_MASK(1:PIM%NPCT,J5,J4) == 1 )              .AND. &
     &            PIM%STA(J4)%PCAL(PIM%CONF%FRQ_GRP)%FREQ(1,IFRQ,1) .GT. 0 .AND. &
     &            ( LTM_DIF ( 0, PIM%CONF%L_PUS, PIM%CONF%PCAL_USE_STA,          &
     &              PIM%C_STA(J4) ) == 0 .OR. PIM%CONF%L_PUS ==0 )               ) THEN
!
! -------------- Fill the PPF-string with the template
!

                 CALL LIB$MOVC3 (  LEN(PCAL_RMS_FREQ_TEMPLATE), &
                 &                          %REF(PCAL_RMS_FREQ_TEMPLATE), PPR_FREQ )

                 PPR_FREQ%STA_NAM = PIM%C_STA(J4)
!
! -------------- Code indexes
!
                 CALL INCH ( J5, PPR_FREQ%IND_FRQ )
                 CALL CHASHR   ( PPR_FREQ%IND_FRQ )  
                 WRITE( UNIT=PPR_FREQ%RMS_VAL, FMT='(F8.3)' ) RMS_FREQ(IFRQ,J4)
!
! -------------- Convert PPR_FREQ to the string
!
                 CALL CLRCH ( OUT_FREQ )
                 CALL LIB$MOVC3 (  LEN(PCAL_RMS_FREQ_TEMPLATE), PPR_FREQ, %REF(OUT_FREQ) )
!
! ------------ ... and write this string out
!
                 WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) TRIM(OUT_FREQ)
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IER, STR )
                      CALL ERR_LOG ( 4665, IUER, 'WRI_PCAL_RMS', 'Error '// &
       &                   STR(1:I_LEN(STR))//' in an attempt to write '// &
       &                  'into the output file '//FIL_OUT )
                      RETURN 
                 END IF
            ENDIF   
            IF ( LTM_DIF ( 0, PIM%CONF%L_PUS, PIM%CONF%PCAL_USE_STA, PIM%C_STA(J4) ) == 0 ) THEN
                 WRITE ( UNIT=LUN, FMT='(A)' ) '# '
           END IF
 450     CONTINUE 
 440  CONTINUE
!
! --- Write the trailing format label
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__PCAL_RMS_LABEL(1:LEN(PIMA__PCAL_RMS_LABEL))
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4666, IUER, 'WRI_PCAL_RMS', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the last '// &
     &         'line into the output file '//FIL_OUT )
           RETURN 
      END IF
!
      CLOSE ( UNIT=LUN )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'WRI_PCAL_RMS: pcal rms file is written in '// &
     &                         FIL_OUT(1:I_LEN(FIL_OUT))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRI_PCAL_RMS  !#!#
