      SUBROUTINE WRI_BANDPASS_MASK ( PIM, FIL_GEN, FIL_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRI_BANDPASS_MASK 
! *                                                                      *
! * ### 06-FEB-2009 WRI_BANDPASS_MASK v1.2 (c) L. Petrov 17-AUG-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  FIL_GEN*(*), FIL_OUT*(*)
      INTEGER*4  IUER
      CHARACTER  STR*32, STR1*32, OUT*76
      INTEGER*1  MASK_VALUE 
      INTEGER*4  J1, J2, J3, J4, LUN, IND_ABS_CHN, IER
      TYPE ( BANDPASS_MASK_TEXT__TYPE ) :: BPM
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LTM_DIF
!
      LUN = GET_UNIT()
!
! --- Open the output file
!
      OPEN ( UNIT=LUN, FILE=FIL_OUT, STATUS='UNKNOWN', IOSTAT=IER)
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4661, IUER, 'WRI_BANDPASS_MASK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the output '// &
     &         'file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the labal
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__BPASS_MASK_LABEL(1:LEN(PIMA__BPASS_MASK_LABEL))
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4662, IUER, 'WRI_BANDPASS_MASK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the first '// &
     &         'line into the output file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the preamble
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# BANDPASS_MASK file for experiment '// &
     &                                 PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# created by '//PIMA__BPASS_MASK_GEN// &
     &                              ' on '//GET_CDATE()
      WRITE ( UNIT=LUN, FMT='(A)' ) '# using control file '//FIL_GEN(1:I_LEN(FIL_GEN))
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Cycle over stations
!
      DO 410 J1=1,PIM%NSTA
!
! ------ Cycle over channles and frequencies
!
         IND_ABS_CHN = 0
         DO 420 J2=1,PIM%NFRQ
            DO 430 J3=1,PIM%NCHN
               IND_ABS_CHN = IND_ABS_CHN + 1
!
! ------------ Fill BPM with the tempplate
!
               CALL LIB$MOVC3 (  LEN(BANDPASS_MASK_TEMPLATE), &
     &                          %REF(BANDPASS_MASK_TEMPLATE), BPM )
               BPM%STA_NAM = PIM%C_STA(J1)
!
! ------------ Code indexes
!
               CALL INCH ( J2, BPM%IND_FRQ )
               CALL CHASHR   ( BPM%IND_FRQ )
               CALL INCH ( J3, BPM%IND_CHN )
               CALL CHASHR   ( BPM%IND_CHN )
               CALL INCH ( IND_ABS_CHN, BPM%IND_ABS_CHN )
               CALL CHASHR   ( BPM%IND_ABS_CHN )
!
! ------------ Put the mask value
!
               DO 440 J4=1,PIMA__MASKS
                  IF ( PIM%BANDPASS_MASK(J3,J2,J1,J4) == 0 ) THEN
                       STR = '0'
                    ELSE IF ( PIM%BANDPASS_MASK(J3,J2,J1,J4) == 1 ) THEN
                       STR = '1'
                    ELSE 
                       CALL CLRCH ( STR )
                       CALL INCH  ( INT(PIM%BANDPASS_MASK(J3,J2,J1,J4),KIND=4), STR )
                       CALL CLRCH ( STR1 )
                       CALL INCH  ( J4, STR1 )
                       CALL ERR_LOG ( 4663, IUER, 'WRI_BANDPASS_MASK', &
     &                     'unsupported value of MASK (AUTC) '// &
     &                     STR(1:I_LEN(STR))//' of the '//STR1(1:I_LEN(STR1))// &
     &                     '-th kind for station '//BPM%STA_NAM//' spectral '// &
     &                     'channel '//BPM%IND_CHN//' frequency channel '// &
     &                     BPM%IND_FRQ//' -- only values 0 or 1 are '// &
     &                    'supported ' )
                       RETURN 
                  END IF 
                  IF ( J4 == PIMA__MASK_AUTC ) BPM%MASK_AUTC = STR
                  IF ( J4 == PIMA__MASK_BPAS ) BPM%MASK_BPAS = STR
                  IF ( J4 == PIMA__MASK_FRNG ) BPM%MASK_FRNG = STR
                  IF ( J4 == PIMA__MASK_SPLT ) BPM%MASK_SPLT = STR
 440           CONTINUE 
!
! ------------ Convert BPM to the string
!
               CALL LIB$MOVC3 (  LEN(BANDPASS_MASK_TEMPLATE), BPM, %REF(OUT) )
!
! ------------ ... and write this string out
!
               WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) OUT
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IER, STR )
                    CALL ERR_LOG ( 4665, IUER, 'WRI_BANDPASS_MASK', 'Error '// &
     &                   STR(1:I_LEN(STR))//' in an attempt to write '// &
     &                  'into the output file '//FIL_OUT )
                    RETURN 
               END IF
 430        CONTINUE 
            WRITE ( UNIT=LUN, FMT='(A)' ) '# '
 420     CONTINUE 
 410  CONTINUE 
!
! --- Write the trailing format label
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__BPASS_MASK_LABEL(1:LEN(PIMA__BPASS_MASK_LABEL))
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4666, IUER, 'WRI_BANDPASS_MASK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the last '// &
     &         'line into the output file '//FIL_OUT )
           RETURN 
      END IF
!
      CLOSE ( UNIT=LUN )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'WRI_BANDPASS_MASK: bandpass is written in '// &
     &                         FIL_OUT(1:I_LEN(FIL_OUT))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRI_BANDPASS_MASK  !#!#
