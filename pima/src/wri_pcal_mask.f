      SUBROUTINE WRI_PCAL_MASK ( PIM, FIL_GEN, FIL_OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRI_PCAL_MASK
! *                                                                      *
! * ###  11-MAY-2015  WRI_PCAL_MASK  v1.0 (c) L. Petrov 11-MAY-2015  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE ) :: PIM
      CHARACTER  FIL_GEN*(*), FIL_OUT*(*)
      INTEGER*4  IUER
      CHARACTER  STR*32, STR1*32, OUT*70
      INTEGER*1  MASK_VALUE 
      TYPE     ( PCAL_MASK_TEXT__TYPE ) :: PPM
      INTEGER*4  J1, J2, J3, J4, LUN, IND_ABS_CHN, IER
!
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
           CALL ERR_LOG ( 4661, IUER, 'WRI_PCAL_MASK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to open the output '// &
     &         'file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the labal
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__PCAL_MASK_LABEL(1:LEN(PIMA__PCAL_MASK_LABEL))
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4662, IUER, 'WRI_PCAL_MASK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the first '// &
     &         'line into the output file '//FIL_OUT )
           RETURN 
      END IF
!
! --- Write the preamble
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# PCAL_MASK file for experiment '// &
     &                                 PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# created by '//PIMA__PCAL_MASK_GEN// &
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
            DO 430 J3=1,PIM%NPCT
               IND_ABS_CHN = IND_ABS_CHN + 1
!
! ------------ Fill the PPM-string with the template
!
               CALL LIB$MOVC3 (  LEN(PCAL_MASK_TEMPLATE), &
     &                          %REF(PCAL_MASK_TEMPLATE), PPM )
               PPM%STA_NAM = PIM%C_STA(J1)
!
! ------------ Code indexes
!
               CALL INCH ( J2, PPM%IND_FRQ )
               CALL CHASHR   ( PPM%IND_FRQ )
               CALL INCH ( J3, PPM%IND_TONE )
               CALL CHASHR   ( PPM%IND_TONE )
               CALL INCH ( IND_ABS_CHN, PPM%IND_ABS_TONE )
               CALL CHASHR   ( PPM%IND_ABS_TONE )
               WRITE( UNIT=PPM%MASK, FMT='(I1)' ) PIM%PCAL_MASK(J3,J2,J1)
!
! ------------ Convert PPM to the string
!
               CALL LIB$MOVC3 (  LEN(PCAL_MASK_TEMPLATE), PPM, %REF(OUT) )
!
! ------------ ... and write this string out
!
               WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) OUT
               IF ( IER .NE. 0 ) THEN
                    CALL CLRCH ( STR )
                    CALL INCH  ( IER, STR )
                    CALL ERR_LOG ( 4665, IUER, 'WRI_PCAL_MASK', 'Error '// &
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
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IER ) PIMA__PCAL_MASK_LABEL(1:LEN(PIMA__PCAL_MASK_LABEL))
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IER, STR )
           CALL ERR_LOG ( 4666, IUER, 'WRI_PCAL_MASK', 'Error '// &
     &          STR(1:I_LEN(STR))//' in an attempt to write the last '// &
     &         'line into the output file '//FIL_OUT )
           RETURN 
      END IF
!
      CLOSE ( UNIT=LUN )
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, '(A)' ) 'WRI_PCAL_MASK: pcal mask is written in '// &
     &                         FIL_OUT(1:I_LEN(FIL_OUT))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRI_PCAL_MASK  !#!#
