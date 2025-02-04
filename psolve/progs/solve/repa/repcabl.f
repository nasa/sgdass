      SUBROUTINE REPCABL ( NUMDB, IND_CAB, AVL_ARR, IUER )
!
      IMPLICIT   NONE
      INCLUDE 'solve.i'
      INCLUDE 'cals.i'
      TYPE ( CALS_STRU ) ::  CAL
      INTEGER*4  NUMDB, IND_CAB,IUER
      INTEGER*4  IVRB, IER, J1, J2
      CHARACTER  STR*32
      LOGICAL*4  AVL_ARR(MAX_ARC_STA)
!
      IVRB = 0
!
! --- Reading calibration status
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R ( INT2(NUMDB), IVRB, 0, CAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH ( NUMDB, STR )
           CALL ERR_LOG ( 3231, IUER, 'REPCABL', 'Error in reading '// &
     &         'calibration information while processing database number '//STR )
           RETURN
      END IF
!
! --- Learn the index of "cable calibration"
!
      IND_CAB = 0
      DO 410 J1=1,CAL%L_SCAL
         IF ( CAL%SCAL(J1) .EQ. 'cable   ' ) IND_CAB = J1
 410  CONTINUE
      IF ( IND_CAB .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH ( NUMDB, STR )
           CALL ERR_LOG ( 3232, IUER, 'REPCABL', 'Cable calibration slot '// &
     &         'was not found in calibrations slots. Error detected while '// &
     &         'processing database number '//STR )
!
           RETURN
      END IF
!
      DO 420 J2=1,CAL%L_STA
         AVL_ARR(J2) = CAL%SCAL_AVL(IND_CAB,J2)
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#! REPCABL  #!#
