      SUBROUTINE GET_IONO_AVR_COV ( VTD, STA_NAM1, STA_NAM2, &
     &                              IONO_ZEN_AVR, IONO_ZEN_COV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GET_IONO_AVR_COV 
! *                                                                      *
! * ### 10-OCT-2010 GET_IONO_AVR_COV  v1.0 (c) L. Petrov 10-OCT-2010 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      CHARACTER  STA_NAM1*(*), STA_NAM2*(*)
      REAL*8     IONO_ZEN_AVR(2), IONO_ZEN_COV(3)
      INTEGER*4  IUER 
      TYPE     ( VTD__TYPE     ) :: VTD
      CHARACTER  STA_NAM(2)*8
      INTEGER*4  IND_STA(2), J1, J2, J3
!
      STA_NAM(1) = STA_NAM1
      STA_NAM(2) = STA_NAM2
      CALL VTD_NAME_REPAIR ( STA_NAM(1) )
      CALL VTD_NAME_REPAIR ( STA_NAM(2) )
!
      IND_STA = 0 
      DO 410 J1=1,VTD%L_STA
         IF ( STA_NAM(1) == VTD%STA(J1)%IVS_NAME ) IND_STA(1) = J1
         IF ( STA_NAM(2) == VTD%STA(J1)%IVS_NAME ) IND_STA(2) = J1
 410  CONTINUE 
      IF ( IND_STA(1) == 0 ) THEN
           WRITE ( 6, * ) 'VTD%L_STA= ', VTD%L_STA
           CALL ERR_LOG ( 8381, IUER, 'GET_IONO_AVR_COV', 'Trap of '// &
     &         'internal control: cannot find station '//STA_NAM1// &
     &         ' . The following stations are in VTSD%STA()%IVS_NAME list:' )
           DO 420 J2=1,VTD%L_STA
              WRITE ( 6, '(A)' ) 'Station '//VTD%STA(J2)%IVS_NAME
 420       CONTINUE 
           RETURN 
      END IF
!
      IF ( IND_STA(2) == 0 ) THEN
           WRITE ( 6, * ) 'VTD%L_STA= ', VTD%L_STA
           CALL ERR_LOG ( 8382, IUER, 'GET_IONO_AVR_COV', 'Trap of '// &
     &         'internal control: cannot find station '//STA_NAM2// &
     &         ' . The following stations are in VTSD%STA()%IVS_NAME list:' )
           DO 430 J3=1,VTD%L_STA
              WRITE ( 6, '(A)' ) 'Station '//VTD%STA(J3)%IVS_NAME
 430       CONTINUE 
           RETURN 
      END IF
!
      IF ( .NOT. ASSOCIATED ( VTD%IONO%AVR_STA ) ) THEN
           CALL ERR_LOG ( 8383, IUER, 'GET_IONO_AVR_COV', 'Trap of '// &
     &         'internal control: ionosphere statistics have not '// &
     &         'been computed' )
           RETURN 
      END IF
!
      IONO_ZEN_AVR(1) = VTD%IONO%AVR_STA(IND_STA(1))
      IONO_ZEN_AVR(2) = VTD%IONO%AVR_STA(IND_STA(2))
      IONO_ZEN_COV(1) = VTD%IONO%COV_STA(IND_STA(1),IND_STA(1))
      IONO_ZEN_COV(2) = VTD%IONO%COV_STA(IND_STA(1),IND_STA(2))
      IONO_ZEN_COV(3) = VTD%IONO%COV_STA(IND_STA(2),IND_STA(2))
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_IONO_AVR_COV  !#!#
