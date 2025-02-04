      SUBROUTINE NERS_READ_EARTH_EPHE ( NERS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_READ_EARTH_EPHE reads the file with spline            *
! *   coefficients of the Earth's ephemerides.                           *
! *                                                                      *
! * # 07-JUN-2018 NERS_READ_EARTH_EPHE v1.0 (c) L. Petrov 07-JUN-2018 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      INCLUDE   'ners_local.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  IUER
      CHARACTER  FIL*128, STR*128
      LOGICAL*1  LEX
      INTEGER*4  J1, LUN, LN, NBT, IER
      INTEGER*4, EXTERNAL :: GET_UNIT
!
      IF ( ASSOCIATED ( NERS%EPH%TIM )           ) DEALLOCATE ( NERS%EPH%TIM )
      IF ( ASSOCIATED ( NERS%EPH%COO_EARTH_SPL ) ) DEALLOCATE ( NERS%EPH%COO_EARTH_SPL )
!
! --- Generate the file name
!
      FIL = NERS__PREFIX//'/share/'//NERS__EPHE_FIL
      INQUIRE ( FILE=FIL, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4921, IUER, 'NERS_READ_EARTH_EPHE', 'Trap '// &
     &          'of internal control: file with Earth ephemeride '// &
     &          TRIM(FIL)//' was not found' )
           RETURN 
      END IF
!
! --- Open the ephemeride file
!
      LUN = GET_UNIT()
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FIL, 'old', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL GERROR ( STR )
           CALL ERR_LOG ( 4922, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &         'in openenig file '//TRIM(FIL)//' with Earth ephemeride '// &
     &         'for reading: '//STR )
           RETURN 
      END IF
!
! --- Check the first line -- format label
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_STRING ( LUN, STR, LN, IER )
      IF ( IER .NE. 0 .OR. LN .NE. LEN(NERS__EPHE_LABEL) .OR. &
     &     STR(1:LEN(NERS__EPHE_LABEL)) .NE. NERS__EPHE_LABEL ) THEN
!
           CALL ERR_LOG ( 4923, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &         'in reading file with Earth ephemeride '//TRIM(FIL)// &
     &         ' -- the first line is not the format label' )
           RETURN 
      END IF
!
! --- Get the number of points
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, 4*1, NERS%EPH%L_TIM, NBT, IER )
      IF ( IER .NE. 0 .OR. NBT .NE. 4 ) THEN
           CALL ERR_LOG ( 4924, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &         'in reading file with Earth ephemeride '//FIL )
           RETURN 
      END IF
!
      ALLOCATE ( NERS%EPH%TIM(NERS%EPH%L_TIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*NERS%EPH%L_TIM, STR )
           CALL ERR_LOG ( 4925, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &         'in an attempt to allocate '//TRIM(STR)//' dynamic memory '// &
     &         'for array NERS%EPH%L_TIM' )
           RETURN 
      END IF
      ALLOCATE ( NERS%EPH%COO_EARTH_VAL(NERS%EPH%L_TIM,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*3*NERS%EPH%L_TIM, STR )
           CALL ERR_LOG ( 4926, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &         'in an attempt to allocate '//TRIM(STR)//' dynamic memory '// &
     &         'for array NERS%EPH%COO_EARTH_VAL' )
           RETURN 
      END IF
      ALLOCATE ( NERS%EPH%COO_EARTH_SPL(NERS%EPH%L_TIM,3), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*3*NERS%EPH%L_TIM, STR )
           CALL ERR_LOG ( 4927, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &         'in an attempt to allocate '//TRIM(STR)//' dynamic memory '// &
     &         'for array NERS%EPH%COO_EARTH_SPL' )
           RETURN 
      END IF
      NERS%EPH_STATUS = NERS__ALLC
!
! --- Get array of TAI time tags
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, 8*NERS%EPH%L_TIM, NERS%EPH%TIM, NBT, IER )
      IF ( IER .NE. 0 .OR. NBT .NE. 8*NERS%EPH%L_TIM ) THEN
           CALL ERR_LOG ( 4928, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &         'in reading file with Earth ephemeride '//FIL )
           RETURN 
      END IF
!
! --- Get array of spline coefficients
!
      DO 410 J1=1,3
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_RECORD ( LUN, 8*NERS%EPH%L_TIM, NERS%EPH%COO_EARTH_VAL(1,J1), NBT, IER )
         IF ( IER .NE. 0 .OR. NBT .NE. 8*NERS%EPH%L_TIM ) THEN
              CALL ERR_LOG ( 4929, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &             'in reading file with Earth ephemeride '//FIL )
              RETURN 
         END IF
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_RECORD ( LUN, 8*NERS%EPH%L_TIM, NERS%EPH%COO_EARTH_SPL(1,J1), NBT, IER )
         IF ( IER .NE. 0 .OR. NBT .NE. 8*NERS%EPH%L_TIM ) THEN
              CALL ERR_LOG ( 4930, IUER, 'NERS_READ_EARTH_EPHE', 'Error '// &
     &             'in reading file with Earth ephemeride '//FIL )
              RETURN 
         END IF
 410  CONTINUE 
      NERS%EPH_STATUS = NERS__LOAD
!
! --- Close the file
!
      CALL ERR_PASS ( IUER, IER  )
      CALL BINF_CLOSE ( LUN, IER )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_READ_EARTH_EPHE  !#!#
