      SUBROUTINE UPDATE_SPE ( L_PAR, C_PAR, L_SPE, SPE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine UPDATE_SPE
! *                                                                      *
! *  ### 08-MAR-2005   UPDATE_SPE  v2.0 (c)  L. Petrov  30-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'precm.i'
      INCLUDE   'solve.i'
      INTEGER*4  L_PAR, L_SPE, IUER
      CHARACTER  C_PAR(L_PAR)*(*)
      CHARACTER  STR*20, STA_NAM*8, FILSPE*128
      TYPE ( SPE__TYPE ) ::SPE(L_SPE)
      LOGICAL*4  LEX
      INTEGER*4  J1, J2, INOD, IOS, LUN, NEL, SIZE_SPE, IER
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Read object SPE from the scratch file.
!
      FILSPE = PRE_SCR_DIR(1:PRE_SD_LEN)//'ESPE'//PRE_LETRS
      INQUIRE ( FILE=FILSPE, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 4841, IUER, 'UPDATE_SPE', 'Cannot '// &
     &         'find file '//FILSPE )
           RETURN 
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILSPE, 'OLD', LUN, IER  )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4842, IUER, 'UPDATE_SPE', 'Failure in '// &
     &         'an attempt to open input file '//FILSPE )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL RDBIN_ARRAY  ( LUN, 'I4', 1, L_SPE, NEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4843, IUER, 'UPDATE_SPE', 'Failure in '// &
     &         'reading into the output file '//FILSPE )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL RDBIN_ARRAY  ( LUN, 'I4', 1, SIZE_SPE, NEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4844, IUER, 'UPDATE_SPE', 'Failure in '// &
     &         'reading into the output file '//FILSPE )
           RETURN 
      END IF
!
      CALL ERR_PASS     ( IUER, IER )
      CALL RDBIN_ARRAY  ( LUN, 'B1', SIZE_SPE, SPE, NEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4845, IUER, 'UPDATE_SPE', 'Failure in '// &
     &         'reading into the output file '//FILSPE )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4846, IUER, 'UPDATE_SPE', 'Failure in '// &
     &         'closing the SPESOL output file '//FILSPE )
           RETURN 
      END IF
!
      DO 410 J1=1,L_PAR
         IF ( C_PAR(J1)(9:16) == ' XBSPLN ' .OR. &
     &        C_PAR(J1)(9:16) == ' YBSPLN ' .OR. &
     &        C_PAR(J1)(9:16) == ' ZBSPLN '      ) THEN
              DO 420 J2=1,L_SPE
                 STA_NAM = SPE(J2)%STATION  
                 IF ( C_PAR(J1)(1:8) == STA_NAM ) THEN
                      READ ( UNIT=C_PAR(J1)(17:20), FMT='(I4)', IOSTAT=IOS ) INOD
                      IF ( IOS .NE. 0 ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J1, STR ) 
                           CALL ERR_LOG ( 4847, IUER, 'UPDATE_SPE', 'Error '// &
     &                         'in an attempt to decode parameter name '// &
     &                          STR(1:I_LEN(STR))//' -- '//C_PAR(J1) )
                           RETURN 
                      END IF
                      IF ( INOD < 1 - SPE(J2)%DEGREE  .OR.  &
     &                     INOD .GE.  SPE(J2)%K_NOD         ) THEN
                           CALL CLRCH ( STR )
                           CALL INCH  ( J1, STR ) 
                           CALL ERR_LOG ( 4848, IUER, 'UPDATE_SPE', 'Wrong '// &
     &                         'node indes in parameter name '// &
     &                          STR(1:I_LEN(STR))//' -- '//C_PAR(J1) )
                           RETURN 
                      END IF
                      SPE(J2)%USED(INOD) = .TRUE.
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  UPDATE_SPE  !#!#
