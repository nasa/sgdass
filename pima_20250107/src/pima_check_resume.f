      SUBROUTINE PIMA_CHECK_RESUME ( NEW_CONF, OLD_CONF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PLMA_CHECK_RESUME checks whether resuming computation is   *
! *   feasible by comaring two configuration files: the old one and      *
! *   the new one.                                                       *
! *                                                                      *
! * ## 11-JAN-2006  PIMA_CHECK_RESUME  v1.0 (c) L. Petrov 11-JAN-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      TYPE     ( PIM_CONF__TYPE ) :: NEW_CONF, OLD_CONF
      CHARACTER  STR*128, STR1*128
      INTEGER*4  J1, J2, J3, J4, IP1, IP2, IL1, IL2, IER
      INTEGER*4  IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      IF ( NEW_CONF%L_FIL .NE. OLD_CONF%L_FIL ) THEN
           CALL CLRCH ( STR  ) 
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( NEW_CONF%L_FIL, STR  )
           CALL INCH  ( OLD_CONF%L_FIL, STR1 )
           CALL ERR_LOG ( 7481, IUER, 'PIMA_CHECK_RESUME', 'The total '// &
     &         'number of input FITS-IDI files is different: old: '// &
     &          STR(1:I_LEN(STR))//', new: '//STR1 )
           RETURN 
      END IF
!
      DO 410 J1=1,OLD_CONF%L_FIL
         IP1 = LINDEX ( OLD_CONF%UVFILE_NAME(J1), '/' ) + 1
         IP2 = LINDEX ( NEW_CONF%UVFILE_NAME(J1), '/' ) + 1
         IL1 = I_LEN  ( OLD_CONF%UVFILE_NAME(J1)  )
         IL2 = I_LEN  ( NEW_CONF%UVFILE_NAME(J1)  )
!
         IF ( OLD_CONF%UVFILE_NAME(J1)(IP1:IL1) .NE. &
     &        NEW_CONF%UVFILE_NAME(J1)(IP2:IL2)      ) THEN
!
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 7482, IUER, 'PIMA_CHECK_RESUME', 'The name '// &
     &            'of the '//STR(1:I_LEN(STR))//'th input FITS-IDI files '// &
     &            'is different: old: '// &
     &             OLD_CONF%UVFILE_NAME(J1)(IP1:IL1)// &
     &            ' new: '//NEW_CONF%UVFILE_NAME(J1)(IP2:IL2) )
              RETURN 
         END IF
!
         IF ( OLD_CONF%UVFILE_NAME(J1) .NE. NEW_CONF%UVFILE_NAME(J1) ) THEN
         END IF
 410  CONTINUE 
!
      IP1 = LINDEX ( OLD_CONF%STA_NAMES_FILE, '/' ) + 1
      IP2 = LINDEX ( NEW_CONF%STA_NAMES_FILE, '/' ) + 1
      IL1 = I_LEN  ( OLD_CONF%STA_NAMES_FILE  )
      IL2 = I_LEN  ( NEW_CONF%STA_NAMES_FILE  )
!
!@      IF ( OLD_CONF%STA_NAMES_FILE(IP1:IL1) .NE. &
!@     &     NEW_CONF%STA_NAMES_FILE(IP2:IL2)      ) THEN
!@!
!@           CALL ERR_LOG ( 7483, IUER, 'PIMA_CHECK_RESUME', 'The name '// &
!@     &         'of the station coordinate input file '// &
!@     &         'is different: old: '// &
!@     &          OLD_CONF%STA_NAMES_FILE(IP1:IL1)// &
!@     &         ' new: '//NEW_CONF%STA_NAMES_FILE(IP2:IL2) )
!@           RETURN 
!@      END IF
!@!
!@      IF ( OLD_CONF%SOU_NAMES_FILE(IP1:IL1) .NE. &
!@     &     NEW_CONF%SOU_NAMES_FILE(IP2:IL2)      ) THEN
!@!
!@           CALL ERR_LOG ( 7484, IUER, 'PIMA_CHECK_RESUME', 'The name '// &
!@     &         'of the source coordinate input file '// &
!@     &         'is different: old: '// &
!@     &          OLD_CONF%SOU_NAMES_FILE(IP1:IL1)// &
!@     &         ' new: '//NEW_CONF%SOU_NAMES_FILE(IP2:IL2) )
!@           RETURN 
!@      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_CHECK_RESUME  !#!#
