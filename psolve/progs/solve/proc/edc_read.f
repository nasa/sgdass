      SUBROUTINE EDC_READ ( DB_NAME, EDC_DIR, EDC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine EDC_READ 
! *                                                                      *
! *  ### 25-OCT-2007   EDC_READ    v1.0 (c)  L. Petrov  25-OCT-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'edc.i'
      CHARACTER  DB_NAME*(*), EDC_DIR*(*)
      TYPE     ( EDC__TYPE     ) :: EDC
      CHARACTER  FILIN*128, STR*128
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = MAX_OBS + MAX_ARC_STA + MAX_ARC_SRC + 512 )
      CHARACTER*80, POINTER :: BUF(:)
      INTEGER*4  IUER
      LOGICAL*4  LEX, FL_ENDIAN
      INTEGER*4  J1, J2, J3, IS, IL, IFMT, LUN, NBT, NBUF, IND_OBS, &
     &           IND_STA, IND_SOU, MJD_OBS, IER
      REAL*8     TAI_OBS
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LTM_DIF, READ
!
      CALL NOUT ( SIZEOF(EDC), EDC )
!
! --- First try files with extrension .edb
!
      IF ( DB_NAME(1:1) == '$' ) THEN
           FILIN = EDC_DIR(1:I_LEN(EDC_DIR))//'/'// &
     &             DB_NAME(2:I_LEN(DB_NAME))//'.edb'
         ELSE 
           FILIN = EDC_DIR(1:I_LEN(EDC_DIR))//'/'// &
     &             DB_NAME(1:I_LEN(DB_NAME))//'.edb'
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
!
! -------- No luck? Let us try file with extension .eda
!
           IL = ILEN(FILIN)
           FILIN(IL:IL) = 'a'
      END IF
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN     
           CALL ERR_LOG ( 7641, IUER, 'EDC_READ', 'Cannot find input '// &
     &         'external decimation file '//FILIN )
           RETURN 
      END IF
!
      EDC%EDC_FILE = FILIN 
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL BINF_OPEN ( FILIN, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7642, IUER, 'EDC_READ', 'Error in an '// &
     &         'attempt to open external decimation output file '//FILIN )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), %REF(STR), &
     &                       %VAL(LEN(EDC__BF_LABEL)) )
      IF ( IS .NE. LEN(EDC__BF_LABEL) ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 7643, IUER, 'EDC_READ', 'Error during '// &
     &         'reading the file format label record in the input '// &
     &         'external decimation file '//FILIN(1:ILEN(FILIN))// &
     &         ' --- '//STR )
           RETURN
      END IF
!
      IF ( STR(1:32) == EDC__BF_LABEL(1:32) ) THEN
           IFMT = EDC__BIN
#ifdef BIG_ENDIAN
           IF ( STR(34:35) == EDC__BE ) THEN
                FL_ENDIAN = .FALSE.
              ELSE 
                FL_ENDIAN = .TRUE.
           END IF
#endif
#ifdef LITTLE_ENDIAN
           IF ( STR(34:35) == EDC__LE ) THEN
                FL_ENDIAN = .FALSE.
              ELSE 
                FL_ENDIAN = .TRUE.
           END IF
#endif
         ELSE IF ( STR(1:31) == EDC__AF_LABEL(1:31) ) THEN
           IFMT = EDC__ASC
         ELSE 
           WRITE ( 6, * ) ' STR(1:31) >>'//STR(1:31)//'<<  ' ! %%%%%%
           CALL ERR_LOG ( 7644, IUER, 'EDC_READ', 'Attempt to read a wrong '// &
     &         'input external decimation file '//FILIN(1:ILEN(FILIN))// &
     &         ' -- its format has not been recognized' )
           RETURN
      END IF
!
      IF ( IFMT == EDC__BIN ) THEN
!
! -------- Read binary file
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_RECORD ( LUN, SIZEOF(EDC%HEA), EDC%HEA, NBT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7645, IUER, 'EDC_READ', 'Error during '// &
     &              'reading the header record from the input '// &
     &              'external decimation file '//FILIN(1:ILEN(FILIN))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           ALLOCATE ( EDC%C_STA(EDC%HEA%N_STA), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7646, IUER, 'EDC_READ', 'Failure to '// &
     &              'allocate dynamic array for station names' )
                RETURN 
           END IF
!
           ALLOCATE ( EDC%C_SOU(EDC%HEA%N_SOU), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                WRITE ( 6, * ) 'EDC%HEA%N_SOU = ', EDC%HEA%N_SOU
                CALL ERR_LOG ( 7647, IUER, 'EDC_READ', 'Failure to '// &
     &              'allocate dynamic array for source names' )
                RETURN 
           END IF
!
           ALLOCATE ( EDC%OBS(EDC%HEA%N_OBS), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( SIZEOF(EDC%OBS(1))*EDC%HEA%N_OBS, STR )
                CALL ERR_LOG ( 7648, IUER, 'EDC_READ', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for observation sub-object of EDC' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_RECORD ( LUN, EDC%HEA%N_STA*SIZEOF(EDC%C_STA(1)), &
     &                         EDC%C_STA, NBT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7649, IUER, 'EDC_READ', 'Error during '// &
     &              'reading the station record from the input '// &
     &              'external decimation file '//FILIN(1:ILEN(FILIN))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_RECORD ( LUN, EDC%HEA%N_SOU*SIZEOF(EDC%C_SOU(1)), &
     &                         EDC%C_SOU, NBT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7650, IUER, 'EDC_READ', 'Error during '// &
     &              'reading the source record from the input '// &
     &              'external decimation file '//FILIN(1:ILEN(FILIN))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_RECORD ( LUN, EDC%HEA%N_OBS*SIZEOF(EDC%OBS(1)), &
     &                         EDC%OBS, NBT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH   ( STR )
                CALL GERROR  ( STR )
                CALL ERR_LOG ( 7651, IUER, 'EDC_READ', 'Error during '// &
     &              'reading the observation record from the input '// &
     &              'external decimation file '//FILIN(1:ILEN(FILIN))// &
     &              ' --- '//STR )
                RETURN
           END IF
!
           CALL ERR_PASS   ( IUER, IER )
           CALL BINF_CLOSE ( LUN,  IER )
         ELSE IF ( IFMT == EDC__ASC ) THEN
           CALL ERR_PASS   ( IUER, IER )
           CALL BINF_CLOSE ( LUN,  IER )
!
! -------- Allocate buffer
!
           ALLOCATE ( BUF(MBUF), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( MBUF*SIZEOF(BUF(1)), STR )
                CALL ERR_LOG ( 7652, IUER, 'EDC_READ', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory for a temporary text buffer' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( FILIN, MBUF, BUF, NBUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 7653, IUER, 'EDC_READ', 'Error in an '// &
     &              'attempt read the input external decimation file '// &
     &               FILIN(1:ILEN(FILIN))//' in ascii format' )
                RETURN 
           END IF
!
           IF ( BUF(1) .NE. BUF(NBUF) ) THEN
                CALL ERR_LOG ( 7654, IUER, 'EDC_READ', 'Trailer line in '// &
     &              'the input external decimation file '// &
     &               FILIN(1:ILEN(FILIN))//' was not found. Apparanetly '// &
     &              'the file has been damaged' )
                RETURN 
           END IF
!
           DO 410 J1=2,NBUF-1
              IF ( BUF(J1)(1:1) == '#' ) GOTO 410
              IF ( BUF(J1)(1:4) == 'DBN:' ) THEN
                   EDC%HEA%DB_NAME = BUF(J1)(6:)
                 ELSE IF ( BUF(J1)(1:4) == 'PRC:' ) THEN
                   EDC%HEA%PRC_NAME = BUF(J1)(6:)
                 ELSE IF ( BUF(J1)(1:8) == 'DAT_CRE:' ) THEN
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL DATE_TO_TIME ( BUF(J1)(10:30), EDC%HEA%MJD_CRE, &
     &                                 EDC%HEA%TIM_CRE, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7655, IUER, 'EDC_READ', 'Error in '// &
     &                      'date transformation '//BUF(J1)(10:30)// &
     &                      ' in processing line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
                 ELSE IF ( BUF(J1)(1:6) == 'N_OBS:' ) THEN
                   CALL CHIN ( BUF(J1)(8:13), EDC%HEA%N_OBS )
                   ALLOCATE ( EDC%OBS(EDC%HEA%N_OBS), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL IINCH ( SIZEOF(EDC%OBS(1))*EDC%HEA%N_OBS, STR )
                        CALL ERR_LOG ( 7656, IUER, 'EDC_READ', 'Failure '// &
     &                      'to allocate '//STR(1:I_LEN(STR))// &
     &                      ' bytes of dynamic memory for the '// &
     &                      'observation sub-object of EDC' )
                        RETURN 
                   END IF
                 ELSE IF ( BUF(J1)(1:6) == 'N_SCA:' ) THEN
                   CALL CHIN ( BUF(J1)(8:13), EDC%HEA%N_SCA )
                 ELSE IF ( BUF(J1)(1:6) == 'N_STA:' ) THEN
                   CALL CHIN ( BUF(J1)(8:13), EDC%HEA%N_STA )
                   ALLOCATE ( EDC%C_STA(EDC%HEA%N_STA), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7657, IUER, 'EDC_READ', 'Failure '// &
     &                      'to allocate dynamic array for station names' )
                        RETURN 
                   END IF
                 ELSE IF ( BUF(J1)(1:6) == 'N_SOU:' ) THEN
                   CALL CHIN ( BUF(J1)(8:13), EDC%HEA%N_SOU )
                   ALLOCATE ( EDC%C_SOU(EDC%HEA%N_SOU), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 7658, IUER, 'EDC_READ', 'Failure '// &
     &                      'to allocate dynamic array for source names' )
                        RETURN 
                   END IF
                 ELSE IF ( BUF(J1)(1:8) == 'DAT_SES:' ) THEN
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL DATE_TO_TIME ( BUF(J1)(10:30), EDC%HEA%MJD_SES, &
     &                                 EDC%HEA%TAI_SES, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7659, IUER, 'EDC_READ', 'Error in '// &
     &                      'date transformation '//BUF(J1)(10:30)// &
     &                      ' in processing line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
                 ELSE IF ( BUF(J1)(1:6) == 'C_STA:' ) THEN
                   CALL CHIN (  BUF(J1)(8:11), IND_STA )
                   IF ( IND_STA .LE. 0  .OR.  IND_STA > EDC%HEA%N_STA ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7660, IUER, 'EDC_READ', 'Wrong '// &
     &                      'station index '//BUF(J1)(8:11)// &
     &                      ' at line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
                   EDC%C_STA(IND_STA) = BUF(J1)(14:21)
                 ELSE IF ( BUF(J1)(1:6) == 'C_SOU:' ) THEN
                   CALL CHIN (  BUF(J1)(8:11), IND_SOU )
                   IF ( IND_SOU .LE. 0  .OR.  IND_SOU > EDC%HEA%N_SOU ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7661, IUER, 'EDC_READ', 'Wrong '// &
     &                      'source index '//BUF(J1)(8:11)// &
     &                      ' at line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
                   EDC%C_SOU(IND_SOU) = BUF(J1)(14:21)
                 ELSE IF ( BUF(J1)(1:4) == 'Obs:' ) THEN
                   CALL CHIN (  BUF(J1)(6:11), IND_OBS )
                   IF ( IND_OBS .LE. 0  .OR.  IND_OBS > EDC%HEA%N_OBS ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7662, IUER, 'EDC_READ', 'Wrong '// &
     &                      'observation index '//BUF(J1)(6:11)// &
     &                      ' at line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
!
                   CALL ERR_PASS ( IUER, IER ) 
                   CALL DATE_TO_TIME ( BUF(J1)(14:34), MJD_OBS, TAI_OBS, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7663, IUER, 'EDC_READ', 'Error in '// &
     &                      'date transformation '//BUF(J1)(14:34)// &
     &                      ' in processing line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
!
                   EDC%OBS(IND_OBS)%TIM_OBS = &
     &                             ( MJD_OBS - EDC%HEA%MJD_SES )*86400.0D0 + &
     &                             ( TAI_OBS - EDC%HEA%TAI_SES )
                   EDC%OBS(IND_OBS)%IND_STA(1) =  &
     &                 LTM_DIF ( 0, EDC%HEA%N_STA, EDC%C_STA, BUF(J1)(37:44) )
                   EDC%OBS(IND_OBS)%IND_STA(2) =  &
     &                 LTM_DIF ( 0, EDC%HEA%N_STA, EDC%C_STA, BUF(J1)(46:53) )
                   EDC%OBS(IND_OBS)%IND_SOU = &
     &                 LTM_DIF ( 0, EDC%HEA%N_SOU, EDC%C_SOU, BUF(J1)(56:63) )
                   CALL CHIN ( BUF(J1)(66:69), EDC%OBS(IND_OBS)%SUP_STS )
                   CALL CHIN ( BUF(J1)(72:75), EDC%OBS(IND_OBS)%DCM_STS )
                   IF ( EDC%OBS(IND_OBS)%IND_STA(1) .LE. 0  .OR. &
                        EDC%OBS(IND_OBS)%IND_STA(1) > EDC%HEA%N_STA ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7664, IUER, 'EDC_READ', 'Undefined '// &
     &                      'station name '//BUF(J1)(37:44)// &
     &                      ' at line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
                   IF ( EDC%OBS(IND_OBS)%IND_STA(2) .LE. 0  .OR. &
                        EDC%OBS(IND_OBS)%IND_STA(2) > EDC%HEA%N_STA ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7665, IUER, 'EDC_READ', 'Undefined '// &
     &                      'station name '//BUF(J1)(46:53)// &
     &                      ' at line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
                   IF ( EDC%OBS(IND_OBS)%IND_SOU .LE. 0  .OR. &
                        EDC%OBS(IND_OBS)%IND_SOU > EDC%HEA%N_SOU ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( J1, STR )
                        CALL ERR_LOG ( 7666, IUER, 'EDC_READ', 'Undefined '// &
     &                      'source name '//BUF(J1)(56:63)// &
     &                      ' at line '//BUF(J1)(1:8)//' of '// &
     &                      'the external decimation file '//FILIN )
                        RETURN 
                   END IF
                 ELSE 
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 7667, IUER, 'EDC_READ', 'Unrecoginzed '// &
     &                 'prefix '//BUF(J1)(1:8)//' at line '// &
     &                  STR(1:I_LEN(STR))//'of the external decimation '// &
     &                 'file '//FILIN )
                   RETURN 
              END IF
 410       CONTINUE 
           DEALLOCATE ( BUF )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EDC_READ  !#!#
