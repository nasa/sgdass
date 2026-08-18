      SUBROUTINE VTD_READ_BINTEX ( FILIN, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_READ_BINTEX reads a file with antenna phase offset     *
! *   in binary BINTEX format and loads it to the next object VTD%PCO    *
! *   in slot VTD%L_PCO+1. After that it augments VTD%L_PCO counter.     *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * FILIN ( CHARACTER ) -- File with antenna phase offsets.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 04-SEP-2025 VTD_READ_BINTEX v1.0 (d) L. Petrov  04-SEP-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      CHARACTER  FILIN*(*)
      INTEGER*4  IUER
      CHARACTER  STR*128
      INTEGER*4  SIZ, NBT, LN, LUN, J1, J2, IER
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FILIN, 'UNKNOWN', LUN, IER )
      IF  ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 2971, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &          'in an attempt to open file with PCO/PCV varaition '// &
     &          'in binary format for reading '//FILIN )
            RETURN
      END IF
!
      CALL RDBIN_STRING ( LUN, STR, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2972, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &         'in an attempt to read the first record from the file '// &
     &         'with PCO/PCV varaition in binary format '//FILIN )
           RETURN
      END IF
!
      IF ( LN .NE. LEN(VTD__ANTEX_BIN) ) THEN
           CALL ERR_LOG ( 2973, IUER, 'VTD_READ_BINTEX', 'Input file '// &
     &         'with PCO/PCV varaition in binary format '//TRIM(FILIN)// &
     &         ' has wrong first line. Expected magic: '//VTD__ANTEX_BIN )
           RETURN
      END IF
!
      IF ( STR(1:LN) .NE. VTD__ANTEX_BIN ) THEN
           CALL ERR_LOG ( 2974, IUER, 'VTD_READ_BINTEX', 'Input file '// &
     &         'with PCO/PCV varaition in binary format '//TRIM(FILIN)// &
     &         ' has wrong first line. Expected magic: '//VTD__ANTEX_BIN )
           RETURN
      END IF
!
      VTD%L_PCO = VTD%L_PCO +  1
      IF ( VTD%L_PCO > VTD__M_PCO ) THEN
           CALL ERR_LOG ( 2975, IUER, 'VTD_READ_BINTEX', 'Too many '// &
     &         'records with PCO/PVO -- more than '//TRIM(STR)//&
     &         '. Please increase VTD__M_PCO '//STR )
           RETURN 
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL RDBIN_STRING ( LUN, STR, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2976, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &         'in an attempt to read the second record from the file '// &
     &         'with PCO/PCV varaition in binary format '//FILIN )
           RETURN
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL RDBIN_STRING ( LUN, STR, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2977, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &         'in an attempt to read the third record from the file '// &
     &         'with PCO/PCV varaition in binary format '//FILIN )
           RETURN
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL RDBIN_STRING ( LUN, STR, LN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2978, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &         'in an attempt to read the fourth record from the file '// &
     &         'with PCO/PCV varaition in binary format '//FILIN )
           RETURN
      END IF
!
      SIZ = %LOC(VTD%PCO(VTD%L_PCO)%STATUS) - &
     &      %LOC(VTD%PCO(VTD%L_PCO)%ANTENNA_TYPE) + &
     &      SIZEOF(VTD%PCO(VTD%L_PCO)%STATUS)
!
      CALL ERR_PASS  ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, SIZ, VTD%PCO(VTD%L_PCO)%ANTENNA_TYPE, NBT, IER )
      IF ( IER .NE. 0 .OR. NBT .NE. SIZ ) THEN
           CALL ERR_LOG ( 2979, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &         'in an attempt to read the data record from the file '// &
     &         'with PCO/PCV varaition in binary format '//FILIN )
           RETURN
      END IF
!
      IF ( ASSOCIATED ( VTD%PCO(VTD%L_PCO)%BAND ) ) THEN
           DEALLOCATE ( VTD%PCO(VTD%L_PCO)%BAND )
      END IF
      ALLOCATE ( VTD%PCO(VTD%L_PCO)%BAND(VTD%PCO(VTD%L_PCO)%NBANDS), &
     &           STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2980, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &         'in an attempt to allocate mmory for PCO' )
           RETURN
      END IF
!
      DO 410 J1=1,VTD%PCO(VTD%L_PCO)%NBANDS
         SIZ = %LOC(VTD%PCO(VTD%L_PCO)%BAND(J1)%STATUS) - &
     &         %LOC(VTD%PCO(VTD%L_PCO)%BAND(J1)%PCO_UEN(1)) + &
     &         SIZEOF(VTD%PCO(VTD%L_PCO)%BAND(J1)%STATUS)
         CALL ERR_PASS  ( IUER, IER )
         CALL RDBIN_RECORD ( LUN, SIZ, VTD%PCO(VTD%L_PCO)%BAND(J1)%PCO_UEN(1), NBT, IER )
         IF ( IER .NE. 0 .OR. NBT .NE. SIZ ) THEN
              CALL ERR_LOG ( 2981, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &            'in an attempt to read the frequency data record '// &
     &            'from the file with PCO/PCV varaition in binary '// &
     &            'bintex format '//FILIN )
              RETURN
         END IF
!
         ALLOCATE ( VTD%PCO(VTD%L_PCO)%BAND(J1)%PHASE_PAT(VTD%PCO(VTD%L_PCO)%BAND(J1)%NAZ,VTD%PCO(VTD%L_PCO)%BAND(J1)%NEL), &
     &             STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2982, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &            'in an attempt to allocate mmory for PCO' )
              RETURN
         END IF
!
         SIZ = 8*VTD%PCO(VTD%L_PCO)%BAND(J1)%NAZ*VTD%PCO(VTD%L_PCO)%BAND(J1)%NEL
         CALL ERR_PASS  ( IUER, IER )
         CALL RDBIN_RECORD ( LUN, SIZ, VTD%PCO(VTD%L_PCO)%BAND(J1)%PHASE_PAT, &
     &                       NBT, IER )
         IF ( IER .NE. 0 .OR. NBT .NE. SIZ ) THEN
              CALL ERR_LOG ( 2983, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &            'in an attempt to read the pattern data record '// &
     &            'from the file with PCO/PCV varaition in binary '// &
     &            'bintex format '//FILIN )
              RETURN
         END IF
 410  CONTINUE 
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2984, IUER, 'VTD_READ_BINTEX', 'Error '// &
     &         'in an attempt to close the file PCO/PCV varaition '// &
     &         'in binary format '//FILIN )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE VTD_READ_BINTEX  !#!#
