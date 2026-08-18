      SUBROUTINE VTD_WRITE_BINTEX ( VTD, ANT_CLASS, DIR_BTX, FINAM_PCO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_WRITE_BINTEX writes all phase calibration offset       *
! *   objects of class ANT_CLASS defined in the VTD object into files    *
! *   oin binary format BINTEX in the specified directory DIR_BTX.       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! * ANT_CLASS ( CHARACTER ) -- Antenna class. Supported classes:         *
! *                            VTD__SPACE for space-born atennas and     *
! *                            VTD__GROUND for ground antennas, mainly   *
! *                            designed for GNSS observations.           *
! *                            Naming convention is different for        *
! *                            these two types.                          *
! * DIR_BTX   ( CHARACTER ) -- Directory with phase offset files in      *
! *                            BINTEX format.                            *
! * FINAM_PCO ( CHARACTER ) -- an original file wit antenna phase center *
! *                            offset, for instance, in ANTEX format.    *
! *                            Used for reporting in the header.         *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * ### 02-SEP-2025  VTD_WRITE_BINTEX v1.0 (d) L. Petrov 05-SEP-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      CHARACTER  ANT_CLASS*(*), DIR_BTX*(*), FINAM_PCO*(*)
      INTEGER*4  IUER
      CHARACTER  FILOUT*128, DATE_VALID_FROM*24, DATE_VALID_UNTIL*24, &
     &           STR*20, STR1*128
      INTEGER*4  LUN, SIZ, IS, J1, J2, J3, J4, IER
      INTEGER*2  MASK
      DATA MASK / O'775' / ! Protection mask: read-write-execute for owner and
!                          ! group, read execute for others
      CHARACTER, EXTERNAL :: GET_UTC_CDATE*19, MJDSEC_TO_DATE*30 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, MKDIR
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
!
      IF ( VTD%L_PCO == 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IF ( .NOT. IS_DIR_EXIST ( DIR_BTX, STR1 ) ) THEN
           IS = MKDIR ( TRIM(DIR_BTX)//CHAR(0), %VAL(MASK) )
           IF ( IS .NE. 0 ) THEN
                CALL CLRCH   ( STR1 )
                CALL ERR_LOG ( 2931, IUER, 'VTD_WRITE_BINTEX', 'Failure to '// &
     &                        'create directory '//TRIM(DIR_BTX)//' -- '//STR1 )
                RETURN
           END IF
      END IF
!
      DO 410 J1=1,VTD%L_PCO
         IF ( ANT_CLASS == VTD__SPACE .AND. VTD%PCO(J1)%LOCATION == VTD__SPACE ) THEN
              DATE_VALID_FROM  = MJDSEC_TO_DATE ( VTD%PCO(J1)%MJD_VALID_FROM,  VTD%PCO(J1)%TAI_VALID_FROM  + VTD__GPS_M_TAI, IER )
              ier = -1
              DATE_VALID_UNTIL = MJDSEC_TO_DATE ( VTD%PCO(J1)%MJD_VALID_UNTIL, VTD%PCO(J1)%TAI_VALID_UNTIL + VTD__GPS_M_TAI, IER )
              FILOUT = TRIM(DIR_BTX)//'/'//TRIM(VTD%PCO(J1)%SAT_CODE)//'_'// &
     &                 DATE_VALID_FROM(1:4)//DATE_VALID_FROM(6:7)//DATE_VALID_FROM(9:10)//'_'// &
     &                 DATE_VALID_UNTIL(1:4)//DATE_VALID_UNTIL(6:7)//DATE_VALID_UNTIL(9:10)//'.btx'
            ELSE IF ( ANT_CLASS == VTD__GROUND .AND. VTD%PCO(J1)%LOCATION == VTD__GROUND  ) THEN
              STR = VTD%PCO(J1)%ANTENNA_TYPE
              DO 420 J2=1,I_LEN(STR)
                 IF ( STR(J2:J2) == '/' ) STR(J2:J2) = '@'
 420          CONTINUE 
              FILOUT = TRIM(DIR_BTX)//'/'//TRIM(STR)//'.btx'
            ELSE 
              GOTO 410
         END IF
!
         CALL ERR_PASS  ( IUER, IER )
         CALL BINF_OPEN ( FILOUT, 'UNKNOWN', LUN, IER )
         IF  ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 2932, IUER, 'VTD_WRITE_BINTEX', 'Error '// &
     &             'in an attempt to open file with PCO/PCV varaition '// &
     &             'in binary format for writing '//FILOUT )
               RETURN
         END IF
!
         CALL WRBIN_STRING ( LUN, VTD__ANTEX_BIN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2933, IUER, 'VTD_WRITE_BINTEX', 'Error '// &
     &            'in an attempt to write a record in file with '// &
     &            'PCO/PCV varaition in binary format for writing '// &
     &             FILOUT )
              RETURN
         END IF
!
         CALL ERR_PASS  ( IUER, IER )
         CALL WRBIN_STRING ( LUN, 'Generator '//VTD__LABEL,   IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2934, IUER, 'VTD_WRITE_BINTEX', 'Error '// &
     &            'in an attempt to write a record in file with '// &
     &            'PCO/PCV varaition in binary format for writing '// &
     &             FILOUT )
              RETURN
         END IF
!
         CALL ERR_PASS  ( IUER, IER )
         CALL WRBIN_STRING ( LUN, 'Created at '//GET_UTC_CDATE()//' UTC', IER )
!
         CALL ERR_PASS  ( IUER, IER )
         CALL WRBIN_STRING ( LUN, 'Input file: '//TRIM(FINAM_PCO), IER )
         SIZ = %LOC(VTD%PCO(J1)%STATUS) - %LOC(VTD%PCO(J1)%ANTENNA_TYPE) + SIZEOF(VTD%PCO(J1)%STATUS)
!
         CALL ERR_PASS  ( IUER, IER )
         CALL WRBIN_ARRAY  ( LUN, 'B1', SIZ, VTD%PCO(J1)%ANTENNA_TYPE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2935, IUER, 'VTD_WRITE_BINTEX', 'Error '// &
     &            'in an attempt to write arecord in file with '// &
     &            'PCO/PCV varaition in binary format for writing '// &
     &             FILOUT )
              RETURN
         END IF
!
         DO 430 J3=1,VTD%PCO(J1)%NBANDS
            SIZ = %LOC(VTD%PCO(J1)%BAND(J3)%STATUS) - %LOC(VTD%PCO(J1)%BAND(J3)%PCO_UEN(1)) + SIZEOF(VTD%PCO(J1)%BAND(J3)%STATUS)
            CALL ERR_PASS  ( IUER, IER )
            CALL WRBIN_ARRAY  ( LUN, 'B1', SIZ, VTD%PCO(J1)%BAND(J3)%PCO_UEN(1), IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 2936, IUER, 'VTD_WRITE_BINTEX', 'Error '// &
     &               'in an attempt to write arecord in file with '// &
     &               'PCO/PCV varaition in binary format for writing '// &
     &                FILOUT )
                 RETURN
            END IF
!
            SIZ = VTD%PCO(J1)%BAND(J3)%NAZ*VTD%PCO(J1)%BAND(J3)%NEL
            CALL ERR_PASS  ( IUER, IER )
            CALL WRBIN_ARRAY  ( LUN, 'R8', SIZ, VTD%PCO(J1)%BAND(J3)%PHASE_PAT, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 2937, IUER, 'VTD_WRITE_BINTEX', 'Error '// &
     &               'in an attempt to write arecord in file with '// &
     &               'PCO/PCV varaition in binary format for writing '// &
     &                FILOUT )
                 RETURN
            END IF
 430     CONTINUE 
!
         CALL ERR_PASS  ( IUER, IER )
         CALL BINF_CLOSE ( LUN, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2938, IUER, 'VTD_WRITE_BINTEX', 'Error '// &
     &            'in an attempt to write arecord in file with '// &
     &            'PCO/PCV varaition in binary format for writing '// &
     &             FILOUT )
              RETURN
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_WRITE_BINTEX  !#!#
