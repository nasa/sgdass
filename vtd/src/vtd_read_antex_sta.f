      SUBROUTINE VTD_READ_ANTEX_STA ( VTD, FINAM_PCO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_READ_ANTEX_STA reads a file with antenna phase center *
! *   offsets in the International Global Navigation Satellite System    *
! *   Service (IGS) ascii ANTEX format defined in                        *
! *   https://files.igs.org/pub/data/format/antex14.txt , finds there    *
! *   phase centers offsets for all ground stations, and loads them to   *
! *   the array of VTD%PCO objects inside the VTD object. If array       *
! *   VTD%PCO was already populated, VTD_READ_ANTEX_STA overwrites       *
! *   existing elements of VTD%PCO.                                      *
! *                                                                      *
! *   If Antenna Serial Number field  is NONE, it is not consider a part *
! *   of antenna type field. If it is not NONE, antenna serial number is *
! *   considered as a part of the antenna type field. All blanks inside  *
! *   are replaced with underscores.                                     *
! *                                                                      *
! *   Character / inside antenna type is replaced with character @.      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * FILIN ( CHARACTER ) -- File with the antenna phase offsets.          *
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
! * ## 02-SEP-2025 VTD_READ_ANTEX_STA v1.3 (d) L. Petrov 23-SEP-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      INTEGER*4  IUER
      CHARACTER  FINAM_PCO*(*)
      CHARACTER  STR*128, DATE_VALID_FROM*27, DATE_VALID_UNTIL*27
      CHARACTER, ALLOCATABLE :: BUF(:)*512
      LOGICAL*1  LEX
      REAL*8     VALS(4), DZEN
      INTEGER*4  L_ANT, NBUF, LUN, IOS, ISF, L_FRQ, NEL, NAZ, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, IER
      CHARACTER, EXTERNAL :: GET_UTC_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check whether the file really exists
!
      IF ( ILEN(FINAM_PCO) == 0 ) THEN
           CALL ERR_LOG ( 2951, IUER, 'VTD_READ_ANTEX_STA', 'File '// &
     &         'for satellite phase offsets was not specified. It is required '// &
     &         'when processing obsevations of GNSS satellites' )
           RETURN 
      END IF
!
      INQUIRE ( FILE=FINAM_PCO, EXIST=LEX ) 
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 2952, IUER, 'VTD_READ_ANTEX_STA', 'File '// &
     &         'for satellite phase offsets '// &
     &         TRIM(FINAM_PCO)//' was not found' )
           RETURN 
      END IF
!
! --- Allocate memory for parsing the file
!
      ALLOCATE ( BUF(VTD__M_ATX), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( VTD__M_ATX*VTD__M_STR, STR )
           CALL ERR_LOG ( 2953, IUER, 'VTD_READ_ANTEX_STA', 'Error in an '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory' )
           RETURN 
      END IF
!
      VTD%L_PCO = 0
!
! --- Read the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM_PCO, VTD__M_ATX, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2955, IUER, 'VTD_READ_ANTEX_STA', 'Error in '// &
     &         'an attempt to read input file with phase offset and '// &
     &         'variation coefficients '//FINAM_PCO )
           DEALLOCATE ( BUF )
           RETURN 
      END IF
!
      L_ANT = VTD%L_PCO
      DO 410 J1=1,NBUF-1
         IF ( BUF(J1)(61:76) == 'START OF ANTENNA' ) THEN
!
! ----------- Parse the antenna section
!
              IF ( ILEN(BUF(J1+1)(21:23)) .LT. 1  ) THEN
!
! ---------------- This was the marker of ground antenna
!
                   L_ANT = L_ANT + 1
                   IF ( BUF(J1+1)(17:20) == 'NONE' ) THEN 
                        VTD%PCO(L_ANT)%ANTENNA_TYPE = BUF(J1+1)(1:16)
                      ELSE
                        CALL BLANK_TO_UNDSCR ( BUF(J1+1)(1:20) )
                        VTD%PCO(L_ANT)%ANTENNA_TYPE = BUF(J1+1)(1:20)
                   END IF
                   VTD%PCO(L_ANT)%SAT_CODE     = '    '
                   VTD%PCO(L_ANT)%LOCATION = VTD__GROUND
                   DATE_VALID_FROM  = '1970.01.01_00:00:00.0000000'
                   DATE_VALID_UNTIL = '2099.12.31_23:59:59.9999999'
!
! ---------------- Now we read remaining of the file section related to 
! ---------------- this antenna
!
                   DO 420 J2=J1+1,NBUF
                      IF ( BUF(J2)(61:78) == 'START OF FREQUENCY' ) THEN
!
! ------------------------ Store the index of the 1st frequency
!
                           ISF = J2
                           GOTO 820
                      END IF
                      IF ( BUF(J2)(61:64) == 'DAZI' ) THEN
!
! ------------------------ Step over azimuth
!
                           READ ( BUF(J2)(3:8), FMT='(F6.1)' ) VALS(1)
                           VTD%PCO(L_ANT)%DAZI = VALS(1)*DEG__TO__RAD
                        ELSE IF ( BUF(J2)(61:78) == 'ZEN1 / ZEN2 / DZEN' ) THEN
!
! ------------------------ Zenith angle range and step over zenith
!
                           READ ( BUF(J2)(3:8),   FMT='(F6.1)' ) VALS(1)
                           READ ( BUF(J2)(9:14),  FMT='(F6.1)' ) VALS(2)
                           READ ( BUF(J2)(15:20), FMT='(F6.1)' ) VALS(3)
                           VTD%PCO(L_ANT)%EL_MIN = VALS(1)*DEG__TO__RAD
                           VTD%PCO(L_ANT)%EL_MAX = VALS(2)*DEG__TO__RAD
                           DZEN = VALS(3)*DEG__TO__RAD
                           VTD%PCO(L_ANT)%DELEV = DZEN
                        ELSE IF ( BUF(J2)(61:70) == 'SINEX CODE' ) THEN
                           VTD%PCO(L_ANT)%SINEX_CODE = BUF(J2)(1:10)
                        ELSE IF ( BUF(J2)(61:78) == '# OF FREQUENCIES' ) THEN
!
! ------------------------ Learn the number of frequencies
!
                           CALL CHIN ( BUF(J2)(1:6), VTD%PCO(L_ANT)%NBANDS )
!
! ------------------------ Compute the number of elements of the grid
! ------------------------ over azimuth (NAZ) and elevation (NEL)
!
                           NEL = NINT( (VTD%PCO(L_ANT)%EL_MAX - VTD%PCO(L_ANT)%EL_MIN)/DZEN ) + 1
                           IF ( VTD%PCO(L_ANT)%DAZI < 1.D-5 ) THEN
                                NAZ = 1
                             ELSE
                                NAZ = NINT( PI2/VTD%PCO(L_ANT)%DAZI ) + 1
                           END IF
!
! ------------------------ Allocate memory for the PCO pattern
!
                           ALLOCATE  ( VTD%PCO(L_ANT)%BAND(VTD%PCO(L_ANT)%NBANDS) )
                      END IF 
 420               CONTINUE 
 820               CONTINUE 
!
! ---------------- Transform validity dates to MJD/TAI
!
                   CALL DATE_TO_TIME ( DATE_VALID_FROM,  VTD%PCO(L_ANT)%MJD_VALID_FROM, &
     &                                 VTD%PCO(L_ANT)%TAI_VALID_FROM, IER )
                   CALL DATE_TO_TIME ( DATE_VALID_UNTIL, VTD%PCO(L_ANT)%MJD_VALID_UNTIL, &
     &                                 VTD%PCO(L_ANT)%TAI_VALID_UNTIL, IER )
!
! ---------------- Cycle over frequencies
!
                   L_FRQ = 0
                   DO 430 J3=ISF,NBUF
                      IF ( BUF(J3)(61:78) == 'START OF FREQUENCY' ) THEN
!
! ------------------------ New frequecy arrauved!
!
                           L_FRQ = L_FRQ + 1
!
! ------------------------ Allocate memory for the frequency pattern
!
                           ALLOCATE ( VTD%PCO(L_ANT)%BAND(L_FRQ)%PHASE_PAT(NAZ,NEL) )
                           VTD%PCO(L_ANT)%BAND(L_FRQ)%NAZ = NAZ
                           VTD%PCO(L_ANT)%BAND(L_FRQ)%NEL = NEL
                           VTD%PCO(L_ANT)%BAND(L_FRQ)%FREQ_NAME = BUF(J3)(4:6)
                           IF ( ILEN(VTD%PCO(L_ANT)%BAND(L_FRQ)%FREQ_NAME) < 1 ) THEN
                                CALL CLRCH ( STR ) 
                                CALL INCH  ( J3, STR )
                                CALL ERR_LOG ( 2957, IUER, 'VTD_READ_ANTEX_STA', &
     &                              'Trap of internal control: line '//TRIM(STR)// &
     &                              ' has no frequency code' )
                                DEALLOCATE ( BUF )
                                RETURN 
                           END IF
!
! ------------------------ Determine the frequency from the frqeuency name
!
                           CALL ERR_PASS ( IUER, IER )
                           CALL VTD_ANTEX_FREQ ( VTD, &
     &                                           VTD%PCO(L_ANT)%BAND(L_FRQ)%FREQ_NAME, &
     &                                           L_ANT, L_FRQ, IER )
                           IF ( IER .NE. 0 ) THEN
                                CALL CLRCH ( STR ) 
                                CALL INCH  ( J3, STR )
                                CALL ERR_LOG ( 2958, IUER, 'VTD_READ_ANTEX_STA', &
     &                              'Trap of internal control: unknown frequency '// &
     &                              'code at line '//STR )
                                DEALLOCATE ( BUF )
                                RETURN 
                           END IF
                           VTD%PCO(L_ANT)%BAND(L_FRQ)%STATUS = VTD__LOAD
                        ELSE IF ( BUF(J3)(61:77) == 'NORTH / EAST / UP' ) THEN
!
! ------------------------ Get constant eccentricity vector
!
                           READ ( BUF(J3)(1:10),  FMT='(F10.2)' ) VALS(1)
                           READ ( BUF(J3)(11:20), FMT='(F10.2)' ) VALS(2)
                           READ ( BUF(J3)(21:30), FMT='(F10.2)' ) VALS(3)
                           VTD%PCO(L_ANT)%BAND(L_FRQ)%PCO_UEN(1) = VALS(3)*0.001D0
                           VTD%PCO(L_ANT)%BAND(L_FRQ)%PCO_UEN(2) = VALS(2)*0.001D0
                           VTD%PCO(L_ANT)%BAND(L_FRQ)%PCO_UEN(3) = VALS(1)*0.001D0
                        ELSE IF ( BUF(J3)(1:9) == '   NOAZI ' ) THEN
!
! ------------------------ Get azimuth-independent 1D pattern
!
                           DO 440 J4=1,NEL
                              READ ( UNIT=BUF(J3)(9+(J4-1)*8:9+J4*8), FMT='(F8.2)' ) VALS(1)
                              VTD%PCO(L_ANT)%BAND(L_FRQ)%PHASE_PAT(1,J4) = VALS(1)*0.001D0
 440                       CONTINUE 
                           IF ( NAZ > 1 ) THEN
!
! ----------------------------- Get azimuth/elevation 2D pattern
!
                                DO 450 J5=1,NAZ
                                   DO 460 J6=1,NEL
                                      READ ( UNIT=BUF(J3+J5)(9+(J6-1)*8:9+J6*8), FMT='(F8.2)' ) VALS(1)
                                      VTD%PCO(L_ANT)%BAND(L_FRQ)%PHASE_PAT(J5,J6) = VALS(1)*0.001D0
 460                               CONTINUE 
 450                            CONTINUE 
                           END IF
                      END IF 
                      IF ( BUF(J3)(61:74) == 'END OF ANTENNA' ) THEN
                           VTD%PCO(L_ANT)%STATUS = VTD__LOAD
                           GOTO 830
                      END IF
 430               CONTINUE 
 830               CONTINUE 
              END IF
         END IF
 410  CONTINUE 
      VTD%L_PCO = L_ANT
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE VTD_READ_ANTEX_STA  !#!#
