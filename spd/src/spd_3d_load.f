      SUBROUTINE SPD_3D_LOAD ( FIL_MET, HEB_GEOID_BSPL, SPD, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SPD_3D_LOAD  reads 
! *                                                                      *
! *  ### 16-DEC-2007  SPD_3D_LOAD  v4.0 (c) L. Petrov  01-AUG-2021  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'spd.i'
      INCLUDE   'heb.i'
      CHARACTER  FIL_MET*(*)
      TYPE     ( HEB__TYPE    ) :: HEB_GEOID_BSPL
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( HEB__TYPE    ) :: HEB_D, HEB_T, HEB_Q, HEB_OH
      INTEGER*4  IUER
      LOGICAL*1  LEX
      CHARACTER  STR*128, FILD*128, FILT*128, FILQ*128
      REAL*8     CNS_DR2(2), LON_STEP, LAT_STEP, LON, LAT, ARC
      LOGICAL*1  FL_MASK
      INTEGER*4  ID, J1, J2, J3, J4, J5, I_LON, NUM_PT, NLON, NLAT, DIMO(4), IVRB, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: ARC_LEN_AD 
!
      IF ( SPD%CONF%SOB_ALG == SOB__ALG_ZPD .OR. SPD%CONF%SOB_ALG == SOB__ALG_MZPD ) THEN
           FL_MASK = .FALSE.
         ELSE 
           FL_MASK = .TRUE.
      END IF
!
      DIMO = 0
      IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( %VAL(0) )
      END IF
      INQUIRE ( FILE=FIL_MET, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 6211, IUER, 'SPD_3D_LOAD', 'Cannot find input '// &
     &         'meteorological file '//FIL_MET )
           RETURN 
      END IF
      ID = ILEN(FIL_MET)
      IF ( ID < 26 ) THEN
           CALL ERR_LOG ( 6212, IUER, 'SPD_3D_LOAD', 'Wrong name (too short) '// &
     &         'of the input meteorological file '//FIL_MET )
           RETURN 
      END IF
      IF ( FIL_MET(ID-7:ID) == '.heb.bz2' ) THEN
           FILD = FIL_MET(1:ID-25)//'d/d'//FIL_MET(ID-21:ID)
           FILT = FIL_MET(1:ID-25)//'t/t'//FIL_MET(ID-21:ID)
           FILQ = FIL_MET(1:ID-25)//'q/q'//FIL_MET(ID-21:ID)
        ELSE IF ( FIL_MET(ID-3:ID) == '.heb' ) THEN
           FILD = FIL_MET(1:ID-21)//'d/d'//FIL_MET(ID-17:ID)
           FILT = FIL_MET(1:ID-21)//'t/t'//FIL_MET(ID-17:ID)
           FILQ = FIL_MET(1:ID-21)//'q/q'//FIL_MET(ID-17:ID)
        ELSE 
           CALL ERR_LOG ( 6213, IUER, 'SPD_3D_LOAD', 'Wrong extension of '// &
     &         'the input meteorological file '//FIL_MET(1:I_LEN(FIL_MET))// &
     &         ' -- extensions .heb or .heb.bz2 were expected' )
           RETURN 
      END IF
      IF ( IVRB > 1 ) THEN
           WRITE ( 6, '(A)' ) 'Started reading and decompressing input files' 
      END IF
!
! --- Read heb file with atmosphere layer thickness
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FILD, HEB_D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6214, IUER, 'SPD_3D_LOAD', 'Error in reading '// &
     &         'the input file with atmosphere layer thickness '//FILD )
           RETURN 
      END IF
      SPD%MJD = HEB_D%MJD
      SPD%UTC = HEB_D%UTC
      SPD%TAI = HEB_D%TAI
!
! --- Read heb file with air temperature
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FILT, HEB_T, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6215, IUER, 'SPD_3D_LOAD', 'Error in reading '// &
     &         'the input file with air temperature '//FILT )
           RETURN 
      END IF
!
! --- Read heb file with specific humidity
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( FILQ, HEB_Q, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6216, IUER, 'SPD_3D_LOAD', 'Error in reading '// &
     &         'the input file with specific humidity '//FILQ )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_HEB ( SPD%CONF%FIL_OH, HEB_OH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6217, IUER, 'SPD_3D_LOAD', 'Error in reading '// &
     &         'the input file with heights above geoid '//SPD%CONF%FIL_OH )
           RETURN 
      END IF
      IF ( SPD%CONF%TEST_STR == 'timer' ) THEN
           CALL WALL_TIMER ( STR )
           WRITE ( 6, '(A)' ) 'Loading the data files:  '//STR(1:I_LEN(STR)-5)
           CALL WALL_TIMER ( %VAL(0) )
      END IF
!
      NLON = HEB_D%DIMS(1)
      NLAT = HEB_D%DIMS(2)
      ALLOCATE ( SPD%MASK(NLON,NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6218, IUER, 'SPD_3D_LOAD', 'Error in an attempt '// &
     &         'to allocate SPD%MASK array' )
           RETURN 
      END IF
!
      IF ( FL_MASK ) THEN
           IF ( IVRB > 1 ) THEN
                WRITE ( 6, '(A)' ) 'Started mask computation'
           END IF
!
! -------- Making a mask in order to bypass the area far away from any
! -------- station for computation of the refractivity field
!
           CALL WALL_TIMER ( %VAL(0) )
           SPD%MASK = .FALSE.
           LAT_STEP = PI__NUM/(NLAT-1)
           LON_STEP = PI2/NLON
           DO 410 J1=1,NLAT
              LAT = -P2I + (J1-1)*LAT_STEP
              DO 420 J2=1,NLON
!
! -------------- NB: Merra/Geos longitude starts from -180deg   to +180 deg
! -------------- J4    -- Merra/GEOS indexing from    -180 deg
! -------------- I_LON -- SPD indexing from 0deg to   +360 deg
!
                 I_LON = J2 + NLON/2
                 IF ( I_LON > NLON ) I_LON = I_LON - NLON
                 LON = (I_LON-1)*LON_STEP
                 DO 430 J3=1,SPD%NSTA
!
! ----------------- Check the arc between point (j2,j1) and the j3-th station
!
                    ARC = ARC_LEN_AD ( LON, LAT, SPD%STA(J3)%LON, SPD%STA(J3)%LAT_GDT )
                    IF ( ARC < SPD__MAX_ARC/SPD__REA ) THEN
!
! ---------------------- The arc is shorter than the limit? Use that point!
!
                         SPD%MASK(J2,J1) = .TRUE.       
                    END IF
 430             CONTINUE 
 420          CONTINUE 
 410       CONTINUE 
           IF ( IVRB > 1 ) THEN
                NUM_PT = 0
                DO 440 J4=1,NLAT
                   DO 450 J5=1,NLON
                      IF ( SPD%MASK(J5,J4) ) NUM_PT = NUM_PT + 1
 450               CONTINUE 
 440            CONTINUE 
                WRITE ( 6, 110 ) 100.0*FLOAT(NUM_PT)/(NLAT*NLON)
 110            FORMAT ( 'SPD_3D_LOAD: mask is applied. ', F7.3, ' % points are used' )
                CALL WALL_TIMER ( STR )
                WRITE ( 6, '(A)' ) 'Mask computation took:  '//STR(1:I_LEN(STR)-5)
           END IF
           CALL WALL_TIMER ( %VAL(0) )
         ELSE 
!
! -------- Use all the points
!
           SPD%MASK = .TRUE.
           IF ( IVRB > 1 ) THEN
                WRITE ( 6, '(A)' ) 'Mask was not computed. All points will be used'
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL SPD_3D_REFRA ( SPD, HEB_D, HEB_T, HEB_Q, HEB_OH, HEB_GEOID_BSPL, &
     &                    DIMO, CNS_DR2, IVRB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6219, IUER, 'SPD_3D_LOAD', 'Error in an attempt '// &
     &         'to compute the coefficients of expansion of refractivity '// &
     &         'field into tensor product of 3D B-splines' )
           RETURN 
      END IF
      CALL FREE_HEB ( HEB_D  )
      CALL FREE_HEB ( HEB_T  )
      CALL FREE_HEB ( HEB_Q  )
      CALL FREE_HEB ( HEB_OH )
!
      SPD%CONF%TITLE       = HEB_D%TITLE
      SPD%CONF%INSTITUTION = HEB_D%INSTITUTION
      SPD%CONF%REFERENCE   = HEB_D%REFERENCES
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SPD_3D_LOAD  !#!#
