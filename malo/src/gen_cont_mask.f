      PROGRAM    GEN_CONT_MASK
! ************************************************************************
! *                                                                      *
! *   Program GEN_CONT_MASK creates the continent mask.                  *
! *                                                                      *
! *  ### 03-DEC-2017  GEN_CONT_MASK v1.0 (c) L. Petrov  03-DEC-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_OUT
      CHARACTER  FILIN*128, FILOUT*128, MODE*4, LAT_STR*128, LON_STR*128
      INTEGER*8  LAND_CELLS
      REAL*8     LAT, LON
      INTEGER*4  J1, J2, IND_LON, IND_LAT, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, * ) 'Usage gen_cont_mask heb_in land|sea lat lon heb_out'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILIN   )
           CALL GETARG ( 2, MODE    )
           CALL GETARG ( 3, LAT_STR )
           IF ( INDEX( LAT_STR, '.' ) < 1 ) LAT_STR = TRIM(LAT_STR)//'.0'
           READ ( UNIT=LAT_STR, FMT=* ) LAT
           LAT = LAT*DEG__TO__RAD
!
           CALL GETARG ( 4, LON_STR )
           IF ( INDEX( LON_STR, '.' ) < 1 ) LON_STR = TRIM(LON_STR)//'.0'
           READ ( UNIT=LON_STR, FMT=* ) LON
           LON = LON*DEG__TO__RAD
           CALL GETARG ( 5, FILOUT )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILIN, HEB_IN, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 3581, IUER, 'GEN_CONT_MASK', 'Error '// &
     &         'in an attempt to read heb-file with land-water '// &
     &         'mask '//FILIN )
           CALL EXIT ( 1 )
      END IF   
!
      IND_LAT = NINT ( (LAT + P2I)/PI__NUM*(HEB_IN%DIMS(2)-1) ) + 1
      IF ( IND_LAT < 1              ) IND_LAT = 1
      IF ( IND_LAT > HEB_IN%DIMS(2) ) IND_LAT = HEB_IN%DIMS(2)
!
      IND_LON= NINT ( LON/PI2*HEB_IN%DIMS(1) ) + 1
      IF ( IND_LON < 1              ) IND_LON = 1
      IF ( IND_LON > HEB_IN%DIMS(1) ) IND_LON = HEB_IN%DIMS(1)
      HEB_OUT = HEB_IN
      ALLOCATE ( HEB_OUT%VAL1(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)) )
      HEB_OUT%VAL1 = 0
!
      IF ( IND_LAT == 1 ) THEN
           HEB_OUT%VAL1(1:HEB_OUT%DIMS(1),1,1,1) = 1
           HEB_OUT%VAL1(1:HEB_OUT%DIMS(1),2,1,1) = 1
           IND_LAT = 2
      END IF
      write ( 6, * ) 'lon = ', lon, ' lat= ', lat ! %%%
      write ( 6, * ) 'ind_lon = ', ind_lon, ' ind_lat= ', ind_lat ! %%%
      CALL UPDATE_CONT_MASK ( IND_LON, IND_LAT, HEB_IN, HEB_OUT )
!
      HEB_OUT%SDS_NAME = 'Continent mask with center lat/lon '// &
     &                    TRIM(LAT_STR)//' '//TRIM(LON_STR)//' deg'
      HEB_OUT%UNITS = 'dimensionless'
      HEB_OUT%VALID_RANGE(1) = 0
      HEB_OUT%VALID_RANGE(2) = 1
      HEB_OUT%MIN_VALUE      = 0
      HEB_OUT%MAX_VALUE      = 1
      HEB_OUT%DATA_FORMAT = HEB__I1
      HEB_OUT%COMMENT(1) = '1 -- land; 0 -- sea'
!
      LAND_CELLS = 0
      DO 410 J1=1,HEB_OUT%DIMS(2)
         DO 420 J2=1,HEB_OUT%DIMS(1)
            IF ( HEB_OUT%VAL1(J2,J1,1,1) == 1 ) LAND_CELLS = LAND_CELLS + 1
 420     CONTINUE 
 410  CONTINUE 
      WRITE ( UNIT=HEB_OUT%COMMENT(2), FMT=120 ) LAND_CELLS
 120  FORMAT ( 'The total number of land cells: ', I15 )
!
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL1, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, '(A)' ) 'Falure in an attempt to write into '// &
     &                        'the output file '//TRIM(FILOUT)
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Written the output file '//TRIM(FILOUT)
!
      END  PROGRAM  GEN_CONT_MASK  !#!  
!
! ------------------------------------------------------------------------
!
      RECURSIVE SUBROUTINE UPDATE_CONT_MASK ( IND_LON, IND_LAT, HEB_IN, HEB_OUT )
! ************************************************************************
! *                                                                      *
! *   Routine UPDATE_CONT_MASK
! *                                                                      *
! * ### 03-DEC-2017 UPDATE_CONT_MASK v1.0 (c) L. Petrov  03-DEC-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      INTEGER*4  IND_LON, IND_LAT
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_OUT
      INTEGER*4  J1, J2, I_LON, I_LAT
      REAL*8     EPS
      PARAMETER  ( EPS = 0.01D0 )
      DO 410 J1=-1,1
         I_LON = IND_LON + J1
         IF ( I_LON < 1              ) I_LON = I_LON + HEB_IN%DIMS(1)
         IF ( I_LON > HEB_IN%DIMS(1) ) I_LON = I_LON - HEB_IN%DIMS(1)
         DO 420 J2=-1,1
            I_LAT = IND_LAT + J2
            IF ( HEB_IN%VAL(I_LON,I_LAT,1,1) > EPS .AND. HEB_OUT%VAL1(I_LON,I_LAT,1,1) == 0 ) THEN
                 HEB_OUT%VAL1(I_LON,I_LAT,1,1) = 1
                 CALL UPDATE_CONT_MASK ( I_LON, I_LAT, HEB_IN, HEB_OUT )
            END IF
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  UPDATE_CONT_MASK  !#!  
