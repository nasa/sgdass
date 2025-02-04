      PROGRAM    MASK_DIFF
! ************************************************************************
! *                                                                      *
! *   Program MASK_DIFF
! *                                                                      *
! *  ### 21-JAN-2016   MASK_DIFF   v1.0 (c)  L. Petrov  21-JAN-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      INCLUDE   'malo_local.i'
      TYPE     ( HEB__TYPE  ) :: HEB_FULL, HEB_MASK, HEB_OUT
      REAL*8     LAT, LON
      CHARACTER  MODE_STR*128, FILE_FULL*128, FILE_MASK*128, FILE_OUT*128, STR*128
      INTEGER*4  J1, J2, J3, J4, J5, ILON, ILAT, NTHR, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
!      FILE_FULL = '/s0/mod44w/mod44w_lls.heb'
!      FILE_MASK = '/s0/mod44w/mod44w_ls_blackman_d2699.heb'
!      FILE_OUT  = '/s0/mod44w/mask_diff_d2699.heb'
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, * ) 'Usage: mask_diff mode full_mask small_mask out_file'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, MODE_STR  )
           CALL GETARG ( 2, FILE_FULL )
           CALL GETARG ( 3, FILE_MASK )
           CALL GETARG ( 4, FILE_OUT  )
      END IF
      CALL GETENVAR ( 'OMP_NUM_THREADS', STR )
      IF ( ILEN(STR) == 0 ) THEN
           NTHR = 1
         ELSE
           CALL CHIN ( STR, NTHR )
      END IF
      CALL OMP_SET_NUM_THREADS ( %VAL(NTHR) )
!
      IF ( MODE_STR == 'land/sea' ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 3401, IUER, 'MASK_DIFF', 'Unsupported mode '// &
     &          MODE_STR(1:I_LEN(MODE_STR))//'. Supported modes: '// &
     &         'land/sea' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILE_FULL, HEB_FULL, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 3402, IUER, 'MASK_DIFF', 'Error '// &
     &         'in an attempt to read heb-file with land-water '// &
     &         'mask '//FILE_FULL )
           CALL EXIT ( 1 )
      END IF   
!
      IUER = -1
      CALL READ_HEB ( FILE_MASK, HEB_MASK, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 3403, IUER, 'MASK_DIFF', 'Error '// &
     &         'in an attempt to read heb-file with land-water '// &
     &         'mask '//FILE_MASK )
           CALL EXIT ( 1 )
      END IF   
      HEB_OUT = HEB_MASK
      HEB_OUT%DIMS = HEB_FULL%DIMS
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL IINCH8 ( INT8(4)*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2), STR )
           IUER = -2
           CALL ERR_LOG ( 3404, IUER, 'MASK_DIFF', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         'bytes of dynamic memory for arrah HEB_OUT%VAL' )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,HEB_FULL%DIMS(2)
         LAT = -P2I + (J1-1)*PI__NUM/(HEB_FULL%DIMS(2)-1)
         ILAT = IDNINT ( (HEB_MASK%DIMS(2)-1)*(LAT + P2I)/PI__NUM ) + 1
!!         write ( 6, * ) 'j1= ', j1, ' lat= ', sngl(lat), ' ilat= ', ilat ! %%%%%
!$OMP    PARALLEL DO IF ( NTHR > 1 ) PRIVATE ( J2, LON, ILON ), SCHEDULE ( STATIC )
         DO 420 J2=1,HEB_FULL%DIMS(1)
            LON = (J2-1)*PI2/HEB_FULL%DIMS(1)
            ILON = IDNINT ( HEB_MASK%DIMS(1)*LON/PI2 ) + 1
            IF ( ILON > HEB_MASK%DIMS(1) ) ILON = 1
!!           write ( 6, * ) 'j2= ', j2, ' lon= ', sngl(lon), ' ilon= ', ilon ! %%%%%
            IF ( MODE_STR == 'land/sea' ) THEN
                 IF ( HEB_FULL%VAL1(J2,J1,1,1) .EQ. MALO__SEA_VAL ) THEN
                      HEB_FULL%VAL1(J2,J1,1,1) = MALO__WATER_VAL 
                    ELSE ! land or lake
                      HEB_FULL%VAL1(J2,J1,1,1) = MALO__LAND_VAL
                 END IF
            END IF
            HEB_OUT%VAL(J2,J1,1,1) = HEB_FULL%VAL1(J2,J1,1,1) - HEB_MASK%VAL(ILON,ILAT,1,1)
 420     CONTINUE 
!$OMP    END PARALLEL DO
 410  CONTINUE 
      HEB_OUT%SDS_NAME = 'Differences of '//HEB_OUT%SDS_NAME 
      HEB_OUT%TITLE    = HEB_OUT%SDS_NAME
      CALL HEB_MINMAX ( HEB_OUT, HEB_OUT%VAL, 1.0E8 )
      HEB_OUT%VALID_RANGE(1) = HEB_OUT%MIN_VALUE        
      HEB_OUT%VALID_RANGE(2) = HEB_OUT%MAX_VALUE        
!
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FILE_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 3405, IUER, 'MASK_DIFF', 'Failure in writing '// &
     &         'into the output file '//FILE_OUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Output file: '//FILE_OUT(1:I_LEN(FILE_OUT))
      CALL FLUSH ( 6 ) 

      END  PROGRAM   MASK_DIFF  !#!  
