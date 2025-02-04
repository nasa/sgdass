      PROGRAM    HEI_GD_TO_ELL
! ************************************************************************
! *                                                                      *
! *   Program  HEI_GD_TO_ELL
! *                                                                      *
! *  ### 06-AUG-2015  HEI_GD_TO_ELL  v1.0 (c) L. Petrov  06-AUG-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_GEOID_BSPL, HEB_OUT
      INTEGER*4  J1, J2, IB, IE, IUER
      REAL*8     LAT_GDT, LON
      CHARACTER  FIL_IN*128, FIL_GEOID_BSPL*128, FIL_OUT*128, STR*128
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      REAL*8,    EXTERNAL :: GET_GEOID
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: hei_gd_to_ell  fil_geoid bspl_geoid_file fil_ellipsoid'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_IN )
           CALL GETARG ( 2, FIL_GEOID_BSPL )
           CALL GETARG ( 3, FIL_OUT )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FIL_IN, HEB_IN, IUER )
      IF ( IUER  .NE. 0 ) THEN
           CALL GERROR  ( STR )
           IUER = -1
           CALL ERR_LOG ( 4501, IUER, 'HEI_GD_TO_ELL', 'Error in reading '// &
     &         'input heb-file '//FIL_IN )
           CALL EXIT ( 1 )
      END IF
!
      HEB_OUT = HEB_IN
      ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*HEB_OUT%DIMS(1)*HEB_OUT%DIMS(2)*HEB_OUT%DIMS(3)*HEB_OUT%DIMS(4), STR )
           CALL ERR_LOG ( 4502, IUER, 'HEI_GD_TO_ELL', 'Error in allocation '// &
     &         'of '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,HEB_OUT%DIMS(2)
         LAT_GDT = -P2I + (J1-1)*PI__NUM/(HEB_OUT%DIMS(2)-1)
         DO 420 J2=1,HEB_OUT%DIMS(1)
            LON = -PI__NUM + (J2-1)*PI2/HEB_OUT%DIMS(1)
            IF ( LON < 0.0 ) LON = LON + PI2
            IUER  = -1
            HEB_OUT%VAL(J2,J1,1,1) = HEB_IN%VAL(J2,J1,1,1) + &
     &                               GET_GEOID ( LAT_GDT, LON, FIL_GEOID_BSPL, &
     &                                           HEB_GEOID_BSPL, IUER )
            IF ( IUER  .NE. 0 ) THEN
                 CALL GERROR  ( STR )
                 IUER = -1
                 CALL ERR_LOG ( 4503, IUER, 'HEI_GD_TO_ELL', 'Error in '// &
     &               'computing geoid height for this point' )
                 CALL EXIT ( 1 )
            END IF
 420     CONTINUE 
 410  CONTINUE 
      IB = INDEX ( HEB_OUT%SDS_NAME, 'geoid' )
      IF ( IB > 0 ) THEN
           IE = IB + LEN('reference ellipsoid' )
           HEB_OUT%SDS_NAME = HEB_OUT%SDS_NAME(1:IB-1)// &
     &                        'reference ellipsoid'//HEB_OUT%SDS_NAME(IE:)
      END IF
!
      IUER = -1
      CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL, FIL_OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4504, IUER, 'HEI_GD_TO_ELL', 'Failure '// &
     &         'in an attempt to write into output file '//FIL_OUT )
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM   HEI_GD_TO_ELL  !#!  
