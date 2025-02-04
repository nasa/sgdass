      PROGRAM    HEB_SMOOTH
! ************************************************************************
! *                                                                      *
! *   Program HEB_SMOOTH appliees smoothing to spatial data.
! *                                                                      *
! *  ### 23-MAR-2018   HEB_SMOOTH  v1.0 (c)  L. Petrov  23-MAR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_IN, HEB_OUT
      REAL*8     SM_RAD
      CHARACTER  FIL_IN*128, FIL_OUT*128, STR*128
      REAL*8     LON, LAT
      REAL*8,    ALLOCATABLE :: VEC(:,:,:), VAL8(:,:)
      REAL*4,    ALLOCATABLE :: LS(:,:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, ND, NLON, NLAT, KL, KW, NTHR, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      NTHR = 16
      IF ( IARGC () < 3 ) THEN
           WRITE ( 6, * ) 'Usage heb_in heb_out smoothing_radius'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FIL_IN )
           CALL GETARG ( 2, FIL_OUT )
           CALL GETARG ( 3, STR )
           READ ( UNIT=STR, FMT=* ) SM_RAD
      END IF
      ND = 1.D7/SM_RAD
      write ( 6, * ) 'ND= ', ND
!
      IUER = -1
      CALL READ_HEB ( FIL_IN, HEB_IN, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 3801, IUER, 'HEB_SMOOTH', 'Error '// &
     &         'in an attempt to read inpt heb-file '//FIL_IN )
           CALL EXIT ( 1 )
      END IF
      NLON = HEB_IN%DIMS(1)
      NLAT = HEB_IN%DIMS(2)
!
! --- Allocate memory for temporary arrays VEC, VAL8, and LS
!
      ALLOCATE ( VEC(3,NLON,NLAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*3*NLON*NLAT, STR )
           CALL ERR_LOG ( 3802, IUER, 'HEB_SMOOTH', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array VEC' )
           CALL EXIT ( 1 ) 
      END IF   
!
      ALLOCATE ( VAL8(NLON,NLAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NLON*NLAT, STR )
           CALL ERR_LOG ( 3803, IUER, 'HEB_SMOOTH', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array VAL8' )
           CALL EXIT ( 1 ) 
      END IF   
!
      ALLOCATE ( LS(NLON,NLAT), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*NLON*NLAT, STR )
           CALL ERR_LOG ( 3804, IUER, 'HEB_SMOOTH', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array LS' )
           CALL EXIT ( 1 ) 
      END IF   
      HEB_OUT = HEB_IN
      IF ( ASSOCIATED ( HEB_IN%VAL ) ) THEN
           ALLOCATE ( HEB_OUT%VAL(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(4)*HEB_IN%DIMS(1)*HEB_IN%DIMS(2)*HEB_IN%DIMS(3)*HEB_IN%DIMS(4), STR )
                CALL ERR_LOG ( 3805, IUER, 'HEB_SMOOTH', 'Error '// &
     &              'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of memory for array HEB_OUT%VAL' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
      IF ( ASSOCIATED ( HEB_IN%VAL8 ) ) THEN
           ALLOCATE ( HEB_OUT%VAL8(HEB_OUT%DIMS(1),HEB_OUT%DIMS(2),HEB_OUT%DIMS(3),HEB_OUT%DIMS(4)), STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(8)*HEB_IN%DIMS(1)*HEB_IN%DIMS(2)*HEB_IN%DIMS(3)*HEB_IN%DIMS(4), STR )
                CALL ERR_LOG ( 3806, IUER, 'HEB_SMOOTH', 'Error '// &
     &              'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of memory for array HEB_OUT%VAL' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
!
! --- Compute cartezian coordinates of grid points.
! --- This is done for speeding up computations
!
      DO 410 J1=1,NLAT
         LAT = -P2I + (J1-1)*PI__NUM/NLAT
         DO 420 J2=1,NLON
            LON = (J2-1)*PI2/NLON
            VEC(1,J2,J1) = DCOS(LAT)*DCOS(LON)
            VEC(2,J2,J1) = DCOS(LAT)*DSIN(LON)
            VEC(3,J2,J1) = DSIN(LAT)
 420     CONTINUE 
 410  CONTINUE 
    write ( 6, * ) 'heb_smooth 111' ; call flush ( 6 ) ! %%%%
!
      DO 430 J3=1,HEB_IN%DIMS(4)
!$OMP    PARALLEL DO IF ( NTHR > 1 ) &
!$OMP    PRIVATE ( J4, J5, J6, J7, J8, VAL8, LS, KW, KL, IUER ) SCHEDULE ( STATIC, 1 )
         DO 440 J4=1,HEB_IN%DIMS(3)
            KL = 0
            KW = 0
            DO 450 J5=1,NLAT
               DO 460 J6=1,NLON
                  IF ( ASSOCIATED ( HEB_IN%VAL ) ) THEN
                       IF ( ABS(HEB_IN%VAL(J6,J5,J4,J3)) < HEB_IN%FILL_VALUE/2.0 ) THEN
                            VAL8(J6,J5) = HEB_IN%VAL(J6,J5,J4,J3)
                            LS(J6,J5) = 1.0
                            KL = KL + 1
                          ELSE
                            LS(J6,J5) = 0.0
                            KW = KW + 1
                       END IF
                    ELSE IF ( ASSOCIATED ( HEB_IN%VAL8 ) ) THEN
                       IF ( DABS(HEB_IN%VAL8(J6,J5,J4,J3)) < HEB_IN%FILL_VALUE/2.0 ) THEN
                            VAL8(J6,J5) = HEB_IN%VAL8(J6,J5,J4,J3)
                            LS(J6,J5) = 1.0
                          ELSE
                            LS(J6,J5) = 0.0
                       END IF
                  END IF
 460           CONTINUE 
 450        CONTINUE 
!
! --------- Run exponential smoothing
!
!!    write ( 6, * ) 'heb_smooth 137 ', NLON, NLAT, ND, ' KL= ', KL, ' KW= ', KW ; call flush ( 6 ) ! %%%%
            IUER = -1
            CALL MALO_CLEAN_EXP_AA ( NLON, NLAT, ND, LS, VEC, VAL8, IUER )
            IF ( IUER .NE. 0 ) THEN
                 CALL ERR_LOG ( 3807, IUER, 'HEB_SMOOTH', 'Error '// &
     &                'in an attempt to perform exponential smooting' )
                 CALL EXIT ( 1 )
            END IF 
!!    write ( 6, * ) 'heb_smooth 145' ; call flush ( 6 ) ! %%%%
            DO 470 J7=1,NLAT
               DO 480 J8=1,NLON
                  IF ( ASSOCIATED ( HEB_OUT%VAL ) ) THEN
                       IF ( LS(J8,J7) == 1.0 ) THEN
                            HEB_OUT%VAL(J8,J7,J4,J3)  = VAL8(J8,J7) 
                          ELSE
                            HEB_OUT%VAL(J8,J7,J4,J3)  = HEB_OUT%FILL_VALUE
                       END IF
                  END IF
                  IF ( ASSOCIATED ( HEB_OUT%VAL8 ) ) THEN
                       IF ( LS(J8,J7) == 1.0 ) THEN
                            HEB_OUT%VAL8(J8,J7,J4,J3) = VAL8(J8,J7) 
                          ELSE
                            HEB_OUT%VAL8(J8,J7,J4,J3) = HEB_OUT%FILL_VALUE
                       END IF
                  END IF
 480           CONTINUE 
 470        CONTINUE 
!!          if ( j4 == 4 ) goto 830
 440     CONTINUE 
!$OMP END PARALLEL DO
 430  CONTINUE 
 830  CONTINUE 
!
! --- Write down the output dataset
!
      IUER = -1
      IF ( ASSOCIATED ( HEB_OUT%VAL ) ) THEN
           CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL,  FIL_OUT, IUER )
         ELSE 
           CALL WRITE_HEB ( HEB_OUT, HEB_OUT%VAL8, FIL_OUT, IUER )
      END IF
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 3807, IUER, 'HEB_SMOOTH', 'Error '// &
     &         'in an attempt to write in the output file '//FIL_OUT )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Smoothed field is written in '//TRIM(FIL_OUT)
!
      END  PROGRAM  HEB_SMOOTH  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_CLEAN_EXP_AA ( NLON, NLAT, ND, LS, VEC, SPR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_CLEAN_EXP performs smoothing by convolving the        *
! *   input surface pressure field over the ocean with an exponenial     *
! *   function that decades with a distance.                             *
! *                                                                      *
! * ### 25-MAR-2014   MALO_CLEAN_EXP  v2.1 (c) L. Petrov 17-APR-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  NLON, NLAT, MD, ND, IUER
      REAL*4     LS(NLON,NLAT)
      REAL*8     SPR(NLON,NLAT), VEC(3,NLON,NLAT)
      REAL*8     LAT, LON, DP, DIST, DIST_SQ, DIST_MAX, WEI, &
     &           SUM_PAR, SUM_WEI, DAC, COEF, ATT, FUDGE
      INTEGER*4  ME
      REAL*8     EPS_LS, MIN_VAL, EXPS
      PARAMETER  ( EPS_LS  = 0.001    )
      PARAMETER  ( MIN_VAL = 1.0E-10  )
      PARAMETER  ( EXPS    = 1024.0D0 )
      PARAMETER  ( ME      = 4095     )
      PARAMETER  ( FUDGE   = 1.1D0    )
      CHARACTER  STR*128
      LOGICAL*1  FL_OCEAN, FL_LAND, FL_MIXED 
      REAL*8,    ALLOCATABLE :: PRS(:,:), EXPA(:)
      INTEGER*4  J1, J2, J3, J4, J5, ID, MI, MS, JLON, JLAT, JSH_LON, IER
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: SPHE_INIT 
!
! --- Smoothing parameters:
! --- MI -- number of grid points to be used in covolution
! --- MS -- stride in convolution
! --- DP -- a coefficients at the exponent used for smoothing
!
      ATT = 0.1
      IF ( NLAT < 600 ) THEN
           MI  = 5
           MS  = 1
         ELSE IF ( (NLAT-1)/2 < 8 ) THEN
           MI = 20 
           MS =  4
         ELSE 
           MI  = 320
           MS  =   4
      END IF
      DP  = 1.0*PI__NUM/(2.0*ND)
!   write ( 6, * )  'malo_clean-235 ', mi, ms, dp ; call flush ( 6 ) ! %%%
!
      ALLOCATE ( PRS(NLON,NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*NLON*NLAT, STR )
           CALL ERR_LOG ( 6641, IUER, 'MALO_CLEAN_EXP', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array PRS' )
           RETURN
      END IF   
!
      ALLOCATE ( EXPA(0:ME), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*(ME+1), STR )
           CALL ERR_LOG ( 6642, IUER, 'MALO_CLEAN_EXP', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array EXPA' )
           RETURN
      END IF   
!
! --- Prepare the look-up table for exponents.
!
      DIST_MAX = FUDGE * (PI__NUM/NLAT) *MI*DSQRT(2.0D0)
      COEF = ME/DIST_MAX**2
      DP = ATT*DIST_MAX
      DO 410 J1=0,ME
         EXPA(J1) = DEXP( -SQRT(J1/COEF)/DP )
 410  CONTINUE 
!
      PRS = 0.0D0
      DO 420 J2=1,NLAT
         LAT = -P2I + (J2-1)*PI__NUM/NLAT
         DO 430 J3=1,NLON
            LON = (J3-1)*PI2/NLON
!
! --------- Check whether a given cell is a land or ocean
!
            FL_OCEAN = .FALSE.
            FL_LAND  = .FALSE.
            FL_MIXED = .FALSE.
            IF ( LS(J3,J2) < EPS_LS ) THEN
                 FL_OCEAN = .TRUE.
               ELSE IF ( LS(J3,J2) > 1.0 - EPS_LS ) THEN
                 FL_LAND  = .TRUE.
               ELSE 
                 FL_MIXED = .TRUE.
            END IF
            SUM_PAR = 0.0D0
            SUM_WEI = 0.0D0
            IF ( FL_LAND .OR. FL_MIXED ) THEN
                 DO 440 J4=-MI,MI,MS
                    JLAT = J2 + J4
                    JSH_LON = 0
                    IF ( JLAT > NLAT ) THEN
                         JLAT = 2*NLAT - JLAT
                         JSH_LON = NLON/2
                    END IF 
                    IF ( JLAT < 1    ) THEN
                         JLAT = 1 - JLAT
                         JSH_LON = NLON/2
                    END IF
                    DO 450 J5=-MI,MI,MS
                       IF ( J5 == 0 .AND. J4 == 0 ) THEN
                            SUM_PAR = SUM_PAR + SPR(J3,J2)
                            SUM_WEI = SUM_WEI + 1.0D0
                         ELSE
                           JLON = J3 + J5 + JSH_LON
                           IF ( JLON < 1 ) JLON = JLON + NLON
                           IF ( JLON > NLON ) JLON = JLON - NLON
                           IF ( LS(JLON,JLAT) > EPS_LS ) THEN
! 
! ----------------------------- Compute a distance between JLON/JLAT and J3/J2 points
! ----------------------------- A straitforward solution would be
! ----------------------------- WEI = DEXP ( -DACOS ( DSQRT ( DP_VV_V ( 3, VEC(1,JLON,JLAT), VEC(1,J3,J2) ) ) )/DP )
! ----------------------------- but we can speed up computation several times if we use
! ----------------------------- an approximation of acos(1-x) = sqrt ( 2*(1-x) )
!
!!                                dac = dp_vv_v ( 3, vec(1,jlon,jlat), vec(1,j3,j2) )
!
                                DAC = VEC(1,JLON,JLAT)*VEC(1,J3,J2) + &
     &                                VEC(2,JLON,JLAT)*VEC(2,J3,J2) + &
     &                                VEC(3,JLON,JLAT)*VEC(3,J3,J2)
                                DIST_SQ = 2.0D0*(1.0D0 - DAC)
!
! ----------------------------- Another trick: computation of exponent is replaced
! ----------------------------- with a lookup table. The following two lines are 
! ----------------------------- equivalent to WEI  = EXP(-DIST/DP)
!
                                ID = IDNINT(DIST_SQ*COEF)
                                WEI  = EXPA(ID)
!
!! wei = dexp ( -dacos ( dsqrt ( dp_vv_v ( 3, vec(1,jlon,jlat), vec(1,j3,j2) ) ) )/dp )
!
                                SUM_PAR = SUM_PAR + WEI*SPR(JLON,JLAT)
                                SUM_WEI = SUM_WEI + WEI
                           END IF
                       END IF
 450                CONTINUE 
 440             CONTINUE 
!
                 IF ( SUM_WEI > MIN_VAL ) THEN
                      PRS(J3,J2) = SUM_PAR/SUM_WEI
                 END IF
            END IF
 430     CONTINUE 
 420  CONTINUE 
!
! --- Replace SPR array with new pressure field
!
      SPR = PRS
!
      DEALLOCATE   ( EXPA )
      DEALLOCATE   ( PRS )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE MALO_CLEAN_EXP_AA  !#!#
