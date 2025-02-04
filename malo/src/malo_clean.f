      SUBROUTINE MALO_CLEAN_SH ( NLON, NLAT, MD, ND, LS, SPH, SPR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_CLEAN_SH
! *                                                                      *
! * ### 25-MAR-2014  MALO_CLEAN_SH   v1.1 (c) L. Petrov 17-AUG-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INTEGER*4  NLON, NLAT, MD, ND, IUER
      REAL*4     LS(NLON,NLAT)
      REAL*8     SPH(2,0:MD,0:MD), SPR(NLON,NLAT)
      REAL*8     LAT, LON, DP, DIST, WEI, SUM_PAR, SUM_WEI
      REAL*8,    ALLOCATABLE :: VEC(:,:,:), SPH_ORIG(:,:,:)
      REAL*8     MIN_VAL, EPS_LS
      CHARACTER  TIM_STR1*28, TIM_STR2*28, TIM_STR3*28, TIM_STR4*28, &
     &           TIM_STR5*28, STR*128
      PARAMETER  ( MIN_VAL = 1.0E-30 )
      PARAMETER  ( EPS_LS  = 0.001   )
      LOGICAL*1  FL_OCEAN, FL_LAND, FL_MIXED, FL_TIMING, FL_POLE
      INTEGER*8  FSH
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, JLON, JLAT, IER
      REAL*8,    EXTERNAL :: DP_VV_V
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: SPHE_INIT 
!
      FL_TIMING = .FALSE. 
!
! --- Allocate memory for temporary arrays VEC and SPH_ORIG
!
      ALLOCATE ( VEC(3,NLON,NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*3*NLON*NLAT, STR )
           CALL ERR_LOG ( 6621, IUER, 'MALO_CLEAN_SH', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array VEC' )
           RETURN
      END IF   
!
      ALLOCATE ( SPH_ORIG(2,0:MD,0:MD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*2*(MD+1)**2, STR )
           CALL ERR_LOG ( 6622, IUER, 'MALO_CLEAN_SH', 'Error '// &
     &         'in an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of memory for array SPH_ORIG' )
           RETURN
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
!
! --- Initialize spherical harmonic transform
!
      CALL ERR_PASS ( IUER, IER )
      FSH = SPHE_INIT ( -1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6623, IUER, 'MALO_CLEAN_SH', 'Error '// &
     &         'in an attempt to initialize FSH object for '// &
     &         'spherical harmonics transform'  )
           RETURN
      END IF   
!
! --- Perfrom inverse spherical harmonics trasnform
!
      IF ( NLAT == NLON/2 + 1 ) THEN
           FL_POLE = .TRUE.
         ELSE
           FL_POLE = .FALSE.
      END IF
      SPH_ORIG = SPH
!
      CALL ERR_PASS ( IUER, IER )
      IF ( FL_TIMING ) CALL CPU_TIMER ( %VAL(0) )
      IF ( FL_POLE ) THEN
           CALL SPHE_INV_2NN ( %VAL(FSH), MD, MD, 1, 1, SPH, NLAT-1, SPR, IER )
           SPR(1:NLON,NLAT) = SPR(1:NLON,NLAT-1) 
         ELSE
           CALL SPHE_INV_2NN ( %VAL(FSH), MD, MD, 1, 1, SPH, NLAT, SPR, IER )
      END IF
      IF ( FL_TIMING ) CALL CPU_TIMER ( TIM_STR1 )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6624, IUER, 'MALO_CLEAN_SH', 'Error '// &
     &            'in an attempt to perfrom inverse spherical '// &
     &            'harmonic transform' )
           CALL EXIT ( 1 )
      END IF   
!
! --- Run exponential smoothing the field over ocean
!
      CALL ERR_PASS  ( IUER, IER )
      IF ( FL_TIMING ) CALL CPU_TIMER ( %VAL(0) )
      CALL MALO_CLEAN_EXP ( NLON, NLAT, ND, LS, VEC, SPR, IER )
      IF ( FL_TIMING ) CALL CPU_TIMER ( TIM_STR2 )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6625, IUER, 'MALO_CLEAN_SH', 'Error '// &
     &         'in an attempt to perfrom the 1st exponential smoothing' )
           RETURN 
      END IF   
      IF ( .NOT. ( MD == 2699 .AND. ND == 100 ) ) THEN
!
! -------- Direct spherical harmonics transform of the smoothed field
!
           CALL ERR_PASS ( IUER, IER ) 
           IF ( FL_TIMING ) CALL CPU_TIMER ( %VAL(0) )
           IF ( FL_POLE ) THEN
                CALL SPHE_DIR_2NN ( %VAL(FSH), NLAT-1, SPR, MD, MD, 1, 1, SPH, IER )
              ELSE 
                CALL SPHE_DIR_2NN ( %VAL(FSH), NLAT, SPR, MD, MD, 1, 1, SPH, IER )
           END IF
           IF ( FL_TIMING ) CALL CPU_TIMER ( TIM_STR3 )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6626, IUER, 'MALO_CLEAN_SH', 'Error '// &
     &              'in an attempt to perfrom inverse spherical '// &
     &              'harmonic transform' )
                RETURN 
           END IF   
!
! -------- Replace low degree spherical harmonics with original ones
!
           DO 430 J3=0,ND
              DO 440 J4=0,ND
                 SPH(1:2,J3,J4) = SPH_ORIG(1:2,J3,J4) 
 440          CONTINUE 
 430       CONTINUE 
!
! -------- Again run inverse spherical harmonics transform
!
           CALL ERR_PASS ( IUER, IER )
           IF ( FL_TIMING ) CALL CPU_TIMER ( %VAL(0) )
           IF ( FL_POLE ) THEN
                CALL SPHE_INV_2NN ( %VAL(FSH), MD, MD, 1, 1, SPH, NLAT-1, SPR, IER )
              ELSE 
                CALL SPHE_INV_2NN ( %VAL(FSH), MD, MD, 1, 1, SPH, NLAT, SPR, IER )
           END IF
           IF ( FL_TIMING ) CALL CPU_TIMER ( TIM_STR4 )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6627, IUER, 'MALO_CLEAN_SH', 'Error '// &
     &                 'in an attempt to perfrom inverse spherical '// &
     &                 'harmonic transform' )
                RETURN 
           END IF   
!
! -------- ... and again run exponential smoothing
!
           CALL ERR_PASS  ( IUER, IER ) 
           IF ( FL_TIMING ) CALL CPU_TIMER ( %VAL(0) )
           CALL MALO_CLEAN_EXP ( NLON, NLAT, ND, LS, VEC, SPR, IER )
           IF ( FL_TIMING ) CALL CPU_TIMER ( TIM_STR5 )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6628, IUER, 'MALO_CLEAN_SH', 'Error '// &
     &              'in an attempt to perfrom the 1st exponential smoothing' )
                RETURN 
           END IF   
      END IF   
!
      DEALLOCATE ( SPH_ORIG )
      DEALLOCATE ( VEC )
      IF ( FL_TIMING ) THEN
           WRITE ( 6, * ) 'NLON= ', NLON, ' NLAT= ', NLAT
           WRITE ( 6, * ) 'Tim1: ', TIM_STR1 
           WRITE ( 6, * ) 'Tim2: ', TIM_STR2
           WRITE ( 6, * ) 'Tim3: ', TIM_STR3 
           WRITE ( 6, * ) 'Tim4: ', TIM_STR4 
           WRITE ( 6, * ) 'Tim5: ', TIM_STR5 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_CLEAN_SH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_CLEAN_EXP ( NLON, NLAT, ND, LS, VEC, SPR, IUER )
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
           MI  = 160
           MS  =  40
      END IF
      DP  = 1.0*PI__NUM/(2.0*ND)
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
            IF ( FL_OCEAN .OR. FL_MIXED ) THEN
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
                           IF ( LS(JLON,JLAT) < EPS_LS ) THEN
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
      END  SUBROUTINE MALO_CLEAN_EXP  !#!#
