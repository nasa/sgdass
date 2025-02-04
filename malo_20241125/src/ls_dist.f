      PROGRAM    LS_DIST 
! ************************************************************************
! *                                                                      *
! *   Program  LS_DIST computes the map of distances to the ocean.       *
! *   The distance is measured in cell size along latitude.              *
! *   If a given cell is totally ocean or the center of the cell is in   *
! *   the ocean, the cell is considered as sea, and the distance is set  *
! *   to -1. If a cell has distance to the sea greater than some limit   *
! *   (8 cells), the fill value 0 is put.                                *
! *                                                                      *
! *   Usage: ls_dist  heb_lls  dim  md  heb_ls_dist                      *
! *                                                                      *
! *   where     heb_lls is the land/lake/sea mask derived from MOD44W by *
! *                     program lw_transform.                            *
! *                                                                      *
! *                 md  maximim distance along longitude or latitude     *
! *                     in cells.                                        *
! *                                                                      *
! *                 dim dimension. Should be an integer fraction of the  *
! *                     land/lake/sea mask.                              *
! *                                                                      *
! *         heb_ls_dist name of the output file in HEB format.           *
! *                                                                      *
! *  ### 11-FEB-2016    LS_DIST    v4.0 (c)  L. Petrov  17-APR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE  
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      INCLUDE   'malo.i'
      TYPE     ( HEB__TYPE ) :: HEB_LS, HEB_DLS
      REAL*8       LAT_MIN, LAT_MAX
      PARAMETER  ( LAT_MIN  = -85.0D0*DEG__TO__RAD )
      PARAMETER  ( LAT_MAX  =  85.0D0*DEG__TO__RAD )
      INTEGER*4    MAX_LON, MAX_LAT
      PARAMETER  ( MAX_LON = 172800 )
      PARAMETER  ( MAX_LAT = 86401  )
      REAL*8       EPS
      PARAMETER  ( EPS = 1.0D-14 )
      REAL*4       EPS_LS
      PARAMETER  ( EPS_LS = 1.0E-5 )
      CHARACTER  FIL_LS*128, FIL_DLS*128, STR*128, STR1*128, MODE_STR*8, &
     &           LS_DIST__LABEL*32
      PARAMETER  ( LS_DIST__LABEL = 'ls_dist 4.0   Vers of 2016.04.17' )
      LOGICAL*1  FL_OCEAN
      INTEGER*2, ALLOCATABLE :: LS_INIT(:,:)
      INTEGER*4, ALLOCATABLE :: LS_PREM(:,:)
      REAL*8     TIM_R8
      REAL*8     CLA_LS(MAX_LAT), SLA_LS(MAX_LAT), CLO_LS(MAX_LON), SLO_LS(MAX_LON), &
     &           DAC, DIST_SQ, VEC_DLS(3), VEC_LS(3)
      REAL*8     LAT, LON, DIST
      LOGICAL*1  FL_LS
      INTEGER*4  NLON, NLAT, ILAT, ILON, JLAT, JLON, MD, J1, J2, J3, J4, J5, J6, &
     &           J7, J8, J9, J10, J11, J12, J13, J14, KLON, KLAT, &
     &           NLON_LS, NLAT_LS, NC, DIM, IR, ML, MODE, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      REAL*8,    EXTERNAL :: WALL_TIMER 
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, * ) 'Usage: ls_dist heb_lls dim md sea/land heb_ls_dist'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FIL_LS   )
           CALL GETARG ( 2, STR      )
           CALL CHIN   ( STR, DIM    )
           CALL GETARG ( 3, STR1     )
           CALL CHIN   ( STR1, MD    )
           CALL GETARG ( 4, MODE_STR )
           CALL GETARG ( 5, FIL_DLS  )
      END IF
!
      IF ( MODE_STR == 'sea' ) THEN
           FL_LS = .FALSE.
           MODE = MALO__SEA_VAL
        ELSE IF ( MODE_STR == 'land' ) THEN
           FL_LS = .FALSE.
           MODE = MALO__LAND_VAL
        ELSE IF ( MODE_STR == 'sea0' ) THEN
           FL_LS = .TRUE.
           MODE = MALO__SEA_VAL
        ELSE IF ( MODE_STR == 'land0' ) THEN
           FL_LS = .TRUE.
           MODE = MALO__LAND_VAL
        ELSE 
           IUER = -1
           CALL ERR_LOG ( 7401, IUER, 'LS_DIST', 'Unsupported 4th argument '// &
     &          MODE_STR(1:I_LEN(MODE_STR))//'.  Sea or land are supported.' )
           CALL EXIT ( 1 )
      END IF
!
! --- Read the land-sea mask
!
      WRITE ( 6, '(A,I4)' ) 'Started reading the data         ' ; CALL FLUSH ( 6 ) 
      TIM_R8 = WALL_TIMER ( %VAL(0) )
      IUER = -1
      CALL READ_HEB ( FIL_LS, HEB_LS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7401, IUER, 'LS_DIST', 'Failure to open input '// &
     &         'land/sea/lake mask '//FIL_LS )
           CALL EXIT ( 1 )
      END IF
      NLAT_LS = HEB_LS%DIMS(2)
      NLON_LS = HEB_LS%DIMS(1) 
!
! --- Allocate memory for the output mask
!
      NLON = (DIM+1)*4
      NLAT = (DIM+1)*2 + 1
      IR = NLON_LS/NLON
      IF ( IR*NLON .NE. NLON_LS ) THEN
           IUER = -1
           CALL CLRCH ( STR1 ) 
           CALL INCH  ( NLON_LS/4, STR1 )
           CALL ERR_LOG ( 7402, IUER, 'LS_DIST', 'Wrong dimension '// &
     &          STR(1:I_LEN(STR))//'. The dimension + 1 should be an '// &
     &         'integer fraction of the land/sea/lake mask dimension '// &
     &          STR1 )
           CALL EXIT ( 1 )
      END IF
      HEB_DLS = HEB_LS
      HEB_DLS%DIMS(1) = NLON
      HEB_DLS%DIMS(2) = NLAT
      HEB_DLS%DIMS(3) = 1
      HEB_DLS%DIMS(4) = 1
      ALLOCATE ( HEB_DLS%VAL(HEB_DLS%DIMS(1),HEB_DLS%DIMS(2),1,1) )
      ALLOCATE ( LS_PREM(HEB_DLS%DIMS(1),HEB_DLS%DIMS(2)) )
      ALLOCATE ( LS_INIT(HEB_DLS%DIMS(1),HEB_DLS%DIMS(2)) )
!
! --- Initialize it with "land"
!
      HEB_DLS%VAL = 0.0
!
      TIM_R8 = WALL_TIMER ( %VAL(2) )
      WRITE ( 6, '(A,F8.2," IR: ", I4)' ) 'Started the first  pass Time: ', TIM_R8, IR ; CALL FLUSH ( 6 ) 
      TIM_R8 = WALL_TIMER ( %VAL(0) )
!
! --- Build arrays of sine/cosine latitude and longitude for the land/sea mask grid
!
      DO 410 J1=1,NLAT_LS
         LAT  = -P2I + (J1-1)*(PI__NUM/(HEB_LS%DIMS(2)-1)) 
         CLA_LS(J1) = DCOS(LAT)
         SLA_LS(J1) = DSIN(LAT)
 410  CONTINUE 
      DO 420 J2=1,NLON_LS
         LON  = (J2-1)*PI2/HEB_LS%DIMS(1)
         CLO_LS(J2) = DCOS(LON)
         SLO_LS(J2) = DSIN(LON)
 420  CONTINUE 
!
! --- Initial mask: we count the number of land MOD44W sub-cells into DLS cells
!
      LS_INIT = 0
      DO 430 J3=1,NLAT_LS
         ILAT = (J3-1)/IR + 1
         DO 440 J4=1,NLON_LS
            ILON = (J4-1)/IR + 1
            IF ( MODE == MALO__SEA_VAL  .AND. HEB_LS%VAL1(J4,J3,1,1) .NE. MALO__SEA_VAL ) THEN
!
! -------------- Number of cells that are either land or lake
!
                 LS_INIT(ILON,ILAT) = LS_INIT(ILON,ILAT) + 1
            END IF
            IF ( MODE == MALO__LAND_VAL .AND. HEB_LS%VAL1(J4,J3,1,1) .EQ. MALO__SEA_VAL ) THEN
!
! -------------- Number of cells that are sea
!
                 LS_INIT(ILON,ILAT) = LS_INIT(ILON,ILAT) + 1
            END IF
 440     CONTINUE 
 430  CONTINUE 
!
      TIM_R8 = WALL_TIMER ( %VAL(2) )
      WRITE ( 6, '(A,F8.2)' ) 'Started the second pass Time: ', TIM_R8 ; CALL FLUSH ( 6 ) 
      TIM_R8 = WALL_TIMER ( %VAL(0) )
!
! --- Secondary mask: we count the number of MOD4W cells in -+MD DLS cells with respect
! --- to a given cell. The purpose of this mask to bypass cells that have only land
! --- or only ocean within +-MD cells
!
      LS_PREM = 0
      DO 450 J5=1,NLAT
         LAT = -P2I + (J5-1)*PI__NUM/(NLAT-1)
         ML = NINT(MD/COS(LAT)+0.5)
         IF ( MODE == MALO__SEA_VAL .AND. J5 < MD + 1 ) THEN
!
! ----------- Antarctic region: all land
!
              LS_PREM(1:NLON,J5) = (2*MD+1)*(2*ML+1)*IR**2
           ELSE IF ( MODE == MALO__LAND_VAL  .AND. J5 < MD + 1 ) THEN
              LS_PREM(1:NLON,J5) = 0
           ELSE IF ( MODE == MALO__SEA_VAL .AND. J5 > NLAT - MD - 1 ) THEN
!
! ----------- Arctic region: all sea
!
              LS_PREM(1:NLON,J5) = 0
           ELSE IF ( MODE == MALO__LAND_VAL .AND. J5 > NLAT - MD - 1 ) THEN
              LS_PREM(1:NLON,J5) = (2*MD+1)*(2*ML+1)*IR**2
           ELSE
!$OMP PARALLEL DO DEFAULT ( NONE ) &
!$OMP          PRIVATE ( J6, J7, J8, ILAT, ILON ), &
!$OMP          SHARED  ( J5, NLON, MD, ML, LS_PREM, LS_INIT )
              DO 460 J6=1,NLON 
                 DO 470 J7=-MD,MD
                    ILAT = J5 + J7
                    DO 480 J8=-ML,ML
                       ILON = J6 + J8
                       IF ( ILON < 1    ) ILON = ILON + NLON
                       IF ( ILON > NLON ) ILON = ILON - NLON
                       LS_PREM(J6,J5) = LS_PREM(J6,J5) + LS_INIT(ILON,ILAT) 
 480               CONTINUE 
 470             CONTINUE 
 460          CONTINUE 
!$OMP    END PARALLEL DO
         END IF
 450  CONTINUE 
!!      DEALLOCATE ( LS_INIT )
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!  klat = nint( (56.06*deg__to__rad + p2i)/ ( pi__num/(nlat-1) ) ) + 1
!  klon = nint( 356.27*deg__to__rad/ (pi2/nlon) ) + 1
!    write ( 6, * ) ' klon/klat= ', klon, klat ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      TIM_R8 = WALL_TIMER ( %VAL(2) )
      WRITE ( 6, '(A,F8.2)' ) 'Started the main   pass Time: ', TIM_R8 ; CALL FLUSH ( 6 ) 
      TIM_R8 = WALL_TIMER ( %VAL(0) )
!
! --- Final fine pass
!
      DO 490 J9=1,NLAT
         LAT = -P2I + (J9-1)*PI__NUM/(NLAT-1)
         IF ( LAT < LAT_MIN ) THEN
!
! ----------- Antarctic region: all land
!
              IF ( MODE == MALO__SEA_VAL ) THEN
                   HEB_DLS%VAL(1:NLON,J9,1,1) =  0.0
                 ELSE 
                   HEB_DLS%VAL(1:NLON,J9,1,1) = -2.0
              END IF
              GOTO 490
           ELSE IF ( LAT > LAT_MAX ) THEN
!
! ----------- Arctic region: all sea
!
              HEB_DLS%VAL(1:NLON,J9,1,1) = -1.0
              GOTO 490
         END IF
         TIM_R8 = WALL_TIMER ( %VAL(2) )
         WRITE  ( 6, 210 ) J9, NLAT, TIM_R8, CHAR(13)
         CALL FLUSH ( 6 ) 
 210     FORMAT ( '  Lat ', I6, ' ( ', I6, ' )  ',F8.3, '  ', A$ ) 
         TIM_R8 = WALL_TIMER ( %VAL(0) )
         ILAT = (J9-1)*IR + 1
         ML = NINT(MD/COS(LAT)+0.5)
!
!$OMP PARALLEL DO IF ( J9 > -7 ) DEFAULT ( NONE ) &
!$OMP          PRIVATE ( J10, J11, J12, ILON, JLON, JLAT, &
!$OMP                    DAC, LON, DIST_SQ, VEC_DLS, VEC_LS ), &
!$OMP          SHARED  ( IR, ML, MD, J9, NLON, NLAT, NLON_LS, NLAT_LS, LAT, &
!$OMP                    ILAT, KLAT, KLON, HEB_DLS, HEB_LS, CLA_LS, SLA_LS, &
!$OMP                    CLO_LS, SLO_LS, LS_PREM, LS_INIT, MODE ) 
         DO 4100 J10=1,NLON
            LON  = (J10-1)*PI2/NLON
            VEC_DLS(1) = DCOS(LAT)*DCOS(LON)
            VEC_DLS(2) = DCOS(LAT)*DSIN(LON)
            VEC_DLS(3) = DSIN(LAT)
            ILON = (J10-1)*IR + 1
            IF ( MODE == MALO__SEA_VAL .AND.  LS_PREM(J10,J9) == 0 ) THEN
!
! -------------- There is only ocean within -+ MD  DLS cells
!
                 HEB_DLS%VAL(J10,J9,1,1) = -1.0
               ELSE IF ( MODE == MALO__SEA_VAL  .AND. LS_PREM(J10,J9) == (2*MD+1)*(2*ML+1)*IR**2 ) THEN
!
! -------------- There is only land within -+ MD  DLS cells
!
                 HEB_DLS%VAL(J10,J9,1,1) =  0.0
               ELSE IF ( MODE == MALO__SEA_VAL  .AND. HEB_LS%VAL1(ILON,ILAT,1,1) == MALO__SEA_VAL ) THEN
!
! -------------- The center of the current cell is in the ocean
!
                 HEB_DLS%VAL(J10,J9,1,1) = -1.0
               ELSE IF ( MODE == MALO__LAND_VAL .AND. LS_PREM(J10,J9) == 0 ) THEN
!
! -------------- There is only land within -+ MD  DLS cells
!
                 HEB_DLS%VAL(J10,J9,1,1) = -2.0
               ELSE IF ( MODE == MALO__LAND_VAL  .AND. LS_PREM(J10,J9) == (2*MD+1)*(2*ML+1)*IR**2 ) THEN
!
! -------------- There is only ocean within -+ MD  DLS cells
!
                 HEB_DLS%VAL(J10,J9,1,1) = -1.0
               ELSE IF ( MODE == MALO__LAND_VAL .AND. HEB_LS%VAL1(ILON,ILAT,1,1) .NE. MALO__SEA_VAL ) THEN
!
! -------------- The center of the current cell is in the land
!
                 HEB_DLS%VAL(J10,J9,1,1) = -2.0
               ELSE 
!
! -------------- There is both land and ocean within -+ MD DLS cells 
!
                 DO 4110 J11=-MD*IR,MD*IR
!
! ----------------- Look -+ MD cells along latitude
! 
                    JLAT = ILAT + J11
                    VEC_LS(3) = SLA_LS(JLAT)
                    DO 4120 J12=-ML*IR,ML*IR
!
! -------------------- Look -+ MD cells along longitude
!
                       JLON = ILON + J12
                       IF ( JLON < 1       ) JLON = JLON + NLON_LS
                       IF ( JLON > NLON_LS ) JLON = ILON - NLON_LS
                       IF ( ( MODE == MALO__SEA_VAL  .AND. HEB_LS%VAL1(JLON,JLAT,1,1) .EQ. MALO__SEA_VAL  ) .OR. &
     &                      ( MODE == MALO__LAND_VAL .AND. HEB_LS%VAL1(JLON,JLAT,1,1) .NE. MALO__SEA_VAL )      ) THEN
!!
!!                            dist = sqrt ( (j12*cos(lat))**2 + j11**2 ) ! long way
!!                            dist = dsqrt ( 2.0d0*(1.0d0 - dac) ) + dsqrt ( 2.0d0*(1.0d0 - dac) )**3/24.0d0 + ...
!!
!
! ------------------------- Compute the distance using a simplified approach. We expand arccos
! ------------------------- near 1 in the series and keep the first term. This is euquivalent
! ------------------------- to use the chord between two points instead of the arc
!

                            VEC_LS(1) = CLA_LS(JLAT)*CLO_LS(JLON)
                            VEC_LS(2) = CLA_LS(JLAT)*SLO_LS(JLON)
                            DAC = VEC_LS(1)*VEC_DLS(1) + VEC_LS(2)*VEC_DLS(2) + VEC_LS(3)*VEC_DLS(3)
                            DIST_SQ = 2.0D0 - 2.0D0*DAC
!
                            IF ( HEB_DLS%VAL(J10,J9,1,1) > EPS ) THEN
!
! ------------------------------ The distance square to that cell was already computed?
! ------------------------------ Let us check, whether the new distance is less
!
                                 IF ( DIST_SQ < HEB_DLS%VAL(J10,J9,1,1) ) THEN
!
! ----------------------------------- Yes, it less than that
!
                                      HEB_DLS%VAL(J10,J9,1,1) = DIST_SQ
                                 END IF
                               ELSE
!
! ------------------------------ The distance was not yet computed
!
                                 HEB_DLS%VAL(J10,J9,1,1) = DIST_SQ
                           END IF
                       END IF
 4120               CONTINUE 
 4110            CONTINUE 
                 IF ( MODE == MALO__LAND_VAL .AND. HEB_DLS%VAL(J10,J9,1,1) == 0.0 ) THEN
!
! ------------------- There is only ocean within -+ MD  DLS cells
!
                      HEB_DLS%VAL(J10,J9,1,1) = -1.0
                 END IF
            END IF
 4100    CONTINUE 
!$OMP    END PARALLEL DO
 490  CONTINUE 
!
      WRITE  ( 6, * ) 'Last pass                         ' ; CALL FLUSH ( 6 ) 
      NC = 0
!
! --- During previous step for speed we computed the square of distance. Time
! --- came to take the square root and convert the arc in radians to the 
! --- arc in cells.
!
      DO 4130 J13=1,NLAT
         DO 4140 J14=1,NLON
            IF ( HEB_DLS%VAL(J14,J13,1,1) > 0.0 ) THEN
                 HEB_DLS%VAL(J14,J13,1,1) = SQRT ( HEB_DLS%VAL(J14,J13,1,1) )/(PI2/NLON)
                 NC = NC + 1
            END IF
            IF ( FL_LS  .AND.  MODE == MALO__SEA_VAL ) THEN
                 IF ( HEB_DLS%VAL(J14,J13,1,1) > -EPS_LS .AND. &
     &                HEB_DLS%VAL(J14,J13,1,1) <  EPS_LS       ) THEN
                      HEB_DLS%VAL(J14,J13,1,1) = 1.0
                    ELSE
                      HEB_DLS%VAL(J14,J13,1,1) = 0.0
                 END IF
              ELSE IF ( FL_LS  .AND.  MODE == MALO__LAND_VAL ) THEN
                 IF ( HEB_DLS%VAL(J14,J13,1,1) > -1.0 - EPS_LS .AND. &
     &                HEB_DLS%VAL(J14,J13,1,1) < -1.0 + EPS_LS       ) THEN
                      HEB_DLS%VAL(J14,J13,1,1) = 0.0
                    ELSE
                      HEB_DLS%VAL(J14,J13,1,1) = 1.0
                 END IF
            END IF
 4140    CONTINUE 
 4130  CONTINUE 
      WRITE  ( 6, * ) 'Prepare the header' ; CALL FLUSH ( 6 ) 
!
! --- Prepare the HEB header
!
      HEB_DLS%FILL_VALUE   = 0.0
      HEB_DLS%DATA_FORMAT  = HEB__R4
      HEB_DLS%OFFSET       = 0.0
      HEB_DLS%SCALE_FACTOR = 1.0
      IF ( .NOT. FL_LS ) THEN
           IF ( MODE == MALO__SEA_VAL ) THEN
                HEB_DLS%SDS_NAME     = 'Distance to the coast from land for a bandlimite land/sea mask'
                HEB_DLS%PROD_NAME    = 'Distance to the ocean'
                HEB_DLS%TITLE        = 'Distance to the ocean'
              ELSE IF ( MODE == MALO__LAND_VAL ) THEN
                HEB_DLS%SDS_NAME     = 'Distance to the coast from the ocean for a bandlimite land/sea mask'
                HEB_DLS%PROD_NAME    = 'Distance to the land'
                HEB_DLS%TITLE        = 'Distance to the land'
           END IF
         ELSE IF ( FL_LS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MD, STR )
           IF ( MODE == MALO__SEA_VAL ) THEN
                HEB_DLS%SDS_NAME     = 'Land sea-mask expanded to the land at '//STR(1:I_LEN(STR))//' cells'
                HEB_DLS%PROD_NAME    = HEB_DLS%SDS_NAME     
                HEB_DLS%TITLE        = HEB_DLS%SDS_NAME     
              ELSE IF ( MODE == MALO__LAND_VAL ) THEN
                HEB_DLS%SDS_NAME     = 'Land sea-mask expanded to the ocean at '//STR(1:I_LEN(STR))//' cells'
                HEB_DLS%PROD_NAME    = HEB_DLS%SDS_NAME     
                HEB_DLS%TITLE        = HEB_DLS%SDS_NAME     
           END IF
      END IF
      HEB_DLS%INSTITUTION  = 'Astrogeo Center'
      HEB_DLS%HISTORY      = 'Land/lake/sea mask derived from MOD44W was used: '//FIL_LS(1:I_LEN(FIL_LS))// &
     &                       ' produced by '//HEB_DLS%HISTORY(1:I_LEN(HEB_DLS%HISTORY))
      HEB_DLS%UNITS        = 'cells'
      HEB_DLS%VERSION_ID   = LS_DIST__LABEL
      CALL CLRCH ( STR )
      CALL INCH  ( MD, STR )
      IF ( .NOT. FL_LS ) THEN
           IF ( MODE == MALO__SEA_VAL ) THEN
                HEB_DLS%COMMENT(1)   = 'Distance from a given cell to the closet cell at original MOD44W land/sea mask that is sea'
                HEB_DLS%COMMENT(2)   = 'If distance less than 1, but greater than 0 that means the cell is partly water'
                HEB_DLS%COMMENT(3)   = 'If the cell is in the sea, the distance is -1. '// &
     &                                 'If the distance to the sea is greather than '// &
     &                                  STR(1:I_LEN(STR))//', it is set to 0'
              ELSE IF ( MODE == MALO__LAND_VAL ) THEN
                HEB_DLS%COMMENT(1)   = 'Distance from a given cell to the closet cell at original MOD44W land/sea mask that is land'
                HEB_DLS%COMMENT(2)   = 'If distance less than 1, but greater than 0 that means the cell is partly land'
                HEB_DLS%COMMENT(3)   = 'If the cell is in the sea, the distance is -1. '// &
     &                                 'If the cell is in the land, the distance is -2.'
           END IF
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:10), FMT='(I10)' ) NC
           WRITE ( UNIT=STR(11:18), FMT='(F8.6)' ) NC/(FLOAT(NLON)*FLOAT(NLAT))
           HEB_DLS%COMMENT(4) = STR(1:10)//' cells near the coast. Their share is '//STR(11:18)
         ELSE IF ( FL_LS ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:10), FMT='(I10)' ) NC
           WRITE ( UNIT=STR(11:18), FMT='(F8.6)' ) NC/(FLOAT(NLON)*FLOAT(NLAT))
           HEB_DLS%COMMENT(1) = STR(1:10)//' cells near the coast. Their share is '//STR(11:18)
           CALL CLRCH ( HEB_DLS%COMMENT(2) )
           CALL CLRCH ( HEB_DLS%COMMENT(3) )
           CALL CLRCH ( HEB_DLS%COMMENT(4) )
      END IF
      CALL HEB_MINMAX ( HEB_DLS, HEB_DLS%VAL, 100.0 )
      HEB_DLS%VALID_RANGE(1) = -1.0
      HEB_DLS%VALID_RANGE(2) =  MD*1.0
      HEB_DLS%PROD_DATE_TIME = GET_CDATE()
!
! --- Write the output file
!
      IUER = -1
      CALL WRITE_HEB ( HEB_DLS, HEB_DLS%VAL, FIL_DLS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7403, IUER, 'LS_DIST', 'Failure to write '// &
     &         'the output file '//FIL_DLS )
           CALL EXIT ( 1 )
      END IF
      WRITE ( 6, '(A)' ) 'Written file '//FIL_DLS(1:I_LEN(FIL_DLS))
!
      END  PROGRAM  LS_DIST  !#!#
