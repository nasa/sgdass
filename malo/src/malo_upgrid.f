      SUBROUTINE MALO_UPGRID ( MODE, HEBIN, HEBLS, HEB_MASK, HEBOUT, &
     &                         MSN, MSF, MST, MIN_VAL, MAX_VAL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_UPGRID 
! *                                                                      *
! *  ### 22-DEC-2013  MALO_UPGRID  v4.0 (c)  L. Petrov  10-JUN-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  MODE, IUER
      REAL*4     MIN_VAL, MAX_VAL
      INCLUDE   'astro_constants.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEBIN, HEBLS, HEB_MASK, HEBOUT
      INTEGER*4    MSN, MSF, MST
      REAL*4       EPS_VAL, EPS_LS, REA
      PARAMETER  ( EPS_VAL = 1.0E-15 )
      PARAMETER  ( EPS_LS  = 0.002   )
      REAL*8       PHI_MAX, PHI_MIN
      PARAMETER  ( PHI_MIN = -85.0D0*DEG__TO__RAD )
      PARAMETER  ( PHI_MAX =  85.0D0*DEG__TO__RAD )
      LOGICAL*1  FL_LAND, FL_MIXED, FL_OCEAN
      REAL*4     DP_NEAR, DP_FAR, DP_FINE, DP_NEAR_SQ, DP_FAR_SQ, DP_FINE_SQ, &
     &           LON, LAT, LONJ, LATJ, DIST, DIST_SQ, WEI, SUM_PAR, SUM_WEI
      REAL*8     DAC
      LOGICAL*1  FL_TIMER
      REAL*8,    ALLOCATABLE :: VECIN(:,:,:), VECOUT(:,:,:)
      REAL*4,    ALLOCATABLE :: FINE_OUT(:,:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, MSN_LAT, &
     &           ILON, ILAT, JLON, JLAT, JSH_LON, ILA_MIN, ILA_MAX, KLAT, KLON, &
     &           KP, IER
      REAL*8,    EXTERNAL :: ARC_LEN_AD, DP_VV_V 
!
      FL_TIMER = .FALSE.
!
      DP_NEAR    = 0.5*PI__NUM/(HEBIN%DIMS(2)-1) 
      IF ( MODE == 1 ) THEN
           DP_FAR =  (MSF/8.0)*PI__NUM/(HEBIN%DIMS(2)-1)
         ELSE 
           DP_FAR =  (MSF/4.0)*PI__NUM/(HEBIN%DIMS(2)-1)
      END IF
      DP_FINE    = 0.5*PI__NUM/(HEBOUT%DIMS(2)-1)
      DP_NEAR_SQ = DP_NEAR**2
      DP_FAR_SQ  = DP_FAR**2
      DP_FINE_SQ = DP_FINE**2
!
      IF ( MODE == 1 ) THEN
!
! -------- Ocean
!
           ILA_MIN = (P2I + PHI_MIN)/PI__NUM*(HEBOUT%DIMS(2)-1)
           ILA_MAX = HEBOUT%DIMS(2)
        ELSE IF ( MODE == 2 ) THEN
!
! -------- Land
!
           ILA_MIN = 1
           ILA_MAX = (P2I + PHI_MAX)/PI__NUM*(HEBOUT%DIMS(2)-1)
        ELSE IF ( MODE == 3 ) THEN
!
! -------- Everythere
!
           ILA_MIN = 1
           ILA_MAX = HEBOUT%DIMS(2)
      END IF
!
      ALLOCATE ( VECIN(3,HEBIN%DIMS(1),HEBIN%DIMS(2)),    STAT=IER )
      ALLOCATE ( VECOUT(3,HEBOUT%DIMS(1),HEBOUT%DIMS(2)), STAT=IER )
!
      IF ( FL_TIMER ) CALL WALL_TIMER ( %VAL(0) )
      DO 510 J1=1,HEBIN%DIMS(2)
         LAT  = (J1-1)*(PI__NUM/(HEBIN%DIMS(2)-1)) - P2I
         DO 520 J2=1,HEBIN%DIMS(1)
            LON  = (J2-1)*PI2/HEBIN%DIMS(1)
            VECIN(1,J2,J1) = COS(LAT)*COS(LON)
            VECIN(2,J2,J1) = COS(LAT)*SIN(LON)
            VECIN(3,J2,J1) = SIN(LAT)
 520     CONTINUE 
 510  CONTINUE 
!
      DO 530 J3=1,HEBOUT%DIMS(2)
         LAT  = (J3-1)*(PI__NUM/(HEBOUT%DIMS(2)-1)) - P2I
         DO 540 J4=1,HEBOUT%DIMS(1)
            LON  = (J4-1)*PI2/HEBOUT%DIMS(1)
            VECOUT(1,J4,J3) = COS(LAT)*COS(LON)
            VECOUT(2,J4,J3) = COS(LAT)*SIN(LON)
            VECOUT(3,J4,J3) = SIN(LAT)
 540     CONTINUE 
 530  CONTINUE 
      IF ( MODE == 1 .AND. MSN > 0 ) THEN
           ALLOCATE ( FINE_OUT(HEBOUT%DIMS(1),HEBOUT%DIMS(2)), STAT=IER )
      END IF
      DO 410 J1=1,HEBOUT%DIMS(4)
         DO 420 J2=1,HEBOUT%DIMS(3)
!$OMP PARALLEL DO IF ( HEBOUT%DIMS(1) > 5000 ) DEFAULT ( NONE ) &
!$OMP                  PRIVATE ( J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
!$OMP                            ILON, ILAT, LON, LAT, KP, FL_OCEAN, FL_LAND, FL_MIXED, WEI, &
!$OMP                            SUM_PAR, SUM_WEI, JLON, JLAT, JSH_LON, DAC, DIST_SQ ) &
!$OMP                  SHARED  ( HEBOUT, HEBIN, HEBLS, HEB_MASK, MSN, MSF, MST, MODE, &
!$OMP                            MIN_VAL, MAX_VAL, DP_FAR_SQ, DP_NEAR_SQ, DP_FINE_SQ, &
!$OMP                            VECIN, VECOUT, J1, J2, KLAT, KLON,  ILA_MIN, ILA_MAX )
            DO 430 J3=ILA_MIN,ILA_MAX
               LAT  = -P2I + (J3-1)*( PI__NUM/(HEBOUT%DIMS(2)-1) ) 
               ILAT = INT( (LAT + P2I)/ ( PI__NUM/(HEBIN%DIMS(2)-1) ) ) + 1
               DO 440 J4=1,HEBOUT%DIMS(1)
                  HEBOUT%VAL(J4,J3,J2,J1) = 0.0
                  IF ( ASSOCIATED ( HEB_MASK%VAL ) ) THEN
                       IF ( HEB_MASK%VAL(J4,J3,1,1) == 0.0 ) THEN
                            GOTO 440
                       END IF
                  END IF
                  LON  = (J4-1)*PI2/HEBOUT%DIMS(1)
                  ILON = INT( LON/ (PI2/HEBIN%DIMS(1)) ) + 1
                  FL_OCEAN = .FALSE.
                  FL_LAND  = .FALSE.
                  FL_MIXED = .FALSE.
                  IF ( INDEX ( HEBLS%SDS_NAME, 'Distance to the coast from the ocean' ) > 0 ) THEN
                       IF ( HEBLS%VAL(J4,J3,1,1) > -1.0 - EPS_LS .AND. &
     &                      HEBLS%VAL(J4,J3,1,1) < -1.0 + EPS_LS       ) THEN
                            FL_OCEAN  = .TRUE.
                          ELSE
                            FL_LAND  = .TRUE.
                       END IF
                     ELSE
                       IF ( HEBLS%VAL(J4,J3,1,1) < EPS_LS ) THEN
                            FL_OCEAN = .TRUE.
                         ELSE IF ( HEBLS%VAL(J4,J3,1,1) > 1.0 - EPS_LS ) THEN
                            FL_LAND  = .TRUE.
                         ELSE 
                            FL_MIXED = .TRUE.
                       END IF
                  END IF
!
                  IF ( MODE == 1 ) THEN
!
! ================== Expand over ocean cells
!
                     IF ( FL_LAND ) THEN
!
! ----------------------- LS mask is land
!
                          HEBOUT%VAL(J4,J3,J2,J1)= 0.0
                        ELSE
!
! ----------------------- Everything else
!
                          SUM_PAR = 0.0D0
                          SUM_WEI = 0.0D0
!
! ----------------------- Near ocean zone
!
                          KP = 0
                          DO 450 J5=-MSN,MSN
                             JLAT = ILAT + J5
                             JSH_LON = 0
                             IF ( JLAT > HEBIN%DIMS(2) ) THEN
                                  JSH_LON = HEBIN%DIMS(1)/2
                                  JLAT    = 2*HEBIN%DIMS(2) - JLAT
                             END IF
                             IF ( JLAT < 1 ) THEN
                                  JSH_LON = HEBIN%DIMS(1)/2
                                  JLAT    = 1 - JLAT
                             END IF
                             DO 460 J6=-MSN,MSN
                                JLON = ILON + J6 + JSH_LON
                                IF ( JLON < 1             ) JLON = JLON + HEBIN%DIMS(1)
                                IF ( JLON > HEBIN%DIMS(1) ) JLON = JLON - HEBIN%DIMS(1) 
                                IF ( HEBIN%VAL(JLON,JLAT,J2,J1) > MIN_VAL .AND. &
     &                               HEBIN%VAL(JLON,JLAT,J2,J1) < MAX_VAL       ) THEN
                                     DAC =   VECIN(1,JLON,JLAT)*VECOUT(1,J4,J3) &
     &                                     + VECIN(2,JLON,JLAT)*VECOUT(2,J4,J3) &
     &                                     + VECIN(3,JLON,JLAT)*VECOUT(3,J4,J3)
                                     IF ( DAC > 1.0D0 - 1.D-10 ) THEN
                                          DIST_SQ = 0.0D0
                                        ELSE 
!!
!!                                              dist = dacos( dac )
!!                                              dist = dsqrt ( 2.0d0*(1.0d0 - dac) ) + dsqrt ( 2.0d0*(1.0d0 - dac) )**3/24.0d0
!!
                                          DIST_SQ = 2.0D0*(1.0D0 - DAC)
                                     END IF
                                     WEI  = EXP(-DIST_SQ/DP_NEAR_SQ)
                                     SUM_PAR = SUM_PAR + WEI*HEBIN%VAL(JLON,JLAT,J2,J1)
                                     SUM_WEI = SUM_WEI + WEI
                                     KP = KP + 1
                                END IF
 460                         CONTINUE 
 450                      CONTINUE 
                          IF ( KP == (2*MSN+1)**2 ) THEN
!
! ---------------------------- All points were ocean
!
                               HEBOUT%VAL(J4,J3,J2,J1) = SUM_PAR/SUM_WEI
                             ELSE IF ( MSF > 0 ) THEN
                               SUM_PAR = 0.0
                               SUM_WEI = 0.0
!
! ---------------------------- Far ocean zone
!
                               DO 470 J7=-MSF,MSF,MST
                                  JLAT = ILAT + J7
                                  JSH_LON = 0
                                  IF ( JLAT > HEBIN%DIMS(2) ) THEN
                                       JSH_LON = HEBIN%DIMS(1)/2
                                       JLAT    = 2*HEBIN%DIMS(2) - JLAT
                                  END IF
                                  IF ( JLAT < 1 ) THEN
                                       JSH_LON = HEBIN%DIMS(1)/2
                                       JLAT    = 1 - JLAT
                                  END IF
!
                                  DO 480 J8=-MSF,MSF,MST
                                     JLON = ILON + J8 + JSH_LON 
                                     IF ( JLON < 1             ) JLON = JLON + HEBIN%DIMS(1)
                                     IF ( JLON > HEBIN%DIMS(1) ) JLON = JLON - HEBIN%DIMS(1) 
                                     IF ( HEBIN%VAL(JLON,JLAT,J2,J1) > MIN_VAL .AND. &
     &                                    HEBIN%VAL(JLON,JLAT,J2,J1) < MAX_VAL       ) THEN
                                          DAC = VECIN(1,JLON,JLAT)*VECOUT(1,J4,J3) + &
     &                                          VECIN(2,JLON,JLAT)*VECOUT(2,J4,J3) + &
     &                                          VECIN(3,JLON,JLAT)*VECOUT(3,J4,J3) 
                                          IF ( DAC > 1.0D0 - 1.D-10 ) THEN
                                               DIST_SQ = 0.0D0
                                            ELSE 
                                               DIST_SQ = 2.0D0*(1.0D0 - DAC)
                                          END IF
                                          WEI  = EXP(-DIST_SQ/DP_FAR_SQ)
                                          SUM_PAR = SUM_PAR + WEI*HEBIN%VAL(JLON,JLAT,J2,J1)
                                          SUM_WEI = SUM_WEI + WEI
                                     END IF
 480                              CONTINUE 
 470                           CONTINUE 
                               IF ( SUM_WEI > EPS_VAL ) THEN
                                    HEBOUT%VAL(J4,J3,J2,J1) = SUM_PAR/SUM_WEI
                                  ELSE 
                                    HEBOUT%VAL(J4,J3,J2,J1) = 0.0
                               END IF
                          END IF
                     END IF
                   ELSE IF ( MODE == 2 .OR. MODE == 3 ) THEN
!
! ================== Expand over land cells
!
                     IF ( FL_OCEAN .AND. MODE == 2 ) THEN
!
! ----------------------- LS is total ocean
!
                          HEBOUT%VAL(J4,J3,J2,J1) = 0.0
                       ELSE 
!
! ----------------------- Everything else
!
                          SUM_PAR = 0.0D0
                          SUM_WEI = 0.0D0
!
! ----------------------- Near land zone
!
                          KP = 0
                          DO 490 J9=-MSN,MSN
                             JLAT = ILAT + J9
                             JSH_LON = 0
                             IF ( JLAT > HEBIN%DIMS(2) ) THEN
                                  JSH_LON = HEBIN%DIMS(1)/2
                                  JLAT    = 2*HEBIN%DIMS(2) - JLAT
                             END IF
                             IF ( JLAT < 1 ) THEN
                                  JSH_LON = HEBIN%DIMS(1)/2
                                  JLAT    = 1 - JLAT
                             END IF
                             DO 4100 J10=-MSN,MSN
                                JLON = ILON + J10 + JSH_LON
                                IF ( JLON < 1             ) JLON = JLON + HEBIN%DIMS(1)
                                IF ( JLON > HEBIN%DIMS(1) ) JLON = JLON - HEBIN%DIMS(1) 
                                IF ( HEBIN%VAL(JLON,JLAT,J2,J1) > MIN_VAL .AND. &
     &                               HEBIN%VAL(JLON,JLAT,J2,J1) < MAX_VAL       ) THEN
                                     DAC =   VECIN(1,JLON,JLAT)*VECOUT(1,J4,J3) &
     &                                     + VECIN(2,JLON,JLAT)*VECOUT(2,J4,J3) &
     &                                     + VECIN(3,JLON,JLAT)*VECOUT(3,J4,J3)
                                     IF ( DAC > 1.0D0 - 1.D-10 ) THEN
                                          DIST_SQ = 0.0D0
                                        ELSE 
                                          DIST_SQ = 2.0D0*(1.0D0 - DAC)
                                     END IF
                                     WEI  = EXP(-DIST_SQ/DP_NEAR_SQ)
                                     SUM_PAR = SUM_PAR + WEI*HEBIN%VAL(JLON,JLAT,J2,J1)
                                     SUM_WEI = SUM_WEI + WEI
                                     KP = KP + 1
                                END IF
 4100                        CONTINUE 
 490                      CONTINUE 
                          IF ( KP == (2*MSN+1)**2 ) THEN
!
! ---------------------------- All points were land
!
                               HEBOUT%VAL(J4,J3,J2,J1) = SUM_PAR/SUM_WEI
                             ELSE IF ( MSF > 0 ) THEN
!
! ---------------------------- Far land zone
!
                               DO 4110 J11=-MSF,MSF,MST
                                  JLAT = ILAT + J11
                                  IF ( JLAT < 1 ) GOTO 4110
                                  IF ( JLAT > HEBIN%DIMS(2) ) GOTO 4110
                                  DO 4120 J12=-MSF,MSF,MST
                                     JLON = ILON + J12
                                     IF ( JLON < 1             ) JLON = JLON + HEBIN%DIMS(1)
                                     IF ( JLON > HEBIN%DIMS(1) ) JLON = JLON - HEBIN%DIMS(1) 
                                     IF ( HEBIN%VAL(JLON,JLAT,J2,J1) > MIN_VAL .AND. &
     &                                    HEBIN%VAL(JLON,JLAT,J2,J1) < MAX_VAL       ) THEN
                                          DAC = DP_VV_V ( 3, VECIN(1,JLON,JLAT), VECOUT(1,J4,J3) )
                                          IF ( DAC > 1.0D0 - 1.D-12 ) THEN
                                               DIST_SQ = 0.0D0
                                             ELSE 
                                               DIST_SQ = 2.0D0*(1.0D0 - DAC)
                                          END IF
                                          WEI  = EXP(-DIST_SQ/DP_FAR_SQ)
                                          SUM_PAR = SUM_PAR + WEI*HEBIN%VAL(JLON,JLAT,J2,J1)
                                          SUM_WEI = SUM_WEI + WEI
                                     END IF
 4120                             CONTINUE 
 4110                          CONTINUE 
                               IF ( SUM_WEI > EPS_VAL ) THEN
                                    HEBOUT%VAL(J4,J3,J2,J1) = SUM_PAR/SUM_WEI
                                  ELSE 
                                    HEBOUT%VAL(J4,J3,J2,J1) = 0.0
                               END IF
                          END IF
                       END IF
                  END IF
 440           CONTINUE 
 430        CONTINUE 
!$OMP       END PARALLEL DO
!
            IF ( MODE == 1 .AND. MSN > 0 ) THEN
!
! -------------- Fine smoothing of the output field
!
                 FINE_OUT(1:HEBOUT%DIMS(1),1:HEBOUT%DIMS(2)) = HEBOUT%VAL(1:HEBOUT%DIMS(1),1:HEBOUT%DIMS(2),J2,J1) 
!$OMP PARALLEL DO IF ( HEBOUT%DIMS(1) > 5000 ) DEFAULT ( NONE ) &
!$OMP                  PRIVATE ( J13, J14, J15, J16, FL_OCEAN, FL_LAND, FL_MIXED, WEI, &
!$OMP                            SUM_PAR, SUM_WEI, JLON, JLAT, DAC, DIST_SQ, LAT, MSN_LAT ) &
!$OMP                  SHARED  ( HEBOUT, HEBIN, HEBLS, MSN, DP_FINE_SQ, VECOUT, &
!$OMP                            FINE_OUT, J1, J2 )
                 DO 4130 J13=2*MSN+1,HEBOUT%DIMS(2)-2*MSN-1
                    LAT  = -P2I + (J13-1)*( PI__NUM/(HEBOUT%DIMS(2)-1) ) 
                    MSN_LAT = MSN/COS(LAT)
                    DO 4140 J14=1,HEBOUT%DIMS(1)
                       FL_OCEAN = .FALSE.
                       FL_LAND  = .FALSE.
                       FL_MIXED = .FALSE.
                       IF ( INDEX ( HEBLS%SDS_NAME, 'Distance to the coast from the ocean' ) > 0 ) THEN
                            IF ( HEBLS%VAL(J14,J13,1,1) > -1.0 - EPS_LS .AND. &
     &                           HEBLS%VAL(J14,J13,1,1) < -1.0 + EPS_LS       ) THEN
                                 FL_OCEAN  = .TRUE.
                               ELSE
                                 FL_LAND  = .TRUE.
                            END IF
                         ELSE
                            IF ( HEBLS%VAL(J14,J13,1,1) < EPS_LS ) THEN
                                 FL_OCEAN = .TRUE.
                              ELSE IF ( HEBLS%VAL(J14,J13,1,1) > 1.0 - EPS_LS ) THEN
                                 FL_LAND  = .TRUE.
                              ELSE 
                                 FL_MIXED = .TRUE.
                            END IF
                       END IF
                       IF ( .NOT. FL_LAND ) THEN
                            SUM_PAR = 0.0
                            SUM_WEI = 0.0
                            DO 4150 J15=-MSN,MSN
                               JLAT = J15+J13
                               DO 4160 J16=-MSN_LAT,MSN_LAT
                                  JLON = J16+J14
                                  IF ( JLON < 1              ) JLON = JLON + HEBOUT%DIMS(1)
                                  IF ( JLON > HEBOUT%DIMS(1) ) JLON = JLON - HEBOUT%DIMS(1) 
                                  DAC = DP_VV_V ( 3, VECOUT(1,JLON,JLAT), VECOUT(1,J14,J13) )
                                  IF ( DAC > 1.0D0 - 1.D-12 ) THEN
                                       DIST_SQ = 0.0D0
                                     ELSE 
                                       DIST_SQ = 2.0D0*(1.0D0 - DAC)
                                  END IF
                                  WEI  = EXP(-DIST_SQ/DP_FINE_SQ)
                                  SUM_PAR = SUM_PAR + WEI*HEBOUT%VAL(JLON,JLAT,J2,J1)
                                  SUM_WEI = SUM_WEI + WEI
 4160                          CONTINUE 
 4150                       CONTINUE 
                            IF ( SUM_WEI > EPS_VAL ) THEN
                                 FINE_OUT(J14,J13) = SUM_PAR/SUM_WEI
                            END IF
                       END IF
 4140               CONTINUE 
 4130            CONTINUE 
!$OMP END PARALLEL DO
                 HEBOUT%VAL(1:HEBOUT%DIMS(1),1:HEBOUT%DIMS(2),J2,J1) = FINE_OUT(1:HEBOUT%DIMS(1),1:HEBOUT%DIMS(2))
            END IF
            DO 4170 J17=1,ILA_MIN-1
               HEBOUT%VAL(1:HEBOUT%DIMS(1),J17,J2,J1) = 0.0
 4170       CONTINUE 
            DO 4180 J18=ILA_MAX+1,HEBOUT%DIMS(2)
               HEBOUT%VAL(1:HEBOUT%DIMS(1),J18,J2,J1) = 0.0
 4180       CONTINUE 
 420    CONTINUE 
 410  CONTINUE 
      IF ( FL_TIMER ) CALL WALL_TIMER ( %VAL(1) )
      DEALLOCATE ( VECIN  )
      DEALLOCATE ( VECOUT )
      IF ( ALLOCATED(FINE_OUT) ) DEALLOCATE ( FINE_OUT )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_UPGRID  !#!  
