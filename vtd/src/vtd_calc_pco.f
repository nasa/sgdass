      SUBROUTINE VTD_RX_PCO ( VTD, RX_PCO_TOT, RX2SAT, OBS_TYP, ISTA, IUER)
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_RX_PCO calculates the position offset/variation       *
! *   for a receiving antenna from a GNSS source                         *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *      RX2SAT ( REAL*8    ) -- Line of sight vector from RX to SAT     *
! *                                  in terrestrial coords               *
! *  OBS_TYP ( VTD__OBS_TYPE ) -- The object with information about      *
! *                                  observation type, polarization and  *
! *                                  frequency setup of the experiment.  *
! *     ISTA ( INTEGER*4 ) -- Index of the station in the VTD station    *
! *                           list.                                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      RX_PCO_TOT ( REAL*8    ) -- XYZ Phase center offset             *
! *                                  for source ISOU                     *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 26-MAR-2024  VTD_READ_ANTEX   v1.0 (c)  J. Skeens  26-MAR-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INCLUDE   'astro_constants.i'
      TYPE ( VTD__TYPE ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  STR*128
      INTEGER*4  ISTA, IUER, IER
      INTEGER*4  ITYPE, IFRQ, J1, J2, J3, AZI_IDX, ELEV_IDX
      LOGICAL*1  FOUND_TABLE
      REAL*8     RX_PCO_TOT(3), PCO_XYZ(3), PCV_XYZ(3), A_FRAC, E_FRAC, &
     &           PHASE_PAT_BL, PHASE_PAT_UL, PHASE_PAT_BR, PHASE_PAT_UR, &
     &           RX2SAT(3), PCV, PHASE_PAT_B, PHASE_PAT_U
      FOUND_TABLE = .FALSE.
      DO 410 J1=1,VTD%L_ANT_TYPE 
         IF ( VTD%PCO(J1)%ANTENNA_TYPE == VTD%STA(ISTA)%ANTEX_TYPE ) THEN
             DO 420 J2 = 1,VTD%PCO(J1)%NFREQ
                IF ( ABS ( OBS_TYP%FRQ_REF(1) - VTD%PCO(J1)%RINEX_FREQ(J2)%FREQ ) < 1.0D8 ) THEN
!
! ----------------- Found an applicable PCO/PCV table, get PCO
!            
                    FOUND_TABLE = .TRUE.
                    CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, 3, &
     &                                 VTD%PCO(J1)%RINEX_FREQ(J2)%UEN, 3, &
     &                                 PCO_XYZ, IER )
!
! ----------------- Prepare for bilinear interpolation in elevation, azimuth to get PCV
!            
                    ELEV_IDX = FLOOR ( (VTD%STA(ISTA)%ELEV/DEG__TO__RAD & 
                        - VTD%PCO(J1)%BOUND_LOW_ELEV) /VTD%PCO(J1)%DELEV ) + 1
                    E_FRAC = (VTD%STA(ISTA)%ELEV/DEG__TO__RAD - VTD%PCO(J1)%BOUND_LOW_ELEV) &
     &                        / VTD%PCO(J1)%DELEV - (ELEV_IDX - 1)
                    IF ( VTD%PCO(J1)%DAZI > 0.0D0 ) THEN
                        AZI_IDX = FLOOR ( VTD%STA(ISTA)%AZ/(DEG__TO__RAD* VTD%PCO(J1)%DAZI) ) + 1
                        A_FRAC = VTD%STA(ISTA)%AZ/(DEG__TO__RAD*VTD%PCO(J1)%DAZI) - (AZI_IDX-1)
                        PHASE_PAT_BL = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(AZI_IDX, ELEV_IDX)
                        PHASE_PAT_UL = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(AZI_IDX, ELEV_IDX + 1)
                        PHASE_PAT_BR = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(AZI_IDX + 1, ELEV_IDX)
                        PHASE_PAT_UR = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(AZI_IDX + 1, ELEV_IDX + 1)
                        PCV = (1-A_FRAC) * (1-E_FRAC) * PHASE_PAT_BL + A_FRAC * (1-E_FRAC) * PHASE_PAT_BR &
     &                        + (1-A_FRAC) * E_FRAC * PHASE_PAT_UL + A_FRAC * E_FRAC * PHASE_PAT_UR 
                    ELSE
                        PHASE_PAT_B = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(1, ELEV_IDX)
                        PHASE_PAT_U = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(1, ELEV_IDX + 1)
                        PCV = (1-E_FRAC) * PHASE_PAT_B + E_FRAC * PHASE_PAT_U 
                    END IF
                    RX_PCO_TOT = (PCO_XYZ - PCV * RX2SAT) * 1.0D-3
!
                    GOTO 430
                END IF
 420         CONTINUE            
             IF ( FOUND_TABLE .EQV. .FALSE. ) THEN
                 CALL CLRCH ( STR )            
                 WRITE ( UNIT=STR, FMT='(F9.3)' ) OBS_TYP%FRQ_REF(1)
                 CALL ERR_LOG ( 2924, IUER, 'VTD_RX_PCO', 'No PCO '//  &
     &               ' table found for reference frequency '//STR )
                 RETURN
             END IF
         END IF

 410  CONTINUE
      IF ( FOUND_TABLE .EQV. .FALSE. ) THEN
          CALL ERR_LOG ( 2925, IUER, 'VTD_RX_PCO', 'No PCO '//  &
     &        ' table found for antenna type '//TRIM(VTD%STA(ISTA)%ANTEX_TYPE) )
          RETURN
      END IF
!
 430  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_RX_PCO  !#!#


      SUBROUTINE VTD_SAT_PCO ( VTD, SAT_PCO_TOT, OBS_TYP, ISOU, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_SAT_PCO calculates the position offset/variation      *
! *   for a transmitting antenna on a GNSS satellite                     *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *  OBS_TYP ( VTD__OBS_TYPE ) -- The object with information about      *
! *                                  observation type, polarization and  *
! *                                  frequency setup of the experiment.  *
! *     ISOU ( INTEGER*4 ) -- Index of the source in the VTD source list.*
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      SAT_PCO_TOT ( REAL*8    ) -- XYZ Phase center offset            *
! *                                  for source ISOU                     *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 26-MAR-2024  VTD_READ_ANTEX   v1.0 (c)  J. Skeens  26-MAR-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  STR*128, SOU_ANT_NAME*3
      INTEGER*4  ISOU, IUER, IER
      INTEGER*4  ITYPE, IFRQ, J1, J2, J3
      LOGICAL*1  FOUND_TABLE
      REAL*8     SAT_PCO_TOT(3)
      
      FOUND_TABLE = .FALSE.
      SOU_ANT_NAME = VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NAME
      DO 410 J1=1,VTD%L_ANT_TYPE 
         IF ( TRIM ( VTD%PCO(J1)%ANTENNA_TYPE ) == TRIM ( SOU_ANT_NAME ) ) THEN
             DO 420 J2 = 1,VTD%PCO(J1)%NFREQ
                IF ( ABS ( OBS_TYP%FRQ_REF(1) - VTD%PCO(J1)%RINEX_FREQ(J2)%FREQ ) < 1.0D8 ) THEN
!
! ----------------- Found an applicable PCO/PCV table, get PCO
!            
                    FOUND_TABLE = .TRUE.
                    SAT_PCO_TOT = VTD%PCO(J1)%RINEX_FREQ(J2)%UEN / 1.0D3
                    GOTO 430
                END IF
 420         CONTINUE            
             IF ( FOUND_TABLE .EQV. .FALSE. ) THEN
                 CALL CLRCH ( STR )            
                 WRITE ( UNIT=STR, FMT='(F9.3)' ) OBS_TYP%FRQ_REF(1)
                 CALL ERR_LOG ( 2925, IUER, 'VTD_SAT_PCO', 'No PCO '//  &
     &               ' table found for reference frequency '//STR )
                 RETURN
             END IF
         END IF
 410  CONTINUE

 430  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_SAT_PCO  !#!#

      SUBROUTINE VTD_SAT_STA_PCV ( VTD, SAT_PCV, OBS_TYP, ISTA, ISOU, &
     &                             SATBODY_TO_ECEF, RX2SAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_SAT_STA_PCV calculates the satellite position         *
! *   variation to a receiving antenna from a GNSS source                *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *  OBS_TYP ( VTD__OBS_TYPE ) -- The object with information about      *
! *                               observation type, polarization and     *
! *                               frequency setup of the experiment.     *
! *     ISOU ( INTEGER*4 ) -- Index of the source in the VTD source list.*
! *     SATBODY_TO_ECEF ( REAL*8    ) -- Rotation matrix from satellite  *
! *                                   body frame to ECEF                 *
! *      RX2SAT ( REAL*8    ) -- Line of sight vector from RX to SAT     *
! *                                  in terrestrial coords               *
! *                                                                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      SAT_PCV ( REAL*8    ) -- Phase center variation                 *
! *                               from source ISOU to station ISTA       *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 26-MAR-2024  VTD_SAT_STA_PCV   v1.0 (c)  J. Skeens  26-MAR-2024 ##*
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      TYPE ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  STR*128, SOU_ANT_NAME*3
      INTEGER*4  ISTA, ISOU, IUER, IER
      INTEGER*4  ITYPE, IFRQ, J1, J2, J3, NAD_IDX, AZI_IDX
      LOGICAL*1  FOUND_TABLE
      REAL*8     RX_PCO_TOT(3), PCO_XYZ(3), PCV_XYZ(3), AZI_FRAC, ELEV_FRAC, &
     &           SATBODY_TO_ECEF(3,3), ECEF_TO_SATBODY(3,3), SAT_PCV, &
     &           PHASE_PAT_BL, PHASE_PAT_UL, PHASE_PAT_BR, PHASE_PAT_UR, &
     &           NADIR, AZIMUTH, N_FRAC, A_FRAC, RX2SAT_BODY(3), RX2SAT(3), &
     &           SAT_POS_TRS(3), PHASE_PAT_B, PHASE_PAT_U
      
      FOUND_TABLE = .FALSE.
      CALL TM83 ( SATBODY_TO_ECEF, ECEF_TO_SATBODY )
!
! --- Get nadir and azimuth angle of station from satellite
!            
      CALL ERR_PASS  ( IUER, IER )
      CALL MUL_MV_IV_V ( 3, 3, ECEF_TO_SATBODY, 3, RX2SAT, 3, RX2SAT_BODY, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2928, IUER, 'VTD_SAT_STA_PCV', 'Failure in attempt '// &
     &         'to transform RX2SAT vector to body frame' )
           RETURN
      END IF
      NADIR =  ACOSD ( -RX2SAT_BODY(1) )
      AZIMUTH = ATAN2D ( -RX2SAT_BODY(2), -RX2SAT_BODY(3) )
      IF ( AZIMUTH < 0.0D0 ) THEN
          AZIMUTH = AZIMUTH + 360.0D0
      END IF
      SOU_ANT_NAME = VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NAME
      DO 410 J1=1,VTD%L_ANT_TYPE 
         IF ( TRIM ( VTD%PCO(J1)%ANTENNA_TYPE ) == TRIM ( SOU_ANT_NAME ) ) THEN
             DO 420 J2 = 1,VTD%PCO(J1)%NFREQ
                IF ( ABS ( OBS_TYP%FRQ_REF(1) - VTD%PCO(J1)%RINEX_FREQ(J2)%FREQ ) < 1.0D8 ) THEN
!
! ----------------- Found an applicable PCV table
!            
                    FOUND_TABLE = .TRUE.
!
! ----------------- Prepare for bilinear interpolation in elevation, azimuth to get PCV
!            
                    NAD_IDX = FLOOR ( (NADIR - VTD%PCO(J1)%BOUND_LOW_ELEV) &
     &                             /VTD%PCO(J1)%DELEV ) + 1
                    N_FRAC = (NADIR - VTD%PCO(J1)%BOUND_LOW_ELEV) &
     &                        / VTD%PCO(J1)%DELEV - (NAD_IDX - 1)
                    IF ( VTD%PCO(J1)%DAZI > 0.0D0 ) THEN
                        AZI_IDX = FLOOR ( AZIMUTH/VTD%PCO(J1)%DAZI ) + 1
                        A_FRAC = AZIMUTH/VTD%PCO(J1)%DAZI - (AZI_IDX - 1)
                        PHASE_PAT_BL = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(AZI_IDX, NAD_IDX)
                        PHASE_PAT_UL = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(AZI_IDX, NAD_IDX + 1)
                        PHASE_PAT_BR = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(AZI_IDX + 1, NAD_IDX)
                        PHASE_PAT_UR = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(AZI_IDX + 1, NAD_IDX + 1)
                        SAT_PCV = (1-A_FRAC) * (1-N_FRAC) * PHASE_PAT_BL + A_FRAC * (1-N_FRAC) * PHASE_PAT_BR &
     &                        + (1-A_FRAC) * N_FRAC * PHASE_PAT_UL + A_FRAC * N_FRAC * PHASE_PAT_UR 
                    ELSE
                        PHASE_PAT_B = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(1, NAD_IDX)
                        PHASE_PAT_U = VTD%PCO(J1)%RINEX_FREQ(J2)%PHASE_PAT(1, NAD_IDX + 1)
                        SAT_PCV = (1-N_FRAC) * PHASE_PAT_B + N_FRAC * PHASE_PAT_U 
                    END IF
                    SAT_PCV = SAT_PCV / 1.0D3
!
                    GOTO 430
                END IF
 420         CONTINUE            
             IF ( FOUND_TABLE .EQV. .FALSE. ) THEN
                 CALL CLRCH ( STR )            
                 WRITE ( UNIT=STR, FMT='(F9.3)' ) OBS_TYP%FRQ_REF(1)
                 CALL ERR_LOG ( 2926, IUER, 'VTD_SAT_STA_PCV', 'No PCV '//  &
     &               ' table found for reference frequency '//STR )
                 RETURN
             END IF
         END IF
 410  CONTINUE

 430  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_SAT_STA_PCV  !#!#

      SUBROUTINE VTD_CALC_SATBODY_TRANSF ( VTD, SAT_POS_TRS, SATBODY_TO_ECEF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_CALC_SATBODY_TRANSF calculates the transformation      *
! *   from the satellite body-fixed frame to ECEF                        *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *      SAT_POS_TRS ( REAL*8    ) -- Position of satellite in ECEF      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *     SATBODY_TO_ECEF ( REAL*8    ) -- Rotation matrix from satellite  *
! *                                   body frame to ECEF                 *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ## 26-MAR-2024  VTD_READ_ANTEX   v1.0 (c)  J. Skeens  26-MAR-2024 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  IUER, IER
      REAL*8     SATX(3), SATY(3), SATZ(3), COO_SUN(3), VEL_SUN(3), ACC_SUN(3), &
     &           SAT_POS_TRS(3), COO_EAR(3), VEL_EAR(3), ACC_EAR(3), SAT2SUN(3), &
     &           RN, SATBODY_TO_ECEF(3,3), CRS_TO_TRS(3,3), SUN_POS_TRS(3)
      REAL*8,    EXTERNAL :: DP_VV_V
      
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, VTD%MOM%MJD, VTD%MOM%TAI, 'EARTH', &
     &                      COO_EAR, VEL_EAR, ACC_EAR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2927, IUER, 'VTD_CALC_SATBODY_TRANSF', 'Failure in attempt '// &
     &         'to get barycentric coordinates of the Earth' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL PLANETA_DE_EPH ( VTD%DE_EPH, VTD%MOM%MJD, VTD%MOM%TAI, 'SUN', &
     &                      COO_SUN, VEL_SUN, ACC_SUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2928, IUER, 'VTD_CALC_SATBODY_TRANSF', 'Failure in attempt '// &
     &         'to get barycentric coordinates of the Sun' )
           RETURN
      END IF
      COO_SUN = COO_SUN - COO_EAR

      CALL TM83 ( VTD%MOM%TRS_TO_CRS, CRS_TO_TRS )
      CALL MUL_MV_IV_V ( 3, 3, CRS_TO_TRS, &
     &                   3, COO_SUN, 3, SUN_POS_TRS, IER )
      SAT2SUN = SUN_POS_TRS - SAT_POS_TRS
      CALL NORM_VEC ( 3, SAT2SUN, RN ) 
      SATZ = -SAT_POS_TRS 
      CALL NORM_VEC ( 3, SATZ, RN ) 
      CALL CP_VV_V3 ( SATZ, SAT2SUN, SATY )
      CALL NORM_VEC ( 3, SATY, RN ) 
      CALL CP_VV_V3 ( SATY, SATZ, SATX )
      IF ( DP_VV_V ( 3, SATX, SAT2SUN ) < 0 ) THEN
          SATX = -1.0D0*SATX
          SATY = -1.0D0*SATY
      END IF
      SATBODY_TO_ECEF(1:3,1) = SATZ
      SATBODY_TO_ECEF(1:3,2) = SATY
      SATBODY_TO_ECEF(1:3,3) = SATX
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_CALC_SATBODY_TRANSF  !#!#

      SUBROUTINE VTD_CALC_PHASE_WINDUP ( VTD, RX2SAT, SATBODY_TO_ECEF, &
     &           OBS_TYP, ISTA, ISOU, PHASE_WINDUP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_CALC_PHASE_WINDUP calculates and compensates for       *
! *   phase windup caused by polarization/satellite orientation          *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      VTD ( RECORD    ) -- Object which keeps configuration and data  *
! *                           related to VLBI Theoretical Delay (VTD)    *
! *                           package.                                   *
! *      RX2SAT ( REAL*8    ) -- Line of sight vector from RX to SAT     *
! *                                  in terrestrial coords               *
! *     SATBODY_TO_ECEF ( REAL*8    ) -- Rotation matrix from satellite  *
! *                                   body frame to ECEF                 *
! *  OBS_TYP ( VTD__OBS_TYPE ) -- The object with information about      *
! *                                  observation type, polarization and  *
! *                                  frequency setup of the experiment.  *
! *     ISTA ( INTEGER*4 ) -- Index of the station in the VTD station    *
! *                           list.                                      *
! *     ISOU ( INTEGER*4 ) -- Index of the source in the VTD source list.*
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      PHASE_WINDUP ( REAL*8    ) -- phase windup delay in seconds     *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD%NZO ( RECORD ) -- Phase windup in cycles is computed/stored  *
! *                           by satellite for the next execution of     *
! *                           VTD_CALC_PHASE_WINDUP                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  18-APR-2024  VTD_CALC_PHASE_WINDUP v1.0 (c)  J. Skeens  18-APR-2024 *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INCLUDE   'astro_constants.i'
      TYPE ( VTD__TYPE ) :: VTD
      TYPE ( VTD__OBS_TYPE  ) :: OBS_TYP
      INTEGER*4  IUER, IER, ISTA, ISOU
      REAL*8     RX2SAT(3), SAT2RX(3), RN, SATBODY_TO_ECEF(3,3), E_VEC(3), &
     &           N_VEC(3), U_VEC(3), E_VEC_ECEF(3), N_VEC_ECEF(3), U_VEC_ECEF, &
     &           PHASE_WINDUP, TX(3), TY(3), TZ(3), RX_A(3), RX_T(3), &
     &           KXTX(3), KXTXXK(3), KXTY(3), KXTYXK(3)
      REAL*8,    EXTERNAL :: DP_VV_V
      
      SAT2RX = -RX2SAT
      U_VEC = 0.0D0
      N_VEC = 0.0D0
      E_VEC = 0.0D0
      U_VEC(1) = 1.0D0
      E_VEC(2) = 1.0D0
      N_VEC(3) = 1.0D0
      TX = SATBODY_TO_ECEF(1:3,3)  
      TY = SATBODY_TO_ECEF(1:3,2)
      TZ = SATBODY_TO_ECEF(1:3,1)
      CALL VM83     ( SAT2RX, TY, KXTY )
      CALL VM83     ( KXTY,   SAT2RX, KXTYXK )
      CALL VM83     ( SAT2RX, TX,     KXTX )
      CALL VM83     ( KXTX,   SAT2RX, KXTXXK )
!
! --- Compute the satellite phase windup in cycles
!  
      IF ( VTD%STA(ISTA)%MOUNT_TYPE  == 'GNSS' ) THEN
          CALL ERR_PASS  ( IUER, IER )
          CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, 3, &
     &                       N_VEC, 3, &
     &                       N_VEC_ECEF, IER )
          CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, 3, &
     &                       E_VEC, 3, &
     &                       E_VEC_ECEF, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 2929, IUER, 'VTD_CALC_PHASE_WINDUP', 'Failure in attempt '// &
     &             'to transform East and North vector to ECEF' )
               RETURN
          END IF
          PHASE_WINDUP = ATAN2 ( DP_VV_V ( 3, KxTYxK, E_VEC_ECEF ) + DP_VV_V ( 3, KxTXxK, N_VEC_ECEF ), &
                         DP_VV_V ( 3, KxTXxK, E_VEC_ECEF ) - DP_VV_V ( 3, KxTYxK, N_VEC_ECEF ) ) / PI2
      ELSE IF ( VTD%STA(ISTA)%MOUNT_TYPE  == 'AZEL' ) THEN
          CALL ERR_PASS  ( IUER, IER )
          CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS, 3, &
     &                       U_VEC, 3, &
     &                       U_VEC_ECEF, IER )
          IF ( IER .NE. 0 ) THEN
               CALL ERR_LOG ( 2930, IUER, 'VTD_CALC_PHASE_WINDUP', 'Failure in attempt '// &
     &             'to transform local up vector to ECEF' )
               RETURN
          END IF
          CALL VM83     ( U_VEC_ECEF, RX2SAT, RX_A )
          CALL VM83     ( RX2SAT, RX_A, RX_T  )
          CALL NORM_VEC ( 3, RX_T, RN )
          CALL NORM_VEC ( 3, RX_A, RN )
          PHASE_WINDUP = ATAN2 ( DP_VV_V ( 3, KxTYxK, RX_A ) + DP_VV_V ( 3, KxTXxK, RX_T ), &
                         DP_VV_V ( 3, KxTXxK, RX_A ) - DP_VV_V ( 3, KxTYxK, RX_T ) ) / PI2
      ELSE
           CALL ERR_LOG ( 2931, IUER, 'VTD_CALC_PHASE_WINDUP', 'Unimplemented '// &
     &         'mount type ',  VTD%STA(ISTA)%MOUNT_TYPE)
           RETURN
      ENDIF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_CALC_PHASE_WINDUP !#!#
