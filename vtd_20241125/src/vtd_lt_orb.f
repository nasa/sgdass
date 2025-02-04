      SUBROUTINE VTD_LT_ORB ( VTD, STA_GRO, STA_ORB, MJD, TAI, LT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_LT_ORB computes the light time between the ground      *
! *   station STA_GRO and orbiting station STA_ORB at the moment of time *
! *   MJD,TAI on the clock synchronized with TAI for the ground station. *
! *   It is assumed that the ephemeride of the orbiting station and the  *
! *   catalogue of ground stations has already been loaded.              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
! * STA_GRO ( CHARACTER ) -- Name of the ground station.                 *
! * STA_ORB ( CHARACTER ) -- Name of the orbiting station.               *
! *                          The station should be in the input          *
! *                          catalogue defined in the control file.      *
! *     MJD ( INTEGER*4 ) -- Modified Julian date of the midnight of     *
! *                          the observation.                            *
! *     TAI ( INTEGER*4 ) -- Time of the observations in seconds at      *
! *                          time scale TAI elapsed from the midnight.   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      LT ( REAL*8    ) -- time for propagation of light in vacuum     *
! *                          from the ground station to the orbiting     *
! *                          station.                                    *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *  ### 14-JUN-2012   VTD_LT_ORB  v1.0 (c)  L. Petrov  14-OCT-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      TYPE     ( VTD__OBS_TYPE  ) :: OBS_TYP
      CHARACTER  STA_GRO*(*), STA_ORB*(*)
      INTEGER*4  MJD, IUER
      REAL*8     TAI, LT
      INTEGER*4  M__LT_ITER
      PARAMETER  ( M__LT_ITER = 3 )
      REAL*8     COO_GRO(3), DER_DEL(VTD__NDER), DER_RAT(VTD__NDER), &
     &           DELAY, RATE
      CHARACTER  VTD_DIFX_COMPAT*3
      INTEGER*4  J1, J2, J3, J4, ISTA_GRO, ISTA_ORB, IND_NZO, IER
      LOGICAL*1  FL_COMPAT
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      IF ( VTD%STATUS .NE. VTD__LOAD ) THEN
           CALL ERR_LOG ( 2212, IUER, 'VTD_LT_ORB', 'Routine VTD_LOAD was '// &
     &         'not executed before the call of VTD_DELAY' )
           RETURN
      END IF
      CALL GETENVAR ( 'VTD_DIFX_COMPAT', VTD_DIFX_COMPAT )
      IF ( VTD_DIFX_COMPAT == 'yes' .OR. VTD_DIFX_COMPAT == 'YES' ) THEN
           FL_COMPAT = .TRUE.
         ELSE 
           FL_COMPAT = .FALSE.
      END IF
!
      OBS_TYP%PLRZ    = 'RR'     
      OBS_TYP%FRQ_REF(1) = 2.2D9
      OBS_TYP%FRQ_REF(2) = 8.2D9
      OBS_TYP%N_BND      = 1
      OBS_TYP%DELAY_TYPE = VTD__MLMH__DTP 
      OBS_TYP%FRQ_ION_EFF(1) = 2.3D9
      OBS_TYP%FRQ_ION_EFF(2) = 8.5D9
      OBS_TYP%EXP_NAME   = 'Test'
      OBS_TYP%SCAN_NAME  = 'Scan_0001'
      OBS_TYP%STATUS     = VTD__BND 
!
! --- Find indices of the ground and orbiting stations
!
      ISTA_GRO = 0
      ISTA_ORB= 0
      DO 410 J1=1,VTD%L_STA
         IF ( VTD%STA(J1)%STA_TYP .EQ. VTD__GR ) THEN
              IF ( STA_GRO == VTD%STA(J1)%IVS_NAME ) ISTA_GRO = J1
           ELSE IF ( VTD%STA(J1)%STA_TYP .EQ. VTD__OR ) THEN
              IF ( STA_ORB == VTD%STA(J1)%IVS_NAME ) ISTA_ORB = J1
         END IF
 410  CONTINUE 
      IF ( ISTA_GRO == 0 ) THEN
           CALL ERR_LOG ( 3251, IUER, 'VTD_LT_ORB', 'Cannot find '// &
     &         'ground station '//STA_GRO(1:I_LEN(STA_GRO))// &
     &         ' -- it was not defined early when VTD_LOAD '// &
     &         ' was called' )
           RETURN 
      END IF
      IF ( ISTA_ORB == 0 ) THEN
           CALL ERR_LOG ( 3252, IUER, 'VTD_LT_ORB', 'Cannot find '// &
     &         'orbit station '//STA_ORB(1:I_LEN(STA_ORB))// &
     &         ' -- it was not defined early when VTD_LOAD_NZO '// &
     &         ' was called' )
           RETURN 
      END IF
!
      IF ( VTD%L_NZO == 0 ) THEN
           CALL ERR_LOG ( 3253, IUER, 'VTD_LT_ORB', 'No position of '//  &
     &         'any orbiting object was loaded. Cannot get position '// &
     &         'for orbiting antenna '//STA_ORB )
           RETURN
      END IF
!
! --- Get the index of the near zone object (usually 1)
!
      IND_NZO = 0
      DO 420 J2=1,VTD%L_NZO
         IF ( VTD%NZO(J2)%NAME == STA_ORB ) THEN
              IND_NZO = J2
         END IF
 420  CONTINUE
      IF ( IND_NZO == 0 ) THEN
           CALL ERR_LOG ( 3254, IUER, 'VTD_LT_ORB', 'No ephemerides '// &
     &         'the the orbiting antenna '//STA_ORB(1:I_LEN(STA_ORB))// &
     &         ' was found' )
           RETURN
      END IF
      VTD%MOM%MJD = 0 ! clean cashe
      VTD%STA(ISTA_GRO)%MJD = 0
      VTD%STA(ISTA_GRO)%TAI = 0.0
!
! --- Yes, we call VTD_DELAY. It may seem strange. 
! --- We do it because VTD_DELAY computes position of the stations 
! --- in CRS by applying the most sophisticated model.
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_DELAY ( VTD%SOU(1)%IVS_NAME, STA_GRO, STA_GRO, MJD, TAI, &
     &                 OBS_TYP, VTD, DELAY, RATE, DER_DEL, DER_RAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3255, IUER, 'VTD_LT_ORB', 'Error in '// &
     &         'an attempt to compute time delay between the ground '// &
     &         'station' )
           RETURN
      END IF
!
! --- Get position of the ground station at moment MJD, TAI from 
! --- VTD data structure
!
      COO_GRO = VTD%STA(ISTA_GRO)%COO_CRS
!
! --- Starting from the first guess 0.0 we iteratively improve 
! --- the estimate of the light time
!
      LT = 0.0D0
      DO 430 J3=1,M__LT_ITER
         IF ( FL_COMPAT ) LT = 0.0D0
         VTD%MOM%MJD = 0 ! clean cache
         VTD%STA(ISTA_ORB)%MJD = 0
         VTD%STA(ISTA_ORB)%TAI = 0.0
!
! ------ Among other things, this routine computes position of the
! ------ orbiting station at the retarded moment of time TAI-LT
!
         CALL ERR_PASS ( IUER, IER )
         CALL VTD_MOMENT ( VTD%SOU(1)%IVS_NAME, MJD, TAI-LT, VTD, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 3256, IUER, 'VTD_LT_ORB', 'Error in '// &
     &            'an attempt to compute station-independent time '// &
     &            'varying intermediate quantities' )
              RETURN
         END IF
!
! ------ Get Light time
!
         LT = DSQRT ( ( COO_GRO(1) - VTD%STA(ISTA_ORB)%COO_CRS(1) )**2 + &
     &                ( COO_GRO(2) - VTD%STA(ISTA_ORB)%COO_CRS(2) )**2 + &
     &                ( COO_GRO(3) - VTD%STA(ISTA_ORB)%COO_CRS(3) )**2   &
     &              )/VTD__C
 430  CONTINUE 
!
      VTD%MOM%MJD = 0 ! clean cache again
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END   SUBROUTINE VTD_LT_ORB  !#!  
