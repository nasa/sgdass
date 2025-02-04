      SUBROUTINE VTD_DOPPLER ( SOU_NAM, STA_NAM, MJD, TAI, VTD, D_FRQ, &
     &                         DER_FRQ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_DOPPLER is the main routine of the package VTD for    *
! *   computation of the Doppler frequency shift parameter. It computes  *
! *   also partial derivatives of the Doppler frequency shift parameter  *
! *   with respect to parameters of the model at the specified moment of *
! *   time, for the specified stations and for the specified near zone   *
! *   source using the model specified from the control file previously  *
! *   loaded into the data structure VTD. It is assumed that routines    *
! *   VTD_INIT, VTD_CONF and VTD_LOAD were called before the call        *
! *   of VTD_DO.                                                         *
! *                                                                      *
! *   The Doppler frequency shift parameter is defined as a ratio of     *
! *   the observed frequency to the emitted frequency minus 1.           *
! *                                                                      *
! *   Precision of computation of VLBI time delay is deemed to be no     *
! *   worse than 1.D-11 .                                                *
! *                                                                      *
! *   CAVEAT: as of 2007.01.11, computation of partial derivatives is    *
! *           not implemented.                                           *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  SOU_NAM ( CHARACTER ) -- Name of the source. The source should      *
! *                           be in the input catalogue defined in the   *
! *                           control file.                              *
! *  STA_NAM ( CHARACTER ) -- Name of the station. The station name      *
! *                           should be in the input catalogue defined   *
! *                           in the control file.                       *
! *      MJD ( INTEGER*4 ) -- Modified Julian date of the midnight of    *
! *                           the observation.                           *
! *      TAI ( INTEGER*4 ) -- Time of the observations in seconds at     *
! *                           time scale TAI elapsed from the midnight.  *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    D_FRQ ( REAL*8    ) -- The Doppler frequency shift parameter      *
! *                           defined as a ratio of the observed         *
! *                           frequency to the emitted frequency minus 1.*
! *  DER_DEL ( REAL*8    ) -- Vector of partial derivatives of the       *
! *                           Doppler frequency ratio with respect       *
! *                           to parameters of the model. Dimension:     *
!                             DER__NDER.                                 *
! *            Symbolic names of the indexes of the partial derivatives  *
! *            in the array DER_DEL are defined in vtd.i .               *
! *            Their meaning:                                            *
! *                                                                      *
! *            VTD__DER_E1    -- Earth rotation Euler angle 1            *
! *            VTD__DER_E2    -- Earth rotation Euler angle 2            *
! *            VTD__DER_E3    -- Earth rotation Euler angle 3            *
! *            VTD__DER_ST1X  -- Station, X coordinate                   *
! *            VTD__DER_ST1Y  -- Station, Y coordinate                   *
! *            VTD__DER_ST1Z  -- Station, Z coordinate                   *
! *            VTD__DER_RA    -- Right ascension of the far-zone source  *
! *            VTD__DER_DL    -- Declination of the far-zone source      *
! *            VTD__DER_POS1  -- Position of near zone object, X coord.  *
! *            VTD__DER_POS2  -- Position of near zone object, Y coord.  *
! *            VTD__DER_POS3  -- Position of near zone object, Z coord.  *
! *            VTD__DER_VEL1  -- Velocity of near zone object, X coord.  *
! *            VTD__DER_VEL2  -- Velocity of near zone object, Y coord.  *
! *            VTD__DER_VEL3  -- Velocity of near zone object, Z coord.  *
! *                                                                      *
! *  DER_FRQ ( REAL*8    ) -- Vector of partial derivatives of phase     *
! *                           delay rate with respect to parameters of   *
! *                           the model. Dimension: DER__NDER.           *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
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
! *  ### 27-DEC-2006  VTD_DOPPLER  v1.0 (c)  L. Petrov  08-JAN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      CHARACTER  SOU_NAM*(*), STA_NAM*(*)
      INTEGER*4  MJD, IUER
      REAL*8     TAI, D_FRQ, DER_FRQ(VTD__NDER)
      TYPE     ( VTD__TYPE      ) :: VTD
      REAL*8     VEL_O(3), COO_O(3), COO_PLAN(3), VEL_PLAN(3), R_VEC(3), &
     &           COO_E(3), VEL_E(3), R_O(3), R_E(3), COO_OE(3)
      REAL*8     DIST_SUN_EARTH, U_TERM, V_TERM, VV, TAU_EMIT, TARG, R_LEN, &
     &           TAU_RETR, RO_LEN, RE_LEN, CO_ACC, CE_ACC, OE_LEN, TIM_ARG, &
     &           PAR0, PAR1, O_LEN, E_LEN, A_E, A_O, B_E, B_O, C_E, C_O
      REAL*8     AXOF_CRS(3), AXOF_TRS(3), STD_REN(3), STV_REN(3), &
     &           STD_TRS(3),  STV_TRS(3),  PTD_REN(3), PTD_TRS(3), &
     &           PSD_TRS(3),  PSV_TRS(3),  PSD_REN(3), PSV_REN(3), &
     &           RT_CRS(3),   STD_CRS(3),  STV_CRS(3), PSD_TRS_SUM(3), &
     &           PSV_TRS_SUM(3), TIME_REF, RD, UP_RATE_CRS(3),   &
     &           OMEGA,  AXOF_UP(2), AXOF_UP_RATE(2), PSD_CRS(3), &
     &           AXOF_CRS_RATE(3), AXOF_TRS_RATE(3), STD_CRS_DER1(3), &
     &           AVEC_CRS(3), AVEC_CRS_RATE(3), UNIT_AXOF_CRS(3), &
     &           UNIT_AXOF_CRS_RATE(3), SUR_PRS, SUR_PWP, SUR_TEM
      REAL*8     UP_CRS(3),    UP_UEN(3),    UP_TRS(3),    &
     &           NORTH_CRS(3), NORTH_UEN(3), NORTH_TRS(3), &
     &           EAST_CRS(3), VEC_PROJ_EN(3), VAL, DT, N_PROJ, E_PROJ, &
     &           S_APP(3), VEC1(3), SV
      INTEGER*4  ISTA, ISOU, KNOT, J1, J2, J3, J4, J5, J6, J7, J8, J9, NN, IER
      CHARACTER  STR*128
      LOGICAL*4, EXTERNAL :: PROBE_WRITE_ADDRESS
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: ATAN_CS, BSPL_VAL, BSPL_DER, DP_VV_V
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, IXMN8, VTD_STA_INDEX, VTD_SOU_INDEX 
!
! --- Check VTD
!
      IF ( .NOT. PROBE_WRITE_ADDRESS ( VTD%STATUS ) ) THEN
           CALL ERR_LOG ( 2811, IUER, 'VTD_DOPPLER', 'Object VTD is not '// &
     &         'accessible for writing. It is an indication of a very '// &
     &         'serious error. Please check the argument list' )
           RETURN
      END IF
!
! --- Check whether the station/source catalogues were loaded
!
      IF ( VTD%STATUS .NE. VTD__LOAD ) THEN
           CALL ERR_LOG ( 2812, IUER, 'VTD_DOPPLER', 'Routine VTD_LOAD was '// &
     &         'not executed before the call of VTD_DOPPLER' )
           RETURN
      END IF
!
! --- Check whether the station/source catalogues were loaded
!
      IF ( VTD%L_NZO .EQ. 0 ) THEN
           CALL ERR_LOG ( 2813, IUER, 'VTD_DOPPLER', 'The catalogue of '// &
     &         'the near-zone objects was not loaded before the call of '// &
     &         'VTD_DOPPLER' )
           RETURN
      END IF
!
! --- Compute parameters which depends only on time, but do not depend on
! --- station
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_MOMENT ( SOU_NAM, MJD, TAI, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2814, IUER, 'VTD_DOPPLER', 'Error in an attempt '// &
     &         'to compute station-independent time varying intermediate '// &
     &         'quantities' )
           RETURN
      END IF
!
      IF ( VTD%CONF%IVRB .GE. 1 ) THEN
           WRITE ( 6, 210 ) 'TRS_TO_CRS  = ', &
     &                      VTD%MOM%TRS_TO_CRS(1,1), &
     &                      VTD%MOM%TRS_TO_CRS(2,1), &
     &                      VTD%MOM%TRS_TO_CRS(3,1), &
     &                      'TRS_TO_CRS  = ',        &
     &                      VTD%MOM%TRS_TO_CRS(1,2), &
     &                      VTD%MOM%TRS_TO_CRS(2,2), &
     &                      VTD%MOM%TRS_TO_CRS(3,2), &
     &                      'TRS_TO_CRS  = ',        &
     &                      VTD%MOM%TRS_TO_CRS(1,3), &
     &                      VTD%MOM%TRS_TO_CRS(2,3), &
     &                      VTD%MOM%TRS_TO_CRS(3,3)
           WRITE ( 6, 210 ) 'TRS_TO_CRS1 = ',             &
     &                      VTD%MOM%TRS_TO_CRS_DER1(1,1), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(2,1), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(3,1), &
     &                      'TRS_TO_CRS1 = ',             &
     &                      VTD%MOM%TRS_TO_CRS_DER1(1,2), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(2,2), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(3,2), &
     &                      'TRS_TO_CRS1 = ',             &
     &                      VTD%MOM%TRS_TO_CRS_DER1(1,3), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(2,3), &
     &                      VTD%MOM%TRS_TO_CRS_DER1(3,3)
   210     FORMAT ( 3( A, 3(F18.15,2X)/) )
      END IF
!
      ISTA = VTD_STA_INDEX ( VTD, STA_NAM )
      IF ( ISTA .LE. 0 ) THEN
           CALL ERR_LOG ( 2815, IUER, 'VTD_DOPPLER', 'Cannot find station '// &
     &          'name '//STA_NAM//' in the list of loaded stations. Please '// &
     &          'check the list of stations in the file passed to VTD_LOAD' )
           RETURN 
      END IF 
!
      ISOU = VTD_SOU_INDEX ( VTD, SOU_NAM  )
      IF ( ISOU .EQ. 0 ) THEN
           CALL ERR_LOG ( 2816,  IUER, 'VTD_DOPPLER', 'Source '//SOU_NAM// &
     &         ' was not found in the list of sources' )
           RETURN
      END IF
!
      CALL NOUT_R8 ( 3, AXOF_CRS )
      CALL NOUT_R8 ( 3, AXOF_TRS )
      CALL NOUT_R8 ( 3, AXOF_CRS_RATE )
      CALL NOUT_R8 ( 3, AXOF_TRS_RATE )
      AXOF_UP = 0.0D0
      AXOF_UP_RATE = 0.0D0
      IF ( ( VTD%CONF%AXOF_MODEL .EQ. VTD__YES  .OR. &
     &       VTD%CONF%AXOF_MODEL .EQ. VTD__CALC      ) .AND.  &
     &     DABS ( VTD%STA(ISTA)%AXIS_OFFSET ) .GT. 1.D-4 ) THEN
!
! -------- Compute the vector of antenna's axis offset
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_AXOF ( VTD, ISTA, ISOU, AXOF_CRS, &
     &                     AXOF_CRS_RATE, AXOF_UP, AXOF_UP_RATE, &
     &                     AVEC_CRS, AVEC_CRS_RATE, UNIT_AXOF_CRS, &
     &                     UNIT_AXOF_CRS_RATE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2817, IUER, 'VTD_DOPPLER', 'Error in an '// &
     &              'attempt to compute antenna axis offset for station '// &
     &               VTD%STA(ISTA)%IVS_NAME )
                RETURN
           END IF
!
! -------- Transform the vector of antenna axis offset from CRS to TRS
!
           IER = -1
           CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                           3, AXOF_CRS, &
     &                           3, AXOF_TRS, IER )
           IF ( VTD%CONF%FL_RATE ) THEN
                IER = -1
                CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                                3, AXOF_CRS_RATE,      &
     &                                3, AXOF_TRS_RATE,      IER )
                IER = -1
                CALL MUL_MV_TV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                                3, AXOF_CRS, &
     &                                3, VEC1, IER )
                CALL ADD_VV ( 3, AXOF_TRS_RATE, VEC1 ) 
           END IF
!
           IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                WRITE ( 6, 110 ) VTD%STA(ISTA)%IVS_NAME, &
     &                           AXOF_CRS(1:3)
 110            FORMAT ( 1X,'Axof vector      ',A, ' (CRS): ', &
     &                      3(F12.9,', '),' m ' )
           END IF
      END IF
!
! --- Compute displacement caused by the solid Earth tides
!
      IF ( VTD%CONF%STD_2ND_MODEL == VTD__NONE .OR. &
     &     VTD%STA(ISTA)%STA_TYP .NE. VTD__GR       ) THEN
           CALL NOUT_R8 ( 3, STD_REN )
           CALL NOUT_R8 ( 3, STD_TRS )
           CALL NOUT_R8 ( 3, STV_REN )
           CALL NOUT_R8 ( 3, STV_TRS )
         ELSE
           CALL SOTID_DSP ( VTD%TIDCNF_STD, VTD%TIMTID, VTD%STATID(ISTA), &
     &                      STD_REN, STV_REN )
!
! -------- Transform the displacement vector and its first time derivative
! -------- from REN to TRS
!
           CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%REN_TO_TRS, &
     &                           3, STD_REN, 3, STD_TRS, IER )
           CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%REN_TO_TRS, &
     &                           3, STV_REN, 3, STV_TRS, IER )
           IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                                3, STD_TRS, 3, STD_CRS, IER )
                CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                                3, STV_TRS, 3, STV_CRS, IER )
!
                WRITE ( 6, 120 ) VTD%STA(ISTA)%IVS_NAME, 'REN', STD_REN(1:3)
                WRITE ( 6, 120 ) VTD%STA(ISTA)%IVS_NAME, 'TRS', STD_TRS(1:3)
                WRITE ( 6, 120 ) VTD%STA(ISTA)%IVS_NAME, 'CRS', STD_CRS(1:3)
 120            FORMAT ( 1X,'Solid Earth tide ',A, ' (',A,'): ', &
     &                    3(F12.9,', '),' m ' )
!
                CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                                3, STD_TRS, 3, STD_CRS_DER1, IER )
!
                WRITE ( 6, 130 ) VTD%STA(ISTA)%IVS_NAME, 'REN', STV_REN(1:3)
                WRITE ( 6, 130 ) VTD%STA(ISTA)%IVS_NAME, 'TRS', STV_TRS(1:3)
                WRITE ( 6, 130 ) VTD%STA(ISTA)%IVS_NAME, 'CRS', &
     &                           (STV_CRS(NN)+STD_CRS_DER1(NN), NN=1,3)
!
 130            FORMAT ( 1X,'Rate of std      ',A, ' (',A,'): ', &
     &                   3(1PD12.5,', '),' m/' )
           END IF
      END IF
!
! --- Compute displacements caused by pole tide
!
      IF ( VTD%CONF%PTD_MODEL == VTD__NONE    .OR. &
     &     VTD%STA(ISTA)%STA_TYP .NE. VTD__GR      ) THEN
!
           CALL NOUT_R8 ( 3, PTD_REN )
           CALL NOUT_R8 ( 3, PTD_TRS )
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_POTID_DSP ( VTD, ISTA, PTD_REN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2818, IUER, 'VTD_DOPPLER', 'Error in an '// &
     &              'attempt to compute displacements caused by pole '// &
     &              'tide at station '//VTD%STA(ISTA)%IVS_NAME )
                RETURN
           END IF
!
           IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                WRITE ( 6, 140 ) VTD%STA(ISTA)%IVS_NAME, PTD_REN(1:3)
 140            FORMAT ( 1X,'Pole tide for    ',A, ' (REN): ', &
     &                   3(F12.9,', '),' m ' )
           END IF
!
! -------- Transform the displacement vector from REN to TRS
!
           CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%REN_TO_TRS, &
     &                           3, PTD_REN(1:3), 3, PTD_TRS(1:3), IER )
      END IF
!
! --- Compute position variations for various other numerical models
!
      CALL NOUT_R8 ( 3, PSD_TRS_SUM )
      CALL NOUT_R8 ( 3, PSV_TRS_SUM )
      DO 440 J4=1,VTD__M_PSF
         IF ( ILEN(VTD%CONF%POSVAR_FIL(J4)) .GT. 0  .OR. &
     &        VTD%STA(ISTA)%STA_TYP .NE. VTD__GR         ) THEN
!
              CALL ERR_PASS ( IUER, IER )
              CALL VTD_GET_POSVAR ( VTD, ISTA, J4, PSD_TRS, PSV_TRS, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 2819, IUER, 'VTD_DOPPLER', 'Error in an '// &
     &                 'attempt to compute displacements caused by '// &
     &                 'position variation at station '// &
     &                  VTD%STA(ISTA)%IVS_NAME )
                   RETURN
              END IF
!
              IF ( VTD%CONF%IVRB .GE. 1 ) THEN
                   CALL MUL_MV_TV_V ( 3, 3, VTD%STA(ISTA)%REN_TO_TRS, &
     &                                3, PSD_TRS, 3, PSD_REN, IER )
                   CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, 3, &
     &                                PSD_TRS, 3, PSD_CRS, IER )
                   WRITE ( 6, 150 ) J4, VTD%STA(ISTA)%IVS_NAME, 'TRS', &
     &                              (PSD_TRS(NN), NN=1,3)
                   WRITE ( 6, 150 ) J4, VTD%STA(ISTA)%IVS_NAME, 'REN', &
     &                              (PSD_REN(NN), NN=1,3)
                   WRITE ( 6, 150 ) J4, VTD%STA(ISTA)%IVS_NAME, 'CRS', &
     &                              (PSD_CRS(NN), NN=1,3)
 150               FORMAT ( 1X,'Pos.varitaions ',I1, 1X, A, ' (',A,'): ', &
     &                          3(F12.9,', '),' m ' )
!
                   CALL MUL_MV_TV_V ( 3, 3, VTD%STA(ISTA)%REN_TO_TRS, &
     &                                3, PSV_TRS, 3, PSV_REN, IER )
                   WRITE ( 6, 160 ) J4, VTD%STA(ISTA)%IVS_NAME, 'TRS', &
     &                              (PSV_TRS(NN), NN=1,3)
                   WRITE ( 6, 160 ) J4, VTD%STA(ISTA)%IVS_NAME, 'REN', &
     &                              (PSV_REN(NN), NN=1,3)
 160               FORMAT ( 1X,'Rate pos.var.  ',I1, 1X, A, ' (',A,'): ', &
     &                       3(1PD12.5,', '),' m/' )
              END IF
!
! ----------- Add displacements due to this model to the total displacements
!
              CALL ADD_VV ( 3, PSD_TRS_SUM, PSD_TRS )
              CALL ADD_VV ( 3, PSV_TRS_SUM, PSV_TRS )
         END IF
 440  CONTINUE
!
! --- Compute durataion of time from the site position reference epoch till now
!
      TIME_REF =   (VTD%MOM%MJD - VTD%STA(ISTA)%MJD_REF)*86400.0D0 &
     &           + (VTD%MOM%TAI - VTD%STA(ISTA)%TAI_REF)
      DO 450 J5=1,3
         VTD%STA(ISTA)%MOM_COO_TRS(J5) = VTD%STA(ISTA)%BEG_TRS(J5) &
     &                   + VTD%STA(ISTA)%VEL_TRS(J5)*TIME_REF  &
     &                   + AXOF_TRS(J5)                         &
     &                   + STD_TRS(J5)                          &
     &                   + PTD_TRS(J5)                          &
     &                   + PSD_TRS_SUM(J5)
         VTD%STA(ISTA)%MOM_VEL_TRS(J5) =   &
     &                      VTD%STA(ISTA)%VEL_TRS(J5) &
     &                    + AXOF_TRS_RATE(J5)          &
     &                    + STV_TRS(J5)                &
     &                    + PSV_TRS_SUM(J5)
         IF ( VTD%CONF%IVRB .GE. 1 ) THEN
              WRITE ( 6, 170 ) VTD%MOM%MJD, VTD%MOM%TAI, J5, &
     &                         VTD%STA(ISTA)%IVS_NAME, 'TRS', &
     &                         VTD%STA(ISTA)%MOM_COO_TRS(J5), & 
     &                         VTD%STA(ISTA)%MOM_VEL_TRS(J5) 
 170          FORMAT ( 1X,'MJD= ',I5,' TAI= ', F8.2, ' pos. ',I1, 1X, &
     &                 A, ' (',A,'): ', F14.5,' ', F12.7 )
         ENDIF
 450  CONTINUE
!
! --- Transform the total vector of instantaneous site coordinates
! --- from TRS to CRS
!
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                      3, VTD%STA(ISTA)%MOM_COO_TRS, &
     &                      3, VTD%STA(ISTA)%COO_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                      3, VTD%STA(ISTA)%MOM_VEL_TRS, &
     &                      3, RT_CRS, IER )
!
! --- Compute the vector of local zenith in CRS and then
! --- rate of its change
!
      UP_UEN(1) = 1.0D0
      UP_UEN(2) = 0.0D0
      UP_UEN(3) = 0.0D0
      CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS,  &
     &                      3, UP_UEN, 3, UP_TRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                      3, UP_TRS, 3, UP_CRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                      3, UP_TRS, 3, UP_RATE_CRS, IER )
      NORTH_UEN(1) = 0.0D0
      NORTH_UEN(2) = 0.0D0
      NORTH_UEN(3) = 1.0D0
      CALL MUL_MV_IV_V ( 3, 3, VTD%STA(ISTA)%UEN_TO_TRS,  &
     &                      3, NORTH_UEN, 3, NORTH_TRS, IER )
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS, &
     &                      3, NORTH_TRS, 3, NORTH_CRS, IER )
!
! --- Compute velocity of the station
! --- First: TRS_TO_CRS' * R_VEC
!
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER1, &
     &                      3, VTD%STA(ISTA)%MOM_COO_TRS, &
     &                      3, VTD%STA(ISTA)%VEL_CRS, IER )
!
! --- Second: TRS_TO_CRS * R_VEC'
!
      CALL ADD_VV ( 3, VTD%STA(ISTA)%VEL_CRS, RT_CRS )
!
! --- Compute acceleration of the station as TRS_TO_CRS'' * R_VEC
!
      CALL MUL_MV_IV_V ( 3, 3, VTD%MOM%TRS_TO_CRS_DER2, &
     &                      3, VTD%STA(ISTA)%MOM_COO_TRS, &
     &                      3, VTD%STA(ISTA)%ACC_CRS, IER )
!
! --- Compute geometric elevation angle (no refraction, but aberration)
!
      SV = DP_VV_V  ( 3, VTD%SOU(ISOU)%S_CRS, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART))
      CALL ADDC_VV  ( 3, 1.0D0/VTD__C, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                   (1.0D0 - SV/VTD__C), VTD%SOU(ISOU)%S_CRS, &
     &                   S_APP )
      CALL NORM_VEC ( 3, S_APP, RD )
      VTD%STA(ISTA)%ELEV = DASIN ( DP_VV_V ( 3, UP_CRS, S_APP ) )
!
! --- ... and its rate of change
!
      VTD%STA(ISTA)%ELEV_DER = DP_VV_V( 3, UP_RATE_CRS, S_APP)/ &
     &           DSQRT ( 1.0D0 - DP_VV_V( 3, UP_CRS, S_APP)**2 )
!
! --- Now let us compute azimuth. In order to do it first compute 
! --- the projection of the source vector to the horizontal plane
!
      CALL ADDC_VV ( 3, 1.0D0, S_APP, -DP_VV_V( 3, UP_CRS, S_APP ), UP_CRS, &
     &                  VEC_PROJ_EN )
      CALL NORM_VEC ( 3, VEC_PROJ_EN, VAL )
!
! --- Then compute the north projection of that projection ...
!
      N_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, NORTH_CRS )
!
! --- ... and east projection of that projection.
!
      CALL VM83 ( NORTH_CRS, UP_CRS, EAST_CRS )
      E_PROJ = DP_VV_V ( 3, VEC_PROJ_EN, EAST_CRS  )
!
! --- From these two projections we get the azimuth. Ugh!
!
      VTD%STA(ISTA)%AZ = ATAN_CS ( N_PROJ, E_PROJ )
!
! --- Compute troposperic path delay
!
      IF ( VTD%STA(ISTA)%STA_TYP == VTD__GR ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_TROPDEL ( VTD, ISTA,                     &
     &                        VTD%STA(ISTA)%TROP_HZD,        &
     &                        VTD%STA(ISTA)%TROP_WZD,        &
     &                        VTD%STA(ISTA)%TROP_DEL,        &
     &                        VTD%STA(ISTA)%TROP_DEL_RATE,   &
     &                        VTD%STA(ISTA)%TROP_ZEN_DER,    &
     &                        VTD%STA(ISTA)%TROP_ZEN_RATE,   &
     &                        VTD%STA(ISTA)%TROP_TILT_N_DER, &
     &                        VTD%STA(ISTA)%TROP_TILT_E_DER, &
     &                        SUR_PRS,                       &
     &                        SUR_PWP,                       &
     &                        SUR_TEM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2820, IUER, 'VTD_DOPPLER', 'Error in an '// &
     &              'attempt to compute tropospheric delay at station '// &
     &               VTD%STA(ISTA)%IVS_NAME )
                RETURN
           END IF
         ELSE
           VTD%STA(ISTA)%TROP_HZD        = 0.0D0
           VTD%STA(ISTA)%TROP_WZD        = 0.0D0
           VTD%STA(ISTA)%TROP_DEL        = 0.0D0
           VTD%STA(ISTA)%TROP_DEL_RATE   = 0.0D0
           VTD%STA(ISTA)%TROP_ZEN_DER    = 0.0D0
           VTD%STA(ISTA)%TROP_TILT_N_DER = 0.0D0
           VTD%STA(ISTA)%TROP_TILT_E_DER = 0.0D0
      END IF
!
      IF ( VTD%CONF%IVRB .GE. 1 ) THEN
           WRITE ( 6, 180 ) VTD%STA(ISTA)%IVS_NAME, &
     &                      VTD%STA(ISTA)%ELEV/DEG__TO__RAD, &
     &                      VTD%STA(ISTA)%TROP_HZD*1.D9, &
     &                      VTD%STA(ISTA)%TROP_DEL*1.D9, &
     &                      VTD%STA(ISTA)%ATM_PRES, &
     &                      VTD%STA(ISTA)%AIR_TEMP, &
     &                      VTD%STA(ISTA)%LAT_GDT, &
     &                      VTD%STA(ISTA)%HEI_ELL, &
     &                      VTD%STA(ISTA)%TROP_ZEN_DER, &
     &                      VTD%STA(ISTA)%AZ/DEG__TO__RAD 
  180      FORMAT ( 1X, A, ' Elevation: ', F10.6,' deg ', &
     &                     ' TROP_HZD = ', F9.4,' TROP_DEL = ',F9.4/ &
     &                     '          Pres=', F8.1,' Temp=',F7.3, &
     &                  9X,' Azimuth: ',F11.6, ' deg' )
      END IF
!
! --- Compute the distance between the EArth and the Sun
!
      DIST_SUN_EARTH = DSQRT (   &
     &    (VTD%MOM%PLAN(1,VTD__COO,VTD__EART) - VTD%MOM%PLAN(1,VTD__COO,VTD__SUN))**2 &
     &  + (VTD%MOM%PLAN(2,VTD__COO,VTD__EART) - VTD%MOM%PLAN(2,VTD__COO,VTD__SUN))**2 &
     &  + (VTD%MOM%PLAN(3,VTD__COO,VTD__EART) - VTD%MOM%PLAN(3,VTD__COO,VTD__SUN))**2 &
     &                       )
!
! --- Compute intermediate quanities for the relativistic coordinate 
! --- transformation
!
      U_TERM = VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 - VTD__LB 
      V_TERM = 0.5D0*DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                         VTD%MOM%PLAN(1,VTD__VEL,VTD__EART) )/VTD__C**2
      VV = DP_VV_V ( 3, VTD%STA(ISTA)%VEL_CRS, &
     &                  VTD%MOM%PLAN(1,VTD__VEL,VTD__EART) )
!
      PAR0 =  1.D0 - VTD__GM(VTD__SUN)/DIST_SUN_EARTH/VTD__C**2 &
     &             - VTD__LB 
      PAR1 = -DP_VV_V ( 3, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                     VTD%STA(ISTA)%COO_CRS )/ 2.0D0/VTD__C**2
!
! --- Get position of the observer
!
      CALL ADDC_VV  ( 3, PAR0, VTD%STA(ISTA)%COO_CRS, &
     &                   PAR1, VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &                   COO_O )
      CALL ADD_VV   ( 3, COO_O, VTD%MOM%PLAN(1,VTD__COO,VTD__EART) )
!
! --- Get the velocity of the observer
!
      CALL ADDC_VV ( 3, (1.0D0 - U_TERM - V_TERM - VV/VTD__C**2), &
     &                  VTD%STA(ISTA)%VEL_CRS, &
     &                  (1.0D0 - 0.5D0*VV/VTD__C**2), &
     &                  VTD%MOM%PLAN(1,VTD__VEL,VTD__EART), &
     &               VEL_O )
!
      IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__SS ) THEN
           TIM_ARG = VTD%MOM%TDB
         ELSE IF ( VTD%SOU(ISOU)%OBJ_TYPE == VTD__ES ) THEN
           TIM_ARG = VTD%MOM%TDT
      END IF
!
! --- Compute coordaintes and velocities of the observer at the moment 
! --- of time of photon emission by solbing the null-cone equation with
! --- severla iterations
!
      TAU_EMIT = 0.0D0 ! Initial value of travel time emitter-observer
      DO 460 J6=1,VTD__NZO_ITER
!
! ------ Retarted time tag
!
         TARG = (VTD%MOM%MJD - VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG)*86400.0D0 + &
     &          (TIM_ARG     - VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG) &
     &          + TAU_EMIT  ! NB: sign for tau_emit
!
! ------ Knot of the spline
!
         KNOT = IXMN8 ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                  VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), TARG )
         IF ( KNOT .EQ. -1 ) THEN
              IER = -1
              STR(1:28)  = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
              STR(31:58) = MJDSEC_TO_DATE ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG, &
     &                                      VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG, IER )
              CALL ERR_LOG ( 2821, IUER, 'VTD_DOPPLER', 'Moment of time '// &
     &             STR(1:28)//' at TAI precedes the first epoch for the '// &
     &            'near zone object '//VTD%SOU(ISOU)%NZO_NAME//' -- '// &
     &             STR(31:58) )
              RETURN 
         END IF
!
         IF ( KNOT .EQ. -2 ) THEN
              IER = -1
              STR(1:28)  = MJDSEC_TO_DATE ( VTD%MOM%MJD, VTD%MOM%TAI, IER )
              STR(31:53) = MJDSEC_TO_DATE ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%MJD_BEG, &
     &                                      VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_BEG + &
     &                     VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL), IER )
              CALL ERR_LOG ( 2822, IUER, 'VTD_DOPPLER', 'Moment of time '// &
     &             STR(1:28)//' at TAI is after the last epoch for the '// &
     &            'near zone object '//VTD%SOU(ISOU)%NZO_NAME//' -- '// &
     &             STR(31:58) )
              RETURN 
         END IF
!
! ------ Cycle over three components of positions and velocities
!
         DO 470 J7=1,3
            COO_E(J7) = 0.0D0
            VEL_E(J7) = 0.0D0
!
! --------- Cycle over knots of the B-spline
!
            DO 480 J8=KNOT-VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL,KNOT
               COO_E(J7) = COO_E(J7) + &
     &             VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J8,J7)* &
     &             BSPL_VAL ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                        VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
     &                        VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J8, TARG )
               VEL_E(J7) = VEL_E(J7) + &
     &             VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%SPL_ARR(J8,J7)* &
     &             BSPL_DER ( VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%NOD_SPL, &
     &                        VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%TIM_ARR(1), &
     &                        VTD%NZO(VTD%SOU(ISOU)%IND_NZO)%DEG_SPL, J8, TARG )
 480        CONTINUE 
 470     CONTINUE 
!
! ------ Get OC_LEN -- distance between the observer and the emitter
!
         CALL SUB_VV_V ( 3, COO_O, COO_E, COO_OE )
         CALL NORM_VEC ( 3, COO_OE, OE_LEN )
!
! ------ Compute travel time between the observer and the emitter
!
         TAU_EMIT = -OE_LEN/VTD__C  ! NB: sign for tau_emit
 460  CONTINUE 
!
! --- Compute contribution CO and CE terms of gravitational frequency shift
!
      CO_ACC = 0.0D0
      CE_ACC = 0.0D0
!
! --- Cycle over planets
!
      DO 490 J9=1,VTD__M_PLA
!
! ------ Get cordinates of the planet at the moment of time of arrival of
! ------ the photon at the station
!
         CALL COPY_R8  ( 3, VTD%MOM%PLAN(1,VTD__COO,J9), COO_PLAN )
         CALL COPY_R8  ( 3, VTD%MOM%PLAN(1,VTD__VEL,J9), VEL_PLAN )
!
! ------ Solve light cone equation by iteration
! ------ R_VEC -- vetor from the gravitating body to the observer
!
         CALL SUB_VV_V ( 3, COO_O, COO_PLAN, R_VEC )
         CALL NORM_VEC ( 3, R_VEC, R_LEN )
         TAU_RETR = -R_LEN/VTD__C ! Get retardation time 
!
! ------ Compute position of the gravitating body at the retarded moment 
! ------ of time
!
         CALL ADDC_VV  ( 3, 1.0D0, VTD%MOM%PLAN(1,VTD__COO,J9), &
     &                   TAU_RETR, VEL_PLAN, COO_PLAN )
         CALL SUB_VV_V ( 3, COO_O, COO_PLAN, R_O )
         CALL SUB_VV_V ( 3, COO_E, COO_PLAN, R_E )
         CALL NORM_VEC ( 3, R_O, RO_LEN ) 
         CALL NORM_VEC ( 3, R_E, RE_LEN ) 
         CO_ACC = CO_ACC + VTD__GM(J9)/(RO_LEN*VTD__C**2)
         CE_ACC = CE_ACC + VTD__GM(J9)/(RE_LEN*VTD__C**2)
 490  CONTINUE 
!
! --- Get the normalized baricentric vector of the emitter
!
      CALL NORM_VEC ( 3, COO_O, O_LEN )
      CALL NORM_VEC ( 3, COO_E, E_LEN )
!
!@      A_O = 1.0D0 - DP_VV_V ( 3, VEL_O, COO_O )/VTD__C
!@      A_E = 1.0D0 - DP_VV_V ( 3, VEL_E, COO_E )/VTD__C
!@      B_O = DSQRT ( 1.0D0 - DP_VV_V ( 3, VEL_O, VEL_O )/VTD__C**2 )
!@      B_E = DSQRT ( 1.0D0 - DP_VV_V ( 3, VEL_E, VEL_E )/VTD__C**2 )
!@      C_O = 1.0D0 - CO_ACC
!@      C_E = 1.0D0 - CE_ACC
!@!
!@!      D_FRQ = (A_E/A_O)*(B_O/B_E)*(C_E/C_O) - 1.0D0
!
! --- Compute the Doppler frequency shift parameter
!
      A_O = DP_VV_V ( 3, VEL_O, COO_O )/VTD__C
      A_E = DP_VV_V ( 3, VEL_E, COO_E )/VTD__C
      B_O = DSQRT ( DP_VV_V ( 3, VEL_O, VEL_O )/VTD__C**2 )
      B_E = DSQRT ( DP_VV_V ( 3, VEL_E, VEL_E )/VTD__C**2 )
      C_O = DSQRT ( CO_ACC )
      C_E = DSQRT ( CE_ACC )
!
      D_FRQ = A_O - A_E + &
     &       (A_O**2 - A_E*A_O - 0.5D0*B_O**2 + 0.5D0*B_E**2 - C_E**2 + C_O**2 ) + &
     &       (A_O**3 - A_E*A_O**2 - 0.5D0*A_O*B_O**2 + 0.5D0*A_O*B_E**2 + &
     &         0.5D0*A_E*B_O**2 - 0.5D0*A_E*B_E**2 - A_O*C_E**2 + A_E*C_E**2 + &
     &         A_O*C_O**2 - A_E*C_O**2 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_DOPPLER  !#!#
