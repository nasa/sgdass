      SUBROUTINE PRE_VTD ( VTD, VTD_STATUS_FIELD, VTD_CONF_FILE, &
     &                     FL_VTD_IONO, VTD_IONO_SCALE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PRE_VTD  performs initializataion of VTD data structures   *
! *   and some Solve arrays whcih are needed for computing theoretical   *
! *   path delay, partial derivatvies and information about apriori      *
! *   coordaintes.                                                       *
! *                                                                      *
! *   FL_VTD_IONO returns .TRUE. if the ionospheric calibration from     *
! *   GPS TEC model will be computed during data reduction.              *
! *                                                                      *
! *  ### 09-FEB-2006     PRE_VTD   v1.9 (c)  L. Petrov  07-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'vtd.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'precm.i'
      INCLUDE   'prfil.i'
      INTEGER*4  VTD_STATUS_FIELD, IUER 
      TYPE     ( VTD__TYPE     ) :: VTD
      TYPE     ( VTD__OBS_TYPE ) :: OBS_TYP
      REAL*8     VTD_IONO_SCALE
      REAL*8       JUL_YEAR__TO__SEC
      PARAMETER  ( JUL_YEAR__TO__SEC = 365.25D0*86400.0D0 ) ! Julian year
      CHARACTER  VTD_CONF_FILE*(*)
      LOGICAL*1  FL_VTD_IONO
      CHARACTER  STR*80
      INTEGER*2  ICONT_I2, IERR_I2
      INTEGER*4  J0, J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           IND_STA, L_STA, &
     &           L_SOU, MJD_TAB, MJD_TAB_FIRST, MJD_TAB_LAST, IER
      INTEGER*4  MJD_BEG, MJD_END
      REAL*8     TAI_BEG, TAI_END, FJDOBS, LJDOBS, TIME_REF, CROSS_NUT_E3 
      REAL*8     E1, E2, E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &           DPSI_WAHR_BEG,    DPSI_WAHR_END,    &
     &           DEPS_WAHR_BEG,    DEPS_WAHR_END,    &
     &           DPSI_APR_BEG,     DPSI_APR_END,     &
     &           DEPS_APR_BEG,     DEPS_APR_END, &
     &           TDB_BEG, TARG_TDB_BEG, TDB_END, TARG_TDB_END, &
     &           JDPTB(MAX_EROT_VALUES)
      CHARACTER  C_STA(MAX_ARC_STA)*8, C_SOU(MAX_ARC_SRC)*8, STA_NAME*8
      LOGICAL*4  FL_SAVED_ADR 
      ADDRESS__TYPE :: VTD_ADR_SAVE, TRP_ADR_SAVE
      INTEGER*4  NF_GOOD, NL_GOOD, STS_TRP_SAVE, IDBEND_SAVE(MAX_DBS)
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: MJD_SEC_TO_JD 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Get start and stop time
!
      CALL OBSTM ( FJDOBS, LJDOBS )
      CALL JD_TO_MJD_SEC ( FJDOBS, MJD_BEG, TAI_BEG )
      CALL JD_TO_MJD_SEC ( LJDOBS, MJD_END, TAI_END )
!
! --- Start and stop time are first in UTC, second are unreliable.
! --- We increase the time span by 10 minutes for safety
!
      TAI_BEG = TAI_BEG - 300.0D0
      TAI_END = TAI_END + 300.0D0
!
      L_STA = NUMSTA
      L_SOU = NUMSTR
      CALL LIB$MOVC3 ( L_STA*LEN(C_STA(1)), ISITN_CHR, C_STA )
      CALL LIB$MOVC3 ( L_SOU*LEN(C_SOU(1)), ISTRN_CHR, C_SOU )
      DO 430 J3=1,L_STA
         CALL VTD_NAME_REPAIR ( C_STA(J3) )
 430  CONTINUE 
!
      IF ( VTD%SESS_NAME == DBNAME_CH .AND. VTD%STATUS == VTD__LOAD ) THEN
           CONTINUE 
         ELSE 
           IF ( VTD%STATUS == VTD__LOAD .OR. &
     &          VTD%STATUS == VTD__ALLC      ) THEN
!
! ------------- It may happen that some field of VTD are already loaded, 
! ------------- for example, POSVAR fields, then we need to free the allocated
! ------------- memory
!
! ------------- Release dynamicaly allocated memory
!
                CALL ERR_PASS ( IUER, IER )
                CALL VTD_QUIT ( VTD, IER ) 
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8271, IUER, 'PRE_VTD', 'Failure in attempt '// &
     &                   'to free memory previously allocated in VTD' )
                     RETURN 
                END IF
           END IF
!
! -------- Initialization of VTD
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_INIT ( VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8271, IUER, 'PRE_VTD', 'Failure to initialize '// &
     &              'VTD object' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_CONF ( VTD_CONF_FILE, VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8272, IUER, 'PRE_VTD', 'Failure to load VTD '// &
     &              'configuration file '//VTD_CONF_FILE )
                RETURN 
           END IF
!
! -------- Load catalogues, ephemerides, EOP series and other data files
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD  ( VTD, L_STA, C_STA, L_SOU, C_SOU, MJD_BEG, TAI_BEG, &
     &                      MJD_END, TAI_END, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8273, IUER, 'PRE_VTD', 'Error in an '// &
     &              'attempt to load the data into VTD data structure' )
                CALL EXIT ( 1 ) 
           END IF
      END IF
      IF ( VTD%CONF%IONO_MODEL == VIO__GNSS_TEC    .OR. &
     &     VTD%CONF%IONO_MODEL == VIO__GNSS_TEC_MOD     ) THEN
           FL_VTD_IONO = .TRUE.
         ELSE 
           FL_VTD_IONO = .FALSE.
      END IF
!
      VTD_ADR_SAVE = VTD_ADR
      TRP_ADR_SAVE = ADR_TRP
      STS_TRP_SAVE = STS_TRP
      CALL USE_GLBFIL_4 ( 'ORC' )
      VTD_ADR = VTD_ADR_SAVE 
      ADR_TRP = TRP_ADR_SAVE 
      STS_TRP = STS_TRP_SAVE 
      VSITEC_TIM_REF_MJD = VTD%STA(1)%MJD_REF + VTD%STA(1)%TAI_REF/86400.0D0 + 2400000.5D0
!
! --- Update station coordinates
!
      DO 440 J4=1,L_STA
         IND_STA = 0
         DO 450 J5=1,VTD%L_STA
            TIME_REF = (   (MJD_BEG - VTD%STA(J5)%MJD_REF)*86400.0D0   &
     &                   + (TAI_BEG - VTD%STA(J5)%TAI_REF)             &
     &                   + (MJD_BEG - VTD%STA(J5)%MJD_REF)*86400.0D0   &
     &                   + (TAI_BEG - VTD%STA(J5)%TAI_REF)           )/2.0D0
            STA_NAME = ISITN_CHR(J4)
            CALL VTD_NAME_REPAIR ( STA_NAME )
            IF ( STA_NAME == VTD%STA(J5)%IVS_NAME ) THEN
                 IND_STA = J5
                 VSITEC(1,J4) = VTD%STA(J5)%COO_TRS(1,1)
                 VSITEC(2,J4) = VTD%STA(J5)%COO_TRS(2,1)
                 VSITEC(3,J4) = VTD%STA(J5)%COO_TRS(3,1)
!
                 VSITEV(1,J4) = VTD%STA(J5)%VEL_TRS(1)*JUL_YEAR__TO__SEC
                 VSITEV(2,J4) = VTD%STA(J5)%VEL_TRS(2)*JUL_YEAR__TO__SEC
                 VSITEV(3,J4) = VTD%STA(J5)%VEL_TRS(3)*JUL_YEAR__TO__SEC
                 VAXOF(J4)    = VTD%STA(J5)%AXIS_OFFSET
!
                 IF ( SOLTYP_CH == 'I'  .OR.  .NOT. KBATCH ) THEN
                      NVSITEC(1,J4) = VTD%STA(J5)%COO_TRS(1,1) + &
     &                                VTD%STA(J5)%VEL_TRS(1)*TIME_REF
                      NVSITEC(2,J4) = VTD%STA(J5)%COO_TRS(2,1) + &
     &                                VTD%STA(J5)%VEL_TRS(2)*TIME_REF
                      NVSITEC(3,J4) = VTD%STA(J5)%COO_TRS(3,1) + &
     &                                VTD%STA(J5)%VEL_TRS(3)*TIME_REF
                 END IF
!
                 WRITE ( UNIT=LMONUMENTS(J4)(1:4), FMT='(I4)', &
     &                   IOSTAT=IER ) VTD%STA(J5)%CDP_NUMBER
            END IF
 450     CONTINUE 
!
         IF ( IND_STA == 0 ) THEN
              CALL ERR_LOG ( 8274, IUER, 'PRE_VTD', 'Trap of internal '// &
     &            'control: station '//ISITN_CHR(J4)//' was not found '// &
     &            'in VTD data structure' )
              RETURN 
         END IF
!
         IF ( J4 == 1 ) THEN
              ICONT_I2 = 1 
            ELSE 
              ICONT_I2 = 0
         END IF
!
         CALL CLRCH ( STR )
         CALL GETCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
         IF ( IERR_I2 .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(IERR_I2), STR )
              CALL ERR_LOG ( 8275, IUER, 'PRE_VTD', 'Error in reading '// &
     &            'SITE namefile card: '//STR )
              RETURN 
         END IF
         IND_STA = 0
         STA_NAME = STR(6:13)
         DO 460 J6=1,VTD%L_STA
            CALL VTD_NAME_REPAIR ( STA_NAME )
            IF ( STA_NAME == VTD%STA(J6)%IVS_NAME ) THEN
                 IND_STA = J6
            END IF
 460     CONTINUE 
!
         IF ( STA_NAME == VTD%STA(IND_STA)%IVS_NAME ) THEN
              IF ( VTD%STA(IND_STA)%N_ECC > 1 ) THEN
!@                   WRITE ( 6, * ) ' VTD%STA(IND_STA)%N_ECC = ', VTD%STA(IND_STA)%N_ECC 
!@                   CALL ERR_LOG ( 8276, IUER, 'PRE_VTD', 'Trap of internal '// &
!@     &                 'control: station '//ISITN_CHR(J4)//' has '// &
!@     &                 'more than one eccentricity epoch. This case is not '// &
!@     &                 'yet supported' )
!@                   RETURN 
              END IF
              WRITE ( UNIT=STR(15:18), FMT='(I4)'    ) VTD%STA(IND_STA)%CDP_NUMBER
              WRITE ( UNIT=STR(29:38), FMT='(F10.4)' ) VTD%STA(IND_STA)%ECC_TRS(1,1)  
              WRITE ( UNIT=STR(40:49), FMT='(F10.4)' ) VTD%STA(IND_STA)%ECC_TRS(2,1)  
              WRITE ( UNIT=STR(51:60), FMT='(F10.4)' ) VTD%STA(IND_STA)%ECC_TRS(3,1)  
              STR(62:63) = 'XY'
         END IF
         ICONT_I2 = 4
         CALL PUTCARD ( INT2(1), 'SITE', ICONT_I2, STR, IERR_I2 )
         IF ( IERR_I2 .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( INT4(IERR_I2), STR )
              CALL ERR_LOG ( 8277, IUER, 'PRE_VTD', 'Error in writing '// &
     &            'SITE namefile card: '//STR )
              RETURN 
         END IF
!
         TIME0 = MJD_SEC_TO_JD ( VTD%STA(J4)%MJD_REF, 0.D0 )/YEAR__TO__DAY
 440  CONTINUE 
!
! --- Update source coordinates
!
      DO 470 J7=1,L_SOU
         DO 480 J8=1,VTD%L_SOU
            IF ( ISTRN_CHR(J7) == VTD%SOU(J8)%IVS_NAME ) THEN
                 VSTARC(1,J7) = VTD%SOU(J8)%ALPHA
                 VSTARC(2,J7) = VTD%SOU(J8)%DELTA
            END IF
 480     CONTINUE 
 470  CONTINUE 
      CALL USE_GLBFIL   ( 'OW' )
      CALL USE_GLBFIL_4 ( 'WC' )
!
! --- Get date of the nominal experiment start time
!
      CALL JD_TO_MJD_SEC ( FJDOBS, MJD_BEG, TAI_BEG )
      CALL TAI_TO_TDB  ( MJD_BEG, TAI_BEG, TDB_BEG )
      TARG_TDB_BEG = (MJD_BEG - J2000__MJD - 0.5D0)*86400.0D0 + TDB_BEG
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_MOMENT ( C_SOU(1), MJD_BEG, TAI_BEG, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8278, IUER, 'PRE_VTD', 'Error in computing '// &
     &         'time dependent intermediate quantities for the session '// &
     &         'nominal start' )
           RETURN 
      END IF
      VTD_IONO_SCALE = VTD%CONF%IONOSPHERE_SCALE
!
! --- Compute Reference nutation Wahr1980 and MHB2000 expansions on the epoch
! --- of nominal start
!
      CALL HEO_WAHR1980 ( 4, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                       DPSI_WAHR_BEG, DEPS_WAHR_BEG, E1_RATE, E2_RATE, &
     &                       DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
!
      IF ( VTD%CONF%NUT_EXP  == NUT__WAHR1980 ) THEN
           CALL HEO_WAHR1980 ( 4, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                            DPSI_APR_BEG, DEPS_APR_BEG, E1_RATE, E2_RATE, &
     &                            DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__IERS1996 ) THEN
           CALL HEO_IERS1996 ( 4, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                            DPSI_APR_BEG, DEPS_APR_BEG, E1_RATE, E2_RATE, &
     &                            DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__REN2000 .OR. VTD%CONF%NUT_EXP == VTD__NERS ) THEN
           CALL HEO_REN2000  ( 4, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                            DPSI_APR_BEG, DEPS_APR_BEG, E1_RATE, E2_RATE, &
     &                            DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000 ) THEN
           CALL HEO_MHB2000  ( 4, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                            DPSI_APR_BEG, DEPS_APR_BEG, E1_RATE, E2_RATE, &
     &                            DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000_TRANSF ) THEN
           CALL HEO_MHB2000_TRANSF ( 4, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, &
     &                                  E1, E2, DPSI_APR_BEG, DEPS_APR_BEG, &
     &                                  E1_RATE, E2_RATE, &
     &                                  DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETA ) THEN
           CALL HEO_EMPI ( 4, NUT__PETA, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, &
     &                        E1, E2, DPSI_APR_BEG, DEPS_APR_BEG, &
     &                        E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                        CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETB ) THEN
           CALL HEO_EMPI ( 4, NUT__PETB, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, &
     &                        E1, E2, DPSI_APR_BEG, DEPS_APR_BEG, &
     &                        E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                        CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETC ) THEN
           CALL HEO_EMPI ( 4, NUT__PETC, TARG_TDB_BEG, VTD%MOM%UT1_M_TAI, &
     &                        E1, E2, DPSI_APR_BEG, DEPS_APR_BEG, &
     &                        E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                        CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == VTD__UNDF  .AND. &
     &             ILEN(VTD%CONF%FINAM_AEM) > 0         ) THEN
           E1_RATE   = 0.0D0 
           E2_RATE   = 0.0D0 
           DPSI_RATE = 0.0D0
           DEPS_RATE = 0.0D0
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( VTD%CONF%NUT_EXP, STR ) 
           CALL ERR_LOG ( 8279, IUER, 'PRE_VTD', 'Trap of internal '// &
     &         'control. Unknown nutation expansion code: '//STR )
           RETURN 
      END IF
!
      CALL JD_TO_MJD_SEC ( LJDOBS, MJD_END, TAI_END )
      CALL TAI_TO_TDB  ( MJD_END, TAI_END, TDB_END )
      TARG_TDB_END = (MJD_END - J2000__MJD - 0.5D0)*86400.0D0 + TDB_END
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_MOMENT   ( C_SOU(1), MJD_END, TAI_END, VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8280, IUER, 'PRE_VTD', 'Error in computing '// &
     &         'time dependent intermediate quantities for session nominal '// &
     &         'stop' )
           RETURN 
      END IF
!
! --- Compute reference nutation Wahr1980 and MHB2000 expansions on the epoch
! --- of nominal end of the experiment
!
      CALL HEO_WAHR1980 ( 4, TARG_TDB_END, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                       DPSI_WAHR_END, DEPS_WAHR_END, E1_RATE, E2_RATE, &
     &                       DPSI_RATE, DEPS_RATE, CROSS_NUT_E3  )
!
      IF ( VTD%CONF%NUT_EXP  == NUT__WAHR1980 ) THEN
           CALL HEO_WAHR1980 ( 4, TARG_TDB_END, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                            DPSI_APR_END, DEPS_APR_END, E1_RATE, E2_RATE, &
     &                            DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__IERS1996 ) THEN
           CALL HEO_IERS1996 ( 4, TARG_TDB_END, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                            DPSI_APR_END, DEPS_APR_END, E1_RATE, E2_RATE, &
     &                            DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__REN2000 ) THEN
           CALL HEO_REN2000  ( 4, TARG_TDB_END, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                            DPSI_APR_END, DEPS_APR_END, E1_RATE, E2_RATE, &
     &                            DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000 .OR. VTD%CONF%NUT_EXP == VTD__NERS  ) THEN
           CALL HEO_MHB2000  ( 4, TARG_TDB_END, VTD%MOM%UT1_M_TAI, E1, E2, &
     &                            DPSI_APR_END, DEPS_APR_END, E1_RATE, E2_RATE, &
     &                            DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__MHB2000_TRANSF ) THEN
           CALL HEO_MHB2000_TRANSF ( 4, TARG_TDB_END, VTD%MOM%UT1_M_TAI, &
     &                                  E1, E2, DPSI_APR_END, DEPS_APR_END, &
     &                                  E1_RATE, E2_RATE, &
     &                                  DPSI_RATE, DEPS_RATE, CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETA ) THEN
           CALL HEO_EMPI ( 4, NUT__PETA, TARG_TDB_END, VTD%MOM%UT1_M_TAI, &
     &                        E1, E2, DPSI_APR_END, DEPS_APR_END, &
     &                        E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                        CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETB ) THEN
           CALL HEO_EMPI ( 4, NUT__PETB, TARG_TDB_END, VTD%MOM%UT1_M_TAI, &
     &                        E1, E2, DPSI_APR_END, DEPS_APR_END, &
     &                        E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                        CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == NUT__PETC ) THEN
           CALL HEO_EMPI ( 4, NUT__PETC, TARG_TDB_END, VTD%MOM%UT1_M_TAI, &
     &                        E1, E2, DPSI_APR_END, DEPS_APR_END, &
     &                        E1_RATE, E2_RATE, DPSI_RATE, DEPS_RATE, &
     &                        CROSS_NUT_E3 )
         ELSE IF ( VTD%CONF%NUT_EXP == VTD__UNDF  .AND. &
     &             ILEN(VTD%CONF%FINAM_AEM) > 0         ) THEN
           E1_RATE   = 0.0D0 
           E2_RATE   = 0.0D0 
           DPSI_RATE = 0.0D0
           DEPS_RATE = 0.0D0
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( VTD%CONF%NUT_EXP, STR ) 
           CALL ERR_LOG ( 8281, IUER, 'PRE_VTD', 'Trap of internal '// &
     &         'control. Unknown nutation expansion code: '//STR )
           RETURN 
      END IF
!
      UT1INB(1) = 2400000.5D0 + MJD_BEG - MAX_EROT_VALUES/2
      UT1INB(2) = 1.0D0
      UT1INB(3) = MAX_EROT_VALUES
!
      WOBINB(1) = UT1INB(1) 
      WOBINB(2) = UT1INB(2) 
      WOBINB(3) = UT1INB(3) 
!
! --- Store apriori EOP. They will be needed later for reporting totals
!
      NF_GOOD = 0
      NL_GOOD = 0
      DO 490 J9=1,MAX_EROT_VALUES
         MJD_TAB   = UT1INB(1) + J9-1 - 2400000.5D0
         JDPTB(J9) = UT1INB(1) + J9-1 
         IER = 0
         CALL VTD_MOMENT ( C_SOU(1), MJD_TAB, 0.0D0, VTD, IER )
         IF ( IER .EQ. 0 ) THEN
              IF ( NF_GOOD == 0 ) NF_GOOD = J9
              NL_GOOD = J9
         END IF
         UT1PTB(J9) = -VTD%MOM%UT1_M_TAI
         WOBXXB(J9) =  VTD%MOM%XPL/MAS__TO__RAD
         WOBYYB(J9) =  VTD%MOM%YPL/MAS__TO__RAD
 490  CONTINUE 
!
! --- It may happen that no apriori EOP are avaialble at the beginning or
! --- end of the time range. Then we fill miss slots with the closest
! --- available data
!
      IF ( NL_GOOD > 1 ) THEN
           DO 4100 J10=1,NL_GOOD-1
              MJD_TAB     = UT1INB(1) + J10-1 - 2400000.5D0
              JDPTB(J10)  = UT1INB(1) + J10-1 
              UT1PTB(J10) = UT1PTB(NL_GOOD) 
              WOBXXB(J10) = WOBXXB(NL_GOOD) 
              WOBYYB(J10) = WOBYYB(NL_GOOD) 
 4100      CONTINUE 
      END IF
      IF ( NF_GOOD < MAX_EROT_VALUES ) THEN
           DO 4110 J11=NF_GOOD+1,MAX_EROT_VALUES 
              MJD_TAB   = UT1INB(1) + J11-1 - 2400000.5D0
              JDPTB(J11) = UT1INB(1) + J11-1 
              UT1PTB(J11) = UT1PTB(NF_GOOD) 
              WOBXXB(J11) = WOBXXB(NF_GOOD) 
              WOBYYB(J11) = WOBYYB(NF_GOOD) 
 4110      CONTINUE 
      END IF
!
      IF ( NF_GOOD == 0 ) THEN
!
! -------- But if no EOP was found for any slot, we report an error
!
           MJD_TAB_FIRST = UT1INB(1) - 2400000.5D0
           MJD_TAB_LAST  = UT1INB(1) + (MAX_EROT_VALUES - 1) - 2400000.5D0
           CALL ERR_LOG ( 8282, IUER, 'PRE_VTD', 'Error in an '// &
     &         'attempt to compute interpolated value of UT1 and polar '// &
     &         'motion for the interval ['// &
     &          MJDSEC_TO_DATE ( MJD_TAB_FIRST, 0.0D0, -2 )//', '// &
     &          MJDSEC_TO_DATE ( MJD_TAB_LAST,  0.0D0, -2 )//'] ' )
           RETURN 
      END IF
!
! --- Update namfil
!
      CALL CLOSENAMFIL()
      CALL OPENNAMFIL()
      CALL USE_PARFIL ( 'OWC' )
!
      IDBEND_SAVE = IDBEND
      CALL USE_COMMON ( 'ORC' )
      IDBEND = IDBEND_SAVE 
!
! --- Compute average apriori nutation
!
      NUTPSI_AVE = (DPSI_APR_BEG  + DPSI_APR_END)/2.0D0
      NUTEPS_AVE = (DEPS_APR_BEG  + DEPS_APR_END)/2.0D0
!
! --- ... and the average difference between Wahr nutation and the apriori
!
      NUTPSI_DIF = NUTPSI_AVE - (DPSI_WAHR_BEG + DPSI_WAHR_END)/2.0D0 
      NUTEPS_DIF = NUTEPS_AVE - (DEPS_WAHR_BEG + DEPS_WAHR_END)/2.0D0 
      CALCV = -101.0D0
      CALL USE_COMMON ( 'OWC' )
      VTD_STATUS_FIELD = VTD%STATUS 
!
! --- Check for a special kludge variable
!
      CALL GETENVAR ( 'SOLVE_NO_STRUC', STR )
      IF ( STR == 'YES'  .OR.  STR == 'yes' ) THEN
           VTD%CONF%TEST(1) = 1
      END IF
      CALL GETENVAR ( 'VTD_TEST', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, VTD%CONF%TEST(1) )
      END IF
!
! --- Check for a special kludge variable
!
      CALL GETENVAR ( 'VTD_DEBUG', STR )
      IF ( ILEN(STR) > 0 ) THEN
           CALL CHIN ( STR, VTD%CONF%IVRB )
      END IF
!
      CALL GETENVAR ( 'SOLVE_DEBUG', STR )
      IF ( FL_VTD_IONO ) THEN
!
! -------- Set fields of OBS_TYP
!
           CALL SET_OBSTYP ( OBS_TYP )
           DO 4120 J12=1,NUMOBS
               CALL USE_OBSFIL ( IOBSFIL, J12, 'R' )
               IF ( IDATYP == G_GXS__DTP ) THEN
                    IF ( EFFREQ_S*1.D6 > VTD__FREQ_MIN .AND. &
     &                   EFFREQ_S*1.D6 < VTD__FREQ_MAX .AND. &
     &                   EFFREQ*1.D6   > VTD__FREQ_MIN .AND. &
     &                   EFFREQ*1.D6   < VTD__FREQ_MAX       ) THEN
                         CALL SET_OBSTYP ( OBS_TYP )
                         GOTO 8120
                    END IF
                  ELSE IF ( IDATYP == GX__DTP ) THEN
                    IF ( EFFREQ*1.D6   > VTD__FREQ_MIN .AND. &
     &                   EFFREQ*1.D6   < VTD__FREQ_MAX       ) THEN
                         CALL SET_OBSTYP ( OBS_TYP )
                         GOTO 8120
                    END IF
                  ELSE IF ( IDATYP == GS__DTP ) THEN
                    IF ( EFFREQ_S*1.D6 > VTD__FREQ_MIN .AND. &
     &                   EFFREQ_S*1.D6 < VTD__FREQ_MAX       ) THEN
                         CALL SET_OBSTYP ( OBS_TYP )
                         GOTO 8120
                    END IF
               END IF
 4120      CONTINUE 
 8120      CONTINUE 
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_IONO_STAT ( VTD, OBS_TYP, IER )
           IF ( IER .NE. 0 ) THEN
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8283, IUER, 'PRE_VTD', 'Failure in '// &
     &                   'attempt to compute statistics for '// &
     &                   'ionosphere path delay' )
                     RETURN 
                END IF
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PRE_VTD  !#!#
