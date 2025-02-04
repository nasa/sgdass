      SUBROUTINE VTD_LOAD ( VTD, L_STA, C_STA, L_SOU, C_SOU, &
     &                      MJD_BEG, TAI_BEG, MJD_END, TAI_END, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  VTD_LOAD  loads lists of stations participated in the     *
! *   observing session, list of observed sources, time of the first and *
! *   the last observation. It puts station and sources list in the      *
! *   internal field of VTD. VTD reads the catalogue specified in the    *
! *   control file, reads the Earth orientation parameters file, reads   *
! *   planetary ephemerides, computes time-independent quantities needed *
! *   for solid Earth tide computations, loads position variations       *
! *   modes,  computes interpolating coefficients of site displacements  *
! *   if needed. VTD_LOAD  should be called before computation of time   *
! *   delay for the first observation.                                   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     L_STA ( INTEGER*4 ) -- The number of stations participating in   *
! *                            the experiment.                           *
! *     C_STA ( CHARACTER ) -- List of station names participating in    *
! *                            the experiment. Dimenstion: L_STA.        *
! *     L_SOU ( INTEGER*4 ) -- The number of observed sources.           *
! *     C_SOU ( CHARACTER ) -- List of source names participating in     *
! *                            the experiment. Dimenstion: L_SOU.        *
! *   MJD_BEG ( INTEGER*4 ) -- Modified Julian data on the midnight of   *
! *                            the first observation. Units: days.       *
! *   TAI_BEG ( REAL*8    ) -- Moment of time of the first observation.  *
! *                            Units: sec.                               *
! *   MJD_END ( INTEGER*4 ) -- Modified Julian data on the midnight of   *
! *                            the last observation. Units: days.        *
! *   TAI_END ( REAL*8    ) -- Moment of time of the last observation.   *
! *                            Units: sec.                               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 26-JAN-2004    VTD_LOAD   v2.13 (c) L. Petrov  17-APR-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'ners_local.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  L_STA, L_SOU, MJD_BEG, MJD_END, IUER
      CHARACTER  C_STA(VTD__M_STA)*(*), C_SOU(VTD__M_SOU)*(*)
      REAL*8     TAI_BEG, TAI_END
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = VTD__M_SCC*VTD__M_SRC )
      CHARACTER*256, ALLOCATABLE :: BUF(:)
      CHARACTER  STR*80, STR1*80, STR2*80
      LOGICAL*4  LEX, FL_FOUND_ECC 
      REAL*8     CF, SF, CL, SL, PP, MU
      REAL*8     R_TRS(3,VTD__M_STA), TIME_BEG, TIME_END, &
     &           TIME_ECC_BEG, TIME_ECC_END
      INTEGER*4  IPAR_ORD, IPAR_3RD, IPAR_2ND, NBUF, J1, J2, J3, J4, &
     &           J5, J6, NN, IOS, IER
      CHARACTER  C_SYN_STA(VTD__M_STA)*8
      LOGICAL*4  PROBE_WRITE_ADDRESS
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*8,    EXTERNAL :: ATAN_CS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      LOGICAL*4, EXTERNAL :: IS_R8_NAN
!
      IF ( .NOT. PROBE_WRITE_ADDRESS ( VTD%STATUS ) ) THEN
           CALL ERR_LOG ( 2121, IUER, 'VTD_LOAD', 'Object VTD is not '// &
     &         'accessible for writing. It is an indication of a very '// &
     &         'serious error. Please check the argument list' )
           RETURN
      END IF
!
      IF ( VTD%STATUS .NE. VTD__INIT ) THEN
           CALL ERR_LOG ( 2122, IUER, 'VTD_LOAD', 'Object VTD was not '// &
     &         'initialized ' )
           RETURN
      END IF
!
! --- Check parameters MJD_BEG, TAI_BEG, MJD_END, TAI_END
!
      IF ( MJD_BEG < VTD__EARLIEST_MJD ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MJD_BEG, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( VTD__EARLIEST_MJD, STR1 )
           CALL ERR_LOG ( 2123, IUER, 'VTD_LOAD', 'Parameter MJD_BEG is '// &
     &         'out of range: '//TRIM(STR)//' -- it should be > '//STR1 )
           RETURN 
      END IF
      IF ( MJD_BEG > VTD__LATEST_MJD ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MJD_BEG, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( VTD__LATEST_MJD, STR1 )
           CALL ERR_LOG ( 2124, IUER, 'VTD_LOAD', 'Parameter MJD_BEG is '// &
     &         'out of range: '//TRIM(STR)//' -- it should be < '//STR1 )
           RETURN 
      END IF
      IF ( MJD_END < MJD_BEG ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MJD_END, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( MJD_BEG, STR1 )
           CALL ERR_LOG ( 2125, IUER, 'VTD_LOAD', 'Parameter MJD_BEG is '// &
     &         'out of range: '//TRIM(STR)//' -- it should be > or = '//STR1 )
           RETURN 
      END IF
      IF ( MJD_END > VTD__LATEST_MJD ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MJD_BEG, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( VTD__LATEST_MJD, STR1 )
           CALL ERR_LOG ( 2126, IUER, 'VTD_LOAD', 'Parameter MJD_END is '// &
     &         'out of range: '//TRIM(STR)//' -- it should be < '//STR1 )
           RETURN 
      END IF
      IF ( IS_R8_NAN(TAI_BEG) ) THEN
           CALL ERR_LOG ( 2127, IUER, 'VTD_LOAD', 'Parameter TAI_BEG is '// &
     &         'NaN' )
           RETURN 
      END IF
      IF ( TAI_BEG < VTD__EARLIEST_TAI ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR,  FMT='(1PD15.7)' ) TAI_BEG
           WRITE ( UNIT=STR1, FMT='(F12.6)'   ) VTD__EARLIEST_TAI 
           CALL ERR_LOG ( 2128, IUER, 'VTD_LOAD', 'Parameter TAI_BEG '// &
     &          TRIM(STR)//' is too small: less than '//STR1 )
           RETURN 
      END IF
      IF ( TAI_BEG > VTD__LATEST_TAI ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR,  FMT='(1PD15.7)' ) TAI_BEG
           WRITE ( UNIT=STR1, FMT='(F12.6)'   ) VTD__LATEST_TAI 
           CALL ERR_LOG ( 2129, IUER, 'VTD_LOAD', 'Parameter TAI_BEG '// &
     &          TRIM(STR)//' is too big: greater than '//STR1 )
           RETURN 
      END IF
      IF ( IS_R8_NAN(TAI_END) ) THEN
           CALL ERR_LOG ( 2130, IUER, 'VTD_LOAD', 'Parameter TAI_BEG is '// &
     &         'NaN' )
           RETURN 
      END IF
      IF ( TAI_END < VTD__EARLIEST_TAI ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR, FMT='(1PD15.7)' ) TAI_END
           WRITE ( UNIT=STR1, FMT='(F12.6)'   ) VTD__EARLIEST_TAI 
           CALL ERR_LOG ( 2131, IUER, 'VTD_LOAD', 'Parameter TAI_END '// &
     &          TRIM(STR)//' is too small: less than '//STR1 )
           RETURN 
      END IF
      IF ( TAI_END > VTD__LATEST_TAI ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           WRITE ( UNIT=STR, FMT='(1PD15.7)' ) TAI_END
           WRITE ( UNIT=STR1, FMT='(F12.6)'   ) VTD__LATEST_TAI 
           CALL ERR_LOG ( 2132, IUER, 'VTD_LOAD', 'Parameter TAI_END '// &
     &          TRIM(STR)//' is too big: greater than '//STR1 )
           RETURN 
      END IF
!
      IF ( (MJD_END*86400.0D0 + TAI_END) - (MJD_BEG*86400.0D0 + TAI_BEG) < VTD__MIN_INTRV ) THEN
           CALL CLRCH ( STR  )
           WRITE ( UNIT=STR, FMT='(1PD15.7)' ) (MJD_END*86400.0D0 + TAI_END) - (MJD_BEG*86400.0D0 + TAI_BEG) 
           CALL CLRCH ( STR1  )
           WRITE ( UNIT=STR1, FMT='(1PD15.7)' ) VTD__MIN_INTRV 
           CALL ERR_LOG ( 2133, IUER, 'VTD_LOAD', 'Wrong parameters '// &
     &         'MJD_BEG, MJD_END, TAI_BEG, TAI_END: the interval '// &
     &         TRIM(STR)//' is shorter than '//TRIM(STR1)//' seconds' )
           RETURN 
      END IF
!
! --- Allocate dynamic memory for buffers
!
      ALLOCATE ( BUF(MBUF), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 256*MBUF, STR )
           CALL ERR_LOG ( 2134, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &         'to allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
! --- Check validity of input parameters
!
      IF ( L_STA .LE. 0 ) THEN
           CALL ERR_LOG ( 2135, IUER, 'VTD_LOAD', 'Parameter L_STA is 0' )
           RETURN
      END IF
!
      IF ( L_STA .GT. VTD__M_STA ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_STA, STR )
           CALL INCH  ( VTD__M_STA, STR1 )
           CALL ERR_LOG ( 2136, IUER, 'VTD_LOAD', 'Parameter L_STA '// &
     &          STR(1:I_LEN(STR))//' exceeds constants VTD__M_STA '// &
     &          STR1 )
           RETURN
      END IF
!
      IF ( L_SOU .LE. 0 ) THEN
           CALL ERR_LOG ( 2137, IUER, 'VTD_LOAD', 'Parameter L_SOU is 0' )
           RETURN
      END IF
!
      IF ( L_SOU .GT. VTD__M_SOU ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( L_SOU, STR )
           CALL INCH  ( VTD__M_SOU, STR1 )
           CALL ERR_LOG ( 2138, IUER, 'VTD_LOAD', 'Parameter L_SOU '// &
     &          STR(1:I_LEN(STR))//' exceeds constants VTD__M_SOU '// &
     &          STR1 )
           RETURN
      END IF
!
! --- Store the dates of the first and the last observation
!
      VTD%MJD_BEG = MJD_BEG
      VTD%MJD_END = MJD_END
      VTD%TAI_BEG = TAI_BEG
      VTD%TAI_END = TAI_END
!
      IF ( VTD%STATUS_STA .NE. VTD__BYPS ) THEN
!
! -------- Load station description from the external file specifed in VTD%CONF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_STADESC ( VTD, L_STA, C_STA, MBUF, BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2139, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to load station description from the input file '// &
     &               VTD%CONF%FINAM_STADESC )
                DEALLOCATE ( BUF )
                RETURN
           END IF
!
! -------- Load station coordinates from the external file specifed in VTD%CONF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_STACOO ( VTD, MBUF, BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2140, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to load station coordinates from the input file '// &
     &               VTD%CONF%FINAM_STACOO )
                DEALLOCATE ( BUF )
                RETURN
           END IF
!
! -------- Load station coordinates from the external file specifed in VTD%CONF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_STAECC ( VTD, MBUF, BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2141, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to load station eccentricities from the input file '// &
     &               VTD%CONF%FINAM_STAECC )
                DEALLOCATE ( BUF )
                RETURN
           END IF
!
! -------- Load station velocities from the external file specifed in VTD%CONF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_STAVEL ( VTD, MBUF, BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2142, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to load station velocities from the input file '// &
     &               VTD%CONF%FINAM_STAVEL )
                DEALLOCATE ( BUF )
                RETURN
           END IF
      END IF
!
! --- Load DE ephemerides from the external file specifed in VTD%CONF
!
      CALL ERR_PASS ( IUER, IER )
      CALL READ_DE_EPH ( VTD%CONF%FINAM_DE_EPH, VTD%DE_EPH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2146, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &         'to load planet ephemerides the input file '// &
     &          VTD%CONF%FINAM_DE_EPH )
           DEALLOCATE ( BUF )
           RETURN
      END IF
      IF ( VTD%CONF%FINAM_LEAPSEC .EQ. VTD__NERS_STR ) THEN
!
! -------- Leap second is taken via NERS. Then let us initialize NERS
!
           CALL ERR_PASS  ( IUER, IER )
           CALL NERS_INIT ( NERS__CONFIG, VTD%NERS, &
     &              (VTD%MJD_BEG - J2000__MJD)*86400.0D0 + VTD%TAI_BEG - VTD__EOP_TIM_MAR, &
     &              (VTD%MJD_END - J2000__MJD)*86400.0D0 + VTD%TAI_END + VTD__EOP_TIM_MAR, &
     &               IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2147, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to initialize NERS data structure' )
                DEALLOCATE ( BUF )
           END IF
           VTD%LEAPSEC%STATUS = VTD__NERS
         ELSE
!
! -------- Load leap second file from the external file specifed in VTD%CONF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_LEAPSEC ( VTD, MBUF, BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2148, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to load leap second file '//VTD%CONF%FINAM_LEAPSEC )
                DEALLOCATE ( BUF )
                RETURN
           END IF
      END IF
!
! --- Load the file with the Earth orientation parameters from the external
! --- file specifed in VTD%CONF
!
      IF ( ILEN(VTD%CONF%FINAM_EOP) == 0 ) THEN
           VTD%UEOP%NP = 0
         ELSE 
           IF ( VTD%CONF%FINAM_EOP == VTD__NERS_STR ) THEN
                VTD%CONF%FINAM_EOP = NERS__CONFIG
           END IF
           IF ( VTD%STATUS_SOU .NE. VTD__BYPS ) THEN
!
! ------------- Load EOP time series
!
                CALL ERR_PASS  ( IUER, IER )
                CALL VTD_UEOP_INIT ( VTD, VTD%CONF%FINAM_EOP, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 2149, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &                    'to load apriori EOP from the input file '// &
     &               VTD%CONF%FINAM_EOP )
                     DEALLOCATE ( BUF )
                     RETURN
                END IF
           END IF
      END IF
!
! --- Load position variation files
!
      DO 410 J1=1,VTD__M_PSF
         IF ( VTD%POSVAR(J1)%STATUS == VTD__LOAD ) THEN
!
! ----------- Already loaded? nothing to do!
!
              CONTINUE 
            ELSE 
              VTD%POSVAR(J1)%STATUS = VTD__UNDF
              IF ( VTD%CONF%POSVAR_MOD(J1) .EQ. PSV__HMD ) THEN
!
! ---------------- Aga. This is a harmonic site position variation file
!
                   CALL ERR_PASS  ( IUER, IER )
                   CALL VTD_LOAD_HARPOS ( VTD, J1, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2150, IUER, 'VTD_LOAD', 'Error in '// &
     &                       'an attempt to load harmonic site position '// &
     &                       'variation file '//VTD%CONF%POSVAR_FIL(J1) )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
                ELSE IF ( VTD%CONF%POSVAR_MOD(J1) .EQ. PSV__TSR ) THEN
!
! ---------------- This is a file with time series of position variations
!
                   CALL ERR_PASS  ( IUER, IER )
                   CALL VTD_LOAD_BINDISP ( VTD, J1, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2151, IUER, 'VTD_LOAD', 'Error in an '// &
     &                       'attempt to load a directory with site '// &
     &                       'position variation files '// &
     &                       VTD%CONF%POSVAR_FIL(J1) )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
                ELSE IF ( VTD%CONF%POSVAR_MOD(J1) .EQ. PSV__BSP ) THEN
!
! ---------------- This is a file with time series of position variations
!
                   CALL ERR_PASS  ( IUER, IER )
                   CALL VTD_LOAD_BSPPOS ( VTD, J1, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 2152, IUER, 'VTD_LOAD', 'Error in an '// &
     &                       'attempt to load a file of position variation '// &
     &                       'expension into the B-spline basis '// &
     &                       VTD%CONF%POSVAR_FIL(J1) )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
              END IF
         END IF
 410  CONTINUE
!
! --- Process all stations
!
      TIME_BEG = (VTD%MJD_BEG - J2000__MJD)*86400.0D0 + (VTD%TAI_BEG - 43200.0D0)
      TIME_END = (VTD%MJD_END - J2000__MJD)*86400.0D0 + (VTD%TAI_END - 43200.0D0)
      DO 420 J2=1,VTD%L_STA
         IF ( VTD%STA(J2)%STA_TYP .EQ. VTD__OR ) THEN
              VTD%STA(J2)%REN_TO_TRS  = 0.0D0
              VTD%STA(J2)%UEN_TO_TRS  = 0.0D0
              VTD%STA(J2)%MOM_COO_TRS = 0.0D0
              VTD%STA(J2)%MOM_VEL_TRS = 0.0D0
              VTD%STA(J2)%MOM_ACC_TRS = 0.0D0
              R_TRS(1,J2) = VTD__REA
              R_TRS(2,J2) = 0.0D0
              R_TRS(3,J2) = 0.0D0
              GOTO 420
         END IF
!
! ------ Copy initial position
!
         CALL COPY_R8 ( 3, VTD%STA(J2)%COO_TRS(1,1), VTD%STA(J2)%BEG_TRS )
         IF ( VTD%STA(J2)%N_EPC .GT. 1 ) THEN
              DO 430 J3=2,VTD%STA(J2)%N_EPC
                 IF ( VTD%STA(J2)%MJD_EPC(J3) .GT. MJD_BEG .AND. &
     &                VTD%STA(J2)%TAI_EPC(J3) .GT. TAI_BEG       ) THEN
                      CALL COPY_R8 ( 3, VTD%STA(J2)%COO_TRS(1,J3), &
     &                                  VTD%STA(J2)%BEG_TRS )
                 END IF
 430          CONTINUE
         END IF
!
! ------ Apply eccentricity vector
!
         FL_FOUND_ECC = .FALSE.
         IF ( VTD%CONF%FINAM_STAECC .NE. 'NONE' ) THEN
              DO 440 J4=1,VTD%STA(J2)%N_ECC
                 TIME_ECC_BEG = &
     &                ( VTD%STA(J2)%ECC_MJD_BEG(J4) - J2000__MJD)*86400.0D0 + &
     &                ( VTD%STA(J2)%ECC_TAI_BEG(J4) - 43200.0D0)
                 TIME_ECC_END = &
     &                ( VTD%STA(J2)%ECC_MJD_END(J4) - J2000__MJD)*86400.0D0 + &
     &                ( VTD%STA(J2)%ECC_TAI_END(J4) - 43200.0D0)
                 IF ( TIME_BEG .GE. TIME_ECC_BEG  .AND.  &
     &                TIME_END .LE. TIME_ECC_END         ) THEN
                      CALL ADD_VV ( 3, VTD%STA(J2)%BEG_TRS, &
     &                                 VTD%STA(J2)%ECC_TRS(1,J4)  )
                      FL_FOUND_ECC = .TRUE.
                 END IF
 440          CONTINUE
              IF ( .NOT. FL_FOUND_ECC ) THEN
                    WRITE ( 6, * ) 'VTD%STA(J2)%N_ECC= ', VTD%STA(J2)%N_ECC
                    CALL ERR_LOG ( 2153, IUER, 'VTD_LOAD', 'No eccentricity '// &
     &                  'was found for station '//VTD%STA(J2)%IVS_NAME// &
     &                  ' for the range ['// &
     &                  MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG, -3 )//', '// &
     &                  MJDSEC_TO_DATE ( MJD_END, TAI_END, -3 )//'] '// &
     &                  ' in file '//VTD%CONF%FINAM_STAECC )
                    DEALLOCATE ( BUF )
                    RETURN
              END IF
         END IF
!
! ------ Calculation longitude LONGITUDE and geocentric lattitude PHI_GCN of
! ------ the station
!
         IF ( ( VTD%STA(J2)%BEG_TRS(1)**2 + &
     &          VTD%STA(J2)%BEG_TRS(2)**2 + &
     &          VTD%STA(J2)%BEG_TRS(3)**2   ) .LT. 0.9*VTD__REA ) THEN
                IF ( VTD%STA(J2)%STA_TYP == VTD__GC ) THEN
                     CONTINUE 
                   ELSE IF ( VTD%STA(J2)%STA_TYP == VTD__OR ) THEN
                     CONTINUE 
                   ELSE
                     CALL ERR_LOG ( 2154, IUER, 'VTD_LOAD', 'Position of ground '// &
     &                   'station '//VTD%STA(J2)%IVS_NAME//' is well below '// &
     &                   'the surface. Please check its coordinates and its '// &
     &                   'type: GROUND, GEOCENTER or ORBITING' )
                    DEALLOCATE ( BUF )
                    RETURN
                END IF
         ENDIF
         IF ( ( VTD%STA(J2)%BEG_TRS(1)**2 + &
     &          VTD%STA(J2)%BEG_TRS(2)**2 + &
     &          VTD%STA(J2)%BEG_TRS(3)**2   ) .GT. VTD__HEIGHT_MIN**2 .AND. &
     &        ( VTD%STA(J2)%BEG_TRS(1)**2 + &
     &          VTD%STA(J2)%BEG_TRS(2)**2 + &
     &          VTD%STA(J2)%BEG_TRS(3)**2   ) .LT. VTD__HEIGHT_MAX**2       ) THEN
!
              VTD%STA(J2)%LONG = ATAN_CS ( VTD%STA(J2)%BEG_TRS(1), &
     &                                     VTD%STA(J2)%BEG_TRS(2)  )
              IF ( VTD%STA(J2)%LONG       .LT. 0.0D0 ) THEN
                   VTD%STA(J2)%LONG = PI2 + VTD%STA(J2)%LONG
              END IF

              PP  = DSQRT ( VTD%STA(J2)%BEG_TRS(1)**2 + &
     &                      VTD%STA(J2)%BEG_TRS(2)**2 )
              IF ( DABS(PP) .LT. 1.D-8 ) PP=1.D-8
              VTD%STA(J2)%RAD = DSQRT ( VTD%STA(J2)%BEG_TRS(3)**2 + PP**2    )
              IF ( DABS ( VTD%STA(J2)%RAD - VTD__REA ) .GT. VTD__HEIGHT_MIN .AND. &
                   DABS ( VTD%STA(J2)%RAD - VTD__REA ) .LT. VTD__HEIGHT_MAX       ) THEN
!
                   CALL CLRCH ( STR )
                   WRITE ( UNIT=STR, FMT='(I12)' ) J2
                   CALL CHASHL ( STR  )
                   CALL CLRCH  ( STR2 )
                   WRITE ( UNIT=STR2, FMT='(3(F15.3,2X))' ) &
     &                   ( VTD%STA(J2)%BEG_TRS(NN), NN=1,3 )
                   CALL ERR_LOG ( 2155, IUER, 'VTD_LOAD', 'Wrong '// &
     &                 'positions of station '//VTD%STA(J2)%IVS_NAME// &
     &                 ' -- '//STR2(1:ILEN(STR2))// &
     &                 ' -- they are not on the surface of our planet' )
                   RETURN
              END IF
!
              VTD%STA(J2)%LAT_GCN = DATAN( VTD%STA(J2)%BEG_TRS(3)/PP )
!
              C_STA(J2)   = VTD%STA(J2)%IVS_NAME
              CALL COPY_R8 ( 3, VTD%STA(J2)%BEG_TRS, R_TRS(1,J2) )
!
! ----------- Computation of geodetic latitude
!
              MU = DATAN ( VTD%STA(J2)%BEG_TRS(3)/PP * &
     &                     ( (1.D0 - VTD__FE) + VTD__EXC_SQ*VTD__REA/ &
     &                     VTD%STA(J2)%RAD  ) )
!
              VTD%STA(J2)%LAT_GDT = &
     &            DATAN( ( (1.D0 - VTD__FE)*VTD%STA(J2)%BEG_TRS(3) + &
     &                      VTD__EXC_SQ*VTD__REA*DSIN(MU)**3 ) / &
     &                     ( (1.D0 - VTD__FE)* &
     &                     ( PP  - VTD__EXC_SQ*VTD__REA*DCOS(MU)**3 )) )
!
! ----------- Computation of height above the ellipsoid
!
              VTD%STA(J2)%HEI_ELL = PP*DCOS(VTD%STA(J2)%LAT_GDT) + &
     &                VTD%STA(J2)%BEG_TRS(3)*DSIN(VTD%STA(J2)%LAT_GDT) - &
     &                VTD__REA* &
     &                DSQRT( 1.D0 - VTD__EXC_SQ*DSIN(VTD%STA(J2)%LAT_GDT)**2 )
!
! ----------- Computation of the local gravity accelration on the ellipsoid
!
              VTD%STA(J2)%GAC_ELL = VTD__ACC_EQU* &
     &               (1.D0 + VTD__GRV_LAT* DSIN(VTD%STA(J2)%LAT_GCN)**2 + &
     &               VTD__GRV_H*VTD__GRV_H )/ &
     &               DSQRT (1.D0 - VTD__EXC_SQ*DSIN(VTD%STA(J2)%LAT_GCN)**2 )
!
! ----------- Calculation matrix of transformation from REN (local topocentric,
! ----------- (Radial,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
              CF = DCOS(VTD%STA(J2)%LAT_GCN)
              SF = DSIN(VTD%STA(J2)%LAT_GCN)
              CL = DCOS(VTD%STA(J2)%LONG)
              SL = DSIN(VTD%STA(J2)%LONG)
!
              VTD%STA(J2)%REN_TO_TRS(1,1) = CF*CL
              VTD%STA(J2)%REN_TO_TRS(2,1) = CF*SL
              VTD%STA(J2)%REN_TO_TRS(3,1) = SF
!
              VTD%STA(J2)%REN_TO_TRS(1,2) = -SL
              VTD%STA(J2)%REN_TO_TRS(2,2) =  CL
              VTD%STA(J2)%REN_TO_TRS(3,2) =  0.D0
!
              VTD%STA(J2)%REN_TO_TRS(1,3) = -SF*CL
              VTD%STA(J2)%REN_TO_TRS(2,3) = -SF*SL
              VTD%STA(J2)%REN_TO_TRS(3,3) =  CF
!
! ----------- Calculation matrix of transformation from UEN (local topocentric,
! ----------- (Up,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
              CF = DCOS(VTD%STA(J2)%LAT_GDT)
              SF = DSIN(VTD%STA(J2)%LAT_GDT)
!
              VTD%STA(J2)%UEN_TO_TRS(1,1) = CF*CL
              VTD%STA(J2)%UEN_TO_TRS(2,1) = CF*SL
              VTD%STA(J2)%UEN_TO_TRS(3,1) = SF
!
              VTD%STA(J2)%UEN_TO_TRS(1,2) = -SL
              VTD%STA(J2)%UEN_TO_TRS(2,2) =  CL
              VTD%STA(J2)%UEN_TO_TRS(3,2) =  0.D0
!
              VTD%STA(J2)%UEN_TO_TRS(1,3) = -SF*CL
              VTD%STA(J2)%UEN_TO_TRS(2,3) = -SF*SL
              VTD%STA(J2)%UEN_TO_TRS(3,3) =  CF
            ELSE
!
! ----------- Special case of the station in the hell (i.e, geocenter)
!
              VTD%STA(J2)%LONG    = 0.0D0
              VTD%STA(J2)%LAT_GCN = 0.0D0
              VTD%STA(J2)%LAT_GDT = 0.0D0
              VTD%STA(J2)%HEI_ELL = 0.0D0
              VTD%STA(J2)%GAC_ELL = 0.0D0
              CALL NOUT_R8 ( 9, VTD%STA(J2)%REN_TO_TRS )
              VTD%STA(J2)%REN_TO_TRS(1,1) = 1.0D0
              VTD%STA(J2)%REN_TO_TRS(2,2) = 1.0D0
              VTD%STA(J2)%REN_TO_TRS(3,3) = 1.0D0
              VTD%STA(J2)%RAD = DSQRT ( VTD%STA(J2)%BEG_TRS(1)**2 + &
     &                                  VTD%STA(J2)%BEG_TRS(2)**2 + &
     &                                  VTD%STA(J2)%BEG_TRS(3)**2   )
!
! ----------- We do it in order to cheat SOTID_PRE
!
              C_STA(J2)   = VTD%STA(J2)%IVS_NAME
              R_TRS(1,J2) = VTD__REA
              R_TRS(2,J2) = 0.0D0
              R_TRS(3,J2) = 0.0D0
         END IF
!
         VTD%STA(J2)%STATUS = VTD__STRT
 420  CONTINUE
!
! --- Set parameters of the tidal model
!
      IF ( VTD%CONF%STD_2ND_MODEL == SOTID__2D_NONE ) THEN
           IPAR_2ND = SOTID__LOVE
           IPAR_ORD = SOTID__2D_NONE
         ELSE
           IPAR_2ND = VTD%CONF%STD_2ND_MODEL
           IPAR_ORD = SOTID__2D_012ORD
      END IF
!
! --- First for solid Earth tides model
!
      CALL ERR_PASS  ( IUER, IER )
      CALL SOTID_SET ( SOTID__GEN_ALL, IPAR_2ND, IPAR_ORD, &
     &                 VTD%CONF%STD_ZF_MODEL, VTD%CONF%STD_3RD_MODEL, &
     &                 VTD%TIDCNF_STD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2156, IUER, 'VTD_LOAD', 'Error in '// &
     &         'an attempt to load parameters of the tidal model '// &
     &         'into internal data structures' )
           RETURN
      END IF
!
      IF ( VTD%CONF%PTD_MODEL == SOTID__2D_NONE ) THEN
           IPAR_2ND = SOTID__LOVE
           IPAR_ORD = SOTID__2D_NONE
         ELSE
           IPAR_2ND = VTD%CONF%PTD_MODEL
           IPAR_ORD = SOTID__2D_012ORD
      END IF
!
! --- Then for the pole tide
!
      CALL ERR_PASS  ( IUER, IER )
      CALL SOTID_SET ( SOTID__GEN_ALL, IPAR_2ND, IPAR_ORD, &
     &                 VTD%CONF%STD_ZF_MODEL, SOTID__3D_NONE, VTD%TIDCNF_PTD, &
     &                 IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2157, IUER, 'VTD_LOAD', 'Error in '// &
     &         'an attempt to load parameters pf the tidal model '// &
     &         'into internal data structures' )
           RETURN
      END IF
!
! --- Compute time independent argumetns for solid Earth tides computation
!
      CALL ERR_PASS  ( IUER, IER )
      CALL SOTID_PRE ( VTD%L_STA, C_STA, R_TRS, VTD%TIDCNF_STD, VTD%STATID, &
     &                 IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2158, IUER, 'VTD_LOAD', 'Error in '// &
     &         'an attempt to compute time independent arguments for '// &
     &         'further computation of displacements caused by solid '// &
     &         'Earth tides' )
           RETURN
      END IF
!
! --- Load site position variations models
!
      CALL ERR_PASS ( IUER, IER )
      CALL VTD_POSVAR_INIT ( VTD, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2159, IUER, 'VTD_LOAD', 'Error in '// &
     &         'an attempt to initialize position variations interpolation '// &
     &         'polynomials' )
           RETURN
      END IF
!
! --- Load harmonic EOP file
!
      IF ( VTD%CONF%FINAM_HEO == VTD__NERS_STR ) THEN
           VTD%L_HEO = VTD__NERS
        ELSE IF ( ILEN(VTD%CONF%FINAM_HEO) .GT. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL READ_HEO ( VTD%CONF%FINAM_HEO, M__HEO, VTD%L_HEO, VTD%HEO, &
     &                     VTD%HEO_NAME, VTD%HEO_EPOCH_SEC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2160, IUER, 'VTD_LOAD', 'Error in '// &
     &              'an attempt to read harmonic Earth orientaiton '// &
     &              'parameters file' )
                RETURN
           END IF
         ELSE
           VTD%L_HEO = 0
      END IF
!
! --- Load file with B-spline coefficients of the empirical Earth rotation model
!
      IF ( ILEN(VTD%CONF%FINAM_ERM) == 0 ) THEN
           VTD%ERM%STATUS = VTD__NOAV
        ELSE 
           CALL ERR_PASS ( IUER, IER )
           CALL READ_ERM ( VTD%CONF%FINAM_ERM, VTD__M_ERD, VTD__M_ERM, &
     &                     VTD%ERM%DEGREE, VTD%ERM%NKNOTS, VTD%ERM%MJD_BEG, &
     &                     VTD%ERM%TAI_BEG, VTD%ERM%TIM, VTD%ERM%VAL, &
     &                     VTD%ERM%ERR, VTD%ERM%COV, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2161, IUER, 'VTD_LOAD', 'Error in '// &
     &              'an attempt to read the file with B-spline '// &
     &              'coefficients which describes variations in small '// &
     &              'vector of perturbation rotation of the planet Earth' )
                RETURN
           END IF
           VTD%ERM%STATUS = VTD__LOAD
      END IF
      IF ( ILEN(VTD%CONF%FINAM_ANTI) == 0 ) THEN
           VTD%STATUS_ANTI = VTD__NONE
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL ANTI_PARSE ( VTD%CONF%FINAM_ANTI, VTD%ANTI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2162, IUER, 'VTD_LOAD', 'Error in '// &
     &              'an attempt to read the file with antenna '// &
     &              'information '//VTD%CONF%FINAM_ANTI )
                RETURN
           END IF
           VTD%STATUS_ANTI = VTD__YES
      END IF
!
      IF ( ILEN(VTD%CONF%FINAM_AGD) == 0 ) THEN
           VTD%STATUS_AGD = VTD__NONE
         ELSE
           CALL ERR_PASS ( IUER, IER )
           CALL AGD_PARSE ( VTD%CONF%FINAM_AGD, VTD%AGD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2163, IUER, 'VTD_LOAD', 'Error in '// &
     &              'an attempt to read the file with antenna '// &
     &              'gravity deformations '//VTD%CONF%FINAM_AGD )
                RETURN
           END IF
           VTD%STATUS_AGD = VTD__YES
      END IF
!
      IF ( ILEN(VTD%CONF%FINAM_MMF) == 0 ) THEN
           VTD%STATUS_MMF = VTD__NONE
         ELSE
!!
!           C_SYN_STA = C_STA
!           CALL VTD_STA_SYN_REPL ( L_STA, C_SYN_STA )
!           CALL ERR_PASS ( IUER, IER )
!           CALL VTD_LOAD_MMF ( VTD%CONF%FINAM_MMF, L_STA, C_SYN_STA, VTD, IER )
!           IF ( IER .NE. 0 ) THEN
!                CALL ERR_LOG ( 2164, IUER, 'VTD_LOAD', 'Error in '// &
!     &              'an attempt to read the file with the MMF '// &
!     &              'maping function '//VTD%CONF%FINAM_MMF )
!                RETURN
!           END IF
!           VTD%STATUS_MMF = VTD__YES
!
!!
            CALL ERR_LOG ( 2165, IUER, 'VTD_LOAD', 'Option MMF is declared '// &
     &          'obsolete, and it is disabled' )
            RETURN 
      END IF
!
! --- Load SPD (slant path delay) data and compute interpolating spline
!
      IF ( ILEN(VTD%CONF%DIR_EPD(1)) > 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_SPD ( L_STA, C_STA, VTD, MJD_BEG, TAI_BEG, MJD_END, &
     &                         TAI_END, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2166, IUER, 'VTD_LOAD', 'Error in '// &
     &              'an attempt to load slant path delay from external '// &
     &              'files for one or more stations. VTD configuration '// &
     &              'file: '//VTD%CONF%CONFIG_FINAM )
                RETURN
           END IF
      END IF
!
      IF ( VTD%CONF%ATM_PARTIAL_TYPE == VTD__GL ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_MF ( VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2167, IUER, 'VTD_LOAD', 'Error in '// &
     &              'an attempt to compute GAUSSIAN_LAYER mapping '// &
     &              'function' )
                RETURN
           END IF
      END IF
!
! --- Load VIONO (ionosphere TEC maps) data and compute interpolating spline
!
      IF ( VTD%CONF%IONO_MODEL == VIO__GNSS_TEC     .OR. &
           VTD%CONF%IONO_MODEL == VIO__GNSS_TEC_MOD .OR. &
           VTD%CONF%IONO_MODEL == VIO__GNSS_TEC_MHI      ) THEN
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_IONO ( MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                          VTD__M_IOF, VTD%CONF%IONO_FILE, VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2168, IUER, 'VTD_LOAD', 'Error in '// &
     &              'an attempt to load global GPS TEC ionosphere maps' )
                RETURN
           END IF
      END IF
!
      IF ( ILEN(VTD%CONF%DIR_NZO) .GT. 0 ) THEN
           CALL ERR_PASS  ( IUER, IER )
           CALL VTD_LOAD_NZO ( VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2143, IUER, 'VTD_LOAD', 'Error in an '// &
     &              'attempt to load NZO data into VTD data structure' )
                RETURN
           END IF
      END IF
!
      IF ( VTD%STATUS_SOU .NE. VTD__BYPS ) THEN
!
! -------- Load source coordinates from the external file specifed in VTD%CONF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_SOUCOO ( VTD, L_SOU, C_SOU, MBUF, BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2144, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to load source coordinates from the input file '// &
     &               VTD%CONF%FINAM_SOUCOO )
                DEALLOCATE ( BUF )
                RETURN
           END IF
      END IF
!
      IF ( VTD%STATUS_SOU .NE. VTD__BYPS .AND. ILEN(VTD%CONF%FINAM_SOUPRL_PRP) > 0 ) THEN
!
! -------- Load source coordinates from the external file specifed in VTD%CONF
!
           CALL ERR_PASS ( IUER, IER )
           CALL VTD_LOAD_SOUPRL ( VTD, MBUF, BUF, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2145, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to load source coordinates from the input file '// &
     &               VTD%CONF%FINAM_SOUCOO )
                DEALLOCATE ( BUF )
                RETURN
           END IF
      END IF
!
      IF ( ILEN(VTD%CONF%FINAM_STRUC) .GT. 0 ) THEN
!
! -------- Load source maps
!
           CALL ERR_PASS  ( IUER, IER )
           CALL VTD_READ_STRUC ( VTD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2169, IUER, 'VTD_LOAD', 'Error in '// &
     &              'an attempt to parse the file '// &
     &               VTD%CONF%FINAM_STRUC(1:I_LEN(VTD%CONF%FINAM_STRUC))// &
     &              ' with source structures' )
                RETURN
           END IF
      END IF
!
      DEALLOCATE ( BUF )
      VTD%STATUS = VTD__LOAD
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_LOAD  !#!#
