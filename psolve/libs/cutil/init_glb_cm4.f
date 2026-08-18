      SUBROUTINE INIT_GLB_CM4 ( HFEOP_CAL_FILE )
!
!     purpose: init_glb_cm4 initializes selected variables in the glbfil.
!              This subroutine has been written to correct a failure to
!              initialize and re-initialize certain glbcm and glbc4 variables
!              in solve, solve_reset, sdbh/blkcl, gtsup and sskedh, so it
!              will only set those problem variables.
!              NOTE: This was written on the assumption that it would be
!              called in interactive mode.
!
      Implicit none
      INCLUDE 'astro_constants.i'
      INCLUDE 'precm.i'
      INCLUDE 'solve.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
!
!     input variables
!
!     hfeop_cal_file - name of the file of high frequency earth orientation
!                      calibrations
!
      character*(*) hfeop_cal_file
!
!
!     history
!     who   when   what
!     kdb  960530  Created.
!     kdb  960613  New site weighting feature (add weighting_type).
!     kdb  970610  New glbcm variable (intracom_reway).
!     kdb  971029  New glbcm variable, sigma_type.  (From 970401)
!     kdb  980305  New glbcm variables, ksinex and lsnxdir.
!     pet  2005.01.27  New glbc4 variable  FRINGE_ROOT_DIR 
!     pet  2005.03.16  Added initialization of ADR_HEO, ADR_SPE, ADR_BSP,
!                                              L_HEO, L_SPE, L_BSP
!     pet   2006.01.18  Added initialization of parameters related to ERM estimation
!     pet   2007.11.06  Added initialization of parameters related to &
!                       decimiation and theoretical path delay files support
!     pet   2023.12.22  Added initiaization o VCAT_CONF and VCAT_REPO
!
!     local variables
!
      INTEGER*4  MC
      PARAMETER  ( MC = 1024 )
      CHARACTER  BUF(MC)*128, STR*32
      CHARACTER*(NAME_SIZE) BLANK
      DATA BLANK(1:NAME_SIZE)/' '/
      INTEGER*2  ICT, JCT
      INTEGER*4  NC, J1, IP, IUER
      INTEGER*4, EXTERNAL :: ILEN
!
! --- Open and read the first and fourth parts of the glbfil.
!
      CALL USE_GLBFIL('OR')
      CALL USE_GLBFIL_4('RC')
!
! --- Initialize the desired variables.
!
! --- Clear the output cgm name
!
      ONAMCG = BLANK
!
! --- Turn off the source constraint feature
!
      KSRC_CONST = .FALSE.
!
! --- turn off the flag that indicates that Solve is running in batch mode,
! --- in the subtraction solution type
!
      SUBTRACT_ARC = .FALSE.
!
! --- set ion correction flag (turn old ionosphere off)
!
      KIONO = .TRUE.
!
! --- Turn off user constraint program feature
!
      KUSER_CONST = .FALSE.
      USER_CONST_PROG = ' '
!
! --- Turn off the elevation dependent noise feature
!
      keldep_noise = .FALSE.
      eldep_file=' '
!
! --- Reset the minimum reweight values to the standard values.
!
      MIN_ET(1) =   15.D-12          ! Group delay 15 ps
      MIN_ET(2) =  100.D-15          ! Phase delay rate 100 fs/s
      MIN_ET(3) =    5.D-12          ! Phase delay  5 ps
      MIN_ET(4) =    0.D0            ! Not currently used.
!
! --- Reset the site plate map file to the standard choice
!
      SITPL_FIL = SITPL_FILE
!
! --- Turn off source weighting
!
      SOURCE_WEIGHT_FILE = ' '
!
! --- Turn off data decimation.
!
      EVINT = 0
      EVSTART = 0
!
! --- reset the site file used in the no net translation constraint to the
! --- standard site file.
!
      STA_WT_FIL = STATION_WEIGHT_FILE
!
! --- Turn off the feature that calculates the epoch for which sigmas are at
! --- a minimum.
!
      MINIMIZE_SIGS = .FALSE.
!
! --- If a simulation data base is read in, only use observations with
! --- good weights.  (That is, turn off the feature that uses all observations
! --- from a simulation data base, regardless of their weights.)
!
      ALL_SIM_FLG = .FALSE.
!
! --- Turn off a batch feature (a factor by which earth orientation sigmas
! --- should be multiplied.)
!
      EOP_FACTOR = 0.0D0
!
! --- If a station table is selected, print it with the standard number of
! --- years, starting in the default year.
!
      STAPOSEPOCH = 0
      STAPOSNUM   = 0
!
! --- Make sure that solve uses a clock reference station.  (This makes sure
! --- that a special batch feature that permits skipping of a station is
! --- turned off.)
!
      REQREF = .TRUE.
!
! --- Turn off the offset monument file.  (Currently not used.)
!
      LMNAM(1)=2H::
!
! --- Turn the use of the high frequency earth orientation
! --- calibration file on or off, according to the caller's request.
! --- (If hfeop_cal_file is NONE or blank, the calibrations are disabled.)
!
      CALL SHFEOP_INT ( HFEOP_CAL_FILE )
!
! --- Turn off the estimation of the high frequency earth orientation
! --- parameters.
!
      DO ICT=1,MAX_SDC
         DO JCT=1,2
            SDE_VAL(JCT,ICT) = 0.D0
         ENDDO
         DO JCT=1,6
            SDE_ARG(JCT,ICT) = 0
         ENDDO
      ENDDO
      NUM_SDE_UT1 = 0
      NUM_SDE_XY = 0
!
! --- Ignore any eop sigmas left over from the control file of the last batch
! --- run.
!
      MDORCTL = 1
!
! --- Set the solution type to independent.  Theoretically, the concept of
! --- a solution type is only meaningful in batch, but in practice,
! --- an interactive solution that is iterating weights to unity will behave
! --- somewhat like a batch solution, and it will consult the solution type
! --- in two places:
!
! ---   solution type      uses arc files?        prints adjst parameters?
!
!            I                 no                         yes
!         not I                yes                        no
!
! --- In the past islty2 defaulted randomly to ' ', but it is now considered
! --- preferable to avoid the arc files and print the adjst parameters.
!
      islty2 = 'I'
!
! --- In nuvel site position mapping, do not fix a plate.  (That is, do
! --- no net rotation.)
!
      NUVEL_FIXED = 'NONE'
!
! --- Weighting_type gives the type of weighting for the current solution.
! --- Batch directly sets weighting_type to 'BL', 'ST' or 'DB'
! --- (baseline, site or by arc weighting).  The interactive mode sets the
! --- weighting_type to '??' (unknown) initially, indicating that the type
! --- must be determined before being used.
!
      WEIGHTING_TYPE = '??'
      INTRACOM_REWAY = 1
!
! --- Initially plot pre-fit sigmas, the original type, in cnplt.
!
      SIGMA_TYPE = 'PR'
!
! --- Clear FRINGE_ROOT_DIR
!
      CALL CLRCH ( FRINGE_ROOT_DIR )
!
      CALL CLRCH ( FINAM_HEO )
      CALL CLRCH ( NAME_HEO )
      CALL CLRCH ( VTD_CONF_SES )
      CALL CLRCH ( EDC_DIR )
      FL_VTD_SES = .FALSE.
      VTD_STATUS = 0
      L_HEO  = 0
      L_SPE  = 0
      L_HEO  = 0
      L_BSP  = 0
      L_EERM = 0
      L_MERM = 0
      L_EHEO = 0
      L_EHEC = 0
      L_AEM  = 0
      ADR_HEO  = 0
      ADR_HEO  = 0
      ADR_SPE  = 0
      ADR_BSP  = 0
      ADR_EERM = 0
      ADR_MERM = 0
      ADR_EHEO = 0
      ADR_EHEC = 0
      ADR_AEM  = 0
      SOU_ADM_FLAG = SOUADM__NO
      SOU_ADM_CNS = 0.0D0
      EDC_USE  = EDC__UNDF
      EDC_PAR  = 0
      TPD_DIR  = PRE_SCR_DIR
      TPD_FLAG = TPD__UPD
      N_TPD_INIT = 0
      SOU_EST_EPOCH = J2000__JD
!
      STAT_HEO = 0
      HEO_EPOCH_SEC = 0.0D0
      RWT_EL_USE    = SOLVE__NO
      RWT_SRC_USE   = SOLVE__NO
      CALL CLRCH ( RWT_SRC_FIL    )
      CALL CLRCH ( RWT_STA_EL_FIL )
      CALL CLRCH ( TRP_DIR        )
      TRP_USE = 0
      N_FIL_TRP = 0
      ADR_TRP_FIL_BUF = 0
      STS_TRP_FIL = 0
      ADR_TRP_SES_BUF = 0
      STS_TRP_SES = 0
      ADR_TRP = 0
      STS_TRP = 0
      NUT_USE_CODE = 0
      ATD_USE      = 0
      ATD_MEA_USE  = 0 
      ATD_ADR      = 0
      ATD_STS      = 0
      SNR_MIN_X    = 0.0D0
      SNR_MIN_S    = 0.0D0
      DTEC_ERR_SCL = 1.0D0
      DTEC_SBA_USE = .FALSE.
      CALL CLRCH ( AOC_FIL )
      CALL CLRCH ( ADDW_FIL )
      CALL CLRCH ( EDIT_FIL )
      CALL CLRCH ( ATD_FIL )
      CALL CLRCH ( ATD_MEA_FIL )
      CALL CLRCH ( TEC_NOISE_FILE )
      FL_NO_IONO_DEL = .FALSE.
      SESS_REWEI_SCALE = 1.0D0
      SESS_REWEI_QUADR = 0.0D0
      CALL CLRCH ( EDIT_FIL )
      CALL CLRCH ( ADDW_FIL )
      CALL CLRCH ( DTEC_FIL )
      CALL CLRCH ( EXT_ERR_FIL )
      TEC_SCAL = 0.0
      TEC_BIAS = 0.0
      ADDW_SCL = 1.0
      GIM_COLLECT_INFO = .FALSE. ! Flag whether to collect information about TEC from dual-band VLBI data and from GNSS
      GIM_EST          = .FALSE. ! Flag whether to run estimation of a model for TEC bias
      GIM_RGR          = .FALSE. ! Flag whether to run a regression model for TEC errors
      GIM_WRI          = .FALSE. ! Flag whether to write adjuisted information about differential TEC to the last version of the database
!
! --- Set VCAT configuation file
!
      CALL GETENVAR ( 'VCAT_CONF', VCAT_CONF_FILE )
      IF ( ILEN(VCAT_CONF_FILE) == 0 ) THEN
           VCAT_CONF_FILE = SOLVE_SAVE_DIR//'/vcat.conf'
      END IF
      IF ( ILEN(VCAT_REPO) == 0 ) THEN
!
! -------- VCAT repo is not defined. Let us extrat if from the VCAT 
! -------- configuration file
!
           CALL RD_TEXT ( VCAT_CONF_FILE, MC, BUF, NC, IUER )
           IF ( IUER == 0 ) THEN
                DO 410 J1=1,NC
                   IF ( BUF(J1)(1:14) == 'GVF_REP_NAMES:' ) THEN
!
! --------------------- set VCAT_REPO to the secnd word of GVF_REP_NAMES
!
                        STR = BUF(J1)(15:)
                        CALL CHASHL ( STR )
                        IP = INDEX ( STR, ' ' )
                        IF ( IP == 0 .OR. IP > 4 ) IP = 4
                        VCAT_REPO = STR(1:IP) 
                   END IF
 410            CONTINUE 
              ELSE
                VCAT_REPO = 'UNKN'
           END IF
      END IF
!
! --- Write the variables to the glbfil and close it.
!
      CALL USE_GLBFIL   ( 'OW' )
      CALL USE_GLBFIL_4 ( 'WC' )
!
      RETURN 
      END  SUBROUTINE  INIT_GLB_CM4  !#!#
