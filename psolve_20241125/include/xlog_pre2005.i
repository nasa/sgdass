!
! >>>>> INCLUDE-BLOCK with descriptions of data structures used for XLOG
!
!       XLOG.I
!
!       AUTHOR: Wolfgang Schwegmann, DGFI, Munich, MAR-1999
!
!       LAST UPDATE: 11-OCT-1999  W. Schwegmann
!
! **********************************************************************
! --- Input and output variables
! **********************************************************************
!
      INTEGER*4 ICRT, KBRD, P0, P1, P2, P3
      PARAMETER ( ICRT = 6 )
      PARAMETER ( KBRD = 5 )
      PARAMETER ( P0 = 0 )
      PARAMETER ( P1 = 1 )
      PARAMETER ( P2 = 2 )
      PARAMETER ( P3 = 3 )
!
! **********************************************************************
! --- Limits specifications
! **********************************************************************
!
      INTEGER*4 MNIC, MNCD, MAXYR, MINYR, MNDP, IN_UNIT, OUT_UNIT, &
     &          CH_YEAR, MAX_LEN, NUM_C, MNS
      PARAMETER ( CH_YEAR = 70    ) ! If year of experiment in MarkIV
! log-files is greater than CH_YEAR,
! then we add 1900 to year of
! experiment. Otherwise we add 2000
      PARAMETER ( MINYR = 1950    ) ! Year of experiment must be greater
! than MINYR
      PARAMETER ( MAXYR = 2050    ) ! Year of experiment must be less
! than MAXYR
      PARAMETER ( MNIC = 50       ) ! Max number of input-files in
! control-file of xlog
      PARAMETER ( MNCD = 10       ) ! Number of characters of MarkIII
! database name
      PARAMETER ( MNDP = 3000     ) ! Max number of data points
      PARAMETER ( MNS = 150     )   ! Max number of stations in array ASTN
      PARAMETER ( IN_UNIT = 1234  ) ! Number of unit to open input-file
! and to read from input-file
      PARAMETER ( OUT_UNIT = 4321 ) ! Number of unit to open output-file
! and to write to output-file
      PARAMETER ( MAX_LEN = 256 )   ! Max. length of strings which keep
! file names.
      PARAMETER ( NUM_C = 14 )      ! Number of channels
!
! **********************************************************************
! --- Variables to detect the kind of input-file to be processed
! --- with XLOG
! **********************************************************************
!
      INTEGER*2 PLACE8, PLACE9, PLACECB1, PLACECB2, PLACEWX1, PLACEWX2, &
     &          PLACEN
      CHARACTER IDENT8*1, IDENT9*1, IDENTN*1, IDENTCB*9, IDENTWX*12
      PARAMETER ( PLACEN   =  21 ) ! If the character at placen in each
      PARAMETER ( IDENTN   = ';' ) ! line of the input-file fits identn,
! then the input-file is log-file
! with Field System 9+ and new format
! for the time entries
! (1999:116.11:05:36:87)
      PARAMETER ( PLACE8   =  10 ) ! If the character at place8 in each
      PARAMETER ( IDENT8   = ';' ) ! line of the input-file fits ident8,
! then the input-file is log-file
! with Field System version 8 or less
      PARAMETER ( PLACE9   =  14 ) ! If the character at place9 in each
      PARAMETER ( IDENT9   = ';' ) ! line of the input-file fits ident9,
! then the input-file is log-file
! with Field System version 9+
      PARAMETER ( PLACEWX1 =  15 ) ! If the characters between place 15
      PARAMETER ( PLACEWX2 =  26 ) ! and 26 of the second line of the
! input-file fit identwx, then the
! input-file is an existing met-file
      PARAMETER ( PLACECB1 =  15 ) ! If the characters between place 15
      PARAMETER ( PLACECB2 =  23 ) ! and 23 of the second line of the
! input-file fit identcb, then the
! input-file is an existing cable-file
      PARAMETER ( IDENTWX  = 'WEATHER DATA' )
      PARAMETER ( IDENTCB  = 'CABLE CAL'    )
!
! **********************************************************************
! --- Limits specifications for the check of the data
! **********************************************************************
!
      REAL*8 W_TJUMP, W_PJUMP, W_HJUMP, W_TIME, T_JUMP, P_JUMP, H_JUMP, &
     &       T_OFF1, T_OFF2, P_OFF1, P_OFF2, H_OFF1, H_OFF2, T_OUT1, &
     &       T_OUT2, P_OUT1, P_OUT2, H_OUT1, H_OUT2, T_MAX, T_MIN, &
     &       P_MAX, P_MIN, H_MAX, H_MIN, T_DAVG, P_DAVG, H_DAVG, &
     &       T_MAXGRAD1, P_MAXGRAD1, H_MAXGRAD1, T_MAXGRAD2, &
     &       P_MAXGRAD2, H_MAXGRAD2, STRONG, WEAK
      PARAMETER ( W_TJUMP =    6.5 ) ! Parameter W_TJUMP is used to
! detect a probable jump in the
! temperature data
      PARAMETER ( W_PJUMP =    7.0 ) ! Parameter W_PJUMP is used to
! detect a probable jump in the
! pressure data
      PARAMETER ( W_HJUMP =   25.0 ) ! Parameter W_HJUMP is used to
! detect a probable jump in the
! humidity data
      PARAMETER ( W_TIME  = 0.0625 ) ! ~ 90 minutes
      PARAMETER ( T_JUMP  =   10.5 ) ! Parameter T_JUMP is used to detect
! jumps in the temperature data
      PARAMETER ( P_JUMP  =    9.0 ) ! Parameter P_JUMP is used to
! detect jumps in the pressure data
      PARAMETER ( H_JUMP  =   25.8 ) ! Parameter H_JUMP is used to
! detect jumps in the humidity data
      PARAMETER ( T_OFF1  =    8.0 ) ! Parameter T_OFF1, T_OFF2 are used
      PARAMETER ( T_OFF2  =    5.0 ) ! to detect offsets in the
! temperature data
      PARAMETER ( P_OFF1  =    8.0 ) ! Parameter P_OFF1, P_OFF2 are used
      PARAMETER ( P_OFF2  =    5.0 ) ! to detect offsets in the pressure
! data
      PARAMETER ( H_OFF1  =   25.0 ) ! Parameter H_OFF1, H_OFF2 are used
      PARAMETER ( H_OFF2  =   20.0 ) ! to detect offsets in the humidity
! data
      PARAMETER ( T_OUT1  =    3.9 ) ! Parameter T_OUT1, T_OUT2 are used
      PARAMETER ( T_OUT2  =   15.0 ) ! to detect single outliers in the
! temperature data
      PARAMETER ( P_OUT1  =    6.5 ) ! Parameter P_OUT1, P_OUT2 are used
      PARAMETER ( P_OUT2  =   10.0 ) ! to detect single outliers in the
! pressure data
      PARAMETER ( H_OUT1  =    8.0 ) ! Parameter H_OUT1, H_OUT2 are used
      PARAMETER ( H_OUT2  =   15.0 ) ! to detect single outliers in the
! humidity data
      PARAMETER ( T_DAVG  =   20.0 ) ! Parameter T_DAVG is used to check
! if average(temp.)-temperature(i)
! is too high
      PARAMETER ( P_DAVG  =   25.0 ) ! Parameter P_DAVG is used to check
! if average(pressure)-pressure(i)
! is too high
      PARAMETER ( H_DAVG  =   69.0 ) ! Parameter H_DAVG is used to check
! if average(humidity)-humiditiy(i)
! is too high
      PARAMETER ( T_MAX   =   45.0 ) ! Maximum of temperature values
      PARAMETER ( T_MIN   = - 40.0 ) ! Minimum of temperature values
      PARAMETER ( P_MAX   = 1100.0 ) ! Maximum of pressure values
      PARAMETER ( P_MIN   =  790.0 ) ! Minimum of pressure values
      PARAMETER ( H_MAX   =  125.0 ) ! Maximum of humidity values
      PARAMETER ( H_MIN   =   -7.0 ) ! Minimum of humidity values
      PARAMETER ( T_MAXGRAD1 = 0.5 ) ! Maximum of gradient of a tempera-
      PARAMETER ( T_MAXGRAD2 = 2.0 ) ! ture value; are used to detect
! offsets
      PARAMETER ( P_MAXGRAD1 = 0.5 ) ! Maximum of gradient of a pressure
      PARAMETER ( P_MAXGRAD2 = 2.1 ) ! value; are used to detect offsets
      PARAMETER ( H_MAXGRAD1 = 4.5 ) ! Maximum of gradient of a humidity
      PARAMETER ( H_MAXGRAD2 =15.0 ) ! value; is used to detect offsets
      PARAMETER ( STRONG = 0.9     ) ! Parameter PARA of structure
! CONTROL_STRU is multiplied by STRONG
! to check the data using a stronger
! set of parameters
      PARAMETER ( WEAK   = 1.1     ) ! Parameter PARA of structure
! CONTROL_STRU is multiplied by WEAK
! to check the data using a weaker
! set of parameters
!
! **********************************************************************
! --- Station dependent parameters
! **********************************************************************
!
      INTEGER*4 NYAL_T, NYAL_P, NYAL_H
      PARAMETER ( NYAL_T = 20 ) ! Field number of temperature in
! NyAlesund met-file
      PARAMETER ( NYAL_P = 23 ) ! Field number of pressure in NyAlesund
! met-file
      PARAMETER ( NYAL_H = 22 ) ! Field number of humidity in NyAlesund
! met-file
!
! **********************************************************************
! --- Data structure to store the informations from the control-file
! --- of XLOG
! **********************************************************************
!
      TYPE      CONTROL_STRU
        CHARACTER*8 ASTN(MNS,4)      ! Array to hold station names read
! from file in CONTROL_S.S_NAMES
        CHARACTER OUT_DIR*(MAX_LEN) ! Output directory of XLOG
        CHARACTER OUT_FIL*(MAX_LEN) ! Output-filename of XLOG
        CHARACTER S_NAMES*(MAX_LEN) ! Name of file which contains the
! informations about station names
        INTEGER*4 COUNTER    ! Number of input-files in the control-file
        CHARACTER*80 COMMENT ! Information from the first line
        INTEGER*4    XMOD    ! User-mode in which XLOG will be run
        REAL*8       PARA    ! PARA is used to change the parameter set
! for the plausibility check of the data
        CHARACTER*14 EXP     ! History entry for the met- and cab-files
! and name of DBCAL control-file
        CHARACTER*1  TSYS    ! Specifies if the system temperature data
! have to be read from the log-files
        CHARACTER*(MAX_LEN) INPUT_FILES(MNIC+1)
! Array with input-filenames
        CHARACTER*(MAX_LEN) DB_WX_FIL(MNIC)
! Array with wx calibration-files to be
! processed with DBCAL
        CHARACTER*(MAX_LEN) DB_CB_FIL(MNIC)
! Array with cable calibration files
! to be processed with DBCAL
        CHARACTER*(MAX_LEN) CNTRL_FIL
! Name of the control file of XLOG
        CHARACTER*(MAX_LEN) DBCAL_FIL
! Name of the control file of DBCAL
        CHARACTER*(MNCD) DATABASE
! Database which should be used by DBCAL
        CHARACTER*80 HISTORY ! History entry for DBCAL
        CHARACTER*2 OPTION   ! Contains the option choosen in DIAGI
! when plotting the data
        LOGICAL*4 PRINT_CB   ! XLOG prints the cable data, if PRINT_CB
! is true
        LOGICAL*4 PRINT_TE   ! XLOG prints the temperature data,
! if PRINT_TE is true
        LOGICAL*4 PRINT_PR   ! XLOG prints the pressure data,
! if PRINT_PR is true
        LOGICAL*4 PRINT_HU   ! XLOG prints the humidity data,
! if PRINT_HU is true
        LOGICAL*4 SAVE_CB    ! XLOG saves the cable data, if SAVE_CB
! is true
        LOGICAL*4 SAVE_WX    ! XLOG saves the wx data, if SAVE_WX is true
        LOGICAL*4 SAVE_ST    ! XLOG saves the system temperature data,
! if SAVE_ST is true
        LOGICAL*4 PROB       ! PROB is true if there are any suspicious
! and not deleted data
        LOGICAL*4 DBCAL      ! DBCAL is true if DBCAL control-file will be
! written by XLOG
        CHARACTER SKED*(MAX_LEN) ! Name of the sked file of the experiment
      END TYPE  CONTROL_STRU
!
! **********************************************************************
! --- Data structure to hold information about the actual input-file
! **********************************************************************
!
      TYPE      INPUT_STRU
      INTEGER*4        INTYPE      ! Type of the input-file
      CHARACTER*(MAX_LEN) FILNAM   ! Name of the input-file
      CHARACTER*(MAX_LEN) FILNAM2  ! Name of additional met-input-file if
! log-file contains no met-data
      REAL*8           JDAY        ! Julian day at start of year of
! the experiment
      CHARACTER*100     IBFR       ! String to store a line from the
! log-file for further processing
      CHARACTER*8      ISTN        ! Station name
      INTEGER*4        NYEAR       ! Year of the experiment
      REAL*8           BEGEXP      ! Begin of the experiment
      REAL*8           ENDEXP      ! End of the experiment
      INTEGER*4        FDOYWX      ! Day of year of first wx point
      INTEGER*4        FDOYCB      ! Day of year of first cable point
      INTEGER*4        FDOYST      ! Day of year of first system
! temperature point
      INTEGER*4        NWX         ! Number of points with weather data
      INTEGER*4        NCB         ! Number of points with cable data
      INTEGER*4        NST         ! Max number of points with system
! temperature data
      INTEGER*4        NSTA(14)    ! Array to hold the number of points
! of the system temperature data
      REAL*8           XW(MNDP)    ! Array of the time of wx points
      REAL*8           YW1(MNDP)   ! Array with temperature data
      REAL*8           YW2(MNDP)   ! Array with pressure data
      REAL*8           YW3(MNDP)   ! Array with humidity data
      REAL*8           XS(14,MNDP) ! Array of the time of system
! temperature points
      REAL*8           YS(14,MNDP) ! Array with system-temperature data
      REAL*8           XC(MNDP)    ! Array of the time of cable
! calibration points
      REAL*8           YC(MNDP)    ! Array with cable calibration data
      INTEGER*4        NWXO        ! Original number of points with
! weather data
      INTEGER*4        NCBO        ! Original number of points with
! cable data
      INTEGER*4        NSTO        ! Original number of points with system
! temperature data
      REAL*8           XWO(MNDP)   ! Array of the time of original
! wx points
      REAL*8           YW1O(MNDP)  ! Array with origianl temperature
! data
      REAL*8           YW2O(MNDP)  ! Array with original pressure data
      REAL*8           YW3O(MNDP)  ! Array with original humidity data
      REAL*8           XSO(MNDP)! Array of the time of original
! system-temperature points
      REAL*8           YSO(14,MNDP)! Array with original system
! temperature data
      REAL*8           XCO(MNDP)   ! Array of the time of original cable
! calibration points
      REAL*8           YCO(MNDP)   ! Array with original cable
! calibration data
      CHARACTER*1      SIGN        ! Cable sign
      CHARACTER*1      DEFSIGN     ! Default cable sign
      LOGICAL*4        LSIGN       ! Cable sign couldn't be detected
! automatically, if LSIGN is false
      INTEGER*4        DELWX       ! Number of deleted wx points
      INTEGER*4        DELCB       ! Number of deleted cable points
      INTEGER*4        DELST       ! Number of deleted system temperature
! points
      REAL*8           GAPWX       ! Sum of gaps in the meteorological
! data
      REAL*8           GAPCB       ! Sum of gaps in the cable
! calibration data
      LOGICAL*4        GAP         ! GAP is .FALSE. if input file is special
! NyAlesund met-file; in this case gaps between first point with
! met data and start of experiment and between last point with
! met data and end of experiment won't be determined.
      LOGICAL*4        KEEP_ALL
      END TYPE  INPUT_STRU
!
! **********************************************************************
! --- Data structure to hold information concerning the output of XLOG
! **********************************************************************
!
      TYPE      OUTPUT_STRU
        CHARACTER*(MAX_LEN) KWX    ! Name of the file in which the
! meteorological data should be stored
        CHARACTER*(MAX_LEN) NCABL  ! Name of the file in which the cable
! calibration data should be stored
        CHARACTER*(MAX_LEN) NST    ! Name of the file in which the system
! temperature data should be stored
      END TYPE  OUTPUT_STRU
!
! **********************************************************************
! --- Data structure to hold the statistics of the data
! **********************************************************************
!
      TYPE      STAT_STRU
        REAL*8 GRAD(MNDP)  ! GRAD(I) is the gradient between point I
! and I+1
        REAL*8 DTIME(MNDP) ! Difference in time (in minutes) between
! point I and I+1
        REAL*8 AVG         ! Average of the data
        REAL*8 STD         ! Standard deviation
        REAL*8 MAX         ! Maximum
        REAL*8 MIN         ! Minimum
      END TYPE  STAT_STRU
!
! **********************************************************************
! --- Data structure to hold the frequency information from the
! --- schedule file
! **********************************************************************
!
      TYPE      SKED_STRU
        CHARACTER*8 S_NAMES(MNIC)     ! 8 Character station names
        CHARACTER*10 LO(MNIC, NUM_C)  ! Total LO in Mhz
        CHARACTER*4 IF(MNIC, NUM_C)   ! IF distributor channel name
        CHARACTER*10 SKY(MNIC,NUM_C)  ! Observing frequency in Mhz
        INTEGER*4 S_IND               ! Number of station in the sked file
        LOGICAL*4 ALL_STATIONS        ! if true: one frequency setup for all
! stations of the experiment
      END TYPE  SKED_STRU
!
! **********************************************************************
! --- Specifications for using DIAGI_MUL
! **********************************************************************
!
      INTEGER*4 MAX_FUNC
      PARAMETER ( MAX_FUNC = 14 ) ! Maximal number of functions to be
! plotted with diagi
!
! **********************************************************************
! --- Data structure for plotting the data
! **********************************************************************
!
      TYPE      PLOT_STRU
        INTEGER*4 N1_TAB(MAX_FUNC)  ! N1_TAB(K) is the number of points
! of the k-th functions to be plotted
        INTEGER*4 N2                ! Current number of functions to be
! plotted
        CHARACTER*160 TITLE         ! Title message to put at the header
! of the plot
        REAL*8 T(MNDP, MAX_FUNC)    ! Array of arguments: K-th column of
! the matrix T contains MNDP values
! of the argument
        REAL*8 X(MNDP, MAX_FUNC)    ! Array of values: K-th column of the
! matrix X contains MNDP values of
! the function
      END TYPE  PLOT_STRU
!
! **********************************************************************
! --- Data structure to hold parameters which can be changed
! **********************************************************************
!
      TYPE      PARAMS_STRU
        INTEGER*4 GAP                ! If the difference in time of two
! following points with meteorolo-
! gical or cable calibration data is
! greater than GAP (in hours), XLOG
! detects a gap, sums all detected
! gaps and sends a warning message
! about the sum of all gaps.
        LOGICAL*4 B_GAP              ! If B_GAP is true XLOG searches for
! a gap between time of the start of
! the experiment and time of the first
! point with meteorological or cable
! calibration data
        LOGICAL*4 E_GAP              ! If E_GAP is true XLOG searches for
! a gap between time of the end of
! the experiment and time of the last
! point with meteorological or cable
! calibration data
        INTEGER*4 ABSDEL             ! XLOG sends a warning message if more
! than (ABSDEL)% of all points were deleted
        INTEGER*4 EQUAL              ! XLOG sends a warning message if
! more than (EQUAL)% of all points are equal
      END TYPE  PARAMS_STRU
! **********************************************************************
!
!
!
!
