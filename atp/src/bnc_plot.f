      PROGRAM BNC_PLOT
!
! **************************************************************************************
! *                                                                                    *
! *   Program BNC_PLOT                                                                 *
! *                                                                                    *
! *   N.B: If the 3rd Argument is -tim_rng then the 4th argument is actually two dates *
! *        dates, therefore all other arguments are increased by a value of 1, i.e.,   *
! *        what is "-pol" is now the 6th argument, "-ord" is the 8th, etc.             *
! *                                                                                    *
! *   Usage: -exp                                  sde|vlbi                            *
! *          -i                                    input_file                          *      
! *          -frq_ind|-frq_val|-tim_ind|-tim_rng   index|freq|index|time_range         *
! *          -pol                                  polarization                        *
! *          -ord                                  plot_ordinate                       *
! *          -var                                  plot_variable                       * 
! *          -del                                  tim_dif_max                         *
! *          -plot                                 raw|ave                             *
! *          -o                                    output                              *
! *                                                                                    *
! *   1st Argument: experiment type flag. Supports:                                    *
! *       -exp --                                                                      *
! *   2nd Argument: experiment type. Supports:              { char }                   *
! *       sde, and vlbi                                                                *
! *   3rd Argument: input file flag. Supports:                                         *
! *       -i -- binary file                                 { char }                   *
! *   4th Argument: path to binary file                     { char }                   *
! *   5th Argument: plotting value flag. Supports:                                     *
! *       -frq_ind -- Frequency index to plot               { char }                   *
! *       -frq_val -- Frequency value (or nearest) to plot  { char }                   *
! *       -tim_ind -- Time index to plot                    { char }                   *
! *       -tim_rng -- Range of time slots to plot between   { char }                   *
! *   6th Argument:                                                                    *
! *       if -frq_ind -- index || tot                       { int*4 }                  *
! *       if -frq_val -- frequency || tot                   { real*8 } [MHz]           *
! *       if -tim_ind -- index     || tot                   { int*4 }                  *
! *       if -tim_rng -- date ranges             { char } + { char }   [Psolve Format] *
! *   7th Argument: polarization flag. Supports:                                       *
! *       -pol --                                                                      *
! *   8th Argument: Polarizations. Supports:                                           *
! *       x, y, r, l, h, and v                                                         *
! *   9th Argument: Plot ordinate flag. Supports:                                      *
! *       -ord --                                                                      *
! *   10th Argument: Accepted labels for the ordinate                                   *
! *       tsys -- System Temperatures                                   [K]            *
! *       phas -- Phase Calibration                                     [rad]          *
! *       ampl -- Amplitude                                             []             *
! *       fmgt -- GPS timer difference                                  [10^-6 s]      *
! *       fmpt -- PPS timer difference                                  [10^-9 s]      *
! *   11th Argument: Plot abscissa. Supports                                            *
! *       -var --                                                                      *
! *   12th Argument: Accepted labels for the variable                                  *
! *       time -- time                                                  [hrs]          *
! *       elev -- elevation                                             [deg]          *
! *       azim -- Azimuth                                               [deg]          *
! *       azel -- azimuth and elevation                                 [deg]          *
! *       spectr -- spectrum                                            [GHz]          *
! *                 IF ( -ord == tsys ) THEN                                           *
! *                     IF ( -var == time ) THEN                                       *
! *                          -- Tsys [K] vs run Time [Hrs] at IF                       *
! *                          -- Tsys_ave [K] vs run Time [Hrs]                         *
! *                     ELSEIF ( -var == elev ) THEN                                   *
! *                          -- Tsys [K] vs Elevation [deg] at IF                      *
! *                     ELSEIF ( -var == azim ) THEN                                   *
! *                          -- Tsys [K] vs Azimuth [deg] at IF                        *
! *                     ELSEIF ( -var == azel ) THEN                                   *
! *                          -- [X,Y,Z] = [ Az, El, Tsys_Range]                        *
! *                     ELSEIF ( -var == spectr ) THEN                                 *
! *                          -- Tsys [K] vs Freq [MHz] at given time                   *
! *                 IF ( -ord == phas ) THEN                                           *
! *                     IF ( -var == time ) THEN                                       *
! *                          -- Phase [rad] vs run Time [Hrs] at IF                    *
! *                          -- Phase_ave [rad] vs run Time [Hrs]                      *
! *                     ELSEIF ( -var == spectr ) THEN                                 *
! *                          -- Phase [rad] vs Freq [GHz] at given time                *
! *                          -- Phase_ave [rad] vs Freq [GHz]                          *
! *                 IF ( -ord == ampl ) THEN                                           *
! *                     IF ( -var == time ) THEN                                       *
! *                          -- Amplitude [] vs run Time [Hrs] at IF                   *
! *                          -- Amplitude_ave [] vs run Time [Hrs]                     *
! *                     ELSEIF ( -var == spectr ) THEN                                 *
! *                          -- Amplitude [] vs Freq [GHz] at given time               *
! *                          -- Amplitude ave [] vs Freq [GHz] at given time           *
! *                 IF ( -ord == fmgt ) THEN                                           *
! *                     IF ( -var == time ) THEN                                       *
! *                          -- FMTGPS [10^-6 s] vs run Time [Hrs] at Timer Tag        *
! *                          -- FMTGPS_ave [10^-6 s] vs run Time [Hrs]                 *
! *                 IF ( -ord == fmpt ) THEN                                           *
! *                     IF ( -var == time ) THEN                                       *
! *                          -- FMTPPS [10^-6 s] vs run Time [Hrs] at Timer Tag        *
! *                          -- FMTPPS_ave [10^-6 s] vs run Time [Hrs]                 *     
! *   13th Argument: flag for scan difference. Supports:                               *
! *       -del --                                                                      *
! *   14th Argument: Maximum difference between scan        { real*8 } [s]             *
! *   15th Argument: flag for plot to make. Supports                                   *
! *       -plot --                                                                     *
! *   16th Argument: Accepted labels for the plots:                                    *
! *       raw -- raw values with the outliers removed       { char }                   *
! *       ave -- average scan values                        { char }                   *
! *   17th Argument: output file flag. Supports:                                       *
! *       -o -- output file                                 { char }                   *
! *   18th Argument: Path to output anc file                                           *
! *                                                                                    *
! *  ### 25-AUG-2021     BNC_PLOT            v6.0 (c)    N. Habana   28-SEP-2023 ###   *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'atp.i'
      INCLUDE    'diagi.i'
      TYPE ( ANC__TYP   ) :: ANC
      TYPE (DIAGI_STRU) :: DIA(2)
      CHARACTER  FIL_BNC*128, FILOUT*128, STR*128, PLOTVAR*6, PLOTORD*4
      CHARACTER  STR1(2)*32
      CHARACTER  STR_DO_DATE*64, TIT*64, CH_IND*12, CH_POL*2, TIT2*64
      CHARACTER  CH_FRQ*12, CH_BND, DIR_PLT*64, TITS(2)*64
      CHARACTER  CH_FLG1*10, CH_FLG2*10, CH_FLG3*10, CH_FLG4*10
      CHARACTER  CH_FLG5*10, CH_FLG6*10, CH_FLG7*10, CH_FLG8*10
      CHARACTER  CH_FLG9*10
      CHARACTER  CH_PLOT*4, CH_DATE(2)*22, SPEC_DATE(2)*23
      CHARACTER  CH_EXP*4
      INTEGER*4  MJD_RNG(2)
      INTEGER*4  LBNC
      REAL*8     UTC_RNG(2), TSINCE(2)
      INTEGER*4  IER, IUER, IDEV, PRINTMOD
      REAL*8     TIM_DIF_MAX
      CHARACTER  CH_TIM_DIF*6
      REAL*8     TIM_AVR(ANC__MEPC), TSYS_AVR(ANC__MEPC)
      REAL*8     TSYS_RMS(ANC__MEPC), AMP_SCA(ANC__MEPC)
      COMPLEX*8  PCAL_AVR(ANC__MEPC), PCAL_SCA(ANC__MEPC)
      COMPLEX*8  PCAL_FRQ_R8(ANC__MEPC)
      REAL*8     PCAL_AMP_RMS(ANC__MEPC), PCAL_PHA_RMS(ANC__MEPC)
      REAL*8     AMP_FRQ_E8(ANC__MEPC), PHA_FRQ_E8(ANC__MEPC)
      REAL*8     T1(ANC__MEPC), X1(ANC__MEPC), T2(ANC__MEPC)
      REAL*8     X2(ANC__MEPC), E2(ANC__MEPC), T11(ANC__MEPC)
      REAL*8     T12(ANC__MEPC), T22(ANC__MEPC)
      REAL*8     T3(ANC__MEPC), T33(ANC__MEPC)
      REAL*8     X3(ANC__MEPC), T4(ANC__MEPC), X4(ANC__MEPC)
      REAL*8     AZ_SCA(ANC__MEPC), EL_SCA(ANC__MEPC)
      REAL*8     TSYS_SCA(ANC__MEPC), TSYS_ARR(ANC__MEPC)
      REAL*8     FRQ(ANC__MEPC), F1(ANC__MEPC), FRQ_VEC(ANC__MEPC)
      REAL*8     TSYS_FRQ_R8(ANC__MEPC), TSYS_FRQ_E8(ANC__MEPC)
      COMPLEX*8  CX1(ANC__MEPC), CX2(ANC__MEPC)
      INTEGER*4  SEEK_SET, ARG_LEN, PREF
      LOGICAL*1  LEX, FL_APPLY_TATM, FL_PRINT
      CHARACTER  ID_ARR(ANC__MTPS)*4, MODE*6
      INTEGER*4  ME, NS, NF
      PARAMETER  ( ME = 16 ) 
      REAL*8     ELEV_TATM(ME), VAL_TATM(ME), SPL_TATM(ME), TATM
      INTEGER*4  IND_ARR(ANC__MEPC), IND_SCA(ANC__MEPC)
      REAL*8     VAL_FRQ, FRQ_ARR(ANC__MEPC), E1(ANC__MEPC)
      INTEGER*4  LUN, NBT, IND_FRQ, IND_TIM, IND_POL, LE, LOC_DEC
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8
      INTEGER*4  IP, KP, LP, NP, JP, JN, KB, K1, K2
      INTEGER*1  IN_POL, POL_ARR(ANC__MTPS)
      INTEGER*8  IS
      REAL*8     FMGT(ANC__MEPC), FMPT(ANC__MEPC)
      CHARACTER  FIL_STA_LST*128, STA_NAM*16, STA_ID*8
      REAL*8,    EXTERNAL :: FSPL8, MJDSEC_TO_TIM
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*8, EXTERNAL :: LSEEK, READ
      INTEGER*4, EXTERNAL :: IXMN8, IXMN4, LINDEX, I_LEN
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
! ---
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV_DEF
      INTEGER*4  ICL1, ICL2, ICL3
      CHARACTER  ZAG*128, UNIT*128, FNAM_PLT(2)*128
      REAL*8     Y_TEMP(ANC__MEPC), Y_RNG, X_RNG
      INTEGER*4  JJ11
      INTEGER*4  IPRN
!      
! --- Pre-define variables
!
      IND_FRQ        = 0         ! Frequency index
      FL_APPLY_TATM  = .FALSE.   ! Should we use the TATM azimuths?
      PLOTORD        = 'tsys'
      PLOTVAR        = 'time'    ! or 'elev'  or 'spectr' or 'azim' or 'azel'
      TIM_DIF_MAX    = 9.5D0 
      PRINTMOD       = 1
      DIR_PLT        = TRIM(ANC__PLOTS_DIR)//'/'
!     
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~ 'Usage: bnc_plot -exp vlbi|sde -i bnc_file \
!                    -frq_ind|-frq_val|-tim_ind|-tim_rng index|freq|index|time_range \
!                    -pol polarization -ord plot_ordinate -var plot_variable -del tim_dif_max -plot raw|ave \
!                    -o output'
!~~ examples
! bnc_plot -exp sde -i gsst04_orig.bnc -frq_ind 999 -pol h -ord tsys -var time -del 8.0 -plot ave -o gsst04.dat
! bnc_plot -exp sde -i gsst04_orig.bnc -tim_rng 2022.12.25-18:56:45.00 2022.12.31-02:02:30.00 -pol h \
!          -ord tsys -var spectr -del 8.0 -plot ave -o gsst04.anc
! bnc_plot -exp sde -i gsst04_orig.bnc -tim_ind 999 -pol v -ord phas -var spectr -del 4.0 -plot raw -o x.x
! bnc_plot -exp vlbi -i vo3250_orig.bnc -tim_ind 999 -pol v -ord pps -var time -del 4.0 -plot tot -o x.x
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
! --- Read user input
!
      IF ( IARGC() < 1 .OR. IARGC() > 19 ) THEN
         WRITE ( 6, '(A)' ) 'USAGE:  '//                                &
     &         'bnc_plot -exp  vlbi|sde '//                             &
     &                  '-i    bnc_file '//                             &
     &                  '-frq_ind|-frq_val|-tim_ind|-tim_rng '//        &
     &                               'index|freq|index|time_range '//   &
     &                  '-pol  polarization '//                         &
     &                  '-ord  plot_ordinate '//                        &
     &                  '-var  plot_variable '//                        &
     &                  '-del  tim_dif_max '//                          &
     &                  '-plot raw|ave|tot '//                          &
     &                  '-o    plot_file'
         CALL EXIT ( 0 )
      ELSE
! ------
         write (6,'(A)') 'running BNC_PLOT'
!
! ------ Arg. 1
! ------ Input flag 1
!
         CALL CLRCH  ( CH_FLG1 )
         CALL GETARG ( 1, CH_FLG1 )
         IF ( CH_FLG1 .NE. '-exp' ) THEN !
            WRITE ( 6, '(A)' ) 'First flag should be -exp not '//       &
     &	                        TRIM(CH_FLG1)
	    CALL EXIT (1)
         END IF ! ch_flg1
!
! ------ Arg. 2
! ------ Get the experiment type
!
         CALL CLRCH ( STR )
         CALL CLRCH ( CH_EXP )
         CALL GETARG ( 2, STR )
         CALL TRAN ( 11, STR, CH_EXP )
         IF ( CH_EXP == 'SDE' .OR. CH_EXP == 'VLBI' ) THEN !
            CONTINUE
         ELSE
            IUER = -1
            CALL ERR_LOG ( 5045, IUER, 'BNC_PLOT',                      &
     &              'Unsupported experiment input '//CH_EXP//           &
     &              ' expected: SDE, or VLBI.' )
            CALL EXIT ( 1 )
         END IF ! ch_flg1
!     
! ------ Arg. 3
! ------ Input flag 2
!
         CALL CLRCH  ( CH_FLG2 )
         CALL GETARG ( 3, CH_FLG2 )
         IF ( CH_FLG2 .NE. '-i' ) THEN !
            WRITE ( 6, '(A)' ) 'First flag should be -i not '//         &
     &	                        TRIM(CH_FLG2)
	    CALL EXIT (1)
         END IF ! ch_flg2
!
! ------ Arg. 4
! ------ Get the file and check if it exists?
!
         CALL GETARG ( 4, FIL_BNC )
         INQUIRE     ( FILE=FIL_BNC, EXIST=LEX )
         IF ( .NOT. LEX ) THEN ! 
            IUER = -1
            CALL ERR_LOG ( 5001, IUER, 'BNC_PLOT',                      &
     &              'Cannot find file '//TRIM(FIL_BNC) )
            CALL EXIT ( 1 )
         END IF ! lex
!
! ------ Arg. 5
! ------ Input flag 3
!
         CALL CLRCH  ( CH_FLG3 )
	 CALL GETARG ( 5, CH_FLG3 )
         IF ( CH_FLG3 == '-frq_ind' .OR.                                &
     &        CH_FLG3 == '-frq_val' .OR.                                &
     &        CH_FLG3 == '-tim_ind' .OR.                                &
     &        CH_FLG3 == '-tim_rng'       ) THEN ! 
! ---------
	    CONTINUE
! ---------
            IF ( CH_FLG3 == '-tim_rng' ) THEN ! 
               IF ( IARGC() .NE. 19 ) THEN !
                  IUER = -1
                  CALL ERR_LOG ( 5002, IUER, 'BNC_PLOT',                &
     &                    'For flag '//CH_FLG3//' we expect 19 inputs ')
                  CALL EXIT ( 1 )
               END IF ! iargc =/= 19
            END IF ! ch_flg3 == -tim_rng
         ELSE
            IUER = -1
            CALL ERR_LOG ( 5003, IUER, 'BNC_PLOT',                      &
     &              'Unsupported flag '//CH_FLG3//'expected: '//        &
     &              ' -frq_ind, -frq_val, -tim_ind, or -tim_rng' )
            CALL EXIT ( 1 )
         END IF ! ch_flg3
!
! ------ List of arguments if we are not doing the time range
!
         IF ( CH_FLG3 .NE. '-tim_rng' ) THEN !
!
! --------- Arg. 6
! --------- Get either the frequency index, frequency value,  or time index
!
            CALL CLRCH  ( CH_IND )
            CALL GETARG ( 6, CH_IND )
!
! --------- Check if this is an integer or real number
! --------- is there a decimal point on the input ==> -frq_val
!
            LOC_DEC = LINDEX( CH_IND, ".")
            IF ( LOC_DEC == 0 ) THEN !
               CALL CHIN ( CH_IND, IND_FRQ )
            ELSE
               IND_FRQ = ATP__INIT
               READ ( UNIT=CH_IND, FMT='(F12.2)', IOSTAT=IER ) VAL_FRQ
               IF ( IER .NE. 0 ) THEN !
                  IUER = -1
                  CALL ERR_LOG ( 5004, IUER, 'BNC_PLOT',           &
     &                    'Expected Real value for '//TRIM(CH_IND) )
               END IF ! ier =/= 0
            END IF ! loc_dec = 0
!
! --------- Arg. 7
! --------- Get the polarization flag
!
            CALL CLRCH  ( CH_FLG4 )
  	    CALL GETARG ( 7, CH_FLG4 )
	    IF ( CH_FLG4 .NE. '-pol' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5005, IUER, 'BNC_PLOT',                 &
     &                 'Unsupported flag '//CH_FLG4//' expected -pol ' )
	       CALL EXIT ( 1 )
            END IF ! ch_flg4
!
! --------- Arg. 8
! --------- Get the polarization value
!
            CALL CLRCH ( STR )
            CALL CLRCH ( CH_POL )
  	    CALL GETARG ( 8, STR )
            CALL TRAN ( 11, STR, CH_POL )
            IF ( CH_POL == 'R' ) THEN !
               IN_POL = ANC__R_POL
            ELSEIF ( CH_POL == 'L' ) THEN
               IN_POL = ANC__L_POL
            ELSEIF ( CH_POL == 'H' ) THEN
               IN_POL = ANC__H_POL
            ELSEIF ( CH_POL == 'V' ) THEN
               IN_POL = ANC__V_POL
            ELSEIF ( CH_POL == 'X' ) THEN
               IN_POL = ANC__X_POL
            ELSEIF ( CH_POL == 'Y' ) THEN
               IN_POL = ANC__Y_POL
            ELSE
               IUER = -1
               CALL ERR_LOG ( 5006, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported polarization input '//CH_POL//      &
     &                 ' expected: R, L, H, V, X, or Y.' )
	       CALL EXIT ( 1 )
            END IF ! ch_pol
!
! --------- Arg. 9
! --------- Get the ordinate flag
!
            CALL CLRCH  ( CH_FLG5 )
            CALL GETARG ( 9, CH_FLG5 )
	    IF ( CH_FLG5 .NE. '-ord' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5007, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported flag '//CH_FLG5//'expected -ord ' )
               CALL EXIT ( 1 )
            END IF ! ch_flg5
!
! --------- Arg. 10
! --------- Get the variable to plot
!
            CALL GETARG ( 10, PLOTORD )
            IF ( PLOTORD == 'tsys' .OR. PLOTORD == 'phas' .OR.          &
     &           PLOTORD == 'ampl' .OR. PLOTORD == 'fmgt' .OR.          &
     &           PLOTORD == 'fmpt'       ) THEN              !
               CONTINUE
            ELSE 
               IUER = -1
               CALL ERR_LOG ( 5008, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported mode '//PLOTORD//'. Expected '//    &
     &                 ' -- tsys, phas, ampl, fmgt, or fmpt' )
               CALL EXIT ( 1 )
            END IF ! plotord
!
! --------- Arg. 11
! --------- Get the variable flag
!
            CALL CLRCH  ( CH_FLG6 )
            CALL GETARG ( 11, CH_FLG6 )
            IF ( CH_FLG6 .NE. '-var' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5009, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported flag '//CH_FLG6//'expected -var ' )
               CALL EXIT ( 1 )
            END IF ! ch_flg6
!
! --------- Arg. 12
! --------- Get the plot type
!
            CALL CLRCH ( PLOTVAR )
            CALL GETARG ( 12, PLOTVAR )
!
! --------- Is this an acceptable plotting mode, for the plot type?
! 
            IF ( PLOTORD == 'tsys' ) THEN !
               IF ( PLOTVAR == 'time' .OR. PLOTVAR == 'elev' .OR.       &
     &              PLOTVAR == 'azim' .OR. PLOTVAR == 'azel'     ) THEN !
                  CONTINUE
! ------------
               ELSEIF ( PLOTVAR == 'spectr' ) THEN
!
! --------------- For spectrum use the time index
!
                  IF ( CH_FLG3 .NE. '-tim_ind' ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 5042, IUER, 'BNC_PLOT',             &
     &                       'Switch the flag "'//TRIM(CH_FLG3)//       &
     &                       '" to "-tim_ind" ' )
                  END IF
!
! --------------- Set the Time Index to Freq index if plotting
!                 Tsys vs spectrum
!
                  IF ( IND_FRQ .NE. ATP__INIT ) THEN !
                     IND_TIM = IND_FRQ
                  ELSE
                     CALL ERR_LOG ( 5010, IUER, 'BNC_PLOT',        &
     &                    'When plotting Tsys vs Time, an integer for ' &
     &                    //'time index is expected' )
                     CALL EXIT ( 1 )
                  END IF ! ind_frq
! ------------
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5011, IUER, 'BNC_PLOT',           &
     &                    'Tsys plot unsupported mode '//PLOTVAR//      &
     &                    ' -- time, elev, azim, azel, or '//           &
     &                    'spectr expected' )
                  CALL EXIT ( 1 )
               END IF ! plotvar (given plotord = tsys)
! ---------
            ELSEIF ( PLOTORD == 'phas' .OR. PLOTORD == 'ampl' ) THEN
               IF ( PLOTVAR == 'time' ) THEN !
                  CONTINUE
! ------------
               ELSEIF ( PLOTVAR == 'spectr'  ) THEN
!
! --------------- For spectrum use the time index
!
                  IF ( CH_FLG3 .NE. '-tim_ind' ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 5043, IUER, 'BNC_PLOT',             &
     &                       'Switch the flag "'//TRIM(CH_FLG3)//       &
     &                       '" to "-tim_ind" ' )
                  END IF
!     
! --------------- Set the Time Index to Freq index if plotting
!		  Pcal vs spectrum
!
                  IF ( IND_FRQ .NE. ATP__INIT ) THEN !
                     IND_TIM = IND_FRQ
                  ELSE
                     CALL ERR_LOG ( 5012, IUER, 'BNC_PLOT',             &
     &                    'When plotting Pcal vs Spectrum, an integer'  &
     &                    //' for time index is expected' )
                     CALL EXIT ( 1 )
                  END IF ! ind_frq
! ------------
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5013, IUER, 'BNC_PLOT',                &
     &                    'phas and ampl plot unsupported mode '//      &
     &                    PLOTVAR//' -- time, or spectr expected' )
                  CALL EXIT ( 1 )
               END IF ! plotvar (given plotord = phas | ampl)
! ---------
            ELSEIF ( PLOTORD == 'fmgt' .OR. PLOTORD == 'fmpt' ) THEN
               IF ( PLOTVAR == 'time' ) THEN !
                  CONTINUE
! ------------
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5042, IUER, 'BNC_PLOT',                &
     &                    'fmgt and fmpt plot unsupported mode '//      &
     &                     PLOTVAR//' -- time expected' )
                  CALL EXIT ( 1 )
               END IF ! plotvar (given plotord = fmgt | fmpt)
            END IF              ! plotord 
!
! --------- Arg. 13
! --------- Get the scan difference flag
!
            CALL CLRCH  ( CH_FLG7 )
  	    CALL GETARG ( 13, CH_FLG7 )
	    IF ( CH_FLG7 .NE. '-del' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5014, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported flag '//CH_FLG7//'expected -del ' )
	       CALL EXIT ( 1 )
            END IF ! ch_flg7
!
! --------- Arg. 14
! --------- Get the scan definition
!
            CALL CLRCH  ( CH_TIM_DIF )
            CALL GETARG ( 14, CH_TIM_DIF )
!
! --------- Check if the number has a decimal point?
!     
            READ (UNIT=CH_TIM_DIF, FMT='(F6.2)', IOSTAT=IER) TIM_DIF_MAX
            IF ( IER .NE. 0 ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5015, IUER, 'BNC_PLOT',                   &
     &                 'Expected real number for scan diff. '//         &
     &                 CH_TIM_DIF )
            END IF ! ier
!
! --------- Arg. 15
! --------- Get the plotting flag
!
            CALL CLRCH  ( CH_FLG8 )
  	    CALL GETARG ( 15, CH_FLG8 )
	    IF ( CH_FLG8 .NE. '-plot' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5016, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported flag '//CH_FLG8//'expected -plot ' )
            END IF ! ch_flg8
!
! --------- Arg. 16
! --------- The plots to generate and write to file
!
            CALL CLRCH ( CH_PLOT )
            CALL GETARG ( 16, CH_PLOT )
            IF ( CH_PLOT == 'raw' .OR. CH_PLOT == 'ave' .OR.            &
     &           CH_PLOT == 'tot' ) THEN !
               CONTINUE
!@@!               IF ( PLOTVAR == 'elev' .AND. CH_PLOT == 'ave' ) THEN
!@@!                  CH_PLOT = 'nrm'
!@@!               END IF
            ELSE
               CALL ERR_LOG ( 5017, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported plot '//CH_PLOT//' expected: '//    &
     &                 'raw, ave, or tot' )
            END IF ! ch_plot
!
! --------- Arg. 17
! --------- Output flag
!
            CALL CLRCH  ( CH_FLG9 )
            CALL GETARG ( 17, CH_FLG9 )
            IF ( CH_FLG9 .NE. '-o' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5018, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported flag '//CH_FLG9//'expected -o ' )
               CALL EXIT (1)
            END IF ! ch_flg9
!
! --------- Arg. 18
! --------- Output file
!
	    CALL CLRCH ( FILOUT )
   	    CALL GETARG ( 18, FILOUT )            
!
! ------ If the second flag is the time range, i.e., "-tim_rng"
!
         ELSE
!
! --------- Arg. 6 & Arg. 7
! --------- Get the initial date
!
            CALL CLRCH  ( CH_DATE(1) )
            CALL CLRCH  ( CH_DATE(2) )
            CALL GETARG ( 6, CH_DATE(1) )
            CALL GETARG ( 7, CH_DATE(2) )
!
! --------- Convert date to MJD and UTC
!
            IUER = -1
            CALL DATE_TO_TIME (CH_DATE(1), MJD_RNG(1), UTC_RNG(1), IUER)
            IF ( IUER .NE. 0 ) THEN !
               CALL ERR_LOG ( 5019, IUER, 'BNC_PLOT',              &
     &                 'Failure converting start date:'//CH_DATE(1)//   &
     &                 ' to MJD and UTC seconds.' )
               CALL EXIT (1)
            END IF ! iuer
! ---------
            IUER = -1
            CALL DATE_TO_TIME (CH_DATE(2), MJD_RNG(2), UTC_RNG(2), IUER)
            IF ( IUER .NE. 0 ) THEN !
               CALL ERR_LOG ( 5020, IUER, 'BNC_PLOT',              &
     &                 'Failure converting end date:'//CH_DATE(2)//     &
     &                 ' to MJD and UTC seconds.' )
               CALL EXIT (1)
            END IF ! iuer
!
! --------- Arg. 8
! --------- Get the polarization flag
!
            CALL CLRCH  ( CH_FLG4 )
  	    CALL GETARG ( 8, CH_FLG4 )
	    IF ( CH_FLG4 .NE. '-pol' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5021, IUER, 'BNC_PLOT',                 &
     &                 'Unsupported flag '//CH_FLG4//' expected -pol ' )
	       CALL EXIT ( 1 )
            END IF ! ch_flg4
!
! --------- Arg. 9
! --------- Get the polarization value
!
            CALL CLRCH ( STR )
            CALL CLRCH ( CH_POL )
  	    CALL GETARG ( 9, STR )
            CALL TRAN ( 11, STR, CH_POL )
            IF ( CH_POL == 'R' ) THEN !
               IN_POL = ANC__R_POL
            ELSEIF ( CH_POL == 'L' ) THEN
               IN_POL = ANC__L_POL
            ELSEIF ( CH_POL == 'H' ) THEN
               IN_POL = ANC__H_POL
            ELSEIF ( CH_POL == 'V' ) THEN
               IN_POL = ANC__V_POL
            ELSEIF ( CH_POL == 'X' ) THEN
               IN_POL = ANC__X_POL
            ELSEIF ( CH_POL == 'Y' ) THEN
               IN_POL = ANC__Y_POL
            ELSE
               IUER = -1
               CALL ERR_LOG ( 5022, IUER, 'BNC_PLOT',              &
     &                 'Unsupported polarization input '//CH_POL//      &
     &                 ' expected: R, L, H, V, X, or Y.' )
	       CALL EXIT ( 1 )
            END IF ! ch_pol
!
! --------- Arg. 10
! --------- Get the ordinate flag
!
            CALL CLRCH  ( CH_FLG5 )
            CALL GETARG ( 10, CH_FLG5 )
	    IF ( CH_FLG5 .NE. '-ord' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5023, IUER, 'BNC_PLOT',              &
     &                 'Unsupported flag '//CH_FLG5//'expected -ord ' )
               CALL EXIT ( 1 )
            END IF ! ch_flg5
!
! --------- Arg. 11
! --------- Get the variable to plot
!
            CALL GETARG ( 11, PLOTORD )
            IF ( PLOTORD == 'tsys' .OR. PLOTORD == 'phas' .OR.          &
     &           PLOTORD == 'ampl' .OR. PLOTORD == 'fmgt' .OR.          &
     &           PLOTORD == 'fmpt'       ) THEN              !
               CONTINUE
            ELSE
               IUER = -1
               CALL ERR_LOG ( 5024, IUER, 'BNC_PLOT',              &
     &                 'Unsupported mode '//PLOTORD//'. Expected '//    &
     &                 ' -- tsys, phas, ampl, fmgt, or fmpt' )
               CALL EXIT ( 1 )
            END IF ! plotord
!
! --------- Arg. 12
! --------- Get the variable flag
!
            CALL CLRCH  ( CH_FLG6 )
            CALL GETARG ( 12, CH_FLG6 )
            IF ( CH_FLG6 .NE. '-var' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5025, IUER, 'BNC_PLOT',                   &
     &                 'Unsupported flag '//CH_FLG6//'expected -var ' )
               CALL EXIT ( 1 )
            END IF ! ch_flg5
!
! --------- Arg. 13
! --------- Get the plot type
!
            CALL CLRCH ( PLOTVAR )
            CALL GETARG ( 13, PLOTVAR )
!
! --------- Is this an acceptable plotting mode, for the plot type?
! 
            IF ( PLOTORD == 'tsys' ) THEN !
               IF ( PLOTVAR == 'time' .OR. PLOTVAR == 'elev' .OR.       &
     &              PLOTVAR == 'azim' .OR. PLOTVAR == 'azel'     ) THEN !
                  CONTINUE
! ------------
               ELSEIF ( PLOTVAR == 'spectr' ) THEN
!
! --------------- Set the Time Index to Freq index if plotting
!                 Tsys vs spectrum
!
                  IF ( IND_FRQ .NE. ATP__INIT ) THEN !
                     IND_TIM = IND_FRQ
                  ELSE
                     CALL ERR_LOG ( 5026, IUER, 'BNC_PLOT',             &
     &                    'When plotting Tsys vs Time, an integer for ' &
     &                    //'time index is expected' )
                     CALL EXIT ( 1 )
                  END IF ! ind_frq
! ------------ 
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5027, IUER, 'BNC_PLOT',                &
     &                    'Tsys plot unsupported mode '//PLOTVAR//      &
     &                    ' -- time, elev, azim, azel, or '//           &
     &                    'spectr expected' )
                  CALL EXIT ( 1 )
               END IF ! plotvar ( given plotord is tsys)
            ELSEIF ( PLOTORD == 'phas' .OR. PLOTORD == 'ampl' ) THEN
               IF ( PLOTVAR == 'time' ) THEN !
                  CONTINUE
! ------------
               ELSEIF ( PLOTVAR == 'spectr'  ) THEN
!
! --------------- Set the Time Index to Freq index if plotting
!		  Pcal vs spectrum
!
                  IF ( IND_FRQ .NE. ATP__INIT ) THEN !
                     IND_TIM = IND_FRQ
                  ELSE
                     CALL ERR_LOG ( 5028, IUER, 'BNC_PLOT',        &
     &                    'When plotting Pcal vs Time, an integer '//   &
     &                    'for time index is expected' )
                     CALL EXIT ( 1 )
                  END IF ! ind_frq
! ------------
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5029, IUER, 'BNC_PLOT',           &
     &                    'phase and ampl plot unsupported mode '//     &
     &                 PLOTVAR//' -- time, or spectr expected')
                  CALL EXIT ( 1 )
               END IF ! plotvar ( given plotord is phas | ampl)
! ---------
            ELSEIF ( PLOTORD == 'fmgt' .OR. PLOTORD == 'fmpt' ) THEN
               IF ( PLOTVAR == 'time' ) THEN !
                  CONTINUE
! ------------
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5043, IUER, 'BNC_PLOT',                &
     &                    'fmgt and fmpt plot unsupported mode '//      &
     &                     PLOTVAR//' -- time expected' )
                  CALL EXIT ( 1 )
               END IF ! plotvar (given plotord = fmgt | fmpt)
            END IF ! plotord
!
! --------- Arg. 14
! --------- Get the scan difference flag
!
            CALL CLRCH  ( CH_FLG7 )
  	    CALL GETARG ( 14, CH_FLG7 )
            IF ( CH_FLG7 .NE. '-del' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5030, IUER, 'BNC_PLOT',              &
     &                 'Unsupported flag '//CH_FLG7//'expected -del ' )
	       CALL EXIT ( 1 )
            END IF ! ch_flg6
!
! --------- Arg. 15
! --------- Get the scan definition
!
            CALL CLRCH  ( CH_TIM_DIF )
            CALL GETARG ( 15, CH_TIM_DIF )
!
! --------- Check if the number has a decimal point?
!     
            READ (UNIT=CH_TIM_DIF, FMT='(F6.2)', IOSTAT=IER) TIM_DIF_MAX
            IF ( IER .NE. 0 ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5031, IUER, 'BNC_PLOT',              &
     &                 'Expected real number for scan diff. '//         &
     &                 CH_TIM_DIF )
            END IF ! ier
!
! --------- Arg. 16
! --------- Get the plotting flag
!
            CALL CLRCH  ( CH_FLG8 )
  	    CALL GETARG ( 16, CH_FLG8 )
	    IF ( CH_FLG8 .NE. '-plot' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5032, IUER, 'BNC_PLOT',              &
     &                 'Unsupported flag '//CH_FLG8//'expected -plot ' )
            END IF ! ch_flg8
!
! --------- Arg. 17
! --------- The plots to generate and write to file
!
            CALL CLRCH ( CH_PLOT )
            CALL GETARG ( 17, CH_PLOT )
            IF ( CH_PLOT == 'raw' .OR. CH_PLOT == 'ave' .OR.            &
     &           CH_PLOT == 'tot' ) THEN
               CONTINUE
!@@!               IF ( PLOTVAR == 'elev' .AND. CH_PLOT == 'ave' ) THEN
!@@!                  CH_PLOT = 'nrm'
!@@!               END IF
            ELSE
               CALL ERR_LOG ( 5033, IUER, 'BNC_PLOT',              &
     &                 'Unsupported plot '//CH_PLOT//' expected: '//    &
     &                 'raw, ave, or tot' )
            END IF ! ch_plot
!
! --------- Arg. 18
! --------- Output flag
!
            CALL CLRCH  ( CH_FLG9 )
            CALL GETARG ( 18, CH_FLG9 )
            IF ( CH_FLG9 .NE. '-o' ) THEN
               IUER = -1
               CALL ERR_LOG ( 5034, IUER, 'BNC_PLOT',              &
     &                 'Unsupported flag '//CH_FLG9//'expected -o ' )
               CALL EXIT (1)
            END IF ! ch_flg9
!
! --------- Arg. 19
! --------- Output file
!
	    CALL CLRCH ( FILOUT )
   	    CALL GETARG ( 19, FILOUT )            
	 END IF ! ch_flg3 (using vs not using -tim_rng)
      END IF
!     
!##############################################################
!#######                                                #######
!#######  EVENTUALLY INCLUDE THIS IN CONFIG FILE        #######
!#######                                                #######
!##############################################################
! --- Declare the a priori file with station names 
!
      FIL_STA_LST = '/apr/sta/vlbi_station.names'
!     
! --- Get the Spline coefficients of the ???effect temperature of the 
!     atmosphere???
!
      CALL GET_SPL_TATM ( ME, LE, ELEV_TATM, VAL_TATM, SPL_TATM )
!
! --- Parse data from binary file to ANC type
!
      IUER = -1
      CALL BNC_PARSE ( FIL_BNC, ANC, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -2
         CALL ERR_LOG ( 5035, IUER, 'BNC_PLOT',                         &
     &           'Failure in parsing binary file data from '//FIL_BNC )
         CALL EXIT ( 1 )
      END IF
!     
! --- Check if the experiment type was declared accurately
!
      LBNC = I_LEN ( FIL_BNC ) ! Length of the binary file name
! ---
      IF ( CH_EXP == 'SDE' ) THEN 
         STR1(1) = FIL_BNC(LBNC-14:LBNC-9)        ! Exp name from sde bnc file path
!
! ------ Does the experiment name from the file name match the one in
!        the derived type?
!
         IF ( TRIM(STR1(1)) .NE. TRIM(ANC%EXP_CODE) ) THEN
            IUER = -1
            CALL ERR_LOG ( 5046, IUER, 'BNC_PLOT',                      &
     &              'Check if this is really an SDE. We expected the'// &
     &              ' file name to be for exp '//TRIM(ANC%EXP_CODE)//   &
     &              ' not for exp '//TRIM(STR1(1) ) )
            CALL EXIT ( 1 )
         END IF
      ELSE
        STR1(1) = FIL_BNC ( LBNC-16:LBNC-9 )        ! Exp name from vlbi bnc file path (including station)
        STR1(2) = STR1(1)( 1:I_LEN(STR1(1))-2 )        ! Exp name from vlbi bnc file path
!
! ------ Does the experiment name from the file name match the one in
!        the derived type?
!
         IF ( TRIM(STR1(2)) .NE. TRIM(ANC%EXP_CODE) ) THEN
            IUER = -1
            CALL ERR_LOG ( 5047, IUER, 'BNC_PLOT',                      &
     &             'Check if this is really a VLBI exp. We expected '// &
     &             'the file name to be for exp '//TRIM(ANC%EXP_CODE)// &
     &             ' not for exp '//TRIM(STR1(2) ) )
            CALL EXIT ( 1 )
         END IF
      END IF 
!
! --- Get the station ID
!
      IUER = -1
      STA_NAM = ANC%STA_NAM
      CALL GET_STA_ID ( FIL_STA_LST, STA_NAM, STA_ID, IUER )
      CALL TRAN ( 12, STA_ID, STA_ID )                          ! Sta_id in lower case
!     
! --- Setting defaults values of the plotting parameters
!
      IUER = -1
      CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV_DEF, ZAG,     &
     &                 UNIT, ICL1, ICL2, ICL3, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5036, IUER, 'BNC_PLOT',                       &
     &             'Error setting default values for the plot' )
           RETURN
      END IF
!
! --- Get the frequency index closest to the defined frequency
! --- N.B: Make sure it matches the polarization, this applies only to
!          cases where var =/= spectr
!
      IF ( PLOTVAR .NE. 'spectr' ) THEN 
         IF ( IND_FRQ == ATP__INIT ) THEN
!
! --------- If dealing with Tsys
!
            FRQ_ARR = 0.D0
            POL_ARR = INT(0,1)
            IF ( PLOTORD == 'tsys' ) THEN
               DO 310 J1 = 1, ANC%NUM_TPS
                  FRQ_ARR(J1) = ANC%TPS(J1)%SKY_FRQ
                  POL_ARR(J1) = ANC%TPS(J1)%POL !@#@!
 310           CONTINUE
! ------------
               CALL TSYS_FRQ_POL_IDX(ANC, VAL_FRQ, IN_POL,IND_FRQ, IUER)
!
! --------- If dealing with Pcal
!
            ELSE
               DO 311 J1 = 1, ANC%NUM_PCS
                  FRQ_ARR(J1) = ANC%PCS(J1)%SKY_FRQ
                  POL_ARR(J1) = ANC%PCS(J1)%POL !@#@!
 311           CONTINUE

               CALL PCAL_FRQ_POL_IDX(ANC, VAL_FRQ, IN_POL,IND_FRQ, IUER)

            END IF
!
! ------ In the event where an index is given
!
         ELSE
!
! --------- If dealing with Tsys
!
            IF ( PLOTORD == 'tsys' ) THEN
               IF ( ANC%TPS(IND_FRQ)%POL .NE. IN_POL ) THEN
                  WRITE ( 6, '(A)' ) 'WARNING: Input polarization '//   &
     &                               CH_POL//' does not correspond to ' &  
     &                               //'the indexed '//                 &
     &                               ANC__POL(ANC%TPS(IND_FRQ)%POL)//   &
     &                               '. We have corrected this.'
                  CH_POL = ANC__POL(ANC%TPS(IND_FRQ)%POL)
               END IF
!
! --------- If dealing with Tsys
!
            ELSEIF ( PLOTORD == 'phas' .OR. PLOTORD == 'ampl' ) THEN
               IF ( ANC%PCS(IND_FRQ)%POL .NE. IN_POL ) THEN
                  WRITE ( 6, '(A)' ) 'WARNING: Input polarization '//   &
     &                               CH_POL//' does not correspond to ' &  
     &                               //'the indexed '//                 &
     &                               ANC__POL(ANC%PCS(IND_FRQ)%POL)//   &
     &                               '. We have corrected this.'
                  CH_POL = ANC__POL(ANC%PCS(IND_FRQ)%POL)
               END IF
            END IF
         END IF
      END IF
! ---
      IUER = 0
      STR_DO_DATE = MJDSEC_TO_DATE ( ANC%MJD_DOO, ANC%TAI_DOO +         &
     &                               ANC%UTC_MTAI, IUER )
! ---
      WRITE ( 6, * ) '_________________________________________________'
      WRITE ( 6 ,* ) 'Exp_Code: ', ANC%EXP_CODE
      WRITE ( 6 ,* ) 'Sta_Name: ', ANC%STA_NAM, '\t', STA_ID
      WRITE ( 6 ,* ) 'Obs_Date: ', STR_DO_DATE
      WRITE ( 6 ,* ) 'Num_tps:  ', ANC%NUM_TPS,  'Num_Tsys: ', ANC%NUM_TSYS
      WRITE ( 6 ,* ) 'Num_pcs:  ', ANC%NUM_PCS,  'Num_Pcal: ', ANC%NUM_PCAL
      WRITE ( 6 ,* ) 'Num_tgps: ', ANC%NUM_TGPS, 'Num_GPS:  ', ANC%NUM_GPS
      WRITE ( 6, * ) '_________________________________________________'
!
! --- Convert index to character
!
      CALL CLRCH ( CH_IND)
      CALL INCH ( IND_FRQ, CH_IND )
!
! --- Pre-fill the frequency and bands
!
      CH_FRQ = ANC__FILLER_CH
      CH_BND = ANC__FILLER_CH
! ---
      IF ( PLOTORD == 'tsys' ) THEN
!
! ------ Convert sky frequency to string, shift to the left incase
!        of inital blanks.
!
         WRITE ( CH_FRQ, 111 ) ANC%TPS(IND_FRQ)%SKY_FRQ
         CH_FRQ = CH_FRQ(1:I_LEN(CH_FRQ)-2)
         CALL CHASHL( CH_FRQ )
!
! ------ Get the frequency channel band (Hardware)
!
         CH_BND = ANC%TPS(IND_FRQ)%ID(3:3)
!
! ------ Write the Plot title to "TIT"
!
         WRITE ( TIT, 110 ) STR_DO_DATE, PLOTORD,                       &
     &                      ANC%TPS(IND_FRQ)%SKY_FRQ,                   &
     &                      ANC__POL(ANC%TPS(IND_FRQ)%POL),             &
     &                      ANC%TPS(IND_FRQ)%ID
! ---
      ELSEIF ( PLOTORD == 'phas' .OR. PLOTORD == 'ampl' ) THEN
!     
! ------ Convert sky frequency to string, shift to the left incase
!        of inital blanks.
!
         WRITE ( CH_FRQ, 111 ) ANC%PCS(IND_FRQ)%SKY_FRQ
         CH_FRQ = CH_FRQ(1:I_LEN(CH_FRQ)-2)
         CALL CHASHL( CH_FRQ )
!
! ------ Get the frequency channel band (Hardware)
!
         CH_BND = ANC%PCS(IND_FRQ)%ID(2:2)
!
! ------ Write the Plot title to "TIT"
!
         WRITE ( TIT, 110 ) STR_DO_DATE, PLOTORD,                       &
     &                      ANC%PCS(IND_FRQ)%SKY_FRQ,                   &
     &                      ANC__POL(ANC%PCS(IND_FRQ)%POL),             &
     &                      ANC%PCS(IND_FRQ)%ID

      ENDIF
      WRITE ( 6, '(A)' ) 'Title: '//TRIM(TIT)
!
! --- Generate plot file names for both raw and ave data
!     if SDE:
!     e.g. mg0024_tsys_time_198_7416Va_ave.gif =
!                            {exp_name}_{ord}_{var}_{ind}_{freq}{pol}{band}_{raw|ave}.gif
!     if VLBI:
!     e.g. vo3250mg_tsys_time_61_5704Hb_ave.gif =
!                            {exp_name}{stn_id}_{ord}_{var}_{ind}_{freq}{pol}{band}_{raw|ave}.gif
!
      IF ( PLOTVAR == 'time' .OR. PLOTVAR == 'elev' .OR.                &
     &     PLOTVAR == 'azim' .OR. PLOTVAR == 'azel'      ) THEN
!
         IF ( CH_EXP == 'SDE' ) THEN
            FNAM_PLT(1) = TRIM(DIR_PLT)//                               &
     &                    TRIM(ANC%EXP_CODE)//'_'//                     &
     &                    TRIM(PLOTORD)//'_'//                          &
     &                    TRIM(PLOTVAR)//'_'//                          &
     &                    TRIM(CH_IND)//'_'//                           &
     &                    TRIM(CH_FRQ)//CH_POL(1:1)//CH_BND//'_raw.gif'
            FNAM_PLT(2) = TRIM(DIR_PLT)//                               &
     &                    TRIM(ANC%EXP_CODE)//'_'//                     &
     &                    TRIM(PLOTORD)//'_'//                          &
     &                    TRIM(PLOTVAR)//'_'//                          &
     &                    TRIM(CH_IND)//'_'//                           &
     &                    TRIM(CH_FRQ)//CH_POL(1:1)//CH_BND//'_ave.gif'
         ELSE
            FNAM_PLT(1) = TRIM(DIR_PLT)//                               &
     &                    TRIM(ANC%EXP_CODE)//                          &
     &                    TRIM(STA_ID)//'_'//                           &
     &                    TRIM(PLOTORD)//'_'//                          &
     &                    TRIM(PLOTVAR)//'_'//                          &
     &                    TRIM(CH_IND)//'_'//                           &
     &                    TRIM(CH_FRQ)//CH_POL(1:1)//CH_BND//'_raw.gif'
            FNAM_PLT(2) = TRIM(DIR_PLT)//                               &
     &                    TRIM(ANC%EXP_CODE)//                          &
     &                    TRIM(STA_ID)//'_'//                           &
     &                    TRIM(PLOTORD)//'_'//                          &
     &                    TRIM(PLOTVAR)//'_'//                          &
     &                    TRIM(CH_IND)//'_'//                           &
     &                    TRIM(CH_FRQ)//CH_POL(1:1)//CH_BND//'_ave.gif'
         END IF
!     
! --- For the spectrum plots
! e.g. mg0024_tsys_spectr_tim_ind_198_V_raw.gif = {exp_name}_{ord}_{var}_tim_ind_{tim_ind}_{pol}_{raw}.gif
! e.g. vo3250gs_phas_spectr_sca_ind_3_V_ave.gif = {exp_name}{sta_id}_{ord}_{var}_sca_ind_{sca_ind}_{pol}_{ave}.gif
!
      ELSE
         IF ( CH_PLOT == 'raw' ) THEN
! ---------
 201        CONTINUE
! ---------
            IF ( CH_EXP == 'SDE' ) THEN
               FNAM_PLT(1) = TRIM(DIR_PLT)//                            &
     &                       TRIM(ANC%EXP_CODE)//'_'//                  &
     &                       TRIM(PLOTORD)//'_'//                       &
     &                       TRIM(PLOTVAR)//'_tim_ind_'//               &
     &                       TRIM(CH_IND)//'_'//                        &
     &                       CH_POL(1:1)//'_raw.gif'
            ELSE
               FNAM_PLT(1) = TRIM(DIR_PLT)//                            &
     &                       TRIM(ANC%EXP_CODE)//                       &
     &                       TRIM(STA_ID)//'_'//                        &
     &                       TRIM(PLOTORD)//'_'//                       &
     &                       TRIM(PLOTVAR)//'_tim_ind_'//               &
     &                       TRIM(CH_IND)//'_'//                        &
     &                       CH_POL(1:1)//'_raw.gif'
            END IF
! ---------
            IF ( CH_PLOT == 'tot' ) GOTO 202
! ---------
         ELSEIF ( CH_PLOT == 'ave') THEN
! ---------
 202        CONTINUE
! ---------
            IF ( CH_EXP == 'SDE' ) THEN
               FNAM_PLT(2) = TRIM(DIR_PLT)//                            &
     &                       TRIM(ANC%EXP_CODE)//'_'//                  &
     &                       TRIM(PLOTORD)//'_'//                       &
     &                       TRIM(PLOTVAR)//'_sca_ind_'//               &
     &                       TRIM(CH_IND)//'_'//                        &
     &                       CH_POL(1:1)//'_ave.gif'
            ELSE
               FNAM_PLT(2) = TRIM(DIR_PLT)//                            &
     &                       TRIM(ANC%EXP_CODE)//                       &
     &                       TRIM(STA_ID)//'_'//                        &
     &                       TRIM(PLOTORD)//'_'//                       &
     &                       TRIM(PLOTVAR)//'_sca_ind_'//               &
     &                       TRIM(CH_IND)//'_'//                        &
     &                       CH_POL(1:1)//'_ave.gif'
            END IF
! ---------
            IF ( CH_PLOT == 'tot' ) GOTO 203
! ---------
         ELSE
            GOTO 201
         END IF
! ------
 203     CONTINUE
      END IF
!     
! --- PLOT TSYS
!
      IF ( PLOTORD == 'tsys' ) THEN 
!
! ------ Deal with Tsys vs Time or the look angles
!
         IF ( PLOTVAR == 'time' .OR. PLOTVAR == 'elev' .OR.             &
     &        PLOTVAR == 'azim' .OR. PLOTVAR == 'azel'      ) THEN
!
! --------- Get the raw Tsys data (filtered for outliers)
!
            IUER = -1
            CALL TSYS_TIME_FILTER_RAW ( ANC, IND_FRQ, NP, T1, X1, T11,  &
     &                                  T12, IUER )
!
! --------- Get the scan averages and RMS for Tsys (at the given
!           scan differences)
!           The averages are also filtered out for outliers.
!
            IUER = -1
            CALL TSYS_TIME_FILTER_SCAN ( ANC, IND_FRQ, TIM_DIF_MAX, NP, &
     &                                   T1, X1, NS,                    &
     &                                   TIM_AVR, TSYS_AVR, TSYS_RMS,   &
     &                                   IND_SCA, IND_ARR, IUER )
!     
! --------- Proceed only if there are actual points to use
!
            IF ( NP > 0 ) THEN 
!
! ------------ Convert time to hours from midnight UTC
!
               DO 410 J1 = 1, NP !LP
                  T1(J1) = (T1(J1) + ANC%TAI_TSYS + ANC%UTC_MTAI)/3.6D3 &
     &                     - 24.D0
 410           CONTINUE
!
! ------------ Filter out any zero values from the average values
!
               JP = 0
               DO 420 J2 = 1, NS
!
! --------------- Is the average Tsys of this scan above minimum Tsys
!
                  IF ( TSYS_AVR(J2) > ANC__TSYS_MIN .AND.                  &
     &                 TSYS_AVR(J2) < ANC__TSYS_MAX       ) THEN
!
! ------------------ Skip elevations that are below the minimum elevation
! ------------------ This is the Elevation where this scan ends
!
                     IF ( PLOTVAR == 'elev' ) THEN
                        IF ( T11(IND_SCA(J2)) < ANC__EL_MIN ) THEN
                           GOTO 420
                        END IF
                     END IF
! ------------------
                     JP = JP + 1                                           ! Update counter
! ------------------
                     AZ_SCA(JP)   = T12(IND_SCA(J2))/DEG__TO__RAD          ! Scan Azimuths     
                     EL_SCA(JP)   = T11(IND_SCA(J2))/DEG__TO__RAD          ! Scan Elevations (> ANC__EL_MIN)
                     TSYS_SCA(JP) = TSYS_AVR(J2)                           ! Filtered Tsys ave.
!
! ------------------ Plotting values
!
                     T2(JP)  = ( TIM_AVR(J2) + ANC%TAI_TSYS +            &
     &                           ANC%UTC_MTAI )/3600.D0 - 24.D0            ! Time in hrs from the first 00:00 UTC
                     T3(JP)  = TIM_AVR(J2) + ANC%TAI_TSYS                  ! Time in seconds 
                     T22(JP) = EL_SCA(JP)
                     T33(JP) = AZ_SCA(JP)
                     X2(JP)  = TSYS_AVR(J2)                                ! Same as TSYS_SCA
                     E2(JP)  = TSYS_RMS(J2)                                ! Tsys error
!
! ------------------ If using the effective atmospheric temperature
! ------------------ Use Spline coefficients to compute teh TATM and 
!                    substract its effect from the total Tsys
!
                     IF ( FL_APPLY_TATM ) THEN
                        IP   = IXMN8 ( LE, ELEV_TATM, T22(JP) )
                        TATM = FSPL8 ( T22(JP), LE, ELEV_TATM, VAL_TATM,   &
     &                                 IP, SPL_TATM )
                        X2(JP) = X2(JP) - TATM
                        E2(JP) = E2(JP) - TATM
                     END IF
                  END IF
 420           CONTINUE
!     
! ------------ 
!
               IF ( JP == 0 ) THEN
                  IUER = -1
                  CALL ERR_LOG ( 5037, IUER, 'BNC_PLOT',                &
     &                    'Error computing scan averages for '//        &
     &                    TRIM(FIL_BNC)//' tsys at freq ind '//            &
     &                    TRIM(CH_IND))
               END IF
!     
! ------------ Populate the diagi derived type
!
               DO 430 J3 = 1, 2
                  CALL NOUT ( SIZEOF(DIA(J3)), DIA(J3) )
!
                  DIA(J3)%IDEV      = 10
                  DIA(J3)%NCLR      = 1
                  DIA(J3)%ICOL(1)   = ICL1
                  DIA(J3)%IBST(1)   = 1         !2
                  DIA(J3)%ILST(1)   = 1         !ILST
                  DIA(J3)%IOST(1)   = IOST
                  DIA(J3)%IPST(1)   = 5         !IPST
                  DIA(J3)%IWST(1)   = IWST
                  DIA(J3)%ICLR      = 1
                  DIA(J3)%ZAG       = TRIM(TIT)
                  DIA(J3)%NAME      = FNAM_PLT(J3)
                  DIA(J3)%ITRM      = 0
                  DIA(J3)%IBATCH    = 1
 430           CONTINUE
!
! ------------ Plot(s) when type is time
!
               IF ( PLOTVAR == 'time' ) THEN

                  DO 440 J4 = 1, 2
                     DIA(J4)%ARG_UNITS = 'Time [hrs]'
 440              CONTINUE
!
! --------------- Filtered data plots Tsys vs Time @ IF
!
                  IF ( CH_PLOT == 'raw' ) THEN

 441                 CONTINUE
! ------------------
                     Y_TEMP = 0.D0
                     Y_TEMP = X1
                     CALL SORT_R8 ( NP, Y_TEMP)
                     Y_RNG = Y_TEMP(NP) - Y_TEMP(1)
                     X_RNG = T1(NP) - T1(1)
! ------------------                     
                     DIA(1)%NPOI(1)   = NP
                     DIA(1)%ADR_X8(1) = LOC(T1)
                     DIA(1)%ADR_Y8(1) = LOC(X1)
                     DIA(1)%LER(1)    = .FALSE.
                     DIA(1)%XMIN      = T1(1)      - X_RNG*DIAGI_FIE
                     DIA(1)%XMAX      = T1(NP)     + X_RNG*DIAGI_FIE
                     DIA(1)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(1)%YMAX      = Y_TEMP(NP) + Y_RNG*DIAGI_FIE
                     DIA(1)%STATUS    = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(1), IUER )
! ---------------
                     IF ( CH_PLOT == 'tot' ) GOTO 442
!
! --------------- Average data plots Tsys_ave vs Time @IF
!
                  ELSEIF ( CH_PLOT == 'ave' ) THEN
! ------------------
 442                 CONTINUE
! ------------------
                     Y_TEMP = 0.D0
                     Y_TEMP = X2
                     CALL SORT_R8 ( JP, Y_TEMP)
            
                     Y_RNG = Y_TEMP(JP) - Y_TEMP(1)
                     X_RNG = T2(JP) - T2(1)
! ------------------
                     DIA(2)%NPOI(1)    = JP
                     DIA(2)%ADR_X8(1)  = LOC(T2)
                     DIA(2)%ADR_Y8(1)  = LOC(X2)
                     DIA(2)%ADR_E8(1)  = LOC(E2)
                     DIA(2)%LER(1)     = .TRUE.
                     DIA(2)%XMIN       = T2(1)      - X_RNG*DIAGI_FIE
                     DIA(2)%XMAX       = T2(JP)     + X_RNG*DIAGI_FIE
                     DIA(2)%YMIN       = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(2)%YMAX       = Y_TEMP(JP) + Y_RNG*DIAGI_FIE
                     DIA(2)%STATUS     = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(2), IUER )
! ------------------
                     IF ( CH_PLOT == 'tot' ) GOTO 443

                  ELSE
! ------------------
                     GOTO 441
                  END IF
      
 443              CONTINUE
!
! ------------ Plot(s) when type is elev
!
               ELSEIF ( PLOTVAR == 'elev' ) THEN
! ---------------
                  DO 450 J4 = 1, 2
                     DIA(J4)%ARG_UNITS = 'Elev [deg]'
 450              CONTINUE
!
! --------------- Normalize elevation
!
                  CALL ELEV_NRML ( JP, T3, T22, X2, JN, T4, X4 )
!
! --------------- Filtered data plots Tsys vs Elev
!
                  T11(1:NP) = T11(1:NP)/DEG__TO__RAD 
!
! --------------- 
!
                  IF ( CH_PLOT == 'raw' ) THEN
! ------------------
 451                 CONTINUE
! ------------------
                     Y_TEMP = 0.D0
                     Y_TEMP = X1
                     CALL SORT_R8 ( NP, Y_TEMP)
                     Y_RNG = Y_TEMP(NP) - Y_TEMP(1)
                     X_RNG = T11(NP) - T11(1)
! ------------------
                     DIA(1)%NPOI(1)   = NP
                     DIA(1)%ADR_X8(1) = LOC(T11)
                     DIA(1)%ADR_Y8(1) = LOC(X1)
                     DIA(1)%LER(1)    = .FALSE.
                     DIA(1)%XMIN      = T11(1)     - X_RNG*DIAGI_FIE
                     DIA(1)%XMAX      = T11(NP)    + X_RNG*DIAGI_FIE
                     DIA(1)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(1)%YMAX      = Y_TEMP(NP) + Y_RNG*DIAGI_FIE
                     DIA(1)%STATUS    = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(1), IUER )
! ---------------
                     IF ( CH_PLOT == 'tot' ) GOTO 452
!
! --------------- Average and normalized data plots of Tsys vs Elev
!                 at given Sky frequency
!
                  ELSEIF ( CH_PLOT == 'ave') THEN
! ------------------
 452                 CONTINUE
! ------------------
                     Y_TEMP = 0.D0
                     Y_TEMP = X2
                     CALL SORT_R8 ( JP, Y_TEMP)
                     Y_RNG = Y_TEMP(JP) - Y_TEMP(1)
                     X_RNG = T22(JP) - T22(1)
! ------------------
                     DIA(2)%NPOI(1)    = JP
                     DIA(2)%ADR_X8(1)  = LOC(T22)
                     DIA(2)%ADR_Y8(1)  = LOC(X2)
                     DIA(2)%ADR_E8(1) = LOC(E2)
                     DIA(2)%LER(1)     = .TRUE.
                     DIA(2)%XMIN       = T22(1)     - X_RNG*DIAGI_FIE
                     DIA(2)%XMAX       = T22(JP)    + X_RNG*DIAGI_FIE
                     DIA(2)%YMIN       = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(2)%YMAX       = Y_TEMP(JP) + Y_RNG*DIAGI_FIE
                     DIA(2)%STATUS     = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(2), IUER )                        ! average plot
!
! ------------------
!
                     CALL CLRCH ( FNAM_PLT(1) )
                     FNAM_PLT(1) = TRIM(DIR_PLT)//                      &
     &                             TRIM(ANC%EXP_CODE)//'_'//            &
     &                             TRIM(PLOTORD)//'_'//                 &
     &                             TRIM(PLOTVAR)//'_'//                 &
     &                             TRIM(CH_IND)//'_'//                  &
     &                             TRIM(CH_FRQ)//CH_POL(1:1)//CH_BND//  &
     &                             '_nrm.gif'
                     WRITE (TIT2, 110) STR_DO_DATE, PLOTORD,            &
     &                                 ANC%PCS(IND_FRQ)%SKY_FRQ,        &
     &                                 ANC__POL(ANC%PCS(IND_FRQ)%POL),  &
     &                                 ANC%PCS(IND_FRQ)%ID
! ------------------                     
                     CALL NOUT ( SIZEOF(DIA(1)), DIA(1) )
! ------------------
                     DIA(1)%IDEV      = 10
                     DIA(1)%NCLR      = 1
                     DIA(1)%ICOL(1)   = ICL1
                     DIA(1)%IBST(1)   = 1               !2
                     DIA(1)%ILST(1)   = 1               !ILST
                     DIA(1)%IOST(1)   = IOST
                     DIA(1)%IPST(1)   = 5               !IPST
                     DIA(1)%IWST(1)   = IWST
                     DIA(1)%ICLR      = 1
                     DIA(1)%ZAG       = TRIM(TIT2)
                     DIA(1)%NAME      = FNAM_PLT(1)
                     DIA(J4)%ARG_UNITS = 'Elev [deg]'
                     DIA(1)%ITRM      = 0
                     DIA(1)%IBATCH    = 1
                     
                     Y_TEMP = 0.D0
                     Y_TEMP = X4
                     CALL SORT_R8 ( JN, Y_TEMP)
                     Y_RNG = Y_TEMP(JN) - Y_TEMP(1)
                     X_RNG = T4(JN) - T4(1)

                     DIA(1)%NPOI(1)    = JN
                     DIA(1)%ADR_X8(1)  = LOC(T4)
                     DIA(1)%ADR_Y8(1)  = LOC(X4)
                     DIA(1)%LER(1)     = .FALSE.
                     DIA(1)%XMIN       = T4(1)      - X_RNG*DIAGI_FIE
                     DIA(1)%XMAX       = T4(JN)     + X_RNG*DIAGI_FIE
                     DIA(1)%YMIN       = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(1)%YMAX       = Y_TEMP(JN) + Y_RNG*DIAGI_FIE
                     DIA(1)%STATUS     = DIA__DEF

                     IUER = -1
                     CALL DIAGI ( DIA(1), IUER )                        ! Normalized plot
! ------------------
                     IF ( CH_PLOT == 'tot' ) GOTO 453
!
! --------------- Plot all variables
!
                  ELSE
                     GOTO 451
                  END IF

 453              CONTINUE
!
! ------------ Consider the case where the azimuth is the variable
!
               ELSEIF ( PLOTVAR == 'azim' ) THEN
! ---------------
                  DO 460 J4 = 1, 2
                     DIA(J4)%ARG_UNITS = 'Azim [deg]'
 460              CONTINUE
!
! --------------- Normalize elevation
!
                  CALL AZIM_NRML ( JP, T3, T33, X2, JN, T4, X4 )
!
! --------------- Filtered data plots Tsys vs Azim
!
                  T12(1:NP) = T12(1:NP)/DEG__TO__RAD 
!
! --------------- 
!
                  IF ( CH_PLOT == 'raw' ) THEN
! ------------------
 461                 CONTINUE
! ------------------
                     Y_TEMP = 0.D0
                     Y_TEMP = X1
                     CALL SORT_R8 ( NP, Y_TEMP)
                     Y_RNG = Y_TEMP(NP) - Y_TEMP(1)
                     X_RNG = T12(NP) - T12(1)
! ------------------
                     DIA(1)%NPOI(1)   = NP
                     DIA(1)%ADR_X8(1) = LOC(T12)
                     DIA(1)%ADR_Y8(1) = LOC(X1)
                     DIA(1)%LER(1)    = .FALSE.
                     DIA(1)%XMIN      = T12(1)     - X_RNG*DIAGI_FIE
                     DIA(1)%XMAX      = T12(NP)    + X_RNG*DIAGI_FIE
                     DIA(1)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(1)%YMAX      = Y_TEMP(NP) + Y_RNG*DIAGI_FIE
                     DIA(1)%STATUS    = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(1), IUER )
! ---------------
                     IF ( CH_PLOT == 'tot' ) GOTO 462
!
! --------------- Average and normalized data plots of Tsys vs Azim
!                 at given Sky frequency
!
                  ELSEIF ( CH_PLOT == 'ave') THEN
! ------------------
 462                 CONTINUE
! ------------------
                     Y_TEMP = 0.D0
                     Y_TEMP = X2
                     CALL SORT_R8 ( JP, Y_TEMP)
                     Y_RNG = Y_TEMP(JP) - Y_TEMP(1)
                     X_RNG = T33(JP) - T33(1)
! ------------------
                     DIA(2)%NPOI(1)    = JP
                     DIA(2)%ADR_X8(1)  = LOC(T33)
                     DIA(2)%ADR_Y8(1)  = LOC(X2)
                     DIA(2)%ADR_E8(1)  = LOC(E2)
                     DIA(2)%LER(1)     = .TRUE.
                     DIA(2)%XMIN       = T33(1)     - X_RNG*DIAGI_FIE
                     DIA(2)%XMAX       = T33(JP)    + X_RNG*DIAGI_FIE
                     DIA(2)%YMIN       = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(2)%YMAX       = Y_TEMP(JP) + Y_RNG*DIAGI_FIE
                     DIA(2)%STATUS     = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(2), IUER )                        ! average plot
!
! ------------------
!
                     CALL CLRCH ( FNAM_PLT(1) )
                     FNAM_PLT(1) = TRIM(DIR_PLT)//                      &
     &                             TRIM(ANC%EXP_CODE)//'_'//            &
     &                             TRIM(PLOTORD)//'_'//                 &
     &                             TRIM(PLOTVAR)//'_'//                 &
     &                             TRIM(CH_IND)//'_'//                  &
     &                             TRIM(CH_FRQ)//CH_POL(1:1)//CH_BND//  &
     &                             '_nrm.gif'
                     WRITE (TIT2, 110) STR_DO_DATE, PLOTORD,            &
     &                                 ANC%PCS(IND_FRQ)%SKY_FRQ,        &
     &                                 ANC__POL(ANC%PCS(IND_FRQ)%POL),  &
     &                                 ANC%PCS(IND_FRQ)%ID
!
! ------------------
!
                     Y_TEMP = 0.D0
                     Y_TEMP = X4
                     CALL SORT_R8 ( JN, Y_TEMP)
                     Y_RNG = Y_TEMP(JN) - Y_TEMP(1)
                     X_RNG = T4(JN) - T4(1)

                     CALL NOUT ( SIZEOF(DIA(1)), DIA(1) )
! ------------------
                     DIA(1)%IDEV      = 10
                     DIA(1)%NCLR      = 1
                     DIA(1)%ICOL(1)   = ICL1
                     DIA(1)%IBST(1)   = 1               !2
                     DIA(1)%ILST(1)   = 1               !ILST
                     DIA(1)%IOST(1)   = IOST
                     DIA(1)%IPST(1)   = 5               !IPST
                     DIA(1)%IWST(1)   = IWST
                     DIA(1)%ICLR      = 1
                     DIA(1)%ZAG       = TRIM(TIT2)
                     DIA(1)%NAME      = FNAM_PLT(1)
                     DIA(J4)%ARG_UNITS = 'Azim [deg]'
                     DIA(1)%ITRM      = 0
                     DIA(1)%IBATCH    = 1
                     DIA(1)%NPOI(1)    = JN
                     DIA(1)%ADR_X8(1)  = LOC(T4)
                     DIA(1)%ADR_Y8(1)  = LOC(X4)
                     DIA(1)%LER(1)     = .FALSE.
                     DIA(1)%XMIN       = T4(1)      - X_RNG*DIAGI_FIE
                     DIA(1)%XMAX       = T4(JN)     + X_RNG*DIAGI_FIE
                     DIA(1)%YMIN       = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(1)%YMAX       = Y_TEMP(JN) + Y_RNG*DIAGI_FIE
                     DIA(1)%STATUS     = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(1), IUER )                        ! Normalized plot
! ------------------
                     IF ( CH_PLOT == 'tot' ) GOTO 463
!
! --------------- Plot all variables
!
                  ELSE
                     GOTO 461
                  END IF
! ---------------
 463              CONTINUE
! ---------------
               END IF
            END IF
!
! ------ Deal with the spectrum as the variable
!
         ELSEIF ( PLOTVAR == 'spectr' ) THEN
!
! --------- Generate plots
!
            IF ( CH_PLOT == 'raw' )  THEN
! ------------
 811           CONTINUE
!     
! ------------ Get the raw Tsys data (filtered for outliers)
!
               IUER = -1
               CALL TSYS_FREQ_FILTER_RAW ( ANC, IND_TIM, IN_POL, NP,    &
     &                                     FRQ, TSYS_ARR, IUER )
!     
! ------------ Do we have at least 1 point to plot?
!
               IF ( NP > 0 ) THEN
!
! --------------- Get the time for this index
! --------------- Get seconds since J2000 and (MJD_TSYS, TAI_TSYS)
!
                  TSINCE(1) = MJDSEC_TO_TIM (ANC%MJD_TSYS, ANC%TAI_TSYS)
!
! --------------- Get seconds since J2000 and time of computation
!
                  TSINCE(1) = TSINCE(1) + ANC%TSYS(IND_TIM)%TIM
!
! --------------- Convert the time elapsed to a date format
!
                  IUER = -1
                  SPEC_DATE(1) = TIM_TO_DATE ( TSINCE(1), IUER )
!
! --------------- Write title to variable
!

                  WRITE ( TITS(1), 113 ) SPEC_DATE(1), PLOTORD,         &
     &                                   ANC__POL(IN_POL), IND_TIM
!
! --------------- Convert to frequency to GHz
!
                  F1 = 0.D0
                  X1 = 0.D0
                  DO 810 J1 =  1, NP
                     F1(J1) = FRQ(J1)/1.D3
                     X1(J1) = TSYS_ARR(J1)
 810              CONTINUE
!     
! --------------- Populate the diagi derived type
!
                  CALL NOUT ( SIZEOF(DIA(1)), DIA(1) )
!
                  DIA(1)%IDEV      = 10
                  DIA(1)%NCLR      = 1
                  DIA(1)%ICOL(1)   = ICL1
                  DIA(1)%IBST(1)   = 1         !2
                  DIA(1)%ILST(1)   = 1         !ILST
                  DIA(1)%IOST(1)   = IOST
                  DIA(1)%IPST(1)   = 5         !IPST
                  DIA(1)%IWST(1)   = IWST
                  DIA(1)%ICLR      = 1
                  DIA(1)%ZAG       = TRIM(TITS(1))
                  DIA(1)%NAME      = FNAM_PLT(1)
                  DIA(1)%ITRM      = 0
                  DIA(1)%IBATCH    = 1
                  DIA(1)%ARG_UNITS = 'Freq [GHz]'
!
! --------------- Calculate the plot boundaries

                  Y_TEMP = 0.D0
                  Y_TEMP = X1
                  CALL SORT_R8 ( NP, Y_TEMP)
                  Y_RNG = Y_TEMP(NP) - Y_TEMP(1)
                  X_RNG = F1(NP) - F1(1)
!
! --------------- Submit plot points to diagi                 
!
                  DIA(1)%NPOI(1)   = NP
                  DIA(1)%ADR_X8(1) = LOC(F1)
                  DIA(1)%ADR_Y8(1) = LOC(X1)
                  DIA(1)%LER(1)    = .FALSE.
                  DIA(1)%XMIN      = F1(1)      - X_RNG*DIAGI_FIE
                  DIA(1)%XMAX      = F1(NP)     + X_RNG*DIAGI_FIE
                  DIA(1)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                  DIA(1)%YMAX      = Y_TEMP(NP) + Y_RNG*DIAGI_FIE
                  DIA(1)%STATUS    = DIA__DEF
! ---------------
                  IUER = -1
                  CALL DIAGI ( DIA(1), IUER )
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5039, IUER, 'BNC_PLOT',                &
     &                    'Not enough points to make a Tsys raw '//     &
     &                    'spectrum for time index '//TRIM(CH_IND) )
               END IF
! ------------
               IF ( CH_PLOT == 'tot' ) GOTO 812
! ------------
            ELSEIF ( CH_PLOT == 'ave' ) THEN
! ------------
 812           CONTINUE
!     
! ------------ Get the scan averages and RMS for Tsys for this particular scan
!
               IUER = -1
               CALL TSYS_FREQ_FILTER_SCAN ( ANC, IND_TIM, IN_POL,       &
     &                                      TIM_DIF_MAX, NF, NS, NP,    &
     &                                      TIM_AVR, FRQ_VEC,           &
     &                                      TSYS_FRQ_R8, TSYS_FRQ_E8,   &
     &                                      IUER )
!     
! ------------ Do we have at least 1 point to plot?
!
               IF ( NF > 0 ) THEN
!
! --------------- Get the time for this index
! --------------- Get seconds since J2000 and (MJD_TSYS, TAI_TSYS)
!
                  TSINCE(2) = MJDSEC_TO_TIM (ANC%MJD_TSYS, ANC%TAI_TSYS)
!
! --------------- Get seconds since J2000 and time of computation
!
                  TSINCE(2) = TSINCE(2) + TIM_AVR(IND_TIM)
!
! --------------- Convert the time elapsed to a date format
!
                  IUER = -1
                  SPEC_DATE(2) = TIM_TO_DATE ( TSINCE(2), IUER )
!
! --------------- Write title to variable
!
                  WRITE ( TITS(2), 114 ) SPEC_DATE(2), PLOTORD,         &
     &                                   ANC__POL(IN_POL), IND_TIM
!
! --------------- Convert to frequency to GHz
!
                  F1 = 0.D0
                  X1 = 0.D0
                  E1 = 0.D0
                  DO 820 J1 =  1, NF
                     F1(J1) = FRQ_VEC(J1)/1.D3
                     X1(J1) = TSYS_FRQ_R8(J1)
                     E1(J1) = TSYS_FRQ_E8(J1)
 820              CONTINUE
!     
! --------------- Populate the diagi derived type
!
                  CALL NOUT ( SIZEOF(DIA(2)), DIA(2) )
!
                  DIA(2)%IDEV      = 10
                  DIA(2)%NCLR      = 1
                  DIA(2)%ICOL(1)   = ICL1
                  DIA(2)%IBST(1)   = 1         !2
                  DIA(2)%ILST(1)   = 1         !ILST
                  DIA(2)%IOST(1)   = IOST
                  DIA(2)%IPST(1)   = 5         !IPST
                  DIA(2)%IWST(1)   = IWST
                  DIA(2)%ICLR      = 1
                  DIA(2)%ZAG       = TRIM(TITS(2))
                  DIA(2)%NAME      = FNAM_PLT(2)
                  DIA(2)%ITRM      = 0
                  DIA(2)%IBATCH    = 1
                  DIA(2)%ARG_UNITS = 'Freq [GHz]'
!
! --------------- Calculate the plot boundaries

                  Y_TEMP = 0.D0
                  Y_TEMP = X1
                  CALL SORT_R8 ( NF, Y_TEMP)
                  Y_RNG = Y_TEMP(NF) - Y_TEMP(1)
                  X_RNG = F1(NF) - F1(1)
!
! --------------- Submit plot points to diagi                 
!
                  DIA(2)%NPOI(1)   = NF
                  DIA(2)%ADR_X8(1) = LOC(F1)
                  DIA(2)%ADR_Y8(1) = LOC(X1)
                  DIA(2)%ADR_E8(1) = LOC(E1)
                  DIA(2)%LER(1)    = .TRUE.
                  DIA(2)%XMIN      = F1(1)      - X_RNG*DIAGI_FIE
                  DIA(2)%XMAX      = F1(NF)     + X_RNG*DIAGI_FIE
                  DIA(2)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                  DIA(2)%YMAX      = Y_TEMP(NF) + Y_RNG*DIAGI_FIE
                  DIA(2)%STATUS    = DIA__DEF
! ---------------
                  IUER = -1
                  CALL DIAGI ( DIA(2), IUER )
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5040, IUER, 'BNC_PLOT',                &
     &                    'Not enough scans to make a Tsys ave '//      &
     &                    'spectr plot for time index '//TRIM(CH_IND) )
               END IF
! ------------
               IF ( CH_PLOT == 'tot' ) GOTO 813
! ------------
            ELSE
! ------------
               GOTO 811
            END IF
! ---------
 813        CONTINUE
! ------
         END IF
!$$$$$$$$$$-Phase Data BEG-$$$$$$$$$$$$$$
!     
! --- PLOT PHASE
!
      ELSEIF ( PLOTORD == 'phas' .OR. PLOTORD == 'ampl' ) THEN
!
! ------
!
         IF ( PLOTVAR == 'time' ) THEN
!
! --------- Get the raw Phase cal data (Filtered for NaN values)
!
            IUER = -1
            CALL PCAL_TIME_FILTER_RAW (ANC, IND_FRQ, NP, T1, CX1, IUER)
!
! --------- Get the PCAL scan averages and RMS for Amplitude and
!           Phase (at the given scan differences).
! --------- The averages are also filtered out for outliers.
!     
            IUER = -1
            CALL PCAL_TIME_FILTER_SCAN ( ANC, IND_FRQ, TIM_DIF_MAX, NP, &
     &                                   T1, CX1, NS, TIM_AVR,          &
     &                                   PCAL_AVR, PCAL_AMP_RMS,        &
     &                                   PCAL_PHA_RMS, IND_SCA,         &
     &                                   IND_ARR, IUER )
!
! --------- Proceed only if there are actual points to use
!
            IF ( NP > 0 ) THEN 
!
! ------------ Convert time to hours from midnight UTC
!     
               DO 510 J1 = 1, NP !LP
                  T1(J1) = (T1(J1) + ANC%TAI_PCAL + ANC%UTC_MTAI)/3.6D3 &
     &                     - 24.D0
 510           CONTINUE
!
! ------------ Plot for the phase
!
               IF ( PLOTORD == 'phas' ) THEN
!
! --------------- Filter out any zero values from the average values
!
                  JP = 0
                  DO 520 J2 = 1, NS
!     
! ------------------ In the event of one scan, let's override the check
!
                     IF ( NS == 1 ) GOTO 521
!     
! ------------------ Is the average Pcal of this scan above minimum Pcal
!
                     IF ( ( ABS(PCAL_AVR(J2)) > ANC__AMP_MIN ) .AND.    &
     &                    ( ABS(PCAL_AVR(J2)) < ANC__AMP_MAX ) ) THEN
! ---------------------
 521                    CONTINUE
! ---------------------
                        JP = JP + 1                                        ! Update counter
! ---------------------
                        PCAL_SCA(JP) = PCAL_AVR(J2)                        ! Filtered Pcal ave.
!
! --------------------- Plotting values
!
                        T2(JP)  = ( TIM_AVR(J2) + ANC%TAI_PCAL +        &
     &                              ANC%UTC_MTAI )/3600.D0 - 24.D0         ! Time in hrs from the first 00:00 UTC
                        T3(JP)  = TIM_AVR(J2) + ANC%TAI_PCAL               ! Time in seconds 
                        CX2(JP) = PCAL_AVR(J2)                             ! Same as PCAL_SCA
                        E2(JP)  = PCAL_PHA_RMS(J2)                         ! Pcal error
! ---------------------
                        IF ( NS == 1 ) GOTO 522
! ---------------------
                     END IF
 520              CONTINUE
! ---------------
 522              CONTINUE
!     
! --------------- In the event no scans qualify for analysis, report
!                 error message.
!
                  IF ( JP == 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 5038, IUER, 'BNC_PLOT',             &
     &                       'Error computing scan averages for '//     &
     &                       TRIM(FIL_BNC)//' tsys at freq ind '//        &
     &                       TRIM(CH_IND))
                  END IF
!     
! --------------- Populate the diagi derived type that relates to both
!                 raw and average plots
!
                  DO 530 J3 = 1, 2
                     CALL NOUT ( SIZEOF(DIA(J3)), DIA(J3) )
!
                     DIA(J3)%IDEV      = 10
                     DIA(J3)%NCLR      = 1
                     DIA(J3)%ICOL(1)   = ICL1
                     DIA(J3)%IBST(1)   = 1              !2
                     DIA(J3)%ILST(1)   = 1              !ILST
                     DIA(J3)%IOST(1)   = IOST
                     DIA(J3)%IPST(1)   = 5              !IPST
                     DIA(J3)%IWST(1)   = IWST
                     DIA(J3)%ICLR      = 1
                     DIA(J3)%ZAG       = TRIM(TIT)
                     DIA(J3)%NAME      = FNAM_PLT(J3)
                     DIA(J3)%ARG_UNITS = 'Time [hrs]'
                     DIA(J3)%ITRM      = 0
                     DIA(J3)%IBATCH    = 1
 530              CONTINUE
!
! --------------- Raw fills
!
                  IF ( CH_PLOT == 'raw' ) THEN
! ------------------
 551                 CONTINUE
!     
! ------------------ Resolve phase values and fit to the range of
!                    of [-pi, pi]
!
                     DO 541 J5 = 1, NP
                        X1(J5) = PHAS_CMPL_R4(CX1(J5))
 541                 CONTINUE
!
! ------------------ sort the ordinate elements (in increasing) order
!                    and get a range for both ordinate (Y-axis) and
!                    abscissa (X-axis)
!
                     Y_TEMP = 0.D0
                     Y_TEMP = X1
                     CALL SORT_R8 ( NP, Y_TEMP)
                     Y_RNG = Y_TEMP(NP) - Y_TEMP(1)
                     X_RNG = T1(NP) - T1(1)
! ------------------                     
                     DIA(1)%NPOI(1)   = NP
                     DIA(1)%ADR_X8(1) = LOC(T1)
                     DIA(1)%ADR_Y8(1) = LOC(X1)
                     DIA(1)%LER(1)    = .FALSE.
                     DIA(1)%XMIN      = T1(1)      - X_RNG*DIAGI_FIE
                     DIA(1)%XMAX      = T1(NP)     + X_RNG*DIAGI_FIE
                     DIA(1)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(1)%YMAX      = Y_TEMP(NP) + Y_RNG*DIAGI_FIE
                     DIA(1)%STATUS    = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(1), IUER )
! ------------------
                     IF ( CH_PLOT == 'tot' ) GOTO 552
!
! --------------- Plot the averages
!
                  ELSEIF ( CH_PLOT == 'ave' ) THEN
! ------------------
 552                 CONTINUE
!     
! ------------------ Resolve phase values and fit to the range of
!                    of [-pi, pi]
!
                     DO 542 J5 = 1, JP
                        X2(J5) = PHAS_CMPL_R4(CX2(J5))
 542                 CONTINUE
!
! ------------------ sort the ordinate elements (in increasing) order
!                    and get a range for both ordinate (Y-axis) and
!                    abscissa (X-axis)
!
                     Y_TEMP = 0.D0
                     Y_TEMP = X2
                     CALL SORT_R8 ( JP, Y_TEMP)
                     Y_RNG = Y_TEMP(JP) - Y_TEMP(1)
                     X_RNG = T2(JP) - T2(1)
! ------------------
                     DIA(2)%NPOI(1)    = JP
                     DIA(2)%ADR_X8(1)  = LOC(T2)
                     DIA(2)%ADR_Y8(1)  = LOC(X2)
                     DIA(2)%ADR_E8(1) = LOC(E2)
                     DIA(2)%LER(1)     = .TRUE.
                     DIA(2)%XMIN       = T2(1)      - X_RNG*DIAGI_FIE
                     DIA(2)%XMAX       = T2(JP)     + X_RNG*DIAGI_FIE
                     DIA(2)%YMIN       = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(2)%YMAX       = Y_TEMP(JP) + Y_RNG*DIAGI_FIE
                     DIA(2)%STATUS     = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(2), IUER )
! ------------------
                     IF ( CH_PLOT == 'tot' ) GOTO 553
!     
! --------------- Generate all plots
!
                  ELSE
! ------------------
                     GOTO 551
! ------------------
                  END IF
! ---------------
 553              CONTINUE
!
! ------------ Plot for the amplitude
!
               ELSE
!
! --------------- Filter out any zero values from the average values
!
                  JP = 0
                  DO 620 J2 = 1, NS
!
! ------------------ In the event of one scan, let's override the check
!
                     IF ( NS == 1 ) GOTO 621
!     
! ------------------ Is average Amplitude of this scan above minimum Pcal
!
                     IF ( ( ABS(PCAL_AVR(J2)) > ANC__AMP_MIN ) .AND.     &
     &                    ( ABS(PCAL_AVR(J2)) < ANC__AMP_MAX )    ) THEN
! ---------------------
 621                    CONTINUE
! ---------------------                        
                        JP = JP + 1                                        ! Update counter
! ---------------------
                        AMP_SCA(JP) = ABS(PCAL_AVR(J2))                    ! Filtered Pcal ave.
!
! --------------------- Plotting values
!
                        T2(JP)  = ( TIM_AVR(J2) + ANC%TAI_PCAL +        &
     &                              ANC%UTC_MTAI )/3600.D0 - 24.D0         ! Time in hrs from the first 00:00 UTC
                        T3(JP)  = TIM_AVR(J2) + ANC%TAI_PCAL               ! Time in seconds 
                        X2(JP)  = ABS(PCAL_AVR(J2))                        ! Same as AMP_SCA
                        E2(JP)  = PCAL_AMP_RMS(J2)                         ! Pcal amplitude scan error
! ---------------------
                        IF ( NS == 1 ) GOTO 622
! ---------------------
                     END IF
 620              CONTINUE
! ---------------
 622              CONTINUE
!     
! --------------- Populate the diagi derived type that relates to both
!                 raw and average plots
!
                  DO 630 J3 = 1, 2
                     CALL NOUT ( SIZEOF(DIA(J3)), DIA(J3) )
! ------------------
                     DIA(J3)%IDEV      = 10
                     DIA(J3)%NCLR      = 1
                     DIA(J3)%ICOL(1)   = ICL1
                     DIA(J3)%IBST(1)   = 1              !2
                     DIA(J3)%ILST(1)   = 1              !ILST
                     DIA(J3)%IOST(1)   = IOST
                     DIA(J3)%IPST(1)   = 5              !IPST
                     DIA(J3)%IWST(1)   = IWST
                     DIA(J3)%ICLR      = 1
                     DIA(J3)%ZAG       = TRIM(TIT)
                     DIA(J3)%NAME      = FNAM_PLT(J3)
                     DIA(J3)%ARG_UNITS = 'Time [hrs]'
                     DIA(J3)%ITRM      = 0
                     DIA(J3)%IBATCH    = 1
 630              CONTINUE
!
! --------------- fill the raw data elements
!
                  IF ( CH_PLOT == 'raw' ) THEN
! ------------------
 651                 CONTINUE
!     
! ------------------ Resolve amplitude values
!
                     DO 641 J5 = 1, NP
                        X1(J5) = ABS(CX1(J5))
 641                 CONTINUE
!
! ------------------ sort the ordinate elements (in increasing) order
!                    and get a range for both ordinate (Y-axis) and
!                    abscissa (X-axis)
!
                     Y_TEMP = 0.D0
                     Y_TEMP = X1
                     CALL SORT_R8 ( NP, Y_TEMP)
                     Y_RNG = Y_TEMP(NP) - Y_TEMP(1)
                     X_RNG = T1(NP) - T1(1)
! ------------------
                     DIA(1)%NPOI(1)   = NP
                     DIA(1)%ADR_X8(1) = LOC(T1)
                     DIA(1)%ADR_Y8(1) = LOC(X1)
                     DIA(1)%LER(1)    = .FALSE.
                     DIA(1)%XMIN      = T1(1)      - X_RNG*DIAGI_FIE
                     DIA(1)%XMAX      = T1(NP)     + X_RNG*DIAGI_FIE
                     DIA(1)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(1)%YMAX      = Y_TEMP(NP) + Y_RNG*DIAGI_FIE
                     DIA(1)%STATUS    = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(1), IUER )
! ------------------
                     IF ( CH_PLOT == 'tot' ) GOTO 652
!
! --------------- Plot the averages
!
                  ELSEIF ( CH_PLOT == 'ave' ) THEN
! ------------------
 652                 CONTINUE
!
! ------------------ sort the ordinate elements (in increasing) order
!                    and get a range for both ordinate (Y-axis) and
!                    abscissa (X-axis)
!
                     Y_TEMP = 0.D0
                     Y_TEMP = X2
                     CALL SORT_R8 ( JP, Y_TEMP)
                     Y_RNG = Y_TEMP(JP) - Y_TEMP(1)
                     X_RNG = T2(JP) - T2(1)
! ------------------
                     DIA(2)%NPOI(1)    = JP
                     DIA(2)%ADR_X8(1)  = LOC(T2)
                     DIA(2)%ADR_Y8(1)  = LOC(X2)
                     DIA(2)%ADR_E8(1) = LOC(E2)
                     DIA(2)%LER(1)     = .TRUE.
                     DIA(2)%XMIN       = T2(1)      - X_RNG*DIAGI_FIE
                     DIA(2)%XMAX       = T2(JP)     + X_RNG*DIAGI_FIE
                     DIA(2)%YMIN       = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                     DIA(2)%YMAX       = Y_TEMP(JP) + Y_RNG*DIAGI_FIE
                     DIA(2)%STATUS     = DIA__DEF
! ------------------
                     IUER = -1
                     CALL DIAGI ( DIA(2), IUER )
! ------------------
                     IF ( CH_PLOT == 'tot' ) GOTO 653
!     
! --------------- Generate all plots
!
                  ELSE
! ------------------
                     GOTO 651
! ------------------
                  END IF
! ---------------
 653              CONTINUE
               END IF
            END IF
!
! ------ Plot(s) when type is Spectr
!
         ELSEIF ( PLOTVAR == 'spectr' ) THEN
!
! --------- fill the raw plots
!
            IF ( CH_PLOT == 'raw' ) THEN
! ------------
 671           CONTINUE               
!     
! ------------ Get the raw Phase cal data (Filtered for NaN values)
!
               IUER = -1
               CALL PCAL_FREQ_FILTER_RAW ( ANC, IND_TIM, IN_POL, NP,    &
     &                                     FRQ, CX1, IUER )
!
! ------------ Do we have at least 1 point to plot?
!
               IF ( NP > 0 ) THEN
!
! --------------- Get the time for this index
! --------------- Get seconds since J2000 and (MJD_TSYS, TAI_TSYS)
!
                  TSINCE(1) = MJDSEC_TO_TIM (ANC%MJD_PCAL, ANC%TAI_PCAL)
!
! --------------- Get seconds since J2000 and time of computation
!
                  TSINCE(1) = TSINCE(1) + ANC%PCAL(IND_TIM)%TIM
!
! --------------- Convert the time elapsed to a date format
!
                  IUER = -1
                  SPEC_DATE(1) = TIM_TO_DATE ( TSINCE(1), IUER )
!
! --------------- Write title to variable
!
                  WRITE ( TITS(1), 113 ) SPEC_DATE(1), PLOTORD,         &
     &                                   ANC__POL(IN_POL), IND_TIM
!     
! --------------- Deal with phas data 
!
                  IF ( PLOTORD == 'phas' ) THEN
!
! ------------------ Convert to frequency to GHz, and get phase in
!                    range [-pi, pi]
!
                     F1 = 0.D0
                     X1 = 0.D0
                     DO 681 J1 =  1, NP
                        F1(J1) =  FRQ(J1)/1.D3                  ! Frequency [GHz]
                        X1(J1) =  PHAS_CMPL_R4(CX1(J1))         ! Phase     [-pi,pi]
 681                 CONTINUE
!     
! --------------- Deal with ampl data 
!
                  ELSE
!
! ------------------ Convert to frequency to GHz, and get phase ampl.
!     
                     F1 = 0.D0
                     X1 = 0.D0
                     DO 691 J1 =  1, NP
                        F1(J1) =  FRQ(J1)/1.D3                  ! Frequency  [GHz]
                        X1(J1) =  ABS(CX1(J1))                  ! Phase Ampl []
 691                 CONTINUE
                  END IF
!
! --------------- Populate the diagi derived type
!
                  CALL NOUT ( SIZEOF(DIA(1)), DIA(1) )
! ---------------
                  DIA(1)%IDEV      = 10
                  DIA(1)%NCLR      = 1
                  DIA(1)%ICOL(1)   = ICL1
                  DIA(1)%IBST(1)   = 1         !2
                  DIA(1)%ILST(1)   = 1         !ILST
                  DIA(1)%IOST(1)   = IOST
                  DIA(1)%IPST(1)   = 5         !IPST
                  DIA(1)%IWST(1)   = IWST
                  DIA(1)%ICLR      = 1
                  DIA(1)%ZAG       = TRIM(TITS(1))
                  DIA(1)%NAME      = FNAM_PLT(1)
                  DIA(1)%ITRM      = 0
                  DIA(1)%IBATCH    = 1
                  DIA(1)%ARG_UNITS = 'Freq [GHz]'
!
! --------------- Calculate the plot boundaries

                  Y_TEMP = 0.D0
                  Y_TEMP = X1
                  CALL SORT_R8 ( NP, Y_TEMP)
                  Y_RNG = Y_TEMP(NP) - Y_TEMP(1)
                  X_RNG = F1(NP) - F1(1)
!
! --------------- Submit plot points to diagi                 
!
                  DIA(1)%NPOI(1)   = NP
                  DIA(1)%ADR_X8(1) = LOC(F1)
                  DIA(1)%ADR_Y8(1) = LOC(X1)
                  DIA(1)%LER(1)    = .FALSE.
                  DIA(1)%XMIN      = F1(1)      - X_RNG*DIAGI_FIE
                  DIA(1)%XMAX      = F1(NP)     + X_RNG*DIAGI_FIE
                  DIA(1)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                  DIA(1)%YMAX      = Y_TEMP(NP) + Y_RNG*DIAGI_FIE
                  DIA(1)%STATUS    = DIA__DEF
! ---------------
                  IUER = -1
                  CALL DIAGI ( DIA(1), IUER )
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5041, IUER, 'BNC_PLOT',                & 
     &                    'Not enough points to make a '//PLOTORD//     &
     &                    ' raw spectr for time index '//TRIM(CH_IND) )
               END IF
! ------------
               IF ( CH_PLOT == 'tot' ) GOTO 672
! ------------
            ELSEIF ( CH_PLOT == 'ave' ) THEN
! ------------
 672           CONTINUE               
!     
! ------------ Get the averaged Phase cal data (Filtered for NaN values)
!
               IUER = -1
               CALL PCAL_FREQ_FILTER_SCAN (ANC, IND_TIM, IN_POL,        &
     &                                     TIM_DIF_MAX, NF, NS, NP,     &
     &                                     TIM_AVR, FRQ, PCAL_FRQ_R8,   &
     &                                     PHA_FRQ_E8, AMP_FRQ_E8, IUER)
!
! ------------ Do we have at least 1 point to plot?
!
               IF ( NF > 0 ) THEN
!
! --------------- Get the time for this index
! --------------- Get seconds since J2000 and (MJD_PCAL, TAI_PCAL)
!
                  TSINCE(2) = MJDSEC_TO_TIM (ANC%MJD_PCAL, ANC%TAI_PCAL)
!
! --------------- Get seconds since J2000 and time of computation
!
                  TSINCE(2) = TSINCE(2) + TIM_AVR(IND_TIM)
!
! --------------- Convert the time elapsed to a date format
!
                  IUER = -1
                  SPEC_DATE(2) = TIM_TO_DATE ( TSINCE(2), IUER )
!
! --------------- Write title to variable
!
                  WRITE ( TITS(2), 114 ) SPEC_DATE(2), PLOTORD,         &
     &                                   ANC__POL(IN_POL), IND_TIM
!
! --------------- Deal with phas data
!
                  IF ( PLOTORD == 'phas' ) THEN
!
! ------------------ Convert to frequency to GHz
!
                     F1 = 0.D0
                     X2 = 0.D0
                     E2 = 0.D0
                     DO 682 J1 =  1, NF
                        F1(J1) =  FRQ(J1)/1.D3                          ! Frequency [GHz]
                        X2(J1) =  PHAS_CMPL_R4(PCAL_FRQ_R8(J1))         ! Phase     [-pi,pi]
                        E2(J1) =  PHA_FRQ_E8(J1)
 682                 CONTINUE
!
! --------------- Deal with ampl data
!
                  ELSE
!
! ------------------ Convert to frequency to GHz
!
                     F1 = 0.D0
                     X2 = 0.D0
                     E2 = 0.D0
                     DO 692 J1 =  1, NF
                        F1(J1) =  FRQ(J1)/1.D3                          ! Frequency  [GHz]
                        X2(J1) =  ABS(PCAL_FRQ_R8(J1))                  ! Phase Ampl []
                        E2(J1) =  AMP_FRQ_E8(J1)
 692                 CONTINUE
                  END IF
!
! --------------- Populate the diagi derived type
!
                  CALL NOUT ( SIZEOF(DIA(2)), DIA(2) )
! ---------------
                  DIA(2)%IDEV      = 10
                  DIA(2)%NCLR      = 1
                  DIA(2)%ICOL(1)   = ICL1
                  DIA(2)%IBST(1)   = 1         !2
                  DIA(2)%ILST(1)   = 1         !ILST
                  DIA(2)%IOST(1)   = IOST
                  DIA(2)%IPST(1)   = 5         !IPST
                  DIA(2)%IWST(1)   = IWST
                  DIA(2)%ICLR      = 1
                  DIA(2)%ZAG       = TRIM(TITS(2))
                  DIA(2)%NAME      = FNAM_PLT(2)
                  DIA(2)%ITRM      = 0
                  DIA(2)%IBATCH    = 1
                  DIA(2)%ARG_UNITS = 'Freq [GHz]'
!
! --------------- Calculate the plot boundaries

                  Y_TEMP = 0.D0
                  Y_TEMP = X2
                  CALL SORT_R8 ( NF, Y_TEMP)
                  Y_RNG = Y_TEMP(NF) - Y_TEMP(1)
                  X_RNG = F1(NF) - F1(1)
!
! --------------- Submit plot points to diagi                 
!
                  DIA(2)%NPOI(1)   = NF
                  DIA(2)%ADR_X8(1) = LOC(F1)
                  DIA(2)%ADR_Y8(1) = LOC(X2)
                  DIA(2)%ADR_E8(1) = LOC(E2)
                  DIA(2)%LER(1)    = .TRUE.
                  DIA(2)%XMIN      = F1(1)      - X_RNG*DIAGI_FIE
                  DIA(2)%XMAX      = F1(NF)     + X_RNG*DIAGI_FIE
                  DIA(2)%YMIN      = Y_TEMP(1)  - Y_RNG*DIAGI_FIE
                  DIA(2)%YMAX      = Y_TEMP(NF) + Y_RNG*DIAGI_FIE
                  DIA(2)%STATUS    = DIA__DEF
! ---------------
                  IUER = -1
                  CALL DIAGI ( DIA(2), IUER )
! ---------------
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5044, IUER, 'BNC_PLOT',                &
     &                    'Not enough scans to make a '//PLOTORD//      &
     &                    ' ave spectr for time index '//TRIM(CH_IND) )
               END IF
! ------------
               IF ( CH_PLOT == 'tot' ) GOTO 673
! ------------
            ELSE
               GOTO 671
            END IF
! ---------
 673        CONTINUE
         END IF
!$$$$$$$$$$-Phase Data END-$$$$$$$$$$$$$$
!##########-GPS Timer Data BEG-#########
!     
! --- PLOT GPS and PPS Formatter Time differences
!
      ELSEIF ( PLOTORD == 'fmgt' .OR. PLOTORD == 'fmpt' ) THEN

         CALL FMTGPS_TIME_FILTER_RAW ( ANC, IND_FRQ, NP, T1, FMGT,  &
     &                                    FMPT, IUER )






!##########-GPS Timer Data END-#########         
      END IF 
!     
! --- Printing formats
!
!@!##!@! 110  FORMAT ( 'PLOT= ',A5 ,' FRQ_IND= ',I4,' FRQ= ',F8.1,                &
!@!##!@!     &         ' POL= ', A1,' ID= ', A4 )
 110  FORMAT ( A22, ' PLOT=',A4 ,' FRQ=',F7.1,' POL=', A1,' ID=', A7)
 111  FORMAT ( F8.1 )
 112  FORMAT ( A22, ' PLOT=', A4, ' norm ',' FRQ=',F7.1,                 &
     &         ' POL=', A1,' ID=', A7 )
 113  FORMAT ( A22, ' PLOT=', A4, ' raw spec',' POL=', A1,' IND=', I5 )
 114  FORMAT ( A22, ' PLOT=', A4, ' ave spec',' POL=', A1,' IND=', I5 )

!     
 104  FORMAT("TP_SENSOR:  ", I4, 3X, F8.2, 3X, A1, 3X, A4 )
 105  FORMAT("TSYS: ", I4, 1X, I4, 3X, A22, 3X, F7.1, 3X, F10.4, 2X, F8.4 )

      END PROGRAM
