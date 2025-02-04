      PROGRAM BNC_PLOT_TEST
!
! **************************************************************************************
! *                                                                                    *
! *   Program BNC_PLOT                                                                 *
! *                                                                                    *
! *   Plot the Tsys from a given experiment.                                           *
! *   INPUT:                                                                           *
! *          BNC_FILE      =  path to binary experiment file            { CHAR }       *
! *                                                                                    *
! *          IND           =  index to plot (either freq or tsys idx)   { INT }        *
! *                                                                                    *
! *          PLOTORD       =  Ordinate of plot                          { CHAR }       *
! *                           == tsys    - Plot Tsys                                   *
! *                           == phas    - Plot Phase                                  *
! *                           == ampl    - Plot Amplitude                              *
! *                                                                                    *
! *          PLOTVAR       =  Variable to plot (abscissa)               { CHAR }       *
! *                           IF PLOTORD == tsys                                       *
! *                              == time   - Tsys [K] vs run Time [Hrs] at IF          *
! *                                        - Tsys_ave [K] vs run Time [Hrs]            *
! *                                        - [default mode]                            *
! *                              == elev   - Tsys [K] vs Elevation [deg] at IF         *
! *                              == azim   - Tsys [K] vs Azimuth [deg] at IF           *
! *                              == azel   - [X,Y,Z] = [ Az, El, Tsys_Range]           *
! *                              == spectr - Tsys [K] vs Freq [MHz] at given time      *
! *                           IF PLOTORD == phas                                       *
! *                              == time   - Phase [rad] vs run Time [Hrs] at IF       *
! *                                        - Phase_ave [rad] vs run Time [Hrs]         *
! *                              == spectr - Phase [rad] vs Freq [GHz] at given time   *
! *                           IF PLOTORD == ampl                                       *
! *                              == time   - Amplitude [] vs run Time [Hrs] at IF      *
! *                                        - Amplitude_ave [] vs run Time [Hrs]        *
! *                              == spectr - Amplitude [] vs Freq [GHz] at given time  *
! *                                                                                    *
! *          TIM_DIF_MAX   = Time difference between scans              { REAL } [s]   *
! *                          Default, TIM_DIF_MAX = 9.5                                *
! *                                                                                    *
! *   USAGE:                                                                           *
! *           bnc_plot_test BNC_FILE IND PLOTVAR PLOTORD TIM_DIF_MAX                   *
! *                                                                                    *
! *   OUTPUT:                                                                          *
! *           pgplot figure(s)                                                         *
! *                                                                                    *
! *  ### 25-AUG-2021     BNC_PLOT_TEST       v4.0 (c)    N. Habana   19-DEC-2022 ###   *
! *                                                                                    *
! **************************************************************************************
!
      IMPLICIT   NONE
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      CHARACTER  FILIN*128, FILOUT*128, STR*128, PLOTVAR*6, PLOTORD*4
      CHARACTER  STR_DO_DATE*64, TIT*64, CH_IND*12, CH_POL*2
      CHARACTER  CH_FLG1*10, CH_FLG2*10, CH_FLG3*10, CH_FLG4*10
      CHARACTER  CH_FLG5*10, CH_FLG6*10, CH_FLG7*10, CH_FLG8*10
      CHARACTER  CH_PLOT*4, CH_DATE(2)*22
      INTEGER*4  MJD_RNG(2)
      REAL*8     UTC_RNG(2)
      INTEGER*4  IER, IUER, IDEV, PRINTMOD
      REAL*8     TIM_DIF_MAX
      CHARACTER  CH_TIM_DIF*6
      REAL*8     TIM_AVR(ANC__MEPC), TSYS_AVR(ANC__MEPC)
      REAL*8     TSYS_RMS(ANC__MEPC), AMP_SCA(ANC__MEPC)
      COMPLEX*8  PCAL_AVR(ANC__MEPC), PCAL_SCA(ANC__MEPC)
      REAL*8     PCAL_AMP_RMS(ANC__MEPC), PCAL_PHA_RMS(ANC__MEPC)
      REAL*8     T1(ANC__MEPC), X1(ANC__MEPC), T2(ANC__MEPC)
      REAL*8     X2(ANC__MEPC), E2(ANC__MEPC), T11(ANC__MEPC)
      REAL*8     T12(ANC__MEPC), T22(ANC__MEPC), T3(ANC__MEPC)
      REAL*8     X3(ANC__MEPC), T4(ANC__MEPC), X4(ANC__MEPC)
      REAL*8     AZ_SCA(ANC__MEPC), EL_SCA(ANC__MEPC)
      REAL*8     TSYS_SCA(ANC__MEPC), F1(ANC__MEPC)
      REAL*8     FRQ_R8(ANC__MEPC,2), TSYS_FRQ_R8(ANC__MEPC,2)
      COMPLEX*8  CX1(ANC__MEPC), CX2(ANC__MEPC)
      INTEGER*4  SEEK_SET, ARG_LEN, PREF
      LOGICAL*1  LEX, FL_APPLY_TATM, FL_PRINT
      CHARACTER  ID_ARR(ANC__MTPS)*4, MODE*6
      INTEGER*4  ME, NS
      PARAMETER  ( ME = 16 ) 
      REAL*8     ELEV_TATM(ME), VAL_TATM(ME), SPL_TATM(ME), TATM
      INTEGER*4  IND_ARR(ANC__MEPC), IND_SCA(ANC__MEPC)
      REAL*8     VAL_FRQ, FRQ_ARR(ANC__MEPC)
      INTEGER*4  LUN, NBT, IND_FRQ, IND_TIM, IND_POL, LE, LOC_DEC
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8
      INTEGER*4  IP, KP, LP, NP, JP, JN, KB, K1, K2
      INTEGER*4  INPUT_POL
      INTEGER*8  IS
      REAL*8,    EXTERNAL :: FSPL8
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*8, EXTERNAL :: LSEEK, READ
      INTEGER*4, EXTERNAL :: IXMN8, IXMN4, LINDEX
      REAL*4,    EXTERNAL :: PHAS_CMPL_R4
!
! --- Pre-define variables
!
      IND_FRQ = 0               ! Frequency index
      FL_APPLY_TATM  = .FALSE.   ! Should we use the TATM azimuths?
      PLOTORD        = 'tsys'
      PLOTVAR        = 'time'          ! or 'elev'  or 'spectr' or 'azim' or 'azel'
      TIM_DIF_MAX    = 9.5D0 
      PRINTMOD  = 1
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~ 'Usage: bnc_plot -i-fil input_file -ind|-frq|-tim index|freq|time_gap -pol polarization -ord plot_ordinate -var plot_variable -del tim_dif_max -o output
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!## FIX THE ERROR MESSAGES TO USE ERR_LOG
!
! --- Read user input
!
! bnc_plot_test -i gsst04.bnc -frq_ind 999 -pol h -ord tsys -var time -del 8.0 -plot ave -o gsst04.dat
! bnc_plot_test -i gsst04.bnc -tim_rng 2022.12.25-18:56:45.00 2022.12.31-02:02:30.00 -pol h -ord tsys -var spectr -del 8.0 -plot ave -o gsst04.dat

      IF ( IARGC() < 1 .OR. IARGC() > 17 ) THEN
         WRITE ( 6, '(A)' )                                             &
     &         'USAGE: bnc_plot_test -i bnc_file '//                    &
     &                 '-frq_ind|-frq_val|-tim_ind|-tim_rng '//         &
     &                               'index|freq|index|time_range '//   &
     &                 '-pol polarization '//                           &
     &                 '-ord plot_ordinate '//                          &
     &                 '-var plot_variable '//                          &
     &                 '-del tim_dif_max '//                            &
     &                 '-plot raw|ave '//                               &
     &                 '-o output'
         CALL EXIT ( 0 )
      ELSE
! ------
         write (6,'(A)') 'running BNC_PLOT_TEST'
!
! ------ Arg. 1
! ------ Input flag 1
!
         CALL CLRCH  ( CH_FLG1 )
         CALL GETARG ( 1, CH_FLG1 )
         IF ( CH_FLG1 .NE. '-i' ) THEN !
            WRITE ( 6, '(A)' ) 'First flag should be -i not '//         &
     &	                        TRIM(CH_FLG1)
	    CALL EXIT (1)
         END IF ! ch_flg1
!
! ------ Arg. 2
! ------ Get the file and check if it exists?
!
         CALL GETARG ( 2, FILIN )
         INQUIRE     ( FILE=FILIN, EXIST=LEX )
         IF ( .NOT. LEX ) THEN ! 
            IUER = -1
            CALL ERR_LOG ( 5001, IUER, 'BNC_PLOT_TEST',                 &
     &              'Cannot find file '//TRIM(FILIN) )
            CALL EXIT ( 1 )
         END IF ! lex
!
! ------ Arg. 3
! ------ Input flag 2
!
         CALL CLRCH  ( CH_FLG2 )
	 CALL GETARG ( 3, CH_FLG2 )
         IF ( CH_FLG2 == '-frq_ind' .OR.                                &
     &        CH_FLG2 == '-frq_val' .OR.                                &
     &        CH_FLG2 == '-tim_ind' .OR.                                &
     &        CH_FLG2 == '-tim_rng'       ) THEN ! 
! ---------
	    CONTINUE
! ---------
            IF ( CH_FLG2 == '-tim_rng' ) THEN ! 
               IF ( IARGC() .NE. 17 ) THEN !
                  IUER = -1
                  CALL ERR_LOG ( 5002, IUER, 'BNC_PLOT_TEST',           &
     &                    'For flag '//CH_FLG2//' we expect 17 inputs ')
                  CALL EXIT ( 1 )
               END IF ! iargc =/= 17
            END IF ! ch_flg2 == -tim_rng
         ELSE
            IUER = -1
            CALL ERR_LOG ( 5003, IUER, 'BNC_PLOT_TEST',                 &
     &              'Unsupported flag '//CH_FLG2//'expected: '//        &
     &              ' -frq_ind, -frq_val, -tim_ind, or -tim_rng' )
            CALL EXIT ( 1 )
         END IF ! ch_flg2
!
! ------ List of arguments if we are not doing the time range
!
         IF ( CH_FLG2 .NE. '-tim_rng' ) THEN !
!
! --------- Arg. 4
! --------- Get either the frequency index, frequency value,  or time index
!
            CALL CLRCH  ( CH_IND )
            CALL GETARG ( 4, CH_IND )
!
! --------- Check if this is an integer or real number
! --------- is there a decimal point on the input?
!     
            LOC_DEC = LINDEX( CH_IND, ".")
            IF ( LOC_DEC == 0 ) THEN !
               CALL CHIN ( CH_IND, IND_FRQ )
            ELSE
               IND_FRQ = ATP__INIT
               READ ( UNIT=CH_IND, FMT='(F12.2)', IOSTAT=IER ) VAL_FRQ
               IF ( IER .NE. 0 ) THEN !
                  IUER = -1
                  CALL ERR_LOG ( 5004, IUER, 'BNC_PLOT_TEST',           &
     &                    'Expected Real value for '//TRIM(CH_IND) )
               END IF ! ier =/= 0
            END IF ! loc_dec = 0
!
! --------- Arg. 5
! --------- Get the polarization flag
!
            CALL CLRCH  ( CH_FLG3 )
  	    CALL GETARG ( 5, CH_FLG3 )
	    IF ( CH_FLG3 .NE. '-pol' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5005, IUER, 'BNC_PLOT_TEST',                 &
     &                 'Unsupported flag '//CH_FLG3//' expected -pol ' )
	       CALL EXIT ( 1 )
            END IF ! ch_flg3
!
! --------- Arg. 6
! --------- Get the polarization value
!
            CALL CLRCH ( CH_POL )
  	    CALL GETARG ( 6, CH_POL )
            IF ( CH_POL == 'R' ) THEN !
               INPUT_POL = ANC__R_POL
            ELSEIF ( CH_POL == 'L' ) THEN
               INPUT_POL = ANC__L_POL
            ELSEIF ( CH_POL == 'H' ) THEN
               INPUT_POL = ANC__H_POL
            ELSEIF ( CH_POL == 'V' ) THEN
               INPUT_POL = ANC__V_POL
            ELSEIF ( CH_POL == 'X' ) THEN
               INPUT_POL = ANC__X_POL
            ELSEIF ( CH_POL == 'Y' ) THEN
               INPUT_POL = ANC__Y_POL
            ELSE
               IUER = -1
               CALL ERR_LOG ( 5006, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported polarization input '//CH_POL//      &
     &                 ' expected: R, L, H, V, X, or Y.' )
	       CALL EXIT ( 1 )
            END IF ! ch_pol
!
! --------- Arg. 7
! --------- Get the ordinate flag
!
            CALL CLRCH  ( CH_FLG4 )
            CALL GETARG ( 7, CH_FLG4 )
	    IF ( CH_FLG4 .NE. '-ord' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5007, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG4//'expected -ord ' )
               CALL EXIT ( 1 )
            END IF ! ch_flg4
!
! --------- Arg. 8
! --------- Get the variable to plot
!
            CALL GETARG ( 8, PLOTORD )
            IF ( PLOTORD == 'tsys' .OR. PLOTORD == 'phas' .OR.          &
     &           PLOTORD == 'ampl'                           ) THEN !
               CONTINUE
            ELSE 
               IUER = -1
               CALL ERR_LOG ( 5008, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported mode '//PLOTORD//'. Expected '//    &
     &                 ' -- tsys, phas or ampl' )
               CALL EXIT ( 1 )
            END IF ! plotord
!
! --------- Arg. 9
! --------- Get the variable flag
!
            CALL CLRCH  ( CH_FLG5 )
            CALL GETARG ( 9, CH_FLG5 )
            IF ( CH_FLG5 .NE. '-var' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5009, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG5//'expected -var ' )
               CALL EXIT ( 1 )
            END IF ! ch_flg5
!
! --------- Arg. 10
! --------- Get the plot type
!
            CALL CLRCH ( PLOTVAR )
            CALL GETARG ( 10, PLOTVAR )
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
                     CALL ERR_LOG ( 5010, IUER, 'BNC_PLOT_TEST',        &
     &                    'When plotting Tsys vs Time, an integer for ' &
     &                    //'time index is expected' )
                     CALL EXIT ( 1 )
                  END IF ! ind_frq
! ------------ 
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5011, IUER, 'BNC_PLOT_TEST',           &
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
! --------------- Set the Time Index to Freq index if plotting
!		  Pcal vs spectrum
!
                  IF ( IND_FRQ .NE. ATP__INIT ) THEN !
                     IND_TIM = IND_FRQ
                  ELSE
                     CALL ERR_LOG ( 5012, IUER, 'BNC_PLOT_TEST',        &
     &                    'When plotting Pcal vs Spectrum, an integer'  &
     &                    //' for time index is expected' )
                     CALL EXIT ( 1 )
                  END IF ! ind_frq
! ------------
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5013, IUER, 'BNC_PLOT_TEST',           &
     &                    'phase and ampl plot unsupported mode '//     &
     &                 PLOTVAR//' -- time, or spectr expected' )
                  CALL EXIT ( 1 )
               END IF ! plotvar (given plotord =/= tsys)
            END IF ! plotord 
!
! --------- Arg. 11
! --------- Get the scan difference flag
!
            CALL CLRCH  ( CH_FLG6 )
  	    CALL GETARG ( 11, CH_FLG6 )
	    IF ( CH_FLG6 .NE. '-del' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5014, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG6//'expected -del ' )
	       CALL EXIT ( 1 )
            END IF ! ch_flg6
!
! --------- Arg. 12
! --------- Get the scan definition
!
            CALL CLRCH  ( CH_TIM_DIF )
            CALL GETARG ( 12, CH_TIM_DIF )
!
! --------- Check if the number has a decimal point?
!     
            READ (UNIT=CH_TIM_DIF, FMT='(F6.2)', IOSTAT=IER) TIM_DIF_MAX
            IF ( IER .NE. 0 ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5015, IUER, 'BNC_PLOT_TEST',              &
     &                 'Expected real number for scan diff. '//         &
     &                 CH_TIM_DIF )
            END IF ! ier
!
! --------- Arg. 13
! --------- Get the plotting flag
!
            CALL CLRCH  ( CH_FLG7 )
  	    CALL GETARG ( 13, CH_FLG7 )
	    IF ( CH_FLG7 .NE. '-plot' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5016, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG7//'expected -plot ' )
            END IF ! ch_flg7
!
! --------- Arg. 14
! --------- The plots to generate and write to file
!
            CALL CLRCH ( CH_PLOT )
            CALL GETARG ( 14, CH_PLOT )
            IF ( CH_PLOT == 'raw' .OR. CH_PLOT == 'ave' .OR.            &
     &           CH_PLOT == 'tot' ) THEN !
               CONTINUE
            ELSE
               CALL ERR_LOG ( 5017, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported plot '//CH_PLOT//' expected: '//    &
     &                 'raw, ave, or all' )
            END IF ! ch_plot
!
! --------- Arg. 15
! --------- Output flag
!
            CALL CLRCH  ( CH_FLG8 )
            CALL GETARG ( 15, CH_FLG8 )
            IF ( CH_FLG8 .NE. '-o' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5016, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG8//'expected -o ' )
               CALL EXIT (1)
            END IF ! ch_flg8
!
! --------- Arg. 16
! --------- Output file
!
	    CALL CLRCH ( FILOUT )
   	    CALL GETARG ( 16, FILOUT )            
!
! ------ If the second flag is the time range, i.e., "-tim_rng"
!
         ELSE
!
! --------- Arg. 4 & Arg. 5
! --------- Get the initial date
!
            CALL CLRCH  ( CH_DATE(1) )
            CALL CLRCH  ( CH_DATE(2) )
            CALL GETARG ( 4, CH_DATE(1) )
            CALL GETARG ( 5, CH_DATE(2) )
!
! --------- Convert date to MJD and UTC
!
            IUER = -1
            CALL DATE_TO_TIME (CH_DATE(1), MJD_RNG(1), UTC_RNG(1), IUER)
            IF ( IUER .NE. 0 ) THEN !
               CALL ERR_LOG ( 5016, IUER, 'BNC_PLOT_TEST',              &
     &                 'Failure converting start date:'//CH_DATE(1)//   &
     &                 ' to MJD and UTC seconds.' )
               CALL EXIT (1)
            END IF ! iuer
! ---------
            IUER = -1
            CALL DATE_TO_TIME (CH_DATE(2), MJD_RNG(2), UTC_RNG(2), IUER)
            IF ( IUER .NE. 0 ) THEN !
               CALL ERR_LOG ( 5017, IUER, 'BNC_PLOT_TEST',              &
     &                 'Failure converting end date:'//CH_DATE(2)//     &
     &                 ' to MJD and UTC seconds.' )
               CALL EXIT (1)
            END IF ! iuer
!
! --------- Arg. 6
! --------- Get the polarization flag
!
            CALL CLRCH  ( CH_FLG3 )
  	    CALL GETARG ( 6, CH_FLG3 )
	    IF ( CH_FLG3 .NE. '-pol' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5018, IUER, 'BNC_PLOT_TEST',                 &
     &                 'Unsupported flag '//CH_FLG3//' expected -pol ' )
	       CALL EXIT ( 1 )
            END IF ! ch_flg3
!
! --------- Arg. 7
! --------- Get the polarization value
!
            CALL CLRCH ( CH_POL )
  	    CALL GETARG ( 7, CH_POL )
            IF ( CH_POL == 'R' ) THEN !
               INPUT_POL = ANC__R_POL
            ELSEIF ( CH_POL == 'L' ) THEN
               INPUT_POL = ANC__L_POL
            ELSEIF ( CH_POL == 'H' ) THEN
               INPUT_POL = ANC__H_POL
            ELSEIF ( CH_POL == 'V' ) THEN
               INPUT_POL = ANC__V_POL
            ELSEIF ( CH_POL == 'X' ) THEN
               INPUT_POL = ANC__X_POL
            ELSEIF ( CH_POL == 'Y' ) THEN
               INPUT_POL = ANC__Y_POL
            ELSE
               IUER = -1
               CALL ERR_LOG ( 5006, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported polarization input '//CH_POL//      &
     &                 ' expected: R, L, H, V, X, or Y.' )
	       CALL EXIT ( 1 )
            END IF ! ch_pol
!
! --------- Arg. 8
! --------- Get the ordinate flag
!
            CALL CLRCH  ( CH_FLG4 )
            CALL GETARG ( 8, CH_FLG4 )
	    IF ( CH_FLG4 .NE. '-ord' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5020, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG4//'expected -ord ' )
               CALL EXIT ( 1 )
            END IF ! ch_flg4
!
! --------- Arg. 9
! --------- Get the variable to plot
!
            CALL GETARG ( 9, PLOTORD )
            IF ( PLOTORD == 'tsys' .OR. PLOTORD == 'phas' .OR.          &
     &           PLOTORD == 'ampl'                           ) THEN !
               CONTINUE
            ELSE
               IUER = -1
               CALL ERR_LOG ( 5021, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported mode '//PLOTORD//'. Expected '//    &
     &                 ' -- tsys, phas or ampl' )
               CALL EXIT ( 1 )
            END IF ! plotord
!
! --------- Arg. 10
! --------- Get the variable flag
!
            CALL CLRCH  ( CH_FLG5 )
            CALL GETARG ( 10, CH_FLG5 )
            IF ( CH_FLG5 .NE. '-var' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5022, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG5//'expected -var ' )
               CALL EXIT ( 1 )
            END IF ! ch_flg5
!
! --------- Arg. 11
! --------- Get the plot type
!
            CALL CLRCH ( PLOTVAR )
            CALL GETARG ( 11, PLOTVAR )
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
                     CALL ERR_LOG ( 5023, IUER, 'BNC_PLOT_TEST',        &
     &                    'When plotting Tsys vs Time, an integer for ' &
     &                    //'time index is expected' )
                     CALL EXIT ( 1 )
                  END IF ! ind_frq
! ------------ 
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5024, IUER, 'BNC_PLOT_TEST',           &
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
                     CALL ERR_LOG ( 5025, IUER, 'BNC_PLOT_TEST',        &
     &                    'When plotting Pcal vs Time, an integer '//   &
     &                    'for time index is expected' )
                     CALL EXIT ( 1 )
                  END IF ! ind_frq
! ------------
               ELSE
                  IUER = -1
                  CALL ERR_LOG ( 5026, IUER, 'BNC_PLOT_TEST',           &
     &                    'phase and ampl plot unsupported mode '//     &
     &                 PLOTVAR//' -- time, or spectr expected')
                  CALL EXIT ( 1 )
               END IF ! plotvar ( given plotord isnt tsys)
            END IF ! plotord
!
! --------- Arg. 12
! --------- Get the scan difference flag
!
            CALL CLRCH  ( CH_FLG6 )
  	    CALL GETARG ( 12, CH_FLG6 )
            IF ( CH_FLG6 .NE. '-del' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5027, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG6//'expected -del ' )
	       CALL EXIT ( 1 )
            END IF ! ch_flg6
!
! --------- Arg. 13
! --------- Get the scan definition
!
            CALL CLRCH  ( CH_TIM_DIF )
            CALL GETARG ( 13, CH_TIM_DIF )
!
! --------- Check if the number has a decimal point?
!     
            READ (UNIT=CH_TIM_DIF, FMT='(F6.2)', IOSTAT=IER) TIM_DIF_MAX
            IF ( IER .NE. 0 ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5028, IUER, 'BNC_PLOT_TEST',              &
     &                 'Expected real number for scan diff. '//         &
     &                 CH_TIM_DIF )
            END IF ! ier
!
! --------- Arg. 14
! --------- Get the plotting flag
!
            CALL CLRCH  ( CH_FLG7 )
  	    CALL GETARG ( 14, CH_FLG7 )
	    IF ( CH_FLG7 .NE. '-plot' ) THEN !
               IUER = -1
               CALL ERR_LOG ( 5029, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG7//'expected -plot ' )
            END IF ! ch_flg7
!
! --------- Arg. 15
! --------- The plots to generate and write to file
!
            CALL CLRCH ( CH_PLOT )
            CALL GETARG ( 15, CH_PLOT )
            IF ( CH_PLOT == 'raw' .OR. CH_PLOT == 'ave' .OR.            &
     &           CH_PLOT == 'tot' ) THEN
               CONTINUE
            ELSE
               CALL ERR_LOG ( 5030, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported plot '//CH_PLOT//' expected: '//    &
     &                 'raw, ave, or all' )
            END IF ! ch_plot
!
! --------- Arg. 16
! --------- Output flag
!
            CALL CLRCH  ( CH_FLG8 )
            CALL GETARG ( 16, CH_FLG8 )
            IF ( CH_FLG8 .NE. '-o' ) THEN
               IUER = -1
               CALL ERR_LOG ( 5031, IUER, 'BNC_PLOT_TEST',              &
     &                 'Unsupported flag '//CH_FLG8//'expected -o ' )
               CALL EXIT (1)
            END IF ! ch_flg8
!
! --------- Arg. 17
! --------- Output file
!
	    CALL CLRCH ( FILOUT )
   	    CALL GETARG ( 17, FILOUT )            
	 END IF ! ch_flg2 (using vs not using -tim_rng)
      END IF
!     
! --- Get the Spline coefficients of the ???effect temperature of the 
!     atmosphere???
!
      CALL GET_SPL_TATM ( ME, LE, ELEV_TATM, VAL_TATM, SPL_TATM )
!
! --- Parse data from binary file to ANC type
!
      IUER = -1
      CALL BNC_PARSE ( FILIN, ANC, IUER )
      IF ( IUER .NE. 0 ) THEN
         IUER = -2
         CALL ERR_LOG ( 5032, IUER, 'BNC_PLOT',                         &
     &           'Failure in parsing binary file data from '//FILIN )
         CALL EXIT ( 1 )
      END IF
!
! --- Get the frequency index closest to the defined frequency
!
      IF ( IND_FRQ == ATP__INIT ) THEN      
!
! ------ If dealing with Tsys
!
         FRQ_ARR = 0.D0
         IF ( PLOTORD == 'tsys' ) THEN
            DO 310 J1 = 1, ANC%NUM_TPS
               FRQ_ARR(J1) = ANC%TPS(J1)%SKY_FRQ
 310        CONTINUE 
!
            IND_FRQ = IXMN8 ( ANC%NUM_TPS, FRQ_ARR, VAL_FRQ )
!
! ------ If dealing with Pcal
!
         ELSE
            DO 311 J1 = 1, ANC%NUM_PCS
               FRQ_ARR(J1) = ANC%PCS(J1)%SKY_FRQ
 311        CONTINUE 
!
            IND_FRQ = IXMN8 ( ANC%NUM_PCS, FRQ_ARR, VAL_FRQ )
         END IF
      END IF
! ---
      IUER = 0
      STR_DO_DATE = MJDSEC_TO_DATE ( ANC%MJD_DOO, ANC%TAI_DOO +         &
     &                               ANC%UTC_MTAI, IUER )
! ---
      WRITE ( 6, * ) '_________________________________________________'
      WRITE ( 6 ,* ) 'Exp_Code: ', ANC%EXP_CODE
      WRITE ( 6 ,* ) 'Sta_Name: ', ANC%STA_NAM
      WRITE ( 6 ,* ) 'Obs_Date: ', STR_DO_DATE
      WRITE ( 6 ,* ) 'Num_tps:  ', ANC%NUM_TPS, 'Num_Tsys: ', ANC%NUM_TSYS
      WRITE ( 6 ,* ) 'Num_pcs:  ', ANC%NUM_PCS, 'Num_Pcal: ', ANC%NUM_PCAL
      WRITE ( 6, * ) '_________________________________________________'
!
! --- PLOT TSYS
!
      IF ( PLOTORD == 'tsys' ) THEN 
!
! ------ Write the Plot title to "TIT"
!
         WRITE ( TIT, 110 ) PLOTORD, IND_FRQ, ANC%TPS(IND_FRQ)%SKY_FRQ, &
     &                      ANC__POL(ANC%TPS(IND_FRQ)%POL),             &
     &                      ANC%TPS(IND_FRQ)%ID
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
! ------------ Convert time to hours since start time
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
! ------------ Populate the plot parameters, and generate plots
!
               WRITE ( 6, '(A)' ) 'Title: '//TRIM(TIT)
               CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', TRIM(TIT) )
!
! ------------ Plot(s) when type is time
!
               IF ( PLOTVAR == 'time' ) THEN
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Time [hrs]' )
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_IBST', 2 )
!
! --------------- Filtered data plots Tsys vs Time @ IF
!
                  CALL DIAGI_1  ( NP, T1, X1, IUER ) 
!
! --------------- Average data plots Tsys_ave vs Time @IF
!
                  CALL DIAGI_1E ( JP, T2, X2, E2, IUER )
!
! ------------ Plot(s) when type is elev
!
               ELSEIF ( PLOTVAR == 'elev' ) THEN
                  CALL DIAGI_SETDEF (IUER, 'DIAGI_UNIT', 'Elev [deg]')
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_IBST', 2 )
!
! --------------- Normalize elevation
!
                  CALL ELEV_NRML ( JP, T3, T22, X2, JN, T4, X4 )
!
! --------------- Filtered data plots Tsys vs Elev
!
                  T11(1:NP) = T11(1:NP)/DEG__TO__RAD 
                  CALL DIAGI_1  ( NP, T11, X1, IUER )
!
! --------------- 
!
                  CALL DIAGI_1E ( JP, T22, X2, E2, IUER )
!
! --------------- 
!
                  CALL DIAGI_1  ( JN, T4, X4, IUER )
! ------------
               ELSEIF ( PLOTVAR == 'azim' ) THEN
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Azim [deg]' )
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_IBST', 2 )
! ---------------
                  T12(1:NP) = T12(1:NP)/DEG__TO__RAD 
                  CALL DIAGI_1  ( NP, T12, X1, IUER )
               END IF
            END IF
         ELSEIF ( PLOTVAR == 'spectr' ) THEN

            IUER = -1
            CALL TSYS_FREQ_FILTER_RAW ( ANC, IND_TIM, NP, F1, X1, IUER )
 
       PRINT *, "%%%%%%%%%%%% BNC_PLOT_TEST - 842 %%%%%%%%"
       PRINT *, "NP = ", NP

       DO J1 = 1, ANC%NUM_TPS
          PRINT *, J1, "FREQ: ", ANC%TPS(J1)%SKY_FRQ
       END DO
           
       IUER = -1
       F1 = F1*1.D-3
       CALL DIAGI_1  ( NP, F1, X1, IUER ) 



         END IF
!
! --- PLOT PHASE
!
      ELSEIF ( PLOTORD == 'phas' .OR. PLOTORD == 'ampl' ) THEN
!$$$$$$$$$$$$$$$$$$$$$$$$$$
!
! ------ Write the Plot title to "TIT"
!
         WRITE ( TIT, 110 ) PLOTORD, IND_FRQ, ANC%PCS(IND_FRQ)%SKY_FRQ,          &
     &                      ANC__POL(ANC%PCS(IND_FRQ)%POL),             &
     &                      ANC%PCS(IND_FRQ)%ID
!
! ------
!
         IF ( PLOTVAR == 'time' ) THEN
!
! --------- Filter for PCal as a function of time.
! --------- The filter also computes the averages and RMS's of each scan
!
            IUER = -1
            CALL PCAL_TIME_FILTER ( ANC, IND_FRQ, TIM_DIF_MAX, NP,      &
     &                              T1, CX1, NS, TIM_AVR, PCAL_AVR,     &
     &                              PCAL_AMP_RMS, PCAL_PHA_RMS,         &
     &                              LP, IND_ARR, IUER )
!
! --------- Proceed only if there are actual points to use
!
            IF ( NP > 0 ) THEN 
!
! ------------ Convert time to hours since start time
!
               DO 412 J1 = 1, LP
                  T1(J1) = (T1(J1) - T1(1))/3600.D0
 412           CONTINUE
!
! ------------ Plot for the phase
!
               IF ( PLOTORD == 'phas' ) THEN
!
! --------------- Filter out any zero values from the average values
!
                  JP = 0
                  DO 422 J2 = 1, NS
!
! ------------------ Is the average Pcal of this scan above minimum Pcal
!
                     IF ( ( ABS(PCAL_AVR(J2)) .GT. ANC__AMP_MIN ) .AND.  &
     &                    ( ABS(PCAL_AVR(J2)) .LT. ANC__AMP_MAX ) ) THEN
! ---------------------
                        JP = JP + 1                                        ! Update counter
! ---------------------
                        PCAL_SCA(JP) = PCAL_AVR(J2)                        ! Filtered Pcal ave.
!
! --------------------- Plotting values
!
                        T2(JP)  = (TIM_AVR(J2) - T1(1) )/3600.D0           ! Time in hrs from initial time
                        T3(JP)  = TIM_AVR(J2)                              ! Time in seconds 
                        CX2(JP) = PCAL_AVR(J2)                             ! Same as PCAL_SCA
                        E2(JP)  = PCAL_PHA_RMS(J2)                         ! Pcal error
                     END IF
 422              CONTINUE
!
! --------------- Populate the plot parameters, and generate plots
!
                  WRITE ( 6, '(A)' ) 'Title: '//TRIM(TIT)
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', TRIM(TIT) )
!
! --------------- Plot(s) when type is time
!
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Time [hrs]' )
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_IBST', 2 )
!
! --------------- Filtered data plots Phas vs Time @ IF
!
                  DO J5 = 1, LP
                     X1(J5) = PHAS_CMPL_R4(CX1(J5))
                  END DO

                  CALL DIAGI_1  ( LP, T1, X1, IUER )
!
! --------------- Average data plots Pcal_ave vs Time @ IF
!
                  DO J5 = 1, JP
                     X2(J5) = PHAS_CMPL_R4(CX2(J5))
                  END DO
                  CALL DIAGI_1E ( JP, T2, X2, E2, IUER )
!
! ------------ Plot for the amplitude
!
               ELSE
!
! --------------- Filter out any zero values from the average values
!
                  JP = 0
                  DO 423 J2 = 1, NS
!
! ------------------ Is average Amplitude of this scan above minimum Pcal
!
                     IF ( ( ABS(PCAL_AVR(J2)) > ANC__AMP_MIN ) .AND.     &
     &                    ( ABS(PCAL_AVR(J2)) < ANC__AMP_MAX )    ) THEN
! ---------------------
                        JP = JP + 1                                        ! Update counter
! ---------------------
                        AMP_SCA(JP) = ABS(PCAL_AVR(J2))                    ! Filtered Pcal ave.
!
! --------------------- Plotting values
!
                        T2(JP)  = (TIM_AVR(J2) - T1(1) )/3600.D0           ! Time in hrs from initial time
                        T3(JP)  = TIM_AVR(J2)                              ! Time in seconds 
                        X2(JP)  = ABS(PCAL_AVR(J2))                             ! Same as AMP_SCA
                        E2(JP)  = PCAL_AMP_RMS(J2)                         ! Pcal error
                     END IF
 423              CONTINUE
!
! --------------- Populate the plot parameters, and generate plots
!
                  WRITE ( 6, '(A)' ) 'Title: '//TRIM(TIT)
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', TRIM(TIT) )
!
! --------------- Plot(s) when type is time
!
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Time [hrs]' )
                  CALL DIAGI_SETDEF ( IUER, 'DIAGI_IBST', 2 )
!
! --------------- Filtered data plots Ampl vs Time @ IF
!
                  DO J5 = 1, LP
                     X1(J5) = ABS(CX1(J5))
                  END DO
                  CALL DIAGI_1  ( LP, T1, X1, IUER )
!
! --------------- Average data plots Amp_ave vs Time @ IF
!
                  CALL DIAGI_1E ( JP, T2, X2, E2, IUER )
               END IF
            END IF




!
! ------ Plot(s) when type is Spectr
!
         ELSEIF ( PLOTVAR == 'spectr' ) THEN

           WRITE (6,*) "DON'T RUN THIS JUST YET. "
           WRITE (6,*) "NEED TO FIX AN EARIER BUG"

         END IF

!$$$$$$$$$$$$$$$$$$$$$$$$$$
      
      END IF 
!
! --- Printing formats
!
 110  FORMAT ( 'PLOT= ',A5 ,' FRQ_IND= ',I4,' FRQ= ',F8.1,                &
     &         ' POL= ', A1,' ID= ', A4 )
!


 104  FORMAT("TP_SENSOR:  ", I4, 3X, F8.2, 3X, A1, 3X, A4 )
 105  FORMAT("TSYS: ", I4, 1X, I4, 3X, A22, 3X, F7.1, 3X, F10.4, 2X, F8.4 )

      END PROGRAM
