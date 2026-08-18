      SUBROUTINE THIRD ( INUMBERC )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!     Do the atmosphere and clock plots.
!
!     Parameter file
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
!     INPUT Variables:
      INTEGER*2 INUMBERC(*)
!     INUMBERC - Array of selected sites for plotting
!
!     COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'mdlcm.i'
!
!     SUBROUTINE INTERFACE
!
!      Calling subroutine:mdlpl
!      Called subroutines: off_alpha_mode,on_alpha_mode,screen__pausse,
!                          on_graphics_mode,of_graphics_mode,fplot
!
!     LOCAL VARIABLES
      INTEGER*2 ISTA,IROW,I,iyr,imon,iday,ihour,iminute, &
     &MAX_VAL, MIN_VAL, &
     &IY,CLOCK_COUNT,len, dry_count, tot_count
      Integer*2 ierr, system2_sh, iaccros, idown,temp_count, hum_count
      integer*2 press_count, j, ilen, trimlen
      INTEGER*4 JOS, IOS, FPLOT,IERR4, ICHR
      character*4 cchar
      REAL*8 MIN_VAL_CM,MAX_VAL_CM,VAL_MIN,VAL_MAX,CLOCK_RMS,XSTART, &
     &       YSTART, NUM_OF_ATMS, total_zenith, water_partial_pres, &
     &       max_water_partial_pres
      LOGICAL*2 GET_FIRST,OK,KBIT,plot_to_do,do_plot_eop, do_header
      logical*2 found, plot_model
      CHARACTER*100 STRING,errstr
      character*1 one_char
      CHARACTER*80  EO_PLOT_TITLE,bufstr,fname, buffer, window_buffer
      CHARACTER*48  EO_PLOT_APPEND(8)
      CHARACTER*30  control_file_name,data_file_name
      CHARACTER*4 HOLD
      CHARACTER*3 HOLD_EO(3)
      CHARACTER*8 EO_LABEL(3)
      INTEGER*2 IEOP,IEOP_TYPE
      integer*2 ibuf(40),numdd,ldbnam(5,15),idbver(15)
      CHARACTER LETTERS*2, ASIM*1
      INTEGER*4 ISIM
!
      real*8 clock_bounds    (4), atm_bounds    (4),  dry_bounds(4)
      real*8 clock_web_bounds(4), atm_web_bounds(4)
      real*8 clock_std_bounds(4), atm_std_bounds(4)
      real*8 tot_bounds      (4), press_bounds  (4), temp_bounds(4)
      real*8 humid_bounds    (4)
      real*8 rate,fraction
      real*8 over_offset, up_offset
!
      character*300 big_buffer
      real*8 clock_offset(max_pwc_eps,max_arc_sta), &
     &       atmos_offset(max_pwc_eps,max_arc_sta), &
     &       clock_offsig(max_pwc_eps,max_arc_sta), &
     &       atmos_offsig(max_pwc_eps,max_arc_sta), &
     &       jdate_clock (max_pwc_eps), &
     &       jdate_atmos (max_pwc_eps)
      integer*4 jclock, jatmos,ipoint
!
      real*8 character_size
      character*80 cbuf
      equivalence (ibuf(1),cbuf)
      equivalence (ichr,cchar)
!
      character*10 ldbnam_c(15)
      equivalence (ldbnam,ldbnam_c)
      real*8 xrms_sig, yrms_sig, urms_sig
!
      data   tot_bounds /.55, .90, .29, .48/
      data   dry_bounds /.55, .90, .04, .24/
      data clock_std_bounds / .55, .90, .76, .96/
      data   atm_std_bounds / .55, .90, .53, .72/
      data clock_web_bounds / .15, .90, .50, .85/
      data   atm_web_bounds / .15, .90, .10, .45/
!
      data  temp_bounds /.05, .40, .53, .72/
      data humid_bounds /.05, .40, .29, .48/
      data press_bounds /.05, .40, .04, .24/
!
!     data iaccros /1200/
      data iaccros /1000/
!     data idown   / 900/
      data idown   / 700/
!
      DATA HOLD_EO /'xwb','ywb','ut1'/
      DATA EO_LABEL /'X-Pole: ','Y-Pole: ','UT1   : '/
      Data control_file_name /'                              '/
      Data    data_file_name /'                              '/
      DATA &
     &   EO_PLOT_APPEND/'                                                ', &
     &    'with Constrained Rate Breaks                    ', &
     &    'with Diurnal Sine                               ', &
     &    'with Constrained Rate Breaks and Diurnal Sine   ', &
     &    'with Semi-diurnal Sine                          ', &
     &    'with Constrained Rate Breaks & Semi-diurnal sine', &
     &    'with Diurnal and Semi-diurnal Sines             ', &
     &    'with Constrained Rates and Semi- & Diurnal Sines'/
      INTEGER*2  INT2_ARG
!
! 4.  HISTORY
!  WHO  WHEN     WHAT
!  JWR  890228   Average clock offset computation added.
!  KDB  910101   Eop code added.
!  JWR  910214   Changed to put clock and atmosphere plots on the
!                together.
!  AEE  910515   Enhanced error messages written to the error file.
!  jwr  910603   Buq in namely atm line file fixed.
!  jwr  910724   HPGL graphics replaced with pg-plot calls
!  jwr  920828   Major and minor ticks changed to handle pathological cases.
!  jwr  940620   Code added for dry zen, tot zen, press, temp and hum.
!  jwr  940629   Batch mode support added.
!  jwr  940717   Water vapor partial pressure added.
!  jwr  941214   Clock plotting logic improved so that at the epoch of
!                a clock break a vertical bar is drawn.
!  jwr  950324   Small improvement in questioning the user.
!  :95.05.11:jwr: Code added to plot John Gipson's hf eop model along with the
!                hf eop estimates. Also mean sigma computed for eop estimates
!                and the db name put at the top of the eop plot.
!  :95.05.19:jwr:User supplied eop scaling.
!  :95.07.17:jwr:Better documentation of hfeop status in eop plots.
!                Pole units changed to microarcseconds.
!  :95.08.09:kdb: Ability to deal with high frequency eop models other than hf926.
!   kdb  951207  Integer*4 number of observations.
!   pet  970809  Added error message if pc8 failed to be called. Added end_mn
!                just before the call of pc8. The lack of it sometimes hanged
!                terminal input.
!   pet  970903  Got rid from curses before launching pc8 for printing clock and
!                atmosphere plots. This allowed to look at error message when
!                pc8 failed. Changed a bit prompt to look at the next plot.
!   pet  971104  Changed format fro reading CLOCxx and ATMOxx files in according
!                with changes in /adjst/a1jst.f
!   pet  971105  Got rid from curses before launching pc8 for printing EOP
!                plots.
!   jwr  980212  It has been modified to use 'do_web_plots' to suppress the
!                creation of the plots not used on the Web and to change the
!                'view' command so to the two plots for the Web fill the
!                screen.  Also third.f has been modified so that when a clock
!                reference site is encountered then in the place where a clock
!                plot would normally appear the string 'Clock reference site'
!                will appear.
!   pet  980614  Corrected coding error in calculation total zenith content
!   pvt  981016  Corrected a bug: previous version considered all stations
!                as reference station if printing autoconstrainted clock was
!                turned off.
!   pet  981224  Corrected a bug: prevous verion died in tortues when no
!                zenith path delay was used as a priori
!   pet  990409  Replaced call of system with call of system2_sh in order to
!                fix SYSTEM-APR99 bug
!   pvt  990705  Removed definitions TRIMLEN, IDAY, IMON since they are already
!                defined
!   pet  990706  Removed unused variables
!   pet  990709  Replaced /X with /XW for post JUL99 version of pc8
!   pet  990921  Corrected a bug: support of envirnoment variable SOLVE_DIR
!                was erroneous
!   jwr  040531  Sleeping bug dealing with writing integer variable with a
!                f-fromat fixed in numerous places.
!
! 5.  THIRD PROGRAM STRUCTURE
!
!CCCC
!
! --- Set the clock and atmosphere plot area bounds depending on whether the plots
!     are for the web or not.
!
      IF ( DO_WEB_PLOTS ) THEN
           DO I=1,4
              CLOCK_BOUNDS(I) = CLOCK_WEB_BOUNDS(I)
              ATM_BOUNDS  (I) = ATM_WEB_BOUNDS  (I)
           ENDDO
           CHARACTER_SIZE = 1.0
        ELSE
           DO I=1,4
              CLOCK_BOUNDS(I) = CLOCK_STD_BOUNDS(I)
              ATM_BOUNDS  (I) = ATM_STD_BOUNDS  (I)
           ENDDO
           CHARACTER_SIZE = 0.5
      ENDIF
!
! --- Read in the clock and atmos information from ADJST.
!
      FNAME=PRE_SCR_DIR(:PRE_SD_LEN)//'CLOC'//PRE_LETRS
      OPEN ( 35, FILE=FNAME, IOSTAT=IOS, STATUS='OLD' )
      CALL FERR ( INT2(IOS), "Opening CLOCK segment display buffer", &
     &            INT2(0), INT2(0) )
      IF ( IOS .NE. 0 ) THEN
           STOP 'MDLPL/THIRD: No clock file.'
      ENDIF
      IOS = 0
      JCLOCK = 0
!
      DO I = 1,3
         READ ( 35, '(A)', IOSTAT=IOS ) BIG_BUFFER
      ENDDO
!
      DO WHILE ( IOS .EQ. 0 )
         READ(35,'(A)',IOSTAT=IOS) BIG_BUFFER
         IF ( IOS .EQ. 0 ) THEN
              JCLOCK = JCLOCK+1
              READ ( BIG_BUFFER, '(15X,F15.5,32(I12,I6))', IOSTAT=IOS ) &
     &               JDATE_CLOCK (JCLOCK), &
     &             ( CLOCK_OFFSET(JCLOCK,J), CLOCK_OFFSIG(JCLOCK,J), J=1,NUMSTA )
              IF ( IOS .NE. 0 ) THEN
                   CALL FERR ( INT2(2834), 'MDLPL/THIRD.F Error reading '// &
     &                 'internal buffer', INT2(0), INT2(0) )
              ENDIF
         ENDIF
      ENDDO
      CLOSE ( 35 )
!
      FNAME=PRE_SCR_DIR(:PRE_SD_LEN)//'ATMO'//PRE_LETRS
      OPEN ( 35, FILE=FNAME, IOSTAT=IOS, STATUS='OLD' )
      IF ( IOS .NE. 0 ) STOP 'MDLPL/THIRD: No atmosphere file.'
      IOS = 0
      JATMOS = 0
!
! --- Skip first three lines
!
      DO I = 1,3
         READ(35,'(A)',IOSTAT=IOS) BIG_BUFFER
      ENDDO
!
      DO WHILE ( IOS .EQ. 0 )
         READ ( 35, '(A300)', IOSTAT=IOS ) BIG_BUFFER
         IF ( IOS .EQ. 0 ) THEN
              JATMOS = JATMOS+1
              READ ( BIG_BUFFER, '(15X,F15.5,32(I12,I6))' ) &
     &            JDATE_ATMOS(JATMOS),( ATMOS_OFFSET(JATMOS,J), ATMOS_OFFSIG(JATMOS,J), J=1,NUMSTA )
         ENDIF
      ENDDO
      CLOSE ( 35 )
!
      CALL HARD_RESET()
!
      CALL ACS_OBSFIL ( 'C' )
!
      CALL DBPOX ( NUMDD, LDBNAM, IDBVER, IDBEND )
      LETTERS = PRE_LETRS
      CALL LCASEFOLD ( LETTERS )
!
      DO ISTA = 1,NUMSTA     !Run over the sites
!
! ------ Open the plot control file.
!
        WRITE ( HOLD, '(2A2)' ) (ISITN(I,ISTA),I=1,2)
        IF ( SAVE_PLOT_FILES ) THEN
             CONTROL_FILE_NAME = SCRATCH_DIR//'mdlplc_'//HOLD//PRE_LETRS
          ELSE
             CONTROL_FILE_NAME =SCRATCH_DIR//'mdlplc_'//PRE_LETRS
        ENDIF
!
        OPEN ( 25, FILE=CONTROL_FILE_NAME, IOSTAT=IOS )
        IF ( IOS .GT. 0 ) THEN
             WRITE ( ERRSTR, &
     &               '("Error ",I5," in opening plot control file")')IOS
             CALL FERR ( INT2(251), ERRSTR, INT2(0), INT2(0) )
             STOP
        ENDIF
        WRITE(25,'("# plot control file for solve/mdlpl")')
        WRITE(25,'(" begin ",2i6)') iaccros, idown
        write(25,'(" charsz 1.5")')
        IF ( DO_WEB_PLOTS ) THEN
             WRITE ( 25,'(" label .15 .95 1 0 MDLPL for ",a8)') ISITN_CHR(ISTA)
           ELSE
             WRITE ( 25,'(" label .07 .85 1 0 MDLPL for ",a8)') ISITN_CHR(ISTA)
        ENDIF
!
        CALL DBPOX ( NUMDD, LDBNAM, IDBVER, IDBEND )
        FOUND = .FALSE.
        j = 1
        do while (.not.found.and.j.le.15)
          if (kbit(idbsel,j)) then
            found=.TRUE.
          else
            j = j+1
          endif
        enddo
        if (j.gt.15) then
          call ferr( INT2(252), 'No database selected', INT2(0), INT2(0) )
        endif
        If(do_web_plots) then
          write(25,'(" label .55 .95 1 0 ",5A2)')(ldbnam(i,j),i=1,5)
        else
          write(25,'(" label .12 .80 1 0 ",5A2)')(ldbnam(i,j),i=1,5)
        endif
!
        plot_to_do = .false.
        IF ( ATMOS_CONTROL(ISTA) ) THEN ! This site selected.
!
          PLOT_TO_DO = .TRUE.
          DO_HEADER  = .TRUE.
!
! ------- Initiize max and min y-axis scaling
!
          MIN_VAL =  10000
          MAX_VAL = -10000
          GET_FIRST = .TRUE.
          max_water_partial_pres = 0.d0
!
! ------- Open the plot data file.
!
          if(batch_control) then
            ilen = trimlen(run_id)
            open(26, &
     &      file= &
     &      scratch_dir//'atmd_'//hold//"_"//run_id(1:ilen)// &
     &      "_"//pre_letrs, &
     &      status='unknown',iostat=ios)
            if(ios.gt.0) then
              write(errstr,'( &
     &        "mdlpl/third: Error ",I5," in opening plot data file")') &
     &        IOS
              call ferr( INT2(253), errstr, INT2(0), INT2(0) )
            endif
!
!           In case the file already contains data, read to an end-of-file
            ios = 0
            do while(ios.eq.0)
              read(26,'(a1)',iostat=ios) one_char
              if(ios.eq.0) do_header = .false.
            enddo
!
          else
            IF(SAVE_PLOT_fileS) &
     &         THEN
              OPEN(26,file='/tmp/atmd'//HOLD//PRE_LETRS,IOSTAT=IOS)
              OPEN(36,file='/tmp/metd'//HOLD//PRE_LETRS,IOSTAT=JOS)
            ELSE
              OPEN(26,file='/tmp/atmd'//PRE_LETRS,IOSTAT=IOS)
              OPEN(36,file='/tmp/metd'//PRE_LETRS,IOSTAT=JOS)
            ENDIF
            IF(IOS .GT. 0 .or. JOS .GT. 0) THEN
              WRITE(errstr,'( &
     &        "Error ",I5," in opening plot data files")')IOS
              call ferr( INT2(254), errstr, INT2(0), INT2(0) )
              STOP "mdlpl/third: Cannot open atm data plot files."
            endif
          ENDIF
!
! ------- Write the plot title record to data file.
!
          if(do_header) then
            WRITE(26,'( &
     &      "Auto-constrained zenith path delay estimate for ", &
     &      4A2)') (ISITN(I,ISTA),I=1,4)
!
            write(26,'( &
     &"  modified-jd   yr   mn   dy   mn   hr  atm zen    atm zen",/, &
     &"                                         (ps)        (ps) ")')
!
            WRITE(36,'( &
     &      "Auto-constrained zenith path delay estimate for ", &
     &      4A2)') (ISITN(I,ISTA),I=1,4)
!
            write(36,'( &
     &"  modified-jd   yr   mn   dy   mn   hr  atm zen    atm zen", &
     &"     num.  press.  temp  rel.  Sass.     Sass.  ",/, &
     &"                                        adjust.     sigma ", &
     &"     atms.               hum.  dry zen   wet zen",/, &
     &"                                         (ps)        (ps) ", &
     &"           (mbars)  (K)   (%)   (ps)       (ps) ")')
          endif
!
!
          DO IROW = 1, JATMOS  ! Run over the table of segment atm values
!
! ---------- Get 1st time tag for scaling
! ---------- Handle y-component scaling, both ps and cm scales
!
             IF ( MAX_VAL .LT. ( ATMOS_OFFSET(IROW,ISTA)  + &
     &                           ATMOS_OFFSIG(IROW,ISTA))   ) THEN
                  MAX_VAL = ATMOS_OFFSET(IROW,ISTA) + ATMOS_OFFSIG(IROW,ISTA)
             ENDIF
!
             IF ( MIN_VAL .GT. ( ATMOS_OFFSET(IROW,ISTA) - &
     &                           ATMOS_OFFSIG(IROW,ISTA))   ) THEN
                  MIN_VAL = ATMOS_OFFSET(IROW,ISTA) - ATMOS_OFFSIG(IROW,ISTA)
             ENDIF
!
             CALL EPOC ( IMON, IDAY, IYR, IHOUR, IMINUTE, JDATE_ATMOS(IROW) )
             WRITE ( 26, '(f13.3, 5I5, 3F10.3, f7.1, f6.1 )') &
     &               JDATE_ATMOS(IROW), IYR, IMON, IDAY, IHOUR, IMINUTE, &
     &               ATMOS_OFFSET(IROW,ISTA), ATMOS_OFFSIG(IROW,ISTA)
          ENDDO
!
          IPOINT = 1
          DO IROW = 1, ICOUNT  ! Run over the table
!
! ---------- Get 1st time tag for scaling
!
             IF ( SITE_ID(IROW) .EQ. ISTA ) THEN
!
! --------------- Handle y-component scaling, both ps and cm scales
!
                  IF ( MAX_VAL .LT. SASZWET(IROW) ) THEN
                       MAX_VAL = SASZWET(IROW) + 10.D0
                  END IF
!
                  IF ( MIN_VAL .GT. SASZWET(IROW) ) THEN
                       MIN_VAL = SASZWET(IROW) - 10.D0
                  END IF
!
! --------------- Interpolate the atm offset from use with the total plot.
!
                  IF ( IPOINT .LT. JATMOS-1 ) THEN
                       DO WHILE ( REAL_DATE(IROW)+2400000.5D0 .GT. &
     &                            JDATE_ATMOS(IPOINT+1)            )
                          IPOINT = IPOINT+1
                       ENDDO
                  END IF
!
                  RATE = ATMOS_OFFSET(IPOINT+1,ISTA) - ATMOS_OFFSET(IPOINT,ISTA)
!
!
                  FRACTION = (REAL_DATE(IROW)+2400000.5D0- &
     &                       JDATE_ATMOS(IPOINT))/(JDATE_ATMOS(IPOINT+1)-JDATE_ATMOS(IPOINT))
!
                  ATM_VAL(IROW) = ATMOS_OFFSET(IPOINT,ISTA) + RATE*FRACTION
                  ATM_SIG(IROW) = 0.D0
!
! --------------- Write value and sigma to plot data file.
!
                  NUM_OF_ATMS = 1.D0/SIN(ELEVATION(IROW) )
                  IF (NUM_OF_ATMS .LT. 0.0 ) NUM_OF_ATMS = 0.0
                  IF (NUM_OF_ATMS .GT. 20. ) NUM_OF_ATMS = 20.
!
! --------------- Compute the partial pressure of water vapor
! --------------- Formula copied from the documentation for the
! --------------- gps reduction package OMNI (ch. 6 - database computations)
! --------------- units: water vapor pressure in millibars.
! ---------------        humidity in %
! ---------------        temperature Centigrade
!
                  WATER_PARTIAL_PRES = &
     &             (HUMID_H(IROW)/100.)*6.11*(  10**(7.5*TEMPC_H(IROW)/(TEMPC_H(IROW)+237.1))  )
!
                  WRITE ( 36, '(f13.6,5I5,3F10.3,f7.1,f6.1,f6.1,3f10.3)') &
     &                  REAL_DATE(IROW), (TIME_TAG(I,IROW),I=1,5), 0.D0, 0.D0, &
     &                  NUM_OF_ATMS, &
     &                  PRESS_H(IROW), TEMPC_H(IROW), HUMID_H(IROW), &
     &                  SASZDRY(IROW), SASZWET(IROW), WATER_PARTIAL_PRES
             ENDIF
          ENDDO
          CLOSE(26)
          CLOSE(36)
!
! ------- Now make the plot control file.
! ------- Write header card in the data file.
!
          WRITE(25,'("# plot control file for solve/mdlpl")')
          WRITE(25,'(" charsz ",f5.2)') character_size
          WRITE(25,'(" headers 4       ")')
!
! ------- Write view card to set up the screen boundaries.
!
          WRITE(25,'(" view ",4f5.2)') atm_bounds
!
! ------- Write point card for diamond shaped data points
!
          WRITE(25,'(" point 11  ")')
!
! ------- Write line card to connect the points
!
          WRITE(25,'(" line 1    ")')
!
! ------- Major tick interval
!
          write(25,'("#set the major tick interval to 1/8 day")')
          write(25,'(" x_mjtiv 0.125")')
!
! ------- Minor tick interval
!
          write(25,'("#set the subtick interval to 1 hour")')
          write(25,'(" x_sbtiv 3")')
!
!         Write the x-field card for ymdhm data
          WRITE(25,'(" x_field 0 2 6  ")')
!
!         Write the y-field card for values and sigma in columns 6 and 7
          WRITE(25,'(" y_field 1 7 8 Zenith Path Delay (ps) ")')
!
!         Write data file name card
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/atmd",A4,A2)') HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/atmd",A2)') PRE_LETRS
          ENDIF
!
!         Set the error bars for no bars at the end of the lines.
          write(25,'(" ebz 0 ")')
!
!         Set up the axes
          write(25,'("#show numbers on both axes")')
          if(do_web_plots) then
            write(25,'(" axes BCNST BCNST ")')
          else
            write(25,'(" axes BCST BCNST ")')
          endif
!
!         Write the window card. Round Y scaling up and down 10 ps.
!         Also compute the alternate cm scale for the right side of plot
!
          IF(DEFAULT_A_SCALE) THEN
            MIN_VAL    = (MIN_VAL/10)*10-10
            MAX_VAL    = (MAX_VAL/10)*10+20
          ELSE
            IF(SET_SCALE_INT_A) &
     &         THEN
              OK = .FALSE.
              DO WHILE(.not.OK)
                CALL CLEAR_MN()
                WRITE(bufstr,'( &
     &          "Atmosphere scale for ",4A2," in psec:")') &
     &            (ISITN(I,ISTA),I=1,4)
                    call addstr_f(bufstr )
                    call nl_mn()
                call addstr_f(" Min,Max (-2000ps to 2000ps valid) ? " )
                    call getstr_f(bufstr )
                READ(bufstr,*,IOSTAT=ios) MIN_VAL,MAX_VAL
                    call ferr( INT2(ios), "Invalid atm scale", INT2(0), &
     &                   INT2(0) )
!               Check for reasonable values
                IF( MIN_VAL   .lt. MAX_VAL   .and. &
     &             MIN_VAL   .gt. -2010.    .and. &
     &             MAX_VAL   .lt.  2010. )  OK = .TRUE.
              ENDDO
            ELSE
              MIN_VAL    = ATMOS_MIN
              MAX_VAL    = ATMOS_MAX
            ENDIF
          ENDIF
!
!@  call un_curses ()
!@  write ( 6, * ) FIRST_TIME, LAST_TIME, MIN_VAL, MAX_VAL ! %%%%%%%%%
          MIN_VAL_CM = (MIN_VAL/1.D12)*VLIGHT*100.
          MAX_VAL_CM = (MAX_VAL/1.D12)*VLIGHT*100.
          WRITE ( 25, '(" window ", 10I4, 2F10.3, 5X)' ) &
     &            FIRST_TIME, LAST_TIME, MIN_VAL*1.D0, MAX_VAL*1.D0
!
!         Write the read card.
          write(25,'(" read                               ")')
!
!         Put out the label over the plot
!
          IF ( DO_WEB_PLOTS )THEN
               WRITE ( 25, '(" label ",2F5.2," 1 0 \h1              ")' ) &
     &                      ATM_BOUNDS(1) + 0.07, ATM_BOUNDS(4)+0.01
             ELSE
               WRITE ( 25, '(" label ",2F5.2," 1 0 \h1              ")' ) &
     &                      ATM_BOUNDS(1), ATM_BOUNDS(4)+0.01
          ENDIF
!
!         Write the DRAW card
          WRITE(25,'(" draw                               ")')
!
!         Plot the Sass. wet zenith based on the met data.
!
!         Do the water vapor partial pressure plots
          write(25,'(" headers 4                 ")')
          WRITE(25,'(" view ",4f5.2)') atm_bounds
          write(25,'(" point 1                   ")')
          write(25,'(" line 1                    ")')
          write(25,'(" x_field 0 2 6             ")')
          write(25,'(" y_field 1 14 0 ")')
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/metd",A4,A2)') HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/metd",A2)') PRE_LETRS
          ENDIF
          write(25,'(" ebz 0                      ")')
          write(25,'(" axes A A                   ")')
          WRITE ( 25, '( " window ", 10I4, 2I10, 5X)' ) &
     &            FIRST_TIME, LAST_TIME, MIN_VAL, MAX_VAL
          write(25,'(" read                       ")')
          write(25,'(" draw                       ")')
          IF ( DO_WEB_PLOTS ) THEN
               WRITE ( 25, '(" label",2f5.2," 0 1 (.)Wet Sass. from Met.  ")' ) &
     &               ( ATM_BOUNDS(1) - 0.07 ), ( ATM_BOUNDS(3)+.026 )
             ELSE
               WRITE ( 25, '(" label",2f5.2," 0 1 (.)Wet Sass. from Met.  ")') &
     &               ( ATM_BOUNDS(1) - 0.04), ( ATM_BOUNDS(3) + 0.021)
          ENDIF
!
!         Handle plotting the numbers of atmospheres for each point
          write(25,'("#plot the number of atmospheres for each obs")')
          WRITE(25,'(" view ",4f5.2)') atm_bounds
          write(25,'(" headers 4                                  ")')
          write(25,'(" y_field 1 9 0                              ")')
          write(25,'(" x_field 0 2 6                              ")')
          write(25,'(" point 2                                    ")')
          write(25,'(" line 0                                     ")')
          write(25,'(" axes BC BCM ")')
          write(25,'(" window ",10i5," 0 20 ")') first_time, last_time
          if(do_web_plots) then
            write(25, &
     &              '(" label",2f5.2," 0 1 (+)Number of Atmospheres ")')(atm_bounds(2)+.05), (atm_bounds(3)+.02)
          else
            write(25, &
     &              '(" label",2f5.2," 0 1 (+)Number of Atmospheres ")')(atm_bounds(2)+.03), (atm_bounds(3)+.02)
          endif
          write(25,'(" read                                       ")')
          write(25,'(" draw                                       ")')
!
!         Plot the lines for the 5, 10, and 15 atmospheres
          WRITE(25,'(" view ",4f5.2)') atm_bounds
          write(25,'(" headers 0                                  ")')
          write(25,'(" y_field 1 6 0                              ")')
          write(25,'(" x_field 0 1 5                              ")')
          write(25,'(" window ",10i5," 0 20 ")') first_time, last_time
          write(25,'(" point 1                                    ")')
          write(25,'(" line 2                                     ")')
!
!         Handle the lines at 5, 10, and 15 atmospheres
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(HOLD,'(2A2)') (ISITN(I,ISTA),I=1,2)
            OPEN(27,file='/tmp/atml'//HOLD//PRE_LETRS,iostat=ios)
          ELSE
            OPEN(27,file='/tmp/atml'//PRE_LETRS,IOSTAT=IOS)
          ENDIF
          IF(IOS .GT. 0) THEN
            WRITE(errstr,'("Error ",I5," in numbers of atms data", &
     &      " file")') IOS
            call ferr( INT2(255), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
          WRITE(27,'(5I5," 5. 10. 15. ")') FIRST_TIME
          WRITE(27,'(5I5," 5. 10. 15. ")') LAST_TIME
          CLOSE(27)
!
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/atml",A4,A2,"  ")') HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/atml",A2,"     ")') PRE_LETRS
          ENDIF
!
          WRITE(25,'(" read ")')
          WRITE(25,'(" draw ")')
!
          WRITE(25,'(" view ",4f5.2)') atm_bounds
          WRITE(25,'(" window ",10i5," 0 20 ")')first_time, last_time
          WRITE(25,'(" headers 0                                   ")')
          WRITE(25,'(" y_field 1 7 0                               ")')
          WRITE(25,'(" x_field 0 1 5                               ")')
          WRITE(25,'(" point 1                                     ")')
          WRITE(25,'(" line 2                                      ")')
          WRITE(25,'(" read                                        ")')
          WRITE(25,'(" draw                                        ")')
          WRITE(25,'(" view ",4f5.2)') atm_bounds
          WRITE(25,'(" window ",10i5," 0 20 ")')first_time,last_time
          WRITE(25,'(" header 0                                    ")')
          WRITE(25,'(" y_field 1 8 0                               ")')
          WRITE(25,'(" x_field 0 1 5                               ")')
          WRITE(25,'(" point 1                                     ")')
          WRITE(25,'(" line 2                                      ")')
          WRITE(25,'(" read                                        ")')
          WRITE(25,'(" draw                                        ")')
        Endif
!
!       Do the clock plots
!
!       If this is the clock reference site skip out.
!       If this site not a selected site skip out.
        IF(INUMBERC(ISTA).GT.0  .AND. &
     &     CLOCK_CONTROL(ISTA)) THEN
!
!
          plot_to_do = .true.
!         Initil max and min y-axis scaling and clock rms statistics.
          MIN_VAL =  10000
          MAX_VAL = -10000
          GET_FIRST = .TRUE.
          CLOCK_COUNT = 0
          CLOCK_RMS = 0.
!
!         Open the plot data file.
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(HOLD,'(2A2)') (ISITN(I,ISTA),I=1,2)
            OPEN(26,file='/tmp/clkd'//HOLD//PRE_LETRS,IOSTAT=IOS)
          ELSE
            OPEN(26,file='/tmp/clkd'//PRE_LETRS,IOSTAT=IOS)
          ENDIF
          IF(IOS .GT. 0) THEN
            WRITE(errstr, &
     &                   '("Error ",I5," in opening plot data file")')IOS
            call ferr( INT2(256), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
!
!         Write the plot title record to data file.
          WRITE(26,'("Auto-constrained clock delay estimate for ", &
     &    4A2)') (ISITN(I,ISTA),I=1,4)
!
          DO IROW = 1, jclock  !Run over the table
!           Get 1st time tag for scaling
!           Handle y-component scaling
            IF(MAX_VAL .LT. (clock_offset(irow,ista)+clock_offsig(irow, &
     &          ista)))MAX_VAL =    clock_offset(irow,ista)+clock_offsig(irow,ista)
!
            IF(MIN_VAL .GT. (clock_offset(irow,ista)-clock_offsig(irow, &
     &          ista)))MIN_VAL =    clock_offset(irow,ista)-clock_offsig(irow,ista)
!
            CLOCK_RMS=CLOCK_RMS + clock_offset(irow,ista)**2
            CLOCK_COUNT=CLOCK_COUNT+1
!
!           Write value and sigma to plot data file.
            call epoc(imon,iday,iyr,ihour,iminute,jdate_clock(irow) )
            WRITE(26,'(5I5,1X,2F12.3)') iyr,imon,iday,ihour,iminute, &
     &      clock_offset(irow,ista),clock_offsig(irow,ista)
          ENDDO
          CLOSE(26)
!
!         Complete the computation of the clock rms offset
          CLOCK_RMS=SQRT(CLOCK_RMS/CLOCK_COUNT)
!
!         Now make the plot control file.
!         Write header card in the data file.
          WRITE(25,'("# Do the clock plots                     ")')
          WRITE(25,'(" headers 1                               ")')
!
!         Write view card to set up the screen boundaries.
          WRITE(25,'(" view ",4f5.2)') clock_bounds
!
!         Write point card for diamond shaped data points
          WRITE(25,'(" point 11                                ")')
!
!         Write line card to connect the points
          WRITE(25,'(" line 1                                  ")')
!
!         Write the x-field card for ymdhm data
          WRITE(25,'(" x_field 0 1 5                           ")')
!
!         Write the y-field card for values and sigma in columns 6 and 7
          WRITE(25,'(" y_field 1 6 7 Clock delay (ps)          ")')
!
!         Write data file name card
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/clkd",A4,A2)')HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/clkd",A2)') PRE_LETRS
          ENDIF
!
!         Set the error bars to not cross the Ts.
          write(25,'(" ebz 0 ")')
!
!         Set up the axes and labeling
          write(25,'(" axes BCST BCNST ")')
!         Write the window card. Round y scaling up and down 10 ps.
          IF(DEFAULT_C_SCALE) THEN
            MIN_VAL = (MIN_VAL/10)*10-10
            MAX_VAL = (MAX_VAL/10)*10+20
          ELSE
            IF(SET_SCALE_INT_C) &
     &         THEN
              OK = .FALSE.
              DO WHILE(.not.OK)
                CALL CLEAR_MN()
                WRITE(bufstr,'( &
     &       "Clock scale for ",4A2," in psec:")')(ISITN(I,ISTA),I=1,4)
                    call nl_mn()
                call addstr_f(" Min,Max ? " )
                    call getstr_f(bufstr )
                READ(bufstr,*,IOSTAT=ios) MIN_VAL,MAX_VAL
                    call ferr( INT2(ios), "Invalid clock scale", INT2(0), &
     &                   INT2(0) )
!               Check for reasonable values
                IF( MIN_VAL   .lt. MAX_VAL   .and. &
     &             MIN_VAL   .gt. -2010.    .and. &
     &             MAX_VAL   .lt.  2010. )  OK = .TRUE.
              ENDDO
            ELSE
              MIN_VAL = CLOCK_MIN
              MAX_VAL = CLOCK_MAX
            ENDIF
          ENDIF
          WRITE(25, &
     &    '(" window ",10i5,2I7,5X)')first_time, last_time, MIN_VAL,MAX_VAL
!
          write(25,'(" read ")')
!
!         Put out the label over the plot
          if(do_web_plots) then
            write(25, &
     &                '(" label",2f5.2," 1 0 \h1 ")')(clock_bounds(1))+.07, (clock_bounds(4)+.05)
          else
            write(25, &
     &                '(" label",2f5.2," 1 0 \h1 ")')(clock_bounds(1)), (clock_bounds(4)+.03)
          endif
!
!
!         Put out the clock rms statistics card.
          XSTART = (FJD_STOP - FJD_START)*.05
          YSTART = (MAX_VAL  - MIN_VAL  )*.90
!
!         Nominal performance (85 ps rms) + 50% = 127.5 ps
!         Don't flag these values.
          if(do_web_plots) then
            over_offset = .07
            up_offset   = .01
          else
            over_offset = .07
            up_offset   = .01
          endif
!
          IF (CLOCK_RMS .LT. 127.5) THEN
            WRITE(25,'( &
     &      " label",2f5.2," 1 0 ", &
     &      " RMS Clock Offset =",I7," ps.  ")') &
     &     (clock_bounds(1)+over_offset),(clock_bounds(4)+up_offset),CLOCK_RMS
          END IF
!
!         Flag values between 1.5 and 3.0 times nominal value (85 ps rms).
          IF (CLOCK_RMS .GE. 127.5 .AND. CLOCK_RMS .LT. 255.) THEN
            WRITE(25,'( &
     &      " label",2f5.2," 1 0 ", &
     &      " RMS Clock Offset =",I7, &
     &      " ps.  Warning level 1! ")') &
     &     (clock_bounds(1)+over_offset),(clock_bounds(4)+up_offset),CLOCK_RMS
          END IF
!
!         Flag values between 3.0 and 10.0 times nominal value (85 ps).
          IF (CLOCK_RMS .GE. 255. .AND. CLOCK_RMS .LT. 850.) THEN
            WRITE(25,'( &
     &      " label",2f5.2," 1 0 ", &
     &      " RMS Clock Offset =",I7, &
     &      " ps.  Warning level 2! ")') &
     &     (clock_bounds(1)+over_offset),(clock_bounds(4)+up_offset),CLOCK_RMS
          END IF
!
!         Flag values greater than 10 times nominal value (85 ps rms).
          IF (CLOCK_RMS .GE. 850.) THEN
            WRITE(25,'( &
     &      " label",2f5.2," 1 0 ", &
     &      " RMS Clock Offset =",I7, &
     &      " ps.  WARNING LEVEL 3! ")') &
     &     (clock_bounds(1)+over_offset),(clock_bounds(4)+up_offset),CLOCK_RMS
          END IF
!
! ------- Write the DRAW cards
!
          WRITE(25,'(" draw ")')
!
! ------- Write the clock break plotting control, if neccessay.
!
          IF ( BREAKS_PER_SITE(ISTA) .GT. 1 ) THEN
               WRITE(25,'(" view ",4f5.2)') clock_bounds
               write(25,'(" headers 0")')
               write(25,'(" x_field 0 1 5")')
               write(25,'(" y_field 1 6 7")')
               WRITE(25, &
     &                    '(" window ",10i5,2I7,5X)')FIRST_TIME, LAST_TIME, MIN_VAL, MAX_VAL
               write(25,'(" ebz 0")')
               write(25,'(" point 3")')
               write(25,'(" line 0 ")')
               buffer = 'file '//SCRATCH_DIR//'clk_brk_'//PRE_LETRS
               write(25,'(a)') buffer
               write(25,'(" read ")')
               write(25,'(" draw ")')
          ENDIF
        ELSE IF ( INUMBERC(ISTA) .LE. 0 ) THEN
!
! ------- This was the clock reference station
!
          IF ( DO_WEB_PLOTS ) THEN
               write(25,'(" charsz 1.5")')
            ELSE
               write(25,'(" charsz 1.0")')
          ENDIF
          write(25, &
     &    '(" label",2f5.2," 1 0 Clock reference station.")')(clock_bounds(1)), (clock_bounds(4)-.15)
          write(25,'(" charsz ",f5.2)') character_size
        ENDIF
!
! ----- Do the zenith dry plots, but not for the web
!
! ----- If this site not a selected site skip out.
!
        IF(dry_z_control(ISTA) .and. .not.do_web_plots) then
          plot_to_do = .true.
!         Initil max and min y-axis scaling and clock rms statistics.
          MIN_VAL =  10000
          MAX_VAL = -10000
          GET_FIRST = .TRUE.
          dry_count = 0
!
!         Open the plot data file.
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(HOLD,'(2A2)') (ISITN(I,ISTA),I=1,2)
            OPEN(26,file='/tmp/dryd'//HOLD//PRE_LETRS,IOSTAT=IOS)
          ELSE
            OPEN(26,file='/tmp/dryd'//PRE_LETRS,IOSTAT=IOS)
          ENDIF
          IF(IOS .GT. 0) THEN
            WRITE(errstr, &
     &      '("Error ",I5," in zen_dry opening plot data file")')  IOS
            call ferr( INT2(257), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
!
!         Write the plot title record to data file.
          WRITE(26,'("Saastamoinen dry zenith path delay for ", &
     &    4A2)') (ISITN(I,ISTA),I=1,4)
!
          DO IROW = 1, ICOUNT  !Run over the table
!           Get 1st time tag for scaling
            IF(SITE_ID(IROW) .EQ. ISTA) THEN
!
!             Handle y-component scaling
              IF( MAX_VAL .LT. saszdry(IROW)) &
     &           MAX_VAL = saszdry(IROW)
!
              IF( MIN_VAL .GT. saszdry(IROW)) &
     &           MIN_VAL = saszdry(IROW)
!
              dry_count = dry_count+1
!
!             Write value and sigma to plot data file.
              WRITE(26, &
     &          '(5I5,1X,F12.3)')(TIME_TAG(I,IROW),I=1,5),saszdry(IROW)
            ENDIF
          ENDDO
          CLOSE(26)
!
!         Now make the plot control file.
!         Write header card in the data file.
          WRITE(25,'("# Do the dry zenith plots              ")')
          WRITE(25,'(" headers 1                               ")')
!
!         Write view card to set up the screen boundaries.
          WRITE(25,'(" view ",4f5.2)') dry_bounds
!
!         Write point card for simple points
          WRITE(25,'(" point 1                                ")')
!
!         Write line card to connect the points
          WRITE(25,'(" line 1                                  ")')
!
!         Write the x-field card for ymdhm data
          WRITE(25,'(" x_field 0 1 5                           ")')
!
!         Write the y-field card for values and sigma in columns 6 and 7
          WRITE(25,'(" y_field 1 6 0 Dry zenith delay (ps)      ")')
!
!         Write data file name card
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/dryd",A4,A2)')HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/dryd",A2)') PRE_LETRS
          ENDIF
!
!         Set the error bars to not cross the Ts.
          write(25,'(" ebz 0 ")')
!
!         Set up the axes and labeling
          write(25,'(" axes BCNST BCNST ")')
!
!         Write the window card. Round y scaling up and down 10 ps.
          IF(DEFAULT_C_SCALE) THEN
            MIN_VAL = (MIN_VAL/10)*10-10
            MAX_VAL = (MAX_VAL/10)*10+20
          ELSE
            IF(SET_SCALE_INT_C) &
     &         THEN
              OK = .FALSE.
              DO WHILE(.not.OK)
                CALL CLEAR_MN()
                WRITE(bufstr,'( &
     &         "Dry zenith scale for ",4A2," in psec:")') &
     &          (ISITN(I,ISTA),I=1,4)
                call nl_mn()
                call addstr_f(" Min,Max ? " )
                call getstr_f(bufstr )
                READ(bufstr,*,IOSTAT=ios) MIN_VAL,MAX_VAL
                call ferr ( INT2(ios), "Invalid dry scale", INT2(0), INT2(0) )
!               Check for reasonable values
                IF( MIN_VAL   .lt. MAX_VAL   .and. &
     &             MIN_VAL   .gt. -2010.    .and. &
     &             MAX_VAL   .lt.  2010. )  OK = .TRUE.
              ENDDO
            ELSE
              MIN_VAL = CLOCK_MIN
              MAX_VAL = CLOCK_MAX
            ENDIF
          ENDIF
          WRITE(25, &
     &    '(" window ",10i5,2I7,5X)')first_time, last_time, MIN_VAL,MAX_VAL
!
          write(25,'(" read ")')
!
!         Put out the label over the plot
          write(25, &
     &              '(" label",2f5.2," 1 0 \h1 ")')(dry_bounds(1)+.04), (dry_bounds(4)+.01)
!
!         Put out the clock rms statistics card.
          XSTART = (FJD_STOP - FJD_START)*.05
          YSTART = (MAX_VAL  - MIN_VAL  )*.90
!
!         Write the DRAW cards
          WRITE(25,'(" draw ")')
        Endif
!****************
!       Do the zenith total plots
!
!       If this site not a selected site skip out.
        IF(tot_z_control(ISTA) .and. .not.do_web_plots) THEN
!
!
          plot_to_do = .true.
!         Initil max and min y-axis scaling and clock rms statistics.
          MIN_VAL =  10000
          MAX_VAL = -10000
          GET_FIRST = .TRUE.
          dry_count = 0
!
!         Open the plot data file.
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(HOLD,'(2A2)') (ISITN(I,ISTA),I=1,2)
            OPEN(26,file='/tmp/totd'//HOLD//PRE_LETRS,IOSTAT=IOS)
          ELSE
            OPEN(26,file='/tmp/totd'//PRE_LETRS,IOSTAT=IOS)
          ENDIF
          IF(IOS .GT. 0) THEN
            WRITE(errstr, &
     &      '("Error ",I5," in tot_dry opening plot data file")')  IOS
            call ferr( INT2(258), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
! ------- Write the plot title record to data file.
!
          WRITE(26,'("Total zenith path delay for ", &
     &    4A2)') (ISITN(I,ISTA),I=1,4)
!
          DO IROW = 1, ICOUNT  !Run over the table
!
! ---------- Get 1st time tag for scaling
!
             IF ( SITE_ID(IROW) .EQ. ISTA ) THEN
                  IF ( SASZUSE(IROW) ) THEN
                       TOTAL_ZENITH = SASZDRY(IROW) + ATM_VAL(IROW)
                    ELSE
                       TOTAL_ZENITH = ATM_VAL(IROW)
                  END IF
!
! --------------- Handle y-component scaling
!
                  IF ( MAX_VAL .LT. TOTAL_ZENITH  ) MAX_VAL = TOTAL_ZENITH
!
                  IF ( MAX_VAL .LT. SASZDRY(IROW) ) MAX_VAL = SASZDRY(IROW)
!
                  IF ( MIN_VAL .GT. TOTAL_ZENITH  ) MIN_VAL = TOTAL_ZENITH
!
                  IF ( MIN_VAL .GT. SASZDRY(IROW) ) MIN_VAL = SASZDRY(IROW)
!
                  TOT_COUNT = TOT_COUNT+1
!
! --------------- Write value of total and dry zenith to files
!
                  WRITE ( 26, '(5I5,1X,3F12.3)' ) ( TIME_TAG(I,IROW),I=1,5), &
     &                    TOTAL_ZENITH, ATM_SIG(IROW), SASZDRY(IROW)
            ENDIF
          ENDDO
          CLOSE(26)
!
! ------- Now make the plot control file.
! ------- Write header card in the data file.
!
          WRITE(25,'("# Do the total zenith plots              ")')
          WRITE(25,'(" headers 1                               ")')
!
! ------- Write view card to set up the screen boundaries.
!
          WRITE(25,'(" view ",4f5.2)') tot_bounds
!
! ------- Write point card for simple points
!
          WRITE(25,'(" point 1                                ")')
!
! ------- Write line card to connect the points
!
          WRITE(25,'(" line 1                                  ")')
!
! ------- Write the x-field card for ymdhm data
!
          WRITE(25,'(" x_field 0 1 5                           ")')
!
! ------- Write the y-field card for values and sigma in columns 6 and 7
!
          WRITE(25,'(" y_field 1 6 7 Total zenith delay (ps)      ")')
!
! ------- Write data file name card
!
          IF ( SAVE_PLOT_FILES ) THEN
               WRITE ( 25, '(" file /tmp/totd", A4, A2) ' ) HOLD, PRE_LETRS
             ELSE
               WRITE ( 25, '(" file /tmp/totd",A2)') PRE_LETRS
          ENDIF
!
!         Set the error bars to not cross the Ts.
          write(25,'(" ebz 0 ")')
!
!         Set up the axes and labeling
          write(25,'(" axes BCST BCNST ")')
!
!         Write the window card. Round y scaling up and down 10 ps.
          IF(DEFAULT_C_SCALE) THEN
            MIN_VAL = (MIN_VAL/200)*200
            MAX_VAL = (MAX_VAL/200)*200+200
          ELSE
            IF(SET_SCALE_INT_C) &
     &         THEN
              OK = .FALSE.
              DO WHILE(.not.OK)
                CALL CLEAR_MN()
                WRITE(bufstr,'( &
     &         "Total zenith scale for ",4A2," in psec:")') &
     &          (ISITN(I,ISTA),I=1,4)
                call nl_mn()
                call addstr_f(" Min,Max ? " )
                call getstr_f(bufstr )
                READ(bufstr,*,IOSTAT=ios) MIN_VAL,MAX_VAL
                call ferr( INT2(ios), "Invalid dry scale", INT2(0), INT2(0) )
!               Check for reasonable values
                IF( MIN_VAL   .lt. MAX_VAL   .and. &
     &             MIN_VAL   .gt. -2010.    .and. &
     &             MAX_VAL   .lt.  2010. )  OK = .TRUE.
              ENDDO
            ELSE
              MIN_VAL = CLOCK_MIN
              MAX_VAL = CLOCK_MAX
            ENDIF
          ENDIF
          WRITE(25, &
     &    '(" window ",10i5,2I7,5X)')first_time, last_time, MIN_VAL,MAX_VAL
!
          write(25,'(" read ")')
!
!         Put out the label over the plot
          write(25, &
     &              '(" label",2f5.2," 1 0 \h1 ")')(tot_bounds(1)+.04), (tot_bounds(4)+.01)
!
!         Put out the clock rms statistics card.
          XSTART = (FJD_STOP - FJD_START)*.05
          YSTART = (MAX_VAL  - MIN_VAL  )*.90
!
!         Write the DRAW cards
          WRITE(25,'(" draw ")')
!
!         Write the y-field card for Sass. dry
          WRITE(25,'(" y_field 1 8 0       ")')
          write(25,'(" read ")')
          write(25,'(" draw ")')
!
        endif
!
!****************
!       Do the humidity plot
!
!       If this site not a selected site skip out.
        IF(humid_control(ISTA) .and. .not.do_web_plots) then
          plot_to_do = .true.
          MIN_VAL =  10000
          MAX_VAL = -10000
          GET_FIRST = .TRUE.
          temp_count = 0
!
!         Open the plot data file.
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(HOLD,'(2A2)') (ISITN(I,ISTA),I=1,2)
            OPEN(26,file='/tmp/humi'//HOLD//PRE_LETRS,IOSTAT=IOS)
          ELSE
            OPEN(26,file='/tmp/humi'//PRE_LETRS,IOSTAT=IOS)
          ENDIF
          IF(IOS .GT. 0) THEN
            WRITE(errstr, &
     &      '("Error ",I5," in humid   opening plot data file")')  IOS
            call ferr( INT2(259), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
!
!         Write the plot title record to data file.
          WRITE(26,'("Humidity     ", &
     &    4A2)') (ISITN(I,ISTA),I=1,4)
!
          DO IROW = 1, ICOUNT  !Run over the table
!           Get 1st time tag for scaling
            IF(SITE_ID(IROW) .EQ. ISTA) THEN
!
!             Handle y-component scaling
              IF( MAX_VAL .LT. humid_h(IROW)) &
     &           MAX_VAL = humid_h(IROW)
!
              IF( MIN_VAL .GT. humid_h(IROW)) &
     &           MIN_VAL = humid_h(IROW)
!
              hum_count = hum_count+1
!
!             Write value and sigma to plot data file.
              WRITE(26, &
     &          '(5I5,1X,F12.3)')(TIME_TAG(I,IROW),I=1,5),humid_h(IROW)
            ENDIF
          ENDDO
          CLOSE(26)
!
!         Now make the plot control file.
!         Write header card in the data file.
          WRITE(25,'("# Do the humidity plots              ")')
          WRITE(25,'(" headers 1                               ")')
!
!         Write view card to set up the screen boundaries.
          WRITE(25,'(" view ",4f5.2)') humid_bounds
!
!         Write point card for simple points
          WRITE(25,'(" point 1                                ")')
!
!         Write line card to connect the points
          WRITE(25,'(" line 1                                  ")')
!
!         Write the x-field card for ymdhm data
          WRITE(25,'(" x_field 0 1 5                           ")')
!
!         Write the y-field card for values and sigma in columns 6 and 7
          WRITE(25,'(" y_field 1 6 0 Humidity (%)               ")')
!
!         Write data file name card
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/humi",A4,A2)')HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/humi",A2)') PRE_LETRS
          ENDIF
!
!         Set the error bars to not cross the Ts.
          write(25,'(" ebz 0 ")')
!
!         Set up the axes and labeling
          write(25,'(" axes BCST BCNST ")')
!
!         Write the window card. Round y scaling up and down 10 ps.
          IF(DEFAULT_C_SCALE) THEN
            min_val = 0.
            max_val = 100.
!cccc       MIN_VAL = (MIN_VAL/2)*2-2
!ccc        MAX_VAL = (MAX_VAL/2)*2+4
          ELSE
            IF(SET_SCALE_INT_C) &
     &         THEN
              OK = .FALSE.
              DO WHILE(.not.OK)
                CALL CLEAR_MN()
                WRITE(bufstr,'( &
     &         "Temperature scale for ",4A2," in Centigrade:")') &
     &          (ISITN(I,ISTA),I=1,4)
                call nl_mn()
                call addstr_f(" Min,Max ? " )
                call getstr_f(bufstr )
                READ(bufstr,*,IOSTAT=ios) MIN_VAL,MAX_VAL
                call ferr( INT2(ios), "Invalid dry scale", INT2(0), INT2(0) )
!               Check for reasonable values
                IF( MIN_VAL   .lt. MAX_VAL   .and. &
     &             MIN_VAL   .gt. -2010.    .and. &
     &             MAX_VAL   .lt.  2010. )  OK = .TRUE.
              ENDDO
            ELSE
              MIN_VAL = CLOCK_MIN
              MAX_VAL = CLOCK_MAX
            ENDIF
          ENDIF
          WRITE(25, &
     &    '(" window ",10i5,2I7,5X)')first_time, last_time, MIN_VAL,MAX_VAL
!
          write(25,'(" read ")')
!
!         Put out the label over the plot
          write(25, &
     &              '(" label",2f5.2," 1 0 \h1 ")')(humid_bounds(1)+.04), (humid_bounds(4)+.01)
!
!         Write the DRAW cards
          WRITE(25,'(" draw ")')
!
!         Do the water vapor partial pressure plots
          write(25,'(" headers 4                 ")')
          WRITE(25,'(" view ",4f5.2)') humid_bounds
          write(25,'(" point 2                   ")')
          write(25,'(" line 1                    ")')
          write(25,'(" x_field 0 2 6             ")')
          write(25,'(" y_field 1 15 0 )")')
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/atmd",A4,A2)') HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/atmd",A2)') PRE_LETRS
          ENDIF
          write(25,'(" ebz 0                      ")')
          write(25,'(" axes A AM                  ")')
!
!         Set the upper scale of wet_partial pressure to 60 mbars
!         or max value + 10 mbars, whichever is greater.
          If(max_water_partial_pres .gt. 60.d0) then
            max_water_partial_pres = max_water_partial_pres+10.d0
          else
            max_water_partial_pres = 60.d0
          endif
!
          WRITE(25, &
     &    '(" window ",10i5,2f7.1,5X)')first_time, last_time, 0.,  max_water_partial_pres
          write(25,'(" read                       ")')
          write(25,'(" draw                       ")')
          write(25,'(" label",2f5.2, &
     &    " 0 1 (+)Wet Partial Pressure (mbars)")') &
     &              (humid_bounds(2)+.04), (humid_bounds(3)+.02)
        Endif
!****************
!       Do the temperature plot
!
!       If this site not a selected site skip out.
        IF(temp__control(ISTA) .and. .not.do_web_plots) THEN
!
!
          plot_to_do = .true.
!         Initil max and min y-axis scaling and clock rms statistics.
          MIN_VAL =  10000
          MAX_VAL = -10000
          GET_FIRST = .TRUE.
          temp_count = 0
!
!         Open the plot data file.
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(HOLD,'(2A2)') (ISITN(I,ISTA),I=1,2)
            OPEN(26,file='/tmp/temp'//HOLD//PRE_LETRS,IOSTAT=IOS)
          ELSE
            OPEN(26,file='/tmp/temp'//PRE_LETRS,IOSTAT=IOS)
          ENDIF
          IF(IOS .GT. 0) THEN
            WRITE(errstr, &
     &      '("Error ",I5," in temp__  opening plot data file")')  IOS
            call ferr( INT2(260), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
!
!         Write the plot title record to data file.
          WRITE(26,'("Temperature  ", &
     &    4A2)') (ISITN(I,ISTA),I=1,4)
!
          DO IROW = 1, ICOUNT  !Run over the table
!           Get 1st time tag for scaling
            IF(SITE_ID(IROW) .EQ. ISTA) THEN
!
!             Handle y-component scaling
              IF( MAX_VAL .LT. tempc_h(IROW)) &
     &           MAX_VAL = tempc_h(IROW)
!
              IF( MIN_VAL .GT. tempc_h(IROW)) &
     &           MIN_VAL = tempc_h(IROW)
!
              dry_count = dry_count+1
!
!             Write value and sigma to plot data file.
              WRITE(26, &
     &          '(5I5,1X,F12.3)')(TIME_TAG(I,IROW),I=1,5),tempc_h(IROW)
            ENDIF
          ENDDO
          CLOSE(26)
!
!         Now make the plot control file.
!         Write header card in the data file.
          WRITE(25,'("# Do the temperature                   ")')
          WRITE(25,'(" headers 1                               ")')
!
!         Write view card to set up the screen boundaries.
          WRITE(25,'(" view ",4f5.2)') temp_bounds
!
!         Write point card for simple points
          WRITE(25,'(" point 1                                ")')
!
!         Write line card to connect the points
          WRITE(25,'(" line 1                                  ")')
!
!         Write the x-field card for ymdhm data
          WRITE(25,'(" x_field 0 1 5                           ")')
!
!         Write the y-field card for values and sigma in columns 6 and 7
          WRITE(25,'(" y_field 1 6 0 Temperatlure (C)           ")')
!
!         Write data file name card
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/temp",A4,A2)')HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/temp",A2)') PRE_LETRS
          ENDIF
!
!         Set the error bars to not cross the Ts.
          write(25,'(" ebz 0 ")')
!
!         Set up the axes and labeling
          write(25,'(" axes BCST BCNST ")')
!
!         Write the window card. Round y scaling up and down 10 ps.
          IF(DEFAULT_C_SCALE) THEN
            MIN_VAL = (MIN_VAL/2)*2-2
            MAX_VAL = (MAX_VAL/2)*2+4
          ELSE
            IF(SET_SCALE_INT_C) &
     &         THEN
              OK = .FALSE.
              DO WHILE(.not.OK)
                CALL CLEAR_MN()
                WRITE(bufstr,'( &
     &         "Temperature scale for ",4A2," in Centigrade:")') &
     &          (ISITN(I,ISTA),I=1,4)
                call nl_mn()
                call addstr_f(" Min,Max ? " )
                call getstr_f(bufstr )
                READ(bufstr,*,IOSTAT=ios) MIN_VAL,MAX_VAL
                call ferr ( INT2(ios), "Invalid dry scale", INT2(0), INT2(0) )
!               Check for reasonable values
                IF( MIN_VAL   .lt. MAX_VAL   .and. &
     &             MIN_VAL   .gt. -2010.    .and. &
     &             MAX_VAL   .lt.  2010. )  OK = .TRUE.
              ENDDO
            ELSE
              MIN_VAL = CLOCK_MIN
              MAX_VAL = CLOCK_MAX
            ENDIF
          ENDIF
          WRITE(25, &
     &    '(" window ",10i5,2I7,5X)')first_time, last_time, MIN_VAL,MAX_VAL
!
          write(25,'(" read ")')
!
!         Put out the label over the plot
          write(25, &
     &              '(" label",2f5.2," 1 0 \h1 ")')(temp_bounds(1)+.04), (temp_bounds(4)+.01)
!
!         Put out the clock rms statistics card.
          XSTART = (FJD_STOP - FJD_START)*.05
          YSTART = (MAX_VAL  - MIN_VAL  )*.90
!
!         Write the DRAW cards
          WRITE(25,'(" draw ")')
        Endif
!
!****************
!       Do the pressure plot
!
!       If this site not a selected site skip out.
        IF(press_control(ISTA) .and. .not.do_web_plots) THEN
          plot_to_do = .true.
          MIN_VAL =  10000
          MAX_VAL = -10000
          GET_FIRST = .TRUE.
          temp_count = 0
!
!         Open the plot data file.
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(HOLD,'(2A2)') (ISITN(I,ISTA),I=1,2)
            OPEN(26,file='/tmp/pres'//HOLD//PRE_LETRS,IOSTAT=IOS)
          ELSE
            OPEN(26,file='/tmp/pres'//PRE_LETRS,IOSTAT=IOS)
          ENDIF
          IF(IOS .GT. 0) THEN
            WRITE(errstr, &
     &      '("Error ",I5," in pressure opening plot data file")')  IOS
            call ferr( INT2(261), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
!
!         Write the plot title record to data file.
          WRITE(26,'("Pressure     ", &
     &    4A2)') (ISITN(I,ISTA),I=1,4)
!
          DO IROW = 1, ICOUNT  !Run over the table
!           Get 1st time tag for scaling
            IF(SITE_ID(IROW) .EQ. ISTA) THEN
!
!             Handle y-component scaling
              IF( MAX_VAL .LT. press_h(IROW)) &
     &           MAX_VAL = press_h(IROW)
!
              IF( MIN_VAL .GT. press_h(IROW)) &
     &           MIN_VAL = press_h(IROW)
!
              press_count = press_count+1
!
!             Write value and sigma to plot data file.
              WRITE(26, &
     &          '(5I5,1X,F12.3)')(TIME_TAG(I,IROW),I=1,5),press_h(IROW)
            ENDIF
          ENDDO
          CLOSE(26)
!
!         Now make the plot control file.
!         Write header card in the data file.
          WRITE(25,'("# Do the pressure plots              ")')
          WRITE(25,'(" headers 1                               ")')
!
!         Write view card to set up the screen boundaries.
          WRITE(25,'(" view ",4f5.2)') press_bounds
!
!         Write point card for simple points
          WRITE(25,'(" point 1                                ")')
!
!         Write line card to connect the points
          WRITE(25,'(" line 1                                  ")')
!
!         Write the x-field card for ymdhm data
          WRITE(25,'(" x_field 0 1 5                           ")')
!
!         Write the y-field card for values and sigma in columns 6 and 7
          WRITE(25,'(" y_field 1 6 0 Pressure (millibars       ")')
!
!         Write data file name card
          IF(SAVE_PLOT_fileS) &
     &       THEN
            WRITE(25,'(" file /tmp/pres",A4,A2)')HOLD,PRE_LETRS
          ELSE
            WRITE(25,'(" file /tmp/pres",A2)') PRE_LETRS
          ENDIF
!
!         Set the error bars to not cross the Ts.
          write(25,'(" ebz 0 ")')
!
!         Set up the axes and labeling
          write(25,'(" axes BCNST BCNST ")')
!
!         Write the window card. Round y scaling up and down 10 ps.
          IF(DEFAULT_C_SCALE) THEN
            MIN_VAL = (MIN_VAL/2)*2-2
            MAX_VAL = (MAX_VAL/2)*2+4
          ELSE
            IF(SET_SCALE_INT_C) &
     &         THEN
              OK = .FALSE.
              DO WHILE(.not.OK)
                CALL CLEAR_MN()
                WRITE(bufstr,'( &
     &         "Pressure  scale for ",4A2," in millibars:")') &
     &          (ISITN(I,ISTA),I=1,4)
                call nl_mn()
                call addstr_f(" Min,Max ? " )
                call getstr_f(bufstr )
                READ(bufstr,*,IOSTAT=ios) MIN_VAL,MAX_VAL
                call ferr ( INT2(ios), "Invalid dry scale", INT2(0), INT2(0) )
!               Check for reasonable values
                IF( MIN_VAL   .lt. MAX_VAL   .and. &
     &             MIN_VAL   .gt. -2010.    .and. &
     &             MAX_VAL   .lt.  2010. )  OK = .TRUE.
              ENDDO
            ELSE
              MIN_VAL = CLOCK_MIN
              MAX_VAL = CLOCK_MAX
            ENDIF
          ENDIF
          WRITE(25, &
     &    '(" window ",10i5,2I7,5X)')first_time, last_time, MIN_VAL,MAX_VAL
!
          write(25,'(" read ")')
!
!         Put out the label over the plot
          write(25, &
     &              '(" label",2f5.2," 1 0 \h1 ")')(press_bounds(1)+.04), (press_bounds(4)+.01)
!
!         Write the DRAW cards
          WRITE(25,'(" draw ")')
        Endif
!****************
!
!       Close the plot control file
        write(25,'(" end ")')
        CLOSE(25)
!
! ----- Now schedule Frank Gomez's plot program
!
        IF ( PLOT_TO_DO  .AND.  .NOT. BATCH_CONTROL ) THEN
             IF ( OUTPUT_CONTROL) THEN
!
! -------------- Laserjet only
!
                 IERR = FPLOT ( CONTROL_FILE_NAME,'/hpgll   ','-d'//laserid)
               ELSE
                 WRITE ( STRING,' (100X)' )
                 LEN = TRIMLEN(CONTROL_FILE_NAME)
                 FNAME=SOLVE_PROG_DIR//'pc8'
                 IERR =FC_GETENV ( PTR_CH ( 'SOLVE_DIR'//CHAR(0)), PTR_NC(IBUF))
                 IF ( IERR .GT. 0 ) THEN
                      IF ( CBUF(IERR:IERR) .EQ. '/' ) THEN
                           FNAME = CBUF(1:IERR)//'pc8'
                        ELSE
                           FNAME = CBUF(1:IERR)//'/pc8'
                      ENDIF
                 ENDIF
                 WRITE ( STRING, '(A,A," /XW")' ) FNAME(:TRIMLEN(FNAME)+1), &
     &                                           CONTROL_FILE_NAME(1:LEN)
                 CALL ZTERM    ( STRING, IERR4 )
!
                 CALL END_MN()  ! Switching off cureses
                 CALL UN_CURSES ( )
!
! -------------- Launching the program pc8
!
                 IERR = SYSTEM2_SH ( STRING )
                 IF ( IERR .NE. 0 ) THEN
                      CALL ERR_LOG ( 5502, -1, 'THIRD', &
     &                    'Error during trying to launch program '//STRING )
                      STOP '(MDLPL,  Abnormal termination'
                 END IF
!
                 IF ( ISTA .LT. NUMSTA ) THEN
!!                      CALL HARD_RESET
                      CALL PRCH ( CHAR(10)//'Make plot for the next station '// &
     &                            'or quit: C(ontinue)/Q(uit) <C> ? ' )
                      CALL INSIM ( ASIM, ISIM )
                      IF ( ASIM .EQ. 'Q'  .OR.  ASIM .EQ. 'q' ) RETURN
                    ELSE IF ( ISTA .GE. NUMSTA ) THEN
                      RETURN
                 ENDIF
!
! -------------- Launch curser again
!
                 CALL START_MN()
            ENDIF
        ENDIF
!
      ENDDO
!
!     If needed do the earth orientation plots
!       Open the plot control file.
!
!     If there is anything to be plotted open the plot control file
      If( (EO_CONTROL(1) .and. EOP_STYLE(1).eq. EOP__RATES_AND_SEGS ) .or. &
     &    (EO_CONTROL(1) .and. EOP_STYLE(1).eq. EOP__SEGS_ONLY      )) &
     &then !open the control file.
        control_file_name = '/tmp/eopc'//letters
        OPEN(25,file=control_file_name,IOSTAT=IOS)
        If(IOS .GT. 0) then
          write(errstr,'( &
     &    "Error ",I5," in opening plot control file")') ios
           call ferr( INT2(262), errstr, INT2(0), INT2(0) )
          STOP
        endif
        do_plot_eop = .true.
        WRITE(25,'(" begin ",2i6)') iaccros, idown
      else !No eop to plot, so bag it.
        do_plot_eop = .false.
      Endif
!
      xrms_sig = 0.d0
      yrms_sig = 0.d0
      urms_sig = 0.d0
!
!     See if the user wants to override the defaults horizontal scale.
      If(start_time(1).gt.0) then
        do irow = 1,5
          first_time(irow) = start_time(irow)
          last_time (irow) = stop_time (irow)
        enddo
      endif
!
      DO IEOP = 1,3     !Run over x-wobble, y-wobble, UT1
        IF (IEOP .LE. 2) THEN
          IEOP_TYPE = 1     !x & y wobble
        ELSE
          IEOP_TYPE = 2     !ut1
        END IF
!
        IF(EO_CONTROL(IEOP) .AND. EOP_STYLE(1) .EQ. EOP__RATES_AND_SEGS .or. &
     &     EO_CONTROL(IEOP) .AND. EOP_STYLE(1) .EQ. EOP__SEGS_ONLY     )  THEN
!         This type of eo parameter selected and has a parameterization style
!         which could be dealt with
!         Initialize max and min y-axis scaling
          VAL_MIN =  10000.0
          VAL_MAX = -10000.0
          GET_FIRST = .TRUE.
!
!         Open the scratch plot data file.
          HOLD(1:3) = HOLD_EO(IEOP)
          data_file_name = '/tmp/eopd_tmp'//HOLD(1:3)//letters
          OPEN(26,file=data_file_name,IOSTAT=IOS)
          IF(IOS .GT. 0) THEN
            WRITE(errstr,'( &
     &      "Error ",I5," in opening eo plot data file")') ios
            call ferr( INT2(263), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
          DO IROW = 1, IEO_CT  !Run over the table
!
!           Handle y-component scaling, both ps and cm scales
!
            IF(VAL_MAX .LT. (EO_VAL(IEOP,IROW)+EO_SIG(IEOP, &
     &         IROW)))VAL_MAX    =  EO_VAL(IEOP,IROW)+EO_SIG(IEOP,IROW)
            IF(VAL_MAX .LT. (model_val(IEOP,IROW, &
     &         2)))VAL_MAX    =  model_val(IEOP,IROW,2)
!
            IF(VAL_MIN .GT. (EO_VAL(IEOP,IROW)-EO_SIG(IEOP, &
     &         IROW)))VAL_MIN    =  EO_VAL(IEOP,IROW)-EO_SIG(IEOP,IROW)
            IF(VAL_MIN .GT. (model_val(IEOP,IROW, &
     &         2)))VAL_MIN    =  model_val(IEOP,IROW,2)
!
!           Write value and sigma to plot data file.
            WRITE(26, &
     &      '(f15.5,5I5,3F12.3)')eop_date(i),(eop_tag(i,irow),i=1,5), &
     &      EO_VAL(IEOP,IROW),EO_SIG(IEOP,IROW),model_val(ieop,irow,2)
            If(ieop.eq.1) xrms_sig = xrms_sig + eo_sig(1,irow)**2
            If(ieop.eq.2) yrms_sig = yrms_sig + eo_sig(2,irow)**2
            If(ieop.eq.3) urms_sig = urms_sig + eo_sig(3,irow)**2
          ENDDO
          CLOSE(26)
!
!         Sort the data file
          call unix_file_sort(data_file_name,'' )
!
!         Open the sorted scratch file and the real plot data file
          OPEN(27,file=data_file_name,IOSTAT=IOS)
          data_file_name = '/tmp/eopd'//HOLD(1:3)//PRE_LETRS
          OPEN(26,file=data_file_name,IOSTAT=IOS)
          IF(IOS .GT. 0) THEN
            WRITE(errstr,'( &
     &      "Error ",I5," in opening eo plot data file")') ios
            call ferr( INT2(264), errstr, INT2(0), INT2(0) )
            STOP
          ENDIF
!
!         Write the plot title record to data file.
          IF(EOP_STYLE(IEOP_TYPE) .EQ. EOP__RATES_AND_SEGS) then
            EO_PLOT_TITLE = EO_LABEL(IEOP)//'Global rate and segment offsets'
          else
             EO_PLOT_TITLE = EO_LABEL(IEOP)//'Segment offsets only'
          Endif
          WRITE(26,'(A80)') EO_PLOT_TITLE
!
!         Copy the scratch file into the real file
          ios = 0
          do while (ios.eq.0)
             read(27,'(a)',iostat=ios) buffer
             if(ios.eq.0) write(26,'(a)',iostat=ios) buffer
          enddo
          close(26)
          close(27,status='delete')
!
          xrms_sig = sqrt(xrms_sig/ieo_ct)
          yrms_sig = sqrt(yrms_sig/ieo_ct)
          urms_sig = sqrt(urms_sig/ieo_ct)
!
!         Now make the plot control file.
!         Write header card in the data file.
          Write(25,'(" headers 1       ")')
!
!         write character size card
          write(25,'(" charsz .4 ")')
!
!         Write view card to set up the screen boundaries.
          If(ieop.eq.1) write(25,'(" view .1 .90 .05 .25  ")')
          If(ieop.eq.2) write(25,'(" view .1 .90 .35 .55  ")')
          If(ieop.eq.3) write(25,'(" view .1 .90 .65 .85  ")')
!
!
!         Write point card for diamond shaped data points
          WRITE(25,'(" point 11  ")')
!
!         Write line card to connect the points
          WRITE(25,'(" line 1    ")')
!
!         Major tick interval
          write(25,'("#set the major tick interval to 1/8 day")')
          write(25,'(" x_mjtiv .125 ")')
!
!         Minor tick interval
          write(25,'("#set the subtick interval to 1 hour")')
          write(25,'(" x_sbtiv 3")')
!
!         Write the x-field card for ymdhm data
          write(25,'(" x_field 0 2 6  ")')
!
!         Write the y-field card for values and sigma in columns 6 and 7
          If(ieop.eq. &
     &    1)WRITE(25,'(" y_field 1 7 8 Delta (micro-asec)")')
          If(ieop.eq. &
     &    2)WRITE(25,'(" y_field 1 7 8 Delta (micro-aecs)")')
          If(ieop.eq.3) WRITE(25,'(" y_field 1 7 8 Delta (micro-s)")')
!
!
!         Write data file name card
          WRITE(25,'(" file /tmp/eopd",A3,A2)') HOLD(1:3),PRE_LETRS
!
!         Set the error bars for no basrs at the end of the lines.
          write(25,'(" ebz 0 ")')
!
!         Set up the axes
          If(ieop.eq.1) then
            write(25,'(" axes BCNST BCNST ")')
          else
            write(25,'(" axes BCST BCNST ")')
          endif
!
!         Write the x-scale and y_scales
!         Round y-scaling up and down.
          window_buffer = ' '
          if(default_eo_scale) then
            IF (IEOP_TYPE .EQ. 1) THEN
              VAL_MIN    = int((VAL_MIN-.25)*4.)/4.
              VAL_MAX    = int((VAL_MAX+.25)*4.)/4.
              write(window_buffer, &
     &        '(" window ",10i5,1x,2f10.3)')first_time,last_time, val_min,val_max
            ELSE
              VAL_MIN    = int((VAL_MIN - 10.)*.1)*10.
              VAL_MAX    = int((VAL_MAX + 10.)*.1)*10.
              WRITE(window_buffer, &
     &        '(" window ",10i5,1x,2f10.3)')first_time,last_time,val_min,val_max
            END IF
          else
            if(ieop .eq. &
     &         1)WRITE(window_buffer, &
     &        '(" window ",10i5,1x,2f10.3)')first_time,last_time,xp_scale_min,xp_scale_max
            if(ieop .eq. &
     &         2)WRITE(window_buffer, &
     &        '(" window ",10i5,1x,2f10.3)')first_time,last_time,yp_scale_min,yp_scale_max
            if(ieop .eq. &
     &         3)WRITE(window_buffer, &
     &        '(" window ",10i5,1x,2f10.3)')first_time,last_time,ut1_scale_min,ut1_scale_max
          endif
          write(25,'(a)') window_buffer
!
!         Write the READ card.
          WRITE(25,'(" read ")')
!
!         Put out the label over the plot
          write(25,'(" charsz 1.5")')
          write(25, &
     &    '(" label .20 .97 1 0 Database ",a10)')ldbnam_c(1)
          write(25,'(" charsz .6 ")')
          if(ieop.eq.1) write(25,'(" label .05 .28 1 0 \h1      ")')
          if(ieop.eq.1) write(25,'(" label .05 .26 1 0 ", &
     &      "x_pole rms sigma ",f7.1,", micro-arcseconds")') &
     &       xrms_sig
          if(ieop.eq.2) write(25,'(" label .05 .58 1 0 \h1      ")')
          if(ieop.eq.2) write(25,'(" label .05 .56 1 0 ", &
     &      "y_pole rms sigma ",f7.1,", micro-arcseconds")') &
     &       yrms_sig
          if(ieop.eq.3) write(25,'(" label .05 .88 1 0 \h1      ")')
          if(ieop.eq.3) write(25,'(" label .05 .86 1 0 ", &
     &      "ut1 rms sigma ",f10.1,", micro-seconds of time")') urms_sig
!
          If(ieop.eq.1) then
            plot_model = .true.
            If(hfeopf_chr(1:8) .eq. 'NONE    ') then
              write(25,'(" label .50 .90 1 0 High frequency EOP ", &
     &        "model ",A8," adjusted to remove ")') &
     &        hfeop_cmp_file_name(1:8)
              write(25,'(" label .50 .88 1 0 mean differences and ", &
     &        "plotted for comparison")')
              write(25,'(" label .50 .86 1 0 No high frequency ", &
     &        "EOP calibration applied in solution.")')
!
            else
              write(25,'(" label .50 .92 1 0 High frequency EOP ", &
     &        "model ",A8," adjusted to remove ")') &
     &        hfeop_cmp_file_name(1:8)
              write(25,'(" label .50 .90 1 0 mean differences and ", &
     &        "plotted for comparison")')
               write(25,'(" label .50 .88 1 0 ", &
     &        "High frequency EOP model ",a8," used in solution ")') &
     &         hfeopf_chr(1:8)
               write(25,'(" label .50 .86 1 0 ", &
     &        "and restored in these plots!")')
            endif
          endif
          write(25,'(" charsz .4 ")')
!
!         Put out the DRAW card.
          WRITE(25,'(" draw ")')
!
!         Put out high frequency eop information, if called for.
          If(plot_model) then
            If(ieop.eq.1) write(25,'(" view .1 .90 .05 .25  ")')
            If(ieop.eq.2) write(25,'(" view .1 .90 .35 .55  ")')
            If(ieop.eq.3) write(25,'(" view .1 .90 .65 .85  ")')
            write(25,'(" point 1 ")')
            write(25,'(" line 1 ")')
            write(25,'(" x_field 0 2 6 ")')
            write(25,'(" y_field 1 9 0 ")')
            WRITE(25,'(" file /tmp/eopd",A3,A2)') HOLD(1:3),PRE_LETRS
            WRITE(25,'(a)') window_buffer
            WRITE(25,'(" read ")')
            WRITE(25,'(" draw ")')
          endif
        ENDIF !This eo parameter selected and has ok parameterization style
      ENDDO !running over x-wobble, y-wobble, and UT1
!
      If(do_plot_eop) then
        write(25,'(" end ")')
        close(25)
        If(output_control) then !laserjet only
          ierr = fplot(control_file_name,'/hpgll   ','-d'//laserid)
        else
!         ierr = fplot(control_file_name,'/XW        ')
          write(string,'(100x)')
          len = trimlen(control_file_name)
            fname=SOLVE_PROG_DIR//'pc8'
            ierr=fc_getenv(ptr_ch('SOLVE_DIR'//char(0)),ptr_nc(ibuf))
            if (ierr.gt.0) then
              fname = cbuf(1:ierr)//fname
            endif
            write(string, &
     &      '(a,a," /XW")')fname(:trimlen(fname)+1),control_file_name(1:len)
          call zterm(string,ierr4 )
!
          CALL END_MN()  ! Switching off cureses
          CALL UN_CURSES ( )
!
! ------- Launching the program pc8
!
          IERR = SYSTEM2_SH ( STRING )
          IF ( IERR .NE. 0 ) THEN
               CALL ERR_LOG ( 5503, -1, 'THIRD', &
     &             'Error during trying to launch program '//STRING )
               STOP '(MDLPL,  Abnormal termination'
          END IF
!
! -------------- Launch curser again
!
          CALL START_MN()
!
          CALL HARD_RESET()
          CALL ADDSTR_F ( "Make laserjet copy (y/(n)?" )
          call nl_mn()
          call senkr_mn ( iy, iy, ichr )
          one_char = cchar(4:4)
          call casefold ( one_char )
          IF ( ONE_CHAR .EQ. 'Y' ) THEN
               IERR = FPLOT ( CONTROL_FILE_NAME, '/hpgll   ', '-dlaser2' )
          END IF
        ENDIF
      ENDIF
!
! --- Purge the plot files.
!
      IF(.NOT.SAVE_PLOT_fileS .and. .not.batch_control) THEN
        OPEN (25,file='/tmp/atmd'//PRE_LETRS,IOSTAT=IOS)
        CLOSE(25,STATUS='DELETE')
        OPEN (25,file='/tmp/atmc'//PRE_LETRS,IOSTAT=IOS)
        CLOSE(25,STATUS='DELETE')
        OPEN (25,file='/tmp/atml'//PRE_LETRS,IOSTAT=IOS)
        CLOSE(25,STATUS='DELETE')
        OPEN (25,file='/tmp/clkd'//PRE_LETRS,IOSTAT=IOS)
        CLOSE(25,STATUS='DELETE')
        OPEN (25,file='/tmp/clkc'//PRE_LETRS,IOSTAT=IOS)
        CLOSE(25,STATUS='DELETE')
        OPEN (25,file='/tmp/eopdxwb'//letters,IOSTAT=IOS)
        OPEN (26,file='/tmp/eopdywb'//letters,IOSTAT=IOS)
        OPEN (27,file='/tmp/eopdut1'//letters,IOSTAT=IOS)
        CLOSE(25,STATUS='DELETE')
        CLOSE(26,STATUS='DELETE')
        CLOSE(27,STATUS='DELETE')
        OPEN (27,file='/tmp/eopc'//letters,IOSTAT=IOS)
      ENDIF
!
      RETURN
      END  !#!  THIRD  #!#
