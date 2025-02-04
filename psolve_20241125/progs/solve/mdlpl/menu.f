      SUBROUTINE MENU()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  MENU PROGRAM SPECIFICATION
!
! 1.1 Builds and interogates the MDLPL menu.
!
! 1.2 MENU REFERENCES:
!
! 2.  INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'prfil.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'mdlcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!      CALLING SUBROUTINES:mdlpl
!       CALLED SUBROUTINES: reset_screen, hard_reset
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 OK, TOGGLE_ALL, SCREEN_COORDS_SET
      INTEGER*2 ISTA,I,TRIMLEN,iconti,nsite
      INTEGER*4 IOS, NUM_INDIV,ix,iy,ichr,jover,jdown,jo,jd
      integer*4 jdown_save
      character*4 cchar
      character*1 c1
      equivalence (ichr,cchar)
      CHARACTER*3 CLOCK_DISP, ATMOS_DISP,EO_DISP(3)
      CHARACTER*3 tot_z_disp, dry_z_disp,press_disp,temp__disp
      CHARACTER*3 humid_disp
      character*80 bufstr,bufcheck,field_hf
      CHARACTER*8 EO_TITLE(3)
      INTEGER*4 I4P0, I4P1, I4P4, I4P5, I4P6, I4P7, I4P8, I4P9
      INTEGER*4 I4P10, I4P12, I4P15, I4P16, I4P28
      INTEGER*4 I4P40, I4P48, I4P51, I4P64
      logical*2 loop
      CHARACTER*18 STR*54, GET_VERSION*54
      DATA  I4P0,I4P1,I4P4,I4P5,I4P6,I4P7,I4P8,I4P9/0,1,4,5,6,7,8,9/
      DATA  I4P10, I4P12, I4P15, I4P16, I4P28 /10,12,15,16,28/
      DATA  I4P40, I4P48, I4P51, I4P64 /40,48,51,64/
      DATA EO_TITLE/'X-wobble','Y-wobble','UT1     '/
      INTEGER*2  INT2_ARG
      INTEGER*4  I_LEN
!
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!  JWR  880601  Original version
!  JWR  881003  Added scale and plot lu control
!  JWR  910215  Changed OUTPUT_CONTROL initialization from TRUE to FALSE
!  :91,08.01:jwr: Changes made to support Frank Plot.
!   JLR   921216  replaced nJ's with I4Pn's
!  :94.06.20:jwr: Larges changes to support plotting of temp, press, humid,
!                 total and dry zenith path delay.
!  :94.06.29:jwr: Code added for batch mode.
!  :95.05.11:jwr: Logic added to move the curser to 'proceed' after '0' and ';'
!                 (Toggle all for site info and eop.)
!  :95.05.19:jwr: Control of eop scaling added.
!  :95.08.15:kdb: Compare arbitrary high frequency eop model to
!                 the eop totals.
!     KDB  960416 Convert hardcoded date to sccs-based date.
!                 Add sccsid parameter for version tracking.
!     jwr  980212 New logic added to create only clock and atmosphere plots
!                 for use on the VLBI web site. Also more user-friendly
!                 interactions for selecting what is to be plotted.
!     pet  1999.07.28  Removed unused variables
!
! 5.  MENU PROGRAM STRUCTURE
!
!     Initialize all control variables to their defaults.
      DO I=1,NUMSTA
        CLOCK_CONTROL(I) = .TRUE.
        ATMOS_CONTROL(I) = .TRUE.
        tot_z_control(I) = .TRUE.
        dry_z_control(I) = .true.
        press_control(I) = .TRUE.
        temp__control(I) = .TRUE.
        humid_control(I) = .TRUE.
      ENDDO
      DO I=1,3
        EO_CONTROL(I) = .FALSE.
      ENDDO
      OUTPUT_CONTROL  = .false.
      DEFAULT_A_SCALE = .true.
      DEFAULT_C_SCALE = .true.
      DEFAULT_EO_SCALE = .true.
      SAVE_PLOT_FILES = .false.
      ALT_PLOT_LU     = '/dev/tty'
      screen_coords_set = .false.
      CALL USE_GLBFIL_4('ORC' )
!     Set default high frequency eop model for comparison to totals
      hfeop_cmp_file_name = ' '
      hfeop_cmp_file_name = hfeopf_chr(1:trimlen(hfeopf_chr))
      if (hfeop_cmp_file_name.eq.' '.or.hfeop_cmp_file_name.eq. &
     &    'NONE')hfeop_cmp_file_name = 'hf996b  '
!
!     If in the batch mode, go back to main.
      If(batch_control) return
!
!     Resest the terminal to guarentee that log bottom is off.
!     Then play some games to no lose the write statement below.
 910  CONTINUE
      CALL HARD_RESET()
!
!
      NUM_INDIV = NUMSTA
      IF (NUM_INDIV .LT. 3) NUM_INDIV = 3
!
!
!      WRITE(bufstr,'(
!     ."---------------------------------------",
!     ."------------------------------------")')
!      call addstr_f(bufstr)
!      call nl_mn
      call &
     &     addstr_f("Plot Estimated Atmospheres, Clocks and Earth Orientation " )
           STR = GET_VERSION()
           CALL SETCR_MN ( 79-I_LEN(STR), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
      call nl_mn()
      WRITE(bufstr,'( &
     &"---------------------------------------", &
     &"----------------------------------------")')
      call addstr_f(bufstr )
      call nl_mn()
      jover = 0
      jdown = 2
      call setcr_mn(jover,jdown )
      call addstr_f("Station:      Marginal   Zenith    Zenith    " )
      jdown = 3
      call setcr_mn(jover,jdown )
      call addstr_f("               Clock   Adjustment  Total     " )
      jdown = 2
      jover = 45
      call setcr_mn(jover,jdown )
!!      call addstr_f("Zenith    Pressure  Temp.     Humidity")
      call addstr_f("Zenith    Pressure  Temp.     Hum." )
      jdown = 3
      call setcr_mn(jover,jdown )
      call addstr_f("Dry" )
      call nl_mn()
      DO ISTA = 1,NUMSTA
        CLOCK_DISP = 'NO '
        ATMOS_DISP = 'NO '
        tot_z_disp = 'NO '
        dry_z_disp = 'NO '
        press_disp = 'NO '
        temp__disp = 'NO '
        humid_disp = 'NO '
        IF(CLOCK_CONTROL(ISTA)) CLOCK_DISP = 'YES'
        IF(ATMOS_CONTROL(ISTA)) ATMOS_DISP = 'YES'
        IF(tot_z_CONTROL(ISTA)) tot_z_disp = 'YES'
        IF(dry_z_CONTROL(ISTA)) dry_z_disp = 'YES'
        IF(press_CONTROL(ISTA)) press_disp = 'YES'
        IF(temp__CONTROL(ISTA)) temp__disp = 'YES'
        IF(humid_CONTROL(ISTA)) humid_disp = 'YES'
        jdown = 4+ista
        jover = 0
        call setcr_mn(jover,jdown )
        WRITE(bufstr, &
     &  '(4A2," On--> ",a3,6(7x,a3))')(ISITN(I,ISTA),I=1,4), CLOCK_DISP, ATMOS_DISP, &
     &  tot_z_disp,dry_z_disp,press_disp,temp__disp, &
     &  humid_disp
      call addstr_f(bufstr )
      call nl_mn()
      ENDDO
!
      jover = 0
      jdown = 5+numsta
      CALL setcr_mn(jover,jdown )
      call addstr_f("Toggle: (0)All (1)       (2)       (3)       &
     &     (4)(5)       (6)       (7)      " )
      call nl_mn()
!
      jover = 0
      jdown = 7+numsta
      call setcr_mn(jover,jdown )
      call addstr_f("Earth orientation:" )
      call nl_mn()
      DO I = 1,3
        jover = 0
        jdown = i+7+numsta
        CALL setcr_mn(jover,jdown )
        EO_DISP(I) = 'NO '
        IF(EO_CONTROL(I)) EO_DISP(I) = 'YES'
        WRITE(bufstr,'(A8,8X,A3)') EO_TITLE(I), EO_DISP(I)
        call addstr_f(bufstr(:20) )
      ENDDO
      call nl_mn()
!
      jover = 22
      jdown = 7+numsta
      CALL setcr_mn(jover,jdown )
      call addstr_f("Toggle     (;)" )
      call nl_mn()
!
      jover = 0
      jdown = 12+numsta
      CALL setcr_mn(jover,jdown )
      IF(OUTPUT_CONTROL) THEN
       call addstr_f("(!)Copy control: Automatic laserjet copies only." )
      ELSE
       call addstr_f("(!)Copy control: Make screen plot and pause" )
      ENDIF
      call addstr_f("  Comparison (M)odel: " )
      call addstr_f(hfeop_cmp_file_name(1:trimlen(hfeop_cmp_file_name)) )
      call nl_mn()
!
!
      jover = 0
      jdown = 13+numsta
      CALL setcr_mn(jover,jdown )
      if (save_plot_files) then
        write(bufstr,'("(S)ave plot files: YES")')
      else
        write(bufstr,'("(S)ave plot files: NO")')
      endif
!
      If(do_web_plots) then
        write(bufstr(30:),'("($)Do Web plots: YES")')
      else
        write(bufstr(30:),'("($)Do Web plots: NO")')
      endif
!
      call addstr_f(bufstr )
      call nl_mn()
!
      jover = 0
      jdown = 14+numsta
      CALL setcr_mn(jover,jdown )
      WRITE(bufstr,'( &
     &"Set vertical scale: (C)lock, (A)tmos (E)OP")')
      call addstr_f(bufstr )
      call nl_mn()
!
      jover = 0
      jdown = 15+numsta
      call setcr_mn(jover,jdown )
      call &
     &     addstr_f('(P)roceed    (O)ptin   (L)ast page   (T)erminate   Least s(Q)uares' )
      call nl_mn()
!
!     Now deal with the users selections
      c1 = 'X'
!
      DO WHILE(c1.NE.'/' )
        If(.not. screen_coords_set) then
          jover = 1
          jdown = 15+numsta
          screen_coords_set = .true.
        else
          jover = jo
          jdown = jd
        endif
        call setcr_mn(jover,jdown )
        CALL SENKR_MN(IX,IY,ICHR )
        c1 = cchar(4:4)
        jo = ix
        jd = iy
!
!       Handle the possibility of global toggle of site info.
        if( &
     &     c1.eq.'0'  .or. c1.eq.'1' .or. c1.eq.'2' .or. c1.eq.'3' .or. &
     &     c1.eq.'4'  .or. c1.eq.'5' .or. c1.eq.'6' .or. c1.eq. &
     &    '7')iy = 4+numsta
!
!       Replace 'P' with '/', which is the old style interface
        If((c1.eq.'P' .or. c1.eq.'p'                      )  .or. &
     &     ((iy.lt.4 .or. iy.gt.4+numsta) .and. c1.eq.' ' )  ) c1 = '/'
!
!       Handle the copy control and comparison model line
        IF(iy.eq.16+numsta .and. ix .le. 50 .and. c1.eq.' ') c1 = '!'
        IF(iy.eq.16+numsta .and. ix .gt. 50 .and. c1.eq.' ') c1 = 'M'
!
!       This logic handles the lines which deal with either individual sites
!       or individual earth orientation parameters.
        IF ( iy.gt.4 .and. iy.le.4+numsta .and. c1.EQ.' ') then
          nsite = iy-4
          if(ix.ge.9 .and. ix.lt.15) then
            CLOCK_CONTROL(nsite)  = .true.
            ATMOS_CONTROL(nsite)  = .true.
            tot_z_CONTROL(nsite)  = .true.
            dry_z_CONTROL(nsite)  = .true.
            press_CONTROL(nsite)  = .true.
            temp__CONTROL(nsite)  = .true.
            humid_CONTROL(nsite)  = .true.
            goto 910 !This ugly 'goto' cause whole menu to be rewritten.
          else if(ix.ge.15 .and. ix.lt.24) THEN !individual clock
            IF (nsite.le. numsta) then
              IF(CLOCK_CONTROL(IY-4)) then
                CLOCK_CONTROL(nsite) = .FALSE.
              ELSE
                CLOCK_CONTROL(nsite) = .TRUE.
              ENDIF
              jover = 15
              jdown = iy
              CALL setcr_mn(jover,jdown )
              IF(CLOCK_CONTROL(nsite)) THEN
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            END IF
!
          else if (ix.lt.34) THEN !individual atmosphere
            IF (nsite .le. numsta) THEN
              IF(ATMOS_CONTROL(nsite)) THEN
                ATMOS_CONTROL(nsite) = .FALSE.
              ELSE
                ATMOS_CONTROL(nsite) = .TRUE.
              ENDIF
              jover = 25
              jdown = iy
              CALL setcr_mn(jover,jdown )
              IF(ATMOS_CONTROL(nsite)) THEN
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDIF
!
          else if (ix.lt.44) THEN !individual zen total
            IF (nsite .le. numsta) THEN
              IF(tot_z_controL(nsite)) THEN
                tot_z_controL(nsite) = .FALSE.
              ELSE
                tot_z_control(nsite) = .TRUE.
              ENDIF
              jover = 35
              jdown = iy
              CALL setcr_mn(jover,jdown )
              IF(tot_z_control(nsite)) THEN
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDIF
!
          else if (ix.lt.54) THEN !individual zen dry
            IF (nsite .le. numsta) THEN
              IF(dry_z_controL(nsite)) THEN
                dry_z_controL(nsite) = .FALSE.
              ELSE
                dry_z_control(nsite) = .TRUE.
              ENDIF
              jover = 45
              jdown = iy
              CALL setcr_mn(jover,jdown )
              IF(dry_z_control(nsite)) THEN
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDIF
!
          else if (ix.lt.64) THEN !individual press
            IF (nsite .le. numsta) THEN
              IF(press_controL(nsite)) THEN
                press_controL(nsite) = .FALSE.
              ELSE
                press_control(nsite) = .TRUE.
              ENDIF
              jover = 55
              jdown = iy
              CALL setcr_mn(jover,jdown )
              IF(press_control(nsite)) THEN
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDIF
!
          else if (ix.lt.74) THEN !individual temp
            IF (nsite .le. numsta) THEN
              IF(temp__controL(nsite)) THEN
                temp__controL(nsite) = .FALSE.
              ELSE
                temp__control(nsite) = .TRUE.
              ENDIF
              jover = 65
              jdown = iy
              CALL setcr_mn(jover,jdown )
              IF(temp__control(nsite)) THEN
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDIF
!
          else if (ix.lt.94) THEN !individual humidity
            IF (nsite .le. numsta) THEN
              IF(humid_controL(nsite)) THEN
                humid_controL(nsite) = .FALSE.
              ELSE
                humid_control(nsite) = .TRUE.
              ENDIF
              jover = 75
              jdown = iy
              CALL setcr_mn(jover,jdown )
              IF(humid_control(nsite)) THEN
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDIF
          endif
          jover = jo
          jdown = jd
        endif
!
        if(iy.eq.11+numsta) then ! x_pole
          jover = 16
          jdown = numsta+11
          CALL setcr_mn(jover,jdown )
          IF (EO_CONTROL(1)) THEN
            EO_CONTROL(1) = .false.
            call addstr_f("NO " )
          else
            EO_CONTROL(1) = .true.
            call addstr_f("YES" )
          endif
        endif
!
        if(iy.eq.12+numsta) then ! y_pole
          jover = 16
          jdown = numsta+12
          CALL setcr_mn(jover,jdown )
          IF (EO_CONTROL(2)) THEN
            EO_CONTROL(2) = .false.
            call addstr_f("NO " )
          else
            EO_CONTROL(2) = .true.
            call addstr_f("YES" )
          endif
        endif
!
        if(iy.eq.13+numsta) then ! ut1
          jover = 16
          jdown = numsta+13
          CALL setcr_mn(jover,jdown )
          IF (EO_CONTROL(3)) THEN
            EO_CONTROL(3) = .false.
            call addstr_f("NO " )
          else
            EO_CONTROL(3) = .true.
            call addstr_f("YES" )
          endif
        ENDIF
!
        CALL setcr_mn(IX,IY )
!
        IF(iy.eq.8+numsta .or. &
     &     c1.eq.'0'  .or. c1.eq.'1' .or. c1.eq.'2' .or. c1.eq.'3' .or. &
     &     c1.eq.'4'  .or. c1.eq.'5' .or. c1.eq.'6' .or. c1.eq. &
     &    '7')then ! working with the site toggle line
!
          toggle_all = .false.
          IF(c1.eq.'0' .or. c1.eq.' ' .and. ix.le. &
     &       14)toggle_all = .true.
!
          If(toggle_all .or. c1.eq.'1' .or. &
     &    (c1.eq.' ' .and. ix.ge.15 .and. ix.lt.25)) then !clocks
            do ista = 1,numsta
              if(clock_control(ista)) then
                clock_control(ista) = .false.
              ELSE
                clock_control(ista) = .true.
              ENDIF
              jover = 15
              jdown = 4+ista
              CALL setcr_mn(jover,jdown )
              IF(clock_control(ista)) then
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDDO
          endif
!
          if(toggle_all .or. c1.eq.'2' .or. &
     &       (c1.eq.' ' .and. ix.ge.25 .and. ix.lt.35)) then ! zen adjst
            do ista = 1,numsta
              if(atmos_control(ista)) then
                atmos_control(ista) = .false.
              ELSE
                atmos_control(ista) = .true.
              ENDIF
              jover = 25
              jdown = 4+ista
              CALL setcr_mn(jover,jdown )
              IF(atmos_control(ista)) then
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDDO
          endif
!
          if(toggle_all .or. c1.eq.'3' .or. &
     &       (c1.eq.' ' .and. ix.ge.35 .and. ix.lt.45)) then !zenith total
            do ista = 1,numsta
              if(tot_z_control(ista)) then
                tot_z_control(ista) = .false.
              ELSE
                tot_z_control(ista) = .true.
              ENDIF
              jover = 35
              jdown = 4+ista
              CALL setcr_mn(jover,jdown )
              IF(tot_z_control(ista)) then
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDDO
          endif
!
          if(toggle_all .or. c1.eq.'4' .or. &
     &       (c1.eq.' ' .and. ix.ge.45 .and. ix.lt.55)) then !dry_zenith
            do ista = 1,numsta
              if(dry_z_control(ista)) then
                dry_z_control(ista) = .false.
              ELSE
                dry_z_control(ista) = .true.
              ENDIF
              jover = 45
              jdown = 4+ista
              CALL setcr_mn(jover,jdown )
              IF(dry_z_control(ista)) then
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDDO
          endif
!
          if(toggle_all .or. c1.eq.'5' .or. &
     &       (c1.eq.' ' .and. ix.ge.55 .and. ix.lt.65)) then !press
            do ista = 1,numsta
              if(press_control(ista)) then
                press_control(ista) = .false.
              ELSE
                press_control(ista) = .true.
              ENDIF
              jover = 55
              jdown = 4+ista
              CALL setcr_mn(jover,jdown )
              IF(press_control(ista)) then
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDDO
          endif
!
          if(toggle_all .or. c1.eq.'6' .or. &
     &       (c1.eq.' ' .and. ix.ge.65 .and. ix.lt.75)) then !temperature
            do ista = 1,numsta
              if(temp__control(ista)) then
                temp__control(ista) = .false.
              ELSE
                temp__control(ista) = .true.
              ENDIF
              jover = 65
              jdown = 4+ista
              CALL setcr_mn(jover,jdown )
              IF(temp__control(ista)) then
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDDO
          endif
!
          if ( toggle_all .or. c1.eq.'7' .or. &
     &         (c1.eq.' ' .and. ix.ge.75      ) ) then ! humidity
            do ista = 1,numsta
              if(humid_control(ista)) then
                humid_control(ista) = .false.
              ELSE
                humid_control(ista) = .true.
              ENDIF
              jover = 75
              jdown = 4+ista
              CALL setcr_mn(jover,jdown )
              IF(humid_control(ista)) then
                call addstr_f("YES" )
              ELSE
                call addstr_f("NO " )
              ENDIF
            ENDDO
!
            If(toggle_all) then
              jover = 0
              jdown = 17+numsta
              call setcr_mn(jover,jdown )
            endif
          endif
        endif
!
        If((iy.eq.14+numsta .and. c1.eq.' ') .or. c1.eq.';') then
          Do i = 1,3
            jover = 16
            jdown = 7+numsta+i
            call setcr_mn(jover,jdown )
            IF(EO_CONTROL(i))THEN
              EO_CONTROL(i) = .FALSE.
              call addstr_f("NO " )
            ELSE
              EO_CONTROL(i) = .TRUE.
              call addstr_f("YES" )
            ENDIF
            call setcr_mn(jo,jd )
          enddo
          jover = 0
          jdown = 17+numsta
          call setcr_mn(jover,jdown )
        END IF
!
        IF(c1.EQ.'!') THEN !Copy control
          jover = 0
          jdown = 12+numsta
          CALL setcr_mn(jover,jdown )
          IF(.NOT. &
     &       OUTPUT_CONTROL)THEN
            OUTPUT_CONTROL = .TRUE.
            call &
     &           addstr_f("(!)Copy control: Automatic laserjet copies only. " )
          ELSE
            OUTPUT_CONTROL = .FALSE.
            call &
     &           addstr_f("(!)Copy control: Make screen plot and pause.   " )
          ENDIF
          call addstr_f("  Comparison (M)odel: " )
          call &
     &         addstr_f(hfeop_cmp_file_name(1:trimlen(hfeop_cmp_file_name)) )
          call nl_mn()
          jover = 0
          jdown = 17+numsta
          CALL setcr_mn(jover,jdown )
        ENDIF !Copy control
!
        IF(c1.EQ.'M') THEN !Comparison model
!         Get model to be used
          loop = .true.
          jover = 0
          jdown = 12+numsta
          CALL setcr_mn(jover,jdown )
          do while (loop)
            call &
     &           addstr_f("high frequency eop file without the directory " )
            call addstr_f("(:: quits)? " )
            CALL getstr_f(bufstr )
            call splitstring(bufstr,bufcheck,bufstr )
            if (trimlen(bufcheck).eq.0) then
              field_hf = ' '
            else
              field_hf = bufcheck(1:trimlen(bufcheck))
            endif
            call casefold(field_hf )
            if (trimlen(field_hf).eq. &
     &            0.or.field_hf.eq.'NONE') then
              call setcr_mn(jover,jdown )
              call addstr_f("Invalid!  " )
            else
              loop = .false.
            endif
          enddo
          if (index(bufcheck,":").eq.0) then
            hfeop_cmp_file_name = ' '
            hfeop_cmp_file_name = bufcheck(1:trimlen(bufcheck))
          endif
!         Rewrite the screen as needed
!          call reset_screen(jdown)
!          if (index(bufcheck,":").eq.0) then
            jover = 0
            jdown = 12+numsta
            CALL setcr_mn(jover,jdown )
            IF(OUTPUT_CONTROL) THEN
              call &
     &             addstr_f("(!)Copy control: Automatic laserjet copies only. " )
            ELSE
              call &
     &             addstr_f("(!)Copy control: Make screen plot and pause.   " )
            ENDIF
            call addstr_f("  Comparison (M)odel: " )
            call &
     &           addstr_f(hfeop_cmp_file_name(1:trimlen(hfeop_cmp_file_name)) )
            call nl_mn()
!          endif
          jover = 0
          jdown = 17+numsta
          CALL setcr_mn(jover,jdown )
          call refresh_mn()
        ENDIF !comparison model
!
!
!       Handle setting the value scale to override the auto scale
        IF((iy.eq.17+numsta .and. &
     &     IX.GT.60. .and. c1.EQ.' ')  .or. &
     &     c1.eq.'C'   .or. c1.eq.'A' .or. c1.eq.'E') THEN !Scale setting
          jover = 0
          jdown = numsta+14
          CALL setcr_mn(jover,jdown )
          IF (c1 .EQ. ' ') THEN
            IF      (IX .LT. 68) THEN
              c1 = 'C'
            ELSE if (ix .lt. 76) then
              c1 = 'A'
            else
              c1 = 'E'
            END IF
          END IF
          OK = .FALSE.
!
!         Handle atmosphere scale
          DO WHILE(c1.eq.'A' .and. .not.OK)  !Get atmos scale
            DEFAULT_A_SCALE = .FALSE.
            SET_SCALE_INT_A = .TRUE.
            call &
     &           addstr_f("Set scale (G)lobally or (I)nteract for each plot? " )
!!            call nl_mn
            CALL SENKR_MN(IX,IY,ICHR )
            c1 = cchar(4:4)
            call casefold(c1 )
            IF(c1.eq.'G') SET_SCALE_INT_A = .FALSE.
            IF(.NOT.SET_SCALE_INT_A) THEN
              call nl_mn()
              jover = 0
              jdown = 14+numsta
              CALL setcr_mn(jover,jdown )
              call &
     &             addstr_f("Atmosphere scale in psec: Min,Max (-2000ps to 2000ps valid) ? " )
              call getstr_f(bufstr )
              READ(bufstr,*,IOSTAT=ios) ATMOS_MIN,ATMOS_MAX
              if ( ios .ne. 0 ) then
                   call end_mn()
                   call ferr ( INT2(ios), "Invalid scale values", INT2(0), &
     &                  INT2(0) )
                   goto 910 ! This ugly 'goto' cause whole menu to be rewritten
              end if
!             Check for reasonable values
              IF( ATMOS_MIN .lt. ATMOS_MAX .and. &
     &           ATMOS_MIN .gt. -2010.    .and. &
     &           ATMOS_MAX .lt.  2010. )  OK = .TRUE.
            ENDIF
          ENDDO
!
!         Handle eop scale
          DO WHILE(c1.eq.'E' .and. .not.OK)  !Get eop scale
            DEFAULT_EO_SCALE = .FALSE.
            call nl_mn()
            jover = 0
            jdown = 14+numsta
            jdown_save = jdown
            call setcr_mn(jover,jdown )
            call &
     &           addstr_f("EOP Vertical Scale: xp_min,xp_max,yp_min,yp_max,ut_min,ut_max" )
            call nl_mn()
            jdown = jdown+1
            call setcr_mn(jover,jdown )
            call &
     &           addstr_f("mas for pole, mts for ut1 - six values " )
            call nl_mn()
            jdown=jdown+1
            call setcr_mn(jover,jdown )
            call addstr_f("? " )
            jover = 4
            call setcr_mn(jover,jdown )
            call getstr_f(bufstr )
            READ(bufstr,*,IOSTAT=ios) xp_scale_min,xp_scale_max, &
     &      yp_scale_min,yp_scale_max,ut1_scale_min,ut1_scale_max
!           Check for reasonable values
!
!           Try for horizonal scale.
            jover = 0
            jdown = jdown+1
            call setcr_mn(jover,jdown )
            call addstr_f("EOP Time Scale: Set? (y/(n))" )
            jdown = jdown+1
            call getstr_f(bufstr )
            c1 = bufstr(1:1)
            call casefold(c1 )
            if(c1.eq.'Y') then !get scale
              jdown = jdown+1
              ios = 1
              do while (ios.ne.0)
                call setcr_mn(jover,jdown )
                call addstr_f("Start time (ymdhm) ?" )
                call getstr_f(bufstr )
                read(bufstr,*,iostat=ios) start_time
              enddo
              ios = 1
              jdown = jdown+1
              do while (ios.ne.0)
                call setcr_mn(jover,jdown )
                call addstr_f("Stop  time (ymdhm) ?" )
                call getstr_f(bufstr )
                read(bufstr,*,iostat=ios) stop_time
              enddo
            else
              start_time(1) = -1
            endif
!
            jover = 0
            jdown = 14+numsta
            CALL setcr_mn(jover,jdown )
            WRITE(bufstr,'( &
     &           "Set vertical scale: (C)lock, (A)tmos (E)OP")')
            call addstr_f(bufstr )
!!            call reset_screen(jdown_save)
!
            IF( xp_scale_min .lt. xp_scale_max .and. &
     &          yp_scale_min .lt. yp_scale_max .and. &
     &         ut1_scale_min .lt.ut1_scale_max) ok = .true.
            goto 910 !This ugly 'goto' cause whole menu to be rewritten.
          ENDDO
!
!         Handle clock scale
          DO WHILE(c1.eq.'C' .and. .not.OK)  !Get clock scale
            DEFAULT_C_SCALE = .FALSE.
            SET_SCALE_INT_C = .TRUE.
            call &
     &           addstr_f("Set scale (G)lobally or (I)nteract for each plot? " )
!!            call nl_mn
            CALL SENKR_MN(IX,IY,ICHR )
            c1 = cchar(4:4)
            IF(c1.eq.'G') SET_SCALE_INT_C = .FALSE.
            IF(.NOT.SET_SCALE_INT_C) THEN
              call nl_mn()
              call addstr_f("Clock scale in psec: Min,Max ? " )
              call getstr_f(bufstr )
              READ(bufstr,*,IOSTAT=ios) CLOCK_MIN,CLOCK_MAX
              if ( ios .ne. 0 ) then
                   call end_mn()
                   CALL FERR ( INT2(IOS), "Invalid scale values", INT2(0), &
     &                  INT2(0) )
                   goto 910 !This ugly 'goto' cause whole menu to be rewritten.
              end if
!             Check for reasonable values
              IF( CLOCK_MIN .lt. CLOCK_MAX ) OK = .TRUE.
            ENDIF
          ENDDO
!
            jover = 0
            jdown = 14+numsta
            CALL setcr_mn(jover,jdown )
            WRITE(bufstr,'( &
     &           "Set vertical scale: (C)lock, (A)tmos (E)OP")')
            call addstr_f(bufstr )
!!          CALL RESET_SCREEN(jdown)
          jover = 0
          jdown = 17+numsta
          CALL setcr_mn(jover,jdown )
        ENDIF
!
        IF( IY.EQ.17+numsta     .and. &
     &     IX.LE.9.          .and. &
     &     c1.EQ.' ')   c1 = '/'
!
        IF( (IY.EQ.18+numsta    .and.IX.ge.0.         .and. &
     &     IX.le.6.         .and.c1.EQ.' ' ) .or. c1.EQ.'O')CALL &
     &     RUN_PROG('OPTIN', 'PASS', INT2(0) )
!
        IF( (IY.EQ.18+numsta .and.IX.ge.10.and.IX.le.20.and. &
     &     c1.EQ.' ' ) .or. c1.EQ.'L') then
          iconti=-2
          call use_buffer( iconti, INT2(1), 'OWC' )
          CALL RUN_PROG( 'SETFL', 'PASS', INT2(0) )
        endif
!
        IF( (IY.EQ.18+numsta .and. IX.ge.24.and.IX.le.34.and.c1.EQ.' ' ) .or. &
     &     c1.EQ.'T')CALL RUN_PROG( 'SLEND', 'PASS', INT2(0) )
!
        IF( (IY.EQ.18+numsta .and. IX.ge.38.and.IX.le.52.and.c1.EQ.' ' ) .or. &
     &     c1.EQ.'Q')CALL RUN_PROG( 'GLOBL', 'PASS', INT2(0) )
!
        IF((IY.eq.17+numsta .and. IX.GE.12.and.ix.le.30.and. &
     &     c1.eq.' ') .or. c1.eq.'S') THEN
          jover = 0
          jdown = 13+numsta
          CALL setcr_mn(jover,jdown )
          IF(SAVE_PLOT_FILES) &
     &       THEN
!           If web plots turned on, saving plots cannot be turned off.
            If(.not. do_web_plots) then
              SAVE_PLOT_FILES = .FALSE.
              call addstr_f("(S)ave plot files: NO " )
            Endif
          ELSE
            SAVE_PLOT_FILES = .TRUE.
            call addstr_f("(S)ave plot files: YES" )
          ENDIF
          jover = 0
          jdown = 17+numsta
          CALL setcr_mn(jover,jdown )
        ENDIF
!
        IF((IY.eq.17+numsta .and. IX.GE.30.and.ix.le.40.and. &
     &     c1.eq.' ') .or. c1.eq.'$') THEN
          jover = 0
          jdown = 13+numsta
          CALL setcr_mn(jover,jdown )
          IF(do_web_plots) &
     &       THEN
            save_plot_files = .false.
            do_web_plots = .false.
            call addstr_f("(S)ave plot files: NO        ($)Do Web plots: NO " )
          ELSE
            save_plot_files = .true.
            do_web_plots = .true.
            call addstr_f("(S)ave plot files: YES       ($)Do Web plots: YES" )
          ENDIF
          jover = 0
          jdown = 17+numsta
          CALL setcr_mn(jover,jdown )
        ENDIF
      END DO
!
      if (output_control) then
        jover = 0
        jdown = 20+numsta
        CALL setcr_mn(jover,jdown )
        call &
     &       addstr_f("Enter printer ID (laser3,laser4,...(default laser2)): " )
        call getstr_f(laserid )
        if (laserid.eq.' ') laserid = 'laser2'
      endif
!
      JDOWN = 0
      CALL RESET_SCREEN ( JDOWN )
!
      RETURN
      END  !#!  MENU  #!#
