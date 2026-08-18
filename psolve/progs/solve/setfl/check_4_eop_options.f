      SUBROUTINE CHECK_4_EOP_OPTIONS ( IX, IY, C1, REDO_PAGE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      INCLUDE 'solve.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
!
      CHARACTER C1*1, BUFFER*79
      INTEGER*4 IX, IY
      LOGICAL*2 REDO_PAGE, GO_ON, IROTF, IDSP, TURN_OFF_SITE_ESTIMATES
      REAL*8    FIRST_TIME, LAST_TIME, FJLDY, FJDOBS, LJDOBS, TROT_OLD
      INTEGER*2 I, J, I2, IM, ID, IYR, IH, IMIN
      INTEGER*4 IOS
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! 4.  HISTORY
!
!     MODIFICATIONS:
!
!     02-SEP-97 jwr: New code to support changing the eop of the eop
!                    polynomial.
!     03-SEP-97 pet: Partly petified comments. Improved interface for @-option
!     23-SEP-98 pet: Corrected a bug: previous version set one more intervals
!                    than necessary when > option was in use.
!
      REDO_PAGE = .FALSE.
      GO_ON     = .TRUE.
      TURN_OFF_SITE_ESTIMATES = .FALSE.
!
      IF ( C1.EQ.' ' ) RETURN
!
      IF     (            c1 .eq. '#') then !polynomial parameterizaton
        eop_style(1) = eop__poly
!
! ----- Turn on xpole, ypole, and ut1 offset and ut1 rate.
!
        DO I = 1, ROT_BIT_WORDS
           DO J=1,3
              LROT(i,j) = 0
           ENDDO
        ENDDO
!
        IDSP = IROTF( INT2(1), INT2(1), INT2(1), LROT)
        IDSP = IROTF( INT2(1), INT2(2), INT2(1), LROT)
        IDSP = IROTF( INT2(1), INT2(3), INT2(1), LROT)
        IDSP = IROTF( INT2(1), INT2(3), INT2(2), LROT)
        redo_page    = .true.
        go_on        = .false.
        turn_off_site_estimates = .true.
!
      else if(go_on .and. c1 .eq. '/') then !global rate and segments
        eop_style(1) = eop__rates_and_segs
        go_on        = .false.
        redo_page    =.true.
        turn_off_site_estimates = .true.
!
      else if(go_on .and. c1 .eq. '%') then !only segments
        eop_style(1) = eop__segs_only
        go_on        = .false.
        redo_page    =.true.
        turn_off_site_estimates = .true.
!
      else if(go_on .and. c1 .eq. '|') then !sine wave style
        eop_style(1) = eop__sine
        nrot_a1(1)   = 1
        nrot_a1(2)   = 1
        go_on        = .false.
        redo_page    =.true.
        turn_off_site_estimates = .true.
!
      else if(go_on .and. c1 .eq. '@') then !reset polynomial epoch
!
! ----- Clear the display.
!
        CALL SETCR_MN ( 0, 0 )
        CALL CLEAR_MN()
        TROT_OLD = TROT(1)
!
! ----- Get the start and stop times of this session
!
        CALL OBSTM ( FJDOBS, LJDOBS )
!
! ----- Display the current epoch.
!
        IOS = 1
        DO WHILE(IOS.NE.0)
           CALL EPOC ( IM, ID, IYR, IH, IMIN, TROT(1) )
           WRITE ( BUFFER, '("The current polynomial UTC eopch is:", i3,'// &
     &                     '"/",i2,"/ ",i2,i3,":",i2)' ) IYR, IM, ID, IH, IMIN
           CALL WRITE_SCREEN ( BUFFER )
           CALL EPOC ( IM, ID, IYR, IH, IMIN, FJDOBS )
           WRITE ( BUFFER, '("The start of this session UTC is   :", i3,'// &
     &                     '"/",i2,"/ ",i2,i3,":",i2)' ) IYR, IM, ID, IH, IMIN
           CALL WRITE_SCREEN ( BUFFER )
           CALL EPOC ( IM, ID, IYR, IH, IMIN, LJDOBS )
           WRITE ( BUFFER, '("The end of this session UTC is     :", i3,'// &
     &                     '"/",i2,"/ ",i2,i3,":",i2)' ) IYR, IM, ID, IH, IMIN
           CALL  WRITE_SCREEN ( BUFFER )
           CALL  WRITE_SCREEN ( '------------------------------------------'// &
     &                          '----------' )
           CALL  WRITE_SCREEN ( 'Please enter new EOP UTC epoch: must be '// &
     &                          'within the session +/- one hour.' )
           CALL SETCR_MN ( 0, 6 )
           CALL ADDSTR_F ( 'New EOP epoch (format: YY MM DD HH MM '// &
     &                     'or / to quit) >> ' )
           BUFFER = ' '
           CALL GETSTR_F ( BUFFER )
           IF ( BUFFER(1:1) .NE. '/' ) THEN
                READ(BUFFER,*,IOSTAT=IOS) IYR,IM,ID,IH,IMIN
                IF ( IOS .EQ. 0 ) THEN
                     TROT(1) = FJLDY(IM,ID,IYR)+(IH+IMIN/60.D0)/24.D0
                     IF ( TROT(1)+1./24. .LT. FJDOBS .OR. &
     &                    TROT(1)-1./24. .GT. LJDOBS      ) THEN
                        CALL SETCR_MN ( 0, 8 )
                        CALL WRITE_SCREEN ( 'Outside acceptable time window '// &
     &                                      '- try again!' )
                        CALL WRITE_SCREEN ( 'Return to continue' )
                        CALL GETSTR_F(BUFFER )
                        CALL SETCR_MN ( 0, 0 )
                        CALL CLEAR_MN()
                        IOS     = 1
                       TROT(1) = TROT_OLD
                     ENDIF
                   ELSE IF ( IOS .NE. 0 ) THEN
                     CALL SETCR_MN ( 0, 8 )
                     CALL WRITE_SCREEN ( 'Error during parsing input line '// &
     &                                   '- try again!' )
                     CALL WRITE_SCREEN ( 'Return to continue' )
                     CALL GETSTR_F(BUFFER )
                     CALL SETCR_MN ( 0, 0 )
                     CALL CLEAR_MN()
                 ENDIF
              ELSE
               IOS = 0
           ENDIF
        ENDDO
        GO_ON        = .FALSE.
        REDO_PAGE    =.TRUE.
!
      ELSE IF ( GO_ON .AND. C1 .EQ. '>' ) THEN
        if(eop_style(1) .eq. eop__rates_and_segs  .or. &
     &     eop_style(1) .eq. eop__segs_only     ) then
!
! ------- Reset and clear the screem
!
 910      CONTINUE
          CALL SETCR_MN ( 0, 0 )
          CALL CLEAR_MN()
          CALL CLRCH ( BUFFER )
          BUFFER = "Interval for segmented EOP parameterization (hours)? "
          CALL WRITE_SCREEN ( BUFFER )
          CALL SETCR_MN ( 55, 0 )
          CALL CLRCH ( BUFFER )
          CALL GETSTR_F ( BUFFER )
          IF ( ILEN(BUFFER) .EQ. 0 ) THEN
               GO_ON = .FALSE.
               REDO_PAGE    =.TRUE.
               GOTO 810
          END IF
!
          READ ( BUFFER, *, IOSTAT=IOS ) ROT_INTERVAL(1)
          IF ( IOS .NE. 0 ) GOTO 910
          ROT_INTERVAL(1) = ROT_INTERVAL(1)/24.D0
          ROT_INTERVAL(2) = ROT_INTERVAL(1)
          CALL OBSTM ( FIRST_TIME, LAST_TIME )
          NROT_A1(1) = (LAST_TIME - TROT_A1)/ROT_INTERVAL(1) + 1
          IF ( TROT_A1 + ROT_INTERVAL(1)*(NROT_A1(1)-1) .LT. LAST_TIME ) THEN
               NROT_A1(1) = NROT_A1(1)+1
          END IF
          ROT_INTERVAL(2) = ROT_INTERVAL(1)
          NROT_A1(2)=NROT_A1(1)
          GO_ON = .FALSE.
          REDO_PAGE    =.TRUE.
        ENDIF
!
      else if(go_on .and. c1 .eq. '$') then
        if(eop_style(1) .eq. eop__rates_and_segs  .or. &
     &     eop_style(1) .eq. eop__segs_only     ) then
!         Reset and clear the screem
          CALL setcr_mn(0,0 )
          CALL clear_mn()
          write ( buffer, 110 ) SEOCNST(1)
 110      FORMAT ( "Polar motion rate constraint (mas/day,nominal) ", &
     &             f10.2, ") ? " )
          call write_screen(buffer )
          call setcr_mn(60,0 )
          BUFFER = ' '
          CALL GETSTR_F ( BUFFER )
          IF ( BUFFER(1:1) .NE. ' ' ) THEN
               READ ( BUFFER, *, IOSTAT=IOS ) SEOCNST(1)
               IF ( IOS.NE.0 ) SEOCNST(1) = 0.D0
          END IF
          IF ( SEOCNST(1) .LE. 0.D0 ) THEN
               SEOCNST(1) = 0.D0
               CALL SBIT( CONSTRAINT_BITS, INT2(4), INT2(0) )
             ELSE
               CALL SBIT( CONSTRAINT_BITS, INT2(4), INT2(1) )
          ENDIF
          GO_ON = .FALSE.
          REDO_PAGE    =.TRUE.
        endif
      else if(go_on .and. c1 .eq. '&') then
        if(eop_style(1) .eq. eop__rates_and_segs  .or. &
     &     eop_style(1) .eq. eop__segs_only     ) then
!         Reset and clear the screem
          CALL setcr_mn(0,0 )
          CALL clear_mn()
          write(buffer, &
     &          '("UT1 rate constraint (ms/day, nominal ",f10.3,") ? ")')SEOCNST(2)
          call write_screen(buffer )
          call setcr_mn ( 51, 0 )
          buffer = ' '
          CALL GETSTR_F ( BUFFER )
          IF ( BUFFER(1:1) .NE. ' ' ) THEN
               READ ( BUFFER, *, IOSTAT=IOS )  SEOCNST(2)
               IF ( IOS.NE.0 )  SEOCNST(2) = 0.D0
          END IF
!
          If(SEOCNST(2) .le. 0.d0) THEN
            SEOCNST(2) = 0.D0
            CALL SBIT( CONSTRAINT_BITS, INT2(5), INT2(0) )
          ELSE
            CALL SBIT( CONSTRAINT_BITS, INT2(5), INT2(1) )
          ENDIF
          go_on = .false.
          redo_page    =.true.
        endif
        redo_page    =.true.
      endif
!
      EOP_STYLE(2) = EOP_STYLE(1)
!
      IF ( TURN_OFF_SITE_ESTIMATES ) THEN
           DO I =1,STA_BIT_WORDS
              DO J = 1,3
                 LSITEC(I,J) = 0
              ENDDO
           ENDDO
      ENDIF
!
      SOCOM_PLUS_FIRST = SPL__INIT
      CALL SOCOM_EXT()
      CALL USE_COMMON ('OWC' )
      CALL PARCN()
 810  CONTINUE
!
      RETURN
      END  !#!  CHECK_4_EOP_OPTIONS  #!#
