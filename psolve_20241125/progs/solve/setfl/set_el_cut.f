      SUBROUTINE SET_EL_CUT ( SITE_DEP_EL_CUT, ELMIN, ELVCUT, WVMASK, NUMSTA, &
     &                        ISITN, PI )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SET_EL_CUT PROGRAM SPECIFICATION
!
! 1.1 Set elevation cutoffs and WVR mask interactively.
!
! 1.2 REFERENCES:
!
! 2.  SET_EL_CUT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2 SITE_DEP_EL_CUT
      REAL*8 ELVCUT(MAX_ARC_STA)
      REAL*8 ELMIN,PI
      INTEGER*2 NUMSTA,ISITN(4,MAX_ARC_STA),WVMASK(MAX_ARC_STA)
!
! ELMIN - Overall minimum elevation
! ELVCUT - Minimum elevation by station
! ISITN - Array of station names
! PI - 3.14159.......
! SITE_DEP_EL_CUT - True if we are to set elevation cutoffs by station
! WVMASK - WVR masks by station
!
! 2.3 OUTPUT Variables:
!
! ELMIN - Newly selected minimum elevation
! ELVCUT - Newly selected elevation cutoffs by station
! WVMASK - Newly selected WVR masks by station
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rmflg
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,ICHAR,J,N
      INTEGER*4  IOS
      LOGICAL*2 NO_QUIT
      integer*4 ix,iy,ich,ns4
      character*4 cch
      character*2 cchar
      character*79 bufstr
      equivalence (ichar,cchar)
      equivalence (ich,cch)
      CHARACTER  STR*54, GET_VERSION*54
      INTEGER*2   INT2_ARG
      INTEGER*4   I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  890206  Created
!   JWR  890209  WVR mask logic added
!   kdb  950809  Delete lines from
!                wvr warning to make more
!                room for up to 32 sites.
!   jwr  2005:05:05: I_LEN added to buffer reads
!
! 5.  SET_EL_CUT PROGRAM STRUCTURE
!
      NO_QUIT = .TRUE.
      DO WHILE(NO_QUIT)
!
! Write header and current values
!
        CALL setcr_mn ( 0, 0 )
        CALL CLEAR_MN()
        CALL ADDSTR_F ( " Elevation Cutoffs " )
!
        STR = GET_VERSION()
        CALL SETCR_MN ( 79-I_LEN(STR), 0 )
        CALL REVERSE_ON_MN()
        CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
        CALL REVERSE_OFF_MN()
        CALL NL_MN()
        CALL NL_MN()
        CALL NL_MN()
!
      IF ( SITE_DEP_EL_CUT ) THEN
          call addstr_f("Site dependent elevation cutoffs" )
          call nl_mn()
          call nl_mn()
          call addstr_f("Site       Elevation (Deg.) WVMASK" )
          call nl_mn()
          DO I =1,NUMSTA
            WRITE(bufstr, &
     &      '(1X,4A2,F8.1,10X,O8)')(ISITN(J,I),J=1,4),ELVCUT(I)*180.d0/PI,WVMASK(I)
            call addstr_f(bufstr )
            call nl_mn()
          Enddo
!
! Display instructions for changing current values
!
          call nl_mn()
          call &
     &         addstr_f("(S)et Values    S(W)itch to a single value" )
          call nl_mn()
          call &
     &         addstr_f("(O)ptin   (L)ast page   (T)erminate   Least s(Q)uares" )
          call nl_mn()
          call nl_mn()
          call addstr_f("WARNING: NEWDB does not save WVR masks!!" )
          ns4 = numsta
          CALL setcr_mn ( 0, NS4+7 )
!
! Pick up character selected by user
!
          ICHAR = 0
          IY = 0
          n=numsta+7
          DO WHILE(.not.(CCHAR(1:1).eq. &
     &               ' '.or.CCHAR(1:1).eq.'O'    .or. &
     &                   CCHAR(1:1).eq.'L'    .or. &
     &                   CCHAR(1:1).eq.'T'    .or. &
     &                   CCHAR(1:1).eq.'Q'    .or. &
     &                   CCHAR(1:1).eq.'W'    .or. &
     &                   CCHAR(1:1).eq.'S'         ))
            CALL SENKR_MN(IX,IY,ICH )
            cchar(1:1) = cch(4:4)
          ENDDO
!
          IF(CCHAR(1:1).eq.' '.and.iy.eq.n) Then
            if(ix.lt.12) CCHAR(1:1) = 'S'
            IF(IX.ge.16) CCHAR(1:1) = 'W'
          ENDIF
          if(cchar(1:1).eq.' '.and.iy.eq.n+1) then
            if(ix.lt.7) cchar(1:1) = 'O'
            if(ix.ge.10.and.ix.le.20) cchar(1:1)='L'
            if(ix.ge.24.and.ix.le.34) cchar(1:1)='T'
            if(ix.ge.38.and.ix.le.52) cchar(1:1)='Q'
          endif
!
! If done, then quit
!
          IF(CCHAR(1:1) .eq. 'L') NO_QUIT = .false.
          IF(CCHAR(1:1) .eq. 'O') call run_prog( 'OPTIN', 'PASS', INT2(0) )
          IF(CCHAR(1:1) .eq. 'T') call run_prog( 'SLEND', 'PASS', INT2(0) )
          IF(CCHAR(1:1) .eq. 'Q') call run_prog( 'GLOBL', 'PASS', INT2(0) )
!
! Set global cutoff value
!
          IF(CCHAR(1:1) .eq. 'W') then
            SITE_DEP_EL_CUT = .false.
            DO I = 1,NUMSTA
              ELVCUT(I) = ELMIN
            ENDDO
          Endif
!
! Set new cutoff values
!
          IF(CCHAR(1:1) .eq. 'S') then
            CALL setcr_mn ( 0, 0 )
            CALL clear_mn()
            call addstr_f("New elvation cutoff values - degrees:" )
            call nl_mn()
            DO I=1,NUMSTA
              IOS = -1
              DO WHILE ( IOS .LT. 0 )
                 call nl_mn()
              WRITE(bufstr,'(4A2,2X,"Elevation ?")') (ISITN(J,I),J=1,4)
                call addstr_f(bufstr(:22) )
                call getstr_f(bufstr )
                READ ( BUFSTR(1:I_LEN(BUFSTR)), *, IOSTAT=IOS ) ELVCUT(I)
                WRITE(bufstr, &
     &          '(4A2,2X,"WV MASK (7 octal digits) ?")')(ISITN(J,I),J=1,4)
                call addstr_f(bufstr(:37) )
                call getstr_f(bufstr )
                READ ( BUFSTR(1:I_LEN(BUFSTR)), '(O7)', IOSTAT=IOS ) WVMASK(I)
              ENDDO
              ELVCUT(I) = ELVCUT(I)*PI/180.D0
            ENDDO
            ELMIN = 0.D0
          ENDIF
          if(iy.ge.5.and.iy.le.5+numsta.and.ix.ge.12.and.ix.le.27) then
            CALL setcr_mn ( 0, 0 )
            CALL clear_mn()
            call addstr_f("New elvation cutoff values - degrees:" )
            call nl_mn()
            i=iy-5
            IOS = -1
            DO WHILE (IOS .lt. 0)
              call nl_mn()
              WRITE(bufstr,'(4A2,2X,"Elevation ?")') (ISITN(J,I),J=1,4)
                call addstr_f(bufstr(:22) )
                call getstr_f(bufstr )
                READ ( BUFSTR(1:I_LEN(BUFSTR)), *, IOSTAT=IOS ) ELVCUT(I)
                WRITE(bufstr, &
     &          '(4A2,2X,"WV MASK (7 octal digits) ?")')(ISITN(J,I),J=1,4)
                call addstr_f(bufstr(:37) )
                call getstr_f(bufstr )
                READ ( BUFSTR(1:I_LEN(BUFSTR)), '(O7)', IOSTAT=IOS ) WVMASK(I)
              ENDDO
              ELVCUT(I) = ELVCUT(I)*PI/180.D0
           endif
!
! Re-display current values
!
        else
          call addstr_f(" Site independent elevation cutoff" )
          call nl_mn()
          call nl_mn()
          WRITE(bufstr, &
     &    '(" Elevation cutoff = ",F4.1," Deg.")')ELMIN*180.D0/PI
          call addstr_f(bufstr )
          call nl_mn()
          call nl_mn()
          call &
     &         addstr_f("(S)et Values    S(W)itch to site dependent" )
          call nl_mn()
          call &
     &         addstr_f("(O)ptin   (L)ast page   (T)erminate   Least s(Q)uares" )
          call nl_mn()
          call nl_mn()
          CALL setcr_mn ( 1, 7 )
!
          ICHAR = 0
          IY = 0
          n=7
          DO WHILE(.not.((IY.ge.n.and.iy.le.n+1.and.CCHAR(1:1).eq. &
     &               ' ').or.CCHAR(1:1).eq.'O'    .or. &
     &                   CCHAR(1:1).eq.'L'    .or. &
     &                   CCHAR(1:1).eq.'T'    .or. &
     &                   CCHAR(1:1).eq.'Q'    .or. &
     &                   CCHAR(1:1).eq.'W'    .or. &
     &                   CCHAR(1:1).eq.'S'         ))
            CALL SENKR_MN(IX,IY,ICH )
            cchar(1:1) = cch(4:4)
          ENDDO
!
          IF(CCHAR(1:1).eq.' '.and.iy.eq.n) Then
            if(ix.lt.12) CCHAR(1:1) = 'S'
            IF(IX.ge.16) CCHAR(1:1) = 'W'
          ENDIF
          if(cchar(1:1).eq.' '.and.iy.eq.n+1) then
            if(ix.lt.7) cchar(1:1) = 'O'
            if(ix.ge.10.and.ix.le.20) cchar(1:1)='L'
            if(ix.ge.24.and.ix.le.34) cchar(1:1)='T'
            if(ix.ge.38.and.ix.le.52) cchar(1:1)='Q'
          endif
          CALL USE_COMMON ( 'OWC' )
          IF ( CCHAR(1:1) .EQ. 'L' ) NO_QUIT = .FALSE.
          IF ( CCHAR(1:1) .EQ. 'O' ) CALL RUN_PROG ( 'OPTIN', 'PASS', INT2(0) )
          IF(CCHAR(1:1) .eq. 'T') call run_prog( 'SLEND', 'PASS', INT2(0) )
          IF(CCHAR(1:1) .eq. 'Q') call run_prog( 'GLOBL', 'PASS', INT2(0) )
          IF(CCHAR(1:1) .eq. 'W') SITE_DEP_EL_CUT = .true.
          IF(CCHAR(1:1) .eq. 'S') then
            CALL setcr_mn ( 0, 0 )
            CALL clear_mn()
            IOS =-1
            DO WHILE( IOS .lt. 0)
              call addstr_f("New elvation cutoff value - degrees:" )
              call nl_mn()
              call addstr_f("Value  ?" )
              call getstr_f(bufstr )
              READ ( BUFSTR(1:I_LEN(BUFSTR)), *, IOSTAT=IOS ) ELMIN
            ENDDO
            ELMIN = ELMIN*PI/180.D0
            DO I = 1,NUMSTA
              ELVCUT(I) = ELMIN
            ENDDO
          ENDIF
        ENDIF
      ENDDO
!
      RETURN
      END
