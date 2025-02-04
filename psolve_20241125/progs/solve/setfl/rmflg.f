      SUBROUTINE RMFLG ( IKONT, NPMAX )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  RMFLG PROGRAM SPECIFICATION
!
! 1.1 Display and set the polar motion, UT1,
!     earth tide,gamma,precession and nutation flags.
!
! 1.2 REFERENCES:
!
! 2.  RMFLG INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 IKONT
      INTEGER*4 NPMAX
!
! IKONT - Option flag (to run least squares or OPTIN frm here)
! NPMAX - Maximum number of parameters allowed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'oborg.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: setfl
!       CALLED SUBROUTINES: rotfl,conpg,nutat,set_el_cut,parxc,sldb
!
! 3.  LOCAL VARIABLES
!
!
      INTEGER*2 JKONT, ICHR, JCHAR, IDUM2, IROTF, I, KK, K, JJ, II, ILOC, &
     &          KBITN, KERR, LEPS, LPSI, L, NLTIDE, NLCOR, ICOD, NLINE, J, &
     &          ICOUNT, IDIR, IREGB, IREGA, IPOS
      INTEGER*4 IX, IY, IXD, IYD, IDUM, ICH, ILLOC, NCNT, IPOS4, NLREL, &
     &          IONF(3), IMINX_CON, IMINX_INT, IY1, IX_ALT, IY_ALT
      CHARACTER*4 CCH
      CHARACTER*2 CCHAR
      EQUIVALENCE ( ICHR,CCHAR)
      EQUIVALENCE ( ICH,CCH)
!
      LOGICAL*2 IP,KBIT,kston,redo_page,alturnate_position
      CHARACTER*3 RES_FLG, COR_FLG, NORM_OUT_FLAG, NORM_ZERO_FLAG
      CHARACTER   STA_CONST_FLAG*3, MAM(2)*2, BUFSTR*79, BLANK*79
!
      INTEGER*2 IDSP(16), KKBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+4)
      INTEGER*2 NSTPLT(3),NSOLVE(3)
      INTEGER*2 IRC(2,3)
      INTEGER*2 IMINX_TOG(3), IMAXX_CON, IMAXX_INT, IMAXX_TOG(3)
      CHARACTER*10 EOP_DISP_A1(2)
      CHARACTER    DATYP_STR*32, SUPMET_STR*32
!
!     Scheduling of SLVEB moved here, from PROC:
!     Specifications follow (5 lines)
!
!     Modified so that select baselines and select
!     data bases can be passed into 'RMFLG'.
!     Data type for solution put under character (:).
!
      LOGICAL*4   CHECK_STABIT, DATYP_INQ, WAS_REFCLO, F_SUPMET
      REAL*8      REG
      INTEGER*2   IREG(2), J2, ILD, TRIMLEN
      INTEGER*4   J1
      EQUIVALENCE (REG,IREG,IREGA),(IREG(2),IREGB)
      INTEGER*4    I_LEN
!
      DATA NSTPLT/2HST,2HPL,2HT /,NSOLVE/2HSO,2HLV,2HE /
      DATA IRC/2HXW,2HOB,2HYW,2HOB,2HUT,2H1 /
      DATA EOP_DISP_A1 /"X/Y WOBBLE","UT1-TAI   "/
      DATA MAM /"ma"," m"/
!
!     Partial soft coding for the location of the earth orientation fields
!
      DATA IONF /35,52,74/
      DATA IMINX_TOG /13,40,57/, IMAXX_TOG /37,54,76/
      DATA IMINX_INT /13/, IMAXX_INT /48/
      DATA IMINX_CON /51/, IMAXX_CON /79/
!CCCCC
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!  JWR  820202  polar motion inset and delete logic improved
!  JWR  890203  site dependent el-cut off logic added.
!  KDB  90????  Implemented new style earth orientation parameterization
!  kdb  950728  Merge idatyp formats into write statements for
!               convenience of users tracking current data type
!               via last page data type field.
!  kdb  951025  Fix various problems with cursor positioning:
!                 - remove last blank line to decrease number of lines to 24
!                 - move output/input cgm lines to proper places on screen.
!                 - fix failure to return to optin with blank
!                 - fix tendency of W (delay weighting) option to write
!                   current status to wrong line
!                 - fix problems related to printing of elevation cutoff
!                      status (e.g., line too long if site independent
!                      elevation cutoff, ! status not printed if
!                      site dependent elevation cutoff)
!                 - fix cursor positioning problems related to ! status field
!  pet  970610  Fixed problems with losting positions if screen had width less
!               then 100 sumbols. Ordered options. Made appearance more
!               pleasant for eyes
!  jwr  970902  New code to support changing the eop of eop
!  jwr  971105  Minor changes added to provide correct work of an indicator:
!               are station positions estimated
!  pet  971128  Added diabling no-net-translation constraints (if they were
!               setup) when estimation of station coordinates for all stations
!               is lifted
!  pet  971203  Added logic for bypassing deselected station and logic for
!               setting status "deselected" for the stations with no selected
!               baselines
!  pet  980112  Small improvement: reference station for station position will
!               be re-set up after baseline deselection if the station
!               positions are estimated
!  pet  980203  Substituted hard-coded test of data type by DATYP_INQ
!  pet  980217  Added support of change data type in manu mode.
!  pet  980325  Added support of SET_ACM: setting, displaying and changing
!               a priori clock model
!  pet  980430  Added new option: (') -- change suppression method
!  pet  980709  Added new option: (-) -- change singularity check settings
!  pet  990429  disabled printing a warning message of loosing clock reference
!               station after station selection/deselection if clocks are
!               parameterized not in batch mode fashion
!  pet  1999.11.11  Removed Love numbers logic
!  pet  2000.01.25  Added support of toggling NORATE_FLAG.
!  pet  2000.07.04  Added compulsory initialization of Earth tide and nutation
!                   expansion estimation.
!  pet  2000.09.27  Fixed a bug: the previous version didn't allow to set up
!                   estimation of precession rate and relativity parameter
!                   gamma. It terminated abnormally when user tried to set
!                   estimation if nutation angles by using cursor.
!  pet  2011.01.07  Fixed a bug: the rprevious verrsion did not update &
!                   AUTO_SUP after baseline status was changed. It kept it. &
!                   This prevented restoring previously deselected baselines
!
!CCCCCCC
!
! 5.  RMFLG PROGRAM STRUCTURE
!
! --- Initialization. Initialize some archaic arrays: etimation of tides and
! --- some terms of nutation expansion: they are not supported any more,
! --- but it takes too much efforts to eradicate these variables entirely.
!
      ITDGLB = 0
      DO I = 1,STA_BIT_WORDS
         LTIDE(I,1) = 0
         LTIDE(I,2) = 0
         LTIDE(I,3) = 0
      END DO
      DO I=1,7
         IDPNUT(I) = 0
      END DO
      NFLEPS = 0
      NFLPSI = 0
      DO I=1,14
         FLEPS(I) = 0
         FLPSI(I) = 0
      END DO
!CCCC
!
! --- Check: was position of at least one component of at least one station
! --- estimated?
!
      KSTON = .FALSE.
      DO I=1,NUMSTA
!
! ------ Check: was the I-th station in solution?
!
         IF ( CHECK_STABIT ( I ) ) THEN
              DO J = 1,3
                 IF ( KBIT (LSITEC(1,J),I) ) KSTON = .TRUE.
              END DO
         END IF
      END DO
!
      ALTURNATE_POSITION = .FALSE.
      IDIR = 1
!
! --- Initialize IP so the screen is written.
!
      IP = .TRUE.
!
! --- See if 'RMFLG' has been entered only to get at 'select baselines'
! --- or 'select data bases'.
!
      IF(IKONT.EQ.-5 .OR. IKONT.EQ.-6) GO TO 51
      GO TO 50
   51 IF(IKONT.EQ.-5) IX = 42
      IF(IKONT.EQ.-6) IX = 55
      GO TO 1505
!
  50  CONTINUE
!
      CALL WRITE_EOP_MENU ( IP, NLINE )
!
      NLCOR = NLINE + 3
      NLTIDE = NLINE + 2
!
      ICOUNT=ICOUNT+1
      NLINE = NLINE + 2
!
! --- Display relativity and precession flags, and nutation option
!
      LPSI = KBITN( LNUT(1), INT2(1) )
      LEPS = KBITN( LNUT(1), INT2(2) )
      IF ( IP ) THEN
           WRITE ( BUFSTR, 510 ) LREL, LPREC, LPSI, LEPS
  510      FORMAT ( "Gamma, Precession rate    ",8X,2I2, &
     &               '    Nutation(.): Dpsi, Deps ',2I2)
           CALL ADDSTR_F(BUFSTR )
           CALL NL_MN()
      ENDIF
      NLREL = NLINE + 1
!
! --- Display print control flags.
!
      RES_FLG = 'OFF'
      COR_FLG = 'OFF'
      norm_out_FLAG = 'OFF'
      norm_zero_FLAG = 'OFF'
      sta_const_FLAG = 'OFF'
      IF(KBIT( IPRES, INT2(1) )) RES_FLG = 'ON '
      IF(KBIT( IPRES, INT2(3) )) COR_FLG = 'ON '
      IF(KBIT( IPRES, INT2(5))) norm_out_FLAG = 'ON '
      IF(KBIT( IPRES, INT2(6))) norm_zero_FLAG = 'ON '
      IF(KBIT( IPRES, INT2(7))) sta_const_FLAG = 'ON '
      IF ( IP ) then
           call nl_mn()
           WRITE(bufstr,550) RES_FLG, COR_FLG
  550      FORMAT("Print residu(A)ls: ",A3,13X,"Print corr. (M)atrix: ",A3)
           CALL ADDSTR_F(BUFSTR )
           CALL NL_MN()
      ENDIF
      IF ( IP ) THEN
           WRITE  ( BUFSTR, 551 ) NORM_OUT_FLAG, NORM_ZERO_FLAG
  551      FORMAT ( "Print (N)ormal Matrix: ",A3,9X, &
     &              "(Z)ero Normal Matrix: ",A3)
           CALL ADDSTR_F(BUFSTR )
           CALL NL_MN()
      ENDIF
!
! --- Display the minimum elevation angle and display option to
! --- switch between site positions and earth orientation solutions.
!
      IF ( IP ) then
         IF ( SITE_DEP_EL_CUT ) THEN
            call nl_mn()
            call addstr_f("(^)Elev. cutoff: " )
            call addstr_f("Site dependent " )
            call addstr_f("   Pick parameters: (!)Sites " )
            if ( kston ) then
                 call reverse_on_mn()
                 call addstr_f ( " ON" )
                 call reverse_off_mn()
               else
                 call reverse_on_mn()
                 call addstr_f ( "OFF" )
                 call reverse_off_mn()
            endif
            call addstr_f ( "  (#)UT1/PM" )
            call nl_mn()
          else
            IF ( ELMIN .GT. 0.001 ) then
               call nl_mn()
               WRITE(bufstr,'("(^)Elev. cutoff: Indep: ",F4.1, &
     &                        " deg")') ELMIN * 180.0D0/PI__NUM
               call addstr_f(bufstr(:trimlen(bufstr)) )
               call addstr_f("   Pick parameters: (!)Sites " )
               if ( kston ) then
                    call reverse_on_mn()
                    call addstr_f ( " ON" )
                    call reverse_off_mn()
                 else
                    call reverse_on_mn()
                    call addstr_f ( "OFF" )
                    call reverse_off_mn()
               endif
               call addstr_f ( "  (#)UT1/PM" )
               call nl_mn()
             ELSE
               call nl_mn()
               call addstr_f("(^)Elev. cutoff: None           " )
               call addstr_f("   Pick parameters: (!)Sites " )
               if ( kston ) then
                    call reverse_on_mn()
                    call addstr_f ( " ON" )
                    call reverse_off_mn()
                 else
                    call reverse_on_mn()
                    call addstr_f ( "OFF" )
                    call reverse_off_mn()
               endif
               call addstr_f ( "  (#)UT1/PM" )
              call nl_mn()
          ENDIF
        ENDIF
      ENDIF
      IF ( IP ) THEN
           CALL NL_MN()
           WRITE(bufstr,552) sta_const_flag
  552      FORMAT("Wea(K) Station Constraints: ",A3,"    ", &
     &            "(R)Use rate: " )
           CALL ADDSTR_F ( BUFSTR(1:I_LEN(BUFSTR)) )
           CALL ADDSTR_F ( " " )
           CALL REVERSE_ON_MN()
           IF ( NORATE_FLAG ) THEN
                CALL ADDSTR_F( "No"  )
              ELSE
                CALL ADDSTR_F( "Yes" )
           END IF
           CALL REVERSE_OFF_MN()
           CALL NL_MN()
      ENDIF
!
! --- Display the downweight delay flag.
!
      ILLOC = NLREL + 7
      IF ( IP ) CALL SETCR_MN ( 0, ILLOC )
      IF ( IDNWT.EQ.0.AND.IP ) THEN
           call blink_off_mn()
           WRITE(bufstr,2335)
 2335      FORMAT("Use normally (W)eighted delays ",4X,"Select: Baseline", &
     &             '-(C)lock offsets')
           CALL ADDSTR_F(BUFSTR )
           CALL NL_MN()
      ENDIF
      IF(IDNWT.EQ.1.AND.IP) then
        call addstr_f("Down-(W)eighted delays by 1.D9 " )
        call addstr_f("    Select: Baseline-(C)lock offsets" )
        call nl_mn()
      endif
      ILLOC = ILLOC + 1
!
! --- Display delay and rate options.
!
      IF ( IP ) THEN
           CALL SETCR_MN   ( 0, ILLOC+1 )
           CALL SHOW_DATYP ( IDATYP )
      END IF
!
! *** DISPLAY BASELINE AND DATA BASE OPTIONS
!
      IF(IP) CALL setcr_mn ( 33, ILLOC+1)
      IF(IP) then
      call addstr_f("  Select: (B)aselines, (X)Data bases" )
      call nl_mn()
      endif
  620 CONTINUE
!
!**** SET UP THE CONTROL LINE
      IF ( IP ) THEN
         CALL PARCN()
         CALL NL_MN()
         CALL ADDSTR_F ( 'Page: (E)Site       (S)ource       (O)ptions      '// &
     &                   '      (")Constraints' )
         CALL NL_MN()
         CALL ADDSTR_F ( '      (Q)Run least squares         (T)erminate '// &
     &                   'SOLVE    (<)A priori clock' )
         CALL NL_MN()
!
         CALL ADDSTR_F ( '      (+)Change data type          ' )
         CALL CLRCH      (         DATYP_STR )
         CALL DATYP_SHOW ( IDATYP, DATYP_STR )
         CALL REVERSE_ON_MN()
         CALL ADDSTR_F ( DATYP_STR(1:I_LEN(DATYP_STR)) )
         CALL REVERSE_OFF_MN()
         ILD = 21 - I_LEN(DATYP_STR)
         IF ( ILD .LE. 0 ) ILD=1
         CALL CLRCH ( BLANK )
         CALL ADDSTR_F ( BLANK(1:ILD) )
         IF ( SUPMET == SUPMET__META ) THEN
              CALL ADDSTR_F ( '(*) with status export' )
         END IF
         CALL NL_MN()
!
         CALL ADDSTR_F    ( "      (')Change suppression method " )
         CALL CLRCH       ( SUPMET_STR )
         CALL SUPMET_SHOW ( SUPMET, SUPMET_STR )
         CALL REVERSE_ON_MN()
         CALL ADDSTR_F ( SUPMET_STR(1:I_LEN(SUPMET_STR)) )
         CALL REVERSE_OFF_MN()
         ILD = 21 - I_LEN(SUPMET_STR)
         IF ( ILD .LE. 0 ) ILD=1
         CALL CLRCH ( BLANK )
         CALL ADDSTR_F ( BLANK(1:ILD) )
         CALL ADDSTR_F ( '(-) Singularity check' )
         CALL NL_MN()
         CALL NL_MN()
         WRITE ( BUFSTR, '(" Last page   Parms used / Max parms available:", &
     &                       I6,"/",I6)' ) NPARAM, NPMAX
         CALL ADDSTR_F ( BUFSTR )
      ENDIF
!
      NCNT = NLINE + 16
!
! --- If any global (batch) solution selections are turned on flag it.
!
      IF ( IP ) THEN
           IF ( INAMCG(1:1) .NE. ' ' ) THEN
                CALL SETCR_MN ( 50, NCNT-1 )
                CALL ADDSTR_F ( "Input CGM will be used" )
                CALL NL_MN()
           END IF
!
           IF ( IOCGM .NE. 0 ) THEN
                CALL SETCR_MN ( 50, NCNT )
                CALL ADDSTR_F ( "Output CGM will be created" )
           END IF
      END IF
!
!     Reset the curson to the bottom left corner of the page.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!      type *,' ix_alt =',ix_alt,' iy_alt =',iy_alt,' ncnt=',ncnt
!      type *,' alturnate_position = ',alturnate_position
!      pause
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      IF ( ALTURNATE_POSITION ) THEN
           ALTURNATE_POSITION = .FALSE.
           CALL SETCR_MN ( IX_ALT, IY_ALT )
         ELSE
           CALL SETCR_MN ( 0, NCNT )
      ENDIF
!
  650 CONTINUE
      ICOD = 1HA
      IP = .TRUE.
  660 CONTINUE
      CALL senkr_mn(IX,IY,ICH )
      cchar(1:1) = cch(4:4)
!
      CALL CHECK_4_EOP_OPTIONS ( IX, IY, CCHAR(1:1), REDO_PAGE )
      IF ( CCHAR(1:1) .EQ. '#' ) THEN
           KSTON = .FALSE.
           IF ( KCENTERMASS ) THEN
!
! ------------- Disable no-net-translatiuon constraints if it was setup if
! ------------- no station coordinates will be estimated.
!
                CALL SBIT ( DEFCMP, INT2(5), INT2(0) )
                KCENTERMASS = .FALSE.
                CALL USE_GLBFIL_3 ( 'OW'  )
                CALL USE_GLBFIL_4 (  'WC' )
           END IF
      END IF
      IF ( REDO_PAGE ) GOTO 50
!
      if(cchar(1:1) .eq. ' ' .and. iy.ge.1 .and. iy.le.3) then
        call toggle_eop_poly_flags(IY,IX,LROT,IDSP,redo_page )
        if(redo_page) then
          ix_alt = ix
          iy_alt = iy
          alturnate_position = .true.
          go to 50
        Endif
      Endif
!
! --- The following mysterious code handles moving to site parameter selecti
!
      IF (CCHAR(1:1) .EQ. 'E') CCHAR(1:1) = '1'
      IF (CCHAR(1:1) .EQ. 'I') CCHAR(1:1) = '1'
!
! --- The following looks for a senkr_mn returned character of 1 to 9.
! --- If found the page number is put into JKONT
!
      JKONT = 0
      IF('1'.LE. CCHAR(1:1) .AND. CCHAR(1:1) .LE. '9') THEN
        JKONT = JCHAR( ichr, INT2(1) ) - 48
        CCHAR(1:1) = 'I'
      ENDIF
!
! --- End of mysterious code.
!
!!      IF ( CCHAR(1:1) .EQ. 'R' ) GOTO 50
!
! --- Process Earth orientation command line
!
      IF ( CCHAR(1:1).EQ.'/'  .OR. &
     &     CCHAR(1:1).EQ.'U'       ) THEN
!
          IY = NLCOR-3
          IF(CCHAR(1:1).EQ.'/') IX = 20
          IF(CCHAR(1:1).EQ.'U') IX = 74
          CCHAR(1:1) = ' '
      END IF
      IF ( CCHAR(1:1).EQ.'A' .OR. CCHAR(1:1).EQ.'M'      ) GOTO 1400
      IF ( CCHAR(1:1).EQ.'I' .OR. CCHAR(1:1).EQ.'S' .OR. &
     &     CCHAR(1:1).EQ.'O' .OR. CCHAR(1:1).EQ.'"'      ) GOTO 1680
      IF (CCHAR(1:1) .EQ. 'Q' .OR. CCHAR(1:1).EQ.'T'     ) GOTO 1600
      IF (CCHAR(1:1) .EQ. ':' .OR. CCHAR(1:1).EQ.'B' .OR. &
     &    CCHAR(1:1) .EQ. 'X' .OR. CCHAR(1:1).EQ.';'     ) GOTO 1500
      IF (CCHAR(1:1) .EQ. 'W' .OR. CCHAR(1:1) .EQ. 'C'   ) GOTO 1450
      IF (CCHAR(1:1) .EQ. 'N' .OR. CCHAR(1:1) .EQ. 'Z'   ) GOTO 1420
      IF (CCHAR(1:1) .EQ. 'K') GO TO 1430
      IF (CCHAR(1:1) .EQ. 'R') GO TO 1440
      IF (CCHAR(1:1) .EQ. '!' .OR. CCHAR(1:1) .EQ. '#'   ) GOTO 1350
      if (cchar(1:1) .eq. '~'                            ) GOTO 1375
      IF (CCHAR(1:1) .EQ. '^'                            ) GOTO 1300
      IF (CCHAR(1:1) .EQ. '.'                            ) GOTO 1260
      IF (CCHAR(1:1) .EQ. '+'                            ) GOTO 1530
      IF (CCHAR(1:1) .EQ. '-'                            ) GOTO 1540
      IF (CCHAR(1:1) .EQ. '<'                            ) GOTO 1550
      IF (CCHAR(1:1) .EQ. "'"                            ) GOTO 1560
      IF (CCHAR(1:1) .EQ. "*"                            ) GOTO 1535
      IF (CCHAR(1:1) .NE. ' '                            ) GOTO 660
      IPOS = (IX-34)/2 + 1
      IF(IY.EQ.0) GO TO 50
      IF(IY.GE.NLREL + 16) GOTO 1600
      IF(IY.GE.NLREL + 14) GOTO 1680
      IF(IY.GE.NLREL + 12) GOTO 1500
      IF(IY.GE.NLREL + 10) GOTO 1450
      IF(IY.GE.NLREL + 8 ) GOTO 1430
      IF(IY.GE.NLREL + 6 ) GOTO 1420
      IF(IY.GE.NLREL + 4 ) GOTO 1400
      IF(IY.GE.NLREL + 2 ) GOTO 1300
      IF(IY.EQ.NLREL     ) GOTO 1200
      IF ( IY .EQ. 5  .AND.  IX .EQ. 35 ) GOTO 1200 ! Relativity Gamma
      IF ( IY .EQ. 5  .AND.  IX .EQ. 37 ) GOTO 1250 ! Precession constant
      IF ( IY .EQ. 5  .AND.  IX .EQ. 67 ) GOTO 1260 ! Nutation Dpsi
      IF ( IY .EQ. 5  .AND.  IX .EQ. 69 ) GOTO 1265 ! Nutation Deps
!!      CALL SWBIT(LTIDE(1,IPOS),1)
!!      IDSP(1)=KBITN(LTIDE(1,IPOS),1)
!!      LTIDE(KKK,1) = 0
!!      LTIDE(KKK,2) = 0
!!      LTIDE(KKK,3) = 0
!!      WRITE(bufstr,'(i1)') LTIDE(1,IPOS)
      IDSP(1) = 0
!C
      CALL SETCR_MN ( IX, IY )
      CALL ADDSTR_F ( BUFSTR(:1) )
      CALL SETCR_MN ( IX, IY )
      GOTO 50
 1050 CONTINUE
      IF(IPOS.GE.0) GO TO 1075
      ITDGLB = 0
      LTIDE(1,1) = 0
      LTIDE(1,2) = 0
      LTIDE(1,3) = 0
      GO TO 50
 1075 CONTINUE
      L = IY-NLTIDE
      CALL SWBIT(LTIDE(1,IPOS),L )
      IDSP(1)=KBITN(LTIDE(1,IPOS),L)
      CALL setcr_mn(IX,IY )
      WRITE(bufstr,'(i1)') IDSP(1)
      call addstr_f(bufstr(:1) )
      CALL setcr_mn(IX,IY )
      go to 50
!
!**** Flip relativity or precession constant flag, or transfer to nutation
 1200 CONTINUE
      IF ( IX .NE. 35 ) GOTO 1250
      CALL SWBIT( LREL, INT2(1) )
      IDSP(1) = KBITN( LREL, INT2(1) )
      CALL setcr_mn(IX,IY )
      WRITE(bufstr,'(i1)') LREL
      call addstr_f(bufstr(:1) )
      CALL setcr_mn(IX,IY )
      GO TO 50
!
 1250 CONTINUE
      IF ( IX .NE. 37 ) GOTO 1260
      CALL SWBIT( LPREC, INT2(1) )
      IDSP(1) = KBITN( LPREC, INT2(1) )
      CALL setcr_mn(IX,IY )
      WRITE(bufstr,'(i1)') LPREC
      call addstr_f(bufstr(:1) )
      CALL setcr_mn(IX,IY )
      go to 50
!
 1260 CONTINUE
      IF ( IX .NE. 67  .AND.  CCHAR(1:1) .NE. '.' ) GO TO 1265
      CALL   SWBIT( LNUT(1), INT2(1) )
      LPSI = KBITN( LNUT(1), INT2(1) )
      IF  (LPSI.EQ. &
     &  1)THEN  !make certain the time series is off
          LNUT(2) = 0
          LNUT(3) = 0
          NFLPSI = 0
          NFLEPS = 0
          END IF  !make certain the time series is off
      CALL SETCR_MN ( 67, NLREL )
      WRITE ( BUFSTR, '(i1)' ) LPSI
      CALL ADDSTR_F ( BUFSTR(:1) )
      CALL SETCR_MN ( 69, NLREL )
      IF ( CCHAR(1:1) .NE. '.' ) GOTO 50
!
 1265 CONTINUE
      IF ( IX .NE. 69  .AND.  CCHAR(1:1) .NE. '.' ) GOTO 50
      CALL   SWBIT( LNUT(1), INT2(2) )
      LEPS = KBITN( LNUT(1), INT2(2) )
      IF  (LEPS.EQ. &
     &  1)THEN  !make certain the time series is off
          LNUT(2) = 0
          LNUT(3) = 0
          NFLPSI = 0
          NFLEPS = 0
          END IF  !make certain the time series is off
      CALL SETCR_MN ( 69, NLREL )
      WRITE ( BUFSTR, '(i1)' ) LEPS
      CALL ADDSTR_F ( BUFSTR(:1) )
      CALL SETCR_MN ( 67, NLREL )
      GOTO 50
!
!     Process the minimum elevation angle.
 1300 CONTINUE
      IF (IX.GE.36.and.cchar(1:1).ne.'^') GO TO 1350
      CALL SET_EL_CUT ( SITE_DEP_EL_CUT, ELMIN, ELVCUT, WVMASK, &
     &                  NUMSTA, ISITN, PI__NUM )
      GOTO 50
!
! --- Pick a set of parameters.
!
 1350 CONTINUE
      IF ( CCHAR(1:1).NE.'!' .AND. CCHAR(1:1).NE.'#' ) THEN
           IF ( IX .LT. 66 ) CCHAR(1:1) = '!'
           IF ( IX .GE. 66 ) CCHAR(1:1) = '#'
      ENDIF
      CALL PARXC ( CCHAR(1:1), KERR )
      IF ( CCHAR(1:1) .EQ. '!' ) THEN
           KSTON = .TRUE.
        ELSE
           KSTON = .FALSE.
           IF ( KCENTERMASS ) THEN
!
! ------------- Disable no-net-translatiuon constraints if it was setup if
! ------------- no station coordinates will be estimated.
!
                CALL SBIT ( DEFCMP, INT2(5), INT2(0) )
                KCENTERMASS = .FALSE.
                CALL USE_GLBFIL_3 ( 'OW'  )
                CALL USE_GLBFIL_4 (  'WC' )
           END IF
      ENDIF
      IP = .TRUE.
      GO TO 50
!
!    Toggle ut1 rates
!
 1375 continue
      idum2=irotf( INT2(1), INT2(3), INT2(2), lrot)
      goto 50
!
!     Process the print control flag.
 1400 CONTINUE
      if((ix.lt.36.and.cchar(1:1).ne.'M').or.cchar(1:1).eq.'A') then
        IPOS4 = 19
        ILOC = 1
      else
        IPOS4 = 58
        ILOC = 3
      ENDIF
      IY = NLREL+4
      CALL setcr_mn(IPOS4,IY )
      CALL SWBIT(IPRES,ILOC )
      IDSP(1) = KBITN(IPRES,ILOC)
      IF(IDSP(1) .EQ. 0) call addstr_f("OFF")
      IF(IDSP(1) .EQ. 1) call addstr_f("ON ")
      CALL setcr_mn(IX,IY )
      go to 50
!     Process the matrix print control flag.
 1420 CONTINUE
      if((ix.lt.36.and.cchar(1:1).ne.'Z').or.cchar(1:1).eq.'N') then
        IPOS4 = 23
        ILOC = 5
      else
        IPOS4 = 58
        ILOC = 6
      ENDIF
      IY = NLREL+6
      CALL setcr_mn(IPOS4,IY )
      CALL SWBIT(IPRES,ILOC )
      IDSP(1) = KBITN(IPRES,ILOC)
      IF(IDSP(1) .EQ. 0) call addstr_f("OFF")
      IF(IDSP(1) .EQ. 1) call addstr_f("ON ")
      CALL setcr_mn(IX,IY )
      go to 50
!
!     Process the weak station constraint flag.
 1430 CONTINUE
      IPOS4 = 28
      ILOC = 7
      IY = NLREL+8
      CALL setcr_mn(IPOS4,IY )
      CALL SWBIT(IPRES,ILOC )
      IDSP(1) = KBITN(IPRES,ILOC)
      IF(IDSP(1) .EQ. 0) call addstr_f("OFF")
      IF(IDSP(1) .EQ. 1) call addstr_f("ON ")
      CALL setcr_mn(IX,IY )
      go to 50
!
! --- Proicess toggling USE RATE mode
!
 1440 CONTINUE
      IPOS4 = 48
      ILOC = 7
      IY = NLREL+8
      CALL SETCR_MN(IPOS4,IY )
      NORATE_FLAG = .NOT. NORATE_FLAG
      IF (       NORATE_FLAG ) THEN
            CALL REVERSE_ON_MN()
            CALL ADDSTR_F ( "No" )
            CALL REVERSE_OFF_MN()
            CALL ADDSTR_F ( " " )
         ELSE
            CALL REVERSE_ON_MN()
            CALL ADDSTR_F ( "Yes" )
            CALL REVERSE_OFF_MN()
      END IF
      CALL USE_GLBFIL_4 ( 'OWC' )
!
      CALL SETCR_MN ( IX, IY )
      GOTO 50
!
! --- Process the unweight delay flag.
!
 1450 CONTINUE
      ILLOC = NLREL + 10
!
!     Test for normal/down weight or for baseline clock offsets.
      IF (IX .GT. 30 .AND. CCHAR(1:1) .NE. 'W') GO TO 1460
      IF (CCHAR(1:1) .EQ. 'C') GO TO 1460
      CALL SETCR_MN ( 0, ILLOC )
      IF (IDNWT .EQ. &
     &  1)THEN  !re-set to normal weights
          IDNWT = 0
          WRITE(bufstr,2335)
          call addstr_f(bufstr )
        ELSE  !re-set for down weight
          IDNWT = 1
          call addstr_f("Down-(W)eighted delays by 1.D9 " )
          call addstr_f("   Select: Baseline-(C)lock offsets" )
          END IF  !re-set for down weight
      CALL SETCR_MN ( 0, ILLOC )
      GOTO 50
 1460 CONTINUE
!
! --- Baseline dependent clock offsets
!
! --- Write out SOCOM
!
      CALL USE_COMMON ('OWC' )
!
! --- Call program BCLOK
!
      CALL USE_BUFFER( NUMSTA, INT2(1), 'OWC' )
      CALL RUN_PROG( 'BCLOK', 'WAIT', INT2(0) )
      CALL USE_BUFFER( KKBUF, INT2(MAX_ARC_STA*ARC_STA_BIT_WORDS+1), 'ORC' )
!
! --- Read in SOCOM
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
!
! --- Read temp array into SOLVE COMMON and set logical flag for
! --- baseline dependent clocks
!
      LOGBCL = .FALSE.
      DO 1531 JJ = 1, MAX_ARC_STA
        DO 1532 II = 1, ARC_STA_BIT_WORDS
           KK = (JJ - 1)*ARC_STA_BIT_WORDS + II
           IF (KKBUF(KK) .NE. 0) LOGBCL = .TRUE.
           ICLOCK(II,JJ) = KKBUF(KK)
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                BASCL_P(II,JJ) = ICLOCK(II,JJ)
              ELSE
                BASCL_G(II,JJ) = ICLOCK(II,JJ)
           END IF
 1532   CONTINUE
 1531 CONTINUE
!
! ---- Pick up IKONT to branch as desired
!
      IKONT = KKBUF ( MAX_ARC_STA*ARC_STA_BIT_WORDS+1 )
!
! ---- Save flag words in COMMON FILE
!
      CALL USE_COMMON ( 'OWC' )
!
! --- Branch to appropriate routine
!
      IF (IKONT .EQ. -7) GO TO 1680
      IF (IKONT .EQ.-31) GO TO 1510
      IF (IKONT .NE.  0) RETURN
      GO TO 50
!
!     New options listed on "DELAYS VS. RATES" line
!
 1500 CONTINUE
      IF(CCHAR(1:1).EQ.':') IX = 1
      IF(CCHAR(1:1).EQ.';') IX = 16
      IF(CCHAR(1:1).EQ.'B') IX = 42
      IF(CCHAR(1:1).EQ.'X') IX = 55
      IF (IX.GE.42) GO TO 1505
!**** PROCESS THE DELAYS AND RATES FLAGS
      IY1 = NLREL + 12
      CALL SETCR_MN ( 0, IY1 )
!
!   INCREMENT DELAY OR RATE FLAG AS APPROPRIATE
!
!         IDATYP Value     Meaning
!
!               0        grp & rates
!               1        phs & rates
!               2         sb & rates
!               3          grp only
!               4          phs only
!               5           sb only
!               6         rates only
!
!
      IF ( IX .LT. 16 ) THEN
        IF ( IDATYP.EQ.0 ) THEN
          IDATYP = 1
        ELSE IF ( IDATYP.EQ.1 ) THEN
          IDATYP = 2
        ELSE IF ( IDATYP.EQ.2 ) THEN
          IDATYP = 6
        ELSE IF ( IDATYP.EQ.3 ) THEN
          IDATYP = 4
        ELSE IF ( IDATYP.EQ.4 ) THEN
          IDATYP = 5
        ELSE IF ( IDATYP.EQ.5 ) THEN
          IDATYP = 3
        ELSE IF ( IDATYP.EQ.6 ) THEN
          IDATYP = 0
        END IF
      ELSE
        IF ( IDATYP.EQ.0 ) THEN
          IDATYP = 3
        ELSE IF ( IDATYP.EQ.1 ) THEN
          IDATYP = 4
        ELSE IF ( IDATYP.EQ.2 ) THEN
          IDATYP = 5
        ELSE IF ( IDATYP.EQ.3 ) THEN
          IDATYP = 0
        ELSE IF ( IDATYP.EQ.4 ) THEN
          IDATYP = 1
        ELSE IF ( IDATYP.EQ.5 ) THEN
          IDATYP = 2
        ELSE IF ( IDATYP.EQ.6 ) THEN
          IDATYP = 3
        END IF
      END IF
!
! --- Write the modified result on crt
!
      CALL SHOW_DATYP ( IDATYP )
      CALL USE_COMMON ( 'OWC'  )
!
! --- Check: if the data type which uses dela rate, then NORATE_FLAG should
! --- be lift
!
      IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
           IF ( NORATE_FLAG ) THEN
                NORATE_FLAG = .FALSE.
                IP = .TRUE.
           END IF
      END IF
      IX = 1
      CALL setcr_mn(IX,IY )
      go to 50
 1505 IF (IX.GE.55) GO TO 1510
!
 1506 CONTINUE
!
!     Find EOF of spool file and write into common
      CALL USE_COMMON ( 'OWC' )
!
      KKBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+4) = 2
      CALL USE_BUFFER ( KKBUF, INT2(MAX_ARC_STA*ARC_STA_BIT_WORDS+4), 'OWC' )
      CALL RUN_PROG   ( 'SLVEB', 'WAIT', INT2(0) )
      CALL USE_BUFFER ( KKBUF, INT2(MAX_ARC_STA*ARC_STA_BIT_WORDS+4), 'ORC' )
!
! --- Open and read SOCOM
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
!
! --- Update IBLSEL bits for baseline deselection
!
      DO 1507 K=1,ARC_STA_BIT_WORDS
         DO 1508 I=1,MAX_ARC_STA
            KK=(I-1)*ARC_STA_BIT_WORDS+K
            IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------------- Phase delay data type
!
                 IBLSEL_P(K,I) = KKBUF(KK)
              ELSE
!
! -------------- Group delay data type
!
                 IBLSEL_G(K,I) = KKBUF(KK)
            END IF
 1508    CONTINUE
 1507 CONTINUE
      IKONT = KKBUF(MAX_ARC_STA*ARC_STA_BIT_WORDS+1)
      IF ( SUPMET == SUPMET__META ) THEN
           CALL ACS_OBSFIL ( 'O' )
           DO 410 J1=1,NUMOBS
              CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
              CALL AUTO_SUP_UPD ( ISITE, ISTAR, ELEV, AUTO_SUP )
              CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
 410       CONTINUE 
           CALL ACS_OBSFIL ( 'C' )
      END IF
!
! --- Update bit fields STABIT_G, STABIT_P
!
      CALL SET_STABIT ( INT2(2) )
      IF ( KSTON ) THEN
!
! -------- If station positions are estiamted it might be necessary to change
! -------- the reference station for positions
!
           CALL PARXC ( '!', KERR )
      END IF
!
! --- Save  flag words in common file
!
      CALL USE_COMMON ( 'OWC' )
!
! --- Test: was not clock reference station deselected?
!
      WAS_REFCLO = .FALSE.
      DO 420 J2=1,NUMSTA
         IF ( CHECK_STABIT ( J2 ) .AND.  J2 .EQ. BM_REF_CL ) THEN
              WAS_REFCLO = .TRUE.
         END IF
 420  CONTINUE
!
      IF ( BMODE_CL .AND.  .NOT. WAS_REFCLO ) THEN
!
! --------- Yes! It happens just so. (and clocks are set in batch mode fashion)
!
            CALL CLEAR_MN()
            CALL ADDSTR_F ( "You deselected reference clock station!!" )
            CALL NL_MN()
            CALL NL_MN()
            CALL ADDSTR_F ( "Please select reference clock station "// &
     &                       "once more" )
            CALL NL_MN()
            CALL NL_MN()
            CALL ADDSTR_F ( "Hit any key to acknowledge   " )
            CALL SENKR_MN  ( IX, IY, ICH )
            IKONT = 1
            CCHAR(1:1) = 'I'
            GOTO 1680
      END IF
      IF ( IKONT .EQ. -7 ) THEN
           CCHAR(1:1) = 'O'
           GOTO 1680
      ENDIF
      IF ( IKONT .EQ. -31 ) THEN
           CCHAR(1:1) = 'X'
           GOTO 1510
      ENDIF
!
      IF ( IKONT .NE. 0 ) RETURN
      GOTO 50
 1510 CONTINUE
!
! --- Select databases.
!
      CALL SLDB ( IKONT, CCHAR(1:1) )
      IF ( IKONT.EQ.-3  ) RETURN
      IF ( IKONT.EQ.-7  ) GOTO 1680
      IF ( IKONT.EQ.-15 ) GOTO 1506
      GOTO 50
 1530 CONTINUE
!
! --- Process change of data type
!
      CALL CHANGE_DATATYPE ( .FALSE. )
!
! --- Check: if the data type which uses dela rate, then NORATE_FLAG should
! --- be lift
!
      IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
           IF ( NORATE_FLAG ) THEN
                NORATE_FLAG = .FALSE.
           END IF
      END IF
      CALL USE_GLBFIL_4 ( 'OR' )
      FL_VTD_SES = .TRUE.
      CALL USE_GLBFIL_4 ( 'WC' )
      IP = .TRUE.
      GOTO 50
 1535 CONTINUE
!
! --- Process change of data type
!
      CALL CHANGE_DATATYPE ( .TRUE. )
!
! --- Check: if the data type which uses dela rate, then NORATE_FLAG should
! --- be lift
!
      IF ( DATYP_INQ ( IDATYP, RATE__DTP ) ) THEN
           IF ( NORATE_FLAG ) THEN
                NORATE_FLAG = .FALSE.
           END IF
      END IF
      CALL USE_GLBFIL_4 ( 'OR' )
      FL_VTD_SES = .TRUE.
      CALL USE_GLBFIL_4 ( 'WC' )
      IP = .TRUE.
      GOTO 50
 1540 CONTINUE
!
! --- Process of setting, modifying and changing singularity check control
!
      CALL SET_SNGCHK ( SNGCHK_ACTION, SNGCHK_SOUMIN, SNGCHK_STAMIN, &
     &                  SNGCHK_BASMIN )
      CALL USE_GLBFIL_4 ( 'OWC' )
      IP = .TRUE.
      GOTO 50
 1550 CONTINUE
!
! --- Process of setting, modifying and changing a priori clock model
!
      CALL SET_ACM()
      IP = .TRUE.
      GOTO 50
 1560 CONTINUE
!
! --- Process of change of suppression method
!
      CALL CHANGE_SUPMET ( F_SUPMET )
      IF ( F_SUPMET ) CALL USE_COMMON ('OWC' )
      IP = .TRUE.
      GOTO 50
 1600 CONTINUE
!
! --- Process the control line.
!
      IF (IX.GT.49 .AND. CCHAR(1:1).EQ. &
     &  ' ')THEN !Turn off output cgm
           IOCGM = 0
           DO I=1,3
             ARCDIR(I)=' '
           ENDDO
           ICONT=0
           IF(INAMCG(1:1).EQ.' ') ISOLU=0
           CALL USE_GLBFIL('OWC' )
           CALL SETCR_MN ( 50, NCNT )
           CALL ADDSTR_F ( "                             " )
           CALL SETCR_MN ( 50, NCNT )
           GOTO 50
           END IF
!
      IF((IX.GT.20.AND.IX.LE.37.AND.CCHAR(1:1).EQ. &
     &     ' ').OR.CCHAR(1:1).EQ. &
     &  'T')THEN  !terminate SOLVE
          CALL PARCN()
          CALL USE_COMMON ( 'OWC' )
          CALL RUN_PROG( 'SLEND', 'PASS', INT2(0) )
        ELSE IF(IX.LE.13.OR.CCHAR(1:1).EQ.'Q') THEN !run least squares
          CALL SETCR_MN ( 0, 0 )
          CALL CLEAR_MN()
          IKONT = -3
          RETURN
        ELSE
          go to 50
        END IF
!
 1680 CONTINUE
      IF ( IX.GT.49 .AND. CCHAR(1:1).EQ.' ' ) THEN ! Turn off input cgm
           INAMCG = ' '
           IF(IOCGM.EQ.0) ISOLU=0
           DO I=1,3
             ARCDIR(I)=' '
           ENDDO
           CALL USE_GLBFIL('OWC' )
           CALL SETCR_MN ( 50, NCNT-1 )
           CALL ADDSTR_F ( "                             " )
           CALL SETCR_MN ( 50, NCNT-1 )
           GOTO 50
      END IF
!
      IF ( ( IX.GE.33 .AND. IX.LE.46 .AND. CCHAR(1:1).EQ.' ' ) .OR. &
     &       CCHAR(1:1).EQ.'"' ) THEN
           CALL CONPG ( IKONT )
           IF ( IKONT .EQ. -15 ) RETURN
           GOTO 50
      ENDIF
!
      IF  ((IX.GE.23.AND.IX.LE.31.AND.CCHAR(1:1).EQ. &
     &     ' ').OR.CCHAR(1:1).EQ.'O'.OR.IKONT.EQ.- &
     &  7)THEN  !schedule OPTIN
!         COUNT THE PARAMETER FLAGS
          ICLMAX = 0
          CALL PARCN()
!
          IF ( NPARAM .GT. NPMAX ) THEN  !too many parameters
            WRITE(bufstr,4740) NPARAM,NPMAX
 4740       FORMAT(I5," PARAMETERS HAVE BEEN SELECTED, MAXIMUM IS ", I6)
            call addstr_f(bufstr )
            call nl_mn()
            call addstr_f(" RETURN TO CONTINUE" )
            call nl_mn()
            CALL senkr_mn(IXD,IYD,IDUM )
            IKONT = 1
            RETURN
!
            ELSE  !setup for and transfer to OPTIN
              CALL USE_COMMON ( 'OWC' )
              CALL RUN_PROG( 'OPTIN', 'PASS', INT2(0) )
              END IF  !setup for and transfer to OPTIN
          END IF  !schedule OPTIN
      IF  ((IX.GE.14.AND.IX.LE.21.AND.CCHAR(1:1).EQ.' ') .OR. &
     &       CCHAR(1:1).EQ.'S') THEN  !move to source page
          IKONT = -1
          RETURN
        ELSE IF(IX.LE.12.OR.CCHAR(1:1).EQ.'I') THEN  !move to a site page
          IKONT = 1
          IF(JKONT.NE.0) IKONT = JKONT
        ELSE
          go to 50
        END IF
!
      RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SHOW_DATYP ( IDATYP )
!         IDATYP Value     Meaning
!
!               0        grp & rates
!               1        phs & rates
!               2         sb & rates
!               3          grp only
!               4          phs only
!               5           sb only
!               6         rates only
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*2  IDATYP
      LOGICAL*4  DATYP_INQ
!
      CALL ADDSTR_F ( '(:)Delay ' )
      CALL REVERSE_ON_MN()
      IF (       DATYP_INQ ( IDATYP, GROUP__DTP ) ) CALL ADDSTR_F ( ' Group' )
      IF (       DATYP_INQ ( IDATYP, PHASE__DTP ) ) CALL ADDSTR_F ( '  Phas' )
      IF (       DATYP_INQ ( IDATYP, SINGL__DTP ) ) CALL ADDSTR_F ( 'Single' )
      IF ( .NOT. DATYP_INQ ( IDATYP, DELAY__DTP ) ) CALL ADDSTR_F ( '   Off' )
      CALL REVERSE_OFF_MN()
      CALL ADDSTR_F ( '  ' )
      CALL ADDSTR_F ( '(;)Rates ' )
      CALL REVERSE_ON_MN()
      IF (       DATYP_INQ ( IDATYP,  RATE__DTP ) ) CALL ADDSTR_F ( ' On' )
      IF ( .NOT. DATYP_INQ ( IDATYP,  RATE__DTP ) ) CALL ADDSTR_F ( 'Off' )
      CALL REVERSE_OFF_MN()
      CALL ADDSTR_F ( '  ' )
!
      RETURN
      END  !#!  SHOW_DATYP  #!#
