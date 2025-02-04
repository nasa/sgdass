#include <mk5_preprocessor_directives.inc>
      SUBROUTINE MULTI_DIAGI ( COMMON_TITLE, MPL, NC, NR, TITS, MPB, &
     &           BUTTON_NAME, BUTTON_LET, PREF_NAME, DIAGI_S, ICODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MULTI_DIAGI  provides a convenient interface to DiaGI     *
! *   (Dialogue Graphic Interface) utility for the case when more than   *
! *   one plot is to be displayed and some actions should activated by   *
! *   hitting a button.                                                  *
! *                                                                      *
! *   Multi_DiaGI  displays MPL plots and MPB buttons at one graphic     *
! *   PGPLOT screen. MPL or MPB or both can be zero and then Multi_DiaGI *
! *   works in a special mode.                                           *
! *                                                                      *
! *   Common title is displayed at the top of the screen.                *
! *                                                                      *
! *   MPL plots are displayed in small boxes at the left part of the     *
! *   screen. User can point a cursor to a box with a small plot and     *
! *   click the left or middle mouse button. Then the plot will be       *
! *   magnified to entire screen and user is able to manipulate with a   *
! *   plot by using DiaGI commands. After leaving DiaGI level, by        *
! *   hitting X or double clicking the right button, control is passed   *
! *   back to a Multi_DiaGI level and the set of small boxes is          *
! *   displayed again. Changes which user has done in DiaGI level are    *
! *   stored and displayed at Multi_DiaGI level.                         *
! *                                                                      *
! *   MPB buttons are displayed at the right part of the screen. User    *
! *   can activate a button by a) pointing a cursor to a box and         *
! *   clicking a left or middle mouse button; b) hitting a letter-code.  *
! *   Multi_DiaGI returns a button index upon activating a button.       *
! *                                                                      *
! *   The following commands are available in Multi_DiaGI level:         *
! *   1) ? -- get on-line help information;                              *
! *   2) X or <right_mouse_button> -- quit Multi_DiaGI;                  *
! *   3) <left_mouse_button> or <middle_mouse_button> -- magnify the box *
! *      or activate a button (if a cursor points to a small box with    *
! *      plot or to a button).                                           *
! *   4) <CNTRL/P> make a hardcopy of the current window in PS or GIF    *
! *      format;                                                         *
! *   5) <CNTRL/W> make a hardcopy of the current window and all plots   *
! *      in PS or GIF format.                                            *
! *                                                                      *
! *   The following commands are available in DiaGI when called from     *
! *   MultiDiaGi:                                                        *
! *   a) <PgUp> -- draw the next  plot of the MultiDiaGi set;            *
! *   b) <PgDn> -- draw the prior plot of the MultiDiaGi set.            *
! *                                                                      *
! *   If  MPB=0  then plots occupies entire screen.                      *
! *   If  MPL=0  then command buttons occupies entire screen.            *
! *   If  MPL=0  and  MPB=0  then only a common title is displayed.      *
! *                                                                      *
! *   It is assumed that plotting information and arrays to be displayed *
! *   are put in the array of data structures DIAGI_S.                   *
! *                                                                      *
! *   Filenames with hardcopies are formed as                            *
! *                                                                      *
! *   1) Hardcopy of the Multi_DiaGI window:                             *
! *      PREF_NAME + "all" + extension: ".gif" or ".ps"                  *
! *      Only plots and common title are put in the hardcopy: Button are *
! *      not put.                                                        *
! *                                                                      *
! *   2) Hardcopy of the individual plots:                               *
! *      a) DIAGI_S(k).NAME starts from / (absolute path) then name is   *
! *         DIAGI_S(k).NAME + extension: ".gif" or ".ps"                 *
! *                                                                      *
! *      b) DIAGI_S(k).NAME does not start from / the the name is        *
! *         PREF_NAME + DIAGI_S(k).NAME + extension: ".gif" or ".ps"     *
! *                                                                      *
! *      c) If DIAGI_S(k).NAME starts from blank (empty) then "plot01",  *
! *         "plot02" and so on are used instead of DIAGI_S(k).NAME:      *
! *         PREF_NAME + pgpot<num> + extension: ".gif" or ".ps"          *
! *                                                                      *
! *   Comments:                                                          *
! *   ~~~~~~~~~                                                          *
! *                                                                      *
! *   1) If DIAGI_S(k).NCLR = 0 then the k-th plot will not be displayed *
! *      but the place for it will be reserved. There will be a hole     *
! *      at that place.                                                  *
! *                                                                      *
! *   2) If BUTTON_NAME(j) is empty then the j-th command button will    *
! *      not be displayed but the place for it will be reserved.         *
! *      There will be a hole at that place.                             *
! *                                                                      *
! *   3) MPL, MPB should not exceed MAX_PAR (currently 128).             *
! *      Actually plot will look ugly if NC*NR>30 or MPB>20.             *
! *                                                                      *
! *   4) Multi_diagi re-defined colors used for 17-th and 18-th function *
! *      so applications which are using more than 16 functions may      *
! *      look a bit strange.                                             *
! *                                                                      *
! *   5) Action for (normal) termination is defined in diagi_s(1).itrm   *
! *      (If MPL=0, then diagi_s is ignored and the action is            *
! *       DIAGI__CLOSE).                                                 *
! *      User can select:                                                *
! *        diagi__close -- X-window will be iconified                    *
! *        diagi__close_verbose -- user will be prompted to iconify the  *
! *                                X-window;                             *
! *        diagi__erase -- erase X-window contents;                      *
! *        diagi__keep  -- X-window will keep its contents.              *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * COMMON_TITLE ( CHARACTER ) -- A line which is displayed at the top   *
! *                               of the window.                         *
! *          MPL ( INTEGER*4 ) -- Number of plots to be displayed.       *
! *                               Zero values means no plots.            *
! *           NC ( INTEGER*4 ) -- Number of columns in which the plots   *
! *                               will displayed at the screen.          *
! *           NR ( INTEGER*4 ) -- Number of rows in which the plots      *
! *                               will displayed at the screen.          *
! *                               Small plot boxes are located at the    *
! *                               screen from left to right and from top *
! *                               to bottom. If MPL > NC*NR then only    *
! *                               first NC*NR small plots are displayed  *
! *                               and other are ignored. If MPL < NC*NR  *
! *                               then MPL boxes are displayed but the   *
! *                               place for NC*NR is reserved.           *
! *          TIT ( CHARACTER ) -- Array of short titles for each plot.   *
! *                               Long titles are stored in DIAGI_S.     *
! *                               They are displayed in DiaGI mode but   *
! *                               TIT lines are displayed in Multi_DiaGI *
! *                               mode. TIT lines should be short. F.e.  *
! *                               if NC=4 then it should not exceed      *
! *                               16-20 letters. PGPLOT meta-letters are *
! *                               accepted. Dimension: MPL.              *
! *          MPB ( INTEGER*4 ) -- Number of command buttons which are to *
! *                               be displayed. Zero values means no     *
! *                               buttons.                               *
! *  BUTTON_NAME ( CHARACTER ) -- Array with a text buffer for each      *
! *                               button. This text is printed on the    *
! *                               button. Text may contain several lines *
! *                               separated by symbol |. Symbol | is not *
! *                               printed. PGPLOT meta-letters are       *
! *                               accepted. Dimension: MPB.              *
! *   BUTTON_LET ( CHARACTER ) -- Array with a strings with letter-codes *
! *                               specifying the button. The letter code *
! *                               of the first character of the string   *
! *                               is displayed at the up left corner of  *
! *                               the button. Button is activated by     *
! *                               hitting any letter from the string on  *
! *                               the keyboard.                          *
! *                               NB: keys are case sensitive.           *
! *                               NB: <Left_Mouse> is tied with A letter,*
! *                               <Middle_Mouse> is tied with D letter,  *
! *                               <Right_Mouse> is tied with X letter.   *
! *                               Hitting mouse buttons is the same as   *
! *                               hitting these keys in keyboard. Mouse  *
! *                               buttons have a precedence before       *
! *                               keyboard commands, but hitting X or    *
! *                               <Right_Mouse> always causes Multi_DiaGI*
! *                               termination regardless where the cursor*
! *                               points. Dimension: MPB.                *
! *    PREF_NAME ( CHARACTER ) -- A prefix which will be prepended before*
! *                               the filename with a hardcopy. It is    *
! *                               assumed that it contains a pathname.   *
! *                               If PREF_NAME is empty then files with  *
! *                               hardcopies will be put in the present  *
! *                               directory.                             *
! *                                                                      *
! * _______________________ Modified parameters: _______________________ *
! *                                                                      *
! *     ICODE ( INTEGER*4 ) --                                           *
! *                        Input value:                                  *
! *                            if  in range [1, MPL] -- MultiDiaGi will  *
! *                                start in one-plot mode: it will show  *
! *                                the polot with index ICODE.           *
! *                            out of this range -- MultiDiaGi will      *
! *                                start in multi-plot mode.             *
! *                        Output value:                                 *
! *                            -3 -- A user hit PageUp when Multi_DiaGi  *
! *                                  showed the last plot.               *
! *                            -2 -- A user hit PageDn when Multi_DiaGi  *
! *                                  showed the first plot.              *
! *                            -1 -- error.                              *
! *                            -1 -- error.                              *
! *                             0 -- Multi_DiaGI was terminated since    *
! *                                  user hit X of <right_mouse_button>  *
! *                            >0 -- Multi_DiaGI was terminated since    *
! *                                  user hit a command button with index*
! *                                  ICODE.                              *
! *   DIAGI_S ( RECORD    ) -- Array of data structures which keep       *
! *                            DIAGI_S internal parameters and           *
! *                            specifications for plotting all plots.    *
! *                            It is assumed that all necessary fields   *
! *                            were filled before calling Multi_DiaGI.   *
! *                            Content of DIAGI_S array may by           *
! *                            Multi_DiaGI as a response to user's       *
! *                            plots manipulations.                      *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will be put   *
! *                                   on stdout.                         *
! *                            Output: 0 in the case of successful       *
! *                                    completion and non-zero in the    *
! *                                    case of error.                    *
! *                                                                      *
! * ###  20-JUL-1999  MULTI_DIAGI  v2.43 (c) L. Petrov  10-JAN-2014  ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'diagi.i'
      INCLUDE    'diagi_local.i'
      INTEGER*4  MPL, MPB, NC, NR, ICODE, IUER
      CHARACTER  COMMON_TITLE*(*), TITS(MPL)*(*), BUTTON_NAME(MPB)*(*), &
     &           BUTTON_LET(MPB)*(*), PREF_NAME*(*)
      TYPE ( DIAGI_STRU ) ::  DIAGI_S(MPL), DIAGI_M
      INTEGER*4  MAX_BOX
      PARAMETER  ( MAX_BOX=128 )
      TYPE ( DIAGI_BOXES ) ::  BOX_PL(MAX_BOX)
      TYPE ( DIAGI_BOXES ) ::  BOX_OP(MAX_BOX)
      INTEGER*4  COL_OPR(3), COL_LET(3), COL_FRG(3), COL_FRP(3), &
     &           ICOL_OPR, ICOL_LET, ICOL_FRG, ICOL_FRP, N1$ARG
      DATA       (  COL_OPR(N1$ARG), N1$ARG=1,3 ), ICOL_OPR, &
     &           (  COL_LET(N1$ARG), N1$ARG=1,3 ), ICOL_LET, &
     &           (  COL_FRG(N1$ARG), N1$ARG=1,3 ), ICOL_FRG, &
     &           (  COL_FRP(N1$ARG), N1$ARG=1,3 ), ICOL_FRP &
     &           /  220, 208, 185,  37,  & ! color for operation BUTTON
     &              180, 151, 157,  38,  & ! color for letter code
     &              212, 212, 212,  39,  & ! color for plot foreground (XW)
     &              240, 240, 240,  40   & ! color for plot foreground (PS,GIF)
     &           /
      INTEGER*4    NCLR_SAVED, ID_XW, ID_PRI
      CHARACTER    CH*1, STR*80, STR1*80, FINAM*128, FINAM_TRY*128, &
     &             TIT_SAVED*128, ARU_SAVED*128, DEVICE_PRN*128, PRI_STR*128, &
     &             PGPLOT_DEFSTR*32
      REAL*4       XC, YC
      REAL*4       FRACT_HT, FRACT_WD, FRACT_TIT, RESIZE_2
      REAL*4       FRACT_WD0, FRACT_WD2
      INTEGER*4    LCT, LCP, LCT_P, LCP_P, LCA, LCL, LCT_USE
      REAL*4       XCT, YCT, HCT, HCP, HCT_P, HCP_P, HCA, HCT_USE
      REAL*4       VL_XLB, VL_XTU, VL_YLB, VL_YTU
      REAL*4       HL_XLB, HL_XTU, HL_YLB, HL_YTU, HCL
      REAL*4       VL_XLB0, VL_XLB1, VL_XLB2, VL_XTU0, VL_XTU1, VL_XTU2
      REAL*4       YUP_BUT, YUP_BUT0, YUP_BUT2
      PARAMETER  ( RESIZE_2  = 0.92 ) ! resizing plot to fit Letter paper
!
! --- Definition which fraction of the box width and height takes
! --- a) title; b) gaps between plots; c) gaps between command buttons
!
      PARAMETER  ( FRACT_HT  = 0.40,  FRACT_TIT=0.1 )
      PARAMETER  ( FRACT_WD0 = 0.25,  FRACT_WD2=1.0 )
      PARAMETER  ( YCT = 0.96 ) ! Y-coordinate of the title
      PARAMETER  ( LCT=7, LCT_P=4 ) ! fontsize for title
      PARAMETER  ( LCP=5, LCP_P=3 ) ! and plot titles
      PARAMETER  ( LCA=1  ) ! fontsize for command buttons
      PARAMETER  ( LCL=1  ) ! fontsize for Multi_DiaGI label
!
! --- Coordintes of the horizontal and vertical bars
!
      PARAMETER  ( VL_XLB1=0.800, VL_XTU1=0.802, VL_YLB=0.000, VL_YTU=1.000 )
      PARAMETER  ( VL_XLB0=1.000, VL_XTU0=1.000 )
      PARAMETER  ( VL_XLB2=-0.001, VL_XTU2=-0.001 )
!
! --- Coordinates of the common title
!
      PARAMETER  (  HL_XLB=0.000,                HL_YLB=0.947, HL_YTU=0.948 )
      PARAMETER  ( YUP_BUT0=1.0, YUP_BUT2=HL_YLB )
!
      REAL*4     XWD, YHT, YUP, XLF, XRT
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, ICLR, IB, IE, ID, &
     &           IPL, IDEV, IDEV_XS, ITRM_SAVED, IPRN, IL, IO, LUN, NPL_ACT, &
     &           IPLT_INIT, LEN_DIAGI, IOS, IER
      LOGICAL*4  F_PRI, F_WEB, F_TERM 
#ifdef HPUX  ! Actually, it is a bug in HP-UX Fortran compiler
      PARAMETER  ( DIAGI__PGDN = 221 ) ! PageDown key code
      PARAMETER  ( DIAGI__PGUP = 220 ) ! PageUp   key code
#else
      PARAMETER  ( DIAGI__PGDN = CHAR(221) ) ! PageDown key code
      PARAMETER  ( DIAGI__PGUP = CHAR(220) ) ! PageUp   key code
#endif
#ifdef GNU
      LOGICAL*4, INTRINSIC :: ISATTY
      INTRINSIC  FLUSH
#else
      LOGICAL*4, EXTERNAL :: FUNC_ISATTY
#endif
      INTEGER*4, EXTERNAL :: DIAGI_INBOX, GET_UNIT, ILEN, I_LEN, LINDEX, &
     &                       LTM_DIF, PGOPEN
!
! --- Initialization
!
      LEN_DIAGI = LOC(DIAGI_M%STATUS) - LOC(DIAGI_M%IFIRST_FIELD)
      CALL NOUT ( LEN_DIAGI, DIAGI_M )
#ifdef SUN
      F_TERM = FUNC_ISATTY ( 0 ) ! Flag whether the unit 6 is a terminal
#else
#ifdef GNU
      F_TERM = ISATTY ( 6 ) ! Flag whether the unit 6 is a terminal
#else
      F_TERM = FUNC_ISATTY ( 6 ) ! Flag whether the unit 6 is a terminal
#endif
#endif
!
! --- Setting parameters according to the current mode
!
      IF ( MPL .LE.0 ) THEN
!
! -------- No-plot mode
!
           VL_XLB   = VL_XLB2
           VL_XTU   = VL_XTU2
           HL_XTU   = VL_XTU0
           XCT      = VL_XTU0/2.0
           FRACT_WD = FRACT_WD2
           YUP_BUT  = YUP_BUT2
         ELSE
           IF ( MPL .GT. MAX_BOX ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( MPL, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( MAX_BOX, STR1 )
                CALL ERR_LOG ( 4191, IUER, 'MULTI_DIAGI', 'Parameter MPL '// &
     &              'is too large: '//STR(1:I_LEN(STR))//' -- it exceeds '// &
     &              'MAX_BOX='//STR1 )
                RETURN
           END IF
!
! -------- Plot mode
!
           IF ( MPB .LE. 0 ) THEN
!
! ------------- No BUTTON mode
!
                VL_XLB = VL_XLB0
                VL_XTU = VL_XTU0
             ELSE
                IF ( MPB .GT. MAX_BOX ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( MPB, STR )
                     CALL CLRCH ( STR1 )
                     CALL INCH  ( MAX_BOX, STR1 )
                     CALL ERR_LOG ( 4192, IUER, 'MULTI_DIAGI', 'Parameter '// &
     &                   'MPB is too large: '//STR(1:I_LEN(STR))// &
     &                   ' -- it exceeds MAX_BOX='//STR1 )
                     RETURN
                END IF
                VL_XLB = VL_XLB1
                VL_XTU = VL_XTU1
           END IF
!
! -------- Copy the first DIAGI_S to DIAGI_M
!
           CALL LIB$MOVC3 ( LEN_DIAGI, DIAGI_S(1), DIAGI_M )
!
           HL_XTU   = VL_XTU
           FRACT_WD = FRACT_WD0
           YUP_BUT  = YUP_BUT0
           XCT      = VL_XTU/2.0
      END IF
!
! --- Setting position of cursor
!
      IF ( MPB .EQ. 0 ) THEN
           XC = XCT
           YC = 1.0
         ELSE
           XC = (1.0 + VL_XTU)/2.0
           YC = 1.0
      END IF
!
! --- Set parameters for the output device
!
      F_WEB = .FALSE.
!
! --- Set device type for XS-screen
!
      IF ( MPL .GT. 0 ) THEN
           IDEV = DIAGI_M%IDEV
         ELSE
           DIAGI_M%IDEV = IXS__DEF
           IDEV = DIAGI_M%IDEV
      END IF
      IF ( DIAGI_M%IBATCH .EQ. 0  ) THEN
           IF ( IDEV .LE. IXS__MIN  .OR.  IDEV .GE. IXS__MAX ) THEN
!
! ------------- Correct default device type if it is necessary
!
                IDEV = IXS__DEF
!
! ------------- Set screen size in accordance with the environment variable
! ------------- DIAGI_SCREEN
!
                CALL CLRCH ( STR )
                CALL GETENVAR ( 'DIAGI_SCREEN', STR )
                CALL TRAN   ( 11, STR, STR )
                IF ( STR(1:4) .EQ. 'TINY' ) THEN
                     IDEV = 1
                     DIAGI_M%IDEV = IDEV
                  ELSE IF ( STR(1:5) .EQ. 'SMALL' ) THEN
                     IDEV = 2
                     DIAGI_M%IDEV = IDEV
                  ELSE IF ( STR(1:3) .EQ. 'BIG' ) THEN
                     IDEV = 2
                     DIAGI_M%IDEV = IDEV
                  ELSE IF ( STR(1:4) .EQ. 'HUGE' ) THEN
                     IDEV = 4
                     DIAGI_M%IDEV = IDEV
                  ELSE IF ( STR(1:4) .EQ. 'VAST' ) THEN
                     IDEV = 5
                     DIAGI_M%IDEV = IDEV
                END IF
            END IF
            IDEV_XS = IDEV
            F_PRI = .FALSE.
        ELSE IF ( DIAGI_M%IBATCH .EQ. 1 ) THEN
!
! -------- Check validity of the device index
!
           IDEV  = DIAGI_M%IDEV
           IF ( IDEV .LT. IBT__MIN   .OR.  IDEV .GT. MDEV ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( DIAGI_M%IDEV, STR )
                CALL ERR_LOG ( 4193, IUER, 'MULTI_DIAGI', 'Wrong device '// &
     &               'index '//STR(1:I_LEN(STR))//' This device is not '// &
     &               'supported in batch mode' )
                RETURN
           END IF
           IF ( ILEN(DIAGI_M%NAME) .EQ. 0 ) DIAGI_M%NAME = NAME__DEF
           IF ( INDEX ( DEVS(DIAGI_M%IDEV), 'PS' ) .GT. 0 ) THEN
                IF ( ILEN(DIAGI_M%NAME) == 0 ) THEN
                     DEVICE_PRN = 'all.ps'//DEVS(DIAGI_M%IDEV)
                   ELSE
                     DEVICE_PRN = DIAGI_M%NAME(1:ILEN(DIAGI_M%NAME))// &
     &                            'all.ps'//DEVS(DIAGI_M%IDEV)
                END IF
              ELSE IF ( INDEX ( DEVS(DIAGI_M%IDEV), 'GIF' ) .GT. 0 ) THEN
                IF ( ILEN(DIAGI_M%NAME) == 0 ) THEN
                     DEVICE_PRN = 'all.gif'//DEVS(DIAGI_M%IDEV)
                   ELSE
                     DEVICE_PRN = DIAGI_M%NAME(1:I_LEN(DIAGI_M%NAME))// &
     &                            'all.gif'//DEVS(DIAGI_M%IDEV)
                END IF
              ELSE
                IF ( ILEN(DIAGI_M%NAME) == 0 ) THEN
                     DEVICE_PRN = DEVS(DIAGI_M%IDEV)
                   ELSE 
                     DEVICE_PRN = DIAGI_M%NAME(1:I_LEN(DIAGI_M%NAME))// &
     &                            DEVS(DIAGI_M%IDEV)
                END IF
           END IF
           DIAGI_M%ITRM = DIAGI__CLOSE
           F_PRI = .TRUE.
      END IF
      IF ( MPL .GE. 1 ) THEN
           IPLT_INIT = DIAGI_S(1)%MD_IN
         ELSE
           IPLT_INIT = 0
      END IF
      XRT = 1.0
      IF ( IDEV .EQ. 5 ) THEN
!
! -------- Huge screen
!
           HCT   = 1.5  ! fontsize for title
           HCT_P = 1.2  !
           HCP   = 0.7  ! fontsize plot titles
           HCP_P = 0.7  !
           HCA   = 0.8  ! fontsize for command buttons
           HCL   = 0.6  ! fontsize for Multi_DiaGI label
         ELSE
!
! -------- Other size screen
!
           HCT   = 1.7  ! fontsize for title
           HCT_P = 1.4  !
           HCP   = 0.8  ! fontsize plot titles
           HCP_P = 0.8  !
           HCA   = 1.0  ! fontsize for command buttons
           HCL   = 0.6  ! fontsize for Multi_DiaGI label
      END IF
!
      CALL DIAGI_SET ( IDEV, DIAGI_M )
!
! --- Main loop
!
      IF ( ICODE .GT. 0 .AND. ICODE .LE. MPL ) THEN
           ID = ICODE
           GOTO 960
      END IF
 910  CONTINUE
!
! ----- Setting plotting parameters (sizes)
!
        IF ( .NOT. F_PRI ) THEN
!
! ---------- Openning X-window plotting device if it is not yet opened
!
             CALL PGQINF ( 'DEV/TYPE', STR, IL )
             IF ( INDEX ( STR, '/XS' ) .LE. 0 ) THEN
                  ID = PGOPEN ( DIAGI_M%DEVICE )
                  ID_XW = ID
             END IF
          ELSE
            CALL DIAGI_SET ( IDEV, DIAGI_M )
            IF ( DIAGI_M%DEVICE .EQ. '/CPS ' ) THEN
!
! -------------- Resizing of plot size in PS format in order to fit Letter
! -------------- paper format
!
                 DIAGI_M%XRIGHT = RESIZE_2*DIAGI_M%XRIGHT
            END IF
!
! --------- Open plotting device if needed
!
            ID = PGOPEN ( DEVICE_PRN )
            ID_PRI = ID
        END IF
!
        IF ( ID .LE. 0 ) THEN
!
! ---------- Error in opening input file was detected
!
             CALL CLRCH   (     STR )
             CALL INCH    ( ID, STR )
             IF ( .NOT. F_PRI ) THEN
                  CALL ERR_LOG ( 4194, IUER, 'MULTI_DIAGI', 'Error in '// &
     &                'openning the graphic device '//DIAGI_M%DEVICE// &
     &                ' IER='//STR )
                  ICODE = -1
                  RETURN
                ELSE
                  IF ( DIAGI_M%IBATCH .EQ. 0 ) THEN
                       CALL PGSLCT ( ID_XW )
                       CALL PGENDQ
                  END IF
                  CALL ERR_LOG ( 4195, IUER, 'MULTI_DIAGI', 'Error in '// &
     &                'openning the graphic device '// &
     &                 DEVICE_PRN(1:I_LEN(DEVICE_PRN))//'  IER='//STR )
                  ICODE = -1
                  RETURN
             END IF
        END IF
!
! ----- Setting window size in physical coordinates
!
        CALL PGPAP ( (DIAGI_M%XLEFT+DIAGI_M%XRIGHT)/25.4, &
     &             (DIAGI_M%YBOT+DIAGI_M%YTOP)/(DIAGI_M%XLEFT+DIAGI_M%XRIGHT) )
 920    CONTINUE
!
! ----- Setting colours
!
        IF ( MPL .GT. 0 ) THEN
             NCLR_SAVED = DIAGI_S(1)%NCLR
             DIAGI_S(1)%NCLR  = 0
!
! ---------- Predefined DiaGi colors
!
             CALL DIAGI_CLS ( DIAGI_S(1), IER )
           ELSE
!
! ---------- Setting minimum colors for no-plot mode (re-defining backround
! ---------- and foreground colors)
!
             CALL PGCOL_RGB ( BCG_CLRI, BCG_CLR(1), BCG_CLR(2), BCG_CLR(3) )
             CALL PGCOL_RGB ( FRG_CLRI, FRG_CLR(1), FRG_CLR(2), FRG_CLR(3) )
        END IF
!
! ----- Set Multi_Diagi colors
!
        CALL PGCOL_RGB ( ICOL_OPR, COL_OPR(1), COL_OPR(2), COL_OPR(3) )
        CALL PGCOL_RGB ( ICOL_LET, COL_LET(1), COL_LET(2), COL_LET(3) )
        CALL PGCOL_RGB ( ICOL_FRG, COL_FRG(1), COL_FRG(2), COL_FRG(3) )
        CALL PGCOL_RGB ( ICOL_FRP, COL_FRP(1), COL_FRP(2), COL_FRP(3) )
!
        IF ( F_PRI ) THEN
!
! ---------- Colors for non-interactive mode
!
             CALL PGSCR  ( 0, 1.0, 1.0, 1.0 ) ! pure white background
             CALL PGSCR  ( 1, 0.0, 0.0, 0.0 ) ! pure black foreground
        END IF
        IF ( MPL .GT. 0 ) THEN
!
! ---------- Restore DiaGI colors
!
             DIAGI_S(1)%NCLR  = NCLR_SAVED
        END IF
!
! ----- Setting default font type
!
        CALL PGSCF  ( 2 )
!
! ----- Erase the graphic screen
!
        CALL PGERAS
!
        CALL PGBBUF ! start bufferization
        CALL PGSAVE ! 1
!
! ----- Setting new world coodrinates
!
        CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0 )
        CALL PGSWIN  ( 0.0, XRT, 0.0, 1.0 )
!
! ----- Printing common title
!
        CALL PGSCI   (  1  )
        IF ( .NOT. F_PRI ) THEN
             CALL PGSCH   ( HCT )
             CALL PGSLW   ( LCT )
           ELSE
             CALL PGSCH   ( HCT_P )
             CALL PGSLW   ( LCT_P )
        END IF
        CALL GETENVAR ( 'DIAGI_MULTI_TITLE_HEIGT', STR )
        IF ( ILEN(STR) .NE. 0 ) THEN
             IF ( INDEX ( STR, '.' ) .LE. 0 ) STR = STR(1:I_LEN(STR))//'.0'
             READ ( UNIT=STR, FMT='(F32.32)', IOSTAT=IOS ) HCT_USE
             IF ( IOS .EQ. 0 ) THEN
                  CALL PGSCH   ( HCT_USE )
             END IF
        END IF
!
        CALL GETENVAR ( 'DIAGI_MULTI_TITLE_WIDTH', STR )
        IF ( ILEN(STR) .NE. 0 ) THEN
             READ ( UNIT=STR, FMT='(I11)', IOSTAT=IOS ) LCT_USE
             IF ( IOS .EQ. 0 ) THEN
                  CALL PGSLW   ( LCT_USE )
             END IF
        END IF
        CALL PGPTXT  ( XCT, YCT, 0.0, 0.5, COMMON_TITLE(1:I_LEN(COMMON_TITLE)) )
!
! ----- Put vertical and horizonatl lines
!
        CALL PGSLW  ( 1 )
        CALL PGSFS  ( 1 )
        CALL PGRECT ( VL_XLB, VL_XTU, VL_YLB, VL_YTU )
        CALL PGRECT ( HL_XLB, HL_XTU, HL_YLB, HL_YTU )
!
! ----- Set width and heiht of a box for a plot
!
        XWD = VL_XLB/(NC*(1.0+FRACT_WD)+FRACT_WD)
        YHT = HL_YLB/(NR*(1.0+FRACT_HT)+FRACT_HT)
!
! ----- Set shift from left and from up for the first plot
!
        XLF = FRACT_WD*XWD
        YUP = HL_YLB - FRACT_HT*YHT
        IPL = 0
        CALL PGSFS  ( 2 )
!
! ----- Compute actual number of plots to be displayed
!
        NPL_ACT = MPL
        IF ( NPL_ACT .GT. NC*NR ) NPL_ACT = NC*NR
        IF ( IPLT_INIT .GE. 1 .AND. IPLT_INIT .LE. MPL  .AND. MPL .GT. 0 ) THEN
!
! ---------- Special trick: bypass making multiplot
!
             CALL PGUNSA  ! 1
             CALL PGEBUF
             CALL PGUPDT
             GOTO 950
        END IF
!
! ----- Cycle for making plots
!
        DO 410 J1=1,NR
           DO 420 J2=1,NC
              IPL=IPL+1
              IF ( IPL .GT. MPL ) GOTO 420 ! no plots any more
!
              IF ( .NOT. F_PRI ) THEN
                   CALL PGSCH ( HCP   )
                   CALL PGSLW ( LCP   )
                 ELSE
                   CALL PGSCH ( HCP_P )
                   CALL PGSLW ( LCP_P )
              END IF
!
! ----------- Set box sizes
!
              BOX_PL(IPL)%XLB = XLF + (J2-1)*XWD*(1.0+FRACT_WD)
              BOX_PL(IPL)%YLB = YUP - (J1-1)*YHT*(1.0+FRACT_HT) - YHT
              BOX_PL(IPL)%XTU = XLF + XWD + (J2-1)*XWD*(1.0+FRACT_WD)
              BOX_PL(IPL)%YTU = YUP - (J1-1)*YHT*(1.0+FRACT_HT)
!
! ----------- Print a title of the plot
!
              CALL PGSCF   ( 1 )
              CALL PGPTXT  ( (BOX_PL(IPL)%XLB + BOX_PL(IPL)%XTU)/2.0, &
     &                       BOX_PL(IPL)%YTU + YHT*FRACT_TIT*FRACT_HT, &
     &                       0.0, 0.5, TITS(IPL)(1:I_LEN(TITS(IPL))) )
              IF ( DIAGI_S(IPL)%NCLR  .LE. 0 ) GOTO 420 ! no plot
!
              CALL PGSLW  ( 1 )
              IF ( .NOT. F_PRI ) THEN
                   CALL PGSCI ( ICOL_FRG )
                 ELSE
                   CALL PGSCI ( ICOL_FRP )
              END IF
!
! ----------- Fill a plot area by background color
!
              CALL PGSFS  ( 1 )
              CALL PGRECT ( BOX_PL(IPL)%XLB, BOX_PL(IPL)%XTU, &
     &                      BOX_PL(IPL)%YLB, BOX_PL(IPL)%YTU )
!
! ----------- Print a frame with the boundary around the plotting box
!
              CALL PGSCI  ( 1 )
              CALL PGSFS  ( 2 )
              CALL PGRECT ( BOX_PL(IPL)%XLB, BOX_PL(IPL)%XTU, &
     &                      BOX_PL(IPL)%YLB, BOX_PL(IPL)%YTU )
!
! ----------- Set sizes for small plot
!
              CALL DIAGI_SET ( IDEV_MUL, DIAGI_S(IPL) )
              IF ( F_PRI .AND.  DIAGI_S(IPL)%DEVICE(1:4) .EQ. '/GIF' ) THEN
!
! ---------------- Change width of lines for labels and a font height
! ---------------- for GIF-plots
!
                   IF ( MPL .LE. 16 ) DIAGI_S(IPL)%SCH_FRM  = 0.62
                   IF ( MPL .LE.  9 ) DIAGI_S(IPL)%SCH_FRM  = 0.70
                   IF ( MPL .LE.  4 ) DIAGI_S(IPL)%SCH_FRM  = 0.80
              END IF
!
! ----------- Adjust sizes (sizes are in mm)
!
              DIAGI_S(IPL)%XLEFT  = 0.0
              DIAGI_S(IPL)%XRIGHT = (XRIGHTS(IDEV) - XLEFTS(IDEV))* &
     &                              (BOX_PL(IPL)%XTU - BOX_PL(IPL)%XLB)
              DIAGI_S(IPL)%YBOT   = 0.0
              DIAGI_S(IPL)%YTOP   = (YTOPS(IDEV) - YBOTS(IDEV))* &
     &                              (BOX_PL(IPL)%YTU - BOX_PL(IPL)%YLB)
!
! ----------- Initialization of DiaGI for the IPL-th plot
!
              CALL ERR_PASS  ( IUER, IER )
              CALL DIAGI_INT ( 1, DIAGI_S(IPL), ICLR, IER )
              IF ( IER > 0 ) THEN
                   IF ( DIAGI_M%IBATCH .EQ. 0 ) THEN
                        CALL PGSLCT ( ID_XW )
                        CALL PGENDQ
                   END IF
!
                   CALL CLRCH ( STR )
                   CALL INCH  ( IPL, STR )
                   CALL ERR_LOG ( 4196, IUER, 'MULTI_DIAGI', 'Error in '// &
     &                 'initialization of DiaGI internal data structure '// &
     &                 'of the plot '//STR )
                   ICODE = -1
                   CALL PGUNSA  ! 1
                   CALL PGEBUF
                   CALL PGUPDT
                   RETURN
              END IF
!
! ----------- Set new viewing area which is equal to the plotting box
!
              CALL PGSVP ( BOX_PL(IPL)%XLB/XRT, BOX_PL(IPL)%XTU/XRT, &
     &                     BOX_PL(IPL)%YLB, BOX_PL(IPL)%YTU )
!
! ----------- Save title. The title of the plot will not be printed since it
! ----------- may appear too long for a small plot
! ----------- Save argument units. Argument units will be printed only for the
! ----------- the last plot in order to avoid overlapping and to avoid
! ----------- repetition
!
              TIT_SAVED = DIAGI_S(IPL)%ZAG
              ARU_SAVED = DIAGI_S(IPL)%ARG_UNITS
!
! ----------- Temporarily clear title
!
              CALL CLRCH ( DIAGI_S(IPL)%ZAG )
!
! ----------- Temporarily clear argument units unless this is the last plot
!
              IF ( IPL .NE. NPL_ACT ) CALL CLRCH ( DIAGI_S(IPL)%ARG_UNITS )
!
! ----------- Disable setting viewspace and screen rasing for DIAGI_SET_FRAME
!
              DIAGI_S(IPL)%SET_VP       = .FALSE.
              DIAGI_S(IPL)%ERASE_SCREEN = .FALSE.
!
! ----------- Scaling offset for argument units
!
              DIAGI_S(IPL)%YSH_ARU = DIAGI_S(IPL)%YSH_ARU*(1.0 + 0.66*(NR-1) )
!
! ----------- Plot a frame: axis box and labels
!
              CALL DIAGI_SET_FRAME ( DIAGI_S(IPL), ' ' )
!
! ----------- Restore title and argument units
!
              DIAGI_S(IPL)%ZAG = TIT_SAVED
              DIAGI_S(IPL)%ARG_UNITS = ARU_SAVED
!
! ----------- Set colors for the IPL-th plot
!
              CALL DIAGI_CLS ( DIAGI_S(IPL), IER )
              IF ( F_PRI ) THEN
                   CALL PGSCR  ( 0, 1.0, 1.0, 1.0 ) ! pure white background
                   CALL PGSCR  ( 1, 0.0, 0.0, 0.0 ) ! pure black foreground
              END IF
!
! ----------- ... then drawing plots of the functions, colour by colour.
! ----------- The next colour overlaps the previous one.
!
              DO 430 J3=1,DIAGI_S(IPL)%NCLR
                 CALL DIAGI_DRAW ( DIAGI_S(IPL), J3, 0, &
     &                DIAGI_S(IPL)%NPOI(J3), &
     &                %VAL(DIAGI_S(IPL)%ADR_X4(J3)), &
     &                %VAL(DIAGI_S(IPL)%ADR_Y4(J3)), &
     &                %VAL(DIAGI_S(IPL)%ADR_E4(J3)), &
     &                %VAL(DIAGI_S(IPL)%ADR_X8(J3)), &
     &                %VAL(DIAGI_S(IPL)%ADR_Y8(J3)) )
 430          CONTINUE
!
! ----------- Restore viewing area and window area
!
              CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0 )
              CALL PGSWIN  ( 0.0, XRT, 0.0, 1.0 )
              CALL PGSLW   ( 1 )
              CALL PGSCF   ( 1 )
              CALL PGUPDT 
              CALL PGEBUF 
 420       CONTINUE
 410    CONTINUE
!
        IF ( .NOT. F_PRI ) THEN
!
! ---------- Create a command button
!
! ---------- Set width and height of a box for a command
!
             XWD = (1.0 - VL_XTU)/(1.0+2.0*FRACT_WD)
             YHT = YUP_BUT/(MPB+(MPB+1)*FRACT_HT)
!
! ---------- Set shift from left and from up for the first command
!
             YUP = YUP_BUT - FRACT_HT*YHT
             XLF = VL_XTU + XWD*FRACT_WD
!
             DO 440 J4=1,MPB
!
! ------------- Set box coordinates
!
                BOX_OP(J4)%XLB = XLF
                BOX_OP(J4)%YLB = YUP - (J4-1)*YHT*(1.0+FRACT_HT) - YHT
                BOX_OP(J4)%XTU = XLF + XWD
                BOX_OP(J4)%YTU = YUP - (J4-1)*YHT*(1.0+FRACT_HT)
!
! ------------- If the name of button command is not empty then -- make a
! ------------- box with command name. If it is empty it only holds the place
! ------------- and a "hole" will be displayed there
!
                IF ( ILEN(BUTTON_NAME(J4)) .GT. 0 ) THEN
!
! ---------------- Fill a box by new background color
!
                   CALL PGSCI  ( ICOL_OPR )
                   CALL PGSFS  ( 1 )
                   CALL PGRECT ( BOX_OP(J4)%XLB, BOX_OP(J4)%XTU, &
     &                           BOX_OP(J4)%YLB, BOX_OP(J4)%YTU )
!
! ---------------- Print a frame around the box
!
                   CALL PGSCI  ( 1  )
                   CALL PGSFS  ( 2  )
                   CALL PGRECT ( BOX_OP(J4)%XLB, BOX_OP(J4)%XTU, &
     &                           BOX_OP(J4)%YLB, BOX_OP(J4)%YTU )
!
! ---------------- Print a letter code of the command
!
                   CALL PGSCH  ( HCA )
                   CALL PGSLW  ( LCP )
                   CALL PGSCI  ( ICOL_LET )
                   CALL PGPTXT ( BOX_OP(J4)%XLB + HCA/400., &
     &                           BOX_OP(J4)%YTU - HCA/50., 0.0, 0.0, &
     &                           BUTTON_LET(J4)(1:1) )
!
                   CALL PGSCI  ( 1  )
                   CALL PGSCH  ( HCA )
                   CALL PGSLW  ( LCA )
!
! ---------------- Now learn how many lines a command name has
!
                   IB = 1
                   IL = 0
                   DO 450 J5=1,100
                      IL = IL + 1
                      IE = (IB-1) + INDEX ( BUTTON_NAME(J4)(IB:), '|' )
                      IF ( IE .LT. IB ) GOTO 850
                      IB = IE + 1
 450               CONTINUE
 850               CONTINUE
!
! ---------------- Well. Now we know: it has IL lines
!
! ---------------- Then print the command name line by line
!
                   IB = 1
                   DO 460 J6=1,IL
!
! ------------------- Extract the line
!
                      IE = (IB-1) + INDEX ( BUTTON_NAME(J4)(IB:), '|' )
                      IF ( IE .LT. IB ) THEN
                           IE = I_LEN(BUTTON_NAME(J4))
                         ELSE
                           IE = IE-1
                      END IF
!
                      IF ( IB .GE. 0  .AND.  IE .GE. IB ) THEN
!
! ------------------------ Print it
!
                           CALL PGPTXT ( (BOX_OP(J4)%XLB + BOX_OP(J4)%XTU)/2.0, &
     &                                    BOX_OP(J4)%YTU - &
     &                     J6*(BOX_OP(J4)%YTU-BOX_OP(J4)%YLB)/(1.+IL)-HCA/120., &
     &                     0.0, 0.5, BUTTON_NAME(J4)(IB:IE) )
                      END IF
!
! ------------------- Go for the next line
!
                      IB = IE + 2
 460               CONTINUE
                   CALL PGSLW   ( 1 )
                END IF
 440         CONTINUE
        END IF
!
! ----- Flashing buffer
!
        CALL PGUNSA  ! 1
        CALL PGEBUF
        CALL PGUPDT
        IF ( F_PRI ) THEN
!
! ---------- Post-operations in PS or GIF mode
!
! ---------- Closing PGPLOT device
!
             CALL PGCLOS
!
! ---------- Inquiring environment variable for the consequent printing
!
             CALL GETENVAR ( DIAGI_PRICOM, PRI_STR )
             IF ( IPRN .EQ. 1  .AND. ILEN(PRI_STR) .EQ. 0 ) THEN
!
! ---------------- A printing command has not been selected
!
                   IPRN = 0
                 ELSE IF ( IPRN .EQ. 1 .AND. ILEN(PRI_STR) .GT. 0 ) THEN
!
! ---------------- Physical printing mode was selected and environment
! ---------------- variable has been set up
!
                   IL = LINDEX ( DEVICE_PRN, '/' ) - 1
!
! ---------------- Issuing the UNIX command for printing
!
                   CALL SYSTEM ( PRI_STR(1:I_LEN(PRI_STR))//' '// &
     &                           DEVICE_PRN(1:IL)//' &'//CHAR(0) )
             END IF
!
! ---------- Write information about creation of hard copy file at the screen
!
             IL = LINDEX ( DEVICE_PRN, '/' )
             IF ( IL .GT. 1 ) THEN
                  IL = IL -1
                ELSE
                  IL = I_LEN(DEVICE_PRN)
             END IF
             IF ( F_TERM ) THEN
                  WRITE (  6, 110 ) DEVICE_PRN(1:IL)
 110              FORMAT ( 1X,'File created: ',A )
             END IF
!
             IF ( F_WEB ) THEN
                  LUN = GET_UNIT ()
!
! --------------- Cycle on making individual plots
!
                  DO 470 J7=1,MPL
                     IF ( DIAGI_S(J7)%NCLR .LE. 0 ) GOTO 470
                     IF ( ILEN(DIAGI_S(J7)%NAME) .EQ. 0  .OR. &
     &                    DIAGI_S(J7)%NAME(1:1)  .EQ. CHAR(0) ) THEN
!
! ----------------------- Name was not specified. Set it.
!
                          CALL CLRCH  ( STR )
                          CALL INCH   ( J7,    STR(1:2) )
                          CALL CHASHR (        STR(1:2) )
                          CALL BLANK_TO_ZERO ( STR(1:2) )
                          DIAGI_S(J7)%NAME = 'plot'//STR(1:2)
                     END IF
!
! ------------------ Set the full name of device (filename of the file including
! ------------------ path/<device_type>)
!
                     CALL CLRCH ( FINAM )
                     IF ( DIAGI_S(J7)%NAME(1:1) .EQ. '/' ) THEN
                          FINAM = DIAGI_S(J7)%NAME
                        ELSE
!
! ----------------------- The path was not absolute -- then add a prefix
!
                          IF ( ILEN(FINAM) == 0 ) THEN
                               FINAM = DIAGI_S(J7)%NAME
                             ELSE 
                               FINAM = PREF_NAME(1:ILEN(PREF_NAME))// &
     &                                 DIAGI_S(J7)%NAME
                          END IF
                     END IF
!
                     CALL CLRCH ( FINAM_TRY )
                     IF ( INDEX ( DEVICE_PRN, 'PS' ) .GT. 0 ) THEN
                          FINAM_TRY = FINAM(1:I_LEN(FINAM))//'.ps'
                        ELSE IF ( INDEX ( DEVICE_PRN, 'GIF' ) .GT. 0 ) THEN
                          FINAM_TRY = FINAM(1:I_LEN(FINAM))//'.gif'
                        ELSE
                          FINAM_TRY = FINAM
                     END IF
!
! ------------------ Try to open file
!
                     OPEN ( UNIT=LUN, FILE=FINAM_TRY, STATUS='UNKNOWN', &
     &                      IOSTAT=IO )
                     IF ( IO .EQ. 0 ) THEN
                          CLOSE ( UNIT=LUN )
                       ELSE
!
! ----------------------- Failure to open the oputput file
!
                          CALL PGENDQ()
                          CALL ERR_LOG ( 4197, IUER, 'MULTI_DIAGI', 'Error '// &
     &                        'in attempt to open a file with hardcopy '// &
     &                         FINAM_TRY )
                          RETURN
                     END IF
!
                     IF ( INDEX ( DEVICE_PRN, 'PS' ) .GT. 0 ) THEN
                          DEVICE_PRN = FINAM(1:I_LEN(FINAM))//'.ps'//DEVS(IDEV)
                        ELSE IF ( INDEX ( DEVICE_PRN, 'GIF' ) .GT. 0 ) THEN
                          DEVICE_PRN = FINAM(1:I_LEN(FINAM))//'.gif'//DEVS(IDEV)
                     END IF
!
! ------------------ Set sizes and open a PGPLOT device
!
                     CALL DIAGI_SET ( IDEV, DIAGI_S(J7) )
!
                     ID = PGOPEN ( DEVICE_PRN )
                     IF ( ID .LE. 0 ) THEN
                          IF ( DIAGI_M%IBATCH .EQ. 0 ) THEN
                               CALL PGSLCT ( ID_XW )
                               CALL PGENDQ
                          END IF
                          CALL ERR_LOG ( 4198, IUER, 'MULTI_DIAGI', &
     &                        'Error in attempt to open '//DEVICE_PRN )
                          ICODE = -1
                          RETURN
                     END IF
!
! ------------------ Successfull openning. Setting new colors, font size and
! ------------------ plotting window
!
                     CALL DIAGI_CLS ( DIAGI_S(J7), IER )
                     CALL PGSCF     ( 2 )
                     CALL PGSVP     ( 0.0, 1.0, 0.0, 1.0 )
!
! ------------------ Setting anew colour table for the new colour device
!
                     CALL PGSCR ( 0, 1.0, 1.0, 1.0 ) ! pure white background
                     CALL PGSCR ( 1, 0.0, 0.0, 0.0 ) ! pure black foreground
!
! ------------------ Drawing the plot: axis box and functions
!
                     DIAGI_S(J7)%SET_VP       = .TRUE.
                     DIAGI_S(J7)%ERASE_SCREEN = .TRUE.
                     CALL DIAGI_SET_FRAME ( DIAGI_S(J7), ' ' )
!
! ------------------ Drowing the plot: color by color
!
                     DO 480 J8=1,DIAGI_S(J7)%NCLR
                        CALL DIAGI_DRAW ( DIAGI_S(J7), J8, 0, &
     &                       DIAGI_S(J7)%NPOI(J8), &
     &                       %VAL(DIAGI_S(J7)%ADR_X4(J8)), &
     &                       %VAL(DIAGI_S(J7)%ADR_Y4(J8)), &
     &                       %VAL(DIAGI_S(J7)%ADR_E4(J8)), &
     &                       %VAL(DIAGI_S(J7)%ADR_X8(J8)), &
     &                       %VAL(DIAGI_S(J7)%ADR_Y8(J8)) )
 480                 CONTINUE
!
! ------------------ Closing printing device
!
                     CALL PGCLOS()
                     IF ( IPRN .EQ. 1  .AND.  ILEN(PRI_STR) .GT. 0 ) THEN
!
! ----------------------- Physical printing mode was selected and environment
! ----------------------- variable has been set up
!
                          IL = LINDEX ( DEVICE_PRN, '/' ) - 1
!
! ----------------------- Issuing UNIX command for printing
!
                          CALL SYSTEM ( PRI_STR(1:I_LEN(PRI_STR))//' '// &
     &                                  DEVICE_PRN(1:IL)//' &'//CHAR(0) )
                     END IF
!
! ------------------ Write information about creation of hard copy file
! ------------------ at the screen
!
                     IF ( INDEX ( DEVICE_PRN, 'PS' ) .GT. 0 ) THEN
                          WRITE (  6, 110 ) FINAM(1:I_LEN(FINAM))//'.ps'
                        ELSE
                          WRITE (  6, 110 ) FINAM(1:I_LEN(FINAM))//'.gif'
                     END IF
 470              CONTINUE
             END IF
             IF ( DIAGI_M%IBATCH .EQ. 1 ) GOTO 810
!
! ---------- Disable non-interactive mode
!
             XRT   = 1.0
             F_PRI = .FALSE.
             IDEV  = IDEV_XS
!
! ---------- Selecting again X-server as output device
!
             CALL PGSLCT ( ID_XW )
!
! ---------- Ressetiing current plottiing parameters
!
             CALL DIAGI_SET ( IDEV, DIAGI_M )
             CALL PGSVP     ( 0.0, 1.0, 0.0, 1.0  )
!
! ---------- Issuing final printing message
!
             IF ( F_WEB ) THEN
!
! --------------- Issue a message about success
!
                  CALL PGSAVE ! 2
!
! --------------- Deleting previous window
!
                  CALL PGERAS
!
! --------------- Setting new world coodrinates
!
                  CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
                  CALL PGSCH   ( 2.0 )
                  CALL PGSLW   ( 5   )
                  CALL PGSCI   ( 1   )
                  XC = 0.50
                  YC = 0.55
!
                  CALL CLRCH  ( STR )
                  STR = 'Plots were written in files with prefixes'
                  CALL PGPTXT ( XC, YC, 0.0, 0.5, STR(1:I_LEN(STR)) )
                  CALL CLRCH  ( STR )
!
                  YC = 0.45
                  CALL CLRCH  ( STR )
                  STR = PREF_NAME
                  CALL PGPTXT ( XC, YC, 0.0, 0.5, STR(1:I_LEN(STR)) )
!
                  CALL PGBAND ( 0, 0, XC, YC, XC, YC, CH )
                  CALL PGUNSA ! 2
                ELSE
                  CALL DIAGI_PPR ( DEVICE_PRN, 0, IPRN )
             END IF
             F_WEB = .FALSE.
             GOTO 920
        END IF
!
! ----- Printing MULTI_DIAGI label
!
        CALL PGSCF  ( 2   )
        CALL PGSCH  ( HCL )
        CALL PGSLW  ( LCL )
        CALL PGPTXT ( (1.0 + VL_XLB1)/2.0, HCL/200.0, 0.0, 0.5, &
     &                MULTI_DIAGI_LABEL__DEF )
!
 940    CONTINUE
!
! ----- Waiting for user hitting a cursor or mouse button
!
        ICODE = 0
        IF ( IPLT_INIT .GE. 1 .AND. IPLT_INIT .LE. MPL  .AND.  MPL .GT. 0 ) THEN
!
! ---------- Do not wait, bypass it
!
             XC = 0.5
             YC = 0.5
             CH = CHAR(0)
           ELSE
             CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
        END IF
!
! ----- Analysis of what has been entered
!
! ----- Firstly parsing of the keyboard code
!
! ----- Then analysis: in which box the cursor is pointing
!
        IF ( CH .EQ. 'X'  .OR.  CH .EQ. 'x' ) THEN
             ICODE = 0
             GOTO 810
          ELSE IF ( CH .EQ. '?' ) THEN
!
! ---------- Help was requested
!
             CALL ERR_PASS  ( IUER, IER )
             IF ( MPL .GT. 0  .AND.  MPB .GT. 0 ) THEN
                  CALL DIAGI_HLP ( DIAGI_HLP_FIL3, IER )
                ELSE IF ( MPL .LE. 0  .AND.  MPB .GT. 0 ) THEN
                  CALL DIAGI_HLP ( DIAGI_HLP_FIL4, IER )
                ELSE IF ( MPL .GT. 0  .AND.  MPB .LE. 0 ) THEN
                  CALL DIAGI_HLP ( DIAGI_HLP_FIL5, IER )
                ELSE
                  CALL DIAGI_HLP ( DIAGI_HLP_FIL6, IER )
             END IF
             IF ( IER > 0 ) THEN
                  CALL PGSLCT ( ID_XW )
                  CALL PGENDQ
!
                  CALL ERR_LOG ( 4199, IUER, 'MULTI_DIAGI', 'Error in '// &
     &                'an attempt to print help file on the screen' )
                  ICODE = -1
                  RETURN
             END IF
             GOTO 920
          ELSE IF ( ( CH .EQ. CHAR(16)  .OR.  CH .EQ. CHAR(23) )  .AND. &
     &              MPL .GT. 0 ) THEN
!
! ---------- User hit CNTRL/P  or CNTRL/W
!
             IF ( CH .EQ. CHAR(23) ) THEN
!
! --------------- Case of CNTRL/W
!
                  IF ( PREF_NAME(1:1) .EQ. ' ' ) THEN
                       CALL PGSAVE ! 3
!
! -------------------- Deleting previous window
!
                       CALL PGERAS
!
! -------------------- Setting new world coodrinates
!
                       CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
                       CALL PGSCH   ( 2.0 )
                       CALL PGSLW   ( 5   )
                       CALL PGSCI   ( ERR_CLRI )
                       XC = 0.50
                       YC = 0.55
!
                       CALL CLRCH  ( STR )
                       STR = 'Plot were not created.'
                       CALL PGPTXT ( XC, YC, 0.0, 0.5, STR(1:I_LEN(STR)) )
                       CALL CLRCH  ( STR )
!
                       YC = 0.45
                       CALL CLRCH  ( STR )
                       STR = 'Directory name for Web plots was empty.'
                       CALL PGPTXT ( XC, YC, 0.0, 0.5, STR(1:I_LEN(STR)) )
!
                       CALL PGBAND ( 0, 0, XC, YC, XC, YC, CH )
                       CALL PGUNSA ! 3
                       GOTO 920
                     ELSE
                       F_WEB = .TRUE.
                  END IF
             END IF
!
! ---------- Request to make a plot in non-interactive mode (GIF or PS) for
! ---------- making a hardcopy
!
             CALL DIAGI_PRN ( ' ', DEVICE_PRN, IDEV, IPRN )
             IF ( IPRN .EQ. 2 ) THEN
                  CALL PGENDQ
                  CALL ERR_LOG ( 4200, IUER, 'MULTI_DIAGI', 'Error '// &
     &                'in attempt to open a file with hardcopy '//DEVICE_PRN )
                  RETURN
             END IF
!
             IF ( IDEV .GT. 0 ) THEN
!
! --------------- New non-interactive plotting device has been selected.
! --------------- Set utmost right plot coordinate
!
                  XRT   = VL_XLB - 0.0001
                  F_PRI = .TRUE. ! Print
                ELSE
                  IDEV  = IDEV_XS
                  F_PRI = .FALSE.
                  F_WEB = .FALSE.
                  GOTO 920       ! Replot
             END IF
!
! ---------- Set name for a hardcopy file name for a general plot
!
             CALL CLRCH ( FINAM )
!
             IF ( INDEX ( DEVICE_PRN, 'PS' ) .GT. 0 ) THEN
                  FINAM = PREF_NAME(1:I_LEN(PREF_NAME))//'all.ps'
                ELSE IF ( INDEX ( DEVICE_PRN, 'GIF' ) .GT. 0 ) THEN
                  FINAM = PREF_NAME(1:I_LEN(PREF_NAME))//'all.gif'
             END IF
             CALL CLRCH ( DEVICE_PRN )
             DEVICE_PRN = FINAM(1:I_LEN(FINAM))//DEVS(IDEV)
!
! ---------- Try to open file
!
             LUN = GET_UNIT ()
             OPEN ( UNIT=LUN, FILE=FINAM, STATUS='UNKNOWN', IOSTAT=IO )
             IF ( IO .EQ. 0 ) THEN
                  CLOSE ( UNIT=LUN )
                ELSE
!
! --------------- Failure to open the oputput file
!
                  CALL PGENDQ
                  CALL ERR_LOG ( 4201, IUER, 'MULTI_DIAGI', 'Error '// &
     &                'in attempt to open a file with hardcopy '//FINAM )
                  RETURN
             END IF
             GOTO 910       ! Make plot from the very beginning
        END IF
!
! ----- Look: has user entered a letted code of the command
!
        ID = 0
        DO 490 J9=1,MPB
           IF ( INDEX ( BUTTON_LET(J9), CH ) .GT. 0 ) THEN
                ID = J9
                IF ( ILEN(BUTTON_NAME(ID)) .GT. 0 ) THEN
!
! ------------------ Yes. Then exit.
!
                     ICODE = ID
                     GOTO 810
                END IF
           END IF
 490    CONTINUE
!
! ----- Look: has user clicked a plotting box by mouse
!
 950    CONTINUE
!
! ----- Check, whether we have selected the plotting box
!
        IF ( IPLT_INIT .GE. 1 .AND. IPLT_INIT .LE. MPL  .AND. MPL .GT. 0 ) THEN
!
! ---------- No, no, we know it
!
             ID = IPLT_INIT
             IPLT_INIT = 0
           ELSE
!
! ---------- Look
!
             ID = DIAGI_INBOX ( MPL, BOX_PL, XC, YC )
        END IF
!
 960    CONTINUE
        IF ( ID .GT. 0 ) THEN
             IF ( DIAGI_S(ID)%NCLR .GT. 0 ) THEN
!
! --------------- Yes, he/she has. Then make a large picture of the specified
! --------------- plot and descend to DiaGi level
!
                  ITRM_SAVED = DIAGI_S(ID)%ITRM
                  DIAGI_S(ID)%ITRM = DIAGI__ERASE
                  CALL ERR_PASS ( IUER, IER )
                  CALL DIAGI ( DIAGI_S(ID), IER )
                  IF ( IER > 0 ) THEN
                       CALL PGENDQ
                       CALL ERR_LOG ( 4202, IUER, 'MULTI_DIAGI', 'Error in '// &
     &                     'attempt to make a large plot' )
                       ICODE = -1
                       RETURN
                  END IF
!
! --------------- Re-set the status and re-plot the screen
!
                  DIAGI_S(ID)%ITRM   = ITRM_SAVED
                  DIAGI_S(ID)%STATUS = DIA__DEF
                  IF ( DIAGI_S(ID)%MD_OUT .EQ. DIAGI__QUIT ) THEN
                       ICODE = 0
                       GOTO 810
                  END IF
!
! --------------- Special case: a user may hit PgDn or PgUp. This means he
! --------------- or she wants to look at the second DiaGi plot. Well, let's
! --------------- grant user's request
!
                  IF ( DIAGI_S(ID)%LAST_KEY == DIAGI__PGUP ) THEN
                       ID = ID - 1
!@                       IF ( ID .LT. 1 ) ID = MPL
                       IF ( ID .LT. 1 ) THEN
                            ICODE = -2
                            GOTO 810
                       END IF
                       GOTO 960
                  END IF
                  IF ( DIAGI_S(ID)%LAST_KEY == DIAGI__PGDN ) THEN
                       ID = ID + 1
!@                       IF ( ID .GT. MPL ) ID = 1
                       IF ( ID .GT. MPL ) THEN
                            ICODE = -3
                            GOTO 810
                       END IF
                       GOTO 960
                  END IF
                  GOTO 910
             END IF
        END IF
!
! ----- Look: has user clicked a command box by mouse
!
        ID = DIAGI_INBOX ( MPB, BOX_OP, XC, YC )
        IF ( ID .GT. 0 ) THEN
!
! ---------- Yes. Then exit.
!
             IF ( ILEN(BUTTON_NAME(ID)) .GT. 0 ) THEN
                  ICODE = ID
                  GOTO 810
             END IF
        END IF
      GOTO 940
!
 810  CONTINUE
!
! --- Erasing the screen
!
      IF ( DIAGI_M%IBATCH .EQ. 0 ) THEN
           CALL GRQCAP ( PGPLOT_DEFSTR )
         ELSE
           PGPLOT_DEFSTR = 'NNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNN'
      END IF
      IF ( MPL .GT. 0 ) THEN
           ITRM_SAVED = DIAGI_M%ITRM
         ELSE
           ITRM_SAVED = DIAGI__CLOSE
      END IF
      IF ( ITRM_SAVED  .EQ. DIAGI__CLOSE_VERBOSE  .AND. &
     &     PGPLOT_DEFSTR(8:8) .EQ. 'N' ) THEN
!
           CALL PGERAS
!
! -------- Printing farwell message
!
           CALL PGSCH    ( 1.666 )
           CALL PGSLW    ( 5     )
           CALL PGPTXT   ( (DIAGI_M%XMAX+DIAGI_M%XMIN)/2, &
     &                     (DIAGI_M%YMAX+DIAGI_M%YMIN)/2, 0.0, 0.5, &
     &                     'Please, iconify (by <Alt/blank> <n>) '// &
     &                     'graphic window manually' )
      END IF
!
      IF ( ITRM_SAVED .EQ. DIAGI__ERASE  ) THEN
           CALL PGERAS
         ELSE IF ( ITRM_SAVED .EQ. DIAGI__KEEP ) THEN
           CONTINUE
         ELSE
!
! -------- Closing plotting device
!
           CALL PGENDQ
      END IF
      IF ( MPL .GT. 0 ) THEN
           DO 4100 J10=1,MPL
!
! ----------- Deallocation memory used by DiaGI
!
              IF ( DIAGI_S(J10)%STATUS .EQ. DIA__ALL ) THEN
                   CALL ERR_PASS  ( IUER, IER )
                   CALL DIAGI_INT ( 2, DIAGI_S(J10), ICLR, IER )
                   IF ( IER > 0 ) THEN
                        ICODE = -1
                        CALL ERR_LOG ( 4203, IUER, 'MULTI_DIAGI', 'Error in '// &
     &                      'an attempt to make a large plot' )
                        RETURN
                   END IF
              END IF
 4100      CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MULTI_DIAGI  #!#
