      SUBROUTINE MDLPL_EXT ( IMODE, DBNAME, HFEOP_CMP_FINAM, PREF_NAME, &
     &           MDLPL_IPS_PAG, MDLPL_IPC_PAG, MLDLP_FL_EOPMOD, MDLPL_FL_CLF, &
     &           MDLPL_FL_FRAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MDLPL_EXT  makes various plots kind in interactive mode.  *
! *   It supports two modes:                                             *
! *                                                                      *
! *   IMODE=1 (MDLPL_EXT) -- pre-JUL99 style of plots of                 *
! *           a) station dependent clock function modeled by linear      *
! *              spline;                                                 *
! *           b) total station dependent clock function;                 *
! *           c) evolution of adjustments to atmosphere path delay;      *
! *           d) plot of high-frequency variations of the Earth          *
! *              Orientation Parameters.                                 *
! *           Plots are made by using DiaGI interface.                   *
! *                                                                      *
! *   IMODE=2 (MDLPL_PLUS) -- post-JUL99 style of plots of               *
! *           a) station dependent clock function modeled by linear      *
! *              spline;                                                 *
! *           b) evolution of adjustments to atmosphere path delay;      *
! *           c) plot of high-frequency variations of the EOP.           *
! *           d) plot of postfit residuals for each baseline;            *
! *           e) plot of postfit residuals plus clock function for each  *
! *              baseline;                                               *
! *           Plots are made by using Multi_DiaGI interface.             *
! *                                                                      *
! *     It is assumed that solution has been made, postfit residuals are *
! *   written in residual file, values of adjustments to clock function, *
! *   atmosphere path delay EOP are written in the corresponding files.  *
! *                                                                      *
! *     X-resources for pgxwin should be specified as                    *
! *                                                                      *
! *   pgxwin.Win.geometry:      1260x800+0+90                            *
! *   pgxwin.server.visible:    True                                     *
! *   pgxwin.Win.maxColors:     16                                       *
! *   pgxwin.Win.iconize:       True                                     *
! *                                                                      *
! *   for correct work of MDLPL_EXT and MDLPL_PLUS                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       DBNAME ( CHARACTER ) -- String with database name and version  *
! *                               number.                                *
! * HFEOP_CMP_FINAM ( CHARACTER ) -- file name with model of high        *
! *                                  frequency variations of EOP used    *
! *                                  for comparison purposes.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *        IMODE ( INTEGER*4 ) -- MDLPL mode. Input mode is 2. MDLPL_PLUS*
! *                               may reset mode. Then upon completion   *
! *                               another routine from MDLPL family      *
! *                               will be called.                        *
! *                            2 -- MDLPL_PLUS (without change, MDLPL    *
! *                                 will terminate upon completion).     *
! *                            1 -- MDLPL_EXT (it will be called upon    *
! *                                 completion of MPLPL_PLUS).           *
! *                            0 -- MDLPL old mode (it will be called    *
! *                                 upon completion of MPLPL_PLUS).      *
! *    PREF_NAME ( CHARACTER ) -- Prefix string with pathname which will *
! *                               be prepend before the second part of   *
! *                               filename of hardcopies of the plots.   *
! * MDLPL_IPS_PAG ( INTEGER*4) -- Number of the page to be displayed     *
! *                               (for IPSF_TYP plot type).              *
! * MDLPL_IPC_PAG ( INTEGER*4) -- Number of the page to be displayed     *
! *                               (for IPSC_TYP plot type).              *
! * MDLPL_FL_EOPMOD (LOGICAL*4) - Flag of plotting model values for EOP. *
! *                               If TRUE then the a priori and model    *
! *                               values of EOP will be displayed also   *
! *                               (for IEOP_TYP plot only).              *
! * MDLPL_FL_CLF ( LOGICAL*4 ) -- Flag of plotting clock function.       *
! *                               If TRUE then clock function is         *
! *                               displayed (for IPSC_TYP plot type      *
! *                               only).                                 *
! * MDLPL_FL_FRAME (LOGICAL*4) -- Flag. If .TRUE. then the plotting      *
! *                               frame is "global" -- the same for all  *
! *                               stations. If .FALSE. then the          *
! *                               individual frame: min, max is set for  *
! *                               each station.                          *
! *         IUER ( INTEGER*4, OPT ) -- Universal error handler.          *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  31-OCT-97    MDLPL_EXT   v2.0  (c)  L. Petrov  17-AUG-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INCLUDE    'prfil.i'
      INCLUDE    'precm.i'
      INCLUDE    'diagi.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4  IMODE, IUER
      INTEGER*4  M_STA, M_PTS, MBUF, MSTR
      INTEGER*4  MDLPL_IPS_PAG, MDLPL_IPC_PAG
      LOGICAL*4  MLDLP_FL_EOPMOD, MDLPL_FL_CLF, MDLPL_FL_FRAME
      CHARACTER  HFEOP_CMP_FINAM*(*), DBNAME*(*), PREF_NAME*(*)
!
      INTEGER*4  L_STA, L_PTS, LBUF_CLO, LBUF_CLG, LBUF_ATM, LBUF_EOP, &
     &           I_TYP, I_STA, ICLREF_STA
      LOGICAL*4  LCLT_USE, LATM_USE, LEOP_USE
      PARAMETER  ( M_STA = MAX_ARC_STA )
      PARAMETER  ( M_PTS = MAX_PTS     )
      PARAMETER  (  MSTR = 2048        )
      PARAMETER  (  MBUF = 2048        )
      INTEGER*4  J1, IER
      CHARACTER  STANAM(M_STA)*8,       STAT_NAME*8, &
     &           FINAM_CLO*255,   BUF_CLO(MBUF)*(MSTR), &
     &           FINAM_CLG*255,   BUF_CLG(MBUF)*(MSTR), &
     &           FINAM_ATM*255,   BUF_ATM(MBUF)*(MSTR), &
     &           FINAM_EOP*255,   BUF_EOP(MBUF)*(MSTR), &
     &           MDLPL_EXT__LABEL*21, FINAM_EOPCMP*255
      REAL*8     TIME_ARR(M_PTS), VAL_ARR(M_PTS), SIG_ARR(M_PTS), &
     &           MODU_ARR(M_PTS), MODC_ARR(M_PTS)
      INTEGER*4  IUSE_STA(M_STA), ISEL_STA, ISEL_TYP
      LOGICAL*4  CHECK_STABIT
      PARAMETER  ( MDLPL_EXT__LABEL = 'MDLPL_EXT   23-SEP-98' )
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
!
      CALL END_MN()
      CALL SOCOM_EXT()
!
! --- Setting names of atmophere and clock files
!
      CALL CLRCH ( FINAM_CLO )
      FINAM_CLO = PRE_SCR_DIR(:PRE_SD_LEN)//'CLOC'//PRE_LETRS
!
      CALL CLRCH ( FINAM_CLG )
      FINAM_CLG = PRE_SCR_DIR(:PRE_SD_LEN)//'CLOT'//PRE_LETRS
!
      CALL CLRCH ( FINAM_ATM )
      FINAM_ATM = PRE_SCR_DIR(:PRE_SD_LEN)//'ATMO'//PRE_LETRS
!
      CALL CLRCH ( FINAM_EOP )
      FINAM_EOP = PRE_SCR_DIR(:PRE_SD_LEN)//'EOPP'//PRE_LETRS
!
      CALL CLRCH ( FINAM_EOPCMP )
      FINAM_EOPCMP = PRE_SAV_DIR(:PRE_SV_LEN)//'/'//HFEOP_CMP_FINAM
!
! --- Reading high frequency EOP file which holds comparison model
!
!@      CALL INIT_HF_EOP ( FINAM_EOPCMP, INT2(1) )
      L_STA = NUMSTA
!
! --- Learn station status. Gather station names in the array STANAM.
! --- Learn a clock reference station
!
! --- Set station status: 0 -- deselected;
! ---                     1 -- selected in solution
! ---                     2 -- clock reference station
! --- Set the station which wiull be displayed the first in MDLPL_EXT mode
!
      ICLREF_STA = BM_REF_CL
      ISEL_STA = 0
      DO 410 J1=1,L_STA
         CALL CLRCH ( STANAM(J1) )
         CALL LIB$MOVC3 ( 8, ISITN(1,J1), %REF(STANAM(J1)) )
         IUSE_STA(J1) = 1
         IF ( J1 .EQ. ICLREF_STA ) IUSE_STA(J1) = 2
         IF ( .NOT. CHECK_STABIT ( INT2(J1) ) ) IUSE_STA(J1) = 0
         IF ( ISEL_STA .EQ. 0  .AND. IUSE_STA(J1) .EQ. 1 ) ISEL_STA = J1
 410  CONTINUE
      IF ( ISEL_STA .EQ. 0  ) ISEL_STA = 1
!
! --- Reading file for segmented clocks
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM_CLO, MBUF, BUF_CLO, LBUF_CLO, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9221, IUER, 'MDLPL_EXT', 'Error during reading '// &
     &         'segmented clock file '//FINAM_CLO )
           RETURN
      END IF
!
! --- Reading for total clocks
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM_CLG, MBUF, BUF_CLG, LBUF_CLG, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9222, IUER, 'MDLPL_EXT', 'Error during reading '// &
     &         'total clock file '//FINAM_CLG )
           RETURN
      END IF
!
! --- Reading atmosphere file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FINAM_ATM, MBUF, BUF_ATM, LBUF_ATM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9223, IUER, 'MDLPL_EXT', 'Error during reading '// &
     &         'atmosphere file '//FINAM_ATM )
      END IF
!
! --- Reading EOP file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT ( FINAM_EOP, MBUF, BUF_EOP, LBUF_EOP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9224, IUER, 'MDLPL_EXT', 'Error during reading '// &
     &         'EOP file '//FINAM_EOP )
      END IF
!
! --- Set status of estimation
!
      ISEL_TYP = 1
      LCLT_USE = .TRUE.
      IF ( .NOT. UNF_CLO ) LCLT_USE = .FALSE.
      LATM_USE = .TRUE.
      IF ( .NOT. UNF_ATM ) LATM_USE = .FALSE.
      LEOP_USE = .TRUE.
      IF ( .NOT. UNF_EOP ) LEOP_USE = .FALSE.
!
      IF ( IMODE .EQ. 2 ) THEN
!
! -------- Call MDLPL_PLUS routine
!
           CALL ERR_PASS ( IUER, IER )
           CALL MDLPL_PLUS ( IMODE, L_STA, STANAM, DBNAME, PREF_NAME, &
     &          LBUF_ATM, BUF_ATM, FINAM_ATM, LATM_USE, &
     &          LBUF_CLO, BUF_CLO, FINAM_CLO, IUSE_STA, &
     &          LBUF_EOP, BUF_EOP, FINAM_EOP, LEOP_USE, &
     &          MDLPL_IPS_PAG, MDLPL_IPC_PAG, MLDLP_FL_EOPMOD, MDLPL_FL_CLF, &
     &          MDLPL_FL_FRAME, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9225, IUER, 'MDLPL_EXT', 'Error in MDLPL_PLUS' )
                RETURN
           END IF
!
           IF ( IMODE .EQ. 1 ) THEN
!
! ------------- Continue MDLPL_EXT
!
                GOTO 810
             ELSE
!
! ------------- Terminate MDLPL_EXT
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
      END IF
 810  CONTINUE
!
! --- Call MDLPL_EST menu
!
 910  CONTINUE
      CALL ERR_PASS ( IUER, IER )
      CALL MDLPL_EXT_MENU ( L_STA, STANAM, ICLREF_STA, IUSE_STA, ISEL_STA, &
     &                      ISEL_TYP, I_TYP, I_STA, MDLPL_EXT__LABEL, LCLT_USE, &
     &                      LATM_USE, LEOP_USE, IER )
!
! --- Get arrays of values and their formal uncertainties
!
      IF ( I_STA .GT. 0 ) THEN
           STAT_NAME = STANAM(I_STA)
           CALL ERR_PASS ( IUER, IER )
           IF ( I_TYP .EQ. ICLP_TYP ) THEN
!
! ------------- Get estimates of segmented clock function
!
                CALL GETSTA_PTS ( M_PTS, L_PTS, STAT_NAME, I_TYP, LBUF_CLO, &
     &                            BUF_CLO, TIME_ARR, VAL_ARR, SIG_ARR, MODU_ARR, &
     &                            MODC_ARR, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9226, IUER, 'MDLPL_EXT', 'Error during '// &
     &                   'parsing segmented clock file '//FINAM_CLO )
                     RETURN
                END IF
              ELSE IF ( I_TYP .EQ. ICLT_TYP ) THEN
!
! ------------- Get estimates of totla clock function
!
                CALL GETSTA_PTS ( M_PTS, L_PTS, STAT_NAME, I_TYP, LBUF_CLG, &
     &                            BUF_CLG, TIME_ARR, VAL_ARR, SIG_ARR, MODU_ARR, &
     &                            MODC_ARR, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9227, IUER, 'MDLPL_EXT', 'Error during '// &
     &                   'parsing global clock file '//FINAM_ATM )
                     RETURN
                END IF
              ELSE IF ( I_TYP .EQ. IATP_TYP ) THEN
!
! ------------- Get estimates of atmosphere path delay
!
                CALL GETSTA_PTS ( M_PTS, L_PTS, STAT_NAME, I_TYP, LBUF_ATM, &
     &                            BUF_ATM, TIME_ARR, VAL_ARR, SIG_ARR, MODU_ARR, &
     &                            MODC_ARR, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9228, IUER, 'MDLPL_EXT', 'Error during '// &
     &                   'parsing atmosphere file '//FINAM_ATM )
                     RETURN
                END IF
              ELSE IF ( I_TYP .EQ. IXPL_TYP  .OR.  I_TYP .EQ. IYPL_TYP  .OR. &
     &                  I_TYP .EQ. IUT1_TYP                             ) THEN
!
! ------------- Get estimates of EOP
!
                CALL GETSTA_PTS ( M_PTS, L_PTS, STAT_NAME, I_TYP, LBUF_EOP, &
     &                            BUF_EOP, TIME_ARR, VAL_ARR, SIG_ARR, MODU_ARR, &
     &                            MODC_ARR, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9229, IUER, 'MDLPL_EXT', 'Error during '// &
     &                   'parsing atmosphere file '//FINAM_ATM )
                     RETURN
                END IF
           END IF
!
           IF ( L_PTS .EQ. 0 ) THEN
!
! ------------- Case when no EOP data have been read
!
                L_PTS = 2
                TIME_ARR(1) =  0.0
                TIME_ARR(2) = 24.0
                VAL_ARR(1)  =  0.0
                VAL_ARR(2)  =  0.0
                SIG_ARR(1)  =  0.0
                SIG_ARR(2)  =  0.0
                MODU_ARR(1) =  0.0
                MODU_ARR(2) =  0.0
           END IF
!
! -------- Gather parameters for the plot
!
           CALL ERR_PASS    ( IUER, IER )
           CALL MDLPL_DIAGI ( L_PTS, TIME_ARR, VAL_ARR, SIG_ARR, MODU_ARR, &
     &                        MODC_ARR, I_TYP, DBNAME, STAT_NAME, PREF_NAME, &
     &                        DIAGI_S, IER )
!
! -------- Calling the main routine of DiaGI
!
           CALL ERR_PASS    ( IUER, IER )
           CALL DIAGI       ( DIAGI_S, IER )
           IF ( IER .NE. 0 ) THEN
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
           END IF
           GOTO 910
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MDLPL_EXT  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MDLPL_EXT_MENU ( L_STA, STANAM, ICLREF_STA, IUSE_STA, &
     &                            ISEL_STA, ISEL_TYP, I_TYP, I_STA, &
     &                            MDLPL_EXT__LABEL, LCLT_USE, LATM_USE, &
     &                            LEOP_USE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MDLPL_EXT_MENU  requests the station and the plot type    *
! *   in graphic pictogram-style menu. It awaits user reaction parses    *
! *   user responce and returns new plotting type and new station code.  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      L_STA ( INTEGER*4 ) -- Total number of stations participated in *
! *                             the session.                             *
! *     STANAM ( CHARACTER ) -- Stations name list. Dimension: L_STA.    *
! * ICLREF_STA ( INTEGER*4 ) -- Index of the clock reference station in  *
! *                             the stations list.                       *
! *   IUSE_STA ( INTEGER*4 ) -- Array of station usage codes:            *
! *                          0  -- station was deselected from solution; *
! *                          1  -- normal station;                       *
! *                          2  -- clock reference station.              *
! * MDLPL_EXT__LABEL ( CHARACTER ) -- String with NDLPL_EXT label.       *
! *   LCLT_USE ( LOGICAL*4 ) -- Flag. TRUE if segmented clock function   *
! *                             has been estimated for this session.     *
! *   LATM_USE ( LOGICAL*4 ) -- Flag. TRUE if segmented atmoshere path   *
! *                             delay has been estimated for this        *
! *                             session.                                 *
! *   LATM_EOP ( LOGICAL*4 ) -- Flag. TRUE if segmented EOP have been    *
! *                             estimated for this session.              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      I_TYP ( INTEGER*4 ) -- Plot type. One of the:
! *                          ICLP_TYP -- "Piese-wise clock function"     *
! *                          ICLT_TYP -- "Total clock function"          *
! *                          IATP_TYP -- "Piese-wise atmosphere path     *
! *                                       delay"                         *
! *                          IXPL_TYP -- "Piese-wise X pole coordinate"  *
! *                          IYPL_TYP -- "Piese-wise Y pole coordinate"  *
! *                          IUT1_TYP -- "Piese-wise UT1 arguments"      *
! *      I_STA ( INTEGER*4 ) -- Index of the selected station in the     *
! *                             list of stations.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   ISEL_STA ( INTEGER*4 ) -- Input value: index of the station        *
! *                               selected in the previous call of       *
! *                               MDLPL_EXT_MENU.                        *
! *                             Output value: the same as I_STA.         *
! *   ISEL_TYP ( INTEGER*4 ) -- Input value: plot type set in the        *
! *                               previous call of MDLPL_EXT_MENU.       *
! *                             Output value: the same as I_TYP.         *
! *                                                                      *
! *  ###  03-NOV-97  MDLPL_EXT_MENU  v1.5 (c) L. Petrov  27-JUL-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  L_STA, ICLREF_STA, IUSE_STA(L_STA), ISEL_STA, &
     &           ISEL_TYP, I_TYP, I_STA, IUER
      CHARACTER  STANAM(L_STA)*(*), MDLPL_EXT__LABEL*(*)
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4  M_STA, M_TYP
      PARAMETER  ( M_STA = 32 )
      PARAMETER  ( M_TYP = 6  )
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      TYPE ( DIAGI_BOXES ) ::  BOX_STA(M_STA)
      TYPE ( DIAGI_BOXES ) ::  BOX_TYP(M_TYP)
      TYPE ( DIAGI_BOXES ) ::  BOX_OPR
      TYPE ( DIAGI_BOXES ) ::  BOX_QST
      TYPE ( DIAGI_BOXES ) ::  BOX_EXI
      INTEGER*4  ID_XW, IER, MROW, MCOL, IM, J1, J2, J3, J4, IC, IR, &
     &           N1$ARG, N2$ARG
      INTEGER*4  UCUR_STA, UPRV_STA, UCUR_TYP, UPRV_TYP, UTIT
      CHARACTER  STR*20, MES*80, CH*1, MES_OPR*16
      CHARACTER  MES_TYP(2,M_TYP)*16, ABR_TYP(M_TYP)*1
      LOGICAL*4  LCLT_USE, LATM_USE, LEOP_USE
      DATA       (( MES_TYP(N1$ARG, N2$ARG), N1$ARG=1,2 ), ABR_TYP(N2$ARG), &
     &              N2$ARG=1,M_TYP) &
     &           / &
     &             'Clock piece-wise',   'function        ',   'C', &
     &             'Total clock     ',   'function        ',   'T', &
     &             'Atm.  piece-wise',   'path delay      ',   'M', &
     &             'Xpole piece-wise',   'coordinates     ',   'Z', &
     &             'Ypole piece-wise',   'coordinates     ',   'Y', &
     &             'UT1   piece-wise',   'estiamtes       ',   &
     &           'U'/
      INTEGER*4  COL_STA(3,2), ICOL_STA(2), COL_TYP(3,2), ICOL_TYP(2), &
     &           COL_OPR(3),   ICOL_OPR
      DATA       ( (  COL_STA(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_STA(N2$ARG), N2$ARG=1,2           ), &
     &           ( (  COL_TYP(N1$ARG,N2$ARG), N1$ARG=1,3 ), &
     &               ICOL_TYP(N2$ARG), N2$ARG=1,2           ), &
     &             (  COL_OPR(N1$ARG), N1$ARG=1,3 ), &
     &           ICOL_OPR/ &
     &               220, 209, 185,  11,      220, 194, 133,  12, &  ! ( 41;40-100)
     &               199, 205, 220,  13,      157, 177, 220,  14, &  ! (221;24-72)
     &               196, 220, 185,  &                            ! (101;40)
     &           10/
      REAL*4     XCT, YCT, XCB, YCB, XC, YC, XC_S, YC_S, XB, XC_P, YC_P, &
     &           XC_O, YC_O
      REAL*4     SIZV_STA, SIZH_STA, SIZV_TYP, SIZH_TYP, SIZV_OPR, SIZH_OPR
      INTEGER*4  IOPR, IST, ID
      INTEGER*4  I_LEN, PGOPEN, DIAGI_INBOX, IDIV8
      PARAMETER  ( MES_OPR = 'Display the plot' )
      IDIV8(IM) = IM/8 + MIN ( 1, MOD( IM, 8) )
!
      I_TYP = 0
      I_STA = 0
!
! --- Setting plotting parameters
!
      CALL DIAGI_SET ( 1, DIAGI_S )
!
! --- Oprenning X-window plotting device
!
      ID_XW = PGOPEN ( DIAGI_S%DEVICE )
      IF ( ID_XW .LE. 0 ) THEN
           IER = 0
           CALL CLRCH ( STR )
           CALL INCH  ( ID_XW, STR )
           CALL ERR_LOG ( 9241, IUER, 'MDLPL_EXT_MENU', 'Error in openning '// &
     &         'the graphic device '//DIAGI_S%DEVICE//' IER='//STR )
           RETURN
      END IF
!
      DIAGI_S%NCLR  = 0
!
! --- Set colors
!
      CALL DIAGI_CLS ( DIAGI_S, IER )
      CALL PGCOL_RGB ( ICOL_STA(1), COL_STA(1,1), COL_STA(2,1), COL_STA(3,1) )
      CALL PGCOL_RGB ( ICOL_STA(2), COL_STA(1,2), COL_STA(2,2), COL_STA(3,2) )
      CALL PGCOL_RGB ( ICOL_TYP(1), COL_TYP(1,1), COL_TYP(2,1), COL_TYP(3,1) )
      CALL PGCOL_RGB ( ICOL_TYP(2), COL_TYP(1,2), COL_TYP(2,2), COL_TYP(3,2) )
      CALL PGCOL_RGB ( ICOL_OPR,    COL_OPR(1),   COL_OPR(2),   COL_OPR(3)   )
!
! --- Setting default font type
!
      CALL PGSCF   ( 2 )
!
! --- Deleting previous window
!
      CALL PGERAS()
!
      UCUR_STA = 0
      UPRV_STA = 0
      UCUR_TYP = 0
      UPRV_TYP = 0
      UTIT     = 0
!
 910  CONTINUE
      CALL PGBBUF()
      CALL PGSAVE() ! 1
!
! --- Setting new world coodrinates
!
      CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0 )
      CALL PGSWIN  ( 0.0, 1.0, 0.0, 1.0 )
!
! --- Printing MDLPL_EXT label at the left bottom corner
!
      CALL PGSCH   ( 0.8 )
      CALL PGSLW   ( 1   )
      IF ( UTIT .EQ. 0 ) THEN
!
! -------- Printing MDLPL label
!
           CALL PGPTXT  ( 0.01, 0.015, 0.0, 0.0, MDLPL_EXT__LABEL )
!
! -------- Printing a box for "?" (on-line help) action
!
           CALL PGSCH   ( 1.5 )
!
! -------- Setting box parameters
!
           BOX_QST%XLB = 0.2
           BOX_QST%XTU = BOX_QST%XLB + 0.11
           BOX_QST%YLB = 0.00
           BOX_QST%YTU = BOX_QST%YLB + 0.04
!
! -------- Filling a rectangular for a box
!
           CALL PGSLW  ( 4 )
           CALL PGSCI  ( ICOL_OPR )
           CALL PGSFS  ( 1 )
           CALL PGRECT ( BOX_QST%XLB, BOX_QST%XTU, BOX_QST%YLB, BOX_QST%YTU )
!
! -------- Outline the box
!
           CALL PGSCI  ( 1 )
           CALL PGSFS  ( 2 )
           CALL PGRECT ( BOX_QST%XLB, BOX_QST%XTU, BOX_QST%YLB, BOX_QST%YTU )
!
! -------- Printing text with prompt for on-line help
!
           CALL PGSCH  ( 0.8 )
           CALL PGPTXT ( BOX_QST%XLB+0.005, BOX_QST%YLB+0.025, 0.0, 0.0, &
     &                   '?' )
           CALL PGSLW  ( 6   )
           CALL PGSCH  ( 1.5 )
           CALL PGPTXT ( BOX_QST%XLB+0.03, BOX_QST%YLB+0.007, 0.0, 0.0, &
     &                   'help' )
!
! -------- Printing a box for "X" (exit) action
!
           BOX_EXI%XLB = 0.35
           BOX_EXI%XTU = BOX_EXI%XLB + 0.11
           BOX_EXI%YLB = 0.00
           BOX_EXI%YTU = BOX_EXI%YLB + 0.04
!
! -------- Filling the box
!
           CALL PGSLW  ( 4 )
           CALL PGSCI  ( ICOL_OPR )
           CALL PGSFS  ( 1 )
           CALL PGRECT ( BOX_EXI%XLB, BOX_EXI%XTU, BOX_EXI%YLB, BOX_EXI%YTU )
!
! -------- Outline the box
!
           CALL PGSCI  ( 1 )
           CALL PGSFS  ( 2 )
           CALL PGRECT ( BOX_EXI%XLB, BOX_EXI%XTU, BOX_EXI%YLB, BOX_EXI%YTU )
!
! -------- Printing text with prompt for exit action
!
           CALL PGSCH  ( 0.8 )
           CALL PGPTXT ( BOX_EXI%XLB+0.005, BOX_EXI%YLB+0.025, 0.0, 0.0, &
     &                   'X' )
           CALL PGSLW  ( 6 )
           CALL PGSCH  ( 1.5 )
           CALL PGPTXT ( BOX_EXI%XLB+0.03, BOX_EXI%YLB+0.007, 0.0, 0.0, &
     &                  'exit' )
      END IF
!
      CALL PGSCI ( 1 )
!
! --- Printing the banner
!
      XCT  = 0.04
      YCT  = 0.95
      YCB  = 0.90
      MES  = 'Select the station and the plot type and then hit  P'
      XB   = 0.65
      XC_P = 0.83
      YC_P = 0.90
      XC_O = 0.82
      YC_O = 0.0351
      SIZV_OPR = 0.07
      SIZH_OPR = 0.30
!
! --- Diplay the banner
!
      CALL PGSCH   ( 2.0 )
      CALL PGSLW   ( 8 )
      IF ( UTIT .EQ. 0 ) THEN
           CALL PGPTXT  ( XCT, YCT, 0.0, 0.0, MES(1:I_LEN(MES)) )
      END IF
!
      MCOL = (8+L_STA)/8
      IF ( MOD(L_STA,8) .EQ. 0 ) MCOL = MCOL -1
      IF ( L_STA .GE. 8 ) MROW = 8
      IF ( L_STA .LT. 8 ) MROW = L_STA
!
! --- Setting the sizes of the station rectangulars
!
      SIZV_STA = YCB/(2*MROW+1)
      SIZH_STA = 0.12
      XCB = SIZH_STA/2 + 0.005
      IF ( MCOL .EQ. 1 ) THEN
           XCB = 0.30
           XB  = 0.50
      END IF
!
! --- Setting maximum device independent coordiantes
!
      CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
!
! === Display boxes with station names
!
      DO 410 J1=1,L_STA
         IF ( IUSE_STA(J1) .EQ. 0 ) GOTO 410
         IC = IDIV8(J1)
         IR = MOD(J1,8)
         IF ( IR .EQ. 0 ) IR = 8
!
! ------ Specifing the corners of the J1-th box
!
         XC_S = XCB + 1.5*SIZH_STA*(IC-1)
         YC_S = YCB - 2*SIZV_STA*(IR-1) - SIZV_STA
!
         BOX_STA(J1)%XLB = XC_S - SIZH_STA/2
         BOX_STA(J1)%YLB = YC_S - SIZV_STA/2
         BOX_STA(J1)%XTU = XC_S + SIZH_STA/2
         BOX_STA(J1)%YTU = YC_S + SIZV_STA/2
!
         CALL PGSAVE() ! 2A
!
! ------ Printing the box as filled rectangular
!
         IF ( ISEL_STA .EQ. J1 ) THEN
              CALL PGSCI  ( ICOL_STA(2) )
           ELSE
              CALL PGSCI  ( ICOL_STA(1) )
         END IF
!
         IF ( UCUR_STA .EQ. J1  .OR.  UPRV_STA .EQ. J1  .OR. &
     &        UCUR_STA .EQ. 0   .OR.  UPRV_STA .EQ. 0         ) THEN
!
              CALL PGSLW  ( 1 )
              CALL PGSFS  ( 1 )
              CALL PGRECT ( BOX_STA(J1)%XLB, BOX_STA(J1)%XTU, &
     &                      BOX_STA(J1)%YLB, BOX_STA(J1)%YTU )
!
! ----------- ... and then as outlined rectangular
!
             IF ( J1 .EQ. ICLREF_STA ) THEN
!
! --------------- The frame of the outlined rectangular will be red color
!
                  CALL PGSLW  ( 6 )
                  CALL PGSCI  ( ERR_CLRI )
                  CALL PGSFS  ( 2 )
                  CALL PGRECT ( BOX_STA(J1)%XLB, BOX_STA(J1)%XTU, &
     &                          BOX_STA(J1)%YLB, BOX_STA(J1)%YTU )
                  CALL PGSCI  ( 1 )
               ELSE
                  CALL PGSCI  ( 1 )
                  CALL PGSFS  ( 2 )
                  CALL PGRECT ( BOX_STA(J1)%XLB, BOX_STA(J1)%XTU, &
     &                         BOX_STA(J1)%YLB, BOX_STA(J1)%YTU )
             END IF
!
! ---------- Temporary setting world coordinate space for the station box
!
             CALL PGSVP  ( BOX_STA(J1)%XLB, BOX_STA(J1)%XTU, &
     &                     BOX_STA(J1)%YLB, BOX_STA(J1)%YTU )
!
             CALL CLRCH   (     STR )
             CALL INCH    ( J1, STR )
             STR(I_LEN(STR)+1:) = ')'
!
             CALL PGSCH   ( 0.5 )
             CALL PGSLW   ( 1   )
             CALL PGPTXT  ( 0.04, 0.75, 0.0, 0.0, STR(1:I_LEN(STR)) )
!
             CALL PGSCH   ( 1.2 )
             CALL PGSLW   ( 5   )
             CALL PGPTXT  ( 0.1, 0.2, 0.0, 0.0, STANAM(J1) )
         END IF
!
! ------ Restoring world coordinated space
!
         CALL PGSVP  ( 0.0, 1.0, 0.0, 1.0  )
         CALL PGUNSA() ! 2A
 410  CONTINUE
      CALL PGSAVE() ! 3
      IF ( UTIT .EQ. 0 ) THEN
           CALL PGSFS  ( 2 )
           CALL PGSLW  ( 12 )
           CALL PGRECT ( XB, XB+0.0001, 0., YCB )
      END IF
      CALL PGUNSA() ! 3
!
      SIZV_TYP = (YC_P - YC_O - SIZV_OPR/2)/(2*M_TYP+1)
      SIZH_TYP = 0.25
!
! === Display boxes with command buttons
!
      DO 420 J2=1,M_TYP
!
! ------ Specify coordinates of the corners of the j2-th box
!
         BOX_TYP(J2)%XLB = XC_P                 - SIZH_TYP/2
         BOX_TYP(J2)%YLB = YC_P - J2*2*SIZV_TYP + SIZV_TYP/2
         BOX_TYP(J2)%XTU = XC_P                 + SIZH_TYP/2
         BOX_TYP(J2)%YTU = YC_P - J2*2*SIZV_TYP + SIZV_TYP/2 + SIZV_TYP
!
         CALL PGSAVE() ! 4A
!
! ------ Printing the box as filled rectangular
!
         IF ( J2 .EQ. ISEL_TYP ) THEN
              CALL PGSCI  ( ICOL_TYP(2) )
           ELSE
              CALL PGSCI  ( ICOL_TYP(1) )
         END IF
!
         IF ( UCUR_TYP .EQ. J2  .OR.  UPRV_TYP .EQ. J2  .OR. &
     &        UCUR_TYP .EQ. 0   .OR.  UPRV_TYP .EQ. 0         ) THEN
!
              CALL PGSLW  ( 1 )
              CALL PGSFS  ( 1 )
              CALL PGRECT ( BOX_TYP(J2)%XLB, BOX_TYP(J2)%XTU, &
     &                      BOX_TYP(J2)%YLB, BOX_TYP(J2)%YTU )
!
! ----------- ... and then as an outlined rectangular
!
              IF ( ( J2 .EQ. ICLT_TYP .AND. .NOT. LCLT_USE )  .OR. &
     &             ( J2 .EQ. ICLP_TYP .AND. .NOT. LCLT_USE )  .OR. &
     &             ( J2 .EQ. IATP_TYP .AND. .NOT. LATM_USE )  .OR. &
     &             ( J2 .EQ. IXPL_TYP .AND. .NOT. LEOP_USE )  .OR. &
     &             ( J2 .EQ. IYPL_TYP .AND. .NOT. LEOP_USE )  .OR. &
     &             ( J2 .EQ. IUT1_TYP .AND. .NOT. LEOP_USE )        ) THEN
!
! ---------------- We ooutline the box by red to emphasize that this plot type
! ---------------- is not available since these parameters were not estimated
!
                   CALL PGSLW  ( 4 )
                   CALL PGSCI  ( ERR_CLRI )
                   CALL PGSFS  ( 2 )
                   CALL PGRECT ( BOX_TYP(J2)%XLB, BOX_TYP(J2)%XTU, &
     &                           BOX_TYP(J2)%YLB, BOX_TYP(J2)%YTU )
                   CALL PGSCI  ( 1 )
                 ELSE
                   CALL PGSCI  ( 1 )
                   CALL PGSFS  ( 2 )
                   CALL PGRECT ( BOX_TYP(J2)%XLB, BOX_TYP(J2)%XTU, &
     &                           BOX_TYP(J2)%YLB, BOX_TYP(J2)%YTU )
              END IF
!
! ----------- Temporary setting world coordinate space for the station box
!
              CALL PGSVP  ( BOX_TYP(J2)%XLB, BOX_TYP(J2)%XTU, &
     &                      BOX_TYP(J2)%YLB, BOX_TYP(J2)%YTU )
!
              CALL PGSCH   ( 0.6 )
              CALL PGSLW   ( 1   )
              CALL PGPTXT  ( 0.02, 0.80, 0.0, 0.0, ABR_TYP(J2) )
!
              CALL PGSCH   ( 1.2 )
              CALL PGSLW   ( 5   )
              CALL PGPTXT  ( 0.10, 0.60, 0.0, 0.0, MES_TYP(1,J2) )
              CALL PGPTXT  ( 0.10, 0.15, 0.0, 0.0, MES_TYP(2,J2) )
              CALL PGSVP   ( 0.00, 1.0, 0.0, 1.0  )
         END IF
         CALL PGUNSA() ! 4A
 420  CONTINUE
!
! --- Specify corners of the box for operation.
!
      BOX_OPR%XLB = XC_O - SIZH_OPR/2
      BOX_OPR%YLB = YC_O - SIZV_OPR/2
      BOX_OPR%XTU = XC_O + SIZH_OPR/2
      BOX_OPR%YTU = YC_O + SIZV_OPR/2
!
      CALL PGSAVE()  ! 5
!
! --- Printing the box
!
      IF ( UTIT .EQ. 0 ) THEN
           CALL PGSLW   ( 1 )
           CALL PGSCI   ( ICOL_OPR )
           CALL PGSFS   ( 1 )
           CALL PGRECT  ( BOX_OPR%XLB, BOX_OPR%XTU, BOX_OPR%YLB, BOX_OPR%YTU )
!
           CALL PGSCI   ( 1 )
           CALL PGSFS   ( 2 )
           CALL PGRECT  ( BOX_OPR%XLB, BOX_OPR%XTU, BOX_OPR%YLB, BOX_OPR%YTU )
!
           CALL PGSVP   ( BOX_OPR%XLB, BOX_OPR%XTU, BOX_OPR%YLB, BOX_OPR%YTU )
!
           CALL PGSCH   ( 0.5 )
           CALL PGSLW   ( 1   )
           CALL PGPTXT  ( 0.02, 0.82, 0.0, 0.0, 'P' )
           CALL PGSCH   ( 1.8 )
           CALL PGSLW   ( 8   )
           CALL PGPTXT  ( 0.1, 0.3, 0.0, 0.0, MES_OPR )
      END IF
      UTIT = 1
!
! --- Restoring world coordinated space
!
      CALL PGSVP   ( 0.0, 1.0, 0.0, 1.0  )
      CALL PGUNSA()  ! 5
      CALL PGEBUF()
      CALL PGUPDT()
      CALL PGUNSA() ! 1
!
! --- Putting cursor to appropriate place and ...
!
      IF ( UCUR_STA .GT. 0 ) THEN
           XC = BOX_STA(ISEL_STA)%XLB + 0.02
           YC = BOX_STA(ISEL_STA)%YLB + 0.02
         ELSE
           XC = BOX_TYP(ISEL_TYP)%XLB + 0.02
           YC = BOX_TYP(ISEL_TYP)%YLB + 0.02
      END IF
!
! --- ... Waiting for user reaction
!
      CALL PGBAND ( 0, 1, XC, YC, XC, YC, CH )
!
! --- Analysis of what has been entered
!
      IOPR = 0
!
! --- Firstly parsing of the keyboard code
!
      CALL TRAN ( 11, CH, CH )
!
! --- Rught button -- exit
!
      ID = DIAGI_INBOX ( 1, BOX_EXI, XC, YC )
      IF ( ID .EQ. 1  .OR.  CH .EQ. 'X' ) GOTO 830
!
! --- ? key
!
      ID = DIAGI_INBOX ( 1, BOX_QST, XC, YC )
      IF ( ID .EQ. 1   .OR.   CH .EQ. '?'   .OR.   CH .EQ. '/' ) THEN
           CALL ERR_PASS  ( IUER, IER )
           CALL MDLPL_HLP ( MDLPL_EXT_HLPFIL, IER )
           UTIT = 0
           UCUR_STA = 0
           UPRV_STA = 0
           UCUR_TYP = 0
           UPRV_TYP = 0
           GOTO 910
      END IF
!
! --- Key to move to the next
!
      IF ( CH .EQ. 'N'  ) THEN
           DO 430 J3=1,L_STA
              UPRV_STA = ISEL_STA
              UCUR_TYP = -1
              UPRV_TYP = -1
              ISEL_STA = ISEL_STA + 1
              IF ( ISEL_STA .GT. L_STA ) ISEL_STA = 1
              UCUR_STA = ISEL_STA
              IF ( IUSE_STA(J3) .NE. 0 ) GOTO 910
 430       CONTINUE
           GOTO 910
      END IF
!
! --- Digit key -- move to the specified station
!
      CALL CHIN ( CH, IST )
      IF ( IST .GE. 1  .AND. IST .LE. 9 ) THEN
           UPRV_STA = ISEL_STA
           UCUR_TYP = -1
           UPRV_TYP = -1
           ISEL_STA = IST
           UCUR_STA = ISEL_STA
      END IF
!
! --- Key of the type of the operatiuon
!
      DO 440 J4=1,M_TYP
         IF ( CH .EQ. ABR_TYP(J4) ) THEN
              UPRV_TYP = ISEL_TYP
              UCUR_STA = -1
              UPRV_STA = -1
              ISEL_TYP = J4
              UCUR_TYP = ISEL_TYP
         END IF
 440  CONTINUE
      IF ( CH .EQ. 'P'        ) IOPR = 1
      IF ( CH .EQ. 'A'  .OR.  CH .EQ. 'D'  .OR.  CH .EQ. 'X' ) THEN
!
! -------- Mouse button was hit.
! -------- Then analysis: in which box the cursor is pointing
!
! -------- Search among station boxes
!
           ID = DIAGI_INBOX ( L_STA, BOX_STA, XC, YC )
           IF ( ID .GT. 0 ) THEN
                UPRV_STA = ISEL_STA
                UCUR_TYP = -1
                UPRV_TYP = -1
                ISEL_STA = ID
                UCUR_STA = ISEL_STA
           END IF
!
! -------- Search among type boxes
!
           ID = DIAGI_INBOX ( M_TYP, BOX_TYP, XC, YC )
           IF ( ID .GT. 0  .AND.  ( CH .EQ. 'A' .OR.  CH .EQ. 'D' ) ) THEN
                UPRV_TYP = ISEL_TYP
                UCUR_STA = -1
                UPRV_STA = -1
                ISEL_TYP = ID
                UCUR_TYP = ISEL_TYP
           ENDIF
!
! -------- Was that operation box?
!
           ID = DIAGI_INBOX (     1, BOX_OPR, XC, YC )
           IF ( ID .GT. 0 ) IOPR = ID
      END IF
!
      IF ( IOPR .EQ. 1 ) THEN
           I_STA = ISEL_STA
           I_TYP = ISEL_TYP
           GOTO 820
      END IF
!
      GOTO 910  ! Infinte loop
 830  CONTINUE
      CALL PGERAS()
!
! --- Printing farwell message
!
      CALL PGSCH  ( 1.666 )
      CALL PGSLW  ( 5     )
      CALL PGPTXT ( 0.5, 0.5, 0.0, 0.5, &
     &             'Please, iconify (by <Alt/blank> <n>) '// &
     &             'graphic window manually' )
 820  CONTINUE
!
! --- Closing plotting device
!
      CALL PGCLOQ()
      CALL PGENDQ()
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  MDLPL_EXT_MENU  #!#
