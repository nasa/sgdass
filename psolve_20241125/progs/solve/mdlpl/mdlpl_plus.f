      SUBROUTINE MDLPL_PLUS ( IMODE, L_STA, STANAM, DBNAME, PREF_NAME, &
     &          LBUF_ATM, BUF_ATM, FINAM_ATM, LATM_USE, &
     &          LBUF_CLO, BUF_CLO, FINAM_CLO, IUSE_STA, &
     &          LBUF_EOP, BUF_EOP, FINAM_EOP, LEOP_USE, &
     &          MDLPL_IPS_PAG, MDLPL_IPC_PAG, MLDLP_FL_EOPMOD, MDLPL_FL_CLF, &
     &          MDLPL_FL_FRAME, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MDLPL_PLUS  is a part (I thing the best one) of a MDLPL   *
! *   utility. It displayed the menu form at the screen using Multi_DiaGI*
! *   interface, inquire user selection handles user replies and         *
! *   eventually displays different kind of plots using MultiDiaGI       *
! *   interface:                                                         *
! *   1) values of station-dependent clock function for all stations;    *
! *   2) values of atmosphere path delay for all stations;               *
! *   3) values of segmented adjustments to EOP;                         *
! *   4) values of postfit residuals for all baselines                   *
! *   5) values of postfit residuals plus clock function for all         *
! *      baselines;                                                      *
! *                                                                      *
! *   It also allows to change directory name with prefix for hardcopies *
! *   and prints on-line help upon request.                              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       DBNAME ( CHARACTER ) -- String with database name and version  *
! *                               number.                                *
! *        L_STA ( INTEGER*4 ) -- Total number of stations participated  *
! *                               in the session.                        *
! *       STANAM ( CHARACTER ) -- Array of stations names.               *
! *                               Dimension: L_STA.                      *
! *     LBUF_ATM ( INTEGER*4 ) -- Number of lines in the file with       *
! *                               adjustments to atmosphere path delay.  *
! *      BUF_ATM ( CHARACTER ) -- Text buffer with content of the file   *
! *                               with adjustments to atmosphere path    *
! *                               delay.                                 *
! *    FINAM_ATM ( CHARACTER ) -- File name with adjustments to          *
! *                               segmented clock function.              *
! *     LATM_USE ( LOGICAL*4 ) -- Flag of atmosphere estimation:         *
! *                               TRUE means that atmosphere parameters  *
! *                               have been estimated.                   *
! *     LBUF_CLO ( INTEGER*4 ) -- Number of lines in the file with       *
! *                               adjustments to segmented clock function*
! *      BUF_CLO ( CHARACTER ) -- Text buffer with content of the file   *
! *                               with adjustments to segmented clock    *
! *                               function.                              *
! *    FINAM_CLO ( CHARACTER ) -- File name with adjustments to          *
! *                               segmented clock function.              *
! *     IUSE_STA ( INTEGER*4 ) -- Arrays of station selection status.    *
! *                               IUSE_STA(k)=1 means that the k-the     *
! *                               station has been selected in solution. *
! *                               Dimension: L_STA.                      *
! *     LBUF_EOP ( INTEGER*4 ) -- Number of lines in the file with       *
! *                               adjustments to segmented EOP.          *
! *      BUF_EOP ( CHARACTER ) -- Text buffer with content of the file   *
! *                               with adjustments to segmented EOP.     *
! *    FINAM_EOP ( CHARACTER ) -- File name with adjustments to          *
! *                               segmented EOP.                         *
! *     LEOP_USE ( LOGICAL*4 ) -- Flag of EOP estimation:                *
! *                               TRUE means that segmented EOP          *
! *                               parameters have been estimated.        *
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
! *  ###  23-JUL-99   MDLPL_PLUS  v1.6 (c)  L. Petrov  15-DEC-2004  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4  IMODE, L_STA, LBUF_ATM, LBUF_CLO, LBUF_EOP, &
     &           IUSE_STA(L_STA), IUER
      INTEGER*4  MDLPL_IPS_PAG, MDLPL_IPC_PAG
      LOGICAL*4  MLDLP_FL_EOPMOD, MDLPL_FL_CLF, MDLPL_FL_FRAME
      CHARACTER  STANAM(L_STA)*(*), DBNAME*(*), PREF_NAME*(*), &
     &           BUF_ATM(*)*(*), BUF_CLO(*)*(*), BUF_EOP(*)*(*), &
     &           FINAM_ATM*(*), FINAM_CLO*(*), FINAM_EOP*(*)
      LOGICAL*4  LATM_USE, LEOP_USE, F_READ
      TYPE ( DIAGI_STRU ) ::  DIAGI_S
      INTEGER*4    M_BUT, M_BAS
      PARAMETER  ( M_BUT=12 )
      PARAMETER  ( M_BAS = MAX_ARC_BSL )
      CHARACTER  BUT_NAME(M_BUT)*64, BUT_LET(M_BUT)*3, COMMON_TITLE*32, STR*32
      INTEGER*4  L_BUT, IER, ICODE, ITYP, LEN_DIAGI
!
      INTEGER*8        MEM_LEN, MEM_LEN_PSF
      ADDRESS__TYPE :: ADR_DIAGI_ATM, ADR_DIAGI_CLO, ADR_DIAGI_EOP, &
     &                 ADR_DIAGI_RES, ADR_DIAGI_CRS
      ADDRESS__TYPE :: MEM_ADR, MEM_ADR_PSF, ADR_TIM, ADR_VAL, ADR_ERR, &
     &                 ADR_DIAGI_PSF
      INTEGER*8        LEN_DIAGI_ATM, LEN_DIAGI_CLO, LEN_DIAGI_EOP, &
     &                 LEN_DIAGI_RES, LEN_DIAGI_CRS
      INTEGER*4        L_CLO_ARR(M_BAS), ID_XW
      REAL*8     TIM_PSF(MAX_OBS), VAL_PSF(MAX_OBS), ERR_PSF(MAX_OBS), &
     &           VAL_CLF(MAX_OBS), ERR_CLF(MAX_OBS), PSF_DMAX, PSC_DMAX
      REAL*8     CLO_TIM_ARR(M_BAS,MAX_CLK), CLO_VAL_ARR(M_BAS,MAX_CLK), &
     &           CLO_SIG_ARR(M_BAS,MAX_CLK)
      INTEGER*2  STAT_PSF(2,MAX_OBS), UACSUP_PSF(MAX_OBS)
      INTEGER*4  AUTO_SUP_PSF(MAX_OBS), USER_SUP_PSF(MAX_OBS), &
     &           USER_REC_PSF(MAX_OBS)
      INTEGER*4  BAS_COD(MAX_OBS), L_OBS, L_BAS, LIS_BAS(M_BAS)
      CHARACTER  C_BAS(M_BAS)*17
      INTEGER*4  I_LEN, PGOPEN
!
! --- Some initialization
!
      MEM_LEN_PSF = 0
      MEM_LEN     = 0
      F_READ      = .FALSE.
!
      CALL CLRCH ( COMMON_TITLE )
      COMMON_TITLE = 'MDLPL_PLUS'
!
! --- Determine amount of necessary memory
!
      LEN_DIAGI     = LOC(DIAGI_S%STATUS) - LOC(DIAGI_S%IFIRST_FIELD) + 4
      LEN_DIAGI_ATM = L_STA*LEN_DIAGI
      LEN_DIAGI_CLO = L_STA*LEN_DIAGI
      LEN_DIAGI_EOP = 3*LEN_DIAGI
      LEN_DIAGI_RES = (L_STA*(L_STA+1))/2*LEN_DIAGI
      LEN_DIAGI_CRS = (L_STA*(L_STA+1))/2*LEN_DIAGI
!
! --- Grab dynamic memory
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, MEM_LEN,       MEM_ADR,       5, &
     &                     LEN_DIAGI_ATM, ADR_DIAGI_ATM, &
     &                     LEN_DIAGI_CLO, ADR_DIAGI_CLO, &
     &                     LEN_DIAGI_EOP, ADR_DIAGI_EOP, &
     &                     LEN_DIAGI_RES, ADR_DIAGI_RES, &
     &                     LEN_DIAGI_CRS, ADR_DIAGI_CRS    )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL IINCH   ( MEM_LEN, STR )
           CALL ERR_LOG ( 9301, IUER, 'MDLPL_PLUS', 'Error in attempt to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
! --- Set names of button commands and button letters
!
      L_BUT = 0
      IF ( LATM_USE ) THEN
           L_BUT = L_BUT + 1
           CALL CLRCH ( BUT_NAME(L_BUT) )
           BUT_NAME(L_BUT) = 'Plot of atmosphere path delay'
           BUT_LET(L_BUT) = 'Ttt'
      END IF
!
      L_BUT = L_BUT + 1
      CALL CLRCH ( BUT_NAME(L_BUT) )
      BUT_NAME(L_BUT) = 'Plot of clock function'
      BUT_LET(L_BUT) = 'Ccc'
!
      IF ( LEOP_USE ) THEN
           L_BUT = L_BUT + 1
           CALL CLRCH ( BUT_NAME(L_BUT) )
           BUT_NAME(L_BUT) = 'Plot of segemeted EOP'
           BUT_LET(L_BUT) = 'Eee'
      END IF
!
      L_BUT = L_BUT + 1
      CALL CLRCH ( BUT_NAME(L_BUT) )
      BUT_NAME(L_BUT) = 'Plot of postfit residuals'
      BUT_LET(L_BUT) = 'Ppp'
!
      L_BUT = L_BUT + 1
      CALL CLRCH ( BUT_NAME(L_BUT) )
      BUT_NAME(L_BUT) = 'Plot of postfit residuals | plus clock function'
      BUT_LET(L_BUT) = 'Rrr'
!
      L_BUT = L_BUT + 1
      CALL CLRCH ( BUT_NAME(L_BUT) )
      BUT_NAME(L_BUT) = 'Old (PRE JUL99) style MDLPL'
      BUT_LET(L_BUT) = 'Ooo'
!
      L_BUT = L_BUT + 1
      CALL CLRCH ( BUT_NAME(L_BUT) )
      BUT_NAME(L_BUT) = 'Oldest (PRE NOV97) style MDLPL'
      BUT_LET(L_BUT) = '$4$'
!
      L_BUT = L_BUT + 1
      CALL CLRCH ( BUT_NAME(L_BUT) )
      BUT_NAME(L_BUT) = 'HELP'
      BUT_LET(L_BUT) = 'Hhh'
!
      L_BUT = L_BUT + 1
      CALL CLRCH ( BUT_NAME(L_BUT) )
      BUT_NAME(L_BUT) = 'EXIT'
      BUT_LET(L_BUT) = 'Xxx'
 910  CONTINUE
!
! --- Ask user to select a command using Multi_DiaGI interface
!
      ICODE = 0
      CALL ERR_PASS ( IUER, IER )
      CALL MULTI_DIAGI ( COMMON_TITLE, 0, 0, 0, ' ', L_BUT, BUT_NAME, BUT_LET, &
     &                   ' ', %VAL(0), ICODE, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9302, IUER, 'MDLPL_PLUS', 'Error in MULTI_DIAGI' )
           GOTO 810
      END IF
!
! --- Parse the command code
!
      IF ( ICODE .EQ. 0 ) THEN
           CALL PGENDQ() ! Iconify window
           IER = 0
           CALL ERR_LOG ( 0, IUER )
           GOTO 810
      END IF
!
! --- Parse a user reply
!
      CALL ERR_PASS ( IUER, IER )
      IF ( BUT_LET(ICODE)(1:1) .EQ. 'T' ) THEN
           CALL PGENDQ() ! Iconify window
!
! -------- Make plots of atmosphere path delay
!
           ITYP = IATP_TYP
           CALL MDLPL_PLUS_CLOATM ( ITYP, STANAM, DBNAME, PREF_NAME, &
     &          MDLPL_FL_FRAME, L_STA, IUSE_STA, LBUF_ATM, BUF_ATM, FINAM_ATM, &
     &                          LBUF_CLO, BUF_CLO, FINAM_CLO, &
     &                          %VAL(ADR_DIAGI_ATM), IER )
           IF ( IER .EQ. 0 ) GOTO 910
         ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'C' ) THEN
           CALL PGENDQ() ! Iconify window
!
! -------- Make plots of clock function
!
           ITYP = ICLP_TYP
           CALL MDLPL_PLUS_CLOATM ( ITYP, STANAM, DBNAME, PREF_NAME, &
     &          MDLPL_FL_FRAME, L_STA, IUSE_STA, LBUF_ATM, BUF_ATM, FINAM_ATM, &
     &                          LBUF_CLO, BUF_CLO, FINAM_CLO, &
     &                          %VAL(ADR_DIAGI_CLO), IER )
           IF ( IER .EQ. 0 ) GOTO 910
         ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'E' ) THEN
           CALL PGENDQ() ! Iconify window
!
! -------- Make plot of adjustmetns of segmented EOP
!
           CALL MDLPL_PLUS_EOP ( DBNAME, PREF_NAME, &
     &                           LBUF_EOP, BUF_EOP, FINAM_EOP, MLDLP_FL_EOPMOD, &
     &                           %VAL(ADR_DIAGI_EOP), IER )
           IF ( IER .EQ. 0 ) GOTO 910
         ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'P' ) THEN
           CALL PGENDQ() ! Iconify window
!
! -------- Make plot of postifit residuals
!
           CALL ERR_PASS ( IUER, IER )
           CALL PSF_PLOT ( DBNAME, PREF_NAME, F_READ, IPSF_TYP, &
     &          TIM_PSF, VAL_PSF, ERR_PSF, STAT_PSF, UACSUP_PSF, &
     &          AUTO_SUP_PSF, USER_SUP_PSF, USER_REC_PSF, PSF_DMAX, &
     &          VAL_CLF, ERR_CLF, PSC_DMAX, &
     &          L_OBS, BAS_COD, L_BAS, LIS_BAS, C_BAS, &
     &          MEM_LEN_PSF, MEM_ADR_PSF, ADR_DIAGI_PSF, ADR_TIM, ADR_VAL, &
     &          ADR_ERR, L_CLO_ARR, CLO_TIM_ARR, CLO_VAL_ARR, CLO_SIG_ARR, &
     &          MDLPL_IPS_PAG, MDLPL_IPC_PAG, MDLPL_FL_CLF, MDLPL_FL_FRAME, &
     &          IER )
           IF ( IER .EQ. 0 ) GOTO 910
         ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'R' ) THEN
           CALL PGENDQ() ! Iconify window
!
! -------- Make plot of postifit residuals plus clock function
!
           CALL ERR_PASS ( IUER, IER )
           CALL PSF_PLOT ( DBNAME, PREF_NAME, F_READ, IPSC_TYP, &
     &          TIM_PSF, VAL_PSF, ERR_PSF, STAT_PSF, UACSUP_PSF, &
     &          AUTO_SUP_PSF, USER_SUP_PSF, USER_REC_PSF, PSF_DMAX, &
     &          VAL_CLF, ERR_CLF, PSC_DMAX, &
     &          L_OBS, BAS_COD, L_BAS, LIS_BAS, C_BAS, &
     &          MEM_LEN_PSF, MEM_ADR_PSF, ADR_DIAGI_PSF, ADR_TIM, ADR_VAL, &
     &          ADR_ERR, L_CLO_ARR, CLO_TIM_ARR, CLO_VAL_ARR, CLO_SIG_ARR, &
     &          MDLPL_IPS_PAG, MDLPL_IPC_PAG, MDLPL_FL_CLF, MDLPL_FL_FRAME, &
     &          IER )
           IF ( IER .EQ. 0 ) GOTO 910
         ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'O' ) THEN
           CALL PGENDQ() ! Iconify window
!
! -------- Go back and call MDLPL_EXT
!
           IMODE = 1
           IER = 0
           CALL ERR_LOG ( 0, IUER )
           GOTO 810
         ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. '$' ) THEN
           CALL PGENDQ() ! Iconify window
!
! -------- Go back and call atrchaic MDLPL
!
           IMODE = 0
           IER = 0
           CALL ERR_LOG ( 0, IUER )
           GOTO 810
         ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'H' ) THEN
!
! -------- On-line help
!
           CALL ERR_PASS  ( IUER, IER )
           ID_XW = PGOPEN ( DEVS(1) )
           CALL MDLPL_HLP ( MDLPL_PLUS_HLP_OVR, IER )
           CALL PGENDQ() ! Iconify window
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 9303, IUER, 'MDLPL_PLUS', 'Error in attempt '// &
     &              'to get on-line help' )
                GOTO 810
           END IF
           GOTO 910
         ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'X' ) THEN
           CALL PGENDQ() ! Iconify window
!
! -------- Go back
!
           IER = 0
           CALL ERR_LOG ( 0, IUER )
           GOTO 810
      END IF
!
      CALL PGENDQ() ! Iconify window
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 9304, IUER, 'MDLPL_PLUS', 'Error in execution of '// &
     &         'command '//BUT_NAME(ICODE) )
           GOTO 810
      END IF
!
 810  CONTINUE
!
! --- Free dynamic memory
!
      IF ( MEM_LEN_PSF .GT. 0 ) THEN
           CALL FREE_MEM ( MEM_ADR_PSF )
      END IF
      IF ( MEM_LEN .GT. 0 ) THEN
           CALL FREE_MEM ( MEM_ADR )
      END IF
!
      CALL ERR_PASS ( IER, IUER )
      RETURN
      END  !#!  MDLPL_PLUS  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MDLPL_PLUS_CLOATM ( ITYP, STANAM, DBNAME, PREF_NAME, &
     &                               MDLPL_FL_FRAME, L_STA, IUSE_STA, &
     &                               LBUF_ATM, BUF_ATM, FINAM_ATM, &
     &                               LBUF_CLO, BUF_CLO, FINAM_CLO, &
     &                               DIAGI_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MDLPL_PLUS_CLOATM  makes plots of atmosphere path delays  *
! *   (if ITYP=ITYP_ATM ) or segmented clock function (if ITYP=ITYP_CLO) *
! *   using Multi_DiaGI interface.                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        I_TYP ( INTEGER*4 ) -- Type of the plot. Supported types:     *
! *                            IATM_TYP (atmosphere path delay).         *
! *                            ICLO_TYP (segmented clock function).      *
! *       STANAM ( CHARACTER ) -- Array of stations names.               *
! *                               Dimension: L_STA.                      *
! *       DBNAME ( CHARACTER ) -- String with database name and version  *
! *                               number.                                *
! *        L_STA ( INTEGER*4 ) -- Total number of stations participated  *
! *                               in the session.                        *
! *     IUSE_STA ( INTEGER*4 ) -- Arrays of station selection status.    *
! *                               IUSE_STA(k)=1 means that the k-the     *
! *                               station has been selected in solution. *
! *                               Dimension: L_STA.                      *
! *     LBUF_ATM ( INTEGER*4 ) -- Number of lines in the file with       *
! *                               adjustments to atmosphere path delay.  *
! *      BUF_ATM ( CHARACTER ) -- Text buffer with content of the file   *
! *                               with adjustments to atmosphere path    *
! *                               delay.                                 *
! *    FINAM_ATM ( CHARACTER ) -- File name with adjustments to          *
! *                               segmented clock function.              *
! *     LBUF_CLO ( INTEGER*4 ) -- Number of lines in the file with       *
! *                               adjustments to segmented clock function*
! *      BUF_CLO ( CHARACTER ) -- Text buffer with content of the file   *
! *                               with adjustments to segmented clock    *
! *                               function.                              *
! *    FINAM_CLO ( CHARACTER ) -- File name with adjustments to          *
! *                               segmented clock function.              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    DIAGI_ARR ( RECORD    ) -- Array of DiaGI data structures for     *
! *                               plotting. Dimension: L_STA.            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    PREF_NAME ( CHARACTER ) -- Prefix string with pathname which will *
! *                               be prepend before the second part of   *
! *                               filename of hardcopies of the plots.   *
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
! * ### 23-JUL-99  MDLPL_PLUS_CLOATM  v2.2(c) L. Petrov 15-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4  L_STA, IUSE_STA(L_STA), LBUF_ATM, LBUF_CLO, IUER
      LOGICAL*4  MDLPL_FL_FRAME
      CHARACTER  STANAM(L_STA)*(*), DBNAME*(*), PREF_NAME*(*), &
     &           BUF_ATM(*)*(*), BUF_CLO(*)*(*), &
     &           FINAM_ATM*(*),  FINAM_CLO*(*)
      TYPE ( DIAGI_STRU ) ::  DIAGI_ARR(L_STA)
!
      INTEGER*4    M_PTS, M_BUT, M_STA
      PARAMETER  ( M_PTS = MAX_PTS )
      PARAMETER  ( M_BUT = 8       )
      PARAMETER  ( M_STA = MAX_ARC_STA )
      REAL*8     TIME_ARR(M_PTS,M_STA), VAL_ARR(M_PTS,M_STA), &
     &           SIG_ARR(M_PTS,M_STA), MODU_ARR(M_PTS), MODC_ARR(M_PTS), &
     &           ARG_MIN, ARG_MAX, VAL_MIN, VAL_MAX
      INTEGER*4  J1, J2, J3, L_PTS, ITYP, NC, NR, L_BUT, ICODE, IER
      CHARACTER  COMMON_TITLE*64, TITS(M_STA)*8, BUT_NAME(M_BUT)*40, &
     &           BUT_LET(M_BUT)*3, PREF_USE*128
      INTEGER*4  I_LEN
!
 910  CONTINUE
      CALL CLRCH ( COMMON_TITLE )
      CALL CLRCH ( PREF_USE     )
!
! --- Set Common title for entire plot and PREF_USE
! --- PREF_USE is the prefix wcich will be used for printing entire multuplot
! --- Word "atm_" or "clo_" is appended to PREF_NAME
!
      IF ( ITYP .EQ. IATP_TYP ) THEN
           COMMON_TITLE = DBNAME(1:I_LEN(DBNAME))//'  Atmosphere path delay'
           PREF_USE     = PREF_NAME(1:I_LEN(PREF_NAME))//'atm_'
         ELSE IF ( ITYP .EQ. ICLP_TYP ) THEN
           COMMON_TITLE = DBNAME(1:I_LEN(DBNAME))//'  Clock function'
           PREF_USE     = PREF_NAME(1:I_LEN(PREF_NAME))//'clo_'
      END IF
!
! --- Cycle over stations
!
      DO 410 J1=1,L_STA
         CALL ERR_PASS   ( IUER, IER )
         IF ( ITYP .EQ. IATP_TYP ) THEN
!
! ----------- Get estimates of segmented atmosphere path delay
!
              CALL GETSTA_PTS ( M_PTS, L_PTS, STANAM(J1), ITYP, LBUF_ATM, &
     &                          BUF_ATM, TIME_ARR(1,J1), VAL_ARR(1,J1), &
     &                          SIG_ARR(1,J1), MODU_ARR, MODC_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9311, IUER, 'MDLPL_PLUS_CLOATM', 'Error '// &
     &                 'during parsing atmosphere file '//FINAM_ATM )
                   RETURN
               END IF
            ELSE IF ( ITYP .EQ. ICLP_TYP ) THEN
!
! ----------- Get estimates of segmented clock function
!
              CALL GETSTA_PTS ( M_PTS, L_PTS, STANAM(J1), ITYP, LBUF_CLO, &
     &                          BUF_CLO, TIME_ARR(1,J1), VAL_ARR(1,J1), &
     &                          SIG_ARR(1,J1), MODU_ARR, MODC_ARR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 9312, IUER, 'MDLPL_PLUS_CLOATM', 'Error '// &
     &                 'during parsing clock file '//FINAM_CLO )
                   RETURN
               END IF
         END IF
!
         IF ( L_PTS .EQ. 0 ) THEN
!
! ----------- Case when no data have been read
!
              L_PTS = 2
              TIME_ARR(1,J1) =  0.0
              TIME_ARR(2,J1) = 24.0
              VAL_ARR(1,J1)  =  0.0
              VAL_ARR(2,J1)  =  0.0
              SIG_ARR(1,J1)  =  0.0
              SIG_ARR(2,J1)  =  0.0
              MODU_ARR(1)    =  0.0
              MODU_ARR(2)    =  0.0
         END IF
!
! ------ Gather parameters for the plot
!
         CALL ERR_PASS    ( IUER, IER )
         CALL MDLPL_DIAGI ( L_PTS, TIME_ARR(1,J1), VAL_ARR(1,J1), &
     &                      SIG_ARR(1,J1), MODU_ARR, MODC_ARR, ITYP, &
     &                      DBNAME, STANAM(J1), PREF_NAME, DIAGI_ARR(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9313, IUER, 'MDLPL_PLUS_CLOATM', 'Error in '// &
     &            'attempt to put parameters of the plot' )
              RETURN
         END IF
!
         IF ( IUSE_STA(J1) .EQ. 0 ) THEN
              DIAGI_ARR(J1)%NCLR = 0
         END IF
!
         CALL CLRCH ( TITS(J1) )
         TITS(J1) = STANAM(J1)
!
         IF ( J1 .EQ. 1 ) THEN
              ARG_MIN = DIAGI_ARR(J1)%XMIN
              ARG_MAX = DIAGI_ARR(J1)%XMAX
              VAL_MIN = DIAGI_ARR(J1)%YMIN
              VAL_MAX = DIAGI_ARR(J1)%YMAX
            ELSE
              IF ( DIAGI_ARR(J1)%XMIN .LT. ARG_MIN ) THEN
                   ARG_MIN = DIAGI_ARR(J1)%XMIN
              END IF
              IF ( DIAGI_ARR(J1)%XMAX .GT. ARG_MAX ) THEN
                   ARG_MAX = DIAGI_ARR(J1)%XMAX
              END IF
              IF ( DIAGI_ARR(J1)%YMIN .LT. VAL_MIN ) THEN
                   VAL_MIN = DIAGI_ARR(J1)%YMIN
              END IF
              IF ( DIAGI_ARR(J1)%YMAX .GT. VAL_MAX ) THEN
                   VAL_MAX = DIAGI_ARR(J1)%YMAX
              END IF
         END IF
 410  CONTINUE
!
      IF ( MDLPL_FL_FRAME ) THEN
!
! -------- Set global frame
!
           DO 420 J2=1,L_STA
               DIAGI_ARR(J2)%XMIN = ARG_MIN
               DIAGI_ARR(J2)%XMAX = ARG_MAX
               DIAGI_ARR(J2)%YMIN = VAL_MIN
               DIAGI_ARR(J2)%YMAX = VAL_MAX
 420       CONTINUE
      END IF
 920  CONTINUE
!
! --- Clear button names and letter codes
!
      DO 430 J3=1,M_BUT
         CALL CLRCH ( BUT_NAME(J3) )
         CALL CLRCH ( BUT_LET(J3)  )
 430  CONTINUE
!
! --- Set button names and letter codes
!
      BUT_NAME(1) = '|Change directory | for Web plot'
      BUT_LET(1)  = 'Ccc'
!
      BUT_NAME(2) = 'HELP'
      BUT_LET(2)  = 'Hhh'
!
      IF ( MDLPL_FL_FRAME ) THEN
           BUT_NAME(3) = 'Set local | frame'
           BUT_LET(3)  = 'Lll'
         ELSE
           BUT_NAME(3) = 'Set global | frame'
           BUT_LET(3)  = 'Ggg'
      END IF
!
      BUT_NAME(4) = 'EXIT'
      BUT_LET(4)  = 'X'
      L_BUT = 4
!
! --- Determine how many columns and rows the plot should have
!
      IF ( L_STA .LE. 4 ) THEN
           NC = 2
           NR = 2
         ELSE IF ( L_STA .GE. 5  .AND.  L_STA .LE. 9 ) THEN
           NC = 3
           NR = 3
         ELSE IF ( L_STA .GE. 10  .AND.  L_STA .LE. 16 ) THEN
           NC = 4
           NR = 4
         ELSE IF ( L_STA .GE. 17  .AND.  L_STA .LE. 25 ) THEN
           NC = 5
           NR = 5
         ELSE IF ( L_STA .GT. 26 ) THEN
           NC = 6
           NR = 6
      END IF
!
! --- Show plots and a command panel using Multi_DiaGI utility
!
      ICODE = 0
      CALL ERR_PASS ( IUER, IER )
      CALL MULTI_DIAGI ( COMMON_TITLE(1:I_LEN(COMMON_TITLE)), &
     &                   L_STA, NC, NR, TITS, &
     &                   M_BUT, BUT_NAME, BUT_LET, &
     &                   PREF_USE, DIAGI_ARR, ICODE, IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL PGENDQ() ! Iconify window
           IF ( IER .EQ. 4197  .OR.  IER .EQ. 4199  .OR.  IER .EQ. 4200 ) THEN
                CALL ERR_LOG ( 9314, -1, 'MDLPL_PLUS_CLOATM', 'Error in '// &
     &              'attempt to make a hardcopy. Try to change a Web '// &
     &              'directoty' )
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
                GOTO 920
              ELSE
                CALL ERR_LOG ( 9314, IUER, 'MDLPL_PLUS_CLOATM', &
     &               'Error in MULTI_DIAGI' )
                RETURN
           END IF
      END IF
!
! --- Parse a returned command code
!
      IF ( ICODE .GT. 0 ) THEN
           IF ( BUT_LET(ICODE)(1:1) .EQ. 'C' ) THEN
                CALL PGENDQ() ! Iconify window
!
! ------------- Change Web_dir command
!
                CALL CHANGE_WEBDIR ( PREF_NAME )
                GOTO 910
             ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'L' ) THEN
                CALL PGENDQ() ! Iconify window
!
! ------------- Set local frame
!
                MDLPL_FL_FRAME = .FALSE.
                GOTO 910
             ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'G' ) THEN
                CALL PGENDQ() ! Iconify window
!
! ------------- Set global frame
!
                MDLPL_FL_FRAME = .TRUE.
                GOTO 910
             ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'H' ) THEN
!
! ------------- Help command
!
                CALL ERR_PASS  ( IUER, IER )
                IF ( ITYP .EQ. IATP_TYP ) THEN
                     CALL MDLPL_HLP ( MDLPL_PLUS_HLP_ATM, IER )
                  ELSE IF ( ITYP .EQ. ICLP_TYP ) THEN
                     CALL MDLPL_HLP ( MDLPL_PLUS_HLP_CLO, IER )
                END IF
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9315, IUER, 'MDLPL_PLUS_CLOATM', 'Error '// &
     &                   'in attempt to get on-line help' )
                     RETURN
                END IF
                CALL PGENDQ() ! Iconify window
                GOTO 920
             ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'X' ) THEN
                CALL PGENDQ() ! Iconify window
!
! ------------- Exit command
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
          END IF
      END IF
!
      CALL PGENDQ() ! Iconify window
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MDLPL_PLUS_CLOATM  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MDLPL_PLUS_EOP ( DBNAME, PREF_NAME, &
     &                            LBUF_EOP, BUF_EOP, FINAM_EOP, &
     &                            MLDLP_FL_EOPMOD, DIAGI_ARR, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MDLPL_PLUS_EOP  makes plots of segmented adjustments to   *
! *   EOP using Multi_DiaGI interface.                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       DBNAME ( CHARACTER ) -- String with database name and version  *
! *                               number.                                *
! *     LBUF_EOP ( INTEGER*4 ) -- Number of lines in the file with       *
! *                               adjustments to segmented EOP.          *
! *      BUF_EOP ( CHARACTER ) -- Text buffer with content of the file   *
! *                               with adjustments to segmented EOP.     *
! *    FINAM_EOP ( CHARACTER ) -- File name with adjustments to          *
! *                               segmented EOP.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *    DIAGI_ARR ( RECORD    ) -- Array of DiaGI data structures for     *
! *                               plotting. Dimension: 3.                *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * MDLPL_FL_EOPMOD (LOGICAL*4) - Flag of plotting model values for EOP. *
! *                               If TRUE then the a priori and model    *
! *                               values of EOP will be displayed also.  *
! *    PREF_NAME ( CHARACTER ) -- Prefix string with pathname which will *
! *                               be prepend before the second part of   *
! *                               filename of hardcopies of the plots.   *
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
! *  ###  23-JUL-99  MDLPL_PLUS_EOP  v1.2 (c) L. Petrov 15-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'mdlcm.i'
      INTEGER*4  LBUF_EOP, IUER
      CHARACTER  DBNAME*(*), PREF_NAME*(*), BUF_EOP(*)*(*), FINAM_EOP*(*)
      LOGICAL*4  MLDLP_FL_EOPMOD
      TYPE ( DIAGI_STRU ) ::  DIAGI_ARR(*)
!
      INTEGER*4    M_PTS, M_BUT, M_EOP
      PARAMETER  ( M_PTS = MAX_PTS )
      PARAMETER  ( M_BUT = 8       )
      PARAMETER  ( M_EOP = 3       )
      REAL*8     TIME_ARR(M_PTS,M_EOP), VAL_ARR(M_PTS,M_EOP), &
     &           SIG_ARR(M_PTS,M_EOP), MODU_ARR(M_PTS,M_EOP), &
     &           MODC_ARR(M_PTS,M_EOP)
      INTEGER*4  J1, J2, L_PTS, ITYP, NC, NR, L_BUT, ICODE, IER
      CHARACTER  COMMON_TITLE*64, TITS(M_EOP)*20, BUT_NAME(M_BUT)*40, &
     &           BUT_LET(M_BUT)*3, EOPNAM(M_EOP)*3, PREF_USE*128
      DATA       EOPNAM / 'xpl', 'ypl', 'ut1' /
      INTEGER*4  I_LEN
!
 910  CONTINUE
      CALL CLRCH ( COMMON_TITLE )
      CALL CLRCH ( PREF_USE     )
!
! --- Setting common titel and prefix to be used in printing common plot
!
      COMMON_TITLE = DBNAME(1:I_LEN(DBNAME))//'  Estimates of EOP'
      PREF_USE     = PREF_NAME(1:I_LEN(PREF_NAME))//'eop_'
!
! --- Cycle on components of EOP
!
      DO 410 J1=1,M_EOP
         IF ( J1 .EQ. 1 ) ITYP = IXPL_TYP
         IF ( J1 .EQ. 2 ) ITYP = IYPL_TYP
         IF ( J1 .EQ. 3 ) ITYP = IUT1_TYP
         CALL ERR_PASS   ( IUER, IER )
!
! ------ Get vales of adustments of the J1-th component of EOP
!
         CALL GETSTA_PTS ( M_PTS, L_PTS, ' ', ITYP, LBUF_EOP, &
     &                     BUF_EOP, TIME_ARR(1,J1), VAL_ARR(1,J1), &
     &                     SIG_ARR(1,J1), MODU_ARR(1,J1), MODC_ARR(1,J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9321, IUER, 'MDLPL_PLUS_EOP', 'Error '// &
     &            'during parsing atmosphere file '//FINAM_EOP )
              RETURN
         END IF
!
         IF ( L_PTS .EQ. 0 ) THEN
!
! ----------- Case when no data have been read
!
              L_PTS = 2
              TIME_ARR(1,J1) =  0.0
              TIME_ARR(2,J1) = 24.0
              VAL_ARR(1,J1)  =  0.0
              VAL_ARR(2,J1)  =  0.0
              SIG_ARR(1,J1)  =  0.0
              SIG_ARR(2,J1)  =  0.0
              MODU_ARR(1,J1) =  0.0
              MODU_ARR(2,J1) =  0.0
         END IF
!
! ------ Gather parameters for the plot
!
         CALL ERR_PASS    ( IUER, IER )
         CALL MDLPL_DIAGI ( L_PTS, TIME_ARR(1,J1), VAL_ARR(1,J1), &
     &                      SIG_ARR(1,J1), MODU_ARR(1,J1), MODC_ARR(1,J1), ITYP, &
     &                      DBNAME, EOPNAM, PREF_NAME, DIAGI_ARR(J1), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 9322, IUER, 'MDLPL_PLUS_EOP', 'Error in '// &
     &            'attempt to put parameters of the plot' )
              RETURN
         END IF
         IF ( .NOT. MLDLP_FL_EOPMOD ) DIAGI_ARR(J1)%NCLR = 1
!
         CALL CLRCH ( TITS(J1) )
 410  CONTINUE
!
! --- Set plots titles
!
      TITS(1) = 'X pole (microarcsec)'
      TITS(2) = 'Y pole (microarcsec)'
      TITS(3) = 'UT1 (microsec)'
 920  CONTINUE
!
      DO 420 J2=1,M_BUT
         CALL CLRCH ( BUT_NAME(J2) )
         CALL CLRCH ( BUT_LET(J2)  )
 420  CONTINUE
!
! --- Set button names and letter codes
!
      BUT_NAME(1) = '|Change directory | for Web plot'
      BUT_LET(1)  = 'Ccc'
!
      IF ( MLDLP_FL_EOPMOD ) THEN
           BUT_NAME(2) = 'Model off'
           BUT_LET(2)  = 'Fff'
        ELSE
           BUT_NAME(2) = 'Model on'
           BUT_LET(2)  = 'Nnn'
      END IF
!
      BUT_NAME(3) = 'HELP'
      BUT_LET(3)  = 'Hhh'
!
      BUT_NAME(4) = 'EXIT'
      BUT_LET(4)  = 'Xxx'
      L_BUT = 4
!
      NC = 2
      NR = 2
      ICODE = 0
!
! --- Make a plot using Multi_DiaGI interface
!
      CALL ERR_PASS ( IUER, IER )
      CALL MULTI_DIAGI ( COMMON_TITLE(1:I_LEN(COMMON_TITLE)), &
     &                   3, NC, NR, TITS, &
     &                   M_BUT, BUT_NAME, BUT_LET, &
     &                   PREF_USE, DIAGI_ARR, ICODE, IER )
!
      IF ( IER .NE. 0 ) THEN
           CALL PGENDQ() ! Iconify window
           IF ( IER .EQ. 4197  .OR.  IER .EQ. 4199  .OR.  IER .EQ. 4200 ) THEN
                CALL ERR_LOG ( 9323, -1, 'MDLPL_PLUS_EOP', 'Error in attempt '// &
     &              'to make a hardcopy. Try to change a Web directoty' )
                CALL HIT_CONT ( %VAL(0), %VAL(0) )
                GOTO 920
              ELSE
                CALL ERR_LOG ( 9323, IUER, 'MDLPL_PLUS_EOP', &
     &               'Error in MULTI_DIAGI' )
                RETURN
           END IF
      END IF
!
! --- Parse a returned command code
!
      IF ( ICODE .GT. 0 ) THEN
           IF ( BUT_LET(ICODE)(1:1) .EQ. 'C' ) THEN
                CALL PGENDQ() ! Iconify window
!
! ------------- Change Web_dir command
!
                CALL CHANGE_WEBDIR ( PREF_NAME )
                GOTO 910
             ELSE IF ( BUT_LET(ICODE) .EQ. 'F' ) THEN
                CALL PGENDQ() ! Iconify window
!
! ------------- Model OFF
!
                MLDLP_FL_EOPMOD = .FALSE.
                GOTO 920
             ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'N' ) THEN
                CALL PGENDQ() ! Iconify window
!
! ------------- Model ON
!
                MLDLP_FL_EOPMOD = .TRUE.
                GOTO 920
             ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'H' ) THEN
!
! ------------- Help command
!
                CALL ERR_PASS  ( IUER, IER )
                CALL MDLPL_HLP ( MDLPL_PLUS_HLP_EOP, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 9324, IUER, 'MDLPL_PLUS_EOP', 'Error '// &
     &                   'in attempt to get on-line help' )
                     RETURN
                END IF
                CALL PGENDQ() ! Iconify window
                GOTO 920
             ELSE IF ( BUT_LET(ICODE)(1:1) .EQ. 'X' ) THEN
                CALL PGENDQ() ! Iconify window
!
! ------------- Exit command
!
                CALL ERR_LOG ( 0, IUER )
                RETURN
          END IF
      END IF
!
      CALL PGENDQ() ! Iconify window
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MDLPL_PLUS_EOP  #!#
