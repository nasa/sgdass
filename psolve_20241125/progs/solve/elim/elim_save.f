      SUBROUTINE ELIM_SAVE ( N_OBS, IDB2, IDBF, DBOBJ, RES, OBSBAS )
! ************************************************************************
! *                                                                      *
! *     Routine  ELIM_SAVE  updates scratch file: it sets up unweight    *
! *   flag in scratch file in according to the values in RES data        *
! *   structure to the scratch file (and take into accounbt some         *
! *   pecularities of SOLVE downweight scheme).                          *
! *                                                                      *
! *     In the case when during restoration of suppressed observations   *
! *   number of ambiguities was changed ELIM_SAVE updates ambiguity      *
! *   counter, observed X-band delay, ionosphere calibration.            *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *   N_OBS ( INTEGER*4 ) -- Total number of observations in the session *
! *    IDB2 ( INTEGER*2 ) -- Index of the considered database in the     *
! *                          scratch file.                               *
! *    IDBF ( INTEGER*4 ) -- Index the foirst observation of the         *
! *                          database in the scratch file.               *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
! *     RES ( RECORD    ) -- Array of data structures keeping            *
! *                          information about residuals.                *
! *  OBSBAS ( RECORD    ) -- Array of data structures keeping            *
! *                          baseline-dependent parameters of the        *
! *                          observations and observables.               *
! *                                                                      *
! *  ###  25-SEP-97    ELIM_SAVE   v3.6  (c)  L. Petrov 26-JUL-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'socom.i'
      INCLUDE   'oborg.i'
      INCLUDE   'obser.i'
      INTEGER*4  N_OBS, IDBF
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      INTEGER*4  NREC, J1
!
      INTEGER*2   IDB2
      INTEGER*2   JSITN(4,MAX_ARC_STA),   ITT(MAX_ARC_STA)
      INTEGER*4   JCAPPL(MAX_ARC_STA),    JCAVAL(MAX_ARC_STA)
      INTEGER*2   JCAFFL(7,MAX_ARC_STA),  NFCAL,NAMSTA
      INTEGER*2   JSITI(MAX_ARC_STA),     ITTB(MAX_ARC_BSL)
      INTEGER*2   AX_TYPES(MAX_ARC_STA),  OBCAPL, MCAPL
      REAL*8      LATS(MAX_ARC_STA),      HEIGHTS(MAX_ARC_STA)
      REAL*8      BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      REAL*8      ET(2,MAX_ARC_BSL),      AX_OFFS(MAX_ARC_STA), &
     &            SE(MAX_ARC_STA),        SS(MAX_ARC_SRC)
      CHARACTER   FCAL_NAMES(112)*8
      CHARACTER   RW_BAS_NAM(2,MAX_ARC_BSL)*8
      LOGICAL*4   FL_CUEL, FL_DSBS, FL_DSSO 
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4,  EXTERNAL :: DATYP_INQ, SUPR_INQ, META_SUPR_INQ
!
! --- Read station names, status array, eccentricity data, monument
! --- names, and set up a correspondence table between the stations
! --- in NAMFIL (JSIT's) and those in PARFIL (ISIT's).
!
      CALL NCORT ( JSITN, JSITI, JCAPPL, NUMSTA, ITT, IDB2, &
     &             DBOBJ%IDATYP, ITTB, ET, SE, SS, OBCAPL, MCAPL, JCAVAL, &
     &             LATS, HEIGHTS, AX_TYPES, AX_OFFS, BARO_CALS, &
     &             BARO_HEIGHTS, JCAFFL, FCAL_NAMES, NFCAL, NAMSTA, &
     &             CALCV )
!
! --- Openning scratch file
!
      CALL ACS_OBSFIL ( 'O' )
      DO 410 J1=1,N_OBS
         NREC = IDBF+J1-1
!
! ------ Reading the NREC-th record in scratch file which corresponds
! ------ to the J1-th observation
!
         CALL USE_OBSFIL ( IOBSFIL, NREC, 'R' )
!
         IF ( RES(J1)%NUMAMB_USED .NE. RES(J1)%NUMAMB_NEW ) THEN
!
! ----------- Ambiguity was changed.
! ----------- Remind once more:
! -----------        FAMB is in seconds,
! -----------        DOBS, DPH, GION(1), PHION are in microseconds
!
! ----------- 1) Update of ambiguity counter
!
              IF ( DATYP_INQ ( DBOBJ%IDATYP, GROUP__DTP ) ) THEN
!
! ---------------- Group delay
!
                   NUMAMB = NUMAMB + ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED)
                 ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
!
! ---------------- Phase delay
!
                   IF ( DATYP_INQ ( DBOBJ%IDATYP, XBAND__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, COMB__DTP  )      ) THEN
                        NPHAM4 = NPHAM4 + &
     &                           ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED )
                      ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, SBAND__DTP ) ) THEN
                        NPHAM4_S = NPHAM4_S + &
     &                           ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED )
                   END IF
              END IF
!
! ----------- 2) Update of observable
!
              IF ( DATYP_INQ ( DBOBJ%IDATYP, GROUP__DTP ) ) THEN
!
! ---------------- Group delay
!
                   DOBS = DOBS + ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED )* &
     &                           FAMB*1.D6
                 ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
!
! ---------------- Phase delay
!
                   IF ( DATYP_INQ ( DBOBJ%IDATYP, XBAND__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, COMB__DTP  )      ) THEN
                        DPH = DPH + ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED)* &
     &                        PHAMI8
                      ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, SBAND__DTP ) ) THEN
                        DPHXS = DPHXS + &
     &                          ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED )* &
     &                          PHAMI8_S
                   END IF
              END IF
!
! ----------- 3) Update of ionosphere calibration
!
              IF ( DATYP_INQ ( DBOBJ%IDATYP, GROUP__DTP ) ) THEN
!
! ---------------- NB: sign!!
!
                   GION(1) = GION(1) - &
     &                       ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED)* &
     &                       FAMB*1.D6* &
     &                       EFFREQ_XS**2/ ( EFFREQ**2 - EFFREQ_XS**2 )
!
! ---------------- Recalculate formal error. We do it in order to prevent
! ---------------- suppression of this observation only because it had
! ---------------- zero formal error of group delay ionosphere calibration.
!
                   GIONSG(1) = EFFREQ_XS**2 / ( EFFREQ**2 - EFFREQ_XS**2 )* &
     &                         SQRT(DERR**2 + DERRXS**2)
                   GIONSG(2) = EFFREQ_XS**2 / ( EFFREQ**2 - EFFREQ_XS**2 )* &
     &                         SQRT(RERR**2 + RERRXS**2)
!
! ---------------- Setting bit status (for the case)
!
                   IF ( SUPMET .NE. SUPMET__META ) THEN
                        CALL SBIT ( ICORR,   INT2(4),   INT2(1) )
                        CALL SBIT ( ICORR,   INT2(5),   INT2(0) )
                        CALL SBIT ( OBSBAS(J1)%SUPSTAT, GION__SPS, INT2(0) )
                        CALL SBIT ( OBSBAS(J1)%SUPSTAT, GIO1__SPS, INT2(0) )
                        CALL SBIT ( OBSBAS(J1)%SUPSTAT, GIO3__SPS, INT2(0) )
                    END IF
                 ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, PHASE__DTP ) ) THEN
!
! ---------------- NB: sign!! It uses old conventions
!
                   IF ( DATYP_INQ ( DBOBJ%IDATYP, XBAND__DTP ) .OR. &
     &                  DATYP_INQ ( DBOBJ%IDATYP, COMB__DTP  )      ) THEN
                        PHION = PHION + &
     &                     ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED)* &
     &                     PHAMI8* &
     &                     PHEFFREQ_XS**2/ ( PHEFFREQ**2 - PHEFFREQ_XS**2 )
                      ELSE IF ( DATYP_INQ ( DBOBJ%IDATYP, SBAND__DTP ) ) THEN
                        PHION = PHION - &
     &                     ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED)* &
     &                     PHAMI8_S* &
     &                     PHEFFREQ_XS**2/ ( PHEFFREQ**2 - PHEFFREQ_XS**2 )
                   END IF
              END IF
!
! ----------- Setting numamb_new to numamb_used
!
              RES(J1)%NUMAMB_NEW = RES(J1)%NUMAMB_USED
         END IF
!
! ------ Putting suppression stastus and used action for suppression from
! ------ OBSBAS data structure to oborg area
!
         IF ( SUPMET == SUPMET__META ) THEN
              AUTO_SUP   = OBSBAS(J1)%AUTO_SUP
              USER_SUP   = OBSBAS(J1)%USER_SUP
              USER_REC   = OBSBAS(J1)%USER_REC
            ELSE 
              SUPSTAT(1) = OBSBAS(J1)%SUPSTAT(1)
              SUPSTAT(2) = OBSBAS(J1)%SUPSTAT(2)
              UACSUP     = OBSBAS(J1)%UACSUP
!
! ----------- Resetting IUNW, IUNWP flags for backward compatibility 
! ----------- with the PRE-APR98 versions of SOLVE
!
              CALL SUPSTAT_UNW ( SUPSTAT, UACSUP, IUNW, IUNWP )
         END IF
!
         IF ( SUPMET == SUPMET__META ) THEN
              FL_CUEL = BTEST ( AUTO_SUP, INT4(CUEL__SPS) )
              FL_DSBS = BTEST ( AUTO_SUP, INT4(DSBS__SPS) )
              FL_DSSO = BTEST ( AUTO_SUP, INT4(DSSO__SPS) )
            ELSE 
              FL_CUEL = SUPR_INQ ( SUPSTAT, UACSUP, CUEL__SPS )
              FL_DSBS = SUPR_INQ ( SUPSTAT, UACSUP, DSBS__SPS )
              FL_DSSO = SUPR_INQ ( SUPSTAT, UACSUP, DSSO__SPS )
         END IF
!         
         IF ( .NOT. FL_CUEL .AND. &
     &        .NOT. FL_DSBS .AND. &
     &        .NOT. FL_DSSO       ) THEN
!
! ----------- Writing NREC-th record in scratch file if the observation has not
! ----------- been deselected since
!
! ----------- 1) Observation made below cut off limit  OR
! ----------- 2) Observation at deselected baseline    OR
! ----------- 3) Observation of deselected source
!
              CALL USE_OBSFIL ( IOBSFIL, NREC, 'W' )
          END IF
 410  CONTINUE
!
! --- Set off flag of ambiguities changes
!
      DBOBJ%F_AMB_CHANGED = .FALSE.
!
! --- Closing scratch file
!
      CALL ACS_OBSFIL ( 'C' )
!
      RETURN
      END  !#!  ELIM_SAVE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UNPROT_RES ( N_OBS, RES )
! ************************************************************************
! *                                                                      *
! *   Routine  UNPROT_RES  initializes protection array in RES:          *
! *   All observations acquire status "Unprotected".                     *
! *                                                                      *
! *  ###  17-SEP-97   UNPROT_RES   v1.0  (c)  L. Petrov  26-SEP-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INTEGER*4  N_OBS
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      INTEGER*4  J1
!
      DO 410 J1=1,N_OBS
         RES(J1)%PROT = .FALSE.
 410  CONTINUE
!
      RETURN
      END  !#!  UNPROT_RES  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ELIM_WRIEST ( B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Procedure  ELIM_EXPAND  updates covariance matrix and vector of    *
! *   estimates, unscales them and writes down in NRMFIL in expanded     *
! *   form -- just in the form in which CRES is expect to see it.        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  12-SEP-97   ELIM_WRIEST  v1.1  (c)  L. Petrov  19-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'precm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  IUER
      INTEGER*4  JA, JB, JS, IER
      INTEGER*8        ML_FNOR,  LEN_ARR, NELEM
      ADDRESS__TYPE :: MA_FNOR, IADR_ARR
      REAL*8     SIG_B3D
      CHARACTER  STR*32, FINAM_FAST*256
      INTEGER*4  MAT_E
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
!
      ML_FNOR  = -1
!
! --- If the number of parameters has been changed then we should
! --- prepare B3D data structures for calling CRES which will be called
! --- from OPTIN after termination ELIM
!
! --- Unscaling vector of the esimates and covariance matrix
!
      CALL B3D_USC_F ( .TRUE., .TRUE., B3DOBJ%NBS, &
     &          B3DOBJ%N_GLO, (INT8(B3DOBJ%N_GLO)*INT8(B3DOBJ%N_GLO+1))/2, &
     &          B3DOBJ%SB,    (INT8(B3DOBJ%SB)*INT8(B3DOBJ%SB+1))/2, &
     &          B3DOBJ%SX,    (INT8(B3DOBJ%SX)*INT8(B3DOBJ%SX+1))/2, &
     &                        ((B3DOBJ%NBS-1)*(B3DOBJ%NBS-2))/2, &
     &        %VAL(B3DOBJ%AD_B0),    %VAL(B3DOBJ%AD_E0),   %VAL(B3DOBJ%AD_U0), &
     &        %VAL(B3DOBJ%AD_B(1)),  %VAL(B3DOBJ%AD_C(1)), %VAL(B3DOBJ%AD_D(1)), &
     &        %VAL(B3DOBJ%AD_ES(1)), %VAL(B3DOBJ%AD_US(1)), &
     &        %VAL(B3DOBJ%AD_BX),    %VAL(B3DOBJ%AD_CX),   %VAL(B3DOBJ%AD_DX), &
     &        %VAL(B3DOBJ%AD_ESX),   %VAL(B3DOBJ%AD_USX),  %VAL(B3DOBJ%AD_CVF) )
!
! --- Finding estimates of the variances of the parameters
!
      SIG_B3D = 1.D0
      CALL ERR_PASS  ( IUER, IER )
      CALL B3D_DSP_X ( FAST_COV, B3DOBJ%NBS, &
     &          B3DOBJ%N_GLO, (INT8(B3DOBJ%N_GLO)*INT8(B3DOBJ%N_GLO+1))/2, &
     &          B3DOBJ%SB,    (INT8(B3DOBJ%SB)*INT8(B3DOBJ%SB+1))/2, &
     &          B3DOBJ%SX,    (INT8(B3DOBJ%SX)*INT8(B3DOBJ%SX+1))/2, &
     &          %VAL(B3DOBJ%AD_B0),   %VAL(B3DOBJ%AD_Z0), &
     &          %VAL(B3DOBJ%AD_C(1)), %VAL(B3DOBJ%AD_ZS(1)), &
     &          %VAL(B3DOBJ%AD_CX),   %VAL(B3DOBJ%AD_ZSX), SIG_B3D, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6681, IUER, 'ELIM_WRIEST', 'Failure in '// &
     &         'finding estimates of variances of the parameter '// &
     &         'estimates while database '//B3DOBJ%DBNAME_MES// &
     &         ' was processing' )
           RETURN
      END IF
!
! --- Expanding squeezed submatrices of B3D object to full matrix
!
      NELEM = 8*MAT_E4 ( M_GPA, NPARAM )
      LEN_ARR = 8*INT8(NELEM)
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER,  ML_FNOR, MA_FNOR, 1, &
     &                      LEN_ARR, IADR_ARR    )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  (          STR )
           CALL IINCH  ( NELEM*8, STR )
           CALL ERR_LOG ( 6682, IUER, 'ELIM_WRIEST', 'Error during the '// &
     &         'attempt to grab '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory for allocation full normal matrix while database '// &
     &          B3DOBJ%DBNAME_MES//' was processing' )
           RETURN
      END IF
!
! --- Settinng usual SOLVE pointers
!
!  ARR(1) pointer to vectrs of scales
!
      JA = 1+3*M_GPA !  pointer to normal matrix
      JB = 1+2*M_GPA !  pointer to normal vector
      JS = 1+  M_GPA !  pointer to vectors of sigmas
!
      CALL NOUT8 ( 8*(INT8(NPARAM)*INT8(NPARAM+1))/2, %VAL(IADR_ARR + (JA-1)*8) )
      CALL NOUT_R8 (  NPARAM,               %VAL(IADR_ARR + (JB-1)*8) )
      CALL NOUT_R8 (  NPARAM,               %VAL(IADR_ARR + (JS-1)*8) )
      CALL NOUT_R8 (  NPARAM,               %VAL(IADR_ARR)            )
!
! --- Putting expanded and rearranged covariance matrix to ARR(JA),
! --- vector of estimates in ARR(JB), vector of variances of the estimates
! --- in ARR(JS) and scaling vector in ARR(1) (Don't ask me why I move
! --- scaling vector -- who knows, maybe it is used somewhere ;-)     )
! --- Of course, ARR(JA) will have elements of covariance matrix only for
! --- global paramaters, global-local,  main block diagonal and down
! --- block-diagonal. Others elements will be zeroes.
!
      CALL EXPAND_B3D ( B3DOBJ, &
     &                  %VAL(IADR_ARR + (JA-1)*8), &
     &                  %VAL(IADR_ARR + (JB-1)*8), &
     &                  %VAL(IADR_ARR + (JS-1)*8), &
     &                  %VAL(IADR_ARR) )
!
! --- Writing down vector of the estimates and covariance matrix
!
      CALL USE_NRMFIL ( %VAL(IADR_ARR), NPARAM, 'OWC' )
!
! --- Rewrite some fields of B3DOBJ in fast-file. It is done for CRES
! --- since B3DOBJ contains corrected theoretical values for delay and
! --- delay rate and they are necessary for calculations of residuals.
!
      CALL CLRCH ( FINAM_FAST )
      FINAM_FAST = PRE_SCR_DIR(1:PRE_SD_LEN)//'FAST'//PRE_LETRS
!
      CALL ERR_PASS  ( IUER, IER )
      CALL WRNOR_B3D ( FINAM_FAST, B3DOBJ, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6683, IUER, 'ELIM_WRIEST', 'Error during writing '// &
     &         'file '//FINAM_FAST(1:I_LEN(FINAM_FAST))//' while '// &
     &         'session '//B3DOBJ%DBNAME_MES//' was processing' )
           RETURN
      END IF
!
! --- Freeing dynamic memory
!
      IF ( ML_FNOR .GT. 0 ) THEN
           CALL FREE_MEM ( MA_FNOR )
           ML_FNOR = 0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ELIM_WRIEST  #!#
