      SUBROUTINE PAMB_SAVE ( IDBF, N_OBS, DBOBJ, OBSBAS, RES, PAMBI, F_AMB, &
     &                       F_SUP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PAMB_SAVE  updates phase delays, their ambiguities and    *
! *   suppression status in scratch area (oborg.i) using values kept in  *
! *   OBSBAS and PAMBI. It sets also flags in NAMFIL: phase delay        *
! *   ionosphere correction is available. It updates OBSBAS and PAMBI    *
! *   also in order to accommodate changes in oborg area.                *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      IDBF ( INTEGER*4 ) -- Index the first observation of the        *
! *                            database in the scratch file.             *
! *     N_OBS ( INTEGER*4 ) -- Total number of observations in the       *
! *                            session.                                  *
! *     DBOBJ ( RECORD    ) -- Data structure which keeps general        *
! *                            information about the database such as    *
! *                            lists of the objects.                     *
! *    OBSBAS ( RECORD    ) -- Array of data structures which keeps      *
! *                            baseline dependent information about the  *
! *                            session.                                  *
! *       RES ( RECORD    ) -- Array of data structures keeping          *
! *                            information about residuals.              *
! *     PAMBI ( RECORD    ) -- Array of data structures keeping          *
! *                            information about phase delays, their     *
! *                            errors, ambiguities and etc.              *
! *     F_AMB ( LOGICAL*4 ) -- Flag: if .TRUE. then ambiguities will be  *
! *                            saved: rewritten to oborg area.           *
! *     F_SUP ( LOGICAL*4 ) -- Flag: if .TRUE. then suppression status   *
! *                            of phase delay observables will be saved  *
! *                            in oborg area.                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      IUER ( INTEGER*4, OPT ) -- Universal error handler.             *
! *                            Input: switch IUER=0 -- no error messages *
! *                                   will be generated even in the case *
! *                                   of error. IUER=-1 -- in the case   *
! *                                   of error the message will be put   *
! *                                   on stdout.                         *
! *                            Output: 0 in the case of successful       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  10-NOV-97    PAMB_SAVE    v4.5 (c) L. Petrov  24-JUN-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'obser.i'
      INCLUDE   'oborg.i'
      INCLUDE   'pamb.i'
      INTEGER*4  IDBF, N_OBS, IUER
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      TYPE ( BAS_O__STRU ) ::  OBSBAS(N_OBS)
      TYPE ( RES_O__STRU ) ::  RES(N_OBS)
      TYPE ( PAMBI__STRU ) ::  PAMBI(N_OBS)
      INTEGER*2  IP_I2, KERR, ION_STS, ICAL_AV, ICAL_AP, IDATYP_OLD
      CHARACTER  JBUF(MAX_ARC_STA)*70, STR*32, LCARD*4
      INTEGER*4  J1, J2, J3, IO, NCB
      LOGICAL*4  SUPR_INQ, F_AMB, F_SUP
      LOGICAL*4  FL_CUEL, FL_DSBS, FL_DSSO 
      REAL*8     PH_IONO_X, PH_IONO_S, PH_IONO_X_ERR, PH_IONO_S_ERR
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL :: KBIT
!
      CALL ACS_OBSFIL ( 'O' )
      DO 410 J1=IDBF,IDBF+N_OBS-1
         CALL USE_OBSFIL ( IOBSFIL, J1, 'R' )
         IF ( F_AMB ) THEN
              IF ( RES(J1)%NUMAMB_USED .NE. RES(J1)%NUMAMB_NEW ) THEN
!
! ---------------- Phase delay ambiguity written in RES data structure was 
! ---------------- changed. Remind once more:
! ----------------          DPH, PHION are in microseconds
!
! ---------------- 1) Update of ambiguity counter
!
                   NPHAM4 = NPHAM4 + ( RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED)
!
! ---------------- 2) Update of phase delay observable
!
                   DPH = DPH + (RES(J1)%NUMAMB_NEW - RES(J1)%NUMAMB_USED)*PHAMI8
!
! ---------------- Setting numamb_new to numamb_used
!
                   RES(J1)%NUMAMB_NEW = RES(J1)%NUMAMB_USED
              END IF
!
              IF ( RES(J1)%NUMAMB_GR_USED .NE. RES(J1)%NUMAMB_GR_NEW ) THEN
!
! ---------------- Group delay ambiguity written in RES data structure 
! ---------------- was changed. Remind once more:
! ----------------        DOBS is are in microseconds, FAMB is in sec
!
! ---------------- 1) Update of ambiguity counter
!
                   NUMAMB = NUMAMB + ( RES(J1)%NUMAMB_GR_NEW - RES(J1)%NUMAMB_GR_USED)
!
! ---------------- 2) Update of group delay observable
!
                   DOBS = DOBS + (RES(J1)%NUMAMB_GR_NEW - RES(J1)%NUMAMB_GR_USED)*FAMB*1.D6
!
! ---------------- Setting numamb_new to numamb_used
!
                   RES(J1)%NUMAMB_GR_NEW = RES(J1)%NUMAMB_GR_USED
              END IF
!
! ----------- The same for the S-band
!
              IF ( RES(J1)%NUMAMB_S_USED .NE. RES(J1)%NUMAMB_S_NEW ) THEN
!
! ---------------- Ambiguity written in RES data structure was changed.
! ---------------- Remind once more:
! ----------------        DPH, PHION are in microseconds
!
! ---------------- 1) Update of ambiguity counter
!
                   NPHAM4_S = NPHAM4_S + ( RES(J1)%NUMAMB_S_NEW - &
     &                                     RES(J1)%NUMAMB_S_USED )
!
! ---------------- 2) Update of phase delay observable
!
                   DPH = DPH + ( RES(J1)%NUMAMB_S_NEW - &
     &                           RES(J1)%NUMAMB_S_USED  ) *PHAMI8_S
!
! ---------------- Setting numamb_new to numamb_used
!
                   RES(J1)%NUMAMB_NEW = RES(J1)%NUMAMB_USED
              END IF
!
              IF ( RES(J1)%NUMAMB_GR_S_USED .NE. RES(J1)%NUMAMB_GR_S_NEW ) THEN
!
! ---------------- Group delay ambiguity written in RES data structure 
! ---------------- was changed. Remind once more:
! ----------------        DOBS is are in microseconds, FAMB is in sec
!
! ---------------- 1) Update of ambiguity counter
!
                   NUMAMB_S = NUMAMB_S + ( RES(J1)%NUMAMB_GR_S_NEW - RES(J1)%NUMAMB_GR_S_USED)
!
! ---------------- 2) Update of group delay observable
!
                   DOBS_S = DOBS_S + (RES(J1)%NUMAMB_GR_S_NEW - RES(J1)%NUMAMB_GR_S_USED)*FAMB_S*1.D6
!
! ---------------- Setting numamb_new to numamb_used
!
                   RES(J1)%NUMAMB_GR_S_NEW = RES(J1)%NUMAMB_GR_S_USED
              END IF
!
! ----------- Update of phase delay ambiguities for X-band written in PAMBI
! ----------- data structure
!
              NPHAM4   = NPHAM4 + PAMBI(J1)%NPHAMB_X
              DPH      = DPH    + PAMBI(J1)%NPHAMB_X*PHAMI8
              PAMBI(J1)%NPHAMB_X = 0
!
! ----------- Update of phase delay ambiguities for S-band written in PAMBI
! ----------- data structure
!
              NPHAM4_S = NPHAM4_S + PAMBI(J1)%NPHAMB_S
              DPHXS    = DPHXS    + PAMBI(J1)%NPHAMB_S*PHAMI8_S
              PAMBI(J1)%NPHAMB_S = 0
!
! ----------- Calculation of phase delay ionosphere correction
!
              CALL IONO_OBSER ( OBSBAS(J1), PH_IONO_X, PH_IONO_S, &
     &                          PH_IONO_X_ERR, PH_IONO_S_ERR  )
!
! ----------- Update of phase delay ionosphere correction for compatibility
! ----------- with archaic SOLVE  (NB: sign! Archaic SOLVE subtract phase
! ----------- ionosphere correction)
!
              PHION  = -PH_IONO_X *1.D6
              PHIONS =  PH_IONO_X_ERR
!
! ----------- Setting flags of availability of phase delay ionosphere correction
!
              CALL SBIT ( ICORR, INT2( 7), INT2(0) )
              CALL SBIT ( ICORR, INT2( 8), INT2(0) )
              CALL SBIT ( ICORR, INT2( 9), INT2(0) )
              CALL SBIT ( ICORR, INT2(10), INT2(1) )
              CALL SBIT ( ICORR, INT2(11), INT2(0) )
              CALL SBIT ( ICORR, INT2(12), INT2(0) )
         END IF
!
! ------ If status "phase ambiguity is not resolved" is set we don't transfer
! ------ this status to scratch file. Instead of it we set status
! ------ "manual suppression" for such an observation
!
         IF ( KBIT ( OBSBAS(J1)%SUPSTAT(1), XAMB__SPS ) ) THEN
              IF ( SUPMET == SUPMET__META ) THEN
                   OBSBAS(J1)%USER_SUP = IBSET ( OBSBAS(J1)%USER_SUP, &
     &                                           INT4(P_PXS__DTP) )
                   OBSBAS(J1)%USER_SUP = IBSET ( OBSBAS(J1)%USER_SUP, &
     &                                           INT4(PX_GS__DTP) )
                   OBSBAS(J1)%USER_SUP = IBSET ( OBSBAS(J1)%USER_SUP, &
     &                                           INT4(PX_GX__DTP) )
                   OBSBAS(J1)%USER_SUP = IBSET ( OBSBAS(J1)%USER_SUP, &
     &                                           INT4(PX_GXS__DTP) )
                   OBSBAS(J1)%AUTO_SUP = IBCLR ( OBSBAS(J1)%AUTO_SUP, &
     &                                           INT4(XAMB__SPS) )
                ELSE 
                   CALL SUPR_OBS ( P_PXS__DTP, OBSBAS(J1)%SUPSTAT(1), &
     &                             OBSBAS(J1)%UACSUP )
                   CALL SBIT ( OBSBAS(J1)%SUPSTAT(1), XAMB__SPS, INT2(0) )
              END IF
         END IF
!
         IF ( KBIT ( OBSBAS(J1)%SUPSTAT(1), SAMB__SPS ) ) THEN
              IF ( SUPMET == SUPMET__META ) THEN
                   OBSBAS(J1)%USER_SUP = IBSET ( OBSBAS(J1)%USER_SUP, &
     &                                           INT4(P_PXS__DTP) )
                   OBSBAS(J1)%USER_SUP = IBSET ( OBSBAS(J1)%USER_SUP, &
     &                                           INT4(PX_GS__DTP) )
                   OBSBAS(J1)%USER_SUP = IBSET ( OBSBAS(J1)%USER_SUP, &
     &                                           INT4(PX_GX__DTP) )
                   OBSBAS(J1)%USER_SUP = IBSET ( OBSBAS(J1)%USER_SUP, &
     &                                           INT4(PX_GXS__DTP) )
                   OBSBAS(J1)%AUTO_SUP = IBCLR ( OBSBAS(J1)%AUTO_SUP, &
     &                                           INT4(SAMB__SPS) )
                 ELSE 
                   CALL SUPR_OBS ( P_PXS__DTP, OBSBAS(J1)%SUPSTAT(1), &
     &                             OBSBAS(J1)%UACSUP )
                   CALL SBIT ( OBSBAS(J1)%SUPSTAT(1), SAMB__SPS, INT2(0) )
              END IF
         END IF
!
         SUPSTAT(1) = OBSBAS(J1)%SUPSTAT(1)
         SUPSTAT(2) = OBSBAS(J1)%SUPSTAT(2)
         AUTO_SUP   = OBSBAS(J1)%AUTO_SUP
         USER_SUP   = OBSBAS(J1)%USER_SUP
         USER_REC   = OBSBAS(J1)%USER_REC
!
         IF ( SUPMET .NE. SUPMET__META ) THEN
              IF ( SUPR_INQ ( SUPSTAT, UACSUP, URPH__SPS ) ) IUNWP = 98
         END IF
         IF ( F_SUP ) THEN
!
! ----------- Putting suppression status and used action for suppression from
! ----------- OBSBAS data structure to oborg area
!
              IDATYP_OLD = IDATYP
              IDATYP     = P_PXS__DTP
!
              SUPSTAT(1) = OBSBAS(J1)%SUPSTAT(1)
              SUPSTAT(2) = OBSBAS(J1)%SUPSTAT(2)
              UACSUP     = OBSBAS(J1)%UACSUP
!
              IF ( SUPMET .NE. SUPMET__META ) THEN
!
! ---------------- Resetting IUNW, IUNWP flags for backward compatibility with
! ---------------- PRE-APR98 versions of SOLVE
!
                   CALL SUPSTAT_UNW ( SUPSTAT, UACSUP, IUNW, IUNWP )
              END IF
              IDATYP = IDATYP_OLD
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
! ----------- Writing J1-th record in scratch file if the observation has not
! ----------- been deselected since
!
! ----------- 1) Observation made below cut off limit  OR
! ----------- 2) Observation at deselected baseline    OR
! ----------- 3) Observation of deselected source
!
              CALL USE_OBSFIL ( IOBSFIL, J1, 'W' )
         END IF
 410  CONTINUE
!
      CALL ACS_OBSFIL ( 'C' )
!
! --- Update of ionospere flags in NAMFILE to allow to appply phase delay
! --- ionosphere correction
!
      CALL OPENNAMFIL()
      NCB = 0
      DO 420 J2=1,NUMSTA
         IF ( J2 .EQ. 1 ) IP_I2 = INT2(1)
         IF ( J2 .GT. 1 ) IP_I2 = INT2(0)
!
! ------ Reading CALS card for the J2-th station
!
         CALL GETCARD ( INT2(1), 'CALS', IP_I2, JBUF(J2), KERR )
         IF ( INT4(KERR) .EQ. 1 ) THEN
              GOTO 820
           ELSE IF ( INT4(KERR) .NE. 0 ) THEN
              WRITE ( 6, * ) ' j2=',j2,' kerr=',kerr
              CALL ERR_LOG ( 7511, IUER, 'PAMB_SAVE', 'Error in reading '// &
     &                       'CALS namfile-card' )
              RETURN
           ELSE
              NCB = NCB + 1
         END IF
!
! ------ Parsing its content
!
         READ ( JBUF(J2), FMT='(A4, 1X, A8, 1X, 3I7, 35X)', IOSTAT = IO ) &
     &          LCARD, STR(1:8), ION_STS, ICAL_AV, ICAL_AP
         IF ( IO .NE. 0 ) THEN
              CALL ERR_LOG ( 7512, IUER, 'PAMB_SAVE', 'Error in deciphering '// &
     &                      'CALS card' )
              RETURN
         END IF
!
! ------ Setting bits of availability of phase delay ionosphere correction for
! ------ that baseline
!
         CALL SBIT ( ION_STS, INT2(1), INT2(1) )
         CALL SBIT ( ION_STS, INT2(2), INT2(1) )
         WRITE ( JBUF(J2), FMT='(A4, 1X, A8, 1X, 3I7, 35X)', IOSTAT = IO ) &
     &           LCARD, STR(1:8), ION_STS, ICAL_AV, ICAL_AP
 420  CONTINUE
 820  CONTINUE
!
! --- Writing updated cards back to NAMFIL
!
      DO 430 J3=1,NCB
         IF ( J3 .EQ. 1 ) IP_I2 = INT2(1)
         IF ( J3 .GT. 1 ) IP_I2 = INT2(0)
         CALL PUTCARD ( INT2(1), 'CALS', IP_I2, JBUF(J3), KERR )
         IF ( INT4(KERR) .NE. 0 ) THEN
              WRITE ( 6, * ) ' j3=',j3,' kerr=',kerr
              CALL ERR_LOG ( 7513, IUER, 'PAMB_SAVE', 'Error in writing '// &
     &            'CALS namfile-card' )
              RETURN
         END IF
 430  CONTINUE
      CALL CLOSENAMFIL()
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PAMB_SAVE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE IONO_OBSER ( OBSBAS, PH_IONO_X, PH_IONO_S, PH_IONO_X_ERR, &
     &                        PH_IONO_S_ERR )
! ************************************************************************
! *                                                                      *
! *     Routine  IONO_OBSER calcualtes ionosphere content, ionosphere    *
! *   phase delay correction and its formal error.                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    OBSBAS ( RECORD    ) -- data structures which keeps baseline      *
! *                            dependent information about the           *
! *                            observation.                              *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * PH_IONO_X ( REAL*8    ) -- Phase delay ionosphere correction for the *
! *                            X band. It should be added to X-band      *
! *                            phase delay observable to produce phase   *
! *                            delay ionosphere free linear combination. *
! * PH_IONO_S ( REAL*8    ) -- Phase delay ionosphere correction for the *
! *                            S band. It should be added to S-band      *
! *                            phase delay observable to produce phase   *
! *                            delay ionosphere free linear combination. *
! * PH_IONO_X_ERR ( REAL*8 ) - Formal error of phase delay ionosphere    *
! *                            correction for the X band.                *
! * PH_IONO_S_ERR ( REAL*8 ) - Formal error of phase delay ionosphere    *
! *                            correction for the S band.                *
! *                                                                      *
! *  ###  20-MAR-1998  IONO_OBSER   v2.2 (c)  L. Petrov 02-MAR-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'obser.i'
      INCLUDE   'pamb.i'
      INTEGER*4  IUER
      TYPE ( BAS_O__STRU ) ::  OBSBAS
      REAL*8     PH_IONO_X, PH_IONO_S, PH_IONO_X_ERR, PH_IONO_S_ERR, &
     &           Q_PH, Q_PH_ERR
!
! --- Calculate phase delay ionosphere contribution and its error
!
      IF ( OBSBAS%FREQ_IONO_PH                                 .GT. MIN__FRQ*1.D6 .AND. &
     &     OBSBAS%FREQ_IONO_PH_OPP                             .GT. MIN__FRQ*1.D6 .AND. &
     &     DABS(OBSBAS%FREQ_IONO_PH_OPP - OBSBAS%FREQ_IONO_PH) .GT. MIN__FRQ*1.D6       ) THEN
           Q_PH = ( OBSBAS%TAUPH_OBS - OBSBAS%TAUPH_OBS_OPP )* &
     &              OBSBAS%FREQ_IONO_PH**2 * OBSBAS%FREQ_IONO_PH_OPP**2/ &
     &            ( OBSBAS%FREQ_IONO_PH**2 - OBSBAS%FREQ_IONO_PH_OPP**2 )
!
           Q_PH_ERR = DSQRT ( OBSBAS%TAUPH_ERR**2 + OBSBAS%TAUPH_ERR_OPP**2 )* &
     &                     OBSBAS%FREQ_IONO_PH**2 * OBSBAS%FREQ_IONO_PH_OPP**2/ &
     &                   ( OBSBAS%FREQ_IONO_PH**2 - OBSBAS%FREQ_IONO_PH_OPP**2 )
!
! -------- Calculate phase delay ionosphere correction.
! -------- NB: it has negative sign!
!
           PH_IONO_X = -Q_PH/OBSBAS%FREQ_IONO_PH**2
           PH_IONO_S = -Q_PH/OBSBAS%FREQ_IONO_PH_OPP**2
!
! -------- Calculate formal error of phase delay ionosphere correction
!
           PH_IONO_X_ERR = Q_PH_ERR/OBSBAS%FREQ_IONO_PH**2
           PH_IONO_S_ERR = Q_PH_ERR/OBSBAS%FREQ_IONO_PH_OPP**2
        ELSE
          PH_IONO_X     = 0.0D0
          PH_IONO_S     = 0.0D0
          PH_IONO_X_ERR = 0.0D0
          PH_IONO_S_ERR = 0.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  IONO_OBSER  #!#
