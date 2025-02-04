      SUBROUTINE SUPUSE_SET ( SUPSTAT )
! ************************************************************************
! *                                                                      *
! *   Routine  SUPUSE_SET sets usage status code in according with       *
! *   supprssion status bits (SUPSTAT) and suppression method (variablbe *
! *   SUPMET from socom-block). Usage bits codes are set in SUPSTAT bit  *
! *   field.                                                             *
! *                                                                      *
! *  ###  29-APR-98   SUPUSE_SET   v2.0  (c)  L. Petrov 11-JUN-2010 ###  *
! *       04-OCT 02   Last two arguments added to the call to 'ferr'     *
! *       2010.06.11  Updated ti unclude LSNR_SPS.                       *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'socom.i'
      INTEGER*2  SUPSTAT(2)
!
      INTEGER*2  M__ASP, I_UNRC, I_CBAD, N$N, J1, J2
      INTEGER*2  PRE98_PHUNRC,   PRE98_PHCBAD,   PRE98_GRUNRC,   PRE98_GRCBAD, &
     &           PRE98_PHUNRC_N, PRE98_PHCBAD_N, PRE98_GRUNRC_N, PRE98_GRCBAD_N, &
     &           PRE91_PHUNRC,   PRE91_PHCBAD,   PRE91_GRUNRC,   PRE91_GRCBAD, &
     &           PRE91_PHUNRC_N, PRE91_PHCBAD_N, PRE91_GRUNRC_N, PRE91_GRCBAD_N, &
     &           COMB1_G1UNRC,   COMB1_G1CBAD,   COMB1_P1UNRC,   COMB1_P1CBAD, &
     &           COMB1_G1UNRC_N, COMB1_G1CBAD_N, COMB1_P1UNRC_N, COMB1_P1CBAD_N, &
     &           COMB1_G2UNRC,   COMB1_G2CBAD,   COMB1_P2UNRC,   COMB1_P2CBAD, &
     &           COMB1_G2UNRC_N, COMB1_G2CBAD_N, COMB1_P2UNRC_N, COMB1_P2CBAD_N, &
     &           SNGBA_GRUNRC,   SNGBA_GRCBAD,   SNGBA_PHUNRC,   SNGBA_PHCBAD, &
     &           SNGBA_GRUNRC_N, SNGBA_GRCBAD_N, SNGBA_PHUNRC_N, SNGBA_PHCBAD_N
      PARAMETER  ( M__ASP = 20 )
      PARAMETER  ( PRE98_PHUNRC =  1, PRE98_PHUNRC_N =  3 )
      PARAMETER  ( PRE98_PHCBAD =  2, PRE98_PHCBAD_N = 10 )
      PARAMETER  ( PRE98_GRUNRC =  3, PRE98_GRUNRC_N =  2 )
      PARAMETER  ( PRE98_GRCBAD =  4, PRE98_GRCBAD_N = 10 )
      PARAMETER  ( PRE91_PHUNRC =  5, PRE91_PHUNRC_N =  0 )
      PARAMETER  ( PRE91_PHCBAD =  6, PRE91_PHCBAD_N = 10 )
      PARAMETER  ( PRE91_GRUNRC =  7, PRE91_GRUNRC_N =  0 )
      PARAMETER  ( PRE91_GRCBAD =  8, PRE91_GRCBAD_N = 10 )
      PARAMETER  ( COMB1_G1UNRC =  9, COMB1_G1UNRC_N =  3 )
      PARAMETER  ( COMB1_G1CBAD = 10, COMB1_G1CBAD_N =  8 )
      PARAMETER  ( COMB1_P1UNRC = 11, COMB1_P1UNRC_N =  4 )
      PARAMETER  ( COMB1_P1CBAD = 12, COMB1_P1CBAD_N = 10 )
      PARAMETER  ( COMB1_G2UNRC = 13, COMB1_G2UNRC_N =  3 )
      PARAMETER  ( COMB1_G2CBAD = 14, COMB1_G2CBAD_N =  6 )
      PARAMETER  ( COMB1_P2UNRC = 15, COMB1_P2UNRC_N =  4 )
      PARAMETER  ( COMB1_P2CBAD = 16, COMB1_P2CBAD_N =  8 )
      PARAMETER  ( SNGBA_GRUNRC = 17, SNGBA_GRUNRC_N =  2 )
      PARAMETER  ( SNGBA_GRCBAD = 18, SNGBA_GRCBAD_N =  4 )
      PARAMETER  ( SNGBA_PHUNRC = 19, SNGBA_PHUNRC_N =  3 )
      PARAMETER  ( SNGBA_PHCBAD = 20, SNGBA_PHCBAD_N =  5 )
      INTEGER*2  SUP_ARR(MAXC__SPS,M__ASP), SUP_NUM(MAXC__SPS)
!
! --- Codes specifying PRE98 suppression style
!
      DATA    SUP_NUM(PRE98_PHUNRC)           / PRE98_PHUNRC_N /
      DATA  ( SUP_ARR(N$N,PRE98_PHUNRC), N$N=1, PRE98_PHUNRC_N) &
     &      / &
     &        NOFX__SPS, &
     &        LSNR__SPS, &
     &        WPAS__SPS &
     &      /
      DATA    SUP_NUM(PRE98_PHCBAD)           / PRE98_PHCBAD_N /
      DATA  ( SUP_ARR(N$N,PRE98_PHCBAD), N$N=1, PRE98_PHCBAD_N) &
     &      / &
     &        BQCX__SPS, &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BWVR__SPS, &
     &        BPRN__SPS, &
     &        PION__SPS, &
     &        PIO1__SPS, &
     &        PIO2__SPS, &
     &        PIO3__SPS  &
     &      /
      DATA    SUP_NUM(PRE98_GRUNRC)           / PRE98_GRUNRC_N /
      DATA  ( SUP_ARR(N$N,PRE98_GRUNRC), N$N=1, PRE98_GRUNRC_N) &
     &      / &
     &        NOFX__SPS, &
     &        LSNR__SPS  &
     &      /
      DATA SUP_NUM(PRE98_GRCBAD)              / PRE98_GRCBAD_N /
      DATA  ( SUP_ARR(N$N,PRE98_GRCBAD), N$N=1, PRE98_GRCBAD_N) &
     &      / &
     &        BQCX__SPS, &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BWVR__SPS, &
     &        GION__SPS, &
     &        GIO1__SPS, &
     &        GIO2__SPS, &
     &        GIO3__SPS, &
     &        GIO4__SPS &
     &      /
!
! --- Codes specifying PRE91 suppression style
!
      DATA    SUP_NUM(PRE91_PHUNRC)           / PRE91_PHUNRC_N /
!
      DATA    SUP_NUM(PRE91_PHCBAD)           / PRE91_PHCBAD_N /
      DATA  ( SUP_ARR(N$N,PRE91_PHCBAD), N$N=1, PRE91_PHCBAD_N) &
     &      / &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BWVR__SPS, &
     &        BPRN__SPS, &
     &        PION__SPS, &
     &        PIO1__SPS, &
     &        PIO2__SPS, &
     &        PIO3__SPS, &
     &        IUNW__SPS &
     &      /
      DATA    SUP_NUM(PRE91_GRUNRC)           / PRE91_GRUNRC_N /
      DATA SUP_NUM(PRE91_GRCBAD)              / PRE91_GRCBAD_N /
      DATA  ( SUP_ARR(N$N,PRE91_GRCBAD), N$N=1, PRE91_GRCBAD_N) &
     &      / &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BWVR__SPS, &
     &        GION__SPS, &
     &        GIO1__SPS, &
     &        GIO2__SPS, &
     &        GIO3__SPS, &
     &        GIO4__SPS, &
     &        IUNW__SPS &
     &      /
!
! --- Codes specifying COMB1 suppression style for solution types with
! --- computed ionosphere calibration
!
      DATA    SUP_NUM(COMB1_G1UNRC)           / COMB1_G1UNRC_N /
      DATA  ( SUP_ARR(N$N,COMB1_G1UNRC), N$N=1, COMB1_G1UNRC_N) &
     &      / &
     &        NOFX__SPS, &
     &        NOFS__SPS, &
     &        LSNR__SPS  &
     &      /
      DATA    SUP_NUM(COMB1_G1CBAD)           / COMB1_G1CBAD_N /
      DATA  ( SUP_ARR(N$N,COMB1_G1CBAD), N$N=1, COMB1_G1CBAD_N) &
     &      / &
     &        BQCX__SPS, &
     &        BQCS__SPS, &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BWVR__SPS, &
     &        GION__SPS, &
     &        GIO2__SPS &
     &      /
      DATA    SUP_NUM(COMB1_P1UNRC)           / COMB1_P1UNRC_N /
      DATA  ( SUP_ARR(N$N,COMB1_P1UNRC), N$N=1, COMB1_P1UNRC_N) &
     &      / &
     &        NOFX__SPS, &
     &        NOFS__SPS, &
     &        LSNR__SPS, &
     &        WPAS__SPS  &
     &      /
      DATA SUP_NUM(COMB1_P1CBAD)              / COMB1_P1CBAD_N /
      DATA  ( SUP_ARR(N$N,COMB1_P1CBAD), N$N=1, COMB1_P1CBAD_N) &
     &      / &
     &        BQCX__SPS, &
     &        BQCS__SPS, &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BWVR__SPS, &
     &        PION__SPS, &
     &        PIO2__SPS, &
     &        XAMB__SPS, &
     &        SAMB__SPS &
     &      /
!
! --- Codes specifying COMB1 suppression style for solution types without
! --- computed ionosphere calibration (ionosphere calibration is calculated
! --- on the fly)
!
      DATA    SUP_NUM(COMB1_G2UNRC)           / COMB1_G2UNRC_N /
      DATA  ( SUP_ARR(N$N,COMB1_G2UNRC), N$N=1, COMB1_G2UNRC_N) &
     &      / &
     &        NOFX__SPS, &
     &        NOFS__SPS, &
     &        LSNR__SPS  &
     &      /
      DATA    SUP_NUM(COMB1_G2CBAD)           / COMB1_G2CBAD_N /
      DATA  ( SUP_ARR(N$N,COMB1_G2CBAD), N$N=1, COMB1_G2CBAD_N) &
     &      / &
     &        BQCX__SPS, &
     &        BQCS__SPS, &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BWVR__SPS &
     &      /
      DATA    SUP_NUM(COMB1_P2UNRC)           / COMB1_P2UNRC_N /
      DATA  ( SUP_ARR(N$N,COMB1_P2UNRC), N$N=1, COMB1_P2UNRC_N) &
     &      / &
     &        NOFX__SPS, &
     &        NOFS__SPS, &
     &        LSNR__SPS, &
     &        WPAS__SPS  &
     &      /
      DATA SUP_NUM(COMB1_P2CBAD)              / COMB1_P2CBAD_N /
      DATA  ( SUP_ARR(N$N,COMB1_P2CBAD), N$N=1, COMB1_P2CBAD_N) &
     &      / &
     &        BQCX__SPS, &
     &        BQCS__SPS, &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BWVR__SPS, &
     &        XAMB__SPS, &
     &        SAMB__SPS &
     &      /
!
! --- Codes specifying SNGBA suppression style
!
      DATA    SUP_NUM(    SNGBA_GRUNRC)       / SNGBA_GRUNRC_N /
      DATA  ( SUP_ARR(N$N,SNGBA_GRUNRC), N$N=1, SNGBA_GRUNRC_N) &
     &      / &
     &        NOFX__SPS, &
     &        LSNR__SPS  &
     &      /
      DATA    SUP_NUM(    SNGBA_GRCBAD)       / SNGBA_GRCBAD_N /
      DATA  ( SUP_ARR(N$N,SNGBA_GRCBAD), N$N=1, SNGBA_GRCBAD_N) &
     &      / &
     &        BQCX__SPS, &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS &
     &      /
!
      DATA    SUP_NUM(    SNGBA_PHUNRC)       / SNGBA_PHUNRC_N /
      DATA  ( SUP_ARR(N$N,SNGBA_PHUNRC), N$N=1, SNGBA_PHUNRC_N) &
     &      / &
     &        NOFX__SPS, &
     &        LSNR__SPS, &
     &        WPAS__SPS  &
     &      /
      DATA    SUP_NUM(    SNGBA_PHCBAD)       / SNGBA_PHCBAD_N /
      DATA  ( SUP_ARR(N$N,SNGBA_PHCBAD), N$N=1, SNGBA_PHCBAD_N) &
     &      / &
     &        BQCX__SPS, &
     &        CUEL__SPS, &
     &        DSBS__SPS, &
     &        DSSO__SPS, &
     &        BPRN__SPS &
     &      /
!
      INTEGER*4  IP, IARG(2)
      LOGICAL*4  DATYP_INQ
      LOGICAL*2  KBIT
!
      IF ( .NOT. KBIT ( SUPSTAT, SET1__SPS ) ) THEN
           WRITE ( 6, 110 )  SUPSTAT
 110       FORMAT ( 'SUPSTAT = ',B32,' (binary)' )
           CALL FERR ( INT2(420), &
     &         'SUPUSE_SET: SET1__SPS in SUPSTAT bit is not '// &
     &         'been set on: it means that suppression status was '// &
     &         'not defined', INT2(0), INT2(0) )
      END IF
!
! --- Initialization
!
      CALL SBIT ( SUPSTAT, UNRC__SPS, INT2(0) )
      CALL SBIT ( SUPSTAT, CBAD__SPS, INT2(0) )
      CALL SBIT ( SUPSTAT, GOOD__SPS, INT2(0) )
      CALL SBIT ( SUPSTAT, SET2__SPS, INT2(0) )
!
! --- Setting final suppression flags in according with suppression style
!
      IF ( SUPMET .EQ. SUPMET__PRE98 ) THEN
!
! -------- Old method used in SOLVE before March 1998
!
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Phase delay observables
!
                I_UNRC = PRE98_PHUNRC
                I_CBAD = PRE98_PHCBAD
              ELSE
!
! ------------- Group delay or group delay rate observables
!
                I_UNRC = PRE98_GRUNRC
                I_CBAD = PRE98_GRCBAD
           END IF
         ELSE IF ( SUPMET .EQ. SUPMET__PRE91 ) THEN
!
! -------- The oldest method used in SOLVE before 1991
!
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Phase delay observables
!
                I_UNRC = PRE91_PHUNRC
                I_CBAD = PRE91_PHCBAD
              ELSE
!
! ------------- Group delay or group delay rate observables
!
                I_UNRC = PRE91_GRUNRC
                I_CBAD = PRE91_GRCBAD
           END IF
         ELSE IF ( SUPMET .EQ. SUPMET__COMB1 ) THEN
!
! -------- COMB1 suppression stype
!
           IF ( DATYP_INQ ( IDATYP, IOCAL__DTP ) ) THEN
!
! ------------- Solution type with calcualted ionosphere calibration
!
                IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------------ Phase delay observables
!
                     I_UNRC = COMB1_P1UNRC
                     I_CBAD = COMB1_P1CBAD
                   ELSE
!
! ------------------ Group delay or group delay rate observables
!
                     I_UNRC = COMB1_G1UNRC
                     I_CBAD = COMB1_G1CBAD
                END IF
             ELSE
!
! ------------- Solution type without calcualted ionosphere calibration
! ------------- (it is assumed that ionpsphere calibration is calculcated
! -------------  on the fly)
!
                IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------------ Phase delay observables
!
                     I_UNRC = COMB1_P2UNRC
                     I_CBAD = COMB1_P2CBAD
                   ELSE
!
! ------------------ Group delay or group delay rate observables
!
                     I_UNRC = COMB1_G2UNRC
                     I_CBAD = COMB1_G2CBAD
                END IF
           END IF
         ELSE IF ( SUPMET .EQ. SUPMET__SNGBA ) THEN
!
! -------- Single band type of solution. Information about the opposite band
! -------- and ionosphere status is ignored
!
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Phase delay observables
!
                I_UNRC = SNGBA_PHUNRC
                I_CBAD = SNGBA_PHCBAD
              ELSE
!
! ------------- Group delay or group delay rate observables
!
                I_UNRC = SNGBA_GRUNRC
                I_CBAD = SNGBA_GRCBAD
           END IF
         ELSE
           WRITE ( 6, * ) ' SUPMET = ',SUPMET
           CALL ERR_LOG ( 1422, -2, 'SUPUSE_SET' , 'Wrong value for SUPMET' )
           IP = 3
           IP = IARG(IP) 
      ENDIF
!
! --- Scanning bits of unrecoverable status of observation
!
      IF ( SUP_NUM(I_UNRC) .GT. 0 ) THEN
           DO 410 J1=1,SUP_NUM(I_UNRC)
              IF ( KBIT ( SUPSTAT, SUP_ARR(J1,I_UNRC) ) ) THEN
                   CALL SBIT ( SUPSTAT, UNRC__SPS, INT2(1) )
              END IF
 410       CONTINUE
      END IF
!
! --- Scanning bits of conditionally bad status of observation
!
      IF ( SUP_NUM(I_CBAD) .GT. 0 ) THEN
           DO 420 J2=1,SUP_NUM(I_CBAD)
              IF ( KBIT ( SUPSTAT, SUP_ARR(J2,I_CBAD) ) ) THEN
                   CALL SBIT ( SUPSTAT, CBAD__SPS, INT2(1) )
              END IF
 420       CONTINUE
      END IF
!
! --- Setting status GOOD if we may do it
!
      IF ( .NOT. KBIT ( SUPSTAT, UNRC__SPS ) .AND. &
     &     .NOT. KBIT ( SUPSTAT, CBAD__SPS )       ) THEN
!
           CALL SBIT  ( SUPSTAT, GOOD__SPS, INT2(1) )
      END IF
!
! --- Set usage status bit. Setting this bit on indicates on
! --- completion of setting usage status
!
      CALL SBIT ( SUPSTAT, SET2__SPS, INT2(1) )
!
      RETURN
      END  !#!  SUPUSE_SET  #!#
