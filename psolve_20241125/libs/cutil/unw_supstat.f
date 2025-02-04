      SUBROUTINE UNW_SUPSTAT ( IUNW, IUNWP, SUPSTAT, UACSUP )
! ************************************************************************
! *                                                                      *
! *   Routine  UNW_SUPSTAT  sets suppression status and user action of   *
! *   supprssion in according with values of archaic downweight codes    *
! *   IUNW and IUNWP. The purpose of this routine is to provide          *
! *   interface from PRE-APR98 scheme of keeping information about       *
! *   suppression  (downweight) status toi POST-APR98 scheme.            *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     IUNW ( INTEGER*2 ) -- Downweight flag for group delay solution   *
! *                           used before MAY98.                         *
! *    IUNWP ( INTEGER*2 ) -- Downweight flag for phase delay solution   *
! *                           used before MAY98.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  SUPSTAT ( INTEGER*2 ) -- 32-bits field of suppression status        *
! *                           which is set automatically in according    *
! *                           with circumstances of the observation and  *
! *                           global settings (such as quality code      *
! *                           limit, applying ionosphere calibration     *
! *                           etc. )                                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   UACSUP ( INTEGER*2 ) -- 16-bits field of user action for           *
! *                           suppression.                               *
! *                                                                      *
! *  ###  01-MAY-98   UNW_SUPSTAT  v1.3  (c)  L. Petrov  03-NOV-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'socom.i'
      INTEGER*2  IUNW, IUNWP, SUPSTAT(2), UACSUP
      LOGICAL*4  DATYP_INQ, SUPR_INQ
!
! --- Initialization
!
      SUPSTAT(1) = 0
      SUPSTAT(2) = 0
      UACSUP     = 0
!
! --- Setting code: user action for suppression
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay solution type
!
! -------- Setting bits of supprssion status
!
           IF ( IUNWP .EQ.  2 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNWP .EQ.  3 )  CALL SBIT ( SUPSTAT, CUEL__SPS, INT2(1) )
           IF ( IUNWP .EQ.  4 )  CALL SBIT ( SUPSTAT, BPRN__SPS, INT2(1) )
           IF ( IUNWP .EQ.  5 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNWP .EQ.  6 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNWP .EQ.  7 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNWP .EQ.  8 )  CALL SBIT ( SUPSTAT, NOFS__SPS, INT2(1) )
           IF ( IUNWP .EQ.  9 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNWP .EQ. 11 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNWP .EQ. 12 )  CALL SBIT ( SUPSTAT, BWVR__SPS, INT2(1) )
           IF ( IUNWP .EQ. 16 )  CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(1) )
           IF ( IUNWP .EQ. 17 )  CALL SBIT ( SUPSTAT, DSSO__SPS, INT2(1) )
           IF ( IUNWP .EQ. 98 )  CALL SBIT ( SUPSTAT, WPAS__SPS, INT2(1) )
        ELSE
!
! -------- Group delay (or delay rate) solution type
!
! -------- Setting bits of supprssion status
!
           IF ( IUNW  .EQ.  2 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNW  .EQ.  3 )  CALL SBIT ( SUPSTAT, CUEL__SPS, INT2(1) )
           IF ( IUNW  .EQ.  5 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNW  .EQ.  6 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNW  .EQ.  7 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNW  .EQ.  8 )  CALL SBIT ( SUPSTAT, NOFS__SPS, INT2(1) )
           IF ( IUNW  .EQ.  9 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNW  .EQ. 11 )  CALL SBIT ( SUPSTAT, NOFX__SPS, INT2(1) )
           IF ( IUNW  .EQ. 12 )  CALL SBIT ( SUPSTAT, BWVR__SPS, INT2(1) )
           IF ( IUNW  .EQ. 16 )  CALL SBIT ( SUPSTAT, DSBS__SPS, INT2(1) )
           IF ( IUNW  .EQ. 17 )  CALL SBIT ( SUPSTAT, DSSO__SPS, INT2(1) )
      END IF
!
! --- Setting bit of user action for phase delay solution: to suppress
!
      IF ( IUNWP .EQ.  1 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ.  2 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ.  5 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ.  6 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ.  7 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ.  8 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ.  9 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ. 10 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ. 11 )  CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
      IF ( IUNWP .EQ. 98 )  CALL SBIT ( SUPSTAT, WPAS__SPS, INT2(1) )
!
! --- Setting bit of user action for group delay solution: to suppress
!
      IF ( IUNW  .EQ.  1 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
      IF ( IUNW  .EQ.  2 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
      IF ( IUNW  .EQ.  5 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
      IF ( IUNW  .EQ.  6 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
      IF ( IUNW  .EQ.  7 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
      IF ( IUNW  .EQ.  8 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
      IF ( IUNW  .EQ.  9 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
      IF ( IUNW  .EQ. 10 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
      IF ( IUNW  .EQ. 11 )  CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
!
! --- Set suppression status bit. Setting this bit on indicates on
! --- completion of setting suppression status
!
      CALL SBIT ( SUPSTAT, SET1__SPS, INT2(1) )
!
! --- Finally setting usage status bits
!
      CALL SUPUSE_SET ( SUPSTAT )
      IF ( IUNWP .EQ. 0 ) THEN
!
! -------- Kludge. It is possible situation that observation had bad
! -------- quality code but was used in solution. In that case we override
! -------- veto and recover observation
!
           IF ( SUPR_INQ ( SUPSTAT, UACSUP, BQCX__SPS ) ) THEN
!
! ------------- This trick work only if ionsphere was not unrecoverable.
! ------------- Don't ask me why. Pre-MAY98 strategy for observation
! ------------- suppression SOLVE had some caprices
!
                IF ( .NOT. SUPR_INQ ( SUPSTAT, UACSUP, PION__SPS ) .AND. &
     &               .NOT. SUPR_INQ ( SUPSTAT, UACSUP, PIO1__SPS ) .AND. &
     &               .NOT. SUPR_INQ ( SUPSTAT, UACSUP, PIO2__SPS ) .AND. &
     &               .NOT. SUPR_INQ ( SUPSTAT, UACSUP, PIO3__SPS )       ) THEN
!
                     CALL RECV_OBS  ( PHSONL__DTP, SUPSTAT, UACSUP )
                END IF
           END IF
      END IF
!
      IF ( IUNW .EQ. 0 ) THEN
!
! -------- The same for group delay (or delay rate solution type)
!
           IF ( SUPR_INQ ( SUPSTAT, UACSUP, BQCX__SPS ) ) THEN
                IF ( .NOT. SUPR_INQ ( SUPSTAT, UACSUP, GION__SPS ) .AND. &
     &               .NOT. SUPR_INQ ( SUPSTAT, UACSUP, GIO1__SPS ) .AND. &
     &               .NOT. SUPR_INQ ( SUPSTAT, UACSUP, GIO2__SPS ) .AND. &
     &               .NOT. SUPR_INQ ( SUPSTAT, UACSUP, GIO3__SPS ) .AND. &
     &               .NOT. SUPR_INQ ( SUPSTAT, UACSUP, GIO4__SPS )       ) THEN
!
                     CALL RECV_OBS  ( GRPONL__DTP, SUPSTAT, UACSUP )
                END IF
           END IF
      END IF
!
! --- And at alast set bit: "UACSUP has been initilized"
!
      CALL SBIT ( UACSUP, INIT__UAS, INT2(1) )
!
      RETURN
      END  !#!  UNW_SUPSTAT  #!#
