      FUNCTION   RECV_OBS ( IDATYP, SUPSTAT, UACSUP )
! ************************************************************************
! *                                                                      *
! *   Finction changes bits of UACSUP bit field (User Action for         *
! *   SUPression) in order to recover the observation (put it to         *
! *   solution) if it was not there. However it will be done only if     *
! *   observation is recoverable.                                        *
! *                                                                      *
! * _________________________ Input parameters _________________________ *
! *                                                                      *
! *    IDATYP ( INTEGER*2 ) -- Code of solution type.                    *
! *   SUPSTAT ( INTEGER*2 ) -- Suppression status. Array of 2 words.     *
! *                                                                      *
! * ________________________ Modified parameters _______________________ *
! *                                                                      *
! *    UACSUP ( INTEGER*2 ) -- Bit field user action for suppression.    *
! *                                                                      *
! * _________________________ Output parameters ________________________ *
! *                                                                      *
! * <RECV_OBS> ( LOGICAL*4 ) -- Success flag. .TRUE. means that          *
! *                             operation has been successfully          *
! *                             completed and observation appeared to be *
! *                             restored. .FALSE. menas it was           *
! *                             unrecoverable.                           *
! *                                                                      *
! *  ###  03-MAY-98     RECV_OBS   v1.0  (c)  L. Petrov  03-MAY-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*2  IDATYP, SUPSTAT(2), UACSUP
      LOGICAL*4  RECV_OBS, SUPR_INQ, DATYP_INQ
!
      IF ( SUPR_INQ ( SUPSTAT, UACSUP, RECO__SPS ) ) THEN
!
! -------- Observation is recoverable... Go ahead!
!
           IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! ------------- Phase delay case. Lift manual suppression.
!
                CALL SBIT ( UACSUP, PSUP__UAS, INT2(0) )
                IF ( SUPR_INQ ( SUPSTAT, UACSUP, CBAD__SPS ) ) THEN
!
! ------------------ A-a-a! Observation was conditionally bad. Then we set
! ------------------ bit "override veto"
!
                     CALL SBIT ( UACSUP, POVV__UAS, INT2(1) )
                END IF
             ELSE
!
! ------------- Group (or rate) solutiuon type. Lift mannual suppression.
!
                CALL SBIT ( UACSUP, GSUP__UAS, INT2(0) )
                IF ( SUPR_INQ ( SUPSTAT, UACSUP, CBAD__SPS ) ) THEN
!
! ------------------ Set bit "use the observation despite it is condionally
! ------------------ bad"
!
                     CALL SBIT ( UACSUP, GOVV__UAS, INT2(1) )
                END IF
           END IF
           RECV_OBS = .TRUE.
         ELSE
           RECV_OBS = .FALSE.
      END IF
      RETURN
      END  !#!  RECV_OBS #!#
