      SUBROUTINE SUPR_OBS ( IDATYP, SUPSTAT, UACSUP )
! ************************************************************************
! *                                                                      *
! *   Routine  SUPR_OBS  changes bits of UACSUP bit field (User Action   *
! *   for SUPression) in order to suppress the observation (bar it from  *
! *   solution) if it was there.                                         *
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
! *  ###  03-MAY-98     SUPR_OBS   v1.0  (c)  L. Petrov  03-MAY-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INTEGER*2  IDATYP, SUPSTAT(2), UACSUP
      LOGICAL*2  KBIT
      LOGICAL*4  DATYP_INQ
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay case. Set mannual suppression and clearing override veto
!
           CALL SBIT ( UACSUP, PSUP__UAS, INT2(1) )
           CALL SBIT ( UACSUP, POVV__UAS, INT2(0) )
        ELSE
!
! --------- Group (or rate) solutiuon type. Lift mannual suppression and
! --------- clearing bit "override veto"
!
            CALL SBIT ( UACSUP, GSUP__UAS, INT2(1) )
            CALL SBIT ( UACSUP, GOVV__UAS, INT2(0) )
      END IF
!
      RETURN
      END  !#!  SUPR_OBS #!#
