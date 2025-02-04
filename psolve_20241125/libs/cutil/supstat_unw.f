      SUBROUTINE SUPSTAT_UNW ( SUPSTAT, UACSUP, IUNW, IUNWP )
! ************************************************************************
! *                                                                      *
! *   Routine  UNW_SUPSTAT  sets downweight flags IUNW or IUNWP in       *
! *   according with suppression status and user action of supprssion in *
! *   order to provide backward compartibility with PRE-APR98 scheme     *
! *   of keeping information about suppression (downweight) status of    *
! *   the observation.                                                   *
! *                                                                      *
! *  ###  01-MAY-98   UNW_SUPSTAT  v1.1  (c)  L. Petrov  21-OCT-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'socom.i'
      INTEGER*2  IUNW, IUNWP, SUPSTAT(2), UACSUP
      LOGICAL*4  DATYP_INQ, SUPR_INQ
!
      IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! -------- Phase delay solution type
!
           IF ( SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS ) ) THEN
                IUNWP = 0
              ELSE
                IUNWP = 1
                IF ( SUPR_INQ ( SUPSTAT, UACSUP, CBAD__SPS ) .OR. &
     &               SUPR_INQ ( SUPSTAT, UACSUP, UNRC__SPS )      ) THEN
!
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, BQCX__SPS ) ) IUNWP =  2
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, BQCS__SPS ) ) IUNWP =  8
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, NOFX__SPS ) ) IUNWP =  2
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, NOFS__SPS ) ) IUNWP =  8
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, BWVR__SPS ) ) IUNWP = 12
                END IF
           END IF
         ELSE
!
! -------- Group delay (or delay rate) solution type
!
           IF ( SUPR_INQ ( SUPSTAT, UACSUP, USED__SPS ) ) THEN
                IUNW = 0
              ELSE
                IUNW = 1
                IF ( SUPR_INQ ( SUPSTAT, UACSUP, CBAD__SPS ) .OR. &
     &               SUPR_INQ ( SUPSTAT, UACSUP, UNRC__SPS )      ) THEN
!
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, BQCX__SPS ) ) IUNW =  2
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, BQCS__SPS ) ) IUNW =  8
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, NOFX__SPS ) ) IUNW =  2
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, NOFS__SPS ) ) IUNW =  8
                     IF ( SUPR_INQ ( SUPSTAT, UACSUP, BWVR__SPS ) ) IUNW = 12
                END IF
           END IF
      END IF
!
! --- Set exotic code: "Unrecoverable for phase delay solution type since
! --- it has wrong phase delay ambiguity spacing"
!
      IF ( SUPR_INQ ( SUPSTAT, UACSUP, URPH__SPS ) ) IUNWP = 98
!
      RETURN
      END  !#!  SUPSTAT_UNW  #!#
