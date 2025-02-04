      SUBROUTINE SET_STABIT ( IMODE )
! ************************************************************************
! *                                                                      *
! *   Routine  SET_STABIT  sets bit fields of participation of the       *
! *   stations in the solutions. It changes variable STABIT_G and/or     *
! *   STABIT_P in socom-area. It is assumed that socom has been already  *
! *   read in memory before. SET_STABIT don't save socom area.           *
! *                                                                      *
! *   Socom are may be one of two types: arc-type, when it keeps         *
! *   parameters for the certain session and CGM-type, when it keeps     *
! *   some mixture global type of parameters and arc-type of parameters. *
! *   Stataion selection doesn't have sense for CGM-type socom, but      *
! *   variables from socom are still in use by SOLVE prgramms making     *
! *   global solution.                                                   *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   IMODE ( INTEGER*2 ) -- Switch of working mode. Accpetable values   *
! *                          are 1 or 2.                                 *
! *                                                                      *
! *                                                                      *
! *   If IMODE=1 then all bits of participation for both group delay     *
! *   and phase delay solution types will  be set up.                    *
! *                                                                      *
! *   If IMODE=2 and the current socom is of CGM-type then all bits of   *
! *   participation for both group delay and phase delay solution types  *
! *   will be set up. It is done to keep compartibility with parcn,      *
! *   parcng and other SOLVE routines which are making global solutiuon. *
! *                                                                      *
! *   If IMODE=2 and the current socom is of arc-type then               *
! *      1) solution type: phase-delay or group-delay is checked;        *
! *      2) bit fields of baslines selection for either phase delay      *
! *         solution type of group daly solution type are checked.       *
! *         All stations are scanned. If at least one baseline with      *
! *         examined station was selected in current solution type,      *
! *         then this station is marked as selected in solution (for     *
! *         the current solutiuon type. Status of the station for other  *
! *         solution type remains unchanged). Otherwise, if no baseline  *
! *         is selcted in solution with examined station, the station    *
! *         is marked as deselected from solution for the current        *
! *         solution type.                                               *
! *                                                                      *
! *  ###  01-DEC-97   SET_STABIT   v1.3  (c)  L. Petrov 07-MAR-2000 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INTEGER*2  IMODE, LSTA, J1, J2, J3, J4, J5
      LOGICAL*2  KBIT
      LOGICAL*4  DATYP_INQ
      CHARACTER  ERRSTR*128
!
      LSTA = MIN ( MAX_ARC_STA, NUMSTA )
      IF ( IMODE .EQ. 1 ) THEN
!
! -------- Mode: set all bits for all stations known to socom
! -------- for both types of solutions
!
           DO 410 J1=1,MAX_ARC_STA
              IF ( J1 .LE. LSTA ) THEN
                   CALL SBIT ( STABIT_G, J1, INT2(1) )
                   CALL SBIT ( STABIT_P, J1, INT2(1) )
                ELSE
                   CALL SBIT ( STABIT_G, J1, INT2(0) )
                   CALL SBIT ( STABIT_P, J1, INT2(0) )
              END IF
 410       CONTINUE
        ELSE IF ( IMODE .EQ. 2 ) THEN
           IF ( CGM_TYPE ) THEN
!
! ------------ Case then socom belong to CGM. Then we set on all bits for
! ------------ all stations known to socom  both types of solutions
!
               DO 420 J2=1,MAX_ARC_STA
                  IF ( J2 .LE. LSTA ) THEN
                       CALL SBIT ( STABIT_G, J2, INT2(1) )
                       CALL SBIT ( STABIT_P, J2, INT2(1) )
                     ELSE
                       CALL SBIT ( STABIT_G, J2, INT2(0) )
                       CALL SBIT ( STABIT_P, J2, INT2(0) )
                  END IF
 420           CONTINUE
             ELSE
!
! ----------- Mode: set bits in accoding with IBLSEL_x array
!
! ----------- Firstly zero out all bits (all stations become deselscted)
!
              DO 430 J3=1,MAX_ARC_STA
                 IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
                      CALL SBIT ( STABIT_P, J3, INT2(0) )
                   ELSE
                      CALL SBIT ( STABIT_G, J3, INT2(0) )
                 END IF
 430         CONTINUE
!
! ---------- Then scan all possible baselines
!
             DO 440 J4=1,LSTA-1
                DO 450 J5=J4+1,LSTA
!
! ---------------- If the baseline is seleceted -- then both stations will
! ---------------- acquire status "selected"
!
                   IF ( DATYP_INQ ( IDATYP, PHASE__DTP ) ) THEN
!
! --------------------- Phase-delay solution type
!
                        IF ( KBIT ( IBLSEL_P ( 1,J4), J5 ) .OR. &
     &                       KBIT ( IBLSEL_P ( 1,J5), J4 )      ) THEN
!
                             CALL SBIT ( STABIT_P, J4, INT2(1) )
                             CALL SBIT ( STABIT_P, J5, INT2(1) )
                        END IF
                     ELSE
!
! --------------------- Group delay solution type
!
                        IF ( KBIT ( IBLSEL_G ( 1,J4), J5 ) .OR. &
     &                       KBIT ( IBLSEL_G ( 1,J5), J4 )      ) THEN
!
                             CALL SBIT ( STABIT_G, J4, INT2(1) )
                             CALL SBIT ( STABIT_G, J5, INT2(1) )
                        END IF
                    END IF
 450            CONTINUE
 440         CONTINUE
           END IF
        ELSE
           WRITE ( UNIT=ERRSTR, FMT='("SET_STABIT: formal argument IMODE ", &
     &           "has unacceptable value: ",I7," Only 1 or 2 (INTEGER*2) ", &
     &           " are acceptable" )' ) IMODE
           CALL FERR ( INT2(9121), ERRSTR, INT2(0), INT2(0) )
      END IF
!
      RETURN
      END  !#!  SET_STABIT  #!#
