      FUNCTION   REPA_DF_SETBOX ( DIAGI_S, REP, MODE ) 
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_SETBOX changes Y axes of the bounding box in      *
! *   such a way that                                                    *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *     MODE ( INTEGER*4 ) -- Caterory index. One of                     *
! *                        REPA__I_GOO -- Only good observations are to  *
! *                                       be shown.                      *
! *                        REPA__I_BAD -- Good and bad observations are  *
! *                                       to be shown.                   *
! *                        REPA__I_UNR -- Good, bad and unrecoverable    *
! *                                       observations are to be shown.  *
! *                                                                      *
! * ### 07-DEC-2004  REPA_DF_SETBOX  v1.0 (c) L. Petrov 09-DEC-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_SETBOX
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  MODE
      CHARACTER  STR*32
      REAL*8     ARG_RANGE, VAL_RANGE, &
     &           XMIN_GOO, XMAX_GOO, YMIN_GOO, YMAX_GOO, &
     &           XMIN_BAD, XMAX_BAD, YMIN_BAD, YMAX_BAD, &
     &           XMIN_UNR, XMAX_UNR, YMIN_UNR, YMAX_UNR
#ifdef BUG01 ! To circumvent buggy compiler HP-UX 2.4
      REAL*4   REPA_MIN4, REPA_MAX4 
#endif
      INTEGER*4  J1, J2, IND_BAS
!
! --- Determine the index of the baseline which corresponds to the current
! --- window
!
      IND_BAS = 0
      DO 410 J1=1,REP%N_BAS
         IF ( LOC(REP%DIAGI(J1)) .EQ. LOC(DIAGI_S) ) IND_BAS = J1
 410  CONTINUE 
!
      IF ( REP%PLT(IND_BAS)%N_GOO .GT. 0 ) THEN
!
! -------- There are good points. Determine the bounding box around them
!
           CALL REPA_MINMAX ( &
     &               REP%PLT(IND_BAS)%N_GOO,   REP%PLT(IND_BAS)%ARG_GOO, &
     &               REP%PLT(IND_BAS)%VAL_GOO, REP%PLT(IND_BAS)%ERR_GOO, & 
     &               XMIN_GOO, XMAX_GOO,  YMIN_GOO, YMAX_GOO   )
           REP%DIAGI(IND_BAS)%XMIN = XMIN_GOO
           REP%DIAGI(IND_BAS)%XMAX = XMAX_GOO
           REP%DIAGI(IND_BAS)%YMIN = YMIN_GOO
           REP%DIAGI(IND_BAS)%YMAX = YMAX_GOO
!
! -------- Make the bounding box for argument a little bit wider
!
         ELSE
!
! -------- No good points? Mmmm. Let's do something.
!
           REP%DIAGI(IND_BAS)%XMIN = 0.0
           REP%DIAGI(IND_BAS)%XMAX = 0.0+1.D-6
           REP%DIAGI(IND_BAS)%YMIN = 0.0
           REP%DIAGI(IND_BAS)%YMAX = 0.0+1.D-6
      END IF
!
      IF ( ( MODE .EQ. 2  .OR.  MODE .EQ. 3  .OR.  MODE .EQ. 5 ) .AND. &
     &       REP%PLT(IND_BAS)%N_BAD .GT. 0 ) THEN
!
! -------- Adjust the bounding box for conventionally bad observations
!
           CALL REPA_MINMAX ( REP%PLT(IND_BAS)%N_BAD,   REP%PLT(IND_BAS)%ARG_BAD, &
     &                        REP%PLT(IND_BAS)%VAL_BAD, REP%PLT(IND_BAS)%ERR_BAD, &
     &                        XMIN_BAD, XMAX_BAD,  YMIN_BAD, YMAX_BAD   )
#ifdef BUG01 ! To circumvent buggy compiler HP-UX 2.4
           REP%DIAGI(IND_BAS)%XMIN = REPA_MIN4 ( REP%DIAGI(IND_BAS)%XMIN, XMIN_BAD )
           REP%DIAGI(IND_BAS)%XMAX = REPA_MAX4 ( REP%DIAGI(IND_BAS)%XMAX, XMAX_BAD )
           REP%DIAGI(IND_BAS)%YMIN = REPA_MIN4 ( REP%DIAGI(IND_BAS)%YMIN, YMIN_BAD )
           REP%DIAGI(IND_BAS)%YMAX = REPA_MAX4 ( REP%DIAGI(IND_BAS)%YMAX, YMAX_BAD )
#else
           REP%DIAGI(IND_BAS)%XMIN = MIN ( REP%DIAGI(IND_BAS)%XMIN, XMIN_BAD )
           REP%DIAGI(IND_BAS)%XMAX = MAX ( REP%DIAGI(IND_BAS)%XMAX, XMAX_BAD )
!
           REP%DIAGI(IND_BAS)%YMIN = MIN ( REP%DIAGI(IND_BAS)%YMIN, YMIN_BAD )
           REP%DIAGI(IND_BAS)%YMAX = MAX ( REP%DIAGI(IND_BAS)%YMAX, YMAX_BAD )
#endif
      END IF
!
      IF ( MODE .EQ. 3  .AND.  REP%PLT(IND_BAS)%N_UNR .GT. 0 ) THEN
!
! -------- Adjust the bounding box for uncoverable observations
!
           CALL REPA_MINMAX ( REP%PLT(IND_BAS)%N_UNR,   REP%PLT(IND_BAS)%ARG_UNR, &
     &                        REP%PLT(IND_BAS)%VAL_UNR, REP%PLT(IND_BAS)%ERR_UNR, &
     &                        XMIN_UNR, XMAX_UNR,  YMIN_UNR, YMAX_UNR   )
#ifdef BUG01 ! To circumvent buggy compiler HP-UX 2.4
           REP%DIAGI(IND_BAS)%XMIN = REPA_MIN4 ( REP%DIAGI(IND_BAS)%XMIN, XMIN_UNR )
           REP%DIAGI(IND_BAS)%XMAX = REPA_MAX4 ( REP%DIAGI(IND_BAS)%XMAX, XMAX_UNR )
           REP%DIAGI(IND_BAS)%YMIN = REPA_MIN4 ( REP%DIAGI(IND_BAS)%YMIN, YMIN_UNR )
           REP%DIAGI(IND_BAS)%YMAX = REPA_MAX4 ( REP%DIAGI(IND_BAS)%YMAX, YMAX_UNR )
#else
           REP%DIAGI(IND_BAS)%XMIN = MIN ( REP%DIAGI(IND_BAS)%XMIN, XMIN_UNR )
           REP%DIAGI(IND_BAS)%XMAX = MAX ( REP%DIAGI(IND_BAS)%XMAX, XMAX_UNR )
!
           REP%DIAGI(IND_BAS)%YMIN = MIN ( REP%DIAGI(IND_BAS)%YMIN, YMIN_UNR )
           REP%DIAGI(IND_BAS)%YMAX = MAX ( REP%DIAGI(IND_BAS)%YMAX, YMAX_UNR )
#endif
      END IF
!
      IF ( MODE .EQ. 4  .OR.  MODE .EQ. 5 ) THEN
           REP%DIAGI(IND_BAS)%YMIN = -MAX ( ABS(REP%DIAGI(IND_BAS)%YMIN), &
     &                                      ABS(REP%DIAGI(IND_BAS)%YMAX) )
           REP%DIAGI(IND_BAS)%YMAX =  MAX ( ABS(REP%DIAGI(IND_BAS)%YMIN), &
     &                                      ABS(REP%DIAGI(IND_BAS)%YMAX) )
      END IF 
!
! --- Determine the range along the axis X. Make the bounding box a little be
! --- wider by increasing the range by REPA__OVR_ARG
!
      ARG_RANGE = REP%DIAGI(IND_BAS)%XMAX - REP%DIAGI(IND_BAS)%XMIN 
      REP%DIAGI(IND_BAS)%XMIN = REP%DIAGI(IND_BAS)%XMIN - REPA__OVR_ARG*ARG_RANGE
      REP%DIAGI(IND_BAS)%XMAX = REP%DIAGI(IND_BAS)%XMAX + REPA__OVR_ARG*ARG_RANGE
!
! --- Determine the range along the axis Y. Make the bounding box a little be
! --- wider by increasing the range by REPA__OVR_ARG
!
      VAL_RANGE = REP%DIAGI(IND_BAS)%YMAX - REP%DIAGI(IND_BAS)%YMIN 
      REP%DIAGI(IND_BAS)%YMIN = REP%DIAGI(IND_BAS)%YMIN - REPA__OVR_ARG*VAL_RANGE
      REP%DIAGI(IND_BAS)%YMAX = REP%DIAGI(IND_BAS)%YMAX + REPA__OVR_ARG*VAL_RANGE
!
! --- Update the plot:
! --- Firstly erasing graphic window and printing axis box and the title
!
      DIAGI_S%MESS_BOT = DIAGI_S%MESS_BOT_SAVED
      CALL DIAGI_SET_FRAME ( DIAGI_S, DIAGI_S%MESS_BOT )
!
! --- ... then drawing plots of the functions, colour by colour.
! --- The next colour overlap the previous one.
!
      DO 420 J2=1,DIAGI_S%NCLR
         CALL DIAGI_DRAW ( DIAGI_S, J2, 0, &
     &                     DIAGI_S%NPOI(J2), %VAL(DIAGI_S%ADR_X4(J2)), &
     &                %VAL(DIAGI_S%ADR_Y4(J2)), %VAL(DIAGI_S%ADR_E4(J2)), &
     &                %VAL(DIAGI_S%ADR_X8(J2)), %VAL(DIAGI_S%ADR_Y8(J2)) )
 420  CONTINUE
!
      REP%PLT(IND_BAS)%BOU_IND = MODE
      REPA_DF_SETBOX = DIAGI__CONT   
!
      RETURN
      END  FUNCTION  REPA_DF_SETBOX
#ifdef BUG01 ! To circumvent buggy compiler HP-UX 2.4
!
! ------------------------------------------------------------------------
!
      FUNCTION REPA_MIN4 ( VAL1, VAL2 )
      REAL*4   REPA_MIN4, VAL1, VAL2
      IF ( VAL1 .LE. VAL2 ) THEN
           REPA_MIN4 = VAL1
         ELSE
           REPA_MIN4 = VAL2
      END IF
      RETURN
      END  
!
! ------------------------------------------------------------------------
!
      FUNCTION REPA_MAX4 ( VAL1, VAL2 )
      REAL*4   REPA_MAX4, VAL1, VAL2
      IF ( VAL1 .GE. VAL2 ) THEN
           REPA_MAX4 = VAL1
         ELSE
           REPA_MAX4 = VAL2
      END IF
      RETURN
      END  
#endif
