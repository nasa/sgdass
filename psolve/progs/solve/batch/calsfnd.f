      SUBROUTINE CALSFND(SEC,NAMCAL,NAMFCAL,ICALS,IFCALS,NAMAVL, &
     &                   NAMAPL,JCAFFL,FRSTFRM,SAVFRM)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CALSFND PROGRAM SPECIFICATION
!
! 1.1 Found a CALIBRATION section that pertains to the current station.
!     Now turn the calibrations it pertains to
!     on or off, as requested in the section.
!
! 1.2 REFERENCES:
!
! 2.  CALSFND INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 SEC,ICALS,IFCALS,NAMAVL,SAVFRM,FRSTFRM
      CHARACTER*(8) NAMCAL(ICALS), NAMFCAL(112)
!
! FRSTFRM - First frame number
! ICALS - Number of non-flyby calibrations from NAMFIL
! IFCALS - Number of flyby calibrations from NAMFIL
! NAMCAL - List of non-flyby calibrations from NAMFIL
! NAMFCAL - List of flyby calibrations from NAMFIL
! SAVFRM -  Saved frame number
! SEC - Section number
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 NAMAPL,JCAFFL(7)
!
! NAMAPL - Bit map of applied non-flyby calibrations
! JCAFFL - Bit map of applied flyby calibrations
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: dositecals
!       CALLED SUBROUTINES: getitm
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 STATE,CAL
      INTEGER*2 ITEM,FRAME,J,IB
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CALSFND PROGRAM STRUCTURE
!
!  AT THIS POINT, WE HAVE LOCATED A SET OF CALIBRATIONS WHICH CAN AND SHOULD
!  BE TURNED ON/OFF FOR THIS STATION.   THE SET IS EXPRESSED IN ONE OF
!  THE FOLLOWING WAYS:
!
!    FRAME 1             FRAME M
!
!    DEFAULT              DEFAULT
!
!   SINGLE CALIB           ON
!                          OFF
!                        DEFAULT
!
!   GROUP OF CALIBS      DEFAULT
!                     SUBSET OF GROUP TO BE TURNED ON
!                       (expressed as a series of individual calib names)
!
!  THIS SUB WILL GO TO FRAME M, TO GET THE FIRST OF ITS INSTRUCTIONS
!  IF THE INSTRUCTION IS DEFAULT, CAN JUST LEAVE.  OTHERWISE, WILL
!  GO TO FRAME 1 TO GET THE LIST OF CALIBS TO BE AFFECTED BY THIS CALL.
!  IF INSTRUCTION IS OFF OR ON, JUST TURN THE CALIBS OFF/ON AND LEAVE.
!  IF INSTRUCTION IS A NAME, DEALING WITH THE FINAL CASE, TURNING ON
!    A SUBSET OF A GROUP OF CALIBS.  TURN THE OVERALL SET, LISTED IN FRAME 1
!    OFF.  THEN GO BACK TO FRAME M AND TURN ALL THE LISTED ONES ON.
!
!     Get first instruction from frame "m"
!
      FRAME=SAVFRM
      ITEM=0
      CALL GETITM(SEC,FRAME,ITEM,STATE )
!
!     Return if
!     the user wants to leave the calibrations in frame 1 of this section
!     on or off as they are in the current superfile.
!
      IF(STATE.EQ.'DEFAULT') RETURN
!
!     Now either turning all the calibrations in the first frame off or on,
!     or turning them all off so that the subset of them listed in
!     frame 'm' can be turned on.  Whatever the situation, go to frame 1
!     and turn all those calibs off or on, as appropriate.
!
      FRAME=FRSTFRM
      ITEM=0
      CALL GETITM(SEC,FRAME,ITEM,CAL )
      IB=0
      IF(STATE.EQ.'ON') IB=1
      DO WHILE(ITEM.GE.0)
        DO J=1,ICALS
          IF(NAMCAL(J).EQ.CAL) THEN
            CALL SBIT(NAMAPL,J,IB )
            GO TO 50
          ENDIF
        ENDDO
        DO J=1,IFCALS
          IF(NAMFCAL(J).EQ.CAL) THEN
            CALL SBIT(JCAFFL(1),J,IB )
            GO TO 50
          ENDIF
        END DO
!
!       If calib not in namfil, no problem, as long as trying to turn it
!       off.  (Doesn't matter if not applied because turned off or not there.)
!       If trying to turn it on, that's a problem.  Fndcls should prevent
!       getting here, but just in case...
!
        IF (IB .EQ. 1) CALL FERR( INT2(13004), CAL// &
     &  ' NOT IN NAMFIL - CALSFND CANNOT APPLY IT', INT2(0), INT2(0) )
!
 50     CALL GETITM(SEC,FRAME,ITEM,CAL)
      ENDDO
!
!     If the instruction from frame m was to turn the calibs in frame 1
!     on or off, we're done.  Otherwise the instruction was the first
!     of a subset of
!     calibration names from frame 1 to be turned on.
!     Have already turned off
!     all the calibs in frame 1, so now go back to frame m and turn those
!     calibs on.
!
      IF(STATE.EQ.'ON'.OR.STATE.EQ.'OFF') RETURN
!
      FRAME=SAVFRM
      ITEM=0
      CALL GETITM(SEC,FRAME,ITEM,STATE )
      DO WHILE(ITEM.GE.0)
        DO J=1,ICALS
          IF(NAMCAL(J).EQ.STATE) THEN
            CALL SBIT( NAMAPL, J, INT2(1) )
            GO TO 100
          ENDIF
        ENDDO
        DO J=1,IFCALS
          IF(NAMFCAL(J).EQ.STATE) THEN
            CALL SBIT( JCAFFL(1), J, INT2(1) )
            GO TO 100
          ENDIF
        ENDDO
        CALL FERR( INT2(13005), STATE// &
     &      ' NOT IN NAMFIL -- CALSFND CANNOT APPLY', INT2(0), INT2(0) )
100     CONTINUE
        CALL GETITM(SEC,FRAME,ITEM,STATE )
      ENDDO
!
      RETURN
      END
