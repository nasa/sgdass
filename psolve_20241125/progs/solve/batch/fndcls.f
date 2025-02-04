      SUBROUTINE FNDCLS(SEC,NAMSIT,NAMCAL,NAMFCAL,ICALS,IFCALS, &
     &                  NAMAVL,FRSTFRM,SAVFRM)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FNDCLS PROGRAM SPECIFICATION
!
! 1.1 find a calibration frame:  That is, find the next calibration section
!     that applies to the current station.  Then pass back the number of the
!     first frame within it that describes how to turn on and off
!     the calibrations to which the section pertains (the calibs in frame 1).
!
! 1.2 REFERENCES:
!
! 2.  FNDCLS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'calcm.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ICALS, IFCALS, SEC, NAMAVL, FRSTFRM
      CHARACTER NAMCAL(ICALS)*8, NAMFCAL(112)*8, NAMSIT*8
!
! FRSTFRM - First frame number
! ICALS - Number of non-flyby calibrations from NAMFIL
! IFCALS - Number of flyby calibrations from NAMFIL
! NAMAVL - Bit map of available calibrations (non-flyby only)
! NAMCAL - List of non-flyby calibrations from NAMFIL
! NAMFCAL - List of flyby calibrations from NAMFIL
! NAMSIT - Name of this site
! SEC - Section index to position to
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 SAVFRM
!
! SAVFRM - Frame number to be saved
!  may also add a flyby calibration, updating namfcal and ifcals
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: dositecals
!       CALLED SUBROUTINES: fndlst,getitm,setfrm,setsec
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 CAL,STATE,STATION
      INTEGER*2 ITEM,I,FRAME,LSTFRM
      LOGICAL*2 GOTIT,KBIT,PRESENT,THIS_ST,THIS
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  FNDCLS PROGRAM STRUCTURE
!
!  SEARCH FOR A SECTION THAT APPLIES TO THIS STATION
!
      THIS_ST=.FALSE.
      DO WHILE(.NOT.THIS_ST)
!
!  GO TO NEXT SECTION
!
        CALL SETSEC(SEC )
        IF(SEC.LT.0) RETURN
!
!  FIND LAST FRAME AND CHECK THAT THIS STATION IS RELEVENT
!
        FRAME=0
        CALL FNDLST(SEC,FRAME )
        LSTFRM=FRAME
        ITEM=0
        CALL GETITM(SEC,FRAME,ITEM,STATION )
        IF(ITEM.LE.0) THEN
          CALL FERR( INT2(9000), 'INTERNAL ERROR A: FNDCLS', INT2(0), INT2(0) )
        ELSE IF(STATION.EQ.'ALL'.OR.STATION.EQ.'NONE') THEN
          THIS_ST=STATION.EQ.'ALL'
        ELSE
          CALL FERR( INT2(9000), 'INTERNAL ERROR B: FNDCLS', INT2(0), INT2(0) )
        ENDIF
        THIS=.FALSE.
        DO WHILE(ITEM.GT.0.AND..NOT.THIS)
          THIS=STATION.EQ.NAMSIT
          CALL GETITM(SEC,FRAME,ITEM,STATION )
        ENDDO
        IF(THIS) THIS_ST=.NOT.THIS_ST
      ENDDO
!
!  GO TO FIRST FRAME IN THIS SECTION (LIST OF CALIBRATIONS)
!
      FRAME=0
      CALL SETFRM(SEC,FRAME )
      FRSTFRM=FRAME
      GOTIT=.FALSE.
      DO WHILE(.NOT.GOTIT)
!
! check second and higher frames, which request different combinations
!   of calibrations to be applied at this station in this arc.  Looking
!   for the first combination that will work.
!
        CALL SETFRM(SEC,FRAME )
        IF(FRAME.EQ.LSTFRM) THEN
          CALL FERR( INT2(18010), 'NO POSSIBLE CALIBRATION SETUP', INT2(0), &
     &         INT2(0) )
        ENDIF
        SAVFRM=FRAME
        ITEM=0
        CALL GETITM(SEC,FRAME,ITEM,STATE )
!
!  if default or off, then trying to turn off all items in the first frame,
!   or keep them to however they were set in the arc.  Either case
!    is possible.
!
        IF(STATE.EQ.'DEFAULT'.OR.STATE.EQ.'OFF') THEN
          GOTIT=.TRUE.
!
!  if on, then trying to turn on all items in the first frame.  Check to make
!    sure that they are all available.
!
        ELSE IF(STATE.EQ.'ON')THEN
          FRAME=FRSTFRM
          ITEM=0
          CALL GETITM(SEC,FRAME,ITEM,CAL )
          GOTIT=.TRUE.
          DO WHILE(ITEM.GE.0)
            PRESENT=.FALSE.
            DO I=1,ICALS
              PRESENT=PRESENT.OR.(NAMCAL(I).EQ.CAL.AND.KBIT(NAMAVL,I))
            ENDDO
            DO I=1,IFCALS
              PRESENT=PRESENT.OR.(NAMFCAL(I).EQ.CAL)
            ENDDO
!
!           Even if the calibration isn't in the namfil, this sub may be
!           able to add it.  The calib must be flyby and available to
!           the current version of socal
!
            IF (.NOT. PRESENT) THEN
              DO I = 1,IAVAIL
                IF (AVAIL(I) .EQ. CAL) THEN
                  IF (IFCALS .LT. MAX_FLY) THEN
                    IFCALS = IFCALS + 1
                    NAMFCAL(IFCALS) = CAL
                    PRESENT = .TRUE.
                    GO TO 110
                  END IF
                END IF
              END DO
  110         CONTINUE
            END IF
!
            GOTIT=GOTIT.AND.PRESENT
            CALL GETITM(SEC,FRAME,ITEM,CAL )
          ENDDO
          FRAME=SAVFRM
!
! IF list of calibration names, then trying to turn on a specific subset
!   of the calibs in frame 1 (ie, the calibs listed in this frame).  Make
!    sure all the calibs in this frame are available.
!
        ELSE
          GOTIT=.TRUE.
          DO WHILE(ITEM.GE.0)
            PRESENT=.FALSE.
            DO I=1,ICALS
              PRESENT=PRESENT.OR.(NAMCAL(I).EQ.STATE.AND.KBIT(NAMAVL,I))
            ENDDO
            DO I=1,IFCALS
              PRESENT=PRESENT.OR.(NAMFCAL(I).EQ.STATE)
            ENDDO
!
!           If the calib is a flyby available calib, then add it to the namfil
!           if it isn't there.
!
            IF (.NOT. PRESENT) THEN
              DO I = 1,IAVAIL
                IF (AVAIL(I) .EQ. STATE) THEN
                  IF (IFCALS .LT. MAX_FLY) THEN
                    IFCALS = IFCALS + 1
                    NAMFCAL(IFCALS) = STATE
                    PRESENT = .TRUE.
                    GO TO 111
                  END IF
                END IF
              END DO
  111         CONTINUE
            END IF
!
            GOTIT=GOTIT.AND.PRESENT
            CALL GETITM(SEC,FRAME,ITEM,STATE )
          ENDDO
        ENDIF
      ENDDO
!
      RETURN
      END
