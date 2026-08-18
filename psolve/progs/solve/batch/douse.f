      SUBROUTINE DOUSE(NAMCAL,NAMFCAL,ICALS,IFCALS,NAMAVL,NAMAPL,JCAFFL)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DOUSE PROGRAM SPECIFICATION
!
! 1.1  Process USE/FOR clause of CALIBRATIONS section of batch control file.
!
! 1.2 REFERENCES:
!
! 2.  DOUSE INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ICALS,IFCALS,NAMAVL,NAMAPL,JCAFFL(7)
      CHARACTER*(8) NAMCAL(ICALS) ,NAMFCAL(112)
!
! ICALS - Number of non-flyby calibrations from NAMFIL
! IFCALS - Number of flyby calibrations from NAMFIL
! NAMAPL - Bit map of applied non-flyby calibrations
! JCAFFL - Bit map of applied flyby calibrations
! NAMAVL - Bit map of available non-flyby calibrations
! NAMCAL - List of non-flyby calibrations from NAMFIL
! NAMFCAL - List of flyby calibrations from NAMFIL
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'calcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: dositecals
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 ICOUNT,I,J,K,IUSE,IFOR
      LOGICAL*2 KBIT,PRESENT,APPLIED
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DOUSE PROGRAM STRUCTURE
!
      ICOUNT=1
      DO WHILE(ICOUNT.LE.ICLUSE)
!
!       Look for calibration in "for" part of this "use x for y" clause
!       and see if it's applied.  Looking in two lists, flyby and regular.
!
        IFOR = 0
        DO I=1,ICALS
          IF(CALFOR(ICOUNT).EQ.NAMCAL(I).AND.KBIT(NAMAPL,I)) IFOR = I
        END DO
        IF (IFOR.EQ.0) THEN
          DO I = 1,IFCALS
            IF(CALFOR(ICOUNT).EQ.NAMFCAL(I).AND.KBIT(JCAFFL(1), &
     &         I))IFOR = I * -1
          END DO
        END IF
!
        IF (IFOR.NE.0) THEN
!
!         The object of the 'for' exists and is applied, so we need a
!         substitution.  Look for the object of the 'use'.
!
          IUSE = 0
          DO J=1,ICALS
            IF(CALUSE(ICOUNT).EQ.NAMCAL(J)) IUSE = J
          END DO
          IF (IUSE.EQ.0) THEN
            DO J = 1,IFCALS
              IF(CALUSE(ICOUNT).EQ.NAMFCAL(J)) IUSE = -1 * J
            END DO
          END IF
!
!         The calibration to be used is not in the namfil.  If it is a
!         non-flyby one, the user is out of luck-- cannot add it, because
!         too hard to get the calibration values into the obsfil.  If the
!         calibration's a flyby one, it can be added, as long as this
!         version of solve supports it down in socal.
!
          IF (IUSE.EQ.0) THEN
            DO J = 1,IAVAIL
              IF(CALUSE(ICOUNT).EQ.AVAIL(J)) THEN
                IF (IFCALS .LT. MAX_FLY) THEN
                  IFCALS = IFCALS + 1
                  NAMFCAL(IFCALS) = CALUSE(ICOUNT)
                  IUSE = -1 * IFCALS
                  GO TO 110
                END IF
              END IF
            END DO
  110       CONTINUE
          END IF
!
!
          IF (IUSE.NE.0) THEN
!
!           The calibration to be used is at least listed in the namfil.
!           Now make sure available.  Only need to check if for
!           availability if it's a
!           non-flyby calibration.  Any flyby one in the namfil has
!           already been checked by sdbh when it created the namfil
!           in liptn mode, or by douse, just now, when douse added it.
!
!           At the same time, make sure the calibration is not already
!           applied.  Cannot apply a calibration twice
!
            IF (IUSE.GT.0) THEN
              PRESENT = KBIT(NAMAVL,IUSE) !availability check
              APPLIED = KBIT(NAMAPL,IUSE)
            ELSE
              PRESENT = .TRUE.
              APPLIED = KBIT( JCAFFL(1), INT2(IUSE * -1))
            END IF
!
!           Now either do the substitution or dump the error condition found.
!
            IF (.NOT. PRESENT) THEN
              CALL FERR( INT2(13021), CALUSE(ICOUNT)// &
     &            ' NOT AVAILABLE IN SUB DOUSE', INT2(0), INT2(0) )
            ELSE IF (APPLIED) THEN
              CALL FERR( INT2(13022), CALUSE(ICOUNT)// &
     &            ' ALREADY APPLIED IN SUB DOUSE', INT2(0), INT2(0) )
            ELSE
!
!             Turn on the substitute flyby or non-flyby calib
!
              IF (IUSE .LT. 0) THEN !flyby
                IUSE = IUSE * -1
                CALL SBIT( JCAFFL(1), IUSE, INT2(1) )
              ELSE
                CALL SBIT( NAMAPL, IUSE, INT2(1) )
              END IF
!
!             Turn off the old flyby or non-flyby calib
!
              IF (IFOR .LT. 0) THEN !flyby
                IFOR = IFOR * -1
                CALL SBIT( JCAFFL(1), IFOR, INT2(0) )
              ELSE
                CALL SBIT( NAMAPL, IFOR, INT2(0) )
              END IF
            END IF
          ELSE
            CALL FERR( INT2(13030), CALUSE(ICOUNT)// &
     &          ' NOT IN NAMFIL OR UNAVAIL FLY CAL-DOUSE CANNOT SUB FOR '// &
     &           CALFOR(ICOUNT), INT2(0), INT2(0) )
          END IF
        END IF
        ICOUNT = ICOUNT + 1
      END DO
!
      RETURN
      END
