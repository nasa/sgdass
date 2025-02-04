      SUBROUTINE DOSITECALS(NAMSIT,NAMCAL,NAMFCAL,ICALS,IFCALS, &
     &        NAMAVL,NAMAPL,JCAFFL)
      IMPLICIT NONE
!
! 1.  DOSITECALS PROGRAM SPECIFICATION
!
! 1.1 Apply the proper site dependent (flyby and non-flyby) calibrations
!     for this site.
!
! 1.2 REFERENCES:
!
! 2.  DOSITECALS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ICALS,IFCALS,NAMAVL
      CHARACTER*(8) NAMSIT,NAMCAL(ICALS),NAMFCAL(112)
!
! ICALS - Number of non-flyby calibrations from NAMFIL
! IFCALS - Number of flyby calibrations from NAMFIL
! NAMAVL - Bit map of available non-flyby calibrations
! NAMCAL - List of non-flyby calibrations from NAMFIL
! NAMFCAL - List of flyby calibrations from NAMFIL
! NAMSIT - Name of this site
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 NAMAPL,JCAFFL(7)
!
! NAMAPL - Bit map of applied non-flyby calibrations
! JCAFFL - Bit map of applied flyby calibrations
! NAMFCAL and IFCALS may also be updated, if the sub adds flyby calibs
!   to the namfil, because of requests from the batch control file
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: docali
!       CALLED SUBROUTINES: fndcls,calsfnd,douse
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 FRSTFRM,SAVFRM,SEC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  DOSITECALS PROGRAM STRUCTURE
!
!     Earlier, gcalib set up a set of calibration sections, based on how
!     the user's batch control file requested that calibrations should
!     be handled.  Each section
!     describes how one or more calibrations should be handled.  Each
!     section consists of n frames.  The first frame lists the calibration(s)
!     in question -- default for all calibrations, a single calibration or
!     a list for calibrations mentioned in a group phrase in the batch control
!     file.  Frames 2 through n-1 specify which of the calibrations in the
!     first frame should be turned on and off.  The possibilities are:
!
!      for default in frame 1 (all calibs)
!         DEFAULT to how the individual arcs set up their calibs
!
!      for a single calib name in frame 1
!         DEFAULT to how the individual arcs set it up
!         ON for try to turn this calib on
!         OFF for turn this calib off
!
!      for a group of calib names in frame 1
!         DEFAULT to how the individual arcs set them up
!         a subset list of the calibs to be turned on
!           (the ones listed in frame 1 but not listed in this frame
!               will be turned off)
!
!     If the section does not pertain to a group of calibrations,
!     there will be only one middle frame.  If it does pertain to a group,
!     there may be multiple middle frames, describing alternate subsets
!     of the list in frame 1 to be turned on, if previous subsets are
!     unavailable.  The final frame describes the stations to which this
!     section applies.  The format is all or none, possibly followed by one
!     or more stations which are exceptions to the all or none.
!
!     This call of dositecals applies to a single station.  The call will
!     run through the calibration sections looking for ones which pertain to
!     the station.  It will then turn the calibrations to which the section
!     pertains on or off, as specified in the first middle frame of the
!     section that can be used for this arc.
!
      SEC=0
!
!     find the first section pertaining to this station.  savfrm will
!     receive which of the middle frames should be used as the guideline
!     to turning the calibration(s) in frame 1 on and off
!
      CALL FNDCLS(SEC,NAMSIT,NAMCAL,NAMFCAL,ICALS,IFCALS,NAMAVL, &
     &            FRSTFRM,SAVFRM)
      DO WHILE (SEC.GE.0)
!
!       now turn the calibrations in frame 1 on and off as specified
!
        CALL CALSFND(SEC,NAMCAL,NAMFCAL,ICALS,IFCALS,NAMAVL,NAMAPL, &
     &               JCAFFL,FRSTFRM,SAVFRM)
!
!       new section pertaining to this station.
!
        CALL FNDCLS(SEC,NAMSIT,NAMCAL,NAMFCAL,ICALS,IFCALS,NAMAVL, &
     &              FRSTFRM,SAVFRM)
      ENDDO
!
!     the batch control file made one more request; that some calibrations
!     should be applied in place of other calibrations.  Go back over the
!     calibrations that have been turned/left on for this station and make
!     any requested substitutions.
!
      CALL DOUSE(NAMCAL,NAMFCAL,ICALS,IFCALS,NAMAVL,NAMAPL,JCAFFL)
!
      RETURN
      END
