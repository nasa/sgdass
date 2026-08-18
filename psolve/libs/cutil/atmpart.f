      SUBROUTINE ATMPART (ITT,ISITE,ISITN,ISTAR,VSTARC,AZ,ELEV, &
     &                  ATMPR,RELHU,TEMPC,LATS,HEIGHTS,AX_OFFS, &
     &                  AX_TYPES,BARO_CALS,BARO_HEIGHTS,IDB)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
!     Updated to specificaly type integers which
!-------------------------------------------------
! 1.  ATMPART PROGRAM SPECIFICATION
!
! 1.1 Overwrite the wet atmosphere partials with the appropriate values
!
! 1.2 REFERENCES:
!
! 2.  ATMPART INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
!
! 2.3 OUTPUT Variables:
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: partcalc
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IDB,do_prcor
      INTEGER*2 ITT(MAX_ARC_STA), ISITE(2)
      LOGICAL*2 KBIT
      LOGICAL*2   MTT_SEAS_DRY(2), MTT_SEAS_WET(2)
      LOGICAL*2   IFA_SEAS_DRY(2), IFA_SEAS_WET(2)
      INTEGER*2 ISTAR,AX_TYPES(MAX_ARC_STA)
      INTEGER*2 ID_MTTWETSS, ID_MTTWTFLY
      INTEGER*2 ID_IFAWETSS, ID_IFAWTFLY
      INTEGER*2 ID_DRY_CHAO,ID_CHWTPART,ID_NMFWTFLY
      INTEGER*2 ISITN(4,MAX_STA)
      REAL*8 AZ(2),ELEV(2),ATMPR(2),RELHU(2),TEMPC(2)
      REAL*8 VSTARC(2,MAX_SRC),AX_OFFS(MAX_ARC_STA)
      REAL*8 LATS(MAX_ARC_STA),HEIGHTS(MAX_ARC_STA)
      REAL*8 BARO_CALS(MAX_ARC_STA), BARO_HEIGHTS(MAX_ARC_STA)
      integer*2 idb_save,i,ista1,ista2,istat,j,jj,n
      integer*2 id_wet(3)
      logical*2 set_this_id,twice_in_namf
      character*100 errstr
!
! ITT - NAMFIL/PARFIL station correspondence table
! ISITE - Site number of the two stations in this observation
! ISITN - Array of site names
! ISTAR - Source number for this observation
! VSTARC - Array of source coordinates (RA and DEC, in radians)
! AZ - Azimuth
! ELEV - Elevation
! ATMPR - Atmospheric pressure, millibars
! RELHU - Relative humidity
! TEMPC - Temerature (Celsius)
! LATS - Latitude of each station
! HEIGHTS - Heights of each station
! AX_OFFS - Antenna axis offset
! AX_TYPES - ANtenna axis type for each station
! BARO_CALS - Barometer calibration for each station
! BARO_HEIGHTS - Height of barometer for each station
!
      SAVE IDB_SAVE, ID_MTTWETSS, &
     &     ID_MTTWTFLY, MTT_SEAS_DRY, MTT_SEAS_WET, &
     &     ID_IFAWETSS, &
     &     ID_IFAWTFLY, IFA_SEAS_DRY, IFA_SEAS_WET,ID_DRY_CHAO, &
     &     ID_CHWTPART
!
      DATA IDB_SAVE /0/, TWICE_IN_NAMF /.FALSE./
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE 920627   First version. Based on SOCAL but only do partials.
!
! 5.  ATMPART PROGRAM STRUCTURE
!
!   If no partial is selected, return at once
!
      if (part_applied.eq.0) RETURN
!
      ISTA1 = ITT(ISITE(1))
      ISTA2 = ITT(ISITE(2))
!
!     Apply the selected partial:
!
        ID_MTTWETSS = 0
        ID_MTTWTFLY = 0
        ID_IFAWETSS = 0
        ID_IFAWTFLY = 0
        ID_DRY_CHAO = 0
        ID_CHWTPART = 0
        ID_NMFWTFLY = 0
!
          I = 1
          J = 1
              IF (part_array(part_applied) .EQ. 'MTTWETSS') THEN
                IF (ID_MTTWETSS.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_MTTWETSS = J
              ELSE IF(part_array(part_applied) .EQ. 'MTTWTFLY') THEN
                IF (ID_MTTWTFLY.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_MTTWTFLY = J
              ELSE IF(part_array(part_applied) .EQ. 'IFAWETSS') THEN
                IF (ID_IFAWETSS.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_IFAWETSS = J
              ELSE IF(part_array(part_applied) .EQ. 'IFAWTFLY') THEN
                IF (ID_IFAWTFLY.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_IFAWTFLY = J
              ELSE IF(part_array(part_applied) .EQ. 'CHDRPART') THEN
                IF (ID_DRY_CHAO.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_DRY_CHAO = J
              ELSE IF(part_array(part_applied) .EQ. 'CHWTPART') THEN
                IF (ID_CHWTPART.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_CHWTPART = J
              ELSE IF(part_array(part_applied) .EQ. 'NMFWTFLY') THEN
                IF (ID_NMFWTFLY.NE.0) TWICE_IN_NAMF = .TRUE.
                ID_NMFWTFLY = J
!
              ELSE
                write(errstr,'("invalid partial ",a8," in atmpart --", &
     &            " cannot apply")') &
     &            part_array(part_applied)
                call ferr( INT2(201), errstr, INT2(0), INT2(0) )
                call fatal(errstr )
              END IF
              IF (TWICE_IN_NAMF) THEN
                WRITE(ERRSTR,'("IN ATMPART - A PARTIAL ", &
     &               "APPEARS TWICE FOR ONE DB")')
                CALL FERR( INT2(1201), errstr, INT2(0), INT2(0) )
                CALL FATAL(ERRSTR )
              END IF
        IDB_SAVE = IDB
      DO_PRCOR = 0
!
!     Next generate the necessary values for this observation
!
      IF &
     &     (ID_MTTWETSS.NE.0 .OR. &
     &     ID_MTTWTFLY.NE.0 .OR. &
     &     ID_IFAWETSS.NE.0 .OR. &
     &     ID_IFAWTFLY.NE.0 .OR. &
     &     ID_DRY_CHAO.NE.0 .OR. &
     &     ID_NMFWTFLY.NE.0 .OR. &
     &     ID_CHWTPART.NE.0      ) THEN
!
!           the subroutine which calculates the cfadry correction needs
!           to know whether it is calculating the kbdry or jjdry value
!           for the stations in this observation
!
            DO I = 1,2
              ISTAT = ITT(ISITE(I))
              ID_WET(I) = 0
              MTT_SEAS_WET(I) = .FALSE.  ! default for mttwet is non-seasonal
              IFA_SEAS_WET(I) = .FALSE.  ! default for ifawet is non-seasonal
              IF (ID_MTTWETSS .NE.0 ) then
                  ID_WET(I) = 1  ! use mttwet & seasonal mapping function.
                  MTT_SEAS_WET(I) = .TRUE.
              END IF
              IF (ID_MTTWTFLY .NE.0 ) then
                  ID_WET(I) = 1  ! use mttwet & unseasonal mapping function.
                  MTT_SEAS_WET(I) = .FALSE.
              END IF
!
              IF (ID_IFAWETSS .NE.0 ) then
                  ID_WET(I) = 2  ! use ifawet & seasonal mapping function.
                  IFA_SEAS_WET(I) = .TRUE.
              END IF
              IF (ID_IFAWTFLY .NE.0 ) then
                  ID_WET(I) = 2  ! use ifawet & unseasonal mapping function.
                  IFA_SEAS_WET(I) = .FALSE.
              END IF
              IF (ID_DRY_CHAO .NE.0 ) then
                  ID_WET(I) = 3  ! use chao dry mapping function.
              END IF
              IF (ID_CHWTPART .NE.0 ) then
                  ID_WET(I) = 0  ! use chao dry mapping function.
              END IF
              IF (ID_NMFWTFLY .NE.0 ) then
                  ID_WET(I) = 4  ! use Niell mapping function.
              END IF
            END DO
!
            CALL PARTCALC(ISITE,ISITN,ISTAR,VSTARC,AZ,ELEV,PI__NUM, &
     &           ATMPR,RELHU,TEMPC,VLIGHT, &
     &           FLYBY_WARNING,LATS,HEIGHTS,AX_OFFS, &
     &           AX_TYPES,BARO_CALS,BARO_HEIGHTS, &
     &           MTT_SEAS_WET,IFA_SEAS_WET,ID_WET )
      END IF
      RETURN
      END
