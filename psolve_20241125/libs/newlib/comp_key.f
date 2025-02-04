      SUBROUTINE COMP_KEY(IN_NAME,OUT_NAME)
      IMPLICIT NONE
      CHARACTER*(*) IN_NAME,OUT_NAME
!
      INCLUDE 'param.i'
!
!   For an input database key name,
!   convert the typical three character representation of the month
!   ("JUL","AUG" etc.) to a numeric representation (007,008) so that keys
!   can be sorted in time order (e.g., JUL before AUG, not after)
!   Also add the century to take into account the transition to the year
!   2000.
!
!   restrictions:
!     does not handle special key names (ones not in the
!      standard $yrmondy format).
!
!
!
!   INPUT:
!
!    IN_NAME:  a database key name, $YRMONDYXX
!
!   OUTPUT:
!
!    OUT_NAME: $CNYRMONDYXX, where CN represents the century (19 or 20)
!              and MON has been replaced by a 3 digit
!              month number with leading zeros.
!
!   Creation: taken from solve/mksup's comp_name.f by kdb, 980116
!             (Only the part that converts the month representation was
!             needed and taken.)
!             Also modified to output the century.
!             Also modified to handle database key names, rather than
!             superfile disk file names, which are offset by one character.
!
!    modifications
!
!    981008 kdb Y2K fixes.  (Parameterize the starting year for y2k windowing.)
!
!
      CHARACTER*3 MON(12),STR(12)
      INTEGER*2 I
      INTEGER*2 IYEAR,ICENTURY
      DATA MON/'JAN','FEB','MAR','APR','MAY','JUN', &
     &         'JUL','AUG','SEP','OCT','NOV','DEC'/
      DATA STR/'001','002','003','004','005','006', &
     &         '007','008','009','010','011','012'/
!
!     Transfer the basic information to out_name
!
      OUT_NAME(1:1) = '$'
      OUT_NAME(4:)=IN_NAME(2:)
!
!     Add the century
!
      READ (IN_NAME(2:3),"(I2)") IYEAR
      ICENTURY = 19
      IF (IYEAR.LT.Y2K_START_YEAR) ICENTURY = 20
      WRITE(OUT_NAME(2:3),"(I2)") ICENTURY
!
      DO I=1,12
        IF(OUT_NAME(6:8).EQ.MON(I)) THEN
          OUT_NAME(6:8)=STR(I)
          RETURN
        ENDIF
      ENDDO
!
!--- Month not available in keyname. Assume it's January.
!
      out_name(6:8) = str(1)
      return
      END
