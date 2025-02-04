      SUBROUTINE GFLAGS_GN(STACMP)
      IMPLICIT NONE
!
! 1.  GFLAGS_GN PROGRAM SPECIFICATION
!
! 1.1 Parse FLAGS section of the control file in order to find the value
!     of STACMP (which indicates if XYZ or UEN is being estimated).
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
!     stacmp - XYZ or UEN to indicate which components being estimated.
!
      CHARACTER*(*) STACMP
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*128 STRING,TOKEN
      INTEGER*2 LENGTH,IDUM
!
!  EXTERNAL FUNCTIONS
!
      INTEGER*2 CFREAD,TRIMLEN
      LOGICAL*2 CFEOF
!
! 4.  HISTORY
!
!   written 4/17/95 by kdb (from solve/batch/gflags.f)
!
! 5.  GFLAGS_GN PROGRAM STRUCTURE
!
      LENGTH=CFREAD(STRING)
      DO WHILE(STRING(1:1).EQ.' '.AND..NOT.CFEOF(IDUM))
        DO WHILE(TRIMLEN(STRING).GT.0)
          CALL SPLITSTRING(STRING,TOKEN,STRING)
          IF(TOKEN.EQ.'STATIONS') THEN
            CALL SPLITSTRING(STRING,TOKEN,STRING)
            CALL SPLITSTRING(STRING,TOKEN,STRING)
            CALL SPLITSTRING(STRING,STACMP,STRING)
            IF(STACMP.EQ.' ') STACMP='XYZ'
          ENDIF
        ENDDO
        LENGTH=CFREAD(STRING)
      ENDDO
!
      CALL CFUNRD(LENGTH,STRING)
!
      RETURN
      END
