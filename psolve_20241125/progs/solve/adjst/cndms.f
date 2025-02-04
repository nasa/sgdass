      SUBROUTINE CNDMS(MS,ID,IM,FSEC)
      IMPLICIT NONE
!
! 1.  CNDMS PROGRAM SPECIFICATION
!
! 1.1 Convert arc seconds to degrees, minutes and seconds.
!
! 1.2 REFERENCES:
!
! 2.  CNDMS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      REAL*8 FSEC
!
! FSEC - Number of arc seconds
!
! 2.3 OUTPUT Variables:
!
      character ms*1
      INTEGER*2 ID,IM
!
! FSEC - Number of arc seconds (input).  Changed to positive mod 60
!        seconds (output).
! ms - Sign of angle ("+" or "-").
! ID - Number of degrees
! IM - Number of arc minutes
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a1jst
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      REAL*8 FTEMP
!
! FTEMP - Temporary real*8 version of degrees, minutes
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   BA    97.10.20  Added "ms" argument to properly keep track of sign.
!
! 5.  CNDMS PROGRAM STRUCTURE
!
! Set sign.
!
      ms = '+'
      if(fsec.lt.0.D0) then
        ms = '-'
        fsec = -fsec
      endif
!
! Calculate degrees
!
      ID = FSEC/3600.0D0 + 0.1D-10
      FTEMP = ID
      FSEC = FSEC - 3600.0D0 * FTEMP
      IF(ID.NE.0) FSEC = DABS(FSEC)
!
! Calculate Minutes
!
      IM = FSEC/60.0 + 0.1D-10
      FTEMP = IM
!
! Calculate seconds
!
      FSEC = FSEC - 60.0D0 * FTEMP
      IF(IM.NE.0.OR.ID.NE.0) FSEC=DABS(FSEC)
      RETURN
      END
