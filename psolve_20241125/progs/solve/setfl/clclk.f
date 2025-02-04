      SUBROUTINE CLCLK(ISITE)
      IMPLICIT    NONE
!
! 1.  CLCLK PROGRAM SPECIFICATION
!
! 1.1 Clear out all the clock epochs for this station (except the first).
!
! 1.2 REFERENCES:
!
! 2.  CLCLK INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2   ISITE
!
! ISITE - Site number of the station being processed
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: setcl_sta
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   i,J,KBITN,OLDPOS,NEWPOS,K
      LOGICAL*2   SBIT
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CLCLK PROGRAM STRUCTURE
!
!   Clear the epocs and flags for clocks after first epoc
!
      IF(NUMCLK(ISITE).GT.1) THEN
          DO J=ICLSTR(ISITE)+2,ICLSTR(ISITE)+NUMCLK(ISITE)
              FJDCL(J)=0.0
              LCLK(J)=0
          ENDDO
!
!   Decrement the counters for the succeeding stations
!
          IF(ISITE.LT.NUMSTA) THEN
              DO J=ISITE+1,NUMSTA
                   ICLSTR(J)=ICLSTR(J)-NUMCLK(ISITE)+1
                  DO K= 1,NUMCLK(J)
                      OLDPOS=ICLSTR(J)+K+NUMCLK(ISITE)-1
                      NEWPOS=ICLSTR(J)+K
                      LCLK(NEWPOS)=LCLK(OLDPOS)
                      LCLK(OLDPOS)=0
                      FJDCL(NEWPOS)=FJDCL(OLDPOS)
                      FJDCL(OLDPOS)=0.0
                      do i=1,2
                        iclsta(i,newpos)=iclsta(i,oldpos)
                        iclsta(i,oldpos)=0
                      enddo
                  ENDDO
              ENDDO
          ENDIF
!
!   Reset the atmosphere counter for this station
!
          NUMCLK(ISITE)=1
      ENDIF
      RETURN
      END
