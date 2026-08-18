      SUBROUTINE ZTIME(CBUF,IMODE)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!-----The following type specification statements added automatically
!     by imp/jwr July 2002
!
      INTEGER*2 imode, itime, iyear, month, iday, ipmon, ipday
      REAL*4 fsec
!-----END of imp added lines.
!
!
!     ZTIME WILL RETURN THE DATE AND TIME IN THE FORMAT THAT CALC
!     EXPECTS TO SEE.  IT MIMICS THE 360 ROUTINE ZTIME.  THIS ROUTINE
!     WAS WRITTEN BY MODIFYING TAC'S ROUTINE DATER.
!
!     CALLING SEQUENCE - CALL ZTIME(CBUF,IMODE)
!
!     INPUT VARIABLES
!
!     1) IMODE - =2 TO RETURN THE DATE.  =1 TO RETURN THE TIME
!
!       OUTPUT VARIABLES
!
!     1) CBUF(1) - THE RETURNED DATE OR TIME
!
!
!     PROGRAM SPECIFICATIONS
      DIMENSION  IPMON(2) , IPDAY(2) , ITIME(5)
      CHARACTER*(*)  CBUF
!
!     PROGRAMMER - BRUCE SCHUPLER  8 JANUARY 1980
!                  SAVITA GOEL (CDS FOR A900)
!                  Gregg Cooke   90.02.14    Removed exec11 and in2a2 calls
!
!     GET SYSTEM DATE/TIME AND CONVER TO USABLE FORMAT:
      call timeget(ITIME,IYEAR)
      FSEC = FLOAT ( ITIME (1) ) / 100.
      MONTH= 0
      IDAY = ITIME (5)
!
! 2.    CALL CLNDR TO DO ALL THE CONVERSIONS:
!
      CALL CLNDR ( IYEAR , MONTH , IDAY , IPMON , IPDAY )
!
! 3.   SEE WHAT WE NEED TO WRITE AND DO IT
!
      IF(IMODE .EQ. 2) GO TO 400
!
      WRITE(CBUF,310) ITIME(4), ITIME(3), ITIME(2), FSEC
310   FORMAT(I2,".",I2.2,".",I2.2,F3.2,1X)
      GO TO 500
!
! 4.    WE WANT THE DATE
!
400   CONTINUE
!
      WRITE(CBUF,410) IPDAY,IPMON,IDAY,IYEAR
410   FORMAT(A2,A1,1X,A2,A1,1X,I2.2,",",I4,1X)
      GO TO 500
!
! 5.    THE END
!
500   CONTINUE
      RETURN
      END
