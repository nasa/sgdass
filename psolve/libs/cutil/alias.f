      FUNCTION ALIAS (QSTA)
      IMPLICIT NONE
!
! 1.  ALIAS PROGRAM SPECIFICATION
!
! 1.1 Check old station names and change them to new ones,
!       by reading the alias file
!
! 1.2 REFERENCES:
!
! 2.  ALIAS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*8 QSTA
!
! QSTA - Station name to be checked for an alias
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*8 ALIAS
!
! ALIAS - Correct station name
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, N
      INTEGER*4  IOS
      CHARACTER*8 QSTL(2, 15),QT1,QT2, FILE*128
      LOGICAL*2 KFIR, OK
      DATA KFIR /.TRUE./, OK /.TRUE./,N /0/
!
! KFIR - True if this is the first time through
! N - Counter for number of stations in alias file
! OK - FALSE if alias file is not present
! QSTL - Array to hold old, new names from alias file
! QT1 - Old name read from alias file
! QT2 - New name read from alias file
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  891221  documentation, added IMPLICIT NONE, declared N
!
! 5.  ALIAS PROGRAM STRUCTURE
!
! Open the alias file if this is the first time through
!
      IF (KFIR) THEN
        OPEN (UNIT=301, FILE='/solve/arg/alias_file', &
     &         ERR=900)
        REWIND (301)
        N = 0
!
! Read in the alias entries from the alias file
!
  100   CONTINUE
          READ (301,'(A8,2X,A8)',IOSTAT=ios,END=110) QT1,QT2
          call ferr ( ios, "Reading "//FILE, 0, 0 )
          N = N+1
          IF(N.GT.15) call ferr(146,'TOO MANY ALIAS STATIONS',0,0)
          QSTL(1,N)=QT1
          QSTL(2,N)=QT2
          GO TO 100
  110   CONTINUE
        KFIR = .FALSE.
      END IF
!
      ALIAS = QSTA
      IF (.NOT. OK) GO TO 210
!
! Look for a match
!
      DO 200 I = 1, N
        IF (QSTA .EQ. QSTL(1,I)) THEN
          ALIAS = QSTL(2, I)
          GO TO 210
        END IF
  200 CONTINUE
  210 CONTINUE
      RETURN
!
! Error trap if file isn't accessible
!
  900 CONTINUE
      call ferr(147, 'can''t access alias file -- no conversion will be &
     & done',0,0)
      OK = .FALSE.
      RETURN
      END
