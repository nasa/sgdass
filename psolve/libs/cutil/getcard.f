      SUBROUTINE GETCARD ( NUMDB, LCARD, ICONT, JBUF, IERR )
      IMPLICIT NONE
!
! 1.  GETCARD PROGRAM SPECIFICATION
!
! 1.1 Gets card images from the NAMFIL
!
! 1.2 REFERENCES:
!
! 2.  GETCARD INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NUMDB,ICONT
      CHARACTER*4 LCARD
!
! ICONT - Function controller:
!         1 = Get 1st card of type LCARD for database section NUMDB
!         0 = Get next card of type LCARD. Must already be accessing
!             card of this type.
! LCARD - The 4-character designator for the card type requested
! NUMDB - The number of the database section in the OBSFIL relative
!         to the start of OBSFIL
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IERR
      CHARACTER*70 JBUF
!
! IERR - Error return flag:
!        0 = ok
!        1 = No more cards of the requested type
!       -1 = file not found
!       -2 = read failed
!       -3 = card type not found
!       -4 = call for next card without calling for first card
!       -5 = wrong ICONT specified
!       -6 = Invalid NUMDB specified
! JBUF - 70-character buffer to hold card
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'namfl.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: readnamfil,opennamfil
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 LENGTH, TRIMLEN
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!   JWR  860408  Created
!
! 5.  GETCARD PROGRAM STRUCTURE
!
!     Verify ICONT
!
      IF ( ICONT .NE. 0   .AND.  ICONT .NE. 1 ) THEN
           IERR = -5
           RETURN
      END IF
!
! --- See if file is open. If not open it.
!
      IF ( ISTATUS .EQ. 0 ) THEN
           CALL OPENNAMFIL()
      END iF
!
! --- Verify NUMDB.
!
      IF ( NUMDB .LT. 0  .OR.  NUMDB .GT. IDBS ) THEN
           IERR = -6
           RETURN
      END IF
!
      IF ( ICONT .EQ. 1 ) THEN ! First card of type requested
!
! -------- Get the 1st card for section NUMDB
!
           IREC = IFIRST(NUMDB)
           CALL READNAMFIL ( IERR )
           IF ( IERR .NE. 0 ) THEN
                WRITE ( 6, * ) 'GETCARD >>'//KBUF//'<< '
                RETURN
           END IF
           IACTSEC = NUMDB
!
           IF ( IREC == 1 .AND. ILAST_I2 .EQ. 9999 ) THEN
                READ ( UNIT=KBUF(61:68), FMT='(I8)' ) ILAST
              ELSE IF ( IREC == 1 ) THEN
                ILAST = ILAST_I2
           END IF
!
! -------- Loop to find the card type requested.
!
           DO WHILE ( LHOLD .NE. LCARD )
              IF ( IREC .EQ. ILAST ) THEN
                   IERR = -3
                   RETURN
              END IF
              IREC = IREC + 1
              CALL READNAMFIL ( IERR ) 
              IF ( IERR .NE. 0 ) THEN
                   WRITE ( 6, * ) 'GETCARD ILAST= ', ILAST, ' IREC= ', IREC, ' KBUF=>>'//KBUF//'<< '
                   RETURN
              END IF
              IF ( 'INIT' .EQ. LHOLD ) THEN !read whole section and no card
                   IERR = -7
                   RETURN
              END IF
           END DO
!
! -------- Good: found the right card
!
           JBUF = KBUF
           IERR = 0
           RETURN
      END IF              ! First card of type requested
!
! --- Next card requested
! --- Verity that LCARD and NUMDB are currently active.
!
      IF ( LHOLD .NE. LCARD  .OR.  NUMDB .NE. IACTSEC ) THEN
           IERR = -4
           RETURN
      END IF
!
      IF ( IREC .EQ. ILAST ) THEN ! We come to the last valid record in the file.
           IERR = 1
           RETURN
      END IF
!
      IREC = IREC + 1
      CALL READNAMFIL ( IERR )
      IF ( IERR .NE. 0 ) THEN
           WRITE ( 6, * ) 'GETCARD >>'//KBUF//'<< '
           RETURN
      END IF
      IF ( IERR .EQ. 0  .AND.  LHOLD .EQ. LCARD ) JBUF = KBUF
      IF ( LHOLD .NE. LCARD ) IERR = 1
!
      RETURN
      END  !#!  GETCARD  #!#
