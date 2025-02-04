      SUBROUTINE PUTCARD(NUMDB,LCARD,ICONT,JBUF,IERR)
      implicit none
!
! 1.  PUTCARD PROGRAM SPECIFICATION
!
! 1.1 Put card images into the NAMFIL, opening NAMFIL
!     if it is not already open.
!
! 1.2 REFERENCES:
!
! 2.  PUTCARD INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NUMDB,ICONT
      CHARACTER*4 LCARD
      CHARACTER*70 JBUF
!
! NUMDB - The number of the data base section in the OBSFIL
!         relative to the start of OBSFIL.
! LCARD - The 4-character string designator for the card type
!         to be written.
! ICONT - Function controller.
!         1 = Put 1st card of type LCARD for data base section
!             number NUMDB.
!         0 = Put next card of type LCARD. Must already be accessing
!             card of this type.
!         2 = Put this card as a new card at the bottom of NAMFIL.
!             NOTE!! In this mode PUTCARD will look at the the card
!             type identifier in JBUF and if it is 'INIT' it will
!             increment NUMDB by one.
!         3 = Reinitialize NANFIL so that it contains no cards.
!             Note. Functions 2 and 3 are to be used by SDBH only.
!         4 = Rewrite last card read.
! JBUF  - 70-Character string buffer with the card image.
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IERR
!
! IERR  - -2 Write fail
!         -3 Card type not found
!         -4 Call writing next card without writting 1st card of
!            specified type
!         -5 Wrong ICONT specified
!         -6 Invalid NUMBD specified
!         -7 LCARD and the 1st 4 characters of JBUF don't agree
!         -8 Tried to write beyond in the last card in the ICONT = 0
!            mode.
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'namfl.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: opennamfil,readnamfil,writenamfil
!
! 3.  LOCAL VARIABLES
      integer*2 i
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  860421  Add rewrite last card capability
!   JWR  890220  Blank out the 1st 150 card on reinitialization
!
! 5.  PUTCARD PROGRAM STRUCTURE
!
!     See if file is open. If not open it.
      IF(ISTATUS.EQ.0) then
        CALL OPENNAMFIL()
      End If
!
!  Check for valid function request
!
      IF(ICONT.LT.0  .or. ICONT.GT.4) then
        IERR = -5
        RETURN
      End If
!
!     Handle the rewind case
!
      IF(ICONT.EQ.3) then   !Rewind NANFIL
        KSTATUS = 1
        DO I = 1,16
          IQQ(I) = 0
        END DO
        ILAST = 1
        IERR = 0
        RETURN
      END IF
!
!     Make certain that LCARD and JBUF are consistent
!
      IF(LCARD.ne.JBUF(1:4)) THEN
        IERR = -7
        RETURN
      End If
!
!     Handle the write at the bottom case
!
      IF(ICONT.EQ.2) then   !Write at the bottom
        IREC  = ILAST + 1
        ILAST = IREC
        KSTATUS = 1
        KBUF = JBUF
        CALL WRITENAMFIL(IERR)
        IF(IERR.ne.0) then
          IERR = -2
        End If
!
!       Do special case handling if this is a section initialization card
!
        IF(JBUF(1:4) .EQ. 'INIT') then
          IDBS = IDBS + 1
          IFIRST(IDBS) = IREC
          End If
!
        RETURN
      End If
!
!     We're down to the rewriting cases, so see if NUMDB is ok
!
        IF(NUMDB.LT.0  .or. NUMDB.GT.IDBS) then
          IERR = -6
          RETURN
        End If
!
!     Handle the rewrite the 1st card of a type case
!
      IF(ICONT.EQ.1) then !First card of type requested
!
!       Get the 1st card for section NUMDB
!
        IF ( NUMDB .LE. 0 ) NUMDB = 1
        IREC = IFIRST(NUMDB)
        CALL READNAMFIL(IERR)
        IF(IERR.ne.0) RETURN
        IACTSEC = NUMDB
!
!       See if this is the card we are looking for.
!
        DO WHILE (LHOLD.ne.LCARD)
          IF(IREC.EQ.ILAST) then
            IERR = -3
            RETURN
          End If
          IREC = IREC +1
          CALL READNAMFIL(IERR)
          IF(IERR.ne.0) RETURN
          IF('INIT' .EQ. LHOLD) then !read whole section and no card
            IERR = -3
            RETURN
          End If
        End Do
!
!       Good found the right card
!
        KBUF = JBUF
        CALL WRITENAMFIL(IERR)
        IF(IERR.ne.0) IERR = -2
        RETURN
      End If              !First card of type requested
!
!     Rewrite next card or last card read requested.
!
      IF(LHOLD.ne.LCARD .or. NUMDB.ne.IACTSEC) then
        IERR = -4
        RETURN
        End If
!
      IF(IREC.EQ.ILAST .and. ICONT.eq.0 ) then
        IERR = -8
        RETURN
        End If
!
      IF(ICONT.EQ.0) IREC = IREC + 1
      CALL READNAMFIL(IERR)
      IF(IERR.ne.0) then
        IERR = -2
        RETURN
      END IF
      IF ( LHOLD .NE. LCARD ) then
           IERR = -7
           RETURN
      END IF
!
!     Everything is ok to replace the card
!
      KBUF = JBUF
      CALL WRITENAMFIL(IERR)
      RETURN
      END
