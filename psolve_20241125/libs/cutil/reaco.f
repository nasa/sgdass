      SUBROUTINE REACO ( IUNT, LCTYP, LCNUM, LDISP, LCORC, LCSTA, LCFAC, CBUF )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  REACO PROGRAM SPECIFICATION
!
! 1.1 REad a record of the CORFIL, ignoring blank lines, and comment lines
!     The best way to understand the way this routine works
!     and the things it returns is probably to look at
!     a CORFIL.
!
! 1.2 REFERENCES:
!
! 2.  REACO INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
!     Note: on some of the calls to this sub, lctyp is a hidden input
!           variable.  Whenever reaco finds a section header record, it
!           sets lctyp to the new section number, then reads a new record,
!           interpreting the record according to the format for the current
!           section.  Until reaco finds a new section header, it will
!           read a single record, and interpret it according to the last
!           lctyp value.  So lctyp must stay the same between calls to reaco.
!
      INTEGER*2 IUNT
!
! IUNT - Unit that CORFIL is connected to
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LCTYP, LCNUM, LDISP(8), LCORC(7), LCSTA(7), LCFAC(2)
      CHARACTER CBUF*(*)
!
! LCTYP - Section type
! LCNUM - Number of record within section type
! LDISP - Display text for this cal/contrib OR record #'s for default
!         hierarchy in 1st 4 elements (the default hierarchy is just
!         a list of calibrations/contributions which should be applied
!         to an unanlyzed data base OR additional meaning for one of
!         the contribution sections, 41 or 42, (see below)
! LCORC - LCODE for the values for this calib/contrib
! LCSTA - LCODE for station list for this cal/contrib
! LCFAC - Conversion factor for sta1, sta2
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
      INTEGER*2  I, ITEMP,IDUM
      INTEGER*4  IOS
      CHARACTER ERRSTR*200
      CHARACTER*7 CDUM
!
! CBUF - Buffer for reading record into
! I - Loop index
! IOS - IOSTAT return from READ
! ITEMP - Temporary holder for checking for section header number
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   IS   851231  Created
!   JWR  881110  Added error checking on read statements
!   AEE  910515  Enhanced error messages written to the error file.
!   KDB  910715  Revised to read new corfil (flyby calibs in separate
!                section).
!   KDB  911023  Allow comment lines under the new scheme.
!   pet  1999.11.10  Improved comments
!   pet  1999.11.11  Added clearance of the CBUF before reading
!
! 5.  REACO PROGRAM STRUCTURE
!
  100 CONTINUE
      CALL CLRCH ( CBUF )
      READ (IUNT, '(A)', END=900, IOSTAT=IOS, ERR=101 ) CBUF
  101 CONTINUE
      IF ( IOS .GT. 0 ) THEN
           WRITE ( ERRSTR, "( &
     &            'Error encountered while read CORFIL. IOS =',I5)" ) IOS
           CALL FERR ( INT2(205), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
      READ ( CBUF, '(I5)',IOSTAT=IOS, ERR=102 ) ITEMP
  102 CONTINUE
      IF ( IOS .GT. 0 ) THEN
           WRITE ( ERRSTR, "( &
     &             'Error encountered decoding CORFIL card. IOS =',I5)" ) IOS
           CALL FERR ( INT2(206), ERRSTR, INT2(0), INT2(0) )
           WRITE ( ERRSTR, "('Wring line is ',A)") CBUF
           CALL FERR ( INT2(206), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
! --- Skip blank lines (0) and comments (2000)
!
      IF ( ITEMP .EQ. 0 .OR. ITEMP .EQ. 2000) GOTO 100
!
! --- Check for section heading
!
      IF ( ITEMP .EQ. 1000 ) THEN
           READ ( CBUF, *, IOSTAT=IOS, ERR=105 ) IDUM, CDUM, LCTYP
  105      CONTINUE
           IF ( IOS .GT. 0 ) THEN
                WRITE ( ERRSTR, "( &
     &                'Error encountered decoding CORFIL card. IOS =',I5)") IOS
                CALL FERR ( INT2(209), ERRSTR, INT2(0), INT2(0) )
                WRITE ( errstr, "('Wrong line is ',A)" ) CBUF
                CALL FERR ( INT2(209), ERRSTR, INT2(0), INT2(0) )
           ENDIF
           GOTO 100
      END IF
!
! --- Handle records from the sections which contain a list of numbers
!
      IF ( LCTYP .EQ. 11  .OR.  LCTYP .EQ. 21  .OR. &
     &     LCTYP .EQ. 41  .OR.  LCTYP .EQ. 51       ) THEN
           RETURN
         ELSE IF ( LCTYP .EQ. 42 ) THEN
!
! -------- Handle section 42, which now consists of strings of up to
! -------- eight characters ( used to consist of a single number)
!
           READ ( CBUF, 1001, IOSTAT=IOS, ERR=103 ) LCNUM, (LDISP(I), I=1,4)
 1001      FORMAT ( I5, 2X, 4A2 )
 103       CONTINUE
           IF ( IOS .GT. 0 ) THEN
                WRITE ( errstr, "( &
     &                 'Error decoding CORFIL number card. IOS =',I5)" ) IOS
                CALL FERR( INT2(207), ERRSTR, INT2(0), INT2(0) )
                WRITE ( ERRSTR, "('Wring line is ',A)" ) CBUF
                CALL FERR ( INT2(207), ERRSTR, INT2(0), INT2(0) )
           ENDIF
         ELSE
!
! -------- Handle standard case (info lines for calibs/contribs)
!
           READ ( CBUF, 2000, IOSTAT=IOS, ERR=104) &
     &                  LCNUM, (LDISP(I),I=1,8), &
     &                  (LCORC(I), I=1,4), &
     &                  (LCSTA(I), I=1,4), (LCFAC(I),I=1,2)
 2000      FORMAT (I5, 2X, 8A2, 2X, 4A2, T43, 4A2, T58, I2, 2X, I2)
 104       CONTINUE
           IF ( IOS .GT. 0 ) THEN
                WRITE ( ERRSTR, "( &
     &               'Error encourntered decoding CORF line IOS =',I5 )" ) IOS
                CALL FERR ( INT2(208), ERRSTR, INT2(0), INT2(0) )
                WRITE ( ERRSTR, "('Wrong is ',A75)") CBUF
                CALL FERR ( INT2(208), ERRSTR, INT2(0), INT2(0) )
           ENDIF
      END IF
!
! --- Return this record
!
      RETURN
!
! --- End of file
!
  900 CONTINUE
      LCTYP = 99
      RETURN
      END   !#!  REACO  #!#
