      SUBROUTINE CLOSENAMFIL()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CLOSENAMFIL PROGRAM SPECIFICATION
!
! 1.1 Close NAMFIL, rewriting record 1 if that is necessary
!
! 1.2 REFERENCES:
!
! 2.  CLOSENAMFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'namfl.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: writenamfil,ferr
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   IERR
      INTEGER*4   I, IWORDS
      INTEGER*4   IERR4
      CHARACTER*4 Cdummy
      INTEGER*2   IARG_I2
      INTEGER*4   INT4
      INT4(IARG_I2) = INT(IARG_I2,KIND=4)

! CDUMMY - Temporary buffer to hold character version of an integer
! I - Loop index
! IERR - Error return from writenamfil
! IWORDS - Number of words needed to fill out current block
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  860411  Created
!
! 5.  CLOSENAMFIL PROGRAM STRUCTURE
!
!     See if NAMFIL has been rewound or if wholly new records have
!     been written at the bottom. If so rewrite record 1.
!
      IF(KSTATUS.eq.1) then
!
!  Append enough records so that namfil extends to the end of the
!  current block (at least).
!
        IWORDS=BLOCK_WORDS-MOD(ILAST*JNAMREC_WORDS,INT4(BLOCK_WORDS))
        KBUF=' '
        DO I=1,(IWORDS+JNAMREC_WORDS-1)/JNAMREC_WORDS
          IREC=ILAST+I
          CALL WRITENAMFIL(IERR )
          CALL FERR( IERR, 'writing '//NNAME, INT2(0), INT2(0) )
        ENDDO
        KBUF(1:4) = 'POIN'
        DO I = 1,16
          Write(Cdummy,'(I4)') IQQ(I)
          KBUF(1+I*4:4+I*4) = Cdummy !IntToDecimal(IQQ(I))
        END DO
        IREC = 1
        CALL WRITENAMFIL(IERR )
        CALL FERR( IERR, 'writing '//NNAME, INT2(0), INT2(0) )
      END IF
!
      CLOSE(UNITNAM,IOSTAT=IERR4)
      IERR = IERR4
      CALL FERR( IERR, 'closing '//NNAME, INT2(0), INT2(0) )
      KSTATUS = 0
      ISTATUS = 0
      RETURN
      END
