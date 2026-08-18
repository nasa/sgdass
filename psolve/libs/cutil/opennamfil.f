      SUBROUTINE OPENNAMFIL()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  OPENNAMFIL PROGRAM SPECIFICATION
!
! 1.1 Open the NAMFIL and get the housekeeping information
!     from the first card.  NOTE: the file pathname in DIR
!     specifying the directory and subdirectories where the
!     NAMFxx is located must not contain trailing blanks.
!
! 1.2 REFERENCES:
!
! 2.  OPENNAMFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'namfl.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: readnamfil,ferr
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*4  IHOLD
      character*32 cdum
      INTEGER*2    LENGTH,I,LEN, IERR
      INTEGER*4    IOS
!
! I - Loop index
! IERR - Error return from READNAMFIL
! IOS - IOSTAT return from OPEN,CLOSE
! IHOLD - String to hold one 4-character field from KBUF
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JWR  860408  Created
!
! 5.  OPENNAMFIL PROGRAM STRUCTURE
!
!     Open NAMFIL
!
      NNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'NAMF'//PRE_LETRS
      Open(UNIT=UNITNAM,FILE=NNAME,IOSTAT=ios,ACCESS='DIRECT', &
     &     RECL=JNAMREC_WORDS*2)
      CALL FERR( INT2(IOS), 'opening '//NNAME, INT2(0), INT2(0) )
      ISTATUS = 1
!
! --- Get the information from the POINTER card
!
      IREC = 1
      CALL READNAMFIL(IERR )
      CALL FERR( IERR, 'reading '//NNAME, INT2(0), INT2(0) )
!
! --- Painfully get info from 1st card without the formatter
!
      DO I=1,16
         IHOLD = KBUF(1+I*4:4+I*4)
         READ ( IHOLD, '(I5)', IOSTAT=IOS ) IQQ(I)
         IF   ( IOS .NE. 0 ) CALL FERR( INT2(IOS), "Reading NAMFIL", INT2(0), INT2(0) )
      END DO
      IF ( ILAST_I2 .EQ. 9999 ) THEN
           READ ( UNIT=KBUF(61:68), FMT='(I8)' ) ILAST
         ELSE 
           ILAST = ILAST_I2
      END IF
!
      IF ( ( LHOLD .NE. 'POIN' .AND. LHOLD .NE. '    ' ) .OR. &
     &       IDBS  .GT. 15 ) THEN
           IERR = -4
           CLOSE ( UNITNAM, IOSTAT=IOS )
           CALL FERR ( INT2(IOS), 'closing '//NNAME, INT2(0), INT2(0) )
           RETURN
      END IF
!
      RETURN
      END
