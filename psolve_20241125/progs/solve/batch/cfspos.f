      SUBROUTINE CFSPOS(RECORD)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CFSPOS PROGRAM SPECIFICATION
!
! 1.1 Low-level I/O utility to set internal position in control file.
!
! 1.2 REFERENCES:
!
! 2.  CFSPOS INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*4 RECORD
!
! RECORD - Record number at which to position file
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'batcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: rstor
!       CALLED SUBROUTINES: Utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4   IOS
      CHARACTER*1 DUM
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb  961010  Trap rewind errors.
!   pet  990418  Improved error messages
!
! 5.  CFSPOS PROGRAM STRUCTURE
!
!CCCC
!
! --- Start by rewinding to the beginning of the file
!
      REWIND ( 92, IOSTAT=IOS )
      CALL FERR ( INT2(IOS), 'BATCH(cfspos) rewinding control file', INT2(0), &
     &           INT2(0) )
      SAVREC=1
!
! --- Now count up to the desired record
!
      DO WHILE ( SAVREC .LT. RECORD )
         READ ( 92, '(A)', IOSTAT=IOS ) DUM
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) ' SAVREC =',SAVREC,' RECORD = ',RECORD
         END IF
         CALL FERR ( INT2(IOS), 'BATCH(cfspos) positioning control file', &
     &               INT2(0), INT2(0) )
         SAVREC=SAVREC+1
      ENDDO
      KGOT=.FALSE.
      KEOF=.FALSE.
!
      RETURN
      END  !#!  CFSPOS  #!#
