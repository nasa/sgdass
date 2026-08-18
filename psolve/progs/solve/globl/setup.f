      SUBROUTINE SETUP()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETUP PROGRAM SPECIFICATION
!
! 1.1 Deals with the naming and creating of arcfiles.
!
! 1.2 REFERENCES:
!
! 2.  SETUP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
      INCLUDE 'glocm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: globl
!       CALLED  SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*1 MODE
      LOGICAL*2   REALLY_CREATE, KBIT
!
! 4.  HISTORY
!
! 5.  SETUP PROGRAM STRUCTURE
!
      PURGARC=.FALSE.
!
! --- Deal with arc files only if this is a batch but not an independent
! --- solution
!
      IF ( ISLTY2 .NE. 'I'    .AND.   KBATCH ) THEN
           MODE='N'
           IF ( ISOLU .EQ. 1   .AND. RECVR .NE. 1  .AND..NOT. &
     &          KBIT(PRE_IBATCH, INT2(8))       ) MODE='U'
           IF ( KPERMARC  .AND.  RECVR .NE. 1 ) MODE='P'
           RECVR=0
           REALLY_CREATE = .TRUE.
!
! -------- In forward run of global solution in fast mode we are not going to
! -------- create arc-file (it will be correctly created in ARCPE), but we
! -------- will do all things except real creation of the file.
!
           IF ( ISOLU .EQ. 0  .AND.  FAST_MODE .EQ. F__B1B3D ) REALLY_CREATE = &
     &          .FALSE.
           CALL CREATE_ARCF ( SAVAF, PURGARC, IARCNM, ARCDIR, NPARAM, &
     &          KBIT(PRE_IBATCH, INT2(8)), ARCTHER, MODE, REALLY_CREATE, FS_FULL )
!
! -------- Closing arc-file (if it is necessary)
!
           IF ( REALLY_CREATE ) CALL ACS_ARCFIL  ( SAVAF, KBIT( PRE_IBATCH, &
     &          INT2(8)),'C' )
      ENDIF
!
      PURGARC = PURGARC .AND. ISOLU.EQ.1
      RETURN
      END  !#!  SETUP  #!#
