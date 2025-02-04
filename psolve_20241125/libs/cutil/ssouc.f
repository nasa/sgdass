      SUBROUTINE SSOUC ( LSONAM, SUBRD, STRDIF, LNAME, KFBDSP, NVSTARC )
      IMPLICIT NONE
!
! 1.  SSOUC PROGRAM SPECIFICATION
!
! 1.1 Call subroutines GSOUC and DSOUC to read a source position
!     file and to calculate source position differences.
!
! 1.2 REFERENCES:
!
! 2.  SSOUC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2 KFBDSP
      CHARACTER*(*) LNAME
!
! KFBDSP - TRUE if flyby info is to be displayed
! LNAME - Name of source mapping file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 LSONAM(4,*)
      REAL*8    SUBRD(2,*),STRDIF(2,*),NVSTARC(3,*)
      INTEGER*4  J1
!
! LSONAM - Array of source names
! NVSTARC - New source positions, from mod file (RA,DEC in radians)
! STRDIF - Source position differences
! SUBRD - Source positons read from mapping file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'bindisp.i'
      INCLUDE 'flyby.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: gsouc,dsouc
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 NSTAR
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*4, EXTERNAL :: CHECK_STABIT
!
! 4. HISTORY
!   WHO   WHEN        WHAT
!   pet   2003.08.15  Re-wrote. Enabled support of keeping substituion arrays &
!                     in flyby.i, so the file is read only once
!
! 5.  SSOUC PROGRAM STRUCTURE
!
!
! --- Get substitutions
!
      IF ( SRC_SUBSTITUTE_INIT ) THEN
!
! -------- Aaa. Source substitution file has been already read. Merely copy it
! -------- from the save place
!
           NSTAR = NSTAR_KEEP
           IF ( NSTAR_KEEP .GT. 0 ) THEN
                CALL LIB$MOVC3 ( 8*INT4(NSTAR_KEEP),   LSONAM_KEEP, LSONAM )
                CALL LIB$MOVC3 ( 2*8*INT4(NSTAR_KEEP), SUBRD_KEEP,  SUBRD  )
           END IF
         ELSE
!
! -------- Aga. We did not yet read source substitution file. Let's do it
!
           CALL GSOUC ( LSONAM, NSTAR, SUBRD, LNAME, KFBDSP )
!
! -------- Set the flag that we have done it
!
           SRC_SUBSTITUTE_INIT = .TRUE.
!
! -------- Keep arrays which we have read
!
           NSTAR_KEEP = NSTAR
           IF ( NSTAR_KEEP .GT. 0 ) THEN
                CALL LIB$MOVC3 ( 8*INT4(NSTAR_KEEP),   LSONAM, LSONAM_KEEP )
                CALL LIB$MOVC3 ( 2*8*INT4(NSTAR_KEEP), SUBRD,  SUBRD_KEEP )
           END IF
      END IF
!
! --- Do substitutions
!
!        do 410 j1=1,numstr
!           write ( 6, 110 ) istrn_chr(j1), vstarc(1,j1), vstarc(2,j1)
! 110       format ( 'Source: ',A,' alp: ', F18.12,' del: ', f18.12 )
! 410    continue
      CALL DSOUC ( LNAME, LSONAM, NSTAR, SUBRD, NUMSTR, ISTRN, VSTARC, &
     &             STRDIF, FLYBY_WARNING, NVSTARC )
!
      RETURN
      END  !#!  SSOUC  #!#
