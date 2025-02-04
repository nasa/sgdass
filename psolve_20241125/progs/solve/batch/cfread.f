      INTEGER*2 FUNCTION CFREAD(STRING)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CFREAD PROGRAM SPECIFICATION
!
! 1.1 Low-level I/O utility to read the control file.  The effect of
!     CFUNRD is accounted for. Comment lines are skipped.
!
! 1.2 REFERENCES:
!
! 2.  CFREAD INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) STRING
!
! STRING - String read from control file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'batcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcset,tmexc,ctrlfl,garc,gcalib,gcarry,gconst,
!                            gdata,getavl,getgrp,gflags,gmap,gnut,gorisp,
!                            goutpt,graosp,gsetup,gsrcsp,gstasp,gsuprs,
!                            gtclost,gtelev,gtlst,gtlst_st_ca,gtwvm,gvel,
!                            gversp
!
!       CALLED SUBROUTINES: Utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  TRIMLEN
      INTEGER*4  IOS
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  CFREAD PROGRAM STRUCTURE
!
! Check for a previously "unread" record
!
      IF(KGOT) THEN
        STRING=INTERN
        CFREAD=LENRD
        KGOT=.FALSE.
!
! Check for end of file condition
!
      ELSE IF (KEOF) THEN
        CFREAD=-1
        STRING=' '
      ELSE
        STRING(1:1)='*'
!
! Skip over comment lines
!
        DO WHILE ( (STRING(1:1) .EQ. '*' .or. STRING.EQ.' ' ) .AND. .NOT.KEOF )
           SAVREC=SAVREC+1
           READ ( 92, '(A)', IOSTAT=IOS, END=99 ) STRING
           CALL FERR( INT2(IOS), 'READING CONTROL FILE', INT2(0), INT2(0) )
           CFREAD=TRIMLEN(STRING)
        ENDDO
      ENDIF
      RETURN
!
99    CONTINUE
!
! We get here if we have reached the end of file
!
      KEOF=.TRUE.
      CFREAD=-1
      RETURN
      END
