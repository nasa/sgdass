      FUNCTION GTCVR(TOKEN,STRINK,SPACE)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GTCVR PROGRAM SPECIFICATION
!
! 1.1 Parse COVARIANCE section of control file.
!
! 1.2 REFERENCES:
!
! 2.  GTCVR INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*1 SPACE
      CHARACTER*(*) STRINK
!
! STRINK - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) TOKEN
      LOGICAL*2     GTCVR
!
! TOKEN - Individual tokens pulled out of STRINK
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'ba2cm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: goutpt
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  GTCVR PROGRAM STRUCTURE
!
! Pull first token from input string and determine whether we
!  are to output covariances
!
      CALL SPLITSTRING(STRINK,TOKEN,STRINK )
!
!
      IF(TOKEN.EQ.'YES') THEN
          KCORL =.TRUE.
          GTCVR = KCORL
      ELSE IF(TOKEN.EQ.'NO') THEN
          KCORL =.FALSE.
          GTCVR = KCORL
      ELSE
        CALL FERR( INT2(9010), 'ILLEGAL CARRY DEFAULT '//TOKEN(1:16), INT2(0), &
     &       INT2(0) )
        GTCVR = .FALSE.
      ENDIF
!
! If not, then we are finished
!
      If (.not.KCORL) then
          RETURN
      Else
!
!
!   get state field:  will determine type of processing action taken
!   in the covariance calculating program CVRNC
!
          Call SplitString(STRINK,TOKEN,STRINK)
          IDBNAME(1:10) = TOKEN(1:10)
!
!
!   if not of form 'BY_ARC' or 'ALL', then get i-arc dbase version
!
          If ((TOKEN(1:6) .ne. 'BY_ARC') .and. &
     &        (TOKEN(1:3) .ne. 'CGM') .and. &
     &        (TOKEN(1:3) .ne. 'ALL')) then
              Call SplitString(STRINK,TOKEN,STRINK)
              IDBNAME(11:12) = TOKEN(1:2) !append i-arc dbase version
          End if
!
!
!   get first parameter group type: EOP, STAtions, SOUrces, ALL, NUTation
!
          Call SplitString(STRINK,TOKEN,STRINK)
          QICOV = TOKEN
          if (qicov.ne.'STA'.and.qicov.ne.'NUT'.and.qicov.ne.'EOP'.and. &
     &        qicov.ne.'ALL'.and.qicov.ne.'SOU') then
            call ferr( INT2(723), 'Incorrect syntax in COVARIANCES entry', &
     &           INT2(0), INT2(0) )
          endif
      End if
!
      Return
      End
