      SUBROUTINE ADDSTR_F ( STR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 IL
      CHARACTER STR*(*)
!
      IL = MAX( 1, MIN ( 119, LEN(STR) ) )
      CALL ADDSTR_MN ( STR(1:IL)//CHAR(0) )
      RETURN
      END  !#!  ADDSTR_F  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MENUL_F ( STR, LOGI, IYPOS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      character*(*) str
      LOGICAL*2 logi
      integer*4 iypos
!
      integer*4 ix
!
      call addstr_f(str )
      call getxy_mn(ix,iypos )
      IF(logi) then
        call addstr_f("T" )
      else
        call addstr_f("F" )
      endif
      call nl_mn()
      return
      end
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MENUF_F ( STR, NAME, IXPOS, IYPOS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      character*(*) str,name
      integer*4 ixpos,iypos
      integer*2 trimlen
!
      call addstr_f(str )
      call getxy_mn(ixpos,iypos )
      call addstr_f(name(:trimlen(name)) )
      call nl_mn()
      return
      end
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GETSTR_F ( STR )
! ************************************************************************
! *                                                                      *
! *   Routine GETSTR_F provides an interface to GETSTR_MN.               *
! *                                                                      *
! *   Leonid Petrov 2004.04.14 -- Fixed a bug: the previous version      *
! *                               returned null-terminated string, which *
! *                               does not conform Fortran strandard.    *
! *                               It in turn caused failure of Fortran   *
! *                               I/O library for type conversion.       *
! *                                                                      *
! *  ### 14-APR-2004   GETSTR_F    v1.2 (c)  L. Petrov  29-OCT-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      CHARACTER  STR*(*) 
      CHARACTER  STR_TEMP*256
      INTEGER*4  ILN, IL, J1
      EXTERNAL   ILEN
      INTEGER*4  GETSTR_MN, ILEN
!
      CALL CLRCH ( STR ) 
      ILN = GETSTR_MN ( STR_TEMP )
!
! --- Check all characetrs. We should correctly process <BACKSPACE> and 
! --- replace CHAR(0) with blank
!
      CALL CLRCH ( STR )
      IF ( ILN .GT. 0 ) THEN
           IL = 0
           DO 410 J1=1,ILN
              IF ( STR_TEMP(J1:J1) .EQ. CHAR(8) ) THEN
!
! ---------------- This character was <BACKSPACE>
!
                   IL = IL - 1
                   IF ( IL .LE. 0 ) IL = 0
                ELSE 
                   IL = IL + 1
                   IF ( IL .LE. LEN(STR) ) THEN
                        IF ( STR_TEMP(J1:J1) .EQ. CHAR(0) ) THEN
                             STR(IL:IL) = ' '
                           ELSE 
                             STR(IL:IL) = STR_TEMP(J1:J1)
                        END IF
                   END IF
              END IF
 410       CONTINUE 
      END IF
!
      RETURN
      END   !#!  GETSTR_F  #!#
