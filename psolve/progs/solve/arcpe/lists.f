      SUBROUTINE LISTS(IX1T3,NPARM1,NPARM3,NGTA,NLTA,STACM, &
     &     lparm_out)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  LISTS PROGRAM SPECIFICATION
!
! 1.1  Build list of parameters for the current arc.
!
! 1.2 REFERENCES:
!
! 2.  LISTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      LOGICAL*2 STACM
!
! STACM - True if this arc's commons are to be saved
!
! 2.3 OUTPUT Variables:
!
      INTEGER*4 IX1T3(*), NPARM1, NPARM3
      INTEGER*4  NGTA, NLTA
!
! NGTA - Number of globals, this arc
! NLTA - Number of locals, this arc
! IX1T3 - Cross reference between IPARM1 and IPARM3
! NPARM1 - Number of parameters in SOLVE parameter list
! NPARM3 - Number of parameters in final parameter list
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INTEGER*2 IPARM1(10,M_GPA), IPARM2(10,M_GPA), IPARM3(10,M_GPA)
      CHARACTER*20 LPARM_OUT(M_GPA)
      COMMON / PARAM / IPARM1,IPARM2,IPARM3
      character*20 lparm1(M_GPA), lparm2(M_GPA), lparm3(M_GPA)
      equivalence (iparm1,lparm1),(iparm2,lparm2),(iparm3,lparm3)
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcpe
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IWDS,ISTS,NPARM2
      integer*2 i
      integer*2 istr_len/20/
      logical*2 kglob
!
      DATA ISTS/1/,IWDS/10/
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jmg  960610  Use characters, not hollerith
!
! 5.  LISTS PROGRAM STRUCTURE
!
      CALL USE_GLBFIL('OR' )
      old_user_part=num_user_part
      kglob = kglobonly
!
!  HANDLE COMMONS
!
      CALL USE_COMMON('ORC' )
      CALL SOCOM_EXT()
      CALL USE_PARFIL('ORC' )
!
!      CALL PARMS (ISTS, IPARM1, IWDS, M_GPA, NPARM1, .TRUE.)
      call get_NAMES( Lparm1, istr_len, M_GPA, NPARM1, TRUE__L2, &
     &     FALSE__L2 )
! make a copy to export
      do i=1,nparm1
        lparm_out(i)=lparm1(i)
      end do
      CALL USE_GLBFIL_2('R' )
      CALL DEPAR()
!
!      CALL PARMS(ISTS,IPARM2,IWDS,M_GPA,NPARM2,.TRUE.)
      call get_NAMES( Lparm2, istr_len, M_GPA, NPARM2, TRUE__L2, &
     &     FALSE__L2 )
      CALL CUPARM( NPARM1, LPARM1, NPARM2, LPARM2, NPARM3, LPARM3, NGTA, NLTA, &
     &     INT2(1), M_GPA )
      CALL CXEPAR(LPARM1,IX1T3,NPARM1,LPARM3,NPARM3 )
!
      IGLBLS=NGTA
      IARCS=NLTA
      kglobonly=kglob
      num_user_part=old_user_part
      CALL USE_GLBFIL('WC' )
!
!
!  IF DOING CORRELATIONS SAVE COMMONS
!
      IF(STACM) THEN
        CALL USE_COMMON('ORC' )
        CALL USE_PARFIL('ORC' )
        CALL ACS_ARCFIL(SAVAF,STACM,'O' )
        CALL USE_ARCF_COM('W' )
      ENDIF
      RETURN
      END
