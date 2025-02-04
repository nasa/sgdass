      SUBROUTINE LISTS_ARC ( IX1T3, NPARM1, NPARM3, NGTA, NLTA, STACM, &
     &                       LPARM_OUT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  LISTS_ARC PROGRAM SPECIFICATION
!
! 1.1  Build list of parameters for the current arc.
!
! 1.2 REFERENCES:
!
! 2.  LISTS_ARC INTERFACE
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
      INTEGER*4 NGTA, NLTA
!
! NGTA - Number of globals, this arc
! NLTA - Number of locals, this arc
! IX1T3 - Cross reference between IPARM1 and IPARM3
! NPARM1 - Number of parameters in SOLVE parameter list
! NPARM3 - Number of parameters in final parameter list
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INTEGER*2 IPARM1(10,M_GPA), IPARM2(10,M_GPA), IPARM3(10,M_GPA)
      CHARACTER*20 LPARM_OUT(M_GPA)
      COMMON / PARAM / IPARM1,IPARM2,IPARM3
      CHARACTER  LPARM22(M_GPA)*20
      CHARACTER*20 LPARM1(M_GPA), LPARM2(M_GPA), LPARM3(M_GPA)
      EQUIVALENCE (IPARM1,LPARM1), (IPARM2,LPARM2), (IPARM3,LPARM3)
      INTEGER*4  K1, K2, K3
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: arcpe
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 NPARM2, NPARM2_NEW, I, J1
      INTEGER*2 ISTR_LEN / 20 /
      LOGICAL*2 KGLOB
      LOGICAL*4    FL_EERM_SAVE 
      LOGICAL*2    L2_TRUE, L2_FALSE
      PARAMETER  ( L2_TRUE  = .TRUE.  )
      PARAMETER  ( L2_FALSE = .FALSE. )
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   jmg  960610      Use characters, not hollerith
!   pet  971105      Corrected CRL_NOV97 bug (attempt to write arcfile before
!                    its creation in the case of calcualtion of correlations) --
!                    I transferred this place of code to /arcpe/arcpe.f
!   pet  2007.08.10  Added support of source structure admittance parameters
!
! 5.  LISTS_ARC PROGRAM STRUCTURE
!
      CALL USE_GLBFIL ( 'ORC' )
      OLD_USER_PART = NUM_USER_PART
      KGLOB = KGLOBONLY
!
! --- Handle commons
!
      CALL USE_COMMON ( 'ORC' )
      CALL SOCOM_EXT()
      CALL USE_PARFIL ( 'ORC' )
!
! --- NPARM1, LPARM1 -- local parameters only
!
      KGLOBONLY = .FALSE.
      CALL GET_NAMES ( LPARM1, ISTR_LEN, M_GPA, NPARM1, L2_TRUE, L2_FALSE )
      FL_EERM_SAVE = FL_EERM 
!
! --- Make a copy to export
!
      DO I=1,NPARM1
         LPARM_OUT(I)=LPARM1(I)
      END DO
      CALL USE_GLBFIL_2 ( 'ORC' )
      CALL DEPAR()
!
! --- NPARM2, LPARM2 -- global and local parameters
!
      KGLOBONLY = .TRUE.
      CALL GET_NAMES ( LPARM2, ISTR_LEN, M_GPA, NPARM2, L2_TRUE, L2_FALSE )
!
! --- Remove local source admittance parameters. In fact GET_NAMES should
! --- be called with L2_TRUE last argument. I'm hesitatant to turn it on
! --- now (2007.08.10), because I'm afrfraid of causing a new regression bug.
! --- This should be tested later.
!
      NPARM2_NEW = 0
      DO 410 J1=1,NPARM2
         IF ( LPARM2(J1)(1:11) .NE. 'LCL_SOU_ADM' ) THEN
              NPARM2_NEW = NPARM2_NEW + 1
              LPARM22(NPARM2_NEW) = LPARM2(J1)
         END IF
 410  CONTINUE 
      NPARM2 = NPARM2_NEW
      IF ( NPARM2 > 0 ) THEN
           CALL LIB$MOVC3 ( 20*NPARM2, %REF(LPARM22), %REF(LPARM2) )
      END IF
!
      CALL CUPARM ( NPARM1, LPARM1, NPARM2, LPARM2, NPARM3, LPARM3, NGTA, &
     &              NLTA, 1, M_GPA )
      CALL CXEPAR_OPT20 ( LPARM1, IX1T3, NPARM1, LPARM3, NPARM3 )
!
      IGLBLS=NGTA
      IARCS=NLTA
      KGLOBONLY = KGLOB
      NUM_USER_PART = OLD_USER_PART
      CALL USE_GLBFIL ( 'OWC' )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!         write ( 6, * ) 'LIST_ARC: nparm1 =',nparm1,' nparm2=',nparm2, &
!     &            ' nparm3=',nparm3
!         do 610 k1=1,nparm1
!            write ( 6, * ) ' lists_arc: k1 =',k1,'   >>',lparm1(k1),'<< '
! 610     continue
!         do 620 k1=1,nparm2
!            write ( 6, * ) ' lists_arc: k2 =',k1,'   >>',lparm2(k1),'<< '
! 620     continue
!         do 630 k1=1,nparm3
!            write ( 6, * ) ' lists_arc: k3 =',k1,'   >>',lparm3(k1),'<< '
! 630     continue
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      RETURN
      END  !#!  LISTS_ARC  #!#
