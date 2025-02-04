      SUBROUTINE ARC_I(M2)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ARC_I PROGRAM SPECIFICATION
!
! 1.1 Reconstruct the i-arc saved arcfile name from I_ARC
!
! 1.2 REFERENCES:
!
! 2.  ARC_I INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      REAL*8 M2(*)
!
! M2 - CGM matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
      INCLUDE 'baccm.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INTEGER*2 IPARM1(10,M_GPA),IPARM2(10,M_GPA)
      INTEGER*2 IPARM3(10,M_GPA)
      COMMON /PARAM/IPARM1,IPARM2,IPARM3
      character*20 lparm1(M_GPA),lparm2(M_GPA),lparm3(M_GPA)
      equivalence (iparm1,lparm1),(iparm2,lparm2),(iparm3,lparm3)
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: cvrnc
!       CALLED SUBROUTINES: where_arc,in2ch
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IWDS,ISTS,ldbnam(5),idbver, numd
      integer*4 dbend
      PARAMETER (IWDS=10, ISTS=1)
      integer*2 istr_len/20/
!
      INTEGER*2 I,TEMPARM,LENGTH,TRIMLEN
      INTEGER*4  NPARMS
      INTEGER*4 J1, N4,MAT_E
      CHARACTER*10 DUMCH
      CHARACTER*5  I_ARCNUM_CH
      CHARACTER*6  INTTODECIMAL
      CHARACTER*14 I_ARC_NAME
      CHARACTER*(NAME_SIZE) CNAME
      INTEGER*4  I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   kdb  951207  Integer*4 number of observations.
!   jmg  960610  Remove holleriths.
!     :2002.12.19:jwr: TRUE__L2 and FALSE__L2 introduced for -i2 removal
!   pet  2023.11.17  Increased the arc index field with to accoumdat the number of arcs > 9999
!
!
! 5.  ARC_I PROGRAM STRUCTURE
!
      DO J1=1,M_GPA
         IND_ICOV(J1)=0
      ENDDO
!
!
      CALL IN2CH(I_ARC,I_ARCNUM_CH )
      I_ARC_NAME='['//I_ARCNUM_CH//PRE_LETRS
      CALL WHERE_ARC(I_ARC_NAME,CNAME )
      IF ( CNAME .EQ. ' ' ) THEN
           CALL FERR( I_ARC, 'cant find saved arcfile for this arc', INT2(0), &
     &          INT2(0) )
      ENDIF
!
!   read in parm list from saved arcfile of i-arc.
!   note that if only the covariances between parameters within this
!   j-arc are requested, then i-arc information is not required.
!
      CALL ACS_ARCFIL(CNAME,TRUE__L2,'O' )
      CALL USE_ARCF_COM('R' )
      CALL GET_NAMES ( LTPARM, ISTR_LEN, M_GPA, NPARMS, TRUE__L2, FALSE__L2 )
!
!   get name if i-arc from socom
!
      NUMD = 1 
      CALL DBPOX( NUMD, LDBNAM, IDBVER, DBEND )
      CALL HOL2CHAR( LDBNAM(1), INT2(2), INT2(10), DUMCH )
      I_ARCNAME = DUMCH(1:I_LEN(DUMCH))//'.'//INTTODECIMAL(IDBVER)
!
!   generate a reordered i_arc parm list relative to the cov file
!   parm list:  want {local|global} order.
!   this arc at the end of the list
!   NOTE:  we're using TEMPARM since it's ok to put info into IPARM3
!   (it's not used after covariances are generated), but NPARM3 must
!   retain it's value in and out of the covariance units.
!
      CALL CUPARM( NPARMS, LTPARM, NPARM2, LPARM2, TEMPARM, LPARM3, &
     &             I_ARC_GLBS, I_ARC_ARCS, INT2(0), M_GPA )
!
!   determine which parms to output from i-arc
!
      CALL INDX_PARM ( ICOV, IND_ICOV, LPARM3, INT2(I_ARC_ARCS+I_ARC_GLBS), &
     &                 M_GPA, PERMUT, I_ARC_ARCS, I_ARC_GLBS, END_IDX )
      CALL CXEPAR ( LPARM3, IXI2J, TEMPARM, LPARM1, NPARM3 )
      CALL USE_ARCF_MAT( M2, INT2(I_ARC_ARCS+I_ARC_GLBS), 'R' )
!??      N4 = MAT_E ( M_GPA, INT2(I_ARC_ARCS+I_ARC_GLBS) )
      CALL ACS_ARCFIL ( CNAME, TRUE__L2, 'C' )
!
      RETURN
      END
