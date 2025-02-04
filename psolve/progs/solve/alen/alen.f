      PROGRAM ALEN
      IMPLICIT NONE
!
! 1.  ALEN PROGRAM SPECIFICATION
!
! 1.1 Calculate and display arc distances between source pairs,
!     and their error (based on errors in the coordinates and the
!     correlations among the errors in the coordinates).
!
! 1.2 REFERENCES:
!
! 2.  ALEN INTERFACE
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
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: none
!       CALLED SUBROUTINES: arcst
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IFLGA (2,MAX_SRC),IFRACT(21)
      INTEGER*4 INDX4,JK
      INTEGER*8 MATSIZE
      REAL*8 STRFE(2,MAX_SRC),OLDSC(2,MAX_SRC)
      REAL*8 NEWSC(2,MAX_SRC)
      LOGICAL*2  KBIT
      INTEGER*2  ICT,IFR,IST,IST1,IST2,IST3,IST4,J,K,L
      integer*4  JA, JB, JS, IUER, MAT_E
      INTEGER*8  NELEM, MEM_LEM
      ADDRESS__TYPE :: ADDR_MAT, MEM_ADR
      REAL*8 ARCDST,ARCERR,CHANG,FRACT,OLDARC,OLDERR
      COMMON/ALNEMA/IFLGA,STRFE,OLDSC,NEWSC
      integer*4 I4P255,I4P256
      DATA I4P255,I4P256 /255,256/
      INTEGER*8, EXTERNAL :: MAT_E4
!
!  IFLGA: ARRAY TO HOLD THE PARAMETER NUMBER OF ADJUSTED SOURCE
!         PARAMETERS, OR 0 IF THE PARAMETER IS NOT ADJUSTED
!
! 4. HISTORY
!   WHO   WHEN   WHAT
!
!   CAK  820512  Created
!   mwh  940201  Implement dynamic memory allocation for large matrices
!
! 5.  ALEN PROGRAM STRUCTURE
!
      CALL PRE_PROG()
      INCLUDE 'alen_version.i' ! Set revision date of the current version
!
!  OPEN AND READ SOCOM, SET UP SPOOL FILE
!
      CALL USE_COMMON('OR')
!!      nelem = I4P256*((mat_e(MAX_PAR,nparam)+I4P255)/I4P256)
      NELEM = MAT_E4 ( M_GPA, NPARAM )
    write ( 6, * ) ' m_gpa= ', m_gpa, ' nparam= ', nparam, ' nelem= ', nelem ! %%%%%%%%
!
!  convert to number of bytes (8 per real*8 element)
      MATSIZE = NELEM*8
      CALL GRAB_MEM ( IUER, MEM_LEM, MEM_ADR, 1, MATSIZE, ADDR_MAT )
      CALL ALEN_MAIN ( %VAL(ADDR_MAT) )
      END
!
