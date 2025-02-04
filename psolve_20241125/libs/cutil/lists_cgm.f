      SUBROUTINE LISTS_CGM()
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  LISTS_CGM PROGRAM SPECIFICATION
!
! 1.1 Build overall list of parameters, including additions.
!
! 1.2 REFERENCES:
!
! 2.  LISTS_CGM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'addcm.i'
      INTEGER*2      IPARM1(10,M_GPA), IPARM2(10,M_GPA), IPARM3(10,M_GPA)
      INTEGER*4  J1
      COMMON /PARAM/ IPARM1, IPARM2, IPARM3
      CHARACTER*20   LPARM1(M_GPA), LPARM2(M_GPA), LPARM3(M_GPA)
      EQUIVALENCE    (LPARM1,IPARM1), (LPARM2,IPARM2), (LPARM3,IPARM3)
      CHARACTER*20    LPARM0(M_GPA)
      LOGICAL*2      KSHORT,KGLOBAL
      INTEGER*2 ISTR_LEN
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: adder
!       CALLED SUBROUTINES: savcm,adpar
!
! 3.  LOCAL VARIABLES
!
      CHARACTER CNAME*(NAME_SIZE)
      LOGICAL*2 KGLOB
      INTEGER*2 ISTS, IWDS, I
      INTEGER*4 NPARM1, NPARM2, NPARM3, NPARM0, NLTA, NGTA
      CHARACTER STR*16
!
      Data ISTS / 1 /, IWDS / 10 /
      DATA ISTR_LEN /  20     /
      DATA KSHORT   / .TRUE.  /
      DATA KGLOBAL  / .FALSE. /
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb 10/2/95  Some commenting for clarification.
!   jmg  960610  Use characters, not holleriths.
!   pet  970227  Rewrote comments
!   pet  970513  Corrected bug connected with SOCOM_EXT
!     :2002.12.19:jwr: TRUE__L2 and FALSE__L2 introduced for -i2 removal
!
! 5.  LISTS_CGM PROGRAM STRUCTURE
!
!
! --- Fill commons from either the scratch files or a saved arcfile
!
      CALL USE_GLBFIL_4 ( 'ORC' )
!
! --- ARORCG is ARC if adding the data from an ARC and CGM if adding it
! --- from a CGM
!
      IF ( ARORCG .EQ. 'ARC' ) THEN
           CALL USE_COMMON ( 'ORC' )
           CALL SOCOM_EXT()
           CALL USE_PARFIL ( 'ORC' )
           CALL FLYBY_APRIOR()
        ELSE IF ( ARORCG .EQ. 'CGM' ) THEN
           CNAME=THINGN
           CALL ACS_CGMFIL   ( CNAME, 'O' )
           CALL USE_CGMF_COM (        'R' )
           CALL ACS_CGMFIL   ( CNAME, 'C' )
      END IF
!
! --- Call GET_NAMES which will put the names of parameters turned on
! --- in the user's common block into array LPARM1.
!
      IF ( ARORCG .EQ. 'ARC' ) THEN
!
! -------- Build LPARM1 -- list of parameters in this arc.  (This may include
! -------- global parameters pertinent to this arc.)
!
           KGLOBONLY = .FALSE.
           CALL GET_NAMES ( LPARM1, ISTR_LEN, M_GPA, NPARM1, TRUE__L2, &
     &          FALSE__L2 )
!
! -------- Set up glbfil
!
           CALL USE_GLBFIL   ( 'OR' )
           KGLOB         = KGLOBONLY
           OLD_USER_PART = NUM_USER_PART
           CALL USE_GLBFIL_2 ( 'RC' )
!
! -------- Setting flag for further GET_NAMES to build only global parameters
!
           CALL DEPAR()
!
! -------- Build LPARM3 -- list of global parameters.
!
           CALL GET_NAMES ( LPARM3, ISTR_LEN, M_GPA, NPARM3, TRUE__L2, &
     &          FALSE__L2 )
!
           KGLOBONLY     = KGLOB
           NUM_USER_PART = OLD_USER_PART
!
! -------- Generate LPARM2 -- parameter list:  union of IPARM1 and IPARM3
! -------- sets, with the following structure (arc parameters; global
! -------- parameters for this arc; other globals).
!
! -------- LPARM2 now will hold the parameter list in order of the ARC file
!
           CALL CUPARM ( NPARM1, LPARM1, NPARM3, LPARM3, NPARM2, LPARM2, NGTA, &
     &          NLTA, INT2(0), M_GPA )
!
        ELSE
           CALL GET_NAMES ( LPARM2, ISTR_LEN, M_GPA, NPARM2, TRUE__L2, &
     &          TRUE__L2 )
           NGTA=NPARM2
           NLTA=0
      ENDIF
      NPARMT=NPARM2
!
      IF ( CGMINN(1:1).NE.' ' ) THEN
!
! -------- Now save the globals in the commons
!
           CALL SAVCM()
!
! -------- Fill common blocks with data from CGMINN file.
!
           CNAME = CGMINN
           CALL ACS_CGMFIL   ( CNAME, 'O' )
           CALL USE_CGMF_COM ( 'R' )
           CALL ACS_CGMFIL   ( CNAME, 'C' )
!
! -------- Call GET_NAMES which will put the names of parameters turned on
! -------- in the CGM common block into array IPARM1.
!
           CALL GET_NAMES ( LPARM1, ISTR_LEN, M_GPA, NPARM1, TRUE__L2, &
     &                      TRUE__L2 )
           NPARMC=NPARM1
!
! -------- Now add the globals from the other file (ARC or CGM)
!
           CALL ADPAR()
      ENDIF
!
      IF ( KUSER_PART ) CALL UPDATE_USERP()
!
! --- Call GET_NAMES to get the totality of globals
! --- (e.g., may have been globals from the input cgm, if there was one)
!
      CALL GET_NAMES ( LPARM3, ISTR_LEN, M_GPA, NPARM3, TRUE__L2, TRUE__L2 )
      IF ( NPARM3.GT.M_GPA ) CALL FERR ( INT2(9000+NPARM3), &
     &    ' TOO MANY PARAMETERS', INT2(0), INT2(0) )
      NPARMF=NPARM3
!
! --- Generate IXCTF -- a cross reference from the CGM to the final
!
      IF ( CGMINN(1:1) .NE. ' ' ) THEN
           CALL CXEPAR ( LPARM1, IXCTF, NPARM1, LPARM3, NPARM3 )
      ENDIF
!
! --- Generate IXTTF -- a cross reference from  ARC (or CGM) to the final
!
      DO I=1,NLTA
         IXTTF(I)=0
      ENDDO
!
      IF ( NGTA .GT. 0 ) THEN
           CALL CXEPAR ( LPARM2(NLTA+1), IXTTF(NLTA+1), NGTA, LPARM3, NPARM3 )
      ENDIF
!
      RETURN
      END  !#!  LISTS_CGM  #!#
