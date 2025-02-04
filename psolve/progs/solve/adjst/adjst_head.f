      PROGRAM ADJST_LAUNCH
      INCLUDE 'solve.i'
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      CHARACTER    STR*54
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL ADJST_HEAD()
      END  PROGRAM  ADJST_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADJST_HEAD
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ADJST PROGRAM SPECIFICATION
!
! 1.1
!    ****************************************************************
!    *                                                              *
!    * ADJST is the program which will print the parameter          *
!    * adjustments and their formal errors, scaled by the weighted  *
!    * RMS residual value. ADJST has three main parts: A1JST, the   *
!    * old ADJST, which handles parameters up through sources;      *
!    * A2JST, which handles the rest of the parameters; and A3JST   *
!    * which is the epilogue of the old ADJST. This separation      *
!    * was used in case, just in case, we ever had to split this    *
!    * program up again.                                            *
!    *                                                              *
!    ****************************************************************
!
! 1.2 REFERENCES:
!
! 2.  ADJST INTERFACE
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
      INCLUDE 'socom.i'
      INCLUDE 'buff2.i'
      INCLUDE 'cnstr.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'heo.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES: adjst_do
!
! 3.  LOCAL VARIABLES
!
      ADDRESS__TYPE :: IADDR_ARR, MEM_ADR
      INTEGER*8 MATSIZE
      INTEGER*4 IPTR, PAGEWID, IUER
      CHARACTER LBUF(CRES_BUF_LEN)*120
      TYPE ( CNSTR__STRU ) :: CNSTROBJ
      INTEGER*4, EXTERNAL  :: ILEN, MAT_E
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
!
! EOPTRACE   - Sum of earth orientation constraint shares
! KCONS      - True if constraints are applied
! KSRC       - Set True once any source coordinate parameters are printed
! OVRTRA_ATM - Overall sum of atmosphere constraint shares
! OVRTRA_CLK - Overall sum of clock constraint shares
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  940201  Implement dynamic memory allocation for large matrices
!   kdb  951130  12/20/96 21:39:50 accidentally replaced with an actual sccs version.
!                Restore it.
!   pet  990309  Added an additional layer of calls: adjst_do for
!   pet  990317  Added calls of USE_SPOOL (moved from a1jst, a3jst). Renamed
!                OVRTRA to OVRTRA_ATM
!   pet  1999.05.28 Added arguments LBUF_LEN, LBUF, IPTR, PAGEWID to the call
!                   ADJST_DO
!   pet  2002.03.19 Added arguments CNSTROBJ, IUER for ADJST_DO
!   pet  2004.11.09 Added reading HEO file if needed.
!
! 5.  ADJST PROGRAM STRUCTURE
!
     CALL PRE_PROG()
      INCLUDE 'adjst_version.i' ! Set revision date of the current version
!
! --- Read user buffer
!
      BUFF2_WORDS = (   LOC(BUFF2_I2_FILLER) - LOC(LCHAO) &
     &                + SIZEOF(BUFF2_I2_FILLER) )/2
      CALL USE_BUFFER ( LCHAO, BUFF2_WORDS, 'ORC' )
      CALL SET_SIGNAL_CTRLC ( 3 )
!
! --- Open and read common, prfil, and nrmfil
!
      IF ( .NOT. KGLOBALS ) THEN
           CALL USE_COMMON ( 'ORC' )
           CALL SOCOM_EXT()
           CALL USE_PARFIL ( 'ORC' )
           MATSIZE = MAT_E4 ( M_GPA, NPARAM )
           CALL GET_MEM ( INT8(8)*MATSIZE, IADDR_ARR )
           CALL USE_NRMFIL ( %VAL(IADDR_ARR), NPARAM, 'ORC' )
        ELSE
           CALL ACS_COVFIL   ( 'O' )
           CALL USE_COVF_COM ( 'R' )
           MATSIZE = MAT_E4 ( M_GPA, NPARAM )
           CALL GET_MEM ( INT8(8)*MATSIZE, IADDR_ARR )
           CALL USE_COVF_MAT ( %VAL(IADDR_ARR), NPARAM, 'R' )
           CALL ACS_COVFIL   ( 'C' )
      ENDIF
!
      CALL USE_GLBFIL_4 ( 'ORC' )
      IF ( ILEN(FINAM_HEO) .GT. 0 ) THEN
           IUER = -1
           CALL GHEO ( FINAM_HEO, NAME_HEO, L_HEO, STAT_HEO, &
     &                 ADR_HEO, HEO_EPOCH_SEC, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8401, -1, 'ADJST_HEAD', 'Error '// &
     &              'during attempt to read the file with harmonic Earth '// &
     &              'orientation parameters '//FINAM_HEO )
                STOP 'PROC(proc_head) Abnormal termination'
           END IF
           STAT_HEO  = HEO__READ
           CALL USE_GLBFIL_4 ( 'OWC' )
      END IF
!
! --- Initialize iptr and set up top of lbuf
!
      IPTR=1
      PAGEWID=79
!
! --- Open spool file
!
      IF ( KSPOOL ) CALL USE_SPOOL ( 'O' )
!
! --- Do work!
!
      IUER = -1
      CALL ADJST_DO ( NPARAM, %VAL(IADDR_ARR), CRES_BUF_LEN, LBUF, IPTR, &
     &                PAGEWID, CNSTROBJ, IUER )
!
! --- Write spool EOF
!
      IF ( KSPOOL ) CALL USE_SPOOL ( 'C' )
      IF ( IUER .NE. 0 ) THEN
           STOP 'ADJST(adjst_head): Abnormal termination. Errors in ADJST_DO'
      END IF
!
      IF ( KBATCH .OR. ISCREEN == ISCREEN_NO ) THEN
           CALL END_PROG()
      ENDIF
      END  SUBROUTINE  ADJST_HEAD  !#!#
