      PROGRAM    COVP_HEAD
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COVP PROGRAM SPECIFICATION
!
! 1.1 COVP IS THE ROUTINE WHICH WILL TAKE THE COVARIANCE MATRIX
!     (THE INVERSE OF THE NORMAL EQUATIONS MATRIX) AND CALCULATE
!     AND PRINT THE CORRELATION MATRIX.
!
! 1.2 REFERENCES:
!
! 2.  COVP INTERFACE
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
      INCLUDE 'precm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: none
!       CALLED SUBROUTINES: covp_main
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2   KBIT, F_IO_NRM
      INTEGER*4   I4P0, I4P1,I4P51,I4P255,I4P256
      CHARACTER   STR*54, GET_VERSION*54
      INTEGER*4   JA, JB, JS, MAT_E
      INTEGER*8   MATSIZE
      ADDRESS__TYPE :: ADDR_MAT
      ADDRESS__TYPE, EXTERNAL  :: MAT_E4
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INTEGER*4,  EXTERNAL :: I_LEN
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      DATA        I4P0, I4P1, I4P51, I4P255, I4P256 / 0, 1, 51, 255, 256 /
!
! 4.  HISTORY
!  WHO  WHEN    WHAT
!   JLR  921215  replaced nJ's with I4Pn's
!   mwh  940201  implement dynamic memory allocation for large matrices
!   pet  970712  Changed messages in the mode when NORML called by REWAY
!
! 5.  COVP PROGRAM STRUCTURE
!
      CALL PRE_PROG()
!
      INCLUDE 'covp_version.i' ! Set revision date of the current version
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 ( 'OR' )
!
! --- Calculate the correlation matrix
!
      JA = 3*M_GPA
      JB = 2*M_GPA
      JS =   M_GPA
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
           CALL START_MN()
           STR = GET_VERSION()
           CALL SETCR_MN ( 79-I_LEN(STR), 0 )
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F ( STR(1:I_LEN(STR)) )
           CALL REVERSE_OFF_MN()
           CALL NL_MN()
!
           IF ( KBIT ( PRE_IP(3), INT2(12)) ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( INT4(REWAY_ITCOU), STR )
                CALL SETCR_MN ( 0, 0 )
                CALL ADDSTR_F ( '  REWAY --> COVP      Iteration '// &
     &                         STR(1:I_LEN(STR)) )
                CALL SETCR_MN ( 50, 0 )
           END IF
           CALL REFRESH_MN()
      ENDIF
!
! --- Open and read the common block file & open spool file
!
      CALL USE_COMMON ( 'OR' )
      CALL SOCOM_EXT()
      IF ( KSPOOL ) CALL USE_SPOOL('O')
!!      matsize = i4p256* ( (mat_e(max_par,nparam)+i4p255)/i4p256 )
!!      matsize = matsize*8
      MATSIZE = 8*MAT_E4 ( M_GPA, NPARAM )
      CALL GET_MEM ( MATSIZE, ADDR_MAT )
!
      F_IO_NRM = .TRUE.
      CALL COVP_MAIN ( %VAL(ADDR_MAT), F_IO_NRM )
      IF ( KSCREEN .AND. KBIT ( PRE_IP ( 2 ), INT2(6)) ) CALL END_MN
      CALL END_PROG()
!
      END  PROGRAM  COVP_HEAD  !#!#
