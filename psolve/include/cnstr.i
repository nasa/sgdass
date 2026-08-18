!
! >>>>> INCLUDE-BLOCK with descriptions data structure used for
!       imposing constraints in SOLVE
!       cnstr.i  19-JAN-98  v2.6  (c)  L. Petrov  --  13-JUN-2006 22:02:57
!
!  pet 19-JUL-98   Increased MAX_CNSTR from 8192 to 131000.
!                  Added new parameter MAX_VECEL
!  pet 2001.03.14  Increased MAX_CNSTR from 131000 to 1048576
!  pet 2002.03.19  Added new fields for keeping constraints equations
!  pet 2002.05.07  Added fields for keeping off-diagonal elements of weight
!                  constraint matrix
!  pet 2002.09.27  Added field  USR_CNS
!  pet 2002.10.01  Increasd MAX_EQUAT, MAX_ECNST, MAX_OFD
!  pet 2002.10.18  Increasd MAX_EQUAT
!  pet 2006.01.31  Increasd MAX_EQUAT to 128*1024
!  pet 2006.06.13  Added definitions for dynamic constraint equations
!  pet 2014.09.05  increased AX_EQUAT to 2*1024*1024
!
        INTEGER*4  MAX_CNSTR, MAX_VECEL, MAX_TYCNS, MAX_EQUAT, MAX_ECNST, &
     &             MAX_OFD
        PARAMETER  ( MAX_CNSTR = 8*1024*1024 )
        PARAMETER  ( MAX_TYCNS =          64 )
        PARAMETER  ( MAX_VECEL =      4*1024 )
        PARAMETER  ( MAX_EQUAT =    256*1024 )
        PARAMETER  ( MAX_ECNST = 8*1024*1024 )
        PARAMETER  ( MAX_OFD   =        4096 )
!
! ----- Data structure for dynamic constraint equations
!
	TYPE     DYN_CNS__STRU
	    INTEGER*4  STS     ! Status of dynamic constraint equations
	    INTEGER*4  M_CNS   ! Maximum number of constraint equations
	    INTEGER*4  L_CNS   ! Actual  number of constraint equations
            INTEGER*4, POINTER :: EQU_INE(:) ! index of constraint equation
            INTEGER*4, POINTER :: EQU_INP(:) ! index of the parameter in equation
            REAL*8,    POINTER :: EQU_CNS(:) ! coefficient of equation of cnstr.
	END TYPE DYN_CNS__STRU
!
! ----- Main constraint data structure
!
        TYPE      CNSTR__STRU
            INTEGER*4  N_EQUAT  !  Number of equations of constraints
            INTEGER*4  N_ECNST  !  Number of non-zero elements of the matrix
            INTEGER*4  N_OFD    !  Number of off-diagonal terms of weight matrix
!                               !  of constraints
            INTEGER*4  CNS_TYP
            CHARACTER  OBJ_NAM*10  !  Database name or 'CGM      '
!
! --------- Description of the consrtaint
!
            CHARACTER  ABB_CNS(MAX_EQUAT)*8  ! Abbreviation  of the constraint
            CHARACTER  DSC_CNS(MAX_EQUAT)*32 ! Description of the constraint
            CHARACTER  UNT_CNS(MAX_EQUAT)*16 ! Units of the constraint
            INTEGER*4  SBI_CNS(MAX_EQUAT)    ! Subindex of cnstr equation
            REAL*8     SIG_CNS(MAX_EQUAT)    ! Sigma of the constraint
            LOGICAL*4  GLO_CNS(MAX_EQUAT)    ! Flag: is the constraint global?
            LOGICAL*4  USR_CNS(MAX_EQUAT)    ! Flag: is the constraint user?
	    TYPE     ( DYN_CNS__STRU ) :: DYN(MAX_EQUAT) ! Dynamic constraint equations
!
! --------- Coefficients of constraints equations
!
            INTEGER*4  EQU_INE(MAX_ECNST) ! index of constraint equation
            INTEGER*4  EQU_INP(MAX_ECNST) ! index of the parameter in equation
            REAL*8     EQU_CNS(MAX_ECNST) ! coefficient of equation of cnstr.
!
! --------- Right hand side of constraint equations
!
            REAL*8     RTP_CNS(MAX_EQUAT) ! coefficients of right part of
!                                         ! constraint equations
!
! --------- Off-diagonal terms of constraint equation
!
            INTEGER*4  INE1_OFD(MAX_OFD)  ! index_1 of the off-diagonal element
            INTEGER*4  INE2_OFD(MAX_OFD)  ! index_2 of the off-diagonal element
!                                         ! in equations of the weight 
!                                         constraint matrix
            REAL*8     WEI_OFD(MAX_OFD)   ! Off-diagonal elements of weight
!                                         ! constraints matrix
!
! ========= OLD PART =======================================================
!
            INTEGER*4  N_TYCNS  !  Number of types of constraints
            INTEGER*4  N_MATEL  !  Number of corrected terms of normal matrix
            INTEGER*4  N_VECEL  !  Number of corrected terms of normal vector
!
! --------- List of types of the imposed constraints, abbreviations, units
! --------- and sigma applied
!
            CHARACTER  ABBR(MAX_TYCNS)*8   ! Abbreviation  of the constraint
            CHARACTER  DESCR(MAX_TYCNS)*32 ! Description of the constraint
            CHARACTER  UNITS(MAX_TYCNS)*16 ! Units of the constraint
            REAL*8     SIGMA(MAX_TYCNS)    ! Sigma of the constraint
            LOGICAL*4  GLO(MAX_TYCNS)      ! Flag: is the constraint global
!
! --------- List of corrected element of the normal MATRIX due to constraints
!
            INTEGER*4  NP1(MAX_CNSTR) ! First index of the corrected element
            INTEGER*4  NP2(MAX_CNSTR) ! Second index of the corrected element
            REAL*8     MAT_UPD(MAX_CNSTR) ! Value of the corrected element
!
! --------- List of corrected elements of the normal VECTR due to constraints
!
            INTEGER*4  NR(MAX_VECEL)      ! Index of the corrected element
            REAL*8     VEC_UPD(MAX_VECEL) ! Value of the corrected element
        END TYPE  CNSTR__STRU  ! CNSTR__STRU !
!
	INTEGER*4    CNS__ALC
	PARAMETER  ( CNS__ALC = 1093823703 ) ! Status: dynamic memory allocated
!
! <<<<< end of INCLUDE-BLOCK  cnstr.i
!
