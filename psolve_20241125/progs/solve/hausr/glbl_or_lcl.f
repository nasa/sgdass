      SUBROUTINE GLBL_OR_LCL (RECTYPE, IRECORD, KIND)
      IMPLICIT   NONE
!
! 1.  GLBL_OR_LCL PROGRAM SPECIFICATION
!
! 1.1 Check to see if both stations are globl or not
!
! 1.2 REFERENCES:
!
! 2.  GLBL_OR_LCL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      Integer*2   RECTYPE, IRECORD(8)
!
! RECTYPE - 1: baseline
!           0: source
! IRECORD - baseline or source name record
!
! 2.3 OUTPUT Variables:
!
      Character*8 KIND*5
!
! KIND - Globl or local stations
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'hausr.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: outwithitall
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      Integer*4    NRECORDS
      Integer*2   k, H, INAME(4), STATE1, STATE2
      CHARACTER*8 CHNAME, EXCEPT_NAMES(LMAX_SELAR), FOUND*2
      EQUIVALENCE ( ISELAR(1), EXCEPT_NAMES(1) ), &
     &            ( CHNAME, INAME(1) )
!
! 4.  HISTORY
! WHO  WHEN       WHAT
! pet  26-JUL-98  Fixed a bug: previous version had the size of array
!                 EXCEPT_NAMES hardcoded
!
! 5.  GLBL_OR_LCL PROGRAM STRUCTURE
!
!   intialise the name-state and counter variables
!
         STATE1 = 0
         STATE2 = 0
!
!   test first parameter in the passed record
!
      Do K = 1, 4
         INAME(K) = IRECORD(K)
      End do
      H = 1
      FOUND = 'n'
      Do while (FOUND .eq. 'n' .and. H .le. NEXCPT)
          If (CHNAME .EQ. EXCEPT_NAMES(H)) then
              FOUND = 'y'
          End if
          H = H + 1
      End do
      If (RECTYPE .eq. 1 .and. KCSTA) then
          If (FOUND .eq. 'y') then
              STATE1 = 0
          Else
              STATE1 = 1
          End if
      Else if (RECTYPE .eq. 1 .and..not. KCSTA) then
          If (FOUND .eq. 'y') then
              STATE1 = 1
          Else
              STATE1 = 0
          End if
      Else if (RECTYPE .eq. 0 .and. KCSRC) then
          If (FOUND .eq. 'y') then
              STATE1 = 0
          Else
              STATE1 = 1
          End if
      Else if (RECTYPE .eq. 0 .and..not. KCSRC) then
          If (FOUND .eq. 'y') then
              STATE1 = 1
          Else
              STATE1 = 0
          End if
      End if
!
!   if this is a baseline, then reinitialise the counter and
!   test the second station of the baseline for global character
!
      If (RECTYPE .eq. 1) then
!
!   test the second station in the baseline
!
          Do K = 1, 4
             INAME(K) = IRECORD(K+4)
          End do
          H = 1
          FOUND = 'n'
          Do while (FOUND .eq. 'n' .and. H .le. NEXCPT)
              If (CHNAME .eq. EXCEPT_NAMES(H)) then
                  FOUND = 'y'
              End if
              H = H + 1
          End do
          If (KCSTA) then
              If (FOUND .eq. 'y') then
                  STATE2 = 0
              Else
                  STATE2 = 1
              End if
          Else if (.not. KCSTA) then
              If (FOUND .eq. 'y') then
                  STATE2 = 1
              Else
                  STATE2 = 0
              End if
          End if
!
      End if
!
!   set KIND on basis of STATE1, STATE2, & RECTYPE
!
      If (RECTYPE .eq. 0 .and. STATE1 .eq. 0) then
          KIND = 'local'
      Else if (RECTYPE .eq. 0 .and. STATE1 .eq. 1) then
          KIND = 'globl'
      Else if (RECTYPE .eq. 1 .and. (STATE1 .eq. 0 .or. &
     &         STATE2 .eq. 0)) then
          KIND = 'local'
      Else if (RECTYPE .eq. 1 .and. (STATE1 .eq. 1 .and. &
     &         STATE2 .eq. 1)) then
          KIND = 'globl'
      End if
!
      RETURN
      END  !#!  GLBL_OR_LCL  #!#
