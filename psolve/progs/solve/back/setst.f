      SUBROUTINE SETST ( IARRAY, NPARM, PARM_LIST, NGLOB, GLOB_LIST )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SETST PROGRAM SPECIFICATION
!
! 1.1 Set the bit array IARRAY to reflect the global/local
!     structure of PARM_LIST.  IMPORTANT: at present (5.8.87)
!     IARRAY (passed as ISTRUC) is dimensioned in GLBCM
!     to ISTRUC(MAX_STRUC) parameters.
!
! 1.2 REFERENCES:
!
! 2.  SETST INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4    NPARM, NGLOB
      CHARACTER*20 PARM_LIST(NPARM),GLOB_LIST(NGLOB)
!
! GLOB_LIST - CGM parm list
! PARM_LIST - SOLVE parm list
! NGLOB - Number of parameters in CGM parm list
! NPARM - Number of parameters in SOLVE parm list
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IARRAY(*)
!
! IARRAY - Bit map showing global/local structure of PARM_LIST
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: pmcmb
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J
      LOGICAL*2    KBIT
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
! Modified by JMGipson July30 1996 to get rid of integer arrays!
! data passed as strings.  Also, only zero the actual number of parameters
! in this arc in bit array istruc.
!
!   pet   1999.10.12  Got rid from hard coded constatnt 4096
!
! 5.  SETST PROGRAM STRUCTURE
!
      IF ( NPARM .GT. M_GPA ) THEN
           WRITE ( *, * ) "SETST: not enough space for parameters."
           STOP 'BACK'
      ENDIF
      DO I=1,NPARM
         CALL SBIT ( IARRAY, I, INT2(0) )
      ENDDO
!
! --- Process the parameter list PARM_LIST
!
      DO I = 1, NPARM  !run thru PARM_LIST!
         J = 0
         DO WHILE ( (.NOT.(KBIT(IARRAY, I)) ) .AND. (J .LT. NGLOB) )
            J = J + 1  ! Array column counter for GLOB_LIST!
!
! --------- Test to see if parm is global or local:  if global, quit loop
!
            IF ( PARM_LIST(I) .EQ. GLOB_LIST(J) ) THEN
                 CALL SBIT ( IARRAY, I, INT2(1) ) !Set bit on!
            END IF
         END DO
      END DO
!
      RETURN
      END  !#!  SETST  #!#
