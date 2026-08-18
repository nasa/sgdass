      SUBROUTINE UPARM (NPARM1, IPARM1, NPARM2, IPARM2, &
     &                  NPARM3, IPARM3, NGTA, NLTA, LCL, MPAR)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  UPARM PROGRAM SPECIFICATION
!
! 1.1
!
! 1.2 REFERENCES:
!
! 2.  UPARM INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2       NPARM1, NPARM2, MPAR, LCL
      Integer*2       IPARM1(10,NPARM1), IPARM2(10,NPARM2)
!
! IPARM1 - SOLVE parm list
! IPARM2 - CGM parm list
! LCL -
! MPAR - Maximum number of parms allowed
! NPARM1 - Number of parms in IPARM1
! NPARM2 - number of parms in IPARM2
!
! 2.3 OUTPUT Variables:
!
      Integer*2       IPARM3(10,MPAR)
      INTEGER*2       NPARM3, NGTA, NLTA
!
! IPARM3 - Final parameter list
! NPARM3 - Mumber of parameters
! NGTA - Number of globals, this arc
! NLTA - Number of locals, this arc
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: movewords
!
! 3.  LOCAL VARIABLES
!
      Character*20    PARM1, PARM2
      Logical*2       IT_IS_HERE
      INTEGER*2       I, J, K, TEMP1(10), TEMP2(10), TEMP3(10)
      Equivalence     (TEMP1(1), PARM1),(TEMP2(1), PARM2)
!
! I,J,K - Array indices
! IT_IS_HERE -
! PARM1/2,TEMP1/2 - Parameters currently being moved
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  UPARM PROGRAM STRUCTURE
!
!   move arc parameters from IPARM1 to IPARM3
!
      K    = 1
      NLTA = 0
      Do I = 1, NPARM1
          IT_IS_HERE = .FALSE.
          J = 1
          Call MOVEWORDS( IPARM1(1,I), TEMP1, INT2(10))
          Do while (J .le. NPARM2 .and. (.not.IT_IS_HERE))
              Call MOVEWORDS( IPARM2(1,J), TEMP2, INT2(10))
              If (PARM1 .eq. PARM2) IT_IS_HERE = .TRUE.
              J = J + 1
          End do
          If (.not.IT_IS_HERE) then
              Call movewords( IPARM1(1,I), IPARM3(1,K), INT2(10))
              NLTA = NLTA + 1
              K = K + 1
              If ((K-1) .gt. MPAR) Call FERR ( INT2(9151), 'too many parms', &
     &         INT2(0), INT2(0) )
          End if
      End do
!
!   move globals in arc list to end of IPARM3
!
      NGTA = 0
      Do I = 1, NPARM1
          IT_IS_HERE = .FALSE.
          J = 1
          Call MOVEWORDS( IPARM1(1,I), TEMP1, INT2(10))
          Do while (J .le. NPARM2 .and. (.not.IT_IS_HERE))
              Call MOVEWORDS( IPARM2(1,J), TEMP2, INT2(10))
              If (PARM1 .eq. PARM2) IT_IS_HERE = .TRUE.
              J = J + 1
          End do
          If (IT_IS_HERE) then
!             Call Wmov (IPARM1(1,I), 1, IPARM3(1,K), 1, 5)
              Call movewords( IPARM1(1,I), IPARM3(1,K), INT2(10))
              NGTA = NGTA + 1
              K = K + 1
              If ((K-1) .gt. MPAR) Call FERR ( INT2(9152), 'too many parms', &
     &         INT2(0), INT2(0) )
          End if
      End do
!
!
!   move rest of globals not occurring in this arc to end of IPARM3
!
      IF(LCL.EQ.0) THEN
      Do I = 1, NPARM2
          IT_IS_HERE = .FALSE.
          J = 1
          Call MOVEWORDS( IPARM2(1,I), TEMP2, INT2(10))
          Do while (J .le. NPARM1 .and. (.not.IT_IS_HERE))
              Call MOVEWORDS( IPARM1(1,J), TEMP1, INT2(10))
              If (PARM2 .eq. PARM1) IT_IS_HERE = .TRUE.
              J = J + 1
          End do
          If (.not.IT_IS_HERE) then
              Call movewords( IPARM2(1,I), IPARM3(1,K), INT2(10))
              K = K + 1
              If ((K-1) .gt. MPAR) Call FERR ( INT2(9153), 'too many parms', &
     &         INT2(0), INT2(0) )
          End if
      End do
      ENDIF
      NPARM3 = K - 1
!
      RETURN
      END
