      SUBROUTINE CUPARM ( NPARM1, LPARM1, NPARM2, LPARM2, NPARM3, LPARM3, &
     &                    NGTA, NLTA, LCL, MPAR )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!    String version of uparm.
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
      INTEGER*4       NPARM1, NPARM2, MPAR, LCL
      character*20   lparm1(nparm1),lparm2(nparm2)
!
! LPARM1 - SOLVE parm list
! lPARM2 - CGM parm list
! LCL -
! MPAR - Maximum number of parms allowed
! NPARM1 - Number of parms in IPARM1
! NPARM2 - number of parms in IPARM2
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*20   LPARM3(*)
      INTEGER*4      NPARM3, NGTA, NLTA
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
      INTEGER*4       I, J, K
      INTEGER*2       TEMP1(10), TEMP2(10), TEMP3(10)
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
!
! --- Check sanity of NPARM1, NPARM2, NPARM3, and if not drash Solve to
! --- see the stack
!
      IF ( NPARM1 < 0 .OR. NPARM1 > 256*1024 ) THEN
           NLTA = 0 
           WRITE ( 6, * ) 'CUPARAM  NPARM1, NPARM2, NPARM3 = ', NPARM1, NPARM2, NPARM3 
           K = TEMP1(NLTA)
      END IF
      IF ( NPARM2 < 0 .OR. NPARM2 > 256*1024 ) THEN
           NLTA = 0 
           WRITE ( 6, * ) 'CUPARAM  NPARM1, NPARM2, NPARM3 = ', NPARM1, NPARM2, NPARM3 
           K = TEMP1(NLTA)
      END IF
      IF ( NPARM3 < 0 .OR. NPARM3 > 256*1024 ) THEN
           NLTA = 0 
           WRITE ( 6, * ) 'CUPARAM  NPARM1, NPARM2, NPARM3 = ', NPARM1, NPARM2, NPARM3 
           K = TEMP1(NLTA)
      END IF
!
      K    = 1
      NLTA = 0
      DO I = 1, NPARM1
          IT_IS_HERE = .FALSE.
          J = 1
          DO WHILE ( J .LE. NPARM2 .AND. ( .NOT. IT_IS_HERE) )
             IF (LPARM1(I) .EQ. LPARM2(J) ) IT_IS_HERE = .TRUE.
             J = J + 1
          END DO
!
          IF ( .NOT. IT_IS_HERE ) THEN
               LPARM3(K)=LPARM1(I)
               NLTA = NLTA + 1
               K = K + 1
               IF ( (K-1) .GT. MPAR ) CALL FERR ( INT2(9151), &
     &             'CUPARM: too many '//'parmeters', INT2(0), INT2(0) )
          END IF
      END DO
!
! --- move globals in arc list to end of IPARM3
!
      NGTA = 0
      DO I = 1, NPARM1
          IT_IS_HERE = .FALSE.
          J = 1
          DO WHILE ( J .LE. NPARM2 .AND. ( .NOT. IT_IS_HERE ) )
             IF ( LPARM1(I) .EQ. LPARM2(J) ) IT_IS_HERE = .TRUE.
             J = J + 1
          END DO
          IF ( IT_IS_HERE ) THEN
               LPARM3(K)=LPARM1(I)
               NGTA = NGTA + 1
               K = K + 1
               IF ( (K-1) .GT. MPAR ) CALL FERR ( INT2(9152), &
     &             'CUPARM: too many '//'parmeters', INT2(0), INT2(0) )
          END IF
      END DO
!
! --- move rest of globals not occurring in this arc to end of IPARM3
!
      IF ( LCL .EQ. 0 ) THEN
           DO I = 1, NPARM2
               IT_IS_HERE = .FALSE.
               J = 1
               DO WHILE ( J .LE. NPARM1 .AND. ( .NOT. IT_IS_HERE) )
                   IF (LPARM2(I) .EQ. LPARM1(J) ) IT_IS_HERE = .TRUE.
                   J = J + 1
               END DO
               IF ( .NOT. IT_IS_HERE ) THEN
                   LPARM3(K)=LPARM2(I)
                   K = K + 1
                   IF ( (K-1) .GT. MPAR ) THEN
                        CALL FERR ( INT2(9153), 'CUPARM: too many parmeters', &
     &                       INT2(0), INT2(0) )
                   END IF
               END IF
           END DO
      ENDIF
      NPARM3 = K - 1
!
      RETURN
      END  !#!  CUPARM  #!#
