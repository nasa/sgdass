      SUBROUTINE ROTATE_PARTS(ISTA,NP)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ROTATE_PARTS PROGRAM SPECIFICATION
!
! 1.1
!
! 1.2 REFERENCES:
!
! 2.  ROTATE_PARTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISTA,NP
!
! ISTA - Station number
! NP - Number of parameters
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'oborg.i'
      REAL*8 MAT(3,3,MAX_ARC_STA)
      COMMON/ROTCM/MAT
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: ferr,uen_rot
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,IDR
      REAL*8 XYZ(3),TEMP(3)
      LOGICAL*2 KNCALC(MAX_STA),KBIT
      SAVE KNCALC
      DATA KNCALC/MAX_STA*.TRUE./
!
! I,J - Loop indices
! IDR -
! KNCALC -
! TEMP -
! XYZ -
!
! 4.  HISTORY
!  WHO   WHEN   WHAT
!
! 5.  ROTATE_PARTS PROGRAM STRUCTURE
!
!
      IF (KNCALC(ISTA)) THEN
        IF(ISTA.GT.MAX_ARC_STA) THEN
          CALL FERR( INT2(32), 'TOO MANY STATIONS IN ROTATE PARTS', INT2(0), &
     &         INT2(0) )
        ENDIF
        XYZ(1)=VSITEC(1,ISTA)
        XYZ(2)=VSITEC(2,ISTA)
        XYZ(3)=VSITEC(3,ISTA)
        CALL UEN_ROT(XYZ,MAT(1,1,ISTA) )
        KNCALC(ISTA)=.FALSE.
      ENDIF
!
      DO IDR=1,2
        DO I=1,3
          TEMP(I)=0.0
          DO J=1,3
            TEMP(I)=TEMP(I)+MAT(I,J,ISTA)*BP(J,NP,IDR)
          ENDDO
        ENDDO
        DO I=1,3
          BP(I,NP,IDR)=TEMP(I)
        ENDDO
      ENDDO
!
      RETURN
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ROTATE_PARTS_SAVE ( ISTA, NP, BP_UEN )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  ROTATE_PARTS PROGRAM SPECIFICATION
!
! 1.1
!
! 1.2 REFERENCES:
!
! 2.  ROTATE_PARTS INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISTA,NP
!
! ISTA - Station number
! NP - Number of parameters
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'oborg.i'
      REAL*8   BP_UEN(3,2,2)
      REAL*8 MAT(3,3,MAX_ARC_STA)
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: ferr,uen_rot
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,J,IDR
      REAL*8 XYZ(3),TEMP(3)
      LOGICAL*2, EXTERNAL :: KBIT
!
! I,J - Loop indices
! IDR -
! KNCALC -
! TEMP -
! XYZ -
!
! 4.  HISTORY
!  WHO   WHEN         WHAT
!  pet   2008.11.18   
!
! 5.  ROTATE_PARTS PROGRAM STRUCTURE
!
!
      IF( ISTA.GT.MAX_ARC_STA) THEN
          CALL FERR( INT2(32), 'TOO MANY STATIONS IN ROTATE PARTS', INT2(0), &
     &         INT2(0) )
      ENDIF
      XYZ(1)=VSITEC(1,ISTA)
      XYZ(2)=VSITEC(2,ISTA)
      XYZ(3)=VSITEC(3,ISTA)
      CALL UEN_ROT(XYZ,MAT(1,1,ISTA) )
!
      DO IDR=1,2
        DO I=1,3
          TEMP(I)=0.0
          DO J=1,3
            TEMP(I)=TEMP(I)+MAT(I,J,ISTA)*BP(J,NP,IDR)
          ENDDO
        ENDDO
        DO I=1,3
          BP_UEN(I,NP,IDR)=TEMP(I)
        ENDDO
      ENDDO
!
      RETURN
      END
