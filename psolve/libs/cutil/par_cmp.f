      SUBROUTINE PAR_CMP(NP,TU,DERIV,FL,ARGP,FCNPER,NUTP1,NUTP2)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PAR_CMP PROGRAM SPECIFICATION
!
! 1.1 Update array of partial derivatives of parameters
!
! 1.2 REFERENCES:
!
! 2.  PAR_CMP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NP
      INTEGER*2 FL(*)
      REAL*8 FCNPER,NUTP1,NUTP2
      REAL*8 DERIV(M_GPA,2),TU,ARGP(2,*)
!
! ARGP - Array of values of arguments and derivatives of specific terms
! DERIV - Array of partial derivatives of the parameters
! FCNPER - Free core nutation period
! FL - PSI flag bits
! NP - Counter for the bits set (see function KBIT)
! NUTP1 - First half of partial derivatives for daily nutation
! NUTP2 - Second half of partial derivatives for daily nutation
! TU - Number of centuries since J2000
!
! 2.3 OUTPUT Variables:
!
! DERIV - Array of partial derivatives of the parameters
! FL - PSI flag bits
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: nut_parts
!       CALLED SUBROUTINES: kbit
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I,IPOS,IBIT
      LOGICAL*2 KBIT
      REAL*8 PHASE,REF,DPI
!
! 4.  HISTORY
!   WHO  WHEN   WHAT
!
! 5.  PAR_CMP PROGRAM STRUCTURE
!
!  REF IS COORDINATE TIME OF 80/10/17/00:00:00 UT IN CENTURIES FROM J2000
!
      DATA REF/-0.19207390575253448D0/
      DATA DPI/3.1415926535897932D0/
!
      IF(KBIT( FL, INT2(1) )) THEN
         NP=NP+1
!        DERIV(NP,1)=1.0D0*NUTP1
         DERIV(NP,1)=NUTP1
!        DERIV(NP,2)=1.0D0*NUTP2
         DERIV(NP,2)=NUTP2
      ENDIF
      IF(KBIT( FL, INT2(2) )) THEN
         NP=NP+1
         DERIV(NP,1)=(TU-REF)*100.0D0*NUTP1
         DERIV(NP,2)=(TU-REF)*100.0D0*NUTP2
      ENDIF
      PHASE=2.0D0*DPI*36525.0D0*TU/FCNPER
      IF(KBIT( FL, INT2(3) )) THEN
         NP=NP+1
         DERIV(NP,1)=COS(PHASE)*NUTP1
         DERIV(NP,2)=COS(PHASE)*NUTP2
      ENDIF
      IF(KBIT( FL, INT2(4) )) THEN
         NP=NP+1
         DERIV(NP,1)=SIN(PHASE)*NUTP1
         DERIV(NP,2)=SIN(PHASE)*NUTP2
      ENDIF
!
      IPOS=0
      DO I=3,23
        IBIT=(I-1)*2
        IF(KBIT( FL, INT2(IBIT+1)).OR.KBIT( FL, INT2(IBIT+2) )) IPOS=IPOS+1
        IF(KBIT( FL, INT2(IBIT+1))) THEN
          NP=NP+1
          DERIV(NP,1)=COS(ARGP(1,IPOS))*NUTP1
          DERIV(NP,2)=COS(ARGP(1,IPOS))*NUTP2
        ENDIF
        IF(KBIT( FL, INT2(IBIT+2))) THEN
          NP=NP+1
          DERIV(NP,1)=SIN(ARGP(1,IPOS))*NUTP1
          DERIV(NP,2)=SIN(ARGP(1,IPOS))*NUTP2
        ENDIF
      ENDDO
!
      RETURN
      END
