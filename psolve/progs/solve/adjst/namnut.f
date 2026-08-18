      SUBROUTINE NAMNUT(I,TYP,QCOMP,QPHASE,FCNPER)
      IMPLICIT NONE
!
! 1.  NAMNUT PROGRAM SPECIFICATION
!
! 1.1 Get the name label to be printed with a nutation parameter.
!
! 1.2 REFERENCES:
!
! 2.  NAMNUT INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TYP
      INTEGER*2 I
      REAL*8 FCNPER
!
! FCNPER - Free core nutation period
! I - Term number
! TYP - Nutation component type (PSI or EPS)
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) QPHASE,QCOMP
!
! QCOMP - Nutation component name
! QPHASE - Additional label for nutation component
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a2jst
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      CHARACTER*8 QTYP
!
! QTYP -
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  NAMNUT PROGRAM STRUCTURE
!
      IF(TYP.EQ.'PSI') THEN
        QTYP='Nut Long'
      ELSE
        QTYP='Nut Obl '
      ENDIF
      IF(I.LE.2) THEN
        QCOMP=QTYP
        QPHASE='Constant'
        IF(I.EQ.2) QPHASE='Slope/year'
      ELSE IF(I.LE.4) THEN
        WRITE(QCOMP,"(A,"" FCN "",F7.2)") QTYP,FCNPER
        QPHASE='Cosine'
        IF(I.EQ.4) QPHASE='  Sine'
      ELSE
        WRITE(QCOMP,"(A,"" CALC Term "",I3)") QTYP,(I-4+1)/2
        QPHASE='In-phase'
        IF ( (MOD(I,INT2(2)) .EQ. 1 .AND. TYP .EQ. 'PSI' ) .OR. &
     &       (MOD(I,INT2(2)) .EQ. 0 .AND. TYP .EQ. 'EPS')       ) THEN
             QPHASE='Out-of-phase'
        END IF
      ENDIF
      RETURN
      END
