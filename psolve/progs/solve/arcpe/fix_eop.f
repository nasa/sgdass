       SUBROUTINE FIX_EOP ( B3DOBJ, B1B3DOBJ, ARR, IOFF, LPARM, NPARM )
! ************************************************************************
! *                                                                      *
! *                                                                      *
! *    Constraint all EOP estimates. Constraints are so large that EOP   *
! *    parameters are actually swichted off.                             *
! *                                                                      *
! *  ###  ???           FIX_EOP    v2.0  (c)  ???        05-MAR-97  ###  *
! *                                                                      *
! ************************************************************************
       IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
       INCLUDE   'solve.i'
       INCLUDE   'glbc4.i'
       INCLUDE   'fast.i'
       TYPE ( B3D__STRU ) ::    B3DOBJ
       TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
       REAL*8     ARR(*)
       CHARACTER  LPARM(*)*20, TYP*1
       INTEGER*2  NPARM
       REAL*8     BIG
       INTEGER*4  IPARM
       INTEGER*4  IOFF, IR, IC
       INTEGER*8, EXTERNAL :: INDX8
       ADDRESS__TYPE :: IAD_DIAG, IPTR
       INTEGER*2  INT2_ARG
       INTEGER*4  INT4
       INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
       ADDRESS__TYPE :: FULL_B1B3D
!
!      Transformation INT2 --> INT4
!CCCCCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
!   pet  970305  Added support of B1B3D parametrization
!
!CCCCCC
       BIG=1.D25
!
!
       DO IPARM=1,NPARM
          IF ( LPARM(IPARM)(1:8) .EQ. "X WOBBLE" .OR. &
     &         LPARM(IPARM)(1:8) .eq. "Y WOBBLE"      ) THEN
               BIG=5.D26
            ELSE IF ( LPARM(IPARM)(1:7) .eq. "UT1-TAI ") THEN
             BIG=1.D25
            ELSE IF ( LPARM(IPARM)(1:18) .EQ. "LONGITUDE NUTATION" .OR. &
     &                LPARM(IPARM)(1:18) .EQ. "OBLIQUITY NUTATION"     ) THEN
               BIG=1.D27
            ELSE
               GOTO 10
          ENDIF
!
          IF ( FAST_MODE .EQ. F__NONE .OR.  FAST_MODE .EQ. F__PRD  .OR. &
     &         FAST_MODE .EQ. F__B1D  .OR.  FAST_MODE .EQ. F__B3D      ) THEN
!
! ------------ Full matrix case
!
               IPTR=INDX8(IPARM,IPARM) + IOFF
               ARR ( IPTR ) = ARR ( IPTR ) + BIG
             ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------ B1B3D case
!
               IAD_DIAG = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, IPARM, &
     &                                 IPARM, TYP, %VAL(0), IR, IC )
               IF ( TYP .NE. 'L' ) THEN
                    CALL ERR_LOG ( 8571, -1, 'FIX_EOP', 'Internal '// &
     &                  'error: typ is "'//TYP//'". Expected "L"' )
                    STOP 'ARCPE: abnormal termination'
               END IF
               CALL R8_UPDATE ( IAD_DIAG, BIG )
          END IF
!
10        CONTINUE
       END DO
!
       RETURN
       END  !#!  FIX_EOP  #!#
