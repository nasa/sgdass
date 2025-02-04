#include <mk5_preprocessor_directives.inc>
      SUBROUTINE INVSP ( M, MAT, RCOND, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  INVSP  inverts a square symmetric positively defined   *
! *   matrix.                                                            *
! *                                                                      *
! *  ###  10-DEC-96    INVSP     v2.0  (c)  L. Petrov  07-OCT-2002  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definition of DB__INVMAT_MIN
      INTEGER*4  M, IUER
      REAL*8     MAT(*), RCOND
      REAL*8     Z(MAX__DIM), DET, MAT1_OLD
      CHARACTER  STR*12, STR1*12
      INTEGER*4  IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( M .EQ. 1 ) THEN
           IF ( DABS(MAT(1)) .LT. 1.D0/COND__MAX ) THEN
                CALL ERR_LOG ( 1102, IUER, 'INVSP', 'Matrix is (almost) '// &
     &                        'singular' )
                RETURN
              ELSE
                RCOND = 1.0
                MAT(1) = 1.D0/MAT(1)
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
         ELSE IF ( M .EQ. 2 ) THEN
           DET = MAT(1)*MAT(3) - MAT(2)**2
           IF ( DABS(DET) .LT. 1.D0/COND__MAX*MAT(1)*MAT(3) .OR. &
     &          DABS(DET) .GT. COND__MAX*MAT(1)*MAT(3)           ) THEN
                RCOND = DET
                WRITE ( UNIT=STR, FMT='(1PG12.4 )' ) DET
!
                WRITE ( UNIT=6, FMT=110 ) 1, 1, MAT(1)
                WRITE ( UNIT=6, FMT=110 ) 1, 2, MAT(2)
                WRITE ( UNIT=6, FMT=110 ) 2, 2, MAT(3)
 110            FORMAT ( 'MAT(', I1, ',', I1, ')=', 1PG22.15 )
                CALL ERR_LOG ( 1104, IUER, 'INVSP', 'Matrix is (almost) '// &
     &                        'singular: determinant = '//STR )
                RETURN
           END IF
           MAT1_OLD = MAT(1)
           MAT(1) =  MAT(3)/DET
           MAT(2) = -MAT(2)/DET
           MAT(3) = MAT1_OLD/DET
           RCOND = DET
         ELSE
!
           CALL ERR_PASS ( IUER, IER )
#ifdef HPUX
           IF ( M .LE. DB__INVMAT_MIN ) THEN
                CALL DPPCO_VEC  ( M, MAT, COND__MAX, RCOND, Z, IER )
              ELSE
                CALL DPPCO_BLAS ( M, MAT, COND__MAX, RCOND, Z, IER )
           END IF
#else
           CALL DPPCO_BLAS ( M, MAT, COND__MAX, RCOND, Z, IER )
#endif
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1104, IUER, 'INVSP', 'Matrix is (almost) '// &
     &                        'singular' )
                RETURN
           END IF
!
           IF ( RCOND .LT. 1.D0/COND__MAX  .OR.  RCOND .GT. COND__MAX ) THEN
                CALL CLRCH   ( STR )
                WRITE ( UNIT=STR, FMT='(1PD12.4)' ) RCOND
                CALL CHASHL  ( STR )
!
                CALL CLRCH   ( STR1 )
                WRITE ( UNIT=STR1, FMT='(1PD12.4)' ) COND__MAX
                CALL CHASHL  ( STR1 )
!
                CALL ERR_LOG ( 1106, IUER, 'INVSP', 'Matrix is almost '// &
     &              'singular: Condition number = '//STR(1:I_LEN(STR))// &
     &              ' what exceeds the specified limit: '//STR1 )
                RETURN
           END IF
!
#ifdef HPUX
           IF ( M .LE. DB__INVMAT_MIN ) THEN
                CALL DPPIN_VEC  ( M, MAT )
              ELSE
                CALL DPPIN_BLAS ( M, MAT )
           END IF
#else
           CALL DPPIN_BLAS ( M, MAT )
#endif
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  INVSP  #!#
