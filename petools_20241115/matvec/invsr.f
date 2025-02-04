      SUBROUTINE INVSR ( N, MAT, RC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine INVS invertes the square symmetric matrix in packed upper  *
! *   triangular representation.                                         *
! *                                                                      *
! *  ###  19-NOV-2002     INVSR    v1.0 (c)  L. Petrov  19-NOV-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      INTEGER*4  N, IUER
      REAL*8     MAT(*), RC
      REAL*8     Z(MAX__DIM), DET, MAT1_OLD
      REAL*8     EM_MAX, EM_MIN, ER, EMVS
      CHARACTER  STR*32
      INTEGER*4  IT, IER
      INTEGER*4, EXTERNLAL :: I_LEN
!
      IF ( N .EQ. 1 ) THEN
           IF ( DABS(MAT(1)) .LT. 1.D0/EPS__MATVEC ) THEN
                CALL ERR_LOG ( 1281, IUER, 'INVSR', 'Matrix is '// &
     &              '(almost) singular' )
                RETURN
              ELSE
                RC = 1.0
                MAT(1) = 1.D0/MAT(1)
                CALL ERR_LOG ( 0, IUER )
                RETURN
           END IF
         ELSE IF ( N .EQ. 2 ) THEN
           DET = MAT(1)*MAT(3) - MAT(2)**2
           IF ( DABS(DET) .LT. 1.D0/EPS__MATVEC*MAT(1)*MAT(3) .OR. &
     &          DABS(DET) .GT. EPS__MATVEC*MAT(1)*MAT(3)           ) THEN
                RC = DET
                WRITE ( UNIT=STR, FMT='(1PG12.4 )' ) DET
!
                WRITE ( UNIT=6, FMT=110 ) 1, 1, MAT(1)
                WRITE ( UNIT=6, FMT=110 ) 1, 2, MAT(2)
                WRITE ( UNIT=6, FMT=110 ) 2, 2, MAT(3)
 110            FORMAT ( 'MAT(', I1, ',', I1, ')=', 1PG22.15 )
                CALL ERR_LOG ( 1282, IUER, 'INVSR', 'Matrix is '// &
     &                        '(almost) singular: determinant = '//STR )
                RETURN
           END IF
           MAT1_OLD = MAT(1)
           MAT(1) =  MAT(3)/DET
           MAT(2) = -MAT(2)/DET
           MAT(3) = MAT1_OLD/DET
           RC = DET
!!         ELSE IF ( N .LT. 32 ) THEN
!!           CALL ERR_LOG ( 0, IUER )
!!           RETURN
         ELSE
!
! -------- Compute the maximal eigen value of initial matrix
!
!           CALL ERR_PASS ( IUER, IER )
!           EM_MAX = EMVS ( N, MAT, IT, ER, IER )
!           IF ( IER .NE. 0 ) THEN
!                CALL ERR_LOG ( 1283, IUER, 'INVS', 'Matrix is not '// &
!     &              'a positively determined' )
!                RETURN
!           END IF
!
! -------- Invert the matrix
!
           CALL DREORDER ( 'U', 'N', 'R', N, MAT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1284, IUER, 'INVS', 'Error in DREORDER '// &
     &              'IER = '//STR )
                RETURN
           END IF
!
           CALL DRPPTRF  ( 'U', N, MAT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1285, IUER, 'INVS', 'Error in DRPPTRF '// &
     &              'IER = '//STR )
                RETURN
           END IF
!
           CALL DRPPTRI  ( 'U', N, MAT )
!
           CALL DREORDER ( 'U', 'N', 'P', N, MAT, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1286, IUER, 'INVS', 'Error in DREORDER '// &
     &              'IER = '//STR )
                RETURN
           END IF
!
! -------- Compute the maximal eigen value of inverted matrix
!
!           CALL ERR_PASS ( IUER, IER )
!           EM_MIN = EMVS ( N, MAT, IT, ER, IER )
!           IF ( IER .NE. 0 ) THEN
!                CALL ERR_LOG ( 1287, IUER, 'FUNC_INVS', 'Matrix is not '// &
!     &              'a positively determined' )
!                RETURN
!           END IF
!!
!! -------- Compute condition number
!!
!           RC = EM_MAX*EM_MIN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  INVSR  #!#
