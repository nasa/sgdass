#include <mk5_preprocessor_directives.inc>
      SUBROUTINE INVSL ( N, MAT, RC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine INVL invertes the square symmetric matrix in packed upper  *
! *   triangular representation.                                         *
! *                                                                      *
! *  ###  19-NOV-2002     INVSL    v1.0 (c)  L. Petrov  19-NOV-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'matvec.i'
      INTEGER*4  N, IUER
      REAL*8     MAT(*), RC 
      REAL*8     Z(MAX__DIM), DET, MAT1_OLD
      REAL*8     EM_MAX, EM_MIN, ER, EMVS 
      REAL*8,    ALLOCATABLE :: RCT_MAT(:,:)
      CHARACTER  STR*32
      INTEGER*4  IT, LC, J1, J2, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( N .EQ. 1 ) THEN
           IF ( DABS(MAT(1)) .LT. 1.D0/COND__MAX ) THEN
                CALL ERR_LOG ( 1281, IUER, 'INVSL', 'Matrix is '// &
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
           IF ( DABS(DET) .LT. 1.D0/COND__MAX*MAT(1)*MAT(3) .OR. &
     &          DABS(DET) .GT. COND__MAX*MAT(1)*MAT(3)           ) THEN
                RC = DET
                WRITE ( UNIT=STR, FMT='(1PG12.4 )' ) DET
!
                WRITE ( UNIT=6, FMT=110 ) 1, 1, MAT(1)
                WRITE ( UNIT=6, FMT=110 ) 1, 2, MAT(2)
                WRITE ( UNIT=6, FMT=110 ) 2, 2, MAT(3)
 110            FORMAT ( 'MAT(', I1, ',', I1, ')=', 1PG22.15 )
                CALL ERR_LOG ( 1282, IUER, 'INVSL', 'Matrix is '// &
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
           CALL ERR_PASS ( IUER, IER ) 
           EM_MAX = EMVS ( N, MAT, IT, ER, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1283, IUER, 'INVSL', 'Matrix is not '// &
     &              'a positively determined' ) 
                RETURN 
           END IF
!
           ALLOCATE ( RCT_MAT(N,N), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1284, IUER, 'INVSL', 'Error in ALLOCATE '// &
     &              'IER = '//STR )
                RETURN 
           END IF
!
           CALL NOUT_R8 ( N*N, RCT_MAT ) 
!
           LC = 1
           DO 410 J1=1,N
              CALL DCOPY ( J1, MAT(LC), 1, RCT_MAT(1,J1), 1 )
              LC = LC + J1
 410       CONTINUE 
!
           CALL DPOTRF ( 'U', N, RCT_MAT, N, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1285, IUER, 'INVSL', 'Error in DPOTRF '// &
     &              'IER = '//STR )
                DEALLOCATE ( RCT_MAT )
                RETURN 
           END IF
!
           CALL DPOTRI ( 'U', N, RCT_MAT, N, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1286, IUER, 'INVSL', 'Error in DOTPRI '// &
     &              'IER = '//STR )
                DEALLOCATE ( RCT_MAT )
                RETURN 
           END IF
!
           LC = 1
           DO 420 J2=1,N
              CALL DCOPY ( J2, RCT_MAT(1,J2), 1, MAT(LC), 1 )
              LC = LC + J2
 420       CONTINUE 
           DEALLOCATE ( RCT_MAT )
!
! -------- Compute the maximal eigen value of inverted matrix
!
           CALL ERR_PASS ( IUER, IER ) 
           EM_MIN = EMVS ( N, MAT, IT, ER, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1287, IUER, 'INVSL', 'Matrix is not '// &
     &              'a positively determined' ) 
                RETURN 
           END IF
!
! -------- Compute condition number
!
           RC = EM_MAX*EM_MIN
      END IF
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  INVSL  #!#
