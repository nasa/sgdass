#ifdef GEN_PREFIX
#define FUNC_INVSF        GEN_INVSF
#define FUNC_BASIC_INVSF  GEN_BASIC_INVSF
#else
#define FUNC_INVSF        OPT_INVSF
#define FUNC_BASIC_INVSF  BASIC_INVSF
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_INVSF ( N, MAT, RC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine INVSF inverts the square symmetric matrix in packed upper  *
! *   triangular representation.                                         *
! *                                                                      *
! *  ### 18-AUG-2002       INVSF    v1.2 (c)  L. Petrov 17-SEP-2014 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'matvec.i'
      INTEGER*4  N, IUER
      REAL*8     MAT(*), RC 
      REAL*8     Z(MAX__DIM), DET, MAT1_OLD, MAT_INV(6), MATO(6)
      REAL*8     EM_MAX, EM_MIN, ER, EMVS 
      REAL*8     COND_MAX
      CHARACTER  STR*32
      INTEGER*4  IT, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      COND_MAX = 1.0D11
      IF ( N .EQ. 1 ) THEN
           IF ( DABS(MAT(1)) .LT. 1.D0/COND__MAX ) THEN
                CALL ERR_LOG ( 1201, IUER, 'FUNC_INVSF', 'Matrix is '// &
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
                CALL ERR_LOG ( 1202, IUER, 'FUNC_INVSF', 'Matrix is '// &
     &                        '(almost) singular: determinant = '//STR )
                RETURN
           END IF
           MAT1_OLD = MAT(1)
           MAT(1) =  MAT(3)/DET
           MAT(2) = -MAT(2)/DET
           MAT(3) = MAT1_OLD/DET
           RC = DET
         ELSE IF ( N .EQ. 3 ) THEN
!
! -------- Direct computation of the determinant
!
           DET =   MAT(1)*( MAT(6)*MAT(3) - MAT(5)*MAT(5) ) &
     &           - MAT(2)*( MAT(6)*MAT(2) - MAT(5)*MAT(4) ) &
     &           + MAT(4)*( MAT(5)*MAT(2) - MAT(3)*MAT(4) ) 
           IF ( DET < MIN_VAL ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:24), FMT='(1PD24.16)' ) DET
                CALL ERR_LOG ( 1214, IUER, 'INVS', '3x3 matrix is '// &
     &                        '(almost) singular: determinant = '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:6)
                RETURN
           END IF
!
! -------- Direct computation of the inverse matrix
!
           MAT_INV(1) =   MAT(6)*MAT(3) - MAT(5)*MAT(5)
           MAT_INV(2) = -(MAT(6)*MAT(2) - MAT(5)*MAT(4) )   
           MAT_INV(3) =   MAT(6)*MAT(1) - MAT(4)*MAT(4)
           MAT_INV(4) =   MAT(5)*MAT(2) - MAT(3)*MAT(4)
           MAT_INV(5) = -(MAT(5)*MAT(1) - MAT(2)*MAT(4) )
           MAT_INV(6) =   MAT(3)*MAT(1) - MAT(2)*MAT(2)
!
! -------- And then scale it by 1/DET
!
           MAT_INV(1:6) = MAT_INV(1:6)/DET
!
! -------- Now compute the product ot the inverse matix and the inital matrix
!
           MATO(1) = MAT(1)*MAT_INV(1) + MAT(2)*MAT_INV(2) + MAT(4)*MAT_INV(4) 
           MATO(2) = MAT(2)*MAT_INV(1) + MAT(3)*MAT_INV(2) + MAT(5)*MAT_INV(4) 
           MATO(4) = MAT(4)*MAT_INV(1) + MAT(5)*MAT_INV(2) + MAT(6)*MAT_INV(4) 
!
           MATO(3) = MAT(2)*MAT_INV(2) + MAT(3)*MAT_INV(3) + MAT(5)*MAT_INV(5) 
           MATO(5) = MAT(4)*MAT_INV(2) + MAT(5)*MAT_INV(3) + MAT(6)*MAT_INV(5) 
!
           MATO(6) = MAT(4)*MAT_INV(4) + MAT(5)*MAT_INV(5) + MAT(6)*MAT_INV(6) 
!
! -------- Now compute the maximum by modulo residual
!
           RC = -1.D30
           RC = MAX( DABS(MATO(1)-1.0D0), RC )
           RC = MAX( DABS(MATO(2)),       RC )
           RC = MAX( DABS(MATO(3)-1.0D0), RC )
           RC = MAX( DABS(MATO(4)),       RC )
           RC = MAX( DABS(MATO(5)),       RC )
           RC = MAX( DABS(MATO(6)-1.0D0), RC )
           RC = RC_REF*RC
           IF ( RC > COND_MAX ) THEN
                CALL CLRCH ( STR )
                WRITE ( UNIT=STR(1:24), FMT='(1PD24.16)' ) RC
                CALL ERR_LOG ( 1215, IUER, 'INVS', '3x3 matrix is '// &
     &                        '(almost) singular. Maximum residual '// &
     &                        'is too high: '//STR )
                WRITE ( 6, * ) 'MAT = ', MAT(1:10)
                RETURN
           END IF
!
           MAT(1:6) = MAT_INV(1:6)
           IUER = 0
           RETURN
         ELSE
!
! -------- Compute the maximal eigen value of initial matrix
!
           CALL ERR_PASS ( IUER, IER ) 
           EM_MAX = EMVS ( N, MAT, IT, ER, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1203, IUER, 'FUNC_INVSF', 'Matrix is not '// &
     &              'a positively determined' ) 
                RETURN 
           END IF
!
! -------- Invert the matrix
!
           CALL FUNC_BASIC_INVSF ( N, MAT, RC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( IER, STR )
                RC = -1.D0
                CALL ERR_LOG ( -IER, IUER, 'FUNC_INVSF', 'Error in matrix '// &
     &              'inversion at the '//STR(1:I_LEN(STR))//'-th step' )
                RETURN 
           END IF
!
! -------- Compute the maximal eigen value of inverted matrix
!
           CALL ERR_PASS ( IUER, IER ) 
           EM_MIN = EMVS ( N, MAT, IT, ER, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1204, IUER, 'FUNC_INVSF', 'Matrix is not '// &
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
      END  !#!  
!
! ------------------------------------------------------------------------
!
        SUBROUTINE FUNC_BASIC_INVSF ( N, A, RC, IERR )
! ************************************************************************
! *                                                                      *
! *   Simple-minded program for inversion of square symmetrical matrix.
! *                                                                      *
! *  ### 17-FEB-1991  BASIC_INVSF   v1.1 (c)  L. Petrov  17-SEP-2014 ### *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  N, IERR
        REAL*8     A(*), S, RC, EPS
        PARAMETER ( EPS=1.D-15 )
        INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, K, K_BEG, &
     &             KK, K2, K4, K9, K10, K2_BEG, K4_BEG, K68, K87, K68_BEG, &
     &             K87_BEG, K9_BEG, K10_BEG
        REAL*8,    EXTERNAL :: DP_VV_V 
!
        RC = 1.0D0
!
        IF ( A(1).LT.EPS ) THEN
             IERR=1
             RETURN
        END IF
        A(1)=1.D0/DSQRT ( A(1) )
        S=A(1)
        K=2
        DO 410 J1=2,N
           A(K)=A(K)*S
           K=K+J1
  410   CONTINUE
!
        K_BEG=2
        DO 420 J2=2,N
           K = K_BEG+J2-1
           A(K) = A(K) - DP_VV_V ( J2-1, A(K_BEG), A(K_BEG) )
           IF ( A(K).LT.EPS ) THEN
                IERR=J2
                RETURN
           END IF
           A(K)=1.D0/DSQRT ( A(K) )
           K4_BEG=K_BEG+J2
           K4 = K4_BEG+J2-1
           IF ( J2.EQ.N ) GOTO 420
           DO 440 J4=J2+1,N
              A(K4) = ( A(K4) - DP_VV_V ( J2-1, A(K_BEG), A(K4_BEG) ) )*A(K)
              K4_BEG=K4_BEG+J4
              K4=K4+J4
  440      CONTINUE
           K_BEG=K_BEG+J2
  420   CONTINUE
!
        K68_BEG=0
        DO 460 J6=1,N-1
           K68_BEG=K68_BEG+J6
           K87_BEG=K68_BEG+J6
           DO 470 J7=J6+1,N
              S=0.D0
              K68=K68_BEG
              K87=K87_BEG
              DO 480 J8=J6,J7-1
                 S=S-A(K68)*A(K87)
                 K68=K68+J8
                 K87=K87+1
  480         CONTINUE
              A(K68)=S*A(K87)
              K87_BEG=K87_BEG+J7
  470      CONTINUE
  460   CONTINUE
!
        K9_BEG=0
        DO 490 J9=1,N
           K10_BEG=K9_BEG
           DO 4100 J10=J9,N
              S=0.D0
              K9=J9+K10_BEG
              KK=K9
              K10=J10+K10_BEG
              DO 4110 J11=J10,N
                 S=S+A(K9)*A(K10)
                 K9=K9+J11
                 K10=K10+J11
 4110         CONTINUE
              K10=K10_BEG+J10
              A(KK)=S
              K10_BEG=K10_BEG+J10
 4100      CONTINUE
           K9_BEG=K9_BEG+J9
  490   CONTINUE
!
        IERR = 0
        RETURN
        END  !#!  FUNC_BASIC_INVSF  #!#
