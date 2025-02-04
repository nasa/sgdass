#ifdef GEN_PREFIX
#define FUNC_MUL_MM_ST_I  GEN_MUL_MM_ST_I
#else
#define FUNC_MUL_MM_ST_I  OPT_MUL_MM_ST_I
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_MUL_MM_ST_I ( M1, MAT1, M2, N2, MAT2, M3, N3, MATO, &
     &                              IER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MM_ST_I  multiplies matrices:                      *
! *   MATO = MAT1 * MAT2                                                 *
! *                                                                      *
! *   MAT1 -- symmetric matrix in upper triangular representation;       *
! *   MAT2 -- transpose of the rectangular matrix;                       *
! *   MATO -- rectangular matrix.                                        *
! *                                                                      *
! *  ###  11-DEC-96  MUL_MM_ST_I    v4.1 (c)  L. Petrov 29-DEC-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB1__MUL_MM_SI_I, DB2__MUL_MM_SI_I
      INTEGER*4  M1, M2, N2, M3, N3, IER
      REAL*8     MAT1(*), MAT2(M2,N2), MATO(M3,N3)
      CHARACTER  STR*80
!
      REAL*8,    ALLOCATABLE :: MAT1C(:,:,:), MAT2C(:,:,:), MAT3C(:,:,:)
      INTEGER*4  LC, K, IP1, IP2, J1, J2, J3, J4, J5, J6, &
     &           IND_1, IND_2, IND_M, DIM_1, DIM_2, DIM_M, &
     &           IND_THR, NTHR, NTHR_SAVED, IERR
      REAL*8     S
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
      IF ( M1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. N2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
!
      IF ( N2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. M3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
!
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
!CC
#ifdef GEN_PREFIX
      DO 410 J1=1,N3
         DO 420 J2=1,M3
            S=0.D0
            DO 430 J3=1,M1
               LC=LOCS(J2,J3)
               S = S + MAT1(LC)*MAT2(J1,J3)
 430        CONTINUE
            MATO(J2,J1)=S
 420     CONTINUE
 410  CONTINUE
      IER = 0
      RETURN
#else
!
! === Actual version
!
      IF ( M1 .EQ. 2 ) THEN
           DO 510 J1=1,M3
              MATO(1,J1) = MAT1(1)*MAT2(J1,1) + MAT1(2)*MAT2(J1,2) 
              MATO(2,J1) = MAT1(2)*MAT2(J1,1) + MAT1(3)*MAT2(J1,2) 
 510       CONTINUE 
        ELSE IF ( M1 .EQ. 3 ) THEN
           DO 520 J1=1,M3
              MATO(1,J1) = MAT1(1)*MAT2(J1,1) + MAT1(2)*MAT2(J1,2) + &
     &                     MAT1(4)*MAT2(J1,3) 
              MATO(2,J1) = MAT1(2)*MAT2(J1,1) + MAT1(3)*MAT2(J1,2) + &
     &                     MAT1(5)*MAT2(J1,3) 
              MATO(3,J1) = MAT1(4)*MAT2(J1,1) + MAT1(5)*MAT2(J1,2) + &
     &                     MAT1(6)*MAT2(J1,3) 
 520       CONTINUE 
        ELSE IF ( M1 .EQ. 4 ) THEN
           DO 530 J1=1,M3
              MATO(1,J1) = MAT1(1)*MAT2(J1,1) + MAT1(2)*MAT2(J1,2) + &
     &                     MAT1(4)*MAT2(J1,3) + MAT1(7)*MAT2(J1,4) 
              MATO(2,J1) = MAT1(2)*MAT2(J1,1) + MAT1(3)*MAT2(J1,2) + &
     &                     MAT1(5)*MAT2(J1,3) + MAT1(8)*MAT2(J1,4) 
              MATO(3,J1) = MAT1(4)*MAT2(J1,1) + MAT1(5)*MAT2(J1,2) + &
     &                     MAT1(6)*MAT2(J1,3) + MAT1(9)*MAT2(J1,4) 
              MATO(4,J1) = MAT1(7)*MAT2(J1,1) + MAT1(8)*MAT2(J1,2) + &
     &                     MAT1(9)*MAT2(J1,3) + MAT1(10)*MAT2(J1,4) 
 530       CONTINUE 
        ELSE IF ( M1 .EQ. 5 ) THEN
           DO 540 J1=1,M3
              MATO(1,J1) = MAT1(1)*MAT2(J1,1) + MAT1(2)*MAT2(J1,2) + &
     &                     MAT1(4)*MAT2(J1,3) + MAT1(7)*MAT2(J1,4) + &
     &                     MAT1(11)*MAT2(J1,5) 
              MATO(2,J1) = MAT1(2)*MAT2(J1,1) + MAT1(3)*MAT2(J1,2) + &
     &                     MAT1(5)*MAT2(J1,3) + MAT1(8)*MAT2(J1,4) + &
     &                     MAT1(12)*MAT2(J1,5) 
              MATO(3,J1) = MAT1(4)*MAT2(J1,1) + MAT1(5)*MAT2(J1,2) + &
     &                     MAT1(6)*MAT2(J1,3) + MAT1(9)*MAT2(J1,4) + &
     &                     MAT1(13)*MAT2(J1,5) 
              MATO(4,J1) = MAT1(7)*MAT2(J1,1) + MAT1(8)*MAT2(J1,2) + &
     &                     MAT1(9)*MAT2(J1,3) + MAT1(10)*MAT2(J1,4) + &
     &                     MAT1(14)*MAT2(J1,5) 
              MATO(5,J1) = MAT1(11)*MAT2(J1,1) + MAT1(12)*MAT2(J1,2) + &
     &                     MAT1(13)*MAT2(J1,3) + MAT1(14)*MAT2(J1,4) + &
     &                     MAT1(15)*MAT2(J1,5) 
 540       CONTINUE 
        ELSE IF ( M1 .EQ. 6 ) THEN
           DO 550 J1=1,M3
              MATO(1,J1) = MAT1(1)*MAT2(J1,1)  + MAT1(2)*MAT2(J1,2)  + &
     &                     MAT1(4)*MAT2(J1,3)  + MAT1(7)*MAT2(J1,4)  + &
     &                     MAT1(11)*MAT2(J1,5) + MAT1(16)*MAT2(J1,6) 
              MATO(2,J1) = MAT1(2)*MAT2(J1,1)  + MAT1(3)*MAT2(J1,2)  + &
     &                     MAT1(5)*MAT2(J1,3)  + MAT1(8)*MAT2(J1,4)  + &
     &                     MAT1(12)*MAT2(J1,5) + MAT1(17)*MAT2(J1,6) 
              MATO(3,J1) = MAT1(4)*MAT2(J1,1)  + MAT1(5)*MAT2(J1,2)  + &
     &                     MAT1(6)*MAT2(J1,3)  + MAT1(9)*MAT2(J1,4)  + &
     &                     MAT1(13)*MAT2(J1,5) + MAT1(18)*MAT2(J1,6) 
              MATO(4,J1) = MAT1(7)*MAT2(J1,1)  + MAT1(8)*MAT2(J1,2)  + &
     &                     MAT1(9)*MAT2(J1,3)  + MAT1(10)*MAT2(J1,4) + &
     &                     MAT1(14)*MAT2(J1,5) + MAT1(19)*MAT2(J1,6) 
              MATO(5,J1) = MAT1(11)*MAT2(J1,1) + MAT1(12)*MAT2(J1,2) + &
     &                     MAT1(13)*MAT2(J1,3) + MAT1(14)*MAT2(J1,4) + &
     &                     MAT1(15)*MAT2(J1,5) + MAT1(20)*MAT2(J1,6) 
              MATO(6,J1) = MAT1(16)*MAT2(J1,1) + MAT1(17)*MAT2(J1,2) + &
     &                     MAT1(18)*MAT2(J1,3) + MAT1(19)*MAT2(J1,4) + &
     &                     MAT1(20)*MAT2(J1,5) + MAT1(21)*MAT2(J1,6) 
 550       CONTINUE 
        ELSE IF ( M1 .EQ. 7 ) THEN
           DO 560 J1=1,M3
              MATO(1,J1) = MAT1(1)*MAT2(J1,1)  + MAT1(2)*MAT2(J1,2)  + &
     &                     MAT1(4)*MAT2(J1,3)  + MAT1(7)*MAT2(J1,4)  + &
     &                     MAT1(11)*MAT2(J1,5) + MAT1(16)*MAT2(J1,6) + &
     &                     MAT1(22)*MAT2(J1,7) 
              MATO(2,J1) = MAT1(2)*MAT2(J1,1)  + MAT1(3)*MAT2(J1,2)  + &
     &                     MAT1(5)*MAT2(J1,3)  + MAT1(8)*MAT2(J1,4)  + &
     &                     MAT1(12)*MAT2(J1,5) + MAT1(17)*MAT2(J1,6) + &
     &                     MAT1(23)*MAT2(J1,7) 
              MATO(3,J1) = MAT1(4)*MAT2(J1,1)  + MAT1(5)*MAT2(J1,2)  + &
     &                     MAT1(6)*MAT2(J1,3)  + MAT1(9)*MAT2(J1,4)  + &
     &                     MAT1(13)*MAT2(J1,5) + MAT1(18)*MAT2(J1,6) + &
     &                     MAT1(24)*MAT2(J1,7) 
              MATO(4,J1) = MAT1(7)*MAT2(J1,1)  + MAT1(8)*MAT2(J1,2)  + &
     &                     MAT1(9)*MAT2(J1,3)  + MAT1(10)*MAT2(J1,4) + &
     &                     MAT1(14)*MAT2(J1,5) + MAT1(19)*MAT2(J1,6) + &
     &                     MAT1(25)*MAT2(J1,7) 
              MATO(5,J1) = MAT1(11)*MAT2(J1,1) + MAT1(12)*MAT2(J1,2) + &
     &                     MAT1(13)*MAT2(J1,3) + MAT1(14)*MAT2(J1,4) + &
     &                     MAT1(15)*MAT2(J1,5) + MAT1(20)*MAT2(J1,6) + &
     &                     MAT1(26)*MAT2(J1,7) 
              MATO(6,J1) = MAT1(16)*MAT2(J1,1) + MAT1(17)*MAT2(J1,2) + &
     &                     MAT1(18)*MAT2(J1,3) + MAT1(19)*MAT2(J1,4) + &
     &                     MAT1(20)*MAT2(J1,5) + MAT1(21)*MAT2(J1,6) + &
     &                     MAT1(27)*MAT2(J1,7) 
              MATO(7,J1) = MAT1(22)*MAT2(J1,1) + MAT1(23)*MAT2(J1,2) + &
     &                     MAT1(24)*MAT2(J1,3) + MAT1(25)*MAT2(J1,4) + &
     &                     MAT1(26)*MAT2(J1,5) + MAT1(27)*MAT2(J1,6) + &
     &                     MAT1(28)*MAT2(J1,7) 
 560       CONTINUE 
        ELSE IF ( M1 .LT. DB1__MUL_MM_ST_I ) THEN
!
! -------- Small dimensions
!
           DO 410 J1=1,N3 
              MATO(1,J1) = MAT1(1)*MAT2(J1,1)
              K = 2
              DO 420 J2=2,M1
                 S = 0.0D0
                 DO 430 J3=1,J2-1
                    MATO(J3,J1) = MATO(J3,J1) + MAT1(K)*MAT2(J1,J2)
                    S = S + MAT1(K)*MAT2(J1,J3)
                    K = K+1
 430             CONTINUE
                 MATO(J2,J1) = S + MAT1(K)*MAT2(J1,J2)
                 K = K+1
 420          CONTINUE
 410       CONTINUE
        ELSE IF ( M1 .LE. DB2__MUL_MM_ST_I ) THEN
!
! -------- Medium dimensions
!
! -------- Allocate memory for a temporary copy of the left operand
!
           ALLOCATE ( MAT1C(M1,M1,1), STAT=IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*M1*M1, STR )
                CALL ERR_LOG ( 18, IER, 'MUL_MM_ST_I', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                RETURN 
           END IF
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT1C, 0, %VAL(8*M1*M1) )
#endif
!
! -------- Re-arrange matrix from upper triangular rerpresntation to
! -------- rectangular representation
!
           CALL TRG_GATHER ( M1, 1, M1, 1, M1, MAT1, MAT1C )
!
           CALL DGEMM ( 'N', 'T', M1, M2, N2, 1.D0, MAT1C, &
     &                   M1, MAT2, M2, 0.D0, MATO, M3 )
!
           DEALLOCATE ( MAT1C )
        ELSE
!
! -------- Large dimensions
!
           CALL NOUT8_R8 ( INT8(M3)*INT8(N3), MATO )  ! Initialization
!
           IP1 = M3/DB3__MUL_MM_ST_I 
           IF ( IP1*DB3__MUL_MM_ST_I .LT. M3 ) IP1 = IP1 + 1
           IP2 = N3/DB3__MUL_MM_ST_I 
           IF ( IP2*DB3__MUL_MM_ST_I .LT. N3 ) IP2 = IP2 + 1
!
! -------- Set the number of threads
!
           NTHR_SAVED = OMP_GET_MAX_THREADS()
           IF ( OMP_IN_PARALLEL() ) THEN
                NTHR = 1
             ELSE 
                NTHR = MIN ( IP1, OMP_GET_MAX_THREADS() )
           END IF
!
! -------- Allocate memory for temporary copies of operands
!
           ALLOCATE ( MAT1C(DB3__MUL_MM_ST_I, DB3__MUL_MM_ST_I,NTHR), &
     &                MAT2C(DB3__MUL_MM_ST_I, DB3__MUL_MM_ST_I,NTHR), &
     &                MAT3C(DB3__MUL_MM_ST_I, DB3__MUL_MM_ST_I,NTHR), &
     &                STAT=IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*3*DB3__MUL_MM_ST_I*DB3__MUL_MM_ST_I, STR )
                CALL ERR_LOG ( 20, IER, 'MUL_MM_ST_I', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                RETURN 
           END IF
           CALL OMP_SET_NUM_THREADS( %VAL(NTHR) ) 
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT1C, 0, %VAL(8*DB3__MUL_MM_ST_I*DB3__MUL_MM_ST_I,NTHR) )
           CALL MEMSET ( MAT2C, 0, %VAL(8*DB3__MUL_MM_ST_I*DB3__MUL_MM_ST_I,NTHR) )
           CALL MEMSET ( MAT2C, 0, %VAL(8*DB3__MUL_MM_ST_I*DB3__MUL_MM_ST_I,NTHR) )
#endif
!
           IND_2 = 1
           DO 440 J4=1,IP2 ! columns of mato
              DIM_2 = DB3__MUL_MM_ST_I
              IF ( IND_2 + DIM_2-1 .GT. N3 ) DIM_2 = N3 - IND_2 + 1
!
              IND_1 = 1
              DO 450 J5=1,IP1  ! raws of mato
                 DIM_1 = DB3__MUL_MM_ST_I
                 IF ( IND_1 + DIM_1-1 .GT. M3 ) DIM_1 = M3 - IND_1 + 1
!
!$OMP            PARALLEL DO IF ( NTHR > 1 ), DEFAULT ( NONE), &
!$OMP&           PRIVATE ( J6, IND_THR, IND_M, DIM_M ), &
!$OMP&           SHARED  ( NTHR, M1, M2, M3, N2, N3, IP1, IP2, IND_1, IND_2, DIM_1, DIM_2, &
!$OMP&                     MAT1, MAT2, MAT1C, MAT2C, MAT3C, MATO )
                 DO 460 J6=1,IP1
                    IND_THR = OMP_GET_THREAD_NUM() + 1
                    IND_M = 1 + (J6-1)*DB3__MUL_MM_ST_I
                    IF ( IND_M + DB3__MUL_MM_ST_I-1 .GT. M1 ) THEN
                         DIM_M = M1 - IND_M + 1
                       ELSE 
                         DIM_M = DB3__MUL_MM_ST_I
                    END IF
!
! ----------------- Extract the region of the left operand and put it into
! ----------------- the temporary matrix
!
                    CALL TRG_GATHER ( M1, IND_1, DIM_1, IND_M, DIM_M, &
     &                                MAT1, MAT1C(1,1,IND_THR) )
!
! ----------------- Extract the region of the right operand and put it into
! ----------------- the temporary matrix
!
                    CALL RCT_GATHER ( M2, N2, IND_2, DIM_2, IND_M, DIM_M, &
     &                                MAT2, MAT2C(1,1,IND_THR) )
!
! ----------------- Multiply the regions
!
!!                    CALL DGEMM ( 'N', 'T', DIM_1, DIM_2, DIM_M, &
!!     &                            1.D0, MAT1C, DIM_1, MAT2C, DIM_2, 1.D0, &
!!     &                            MATO(IND_1,IND_2), M3 )
                    CALL NOUT_R8 ( DIM_1*DIM_2, MAT3C(1,1,IND_THR) )
                    CALL DGEMM ( 'N', 'T', DIM_1, DIM_2, DIM_M, 1.D0, &
     &                            MAT1C(1,1,IND_THR), DIM_1, &
     &                            MAT2C(1,1,IND_THR), DIM_2, 1.D0, &
     &                            MAT3C(1,1,IND_THR), DIM_1 )
!$OMP     CRITICAL (MATO_UPDATE)
                    CALL RCT_SCATTER_ADD ( DIM_1, DIM_2, M3, N3, IND_1, IND_2, &
     &                                     MAT3C(1,1,IND_THR), MATO )
!$OMP END CRITICAL (MATO_UPDATE)
 460             CONTINUE 
!$OMP END PARALLEL DO
                 IND_1 = IND_1 + DB3__MUL_MM_ST_I
 450          CONTINUE 
              IND_2 = IND_2 + DB3__MUL_MM_ST_I
 440       CONTINUE 
!
           DEALLOCATE ( MAT1C, MAT2C, MAT3C )
           CALL OMP_SET_NUM_THREADS( %VAL(NTHR_SAVED) ) 
      END IF          
#endif  ! generic
      IER = 0
      RETURN
      END  !#!  MUL_MM_ST_I  #!#
