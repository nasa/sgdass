#ifdef GEN_PREFIX
#define FUNC_MUL_MM_TS_I  GEN_MUL_MM_TS_I
#else
#define FUNC_MUL_MM_TS_I  OPT_MUL_MM_TS_I
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_MUL_MM_TS_I ( M1, N1, MAT1, M2, MAT2, M3, N3, MATO, &
     &                              IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MM_TS_I  multiplies matrixes:                      *
! *   MATO = MAT1 * MAT2                                                 *
! *                                                                      *
! *   MAT1 -- transpose of the rectangular matrix;                       *
! *   MAT2 -- symmetric matrix in the upper triangular representation;   *
! *   MATO -- rectangular matrix.                                        *
! *                                                                      *
! *  ###  11-DEC-96  MUL_MM_TS_I   v5.1 (c)  L. Petrov  29-DEC-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definnition of DB1__MUL_MM_TS_I, DB2__MUL_MM_TS_I
      INTEGER*4  M1, N1, M2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(*), MATO(M3,N3)
      CHARACTER  STR*80
!
      REAL*8,    ALLOCATABLE :: MAT1C(:,:,:), MAT2C(:,:,:), MAT3C(:,:,:), VECO(:)
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
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
!
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
!
      IF ( N1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. N3  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
!CC
#ifdef GEN_PREFIX
      DO 410 J1=1,M3
         DO 420 J2=1,N3
            S=0.D0
            DO 430 J3=1,M2
               LC=LOCS(J3,J2)
               S = S + MAT1(J3,J1)*MAT2(LC)
 430        CONTINUE
            MATO(J1,J2)=S
 420     CONTINUE
 410  CONTINUE
      CALL ERR_LOG ( 0, IUER )
      RETURN
#else
!
! === Actual version
!
      IF ( M2 .EQ. 2 ) THEN
           DO 510 J1=1,M3
              MATO(J1,1) = MAT1(1,J1)*MAT2(1) + MAT1(2,J1)*MAT2(2) 
              MATO(J1,2) = MAT1(1,J1)*MAT2(2) + MAT1(2,J1)*MAT2(3) 
 510       CONTINUE 
        ELSE IF ( M2 .EQ. 3 ) THEN
           DO 520 J1=1,M3
              MATO(J1,1) = MAT1(1,J1)*MAT2(1) + MAT1(2,J1)*MAT2(2) + &
     &                     MAT1(3,J1)*MAT2(4)
              MATO(J1,2) = MAT1(1,J1)*MAT2(2) + MAT1(2,J1)*MAT2(3) + &
     &                     MAT1(3,J1)*MAT2(5)
              MATO(J1,3) = MAT1(1,J1)*MAT2(4) + MAT1(2,J1)*MAT2(5) + &
     &                     MAT1(3,J1)*MAT2(6)
 520       CONTINUE 
        ELSE IF ( M2 .EQ. 4 ) THEN
           DO 530 J1=1,M3
              MATO(J1,1) = MAT1(1,J1)*MAT2(1) + MAT1(2,J1)*MAT2(2) + &
     &                     MAT1(3,J1)*MAT2(4) + MAT1(4,J1)*MAT2(7)
              MATO(J1,2) = MAT1(1,J1)*MAT2(2) + MAT1(2,J1)*MAT2(3) + &
     &                     MAT1(3,J1)*MAT2(5) + MAT1(4,J1)*MAT2(8)
              MATO(J1,3) = MAT1(1,J1)*MAT2(4) + MAT1(2,J1)*MAT2(5) + &
     &                     MAT1(3,J1)*MAT2(6) + MAT1(4,J1)*MAT2(9)
              MATO(J1,4) = MAT1(1,J1)*MAT2(7) + MAT1(2,J1)*MAT2(8) + &
     &                     MAT1(3,J1)*MAT2(9) + MAT1(4,J1)*MAT2(10)
 530       CONTINUE 
        ELSE IF ( M2 .EQ. 5 ) THEN
           DO 540 J1=1,M3
              MATO(J1,1) = MAT1(1,J1)*MAT2(1)  + MAT1(2,J1)*MAT2(2)  + &
     &                     MAT1(3,J1)*MAT2(4)  + MAT1(4,J1)*MAT2(7)  + &
     &                     MAT1(5,J1)*MAT2(11) 
              MATO(J1,2) = MAT1(1,J1)*MAT2(2)  + MAT1(2,J1)*MAT2(3)  + &
     &                     MAT1(3,J1)*MAT2(5)  + MAT1(4,J1)*MAT2(8)  + &
     &                     MAT1(5,J1)*MAT2(12)  
              MATO(J1,3) = MAT1(1,J1)*MAT2(4)  + MAT1(2,J1)*MAT2(5)  + &
     &                     MAT1(3,J1)*MAT2(6)  + MAT1(4,J1)*MAT2(9)  + &
     &                     MAT1(5,J1)*MAT2(13) 
              MATO(J1,4) = MAT1(1,J1)*MAT2(7)  + MAT1(2,J1)*MAT2(8)  + &
     &                     MAT1(3,J1)*MAT2(9)  + MAT1(4,J1)*MAT2(10) + &
     &                     MAT1(5,J1)*MAT2(14) 
              MATO(J1,5) = MAT1(1,J1)*MAT2(11) + MAT1(2,J1)*MAT2(12) + &
     &                     MAT1(3,J1)*MAT2(13) + MAT1(4,J1)*MAT2(14) + &
     &                     MAT1(5,J1)*MAT2(15) 
 540       CONTINUE 
        ELSE IF ( M2 .EQ. 6 ) THEN
           DO 550 J1=1,M3
              MATO(J1,1) = MAT1(1,J1)*MAT2(1)  + MAT1(2,J1)*MAT2(2)  + &
     &                     MAT1(3,J1)*MAT2(4)  + MAT1(4,J1)*MAT2(7)  + &
     &                     MAT1(5,J1)*MAT2(11) + MAT1(6,J1)*MAT2(16) 
              MATO(J1,2) = MAT1(1,J1)*MAT2(2)  + MAT1(2,J1)*MAT2(3)  + &
     &                     MAT1(3,J1)*MAT2(5)  + MAT1(4,J1)*MAT2(8)  + &
     &                     MAT1(5,J1)*MAT2(12) + MAT1(6,J1)*MAT2(17) 
              MATO(J1,3) = MAT1(1,J1)*MAT2(4)  + MAT1(2,J1)*MAT2(5)  + &
     &                     MAT1(3,J1)*MAT2(6)  + MAT1(4,J1)*MAT2(9)  + &
     &                     MAT1(5,J1)*MAT2(13) + MAT1(6,J1)*MAT2(18) 
              MATO(J1,4) = MAT1(1,J1)*MAT2(7)  + MAT1(2,J1)*MAT2(8)  + &
     &                     MAT1(3,J1)*MAT2(9)  + MAT1(4,J1)*MAT2(10) + &
     &                     MAT1(5,J1)*MAT2(14) + MAT1(6,J1)*MAT2(19) 
              MATO(J1,5) = MAT1(1,J1)*MAT2(11) + MAT1(2,J1)*MAT2(12) + &
     &                     MAT1(3,J1)*MAT2(13) + MAT1(4,J1)*MAT2(14) + &
     &                     MAT1(5,J1)*MAT2(15) + MAT1(6,J1)*MAT2(20) 
              MATO(J1,6) = MAT1(1,J1)*MAT2(16) + MAT1(2,J1)*MAT2(17) + &
     &                     MAT1(3,J1)*MAT2(18) + MAT1(4,J1)*MAT2(19) + &
     &                     MAT1(5,J1)*MAT2(20) + MAT1(6,J1)*MAT2(21) 
 550       CONTINUE 
        ELSE IF ( M2 .EQ. 7 ) THEN
           DO 560 J1=1,M3
              MATO(J1,1) = MAT1(1,J1)*MAT2(1)  + MAT1(2,J1)*MAT2(2)  + &
     &                     MAT1(3,J1)*MAT2(4)  + MAT1(4,J1)*MAT2(7)  + &
     &                     MAT1(5,J1)*MAT2(11) + MAT1(6,J1)*MAT2(16) + &
     &                     MAT1(7,J1)*MAT2(22)
              MATO(J1,2) = MAT1(1,J1)*MAT2(2)  + MAT1(2,J1)*MAT2(3)  + &
     &                     MAT1(3,J1)*MAT2(5)  + MAT1(4,J1)*MAT2(8)  + &
     &                     MAT1(5,J1)*MAT2(12) + MAT1(6,J1)*MAT2(17) + &
     &                     MAT1(7,J1)*MAT2(23)
              MATO(J1,3) = MAT1(1,J1)*MAT2(4)  + MAT1(2,J1)*MAT2(5)  + &
     &                     MAT1(3,J1)*MAT2(6)  + MAT1(4,J1)*MAT2(9)  + &
     &                     MAT1(5,J1)*MAT2(13) + MAT1(6,J1)*MAT2(18) + &
     &                     MAT1(7,J1)*MAT2(24)
              MATO(J1,4) = MAT1(1,J1)*MAT2(7)  + MAT1(2,J1)*MAT2(8)  + &
     &                     MAT1(3,J1)*MAT2(9)  + MAT1(4,J1)*MAT2(10) + &
     &                     MAT1(5,J1)*MAT2(14) + MAT1(6,J1)*MAT2(19) + &
     &                     MAT1(7,J1)*MAT2(25)
              MATO(J1,5) = MAT1(1,J1)*MAT2(11) + MAT1(2,J1)*MAT2(12) + &
     &                     MAT1(3,J1)*MAT2(13) + MAT1(4,J1)*MAT2(14) + &
     &                     MAT1(5,J1)*MAT2(15) + MAT1(6,J1)*MAT2(20) + &
     &                     MAT1(7,J1)*MAT2(26)
              MATO(J1,6) = MAT1(1,J1)*MAT2(16) + MAT1(2,J1)*MAT2(17) + &
     &                     MAT1(3,J1)*MAT2(18) + MAT1(4,J1)*MAT2(19) + &
     &                     MAT1(5,J1)*MAT2(20) + MAT1(6,J1)*MAT2(21) + &
     &                     MAT1(7,J1)*MAT2(27)
              MATO(J1,7) = MAT1(1,J1)*MAT2(22) + MAT1(2,J1)*MAT2(23) + &
     &                     MAT1(3,J1)*MAT2(24) + MAT1(4,J1)*MAT2(25) + &
     &                     MAT1(5,J1)*MAT2(26) + MAT1(6,J1)*MAT2(27) + &
     &                     MAT1(7,J1)*MAT2(28)
 560       CONTINUE 
        ELSE IF ( M2 .LT. DB1__MUL_MM_TS_I ) THEN
!
! -------- Small dimensions
!
           ALLOCATE ( VECO(N3) )
           DO 410 J1=1,M3 
              VECO(1) = MAT1(1,J1)*MAT2(1)
              K = 2
              DO 420 J2=2,M2
                 S = 0.0D0
                 DO 430 J3=1,J2-1
                    VECO(J3) = VECO(J3) + MAT1(J2,J1)*MAT2(K)
                    S = S + MAT1(J3,J1)*MAT2(K)
                    K = K+1
 430             CONTINUE
                 VECO(J2) = S + MAT1(J2,J1)*MAT2(K)
                 K = K+1
 420          CONTINUE
              CALL DCOPY ( N3, VECO, 1, MATO(J1,1), M3 )
 410       CONTINUE
           DEALLOCATE ( VECO )
         ELSE IF ( M2 .LT. DB2__MUL_MM_TS_I ) THEN
!
! -------- Medium dimensions
!
! -------- Allocate memory for a temporary copy of the left operand
!
           ALLOCATE ( MAT2C(M2,M2,1), STAT=IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*M2*M2, STR )
                CALL ERR_LOG ( 18, IUER, 'MUL_MM_TS_I', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                RETURN 
           END IF
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT2C, 0, %VAL(8*M2*M2) )
#endif
!
! -------- Re-arrange matrix from upper triangular rerpresntation to
! -------- rectangular representation
!
           CALL TRG_GATHER ( M2, 1, M2, 1, M2, MAT2, MAT2C )
!
           CALL DGEMM ( 'T', 'N', N1, M2, M2, 1.D0, MAT1, &
     &                   M1, MAT2C, M2, 0.D0, MATO, M3 )
!
           DEALLOCATE ( MAT2C )
         ELSE
!
! -------- Large dimensions
!
           CALL NOUT8_R8 ( INT8(M3)*INT8(N3), MATO )  ! Initialization
!
           IP1 = M3/DB3__MUL_MM_TS_I 
           IF ( IP1*DB3__MUL_MM_TS_I .LT. M3 ) IP1 = IP1 + 1
           IP2 = N3/DB3__MUL_MM_TS_I 
           IF ( IP2*DB3__MUL_MM_TS_I .LT. N3 ) IP2 = IP2 + 1
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
                CALL IINCH ( 8*3*DB3__MUL_MM_ST_I*DB3__MUL_MM_ST_I*NTHR, STR )
                CALL ERR_LOG ( 20, IUER, 'MUL_MM_TS_I', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                RETURN 
           END IF
           CALL OMP_SET_NUM_THREADS( %VAL(NTHR) ) 
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT1C, 0, %VAL(8*DB3__MUL_MM_TS_I*DB3__MUL_MM_TS_I,NTHR) )
           CALL MEMSET ( MAT2C, 0, %VAL(8*DB3__MUL_MM_TS_I*DB3__MUL_MM_TS_I,NTHR) )
           CALL MEMSET ( MAT2C, 0, %VAL(8*DB3__MUL_MM_TS_I*DB3__MUL_MM_TS_I,NHTR) )
#endif
!
           IND_2 = 1
           DO 440 J4=1,IP2 ! columns of mato
              DIM_2 = DB3__MUL_MM_TS_I
              IF ( IND_2 + DIM_2-1 .GT. N3 ) DIM_2 = N3 - IND_2 + 1
!
              IND_1 = 1
              DO 450 J5=1,IP1  ! raws of mato
                 DIM_1 = DB3__MUL_MM_TS_I
                 IF ( IND_1 + DIM_1-1 .GT. M3 ) DIM_1 = M3 - IND_1 + 1
!
!$OMP            PARALLEL DO IF ( NTHR > 1 ), DEFAULT ( NONE), &
!$OMP&           PRIVATE ( J6, IND_THR, IND_M, DIM_M ), &
!$OMP&           SHARED  ( NTHR, M1, M2, M3, N1, N3, IP2, IND_1, IND_2, DIM_1, DIM_2, &
!$OMP&                     MAT1, MAT2, MAT1C, MAT2C, MAT3C, MATO )
                 DO 460 J6=1,IP2
                    IND_THR = OMP_GET_THREAD_NUM() + 1
                    IND_M = 1 + (J6-1)*DB3__MUL_MM_TS_I
                    IF ( IND_M + DB3__MUL_MM_TS_I-1 .GT. M2 ) THEN
                         DIM_M = M2 - IND_M + 1
                       ELSE 
                         DIM_M = DB3__MUL_MM_TS_I
                    END IF
!
! ----------------- Extract the region of the right operand and put it into
! ----------------- the temporary matrix
!
                    CALL RCT_GATHER ( M1, N1, IND_M, DIM_M, IND_1, DIM_1, &
     &                                MAT1, MAT1C )
!
! ----------------- Extract the region of the left operand and put it into
! ----------------- the temporary matrix
!
                    CALL TRG_GATHER ( M2, IND_M, DIM_M, IND_2, DIM_2, &
     &                                MAT2, MAT2C )
!
! ----------------- Multiply the regions
!
!!                    CALL DGEMM ( 'T', 'N', DIM_1, DIM_2, DIM_M, &
!!     &                            1.D0, MAT1C, DIM_M, MAT2C, DIM_M, 1.D0, &
!!     &                            MATO(IND_1,IND_2), M3 )
                    CALL NOUT_R8 ( DIM_1*DIM_2, MAT3C(1,1,IND_THR) )
                    CALL DGEMM ( 'T', 'N', DIM_1, DIM_2, DIM_M, 1.D0, &
     &                            MAT1C(1,1,IND_THR), DIM_M, &
     &                            MAT2C(1,1,IND_THR), DIM_M, 1.D0, &
     &                            MAT3C(1,1,IND_THR), DIM_1 )
!$OMP     CRITICAL (MATO_UPDATE)
                    CALL RCT_SCATTER_ADD ( DIM_1, DIM_2, M3, N3, IND_1, IND_2, &
     &                                     MAT3C(1,1,IND_THR), MATO )
!$OMP END CRITICAL (MATO_UPDATE)
 460             CONTINUE 
!$OMP END PARALLEL DO
                 IND_1 = IND_1 + DB3__MUL_MM_TS_I
 450          CONTINUE 
              IND_2 = IND_2 + DB3__MUL_MM_TS_I
 440       CONTINUE 
!
           DEALLOCATE ( MAT1C, MAT2C, MAT3C )
           CALL OMP_SET_NUM_THREADS( %VAL(NTHR_SAVED) ) 
      END IF          
#endif  ! generic
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_TS_I #!#
