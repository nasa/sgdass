#ifdef GEN_PREFIX
#define FUNC_MUL_MM_SS_I  GEN_MUL_MM_SS_I
#else
#define FUNC_MUL_MM_SS_I  OPT_MUL_MM_SS_I
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_MUL_MM_SS_I ( M1, MAT1, M2, MAT2, M3, N3, MATO, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MM_SS_I  multiplies matrixes:                      *
! *   MATO = MAT1 * MAT2                                                 *
! *                                                                      *
! *  ###  11-DEC-1996  MUL_MM_SS_I   v5.1 (c)  L. Petrov 29-DEC-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definition of DB1__MUL_MM_SS_I, DB2__MUL_MM_SS_I
      INTEGER*4  M1, M2, M3, N3, NN, IUER
      REAL*8     MAT1(*), MAT2(*), MATO(M3,N3)
!
      CHARACTER  STR*64
      REAL*8,    ALLOCATABLE :: MAT1C(:,:,:), MAT2C(:,:,:), MAT3C(:,:,:)
      INTEGER*4  LC, KL, KR, KR1, KR2, IP1, IP2, L1, L2, &
     &           J1, J2, J3, J4, J5, J6, IND_THR, NTHR, NTHR_SAVED, &
     &           IND_1, IND_2, IND_M, DIM_1, DIM_2, DIM_M, IERR
      ADDRESS__TYPE  :: SSI_FUNC(DB1__MUL_MM_SS_I), ARG_LIST(4)
      ADDRESS__TYPE, EXTERNAL :: &
     &           MUL_MM_SS_I_5,  MUL_MM_SS_I_6,  MUL_MM_SS_I_7,  &
     &           MUL_MM_SS_I_8,  MUL_MM_SS_I_9,  MUL_MM_SS_I_10, &
     &           MUL_MM_SS_I_11, MUL_MM_SS_I_12, MUL_MM_SS_I_13, &
     &           MUL_MM_SS_I_14, MUL_MM_SS_I_15, MUL_MM_SS_I_16
      ADDRESS__TYPE, EXTERNAL :: FUNC_ADDRESS
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
      REAL*8     S
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_SS_I', STR )
           RETURN
      END IF
!
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. M3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SS_I', STR )
           RETURN
      END IF
!
      IF ( M3 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N3  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SS_I', STR )
           RETURN
      END IF
!
#ifdef GEN_PREFIX
      DO 410 J1=1,M1
         DO 420 J2=1,M2
            S=0.D0
            DO 430 J3=1,M3
               L1=LOCS(J1,J3)
               L2=LOCS(J2,J3)
               S = S + MAT1(L1)*MAT2(L2)
 430        CONTINUE
            MATO(J1,J2)=S
 420     CONTINUE
 410  CONTINUE
#else
       IF ( M1 .EQ. 2 ) THEN
            MATO(1,1) = MAT1(1)*MAT2(1) + MAT1(2)*MAT2(2)
            MATO(2,1) = MAT1(2)*MAT2(1) + MAT1(3)*MAT2(2)
!
            MATO(1,2) = MAT1(1)*MAT2(2) + MAT1(2)*MAT2(3)
            MATO(2,2) = MAT1(2)*MAT2(2) + MAT1(3)*MAT2(3)
          ELSE IF ( M1 .EQ. 3 ) THEN
            MATO(1,1) = MAT1(1)*MAT2(1) + MAT1(2)*MAT2(2) + MAT1(4)*MAT2(4) 
            MATO(2,1) = MAT1(2)*MAT2(1) + MAT1(3)*MAT2(2) + MAT1(5)*MAT2(4) 
            MATO(3,1) = MAT1(4)*MAT2(1) + MAT1(5)*MAT2(2) + MAT1(6)*MAT2(4) 
!
            MATO(1,2) = MAT1(1)*MAT2(2) + MAT1(2)*MAT2(3) + MAT1(4)*MAT2(5) 
            MATO(2,2) = MAT1(2)*MAT2(2) + MAT1(3)*MAT2(3) + MAT1(5)*MAT2(5) 
            MATO(3,2) = MAT1(4)*MAT2(2) + MAT1(5)*MAT2(3) + MAT1(6)*MAT2(5) 
!
            MATO(1,3) = MAT1(1)*MAT2(4) + MAT1(2)*MAT2(5) + MAT1(4)*MAT2(6) 
            MATO(2,3) = MAT1(2)*MAT2(4) + MAT1(3)*MAT2(5) + MAT1(5)*MAT2(6) 
            MATO(3,3) = MAT1(4)*MAT2(4) + MAT1(5)*MAT2(5) + MAT1(6)*MAT2(6) 
          ELSE IF ( M1 .EQ. 4 ) THEN
            MATO(1,1) = MAT1(1)*MAT2(1) + MAT1(2)*MAT2(2) + &
     &                  MAT1(4)*MAT2(4) + MAT1(7)*MAT2(7)
            MATO(2,1) = MAT1(2)*MAT2(1) + MAT1(3)*MAT2(2) + &
     &                  MAT1(5)*MAT2(4) + MAT1(8)*MAT2(7)
            MATO(3,1) = MAT1(4)*MAT2(1) + MAT1(5)*MAT2(2) + &
     &                  MAT1(6)*MAT2(4) + MAT1(9)*MAT2(7) 
            MATO(4,1) = MAT1(7)*MAT2(1) + MAT1(8)*MAT2(2) + &
     &                  MAT1(9)*MAT2(4) + MAT1(10)*MAT2(7) 
!
            MATO(1,2) = MAT1(1)*MAT2(2) + MAT1(2)*MAT2(3) + &
     &                  MAT1(4)*MAT2(5) + MAT1(7)*MAT2(8)
            MATO(2,2) = MAT1(2)*MAT2(2) + MAT1(3)*MAT2(3) + &
     &                  MAT1(5)*MAT2(5) + MAT1(8)*MAT2(8)
            MATO(3,2) = MAT1(4)*MAT2(2) + MAT1(5)*MAT2(3) + &
     &                  MAT1(6)*MAT2(5) + MAT1(9)*MAT2(8) 
            MATO(4,2) = MAT1(7)*MAT2(2) + MAT1(8)*MAT2(3) + &
     &                  MAT1(9)*MAT2(5) + MAT1(10)*MAT2(8) 
!
            MATO(1,3) = MAT1(1)*MAT2(4) + MAT1(2)*MAT2(5) + &
     &                  MAT1(4)*MAT2(6) + MAT1(7)*MAT2(9)
            MATO(2,3) = MAT1(2)*MAT2(4) + MAT1(3)*MAT2(5) + &
     &                  MAT1(5)*MAT2(6) + MAT1(8)*MAT2(9)
            MATO(3,3) = MAT1(4)*MAT2(4) + MAT1(5)*MAT2(5) + &
     &                  MAT1(6)*MAT2(6) + MAT1(9)*MAT2(9) 
            MATO(4,3) = MAT1(7)*MAT2(4) + MAT1(8)*MAT2(5) + &
     &                  MAT1(9)*MAT2(6) + MAT1(10)*MAT2(9) 
!
            MATO(1,4) = MAT1(1)*MAT2(7) + MAT1(2)*MAT2(8) + &
     &                  MAT1(4)*MAT2(9) + MAT1(7)*MAT2(10)
            MATO(2,4) = MAT1(2)*MAT2(7) + MAT1(3)*MAT2(8) + &
     &                  MAT1(5)*MAT2(9) + MAT1(8)*MAT2(10)
            MATO(3,4) = MAT1(4)*MAT2(7) + MAT1(5)*MAT2(8) + &
     &                  MAT1(6)*MAT2(9) + MAT1(9)*MAT2(10) 
            MATO(4,4) = MAT1(7)*MAT2(7) + MAT1(8)*MAT2(8) + &
     &                  MAT1(9)*MAT2(9) + MAT1(10)*MAT2(10) 
          ELSE IF ( M1 .GE. 5  .AND.  M1 .LE. DB1__MUL_MM_SS_I ) THEN
            ARG_LIST(1) = 3
            ARG_LIST(2) = LOC(MAT1)
            ARG_LIST(3) = LOC(MAT2)
            ARG_LIST(4) = LOC(MATO)
!
            IF ( M1 .EQ. 5 ) THEN
                 SSI_FUNC(5)  = FUNC_ADDRESS ( MUL_MM_SS_I_5  )
               ELSE IF ( M1 .EQ. 6 ) THEN
                 SSI_FUNC(6)  = FUNC_ADDRESS ( MUL_MM_SS_I_6  )
               ELSE IF ( M1 .EQ. 7 ) THEN
                 SSI_FUNC(7)  = FUNC_ADDRESS ( MUL_MM_SS_I_7  )
               ELSE IF ( M1 .EQ. 8 ) THEN
                 SSI_FUNC(8)  = FUNC_ADDRESS ( MUL_MM_SS_I_8  )
               ELSE
                 SSI_FUNC(9)  = FUNC_ADDRESS ( MUL_MM_SS_I_9  )
                 SSI_FUNC(10) = FUNC_ADDRESS ( MUL_MM_SS_I_10 )
                 SSI_FUNC(11) = FUNC_ADDRESS ( MUL_MM_SS_I_11 )
                 SSI_FUNC(12) = FUNC_ADDRESS ( MUL_MM_SS_I_12 )
                 SSI_FUNC(13) = FUNC_ADDRESS ( MUL_MM_SS_I_13 )
                 SSI_FUNC(14) = FUNC_ADDRESS ( MUL_MM_SS_I_14 )
                 SSI_FUNC(15) = FUNC_ADDRESS ( MUL_MM_SS_I_15 )
                 SSI_FUNC(16) = FUNC_ADDRESS ( MUL_MM_SS_I_16 )
            END IF
!
            CALL LIB$CALLG ( ARG_LIST, %VAL(SSI_FUNC(M1)) )
        ELSE IF ( M1 .LE. DB1__MUL_MM_SS_I ) THEN
!
! -------- Small dimensions
!
           KR = 0
           DO 410 J1=1,N3 
              MATO(1,J1) = MAT1(1)*MAT2(KR+1)
              KL = 2
              KR1 = KR+1
              DO 420 J2=2,M1
                 IF ( J2 .LE. J1 ) THEN
                      KR1 = KR1 + 1
                    ELSE
                      KR1 = KR1 + J2-1
                 END IF
                 S = 0.0D0
                 KR2 = KR
                 DO 430 J3=1,J2-1
                    IF ( J3 .LE. J1 ) THEN
                         KR2 = KR2 + 1
                       ELSE
                         KR2 = KR2 + J3-1
                    END IF
                    MATO(J3,J1) = MATO(J3,J1) + MAT1(KL)*MAT2(KR1)
                    S = S + MAT1(KL)*MAT2(KR2)
                    KL = KL+1
 430             CONTINUE
                 MATO(J2,J1) = S + MAT1(KL)*MAT2(KR1)
                 KL = KL+1
 420          CONTINUE
              KR = KR + J1
 410       CONTINUE
        ELSE IF ( M1 .LE. DB2__MUL_MM_SS_I ) THEN
!
! -------- Medium dimensions
!
! -------- Allocate memory for a temporary copy of the left operand
!
           ALLOCATE ( MAT1C(M1,M1,1), STAT=IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*M1*M1, STR )
                CALL ERR_LOG ( 18, IUER, 'MUL_MM_SS_I', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                RETURN 
           END IF
!
           ALLOCATE ( MAT2C(M2,M2,1), STAT=IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*M2*M2, STR )
                CALL ERR_LOG ( 19, IUER, 'MUL_MM_SS_I', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                DEALLOCATE ( MAT1C )
                RETURN 
           END IF
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT1C, 0, %VAL(8*M1*M1) )
           CALL MEMSET ( MAT2C, 0, %VAL(8*M2*M2) )
#endif
!
! -------- Re-arrange matrix from upper triangular rerpresntation to
! -------- rectangular representation
!
           CALL TRG_GATHER ( M1, 1, M1, 1, M1, MAT1, MAT1C )
           CALL TRG_GATHER ( M2, 1, M2, 1, M2, MAT2, MAT2C )
!
           CALL DGEMM ( 'N', 'N', M1, M2, M2, 1.D0, MAT1C, &
     &                   M1, MAT2C, M2, 0.D0, MATO, M3 )
!
           DEALLOCATE ( MAT1C )
           DEALLOCATE ( MAT2C )
        ELSE
!
! -------- Large dimensions
!
           CALL NOUT8_R8 ( INT8(M3)*INT8(N3), MATO )  ! Initialization
!
           IP1 = M3/DB3__MUL_MM_SS_I 
           IF ( IP1*DB3__MUL_MM_SS_I .LT. M3 ) IP1 = IP1 + 1
           IP2 = N3/DB3__MUL_MM_SS_I 
           IF ( IP2*DB3__MUL_MM_SS_I .LT. N3 ) IP2 = IP2 + 1
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
           ALLOCATE ( MAT1C(DB3__MUL_MM_SS_I,DB3__MUL_MM_SS_I,NTHR), &
     &                MAT2C(DB3__MUL_MM_SS_I,DB3__MUL_MM_SS_I,NTHR), &
     &                MAT3C(DB3__MUL_MM_SS_I,DB3__MUL_MM_SS_I,NTHR), &
     &                STAT=IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*3*DB3__MUL_MM_SS_I*DB3__MUL_MM_SS_I*NTHR, STR )
                CALL ERR_LOG ( 20, IUER, 'MUL_MM_SS_I', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                RETURN 
           END IF
           CALL OMP_SET_NUM_THREADS( %VAL(NTHR) ) 
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT1C, 0, %VAL(8*DB3__MUL_MM_SS_I*DB3__MUL_MM_SS_I*NTHR) )
           CALL MEMSET ( MAT2C, 0, %VAL(8*DB3__MUL_MM_SS_I*DB3__MUL_MM_SS_I*NTHR) )
           CALL MEMSET ( MAT2C, 0, %VAL(8*DB3__MUL_MM_SS_I*DB3__MUL_MM_SS_I*NTHR) )
#endif
!
           IND_2 = 1
           DO 440 J4=1,IP2 ! columns of mato
              DIM_2 = DB3__MUL_MM_SS_I
              IF ( IND_2 + DIM_2-1 .GT. N3 ) DIM_2 = N3 - IND_2 + 1
!
              IND_1 = 1
              DO 450 J5=1,IP1  ! raws of mato
                 DIM_1 = DB3__MUL_MM_SS_I
                 IF ( IND_1 + DIM_1-1 .GT. M3 ) DIM_1 = M3 - IND_1 + 1
!
!$OMP            PARALLEL DO IF ( NTHR > 1 ), DEFAULT ( NONE), &
!$OMP&           PRIVATE ( J6, IND_THR, IND_M, DIM_M ), &
!$OMP&           SHARED  ( NTHR, M1, M2, M3, N3, IP1, IP2, IND_1, IND_2, DIM_1, DIM_2, &
!$OMP&                     MAT1, MAT2, MAT1C, MAT2C, MAT3C, MATO )
                 DO 460 J6=1,IP1
                    IND_THR = OMP_GET_THREAD_NUM() + 1
                    IND_M = 1 + (J6-1)*DB3__MUL_MM_SS_I
                    IF ( IND_M + DB3__MUL_MM_SS_I-1 .GT. M1 ) THEN
                         DIM_M = M1 - IND_M + 1
                       ELSE 
                         DIM_M = DB3__MUL_MM_SS_I
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
                    CALL TRG_GATHER ( M2, IND_M, DIM_M, IND_2, DIM_2, &
     &                                MAT2, MAT2C(1,1,IND_THR) )
!
! ----------------- Multiply the regions
!
                    CALL NOUT_R8 ( DIM_1*DIM_2, MAT3C(1,1,IND_THR) )
                    CALL DGEMM ( 'N', 'N', DIM_1, DIM_2, DIM_M, 1.D0, &
     &                            MAT1C(1,1,IND_THR), DIM_1, &
     &                            MAT2C(1,1,IND_THR), DIM_M, 1.D0, &
     &                            MAT3C(1,1,IND_THR), DIM_1 )
!
! ---------------- Scatter/update the regions
!
!$OMP CRITICAL     (MATO_UPDATE)
                    CALL RCT_SCATTER_ADD ( DIM_1, DIM_2, M3, N3, IND_1, IND_2, &
     &                                     MAT3C(1,1,IND_THR), MATO )
!$OMP END CRITICAL (MATO_UPDATE)
 460             CONTINUE 
!$OMP END PARALLEL DO
                 IND_1 = IND_1 + DB3__MUL_MM_SS_I
 450          CONTINUE 
              IND_2 = IND_2 + DB3__MUL_MM_SS_I
 440       CONTINUE 
!
           DEALLOCATE ( MAT1C, MAT2C, MAT3C )
           CALL OMP_SET_NUM_THREADS( %VAL(NTHR_SAVED) ) 
      END IF          
#endif  ! generic
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_SS_I  #!#
