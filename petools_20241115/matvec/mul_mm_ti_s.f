#ifdef GEN_PREFIX
#define FUNC_MUL_MM_TI_S  GEN_MUL_MM_TI_S
#else
#define FUNC_MUL_MM_TI_S  OPT_MUL_MM_TI_S
#endif
#include <mk5_preprocessor_directives.inc>
      SUBROUTINE FUNC_MUL_MM_TI_S ( M1, N1, MAT1, M2, N2, MAT2, M3, MATO, &
     &                         IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  MUL_MM_TI_S  multiplies matrices:                      *
! *   MATO = MAT1 * MAT2                                                 *
! *                                                                      *
! *  ###  09-DEC-1996   MUL_MM_TI_S  v4.1 (c) L. Petrov  29-DEC-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i' ! Definition of DB1__MUL_MM_TI_S, DB2__MUL_MM_TI_S
      INTEGER*4  M1, N1, M2, N2, M3
      INTEGER*4, OPTIONAL :: IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(*)
      CHARACTER  STR*80
!
      REAL*8,    ALLOCATABLE :: MAT1C(:,:,:), MAT2C(:,:,:), MAT3C(:,:,:)
      INTEGER*4  LC, LC1, IP1, IP2, J1, J2, J3, J4, J5, J6, &
     &           IND_1, IND_2, IND_M, DIM_1, DIM_2, DIM_M, &
     &           IND_THR, NTHR, NTHR_SAVED, IERR
      LOGICAL*1  FL_UPDATE
      LOGICAL*4, EXTERNAL :: PROBE_READ_ADDRESS
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      REAL*8,    EXTERNAL :: DP_VV_V, DDOT
      INTEGER*4  I, J, LOCS
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
      LOGICAL*4, EXTERNAL :: OMP_IN_PARALLEL
      INTEGER*4, EXTERNAL :: OMP_GET_MAX_THREADS, OMP_GET_NUM_THREADS, OMP_GET_THREAD_NUM
!!
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
!
      IF ( N1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. N2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
!
      IF ( M3 .NE. N1 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N1  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
      FL_UPDATE = .FALSE.
      IF ( PRESENT(IUER) ) THEN
           IF ( PROBE_READ_ADDRESS(IUER) ) THEN
                 IF ( IUER == -8 ) FL_UPDATE = .TRUE.
           END IF
      END IF
#ifdef GEN_PREFIX
!
! --- Generic version
!
      DO 410 J1=1,M3
         DO 420 J2=1,J1
            LC=LOCS(J1,J2)
            MATO(LC) = 0.0D0
            DO 430 J3=1,M1
               MATO(LC) = MATO(LC) + MAT1(J3,J2)*MAT2(J3,J1)
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
#else
      IF ( M3 .EQ. 2 .AND.  .NOT. FL_UPDATE ) THEN
           MATO(1) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
           MATO(2) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
         ELSE IF ( M3 .EQ. 3 .AND.  .NOT. FL_UPDATE ) THEN
           MATO(1) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
           MATO(2) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
           MATO(4) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
         ELSE IF ( M3 .EQ. 4 .AND.  .NOT. FL_UPDATE ) THEN
           MATO(1) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
           MATO(2) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
           MATO(4) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
           MATO(7) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,4) )
           MATO(8) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,4) )
           MATO(9) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,4) )
           MATO(10)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,4) )
         ELSE IF ( M3 .EQ. 5 .AND.  .NOT. FL_UPDATE ) THEN
           MATO(1) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
!
           MATO(2) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
!
           MATO(4) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
!
           MATO(7) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,4) )
           MATO(8) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,4) )
           MATO(9) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,4) )
           MATO(10)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,4) )
!
           MATO(11)= DP_VV_V ( M1, MAT1(1,1), MAT2(1,5) )
           MATO(12)= DP_VV_V ( M1, MAT1(1,2), MAT2(1,5) )
           MATO(13)= DP_VV_V ( M1, MAT1(1,3), MAT2(1,5) )
           MATO(14)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,5) )
           MATO(15)= DP_VV_V ( M1, MAT1(1,5), MAT2(1,5) )
         ELSE IF ( M3 .EQ. 6 .AND.  .NOT. FL_UPDATE ) THEN
           MATO(1) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
!
           MATO(2) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
!
           MATO(4) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
!
           MATO(7) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,4) )
           MATO(8) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,4) )
           MATO(9) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,4) )
           MATO(10)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,4) )
!
           MATO(11)= DP_VV_V ( M1, MAT1(1,1), MAT2(1,5) )
           MATO(12)= DP_VV_V ( M1, MAT1(1,2), MAT2(1,5) )
           MATO(13)= DP_VV_V ( M1, MAT1(1,3), MAT2(1,5) )
           MATO(14)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,5) )
           MATO(15)= DP_VV_V ( M1, MAT1(1,5), MAT2(1,5) )
!
           MATO(16)= DP_VV_V ( M1, MAT1(1,1), MAT2(1,6) )
           MATO(17)= DP_VV_V ( M1, MAT1(1,2), MAT2(1,6) )
           MATO(18)= DP_VV_V ( M1, MAT1(1,3), MAT2(1,6) )
           MATO(19)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,6) )
           MATO(20)= DP_VV_V ( M1, MAT1(1,5), MAT2(1,6) )
           MATO(21)= DP_VV_V ( M1, MAT1(1,6), MAT2(1,6) )
         ELSE IF ( M3 .EQ. 7 .AND.  .NOT. FL_UPDATE ) THEN
           MATO(1) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
!
           MATO(2) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
!
           MATO(4) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
!
           MATO(7) = DP_VV_V ( M1, MAT1(1,1), MAT2(1,4) )
           MATO(8) = DP_VV_V ( M1, MAT1(1,2), MAT2(1,4) )
           MATO(9) = DP_VV_V ( M1, MAT1(1,3), MAT2(1,4) )
           MATO(10)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,4) )
!
           MATO(11)= DP_VV_V ( M1, MAT1(1,1), MAT2(1,5) )
           MATO(12)= DP_VV_V ( M1, MAT1(1,2), MAT2(1,5) )
           MATO(13)= DP_VV_V ( M1, MAT1(1,3), MAT2(1,5) )
           MATO(14)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,5) )
           MATO(15)= DP_VV_V ( M1, MAT1(1,5), MAT2(1,5) )
!
           MATO(16)= DP_VV_V ( M1, MAT1(1,1), MAT2(1,6) )
           MATO(17)= DP_VV_V ( M1, MAT1(1,2), MAT2(1,6) )
           MATO(18)= DP_VV_V ( M1, MAT1(1,3), MAT2(1,6) )
           MATO(19)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,6) )
           MATO(20)= DP_VV_V ( M1, MAT1(1,5), MAT2(1,6) )
           MATO(21)= DP_VV_V ( M1, MAT1(1,6), MAT2(1,6) )
!
           MATO(22)= DP_VV_V ( M1, MAT1(1,1), MAT2(1,7) )
           MATO(23)= DP_VV_V ( M1, MAT1(1,2), MAT2(1,7) )
           MATO(24)= DP_VV_V ( M1, MAT1(1,3), MAT2(1,7) )
           MATO(25)= DP_VV_V ( M1, MAT1(1,4), MAT2(1,7) )
           MATO(26)= DP_VV_V ( M1, MAT1(1,5), MAT2(1,7) )
           MATO(27)= DP_VV_V ( M1, MAT1(1,6), MAT2(1,7) )
           MATO(28)= DP_VV_V ( M1, MAT1(1,7), MAT2(1,7) )
         ELSE IF ( M3 .EQ. 2 .AND.  FL_UPDATE ) THEN
           MATO(1) = MATO(1) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
           MATO(2) = MATO(2) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = MATO(3) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
         ELSE IF ( M3 .EQ. 3 .AND.  FL_UPDATE ) THEN
           MATO(1) = MATO(1) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
           MATO(2) = MATO(2) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = MATO(3) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
           MATO(4) = MATO(4) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = MATO(5) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = MATO(6) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
         ELSE IF ( M3 .EQ. 4 .AND.  FL_UPDATE ) THEN
           MATO(1) = MATO(1) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
           MATO(2) = MATO(2) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = MATO(3) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
           MATO(4) = MATO(4) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = MATO(5) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = MATO(6) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
           MATO(7) = MATO(7) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,4) )
           MATO(8) = MATO(8) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,4) )
           MATO(9) = MATO(9) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,4) )
           MATO(10)= MATO(10) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,4) )
         ELSE IF ( M3 .EQ. 5 .AND.  FL_UPDATE ) THEN
           MATO(1) = MATO(1) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
!
           MATO(2) = MATO(2) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = MATO(3) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
!
           MATO(4) = MATO(4) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = MATO(5) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = MATO(6) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
!
           MATO(7) = MATO(7) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,4) )
           MATO(8) = MATO(8) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,4) )
           MATO(9) = MATO(9) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,4) )
           MATO(10)= MATO(10) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,4) )
!
           MATO(11)= MATO(11) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,5) )
           MATO(12)= MATO(12) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,5) )
           MATO(13)= MATO(13) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,5) )
           MATO(14)= MATO(14) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,5) )
           MATO(15)= MATO(15) + DP_VV_V ( M1, MAT1(1,5), MAT2(1,5) )
         ELSE IF ( M3 .EQ. 6 .AND.  FL_UPDATE ) THEN
           MATO(1) = MATO(1) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
!
           MATO(2) = MATO(2) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = MATO(3) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
!
           MATO(4) = MATO(4) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = MATO(5) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = MATO(6) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
!
           MATO(7) = MATO(7) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,4) )
           MATO(8) = MATO(8) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,4) )
           MATO(9) = MATO(9) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,4) )
           MATO(10)= MATO(10) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,4) )
!
           MATO(11)= MATO(11) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,5) )
           MATO(12)= MATO(12) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,5) )
           MATO(13)= MATO(13) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,5) )
           MATO(14)= MATO(14) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,5) )
           MATO(15)= MATO(15) + DP_VV_V ( M1, MAT1(1,5), MAT2(1,5) )
!
           MATO(16)= MATO(16) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,6) )
           MATO(17)= MATO(17) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,6) )
           MATO(18)= MATO(18) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,6) )
           MATO(19)= MATO(19) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,6) )
           MATO(20)= MATO(20) + DP_VV_V ( M1, MAT1(1,5), MAT2(1,6) )
           MATO(21)= MATO(21) + DP_VV_V ( M1, MAT1(1,6), MAT2(1,6) )
         ELSE IF ( M3 .EQ. 7 .AND.  FL_UPDATE ) THEN
           MATO(1) = MATO(1) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,1) )
!
           MATO(2) = MATO(2) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,2) )
           MATO(3) = MATO(3) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,2) )
!
           MATO(4) = MATO(4) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,3) )
           MATO(5) = MATO(5) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,3) )
           MATO(6) = MATO(6) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,3) )
!
           MATO(7) = MATO(7) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,4) )
           MATO(8) = MATO(8) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,4) )
           MATO(9) = MATO(9) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,4) )
           MATO(10)= MATO(10) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,4) )
!
           MATO(11)= MATO(11) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,5) )
           MATO(12)= MATO(12) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,5) )
           MATO(13)= MATO(13) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,5) )
           MATO(14)= MATO(14) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,5) )
           MATO(15)= MATO(15) + DP_VV_V ( M1, MAT1(1,5), MAT2(1,5) )
!
           MATO(16)= MATO(16) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,6) )
           MATO(17)= MATO(17) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,6) )
           MATO(18)= MATO(18) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,6) )
           MATO(19)= MATO(19) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,6) )
           MATO(20)= MATO(20) + DP_VV_V ( M1, MAT1(1,5), MAT2(1,6) )
           MATO(21)= MATO(21) + DP_VV_V ( M1, MAT1(1,6), MAT2(1,6) )
!
           MATO(22)= MATO(22) + DP_VV_V ( M1, MAT1(1,1), MAT2(1,7) )
           MATO(23)= MATO(23) + DP_VV_V ( M1, MAT1(1,2), MAT2(1,7) )
           MATO(24)= MATO(24) + DP_VV_V ( M1, MAT1(1,3), MAT2(1,7) )
           MATO(25)= MATO(25) + DP_VV_V ( M1, MAT1(1,4), MAT2(1,7) )
           MATO(26)= MATO(26) + DP_VV_V ( M1, MAT1(1,5), MAT2(1,7) )
           MATO(27)= MATO(27) + DP_VV_V ( M1, MAT1(1,6), MAT2(1,7) )
           MATO(28)= MATO(28) + DP_VV_V ( M1, MAT1(1,7), MAT2(1,7) )
        ELSE IF ( M1 .LE. DB1__MUL_MM_TI_S ) THEN
!
! -------- Small dimensions
!
           LC1 = 0
           DO 410 J1=1,M3
              LC1 = LC1 + J1
              LC  = LC1
              IF ( FL_UPDATE ) THEN
                   DO 420 J2=J1,M3
                      MATO(LC)= MATO(LC) + DP_VV_V ( M1, MAT1(1,J1), MAT2(1,J2) )
                      LC = LC + J2
 420               CONTINUE
                ELSE 
                   DO 430 J3=J1,M3
                      MATO(LC)= DP_VV_V ( M1, MAT1(1,J1), MAT2(1,J3) )
                      LC = LC + J3
 430               CONTINUE
              END IF
 410      CONTINUE
        ELSE IF ( M1 .LE. DB2__MUL_MM_TI_S ) THEN
!
! -------- Medium dimensions
!
! -------- Allocate memory for a temporary copy of the result
!
           ALLOCATE ( MAT3C(M3,M3,1), STAT=IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*M3*M3, STR )
                CALL ERR_LOG ( 18, IUER, 'MUL_MM_TI_S', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                RETURN 
           END IF
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT3C, 0, %VAL(8*M3*M3) )
#endif
!
           CALL DGEMM ( 'T', 'N', N1, N2, M1, 1.D0, MAT1, M1, MAT2, M2, 0.D0, &
     &                  MAT3C, M3 )
           IF ( .NOT. FL_UPDATE ) THEN
                CALL TRG_SCATTER ( M3, 1, M3, 1, M3, MAT3C, MATO )
              ELSE 
                CALL TRG_SCATTER_UPD ( M3, 1, M3, 1, M3, MAT3C, MATO )
           END IF
!
           DEALLOCATE ( MAT3C )
        ELSE
!
! -------- Large dimensions
!
           IF ( .NOT. FL_UPDATE ) THEN
                 CALL NOUT8_R8 ( (INT8(M3)*INT8(M3+1))/2, MATO )  ! Initialization
           END IF
           IP1 = M3/DB3__MUL_MM_TI_S 
           IF ( IP1*DB3__MUL_MM_TI_S .LT. M3 ) IP1 = IP1 + 1
           IP2 = M1/DB3__MUL_MM_TI_S 
           IF ( IP2*DB3__MUL_MM_TI_S .LT. M1 ) IP2 = IP2 + 1
!
! -------- Set the number of threads
!
           NTHR_SAVED = OMP_GET_MAX_THREADS()
           IF ( OMP_IN_PARALLEL() ) THEN
                NTHR = 1
             ELSE 
                NTHR = MIN ( IP2, OMP_GET_MAX_THREADS() )
           END IF
!
! -------- Allocate memory for temporary copies of operands
!
           ALLOCATE ( MAT1C(DB3__MUL_MM_TI_S, DB3__MUL_MM_TI_S, NTHR), &
     &                MAT2C(DB3__MUL_MM_TI_S, DB3__MUL_MM_TI_S, NTHR), &
     &                MAT3C(DB3__MUL_MM_TI_S, DB3__MUL_MM_TI_S, NTHR), &
     &                STAT=IERR )
           IF ( IERR .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*3*DB3__MUL_MM_TI_S*DB3__MUL_MM_TI_S*NTHR, STR )
                CALL ERR_LOG ( 20, IUER, 'MUL_MM_TI_S', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &              'memory' )
                RETURN 
           END IF
           CALL OMP_SET_NUM_THREADS( %VAL(NTHR) ) 
#ifdef BLAS_NOT_A_NUMBER
           CALL MEMSET ( MAT1C, 0, %VAL(8*DB3__MUL_MM_TI_S*DB3__MUL_MM_TI_S) )
           CALL MEMSET ( MAT2C, 0, %VAL(8*DB3__MUL_MM_TI_S*DB3__MUL_MM_TI_S) )
           CALL MEMSET ( MAT3C, 0, %VAL(8*DB3__MUL_MM_TI_S*DB3__MUL_MM_TI_S) )
#endif
!
!
           IND_2 = 1
           DO 440 J4=1,IP1 ! columns of mato
              DIM_2 = DB3__MUL_MM_TI_S
              IF ( IND_2 + DIM_2-1 .GT. M3 ) DIM_2 = M3 - IND_2 + 1
!
              IND_1 = 1
              DO 450 J5=1,IP1  ! raws of mato
                 DIM_1 = DB3__MUL_MM_TI_S
                 IF ( IND_1 + DIM_1-1 .GT. M3 ) DIM_1 = M3 - IND_1 + 1
!
                 IF ( IND_1 < IND_2 + DIM_2 ) THEN
!$OMP                 PARALLEL DO IF ( NTHR > 1 ), DEFAULT ( NONE), &
!$OMP&                PRIVATE ( J6, IND_THR, IND_M, DIM_M ), &
!$OMP&                SHARED  ( NTHR, M1, M2, M3, N1, N2, IP2, IND_1, IND_2, DIM_1, DIM_2, &
!$OMP&                          MAT1, MAT2, MAT1C, MAT2C, MAT3C, MATO )
                      DO 460 J6=1,IP2
                         IND_THR = OMP_GET_THREAD_NUM() + 1
                         IND_M = 1 + (J6-1)*DB3__MUL_MM_TI_S
                         DIM_M = DB3__MUL_MM_TI_S
                         IF ( DIM_M .GT. M1 ) DIM_M = M1
                         IF ( IND_M + DIM_M-1 .GT. M1 ) DIM_M = M1 - IND_M + 1
!
! ---------------------- Extract the region of the right operand and put it into
! ---------------------- the temporary matrix
!
                         CALL RCT_GATHER ( M1, N1, IND_M, DIM_M, IND_1, DIM_1, &
     &                                     MAT1, MAT1C(1,1,IND_THR) )
!
! ---------------------- Extract the region of the left operand and put it into
! ---------------------- the temporary matrix
!
                         CALL RCT_GATHER ( M2, N2, IND_M, DIM_M, IND_2, DIM_2, &
     &                                     MAT2, MAT2C(1,1,IND_THR) )
!
! ---------------------- Multiply the regions
!
                         CALL DGEMM ( 'T', 'N', DIM_1, DIM_2, DIM_M, 1.D0, &
     &                                 MAT1C(1,1,IND_THR), DIM_M, &
     &                                 MAT2C(1,1,IND_THR), DIM_M, 0.D0, &
     &                                 MAT3C(1,1,IND_THR), DIM_1 )
!$OMP     CRITICAL (MATO_UPDATE)
                         CALL TRG_ADD_SCT ( M3, IND_1, DIM_1, IND_2, DIM_2, &
     &                                      MAT3C(1,1,IND_THR), MATO )
!$OMP END CRITICAL (MATO_UPDATE)
 460                  CONTINUE 
!$OMP END PARALLEL DO
                 END IF
                 IND_1 = IND_1 + DB3__MUL_MM_TI_S
 450          CONTINUE 
              IND_2 = IND_2 + DB3__MUL_MM_TI_S
 440       CONTINUE 
!
           DEALLOCATE ( MAT1C, MAT2C, MAT3C )
           CALL OMP_SET_NUM_THREADS( %VAL(NTHR_SAVED) ) 
      END IF
#endif  ! generic
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_TI_S  #!#
