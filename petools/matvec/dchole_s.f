      SUBROUTINE DCHOLE_S ( N, MAT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  DCHOLE_S performs Cholesky decompostion of a square       *
! *   positively defined matrix in the upper triangular representation   *
! *   of dimension N. The result is the transoposed Cholesky             *
! *   decomposition:                                                     *
! *                                                                      *
! *   MAT  = D * D'                                                      *
! *                                                                      *
! *   The output is non-zero elements of D' in the upper triangular      *
! *   representation.                                                    *                
! *                                                                      *
! *   M11  M12  M13  M14     D11  0   0   0      D11 D12 D13 D14         *
! *   M21  M22  M23  M24  =  D12 D22  0   0   *   0  D22 D23 D24         *
! *   M31  M32  M33  M34     D13 D23 D33  0       0   0  D33 D34         *
! *   M41  M42  M43  M44     D14 D24 D3d D44      0   0   0  D44         *
! *                                                                      *
! *   NB: the output matrix is NOT symmetric. The output is              *
! *                                                                      *
! *       D11 D12 D13 D14                                                *
! *           D22 D23 D24                                                *
! *               D33 D34                                                *
! *                   D44                                                *
! *                                                                      *
! *  ### 16-JUN-2021    DCHOLE_S   v1.0 (c)  L. Petrov  16-JUN-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, IUER
      REAL*8     MAT(*)
      REAL*8,    ALLOCATABLE :: MAT_TEMP(:)
      CHARACTER  STR*128
      INTEGER*4  NB, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Allocate additional memory for reordering
!
      NB = (N-N/2)
      ALLOCATE ( MAT_TEMP((NB*(NB+1))/2), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( (4*NB*(NB+1)), STR )
           CALL ERR_LOG ( 1261, IUER, 'DCHOLE_S', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &         'a temporary array needed for the recursive algorithm '// &
     &         'for matrix inversion' )
           RETURN
      END IF
!
! --- Matrix reordering to recursive packed upper triangular format
!
      CALL DREORDER5 ( N, MAT, MAT_TEMP )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1262, IUER, 'DCHOLE_S','Error in DREORDER '// &
     &         'IER = '//STR )
           DEALLOCATE ( MAT_TEMP, STAT=IER )
           RETURN
      END IF
!
! --- Cholesky decomposition
!
      IER = 0
      CALL DRPPTRF2 ( N, MAT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH  ( STR )
           CALL INCH   ( IER, STR )
           CALL ERR_LOG ( IER, IUER, 'DCHOLE_S', 'Error in matrix '// &
     &         'factorization at the '//STR(1:I_LEN(STR))//'-th step' )
           DEALLOCATE ( MAT_TEMP, STAT=IER )
           RETURN
      END IF
!
! --- Matrix reordering from recursive packed upper triangular format
!
      CALL DREORDER6 ( N, MAT, MAT_TEMP )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1264, IUER, 'DCHOLE_S', 'Error in DREORDER '// &
     &         'IER = '//STR )
           DEALLOCATE ( MAT_TEMP, STAT=IER )
           RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  DCHOLE_S  !#!#
