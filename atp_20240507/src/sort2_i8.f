      SUBROUTINE SORT2_I8 ( L, LIS, MIS )
!      
! ************************************************************************
! *                                                                      *
! * Routine SORT2_I8 sorts both integer arrays LIS and MIS               *
! * of length L in the increasing order of the elements of the array     *
! * LIS. One can consider the lements LIS(I), MIS(I) as connected        *
! * pairs. Thus, these connected paris are sorted.                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * L ( INTEGER*8 ) -- Number of elements in arrays LIS, MIS.            *
! *                                                                      *
! * ___________________ Modified parametersL: __________________________ *
! *                                                                      *
! * LIS ( INTEGER*8 ) -- The first sorted array. It is sorted in         *
! * increasing its elements.                                             *
! * MIS ( INTEGER*8 ) -- The second sorted array. It is sorted in        *
! * the order of increasing elemts of the                                *
! * array LIS.                                                           *
! *                                                                      *
! * NB: Slow algorithm O(N**2) is used. This routine should be           *
! * replaced with fast O(N*log_2(N)) Hoar algorithm for sorting large    *
! * arrays.                                                              *
! *                                                                      *
! * ### 31-JUL-1989 SORT2_I8 v1.0 (c) L. Petrov 30-MAR-1992 ###          *
! *                                                                      *
! ************************************************************************
!
      IMPLICIT NONE
      INTEGER*8 L
      INTEGER*8 LIS(L), MIS(L)
      INTEGER*8 J1, J2, LI, MI, IN, LR, MR
      
      IF ( L .LE. 1 ) RETURN
!
      DO 410 J1=1,L-1
      
         LI=LIS(J1)
         MI=MIS(J1)
         IN=J1
         DO 420 J2=J1+1,L
            IF( LIS(J2).LT.LI ) THEN
               LI=LIS(J2)
               MI=MIS(J2)
               IN=J2
            END IF
 420     CONTINUE
         IF( IN.EQ.J1 ) GOTO 410
         LR=LIS(J1)
         MR=MIS(J1)
         LIS(J1)=LI
         MIS(J1)=MI
         LIS(IN)=LR
         MIS(IN)=MR
 410  CONTINUE
      RETURN
      
      END                       !#! SORT2_I8 #!#
      
