      SUBROUTINE MAT_SUMTRA ( L, MATI, MATO )
! ************************************************************************
! *                                                                      *
! *   Subroutime  MAT_SUMTRA  calculats summa MATI + MATI(T), where      *
! *   MATI is square martix in rectangular representation. MATO is       *
! *   square symmetric matrix in upper-diagonal representation.          *
! *                                                                      *
! *  ###  19-DEC-96   MAT_SUMTRA    v2.0 (c)  L. Petrov 22-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  L
      REAL*8     MATI(L,L), MATO(*)
!
      INTEGER*4  J1, J2, LC
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      LC=0
      DO 410 J1=1,L
         DO 420 J2=1,J1
            LC=LC+1
            MATO(LC)=MATI(J1,J2) + MATI(J2,J1)
 420     CONTINUE
 410  CONTINUE
!
      RETURN
      END  !#!  MAT_SUMTRA  #!#
