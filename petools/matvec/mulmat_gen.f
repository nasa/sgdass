      SUBROUTINE MUL_MM_II_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_II_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  10-Dec-96   MUL_MM_II_I  v1.0  (c)  L. Petrov   10-Dec-96 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3, N3)
      CHARACTER  STR*80
C
      REAL*8     S
      INTEGER*4  J1, J2, J3, I_LEN, ILEN
C
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_II_I', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_II_I', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR = 'N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_II_I', STR )
           RETURN
      END IF
CCC
      DO 410 J1=1,M1
         DO 420 J2=1,N2
            S=0.0D0
            DO 430 J3=1,N1
               S = S + MAT1(J1,J3)*MAT2(J3,J2)
 430        CONTINUE
         MATO(J1,J2) = S
 420     CONTINUE
 410  CONTINUE
CCC
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_II_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_II_S ( M1, N1, MAT1, M2, N2, MAT2, M3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_II_S  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  19-Dec-96   MUL_MM_II_S  v1.0  (c)  L. Petrov  19-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, N2, M3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(*)
      CHARACTER  STR*80
C
      REAL*8     S
      INTEGER*4  J1, J2, J3, LC, I, J, LOC
      INTEGER*4  I_LEN, ILEN
      LOC(I,J) = min(I,J) +(max(I,J)*(max(I,J)-1))/2
C
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_II_S', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_II_S', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR = 'N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_II_S', STR )
           RETURN
      END IF
C
CCC
      LC=0
      DO 410 J1=1,M1
         DO 420 J2=1,J1
            S=0.D0
            DO 430 J3=1,N1
               S = S + MAT1(J2,J3)*MAT2(J3,J1)
 430        CONTINUE
            LC=LC+1
            MATO(LC)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_II_S  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_IS_I ( M1, N1, MAT1, M2, MAT2, M3, N3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_IS_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96    MUL_MM_IS_I   v1.0 (c)  L. Petrov  11-Dec-96 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(*), MATO(M3,N3)
      INTEGER*4  J1, J2, J3, L1, I, J, LOC
      REAL*8     S
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
      LOC(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
C
      IF ( N1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_IS_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_IS_I', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_IS_I', STR )
           RETURN
      END IF
CCC
      DO 410 J1=1,M1
         DO 420 J2=1,N1
            S=0.D0
            DO 430 J3=1,M2
               L1=LOC(J2,J3)
               S = S + MAT1(J1,J3)*MAT2(L1)
 430        CONTINUE
            MATO(J1,J2)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_IS_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_IT_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_IT_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96   MUL_MM_IT_I    v1.0 (c)  L. Petrov  11-Dec-96 ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER, IER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3, N3)
      CHARACTER  STR*80
C
      REAL*8     S
      INTEGER*4  J1, J2, J3, I_LEN, ILEN
C
      IF ( N1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR = 'N1 .NE. N2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_IT_I', STR )
           RETURN
      END IF
CCC
      DO 410 J1=1,M1
         DO 420 J2=1,M2
            S=0.0D0
            DO 430 J3=1,N1
               S = S + MAT1(J1,J3)*MAT2(J2,J3)
 430        CONTINUE
         MATO(J1,J2) = S
 420     CONTINUE
 410  CONTINUE
CCC
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_IT_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_IT_S ( M1, N1, MAT1, M2, N2, MAT2, M3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_IT_S  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  12-Dec-96     MUL_MM_IT_S  v1.0 (c)  L. Petrov 12-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, N2, M3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(*)
      INTEGER*4  J1, J2, J3, LC
      REAL*8     S
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
C
      IF ( N1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR = 'N1 .NE. N2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_IT_S', STR )
           RETURN
      END IF
C
      IF ( M1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M3  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_IT_S', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_IT_S', STR )
           RETURN
      END IF
CCC
      LC=0
      DO 410 J1=1,M1
         DO 420 J2=1,J1
            S=0.D0
            DO 430 J3=1,N1
               S = S + MAT1(J2,J3)*MAT2(J1,J3)
 430        CONTINUE
            LC=LC+1
            MATO(LC)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_IT_S  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_SI_I ( M1, MAT1, M2, N2, MAT2, M3, N3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_SI_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  09-Dec-96  MUL_MM_SI_I    v1.0 (c)  L. Petrov   9-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(*), MAT2(M2,N2), MATO(M3,N3)
      INTEGER*4  J1, J2, J3, L1, I, J, LOC
      REAL*8     S
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
      LOC(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_SI_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. M3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SI_I', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_SI_I', STR )
           RETURN
      END IF
CCC
      DO 410 J1=1,M3
         DO 420 J2=1,N3
            S=0.D0
            DO 430 J3=1,M1
               L1=LOC(J1,J3)
               S = S + MAT1(L1)*MAT2(J3,J2)
 430        CONTINUE
            MATO(J1,J2)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_SI_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_SI_S ( M1, MAT1, M2, N2, MAT2, M3, MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_SI_S  multiplies matrices:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  15-JUL-99  MUL_MM_SI_S    v1.0 (c)  L. Petrov  15-JUL-99  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, M2, N2, M3, IUER
      REAL*8     MAT1(*), MAT2(M2,N2), MATO(*)
      INTEGER*4  J1, J2, J3, L1, LC, I, J, LOC
      REAL*8     S
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
      LOC(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_SI_S', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. M3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SI_S', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. M3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_SI_S', STR )
           RETURN
      END IF
CCC
      LC=0
      DO 410 J1=1,M3
         DO 420 J2=1,J1
            S=0.D0
            DO 430 J3=1,M1
               L1=LOC(J1,J3)
               S = S + MAT1(L1)*MAT2(J3,J2)
 430        CONTINUE
            LC=LC+1
            MATO(LC)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_SI_S  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_SS_I ( M1, MAT1, M2, MAT2, M3, N3, MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_SS_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96  MUL_MM_SS_I    v1.0 (c)  L. Petrov  11-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      INTEGER*4  M1, M2, M3, N3, IUER
      REAL*8     MAT1(*), MAT2(*), MATO(M3,N3)
      INTEGER*4  J1, J2, J3, L1, L2, LC, I, J, LOC
      REAL*8     S
      CHARACTER  STR*80
C
      LOC(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_SS_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. M3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SS_I', STR )
           RETURN
      END IF
C
      IF ( M3 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N3  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SS_I', STR )
           RETURN
      END IF
C
      DO 410 J1=1,M1
         DO 420 J2=1,M2
            S=0.D0
            DO 430 J3=1,M3
               L1=LOC(J1,J3)
               L2=LOC(J2,J3)
               S = S + MAT1(L1)*MAT2(L2)
 430        CONTINUE
            MATO(J1,J2)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_SS_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_SS_S ( M1, MAT1, M2, MAT2, M3, MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_SS_S  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96  MUL_MM_SS_S    v1.0 (c)  L. Petrov  11-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      INTEGER*4  M1, M2, M3, IUER
      REAL*8     MAT1(*), MAT2(*), MATO(*)
      INTEGER*4  J1, J2, J3, L1, L2, LC, I, J, LOC
      REAL*8     S
      CHARACTER  STR*80
C
      LOC(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_SS_S', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. M3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_SS_S', STR )
           RETURN
      END IF
C
      LC=0
      DO 410 J1=1,M1
         DO 420 J2=1,J1
            S=0.D0
            DO 430 J3=1,M3
               L1=LOC(J1,J3)
               L2=LOC(J2,J3)
               S = S + MAT1(L1)*MAT2(L2)
 430        CONTINUE
            LC=LC+1
            MATO(LC)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_SS_S  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_ST_I ( M1, MAT1, M2, N2, MAT2, M3, N3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_ST_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96  MUL_MM_SI_I    v1.0 (c)  L. Petrov  11-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(*), MAT2(M2,N2), MATO(M3,N3)
      INTEGER*4  J1, J2, J3, L1, I, J, LOC
      REAL*8     S
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
      LOC(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
C
      IF ( M1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. M3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_ST_I', STR )
           RETURN
      END IF
CCC
      DO 410 J1=1,M3
         DO 420 J2=1,N3
            S=0.D0
            DO 430 J3=1,M1
               L1=LOC(J1,J3)
               S = S + MAT1(L1)*MAT2(J2,J3)
 430        CONTINUE
            MATO(J1,J2)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_ST_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_TI_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_TI_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  12-Dec-96     MUL_MM_TI_I  v1.0  (c) L. Petrov 12-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3,N3)
      INTEGER*4  J1, J2, J3, LC
      REAL*8     S
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
C
      IF ( N2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='N2 .NE. N3  N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
C
      IF ( M3 .NE. N1 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N1  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TI_I', STR )
           RETURN
      END IF
CCC
      DO 410 J1=1,N1
         DO 420 J2=1,N2
            S=0.D0
            DO 430 J3=1,M1
               S = S + MAT1(J3,J1)*MAT2(J3,J2)
 430        CONTINUE
            MATO(J1,J2)=S
 420     CONTINUE
 410  CONTINUE
CCC
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_TI_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_TI_S ( M1, N1, MAT1, M2, N2, MAT2, M3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_TI_S  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###   9-Dec-96       MUL_MM_TI_S    (c)  L. Petrov   9-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, N2, M3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(*)
      INTEGER*4  J1, J2, J3, LC
      REAL*8     S
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
C
      IF ( N1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. N2  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
C
      IF ( M3 .NE. N1 ) THEN
           CALL CLRCH ( STR )
           STR ='M3 .NE. N1  M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TI_S', STR )
           RETURN
      END IF
CCC
      LC=0
      DO 410 J1=1,N1
         DO 420 J2=1,J1
            S=0.D0
            DO 430 J3=1,M1
               S = S + MAT1(J3,J2)*MAT2(J3,J1)
 430        CONTINUE
            LC=LC+1
            MATO(LC)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_TI_S  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_TS_I ( M1, N1, MAT1, M2, MAT2, M3, N3, MATO,
     #                         IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_TS_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96  MUL_MM_TS_I    v1.0 (c)  L. Petrov  11-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(*), MATO(M3,N3)
      INTEGER*4  J1, J2, J3, L1, I, J, LOC
      REAL*8     S
      CHARACTER  STR*80
C
      INTEGER*4  I_LEN, ILEN
      LOC(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
C
      IF ( M1 .NE. M2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. M2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
C
      IF ( N1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. N3  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TS_I', STR )
           RETURN
      END IF
CCC
      DO 410 J1=1,M3
         DO 420 J2=1,N3
            S=0.D0
            DO 430 J3=1,M2
               L1=LOC(J2,J3)
               S = S + MAT1(J3,J1)*MAT2(L1)
 430        CONTINUE
            MATO(J1,J2)=S
 420     CONTINUE
 410  CONTINUE
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_TS_I  #!#
C
C ------------------------------------------------------------------------
C
      SUBROUTINE MUL_MM_TT_I ( M1, N1, MAT1, M2, N2, MAT2, M3, N3,
     #                         MATO, IUER )
C ************************************************************************
C *                                                                      *
C *   Subroutine  MUL_MM_TT_I  multiplies matrixes:                      *
C *   MATO = MAT1 * MAT2                                                 *
C *                                                                      *
C *  ###  11-Dec-96   MUL_MM_TT_I  v1.0  (c)  L. Petrov  11-Dec-96  ###  *
C *                                                                      *
C ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MATO(M3,N3)
      CHARACTER  STR*80
C
      REAL*8     S
      INTEGER*4  J1, J2, J3, I_LEN, ILEN
C
      IF ( M1 .NE. N2 ) THEN
           CALL CLRCH ( STR )
           STR ='M1 .NE. N2  M1='
           CALL INCH ( M1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N2='
           CALL INCH ( N2, STR(I_LEN(STR)+2:) )
           CALL ERR_LOG ( 12, IUER, 'MUL_MM_TT_I', STR )
           RETURN
      END IF
C
      IF ( N1 .NE. M3 ) THEN
           CALL CLRCH ( STR )
           STR ='N1 .NE. M3  N1='
           CALL INCH ( N1, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'M3='
           CALL INCH ( M3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 14, IUER, 'MUL_MM_TT_I', STR )
           RETURN
      END IF
C
      IF ( M2 .NE. N3 ) THEN
           CALL CLRCH ( STR )
           STR ='M2 .NE. N3  M2='
           CALL INCH ( M2, STR(I_LEN(STR)+1:) )
           STR(ILEN(STR)+2:) = 'N3='
           CALL INCH ( N3, STR(I_LEN(STR)+1:) )
           CALL ERR_LOG ( 16, IUER, 'MUL_MM_TT_I', STR )
           RETURN
      END IF
CCC
      DO 410 J1=1,M3
         DO 420 J2=1,N3
            S=0.0D0
            DO 430 J3=1,M1
               S = S + MAT1(J3,J1)*MAT2(J2,J3)
 430        CONTINUE
         MATO(J1,J2) = S
 420     CONTINUE
 410  CONTINUE
CCC
C
      CALL ERR_LOG ( 0, IUER,, )
      RETURN
      END  !#!  MUL_MM_TT_I  #!#
