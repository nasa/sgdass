#include <mk5_preprocessor_directives.inc>
      PROGRAM    MATVEC_TEST
! ************************************************************************
! *                                                                      *
! *   Program MATVEC_TEST  performs tests of matvec routines.            *
! *   Usage:  mul_mm_test <suffix> <sanity|timer>                        *
! *                                                                      *
! *  ### 17-AUG-2002  MATVEC_TEST  v1.2 (c)  L. Petrov  30-JAN-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FUNC*8, TYP*8
      INTEGER*4  IUER
      INTEGER*4  IARGC
!
      IF ( IARGC() .LT. 2 ) THEN
           WRITE ( 6, * ) 'Usage: mul_mm_test <func> <sanity|timer>'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FUNC )
           CALL GETARG ( 2, TYP  )
      END IF
!
      CALL TRAN ( 12, FUNC, FUNC )
      CALL TRAN ( 12, TYP,  TYP  )
!
      IF ( TYP .EQ. 'sanity' ) THEN
           WRITE ( 6, '(A,A)' ) 'Sanity check for function ', FUNC
           WRITE ( 6, '(A)'   ) ' '
           IUER = -1
           CALL MUL_MM_SANITY ( FUNC, IUER )
           WRITE ( 6, '(A)'   ) ' '
           CALL FLUSH ( 6 )
           IF ( IUER .NE. 0 ) CALL EXIT ( 2 )
         ELSE IF ( TYP .EQ. 'timer' ) THEN
           WRITE ( 6, '(A,A)' ) 'Speed of function ', FUNC
           WRITE ( 6, '(A)'   ) ' '
           IUER = -1
           CALL MUL_MM_TIMER ( FUNC, IUER  )
           WRITE ( 6, '(A)'   ) ' '
           CALL FLUSH ( 6 )
           IF ( IUER .NE. 0 ) CALL EXIT ( 2 )
         ELSE
           CALL ERR_LOG ( 601, -2, 'MATVEC_TEST', 'Wrong value of the '// &
     &                   'second argument: '//TYP//' only sanity and '// &
     &                   'timer are supported' )
           CALL EXIT ( 1 )
      END IF
      END  !#!  MATVEC_TEST  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MM_SANITY ( FUNC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_MM_SANITY
! *                                                                      *
! *  ### 17-AUG-2002  MUL_MM_SANITY v1.2 (c) L. Petrov  20-JAN-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FUNC*(*)
      INTEGER*4  IUER
      INTEGER*4  M_DIM
      PARAMETER  ( M_DIM = 47 )
      ADDRESS__TYPE MEM_ADR, MEM_LEN, ADR_MAT1, ADR_MAT2, ADR_MAT3, ADR_MAT4
      INTEGER*4  DIM_ARR(3,M_DIM), J1, IER
      DATA       DIM_ARR / &
     &                            2,    2,    2, &
     &                            3,    3,    3, &
     &                            4,    4,    4, &
     &                            5,    5,    5, &
     &                            6,    6,    6, &
     &                            7,    7,    7, &
     &                            8,    8,    8, &
     &                            9,    9,    9, &
     &                           16,   16,   16, &
     &                           34,   35,   16, &
     &                           36,   16,   36, &
     &                           16,   34,   34, &
     &                           36,   36,   36, &
     &                          222,  259,  222, &
     &                          222,  259,  259, &
     &                          259,  222,  222, &
     &                          259,  222,  259, &
     &                          512,  512,  512, &
     &                          514,  514,  512, &
     &                          514,  512,  514, &
     &                          512,  514,  514, &
     &                          514,  514,  514, &
     &                        1024,     1, 1024, &
     &                        1024,     2, 1024, &
     &                        1024,     3, 1024, &
     &                        1024,     4, 1024, &
     &                        1024,     5, 1024, &
     &                        1024,     6, 1024, &
     &                        1024,     7, 1024, &
     &                        1024,     8, 1024, &
     &                           1, 1024,  1024, &
     &                           2, 1024,  1024, &
     &                           3, 1024,  1024, &
     &                           4, 1024,  1024, &
     &                           5, 1024,  1024, &
     &                           6, 1024,  1024, &
     &                           7, 1024,  1024, &
     &                           8, 1024,  1024, &
     &                       1024,  1024,     1, &
     &                       1024,  1024,     2, &
     &                       1024,  1024,     3, &
     &                       1024,  1024,     4, &
     &                       1024,  1024,     5, &
     &                       1024,  1024,     6, &
     &                       1024,  1024,     7, &
     &                       1024,  1024,     8, &
     &                       1200,  1200,  1200  &
     &                   /
      CHARACTER  STR*32
#ifdef OPT_DP_VV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_II_I, GEN_MUL_MM_II_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IT_I, GEN_MUL_MM_IT_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TI_I, GEN_MUL_MM_TI_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TT_I, GEN_MUL_MM_TT_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SS_I, GEN_MUL_MM_SS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SS_S, GEN_MUL_MM_SS_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IS_I, GEN_MUL_MM_IS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TS_I, GEN_MUL_MM_TS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SI_I, GEN_MUL_MM_SI_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_ST_I, GEN_MUL_MM_ST_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_II_S, GEN_MUL_MM_II_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IT_S, GEN_MUL_MM_IT_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TI_S, GEN_MUL_MM_TI_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_IV_V, GEN_MUL_MV_IV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_TV_V, GEN_MUL_MV_TV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_SV_V, GEN_MUL_MV_SV_V
      ADDRESS__TYPE, EXTERNAL :: GEN_INVSF, INVSL, INVSP, INVS
#else
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_II_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IT_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TI_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TT_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SS_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SI_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_ST_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_II_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IT_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TI_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_IV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_TV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_SV_V
      ADDRESS__TYPE, EXTERNAL :: GEN_INVSF, INVSL, INVSP, INVS
#endif
      INTEGER*4  I_LEN
!
      CALL ERR_PASS ( IUER, IER )
#ifdef ADR_64BIT
      CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 4, &
     &                8*INT8(4096)*INT8(4096), ADR_MAT1,    &
     &                8*INT8(4096)*INT8(4096), ADR_MAT2,    &
     &                8*INT8(4096)*INT8(4096), ADR_MAT3,    &
     &                8*INT8(4096)*INT8(4096), ADR_MAT4       )
#else
      CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 4, &
     &                8*4096*4096, ADR_MAT1,    &
     &                8*4096*4096, ADR_MAT2,    &
     &                8*4096*4096, ADR_MAT3,    &
     &                8*4096*4096, ADR_MAT4       )
#endif
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
#ifdef ADR_64BIT
           CALL IINCH8 ( MEM_LEN, STR )
#else
           CALL IINCH ( MEM_LEN, STR )
#endif
           CALL ERR_LOG ( 1711, IUER, 'MUL_MM_SANITY', 'Error in an '// &
     &         'attempt to grab '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory' )
           RETURN
      END IF
!
      DO 410 J1=1,M_DIM
         CALL ERR_PASS ( IUER, IER )
         IF ( FUNC .EQ. 'ii_i' ) THEN
              CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_II_I, MUL_MM_II_I,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),    &
     &             DIM_ARR(3,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &             %VAL(ADR_MAT4), IER )
           ELSE IF ( FUNC .EQ. 'it_i' ) THEN
              CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_IT_I, MUL_MM_IT_I,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &             DIM_ARR(2,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &             %VAL(ADR_MAT4), IER )
           ELSE IF ( FUNC .EQ. 'ti_i' ) THEN
              CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_TI_I, MUL_MM_TI_I,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),    &
     &             DIM_ARR(3,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &             %VAL(ADR_MAT4), IER )
           ELSE IF ( FUNC .EQ. 'ss_i' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
!
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_SS_I, MUL_MM_SS_I, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),    &
     &                  DIM_ARR(3,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'ss_s' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
!
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_SS_S, MUL_MM_SS_S, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),    &
     &                  DIM_ARR(3,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'is_i' ) THEN
              IF ( DIM_ARR(2,J1) .EQ. DIM_ARR(3,J1) ) THEN
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_IS_I, MUL_MM_IS_I, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),    &
     &                  DIM_ARR(3,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'ts_i' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1) ) THEN
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_TS_I, MUL_MM_TS_I, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),    &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'si_i' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1) ) THEN
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_SI_I, MUL_MM_SI_I, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),    &
     &                  DIM_ARR(3,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'st_i' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1) ) THEN
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_ST_I, MUL_MM_ST_I, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &                  DIM_ARR(2,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'tt_i' ) THEN
              CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_TT_I, MUL_MM_TT_I,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &             %VAL(ADR_MAT4), IER )
           ELSE IF ( FUNC .EQ. 'ii_s' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1) ) THEN
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_II_S, MUL_MM_II_S, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),    &
     &                  DIM_ARR(1,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'it_s' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1) ) THEN
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_IT_S, MUL_MM_IT_S, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),    &
     &                  DIM_ARR(2,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'ti_s' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1) ) THEN
                   CALL MUL_MM_COMPARE ( FUNC, GEN_MUL_MM_TI_S, MUL_MM_TI_S, &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),    &
     &                  DIM_ARR(2,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),    &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), &
     &                  %VAL(ADR_MAT4), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'invs' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
!
                   CALL INV_CHECK ( FUNC, GEN_INVSF, DIM_ARR(1,J1), &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
                 ELSE
                   IER = 0
              END IF
!@           ELSE IF ( FUNC .EQ. 'invsf' ) THEN
!@              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
!@     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
!@!
!@                   CALL INV_CHECK ( FUNC, GEN_INVSF, DIM_ARR(1,J1), &
!@     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
!@!
!@                   IF ( IER .EQ. 0 ) THEN
!@                        CALL ERR_PASS ( IUER, IER )
!@                        CALL INV_CHECK ( FUNC, INVSF, DIM_ARR(1,J1), &
!@     &                    %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
!@                   END IF
!@                 ELSE
!@                   IER = 0
!@              END IF
           ELSE IF ( FUNC .EQ. 'invsl' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
!
                   CALL INV_CHECK ( FUNC, INVSL, DIM_ARR(1,J1), &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'invsp' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
!
                   CALL INV_CHECK ( FUNC, INVSP, DIM_ARR(1,J1), &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
                 ELSE
                   IER = 0
              END IF
           ELSE IF ( FUNC .EQ. 'iv_v' ) THEN
              CALL MUL_MV_COMPARE ( FUNC, GEN_MUL_MV_IV_V, MUL_MV_IV_V,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), %VAL(ADR_MAT1),         &
     &             %VAL(ADR_MAT2), %VAL(ADR_MAT3), %VAL(ADR_MAT4), IER )
           ELSE IF ( FUNC .EQ. 'tv_v' ) THEN
              CALL MUL_MV_COMPARE ( FUNC, GEN_MUL_MV_TV_V, MUL_MV_TV_V,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), %VAL(ADR_MAT1),         &
     &             %VAL(ADR_MAT2), %VAL(ADR_MAT3), %VAL(ADR_MAT4), IER )
            ELSE IF ( FUNC .EQ. 'sv_v' ) THEN
              CALL MUL_MV_COMPARE ( FUNC, GEN_MUL_MV_SV_V, MUL_MV_SV_V,  &
     &             DIM_ARR(1,J1), DIM_ARR(1,J1), %VAL(ADR_MAT1),         &
     &             %VAL(ADR_MAT2), %VAL(ADR_MAT3), %VAL(ADR_MAT4), IER )
           ELSE
               CALL ERR_LOG ( 1712, IUER, 'MUL_MM_SANITY', 'Unknown '// &
     &                        'function '//FUNC )
               RETURN
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1713, IUER, 'MUL_MM_SANITY', 'Function '// &
     &              FUNC//' did not pass the sanity test' )
              RETURN
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_SANITY  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MM_COMPARE ( FUNC, GEN_FUNC, OPT_FUNC, M1, N1, M2, N2, &
     &           M3, N3, MAT1, MAT2, MAT3, MAT4, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_MM_COMPARE
! *                                                                      *
! * ### 17-AUG-2002  MUL_MM_COMPARE v1.0 (c)  L. Petrov 17-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FUNC*(*)
      INTEGER*4, EXTERNAL :: GEN_FUNC, OPT_FUNC
      INTEGER*4  M1, N1, M2, N2, M3, N3, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MAT3(M3,N3), MAT4(M3,N3)
      INTEGER*4  ISEED, IS, IPOS, I1, I2, J1, J2, J3, J4, J5, J6, J7, J8, IER
      REAL*8     EPS, EPS_MAX, VAL_MIN, THR
      PARAMETER  ( VAL_MIN = 1.D-15 )
      PARAMETER  ( THR = 1.D-10 )
      REAL*8     RAN
!
      ISEED = 32137
      DO 410 J1=1,N1
         DO 420 J2=1,M1
            MAT1(J2,J1) = RAN ( ISEED )
 420     CONTINUE
 410  CONTINUE
!
      DO 430 J3=1,N2
         DO 440 J4=1,M2
            MAT2(J4,J3) = RAN ( ISEED )
 440     CONTINUE
 430  CONTINUE
      IF ( FUNC .EQ. 'ss_i' ) THEN
           IER = -1
           IS = GEN_FUNC ( M1, MAT1, M2, MAT2, M3, N3, MAT3, IER )
           IF ( IER .NE. 0 ) STOP '1. Error in MUL_MM_COMPARE: wrong dimesions'
!
           IER = -1
           IS = OPT_FUNC ( M1, MAT1, M2, MAT2, M3, N3, MAT4, IER )
           IF ( IER .NE. 0 ) STOP '2. Error in MUL_MM_COMPARE: wrong dimesions'
         ELSE IF ( FUNC .EQ. 'ss_s' ) THEN
           IER = -1
           IS = GEN_FUNC ( M1, MAT1, M2, MAT1, M3, MAT3, IER )
           IF ( IER .NE. 0 ) STOP '3. Error in MUL_MM_COMPARE: wrong dimesions'
!
           IER = -1
           IS = OPT_FUNC ( M1, MAT1, M2, MAT1, M3, MAT4, IER )
           IF ( IER .NE. 0 ) STOP '4. Error in MUL_MM_COMPARE: wrong dimesions'
         ELSE IF ( FUNC .EQ. 'si_i' .OR. FUNC .EQ. 'st_i' ) THEN
           IER = -1
           IS = GEN_FUNC ( M1, MAT1, M2, N2, MAT2, M3, N3, MAT3, IER )
           IF ( IER .NE. 0 ) STOP '5. Error in MUL_MM_COMPARE: wrong dimesions'
!!               call matview_1 ( m3, n3, mat3 ) ! %%%%%%%%%%%%
!
           IER = -1
           IS = OPT_FUNC ( M1, MAT1, M2, N2, MAT2, M3, N3, MAT4, IER )
           IF ( IER .NE. 0 ) STOP '6. Error in MUL_MM_COMPARE: wrong dimesions'
!!               call matview_1 ( m3, n3, mat4 ) ! %%%%%%%%%%%%
         ELSE IF ( FUNC .EQ. 'is_i' .OR. FUNC .EQ. 'ts_i' ) THEN
           IER = -1
           IS = GEN_FUNC ( M1, N1, MAT1, M2, MAT2, M3, N3, MAT3, IER )
           IF ( IER .NE. 0 ) STOP '7. Error in MUL_MM_COMPARE: wrong dimesions'
!
           IER = -1
           IS = OPT_FUNC ( M1, N1, MAT1, M2, MAT2, M3, N3, MAT4, IER )
           IF ( IER .NE. 0 ) STOP '8. Error in MUL_MM_COMPARE: wrong dimesions'
         ELSE IF ( FUNC .EQ. 'ii_s' .OR. FUNC .EQ. 'ti_s' .OR. &
     &             FUNC .EQ. 'it_s'                              ) THEN
           IER = -1
           IS = GEN_FUNC ( M1, N1, MAT1, M2, N2, MAT2, M3, MAT3, IER )
           IF ( IER .NE. 0 ) STOP '9. Error in MUL_MM_COMPARE: wrong dimesions'
!
           IER = -1
           IS = OPT_FUNC ( M1, N1, MAT1, M2, N2, MAT2, M3, MAT4, IER )
           IF ( IER .NE. 0 ) STOP '10. Error in MUL_MM_COMPARE: wrong dimesions'
         ELSE
           IER = -1
           IS = GEN_FUNC ( M1, N1, MAT1, M2, N2, MAT2, M3, N3, MAT3, IER )
           IF ( IER .NE. 0 ) STOP '11. Error in MUL_MM_COMPARE: wrong dimesions'
!
           IER = -1
           IS = OPT_FUNC ( M1, N1, MAT1, M2, N2, MAT2, M3, N3, MAT4, IER )
           IF ( IER .NE. 0 ) STOP '12. Error in MUL_MM_COMPARE: wrong dimesions'
      END IF
!
      EPS_MAX = -1.0D0
!
      IF ( FUNC .EQ. 'ss_s' .OR. FUNC .EQ. 'ii_s' .OR. &
     &     FUNC .EQ. 'it_s' .OR. FUNC .EQ. 'ti_s'         ) THEN
!
           DO 450 J5=1,M3
              DO 460 J6=1,J5
!
! -------------- Recasting indices J5,J6 --> I1,I2
!
                 IPOS = J6 + (J5*(J5-1))/2
                 I1 = MOD(IPOS,M3)
                 IF ( I1 .EQ. 0 ) I1=M3
                 I2 = (IPOS-I1)/M3 + 1
                 IF ( DABS(MAT4(I1,I2)) .LT. VAL_MIN ) THEN
                      EPS = DABS( (MAT4(I1,I2) - MAT3(I1,I2)) )
                   ELSE
                      EPS = DABS( (MAT4(I1,I2) - MAT3(I1,I2))/MAT4(I1,I2) )
                 END IF
                 IF ( EPS .GT. THR ) THEN
                      WRITE ( 6, 110 ) FUNC, M1, N1, M2, N2, M3, N3, J6, J5, &
     &                   MAT3(I1,I2), MAT4(I1,I2), (MAT3(I1,I2) - MAT4(I1,I2))
 110                  FORMAT ( 'Function: ',A,' M1=',I4,' N1=',I4,' M2=',I4, &
     &                     ' N2=',I4,' M3=',I4, ' N3=',I4,' i=',I4,' j=',I4/ &
     &                     ' GEN:  mat(i,j) = ',1PG22.15, &
     &                     ' OPT:  mat(i,j) = ',1PG22.15 /' DIFF =',1PG22.15 )
                     CALL ERR_LOG ( 1721, IUER, 'MUL_MM_COMPARE', 'Too '// &
     &                   'large difference while '//FUNC//' was tested' )
                         call matview_2 ( m3, mat4 ) ! %%%%%%%%%%%%
                     RETURN
                 END IF
                 IF ( EPS .GT. EPS_MAX ) THEN
                      EPS_MAX = EPS
                 END IF
 460         CONTINUE
 450      CONTINUE
        ELSE
           DO 470 J7=1,N3
              DO 480 J8=1,M3
                 IF ( DABS(MAT4(J8,J7)) .LT. VAL_MIN ) THEN
                      EPS = DABS( (MAT4(J8,J7) - MAT3(J8,J7)) )
                   ELSE
                     EPS = DABS( (MAT4(J8,J7) - MAT3(J8,J7))/MAT4(J8,J7) )
                 END IF
                 IF ( EPS .GT. THR ) THEN
                      WRITE ( 6, 110 ) FUNC, M1, N1, M2, N2, M3, N3, J8, J7, &
     &                   MAT3(J8,J7), MAT4(J8,J7), (MAT4(J8,J7) - MAT3(J8,J7))
                     CALL ERR_LOG ( 1722, IUER, 'MUL_MM_COMPARE', 'Too '// &
     &                   'large difference while '//FUNC//' was tested' )
                         call matview_2 ( m3, mat4 ) ! %%%%%%%%%%%%
                     RETURN
                 END IF
                 IF ( EPS .GT. EPS_MAX ) THEN
                     EPS_MAX = EPS
                 END IF
 480         CONTINUE
 470      CONTINUE
      END IF
      WRITE  ( 6, 120 ) FUNC, M1, N1, M2, N2, M3, N3, EPS_MAX
 120  FORMAT ( 'Function: ',A, ' Dim: ',6(I4,1X),' Eps_max: ',1PD15.7 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_COMPARE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MV_COMPARE ( FUNC, GEN_FUNC, OPT_FUNC, M1, N1, &
     &                            MAT1, VEC1, VEC2, VEC3, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_MM_COMPARE
! *                                                                      *
! * ### 17-AUG-2002  MUL_MM_COMPARE v1.1 (c)  L. Petrov 05-OCT-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FUNC*(*)
      INTEGER*4  GEN_FUNC, OPT_FUNC
      INTEGER*4  M1, N1, IUER
      REAL*8     MAT1(M1,N1), VEC1(*), VEC2(*), VEC3(*)
      INTEGER*4  ISEED, J1, J2, J3, M3, IS, IER
      REAL*8     EPS, EPS_MAX, VAL_MIN, THR
      PARAMETER  ( VAL_MIN = 1.D-15 )
      PARAMETER  ( THR = 1.D-12 )
      REAL*8     RAN
!
      ISEED = 32137
      DO 410 J1=1,N1
         DO 420 J2=1,M1
            MAT1(J2,J1) = RAN ( ISEED )
 420     CONTINUE
         VEC1(J1) = MAT1(1,J1)
 410  CONTINUE
!
      IF ( FUNC .EQ. 'iv_v' ) THEN
           IER = -1
           IS = GEN_FUNC ( M1, N1, MAT1, N1, VEC1, M1, VEC2, IER )
           IF ( IER .NE. 0 ) STOP '1. Error in MUL_MV_COMPARE: wrong dimesions'
!
           IER = -1
           IS = OPT_FUNC ( M1, N1, MAT1, N1, VEC1, M1, VEC3, IER )
           IF ( IER .NE. 0 ) STOP '2. Error in MUL_MV_COMPARE: wrong dimesions'
           M3 = M1
         ELSE IF ( FUNC .EQ. 'tv_v' ) THEN
           IER = -1
           IS = GEN_FUNC ( M1, N1, MAT1, M1, VEC1, N1, VEC2, IER )
           IF ( IER .NE. 0 ) STOP '1. Error in MUL_MV_COMPARE: wrong dimesions'
!
           IER = -1
           IS = OPT_FUNC ( M1, N1, MAT1, M1, VEC1, N1, VEC3, IER )
           IF ( IER .NE. 0 ) STOP '2. Error in MUL_MV_COMPARE: wrong dimesions'
           M3 = N1
         ELSE IF ( FUNC .EQ. 'sv_v' ) THEN
           IER = -1
           IS = GEN_FUNC ( M1, MAT1, M1, VEC1, M1, VEC2, IER )
           IF ( IER .NE. 0 ) STOP '1. Error in MUL_MV_COMPARE: wrong dimesions'
!
           IER = -1
           IS = OPT_FUNC ( M1, MAT1, M1, VEC1, M1, VEC3, IER )
           IF ( IER .NE. 0 ) STOP '2. Error in MUL_MV_COMPARE: wrong dimesions'
           M3 = M1
      END IF
!
      EPS_MAX = -1.0D0
      DO 430 J3=1,M3
         IF ( VEC2(J3) .LT. VAL_MIN ) THEN
              EPS = DABS(VEC2(J3) - VEC3(J3))
            ELSE
              EPS = DABS( (VEC2(J3) - VEC3(J3))/VEC2(J3) )
         END IF
!
         IF ( EPS .GT. THR ) THEN
              WRITE ( 6, 110 ) FUNC, M1, N1, J3, &
     &                  VEC2(J3), VEC3(J3), VEC2(J3) - VEC3(J3)
 110             FORMAT ( 'Function: ',A,' M1=',I4,' N1=',I4, ' i=',I4, &
     &                    ' GEN:  vec(i) = ',1PG22.15, &
     &                    ' OPT:  vec(i) = ',1PG22.15 /' DIFF =',1PG22.15 )
                 CALL ERR_LOG ( 1771, IUER, 'MUL_MV_COMPARE', 'Too large '// &
     &               'difference while '//FUNC//' was tested' )
                 RETURN
         END IF
         IF ( EPS .GT. EPS_MAX ) THEN
              EPS_MAX = EPS
         END IF
 430  CONTINUE
      WRITE  ( 6, 120 ) FUNC, M1, N1, EPS_MAX
 120  FORMAT ( 'Function: ',A, ' Dim: ',2(I4,1X),' Eps_max: ',1PD15.7 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MV_COMPARE  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MM_TIMER ( FUNC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_MM_TIMER
! *                                                                      *
! *  ### 17-AUG-2002  MUL_MM_TIMER  v1.1 (c) L. Petrov  20-JAN-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FUNC*(*)
      INTEGER*4  IUER
      INTEGER*4  M_DIM
      PARAMETER  ( M_DIM = 43 )
      ADDRESS__TYPE MEM_ADR, MEM_LEN, ADR_MAT1, ADR_MAT2, ADR_MAT3, ADR_MAT4
      INTEGER*4  DIM_ARR(3,M_DIM), J1, IER
      DATA       DIM_ARR / &
     &                    2048,     1, 2048, &
     &                    2048,     2, 2048, &
     &                    2048,     3, 2048, &
     &                    2048,     4, 2048, &
     &                    2048,     5, 2048, &
     &                    2048,     6, 2048, &
     &                    2048,     7, 2048, &
     &                    2048,     8, 2048, &
     &                       2,  2048,    2, &
     &                       3,  2048,    3, &
     &                       4,  2048,    4, &
     &                       5,  2048,    5, &
     &                       6,  2048,    6, &
     &                       7,  2048,    7, &
     &                       8,  2048,    8, &
     &                      32,  2048,   32, &
     &                    2048,    32, 2048, &
     &                        2,    2,    2, &
     &                        3,    3,    3, &
     &                        4,    4,    4, &
     &                        5,    5,    5, &
     &                        6,    6,    6, &
     &                        7,    7,    7, &
     &                        8,    8,    8, &
     &                       12,   12,   12, &
     &                       16,   16,   16, &
     &                       24,   24,   24, &
     &                       32,   32,   32, &
     &                       48,   48,   48, &
     &                       64,   64,   64, &
     &                       96,   96,   96, &
     &                      128,  128,  128, &
     &                      129,  129,  129, &
     &                      192,  192,  192, &
     &                      256,  256,  256, &
     &                      384,  384,  384, &
     &                      512,  512,  512, &
     &                      513,  513,  513, &
     &                      768,  768,  768, &
     &                     1024, 1024, 1024, &
     &                     1536, 1536, 1536, &
     &                     2048, 2048, 2048, &
     &                     4096, 4096, 4096  &
     &                   /
      CHARACTER  STR*32
#ifdef OPT_DP_VV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_II_I, GEN_MUL_MM_II_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IT_I, GEN_MUL_MM_IT_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TI_I, GEN_MUL_MM_TI_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TT_I, GEN_MUL_MM_TT_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SS_I, GEN_MUL_MM_SS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SS_S, GEN_MUL_MM_SS_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IS_I, GEN_MUL_MM_IS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TS_I, GEN_MUL_MM_TS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SI_I, GEN_MUL_MM_SI_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_ST_I, GEN_MUL_MM_ST_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_II_S, GEN_MUL_MM_II_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IT_S, GEN_MUL_MM_IT_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TI_S, GEN_MUL_MM_TI_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_IV_V, GEN_MUL_MV_IV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_TV_V, GEN_MUL_MV_TV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_SV_V, GEN_MUL_MV_SV_V
      ADDRESS__TYPE, EXTERNAL :: INVSL, INVSP, INVS
#else
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_II_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IT_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TI_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TT_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SS_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TS_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_SI_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_ST_I
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_II_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_IT_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MM_TI_S
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_IV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_TV_V
      ADDRESS__TYPE, EXTERNAL :: MUL_MV_SV_V
      ADDRESS__TYPE, EXTERNAL :: INVSL, INVSP, INVS
#endif
      INTEGER*4  I_LEN
!
      CALL ERR_PASS ( IUER, IER )
#ifdef ADR_64BIT
      CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 3, &
     &                8*INT8(4096)*INT8(4096), ADR_MAT1,    &
     &                8*INT8(4096)*INT8(4096), ADR_MAT2,    &
     &                8*INT8(4096)*INT8(4096), ADR_MAT3       )
#else
      CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 3, &
     &                8*4096*4096, ADR_MAT1,    &
     &                8*4096*4096, ADR_MAT2,    &
     &                8*4096*4096, ADR_MAT3       )
#endif
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
#ifdef ADR_64BIT
           CALL IINCH8 ( MEM_LEN, STR )
#else
           CALL IINCH ( MEM_LEN, STR )
#endif
           CALL ERR_LOG ( 1721, IUER, 'MUL_MM_TIMER', 'Error in an '// &
     &         'attempt to grab '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory' )
           RETURN
      END IF
!
      IF ( FUNC(1:3) .EQ. 'inv' ) THEN
           WRITE ( 6, '(A)' ) 'Func      Dimension           Speed'
         ELSE
           WRITE ( 6, '(A)' ) 'Func      Results            Int.'
      END IF
!
      DO 410 J1=1,M_DIM
         CALL ERR_PASS ( IUER, IER )
         IF ( FUNC .EQ. 'ii_i' ) THEN
              CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_II_I, MUL_MM_II_I,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),      &
     &             DIM_ARR(3,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),      &
     &             DIM_ARR(2,J1),                                    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
            ELSE IF ( FUNC .EQ. 'it_i' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_IT_I, MUL_MM_IT_I,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),      &
     &             DIM_ARR(2,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),      &
     &             DIM_ARR(2,J1),                                    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
            ELSE IF ( FUNC .EQ. 'ti_i' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_TI_I, MUL_MM_TI_I,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),      &
     &             DIM_ARR(3,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),      &
     &             DIM_ARR(1,J1),                                    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
            ELSE IF ( FUNC .EQ. 'tt_i' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_TT_I, MUL_MM_TT_I,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),      &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),      &
     &             DIM_ARR(1,J1),                                    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
            ELSE IF ( FUNC .EQ. 'iv_v' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MUL_MM_TIM ( FUNC, GEN_MUL_MV_IV_V, MUL_MV_IV_V,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),      &
     &             DIM_ARR(2,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),      &
     &             DIM_ARR(2,J1),                                    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
            ELSE IF ( FUNC .EQ. 'ss_i' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_SS_I, MUL_MM_SS_I,    &
     &                  DIM_ARR(1,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(1,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(1,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'ss_s' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_SS_S, MUL_MM_SS_S,    &
     &                  DIM_ARR(1,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(1,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(1,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'si_i' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_SI_I, MUL_MM_SI_I,    &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),        &
     &                  DIM_ARR(3,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),        &
     &                  DIM_ARR(2,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'is_i' ) THEN
              IF ( DIM_ARR(2,J1) .EQ. DIM_ARR(3,J1)  ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_IS_I, MUL_MM_IS_I,    &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),        &
     &                  DIM_ARR(3,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),        &
     &                  DIM_ARR(2,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'ts_i' ) THEN
              IF ( DIM_ARR(2,J1) .EQ. DIM_ARR(3,J1)  ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_TS_I, MUL_MM_TS_I,    &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),        &
     &                  DIM_ARR(1,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'st_i' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_ST_I, MUL_MM_ST_I,    &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),        &
     &                  DIM_ARR(2,J1), DIM_ARR(3,J1), DIM_ARR(2,J1),        &
     &                  DIM_ARR(2,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'ii_s' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)  ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_II_S, MUL_MM_II_S,    &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),        &
     &                  DIM_ARR(1,J1), DIM_ARR(1,J1), DIM_ARR(3,J1),        &
     &                  DIM_ARR(2,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'it_s' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)  ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_IT_S, MUL_MM_IT_S,    &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(2,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(2,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'ti_s' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)  ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MM_TI_S, MUL_MM_TI_S,    &
     &                  DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(2,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),        &
     &                  DIM_ARR(1,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'tv_v' ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL MUL_MM_TIM ( FUNC, GEN_MUL_MV_TV_V, MUL_MV_TV_V,  &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(3,J1),      &
     &             DIM_ARR(1,J1), DIM_ARR(2,J1), DIM_ARR(2,J1),      &
     &             DIM_ARR(1,J1),                                    &
     &             %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
            ELSE IF ( FUNC .EQ. 'sv_v' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL MUL_MM_TIM ( FUNC, GEN_MUL_MV_SV_V, MUL_MV_SV_V,    &
     &                  DIM_ARR(1,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(1,J1), DIM_ARR(1,J1), DIM_ARR(1,J1),        &
     &                  DIM_ARR(1,J1),                                      &
     &                  %VAL(ADR_MAT1), %VAL(ADR_MAT2), %VAL(ADR_MAT3), IER )
              END IF
            ELSE IF ( FUNC .EQ. 'invs' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
                   CALL INV_TIM ( 'INVS', %VAL(0), INVS, DIM_ARR(1,J1), &
     &                            %VAL(ADR_MAT1), %VAL(ADR_MAT2) )
              END IF
!@            ELSE IF ( FUNC .EQ. 'invsf' ) THEN
!@              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
!@     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
!@                   CALL INV_TIM ( 'INFSF', %VAL(0), INVSF, DIM_ARR(1,J1), &
!@     &                            %VAL(ADR_MAT1), %VAL(ADR_MAT2) )
!@              END IF
            ELSE IF ( FUNC .EQ. 'invsl' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
                   CALL INV_TIM ( 'INFSL', %VAL(0), INVSL, DIM_ARR(1,J1), &
     &                            %VAL(ADR_MAT1), %VAL(ADR_MAT2) )
              END IF
            ELSE IF ( FUNC .EQ. 'invsp' ) THEN
              IF ( DIM_ARR(1,J1) .EQ. DIM_ARR(2,J1)  .AND. &
     &             DIM_ARR(1,J1) .EQ. DIM_ARR(3,J1)        ) THEN
                   CALL INV_TIM ( 'INVSP', %VAL(0), INVSP, DIM_ARR(1,J1), &
     &                            %VAL(ADR_MAT1), %VAL(ADR_MAT2) )
              END IF
            ELSE
               CALL ERR_LOG ( 1722, IUER, 'MUL_MM_TIMER', 'Unknown '// &
     &                        'function '//FUNC )
               RETURN
         END IF
 410  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_TIMER  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MUL_MM_TIM ( FUNC, GEN_FUNC, OPT_FUNC, M1, N1, M2, N2, &
     &           M3, N3, INT_DIM, MAT1, MAT2, MAT3, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MUL_MM_TIM
! *                                                                      *
! *  ### 17-AUG-2002   MUL_MM_TIM  v1.1 (c)  L. Petrov 05-OCT-2002  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      CHARACTER  FUNC*(*)
      INTEGER*4  GEN_FUNC, OPT_FUNC
      INTEGER*4  M1, N1, M2, N2, M3, N3, INT_DIM, IUER
      REAL*8     MAT1(M1,N1), MAT2(M2,N2), MAT3(M3,N3)
      INTEGER*4  ISEED, IS, N_REP, J1, J2, J3, J4, J5, J6, J7, J8, &
     &           J9, J10, J11, J12, J13, IER
      REAL*8     DNOP, MFLOP_OPT, MFLOP_GEN
      REAL*4     TARG_R4(2), TSTART, TEND
      REAL*4     FOR_ETIME
      REAL*8     RAN
!
      ISEED = 32137
      DO 410 J1=1,N1
         DO 420 J2=1,M1
            MAT1(J2,J1) = RAN ( ISEED )
 420     CONTINUE
 410  CONTINUE
!
      DO 430 J3=1,N2
         DO 440 J4=1,M2
            MAT2(J4,J3) = RAN ( ISEED )
 440     CONTINUE
 430  CONTINUE
!
      IF ( FUNC .EQ. 'iv_v'  .OR.  FUNC .EQ. 'tv_v'  .OR. &
    &      FUNC .EQ. 'sv_v'                                ) THEN
!
           DNOP = 2.D0*DBLE(INT_DIM)*DBLE(M3)
         ELSE IF ( FUNC .EQ. 'ss_s'  .OR.  FUNC .EQ. 'ii_s'  .OR.  &
     &             FUNC .EQ. 'it_s'  .OR.  FUNC .EQ. 'ti_s'        ) THEN
           DNOP = DBLE(INT_DIM)*DBLE(M3)*DBLE(N3)
         ELSE
           DNOP = 2.D0*DBLE(INT_DIM)*DBLE(M3)*DBLE(N3)
      END IF
      IF ( INT_DIM .LT. 8 ) THEN
           N_REP = (MFLOP__PEAK*1.D6/5.0)/DNOP
         ELSE IF ( INT_DIM  .LT. 256 ) THEN
           N_REP = MFLOP__PEAK*1.D6/DNOP
         ELSE
           N_REP = (MFLOP__PEAK*1.D6/1.5)/DNOP
      END IF
!
! --- Exception
!
      IF ( FUNC .EQ. 'it_s' .AND. M1 .GE. 1024 .AND. N1 .LE. 16 ) THEN
           N_REP = N_REP/10
      END IF
!
      IF ( N_REP .LT. 1 ) N_REP = 1
!
      IER = -1
      IF ( FUNC .EQ. 'iv_v'  .OR.  FUNC .Eq. 'tv_v' ) THEN
!
! -------- Test of matrix-vector function
!
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 450 J5=1,N_REP
              IS = OPT_FUNC ( M1, N1, MAT1, N2, MAT2, M3, MAT3, IER )
 450       CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 110 ) FUNC, M1, N1, N3, (TEND-TSTART)
 110            FORMAT ( 1X,' Warning: fuction ',A,' M1=',I4,' N1=',I4, &
     &                      ' N3= ', I4, '  TIM_DIF = ',F8.3 )
                MFLOP_OPT = 1.0
              ELSE
                MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
           END IF
         ELSE IF ( FUNC .Eq. 'sv_v' ) THEN
!
! -------- Test of symmetric matrix-vector function
!
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 470 J7=1,N_REP
              IS = OPT_FUNC ( M1, MAT1, M1, MAT2, M1, MAT3, IER )
 470       CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 110 ) FUNC, M1, N1, N3, (TEND-TSTART)
                MFLOP_OPT = 1.0
              ELSE
                MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
           END IF
         ELSE IF ( FUNC .EQ. 'ss_i' ) THEN
!
! -------- Test of matrix-matrix function
!
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 480 J8=1,N_REP
              IS = OPT_FUNC ( M1, MAT1, M2, MAT2, M3, N3, MAT3, IER )
 480       CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 120 ) FUNC, M1, N1, N3, (TEND-TSTART)
 120            FORMAT ( 1X,' Warning: fuction ',A,' M1=',I4,' N1=',I4, &
     &                      ' N3=',I4,'  TIM_DIF = ',F8.3 )
                MFLOP_OPT = 1.0
              ELSE
                MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
           END IF
         ELSE IF ( FUNC .EQ. 'ss_s' ) THEN
!
! -------- Test of ss_s matrix-matrix function
!
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 490 J9=1,N_REP
              IS = OPT_FUNC ( M1, MAT1, M2, MAT2, M3, MAT3, IER )
 490       CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 120 ) FUNC, M1, N1, N3, (TEND-TSTART)
                MFLOP_OPT = 1.0
              ELSE
                MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
           END IF
         ELSE IF ( FUNC .EQ. 'si_i'  .OR.  FUNC .EQ. 'st_i' ) THEN
!
! -------- Test of symmetric matrix-matrix function
!
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 4100 J10=1,N_REP
              IS = OPT_FUNC ( M1, MAT1, M2, N2, MAT2, M3, N3, MAT3, IER )
 4100      CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 120 ) FUNC, M1, N1, N3, (TEND-TSTART)
                MFLOP_OPT = 1.0
              ELSE 
                MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
           END IF
         ELSE IF ( FUNC .EQ. 'is_i'  .OR.  FUNC .EQ. 'ts_i' ) THEN
!
! -------- Test of matrix-matrix function
!
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 4110 J11=1,N_REP
              IS = OPT_FUNC ( M1, N1, MAT1, M2, MAT2, M3, N3, MAT3, IER )
 4110      CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 120 ) FUNC, M1, N1, N3, (TEND-TSTART)
                MFLOP_OPT = 1.0
              ELSE 
                MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
           END IF
         ELSE IF ( FUNC .EQ. 'ii_s'  .OR.  FUNC .EQ. 'it_s' .OR. &
     &             FUNC .EQ. 'ti_s'                                 ) THEN
!
! -------- Test of general matrix-matrix function
!
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 4120 J12=1,N_REP
              IS = OPT_FUNC ( M1, N1, MAT1, M2, N2, MAT2, M3, MAT3, IER )
 4120      CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 120 ) FUNC, M1, N1, N3, (TEND-TSTART)
                MFLOP_OPT = 1.0
              ELSE 
                MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
           END IF
         ELSE
!
! -------- Other test of matrix-matrix function
!
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 4130 J13=1,N_REP
              IS = OPT_FUNC ( M1, N1, MAT1, M2, N2, MAT2, M3, N3, MAT3, IER )
 4130      CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 120 ) FUNC, M1, N1, N3, (TEND-TSTART)
                MFLOP_OPT = 1.0
              ELSE 
                MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
           END IF
      END IF
!
      IF ( FUNC .EQ. 'iv_v'  .OR.  FUNC .EQ. 'tv_v' ) THEN
           WRITE ( 6, 130 ) FUNC, M3, INT_DIM, MFLOP_OPT
 130       FORMAT ( A,'  dims: ',I4,', ',I4, '  speed: ', F7.1, ' Mflops' )
         ELSE
!          write ( 6, * ) ' n_rep = ',n_re
           WRITE ( 6, 140 ) FUNC, M3, N3, INT_DIM, MFLOP_OPT
 140       FORMAT ( A,'  dims: ',I4,', ',I4,', ',I4, &
     &                '  speed: ', F7.1, ' Mflops' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MUL_MM_TIM  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INV_CHECK ( FUNC_NAME, FUNC, N, MAT1, MAT2, MAT3, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  INV_CHECK
! *                                                                      *
! * ### 18-AUG-2002     INV_CHECK   v1.0 (c)  L. Petrov 18-AUG-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      CHARACTER  FUNC_NAME*(*)
      INTEGER*4  FUNC
      INTEGER*4  N, IUER
      REAL*8     MAT1(N,N), MAT2(*), MAT3(*)
      INTEGER*4  GEN_FUNC, OPT_FINC
      INTEGER*4  ISEED, IS, K, J1, J2, J3, J4, J5, J6, J7, J8, IER
      REAL*8     EPS, EPS_MAX, VAL_MIN, THR, RC
      PARAMETER  ( VAL_MIN = 1.D-15 )
      PARAMETER  ( THR = 1.D-8 )
      REAL*8     RGAUSS, DP_VV_V
      INTEGER*4  LOCS, I, J
      LOCS(I,J) = MIN(I,J) + (MAX(I,J)*(MAX(I,J)-1))/2
!
      ISEED = 425001
      DO 410 J1=1,N
         DO 420 J2=1,N
            MAT1(J2,J1) = RGAUSS ( ISEED, 1.D0 )
 420     CONTINUE
 410  CONTINUE
!
      K = 0
      DO 430 J3=1,N
         DO 440 J4=1,J3
            K=K+1
            MAT2(K) = DP_VV_V ( N, MAT1(1,J4), MAT1(1,J3) )
            MAT3(K) = MAT2(K)
 440     CONTINUE
 430  CONTINUE
!
      IER = -1
      IS = FUNC ( N, MAT2, RC, IER )
      IF ( IER .NE. 0 ) THEN
           WRITE ( 6, * ) 'N=',N,' RC=',RC,' IER=',IER
           CALL ERR_LOG ( 1741, IUER, 'INV_CHECK', 'Error in an attempt '// &
     &         'to invert matrix during testing '//FUNC_NAME )
           RETURN
      END IF
!
      EPS_MAX = -10.0D0
      K = 1
      DO 460 J6=1,N
         DO 470 J7=1,J6
            EPS = 0.0D0
            DO 480 J8=1,N
               EPS = EPS + MAT2(LOCS(J8,J6))*MAT3(LOCS(J8,J7))
 480        CONTINUE
            IF ( J6 .EQ. J7 ) EPS = EPS - 1.D0
            IF ( DABS(EPS) .GT. THR ) THEN
                 WRITE ( 6, 110 ) FUNC_NAME, N, J6, J7, EPS, RC
 110             FORMAT ( 'Function: ',A,' N=',I4,' I1=',I4,' I2=',I4, &
     &                    ' EPS=',1PG22.15/' RC=',1PG22.15 )
                 CALL ERR_LOG ( 1742, IUER, 'INV_CHECK', 'Too large '// &
     &               'residual while function '//FUNC_NAME//' was tested' )
                 RETURN
            END IF
            IF ( DABS(EPS) .GT. EPS_MAX ) THEN
                 EPS_MAX = DABS(EPS)
            END IF
            K = K + 1
 470     CONTINUE
 460  CONTINUE
      WRITE  ( 6, 120 ) FUNC_NAME, N, EPS_MAX, RC
 120  FORMAT ( 'Function: ',A, ' Dim: ',I4,' Eps_max: ',1PD15.7, &
     &         '  RC =',1PD10.3 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  INV_CHECK  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INV_TIM ( FUNC, GEN_FUNC, OPT_FUNC, N, MAT1, MAT2 )
! ************************************************************************
! *                                                                      *
! *   Routine  INV_TIM
! *                                                                      *
! *  ### 17-AUG-2002    INV_TIM    v1.0 (c)  L. Petrov  07-OCT-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'matvec.i'
      CHARACTER  FUNC*(*)
      INTEGER*4, EXTERNAL :: GEN_FUNC, OPT_FUNC
      INTEGER*4  N, IUER
      REAL*8     MAT1(*), MAT2(N,N)
      INTEGER*4  ISEED, IS, N_REP, K, J1, J2, J3, J4, J5, J6, J7, IER
      REAL*8     DNOP, MFLOP_OPT, MFLOP_GEN, RC
      REAL*4     TARG_R4(2), TSTART, TEND
      REAL*4,    EXTERNAL :: FOR_ETIME
      REAL*8,    EXTERNAL :: RGAUSS
!
      ISEED = 2013794381
      DO 410 J1=1,N
         DO 420 J2=1,N
            MAT2(J2,J1) = RGAUSS ( ISEED, 1.D0 )
 420     CONTINUE
 410  CONTINUE
!
      K = 1
      DO 430 J3=1,N
         DO 440 J4=1,J3
            MAT1(K) = 0.0D0
            DO 450 J5=1,N
               MAT1(K) = MAT1(K) + MAT2(J5,J3)*MAT2(J5,J4)
 450        CONTINUE
            K=K+1
 440     CONTINUE
 430  CONTINUE
!
      DNOP = DBLE(N)**3
      IF ( N .LT. 8 ) THEN
           N_REP = (MFLOP__PEAK*1.D6/20.0D0)/DNOP
         ELSE IF ( N .LT. 256 ) THEN
           N_REP = (MFLOP__PEAK*1.D6/2.0D0)/DNOP
         ELSE
           N_REP = (MFLOP__PEAK*1.D6/4.0D0)/DNOP
      END IF
      IF ( N_REP .LT. 1 ) N_REP = 1
!
! --- Test of the optimized function
!
      IF ( LOC(OPT_FUNC) .NE. 0 ) THEN
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 460 J6=1,N_REP
              IER = -1
              IS = OPT_FUNC ( N, MAT1, RC, IER )
              IF ( IER .NE. 0 ) GOTO 810
 460       CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 110 ) FUNC, N, (TEND-TSTART)
 110            FORMAT ( 1X,' Warning: fuction ',A,' M=',I4, &
     &             '  TIM_DIF = ',F8.3 )
           END IF
           MFLOP_OPT = N_REP*DNOP*1.D-6/(TEND-TSTART)
!
           WRITE ( 6, 120 ) FUNC, N, MFLOP_OPT
 120       FORMAT ( A,'  dims: ',I4, '  speed: ', F7.1, ' Mflops' )
         ELSE
           TSTART = FOR_ETIME ( TARG_R4 )  ! start timing
           DO 470 J7=1,N_REP
              IER = -1
              IS = GEN_FUNC ( N, MAT1, RC, IER )
              IF ( IER .NE. 0 ) GOTO 810
 470       CONTINUE
           TEND = FOR_ETIME ( TARG_R4 )    ! end timing
           IF ( (TEND-TSTART) .LT. 0.2 ) THEN
                WRITE ( 6, 110 ) FUNC, N, (TEND-TSTART)
           END IF
           MFLOP_GEN = N_REP*DNOP*1.D-6/(TEND-TSTART)
           WRITE ( 6, 130 ) FUNC, N, MFLOP_GEN
 130       FORMAT ( A,'  dims: ',I4, '  gen_speed: ',F7.1,' Mflops  ' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
 810  CONTINUE
      CALL ERR_LOG ( 1731, IUER, 'INV_TIM', 'Error in test' )
      RETURN
      END  !#!  INV_TIM  #!#
