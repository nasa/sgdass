      SUBROUTINE TRANSFORM_HARPOS ( MODE, L_HSP, HSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine TRANSFORM_HARPOS transforms the vector of estimates and    *
! *   their coavariance matrix. The nature of transformation is          *
! *   determined by paramater MODE.                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    MODE ( INTEGER*4 ) -- Mode of work. Supported modes:              *
! *                          1 -- only rotate coordinate vector from     *
! *                               XYZ crust-fixed coordainte system      *
! *                               to UEN local topocentric coordainte    *
! *                               system.                                *
! *   L_HSP ( INTEGER*4 ) -- The number of harmonics for which position  *
! *                          variations were estimated.                  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   L_HSP ( INTEGER*4 ) -- The number of harmonics for which position  *
! *                          variations were estimated.                  *
! *     HSP ( RECORD    ) -- Array of objects which keep information     *
! *                          about estimates of harmonics site position  *
! *                          variations and their covariance matrix.     *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! * ### 23-MAR-2005 TRANSFORM_HARPOS v1.0 (c) L. Petrov 23-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'hsp.i'
      INTEGER*4  MODE, L_HSP, IUER
      TYPE     ( HSP__TYPE ) :: HSP(L_HSP)
      INTEGER*4  M_PAR, M_PAR2
      PARAMETER  ( M_PAR = 6*MAX_STA, M_PAR2 = (M_PAR*(M_PAR+1))/2 )
      REAL*8     TRS_TO_UEN(3,3)
 
      REAL*8,    ALLOCATABLE :: ROT_MAT(:,:), TMP_MAT(:,:), TMP_VEC(:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, L_PAR, IER
!
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
      REAL*8,    EXTERNAL :: ATAN_CS
!
      DO 410 J1=1,L_HSP
         L_PAR = HSP(J1)%L_STA*6
         ALLOCATE ( ROT_MAT( L_PAR, L_PAR ) )
         ALLOCATE ( TMP_MAT( L_PAR, L_PAR ) )
         ALLOCATE ( TMP_VEC( L_PAR ) )
!
         IF ( MODE .EQ. 1 ) THEN
              CALL NOUT_R8 ( L_PAR*L_PAR, ROT_MAT )
              DO 420 J2=1,HSP(J1)%L_STA
!
                 CALL MAKE_XYZ_TO_UEN  ( HSP(J1)%COO(1,J2), TRS_TO_UEN )
!
! -------------- Build the rotation matrix for EST_VEC vector. 
! -------------- This matrix consists of 3x3 diagonal blocks for rotation 
! -------------- of each triad of EST_VEC
!
                 DO 430 J3=1,3
                    DO 440 J4=1,3
                       ROT_MAT((J2-1)*6+J4,  (J2-1)*6+J3)   = TRS_TO_UEN(J4,J3)
                       ROT_MAT((J2-1)*6+3+J4,(J2-1)*6+3+J3) = TRS_TO_UEN(J4,J3)
 440                CONTINUE 
 430             CONTINUE 
 420          CONTINUE 
!
! ----------- Applying transfromation to the covariance matrix ...
!
              IER = -1
              CALL MUL_MM_ST_I ( L_PAR, HSP(J1)%COV, L_PAR, L_PAR, ROT_MAT, &
     &                           L_PAR, L_PAR, TMP_MAT, IER )
              IER = -1
              CALL MUL_MM_II_S ( L_PAR, L_PAR, ROT_MAT, L_PAR, L_PAR, TMP_MAT, &
     &                           L_PAR, HSP(J1)%COV, IER )
!
! ----------- ... and the covariance vector
!
              IER = -1
              CALL MUL_MV_IV_V ( L_PAR, L_PAR, ROT_MAT, L_PAR, HSP(J1)%EST, &
     &                           L_PAR, TMP_VEC, IER )
!
              CALL COPY_R8 ( L_PAR, TMP_VEC, HSP(J1)%EST )
!
              DEALLOCATE ( ROT_MAT )
              DEALLOCATE ( TMP_MAT )
              DEALLOCATE ( TMP_VEC )
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  TRANSFORM_HARPOS 
