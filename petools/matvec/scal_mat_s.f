      SUBROUTINE SCAL_MAT_S ( N, MAT_S, SCAL_VEC, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SCAL_MAT_S performs matrix scaling by vector scal_vec      *
! *   MAT_S(i,j) = MAT_S(i,j)/(scal_vec(i)*scal_vec(j)), where vector    *
! *   scal_vec is chosen in such a way that the main diagonal of MAT_S   *
! *   is 1.0 .                                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- matrix dimension.                          *
! *                                                                      *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * SCAL_VEC ( REAL*8    ) -- Scaling vector. Dimension: N.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    MAT_S ( REAL*8    ) -- symmetric matrix in the upper triangular   *
! *                            representation.                           *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 1975-2026 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 20-MAR-2003   SCAL_MAT_S  v1.0 (d)  L. Petrov  21-JUL-2026 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, IUER
      REAL*8     MAT_S(*), SCAL_VEC(N)
      REAL*8     MAX_EL, MIN_EL
      CHARACTER  STR*32, STR1*32
      PARAMETER  ( MAX_EL = 1.0D64  )
      PARAMETER  ( MIN_EL =12.0D-64 )
      INTEGER*4  J1, J2, J3, ID
!
      ID = 0
      DO 410 J1=1,N
         ID = ID + J1
         IF ( MAT_S(ID) > MAX_EL ) THEN
              CALL  INCH ( J1, STR )
              WRITE ( UNIT=STR1, FMT='(1PD22.15)' ) MAT_S(ID) 
              CALL ERR_LOG ( 1211, IUER, 'SCAL_MAT_S', 'Too large diagonal '// &
     &            'element at row/columns '//TRIM(STR)//' -- '//STR1 )
              RETURN 
            ELSE IF ( MAT_S(ID) < MIN_EL ) THEN
              CALL  INCH ( J1, STR )
              WRITE ( UNIT=STR1, FMT='(1PD22.15)' ) MAT_S(ID) 
              CALL ERR_LOG ( 1212, IUER, 'SCAL_MAT_S', 'Too small diagonal '// &
     &            'element at row/columng '//TRIM(STR)//' -- '//STR1 )
              RETURN 
         END IF
!
! ------ Get the element of scaling vector: it is just the square root of diagonal.
!
         SCAL_VEC(J1) = DSQRT ( MAT_S(ID) )
 410  CONTINUE 
!
      ID = 1
      MAT_S(ID) = 1.0D0
      DO 420 J2=2,N
         DO 430 J3=1,J2
            ID = ID + 1
            MAT_S(ID) = MAT_S(ID)/SCAL_VEC(J2)/SCAL_VEC(J3)
 430     CONTINUE 
 420  CONTINUE 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SCAL_MAT_S  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE UNSCAL_MAT_S ( N, MAT_S, SCAL_VEC )
! ************************************************************************
! *                                                                      *
! *   Routine UNSCAL_MAT_S performs matrix scaling by the unput vector   *
! *   scal_vec: MAT_S(i,j) = MAT_S(i,j)/(scal_vec(i)*scal_vec(j))        *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        N ( INTEGER*4 ) -- matrix dimension.                          *
! * SCAL_VEC ( REAL*8    ) -- Scaling vector. Dimension: N.              *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    MAT_S ( REAL*8    ) -- symmetric matrix in the upper triangular   *
! *                            representation.                           *
! *          IUER ( INTEGER*4, OPT ) -- Universal error handler.         *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 1975-2026 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 20-MAR-2003   SCAL_MAT_S  v1.0 (d)  L. Petrov  21-JUL-2026 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N
      REAL*8     MAT_S(*), SCAL_VEC(N)
      INTEGER*4  J1, J2, ID
!
      ID = 0
      DO 410 J1=1,N
         DO 420 J2=1,J1
            ID = ID + 1
            MAT_S(ID) = MAT_S(ID)/SCAL_VEC(J1)/SCAL_VEC(J2)
 420     CONTINUE 
 410  CONTINUE 
      RETURN
      END  SUBROUTINE  UNSCAL_MAT_S  !#!  
