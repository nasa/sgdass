      FUNCTION   IS_R4_INF ( R4_VAL )
! ************************************************************************
! *                                                                      *
! *   Logical function  IS_R4_INF  examines a number in REAL*4 type      *
! *   architecture and answers the question whether the argument         *
! *   R4 is "+Infinity" or "-Infinity".                                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      R4_VAL ( REAL*8    ) -- Examined value.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <IS_R4_INF> ( LOGICAL*4 ) -- TRUE if R8 is -INF or +INF.             *
! *                                                                      *
! *  ###  22-OCT-2009  IS_R4_INF   v1.0  (c)  L. Petrov  22-OCT-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      LOGICAL*4  IS_R4_INF   
      REAL*4     R4_VAL
      INTEGER*4  I4_VAL
      INTEGER*4  I4_PLUS_INF, I4_MINUS_INF
!!      PARAMETER  ( I4_PLUS_INF  = '7F800000'Z )
!!      PARAMETER  ( I4_MINUS_INF = 'FF800000'Z )
!!      PARAMETER  ( I4_PLUS_INF  = '01111111100000000000000000000000'B )
!!      PARAMETER  ( I4_MINUS_INF = '11111111100000000000000000000000'B )
      PARAMETER  ( I4_PLUS_INF  = 2139095040 )
      PARAMETER  ( I4_MINUS_INF = -8388608   )
      CALL MEMCPY ( I4_VAL, R4_VAL, %VAL(4) )
      IF ( I4_VAL == I4_PLUS_INF .OR. I4_VAL == I4_MINUS_INF ) THEN
           IS_R4_INF = .TRUE.
         ELSE 
           IS_R4_INF = .FALSE.
      END IF
      RETURN
      END  FUNCTION  IS_R4_INF !#!  
!
! ------------------------------------------------------------------------
!
      FUNCTION   IS_R8_INF ( R8_VAL )
! ************************************************************************
! *                                                                      *
! *   Logical function  IS_R8_INF  examines a number in REAL*8 type      *
! *   architecture and answers the question whether the argument         *
! *   R4 is "+Infinity" or "-Infinity".                                  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *      R8_VAL ( REAL*8    ) -- Examined value.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <IS_R8_INF> ( LOGICAL*4 ) -- TRUE if R8 is -INF or +INF.             *
! *                                                                      *
! *  ###  22-OCT-2009  IS_R8_INF   v1.0  (c)  L. Petrov  22-OCT-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      LOGICAL*4  IS_R8_INF   
      REAL*8     R8_VAL
      INTEGER*8  I8_VAL
      INTEGER*8  I8_PLUS_INF, I8_MINUS_INF
!!      PARAMETER  ( I8_PLUS_INF  = '7FF0000000000000'Z )
!!      PARAMETER  ( I8_MINUS_INF = 'FFF0000000000000'Z )
!!      PARAMETER  ( I8_PLUS_INF  = '0111111111110000000000000000000000000000000000000000000000000000'B )
!!      PARAMETER  ( I8_MINUS_INF = '1111111111110000000000000000000000000000000000000000000000000000'B )
      PARAMETER  ( I8_PLUS_INF  =  9218868437227405312_8 )
      PARAMETER  ( I8_MINUS_INF = -4503599627370496_8 )
      CALL MEMCPY ( I8_VAL, R8_VAL, %VAL(8) )
      IF ( I8_VAL == I8_PLUS_INF .OR. I8_VAL == I8_MINUS_INF ) THEN
           IS_R8_INF = .TRUE.
         ELSE 
           IS_R8_INF = .FALSE.
      END IF
      RETURN
      END  FUNCTION  IS_R8_INF !#!  
