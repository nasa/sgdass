      FUNCTION  IS_R8_NAN ( R8 )
! ************************************************************************
! *                                                                      *
! *   Logical function examines a number in REAL*8 type for HP-725       *
! *   architecture and answers the question does the argument R8 is      *
! *   "not-a-number" or denormalized number. IS_R8_NAN returns NO in     *
! *   examining -INF or +INF.                                            *
! *                                                                      *
! *   Float operastion are not determined for NAN of denoramlized        *
! *   numbers.                                                           *
! *                                                                      *
! *   References: HP FORTRAN/9000 Programmer's Reference,p. 4-12, 4-16.  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *          R8 ( REAL*8    ) -- Examined value.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <IS_R8_NAN> ( LOGICAL*4 ) -- TRUE if R8 is not-a-number or           *
! *                              denormalized number. FALSE is R8 is a   *
! *                              valid number of -INF or +INF.           *
! *                                                                      *
! *  ###  24-DEC-1998  IS_R8_NAN   v3.0  (c)  L. Petrov  04-JAN-2009 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT  NONE
      LOGICAL*4 IS_R8_NAN
      REAL*8    R8
      INTEGER*8 I8, MANT_I8, EXP_I8, SIGN_I8
!
      CALL LIB$MOVC3  ( 8, R8, I8 )
#ifdef BIG_ENDIAN
      CALL ENDIAN_CNV_I8 ( I8 )
#endif
      MANT_I8 = 0 
      EXP_I8 = 0 
      SIGN_I8 = 0 
      CALL MVBITS ( I8, 63,  1, SIGN_I8, 0 )  
      CALL MVBITS ( I8, 52, 11, EXP_I8,  0 )  
      CALL MVBITS ( I8,  0, 52, MANT_I8, 0 )  
!
      IF ( EXP_I8 == 2047  .AND.  MANT_I8 .NE. 0 ) THEN
           IS_R8_NAN = .TRUE.
        ELSE
           IS_R8_NAN = .FALSE.
      END IF
!
      RETURN
      END  FUNCTION  IS_R8_NAN  !#!#
!
! ------------------------------------------------------------------------
!
      FUNCTION  IS_R4_NAN ( R4 )
! ************************************************************************
! *                                                                      *
! *   Logical function examines a number in REAL*4 type for HP-725       *
! *   architecture and answers the question does the argument R4 is      *
! *   "not-a-number" or denormalized number. IS_R4_NAN returns NO in     *
! *   examining -INF or +INF.                                            *
! *                                                                      *
! *   Float operation are not determined for NAN of denoramlized         *
! *   numbers.                                                           *
! *                                                                      *
! *   References: HP FORTRAN/9000 Programmer's Reference,p. 4-12, 4-16.  *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *          R4 ( REAL*4    ) -- Examined value.                         *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! * <IS_R4_NAN> ( LOGICAL*4 ) -- TRUE if R4 is not-a-number or           *
! *                              denormalized number. FALSE is R4 is a   *
! *                              valid number or -INF or +INF.           *
! *                                                                      *
! *  ### 24-DEC-1998   IS_R4_NAN   v3.0  (c)  L. Petrov 04-JAN-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT  NONE
      LOGICAL*4 IS_R4_NAN
      REAL*8    R4
      INTEGER*4 I4, MANT_I4, EXP_I4, SIGN_I4
!
      CALL LIB$MOVC3  ( 4, R4, I4 )
#ifdef BIG_ENDIAN
      CALL ENDIAN_CNV_I4 ( I4 )
#endif
      MANT_I4 = 0 
      EXP_I4  = 0 
      SIGN_I4 = 0 
      CALL MVBITS ( I4, 31,  1, SIGN_I4, 0 )  
      CALL MVBITS ( I4, 23,  8,  EXP_I4, 0 )  
      CALL MVBITS ( I4,  0, 23, MANT_I4, 0 )  
!
      IF ( EXP_I4 == 255  .AND. MANT_I4 .NE. 0 ) THEN
           IS_R4_NAN = .TRUE.
         ELSE 
           IS_R4_NAN = .FALSE.
      END IF
!
      RETURN
      END  FUNCTION  IS_R4_NAN  !#!#
