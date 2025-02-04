      SUBROUTINE ENDIAN_CNV_I2 ( IVAL_I2 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_I2 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of a binary number of INTEGER*2 type.               *
! *                                                                      *
! * ### 24-DEC-2002   ENDIAN_CNV_I2  v1.1 (c) L. Petrov  04-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*2  IVAL_I2
      CHARACTER  BUF(2)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 2, IVAL_I2, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 2, IVAL_I2, %REF(BUF) )
#endif
      SWAP = BUF(1)
      BUF(1) = BUF(2)
      BUF(2) = SWAP
#ifdef SUN
      CALL LIB$MOVC3 ( 2, %VAL(LOC__SUN$$_STR(BUF)), IVAL_I2 )
#else
      CALL LIB$MOVC3 ( 2, %REF(BUF), IVAL_I2 )
#endif
!
      RETURN
      END  !#!  ENDIAN_CNV_I2   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_CNV_I4 ( IVAL_I4 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_I4 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of a binary number of INTEGER*4 type.               *
! *                                                                      *
! * ### 24-DEC-2002   ENDIAN_CNV_I4  v1.1 (c) L. Petrov  04-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IVAL_I4
      CHARACTER  BUF(4)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 4, IVAL_I4, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 4, IVAL_I4, %REF(BUF) )
#endif
!
      SWAP = BUF(1)
      BUF(1) = BUF(4)
      BUF(4) = SWAP
!
      SWAP = BUF(2)
      BUF(2) = BUF(3)
      BUF(3) = SWAP
!
#ifdef SUN
      CALL LIB$MOVC3 ( 4, %VAL(LOC__SUN$$_STR(BUF)), IVAL_I4 )
#else
      CALL LIB$MOVC3 ( 4, %REF(BUF), IVAL_I4 )
#endif
!
      RETURN
      END  !#!  ENDIAN_CNV_I4   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_CNV_R4 ( RVAL_R4 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_R4 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of a binary number of REAL*4 type.                  *
! *                                                                      *
! * ### 24-DEC-2002   ENDIAN_CNV_R4  v1.1 (c) L. Petrov  04-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*4     RVAL_R4
      CHARACTER  BUF(4)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 4, RVAL_R4, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 4, RVAL_R4, %REF(BUF) )
#endif
!
      SWAP = BUF(1)
      BUF(1) = BUF(4)
      BUF(4) = SWAP
!
      SWAP = BUF(2)
      BUF(2) = BUF(3)
      BUF(3) = SWAP
!
#ifdef SUN
      CALL LIB$MOVC3 ( 4, %VAL(LOC__SUN$$_STR(BUF)), RVAL_R4 )
#else
      CALL LIB$MOVC3 ( 4, %REF(BUF), RVAL_R4 )
#endif
      RETURN
      END  !#!  ENDIAN_CNV_R4   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_CNV_R8 ( RVAL_R8 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_R8 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of a binary number of REAL*8 type.                  *
! *                                                                      *
! * ### 24-DEC-2002   ENDIAN_CNV_R8  v1.1 (c) L. Petrov  04-MAY-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     RVAL_R8
      CHARACTER  BUF(8)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 8, RVAL_R8, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 8, RVAL_R8, %REF(BUF) )
#endif
!
      SWAP = BUF(1)
      BUF(1) = BUF(8)
      BUF(8) = SWAP
!
      SWAP = BUF(2)
      BUF(2) = BUF(7)
      BUF(7) = SWAP
!
      SWAP = BUF(3)
      BUF(3) = BUF(6)
      BUF(6) = SWAP
!
      SWAP = BUF(4)
      BUF(4) = BUF(5)
      BUF(5) = SWAP
!
#ifdef SUN
      CALL LIB$MOVC3 ( 8, %VAL(LOC__SUN$$_STR(BUF)), RVAL_R8 )
#else
      CALL LIB$MOVC3 ( 8, %REF(BUF), RVAL_R8 )
#endif
!
      RETURN
      END  !#!  ENDIAN_CNV_R8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_CNV_I8 ( IVAL_I8 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_I8 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of a binary number of INTEGER*8 type.               *
! *                                                                      *
! * ### 14-OCT-2005   ENDIAN_CNV_I8  v1.1 (c) L. Petrov  14-OCT-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      REAL*8     IVAL_I8
      CHARACTER  BUF(8)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 8, IVAL_I8, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 8, IVAL_I8, %REF(BUF) )
#endif
!
      SWAP = BUF(1)
      BUF(1) = BUF(8)
      BUF(8) = SWAP
!
      SWAP = BUF(2)
      BUF(2) = BUF(7)
      BUF(7) = SWAP
!
      SWAP = BUF(3)
      BUF(3) = BUF(6)
      BUF(6) = SWAP
!
      SWAP = BUF(4)
      BUF(4) = BUF(5)
      BUF(5) = SWAP
!
#ifdef SUN
      CALL LIB$MOVC3 ( 8, %VAL(LOC__SUN$$_STR(BUF)), IVAL_I8 )
#else
      CALL LIB$MOVC3 ( 8, %REF(BUF), IVAL_I8 )
#endif
!
      RETURN
      END  !#!  ENDIAN_CNV_I8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_ARRAY_CNV_I2 ( IDIM, ARRAY_I2 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_I2 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of an array of binary number of INTEGER*2 type.     *
! *                                                                      *
! * ## 31-DEC-2003 ENDIAN_ARRAY_CNV_I2 v1.0 (c) L. Petrov 31-DEC-2003 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDIM, J1
      INTEGER*2  ARRAY_I2(IDIM)
      DO 410 J1=1,IDIM
         CALL ENDIAN_CNV_I2 ( ARRAY_I2(J1) )
 410  CONTINUE
      RETURN
      END  !#!  ENDIAN_ARRAY_CNV_I2  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_ARRAY_CNV_I4 ( IDIM, ARRAY_I4 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_I4 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of an array of binary numbers of INTEGER*4 type.    *
! *                                                                      *
! * ## 31-DEC-2003 ENDIAN_ARRAY_CNV_I4 v1.0 (c) L. Petrov 31-DEC-2003 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDIM, J1
      INTEGER*4  ARRAY_I4(IDIM)
      DO 410 J1=1,IDIM
         CALL ENDIAN_CNV_I4 ( ARRAY_I4(J1) )
 410  CONTINUE
      RETURN
      END  !#!  ENDIAN_ARRAY_CNV_I4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_ARRAY_CNV_R4 ( IDIM, ARRAY_R4 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_R4 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of an array of binary numbers of REAL*4 type.       *
! *                                                                      *
! * ## 31-DEC-2003 ENDIAN_ARRAY_CNV_R4 v1.0 (c) L. Petrov 31-DEC-2003 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDIM, J1
      REAL*4     ARRAY_R4(IDIM)
      DO 410 J1=1,IDIM
         CALL ENDIAN_CNV_R4 ( ARRAY_R4(J1) )
 410  CONTINUE
      RETURN
      END  !#!  ENDIAN_ARRAY_CNV_R4  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_ARRAY_CNV_R8 ( IDIM, ARRAY_R8 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_CNV_R8 converts BIG_ENDIAN <--> LITTLE_ENDIAN       *
! *   representation of an array of binary numbers of REAL*8 type.       *
! *                                                                      *
! * ## 31-DEC-2003 ENDIAN_ARRAY_CNV_R8 v1.0 (c) L. Petrov 31-DEC-2003 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IDIM, J1
      REAL*8     ARRAY_R8(IDIM)
      DO 410 J1=1,IDIM
         CALL ENDIAN_CNV_R8 ( ARRAY_R8(J1) )
 410  CONTINUE
      RETURN
      END  !#!  ENDIAN_ARRAY_CNV_R8  #!#
