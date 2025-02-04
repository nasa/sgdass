      SUBROUTINE ENDIAN_PAIR_CNV_I2 ( IVAL_NEW_I2, IVAL_OLD_I2 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_PAIR_CNV_I2 converts BIG_ENDIAN <--> LITTLE_ENDIAN  *
! *   representation of a binary number of INTEGER*2 type.               *
! *                                                                      *
! * ## 24-DEC-2002 ENDIAN_PAIR_CNV_I2 v1.1 (c) L. Petrov 04-MAY-2004  ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*2  IVAL_NEW_I2, IVAL_OLD_I2
      CHARACTER  BUF(2)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 2, IVAL_OLD_I2, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 2, IVAL_OLD_I2, %REF(BUF) )
#endif
      SWAP = BUF(1) 
      BUF(1) = BUF(2)
      BUF(2) = SWAP 
#ifdef SUN
      CALL LIB$MOVC3 ( 2, %VAL(LOC__SUN$$_STR(BUF)), IVAL_NEW_I2 )
#else
      CALL LIB$MOVC3 ( 2, %REF(BUF), IVAL_NEW_I2 )
#endif
!
      RETURN
      END  !#!  ENDIAN_PAIR_CNV_I2   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_PAIR_CNV_I4 ( IVAL_NEW_I4, IVAL_OLD_I4  )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_PAIR_CNV_I4 converts BIG_ENDIAN <--> LITTLE_ENDIAN  *
! *   representation of a binary number of INTEGER*4 type.               *
! *                                                                      *
! * ## 24-DEC-2002  ENDIAN_PAIR_CNV_I4 v1.1 (c) L. Petrov 04-MAY-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  IVAL_NEW_I4, IVAL_OLD_I4
      CHARACTER  BUF(4)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 4, IVAL_OLD_I4, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 4, IVAL_OLD_I4, %REF(BUF) )
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
      CALL LIB$MOVC3 ( 4, %VAL(LOC__SUN$$_STR(BUF)), IVAL_NEW_I4 )
#else
      CALL LIB$MOVC3 ( 4, %REF(BUF), IVAL_NEW_I4 )
#endif
!
      RETURN
      END  !#!  ENDIAN_PAIR_CNV_I4   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_PAIR_CNV_R4 ( RVAL_NEW_R4, RVAL_OLD_R4 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_PAIR_CNV_R4 converts BIG_ENDIAN <--> LITTLE_ENDIAN  *
! *   representation of a binary number of REAL*4 type.                  *
! *                                                                      *
! * ## 24-DEC-2002  ENDIAN_PAIR_CNV_R4 v1.1 (c) L. Petrov 04-MAY-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*4     RVAL_NEW_R4, RVAL_OLD_R4 
      CHARACTER  BUF(4)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 4, RVAL_OLD_R4, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 4, RVAL_OLD_R4, %REF(BUF) )
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
      CALL LIB$MOVC3 ( 4, %VAL(LOC__SUN$$_STR(BUF)), RVAL_NEW_R4 )
#else
      CALL LIB$MOVC3 ( 4, %REF(BUF), RVAL_NEW_R4 )
#endif
      RETURN
      END  !#!  ENDIAN_PAIR_CNV_R4   #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_PAIR_CNV_R8 ( RVAL_NEW_R8, RVAL_OLD_R8 )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_PAIR_CNV_R8 converts BIG_ENDIAN <--> LITTLE_ENDIAN  *
! *   representation of a binary number of REAL*8 type.                  *
! *                                                                      *
! * ## 24-DEC-2002 ENDIAN_PAIR_CNV_R8  v1.1 (c) L. Petrov 04-MAY-2004 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     RVAL_NEW_R8, RVAL_OLD_R8
      CHARACTER  BUF(8)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 8, RVAL_OLD_R8, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 8, RVAL_OLD_R8, %REF(BUF) )
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
      CALL LIB$MOVC3 ( 8, %VAL(LOC__SUN$$_STR(BUF)), RVAL_NEW_R8 )
#else
      CALL LIB$MOVC3 ( 8, %REF(BUF), RVAL_NEW_R8 )
#endif
!
      RETURN
      END  !#!  ENDIAN_PAIR_CNV_R8  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ENDIAN_PAIR_CNV_I8 ( IVAL_NEW_I8, IVAL_OLD_I8  )
! ************************************************************************
! *                                                                      *
! *   Routine ENDIAN_PAIR_CNV_I8 converts BIG_ENDIAN <--> LITTLE_ENDIAN  *
! *   representation of a binary number of INTEGER*8 type.               *
! *                                                                      *
! * ## 14-OCT-2005 ENDIAN_PAIR_CNV_I8  v1.1 (c) L. Petrov 14-OCT-2005 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      REAL*8     IVAL_OLD_I8, IVAL_NEW_I8
      CHARACTER  BUF(8)*1, SWAP*1
      INTEGER*4, EXTERNAL :: LOC__SUN$$_STR
!
#ifdef SUN
      CALL LIB$MOVC3 ( 8, IVAL_OLD_I8, %VAL(LOC__SUN$$_STR(BUF)) )
#else
      CALL LIB$MOVC3 ( 8, IVAL_OLD_I8, %REF(BUF) )
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
      CALL LIB$MOVC3 ( 8, %VAL(LOC__SUN$$_STR(BUF)), IVAL_NEW_I8 )
#else
      CALL LIB$MOVC3 ( 8, %REF(BUF), IVAL_NEW_I8 )
#endif
!
      RETURN
      END  !#!  ENDIAN_PAIR_CNV_I8  #!#
