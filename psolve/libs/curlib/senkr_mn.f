      SUBROUTINE SENKR_MN ( IX, IY, ICHAR4 )
      IMPLICIT   NONE
      INTEGER*4  IX, IY, ICHAR4
!
      INCLUDE 'curlib.i'
      LOGICAL*4  IS_CURLIB_ON
      CHARACTER  CHR4*4, CHR*1
      INTEGER*4  LUN
!
      LUN = 5
      IF ( IS_CURLIB_ON() ) THEN
           CALL SENKR_NB ( IX, IY, ICHAR4 )
         ELSE
           READ ( LUN, '(A)' ) CHR
           CHR4= CHAR(0)//CHAR(0)//CHAR(0)//CHR
           READ ( UNIT=CHR4, FMT='(A)' ) ICHAR4
           IX = 0
           IY = 0
      END IF
#ifdef LITTLE_ENDIAN
!
! --- Change byte order for Little endian architecture
!
      CALL ENDIAN_CNV_I4 ( ICHAR4 )
#endif
      RETURN
      END  !#!  SENKR_MN  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SENKRS_MN ( IX, IY, ICHAR4 )
! ************************************************************************
! *                                                                      *
! *   Routine SENKRS_MN calls curses and returns a character.            *
! *                                                                      *
! *   Value ICHAR dependes on architecture choice: LITTLE_ENDIAN or      *
! *   BIG_ENDIAN.                                                        *
! *                                                                      *
! *   Key PG_UP returns 18 in the 4-th byte.                             *
! *   Key PG_DN returns 19 in the 4-th byte.                             *
! *                                                                      *
! *  ### ??-???-????   SENKRS_MN   v2.0 (c)  L. Petrov  26-OCT-2004 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IX, IY, ICHAR4
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'curlib.i'
      LOGICAL*4  IS_CURLIB_ON
      INTEGER*4  ICHAR_PG_DN, JCHAR_PG_DN, ICHAR_PG_UP, JCHAR_PG_UP
      PARAMETER  ( ICHAR_PG_UP = 339, JCHAR_PG_UP = 18 )
      PARAMETER  ( ICHAR_PG_DN = 338, JCHAR_PG_DN = 19 )
      CHARACTER  CHR4*4, CHR*1
!
      IF ( IS_CURLIB_ON() ) THEN
           IF ( KBEEP ) THEN
                CALL SENKRS_BP ( IX, IY, ICHAR4 )
             ELSE
                CALL SENKRS_NB ( IX, IY, ICHAR4 )
           ENDIF
         ELSE
           READ ( 5, '(A)' ) CHR
           CHR4= CHAR(0)//CHAR(0)//CHAR(0)//CHR
           READ ( UNIT=CHR4, FMT='(A)' ) ICHAR4
           IX = 0
           IY = 0
      ENDIF
!
      IF ( ICHAR4 .EQ. ICHAR_PG_DN ) ICHAR4 = JCHAR_PG_DN 
      IF ( ICHAR4 .EQ. ICHAR_PG_UP ) ICHAR4 = JCHAR_PG_UP
#ifdef LITTLE_ENDIAN
!
! --- Change byte order for Little endian architecture
!
      CALL ENDIAN_CNV_I4 ( ICHAR4 )
#endif
      RETURN
      END  !#!  SENKRS_MN  #!#
