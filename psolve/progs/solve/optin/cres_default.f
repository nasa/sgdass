      SUBROUTINE CRES_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  CRES.                               *
! *   CRES_DEFAULT sets default values in glbc4 for CRES -- program for  *
! *   computation of residulas.                                          *
! *                                                                      *
! *  ###  05-MAY-98  CRES_DEFAULT  v2.0 (c)  L. Petrov  12-AUG-2003 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INTEGER*4  IUER, IER, ILEN, I_LEN
      CHARACTER  STR*20
!
! --- First of all: system-wide defaults
!
      CRES_STYLE = CRES__CURRENT
!
! --- Examining CRES_PRE98
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'CRES_STYLE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:7) .EQ. 'CURRENT' .OR. STR(1:2) .EQ. 'NO' ) THEN
                CRES_STYLE = CRES__CURRENT
             ELSE IF ( STR(1:5) .EQ. 'PRE98' ) THEN
                CRES_STYLE = CRES__PRE98 
             ELSE IF ( STR(1:5) .EQ. 'PRE03' ) THEN
                CRES_STYLE = CRES__PRE03
             ELSE
                CALL ERR_LOG ( 6881, IUER, 'CRES_DEFAULT', 'Environment '// &
     &              'variable CRES_PRE98 has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  CRES_DEFAULT  #!#
