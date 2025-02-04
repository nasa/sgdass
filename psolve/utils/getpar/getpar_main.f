      PROGRAM    GETPAR_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL GETPAR_MAIN()
      END
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GETPAR_MAIN()
! ************************************************************************
! *                                                                      *
! *   Main program for GETPAR. Look at comments in getpar.f              *
! *                                                                      *
! *  ### 06-MAR-2002   GETPAR_MAIN  v1.2 (c) L. Petrov  12-NOV-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      INTEGER*4  IUER
      INTEGER*8  MEM_LEN
      ADDRESS__TYPE :: MEM_ADR
      ADDRESS__TYPE :: C_BAS, LRA_VAL, LDL_VAL, IEXP_TRP, &
     &                 CSTA_TRP, ZEN_TRP, ADJ_TRP, MJD_TRP, ERR_TRP
      IUER = -1
      CALL GRAB_MEM ( IUER, MEM_LEN,         MEM_ADR, 9, &
     &                      INT8(M_BAS*112), C_BAS,      &
     &                      INT8(M_LSO*17),  LRA_VAL,    &
     &                      INT8(M_LSO*17),  LDL_VAL,    &
     &                      INT8(M_TRP*4),   IEXP_TRP,   &
     &                      INT8(M_TRP*8),   CSTA_TRP,   &
     &                      INT8(M_TRP*8),   MJD_TRP,    &
     &                      INT8(M_TRP*8),   ZEN_TRP,    &
     &                      INT8(M_TRP*8),   ADJ_TRP,    &
     &                      INT8(M_TRP*8),   ERR_TRP )
      IF ( IUER .NE. 0 ) THEN
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GETPAR ( %VAL(C_BAS), %VAL(LRA_VAL), %VAL(LDL_VAL), &
     &              %VAL(IEXP_TRP), %VAL(CSTA_TRP), %VAL(MJD_TRP), &
     &              %VAL(ZEN_TRP), %VAL(ADJ_TRP), %VAL(ERR_TRP), &
     &              IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 2 )
      END  SUBROUTINE  GETPAR_MAIN  !#!#
