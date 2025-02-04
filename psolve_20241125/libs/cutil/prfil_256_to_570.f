      SUBROUTINE PRFIL_256_TO_570()
! ************************************************************************
! *                                                                      *
! *   Routine  PRFIL_256_TO_570 transfroms prfil common block from       *
! *   256-block format (pre JUN-2000) to 570-block format                *
! *   (post JUL-2000 )                                                   *
! *                                                                      *
! *   It is assumed that the image of the 256-block of prfil resides     *
! *   there where the image of th 570-block prfil should be located.     *
! *                                                                      *
! * ### 21-JUL-2000 PRFIL_256_TO_570  v1.0 (c) L. Petrov 24-JUL-2000 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'prfil.i'
      INCLUDE   'prfil_256.i'
      INTEGER*4  NB, NB_256
      INTEGER*4  INT4
      INTEGER*2  INT2_ARG
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      NB_256 = JPARFIL_WORDS_256*INT4(WORD_BYTES)
      NB     = JPARFIL_WORDS*INT4(WORD_BYTES)
!
! --- Copyng stuff to the 256-block IPARFIL
!
      CALL LIB$MOVC3 ( NB_256, IPARFIL, IPARFIL_256 )
!
! --- Initialization
!
      CALL NOUT      ( NB,    IPARFIL )
!
! --- Moving variables from 256-block prfil to the 570-block prfil
!
!
! --- REAL*8
!
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA),       VAXOF_256,  VAXOF  )
      CALL LIB$MOVC3 ( 8*3*INT4(MAX_STA),     VSITEC_256, VSITEC )
      CALL LIB$MOVC3 ( 8*2*INT4(MAX_SRC_256), VSTARC_256, VSTARC )
      CALL LIB$MOVC3 ( 8*3,                   VTIDE_256,  VTIDE  )
      CALL LIB$MOVC3 ( 8*2*6,                 VNUT_256,   VNUT   )
      CALL LIB$MOVC3 ( 8*3*INT4(MAX_STA),     VSITEV_256, VSITEV )
      CALL LIB$MOVC3 ( 8*2*6,                 VNUTOP_256, VNUTOP )
      VATM  = VATM_256
      VREL  = VREL_256
      VPREC = VPREC_256
!
! --- REAL*8
!
      CALL LIB$MOVC3 ( 8*INT4(MAX_ARC_STA), BARO_CAL_256,    BARO_CAL    )
      CALL LIB$MOVC3 ( 8*INT4(MAX_ARC_STA), BARO_HEIGHT_256, BARO_HEIGHT )
!
! --- INTEGER*2
!
      CALL LIB$MOVC3 ( 2*4*INT4(MAX_SRC_256), ISTRN_256,     ISTRN )
      CALL LIB$MOVC3 ( 2*4*INT4(MAX_STA),     ISITN_256,     ISITN )
      CALL LIB$MOVC3 ( 2*5*INT4(MAX_STA),     MONUMENTS_256, MONUMENTS )
      CALL LIB$MOVC3 ( 2*6*INT4(MAX_DBS),     IDBPSL_256,    IDBPSL )
      NUMSEL   = NUMSEL_256
      PWCNUMEP = PWCNUMEP_256
!
! --- Character
!
      PWCFNAME = PWCFNAME_256
!
! --- REAL*8
!
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA), VSITED_256, VSITED )
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA), PSITED_256, PSITED )
!
! --- INTEGER*2
!
      PWCSIZEP = PWCSIZEP_256
!
      RETURN
      END  !#!  PRFIL_256_TO_570  #!#
