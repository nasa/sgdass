      SUBROUTINE PRFIL_1074_to_1906 ()
! ************************************************************************
! *                                                                      *
! *   Routine  PRFIL_1074_to_1906 transforms prfil common block from     *
! *   1074-block format (pre MAR-2006) to 1906 format                    *
! *   (post MAR-2006 )                                                   *
! *                                                                      *
! *   It is assumed that the image of the 1074-block of prfil resides    *
! *   there where the image of the 1906-block prfil should be located.   *
! *                                                                      *
! * ## 26-SEP-2002 PRFIL_1074_to_1906  v1.0 (c) L. Petrov 27-MAR-2006 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'prfil_1074.i'
      INCLUDE   'plist.i'
      INTEGER*4  NB, NB_1074
      INTEGER*4  INT4
      INTEGER*2  INT2_ARG
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      NB_1074 = JPARFIL_WORDS_1074*INT4(WORD_BYTES)
      NB      = JPARFIL_WORDS*INT4(WORD_BYTES)
!
! --- Copyng stuff to the 1074-block IPARFIL
!
      CALL LIB$MOVC3 ( NB_1074, IPARFIL, IPARFIL_1074 )
!
! --- Initialization
!
      CALL NOUT      ( NB,    IPARFIL )
!
! --- Moving variables from 1074-block prfil to the 11906-block prfil
!
!
! --- REAL*8
!
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA),        VAXOF_1074,  VAXOF  )
      CALL LIB$MOVC3 ( 8*3*INT4(MAX_STA),      VSITEC_1074, VSITEC )
      CALL LIB$MOVC3 ( 8*2*INT4(MAX_SRC_1074), VSTARC_1074, VSTARC )
      CALL LIB$MOVC3 ( 8*3,                    VTIDE_1074,  VTIDE  )
      CALL LIB$MOVC3 ( 8*2*6,                  VNUT_1074,   VNUT   )
      CALL LIB$MOVC3 ( 8*3*INT4(MAX_STA),      VSITEV_1074, VSITEV )
      CALL LIB$MOVC3 ( 8*2*6,                  VNUTOP_1074, VNUTOP )
      VATM  = VATM_1074
      VREL  = VREL_1074
      VPREC = VPREC_1074
!
! --- REAL*8
!
      CALL LIB$MOVC3 ( 8*INT4(MAX_ARC_STA), BARO_CAL_1074,    BARO_CAL    )
      CALL LIB$MOVC3 ( 8*INT4(MAX_ARC_STA), BARO_HEIGHT_1074, BARO_HEIGHT )
!
! --- INTEGER*2
!
      CALL LIB$MOVC3 ( 2*4*INT4(MAX_SRC_1074), ISTRN_1074,     ISTRN )
      CALL LIB$MOVC3 ( 2*4*INT4(MAX_STA),      ISITN_1074,     ISITN )
      CALL LIB$MOVC3 ( 2*5*INT4(MAX_STA),      MONUMENTS_1074, MONUMENTS )
      CALL LIB$MOVC3 ( 2*6*INT4(MAX_DBS),      IDBPSL_1074,    IDBPSL )
      NUMSEL   = NUMSEL_1074
      PWCNUMEP = PWCNUMEP_1074
!
! --- Character
!
      PWCFNAME = PWCFNAME_1074
!
! --- REAL*8
!
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA), VSITED_1074, VSITED )
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA), PSITED_1074, PSITED )
!
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA), STA_FJD_BEG_1074, STA_FJD_BEG )
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA), STA_FJD_END_1074, STA_FJD_END )
      CALL LIB$MOVC3 ( 8*INT4(MAX_STA), STA_FJD_MID_1074, STA_FJD_MID )
      CALL LIB$MOVC3 ( 8*INT4(MAX_SRC_1074), SRC_FJD_BEG_1074, SRC_FJD_BEG )
      CALL LIB$MOVC3 ( 8*INT4(MAX_SRC_1074), SRC_FJD_END_1074, SRC_FJD_END )
      CALL LIB$MOVC3 ( 8*INT4(MAX_SRC_1074), SRC_FJD_MID_1074, SRC_FJD_MID )
!
      GLO_FJDOBS_MIN_1074 = GLO_FJDOBS_MIN
      GLO_FJDOBS_MAX_1074 = GLO_FJDOBS_MAX
      CALL LIB$MOVC3 ( 4*INT4(MAX_STA),      NSES_STA_1074, NSES_STA )
      CALL LIB$MOVC3 ( 4*INT4(MAX_SRC_1074), NSES_SRC_1074, NSES_SRC )
!
! --- INTEGER*2
!
      PWCSIZEP = PWCSIZEP_1074
!
      RETURN
      END  !#!  PRFIL_1074_to_1906  #!#
