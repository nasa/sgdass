      SUBROUTINE PRFIL_570_TO_1074 ()
! ************************************************************************
! *                                                                      *
! *   Routine  PRFIL_570_TO_1074 transforms prfil common block from      *
! *   570-block format (pre SEP-2002) to 858-block format                *
! *   (post SEP-2002 )                                                   *
! *                                                                      *
! *   It is assumed that the image of the 570-block of prfil resides     *
! *   there where the image of the 858-block prfil should be located.    *
! *                                                                      *
! * ### 26-SEP-2002 PRFIL_570_TO_1074 v1.1 (c) L. Petrov 21-DEC-2002 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE 'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'plist.i'
      INTEGER*4  INT4
      INTEGER*2  INT2_ARG
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      INTEGER*4  J1, J2
!
! --- First zero out STA_FJD_xxx and SRC_FJD_xxx arrays
!
      CALL NOUT_R8 ( INT4(MAX_STA), STA_FJD_BEG )
      CALL NOUT_R8 ( INT4(MAX_STA), STA_FJD_END )
      CALL NOUT_R8 ( INT4(MAX_STA), STA_FJD_MID )
      CALL NOUT_I4 ( INT4(MAX_STA), NSES_STA    )
!
      CALL NOUT_R8 ( INT4(MAX_SRC), SRC_FJD_BEG )
      CALL NOUT_R8 ( INT4(MAX_SRC), SRC_FJD_END )
      CALL NOUT_R8 ( INT4(MAX_SRC), SRC_FJD_MID )
      CALL NOUT_I4 ( INT4(MAX_SRC), NSES_SRC    )
!
! --- Then put J2000.0 epoch there as the best guess since we do not have
! --- information about source and station epcohs
!
      DO 410 J1=1,NUMSTA
         STA_FJD_BEG(J1) = J2000__JD
         STA_FJD_END(J1) = J2000__JD
         STA_FJD_MID(J1) = J2000__JD
         NSES_STA(J1)    = 1
 410  CONTINUE
!
      DO 420 J2=1,NUMSTR
         SRC_FJD_BEG(J2) = J2000__JD
         SRC_FJD_END(J2) = J2000__JD
         SRC_FJD_MID(J2) = J2000__JD
         NSES_SRC(J2)    = 1
 420  CONTINUE
!
      GLO_FJDOBS_MIN = J2000__JD
      GLO_FJDOBS_MAX = J2000__JD
!
      RETURN
      END  !#!  PRFIL_570_TO_1074  #!#
