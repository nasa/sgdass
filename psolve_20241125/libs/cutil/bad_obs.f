      FUNCTION   BAD_OBS ( LQUAL_CHR )
! ************************************************************************
! *                                                                      *
! *   Simple logical function BAS_OBS analyses correlator quality code   *
! *   of VLBI observation. It is has quality code less than 1            *
! *   BAD_OBS = .TRUE., else BAD_OBS = .FALSE.                           *
! *                                                                      *
! *  ###  27-MAR-98     BAS_OBS    v1.0  (c)  L. Petrov  27-MAR-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      LOGICAL*4  BAD_OBS
      CHARACTER  LQUAL_CHR*(*)
      INTEGER*4  INUM, ILEN
!
      INUM = -7
      IF ( INDEX ( LQUAL_CHR, 'F' ) .NE. 0 ) INUM = -6
!!      IF ( INDEX ( LQUAL_CHR, 'E' ) .NE. 0 ) INUM = -5
      IF ( INDEX ( LQUAL_CHR, 'D' ) .NE. 0 ) INUM = -4
      IF ( INDEX ( LQUAL_CHR, 'C' ) .NE. 0 ) INUM = -3
      IF ( INDEX ( LQUAL_CHR, 'B' ) .NE. 0 ) INUM = -2
      IF ( INDEX ( LQUAL_CHR, 'A' ) .NE. 0 ) INUM = -1
      IF ( INDEX ( LQUAL_CHR, '0' ) .NE. 0 ) INUM =  0
      IF ( INDEX ( LQUAL_CHR, '1' ) .NE. 0 ) INUM =  1
      IF ( INDEX ( LQUAL_CHR, '2' ) .NE. 0 ) INUM =  2
      IF ( INDEX ( LQUAL_CHR, '3' ) .NE. 0 ) INUM =  3
      IF ( INDEX ( LQUAL_CHR, '4' ) .NE. 0 ) INUM =  4
      IF ( INDEX ( LQUAL_CHR, '5' ) .NE. 0 ) INUM =  5
      IF ( INDEX ( LQUAL_CHR, '6' ) .NE. 0 ) INUM =  6
      IF ( INDEX ( LQUAL_CHR, '7' ) .NE. 0 ) INUM =  7
      IF ( INDEX ( LQUAL_CHR, '8' ) .NE. 0 ) INUM =  8
      IF ( INDEX ( LQUAL_CHR, '9' ) .NE. 0 ) INUM =  9
!
      IF ( INUM .LE. 0  .OR.  INUM .GT. 9 ) THEN
           BAD_OBS = .TRUE.
         ELSE
           BAD_OBS = .FALSE.
      END IF
!
      RETURN
      END  !#!  BAD_OBS  #!#
