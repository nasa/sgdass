      FUNCTION   GOOD_OBS ( LQUAL_CHR )
! ************************************************************************
! *                                                                      *
! *   Simple logical function GOOD_OBS analyse correlator quality code   *
! *   of VLBI observation. It compare it with global variable            *
! *   QUALCODE_GOOD_LIM and if it finds it good  GOOD_OBS = .TRUE.       *
! *   GOOD_OBS return value .TRUE. even in the case then LQUAL_CHR is    *
! *   equal "  " (no information).                                       *
! *                                                                      *
! *  ###  13-AUG-97    GOOD_OBS    v1.1  (c)  L. Petrov  23-AUG-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      LOGICAL*4  GOOD_OBS
      CHARACTER  LQUAL_CHR*(*)
      INTEGER*4  INUM, ILEN
!
      GOOD_OBS = .FALSE.
      INUM = -7
      IF ( INDEX ( LQUAL_CHR, 'F' ) .NE. 0 ) INUM = -6
      IF ( INDEX ( LQUAL_CHR, 'E' ) .NE. 0 ) INUM = -5
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
      IF ( INUM .GE. QUALCODE_GOOD_LIM ) GOOD_OBS = .TRUE.
      IF ( LQUAL_CHR .EQ. '  '    ) GOOD_OBS = .TRUE.
!
      RETURN
      END  !#!  GOOD_OBS  #!#
