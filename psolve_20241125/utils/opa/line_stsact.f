      FUNCTION LINE_STSACT ( STS_LINE, ACT_LINE )
! ************************************************************************
! *                                                                      *
! *   Character auxilary function LINE_STSACT  returns a three-letter    *
! *   string with values of STS_LINE at the 1-st position and ACT_LINE   *
! *   at the 3-rd position.                                              *
! *                                                                      *
! *  ### 11-OCT-2000  LINE_STSACT  v1.0 (c)  L. Petrov  11-OCT-2000 ###  *
! *                                                                      *
! ************************************************************************
      CHARACTER  LINE_STSACT*3, STS_LINE*1, ACT_LINE*1
      LINE_STSACT = '? ?'
!
      IF ( STS_LINE .EQ. '-' ) LINE_STSACT(1:1) = '-'
      IF ( STS_LINE .EQ. '+' ) LINE_STSACT(1:1) = '+'
      IF ( STS_LINE .EQ. 'N' ) LINE_STSACT(1:1) = 'N'
      IF ( STS_LINE .EQ. 'n' ) LINE_STSACT(1:1) = 'N'
      IF ( ACT_LINE .EQ. '-' ) LINE_STSACT(3:3) = '-'
      IF ( ACT_LINE .EQ. '+' ) LINE_STSACT(3:3) = '+'
      IF ( ACT_LINE .EQ. 'N' ) LINE_STSACT(3:3) = 'N'
      IF ( ACT_LINE .EQ. 'n' ) LINE_STSACT(3:3) = 'N'
      IF ( ACT_LINE .EQ. 'D' ) LINE_STSACT(3:3) = 'D'
      IF ( ACT_LINE .EQ. 'd' ) LINE_STSACT(3:3) = 'D'
!
      RETURN
      END  !#!  LINE_STSACT  #!#
