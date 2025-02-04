      SUBROUTINE timetag(string)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
!
      INCLUDE 'solve.i'
      INCLUDE 'precm.i'
      character(*) string
      integer*4 timenow
!
      CHARACTER*(NAME_SIZE) FILENAME
      Character*(NAME_SIZE+4*7+4) fname
!
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'TIME'//PRE_LETRS
      call ftn_open( INT2(122), FNAME, 'A' )
      write(122,'(a63,2x,i10)') string,timenow()
      close(122)
      return
!
!
!
      END
