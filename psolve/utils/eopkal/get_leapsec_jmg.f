      INTEGER*2 FUNCTION GET_LEAPSEC_JMG ( XJD, XLEAP )
      IMPLICIT  NONE
      INCLUDE  'param.i'
!
!     Routine to retrieve leapsecond information for given date from
!     default leapsecond file.  Passes back a five-element array of
!     leapsecond information for the given date:
!
!       Position   Description
!       ---------------------------
!          1       Date of leapsecond.
!          2       The leapsecond (TAI-UTC).
!          3       Epoch of rate of change.
!          4       Rate of change of leapsecond.
!          5       Date of next leapsecond.
!
!     Programmers:
!       Gregg Cooke  90.02.12    Creation.
!       Gregg Cooke  90.03.30    Returns date of next leapsecond as
!                                        new fifth element of xleap. Also
!                                        removed lpnam.
!
!       JMGipson     92.05.04    Extensively  modified so only reads input
!                                file once!
!       pet          2000.10.02  Removed hardcoded definition of DFLEAP
!
!     Error Returns:
!       0>   FORTRAN file I/O error.
!     -1201  Requested data is before first leapsecond in file.
!     -1301  Requested data is after last leapsecond in file.
!
!     Parameters:
!
!     The default name of the leapsecond file.
!
!      DFLEAP*xxx  defined in $PSOLVE_ROOT/progs/include/param.i
!
!     Specifications:
!
!     LPDCB   --  Unit number of leapsecond file.
!     XLEAPR  --  Current leapsecond entry read from file.
!
      INTEGER*4 ilen,max_len,init,ierr,i,j
      parameter (max_len=100)
      real*8 xjd, xleap(5), xleapr(4,max_len)
!
!
      save xleapr,init,ilen
!
!     Program Structure:
!
!     Initialize the routine.  Get a unit number from getunit.  Then
!     open the leapsecond file.
!
!
      if(init .ne. 12345) then
         init=12345
         OPEN ( UNIT=5, FILE=DFLEAP, STATUS='OLD', IOSTAT=IERR, ERR=911 )
         do ilen=1,max_len
           read(5,'(17X,F9.1,12X,F10.7,12X,F6.0,4X,F9.7,1X)', &
     &         IOSTAT=ierr,ERR=800,END=800) &
     &      xleapr(1,ilen),xleapr(2,ilen),xleapr(3,ilen),xleapr(4,ilen)
         end do
800      continue
         close(5)
         ilen=ilen-1
      endif
!
!
      do i=1,ilen
        if(xjd .lt. xleapr(1,i)) then
          if(i .eq. 1) then
             get_leapsec_jmg = -1201
             return
          else
             goto 850
          endif
        endif
      end do
!
850   continue
      i=i-1
      get_leapsec_jmg = 0
      do j=1,4
        xleap(j)=xleapr(j,i)
      end do
      xleap(5) = xleapr(1,i+1)
      return
!
911   continue
      write(*,*) "FROM GET_LEAPSEC_JMG:  Leap year file not found!"
      CALL EXIT ( 4 )
      END  !#!  GET_LEAPSEC_JMG  #!#
