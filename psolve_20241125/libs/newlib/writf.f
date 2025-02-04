      SUBROUTINE writf (iunit,kerr,ibuf,il)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      integer*2 iunit, kerr, il, il_hold, ibuf(*)
      integer*4 i, k
!
!  Input Variables:
!        iunit : logical unit for writing
!        il    : number of memory words to write
!        ibuf  : integer buffer for writing
!
!  Output Variables:
!        kerr  : variable to return error on output (nonzero if error)
!
!  Local Variables:
!        i     : word counter
!        k     : error return value
!
!  880523  written by P. Ryan
!  880614  revised by L. Floyd
!  030910  revised by J. Ryan for Linux and HP-UX compatibility.
!  040323  JWR. Bug in writing the length of the 2nd record fixed.
!  2004.04.13  Replaced definition ibuf(1) with ibuf(*)
!
!  Actually writes two records: the first contains the length of the second
!
#ifdef LINUX
       il_hold = il
       call endian_swap_i2(il_hold)
       call write_endian_unformatted(iunit,int2(1),il_hold)
       call write_endian_unformatted(iunit,il,ibuf)
       kerr = 0
       return 
#else
       write (iunit,iostat=k,err=20) il
       write (iunit,iostat=k,err=20)(ibuf(i),i=1,il)
#endif
       kerr = k
       return
 20    CONTINUE 
       kerr = k                 ! by coming here it won't do the second write
       return
       end


















