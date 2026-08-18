      SUBROUTINE restore_flags(lclk,lsitec,logbcl,iclocks,iz,klogbcl)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      integer*2 lclk(MAX_CLK),lsitec(STA_BIT_WORDS,3)
      integer*2 iclocks(*),iz(STA_BIT_WORDS)
      logical*2 logbcl,klogbcl
!
! restore flags modified by subroutine rate_flags
!
! input:
!    lclk:   clock flags from socom, with all offsets off
!    lsitec: site coordinate flags from socom, with all z components off
!    logbcl: baseline clocks control logical, off
!    iclocks: array of original offset bits from lclk
!    iz:      array of original z component bits from lsietc
!    klogbcl: original logbcl
!
! output
!    lclk:   restored clock flags
!    lsitec: restored site coordinate flags
!    logbcl: restored baseline clocks control logical
!
      integer*2 i
      logical*2 kbit
!
      do i=1,max_clk
        call ksbit( lclk(i), INT2(1), kbit(iclocks,i) )
      enddo
!
      do i=1,max_sta
        call ksbit(lsitec(1,3),i,kbit(iz,i) )
      enddo
!
      logbcl=klogbcl
!
      return
      end
