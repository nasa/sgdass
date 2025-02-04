      SUBROUTINE rate_flags(lclk,lsitec,logbcl,iclocks,iz,klogbcl)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      integer*2 lclk(MAX_CLK),lsitec(STA_BIT_WORDS,3)
      integer*2 iclocks(*),iz(STA_BIT_WORDS)
      logical*2 logbcl,klogbcl
!
! modify flags for rates only solution, remember old flag settings
! subroutine restore_flags is used to go back to original flags
!
! input:
!    lclk:   clock flags from socom
!    lsitec: site coordinate flags from socom
!    logbcl: baseline clocks control logical
!
! output
!    lclk:   clock flags from socom, with all offset off
!    lsitec: site coordinate flags from socom, with all z components off
!    logbcl: baseline clocks control logical, off
!    iclocks: array of original offset bits from lclk
!    iz:      array of original z component bits from lsietc
!    klogbcl: original logbcl
!
      integer*2 i
      logical*2 kbit
!
      do i=1,max_clk
        call ksbit(iclocks,i,kbit( lclk(i), INT2(1) ) )
        call sbit( lclk(i), INT2(1), INT2(0) )
      enddo
!
      do i=1,max_sta
        call ksbit(iz,i,kbit(lsitec(1,3),i) )
        call sbit( lsitec(1,3), i, INT2(0) )
      enddo
!
      klogbcl=logbcl
      logbcl=.false.
!
      return
      end
