      SUBROUTINE make_ixref(ixref,num_eop,kuse_rate)
! make cross reference from measurements to filter.
      implicit none
      INTEGER*2 ixref(*)               !Cross reference index
      INTEGER*2 num_eop                !Number of EOP measured
      LOGICAL kuse_rate(*)             !Do we use the rates?
!
      ixref(1)=1
      ixref(2)=2
      ixref(3)=7
      IF(num_eop .LT. 3) then
         WRITE(*,*) "Make_ixref: Not enough EOP"
         stop
      else IF(num_eop .EQ. 3) then
         return
      else IF(num_eop .eq. 4) then
         IF(kuse_rate(3)) then
           ixref(4)=8
         else
            WRITE(*,*) "Make_ixref: Not set up to handle PM rates!"
          endif
      else
          WRITE(*,*) "Unknown condition!"
      endif
      end  !#!  MAKE_XREF  #!#
