      integer*4 function fplot(cntfil,devtype,lpopt)
      IMPLICIT NONE                         !Added by IMP/jwr
!
!  fgg -- added lpopt to specify printer 6-5-95
!  if lpopt is empty, will print to default printer
!  if lpopt is set, must be of form "-dprintername"
!
      integer*4 c_fplot
      character*(*) cntfil
      character*(*) devtype
      character*(*) lpopt
      ADDRESS__TYPE :: ptr_ch
!
      fplot=c_fplot(ptr_ch(cntfil//char(32)// &
     &                     devtype//char(32)// &
     &                     lpopt//char(0)))
!
      end
