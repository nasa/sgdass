      integer*4 function fploth(data_fil,num_lines,minx,maxx, &
     &                          num_bins,dev_type)
      IMPLICIT NONE                         !Added by IMP/jwr
!
      integer*4 c_fploth
      character*(*) data_fil
      character*(*) num_lines
      character*(*) minx
      character*(*) maxx
      character*(*) num_bins
      character*(*) dev_type
      ADDRESS__TYPE :: ptr_ch
!
      fploth=c_fploth(ptr_ch(data_fil//char(32)// &
     &                num_lines//char(32)// &
     &                minx//char(32)// &
     &                maxx//char(32)// &
     &                num_bins//char(32)// &
     &                dev_type//char(0)))
!
      end
