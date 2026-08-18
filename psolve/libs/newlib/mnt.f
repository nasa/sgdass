      integer*4 function mnt(area_name,name_len)
      IMPLICIT NONE                         !Added by IMP/jwr
      integer*2 name_len
      integer*4 c_mnt
      character*140 area_name
      ADDRESS__TYPE :: ptr_ch
!
      mnt=c_mnt(ptr_ch(area_name//char(0)),name_len)
      end
