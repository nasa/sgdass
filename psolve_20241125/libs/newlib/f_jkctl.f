      integer*4 function f_jkctl(swmode,jukid,vmode)
      IMPLICIT NONE                         !Added by IMP/jwr
      integer*4 c_f_jkctl
      character*1 swmode,vmode
      character*(*) jukid
      real*8 ptr_ch
!
      f_jkctl=c_f_jkctl(ptr_ch(swmode//char(32)// &
     &                  jukid//char(32)// &
     &                  vmode//char(0)))
!
      end
