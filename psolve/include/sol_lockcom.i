!
!  lockcom.i - include file that remembers the file descriptors for
!                 first catalog file, second catalog file, and the hash file
!
      integer*4 fd1, fd2, fdhash
      common/lockcom/ fd1, fd2, fdhash
