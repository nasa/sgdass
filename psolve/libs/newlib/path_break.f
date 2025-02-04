      SUBROUTINE path_break(full_path,dir_name,idlen,base_name,iblen)
!
!     Given a full path to a file (e.g., /abc/def/ghi),
!        breaks the path down,
!        returning the directory name (/abc/def) and its length,
!        and the file name (ghi) and its length.
!
!     Assumes the input path is a file name, either a simple one
!       (without a preceding directory OR a full path with a directory and
!        base file name).  Will not handle directories (e.g., /tmp,
!          /data2/glbout).  Also will not handle files on root
!        (e.g., /file1)
!
!     created by kdb 96/7/23
!
      IMPLICIT NONE
!
!     full_path - full path to file
!     dir_name - directory name
!     idlen - length of dir_name
!     base_name - file name
!     iblen - lengh of base_name
!
      character*(*) full_path,dir_name,base_name
      integer*2 idlen,iblen
!
!     local variables
!
      integer*2 iflen, trimlen,islash,ict
!
      iflen = trimlen(full_path)
      if (iflen.eq.0) then
        dir_name = ' '
        idlen = 0
        base_name = ' '
        iblen = 0
      endif
!
!     Locate the file name
!
      islash = 0
      do ict = 1,iflen
        if (full_path(ict:ict).eq.'/') islash = ict
      enddo
!
!     Set the output arguments
!
      if (islash .eq.0) then
        dir_name = ' '
        idlen = 0
        base_name = full_path(1:iflen)
        iblen = iflen
      else
        dir_name = full_path(1:islash - 1)
        idlen = islash - 1
        base_name = full_path(islash+1:iflen)
        iblen = iflen - islash
      endif
!
      Return
      END
