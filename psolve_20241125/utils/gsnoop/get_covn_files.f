      SUBROUTINE get_covn_files(item_type,use_file,file_found)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
!     input:
!
!      item_type - type of item to get (S for spool file, etc)
!
      character*1   item_type
!
!     output:
!
!       use_file - full path to requested file (may not contain a valid name
!                  depending on error conditions)
!       file_found - success or failure of attempt to get file
!
      CHARACTER*157 use_file
      LOGICAL*2 file_found
!
      CHARACTER*79  qstr
      integer*4   ix,iy,ich
      character*4 cch
      equivalence (ich, cch)
!
!
!     written  7/12/95 by kdb
!     060323 kdb Senkr_mn handles its input via an integer*4 variable, which
!                is subject to linux endian conversion problems.  So 
!                this subroutine will now transfer senkr_mn's input to a 
!                character variable and work with that.
!
      call clear_mn()
      call refresh_mn()
      cch = ' '
      do while (cch(4:4) .ne. 'C' .and. cch(4:4) .ne. 'U' .and. &
     &          cch(4:4) .ne. 'Q')
        qstr = " (C)atlogued or (U)ncatalogued file or (Q)uit ? "
        call asnl(qstr )
        call senkr_mn(ix,iy,ich )
        call casefold (cch)
      enddo
      if (cch(4:4) .eq. 'C') then !c
        call get_cat_file(item_type,use_file,file_found )
      else if (cch(4:4).eq.'U') then !u
        call get_file_gen(item_type,use_file,file_found )
      endif
!
      return
      end
