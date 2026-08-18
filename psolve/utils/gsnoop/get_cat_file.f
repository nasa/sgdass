      SUBROUTINE get_cat_file(item_type,use_file, file_found)
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!include  'gsnoop_com.i'
!
      INTEGER*4       ios
      INTEGER*2       len_str, trimlen
      CHARACTER*157   hold_name
      LOGICAL*2 file_found
      LOGICAL*4       kexist
!
!     94/06/24 DS Caprette HSTX, converted to curses.
!     95/04/10 K. Baver    HSTX, save the user tag, solution tag and version
!                                that characterize the spoolfile's solution
!                                in case the user later chooses an option
!                                that requires the covariance file from the
!                                same solution.
!     95/07/12 kdb Add ability to use input covariance and control files
!                  that are not catalogued in a solution archiving system.
!     06/04/18 KDB Add conditional compiler directives to comment out solution
!                  archiving code under LINUX, where it is not available yet.
!
!
!     solarc variables:
!
      character*1     item_type
      integer*2       ilocu,iver,flen,plen,iarsc,kerr
      character*1     ccheck,cformat,item
      character*2     user_tag
      character*8     sol_tag
      character*15    qinbuf
      character*79    qstr, qstr2
      character*157   fpath, use_file
      character*140   ppath
      character*255   message
!
!
      ios = 1
      ilocu = 7
      ccheck = ' '
      cformat ='N'
      item = item_type
!
!'''/''''1''''/''''2''''/''''3''''/''''4''''/''''5''''/''''6''''/''''7''''/''''8
#ifdef LINUX
!cout qstr="Enter GLOBL user's initials, solution tag, and version."
!cout qstr2="xx ######## nnn (Left adjusted)   Or enter :: to quit."
      qstr="This option is not yet available under LINUX"
      qstr2="Please enter :: to quit."
#else
!     qstr="Enter GLOBL user's initials, solution tag, and version."
!     qstr2="xx ######## nnn (Left adjusted)   Or enter :: to quit."
      qstr="This option is not yet available under ftn 90"
      qstr2="Please enter :: to quit."
#endif
!
!
!
      do while (ios .ne. 0)
        call clear_mn()
        call refresh_mn()
        call asnl(qstr )
        call asnl(qstr2 )
        call getstr_f(qinbuf )
        read (qinbuf, "(2a)")     user_tag
        if (user_tag .eq. '::') go to 999
        read (qinbuf(4:), "(8a)") sol_tag
        read (qinbuf(12:), *, iostat=ios) iver
        if (ios .ne. 0) then
          iver = 1
          qstr="Looking for version 1."
          call as2nl(qstr )
        end if
        qstr="Just a minute ..."
        call as2nl(qstr )
!
!
#ifdef LINUX
!cout   call &
!cout     &       ITEM_PATH_T(ILOCU,CCHECK,CFORMAT,USER_TAG,SOL_TAG,IVER, &
!cout     &       ITEM,FPATH,FLEN,PPATH,PLEN,IARSC,MESSAGE,KERR )
        message = "Sorry, the solution archiving system is not available yet"
#else
!       call &
!         &       ITEM_PATH_T(ILOCU,CCHECK,CFORMAT,USER_TAG,SOL_TAG,IVER, &
!         &       ITEM,FPATH,FLEN,PPATH,PLEN,IARSC,MESSAGE,KERR )
        message = "Sorry, the solution archiving system is not available yet"
#endif
        ios = kerr
        if (kerr .ne. 0) then
          call asnl(message )
          call return_to_continue()
        else
          file_found = .false.
          len_str = trimlen(fpath)
          write (hold_name,"(a)") fpath(1:len_str)
          IF(user_tag .EQ. '::') then
            return
          else
            use_file = hold_name
            inquire(FILE=use_file,exist=kexist,iostat=ios)
            if (ios .ne. 0) then
              write (qstr,'("Error ",i4," occurred inquiring about")') ios
              call asnl(qstr )
              call as2nl(use_file )
              file_found = .false.
            else if (kexist) then  !We're done here.
              file_found = .true.
            else
              call asnl(use_file )
              qstr = "NOT FOUND"
              call as2nl(qstr )
              file_found = .false.
              call return_to_continue()
              ios = 1
            end if
!
!
          end if
        end if
      end do
!
 999  Return
!
!
      END
