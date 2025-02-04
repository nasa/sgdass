      SUBROUTINE mail_message(mail_user,message_header, &
     &                        num_body,message_body,mail_unit, &
     &                        error_dir, error_file_name)
      implicit none
!
!     e-mail a message to the given user (mail_user) consisting of
!       the given message header and message lines contained in array
!       message_body
!
!     input variables:
!
!       mail_user - e-mail address of person who should receive the message
!       message_header - message header
!       num_body - number of lines in message body
!       message_body - message body (array of lines)
!       mail_unit - file lu to be used internally by mail_message
!                   when it creates a work file
!       error_dir, error_file_name - if this subroutine fails, it should notify
!         the user.  But since it failed to mail the target message,
!         trying to mail another (error) message is not a good idea.
!         Instead, this subroutine will create an empty error file.
!         The calling subroutine must then check for this file.
!
      character*(*) mail_user,message_header,message_body(*),error_dir, &
     &              error_file_name
      integer*2 num_body,mail_unit,imh_len,imu_len,ied_len,ief_len
!
!     local variables
!
      integer*4 ipid,ierr4,getpid
      character*5 cpid
      character*255 temp_mail_file,touch_com,mail_com
      integer*2 trimlen,itlen,ierr,system,kerr2,ict
      integer*2 this_body_len
      character*5 cerr
!
!     created by kdb on 10/19/2000
!
!      1/20/2005 kdb The touch command is used at places to create an error
!                    file whose name indicates an error that occurred.
!                    Add the fortran error return to the file name to give
!                    more information.
! 
!
      imh_len = trimlen(message_header)
      imu_len = trimlen(mail_user)
      ied_len = trimlen(error_dir)
      ief_len = trimlen(error_file_name)
      ipid = GETPID()
      write(cpid,"(i5.5)") ipid
      temp_mail_file = "/tmp/MAIL_MESSAGE."//cpid
      itlen = trimlen(temp_mail_file)
      open(mail_unit,file=temp_mail_file(1:itlen),iostat=ierr4, &
     &       err = 100,STATUS='UNKNOWN',ACCESS='SEQUENTIAL', &
     &        FORM = 'FORMATTED')
  100 write(cerr,"(i5.5)") ierr4
      if (ierr4.ne.0) then
        touch_com = "touch "//error_dir(1:ied_len)// &
     &        "/"//error_file_name(1:ief_len) &
     &        //".open_err."//cerr
        CALL ZTERM(touch_com,IERR4)
        KERR2 = SYSTEM(touch_com)
        return
      endif
      do ict = 1,num_body
        this_body_len = trimlen(message_body(ict))
        write(mail_unit,"(A)",iostat=ierr4,err=200) &
     &     message_body(ict)(1:this_body_len)
  200   write(cerr,"(i5.5)") ierr4
        if (ierr4.ne.0) then
          touch_com = "touch "//error_dir(1:ied_len)// &
     &        "/"//error_file_name(1:ief_len) &
     &        //".write_err."//cerr
          CALL ZTERM(touch_com,IERR4)
          KERR2 = SYSTEM(touch_com)
          return
        endif
      enddo
      close (mail_unit)
!
      MAIL_COM = &
     &  'mailx -s "'//message_header(1:imh_len) // &
     &   '" ' // mail_user(1:imu_len)// &
     &    ' < '//temp_mail_file(1:itlen)// &
     &    ' 2> '//error_dir(1:ied_len)//'/'// &
     &    error_file_name(1:ief_len)//'.mailstanderr'
!     MAIL_COM = &
!    &  '/bin/csh -c "mailx -s '//message_header(1:imh_len) // &
!    &   ' ' // mail_user(1:imu_len)// &
!    &    ' < '//temp_mail_file(1:itlen)// &
!    &    ' >& '//error_dir(1:ied_len)//'/'// &
!    &    error_file_name(1:ief_len)//'.mailstanderr"'
      CALL ZTERM(MAIL_COM,IERR4)
      KERR2 = SYSTEM(MAIL_COM)
      if (kerr2.ne.0) then
        write(cerr,"(i5.5)") kerr2
        touch_com = "touch "//error_dir(1:ied_len)// &
     &        "/"//error_file_name(1:ief_len) &
     &        //".mail_err."//cerr
        CALL ZTERM(touch_com,IERR4)
        KERR2 = SYSTEM(touch_com)
        return
      endif
!
      return
      end
