      PROGRAM STOPRUN
      IMPLICIT NONE
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
      character*50 fname
      character*2  cletrs
      CHARACTER    resp*1
      integer*2 ierr,ibuf(40),wdlen,trimlen,irm(5),letrs
      character*80 cbuf,new_work_dir,bufstr
      equivalence (ibuf(1),cbuf)
      equivalence (letrs,cletrs)
!
      call rmpar(irm)
      call start_mn()
      if ( irm(1) .gt. 64 ) then
           letrs = irm(1)
        else
           call addstr_f("Enter SOLVE initials to be stopped: ")
           call getstr_f(bufstr)
           read(bufstr,'(a2)') cletrs
      endif
!
      call addstr_f("Are you sure (Y/N)? ")
      call getstr_f(bufstr)
      read(bufstr,'(a1)') resp
      if (resp.eq.'Y'.or.resp.eq.'y') then
        call casefold(cletrs)
        ierr=fc_getenv(ptr_ch('WORK_DIR'//char(0)),ptr_nc(ibuf))
        if (ierr.gt.0) then
          NEW_WORK_DIR=cbuf(1:ierr)
        else
          NEW_WORK_DIR=SOLVE_WORK_DIR
        endif
        wdlen=TRIMLEN(NEW_WORK_DIR)
        fname = new_work_dir(:wdlen)//'stop'//cletrs(1:2)
        write(bufstr,'("Deleting file ",a)') fname
        call addstr_f(bufstr)
        call nl_mn()
        call refresh_mn()
        call bin_unlink(fname,ierr)
        if (ierr.ne.0) then
             write(bufstr,'("Error ",i5," deleting file ",a)') ierr,fname
          call addstr_f(bufstr)
          call nl_mn()
          call refresh_mn()
        endif
      endif
      call end_mn()
      END  !#!  STOPRUN  #!#
