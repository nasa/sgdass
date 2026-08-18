!
      SUBROUTINE file_control(fcontrol)
!
      implicit none
      INCLUDE 'gsnoop_com.i'
!
      integer*2 ipath, trimlen
!
      character*72 fcontrol
      character*78 blank, comment, path, buffer
      character*6  file_id
!
      DATA BLANK / &
     &' &
     &            '/
      DATA File_id /'      '/
!
!
      do_vel = .true.
      do_hyper = .false.
!      do_iers_ss = .false.
      do_ep_sites = .false.
      scale_by_rrchi = .false.
!
!
      open (49, file=fcontrol,status='old')
      do while ((buffer(1:4).ne."STOP").and.(buffer(1:4).ne."STOP") &
     &   .and.(buffer(2:5).ne."STOP").and.(buffer(2:5).ne."STOP"))
        read (49, '(a)') buffer
        if (buffer(2:8).eq."file_id") Read(buffer(10:),'(a)') File_id
        if (buffer(2:8).eq."comment") Read(buffer(10:),'(a)') comment
        if (buffer(2:11).eq."spool file") Read(buffer(12:),'(a)') &
     &                                    SPOOL_NAME
!
        if (buffer(2:5).eq."path") then
          Read(buffer(10:),'(a)') path
          ipath = trimlen(path)
        end if
!
        if (buffer(2:13).eq."hypermap yes") do_hyper = .true.
!
!       if (buffer(2:13).eq."ierss_ss yes") do_iers_ss = .true.
!
!
      end do !while
!
!
!
      close (49)
      return
      end
