!
!     sets up buffers for sending data and receiving data over the A900
!     server that accesses the A900 catalog
!
!     send buffer
!
      integer*4 srv_send(75)
      character*112 srv_sendc
      integer*2 srv_send2(150)
!
      equivalence (srv_send(1),srv_sendc)
      equivalence (srv_send(1),srv_send2(1))
!
      character*12 srv_com
      integer*2 srv_cat_or_hsh,srv_buflen,srv_catout(140),srv_hshout(6)
      integer*4 srv_recnum
!
!
!     receive buffer
!
      integer*4 srv_receive(75)
      character*112 srv_receivec
      integer*2 srv_receive2(150)
!
      equivalence (srv_receive(1),srv_receivec)
      equivalence (srv_receive(1),srv_receive2(1))
!
      integer*2 srv_main_err,srv_setpos_err,srv_catin(140),srv_hshin(6), &
     &          srv_post_err,srv_catlu,srv_lstat(11),srv_ustat(11)
      character*2 srv_error_type
      character*10 srv_catname
      character*64 srv_lmsg,srv_umsg
      integer*4 srv_catstart,srv_catsize
!
!
