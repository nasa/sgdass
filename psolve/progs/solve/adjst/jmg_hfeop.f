      SUBROUTINE init_hf_eop()
      implicit none
!
!     modifications:
!
!     kdb  8/15/95 Set max_hf_par to max_sdc, not
!                  hard coded number.
!                  Handle two eop models, the
!                  one used in the solution and
!                  an arbitrary comparison model.
! GIVEN A FILE WITH EOP PARAMETERS, MAKE HF EOP.
      INCLUDE 'solve.i'
      character*80 ldum
      integer*4 max_hf_par
      parameter (max_hf_par = max_sdc)
      integer*2 num_ut1(2),num_xy(2)
      real*8 ut1_val(2,max_hf_par,2), xy_val(2,max_hf_par,2)
      integer*2 ut1_arg(6,max_hf_par,2), xy_arg(6,max_hf_par,2)
      character*50 hfjmg_file_namr
      integer*2 num_model,iut1,ixy,i,ios
      INTEGER*4  IOS4
!
      common /eop/num_ut1,num_xy,ut1_val,xy_val
      common /eop/ut1_arg,xy_arg
!
      num_model = 1 ! L. Petrov 23-DEC-2003 14:17:43
!
      hfjmg_file_namr = '/data1/solve_files/hf996b'
      open(1,file=hfjmg_file_namr,status='old',iostat=ios4)
      ios = ios4
      if(ios.ne.0) stop 'jmg_hfeop 1'
10    continue
!
      read(1,'(a80)') ldum
      if(ldum(1:1) .eq. "*" .or. ldum(1:1) .eq. ";") goto 10
      read(ldum,*) num_ut1(num_model),num_xy(num_model)
!
      iut1=1
20    continue
      read(1,'(a80)')ldum
      if(ldum(1:1) .eq. "*" .or. ldum(1:1) .eq. ";") goto 20
      read(ldum,*) &
     &  (ut1_arg(i,iut1,num_model),i=1,6), &
     &  ut1_val(1,iut1,num_model),ut1_val(2,iut1,num_model)
      iut1=iut1+1
      if(iut1 .le. num_ut1(num_model)) goto 20
!
      ixy=1
30    continue
      read(1,'(a80)') ldum
      if(ldum(1:1) .eq. "*" .or. ldum(1:1) .eq. ";") goto 30
      read(ldum,*) &
     &  (xy_arg(i,ixy,num_model),i=1,6), &
     &   xy_val(1,ixy,num_model),xy_val(2,ixy,num_model)
      ixy=ixy+1
      if(ixy .le. num_xy(num_model)) goto 30
      close(1)
      return
      end
!************************************************************
      SUBROUTINE return_hf_eop(fjday,dut1,dx,dy,num_model)
      implicit none
      INCLUDE 'solve.i'
      real*8 fjday,dut1,dx,dy,dut1d,dxd,dyd
      integer*2 num_model
      integer*4 max_hf_par
      parameter (max_hf_par=MAX_SDC)
      integer*2 num_ut1(2),num_xy(2)
      real*8 ut1_val(2,max_hf_par,2), xy_val(2,max_hf_par,2)
      integer*2 ut1_arg(6,max_hf_par,2), xy_arg(6,max_hf_par,2)
      real*8 ut_val_local(2,max_hf_par), xy_val_local(2,max_hf_par)
      integer*2 ut_arg_local(6,max_hf_par), xy_arg_local(6,max_hf_par)
      integer*2 ict1
      integer*4 ict2
!
      common /eop/num_ut1,num_xy,ut1_val,xy_val
      common /eop/ut1_arg,xy_arg
!
       do ict1 = 1,6
         do ict2 = 1,max_hf_par
           ut_arg_local(ict1,ict2) = ut1_arg(ict1,ict2,num_model)
           xy_arg_local(ict1,ict2) =  xy_arg(ict1,ict2,num_model)
         enddo
       enddo
!
       do ict1 = 1,2
         do ict2 = 1,max_hf_par
           ut_val_local(ict1,ict2) = ut1_val(ict1,ict2,num_model)
           xy_val_local(ict1,ict2) =  xy_val(ict1,ict2,num_model)
         enddo
       enddo
!
       call get_hf_eop_raw(fjday, &
     &     ut_arg_local,xy_arg_local,ut_val_local,xy_val_local, &
     &     num_ut1(num_model),num_xy(num_model), &
     &     dut1,dx,dy, dut1d,dxd,dyd)
!
      dut1=dut1*15.
      return
      END
