      SUBROUTINE MAP_EOP_RESULTS ( TROT_CUR, EOP_INDICIES, MAT, &
     &                             SIGMA_SCALE_FACTOR, LBUF_LEN, LBUF, IPTR, &
     &                             PAGEWID )
      IMPLICIT NONE
!
      INCLUDE 'solve.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
      real*8 trot_cur, fjdobs, ljdobs, test_epoch(3), &
     &       ut1_val, shortp, xwob_val, ywob_val, &
     &       xwob_rate,ywob_rate, xpole(3),ypole(3), ut1(3), &
     &        x_cov(3,3),  y_cov(3,3),  u_cov(3,3), partial_div(3), &
     &       xr_cov(2,2), yr_cov(2,2), ur_cov(2,2), &
     &       mult_vmv,mat(*), &
     &       xpole_off      ,ypole_off      , ut1_off      , &
     &       xpole_rate     ,ypole_rate     , ut1_rate     , &
     &       xpole_off_sigma,ypole_off_sigma, ut1_off_sigma, &
     &       xpole_rat_sigma,ypole_rat_sigma, ut1_rat_sigma, &
     &       xpole_off_adj  ,ypole_off_adj  , ut1_off_adj  , &
     &       xpole_rat_adj  ,ypole_rat_adj  , ut1_rat_adj  , &
     &       return_adjustment, sigma_scale_factor
!
      INTEGER*2 EOP_INDICIES(3,3), IM, ID, IY, IYY, IHR, IMIN
      integer*2 idum2,i,j
      integer*4 num_x_parms,   num_y_parms,   num_u_parms
      integer*4 num_xr_parms,  num_yr_parms,  num_ur_parms
      integer*4  x_parms_list(3), y_parms_list(3),  u_parms_list(3), j3, ierr4
      integer*4 xr_parms_list(2),yr_parms_list(2), ur_parms_list(2)
      integer*4 iptr, pagewid, LBUF_LEN
      character*120 lbuf(LBUF_LEN)
      CHARACTER BUFFER*160, BUF*256
      integer*2 numdd,ldbnam(5,15),idbver(15)
      Integer*4 idbend(15)
      character*10 dbnam_c(15)
      equivalence(ldbnam,dbnam_c)
      logical*4 indicies_set
      CHARACTER  EOP_TAG*23
      CHARACTER, EXTERNAL :: JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      real*8 cnvrt(2)
!     cnvrt(1) is radians to microarcseconds
!     cnvrt(2) is seconds to microseconds
      data cnvrt /206264806.d3, 1000.d3/
      data xpole /3*0.d0/, ypole /3*0.d0/, ut1  /3*0.d0/
      data  x_cov /9*0.d0/,  y_cov /9*0.d0/,  u_cov /9*0.d0/
      data xr_cov /4*0.d0/, yr_cov /4*0.d0/, ur_cov /4*0.d0/
      data xpole_rat_sigma /0.d0/, ypole_rat_sigma /0.d0/, ut1_rat_sigma /0.d0/
      data j3 /3/
!
!     :97.12.01:jwr: Created.
!     :98.01.15:jwr: The used of SIGMA_SCALE_FACTOR eliminate so that unscaled
!                    uncertainties are used. Also header modified for
!                    rate estimates.
!     :98.02.04:jwr: Time epoch for EOP mnapping changed from hard coded
!                    value 18:30 to the vcariable MAPPED_EOP_TIME from glbc4.i
!   pet   1999.05.28. Made LBUF_LEN formal arguments
!   pet   1999.10.15  Changed slightly output format.
!
!     Make certain some indicies have been set. Otherwise skip out.
      indicies_set = .false.
      do i =1,3
        do j = 1,3
        if(eop_indicies(i,j).gt.0)  indicies_set = .true.
        enddo
      enddo
!
      if(indicies_set) then
!
!       Get the name of the 1st db.
        call dbpox(numdd,ldbnam,idbver,idbend)
!
!------ Get the eop adjustments for the standard estimate time, i.e. trot_cur
!       The pointers for the vector of adjustments have been passed in
!       Handles up to 2nd order estimation.
!
!       Get the adjustemnts, parameter count, and matrix pointers for each type
        Do i =1,3
          If(eop_indicies(i,1) .gt.0) then !offset estimated
            xpole(i) = return_adjustment(eop_indicies(i,1),mat)
            num_x_parms = i
            x_parms_list(i) = eop_indicies(i,1)
            if(i.gt.1) xr_parms_list(i-1) = eop_indicies(i,1)
          Endif
!
          If(eop_indicies(i,2) .gt.0) then !rate estimated
            ypole(i) = return_adjustment(eop_indicies(i,2),mat)
            num_y_parms = i
            y_parms_list(i) = eop_indicies(i,2)
            if(i.gt.1) yr_parms_list(i-1) = eop_indicies(i,2)
          Endif
!
          If(eop_indicies(i,3) .gt.0) then !2nd order estimated
            ut1  (i) = return_adjustment(eop_indicies(i,3),mat)
            num_u_parms = i
            u_parms_list(i) = eop_indicies(i,3)
            if(i.gt.1) ur_parms_list(i-1) = eop_indicies(i,3)
          Endif
!
!         The number of parms contributing to rates is one less than offset
          num_xr_parms = num_x_parms-1
          num_yr_parms = num_y_parms-1
          num_ur_parms = num_u_parms-1
        Enddo
!
!------ Get the covarinace matrices so we can map the sigmas to the off epochs.
!       get offset covariance
        call  create_covariance_matrix(num_x_parms,x_parms_list,mat,x_cov,ierr4)
        call  create_covariance_matrix(num_y_parms,y_parms_list,mat,y_cov,ierr4)
        call  create_covariance_matrix(num_u_parms,u_parms_list,mat,u_cov,ierr4)
!       get rate covariance
        If(num_xr_parms .gt. 0) &
     &  call create_covariance_matrix(num_xr_parms,xr_parms_list,mat,xr_cov,ierr4)
        If(num_yr_parms .gt. 0) &
     &  call create_covariance_matrix(num_yr_parms,yr_parms_list,mat,yr_cov,ierr4)
        If(num_ur_parms .gt. 0) &
     &  call create_covariance_matrix(num_ur_parms,ur_parms_list,mat,ur_cov,ierr4)
!
! ----- Get the start and stop times for this sesson
!
        call obstm ( fjdobs, ljdobs )
!
! ----- set the 1st test epoch to MAPPED_EOP_TIME on the 1st day and the 3rd
!       to MAPPED_EOP_TIME on the last (second) day
!
        test_epoch(1) = 0.5d0 + int(fjdobs-0.5d0) + MAPPED_EOP_TIME/86400.D0
        test_epoch(2) =         int(ljdobs-0.5d0) + MAPPED_EOP_TIME/86400.D0
        test_epoch(3) = 0.5d0 + int(ljdobs-0.5d0) + MAPPED_EOP_TIME/86400.D0
!
!------ Run over the test epochs and write the eop offset totals
        If(kscreen) then
          iptr = iptr+1
          write(lbuf(iptr),'( &
     &    "Mapped EOP:   | Julian Date | xpole (microasec) |", &
     &    "  xpole (microasec)|  ut1 (microasec)   |")')
          call addstr_f(lbuf(iptr)(:pagewid))
        Endif
!
        IF ( KSCREEN ) THEN
             CALL NL_MN()
        END IF
        Do i = 1,3
!         Get the apriori totals
!         These routine returns values in the in radians and seconds/
!         Convert to output units when forming the total apriori's.
          If(KEROT) then !flyby mapped values
            CALL INTRP_EOMOD( test_epoch(i), ut1_val, shortp, xwob_val, &
     &       ywob_val, TRUE__L2, ut1_rate, xwob_rate, ywob_rate, idum2)
          else  !standard values
            CALL INTRP_EOVR ( test_epoch(i), ut1_val, shortp, xwob_val, &
     &       ywob_val, TRUE__L2, ut1_rate, xwob_rate, ywob_rate, idum2)
          endif
!
!         Convert the values from radians to micro-arc-s and micro-t-s
!         and flip the sign on the ut1_val to go from tai-ut1 to ut1-tai.
          xwob_val =  xwob_val*cnvrt(1)
          ywob_val =  ywob_val*cnvrt(1)
          ut1_val  = -ut1_val *cnvrt(2)
!
          xwob_rate=  xwob_rate*cnvrt(1)
          ywob_rate=  ywob_rate*cnvrt(1)
          ut1_rate = -ut1_rate *cnvrt(2)
!
!         Create the partial derivatives need to map the sigma to the off
!         epochs
!
          partial_div(1) =  1.d0
          partial_div(2) =  test_epoch(i) - trot_cur
          partial_div(3) = (test_epoch(i) - trot_cur)**2
!
          xpole_off_sigma = (mult_vmv(num_x_parms,partial_div,x_cov, &
     &    partial_div))**0.5
          ypole_off_sigma = (mult_vmv(num_y_parms,partial_div,y_cov, &
     &    partial_div))**0.5
          ut1_off_sigma   = (mult_vmv(num_u_parms,partial_div,u_cov, &
     &    partial_div))**0.5
!
          If(num_xr_parms .gt. 0) &
     &    xpole_rat_sigma = (mult_vmv(num_xr_parms,partial_div,xr_cov, &
     &    partial_div))**0.5
          If(num_yr_parms .gt. 0) &
     &    ypole_rat_sigma = (mult_vmv(num_yr_parms,partial_div,yr_cov, &
     &    partial_div))**0.5
          If(num_ur_parms .gt. 0) &
     &    ut1_rat_sigma   = (mult_vmv(num_ur_parms,partial_div,ur_cov, &
     &    partial_div))**0.5
!
!-------  Create the eop total from the aprioris and adjustments
!
          xpole_off_adj = 0.d0
          ypole_off_adj = 0.d0
          ut1_off_adj   = 0.d0
          Do j=1,3
            xpole_off_adj = xpole_off_adj + xpole(j)*partial_div(j)
            ypole_off_adj = ypole_off_adj + ypole(j)*partial_div(j)
            ut1_off_adj   = ut1_off_adj   + ut1  (j)*partial_div(j)
          Enddo
!
          xpole_rat_adj = 0.d0
          ypole_rat_adj = 0.d0
          ut1_rat_adj   = 0.d0
          Do j=1,2
            xpole_rat_adj = xpole_rat_adj + xpole(j+1)*partial_div(j)
            ypole_rat_adj = ypole_rat_adj + ypole(j+1)*partial_div(j)
            ut1_rat_adj   = ut1_rat_adj   + ut1  (j+1)*partial_div(j)
          Enddo
!
!------   Convert from milli to micro units and add apriori
          xpole_off_adj   = xpole_off_adj*1.d3
          ypole_off_adj   = ypole_off_adj*1.d3
          ut1_off_adj     =   ut1_off_adj*1.d3
!
          xpole_off       = xpole_off_adj      +xwob_val
          ypole_off       = ypole_off_adj      +ywob_val
          ut1_off         =   ut1_off_adj      + ut1_val
!
          xpole_off_sigma = xpole_off_sigma*cnvrt(1)
          ypole_off_sigma = ypole_off_sigma*cnvrt(1)
          ut1_off_sigma   =   ut1_off_sigma*cnvrt(2)
!
          xpole_rat_adj   = xpole_rat_adj*1.d3
          ypole_rat_adj   = ypole_rat_adj*1.d3
          ut1_rat_adj     =   ut1_rat_adj*1.d3
!
          xpole_rate      = xpole_rat_adj      +xwob_rate
          ypole_rate      = ypole_rat_adj      +ywob_rate
          ut1_rate        =   ut1_rat_adj      + ut1_rate
!
          xpole_rat_sigma = xpole_rat_sigma*cnvrt(1)
          ypole_rat_sigma = ypole_rat_sigma*cnvrt(1)
          ut1_rat_sigma   =   ut1_rat_sigma*cnvrt(2)
!
!------   List
!
          IF ( I == 1 ) THEN
               EOP_TAG = JD_TO_DATE ( TEST_EPOCH(I) - 32.184D0/86400.0D0, -2 )
               BUF = 'Mapped EOP results for '//DBNAM_C(1)//'  '//EOP_TAG
               WRITE ( 23, 110 ) BUF(1:I_LEN(BUF)), &
     &                XPOLE_OFF*1.0D-6,     XPOLE_OFF_SIGMA*1.0D-6, &
     &                YPOLE_OFF*1.0D-6,     YPOLE_OFF_SIGMA*1.0D-6, &
     &                UT1_OFF*1.0D-6,       UT1_OFF_SIGMA*1.0D-6,   &
     &                XPOLE_RAT_ADJ, XPOLE_RAT_SIGMA, &
     &                YPOLE_RAT_ADJ, YPOLE_RAT_SIGMA, &
     &                UT1_RAT_ADJ,   UT1_RAT_SIGMA
 110           FORMAT ( A, '  X_pole: ',   F10.7, ' -+ ', F9.7,' arcsec',  &
     &                     '  Y_pole: ',   F10.7, ' -+ ', F9.7,' asrsec',  &
     &                     '  UT1: ',      F11.7, ' -+ ', F9.7,' sec',     &
     &                     '  X_rate: ',   F7.1,  ' -+ ', F7.1,' uas/day', &
     &                     '  Y_rate: ',   F7.1,  ' -+ ', F7.1,' uas/day', &
     &                     '  UT1_rate: ', F7.1,  ' -+ ', F7.1,' us/day'   )
          END IF
          CALL EPOC(IM,ID,IY,IHR,IMIN,test_epoch(i))
          write ( buffer, &
     &           '(i2,"/",i2,"/",i2,i3,":",i2," |",'// &
     &           'f14.5,'// &
     &           '" | ",f9.1,2f7.1,'// &
     &           '" | ",f9.1,2f7.1,'// &
     &           '" | ",f11.1,2f7.1,'// &
     &           '" | ",2f6.1,'// &
     &           '" | ",2f6.1,'// &
     &           '" | ",2f6.1,"|" )' ) &
     &    iy,im,id,ihr,imin,test_epoch(i), &
     &    xpole_off, xpole_off_adj, xpole_off_sigma, &
     &    ypole_off, ypole_off_adj,ypole_off_sigma, &
     &    ut1_off  , ut1_off_adj  ,ut1_off_sigma, &
     &    xpole_rat_adj, xpole_rat_sigma, &
     &    ypole_rat_adj, ypole_rat_sigma, &
     &    ut1_rat_adj  , ut1_rat_sigma
!
          If(kscreen) then
            iptr = iptr+1
            write(lbuf(iptr), &
     &      '( i2,"/",i2,"/",i2,i3,":",i2," |", '//&
     &      'f14.5, '//&
     &      '" | ",f10.1,f7.1,'// &
     &      '" | ",f10.1,f7.1,'// &
     &      '" | ",f12.1,f7.2,a1)' ) &
     &      iy,im,id,ihr,imin,test_epoch(i), &
     &      xpole_off, xpole_off_sigma, &
     &      ypole_off, ypole_off_sigma, &
     &      ut1_off  , ut1_off_sigma,'|'
            call addstr_f(lbuf(iptr)(:pagewid))
            call nl_mn()
          endif
!
          if(i.eq.1) then
            write(23,'( '//&
     &        '"yy mm dd hr mn |  Julian date  |        x-pole (uas)     |"'// &
     &        '"       y-pole  (uas)     |         ut1 (uts)         |x"'// &
     &        '"-p rate (uas/d)|y-p rate (uas/d)|ut rate (uts/d)|"/'// &
!
     &        '"               |               |   total   adjst.  sigma |"'// &
     &        '"   total   adjst.  sigma |     total   adjst.  sigma | "'// &
     &        '" adjst.   sigma|  adjst.   sigma|  adjst.  sigma|")' )
          endif
          write(23,'(a)') buffer(1:160)
          If(i.eq.3) write(23,'(" ")')
        Enddo
!
        iptr = iptr+1
        If(i.eq.1 .and. kscreen)  write(lbuf(iptr),'(" ")')
        IF ( KSCREEN ) THEN
             CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
             CALL NL_MN()
        ENDIF
      ENDIF
!
      RETURN
      END  !#!  MAP_EOP_RESULTS  #!#
