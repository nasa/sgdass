!     changes:
!
!     4/95      kdb Support the 6 1995 iers site submission files.
!                   Subsequently get the input covariance file for 4 of these
!                   from the solution catalog.
!                   Subsequently print variable constraint levels in the
!                   solution/estimate and solution/apriori files.
!     6/14/95   kdb Carry the plates on which the sites were assumed to live
!                   in the input solution.
!     95/07/12 kdb Add ability to use input covariance and control files
!                  that are not catalogued in a solution archiving system.
!     95/09/29 kdb For iers source table, optionally use only sessions with
!                  good observations for the output epochs and session count.
!                  (add src_sum_good_sess, use_bad_src_sess)
!     95/12/29 kdb New "other" page option (gmt plots of the type used by
!                  Jim Ryan's 1995 iugg presentation.)
!     96/04/24 kdb Add iers_var_factor, iers_num_obs, iers_unknowns for
!                  new iers site submission file, solution statistics section.
!     96/04/25 kdb Add ref_ep_yr, ref_ep_mn, ref_ep_dy for removal of hard
!                  coded reference epochs from the iers_solest_<> and
!                  iers_solestap_<> files.
!     96/07/11 kdb Add command_file, icflen and out_comment
!                  to support new option F (command file).
!     96/07/22 kdb Add file_create_time, creation time of output files.
!     97/08/06 kdb Raise max_sta from 500 to 512.
!     98/02/06 kdb Add isign_dec.
!     98/08/06 kdb Add variables for writing continuous piecewise linear
!                  sites to iers_solep_<>.
!     00/10/25 kdb Change max_src parameter to max_src_gsnoop
!                  to avoid gsfcb conflict.
!                  Also change max_sta to max_sta_gsnoop.
!                  Also comment out sta_bit_words and word_bits.
!     01/04/16 pet Increased  max_src_gsnoop  from 2000 to 4096
!     01/09/14 kdb Add separate_header_file.
!     01/09/14 kdb Add constraints_velocities, suppression_velocity_tie,
!                      num_suppression_velocity_tie, first_in_velgrp.
!     02/05/15 kdb Add spool_solve_revision_date_jd,
!                      spool_solve_revision_date_char.
!                  Also add ichangeover_{year,month,day}_0001 and a new
!                      common block to hold them.
!     03/01/09 kdb Change the ichangover_ separate variables (_0001, _0002
!                  etc.) to an array and add an equivalent julian date array.
!     03/03/24 kdb Add ra_adj and dec_adj.
!
      integer*2     ncpls, mcpl
      parameter     (mcpl=10)
      parameter     (ncpls=256)
      integer*2     icpls_date(mcpl,3,ncpls)
      integer*2     cpls_nepochs(mcpl)
!
      real*8        cpls_xyz(mcpl,3,ncpls), &
     &              cpls_uen(mcpl,3,ncpls), &
     &              cpls_xyz_sig(mcpl,3,ncpls), &
     &              cpls_uen_sig(mcpl,3,ncpls)
      integer*4     cpls_sum_dlay(ncpls+1,mcpl)
      real*8        cpls_sum_wmjd(ncpls+1,mcpl)
      real*8        cpls_max(ncpls+1,mcpl)
      real*8        cpls_min(ncpls+1,mcpl)
      real*8        cpls_mean(ncpls+1,mcpl)
!
!
      integer*2     max_sta_gsnoop, max_src_gsnoop
      integer*2     max_vel_grps
!     integer*2     word_bits
!     integer*2     sta_bit_words
!      integer*2     max_iris
      parameter     ( max_sta_gsnoop = 512  )
      parameter     ( max_src_gsnoop = 4096 )
      parameter     ( max_vel_grps = max_sta_gsnoop/2)
!      parameter     (max_iris = 1000)
!     parameter     (word_bits = 16)
!     parameter
!    .  (sta_bit_words = (max_sta_gsnoop+word_bits-1)/word_bits)
!
      integer*2     jcrtr, jcrtw
      parameter     (jcrtr = 5)
      parameter     (jcrtw = 6)
!
      integer*2 nsite, nmin, epochs(20), epoch_count, &
     &          imin_site(23,max_sta_gsnoop), &
     &          isite_names(23,max_sta_gsnoop), &
     &          irefdate(3,max_sta_gsnoop), &
     &          ncplstat
!
!
      character*4 monuments(max_sta_gsnoop), qmcpl(mcpl), &
     &  sol_plate(max_sta_gsnoop)
      character*6 file_tag
      character*8 qskip_stat, qcpl_stat(10)
!
      character*23 min_site(max_sta_gsnoop)
      character*23 site_names(max_sta_gsnoop) !site_names(1:8) = name,
!          9:12 =  monument#, 13:18 = episodic date,  19:23 =index
!
      character*78 user_path
      character*80  cpluen_name
      character*157 spool_name,cov_name,control_name
      character*150 cbuf
!
!
      logical   kdebug
      logical   do_iers_site, site_flyby,   source_flyby, &
     &          do_hyper,     do_annual,    do_iers_source, &
     &          kskip_stat,   bin_uen_sigs, do_lat_lon,  do_ep_sites, &
     &          do_vel,       do_3_sig,     scale_by_rrchi, &
     &          do_xyz_vel,   do_uen_vel,   do_uen_adj, &
     &          do_xyz_cpl,   do_uen_cpl,   do_cmpar, &
     &          do_min_sig,   kwp,          src_plt, &
     &          do_sinex_non, do_sinex_cov, do_sinex_cap, &
     &          do_auto_gmt_jwr
      logical   separate_header_file
!
      equivalence (site_names,isite_names)
      equivalence (min_site, imin_site)
!
      real*8 xyz(3,max_sta_gsnoop),          xyz_sig(3,max_sta_gsnoop), &
     &       xyz_dot_sig(3,max_sta_gsnoop),  xyz_dot(3,max_sta_gsnoop), &
     &       xyz_min(3, max_sta_gsnoop),     uen_min(3,max_sta_gsnoop), &
     &       xyz_min_sig(3, max_sta_gsnoop), &
     &       uen_min_sig(3,max_sta_gsnoop), &
     &       uen_dot(3,max_sta_gsnoop), &
     &       uen_dot_sig(3,max_sta_gsnoop), &
     &       uen(3,max_sta_gsnoop), &
     &       uen_sig(3,max_sta_gsnoop), &   !adjustments
     &       uen_apriori(3,max_sta_gsnoop)
!
      real*8 site_phi(max_sta_gsnoop), site_lon(max_sta_gsnoop), &
     &       height(max_sta_gsnoop), &
     &       xyz_epoch(3,20,max_sta_gsnoop), &
     &       xyz_epoch_sig(3,20,max_sta_gsnoop), &
     &       delta_east_vel(max_sta_gsnoop), &
     &       delta_north_vel(max_sta_gsnoop), &
     &       delta_up_vel(max_sta_gsnoop)
!
      real*8 y_x(max_sta_gsnoop),   z_x(max_sta_gsnoop), &
     &           z_y(max_sta_gsnoop), &
     &       xd_x(max_sta_gsnoop),  xd_y(max_sta_gsnoop), &
     &           xd_z(max_sta_gsnoop), &
     &       yd_x(max_sta_gsnoop),  yd_y(max_sta_gsnoop), &
     &           yd_z(max_sta_gsnoop), &
     &       zd_x(max_sta_gsnoop),  zd_y(max_sta_gsnoop), &
     &           zd_z(max_sta_gsnoop), &
     &       yd_xd(max_sta_gsnoop), zd_xd(max_sta_gsnoop), &
     &           zd_yd(max_sta_gsnoop)
!
      real*8 pos_tot_el(max_sta_gsnoop), &
     &       pos_tot_az(max_sta_gsnoop), &
     &       pos_tot_adj(max_sta_gsnoop), &
     &       pos_hor_az_sig(max_sta_gsnoop), &
     &       pos_hor_az(max_sta_gsnoop), &
     &       pos_hor_adj(max_sta_gsnoop), &
     &       pos_hor_adj_sig(max_sta_gsnoop), &
     &       pos_error_az(3,max_sta_gsnoop), &
     &       pos_error_el(3,max_sta_gsnoop), &
     &       pos_error_amp(3,max_sta_gsnoop), &
     &       pos_hor_error_az(2,max_sta_gsnoop), &
     &       pos_hor_error_amp(2,max_sta_gsnoop)
!
      real*8 tot_vel(max_sta_gsnoop), &
     &       tot_vel_adj(max_sta_gsnoop), &
     &       tot_az(max_sta_gsnoop), &
     &       tot_az_adj(max_sta_gsnoop), &
     &       tot_el(max_sta_gsnoop), &
     &       tot_el_adj(max_sta_gsnoop), &
     &       hor_vel(max_sta_gsnoop), &
     &       hor_az(max_sta_gsnoop), &
     &       hor_vel_sig(max_sta_gsnoop), &
     &       hor_az_sig(max_sta_gsnoop), &
     &       delta_hor_vel(max_sta_gsnoop), &
     &       delta_hor_az(max_sta_gsnoop), &
     &       error_amp(3,max_sta_gsnoop), &
     &       error_az(3,max_sta_gsnoop), &
     &       error_el(3,max_sta_gsnoop), &
     &       hor_az_adj    (max_sta_gsnoop), &
     &       hor_vel_adj    (max_sta_gsnoop), &
     &       hor_az_adj_sig(max_sta_gsnoop), &
     &       hor_vel_adj_sig(max_sta_gsnoop), &
     &       hor_error_az(2,max_sta_gsnoop), &
     &       hor_error_amp(2,max_sta_gsnoop)
!
      real*8 rrchi, scale
      character*1 use_bad_src_sess
!
      real*8 iers_var_factor,iers_num_obs
      integer*4  iers_unknowns
      integer*2 ref_ep_yr, ref_ep_mn, ref_ep_dy
!
      integer*2 icflen
      character*63 command_file
      character*78 out_comment,out_path
      integer*2 out_ipath
      character*12 file_create_time
!
      integer*2 constraints_velocities((max_sta_gsnoop+15)/16), &
     &          suppression_velocity_tie(max_sta_gsnoop), &
     &          num_suppression_velocity_tie, &
     &          first_in_velgrp(max_vel_grps)
!
      real*8 spool_solve_revision_date_jd
      character*10 spool_solve_revision_date_char
!
      common /gsnoop_com/ &
     &ncplstat, &
     &spool_name, &
     &cbuf,       epochs,        irefdate, &
     &site_names, nsite,         epoch_count, monuments, &
     &min_site,   nmin, &
     &xyz,        xyz_dot,       uen,         uen_dot, &
     &xyz_min,    uen_min, &
     &xyz_min_sig,uen_min_sig, &
     &xyz_sig,    xyz_dot_sig,   uen_sig,     uen_dot_sig, &
     &xyz_epoch,  xyz_epoch_sig, site_phi,    site_lon, &
     &height, &
     &y_x  , z_x  , z_y  , xd_x, xd_y , xd_z , yd_x , yd_y, &
     &yd_z , yd_xd, zd_x , zd_y, zd_z , zd_xd, zd_yd, &
     &pos_tot_el,        pos_tot_az,         pos_tot_adj, &
     &pos_hor_az_sig,    pos_hor_adj_sig,    pos_error_az, &
     &pos_hor_adj,       pos_hor_az,         pos_hor_error_amp, &
     &pos_error_el,      pos_error_amp,      pos_hor_error_az, &
     &tot_vel,           tot_vel_adj,        tot_az, &
     &tot_az_adj,        tot_el,             tot_el_adj, &
     &hor_vel,           hor_az,             hor_vel_sig, &
     &hor_az_sig,        delta_hor_vel,      delta_hor_az, &
     &delta_east_vel,    delta_north_vel,    delta_up_vel, &
     &hor_error_az,      hor_error_amp,      uen_apriori, &
     &hor_az_adj_sig,    hor_vel_adj_sig,    error_amp, &
     &error_az, error_el,   hor_az_adj, hor_vel_adj,  scale, &
     &do_hyper, do_annual,  do_lat_lon, do_iers_site, do_iers_source, &
     &site_flyby,           source_flyby,     src_plt, &
     &kdebug,   kskip_stat, qskip_stat, bin_uen_sigs, do_ep_sites, &
     &do_vel,   do_3_sig,   do_min_sig,       scale_by_rrchi,   kwp, &
     &do_xyz_vel,           do_uen_vel,       do_uen_adj,      do_cmpar, &
     &do_xyz_cpl,           do_uen_cpl,       qcpl_stat,          qmcpl, &
     &icpls_date,           cpls_xyz,         cpls_uen,    cpls_xyz_sig, &
     &cpls_uen_sig,         cpluen_name ,     file_tag,    user_path, &
     &do_sinex_non,         do_sinex_cov,     do_sinex_cap, &
     &sol_plate,            cov_name,         control_name, &
     &src_sum_good_sess,    use_bad_src_sess, do_auto_gmt_jwr, &
     &iers_var_factor,      iers_num_obs,     iers_unknowns, &
     &ref_ep_yr,            ref_ep_mn,        ref_ep_dy, &
     &command_file,         icflen,           out_comment, &
     &file_create_time,     cpls_nepochs,     separate_header_file, &
     &constraints_velocities, suppression_velocity_tie, &
     &num_suppression_velocity_tie,      first_in_velgrp, &
     &spool_solve_revision_date_char, spool_solve_revision_date_jd
!
!     these are the variables used in the accumulation of
!     mean epoch information etc.
!
      integer*2     totsta, totsrc, nsrc
      integer*2     src_sum_sess(max_src_gsnoop)
      integer*2     src_sum_good_sess(max_src_gsnoop)
      integer*4     src_sum_rate(max_src_gsnoop)
      integer*4     src_sum_dlay(max_src_gsnoop)
!
      real*8        sta_mean_epoch(max_sta_gsnoop), &
     &              sta_span(max_sta_gsnoop), &
     &              sta_mjdmax(max_sta_gsnoop), &
     &              sta_mjdmin(max_sta_gsnoop), &
     &              src_mean_epoch(max_src_gsnoop), &
     &              src_span(max_src_gsnoop), &
     &              src_mjdmax(max_src_gsnoop), &
     &              src_mjdmin(max_src_gsnoop), &
     &              ra_dec(max_src_gsnoop), &
     &              ra_adj(max_src_gsnoop), &
     &              dec_adj(max_src_gsnoop)
!
      integer*2 ra_hr(max_src_gsnoop+2),   ra_min(max_src_gsnoop+2)
      real*8    ra_sec(max_src_gsnoop+2),  ra_unc(max_src_gsnoop+2)
!
      integer*2 dec_deg(max_src_gsnoop+2), dec_min(max_src_gsnoop+2)
      real*8    dec_sec(max_src_gsnoop+2), dec_unc(max_src_gsnoop+2)
      integer*2 isign_dec(max_src_gsnoop+2)
!
      logical       rates_used
!
      character*8   srcname(max_src_gsnoop+2)
      character*8   gsrcname(max_src_gsnoop) ! g for global
!
      common /gsnoop_epo/ &
     &       src_sum_rate,   rates_used, &
     &       sta_mean_epoch, sta_span,   sta_mjdmax,   sta_mjdmin, &
     &       src_mean_epoch, src_span,   src_sum_sess, src_sum_dlay, &
     &       src_mjdmax,     src_mjdmin, srcname,      gsrcname, &
     &       totsta,         totsrc,     nsrc,         rrchi, &
     &       ra_hr,          ra_min,     ra_sec,       ra_unc, &
     &       dec_deg,        dec_min,    dec_sec,      dec_unc, &
     &       ra_adj,         dec_adj, &
     &       ra_dec,         isign_dec,  cpls_sum_dlay, &
     &       cpls_sum_wmjd,  cpls_max,   cpls_min,     cpls_mean
!
!
!
      integer*2 max_changeovers, num_changeovers
      parameter (max_changeovers = 120)
      integer*2 ichangeover_year(max_changeovers), &
     &          ichangeover_month(max_changeovers), &
     &          ichangeover_day(max_changeovers)
      real*8 changeover_jd(max_changeovers)
      common /changeovers_com/ &
     & num_changeovers, ichangeover_year,ichangeover_month, &
     & ichangeover_day, changeover_jd
