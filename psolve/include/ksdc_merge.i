!@This is the start of file &KSDC_MERGE
!
!     *********************  NOTE  ************************
!
!     This is the version of KSDC that has variables for database merging.
!
!     :95.04.26: kdb Created.
!
      integer*4  max_mrg_obs
      parameter (max_mrg_obs = 5000)
!
      INTEGER*2 &
     &NUM_DB_MRG          ,INM_MRG(5,MAX_DBS)      ,IVER_MRG(MAX_DBS), &
     &NCORR_MRG(MAX_DBS)  ,JCORR_MRG(MAX_DBS)      ,JNSTA_MRG(MAX_DBS), &
     &JNSTR_MRG(MAX_DBS)  ,obsfil_pointer(max_mrg_obs), mrg_counter
!
      real*8 obsfil_ct(max_mrg_obs)
!
      Common /KSDC_MERGE/ &
     &NUM_DB_MRG,              INM_MRG,            IVER_MRG, &
     &NCORR_MRG,             JCORR_MRG,           JNSTA_MRG, &
     &JNSTR_MRG,        obsfil_pointer,           obsfil_ct, &
     &mrg_counter
!
! num_db_mrg = number of input databases for database merging
! inm_mrg = list of input database names
! iver_mrg = list of input database version numbers
! ncorr_mrg(i) = number of station dependent calibrations available at at least
!                one station in input database i.
! jcorr_mrg(i) = bit array showing which station dependent calibrations
!             are available at at least one station in input database i.
!             Each bit corresponds to a calibration in CORFIL order.
! jnsta_mrg - number of sites in each database header.
!             Index to jsitn
!             site list, where jnsta_mrg(n) corresponds to the nth set of
!             site names in jsitn, arranged in the order they occur in
!             the header of database n.
! jnstr_mrg - number of sources in each database header.
!             Index to jstrn source list,
!             similar to jnsta_mrg.  Element n is the number of sources in
!             the header of database n, minus any duplicates that occurred
!             in earlier databases and were eliminated from database n's
!             part of jstrn.
