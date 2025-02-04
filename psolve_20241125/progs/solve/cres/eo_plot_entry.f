      SUBROUTINE EO_PLOT_ENTRY ( IM, ID, IY, IHR, IMIN, DERIV, TROT_CUR, MAT )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  EO_PLOT_ENTRY PROGRAM SPECIFICATION
!
! 1.1   For selected observations, generate  information which
!       will later be used to plot earth orientation adjustments and
!       sigmas vs time.  (Data plotted will be the same as MDLPL plots.)
!       (MWH-920903: Generate one value per hour, using a weighted average
!        of the two observations spanning the top of the hour)
!
! 1.2 REFERENCES:
!
! 2.  EO_PLOT_ENTRY INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ID,IM,IY,IHR,IMIN
      REAL*8  DERIV(M_GPA,2),trot_cur,mat(*)
!
! IM,ID,IY,IMIN,IHR - Time tag parameters
! DERIV - Earth orientation partials array
!
! 2.3 OUTPUT Variables: None
!
! OUTPUT is through CRECM common block
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'oborg.i'
      INCLUDE 'socom.i'
      INCLUDE 'crecm.i'
!      INCLUDE '../include/nrmcm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: secnd
!       CALLED SUBROUTINES: quadp
!
! 3.  LOCAL VARIABLES
!
      REAL*8 TIME_TEST,DDOT,DDEO,DSEO,QUADP,ratap(3),rotap_cur(4), &
     &       SCAL1(M_GPA),tim1,tim2,tim3,fjldy,ddum,cnvrt
      real*8 dcor(3),tim,dud,dxd,dyd
      logical*2 kinterp
      INTEGER*2 IEOP,IEOS,IEON,kp,idum2(2)
      INTEGER*4 IBLAS1, NBLAS, JA, JB
!
      DATA CTIME_TEST /-1.0D0/, IBLAS1 /1/
!
! 4.  HISTORY
!  WHO  WHEN      WHAT
!  KDB  10/30/90  First version
!  MWH  920903    Modify to put entries exactly on the hour by interpolating
!
! 5. EO_PLOT_ENTRY PROGRAM STRUCTURE
!
      JA = 3*M_GPA
      JB = 2*M_GPA
      CNVRT = (180.D0/PI__NUM)*3600.D0*1000.D0
      if ( eoplot_ct .eq. 0 ) then          ! save value for first observation
        eoplot_ct = 1
        kinterp = .FALSE.
      else if (eoplot_tag(4,eoplot_ct).eq.ihr) then  ! same hour; save this one
        kinterp = .FALSE.
      else                              ! new hour; interpolate and save
        eoplot_ct = eoplot_ct + 1
        kinterp = .TRUE.
      endif
!
      EOPLOT_TAG(1,EOPLOT_CT) = IY
      EOPLOT_TAG(2,EOPLOT_CT) = IM
      EOPLOT_TAG(3,EOPLOT_CT) = ID
      EOPLOT_TAG(4,EOPLOT_CT) = IHR
      EOPLOT_TAG(5,EOPLOT_CT) = IMIN
!
      if (kinterp) then      ! set up time tags for doing interpolation
        tim1 = (eoplot_tag(5,eoplot_ct-1)+60*eoplot_tag(4,eoplot_ct- &
     &         1))/60.d0
!       tim1 = (eoplot_tag(5,eoplot_ct-1)+60*eoplot_tag(4,eoplot_ct-1))
!     .         /1440.d0 + fjldy(eoplot_tag(2,eoplot_ct-1),eoplot_tag
!     .         (3,eoplot_ct-1),eoplot_tag(1,eoplot_ct-1)) + 0.5d0
        tim2 = (imin+60*ihr)/60.d0
!       tim2 = (imin+60*ihr)/1440.d0 + fjldy(im,id,iy) + 0.5d0
        if (tim1.gt.tim2) tim2 = tim2 + 24.d0
        tim3 = dint(tim2)
        eoplot_tag(1,eoplot_ct-1) = iy
        eoplot_tag(2,eoplot_ct-1) = im
        eoplot_tag(3,eoplot_ct-1) = id
        eoplot_tag(4,eoplot_ct-1) = ihr
        eoplot_tag(5,eoplot_ct-1) = 0
      endif
!
                      if(KEROT) then !flyby mapped values
                        CALL INTRP_EOMOD(TROT_CUR,ROTAP_CUR(3), &
     &                       ddum,ROTAP_CUR(1),ROTAP_CUR(2), &
     &                       kinterp,RATAP(3),RATAP(1),RATAP(2), &
     &                       IDUM2 )
                      else  !standard values
                        CALL INTRP_EOVR(TROT_CUR,ROTAP_CUR(3), &
     &                       ddum,ROTAP_CUR(1),ROTAP_CUR(2), &
     &                       kinterp,RATAP(3),RATAP(1),RATAP(2), &
     &                       IDUM2 )
                      endif
        rotap_cur(3) = -rotap_cur(3)
        DO IEOP = 1,3 !running over x-wobble,y-wobble,UT1
!
!         I. Generate adjustment value for this time and its sigma:
!
!
!         Pull out the pointers that locate the earth orientation
!         parameters in the b-vector and deriv array
!
          IEOS=EOPLOT_STARTS(IEOP)
          IEON=EOPLOT_NUMS(IEOP)
          NBLAS = IEON
!
!         First, generate the adjustment itself.
!
!         Sum up d delay   * adjustment (parm i)
!                  ------
!                  d parm i
!
!                 to get the total adjustment to the delay due to all
!                 the x-wobble (or y or UT1) parameters
!
          DDEO=DDOT(NBLAS,DERIV(IEOS,1),IBLAS1,mat(jb+IEOS),IBLAS1)
!
!         Now divide by d delay
!                       -------
!                       d total x-wobble (or y or UT1)
!
!         to get the adjustment to the total x-wobble (etc)
!         itself
!
          DDEO=DDEO*EOPLOT_CNVRT(IEOP)/ROTP(IEOP,1)
!
!         Change ROTP's TAI-UT1 to UT1-TAI
!
          IF (IEOP .EQ. 3) DDEO = -DDEO
!
!         Now generate the sigma
!
!         For each pair of x (or y or UT1) parameter coefficients,
!         ai and aj, multiply their covariance (an A matrix element)
!         by d delay       and  d delay
!            -------            -------
!            d ai                d aj
!
!         and add these results to get the total variance of the delay
!         for x-wobble, etc.
!
          DSEO=QUADP(mat(ja+1),DERIV(1,1),SCAL1,IEOS,IEON)
!
!         Now divide the delay's variance by d delay
!                                              -------
!                                              d x-wobble (etc)
!
!          to get the sigma for the total x-wobble adjustment
!          for this observation.
!
          DSEO=DSQRT(DSEO/(ROTP(IEOP,1)*ROTP(IEOP, &
     &              1)))*EOPLOT_CNVRT(IEOP)
!
!         Load info into arrays to be held until it can be written to
!         a file
!
          EOPLOT_ADJ(IEOP,EOPLOT_CT) = DDEO
          EOPLOT_SIG(IEOP,EOPLOT_CT) = DSEO
          eoplot_tot(ieop,eoplot_ct) = &
     &        rotap_cur(ieop)*eoplot_cnvrt(ieop) + ddeo
!
!   Do interpolation if appropriate
!
          if (kinterp) then
            eoplot_adj(ieop,eoplot_ct-1) = eoplot_adj(ieop,eoplot_ct- &
     &        1)* (tim2-tim3)/(tim2-tim1) + DDEO * (tim3-tim1)/(tim2-tim1)
            eoplot_sig(ieop,eoplot_ct-1) = eoplot_sig(ieop,eoplot_ct- &
     &        1)* (tim2-tim3)/(tim2-tim1) + DSEO * (tim3-tim1)/(tim2-tim1)
            eoplot_tot(ieop,eoplot_ct-1) = eoplot_tot(ieop,eoplot_ct- &
     &        1)* (tim2-tim3)/(tim2-tim1) + eoplot_tot(ieop, &
     &        eoplot_ct)* (tim3-tim1)/(tim2-tim1)
            if (ieop.eq.1) then
              tim = trot_cur - imin/1440.d0
              call get_hf_eop( tim, dcor(3), dcor(1), dcor(2), dud, dxd, dyd, &
     &             INT2(1) )
            endif
            eoplot_ctot(ieop,eoplot_ct-1) = eoplot_tot(ieop,eoplot_ct- &
     &           1)+ dcor(ieop)
          endif
!
        END DO !Running over x,y and ut1
!
      RETURN
      END
