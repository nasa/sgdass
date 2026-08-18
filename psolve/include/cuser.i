!  !CUSER  - Calc user type
!
       Character*1 Calc_user, Apply_ocean, Compute_partials, &
     &             Wet_atm
!
!  Set Calc_user = 'A' for use at Mark III VLBI analysis centers:
       Parameter (Calc_user = 'A')
!
!  Set Calc_user = 'C' for use at VLBI correlators:
!      Parameter (Calc_user = 'C')
!
!************************************************************************
!  The following apply only for correlator users (Calc_user = 'C')
!
!   Apply ocean loading to theoreticals (recommended).
      Parameter (Apply_ocean = 'Y')
!   Don't apply ocean loading to theoreticals (not recommended).
!     Parameter (Apply_ocean = 'N')
!
!   Correlator option to combine dry and wet Niell model atmosphere
!    corrections. Set Wet_atm = 'Y' to combine both wet and dry in
!    DATMC. Set Wet_atm = 'N' to use only the dry component.
      Parameter (Wet_atm = 'Y')
!     Parameter (Wet_atm = 'N')
!
!  Switch to turn off unnecessary computations (mostly partials)
!  **** The following not yet implemented ****
!    'N' means don't compute partials (to save computing time)
!     Parameter (Compute_partials = 'N')
!     Parameter (Compute_partials = 'Y')
