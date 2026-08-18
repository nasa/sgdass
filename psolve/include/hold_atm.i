!     This is a small common block which is used in 'mdlpl' to
!     get atm information and zenith path delay from the low
!     level routine 'calcalc' up to the top level routine 'secnd'.
!
!     The values are those actually used to compute the troposphere
!     correction.
!
!     :94.06.15:jwr: Created from scratch.
!     24-DEC-98 pet  Added flag SASSTOM_USED
!
      real*8 pressure(2), temperature(2), humidity(2), &
     &       sasstom_dry_zenith(2), sasstom_wet_zenith(2), &
     &       group_ion_cor, group_ion_sigma, &
     &       phase_ion_cor, phase_ion_sigma, &
     &       cable_cal(2)
      LOGICAL*4  SASSTOM_USED(2)
!
      common /hold_atm/ pressure, temperature, humidity, &
     &       sasstom_dry_zenith , sasstom_wet_zenith, &
     &       group_ion_cor, group_ion_sigma, &
     &       phase_ion_cor, phase_ion_sigma, &
     &       cable_cal, SASSTOM_USED
