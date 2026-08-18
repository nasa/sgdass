! Inputs.i
!
!   Include file for external inputs, etc., 1998.03.05, DG.
!    Tilt file info added, 2004.05.13, DG.
        Character*80 External_inputs, Ex_sites, Ex_stars, Ex_ocean, &
     &               Ex_EOP, Ex_tilts
        Logical*4    External_aprioris, Input_sites, Input_stars, &
     &               Input_ocean, Input_EOP, Input_tilts
        Common /Extrnl/External_inputs, Ex_sites, Ex_stars, Ex_ocean, &
     &               Ex_EOP, Ex_tilts, External_aprioris, Input_sites, &
     &               Input_stars, Input_ocean, Input_EOP, Input_tilts
!
