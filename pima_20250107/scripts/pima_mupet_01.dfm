!YYK: ver2 (with uvav, uvtaper, etc.).
!-----------------------------------------------------------------------
! Hands off mapping procedure for reasonably well calibrated and edited
! data.  Works only in versions 2.1 and later of Difmap.
!
! WARNING: The output of this script should be checked carefully!
! This script is not intended for use as a "black box" that spits
! out maps ready for publication.  Some parameters will require 
! careful setting and adjustment before this script will produce
! reasonable results.  There are several common failure
! modes for this script.  A few of them are indicated below and 
! the potential user is advised to peruse Appendix A. Automatic 
! Mapping.  
!    Failure Mode          Symptoms                  Key Parameter
! ------------------------ -----------------------   ----------------
! 1) Map size too small    high noise in map,bad fit field_size
! 2) dynam too low         too many clean boxes      dynam
! 3) dynam too high        real components missed    dynam
!
! Input:
!  uvfile  literal-string    The name of the UV FITS file.
!  field_size         int    The map dimension for a single field.
!                            This depends on the source size.
!  field_cell       float    The cell size to use with 'field_size'.
!                            This depends on the sampling of the data.
!  clean_niter        int    The number of CLEAN-iterations per cycle.
!  clean_gain       float    The CLEAN loop gain.
!  dynam            float    The minimum dynamic range required for a peak.
!                            This depends strongly on the SNR of the data.
!  soltime          float    Solution time for phase selfcal (minutes).
!                            This depends strongly on the SNR of the data.
!  thresh           float    The threshold peak clean flux above which
!                            unconstrained amplitude self-cal is viable.
!                            This depends strongly on the SNR of the data.
!  win_mult         float    Multiplier for the size of the clean windows
!                            in units of the restoring beam.
! Output:
!  One cleaned and self-calibrated map.
!
!  Modified: 
!     1995.12.21 Greg Taylor to clear model and start over. 
!                Takes a bit longer but seems to improve image quality.
!     2016.05.19 Yuri Kovalev fine tuning
!
!-----------------------------------------------------------------------

float lim_self;  lim_self = 120.0
float lim_val;   lim_va   = 1.2

integer clean_niter; clean_niter = 100
float clean_gain; clean_gain = 0.03
float dynam;  dynam = 6.0
float soltime1; soltime1 = 120
float soltime2; soltime2 = 20
float thresh; thresh = 0.5
float win_mult; win_mult = 1.8
float time_av; time_av = 16

! Define the inner loop as a macro.

float old_peak
float new_peak
float flux_cutoff

#+map_residual \
flux_cutoff = imstat(rms) * dynam;\
repeat;\
 if (peak(flux) > flux_cutoff) peakwin win_mult;\
 clean clean_niter,clean_gain;\
 flux_cutoff = imstat(rms) * dynam;\
 selfcal;\
 new_peak = peak(flux);\
until(new_peak<=flux_cutoff)

#+map_noselfcal \
flux_cutoff = imstat(rms) * dynam;\
repeat;\
 if (peak(flux) > flux_cutoff) peakwin win_mult;\
 clean clean_niter,clean_gain;\
 flux_cutoff = imstat(rms) * dynam;\
 keep;\
 new_peak = peak(flux);\
until(new_peak<=flux_cutoff)


print "==============================================================="
print "=== Difmap script: pima_mupet_01.dvm version of 2016.05.19 === "

mapsize field_size, field_cell

!
! --- Self-cal to a point source.
!

startmod

! Start mapping the field, using uniform weighting.

uvw 2,-1
map_residual

print "============== Finished uniform weighting clean ==============="

! See if natural weighting reveals any further flux in the current field.

uvw 0,-1
win_mult = win_mult * 1.6
clean_niter = clean_niter * 2
dynam = dynam - 0.5
map_residual

print "============== Finished natural weighting clean =============="

restore

!
! --- Check antenna gain calibration (computes 1 scale factor/antenna)
!
gscale true
dynam = dynam - 0.5
map_residual

!
! --- amplitude self-cal with a long solution time.
!
! --- Flux density scale is not modified
!
selfcal true, true, soltime1
dynam = dynam - 0.75
clean clean_niter,clean_gain
selfcal
map_residual

selfcal true, true, soltime2
clean clean_niter,clean_gain
selfcal
map_residual

uvtaper 0.3,taper_size
win_mult = win_mult * 1.2
selfcal true, true, soltime2
clean clean_niter,clean_gain
selfcal
map_residual
uvtaper 0,0
win_mult = win_mult / 1.2

print "============== Finished amplitude self-cal =============="

!
! --- Restore the map and if the peak flux in the clean map is over a certain
! --- threshold then run an unconstrained selfcal on amplitude as well as phase.
!

restore
if(peak(flux) > thresh)
  selfcal true, true
  clean clean_niter,clean_gain
  selfcal
  map_residual
  selfcal true, true
end if

print "============== Clearing model and starting over =========="

clrmod true
uvw 2,-1
clean clean_niter,clean_gain
uvw 0,-1
map_noselfcal

uvtaper 0.3,taper_size
clean clean_niter,clean_gain
map_noselfcal
uvtaper 0,0

!
! --- One last clean/selfcal loop for luck
!
clean 
selfcal

wmodel %1_map.mod
wobs   %1_uvs.fits
wwins  %1_map.win

!
! --- now clean the entire map
!
x = (field_size-8) * field_cell / 4
addwin -x,x,-x,x
clean (field_size*4),0.01
keep
