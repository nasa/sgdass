! === Designed for automapping by Kovalev in Dec 2017. ===
!
! 1. No averaging (done earlier if desired)
! 2. Run startmod with no parameters (i.e. solution interval 0.0) 
!    to prevent smootning since phase should be calibrated for every point
! 3. Phase selfcal on the whole scan ~5 min, after that we selfcal with 
!    shorter soltime ~0.2 min.
! 4. Phase selfcal on the whole scan (in order to get higher sensitivity),
!    after that we selfcal with shorter soltime (~0.5 min)
! 5. Amplitude selfcal on the whole dataset (gscale) for any target;
!    after that - one more amplitude selfcal with relatively long 
!    solution interval for non-weak targets.
! 6. No CLEANing with taper.
!
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
!
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
!  Oringal version: on 1994.03.15: Martin Shepherd and Greg Taylor 
!  Modified: 
!     1995.12.21 Greg Taylor to clear model and start over. 
!                Takes a bit longer but seems to improve image quality.
!     2017.12.26 Yuri Kovalev adapted for processing weak sourecs at
!                1 scans with VLBA
!-----------------------------------------------------------------------

integer clean_niter; clean_niter = 100
float clean_gain; clean_gain = 0.04
float dynam;  dynam = 6.0
float amp_soltime1; amp_soltime1 = 60
float amp_soltime2; amp_soltime2 = 5
float phase_soltime1; phase_soltime1=5
float phase_soltime2; phase_soltime2=0.2
float phase_soltime;  phase_soltime=phase_soltime1
float ampcor_selflim; ampcor_selflim=1.20
float thresh; thresh = 0.0
!float win_mult; win_mult = 4
float win_mult; win_mult = 1.8
float time_av; time_av = 30

!
! --- Define the inner loop as a macro.
!
float old_peak
float new_peak
float flux_cutoff

#+map_residual \
flux_cutoff = imstat(rms) * dynam;\
repeat;\
 if (peak(flux) > flux_cutoff) peakwin win_mult;\
 clean clean_niter,clean_gain;\
 flux_cutoff = imstat(rms) * dynam;\
 selfcal false,false,phase_soltime;\
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

!
! --- Assumes UV data has previously been read in
!
! --- Create the map grid.


print "==============================================================="
print "=== Difmap script: pima_mupet_02.dvm version of 2017.12.26 === "

mapsize field_size, field_cell

!
! --- We begin with phase self-cal on the whole scan
!
phase_soltime=phase_soltime1

!
! --- Uvstat info
!
print "uvstat(rms)=",uvstat(rms),"Jy"

!
! --- Self-cal to a point source.
!
startmod

!
! Start mapping the field, using uniform weighting.
!
uvw 2,-1
map_residual

print "============== Finished uniform weighting CLEAN =============="

!
! --- See if natural weighting reveals any further flux in the current field.
!
uvw 0,-2
win_mult = win_mult * 1.6
clean_niter = clean_niter * 2
dynam = dynam - 0.5
map_residual

print "============== Finished natural weighting CLEAN =============="

!
! --- Check antenna gain calibration (computes 1 scale factor/antenna)
!
gscale
selfcal false, false, phase_soltime
dynam = dynam - 0.5
map_residual

print "============== Finished amplitude gscale SELF-CAL =============="

!
! ---  See if shorter phase_soltime can make data less noisy and 
! --- reveals any further flux in the current field.
!
phase_soltime=phase_soltime2
selfcal false, false, phase_soltime
dynam = dynam - 0.5
map_residual

print "=== Finished natural weighting clean with shorter phase_soltime "

!
! --- Restore the map and if the peak flux in the clean map is over a certain
! --- threshold then run a selfcal on amplitude as well as phase.
!
dynam = dynam - 0.25
restore
if(peak(flux) > thresh)
  selfcal true, true, amp_soltime1
  selfcal false, false, phase_soltime
  map_residual

!
! --- I do not want to go deeper in ampl self-cal for survey data - careful with the amplitudes!
!
   print "============== Finished amplitude self-cal =============="
end if

print "============== Clearing model and starting over =========="
clrmod true,true,true
uvw 2,-1
clean clean_niter, clean_gain
uvw 0,-2
map_noselfcal

!
! --- One last clean/selfcal loop for luck
!
clean 
selfcal false, false, phase_soltime

!
! --- Wright down results
!

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
