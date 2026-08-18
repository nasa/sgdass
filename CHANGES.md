# 2025.06.22  build all

all: support of gcc/gfortean 15.1.0. The previous version failed compilation
     under 15.1.0

psolve:

     a) Added support of of setting environment variables
        SOLVE_DEBUG, SOLVE_EXPORT, SOLVE_EXPORT_DIR inside
        interactive pSolve.
 
     b) Added an option to specify time range for constraint
        on spline for non-linear station position mean value 
        and rate of change.

     c) Fixed a regression bug that prevented setting a priori
        clock offsets.

vtd:

     a) Made an additional update in vtd_apriori.py to 
        better treat processing merged mass loading

     b) Updates in difxvtd related to correlation of
        VLBI observations of GNSS satellites.
        (Joe Skeens).

pima:
     a) fixed pbu.py
     b) fixed pf.py

malo, spd:  numerous changes for supporting more than one backend


# 2025.02.28  build all

  pima: Added support of telemetry in antcal format
        Modified pf.py to call log2ant for processing IVS log files.
        Added support of NOT_TO_USE: or TO_USE: qualifiers for keyword TSYS.   
        Re-wrote task opag. The new version of pima downloads data files
        with opacity and atmospheric brightness temperature from a remote
        server instead of computing it.
        Updated task opal to handle a case when opacity and brightness
        temperature is computed only for one azimuth.
  atp:  Added parsing cable delay
  Added dependencies to tle for vtd, gvh, psolve, pima, sur_sked, malo, spd


# 2024.11.25  build all


  sgdass: added option upgrade to sgdass_update.py
  malo:   added support of new NASA numerical model GEOS-IT.
          Added sanitizing wget command line
  spd,spd_client: revised handling of slant path delay in binary
                  format computation, export, and import.
                  Changed format of bsdp_summary.txt files.
                  Added bspd_util. The old VTD,PIMA,pSolve may not
                  be compatible with the new slant path delays
  malo:           revised handling site displacement in bindsip 
                  format.
  vtd: updated handling slant path delays
  petools: fixed a regression introduced in 2022 that made pSolve 
           much slower.
           Added support of writing graphic files in png format.
  pSolve:  fixed reporting station positions when a priori velocities
           were applied. 
  Added a new package log2ant

# 2024.05.21  build all
