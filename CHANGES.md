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
