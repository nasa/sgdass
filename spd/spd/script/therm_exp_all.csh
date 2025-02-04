#!/bin/csh -f
set num_cpu    = 6
find /g2/merra_heb/ -name "*t_2001*" | sort -r | \
      parallel -P $num_cpu \
      $SPD_DIR/bin/therm_exp \
      $SPD_DIR/share/therm_vlbi.cnf \
      {} \
      /vlbi/solve/save_files/antenna-info.txt \
      /g5/therm_exp_eph/ate_ \
      1
find /g2/merra_heb/ -name "*t_2000*" | sort -r | \
      parallel -P $num_cpu \
      $SPD_DIR/bin/therm_exp \
      $SPD_DIR/share/therm_vlbi.cnf \
      {} \
      /vlbi/solve/save_files/antenna-info.txt \
      /g5/therm_exp_eph/ate_ \
      1
find /g2/merra_heb/ -name "*t_19*" | sort -r | \
      parallel -P $num_cpu \
      $SPD_DIR/bin/therm_exp \
      $SPD_DIR/share/therm_vlbi.cnf \
      {} \
      /vlbi/solve/save_files/antenna-info.txt \
      /g5/therm_exp_eph/ate_ \
      1
