#!/bin/csh -f
set from = ""
set to   = "sa:"
#
if ( $to == "aw:" ) then
     rsync -av  ${from}/imls/load_grid/lws/geosit/*.bz2      ${to}/imls/load_grid/lws/geosit/
     rsync -av "${from}/imls/load_har_grid/lws/geosit/"      ${to}/imls/load_har_grid/lws/geosit/
     rsync -av "${from}/imls/load_har_list/lws/geosit/"      ${to}/imls/load_har_list/lws/geosit/
     rsync -av "${from}/imls/load_int/lws/geosit/"           ${to}/imls/load_int/lws/geosit/
     rsync -av "${from}/imls/load_list/lws/geosit/"          ${to}/imls/load_list/lws/geosit/
     rsync -av "${from}/imls/vgep/lws/geosit/"               ${to}/imls/vgep/lws/geosit/
     rsync -av  ${from}/imls/load_d1_grid/lws/geosit/*.bz2   ${to}/imls/load_d1_grid/lws/geosit/
     rsync -av "${from}/imls/load_d1_har_grid/lws/geosit/"   ${to}/imls/load_d1_har_grid/lws/geosit/
     rsync -av "${from}/imls/load_d1_har_list/lws/geosit/"   ${to}/imls/load_d1_har_list/lws/geosit/
     rsync -av "${from}/imls/load_d1_list/lws/geosit/"       ${to}/imls/load_d1_list/lws/geosit/
     rsync -av "${from}/imls/load_d1_int/lws/geosit/"        ${to}/imls/load_d1_int/lws/geosit/
#
     rsync -av  ${from}/imls/load_grid/atm/geosit/*.bz2      ${to}/imls/load_grid/atm/geosit/
     rsync -av "${from}/imls/load_har_grid/atm/geosit/"      ${to}/imls/load_har_grid/atm/geosit/
     rsync -av "${from}/imls/load_har_list/atm/geosit/"      ${to}/imls/load_har_list/atm/geosit/
     rsync -av "${from}/imls/load_int/atm/geosit/"           ${to}/imls/load_int/atm/geosit/
     rsync -av "${from}/imls/load_list/atm/geosit/"          ${to}/imls/load_list/atm/geosit/
     rsync -av "${from}/imls/vgep/atm/geosit/"               ${to}/imls/vgep/atm/geosit/
     rsync -av  ${from}/imls/load_d1_grid/atm/geosit/*.bz2   ${to}/imls/load_d1_grid/atm/geosit/
     rsync -av "${from}/imls/load_d1_har_grid/atm/geosit/"   ${to}/imls/load_d1_har_grid/atm/geosit/
     rsync -av "${from}/imls/load_d1_har_list/atm/geosit/"   ${to}/imls/load_d1_har_list/atm/geosit/
     rsync -av "${from}/imls/load_d1_list/atm/geosit/"       ${to}/imls/load_d1_list/atm/geosit/
     rsync -av "${from}/imls/load_d1_int/atm/geosit/"        ${to}/imls/load_d1_int/atm/geosit/
#
     rsync -av  ${from}/imls/load_grid/nto/mpiom07/*.bz2     ${to}/imls/load_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_har_grid/nto/mpiom07/"     ${to}/imls/load_har_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_har_list/nto/mpiom07/"     ${to}/imls/load_har_list/nto/mpiom07/
     rsync -av "${from}/imls/load_int/nto/mpiom07/"          ${to}/imls/load_int/nto/mpiom07/
     rsync -av "${from}/imls/load_list/nto/mpiom07/"         ${to}/imls/load_list/nto/mpiom07/
     rsync -av "${from}/imls/vgep/nto/mpiom07/"              ${to}/imls/vgep/nto/mpiom07/
     rsync -av  ${from}/imls/load_d1_grid/nto/mpiom07/*.bz2  ${to}/imls/load_d1_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_har_grid/nto/mpiom07/"  ${to}/imls/load_d1_har_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_har_list/nto/mpiom07/"  ${to}/imls/load_d1_har_list/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_list/nto/mpiom07/"      ${to}/imls/load_d1_list/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_int/nto/mpiom07/"       ${to}/imls/load_d1_int/nto/mpiom07/
#
     rsync -av  ${from}/imls/load_grid/nto/mpiom06/*.bz2     ${to}/imls/load_grid/nto/mpiom06/
     rsync -av "${from}/imls/load_har_grid/nto/mpiom06/"     ${to}/imls/load_har_grid/nto/mpiom06/
     rsync -av "${from}/imls/load_har_list/nto/mpiom06/"     ${to}/imls/load_har_list/nto/mpiom06/
     rsync -av "${from}/imls/load_int/nto/mpiom06/"          ${to}/imls/load_int/nto/mpiom06/
     rsync -av "${from}/imls/load_list/nto/mpiom06/"         ${to}/imls/load_list/nto/mpiom06/
     rsync -av "${from}/imls/vgep/nto/mpiom06/"              ${to}/imls/vgep/nto/mpiom06/
     rsync -av  ${from}/imls/load_d1_grid/nto/mpiom06/*.bz2  ${to}/imls/load_d1_grid/nto/mpiom06/
     rsync -av "${from}/imls/load_d1_har_grid/nto/mpiom06/"  ${to}/imls/load_d1_har_grid/nto/mpiom06/
     rsync -av "${from}/imls/load_d1_har_list/nto/mpiom06/"  ${to}/imls/load_d1_har_list/nto/mpiom06/
     rsync -av "${from}/imls/load_d1_list/nto/mpiom06/"      ${to}/imls/load_d1_list/nto/mpiom06/
     rsync -av "${from}/imls/load_d1_int/nto/mpiom06/"       ${to}/imls/load_d1_int/nto/mpiom06/
  else
#
     rsync -av "${from}/imls/load_grid/nto/mpiom07/"         ${to}/imls/load_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_bds/nto/mpiom07/"          ${to}/imls/load_bds/nto/mpiom07/
     rsync -av "${from}/imls/load_grid/nto/mpiom07/"         ${to}/imls/load_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_har_grid/nto/mpiom07/"     ${to}/imls/load_har_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_har_list/nto/mpiom07/"     ${to}/imls/load_har_list/nto/mpiom07/
     rsync -av "${from}/imls/load_har_spl/nto/mpiom07/"      ${to}/imls/load_har_spl/nto/mpiom07/
     rsync -av "${from}/imls/load_int/nto/mpiom07/"          ${to}/imls/load_int/nto/mpiom07/
     rsync -av "${from}/imls/load_list/nto/mpiom07/"         ${to}/imls/load_list/nto/mpiom07/
     rsync -av "${from}/imls/load_spl/nto/mpiom07/"          ${to}/imls/load_spl/nto/mpiom07/
     rsync -av "${from}/imls/vgep/nto/mpiom07/"              ${to}/imls/vgep/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_grid/nto/mpiom07/"      ${to}/imls/load_d1_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_har_grid/nto/mpiom07/"  ${to}/imls/load_d1_har_grid/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_har_list/nto/mpiom07/"  ${to}/imls/load_d1_har_list/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_har_spl/nto/mpiom07/"   ${to}/imls/load_d1_har_spl/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_int/nto/mpiom07/"       ${to}/imls/load_d1_int/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_list/nto/mpiom07/"      ${to}/imls/load_d1_list/nto/mpiom07/
     rsync -av "${from}/imls/load_d1_spl/nto/mpiom07/"       ${to}/imls/load_d1_spl/nto/mpiom07/
#
     rsync -av "${from}/imls/load_bds/lws/geosit/"           ${to}/imls/load_bds/lws/geosit/
     rsync -av "${from}/imls/load_grid/lws/geosit/"          ${to}/imls/load_grid/lws/geosit/
     rsync -av "${from}/imls/load_har_grid/lws/geosit/"      ${to}/imls/load_har_grid/lws/geosit/
     rsync -av "${from}/imls/load_har_list/lws/geosit/"      ${to}/imls/load_har_list/lws/geosit/
     rsync -av "${from}/imls/load_har_spl/lws/geosit/"       ${to}/imls/load_har_spl/lws/geosit/
     rsync -av "${from}/imls/load_int/lws/geosit/"           ${to}/imls/load_int/lws/geosit/
     rsync -av "${from}/imls/load_list/lws/geosit/"          ${to}/imls/load_list/lws/geosit/
     rsync -av "${from}/imls/load_spl/lws/geosit/"           ${to}/imls/load_spl/lws/geosit/
     rsync -av "${from}/imls/vgep/lws/geosit/"               ${to}/imls/vgep/lws/geosit/
     rsync -av "${from}/imls/load_d1_grid/lws/geosit/"       ${to}/imls/load_d1_grid/lws/geosit/
     rsync -av "${from}/imls/load_d1_har_grid/lws/geosit/"   ${to}/imls/load_d1_har_grid/lws/geosit/
     rsync -av "${from}/imls/load_d1_har_list/lws/geosit/"   ${to}/imls/load_d1_har_list/lws/geosit/
     rsync -av "${from}/imls/load_d1_har_spl/lws/geosit/"    ${to}/imls/load_d1_har_spl/lws/geosit/
     rsync -av "${from}/imls/load_d1_int/lws/geosit/"        ${to}/imls/load_d1_int/lws/geosit/
     rsync -av "${from}/imls/load_d1_list/lws/geosit/"       ${to}/imls/load_d1_list/lws/geosit/
     rsync -av "${from}/imls/load_d1_spl/lws/geosit/"        ${to}/imls/load_d1_spl/lws/geosit/
#
     rsync -av "${from}/imls/load_bds/atm/geosit/"           ${to}/imls/load_bds/atm/geosit/
     rsync -av "${from}/imls/load_grid/atm/geosit/"          ${to}/imls/load_grid/atm/geosit/
     rsync -av "${from}/imls/load_har_grid/atm/geosit/"      ${to}/imls/load_har_grid/atm/geosit/
     rsync -av "${from}/imls/load_har_list/atm/geosit/"      ${to}/imls/load_har_list/atm/geosit/
     rsync -av "${from}/imls/load_har_spl/atm/geosit/"       ${to}/imls/load_har_spl/atm/geosit/
     rsync -av "${from}/imls/load_int/atm/geosit/"           ${to}/imls/load_int/atm/geosit/
     rsync -av "${from}/imls/load_list/atm/geosit/"          ${to}/imls/load_list/atm/geosit/
     rsync -av "${from}/imls/load_spl/atm/geosit/"           ${to}/imls/load_spl/atm/geosit/
     rsync -av "${from}/imls/vgep/atm/geosit/"               ${to}/imls/vgep/atm/geosit/
     rsync -av "${from}/imls/load_d1_grid/atm/geosit/"       ${to}/imls/load_d1_grid/atm/geosit/
     rsync -av "${from}/imls/load_d1_har_grid/atm/geosit/"   ${to}/imls/load_d1_har_grid/atm/geosit/
     rsync -av "${from}/imls/load_d1_har_list/atm/geosit/"   ${to}/imls/load_d1_har_list/atm/geosit/
     rsync -av "${from}/imls/load_d1_har_spl/atm/geosit/"    ${to}/imls/load_d1_har_spl/atm/geosit/
     rsync -av "${from}/imls/load_d1_int/atm/geosit/"        ${to}/imls/load_d1_int/atm/geosit/
     rsync -av "${from}/imls/load_d1_list/atm/geosit/"       ${to}/imls/load_d1_list/atm/geosit/
     rsync -av "${from}/imls/load_d1_spl/atm/geosit/"        ${to}/imls/load_d1_spl/atm/geosit/
#
     rsync -av --exclude "*geosfpit*" "${from}/imls/aam/"    ${to}/imls/aam/
     rsync -av --exclude "*geosfpit*" "${from}/spd/asc/"     ${to}/spd/asc/
     rsync -av --exclude "*geosfpit*" "${from}/spd/bin/"     ${to}/spd/bin/
endif
