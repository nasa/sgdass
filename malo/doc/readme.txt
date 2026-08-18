How to compute atmospheric pressure loading?

1) to compute surface time series

   geosit_to_spr_all.csh

2) Compute the surface pressure model:

   spr_model /imls/spr_2699/geosit/ geosit 1998.01.01 2024.12.18 2 /imls/devel_model/spr_atm_geosit_model_19980101_20241218_d2699.heb




==================================================================

To get data

Run trial command:

/progs/malo_20241125/script/geos_oper.py -c /progs/malo_20241125/share/gen_heb_lws_geosit.conf 4

Adjust in /progs/malo_20241125/share/gen_heb_lws_geosit.conf 

end_date:        
look_back_days:  

Run 

/progs/malo_20241125/script/geos_oper.py -c /progs/malo_20241125/share/gen_heb_lws_geosit.conf 1 >& tee /tmp/sa_get_twland.log &
