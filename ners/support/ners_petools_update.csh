#!/bin/csh -f
set PETOOLS_DIR = /progs/petools_20191020
set NERS_DIR    = /progs/ners_20191020
#
cp -p $PETOOLS_DIR/pet_util/atan_cs.f                    $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/binio.f                      $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/bspl.f                       $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/cha.f                        $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/date_to_time.f               $NERS_DIR/ners_petools/
# cp -p $PETOOLS_DIR/pet_util/ebspl.f                      $NERS_DIR/ners_petools/  # special handing
cp -p $PETOOLS_DIR/pet_util/ebspl_lsq_cns3.f             $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/ebspl_lsq_cns3_vec.f         $NERS_DIR/ners_petools/
# cp -p $PETOOLS_DIR/pet_util/error.f                      $NERS_DIR/ners_petools/  # special handing
cp -p $PETOOLS_DIR/pet_util/file_info.c                  $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/get_cdate.f                  $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/get_hr_timer.c               $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/get_system_constant.c        $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/get_unit.f                   $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/is_nan.f                     $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/ixmn8.f                      $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/jd_to_date.f                 $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/match_wild.f                 $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/mjdsec_to_date.f             $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/petools_set_alarm_handler.c  $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/probe_address.c              $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/rwfil.f                      $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/set_read_lock.f              $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/set_write_lock.f             $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/spl8.f                       $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/tai_to_tdb.f                 $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/tim_to_date.f                $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/tran.f                       $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/pet_util/wrapper.f                    $NERS_DIR/ners_petools/
#
cp -p $PETOOLS_DIR/matvec/addc_vv.f                      $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/matvec/dp_vv_v.f                      $NERS_DIR/ners_petools/
# cp -p $PETOOLS_DIR/matvec/mul_mm_ii_i.f                  $NERS_DIR/ners_petools/  # special handling
# cp -p $PETOOLS_DIR/matvec/mul_mv_iv_v.f                  $NERS_DIR/ners_petools/  # special handling
cp -p $PETOOLS_DIR/matvec/mul_vc_v.f                     $NERS_DIR/ners_petools/
cp -p $PETOOLS_DIR/matvec/vec.f                          $NERS_DIR/ners_petools/
