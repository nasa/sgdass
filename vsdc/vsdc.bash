#!/bin/bash -f
# ************************************************************************
# *                                                                      *
# *   Wrapper to vsdc.py                                                 *
# *                                                                      *
# *   Copyright (c) 1975-2025 United States Government as represented by *
# *   the Administrator of the National Aeronautics and Space            *
# *   Administration. All Rights Reserved.                               *
# *   License: NASA Open Source Software Agreement (NOSA).               *
# *                                                                      *
# *  ### 18-DEC-2021    vsdc.csh   v1.0 (d)  L. Petrov  31-MAR-2021 ###  *
# *                                                                      *
# ************************************************************************
#
vsdc_dir=/progs/vsdc_20211218
vsdc_exe=${vsdc_dir}/vsdc.py
#
${vsdc_exe} $@
