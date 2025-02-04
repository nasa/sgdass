class vsdc_config_class:
   def __init__ ( self, filename ):
       self.config_file        = filename
       self.client             = None
       self.vsdc_dir_name      = None
       self.ddf_dir            = None
       self.url_ddf_files      = None
       self.url_login          = None
       self.url_swin_login     = None
       self.url_submit         = None
       self.url_swin_submit    = None
       self.tar_swin_exclude   = []
       self.curl_extra_opts    = ""
       self.wget_extra_opts    = ""
       self.cddis_cookies      = None
       self.netrc_file         = None
       self.submit_log         = None
       self.master_dir         = None
       self.master_url         = None
       self.nscodes_url        = None
       self.nscodes_file       = None
       self.tmp_dir            = None
#
       self.ddf_file_list      = []
       self.ddf                = {}
       self.master             = {}
       self.nscodes            = {}

vsdc__label             = "VLBI submission to the VLBI Data Center utility"
vsdc__version           = "vsdc 1.22 2024.02.04"
vsdc__ddf_label         = "# CDDIS file definition  Version 1.0  of 2019.10.22"
vsdc__config_label      = "# VSDC_CONFIG file. Version 1.04 of 2021.05.10"
vsdc__nscodes_label     = "* ns-codes.txt 010717 format"
vsdc_swin_gentar__label = "vsdc_swin  1.18 of 2023.01.03"
vsdc__config_pars       = 18
vsdc__ddf_pars          = 14
vsdc__num_mf            = 16
vsdc__gnutar_minvers    = "1.20"
vsdc__master_year_1st   = 1979
vsdc__master_year_2nd   = 2023
vsdc__vgosda_magic      = "VGOSDA Format of 2019.09.09"
vsdc__swin_max_files_to_check = 16

vsdc__master_dir        = "/progs"

vsdc__vers              = vsdc__version.split()[2].replace(".","")

vsdc_data_formats  = [ "ascii", "swin", "vgosda", "vgosdb" ]
vsdc_master_suffix = [ ".txt", "-int.txt", "-vgos.txt", "-eu.txt" ]

ddf_list =       [ "session_aux_antcal.ddf", "data_swin.ddf", "session_aux_fullog.ddf", "session_aux_log.ddf" ]
ddf_local_list = [ "session_aux_antcal.ddf", "data_swin.ddf" ]

vsdc_exec_deps = ["curl", "wget", "ncdump"]
