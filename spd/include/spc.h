struct struct_spd_2p {
    double tai ;
    double emi_1 ;
    double emi_2 ;
    double emi_3 ;
    double rec_1 ;
    double rec_2 ;
    double rec_3 ;
    double del_1st ;
    double del_2nd ;
    double del_rder_1st ;
    double del_rder_2nd ;
    double del_eder_1st ;
    double del_eder_2nd ;
    int    mjd ;
    int    filler_1 ;
};

int  spd_cli_len          ( );
void spd_cli_init         ( char* cli_conf, int* cli, int* iuer, int len_cli_conf );
void spd_cli_ping         ( int* cli, int* ivrb, int* iuer );
void spd_cli_get_2pd      ( int* cli, int* n_del, struct struct_spd_2p * spd_2p, int* ivrb, int* iuer );
void get_spd_cli_lib_path ( int* spd_cli, char * spd_cli_lib_path, int len_spd_cli_lib_path ) ;

int spd__size = 4096/4;
