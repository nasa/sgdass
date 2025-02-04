function [spd_2p, cli_ptr, iuer] = spd_cli_init ( np, cli_conf, mjd_beg, ...
                                                  tai_beg, mjd_end, tai_end, ivrb )
% ************************************************************************
% *                                                                      *
% *   Routine spd_init initialzies interface to SPD client. It creates   *
% *   an array of structrues spd_2p that is used for transport of        *
% *   time tag, coordinates of the emitter andthe receiver as input and  *
% *   path delay and its patial derivatives as the output. Then spd_init *
% *   checkes whether the SPD server is running. If the server is not    *
% *   not running, spd_init issues and error message and sets iuer>0.    *
% *                                                                      *
% * __________________________ Input parameters: _______________________ *
% *                                                                      *
% *   np           ( int  ) -- maximum size of the buffer spd_2p for     *
% *                            transferring parameters to and back       *
% *                            the SPD server.                           *
% *   cli_conf     ( char ) -- name of the configuration file for        *
% *                            spd_client.                               *
% *   spd_lib_path ( char ) -- Installation prefix of the SPD_CLIENT     *
% *                            library.                                  *
% *   mjd_beg      ( int  ) -- MJD of the beginning the interval for     *
% *                            path delay computation.                   *
% *   tai_beg      ( int  ) -- TAI time of the beginning the interval    *
% *                            for path delay computation.               *
% *   mjd_end      ( int  ) -- MJD of the end of the interval for        *
% *                            path delay computation.                   *
% *   tai_end      ( int  ) -- TAI time of the end of the interval       *
% *                            for path delay computation.               *
% *   ivrb         ( int  ) -- Verbosity parameter.                      *
% *                            0 -- silent mode. Only error messages are *
% *                                 printed.                             *
% *                            1 -- normal verbosity. Progress messages  *
% *                                 are printed.                         *
% *                            2 -- debugging mode.                      *
% *                                                                      *
% * __________________________ Output parameters: ______________________ *
% *                                                                      *
% *   spd_2p    ( struct  ) -- Array of np data data structures for      *
% *                            transferring data to andback SPD server.  *
% *  cli_ptr    ( pointer ) -- Pointer to the internal data structure    *
% *                            of the SPD_CLIENT.                        *
% *  iuer       ( int     ) -- Error parameter.                          *
% *                            0 -- no mistakes.                         *
% *                            1 -- fauilure to communicate with the     *
% *                                 server.                              *
% *                                                                      *
% *   Important: the following command should be exected *before*        *
% *   calling MATLAB:                                                    *
% *                                                                      *
% *   setenv SPD_LIB_DIR  $OPT/lib                                       *
% *   setenv LIB_GFORTRAN $SPD_LIB_DIR/libgfortran.3.dylib               *
% *   setenv LIB_NETCDF   $SPD_LIB_DIR/libnetcdf.7.dylib                 *
% *   setenv LIB_PETOOLS  $SPD_LIB_DIR/libpetools.1.dylib                *
% *   setenv LIB_SPC      $SPD_LIB_DIR/libspc.1.dylib                    *
% *   setenv DYLD_INSERT_LIBRARIES ${LIB_SPD}:${LIB_NETCDF}:${LIB_PETOOLS}:${LIB_GFORTRAN}
% *   (Linux: DYLD_INSERT_LIBRARIES should be replaved with LD_PRELOAD)  *
% *                                                                      *
% *   Otherwise, MATLAB will load its own library version that are       *
% *   incompatible with package SPD.                                     *
% *                                                                      *
% *  ### 07-MAR-2015   spd_cli_init v1.1 (c)  L. Petrov  29-APR-2015 ### *
% *                                                                      *
% ************************************************************************
%
   spd_path = '????' ;
%
% --- Search SPD_PATH definition in the SPD_CLIENT configuration file
%
   txt = fileread ( cli_conf );
   strings = strsplit ( txt, '\n' ) ;
   for k=1:length(strings)-1
       words = strsplit ( char(strings(k)) ) ;
       if length(words) > 1 
          if isequal ( char(words(1)), 'SPD_PATH:' )
             spd_path = char(words(2)) ;
          end
       end
   end
   if isequal ( spd_path, '????' )
      fprintf ( 'SPD_PATH: word was not defined in the configuration file %s\n', cli_conf );
      iuer = 601
      return
   end
%
% --- Set up environment variables for Fortran
%
      setenv ( 'GFORTRAN_STDIN_UNIT',  '5' ) 
      setenv ( 'GFORTRAN_STDOUT_UNIT', '6' ) 
      setenv ( 'GFORTRAN_STDERR_UNIT', '0')
%
% --- Load library spd
%
      spd_lib_path = strcat(spd_path,'/lib') ;
      spd_inc_path = strcat(spd_path,'/include') ;
      addpath ( fullfile(spd_lib_path) ) ;
      addpath ( fullfile(spd_inc_path) ) ;
      loadlibrary ( 'libspc', 'spc.h' ) ;
%
% --- Get the length of cli data structure
%
      cli_len = calllib ( 'libspc', 'spd_cli_len' ) ;
      cli = zeros( 1, cli_len ) ;
%
% --- Allocate memory for spd_2p data structure array
%
      spd_2p = repmat( struct ( 'tai',     0.0,      ...
                                'emi_1',   0.0,      ...
                                'emi_2',   0.0,      ...
                                'emi_3',   0.0,      ...
                                'rec_1',   0.0,      ...
                                'rec_2',   0.0,      ...
                                'rec_3',   0.0,      ...
                                'del_1st', 0.0,      ...
                                'del_2nd', 0.0,      ...
                                'del_rder_1st', 0.0, ...
                                'del_rder_2nd', 0.0, ...
                                'del_eder_1st', 0.0, ...
                                'del_eder_2nd', 0.0, ...
                                'mjd',      0,       ...
                                'filler_1', 0        ...
                               ), 1, np ) ;
      
%
% --- Initialize internal data structures of the SPD client
%
      iuer = -1 ;
      ivrb_ptr = libpointer ( 'int32Ptr', ivrb ) ;
      cli_ptr  = libpointer ( 'int32Ptr', cli  ) ;
      iuer_ptr = libpointer ( 'int32Ptr', iuer ) ;
      calllib ( 'libspc', 'spd_cli_init', libpointer ( 'stringPtr', cli_conf ), ... 
                                          cli_ptr,  ...
                                          iuer_ptr, ...
                                          int32(length(cli_conf)) ) ;
      if get(iuer_ptr,'Value') ~= 0
         fprintf ( '\n Failure in parsing SPD Cliebnt configuration file\n' ) ;
	 iuer = 1
         return
      end 
%
% --- Send ping request to the SPD server and listen to the resonse.
% --- Normally, it should reuturn OK.
%
      iuer = -1 ;
      iuer_ptr = libpointer ( 'int32Ptr', iuer ) ;
      calllib ( 'libspc', 'spd_cli_ping', cli_ptr, ivrb_ptr, iuer_ptr ) ;
      if get(iuer_ptr,'Value') ~= 0 
         fprintf ( '\n Failure in connecting to the path delay server iuer= %d\n', iuer ) ;
         return
      end 
%
% --- Fill spd_2p array with zeroes. This is needed in order to
% --- overcome some obscure limitations of MATLAB
%
      for k = 1:np
          spd_2p(k).tai = tai_beg + (k-1)*0.01 ;
          spd_2p(k).emi_1 = 0.0 ;
          spd_2p(k).emi_2 = 0.0 ;
          spd_2p(k).emi_3 = 0.0 ;
          spd_2p(k).rec_1 = 0.0 ;
          spd_2p(k).rec_2 = 0.0 ;
          spd_2p(k).rec_3 = 0.0 ;
          spd_2p(k).del_1st  = 0.0 ;
          spd_2p(k).del_2nd = 0.0 ;
          spd_2p(k).del_rder_1st = 0.0 ;
          spd_2p(k).del_rder_2nd = 0.0 ;
          spd_2p(k).del_eder_1st = 0.0 ;
          spd_2p(k).del_eder_2nd = 0.0 ;
          spd_2p(k).mjd = mjd_beg ;
          spd_2p(k).filler_1 = 0     ;
      end ;

      iuer = 0 ;

return
