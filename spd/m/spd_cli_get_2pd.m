function [spd_2p, iuer] = spd_cli_get_2pd ( np, spd_2p, cli_ptr, ivrb )
% ************************************************************************
% *                                                                      *
% *   Routine spd_cli_get_2pd communicates with the SPD server. It sends *
% *   the request to compute path delays, waits for processing the       *
% *   quest and returns the results.                                     *
% *                                                                      *
% *   Array of data structures spd_2p is used for transport of time      *
% *   tags, coordinates of the emitter and the receiver as input and     *
% *   path delay and its partial derivatives as the output.              *
% *                                                                      *
% *   Meaning of the spd_2p array fields:                                *
% *                                                                      *
% *   spd_2p(k).tai           TAI time after the midnight, in seconds.   *
% *   spd_2p(k).emi_1         X-coordinate of emitter position in the    *
% *                           crust fixed coordinate system, in meters.  *
% *   spd_2p(k).emi_2         Y-coordinate of emitter position in the    *
% *                           crust fixed coordinate system, in meters.  *
% *   spd_2p(k).emi_3         Z-coordinate of emitter position in the    *
% *                           crust fixed coordinate system, in meters.  *
% *   spd_2p(k).rec_1         X-coordinate of receiver position in the   *
% *                           crust fixed coordinate system, in meters.  *
% *   spd_2p(k).rec_2         Y-coordinate of receiver position in the   *
% *                           crust fixed coordinate system, in meters.  *
% *   spd_2p(k).rec_3         Z-coordinate of receiver position in the   *
% *                           crust fixed coordinate system, in meters.  *
% *   spd_2p(k).del_1st       Path delay at 1st wavelength, in meters.   *
% *   spd_2p(k).del_2nd       Path delay at 2nd wavelength, in meters.   *
% *   spd_2p(k).del_rder_1st  Partial derivative of path delay at 1st    *
% *                           wavelength on the height of the receiver.  *
% *   spd_2p(k).del_rder_2nd  Partial derivative of path delay at 2nd    *
% *                           wavelength on the height of the receiver.  *
% *   spd_2p(k).del_eder_1st  Partial derivative of path delay at 1st    *
% *                           wavelength on the height of the emitter.   *
% *   spd_2p(k).del_eder_2nd  Partial derivative of path delay at 2nd    *
% *                           wavelength on the height of the emitter.   *
% *   spd_2p(k).mjd           Integer (sic!) Modified Juilan Date on the *
% *                           midnight preceeding the event.             *
% *   spd_2p(k).filler_1      4-bite long filler.                        *
% *                                                                      *
% * __________________________ Input parameters: _______________________ *
% *                                                                      *
% *   np           ( int  ) -- maximum size of the buffer spd_2p for     *
% *                            transferring parameters to and back       *
% *                            the SPD server.                           *
% *   spd_2p    ( struct  ) -- Array of np data data structures for      *
% *                            transferring data to andback SPD server.  *
% *   cli_ptr   ( pointer ) -- Pointer to the internal data structure    *
% *                            of the SPD_CLIENT.                        *
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
% *   iuer      ( int     ) -- Error parameter.                          *
% *                            0 -- no mistakes.                         *
% *                            1 -- fauilure to communicate with the     *
% *                                 server.                              *
% *                                                                      *
% * ### 07-MAR-2015  spd_cli_get_2pd  v1.1 (c) L. Petrov 29-APR-2015 ### *
% *                                                                      *
% ************************************************************************

%
% --- Call spd_cli_get_2p
%
   spd_2p_ptr = libpointer ( 'struct_spd_2p', spd_2p ) ;
   iuer = -1 ;
   np_ptr   = libpointer ( 'int32Ptr', np ) ;
   ivrb_ptr = libpointer ( 'int32Ptr', ivrb ) ;
   iuer_ptr = libpointer ( 'int32Ptr', iuer ) ;
   %% fprintf ( ' np = %d, ivrb = %d, iuer= %d \n', np_ptr.value, ivrb_ptr.value, iuer_ptr.value ) ;
   calllib ( 'libspc', 'spd_cli_get_2pd', ...
                        cli_ptr, ...	    
                        np_ptr, ...
                        spd_2p_ptr, ...
                        ivrb_ptr, ...
                        iuer_ptr ) ;
%
% --- Check return value of iuer
%
   if get(iuer_ptr,'Value') ~= 0 
      return ;
   end 
   if ivrb > 0
      fprintf ( 'Extracting path delays...\n' )
   end
%
% --- Extract parameters from the pointer.
% --- This is a special trick to handle the deficiency of 
% --- MATLAB design
%
   for ind=1:np
       tmp_ptr = spd_2p_ptr + (ind-1) ; 
       spd_2p(ind).tai   = tmp_ptr.Value.tai   ;
       spd_2p(ind).mjd   = tmp_ptr.Value.mjd   ;
       spd_2p(ind).emi_1 = tmp_ptr.Value.emi_1 ;
       spd_2p(ind).emi_2 = tmp_ptr.Value.emi_2 ;
       spd_2p(ind).emi_3 = tmp_ptr.Value.emi_3 ;
       spd_2p(ind).rec_1 = tmp_ptr.Value.rec_1 ;
       spd_2p(ind).rec_2 = tmp_ptr.Value.rec_2 ;
       spd_2p(ind).rec_3 = tmp_ptr.Value.rec_3 ;
       spd_2p(ind).del_1st = tmp_ptr.Value.del_1st ;
       spd_2p(ind).del_2nd = tmp_ptr.Value.del_2nd ;
       spd_2p(ind).del_rder_1st = tmp_ptr.Value.del_rder_1st ;
       spd_2p(ind).del_rder_2nd = tmp_ptr.Value.del_rder_2nd ;
       spd_2p(ind).del_eder_1st = tmp_ptr.Value.del_eder_1st ;
       spd_2p(ind).del_eder_2nd = tmp_ptr.Value.del_eder_2nd ;
    end ;
    iuer = 0 ;
return
