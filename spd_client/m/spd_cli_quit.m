function spd_quit ( spd_2p, cli_ptr )
% ************************************************************************
% *                                                                      *
% *   Routine spd_quit releases resources allocated for path delay       *
% *   computation.                                                       *
% * __________________________ Input parameters: _______________________ *
% *                                                                      *
% *   spd_2p    ( struct  ) -- Array of np data data structures for      *
% *                            transferring data to and back SPD server. *
% *   cli_ptr   ( pointer ) -- Pointer to the internal data structure    *
% *                            of the SPD_CLIENT.                        *
% *                                                                      *
% *  ###  27-APR-2015  spd_quit   v1.0 (c)  L. Petrov  27-APR-2015  ###  *
% *                                                                      *
% ************************************************************************
clear cli_ptr.value ;
clear cli_ptr ;
clear spd_2p ;
unloadlibrary ('libspc') ;
return ;
