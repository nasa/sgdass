!
!     common for srv_ subroutines which connect the chain routines with
!     the A900 server
!
      INTEGER*4 VC_socket_descriptor
      INTEGER*2 option(14)
      COMMON /SERVCOML/ VC_socket_descriptor,OPTION
