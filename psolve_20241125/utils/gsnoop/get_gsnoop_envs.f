      SUBROUTINE get_gsnoop_envs(root_dir,root_len)
!
!     purpose: return environment variables (or default parameters, if the
!     environment variable isn't set).
!
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
      INCLUDE 'solve.i'
      INCLUDE 'fclib.i'
!
!     input variables: none
!
!     output variables:
!
!       root_dir, root_len - overall root directory for the source
!            (e.g., /data11/mk4) and the directory's length
!
      character*(*) root_dir
      integer*2  root_len
!
!     local variables
!
      CHARACTER*64 CBUF
      INTEGER*2 IBUF(32)
      EQUIVALENCE (IBUF,CBUF)
      INTEGER*2 TRIMLEN
      INTEGER*4 IERR
      INTEGER*2 INT2_ARG
!
!     created: 11/1/2000 by kdb based on code from cutil/unpack_rmpar.f
!
      IERR=FC_GETENV ( PTR_CH ( 'PSOLVE_DIR'//CHAR(0) ), PTR_NC(IBUF) )
      IF ( IERR .GT. 0 ) THEN
           ROOT_DIR = CBUF(1:IERR)
         ELSE
           ROOT_DIR = PSOLVE_DIR
      ENDIF
      ROOT_LEN = TRIMLEN ( ROOT_DIR )
      IF ( ROOT_DIR(ROOT_LEN:ROOT_LEN) .NE. '/' ) THEN
           ROOT_LEN=ROOT_LEN+1
           ROOT_DIR(ROOT_LEN:ROOT_LEN) = '/'
      ENDIF
!
      RETURN
      END
