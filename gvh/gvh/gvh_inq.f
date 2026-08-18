      PROGRAM    GVH_INQ
      IMPLICIT   NONE 
      INCLUDE   'gvh_local.i'
      CHARACTER  PAR*128, STR*128
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: pima_inq  --version|--prefix|--bindir|--root|--exp_dir|--fits_dir|--share_dir|--scratch_dir'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, PAR )
      END IF
!
      IF ( PAR == 'version'   .OR. &
     &     PAR == '--version'      ) THEN
           WRITE ( 6, '(A)' ) GVH__VERSION
        ELSE IF ( PAR == 'prefix'   .OR.  &
     &            PAR == '--prefix'       ) THEN
           WRITE ( 6, '(A)' ) GVH__PREFIX
        ELSE IF ( PAR == 'bindir'   .OR.  &
     &            PAR == '--bindir'       ) THEN
           WRITE ( 6, '(A)' ) GVH__PREFIX//'/bin'
        ELSE IF ( PAR == 'libdir'   .OR.  &
     &            PAR == '--libdir'       ) THEN
           WRITE ( 6, '(A)' ) GVH__PREFIX//'/lib'
        ELSE IF ( PAR == 'root'   .OR.  &
     &            PAR == '--root'       ) THEN
           WRITE ( 6, '(A)' ) GVH__ROOT
        ELSE
           WRITE ( 6, '(A)' ) 'Unsupported argument '//TRIM(PAR)
           WRITE ( 6, '(A)' ) 'Supported arguments: --version, --root, --prefix, --bindir, --libdir'
      END IF
!
      END  PROGRAM   GVH_INQ  !#!#


