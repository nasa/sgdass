      PROGRAM    PIMA_INQ
! ************************************************************************
! *                                                                      *
! *   Program PIMA_INQ prints some parameters of PIMA installtion.       *
! *                                                                      *
! *  ### 16-JAN-2024   PIMA_INQ   v1.0 (c)  L. Petrov  16-JAN-2024  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima_local.i'
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
           WRITE ( 6, '(A)' ) PIMA__VERSION
        ELSE IF ( PAR == 'prefix'   .OR.  &
     &            PAR == '--prefix'       ) THEN
           WRITE ( 6, '(A)' ) PIMA__PREFIX
        ELSE IF ( PAR == 'bindir'   .OR.  &
     &            PAR == '--bindir'       ) THEN
           WRITE ( 6, '(A)' ) PIMA__PREFIX//'/bin'
        ELSE IF ( PAR == 'root'   .OR.  &
     &            PAR == '--root'       ) THEN
           WRITE ( 6, '(A)' ) PIMA__ROOT
        ELSE IF ( PAR == 'exp_dir'   .OR.  &
     &            PAR == '--exp_dir'       ) THEN
           WRITE ( 6, '(A)' ) PIMA__EXP_DIR
        ELSE IF ( PAR == 'fits_dir'   .OR.  &
     &            PAR == '--fits_dir'       ) THEN
           WRITE ( 6, '(A)' ) PIMA__FITS_DIR
        ELSE IF ( PAR == 'share_dir'   .OR.  &
     &            PAR == '--share_dir'       ) THEN
           WRITE ( 6, '(A)' ) PIMA__SHARE_DIR
        ELSE IF ( PAR == 'scratch_dir'   .OR.  &
     &            PAR == '--scratch_dir'       ) THEN
           WRITE ( 6, '(A)' ) PIMA__SCRATCH_DIR
        ELSE
           WRITE ( 6, '(A)' ) 'Unsupported argument '//TRIM(PAR)
           WRITE ( 6, '(A)' ) 'Supported arguments: --version, --prefix, --bindir, --root, --exp_dir, --fits_dir, --share_dir, --scratch_dir'
      END IF
!
      END  PROGRAM   PIMA_INQ  !#!#
