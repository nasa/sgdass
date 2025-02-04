      PROGRAM    SOLVE_INQ
! ************************************************************************
! *                                                                      *
! *   Program SOLVE_INQ prints some parameters of pSolve installtion.    *
! *                                                                      *
! *  ### 22-DEC-2023   SOLVE_INQ   v1.0 (c)  L. Petrov  24-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      CHARACTER  PAR*128, STR*128
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: solve_inq --version|--prefix|--bindir|--root|--solve_dir|--spool|--work|--help|--cgm|--gvf|--solve_pima'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, PAR )
      END IF
!
      IF ( PAR == 'version'   .OR. &
     &     PAR == '--version'      ) THEN
           WRITE ( 6, '(A)' ) SOLVE_VERSION
        ELSE IF ( PAR == 'solve_install'      .OR. &
     &            PAR == 'solve_install_dir'  .OR. &
     &            PAR == 'install'            .OR. &
     &            PAR == 'psolve_install_dir' .OR. &
     &            PAR == 'psolve_install'     .OR. &
     &            PAR == '--solve_install'    .OR. &
     &            PAR == '--solve_prefix'     .OR. &
     &            PAR == '--prefix'                ) THEN
           WRITE ( 6, '(A)' ) PSOLVE_DIR
        ELSE IF ( PAR == 'bindir'       .OR.  &
     &            PAR == 'prog_dir'     .OR.  &
     &            PAR == 'solve_dir'    .OR.  &
     &            PAR == 'psolve_dir'   .OR.  &
     &            PAR == 'prog'         .OR.  &
     &            PAR == '--bindir'     .OR.  &
     &            PAR == '--prog_dir'   .OR.  &
     &            PAR == '--solve_dir'  .OR.  &
     &            PAR == '--psolve_dir' .OR.  &
     &            PAR == '--prog'             ) THEN
           WRITE ( 6, '(A)' ) SOLVE_PROG_DIR    
        ELSE IF ( PAR == 'solve_root'       .OR. &
     &            PAR == 'solve_root_dir'   .OR. &
     &            PAR == 'solve_source_dir' .OR. &
     &            PAR == 'root'             .OR. &
     &            PAR == 'psolve_root_dir'  .OR. &
     &            PAR == 'psolve_soure_dir' .OR. &
     &            PAR == 'psolve_root'      .OR. &
     &            PAR == '--solve_root'     .OR. &
     &            PAR == '--root'                ) THEN
           WRITE ( 6, '(A)' ) PSOLVE_ROOT
        ELSE IF ( PAR == 'solve_dir'   .OR. &
     &            PAR == 'psolve_dir'  .OR. &
     &            PAR == '--solve_dir' .OR. &
     &            PAR == '--psolve_dir'     ) THEN
           WRITE ( 6, '(A)' ) PSOLVE_DIR
        ELSE IF ( PAR == 'solve_work'        .OR. &
     &            PAR == 'solve_work_dir'    .OR. &
     &            PAR == 'psolve_work_dir'   .OR. &
     &            PAR == 'psolve_work'       .OR. &
     &            PAR == 'work'              .OR. &
                  PAR == '--solve_work'      .OR. &
     &            PAR == '--solve_work_dir'  .OR. &
     &            PAR == '--psolve_work_dir' .OR. &
     &            PAR == '--psolve_work'     .OR. &
     &            PAR == '--work'                 ) THEN
           WRITE ( 6, '(A)' ) SOLVE_WORK_DIR
        ELSE IF ( PAR == 'solve_save'        .OR. &
     &            PAR == 'solve_save_dir'    .OR. &
     &            PAR == 'save'              .OR. &
     &            PAR == 'psolve_save'       .OR. &
     &            PAR == 'psolve_save_dir'   .OR. &
     &            PAR == '--solve_save'      .OR. &
     &            PAR == '--solve_save_dir'  .OR. &
     &            PAR == '--psolve_save'     .OR. &
     &            PAR == '--psolve_save_dir' .OR. &
     &            PAR == '--save'                 ) THEN
           WRITE ( 6, '(A)' ) SOLVE_SAVE_DIR
        ELSE IF ( PAR == 'solve_help'        .OR. &
     &            PAR == 'solve_help_dir'    .OR. &
     &            PAR == 'psolve_help_dir'   .OR. &
     &            PAR == 'psolve_help'       .OR. &
     &            PAR == 'help'              .OR. &
     &            PAR == '--solve_help'      .OR. &
     &            PAR == '--solve_help_dir'  .OR. &
     &            PAR == '--psolve_help_dir' .OR. &
     &            PAR == '--psolve_help'     .OR. &
     &            PAR == '--help'                 ) THEN
           WRITE ( 6, '(A)' ) SOLVE_HELP_DIR
        ELSE IF ( PAR == 'solve_spool'        .OR. &
     &            PAR == 'solve_spool_dir'    .OR. &
     &            PAR == 'spool'              .OR. &
     &            PAR == 'psolve_spool'       .OR. &
     &            PAR == 'psolve_spool_dir'   .OR. &
     &            PAR == '--solve_spool'      .OR. &
     &            PAR == '--solve_spool_dir'  .OR. &
     &            PAR == '--spool'            .OR. &
     &            PAR == '--psolve_spool'     .OR. &
     &            PAR == '--psolve_spool_dir'      ) THEN
           WRITE ( 6, '(A)' ) SPOOL_DIR
        ELSE IF ( PAR == 'solve_scratch'      .OR. &
     &            PAR == 'scratch_dir'        .OR. &
     &            PAR == 'scratch'            .OR. &
     &            PAR == 'psolve_scratch'     .OR. &
     &            PAR == 'psolve_scratch_dir' .OR. &
     &            PAR == '--solve_scratch'    .OR. &
     &            PAR == '--scratch'               ) THEN
           WRITE ( 6, '(A)' ) SCRATCH_DIR
        ELSE IF ( PAR == 'solve_cgm'      .OR. &
     &            PAR == 'solve_cgm_dir'  .OR. &
     &            PAR == 'cgm'            .OR. &
     &            PAR == 'psolve_cgm_dir' .OR. &
     &            PAR == 'psolve_cgm'     .OR. &
     &            PAR == '--solve_cgm'    .OR. &
     &            PAR == '--cgm'               ) THEN
           WRITE ( 6, '(A)' ) SCRATCH_DIR
        ELSE IF ( PAR == 'solve_gvf'      .OR. &
     &            PAR == 'solve_gvf_dir'  .OR. &
     &            PAR == 'psolve_gvf_dir' .OR. &
     &            PAR == 'psolve_gvf'     .OR. & 
     &            PAR == 'gvf'            .OR. & 
     &            PAR == '--solve_gvf'    .OR. &
     &            PAR == '--gvf'               ) THEN
           WRITE ( 6, '(A)' ) SOLVE_GVF_DIR
        ELSE IF ( PAR == 'solve_pima'      .OR. &
     &            PAR == 'solve_pima_dir'  .OR. &
     &            PAR == 'pima'            .OR. &
     &            PAR == 'psolve_pima_dir' .OR. &
     &            PAR == 'psolve_pima'     .OR. &
     &            PAR == '--solve_pima'         ) THEN
           WRITE ( 6, '(A)' ) SOLVE_PIMA_DIR
        ELSE
           WRITE ( 6, '(A)' ) 'Unsupported argument '//TRIM(PAR)
           WRITE ( 6, '(A)' ) 'Supported arguments: --version, --prefix, --bindir, --root, --solve_dir, --spool, --work, --help, --cgm, --gvf, --solve_pima'
      END IF
!
      END  PROGRAM   SOLVE_INQ  !#!#
