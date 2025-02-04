      PROGRAM    SPC_INQ
! ************************************************************************
! *                                                                      *
! *   Program SPC_INQ prints some parameters of spd_client installtion.  *
! *                                                                      *
! *  ### 11-JAN-2024    SPC_INQ    v1.1 (c)  L. Petrov  19-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd_local.i'
      CHARACTER  PAR*128, STR*128
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: spc_inq  --version|--prefix|--root|--bindir|--share|--doc|--petools'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, PAR )
      END IF
!
      IF ( PAR == 'version'   .OR. &
     &     PAR == '--version'      ) THEN
           WRITE ( 6, '(A)' ) SPD__VERSION
        ELSE IF ( PAR == 'prefix'       .OR. &
     &            PAR == 'spc_prefix'   .OR. &
     &            PAR == '--prefix'     .OR. &
     &            PAR == '--spc_prefix'      ) THEN
           WRITE ( 6, '(A)' ) SPD__PREFIX
        ELSE IF ( PAR == 'root'         .OR. &
     &            PAR == 'spc_root'     .OR. &
     &            PAR == '--root'       .OR. &
     &            PAR == '--spc_root'        ) THEN
           WRITE ( 6, '(A)' ) SPD__ROOT
        ELSE IF ( PAR == 'bindir'       .OR. &
     &            PAR == 'spc_bindir'   .OR. &
     &            PAR == '--bindir'     .OR. &
     &            PAR == '--spc_bindir'      ) THEN
           WRITE ( 6, '(A)' ) SPD__ROOT
        ELSE IF ( PAR == 'share'        .OR. &
     &            PAR == 'spc_share'    .OR. &
     &            PAR == '--share'      .OR. &
     &            PAR == '--spc_share'       ) THEN
           WRITE ( 6, '(A)' ) SPD__SHARE
        ELSE IF ( PAR == 'script'       .OR. &
     &            PAR == 'spc_script'   .OR. &
     &            PAR == '--share'      .OR. &
     &            PAR == '--spc_script'      ) THEN
           WRITE ( 6, '(A)' ) SPD__SCRIPT
        ELSE IF ( PAR == 'doc'          .OR. &
     &            PAR == 'spc_doc'      .OR. &
     &            PAR == '--doc'        .OR. &
     &            PAR == '--spc_doc'         ) THEN
           WRITE ( 6, '(A)' ) SPD__DOC
        ELSE IF ( PAR == 'petools'        .OR. &
     &            PAR == 'with-petools'   .OR. &
     &            PAR == '--petools'      .OR. &
     &            PAR == '--with-petools'      ) THEN
           WRITE ( 6, '(A)' ) SPC__PETOOLS
        ELSE
           WRITE ( 6, '(A)' ) 'Unsupported argument '//TRIM(PAR)
           WRITE ( 6, '(A)' ) 'Supported arguments: --version, --prefix, --root, --bindir, --share, --doc, --petools'
      END IF        
!
      END  PROGRAM  SPC_INQ  !#!#
