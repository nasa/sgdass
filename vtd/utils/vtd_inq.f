      PROGRAM     VTD_INQ
! ************************************************************************
! *                                                                      *
! *   Prgoram VTD_INQ prints some parameters of VTD installation.        *
! *                                                                      *
! *  ### 23-DEC-2023    VTD_INQ    v1.1 (c)  L. Petrov  19-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE 
      INCLUDE    'vtd_local.i'
      CHARACTER  PAR*32
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: vtd_inq  --version|--prefix|--bindir|--root|--data|--doc'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, PAR )
      END IF
!
      IF ( PAR == 'version'   .OR. &
     &     PAR == '--version'      ) THEN
           WRITE ( 6, '(A)' ) VTD__VERSION
        ELSE IF ( PAR == 'prefix'    .OR. &
     &            PAR == '--prefix'  .OR. &
     &            PAR == 'vtd_prefix'     ) THEN
           WRITE ( 6, '(A)' ) VTD__PREFIX
        ELSE IF ( PAR == 'bindir'    .OR. &
     &            PAR == '--bindir'  .OR. &
     &            PAR == 'vtd_bindir'     ) THEN
           WRITE ( 6, '(A)' ) VTD__PREFIX//'/bin'
        ELSE IF ( PAR == 'root'    .OR. &
     &            PAR == '--root'  .OR. &
     &            PAR == 'vtd_root'     ) THEN
           WRITE ( 6, '(A)' ) VTD__ROOT
        ELSE IF ( PAR == 'data'    .OR. &
     &            PAR == '--data'  .OR. &
     &            PAR == 'vtd_data'     ) THEN
           WRITE ( 6, '(A)' ) VTD__DATA
        ELSE IF ( PAR == 'doc'    .OR. &
     &            PAR == '--doc'  .OR. &
     &            PAR == 'vtd_doc'     ) THEN
           WRITE ( 6, '(A)' ) VTD__DOC
        ELSE
           WRITE ( 6, '(A)' ) 'Unsupported argument '//TRIM(PAR)
           WRITE ( 6, '(A)' ) 'Supported arguments: --version, --prefix, --bindir, --root, --data, --doc'
      END IF
!
      END PROGRAM VTD_INQ  !#!#
