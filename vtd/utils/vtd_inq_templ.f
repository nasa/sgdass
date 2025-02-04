      PROGRAM     VTD_INQ
! ************************************************************************
! *                                                                      *
! *   Prgoram VTD_INQ prints some parameters of VTD installation.        *
! *                                                                      *
! *  ### 23-DEC-2023    VTD_INQ    v1.0 (c)  L. Petrov  23-DEC-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE 
      CHARACTER   VTD__VERSION*%%vtd_version_len%%
      CHARACTER   VTD__PREFIX*%%vtd_prefix_len%%
      CHARACTER   VTD__ROOT*%%vtd_root_len%%
      CHARACTER   VTD__DATA*%%vtd_data_len%%
      CHARACTER   VTD__HELP*%%vtd_help_len%%
!      
      PARAMETER  ( VTD__VERSION = "%%vtd_version_val%%" )
      PARAMETER  ( VTD__PREFIX  = "%%vtd_prefix_val%%" )
      PARAMETER  ( VTD__ROOT    = "%%vtd_root_val%%" )
      PARAMETER  ( VTD__DATA    = "%%vtd_data_val%%" )
      PARAMETER  ( VTD__HELP    = "%%vtd_help_val%%" )
      CHARACTER  PAR*32
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: vtd_inq  parameter'
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
        ELSE IF ( PAR == 'help'    .OR. &
     &            PAR == '--help'  .OR. &
     &            PAR == 'vtd_help'     ) THEN
           WRITE ( 6, '(A)' ) VTD__HELP
      END IF
!
      END PROGRAM VTD_INQ  !#!#
