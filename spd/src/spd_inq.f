      PROGRAM    SPD_INQ
! ************************************************************************
! *                                                                      *
! *   Program SPD_INQ prints some parameters of spd installtion.         *
! *                                                                      *
! *  ### 12-JAN-2024    SPD_INQ    v1.0 (c)  L. Petrov  12-JAN-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'spd_local.i'
      CHARACTER  PAR*128, STR*128
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: spd_inq  parameter'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, PAR )
      END IF
!
      IF ( PAR == 'version'   .OR. &
     &     PAR == '--version'      ) THEN
           WRITE ( 6, '(A)' ) SPD__VERSION
        ELSE IF ( PAR == 'prefix'       .OR. &
     &            PAR == 'spd_prefix'   .OR. &
     &            PAR == '--prefix'     .OR. &
     &            PAR == '--spd_prefix'      ) THEN
           WRITE ( 6, '(A)' ) SPD__PREFIX
        ELSE IF ( PAR == 'root'         .OR. &
     &            PAR == 'spd_root'     .OR. &
     &            PAR == '--root'       .OR. &
     &            PAR == '--spd_root'      ) THEN
           WRITE ( 6, '(A)' ) SPD__ROOT
        ELSE IF ( PAR == 'share'        .OR. &
     &            PAR == 'spd_share'    .OR. &
     &            PAR == '--share'      .OR. &
     &            PAR == '--spd_share'      ) THEN
           WRITE ( 6, '(A)' ) SPD__SHARE
        ELSE IF ( PAR == 'script'       .OR. &
     &            PAR == 'spd_script'   .OR. &
     &            PAR == '--shar'       .OR. &
     &            PAR == '--spd_script'      ) THEN
           WRITE ( 6, '(A)' ) SPD__SCRIPT
        ELSE IF ( PAR == 'doc'          .OR. &
     &            PAR == 'spd_doc'      .OR. &
     &            PAR == '--doc'        .OR. &
     &            PAR == '--spd_doc'         ) THEN
           WRITE ( 6, '(A)' ) SPD__DOC
     END IF        

      END  PROGRAM  SPD_INQ  !#!#
