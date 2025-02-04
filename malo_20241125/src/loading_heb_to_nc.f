      PROGRAM    LOADING_HEB_TO_NC_LAUNCH
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = MALO__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL LOADING_HEB_TO_NC()
      END  PROGRAM  LOADING_HEB_TO_NC_LAUNCH  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE  LOADING_HEB_TO_NC()
! ************************************************************************
! *                                                                      *
! *   Program LOADING_HEB_TO_NC
! *                                                                      *
! * ### 12-APR-2016 LOADING_HEB_TO_NC v1.2 (c) L. Petrov 22-MAY-2017 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE  ) :: HEB_DSPL
      CHARACTER  FILIN*128, FILOUT*128, FILDESC*128, FILCOMM*128, &
     &           COMPR_COM*128, COM_STR*128
      INTEGER*4  IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, * ) 'Usage loading_heb_to_nc filin filout fildesc filcomm [compr]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILIN   )
           CALL GETARG ( 2, FILOUT   )
           CALL GETARG ( 3, FILDESC )
           CALL GETARG ( 4, FILCOMM )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, COMPR_COM )
              ELSE
                COMPR_COM = 'none'
           END IF 
      END IF 
      IF ( COMPR_COM == 'none' ) THEN
           CONTINUE 
           CALL CLRCH ( COMPR_COM )
         ELSE IF ( COMPR_COM == 'gzip' ) THEN
           COMPR_COM = 'gzip -1 -f '
         ELSE IF ( COMPR_COM == 'bzip2' ) THEN
           COMPR_COM = 'bzip2 -9 -f '
         ELSE IF ( COMPR_COM == 'pbzip2' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -f '
         ELSE IF ( COMPR_COM == 'pbzip2_p1' ) THEN
           COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
         ELSE IF ( COMPR_COM == 'lbzip2' ) THEN
           COMPR_COM= 'lbzip2 -9 -f '
         ELSE IF ( COMPR_COM == 'lbzip2_p1' ) THEN
           COMPR_COM= 'lbzip2 -9 -n1 -f '
         ELSE IF ( COMPR_COM == 'lbzip2_1' ) THEN
           COMPR_COM= 'lbzip2 -1 -f '
         ELSE IF ( COMPR_COM == 'lbzip2_1p1' ) THEN
           COMPR_COM= 'lbzip2 -1 -n1 -f '
         ELSE IF ( COMPR_COM == 'lbzip2_2p1' ) THEN
           COMPR_COM = 'lbzip2 -2 -n1 -f '
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 5301, IUER, 'LOADING_HEB_TO_NC',  'Unsupported '// &
     &         'compression method: '//COMPR_COM(1:I_LEN(COMPR_COM))// &
     &         ' . Supported methods: none gzip bzip2 pbzip2 pbzip_p1 '// &
     &         'lbzip2 lbzip2_1 lbzip2_1p1 lbzip2_2p1' ) 
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILIN, HEB_DSPL, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5302, IUER, 'LOADING_HEB_TO_NC', 'Failed to read input '// &
     &         'file with displacements '//FILIN )
           CALL EXIT ( 1 )
      END IF
      HEB_DSPL%TAI = HEB_DSPL%UTC
!
!!      CALL WRITE_LOADING_NC ( INT(HEB_DSPL%DIMS(1),KIND=4), INT(HEB_DSPL%DIMS(2),KIND=4), 1, 26, &
!
      IUER = -11
      CALL WRITE_LOADING_NC ( INT(HEB_DSPL%DIMS(1),KIND=4), INT(HEB_DSPL%DIMS(2),KIND=4), 1, 0, &
     &                        HEB_DSPL%MJD, HEB_DSPL%TAI, %VAL(0), HEB_DSPL%VAL, &
     &                        MALO__LABEL, 'NO', &
     &                        FILDESC, FILCOMM, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5303, IUER, 'LOADING_HEB_TO_NC', 'Failed to write '// &
     &         'displacements into '//FILOUT )
           CALL EXIT ( 1 )
      END IF
      IF ( ILEN(COMPR_COM) .NE. 0 ) THEN
!
! -------- Now compress the output file 
!
           COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &               FILOUT(1:I_LEN(FILOUT))
           CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
      END IF
      CALL EXIT  ( 0 )
      END  SUBROUTINE  LOADING_HEB_TO_NC  !#!#
