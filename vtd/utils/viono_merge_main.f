       PROGRAM    VIONO_MERGE__MAIN
       IMPLICIT   NONE 
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = INT8(4) * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL VIONO_MERGE_MAIN()
       END  PROGRAM  VIONO_MERGE__MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE VIONO_MERGE_MAIN()
! ************************************************************************
! *                                                                      *
! *   Program  VIONO_MERGE merges two ionospheric models global          *
! *   (1st) and the regional (2nd).                                      *
! *                                                                      *
! *  ### 09-FEB-2018  VIONO_MERGE  v1.0 (c)  L. Petrov  09-FEB-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'viono.i'
      CHARACTER  FILG*128, FILR*128, FILO*128
      TYPE     ( IONO__TYPE ) :: VIOG, VIOR, VIOO
      CHARACTER  STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, IVRB, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IVRB = 0
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Viono_merge fil_glo fil_reg fil_out [ivrb]'
           CALL EXIT ( 1 )
         ELSE
           CALL GETARG ( 1, FILG )
           CALL GETARG ( 2, FILR )
           CALL GETARG ( 3, FILO )
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, STR ) 
                CALL CHIN   ( STR, IVRB )
           END IF
      END IF
!
      IUER = -1
      CALL VIO_GET_HEADER ( FILG, VIOG, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4501, IUER, 'VIONO_MERGE', 'Error in reading the '// &
     &         'global ionospheric model file '//FILG )
           RETURN 
      END IF
!
      IUER = -1
      CALL VIO_GET_HEADER ( FILR, VIOR, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 4502, IUER, 'VIONO_MERGE', 'Error in reading the '// &
     &         'regional ionospheric model file '//FILR )
           RETURN 
      END IF
!
      IUER = -1
      CALL VIONO_MERGE ( FILG, FILR, FILO, VIOG, VIOR, VIOO, IVRB, IUER )
!
      END  SUBROUTINE  VIONO_MERGE_MAIN  !#!  
