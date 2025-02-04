      PROGRAM    MALO_UPGRID_MAIN
! ************************************************************************
! *                                                                      *
! *   Program MALO_UPGRID increases spatial resolution of the input      *
! *   dataset to become the same as for the supplied land-sea mask.      *
! *   It works in three modes: 2 -- new small cells are filled with the  *
! *   value of the closest cell within land, 1 -- new small cells are    *
! *   filled with the value of the closest cell within ocean. 0 -- new   *
! *   small cells are fallowed with any value regardless whether it is   *
! *   land or ocean.                                                     *
! *                                                                      *
! *  ### 21-DEC-2013  MALO_UPGRID  v2.2 (c)  L. Petrov  27-APR-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEBIN, HEBLS, HEBMASK, HEBOUT
      CHARACTER  FILIN*128, FILLS*128, FILOUT*128, FILMASK*128, &
     &           FILTMP*128, COM_STR*256
      CHARACTER  STR*128, COMPR_COM*128
      INTEGER*4  IL, MS, MSN_DEF, MSF_DEF, MST_DEF, MODE, IUER
      REAL*4       VAL_MIN_LAND_DEF, VAL_MAX_LAND_DEF, &
     &             VAL_MIN_OCEAN_DEF, VAL_MAX_OCEAN_DEF
      PARAMETER  ( VAL_MIN_LAND_DEF  = -1.0E6 ) 
      PARAMETER  ( VAL_MAX_LAND_DEF  =  1.0E6 ) 
      PARAMETER  ( VAL_MIN_OCEAN_DEF = -1.0E6 ) 
      PARAMETER  ( VAL_MAX_OCEAN_DEF =  1.0E6 ) 
      PARAMETER  ( MSN_DEF = 1 )
      PARAMETER  ( MSF_DEF = 6 )
      PARAMETER  ( MST_DEF = 1 )
      LOGICAL*1  FL_MASK 
      REAL*4     VAL_MIN, VAL_MAX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! $MALO_DIR/bin/malo_upgrid $MALO_DIR/share/spr_nto_omct05_model_d359_1979_2016.heb $MALO_DIR/share/mod44w_d2699_ls_sea_4cells.heb 1  $MALO_DIR/share/spr_nto_omct05_model_d2699_1979_2016.heb 4
! /progs/malo_20170320/bin_static/malo_upgrid /s1/temp/lws_geosfpit_model_2000_2017.heb  /progs/malo_20170320/share/mod44w_ls_blackman_d2699.heb 21 /progs/malo_20170320/share/lws_geosfpit_mask_d2699.heb /progs/malo_20170320/share/twland_lws_geosfpit_model_2000_2017.heb
!
      COMPR_COM = 'pbzip2 -r -m1024 -S4096 -9 -p1 -f '
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: malo_upgrid filin fills mode [fil_mask] filout [ms]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILIN )
           CALL GETARG ( 2, FILLS )
           CALL GETARG ( 3, STR   )
           IF ( STR == '0' ) THEN
                MODE = 0
                FL_MASK = .FALSE.
                VAL_MIN = VAL_MIN_OCEAN_DEF
                VAL_MAX = VAL_MAX_OCEAN_DEF
              ELSE IF ( STR == '1' ) THEN
                MODE = 1  ! Expand over ocean cells
                FL_MASK = .FALSE.
                VAL_MIN = VAL_MIN_OCEAN_DEF
                VAL_MAX = VAL_MAX_OCEAN_DEF
              ELSE IF ( STR == '2' ) THEN
                MODE = 2  ! Expand over land cells
                FL_MASK = .FALSE.
                VAL_MIN = VAL_MIN_LAND_DEF
                VAL_MAX = VAL_MAX_LAND_DEF
              ELSE IF ( STR == '3' ) THEN
                MODE = 3  ! Expand everywhere
                FL_MASK = .FALSE.
                VAL_MIN = VAL_MIN_LAND_DEF
                VAL_MAX = VAL_MAX_LAND_DEF
              ELSE IF ( STR == '11' ) THEN
                MODE = 1  ! Expand over ocean cells
                FL_MASK = .TRUE.
                VAL_MIN = VAL_MIN_OCEAN_DEF
                VAL_MAX = VAL_MAX_OCEAN_DEF
              ELSE IF ( STR == '21' ) THEN
                MODE = 2  ! Expand over land cells
                FL_MASK = .TRUE.
                VAL_MIN = VAL_MIN_LAND_DEF
                VAL_MAX = VAL_MAX_LAND_DEF
              ELSE 
                IUER = -1
                CALL ERR_LOG ( 2101, IUER, 'MALO_UPGRID_MAIN', 'Wrong mode '// &
     &              'parmater: '//STR(1:I_LEN(STR))//' while 0 (all), '// &
     &              '1 or 11 (ocean) or 2 or 21 (land) were expected' )
                CALL EXIT ( 1 )
           END IF
!           
           IF ( FL_MASK ) THEN
                CALL GETARG ( 4, FILMASK )
                CALL GETARG ( 5, FILOUT )
                IF ( IARGC() .GE. 6 ) THEN
                     CALL GETARG ( 6, STR )
                     CALL CHIN ( STR, MS  )
                  ELSE
                     MS = MSF_DEF
                END IF
              ELSE
                CALL CLRCH  ( FILMASK )
                CALL GETARG ( 4, FILOUT )
                IF ( IARGC() .GE. 5 ) THEN
                     CALL GETARG ( 5, STR )
                     CALL CHIN ( STR, MS  )
                  ELSE
                     MS = MSF_DEF
                END IF
           END IF
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILIN, HEBIN, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2102, IUER, 'MALO_UPGRID_MAIN', 'Error in reading '// &
     &         'input file the field to be regridded '//FILIN )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_CHECK_MODEL_FILE ( FILLS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2103, IUER, 'MALO_UPGRID_MAIN', 'Error in searching '// &
     &         'for input file with land-sea mask '//FILLS )
           CALL EXIT ( 1 )
      END IF
!
      IF ( FL_MASK ) THEN
           IUER = -1
           CALL MALO_CHECK_MODEL_FILE ( FILMASK, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 2104, IUER, 'MALO_UPGRID_MAIN', 'Error in searching '// &
     &              'for input file with mask '//FILMASK )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL READ_HEB ( FILMASK, HEBMASK, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 2105, IUER, 'MALO_UPGRID_MAIN', 'Error in reading '// &
     &              'input file with input mask '//FILMASK )
                CALL EXIT ( 1 )
           END IF
         ELSE
           CALL NOUT ( SIZEOF(HEBMASK), HEBMASK )
           HEBMASK%VAL => NULL()
      END IF
!
      IUER = -1
      CALL READ_HEB ( FILLS, HEBLS, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2106, IUER, 'MALO_UPGRID_MAIN', 'Error in reading '// &
     &         'input file with land-sea mask '//FILLS )
           CALL EXIT ( 1 )
      END IF
!
      IF ( HEBLS%DATA_FORMAT == HEB__R8 .AND. ASSOCIATED ( HEBLS%VAL8 ) ) THEN
           ALLOCATE ( HEBLS%VAL(HEBLS%DIMS(1),HEBLS%DIMS(2),HEBLS%DIMS(3),HEBLS%DIMS(4)), &
     &                          STAT=IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH8 ( INT8(4)*HEBLS%DIMS(1)*HEBLS%DIMS(2)*HEBLS%DIMS(3)*HEBLS%DIMS(4), STR )
                IUER = -2
                CALL ERR_LOG ( 2107, IUER, 'MALO_UPGRID_MAIN', 'Failure to '// &
     &              'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory for '// &
     &              'array HEBLS%VAL' )
                CALL EXIT ( 1 )
            END IF 
            CALL SPD_R8_TO_R4 ( HEBLS%DIMS(1)*HEBLS%DIMS(2)*HEBLS%DIMS(3)*HEBLS%DIMS(4), HEBLS%VAL8, HEBLS%VAL )
            DEALLOCATE ( HEBLS%VAL8 )
      END IF
      HEBOUT = HEBIN
      HEBOUT%DIMS(1) = HEBLS%DIMS(1)
      HEBOUT%DIMS(2) = HEBLS%DIMS(2)
      HEBOUT%DIMS(3) = HEBIN%DIMS(3)
      HEBOUT%DIMS(4) = HEBIN%DIMS(4)
      ALLOCATE ( HEBOUT%VAL(HEBOUT%DIMS(1),HEBOUT%DIMS(2),HEBOUT%DIMS(3),HEBOUT%DIMS(4)), &
     &           STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH8 ( INT8(4)*HEBOUT%DIMS(1)*HEBOUT%DIMS(2)*HEBOUT%DIMS(3)*HEBOUT%DIMS(4), STR )
           CALL ERR_LOG ( 2108, IUER, 'MALO_UPGRID_MAIN', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamics memory for the new grid' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_UPGRID ( MODE, HEBIN, HEBLS, HEBMASK, HEBOUT, MSN_DEF, MS, MST_DEF, &
     &                   VAL_MIN, VAL_MAX, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( INT8(4)*HEBOUT%DIMS(1)*HEBOUT%DIMS(2)*HEBOUT%DIMS(3)*HEBOUT%DIMS(4), STR )
           CALL ERR_LOG ( 2109, IUER, 'MALO_UPGRID_MAIN', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamics memory for the new grid' )
           CALL EXIT ( 1 )
      END IF
!
      IL = ILEN(FILOUT)
      IF ( INDEX ( FILOUT(IL-3:IL), '.bz2' ) > 0 ) THEN
           FILTMP = FILOUT(1:IL-4)
         ELSE 
           FILTMP = FILOUT
      END IF
      IUER = -1
      CALL WRITE_HEB ( HEBOUT, HEBOUT%VAL, FILTMP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 2110, IUER, 'MALO_UPGRID_MAIN', 'Error in an attempt to '// &
     &         'write output file '//FILTMP )
           CALL EXIT ( 1 )
      END IF
!
      IF ( INDEX ( FILOUT(IL-3:IL), '.bz2' ) > 0 ) THEN
!
! --------- Now compress the output file 
!
            COM_STR = COMPR_COM(1:I_LEN(COMPR_COM))//' '// &
     &                FILTMP(1:I_LEN(FILTMP))
            CALL SYSTEM ( COM_STR(1:I_LEN(COM_STR))//CHAR(0) )
      END IF
!      
      END  PROGRAM    MALO_UPGRID_MAIN  !#!#
