      PROGRAM    MALO_REGRID_MAIN
! ************************************************************************
! *                                                                      *
! *   Program MALO_REGRID
! *                                                                      *
! * ### 22-OCT-2012 MALO_REGRID_MAIN  v1.0 (c) L. Petrov 24-OCT-2012 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ), POINTER :: MALO(:)
      CHARACTER  FILIN*128, FILMEAN*128, FILOUT*128, FILGEN*128, STR*128, METH*8
      INTEGER*4  J1, J2, DEG, IVRB, ID, IS, IUER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: malo_regrid deg method file_in file_mean file_out [verbosity_level]'
           CALL EXIT ( 0 )
         ELSE 
           CALL GETARG ( 1, STR  )
           CALL CHIN   ( STR, DEG )
           IF ( DEG < 2  .OR. DEG > 4096 ) THEN
                CALL ERR_LOG ( 6201, IUER, 'MALO_REGRID_MAIN', 'Wrong degree '// &
     &              'argument: '//STR(1:I_LEN(STR))// &
     &              ' an integer in range [2, 4096] was expected' )
           END IF 
           CALL GETARG ( 2, METH )
           IF ( METH == 'spline' ) THEN
                CONTINUE 
              ELSE IF ( METH == 'expand' ) THEN
                CONTINUE 
              ELSE
                CALL ERR_LOG ( 6202, IUER, 'MALO_REGRID_MAIN', 'Wrong method '// &
     &              'level argument: '//METH(1:I_LEN(METH))// &
     &              ' only spline and expand are supported' )
                CALL EXIT ( 1 )
           END IF 
           CALL GETARG ( 3, FILIN   )
           CALL GETARG ( 4, FILMEAN )
           CALL GETARG ( 5, FILOUT  )
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 6, STR )
                CALL CHIN   ( STR, IVRB )
                IF ( IVRB < 0 .OR. IVRB > 99  ) THEN
                     IUER = -2
                     CALL ERR_LOG ( 6203, IUER, 'MALO_REGRID_MAIN', 'Wrong verbosity '// &
     &                   'level argument: '//STR(1:I_LEN(STR))// &
     &                   ' an integer in range [0, 99] was expected' )
                     CALL EXIT ( 1 )
                END IF
              ELSE 
                IVRB = 1
           END IF
      END IF
!
      ALLOCATE ( MALO(3), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6204, IUER, 'MALO_REGRID_MAIN', 'Error in an attempt '// &
     &         'to allocate memory for object MALO' )
           CALL EXIT ( 1 )
      END IF
!
      DO 410 J1=1,3
         IUER = -1
         CALL MALO_INIT ( MALO(J1), IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6205, IUER, 'MALO_REGRID_MAIN', 'Error in an attempt '// &
     &            'to initialize object MALO' )
              CALL EXIT ( 1 )
         END IF
 410  CONTINUE 
!
      IUER = -1
      CALL SPR_READ_NC ( FILIN, MALO(1), IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6205, IUER, 'MALO_REGRID_MAIN', 'Error in an attempt '// &
     &         'to read thye surface pressure from the input file '// &
     &          FILIN )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL SPR_READ_NC ( FILMEAN, MALO(2), IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6206, IUER, 'MALO_REGRID_MAIN', 'Error in an attempt '// &
     &         'to read thye surface pressure from the input file '// &
     &          FILIN )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_REGRID ( DEG, METH, MALO(1), MALO(2), MALO(3), IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6207, IUER, 'MALO_REGRID_MAIN', 'Error in an attempt '// &
     &         'to read the surface pressure from the input file '// &
     &          FILIN )
           CALL EXIT ( 1 )
      END IF
!
      ID = LINDEX ( FILOUT, '/' )
      IF ( ID .LE. 0 ) ID = 1
      IS = INDEX( FILOUT(ID:), '_' ) + ID - 1
      IF ( IS .LE. ID ) IS = LINDEX ( FILOUT(ID:), '.' ) + ID - 2
      IF ( IS .LE. 0  ) IS = ILEN(FILOUT)
      FILGEN = FILOUT(1:IS-1)//'_'
!
      IUER = -1
      CALL SPR_WRITE_NC ( MALO(3), FILGEN, MALO(3)%MJD_BEG, MALO(3)%UTC_BEG, &
     &                            MALO(3)%MJD_END, MALO(3)%UTC_END, IVRB, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6207, IUER, 'MALO_REGRID_MAIN', 'Error in an attempt '// &
     &         'to write the output surface pressure to the file with generic '// &
     &         'name '//FILGEN )
           CALL EXIT ( 1 )
      END IF 
      IF ( IVRB .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'Output file '//MALO(3)%FILOUT(1:I_LEN(MALO(3)%FILOUT))
      END IF
!
      CALL EXIT ( 0 )
      END  PROGRAM  MALO_REGRID_MAIN  !#!#
