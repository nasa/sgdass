      SUBROUTINE GVF_DB_GET_ENV_DIR ( GVF_ENV_DIR, M_ENV, L_ENV, F_ENV, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVF_DB_GET_ENV_DIR
! *                                                                      *
! * ### 02-OCT-2007 GVF_DB_GET_ENV_DIR v1.0 (c) L. Petrov 02-OCT-2007 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  M_ENV, L_ENV, IUER
      CHARACTER  GVF_ENV_DIR*(*), F_ENV(M_ENV)*(*)
      CHARACTER  FINAM*128
      INTEGER*4  J1, IB, IP, LEV
      ADDRESS__TYPE :: DIR_DESC(16)
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN, LINDEX
!
      L_ENV = 0
      LEV   = 0
      DO 410 J1=1,M_ENV
         IP = GET_FILE_FROM_DIR ( LEV, DIR_DESC, GVF_ENV_DIR, FINAM )
         IF ( IP .NE. 0 ) THEN
              CALL ERR_LOG ( 8781, IUER, 'GVF_DB_GET_ENV_DIR', 'Error in '// &
     &            'examining contents of directory '//GVF_ENV_DIR )
              RETURN 
         END IF
         IF ( LEV .EQ. 0 ) GOTO 810 ! No more files? exit
!
         IF ( ILEN(FINAM) .GT. LEN('.env') ) THEN
              IF ( FINAM(ILEN(FINAM)-LEN('.env')+1:ILEN(FINAM)) == '.env' ) THEN
                   IB = LINDEX ( FINAM, '/' ) + 1
                   L_ENV = L_ENV + 1
                   F_ENV(L_ENV) = FINAM(IB:)
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_ENV .LE. 0 ) THEN
           CALL ERR_LOG ( 8782, IUER, 'GVF_DB_GET_ENV_DIR', 'Direcotry '// &
     &         'with database envelops '//GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))// &
     &         ' is empty. Nothing to select' )
           RETURN 
      END IF
!
      CALL SORT_CH ( L_ENV, F_ENV )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVF_DB_GET_ENV_DIR  !#!#
