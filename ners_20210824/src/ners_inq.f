      SUBROUTINE NERS_INQ ( NERS, REQ, M_PAR, L_PAR, PARS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NERS_INQ
! *                                                                      *
! *  ### 22-JUN-2016    NERS_INQ   v1.1 (c)  L. Petrov  17-NOV-2016 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'ners.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( NERS__TYPE ) :: NERS
      INTEGER*4  M_PAR, L_PAR, IUER
      CHARACTER  REQ*(*)
      REAL*8     PARS(M_PAR)
      INTEGER*8  SIZE_I8
      REAL*8     UTC_CUR
      CHARACTER  STR*128, STR1*128
      INTEGER*4  UNIX_DATE, IS, IER
      INTEGER*4, EXTERNAL ::  TIME, FILE_INFO
!
      UTC_CUR = TIME ( %VAL(0) ) - UNIX__J2000_UTC
      IF ( NERS%FCS_STATUS .NE. NERS__LOAD .OR. &
     &     (UTC_CUR - NERS%UTC_LOAD) > NERS__AGE_MIN ) THEN
           IF ( NERS%FCS_STATUS .NE. NERS__INIT ) THEN
                CALL ERR_LOG ( 4911, IUER, 'NERS_INQ', 'NERS data '// &
     &              'structure has not been initialized. Please run '// &
     &              'NERS_INIT first' )
                RETURN 
           END IF
!
           IS = FILE_INFO ( TRIM(NERS%CNF%FCS_FILE)//CHAR(0), UNIX_DATE, &
     &                      SIZE_I8 )
           IF ( IS .NE. 0 .OR. (TIME(%VAL(0)) - UNIX_DATE) > NERS%CNF%AGE_FCS ) THEN
                IER = IUER
                CALL NERS_FETCH ( NERS, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4912, IUER, 'NERS_INQ', 'Error in '// &
     &                   'an attempt to retrieve NERS forecast parameters '// &
     &                   'form the remote server' )
                     RETURN 
                END IF
           END IF
!
           IER = IUER
           CALL NERS_LOAD ( NERS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4913, IUER, 'NERS_INQ', 'Error in '// &
     &              'an attempt to retrieve NERS forecast parameters '// &
     &              'form the remote server' )
                RETURN 
           END IF
      END IF
      IF ( REQ == 'range' ) THEN
           L_PAR = 3
           IF ( L_PAR > M_PAR ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( M_PAR, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( L_PAR, STR1 )
                CALL ERR_LOG ( 4914, IUER, 'NERS_INQ', 'Parameter '// &
     &              'M_PAR '//TRIM(STR)//' is too small. It should '// &
     &              'be at least '//STR1 )
                RETURN 
           END IF 
           PARS(1) = NERS%FCS%ARG_C(1)
           PARS(2) = NERS%FCS%ARG_3(NERS%FCS%NK_3)
           IF ( NERS%FCS%NL > 0 ) THEN
                PARS(3) = NERS%FCS%ARG_L(NERS%FCS%NL)
              ELSE 
                PARS(3) = NERS%FCS%ARG_3(NERS%FCS%NK_3)
           END IF
         ELSE IF ( REQ == 'fcs_gen_time' ) THEN
           L_PAR = 1
           IF ( L_PAR > M_PAR ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( M_PAR, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( L_PAR, STR1 )
                CALL ERR_LOG ( 4915, IUER, 'NERS_INQ', 'Parameter '// &
     &              'M_PAR '//TRIM(STR)//' is too small. It should '// &
     &              'be at least '//STR1 )
                RETURN 
           END IF 
           PARS(1) = NERS%FCS%TAI_GEN
         ELSE 
           CALL ERR_LOG ( 4916, IUER, 'NERS_INQ', 'Requect code '// &
     &          TRIM(REQ)//' is not supported' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NERS_INQ  !#!#
