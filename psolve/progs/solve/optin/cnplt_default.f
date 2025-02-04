      SUBROUTINE CNPLT_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  CNPLT.                              *
! *   CNPLT_DEFAULT sets default values in glbc4 for CNPLT -- program    *
! *   for outliers plotting residuals.                                   *
! *                                                                      *
! *  ###  05-MAY-98  CNPLT_DEFAULT  v1.1 (c)  L. Petrov  07-AUG-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'glbcm.i'
      INTEGER*4  IUER, IER, ILEN, I_LEN
      REAL*8     VAL
      CHARACTER  STR*20
!
! --- First of all: system-wide defaults
!
      SIGMA_TYPE       = SIGMA_TYPE__DEF
      CNPLT_SUPR_PRE98 = .FALSE.
      CNPLT_PHASE_S    = .FALSE.
      CNPLT_SHOW_UNRC  = CNPLT_SHOW_UNRC__DEF
      CNPLT_SHOW_CBAD  = CNPLT_SHOW_CBAD__DEF
!
! --- Examining SIGMA_TYPE
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'SIGMA_TYPE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:2) .EQ. 'PR' ) THEN
                SIGMA_TYPE = 'PR'
             ELSE IF ( STR(1:2) .EQ. 'PS' ) THEN
                SIGMA_TYPE = 'PS'
             ELSE
                CALL ERR_LOG ( 6891, IUER, 'CNPLT_DEFAULT', 'Environment '// &
     &              'variable CNPLT_SUPR_PRE98 has wrong value: '// &
     &               STR(1:I_LEN(STR))//' -- only PR or PS are acceptable' )
                RETURN
           END IF
      END IF
!
! --- Examining CNPLT_SUPR_PRE98
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'CNPLT_SUPR_PRE98', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'      .OR. STR(1:3) .EQ. 'YES'         ) THEN
                CNPLT_SUPR_PRE98 = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &                 STR(1:3) .EQ. 'OFF'     .OR. STR(1:2) .EQ. 'NO'  ) THEN
                CNPLT_SUPR_PRE98 = .FALSE.
             ELSE
                CALL ERR_LOG ( 6892, IUER, 'CNPLT_DEFAULT', 'Environment '// &
     &              'variable CNPLT_SUPR_PRE98 has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining CNPLT_SHOW_UNRC
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'CNPLT_SHOW_UNRC', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'      .OR. STR(1:3) .EQ. 'YES'         ) THEN
                CNPLT_SHOW_UNRC = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &                 STR(1:3) .EQ. 'OFF'     .OR. STR(1:2) .EQ. 'NO'  ) THEN
                CNPLT_SHOW_UNRC = .FALSE.
             ELSE
                CALL ERR_LOG ( 6893, IUER, 'CNPLT_DEFAULT', 'Environment '// &
     &              'variable CNPLT_SHOW_UNRC has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining CNPLT_SHOW_CBAD
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'CNPLT_SHOW_CBAD', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'      .OR. STR(1:3) .EQ. 'YES'         ) THEN
                CNPLT_SHOW_CBAD = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &                 STR(1:3) .EQ. 'OFF'     .OR. STR(1:2) .EQ. 'NO'  ) THEN
                CNPLT_SHOW_CBAD = .FALSE.
             ELSE
                CALL ERR_LOG ( 6894, IUER, 'CNPLT_DEFAULT', 'Environment '// &
     &              'variable CNPLT_SHOW_CBAD has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  CNPLT_DEFAULT  #!#
