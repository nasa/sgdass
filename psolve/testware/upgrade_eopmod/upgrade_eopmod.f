      PROGRAM      UPGRADE_EOPMOD
! ************************************************************************
! *                                                                      *
! *   Program  UPGRADE_EOPMOD  is for updating EOP-MOD which used the    *
! *   obsolete, pre-2001 format.                                         *
! *                                                                      *
! *   Usage:  upgrade_eopmod <filin> <filout> [<time_scale>]             *
! *                                                                      *
! *   where <filin>  -- input EOP-MOD file,                              *
! *         <filout> -- output EOP-MOD format                            *
! *         <time_scale>  -- code of the time scale used as time tags    *
! *                          of the nodes of the table. Supported        *
! *                          values: TDB, TAI, UTC, TCG, TCB, UNDEF.     *
! *                          Value UNDEF (undefined) used as default.    *
! *                                                                      *
! * ### 08-JAN-2001  UPGRADE_EOPMOD  v1.0 (c) L. Petrov 08-JAN-2001 ###  *
! *                                                                      *
! ************************************************************************
      CHARACTER    FILIN*128, FILOUT*128, TSC_STR*8, STR*75, COMMENT*75
      CHARACTER    EOPMOD_LABEL*15
      LOGICAL*4    LEX
      PARAMETER  ( EOPMOD_LABEL = 'EOP-MOD Ver 2.0' )
      INTEGER*4    NUMARG, IO
      INTEGER*4,   EXTERNAL :: IARGC, ILEN
!
      NUMARG = IARGC ()
      CALL CLRCH ( TSC_STR )
      IF ( NUMARG .EQ. 2 ) THEN
           CALL GETARG ( 1, FILIN  )
           CALL GETARG ( 2, FILOUT )
         ELSE IF ( NUMARG .GE. 3 ) THEN
           CALL GETARG ( 1, FILIN   )
           CALL GETARG ( 2, FILOUT  )
           CALL GETARG ( 3, TSC_STR )
         ELSE
           WRITE ( 6, '(A)' ) 'upgrade_eopmod v 1.0 . Usage: upgrade_eopmod '// &
     &                        '<filin> <filout> [<time_scale>]'
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(TSC_STR) .EQ. 0 ) TSC_STR='UNDEF   '
!
      INQUIRE ( FILE=FILIN, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 5701, -1, 'UPGRADE_EOPMOD', 'Input file '// &
     &          FILIN(1:I_LEN(FILIN))//' was not found' )
           CALL EXIT ( 2 )
      END IF
!
      IF ( TSC_STR .NE. 'TDB     '  .AND. &
     &     TSC_STR .NE. 'TAI     '  .AND. &
     &     TSC_STR .NE. 'UTC     '  .AND. &
     &     TSC_STR .NE. 'TCG     '  .AND. &
     &     TSC_STR .NE. 'TCB     '  .AND. &
     &     TSC_STR .NE. 'UNDEF   '        ) THEN
!
           CALL ERR_LOG ( 5702, -1, 'UPGRADE_EOPMOD', 'Unknown time scale "'// &
     &          TSC_STR//'"  Only TDB, TAI, UTC, TCG, TCB and UNDEF are '// &
     &          'supported' )
           CALL EXIT ( 2 )
      END IF
!
      OPEN ( UNIT=11, FILE=FILIN, ACCESS='DIRECT', STATUS='OLD', RECL=76, &
     &       FORM='FORMATTED', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 5703, -1, 'UPGRADE_EOPMOD', 'Failure to open '// &
     &         'the input file '//FILIN(1:I_LEN(FILIN))//' iostat='// &
     &          STR )
           CALL EXIT ( 2 )
      END IF
!
      OPEN ( UNIT=22, FILE=FILOUT, ACCESS='DIRECT', STATUS='UNKNOWN', RECL=76, &
     &       FORM='FORMATTED', IOSTAT=IO )
      IF ( IO .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IO, STR )
           CALL ERR_LOG ( 5704, -1, 'UPGRADE_EOPMOD', 'Failure to open '// &
     &         'the output file '//FILOUT(1:I_LEN(FILOUT))//' iostat='// &
     &          STR )
           CALL EXIT ( 2 )
      END IF
!
      DO 410 J1=1,1024*1024
         READ ( UNIT=11, REC=J1, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IO .EQ. -1  ) GOTO 810
         IF ( IO .EQ. 922 ) GOTO 810
         IF ( IO .NE. 0 ) THEN
              WRITE ( 6, * ) 'Line ',J1
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              CALL ERR_LOG ( 5705, -1, 'UPGRADE_EOPMOD', 'Error in reading '// &
     &            'the input file '//FILIN(1:I_LEN(FILIN))//'  iostat='// &
     &             STR )
              CALL EXIT ( 3 )
         END IF
         IF ( J1 .EQ. 1 ) THEN
              CALL CLRCH ( COMMENT )
              COMMENT = '# '//STR(30:)
              STR = EOPMOD_LABEL//'  '//STR(1:9)//'  '//STR(10:13)// &
     &              '   '//STR(14:17)//'  '//'UT1-TAI '//'  '//TSC_STR//' '// &
     &              '               '
         END IF
         WRITE ( 22, REC=J1, FMT='(A)', IOSTAT=IO ) STR//CHAR(10)
         IF ( IO .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IO, STR )
              CALL ERR_LOG ( 5706, -1, 'UPGRADE_EOPMOD', 'Error in writing '// &
     &            'the output file '//FILOUT(1:I_LEN(FILOUT))//'  iostat='// &
     &             STR )
              CALL EXIT ( 4 )
         END IF
 410  CONTINUE
 810  CONTINUE
      CLOSE ( UNIT=11 )
      CLOSE ( UNIT=22 )
      END  !#!  UPGRADE_EOPMOD  #!#
