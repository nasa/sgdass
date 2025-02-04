      SUBROUTINE PAMB_DEFAULT ( IUER )
! ************************************************************************
! *                                                                      *
! *   Initialisation parameters for  PAMB.                               *
! *   PAMB_DEFAULT sets default values in glbc4 for PAMB -- program for  *
! *   resolving phase delay ambiguities.                                 *
! *                                                                      *
! *  ###  15-DEC-98  PAMB_DEFAULT  v1.1  (c)  L. Petrov  21-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'pamb.i'
      INTEGER*4  IUER, IER, ILEN, I_LEN
      REAL*8     VAL
      CHARACTER  STR*20, STR1*20, STR2*20
!
! --- First of all: system-wide defaults
!
      PAMB_VRB       = PAMB_VRB__DEF
      PAMB_PLOT_TYPE = PAMB_PLOT_TYPE__DEF
      PAMB_PSL_TYPE  = PAMB_PSL_TYPE__DEF
      PAMB_PLOT_BAND = PAMB_PLOT_BAND__DEF
      PAMB_PARU_FILE = PAMB_PARU_FILE__DEF
      PAMB_ARFTYPE   = PAMB_ARFTYPE__DEF
!
      PAMB_XGRLIM    = PAMB_XGRLIM__DEF
      PAMB_SGRLIM    = PAMB_SGRLIM__DEF
      PAMB_XPHLIM    = PAMB_XPHLIM__DEF
      PAMB_SPHLIM    = PAMB_SPHLIM__DEF
      PAMB_DEFRG     = PAMB_DEFRG__DEF
      PAMB_ARFMS     = PAMB_ARFMS__DEF
      PAMB_RLIM3     = PAMB_RLIM3__DEF
      PAMB_FRZTR     = PAMB_FRZTR__DEF
      PAMB_SPLSPAN   = PAMB_SPLSPAN__DEF
      PAMB_SPL_CNST  = PAMB_SPL_CNST__DEF
      PAMB_MSC       = PAMB_MSC__DEF
      PAMB_PLOTINI   = PAMB_PLOTINI__DEF
      PAMB_PLOTFIN   = PAMB_PLOTFIN__DEF
      PAMB_INIWEI    = PAMB_INIWEI__DEF
!
! --- Examining PAMB_VRB
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_VRB', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, PAMB_VRB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6921, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_VRB has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_VRB .LT. PAMB_VRB_MIN  .OR. &
     &          PAMB_VRB .GT. PAMB_VRB_MAX       ) THEN
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                CALL INCH  ( PAMB_VRB_MIN, STR1 )
                CALL INCH  ( PAMB_VRB_MAX, STR2 )
!
                CALL ERR_LOG ( 6922, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_VRB has wrong value: '//STR(1:I_LEN(STR))// &
     &              ' what is out of range '// &
     &              '['//STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//']' )
                RETURN
           END IF
      END IF
!
! --- Examining PAMB_PLOT_TYPE
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_PLOT_TYPE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, PAMB_PLOT_TYPE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6923, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_PLOT_TYPE has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_PLOT_TYPE .LT. PAMB_PTP_MIN  .OR. &
     &          PAMB_PLOT_TYPE .GT. PAMB_PTP_MAX       ) THEN
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                CALL INCH  ( PAMB_PTP_MIN, STR1 )
                CALL INCH  ( PAMB_PTP_MAX, STR2 )
!
                CALL ERR_LOG ( 6924, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_PLOT_TYPE has wrong value: '// &
     &               STR(1:I_LEN(STR))//' what is out of range '// &
     &              '['//STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//']' )
                RETURN
           END IF
      END IF
!
! --- Examining PAMB_PSL_TYPE
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_PSL_TYPE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, PAMB_PSL_TYPE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6925, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_PSL_TYPE has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_PSL_TYPE .LT. PSL__MIN  .OR. &
     &          PAMB_PSL_TYPE .GT. PSL__MAX       ) THEN
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                CALL INCH  ( PSL__MIN, STR1 )
                CALL INCH  ( PSL__MAX, STR2 )
!
                CALL ERR_LOG ( 6926, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_PSL_TYPE has wrong value: '// &
     &               STR(1:I_LEN(STR))//' what is out of range '// &
     &              '['//STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//']' )
                RETURN
           END IF
      END IF
!
! --- Examining PAMB_PLOT_BAND
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_PLOT_BAND', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, PAMB_PLOT_BAND, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6927, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_PLOT_BAND has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_PLOT_BAND .LT. PAMB_BAND_MIN .OR. &
     &          PAMB_PLOT_BAND .GT. PAMB_BAND_MAX      ) THEN
!
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                CALL INCH  ( PAMB_BAND_MIN, STR1 )
                CALL INCH  ( PAMB_BAND_MAX, STR2 )
!
                CALL ERR_LOG ( 6928, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_PLOT_BAND has wrong value: '// &
     &               STR(1:I_LEN(STR))//' what is out of range '// &
     &              '['//STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//']' )
                RETURN
           END IF
      END IF
!
! --- Examining PAMB_PARU_FILE
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_PARU_FILE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           PAMB_PARU_FILE = STR
      END IF
!
! --- Examining PAMB_ARFTYPE
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_ARFTYPE', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL IFOR_MEN ( STR, PAMB_ARFTYPE, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6929, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_ARFTYPE has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_ARFTYPE .LT. ARFTYPE__MIN .OR. &
     &          PAMB_ARFTYPE .GT. ARFTYPE__MAX      ) THEN
!
                CALL CLRCH ( STR1 )
                CALL CLRCH ( STR2 )
                CALL INCH  ( ARFTYPE__MIN, STR1 )
                CALL INCH  ( ARFTYPE__MAX, STR2 )
!
                CALL ERR_LOG ( 6930, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_ARFTYPE has wrong value: '// &
     &               STR(1:I_LEN(STR))//' what is out of range '// &
     &              '['//STR1(1:I_LEN(STR1))//', '//STR2(1:I_LEN(STR2))//']' )
                RETURN
           END IF
      END IF
!
! --- Examining PAMB_XGRLIM
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_XGRLIM', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_XGRLIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6931, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_XGRLIM has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_XGRLIM .LT. 0.0D0 ) PAMB_XGRLIM = 0.0D0
      END IF
!
! --- Examining PAMB_XGRLIM
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_SGRLIM', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_SGRLIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6932, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_SGRLIM has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_SGRLIM .LT. 0.0D0 ) PAMB_SGRLIM = 0.0D0
      END IF
!
! --- Examining PAMB_XPHLIM
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_XPHLIM', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_XPHLIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6933, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_XPHLIM has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_XPHLIM .LT. 0.0D0 ) PAMB_XPHLIM = 0.0D0
      END IF
!
! --- Examining PAMB_SPHLIM
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_SPHLIM', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_SPHLIM, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6934, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_SPHLIM has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_SPHLIM .LT. 0.0D0 ) PAMB_SPHLIM = 0.0D0
      END IF
!
! --- Examining PAMB_DEFRG
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_DEFRG', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_DEFRG, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6936, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_DEFRG has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_DEFRG .LT. 0.0D0 ) PAMB_DEFRG = 0.0D0
      END IF
!
! --- Examining PAMB_ARFMS
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_ARFMS', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_ARFMS, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6937, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_ARFMS has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_ARFMS .LT. 0.0D0 ) PAMB_ARFMS = 0.0D0
      END IF
!
! --- Examining PAMB_RLIM3
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_RLIM3', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_RLIM3, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6938, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_RLIM3 has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_RLIM3 .LT. 0.0D0 ) PAMB_RLIM3 = 0.0D0
      END IF
!
! --- Examining PAMB_FRZTR
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_FRZTR', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_FRZTR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6939, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_FRZTR has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_FRZTR .LT. 0.0D0 ) PAMB_FRZTR = 0.0D0
      END IF
!
! --- Examining PAMB_SPLSPAN
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_SPLSPAN', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_SPLSPAN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6940, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_SPLSPAN has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_SPLSPAN .LT. 0.0D0 ) PAMB_SPLSPAN = 0.0D0
      END IF
!
! --- Examining PAMB_SPLCNST
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_SPL_CNST', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_SPL_CNST, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6941, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_SPL_CNST has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_SPL_CNST .LT. 0.0D0 ) PAMB_SPL_CNST = 0.0D0
      END IF
!
! --- Examining PAMB_MSC
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_MSC', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'      .OR. STR(1:3) .EQ. 'YES'         ) THEN
                PAMB_MSC = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &                 STR(1:3) .EQ. 'OFF'     .OR. STR(1:2) .EQ. 'NO'  ) THEN
                PAMB_MSC = .FALSE.
             ELSE
                CALL ERR_LOG ( 6942, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_MSC has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining PAMB_PLOTINI
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_PLOTINI', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'      .OR. STR(1:3) .EQ. 'YES'         ) THEN
                PAMB_PLOTINI = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &                 STR(1:3) .EQ. 'OFF'     .OR. STR(1:2) .EQ. 'NO'  ) THEN
                PAMB_PLOTINI = .FALSE.
             ELSE
                CALL ERR_LOG ( 6943, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_PLOTINI has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining PAMB_PLOTFIN
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_PLOTFIN', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL TRAN ( 11, STR, STR )
           IF ( STR(1:6) .EQ. '.TRUE.'  .OR. STR(1:4) .EQ. 'TRUE'  .OR. &
     &          STR(1:2) .EQ. 'ON'      .OR. STR(1:3) .EQ. 'YES'         ) THEN
                PAMB_PLOTFIN = .TRUE.
             ELSE IF ( STR(1:7) .EQ. '.FALSE.' .OR. STR(1:5) .EQ. 'FALSE' .OR. &
     &                 STR(1:3) .EQ. 'OFF'     .OR. STR(1:2) .EQ. 'NO'  ) THEN
                PAMB_PLOTFIN = .FALSE.
             ELSE
                CALL ERR_LOG ( 6943, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_PLOTFIN has wrong value: '//STR )
                RETURN
           END IF
      END IF
!
! --- Examining PAMB_INIWEI
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_INIWEI', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_INIWEI, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6944, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_INIWEI has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_INIWEI .LT. 0.0D0 ) PAMB_INIWEI = 0.0D0
      END IF
!
! --- Examining PAMB_ARFFLO
!
      CALL CLRCH  ( STR )
      CALL GETENVAR ( 'PAMB_ARFFLO', STR )
      IF ( ILEN(STR) .NE. 0 ) THEN
           CALL DFOR_MEN ( STR, PAMB_ARFFLO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6945, IUER, 'PAMB_DEFAULT', 'Environment '// &
     &              'variable PAMB_ARFFLO has wrong value: '//STR )
                RETURN
           END IF
           IF ( PAMB_ARFFLO .LT. 0.0D0 ) PAMB_ARFFLO = 0.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  PAMB_DEFAULT  #!#
