      SUBROUTINE GVF_DB_READ ( DB_NAME, REPO, VCAT, GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVF_DB_READ  read the GVF database files. The database    *
! *   is identified by it name DB_NAME and depository name REPO.         *
! *   The result of database file parsing is written in the object GVH.  *
! *                                                                      *
! * ###  02-OCT-2007   GVF_DB_READ   v2.0 (c)  L. Petrov 08-JUN-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vcat.i'
      INCLUDE   'gvh.i'
      INTEGER*4  L_ENV, IUER
      CHARACTER  DB_NAME*(*), REPO*(*)
      TYPE     ( VCAT__TYPE ) :: VCAT
      TYPE     ( GVH__STRU  ) :: GVH
      INTEGER*4    M_FIL, MBUF
      PARAMETER  ( M_FIL =  32 ) 
      PARAMETER  ( MBUF  = 256 ) 
      INTEGER*4  J1, L_FIL, REMAINED_BYTES, IER
      CHARACTER  FIL_ENV*128, FIL_DB(M_FIL)*128, STR*128
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Check whether VCAT was loaded
!
      IF ( VCAT%STATUS .NE. VCAT__LOADED ) THEN
           CALL ERR_LOG ( 8811, IUER, 'GVF_DB_READ', 'VCAT was not loaded. '// &
     &         'Please first run routine VCAT_GET_CONF' )
           RETURN 
      END IF
!
! --- Resolve database name. This rourinte will return the full path envelop
! --- file name and L_FIL fill path database file names
!
      CALL ERR_PASS ( IUER, IER )
      CALL VCAT_RESOLVE_DBNAME ( VCAT, DB_NAME, REPO, FIL_ENV, M_FIL, &
     &                           L_FIL, FIL_DB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8812, IUER, 'GVF_DB_READ', 'Cannot find '// &
     &         'the envelop file for the database '// &
     &          DB_NAME(1:I_LEN(DB_NAME))//' . used VCAT configuration '// &
     &         'file: '//VCAT%CONF_FILE )
           RETURN 
      END IF
!
! --- Initialize GVH object
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8813, IUER, 'GVF_DB_READ', 'Error in an '// &
     &         'atttempt to initialize GVH' )
           RETURN 
      END IF
!
      DO 410 J1=1,L_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_READ_BGV ( GVH, 1, FIL_DB(J1), REMAINED_BYTES, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8814, IUER, 'GVF_DB_READ', 'Error in '// &
     &            'an atttempt to read input database file '//FIL_DB(J1) )
              RETURN 
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              CALL ERR_LOG ( 8815, IUER, 'GVF_DB_READ', 'The number of '// &
     &            'remaining bytes after reading input databae file '// &
     &             FIL_DB(J1)(1:I_LEN(FIL_DB(J1)))//' is not 0, but '//STR )
              RETURN 
         END IF
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREGET ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8816, IUER, 'GVF_DB_READ', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVF_DB_READ  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GVF_DB_READ_PRE2020 ( DB_NAME, GVF_DB_DIR, GVF_ENV_DIR, &
     &                         L_ENV, F_ENV, GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GVF_DB_READ 
! *                                                                      *
! * ###  02-OCT-2007   GVF_DB_READ   v1.1 (c)  L. Petrov 09-NOV-2019 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'gvh.i'
      INTEGER*4  L_ENV, IUER
      CHARACTER  F_ENV(L_ENV)*(*), DB_NAME*(*), GVF_DB_DIR*(*), GVF_ENV_DIR*(*)
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4    M_FIL, MBUF
      PARAMETER  ( M_FIL =  32 ) 
      PARAMETER  ( MBUF  = 256 ) 
      INTEGER*4  J1, J2, J3, J4, J5, IB, IE, IV, L_FIL, NBUF, EXP_VERSION, &
     &           REMAINED_BYTES, IER
      CHARACTER  FILENV*128, FILIN(M_FIL)*128, BUF(MBUF)*128, STR*128, &
     &           EXP_NAME*16
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, LINDEX
!
      CALL CLRCH ( FILENV )
      DO 410 J1=1,L_ENV
         IF ( INDEX ( F_ENV(J1), DB_NAME(1:I_LEN(DB_NAME)) ) > 0 ) THEN
              FILENV = GVF_ENV_DIR(1:I_LEN(GVF_ENV_DIR))//'/'//F_ENV(J1)
         END IF
 410  CONTINUE 
!
      IF ( ILEN(FILENV) == 0 ) THEN
           CALL ERR_LOG ( 8821, IUER, 'GVF_DB_READ_PRE2020', 'Cannot find '// &
     &         'the envelop file for the database '// &
     &          DB_NAME(1:I_LEN(DB_NAME))//' in the envelop directory '// &
     &          GVF_ENV_DIR )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILENV, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8822, IUER, 'GVF_DB_READ_PRE2020', 'Error in an '// &
     &         'atttempt to read database envelope file '//FILENV )
           RETURN 
      END IF
!
      IB = LINDEX ( FILENV, '/' ) + 1
      IE =  INDEX ( FILENV(IB:), '_' ) + IB-2
      IF ( IE .LE. IB ) IE = ILEN(FILENV)
      IV  = LINDEX ( FILENV, '_v' )
      EXP_NAME = FILENV(IB:IB+9)
      CALL CHIN ( FILENV(IV+2:IV+4), EXP_VERSION )
!
      L_FIL = 0
      DO 420 J2=1,NBUF
         IF ( BUF(J2)(1:1)  == '#' ) GOTO 420
         IF ( BUF(J2)(1:1)  == '!' ) GOTO 420
         IF ( BUF(J2)(1:1)  == '$' ) GOTO 420
         IF ( ILEN(BUF(J2)) ==  0  ) GOTO 420
         L_FIL = L_FIL + 1
         IF ( BUF(J2)(21:21) .NE. ' ' ) THEN
              FILIN(L_FIL) = GVF_DB_DIR(1:I_LEN(GVF_DB_DIR))//'/'// &
     &                       FILENV(IB:IE)//'_'//BUF(J2)(21:21)//'_'// &
     &                       BUF(J2)(23:I_LEN(BUF(J2)))
            ELSE 
              FILIN(L_FIL) = GVF_DB_DIR(1:I_LEN(GVF_DB_DIR))//'/'// &
     &                       FILENV(IB:IE)
         END IF
         CALL BLANK_TO_ZERO ( BUF(J2)(13:15))
         FILIN(L_FIL) = FILIN(L_FIL)(1:I_LEN(FILIN(L_FIL)))//'_'// &
     &                  BUF(J2)(9:11)//'_v'//BUF(J2)(13:15)//'.'// &
     &                  BUF(J2)(17:19)
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8823, IUER, 'GVF_DB_READ_PRE2020', 'Error in an '// &
     &         'atttempt to initialize GVH' )
           CALL EXIT ( 1 ) 
      END IF
!
      DO 430 J3=1,L_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_READ_BGV ( GVH, 1, FILIN(J3), REMAINED_BYTES, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8824, IUER, 'GVF_DB_READ_PRE2020', 'Error in '// &
     &            'an atttempt to read input database file '//FILIN(J3) )
              RETURN 
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              CALL ERR_LOG ( 8825, IUER, 'GVF_DB_READ_PRE2020', 'The number of '// &
     &            'remaining bytes after reading input databae file '// &
     &             FILIN(J3)(1:I_LEN(FILIN(J3)))//' is not 0, but '//STR )
              RETURN 
         END IF
 430  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREGET ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8826, IUER, 'GVF_DB_READ_PRE2020', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           RETURN 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVF_DB_READ_PRE2020  !#!#
