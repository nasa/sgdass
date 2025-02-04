      SUBROUTINE VCAT_RESOLVE_DBNAME ( VCAT, DB_NAME, REPO, ENV_FILE, M_FIL, &
     &                                 L_FIL, DB_FILE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VCAT_RESOLVE_DBNAME resolves the database name and returns *
! *   associated full path envelop name and the set of full path         *
! *   database file names.                                               *
! *                                                                      *
! *   Teh database is determined by a pair of a database name and        *
! *   repositary name. The full database file name is in the form        *
! *   YYYYMMDD_S_vVVV.env                                                *
! *   where YYYY -- experiment year,                                     *
! *         MM   -- integer experiment month number                      *
! *         DD   -- integer experiment day number in the month           *
! *         S    -- a low case suffix                                    *
! *         VVV  -- database version counter as an integer number.       *
! *                                                                      *
! *   YYYYMMDD_S_vVVV and YYYYMMDD_S are acceptatble. The latter, short  *
! *   form implies the database with the highset version counter.        *
! *                                                                      *
! *   In a case of database name is supplied in the full form anad       *
! *   repositary name is provided, the procedure is straitforward:       *
! *   a) it checks whether the repositary was defined in the VCAT        *
! *   configuraion file; b) it constructs the full path envelop name by  *
! *   concatenating the envelop repositary name and the full database    *
! *   name, c) checks whether the envelop exists; d) determine the       *
! *   matching database directory name; e) reads the envlop file;        *
! *   f) it constrcucts the full path envelop name by concatenating the  *
! *   envelop database directory name and the full database name,        *
! *   g) checks whether the database files exist.                        *
! *                                                                      *
! * ## 14-AUG-2003 VCAT_RESOLVE_DBNAME v2.2 (c) L. Petrov 24-MAR-2022 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vcat.i'
      TYPE     ( VCAT__TYPE ) :: VCAT
      INTEGER*4  M_FIL, L_FIL, IUER
      INTEGER*4  MAX_VER, MAX_FIL 
      PARAMETER  ( MAX_VER = 999 )
      PARAMETER  ( MAX_FIL = 128 )
      CHARACTER  DB_NAME*(*), REPO*(*), ENV_FILE*(*), DB_FILE(M_FIL)*(*)
      LOGICAL*1  LEX, FL_NOVERS
      CHARACTER  STR*128, BUF(MAX_FIL)*128
      INTEGER*4  J1, J2, J3, IND_REP, IND_REP_BEG, IND_REP_END, IB, IE, &
     &           ILN, NBUF, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, LINDEX
!
! --- Check whether VCAT was loaded
!
      IF ( VCAT%STATUS .NE. VCAT__LOADED ) THEN
           CALL ERR_LOG ( 4831, IUER, 'VCAT_RESOLVE_DBNAME', 'VCAT was '// &
     &         'not loaded. Please first run routine VCAT_GET_CONF' )
           RETURN 
      END IF
!
      CALL CLRCH ( ENV_FILE )
      IF ( ILEN(REPO) == 0 .OR. REPO(1:1) == '?' ) THEN
           IND_REP_BEG = 1
           IND_REP_END = VCAT%NREPS
         ELSE
           IND_REP_BEG = LTM_DIF ( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, REPO )
           IF ( IND_REP_BEG < 1 ) THEN
                CALL LIST_TO_LINE ( VCAT%NREPS, VCAT%GVF_REP_NAME, ', ', STR )
                CALL ERR_LOG ( 4832, IUER, 'VCAT_RESOLVE_DBNAME', 'Unknown '// &
     &              'repository '//TRIM(REPO)//' -- the list of supported '// &
     &              'repositories defined in the VCAT configuration file '// &
     &               TRIM(VCAT%CONF_FILE)//' -- '//STR )
                RETURN 
           END IF
           IND_REP_END = IND_REP_BEG 
      END IF      
      IND_REP = 0
      FL_NOVERS = .FALSE.
      DO 410 J1=IND_REP_BEG,IND_REP_END
         IF ( ILEN(DB_NAME) == 10 ) THEN
              STR = TRIM(VCAT%GVF_ENV_DIR(J1))//'/'//DB_NAME(1:10)//'_v001.env' 
              FL_NOVERS = .TRUE.
            ELSE IF ( ILEN(DB_NAME) == 15 ) THEN
              STR = TRIM(VCAT%GVF_ENV_DIR(J1))//'/'//DB_NAME(1:15)//'.env' 
            ELSE IF ( ILEN(DB_NAME) == 19 ) THEN
              STR = TRIM(VCAT%GVF_ENV_DIR(J1))//'/'//DB_NAME(1:19)
              IF ( DB_NAME(13:15) == '000' ) THEN
                   FL_NOVERS = .TRUE.
              END IF
            ELSE
               CALL ERR_LOG ( 4833, IUER, 'VCAT_RESOLVE_DBNAME', 'Wrong '// &
     &            'length of the DB_NAME argument '//TRIM(DB_NAME)//' -- '// &
     &            'it should be 10, 15, or 19 characters' )
             RETURN 
         END IF
         IF ( FL_NOVERS ) THEN
!
! ----------- Check all versions
!
              ILN = ILEN(STR)
              DO 420 J2=1,MAX_VER
                 INQUIRE ( FILE=STR, EXIST=LEX )
                 IF ( LEX ) THEN
                      ENV_FILE = STR
                 END IF
                 CALL INCH ( J2,      STR(ILN-6:ILN-4) )
                 CALL CHASHR (        STR(ILN-6:ILN-4) )
                 CALL BLANK_TO_ZERO ( STR(ILN-6:ILN-4) )
 420          CONTINUE 
              IF ( ILEN(ENV_FILE) > 0 ) THEN
                   IND_REP = J1
                   GOTO 810
              END IF
            ELSE
              INQUIRE ( FILE=STR, EXIST=LEX )
              IF ( LEX ) THEN
                   ENV_FILE = STR
                   IND_REP = J1
                   GOTO 810
             END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( ILEN(ENV_FILE) == 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 1, VCAT%NREPS )
           CALL ERR_LOG ( 4834, IUER, 'VCAT_RESOLVE_DBNAME', 'Cannot find '// &
     &         'an envelope file for '//TRIM(DB_NAME)//' in '//TRIM(STR)// &
     &         ' repositories'  )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( ENV_FILE, MAX_FIL, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4835, IUER, 'VCAT_RESOLVE_DBNAME', 'Error in an '// &
     &         'attempt to read database envelope file '//ENV_FILE )
           RETURN 
      END IF
!
      L_FIL = 0
      DO 430 J3=1,NBUF
         IF ( BUF(J3)(1:1)  == '#' ) GOTO 430
         IF ( BUF(J3)(1:1)  == '!' ) GOTO 430
         IF ( BUF(J3)(1:1)  == '$' ) GOTO 430
         IF ( ILEN(BUF(J3)) ==  0  ) GOTO 430
         L_FIL = L_FIL + 1
         IF ( L_FIL > M_FIL ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MAX_FIL, STR )
              CALL ERR_LOG ( 4836, IUER, 'VCAT_RESOLVE_DBNAME', 'Too many '// &
     &            'database file are defined in the envelope file '//TRIM(ENV_FILE)// &
     &            ' -- more than '//STR )
              RETURN
         END IF
!
! ------ Ancient code to provide comptibility with pre-2019 version of GVF
!
         IB = LINDEX ( ENV_FILE, '/' ) + 1
         IE = IB + 8
         IF ( IE .LE. IB ) IE = ILEN(ENV_FILE)
!
         IF ( BUF(J3)(21:21) .NE. ' ' ) THEN
              DB_FILE(L_FIL) = TRIM(VCAT%GVF_DB_DIR(IND_REP))//'/'// &
     &                         ENV_FILE(IB:IE)//BUF(J3)(21:21)//'_'// &
     &                         BUF(J3)(23:I_LEN(BUF(J3)))
            ELSE 
!
! ----------- Pre 2019 version of the envelop file
!
              DB_FILE(L_FIL) = TRIM(VCAT%GVF_DB_DIR(IND_REP))//'/'// &
     &                         ENV_FILE(IB:IE)
         END IF
         CALL BLANK_TO_ZERO ( BUF(J3)(13:15) )
         DB_FILE(L_FIL) = DB_FILE(L_FIL)(1:I_LEN(DB_FILE(L_FIL)))//'_'// &
     &                  BUF(J3)(9:11)//'_v'//BUF(J3)(13:15)//'.'// &
     &                  BUF(J3)(17:19)
         INQUIRE ( FILE=DB_FILE(L_FIL), EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              CALL ERR_LOG ( 4837, IUER, 'VCAT_RESOLVE_DBNAME', 'Did not '// &
     &            'find file '//TRIM(DB_FILE(L_FIL))//' defined in the '// &
     &            'envelope file '//ENV_FILE )
              RETURN 
         END IF
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VCAT_RESOLVE_DBNAME  !#!#
