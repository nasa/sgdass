      PROGRAM    GVF_SUPR_PROMOTE_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  GVF_SUPR_PROMOTE
! *                                                                      *
! * # 12-FEB-2010 GVF_SUPR_PROMOTE_MAIN v1.2 (c) L. Petrov 08-JUN-2020 # *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vcat.i'
#ifdef GVH_STANDALONE
      INCLUDE   'gvh_solve.i'
#else
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
#endif
      INTEGER*4  M_FIL
      PARAMETER  ( M_FIL = 128 )
      TYPE     ( VCAT__TYPE ) :: VCAT
      CHARACTER  ENV_FIL_INP*128, ENV_FIL*128, ENV_FIL_1ST*128, STR*128, VCAT_CONF_FILE*128, &
     &           GVF_DB_DIR*128, GVF_ENV_DIR*128, VCAT_REPO_NAME*128, DB_FILE(M_FIL)*128
      INTEGER*4  ID, IL, IVER, NOBS, NUM_AOBS, NUM_QOBS, IND_REP, L_FIL, IUER
      LOGICAL*4  LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, LTM_DIF
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gvf_supr_promote {env_file}'
           CALL  EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, ENV_FIL_INP )
      END IF
      ID = LINDEX ( ENV_FIL_INP, '/' ) 
      IF ( ID > 0 ) THEN
           ENV_FIL_INP = ENV_FIL_INP(ID+1:)
      END IF
!
      CALL GETENVAR  ( 'PSOLVE_SAVE_DIR', STR )
      IF ( ILEN(STR) == 0 ) THEN
#ifdef GVH_STANDALONE
           WRITE ( 6, '(A)' ) 'Please define environemnetr variable PSOLVE_SAVE_DIR'
           CALL EXIT ( 1 )
#else 
           STR = SOLVE_SAVE_DIR
#endif
      END IF
      IF ( STR(I_LEN(STR):I_LEN(STR)) .NE. '/' ) THEN
           STR = STR(1:I_LEN(STR))//'/'
      END IF
!
! --- First check the environment variable VCAT_CONF
!
      CALL GETENVAR  ( 'VCAT_CONF', VCAT_CONF_FILE )
      IF ( ILEN(VCAT_CONF_FILE) == 0 ) THEN
           VCAT_CONF_FILE = STR(1:I_LEN(STR))//'vcat.conf'
      END IF
      CALL GETENVAR ( 'VCAT_REPO', VCAT_REPO_NAME )
!
      IUER = -1
      CALL VCAT_GET_CONF ( VCAT_CONF_FILE, VCAT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 1901, IUER, 'GVF_SUPR_PROMOTE_MAIN', 'Failure in '// &
     &         'parsing VCAT configuration file '//VCAT_CONF_FILE )
           CALL EXIT ( 1 )
      END IF
!
! --- Resolve database name
!
      IUER = -1
      CALL VCAT_RESOLVE_DBNAME ( VCAT, ENV_FIL_INP, VCAT_REPO_NAME, ENV_FIL, &
     &                           M_FIL, L_FIL, DB_FILE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1902, IUER, 'GVF_SUPR_PROMOTE_MAIN', 'Error in an '// &
     &         'attempt to resolve database name '//ENV_FIL_INP )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(VCAT_REPO_NAME) == 0 ) THEN
           VCAT_REPO_NAME = VCAT%GVF_REP_NAME(1)
      END IF
!
      IND_REP = LTM_DIF ( 0, VCAT%NREPS, VCAT%GVF_REP_NAME, VCAT_REPO_NAME )
      IF ( IND_REP < 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 1903, IUER, 'GVF_SUPR_PROMOTE_MAIN', 'Error in an '// &
     &         'attempt to resolve database name '//ENV_FIL_INP )
           CALL EXIT ( 1 )
      END IF
      GVF_ENV_DIR = VCAT%GVF_ENV_DIR(IND_REP)
      GVF_DB_DIR  = VCAT%GVF_DB_DIR(IND_REP)
!
      INQUIRE ( FILE=ENV_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1904, -2, 'GVF_SUPR_PROMOTE_MAIN', 'Cannot find '// &
     &         'the envelop file '//ENV_FIL )
           CALL EXIT ( 1 )
      END IF
      IL = ILEN ( ENV_FIL ) 
      IF ( IL < 8 ) THEN
           CALL ERR_LOG ( 1905, -2, 'GVF_SUPR_PROMOTE_MAIN', 'Too short '// &
     &         'file name '//ENV_FIL )
           CALL EXIT ( 1 )
      END IF
      IF ( ENV_FIL(IL-3:IL) .NE. '.env' ) THEN
           CALL ERR_LOG ( 1906, -2, 'GVF_SUPR_PROMOTE_MAIN', 'Wrong '// &
     &         'extension of file '//ENV_FIL(1:I_LEN(ENV_FIL))// &
     &         ' -- .env was expected' )
           CALL EXIT ( 1 )
      END IF
      CALL CHIN ( ENV_FIL(IL-6:IL-4), IVER )
      IF ( IVER < 1 .OR. IVER > 999 ) THEN
           CALL ERR_LOG ( 1907, -2, 'GVF_SUPR_PROMOTE_MAIN', 'Wrong '// &
     &         'format of file name '//ENV_FIL(1:I_LEN(ENV_FIL))// &
     &         ' -- _vxxx.env was spected, where xxx is the version number' )
           CALL EXIT ( 1 )
      END IF
      IF ( IVER == 1 ) THEN
           CALL ERR_LOG ( 1908, -2, 'GVF_SUPR_PROMOTE_MAIN', 'Your file '// &
     &          ENV_FIL(1:I_LEN(ENV_FIL))//' has version 1 -- nothing to do' )
           CALL EXIT ( 0 )
      END IF
!
      ENV_FIL_1ST = ENV_FIL(1:IL-7)//'001'//ENV_FIL(IL-3:)
!
      INQUIRE ( FILE=ENV_FIL_1ST, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 1909, -2, 'GVF_SUPR_PROMOTE_MAIN', 'Cannot find '// &
     &         'file with the 1st version '//ENV_FIL_1ST )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GVF_SUPR_PROMOTE ( GVF_DB_DIR, ENV_FIL_1ST, ENV_FIL, NOBS, &
     &                        NUM_AOBS, NUM_QOBS, IUER )
      IF ( IUER .NE. 0 ) CALL  EXIT ( 1 )
      WRITE ( 6, '(A,A)'  ) 'Promoted suppression status to    ', TRIM(ENV_FIL)
      WRITE ( 6, '(A,I6)' ) 'Total number of observations:     ', NOBS
      WRITE ( 6, '(A,I6)' ) '# Obs with updated AUTO_SUP code: ', NUM_AOBS
      WRITE ( 6, '(A,I6)' ) '# Obs with updated QUALCODE code: ', NUM_QOBS
!
      END  PROGRAM  GVF_SUPR_PROMOTE_MAIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GVF_SUPR_PROMOTE ( GVF_DB_DIR, FIL_ENV_1ST, FIL_ENV, &
     &                              NOBS, NUM_AOBS, NUM_QOBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVF_SUPR_PROMOTE 
! *                                                                      *
! * ## 12-FEB-2010  GVF_SUPR_PROMOTE  v1.2 (c)  L. Petrov 2019.10.25 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
#ifdef GVH_STANDALONE
      INCLUDE   'gvh_solve.i'
#else
      INCLUDE   'solve.i'
      INCLUDE   'precm.i'
#endif
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      CHARACTER  GVF_DB_DIR*(*), FIL_ENV_1ST*(*), FIL_ENV*(*)
      INTEGER*4  NOBS, NUM_AOBS, NUM_QOBS, IUER
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4  MBUF, PIM__MBND
      PARAMETER ( MBUF = 64 )
      PARAMETER ( PIM__MBND = 2 )
      CHARACTER  FIL_GVF(MBUF)*128, BUF(MBUF)*128, EXP_NAME*128, STR*128, &
     &           QUALCODE(PIM__MBND)*2
      INTEGER*4, ALLOCATABLE :: AUTO_SUP_1ST(:), USER_SUP_1ST(:)
      REAL*8,    ALLOCATABLE :: EFF_FREQ(:,:)
      CHARACTER, ALLOCATABLE :: QUALCODE_1ST(:,:)*2
      INTEGER*4  J1, J2, J3, J4, J5, J6, NBUF, L_FIL, IB, IE, IV, EXP_VERSION, &
     &           REMAINED_BYTES, DIMS(3), AUTO_SUP, USER_SUP, &
     &           IND_SL1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      NOBS = 0 
      NUM_AOBS = 0
      NUM_QOBS = 0
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_ENV_1ST, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1921, IUER, 'GVF_SUPR_PROMOTE', 'Error in an '// &
     &         'atttempt to read database envelope file '//FIL_ENV_1ST )
           RETURN 
      END IF
!
      IB = LINDEX ( FIL_ENV_1ST, '/' ) + 1
      IE =  INDEX ( FIL_ENV_1ST(IB:), '_' ) + IB-2
      IF ( IE .LE. IB ) IE = ILEN(FIL_ENV_1ST)
      IV  = LINDEX ( FIL_ENV_1ST, '_v' )
      EXP_NAME = FIL_ENV_1ST(IB:IB+9)
      CALL CHIN ( FIL_ENV_1ST(IV+2:IV+4), EXP_VERSION )
!
! --- Collect names of the 1ST version file
!
      L_FIL = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '!' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '$' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         L_FIL = L_FIL + 1
         IF ( BUF(J1)(20:20) .EQ. ' ' .AND. BUF(J1)(22:22) .EQ. ' ' ) THEN
!
! ----------- Post-Oct2019 scheme
!
              FIL_GVF(L_FIL) = GVF_DB_DIR(1:I_LEN(GVF_DB_DIR))//'/'// &
     &                         FIL_ENV_1ST(IB:IE)//'_'//BUF(J1)(21:21)//'_'// &
     &                         BUF(J1)(23:I_LEN(BUF(J1)))
            ELSE IF ( BUF(J1)(21:21) .NE. ' ' ) THEN
              FIL_GVF(L_FIL) = GVF_DB_DIR(1:I_LEN(GVF_DB_DIR))//'/'// &
     &                         FIL_ENV_1ST(IB:IE)//'_'//BUF(J1)(21:I_LEN(BUF(J1)))
            ELSE 
              FIL_GVF(L_FIL) = FIL_ENV_1ST(IB:IE)
         END IF
         CALL BLANK_TO_ZERO ( BUF(J1)(13:15))
         FIL_GVF(L_FIL) = FIL_GVF(L_FIL)(1:I_LEN(FIL_GVF(L_FIL)))//'_'// &
   &                      BUF(J1)(9:11)//'_v'//BUF(J1)(13:15)//'.'// &
   &                      BUF(J1)(17:19)
 410  CONTINUE 
!     
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1922, IUER, 'GVF_SUPR_PROMOTE', 'Error in an '// &
     &         'attempt to initialize GVH' )
           RETURN 
      END IF
!
      DO 420 J2=1,L_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_READ_BGV ( GVH, 1, FIL_GVF(J2), REMAINED_BYTES, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1923, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'an atttempt to read input database file '// &
     &             FIL_GVF(J2) )
              RETURN 
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              CALL ERR_LOG ( 1924, IUER, 'GVF_SUPR_PROMOTE', 'The number '// &
     &            'of remaining bytes after reading input databae file '// &
     &             FIL_GVF(J2)(1:I_LEN(FIL_GVF(J2)))// &
     &             ' is not 0, but '//STR )
              RETURN 
         END IF
!@         write ( 6, * ) '1: l_fil= ',l_fil,' FIL_GVF= ', trim(fil_gvf(j2))
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREGET ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1925, IUER, 'GVF_SUPR_PROMOTE', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), NOBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1926, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &         'getting lcode NUMB_OBS' )
           RETURN 
      END IF
!
      ALLOCATE ( AUTO_SUP_1ST(NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*NOBS, STR )
           CALL ERR_LOG ( 1927, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array AUTO_SUP_1ST' )
           RETURN 
      END IF
!
      ALLOCATE ( USER_SUP_1ST(NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*NOBS, STR )
           CALL ERR_LOG ( 1928, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array USER_SUP_1ST' )
           RETURN 
      END IF
!
      ALLOCATE ( QUALCODE_1ST(2,NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 2*PIM__MBND*NOBS, STR )
           CALL ERR_LOG ( 1929, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array QUALCODE_1ST' )
           RETURN 
      END IF
!
      ALLOCATE ( EFF_FREQ(6,NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 2*PIM__MBND*NOBS, STR )
           CALL ERR_LOG ( 1930, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array EFF_FREQ' )
           RETURN 
      END IF
!
      DO 430 J3=1,NOBS
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'AUTO_SUP', J3, 1, 4, DIMS(1), DIMS(2), &
     &                     AUTO_SUP_1ST(J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1931, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'getting lcode AUTO_SUP' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'USER_SUP', J3, 1, 4, DIMS(1), DIMS(2), &
     &                     USER_SUP_1ST(J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1932, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'getting lcode USER_SUP' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'QUALCODE', J3, 1, 2*PIM__MBND, DIMS(1), &
     &                     DIMS(2), QUALCODE_1ST(1,J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1933, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'getting lcode QUALCODE' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'EFF_FREQ', J3, 1, 3*8*PIM__MBND, DIMS(1), &
     &                     DIMS(2), EFF_FREQ(1,J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1934, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'getting lcode EFF_FREQ' )
              RETURN
         END IF
 430  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL GVH_RELEASE ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1935, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &         'attempt to release memory allocated by GVH' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_ENV, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1936, IUER, 'GVF_SUPR_PROMOTE', 'Error in an '// &
     &         'atttempt to read database envelope file '//FIL_ENV )
           RETURN 
      END IF
!
! --- Collect names of the last version file
!
      L_FIL = 0
      DO 440 J4=1,NBUF
         IF ( BUF(J4)(1:1)  == '#' ) GOTO 440
         IF ( BUF(J4)(1:1)  == '!' ) GOTO 440
         IF ( BUF(J4)(1:1)  == '$' ) GOTO 440
         IF ( ILEN(BUF(J4)) ==  0  ) GOTO 440
         L_FIL = L_FIL + 1
         IF ( BUF(J4)(20:20) .EQ. ' ' .AND. BUF(J4)(22:22) .EQ. ' ' ) THEN
!
! ----------- Post-Oct2019 scheme
!
              FIL_GVF(L_FIL) = GVF_DB_DIR(1:I_LEN(GVF_DB_DIR))//'/'// &
     &                         FIL_ENV(IB:IE)//'_'//BUF(J4)(21:21)//'_'// &
     &                         BUF(J4)(23:I_LEN(BUF(J4)))
            ELSE IF ( BUF(J4)(21:21) .NE. ' ' ) THEN
              FIL_GVF(L_FIL) = GVF_DB_DIR(1:I_LEN(GVF_DB_DIR))//'/'// &
     &                         FIL_ENV(IB:IE)//'_'//BUF(J4)(21:I_LEN(BUF(J4)))
            ELSE 
              FIL_GVF(L_FIL) = FIL_ENV(IB:IE)
         END IF
         CALL BLANK_TO_ZERO ( BUF(J4)(13:15))
         FIL_GVF(L_FIL) = FIL_GVF(L_FIL)(1:I_LEN(FIL_GVF(L_FIL)))//'_'// &
   &                      BUF(J4)(9:11)//'_v'//BUF(J4)(13:15)//'.'// &
   &                      BUF(J4)(17:19)

!@         write ( 6, * ) '2: l_fil= ',l_fil,' FIL_GVF= ',FIL_GVF(l_fil)(1:i_len(FIL_GVF(l_fil)))
 440  CONTINUE 
!
      DO 450 J5=1,L_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_READ_BGV ( GVH, 1, FIL_GVF(J5), REMAINED_BYTES, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1937, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'an atttempt to read input database file '// &
     &             FIL_GVF(J5) )
              RETURN 
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              CALL ERR_LOG ( 1938, IUER, 'GVF_SUPR_PROMOTE', 'The number '// &
     &            'of remaining bytes after reading input databae file '// &
     &             FIL_GVF(J5)(1:I_LEN(FIL_GVF(J5)))// &
     &             ' is not 0, but '//STR )
              RETURN 
         END IF
!
         IF ( BUF(J5)(9:11) == 'sl1' ) THEN
              IND_SL1 = J5
         END IF
 450  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREGET ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1939, IUER, 'GVF_SUPR_PROMOTE', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           RETURN 
      END IF
!
      DO 460 J6=1,NOBS
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'AUTO_SUP', J6, 1, 4, DIMS(1), DIMS(2), &
     &                     AUTO_SUP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1940, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'getting lcode AUTO_SUP' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'USER_SUP', J6, 1, 4, DIMS(1), DIMS(2), &
     &                     USER_SUP, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1941, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'getting lcode USER_SUP' )
              RETURN
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'QUALCODE', J6, 1, 4, DIMS(1), DIMS(2), &
     &                     QUALCODE, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1942, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'getting lcode QUALCODE' )
              RETURN
         END IF
!
         IF ( AUTO_SUP .NE. AUTO_SUP_1ST(J6) ) THEN
              IF ( BTEST ( AUTO_SUP_1ST(J6), BQCX__SPS ) ) THEN
                   AUTO_SUP = IBSET ( AUTO_SUP, BQCX__SPS )
                   AUTO_SUP = IBSET ( AUTO_SUP, CBAD__SPS )
                   AUTO_SUP = IBCLR ( AUTO_SUP, GOOD__SPS )
                 ELSE 
                   AUTO_SUP = IBCLR ( AUTO_SUP, BQCX__SPS )
              END IF
              IF ( BTEST ( AUTO_SUP_1ST(J6), BQCS__SPS ) ) THEN
                   AUTO_SUP = IBSET ( AUTO_SUP, BQCS__SPS )
                   AUTO_SUP = IBSET ( AUTO_SUP, CBAD__SPS )
                   AUTO_SUP = IBCLR ( AUTO_SUP, GOOD__SPS )
                 ELSE 
                   AUTO_SUP = IBCLR ( AUTO_SUP, BQCS__SPS )
              END IF
!
              IF ( BTEST ( AUTO_SUP_1ST(J6), NOFX__SPS ) ) THEN
                   AUTO_SUP = IBSET ( AUTO_SUP, NOFX__SPS )
                   AUTO_SUP = IBSET ( AUTO_SUP, CBAD__SPS )
                   AUTO_SUP = IBCLR ( AUTO_SUP, GOOD__SPS )
                 ELSE 
                   AUTO_SUP = IBCLR ( AUTO_SUP, NOFX__SPS )
              END IF
!
              IF ( BTEST ( AUTO_SUP_1ST(J6), NOFS__SPS ) ) THEN
                   AUTO_SUP = IBSET ( AUTO_SUP, NOFS__SPS )
                   AUTO_SUP = IBSET ( AUTO_SUP, CBAD__SPS )
                   AUTO_SUP = IBCLR ( AUTO_SUP, GOOD__SPS )
                 ELSE 
                   AUTO_SUP = IBCLR ( AUTO_SUP, NOFS__SPS )
              END IF
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'AUTO_SUP', J6, 0, AUTO_SUP, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1943, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &                 'putting "AUTO_SUP" lcode' )
                   RETURN
              END IF
              NUM_AOBS = NUM_AOBS + 1
         END IF
!
         IF ( QUALCODE(1) .NE. QUALCODE_1ST(1,J6) .OR. &
     &        QUALCODE(2) .NE. QUALCODE_1ST(2,J6)      ) THEN
!
              CALL ERR_PASS ( IUER, IER )
              CALL GVH_PLCODE ( GVH, 'QUALCODE', J6, 0, QUALCODE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1944, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &                 'putting "QUALCODE" lcode' )
                   RETURN
              END IF
              NUM_QOBS = NUM_QOBS + 1
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'EFF_FREQ', J6, 0, EFF_FREQ(1,J6), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1945, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &            'putting "EFF_FREQ" lcode' )
              RETURN
         END IF
 460  CONTINUE 
!
      IF ( NUM_AOBS > 0 .OR. NUM_QOBS > 0 ) THEN
           CALL ERR_PASS ( IUER, IER ) 
           CALL GVH_WRITE_BGV ( GVH, IND_SL1, GVH__CRT, FIL_GVF(IND_SL1), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1946, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &              'attempt to write output database file '//FIL_GVF(IND_SL1) )
                RETURN 
           END IF
!@           write ( 6, * ) '3: updated file: ', trim(fil_gvf(ind_sl1))
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL GVH_RELEASE ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1947, IUER, 'GVF_SUPR_PROMOTE', 'Error in '// &
     &         'attempt to release memory allocated by GVH' )
           RETURN
      END IF
!
      DEALLOCATE ( AUTO_SUP_1ST )
      DEALLOCATE ( USER_SUP_1ST )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVF_SUPR_PROMOTE  !#!  
