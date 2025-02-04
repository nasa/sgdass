      PROGRAM    GVF_GRAMBIG_RESET_MAIN
! ************************************************************************
! *                                                                      *
! *   Program  GVF_GRAMBIG_RESET
! *                                                                      *
! * # 11-APR-2010 GVF_GRAMBIG_RESET_MAIN v1.1 (c) L. Petrov 08-JUN-2020 #*
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
      TYPE     ( VCAT__TYPE ) :: VCAT
      INTEGER*4  M_FIL
      PARAMETER  ( M_FIL = 32 )
      CHARACTER  ENV_FIL_INP*128, ENV_FIL*128, STR*128, VCAT_CONF_FILE*128, &
     &           GVF_DB_DIR*128, GVF_ENV_DIR*128, VTD_CONF_SES*128, &
     &           DB_FILE(M_FIL)*128, VCAT_REPO_NAME*128
      INTEGER*4  ID, IL, IVER, NOBS, NUM_UOBS, L_FIL, IUER
      LOGICAL*4  LEX
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      IF ( IARGC() < 1 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: gvf_supr_promote {env_file}'
           CALL  EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, ENV_FIL_INP )
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
           CALL ERR_LOG ( 2901, IUER, 'GVF_GRAMBIG_RESET_MAIN', 'Failure in '// &
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
           CALL ERR_LOG ( 2902, IUER, 'GVF_GRAMBIG_RESET_MAIN', 'Error in an '// &
     &         'attempt to resolve database name '//ENV_FIL_INP )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL GVF_GRAMBIG_RESET ( GVF_DB_DIR, ENV_FIL, NOBS, NUM_UOBS, IUER )
      IF ( IUER .NE. 0 ) CALL  EXIT ( 1 )
      WRITE ( 6, '(A,I6)' ) 'Total number of observations:    ', NOBS
      WRITE ( 6, '(A,I6)' ) '# Obs with updated N_GRAMB code: ', NUM_UOBS
!
      END  PROGRAM  GVF_GRAMBIG_RESET_MAIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE GVF_GRAMBIG_RESET ( GVF_DB_DIR, FIL_ENV, NOBS, NUM_UOBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVF_GRAMBIG_RESET 
! *                                                                      *
! * ## 12-FEB-2010  GVF_GRAMBIG_RESET  v1.0 (c) L. Petrov 12-FEB-2010 ## *
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
      CHARACTER  GVF_DB_DIR*(*), FIL_ENV*(*)
      INTEGER*4  NOBS, NUM_UOBS, IUER
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4  MBUF, PIM__MBND
      PARAMETER ( MBUF = 64 )
      PARAMETER ( PIM__MBND = 2 )
      CHARACTER  FIL_GVF(MBUF)*128, BUF(MBUF)*128, EXP_NAME*128, STR*128, &
     &           QUALCODE(PIM__MBND)*2
      INTEGER*4, ALLOCATABLE :: N_GRAMB(:,:)
      INTEGER*4  J1, J2, J3, J4, J5, J6, NBUF, L_FIL, IB, IE, IV, EXP_VERSION, &
     &           REMAINED_BYTES, DIMS(3), NAMB, IND_SL1, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX
!
      NOBS = 0 
      NUM_UOBS = 0
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_ENV, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2911, IUER, 'GVF_GRAMBIG_RESET', 'Error in an '// &
     &         'atttempt to read database envelope file '//FIL_ENV )
           RETURN 
      END IF
!
      IB = LINDEX ( FIL_ENV, '/' ) + 1
      IE =  INDEX ( FIL_ENV(IB:), '_' ) + IB-2
      IF ( IE .LE. IB ) IE = ILEN(FIL_ENV)
      IV  = LINDEX ( FIL_ENV, '_v' )
      EXP_NAME = FIL_ENV(IB:IB+9)
      CALL CHIN ( FIL_ENV(IV+2:IV+4), EXP_VERSION )
!
! --- Collect names of the data files
!
      L_FIL = 0
      DO 410 J1=1,NBUF
         IF ( BUF(J1)(1:1)  == '#' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '!' ) GOTO 410
         IF ( BUF(J1)(1:1)  == '$' ) GOTO 410
         IF ( ILEN(BUF(J1)) ==  0  ) GOTO 410
         L_FIL = L_FIL + 1
         IF ( BUF(J1)(21:21) .NE. ' ' ) THEN
              FIL_GVF(L_FIL) = GVF_DB_DIR(1:I_LEN(GVF_DB_DIR))//'/'// &
     &                         FIL_ENV(IB:IE)//'_'//BUF(J1)(21:I_LEN(BUF(J1)))
            ELSE 
              FIL_GVF(L_FIL) = FIL_ENV(IB:IE)
         END IF
         CALL BLANK_TO_ZERO ( BUF(J1)(13:15))
         FIL_GVF(L_FIL) = FIL_GVF(L_FIL)(1:I_LEN(FIL_GVF(L_FIL)))//'_'// &
   &                      BUF(J1)(9:11)//'_v'//BUF(J1)(13:15)//'.'// &
   &                      BUF(J1)(17:19)
!
         IF ( BUF(J1)(9:11) == 'sl1' ) THEN
              IND_SL1 = J1
         END IF
!!       write ( 6, * ) '1: l_fil= ',l_fil,' FIL_GVF= ',FIL_GVF(l_fil)(1:i_len(FIL_GVF(l_fil)))
 410  CONTINUE 
!     
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_INIT ( GVH,  IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2912, IUER, 'GVF_GRAMBIG_RESET', 'Error in an '// &
     &         'attempt to initialize GVH' )
           RETURN 
      END IF
!
      DO 420 J2=1,L_FIL
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_READ_BGV ( GVH, 1, FIL_GVF(J2), REMAINED_BYTES, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2913, IUER, 'GVF_GRAMBIG_RESET', 'Error in '// &
     &            'an atttempt to read input database file '// &
     &             FIL_GVF(J2) )
              RETURN 
         END IF
         IF ( REMAINED_BYTES .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( REMAINED_BYTES, STR )
              CALL ERR_LOG ( 2914, IUER, 'GVF_GRAMBIG_RESET', 'The number '// &
     &            'of remaining bytes after reading input databae file '// &
     &             FIL_GVF(J2)(1:I_LEN(FIL_GVF(J2)))// &
     &             ' is not 0, but '//STR )
              RETURN 
         END IF
 420  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREGET ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2915, IUER, 'GVF_GRAMBIG_RESET', 'Error in an '// &
     &         'attempt to execute GVH_PREGET' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'NUMB_OBS', 0, 0, 4, DIMS(1), DIMS(2), NOBS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2916, IUER, 'GVF_GRAMBIG_RESET', 'Error in '// &
     &         'getting lcode NUMB_OBS' )
           RETURN 
      END IF
!
      ALLOCATE ( N_GRAMB(2,NOBS), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL IINCH ( 4*NOBS, STR )
           CALL ERR_LOG ( 2917, IUER, 'GVF_GRAMBIG_RESET', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array AUTO_SUP_1ST' )
           RETURN 
      END IF
!
      DO 430 J3=1,NOBS
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, 'N_GRAMB ', J3, 1, 8, DIMS(1), DIMS(2), &
     &                     N_GRAMB(1,J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2920, IUER, 'GVF_GRAMBIG_RESET', 'Error in '// &
     &            'getting lcode N_GRAMB ' )
              RETURN
         END IF
!
         IF ( N_GRAMB(1,J3) .NE. 0 ) NUM_UOBS = NUM_UOBS + 1
         IF ( N_GRAMB(2,J3) .NE. 0 ) NUM_UOBS = NUM_UOBS + 1
         N_GRAMB(1,J3) = 0
         N_GRAMB(2,J3) = 0
!
         CALL ERR_PASS ( IUER, IER )
         CALL GVH_PLCODE ( GVH, 'N_GRAMB ', J3, 0, N_GRAMB(1,J3), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2931, IUER, 'GVF_GRAMBIG_RESET', 'Error in '// &
     &            'putting "N_GRAMB " lcode' )
              RETURN
         END IF
 430  CONTINUE 
!
      IF ( NUM_UOBS > 0 ) THEN
           CALL ERR_PASS ( IUER, IER ) 
           CALL GVH_WRITE_BGV ( GVH, IND_SL1, GVH__CRT, FIL_GVF(IND_SL1), IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 2933, IUER, 'GVF_GRAMBIG_RESET', 'Error in '// &
     &              'attempt to write output database file '//FIL_GVF(IND_SL1) )
                RETURN 
           END IF
      END IF
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL GVH_RELEASE ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2934, IUER, 'GVF_GRAMBIG_RESET', 'Error in '// &
     &         'attempt to release memory allocated by GVH' )
           RETURN
      END IF
!
      DEALLOCATE ( N_GRAMB )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVF_GRAMBIG_RESET  !#!  
