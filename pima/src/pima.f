#include <mk5_preprocessor_directives.inc>
       PROGRAM    PIMA_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*4, EXTERNAL :: ILEN
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL GETENVAR ( 'GOMP_STACKSIZE', STR )
       IF ( ILEN(STR) == 0 ) THEN
            CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       END IF
       CALL ERR_MODE ( 'NO_PROBE' )
!
       CALL PIMA()
       END  PROGRAM  PIMA_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE PIMA()
! ************************************************************************
! *                                                                      *
! *   Program PIMA is for analysis of the spectrum of the complex        *
! *   cross-correlation function measured with VLBI. It performs         *
! *   amplitude and phase calibration, find phase delay, phase delay     *
! *   rate, singleband and mutlitband delay which corresponds to the     *
! *   maximum of fringe amplitude, performs quality check and make many  *
! *   other useful things.                                               *
! *                                                                      *
! *  ### 06-JAN-2006      PIMA    v1.33 (c)  L. Petrov  08-JAN-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_local.i'
      INCLUDE   'vtd.i'
      INCLUDE   'gvh.i'
      TYPE     ( PIMA__TYPE     ),   POINTER :: PIM(:)
      TYPE     ( PIM_STA__TYPE  ) :: STA_CAT(PIM__MCST)
      TYPE     ( PIM_SOU__TYPE  ) :: SOU_CAT(PIM__MCSO)
      TYPE     ( PIM_CONF__TYPE ) :: CONF
      TYPE     ( VTD__TYPE      ),   POINTER :: VTD
      LOGICAL*4  LEX, GEPM_OVR
      CHARACTER  PIMA_CONF_FILE*128, PRCS_CODE*8, FINAM*128, PETOOLS_LABEL*30, &
     &           STR*128, STR1*128, STR2*128
      CHARACTER  KEYWORD(PIM__MOPT)*80, VALUE(PIM__MOPT)*80, &
     &           ACT_CODE*128, PIMA_OPERATION*128, STA_NAM(2)*8, &
     &           FILE_PLOT*128, TSPL_DATA*128, GEPM_OUTFILE*128, REG*5
      PARAMETER  ( REG = CHAR(0)//CHAR(32)//CHAR(9)//',;' )
      INTEGER*4  MIND
      PARAMETER  ( MIND = 16 )
      REAL*8     GAIN_VAL
      INTEGER*4  L_CST, L_CSO, IP, L_OPT, L_ACT, LIND, IND(2,MIND), &
     &           J1, J2, J3, IDEV, IND_STA1, IND_STA2, IB, IL, IR, &
     &           GEPM_MCHAN, PCAL_TYPE, MAX_COUNT, IUER
      REAL*8     TREC_CONST, TATM_CONST, TIM_MSEG, TIME_THRESH, DIFF_THRESH
      INTEGER*8  DIR_DESC(16)
      INTEGER*8, EXTERNAL :: FUNC_OPENDIR
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LINDEX, LTM_DIF, RENAME, &
     &                       OMP_GET_MAX_THREADS, OMP_GET_THREAD_NUM, &
     &                       OMP_GET_NUM_THREADS, CLOSEDIR
      CHARACTER, EXTERNAL :: GET_CDATE_MS*23 
!
      IF ( IARGC() < 2 ) THEN
           CALL GETARG ( 1, KEYWORD(1) )
           IF ( KEYWORD(1)(1:2) == '-v' .OR. &
     &          KEYWORD(1)(1:3) == '--v'     ) THEN
                WRITE ( 6, '(A)' ) PIMA__LABEL
                CALL EXIT ( 0 )
           END IF
           IF ( KEYWORD(1)(1:12) == '-dumpversion' .OR. &
     &          KEYWORD(1)(1:13) == '--dumpversion'     ) THEN
                CALL PETOOLS_VERS ( PETOOLS_LABEL )
!
                WRITE ( 6, '(A)' ) PIMA__LABEL
                WRITE ( 6, '(A)' ) GVH__LABEL 
                WRITE ( 6, '(A)' ) VTD__LABEL
                WRITE ( 6, '(A)' ) SPD_CLIENT__LABEL
                WRITE ( 6, '(A)' ) NERS__LABEL
                WRITE ( 6, '(A)' ) PETOOLS_LABEL
                CALL EXIT ( 0 )
           END IF
           IF ( KEYWORD(1)(1:12) == '--share' ) THEN
                WRITE ( 6, '(A)' ) PIMA__ROOT//'/share/pima'
                CALL EXIT ( 0 )
           END IF
           WRITE ( 6, '(A)' ) 'Usage: pima <control_file> <operation(s)> [options]'
           CALL EXIT ( 1 )
         ELSE
           IF ( MOD(IARGC(),2) == 1 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IARGC(), STR )
                IUER = -1
                CALL ERR_LOG ( 7001, IUER, 'PIMA', 'The number of arguments '// &
     &              'should be even, but you specified '//STR )
                CALL EXIT ( 1 )
           END IF
!
           CALL GETARG ( 1, PIMA_CONF_FILE )
           CALL GETARG ( 2, ACT_CODE )
           L_OPT = IARGC()/2 - 1
           DO 410 J1=1,L_OPT
              CALL GETARG ( J1*2+1, KEYWORD(J1) )
              CALL GETARG ( J1*2+2, VALUE(J1)   )
 410       CONTINUE
      END IF
!
      CALL SET_SIGNAL_CTRLC ( 1 )
      IUER = -1
      CALL PIMA_VERSION_CHECK ( IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7002, IUER, 'PIMA', 'Version mismatch. You need '// &
     &         'recompile and/or relink PIMA' )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=PIMA_CONF_FILE, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 7003, IUER, 'PIMA', 'Cannot find control file '// &
     &          PIMA_CONF_FILE )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( PIM(1), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(PIM), STR )
           IUER = -1
           CALL ERR_LOG ( 7004, IUER, 'PIMA', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for PIMA '// &
     &         'internal data structures. What is going on? Do you really '// &
     &         'have so few memory?' )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( VTD, STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( SIZEOF(VTD), STR )
           IUER = -1
           CALL ERR_LOG ( 7005, IUER, 'PIMA', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for VTD '// &
     &         'internal data structures. What is going on? Do you really '// &
     &         'have so few memory?' )
           CALL EXIT ( 1 )
      END IF
!
      CALL PIMA_INIT ( PIM(1) )
!
      CALL EXWORD ( ACT_CODE, MIND, LIND, IND, REG, -3 )
      PIMA_OPERATION = '?'
      DO 420 J2=1,LIND
         IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'load' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__LOAD_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'frib' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__FRIB_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'frip' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__FRIP_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'tspl' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__TSPL_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'pcpl' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__PCPL_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'pdpl' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__PDPL_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'acpl' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__ACPL_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'gean' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__GEAN_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'moim' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__MOIM_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'mkdb' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__MKDB_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'mppl' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__MPPL_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'bpas' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__BPAS_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'bplt' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__BPLT_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'pplt' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__PPLT_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'bmge' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__BMGE_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'pmge' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__PMGE_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'dipc' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__DIPC_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'upgr' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__UPGR_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'prga' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__PRGA_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'splt' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__SPLT_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'acta' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__ACTA_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'opag' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__OPAG_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'opal' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__OPAL_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'onof' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__ONOF_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'gaco' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__GACO_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'frtr' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__FRTR_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'tsmo' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__TSMO_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'gena' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__GENA_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'gepm' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__GEPM_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'clpc' ) THEN
              PIM(1)%CONF%ACT_CODE = PIMA__CLPC_CODE
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'tst1' ) THEN
              PIM(1)%CONF%ACT_CODE = 'tst1'
            ELSE IF ( ACT_CODE(IND(1,J2):IND(2,J2)) == 'check' ) THEN
              WRITE ( 6, * ) 'CONF_LEN:  ', LOC(PIM(1)%CONF%LAST_FIELD) - LOC(PIM(1)%CONF%BAND) + &
     &                       SIZEOF(PIM(1)%CONF%LAST_FIELD)
              WRITE ( 6, * ) 'PIM__CNFL: ', INT8(PIM__CNFL)
              CALL EXIT ( 0 )
            ELSE
              IUER = -1
              CALL ERR_LOG ( 7006, IUER, 'PIMA', 'Unrecoginzed operation code: '// &
     &             ACT_CODE(IND(1,J2):IND(2,J2))//' -- List of recognzed '// &
     &             'codes: load frib tspl pcpl pdpl acpl gean mkdb moim '// &
     &             'bpas bplt pplt bmge pmge dipc upgr splt prga '// &
     &             'opag opal onof mppl gaco frtr tsmo gena gepm clpc' )
              CALL EXIT ( 1 )
         ENDIF
 420  CONTINUE
      IF ( PIM__CNFL .NE. LOC(PIM(1)%CONF%LAST_FIELD) - LOC(PIM(1)%CONF%BAND) + &
     &                       SIZEOF(PIM(1)%CONF%LAST_FIELD) ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL INCH  ( PIM__CNFL, STR )
           CALL INCH  ( LOC(PIM(1)%CONF%LAST_FIELD) - LOC(PIM(1)%CONF%BAND) + &
     &                  SIZEOF(PIM(1)%CONF%LAST_FIELD), STR1 )
           IUER = -1
           CALL ERR_LOG ( 7007, IUER, 'PIMA', 'Trap of internal control: '// &
     &         'PIM__CNFL = '//STR(1:I_LEN(STR))//' Actual length: '// &
     &          STR1(1:I_LEN(STR1))//' -- please correct parameter PIM__FILL' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__UPGR_CODE ) THEN
           IUER = -1
           CALL PIMA_UPGR ( PIMA_CONF_FILE, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7008, IUER, 'PIMA', 'Failure to '// &
     &              'upgrade configuration file '//PIMA_CONF_FILE )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__BMGE_CODE ) THEN
           IF ( KEYWORD(1) .NE. 'mask_gen' ) THEN
                CALL ERR_LOG ( 7009, -2, 'PIMA', 'Keyword mask_gen was '// &
     &              'expected immediatly after bmge operation code' )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__PMGE_CODE ) THEN
           IF ( KEYWORD(1) .NE. 'mask_gen' ) THEN
                CALL ERR_LOG ( 7010, -2, 'PIMA', 'Keyword mask_gen was '// &
     &              'expected immediatly after pmge operation code' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL PIMA_CONF ( PIMA_CONF_FILE, PIM(1), L_OPT, KEYWORD, VALUE, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7011, IUER, 'PIMA', 'Error in parsing '// &
     &         'control file '//TRIM(PIMA_CONF_FILE) )
           CALL EXIT ( 1 )
      END IF
!
      IF ( ILEN(PIM(1)%CONF%STAGING_DIR) > 0 ) THEN
           IUER = -1
           CALL PIMA_STAGING ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7012, IUER, 'PIMA', 'Failure in an attempt '// &
     &              'to move the data into the staging directory' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__LOAD_CODE ) THEN
           IUER = -1
           CALL PIMA_LOAD_STACAT ( PIM(1), L_CST, STA_CAT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7013, IUER, 'PIMA', 'Error in parsing '// &
     &             'control file '//PIMA_CONF_FILE )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL PIMA_LOAD_SOUCAT ( PIM(1), L_CSO, SOU_CAT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7014, IUER, 'PIMA', 'Error in parsing '// &
     &              'control file '//PIMA_CONF_FILE )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL PIMA_UV_EXCLUDE ( PIM, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7015, IUER, 'PIMA', 'Error in an attempt '// &
     &              'to load UV exclusion file' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL PIMA_INDX ( PIM(1), L_CST, STA_CAT, L_CSO, SOU_CAT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7016, IUER, 'PIMA', 'Error in attempt '// &
     &              'to index input FITS-IDI files' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL PIMA_THEO ( PIM, VTD, 'OBS_BEG', '1ST_OBS', IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7017, IUER, 'PIMA', 'Error in an attempt '// &
     &              'to compute theoretical path delays' )
                CALL EXIT ( 1 )
           END IF
!
           IF ( PIM(1)%CONF%PHAS_CAL_CODE .NE. PIMA__PCAL_NO ) THEN
                IUER = -1
                CALL PIMA_GET_PCAL ( PIM(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7018, IUER, 'PIMA', 'Error in attempt '// &
          &              'to extract phase calibration information '// &
     &                   'from FITS-IDI files' )
                     CALL EXIT ( 1 )
                END IF
           END IF
!
           IF ( PIM(1)%CONF%TSYS_CAL_CODE .NE. PIMA__TSYS_NO ) THEN
                IUER = -1
                CALL PIMA_GET_TSYS ( PIM(1), VTD, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7019, IUER, 'PIMA', 'Error in '// &
          &              'attempt to extract system temperature '// &
     &                   'information from FITS-IDI files' )
                    CALL EXIT ( 1 )
                END IF
           END IF
!
           IF ( PIM(1)%CONF%GAIN_CAL_CODE .NE. PIMA__GAIN_NO ) THEN
                IUER = -1
                CALL PIMA_GET_GAIN ( PIM(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7020, IUER, 'PIMA', 'Error in '// &
          &              'attempt to extract antenna gain information '// &
     &                   'from FITS-IDI files' )
                     CALL EXIT ( 1 )
                END IF
           END IF
!
           IUER = -1
           CALL PIMA_WRITE_OBS ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7021, IUER, 'PIMA', 'Error in attempt to '// &
          &         'write observation file into output directory '// &
     &              PIM(1)%CONF%EXPER_DIR )
                CALL EXIT ( 1 )
           END IF
         ELSE
           IUER = -1
           FINAM = PIM(1)%CONF%SESS_CODE(1:I_LEN(PIM(1)%CONF%SESS_CODE))//'.pim'
           IP = I_LEN(PIM(1)%CONF%EXPER_DIR)
           IF ( PIM(1)%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                FINAM = PIM(1)%CONF%EXPER_DIR(1:IP)//FINAM
              ELSE
                FINAM = PIM(1)%CONF%EXPER_DIR(1:IP)//'/'//FINAM
           END IF
           CONF = PIM(1)%CONF  ! Save confgirutaton
!
           IF ( ILEN(PIM(1)%CONF%MKDB_2ND_BAND_FILE) > 0 ) THEN
                PIM(1)%NBND = 2
              ELSE 
                PIM(1)%NBND = 1
           END IF
!
           IUER  = -1
           CALL PIMA_READ_OBS ( FINAM, CONF, PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7022, IUER, 'PIMA', 'Error in attempt to '// &
          &         'read observation file from the input directory '// &
     &               PIM(1)%CONF%EXPER_DIR )
                CALL EXIT ( 1 )
           END IF
           PIM(1)%CONF = CONF ! Restore configuration
           PIM(1)%CONF_FILE = PIMA_CONF_FILE
           IF ( PIM(1)%CONF%FRQ_GRP > PIM(1)%NFRG ) THEN
                CALL CLRCH ( STR ) 
                CALL INCH  ( PIM(1)%CONF%FRQ_GRP, STR )
                CALL CLRCH ( STR1 ) 
                CALL INCH  ( PIM(1)%NFRG, STR1 )
                IUER = -1
                CALL ERR_LOG ( 7023, IUER, 'PIMA', 'Wrong keyword '// &
     &              'FRQ_GRP: '//STR(1:I_LEN(STR))//' -- the frequency group '// &
     &               'index should be no greater than '//STR1 )
                CALL EXIT ( 1 )
           END IF
           IF ( PIM(1)%CONF%END_FRQ  > PIM(1)%NFRQ ) THEN
                CALL CLRCH ( STR ) 
                CALL INCH  ( PIM(1)%CONF%END_FRQ, STR )
                CALL CLRCH ( STR1 ) 
                CALL INCH  ( PIM(1)%NFRQ, STR1 )
                IUER = -1
                CALL ERR_LOG ( 7024, IUER, 'PIMA', 'Wrong keyword '// &
     &              'END_FRQ: '//STR(1:I_LEN(STR))//' -- the end frequency '// &
     &               'index should be no greater than '//STR1 )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == 'tst1' ) THEN
           IUER = -1
!!           CALL PIMA_TST1 ( PIM(1), VTD, IUER )
           CALL PIMA_GET_ADDCLO ( PIM(1), IUER )
           CALL EXIT ( 0 )
      END IF 
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__PRGA_CODE ) THEN
           IUER = -1
           CALL PIMA_PRGA ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7025, IUER, 'PIMA', 'Failure to '// &
     &              'print gain table' )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__OPAG_CODE ) THEN
           IF ( ILEN(KEYWORD(1)) == 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7026, IUER, 'PIMA', 'Keyword atmo_dir was '// &
     &              'expected to follow action opag' )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(VALUE(1)) == 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7027, IUER, 'PIMA', 'Value is expected '// &
     &              'to follow qualifier atmo_dir' )
                CALL EXIT ( 1 )
           END IF
           IF ( KEYWORD(1) .NE. 'spd_url' ) THEN
                IUER = -1
                CALL ERR_LOG ( 7028, IUER, 'PIMA', 'Unsupported keyword '// &
     &               KEYWORD(1)(1:I_LEN(KEYWORD(1)))//' atmo_dir was expected' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Download slant path delay, atmospheric opacity and atmosphere
! -------- brightness temperature on a az/el grid and put in in directory
! -------- VALUE(1)
!
           IUER = -1
           CALL PIMA_OPAG ( PIM(1), VTD, VALUE(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7030, IUER, 'PIMA', 'Failure to '// &
     &              'compute opacity from numerical weather model output' )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__OPAL_CODE ) THEN
!
! -------- Load station and source catalogues
!
           IUER = -1
           CALL PIMA_LOAD_STACAT ( PIM(1), L_CST, STA_CAT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7031, IUER, 'PIMA', 'Error in an attempt '// &
     &              'to load station file ' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL PIMA_LOAD_SOUCAT ( PIM(1), L_CSO, SOU_CAT, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7032, IUER, 'PIMA', 'Error in an attempt '// &
     &              'to load source file' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Compute path delay 
!
           IUER = -1
           CALL PIMA_THEO ( PIM, VTD, 'OBS_BEG', '1ST_OBS', IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7033, IUER, 'PIMA', 'Error in an attempt '// &
     &              'to compute theoretical path delays' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Load opacity and atmosphere temperature data into PIMA internal data structure
!
           IUER = -1
           CALL PIMA_OPAL ( PIM(1), VTD, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7034, IUER, 'PIMA', 'Failure to load '// &
     &              'opacity and atmosphere temperature data into '// &
     &              'PIMA internal data structure during processing '// &
     &              'experiment '//PIM(1)%CONF%SESS_CODE )
                CALL EXIT ( 1 )
           END IF
!
! -------- Write updated object PIM(1) into the file with extension .pim
!
           IUER = -1
           CALL PIMA_WRITE_OBS ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7035, IUER, 'PIMA', 'Error in attempt to '// &
          &         'write observation file for experiment '// &
     &               PIM(1)%CONF%SESS_CODE(1:I_LEN(PIM(1)%CONF%SESS_CODE))// &
     &              'into the output directory '// &
     &               PIM(1)%CONF%EXPER_DIR )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__BMGE_CODE ) THEN
           IF ( KEYWORD(1) .NE. 'mask_gen' ) THEN
                CALL ERR_LOG ( 7036, -2, 'PIMA', 'Upsupported option '// &
     &               KEYWORD(1)(1:I_LEN(KEYWORD(1)))//' for operation bmge: '// &
     &              ' mask_gen was expected' )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(VALUE(1)) == 0 ) THEN
                CALL ERR_LOG ( 7037, -2, 'PIMA', 'No value is provided '// &
     &              'after option '//KEYWORD(1)(1:I_LEN(KEYWORD(1)))// &
     &              ' . Name of the the mask generator file was exepcted' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL PARSE_BPS_GEN_CNT ( PIM, VALUE(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7038, -2, 'PIMA', &
     &              'Error in an attempt to parse bandpass '// &
     &              'generation file '//VALUE(1) )
                CALL EXIT ( 1 ) 
           END IF
!
           IUER = -1
           CALL WRI_BANDPASS_MASK ( PIM, VALUE(1), &
     &                              PIM(1)%CONF%BANDPASS_MASK_FILE, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7039, -2, 'PIMA', 'Error '// &
     &              'in an attempt to write bandpass mask file '//VALUE(1) )
                CALL EXIT ( 1 ) 
           END IF
           CALL EXIT ( 0 )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__PMGE_CODE ) THEN
           IF ( KEYWORD(1) .NE. 'mask_gen' ) THEN
                CALL ERR_LOG ( 7040, -2, 'PIMA', 'Upsupported option '// &
     &               KEYWORD(1)(1:I_LEN(KEYWORD(1)))//' for operation pmge: '// &
     &              ' mask_gen was expected' )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(VALUE(1)) == 0 ) THEN
                CALL ERR_LOG ( 7041, -2, 'PIMA', 'No value is provided '// &
     &              'after option '//KEYWORD(1)(1:I_LEN(KEYWORD(1)))// &
     &              ' . Name of the the mask generator file was exepcted' )
                CALL EXIT ( 1 )
           END IF
!
           IUER = -1
           CALL PARSE_PCAL_MASK_GEN_CNT ( PIM, VALUE(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7042, -2, 'PIMA', &
     &              'Error in an attempt to parse pcal mask '// &
     &              'generation file '//VALUE(1) )
                CALL EXIT ( 1 ) 
           END IF
!
           IUER = -1
           CALL WRI_PCAL_MASK ( PIM, VALUE(1), &
     &                              PIM(1)%CONF%PCAL_MASK_FILE, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7043, -2, 'PIMA', 'Error '// &
     &              'in an attempt to write pcal mask file '//VALUE(1) )
                CALL EXIT ( 1 ) 
           END IF
           CALL EXIT ( 0 )
      END IF
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__MOIM_CODE ) THEN
           IF ( PIM(1)%CONF%INTMOD_TYPE == PIMA__MOD_VERA1000 .OR. &
     &          PIM(1)%CONF%INTMOD_TYPE == PIMA__MOD_VERA2000      )  THEN
                IUER = -1
                CALL PIMA_LOAD_VERA_MOD ( PIM, VTD, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7044, -2, 'PIMA', 'Failure to '// &
     &                   'load VERA Interferometric Model from directory '// &
     &                    PIM(1)%CONF%INTMOD_FILE(1) )
                     CALL EXIT ( 1 )
               END IF
             ELSE IF ( PIM(1)%CONF%INTMOD_TYPE == PIMA__MOD_SFXC ) THEN
                IUER = -1
                CALL PIMA_LOAD_SFXC_MOD ( PIM, VTD, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7045, -2, 'PIMA', 'Failure to '// &
     &                   'load SFXC Interferometric Model from directory '// &
     &                    PIM(1)%CONF%INTMOD_FILE(1) )
                     CALL EXIT ( 1 )
               END IF
             ELSE IF ( PIM(1)%CONF%INTMOD_TYPE == PIMA__MOD_KJCC ) THEN
                IUER = -1
                CALL PIMA_LOAD_KJCC_MOD ( PIM, VTD, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7046, -2, 'PIMA', 'Failure to '// &
     &                   'load KJCC Interferometric Model from directory '// &
     &                    PIM(1)%CONF%INTMOD_FILE(1) )
                     CALL EXIT ( 1 )
               END IF
             ELSE IF ( PIM(1)%CONF%INTMOD_TYPE == PIMA__MOD_NO ) THEN
               CALL ERR_LOG ( 7047, -2, 'PIMA', 'Interferometric model type '// &
     &             'has not been defined in the control file' )
               CALL EXIT ( 1 )
             ELSE 
               CALL ERR_LOG ( 7048, -2, 'PIMA', 'Interferometric model type '// &
     &              PIM(1)%CONF%INTMOD_TYPE//' is not supported' )
               CALL EXIT ( 1 )
           END IF
!
! -------- Write updated object PIM(1) into the file with extension .pim
!
           IUER = -1
           CALL PIMA_WRITE_OBS ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7049, IUER, 'PIMA', 'Error in attempt to '// &
          &         'write observation file into output directory '// &
     &              PIM(1)%CONF%EXPER_DIR )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
!
      IF ( PIM(1)%CONF%BANDPASS_MASK_FILE .NE. PIMA__BPASS_NO  .AND. &
     &     PIM(1)%CONF%ACT_CODE .NE. PIMA__GEPM_CODE                 ) THEN
!
! -------- Read the file with bandpass mask
!
           IUER  = -1
           CALL READ_BANDPASS_MASK ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7050, IUER, 'PIMA', 'Error in attempt to '// &
          &         'load the bandpass mask from file '// &
     &              PIM(1)%CONF%BANDPASS_MASK_FILE )
                CALL EXIT ( 1 )
           END IF
         ELSE
           PIM(1)%BANDPASS_MASK_STYLE = PIMA__NO
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__BPLT_CODE .OR. &
     &     PIM(1)%CONF%ACT_CODE == PIMA__PPLT_CODE      ) THEN
!
! -------- Plot of the bandpass
!
           CALL CLRCH ( STA_NAM(1) ) 
           CALL CLRCH ( STA_NAM(2) ) 
           CALL CLRCH ( FILE_PLOT ) 
           IF ( KEYWORD(1) .NE. 'sta1' ) THEN
                CALL ERR_LOG ( 7051, -2, 'PIMA', &
     &              'The first keyword is '//KEYWORD(1)(1:I_LEN(KEYWORD(1)))// &
     &              ' while sta1 was expected' )
                CALL EXIT ( 1 ) 
           END IF
           STA_NAM(1) = VALUE(1)
           IF ( KEYWORD(2) .EQ. 'sta2' ) THEN
                STA_NAM(2) = VALUE(2)
              ELSE IF ( KEYWORD(2) .EQ. 'plot_file' ) THEN
                FILE_PLOT = VALUE(2)
              ELSE IF ( ILEN(KEYWORD(2)) .NE. 0 ) THEN
                CALL ERR_LOG ( 7052, -2, 'PIMA', 'Unsupported 2nd keyword: '// &
     &               KEYWORD(2)(1:I_LEN(KEYWORD(2)))//' while sta2 or plot_file '// &
     &              'were expected' )
                CALL EXIT ( 1 ) 
           END IF
           IF ( KEYWORD(3) .EQ. 'plot_file' ) THEN
                FILE_PLOT = VALUE(3)
!?              ELSE IF ( ILEN(KEYWORD(3)) .NE. 0 ) THEN
!?                CALL ERR_LOG ( 7053, -2, 'PIMA', 'Unsupported 2nd keyword: '// &
!?     &               KEYWORD(2)(1:I_LEN(KEYWORD(2)))//' while plot_file '// &
!?     &              'was expected' )
!?                CALL EXIT ( 1 ) 
           END IF
           IF ( ILEN(STA_NAM(2)) == 0 ) STA_NAM(2) = STA_NAM(1) 
!
! -------- Read the file with bandpass
!
           IF ( PIM(1)%CONF%ACT_CODE == PIMA__BPLT_CODE ) THEN
                IUER  = -1
                CALL PIMA_READ_BPASS ( PIM(1)%CONF%BANDPASS_FILE, PIM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7054, IUER, 'PIMA', 'Error in attempt to '// &
               &         'load bandpasses from file '//PIM(1)%CONF%BANDPASS_FILE )
                     CALL EXIT ( 1 )
                END IF
             ELSE IF ( PIM(1)%CONF%ACT_CODE == PIMA__PPLT_CODE ) THEN
                IUER  = -1
                CALL PIMA_READ_BPASS ( PIM(1)%CONF%BANDPASS_FILE, PIM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7055, IUER, 'PIMA', 'Error in attempt to '// &
               &         'load bandpasses from file '//PIM(1)%CONF%BANDPASS_FILE )
                     CALL EXIT ( 1 )
                END IF
!
                IUER  = -1
                CALL PIMA_READ_PBP ( PIM(1)%CONF%POLARCAL_FILE, PIM, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7056, IUER, 'PIMA', 'Error in attempt to '// &
     &                        'load polarization bandpasses from file '// &
     &                         PIM(1)%CONF%POLARCAL_FILE )
                     CALL EXIT ( 1 )
                 END IF
           END IF
           IF ( INDEX ( FILE_PLOT, 'gif' ) > 0 ) THEN
                IDEV = 11
             ELSE IF ( INDEX ( FILE_PLOT, 'ps' ) > 0 ) THEN
               IDEV = 8
             ELSE 
               IDEV = 0
           END IF
!
           IUER = -1
           CALL PIMA_PLOT_BPASS ( PIM, STA_NAM, FILE_PLOT, IDEV, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7057, IUER, 'PIMA', 'Error in attempt to '// &
          &         'make bandpass plot' )
                CALL EXIT ( 1 )
           END IF
!
           IF ( ILEN(FILE_PLOT) > 0 ) THEN
                IB = LINDEX ( FILE_PLOT, '.' ) 
                IL = ILEN(FILE_PLOT)
                IR = RENAME ( FILE_PLOT(1:IL)//'all'//FILE_PLOT(IB:IL)//CHAR(0), &
     &                        FILE_PLOT(1:IL)//CHAR(0) )
                IF ( IR == 0 ) THEN
                     WRITE ( 6, * ) 'Output file is renamed to '//FILE_PLOT(1:IL)
                END IF
           END IF
           CALL EXIT ( 0 )
      END IF 
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__TSMO_CODE ) THEN
           IF ( ILEN(KEYWORD(1)) == 0 ) THEN
                CALL ERR_LOG ( 7058, -2, 'PIMA', 'Mandatory option mode of '// &
     &              'task tsmo was not provided' )
                CALL EXIT ( 1 )
           END IF
!
           IF ( KEYWORD(1) .NE. 'mode' ) THEN
                CALL ERR_LOG ( 7059, -2, 'PIMA', 'Upsupported option '// &
     &               KEYWORD(1)(1:I_LEN(KEYWORD(1)))//' for operation tsmo: '// &
     &              ' mode was expected' )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(VALUE(1)) == 0 ) THEN
                CALL ERR_LOG ( 7060, -2, 'PIMA', 'No value is provided '// &
     &              'after option '//KEYWORD(1)(1:I_LEN(KEYWORD(1)))// &
     &              ' . Modes such as if, elev should be specified separated '// &
     &              'with comma' )
                CALL EXIT ( 1 )
           END IF
           IF ( VALUE(1) == 'if'      .OR. &
     &          VALUE(1) == 'elev'    .OR. &
     &          VALUE(1) == 'if,elev' .OR. &
     &          VALUE(1) == 'elev,if'      ) THEN
                CONTINUE 
              ELSE
                CALL ERR_LOG ( 7061, -2, 'PIMA', 'Unsupported value of mode '// &
     &               VALUE(1)(1:I_LEN(VALUE(1)))//' while "if", "elev", "if,elev", '// &
     &               '"elev,if" were expected' ) 
                CALL EXIT ( 1 )
           END IF
!
! -------- Compute Tsys MOdel
!
           IUER = -1
           CALL PIMA_TSMO ( PIM(1), VALUE(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7062, IUER, 'PIMA', 'Error in modeling Tsys' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Write updated object PIM(1) into the file with extension .pim
!
           IUER = -1
           CALL PIMA_WRITE_OBS ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7063, IUER, 'PIMA', 'Error in attempt to '// &
          &         'write observation file for experiment '// &
     &               PIM(1)%CONF%SESS_CODE(1:I_LEN(PIM(1)%CONF%SESS_CODE))// &
     &              'into the output directory '// &
     &               PIM(1)%CONF%EXPER_DIR )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__GEPM_CODE ) THEN
           IF ( ILEN(KEYWORD(1)) == 0 ) THEN
                CALL ERR_LOG ( 7064, -2, 'PIMA', 'Mandatory option sta of '// &
     &              'task gepm was not provided' )
                CALL EXIT ( 1 )
           END IF
           IF ( KEYWORD(1) .NE. 'sta' ) THEN
                CALL ERR_LOG ( 7065, -2, 'PIMA', 'Mandatory option sta of '// &
     &              'task gepm was required, but got '//KEYWORD(1) )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(VALUE(1)) == 0 ) THEN
                CALL ERR_LOG ( 7066, -2, 'PIMA', 'No value is provided '// &
     &              'after option '//KEYWORD(1) )
                CALL EXIT ( 1 )
           END IF
!
           IF ( LTM_DIF ( 0, PIM(1)%NSTA, PIM(1)%C_STA, VALUE(1) ) < 1 .AND. & 
     &          TRIM(VALUE(1)) .NE. 'all' .AND. TRIM(VALUE(1)) .NE. 'ALL' ) THEN
                CALL ERR_LOG ( 7067, -2, 'PIMA', 'Station '//TRIM(VALUE(1))// &
     &              ' did not observed in experiment '//PIM%CONF%SESS_CODE )
                CALL EXIT ( 1 )
           END IF
!
           IF ( ILEN(KEYWORD(2)) > 0 ) THEN
                IF ( KEYWORD(2) .NE. 'tim_mseg' ) THEN
                     CALL ERR_LOG ( 7068, -2, 'PIMA', 'Unsupported 2nd keyword '// &
     &                    TRIM(KEYWORD(2))//' while tim_mseg was expected' )
                     CALL EXIT ( 1 )
                END IF 
                IF ( ILEN(VALUE(2)) == 0 ) THEN
                     CALL ERR_LOG ( 7069, -2, 'PIMA', 'No value is provided '// &
     &                   'after option '//KEYWORD(2) )
                     CALL EXIT ( 1 )
                END IF
                READ ( UNIT=VALUE(2), FMT=*, IOSTAT=IUER ) TIM_MSEG
                IF ( IuER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7070, -2, 'PIMA', 'Error in parsing '// &
     &                   'the value of the keyword tim_mseg '//TRIM(VALUE(2))// &
     &                   ' -- a real number was expected' )
                     CALL EXIT ( 1 )
                END IF 
              ELSE
                TIM_MSEG = PIMA__GEPM_TIM_MSEG_DEF
           END IF
           IF ( ILEN(KEYWORD(3)) > 0 ) THEN
                IF ( KEYWORD(3)(1:4) .NE. 'over' ) THEN
                     CALL ERR_LOG ( 7071, -2, 'PIMA', 'Unsupported 3rd keyword '// &
     &                    TRIM(KEYWORD(3))//' while overwrite was expected' )
                     CALL EXIT ( 1 )
                END IF 
                IF ( ILEN(VALUE(3)) == 0 ) THEN
                     CALL ERR_LOG ( 7072, -2, 'PIMA', 'No value is provided '// &
     &                   'after option '//KEYWORD(3) )
                     CALL EXIT ( 1 )
                END IF
                IF ( VALUE(3)(1:1) == 'Y' .OR. VALUE(3)(1:1) == 'y' ) THEN
                     GEPM_OVR = .TRUE.
                   ELSE IF ( VALUE(3)(1:1) == 'N' .OR. VALUE(3)(1:1) == 'n' ) THEN
                     GEPM_OVR = .FALSE.
                   ELSE
                     CALL ERR_LOG ( 7073, -2, 'PIMA', 'Unsupported value is provided '// &
     &                   'after option '//TRIM(KEYWORD(3))//' -- '//TRIM(VALUE(3))// &
     &                   ' while yes or no were expected' )
                     CALL EXIT ( 1 )
                END IF 
              ELSE
                GEPM_OVR = .FALSE.
           END IF 
!
           IF ( ILEN(KEYWORD(4)) > 0 ) THEN
                IF ( KEYWORD(4)(1:10) .NE. 'tim_thresh' ) THEN
                     CALL ERR_LOG ( 7074, -2, 'PIMA', 'Unsupported 4th keyword '// &
     &                    TRIM(KEYWORD(4))//' while tim_thresh was expected' )
                     CALL EXIT ( 1 )
                END IF 
                IF ( ILEN(VALUE(4)) == 0 ) THEN
                     CALL ERR_LOG ( 7075, -2, 'PIMA', 'No value is provided '// &
     &                   'after option '//KEYWORD(4) )
                     CALL EXIT ( 1 )
                END IF
                READ ( UNIT=VALUE(4), FMT=*, IOSTAT=IUER ) TIME_THRESH
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7076, -2, 'PIMA', 'Error in parsing '// &
     &                   'the value of the keyword tim_thresh '//TRIM(VALUE(4))// &
     &                   ' -- a real number was expected' )
                     CALL EXIT ( 1 )
                END IF 
              ELSE
                TIME_THRESH = PIMA__GEPM_TIME_THRESH__DEF
           END IF 
!
           IF ( ILEN(KEYWORD(5)) > 0 ) THEN
                IF ( KEYWORD(5)(1:11) .NE. 'diff_thresh' ) THEN
                     CALL ERR_LOG ( 7077, -2, 'PIMA', 'Unsupported 5th keyword '// &
     &                    TRIM(KEYWORD(5))//' while diff_thresh was expected' )
                     CALL EXIT ( 1 )
                END IF 
                IF ( ILEN(VALUE(5)) == 0 ) THEN
                     CALL ERR_LOG ( 7078, -2, 'PIMA', 'No value is provided '// &
     &                   'after option '//KEYWORD(5) )
                     CALL EXIT ( 1 )
                END IF
                READ ( UNIT=VALUE(5), FMT=*, IOSTAT=IUER ) DIFF_THRESH
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7079, -2, 'PIMA', 'Error in parsing '// &
     &                   'the value of the keyword diff_thresh '//TRIM(VALUE(5))// &
     &                   ' -- a real number was expected' )
                     CALL EXIT ( 1 )
                END IF 
              ELSE
                DIFF_THRESH = PIMA__GEPM_DIFF_THRESH__DEF
           END IF
!
           IF ( ILEN(KEYWORD(6)) > 0 ) THEN
                IF ( KEYWORD(6)(1:9) .NE. 'max_count' ) THEN
                     CALL ERR_LOG ( 7080, -2, 'PIMA', 'Unsupported 6th keyword '// &
     &                    TRIM(KEYWORD(6))//' while max_count was expected' )
                     CALL EXIT ( 1 )
                END IF 
                IF ( ILEN(VALUE(6)) == 0 ) THEN
                     CALL ERR_LOG ( 7081, -2, 'PIMA', 'No value is provided '// &
     &                   'after option '//KEYWORD(6) )
                     CALL EXIT ( 1 )
                END IF
                READ ( UNIT=VALUE(6), FMT=*, IOSTAT=IUER ) MAX_COUNT
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7082, -2, 'PIMA', 'Error in parsing '// &
     &                   'the value of the keyword max_count '//TRIM(VALUE(6))// &
     &                   ' -- an integer number was expected' )
                     CALL EXIT ( 1 )
                END IF 
              ELSE
                MAX_COUNT = PIMA__GEPM_MAX_COUNT__DEF
           END IF              
!     
           IUER = -1
           CALL PIMA_GEPM ( PIM(1), VALUE(1), TIM_MSEG, GEPM_OVR, TIME_THRESH, DIFF_THRESH, MAX_COUNT, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7083, -2, 'PIMA', 'Error in an attempt to '// &
     &              'generate the mask definition file around phase '// &
     &              'calibration tones' )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
!
! --- Process INCLUDE_OBS_FILE, EXCLUDE_OBS_FILE keywords
!
      IUER  = -1
      CALL PIMA_USE_OBS ( PIM, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7084, IUER, 'PIMA', 'Error in attempt to '// &
          &    'select which observations to include and which to exclude' )
           CALL EXIT ( 1 )
      END IF
!
      IF ( PIM(1)%CONF%FFT_METHOD == FFT_MKL ) THEN
           IUER = -1
           CALL INIT_FFT_MKL ( PIM(1)%CONF%NUM_THREADS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7085, IUER, 'PIMA', 'Error in initializing'// &
          &         ' FFT using the MKL' )
                CALL EXIT ( 1 )
           END IF
         ELSE
           IUER = -1
           CALL INIT_FFTW ( PIM(1)%CONF%FFT_CONFIG_FILE, &
     &                      PIM(1)%CONF%FFT_METHOD, 3.0D0, &
     &                      PIM(1)%CONF%NUM_THREADS, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7086, IUER, 'PIMA', 'Error in initializing'// &
          &         ' FFT using the FFTW' )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__ACTA_CODE ) THEN
           IUER = -1
           CALL PIMA_ACTA ( PIM(1), VTD, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 7087, IUER, 'PIMA', 'Failure to '// &
     &              'compute time average autocorrelation spectrum' )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
!
      IF ( PIM(1)%CONF%PCAL_MASK_FILE .NE. PIMA__BPASS_NO ) THEN
!
! -------- Read the file with pcal mask
!
           IUER  = -1
           CALL READ_PCAL_MASK ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7088, IUER, 'PIMA', 'Error in attempt to '// &
          &         'load the pcal mask from file '// &
     &              PIM(1)%CONF%PCAL_MASK_FILE )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__CLPC_CODE ) THEN
           IUER = -1
           CALL PIMA_CLPC ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7089, IUER, 'PIMA', 'Error in attempt to '// &
          &         'clean phase calibration phases' )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__BPAS_CODE ) THEN
           IUER = -1
           CALL PIMA_BPASS ( PIM(1), VTD, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7090, IUER, 'PIMA', 'Error in attempt to '// &
          &         'compute complex bandpass' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE      .NE. PIMA__LOAD_CODE  .AND.  &
     &     PIM(1)%CONF%ACT_CODE      .NE. PIMA__BPAS_CODE  .AND.  &
     &     PIM(1)%CONF%ACT_CODE      .NE. PIMA__GEAN_CODE  .AND.  &
     &     PIM(1)%CONF%BANDPASS_FILE .NE. PIMA__BPASS_NO   .AND.  &
     &     PIM(1)%CONF%BANDPASS_USE  .NE. PIMA__BPASS_NO          ) THEN
!
! -------- Read the file with bandpass
!
           IUER  = -1
           IF ( PIM(1)%CONF%ACT_CODE .EQ. PIMA__MPPL_CODE ) IUER = 0
           CALL PIMA_READ_BPASS ( PIM(1)%CONF%BANDPASS_FILE, PIM, IUER )
           IF ( IUER .NE. 0 .AND. PIM(1)%CONF%ACT_CODE .NE. PIMA__MPPL_CODE ) THEN
                IUER = -1
                CALL ERR_LOG ( 7091, IUER, 'PIMA', 'Error in attempt to '// &
          &         'load bandpasses from file '//PIM(1)%CONF%BANDPASS_FILE )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__MPPL_CODE ) THEN
           IUER = -1
           CALL PIMA_MULTI_PC_PLOT ( PIM(1), PIMA__MPPL_OBS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7092, IUER, 'PIMA', 'Error in attempt to '// &
          &         'make phase plots of phase calibration' )
                CALL EXIT ( 1 )
           END IF
           CALL EXIT ( 0 )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE      .NE. PIMA__LOAD_CODE  .AND.  &
     &     PIM(1)%CONF%ACT_CODE      .NE. PIMA__BPAS_CODE  .AND.  &
     &     PIM(1)%CONF%ACT_CODE      .NE. PIMA__GEAN_CODE  .AND.  &
     &     PIM(1)%CONF%ACT_CODE      .NE. PIMA__PDPL_CODE  .AND.  &
     &     PIM(1)%CONF%ACT_CODE      .NE. PIMA__PCPL_CODE  .AND.  &
     &     PIM(1)%CONF%ACT_CODE      .NE. PIMA__TSPL_CODE  .AND.  &
     &     PIM(1)%CONF%BANDPASS_FILE .NE. PIMA__BPASS_NO   .AND.  &
     &     PIM(1)%CONF%POLARCAL_FILE .NE. PIMA__BPASS_NO   .AND.  &
     &     PIM(1)%CONF%BANDPASS_USE  .NE. PIMA__BPASS_NO         ) THEN
!
! -------- Read the file with polarization bandpass
!
           IUER  = -1
           CALL PIMA_READ_PBP ( PIM(1)%CONF%POLARCAL_FILE, PIM, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7093, IUER, 'PIMA', 'Error in attempt to '// &
          &         'load bandpasses from file '//PIM(1)%CONF%BANDPASS_FILE )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( ILEN(PIM(1)%CONF%TIME_FLAG_FILE) > 0 ) THEN
           INQUIRE ( FILE=PIM(1)%CONF%TIME_FLAG_FILE, EXIST=LEX )
           IF ( LEX ) THEN
                IUER = -1
                CALL PIMA_READ_TIME_FLAG ( PIM(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7094, IUER, 'PIMA', 'Error in an '// &
     &                   'attempt to read and parse the time flag file '// &
     &                    PIM(1)%CONF%TIME_FLAG_FILE )
                     CALL EXIT ( 1 )
                END IF
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__ONOF_CODE ) THEN
           IUER = -1
           CALL PIMA_ONOF_SESS ( PIM(1), VTD, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7095, IUER, 'PIMA', 'Error in an attempt to '// &
     &              'determine on-off flags' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__FRIB_CODE ) THEN
           CALL GETENVAR ( 'PIMAVAR_GETUV_PRENOV18', STR )
           IUER = -1
           CALL PIMA_FRINGE ( PIM(1), VTD, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7096, IUER, 'PIMA', 'Error in an attempt to '// &
     &              'perform baseline fringe search' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__FRIP_CODE ) THEN
           IUER = -1
           CALL PIMA_FRIP ( PIM(1), VTD, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7097, IUER, 'PIMA', 'Error in an attempt to '// &
     &              'perform phase-referenced fringe search' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__TSPL_CODE ) THEN
           IF ( KEYWORD(1) == 'data' ) THEN
                IF ( VALUE(1) == 'measured' ) THEN
                     TSPL_DATA = PIMA__MEASURED
                   ELSE IF ( VALUE(1) == 'nwm' ) THEN
                     TSPL_DATA = PIMA__NWM
                   ELSE IF ( VALUE(1) == 'opacity' ) THEN
                     TSPL_DATA = PIMA__OPACITY
                   ELSE IF ( VALUE(1) == 'trec' ) THEN
                     TSPL_DATA = PIMA__TREC
                   ELSE IF ( VALUE(1) == 'tatm' ) THEN
                     TSPL_DATA = PIMA__NWM
                   ELSE IF ( VALUE(1) == 'table' ) THEN
                     TSPL_DATA = PIMA__TABL
                   ELSE IF ( VALUE(1) == 'tabmod' ) THEN
                     TSPL_DATA = PIMA__TABMOD
                   ELSE
                     IUER = -1
                     CALL ERR_LOG ( 7098, IUER, 'PIMA', 'Unsupported '// &
     &                   'qualifier of keyword data for action tspl: '// &
     &                    VALUE(1)(1:I_LEN(VALUE(1)))//' -- only '// &
     &                   'measured, opacity, trec, tatm, table, and nwm are supported' )
                    CALL EXIT ( 1 )
                END IF
              ELSE 
                TSPL_DATA = PIMA__MEASURED
           END IF
!
           IUER = -1
           CALL PIMA_PLOT_TSYS ( PIM(1), TSPL_DATA, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1  )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__ACPL_CODE ) THEN
           IUER = -1
           CALL PIMA_PLOT_ACRL ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1  )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__PCPL_CODE ) THEN
           PCAL_TYPE = PIMA__AVR
           DO 430 J3=1,128
              IF ( KEYWORD(J3) .EQ. 'pcal_type' ) THEN
                   IF ( ILEN(VALUE(J3)) == 0 ) THEN
                        CALL ERR_LOG ( 7099, -2, 'PIMA', 'No value of the '// &
     &                      'qualirifed pcal_type is provided. raw or avr '// &
     &                      'were expected' )
                        CALL EXIT ( 1 )
                     ELSE IF ( VALUE(J3) == 'raw' ) THEN
                        PCAL_TYPE = PIMA__RAW
                     ELSE IF ( VALUE(J3) == 'average'  .OR. &
     &                         VALUE(J3) == 'averaged' .OR. &
     &                         VALUE(J3) == 'avr'           ) THEN
                        PCAL_TYPE = PIMA__AVR
                     ELSE
                        CALL ERR_LOG ( 7100, -2, 'PIMA', 'Unsupported value of '// &
     &                      'pcal_type: '//TRIM(VALUE(J3))//' while raw or average '// &
     &                      'were expected' )
                        CALL EXIT ( 1 )
                   END IF
              END IF
 430       CONTINUE 
!
           IUER = -1
           CALL PIMA_PLOT_PCAL ( PIM(1), PCAL_TYPE, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1  )
      END IF
!      
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__PDPL_CODE ) THEN
           IF ( PIM(1)%NPOL < 2 ) THEN
                CALL ERR_LOG ( 7100, -2, 'PIMA', 'Task pdpl assumes '// &
     &              'the number of polarizations is more than one' )
                CALL EXIT ( 1 )
           END IF
           IUER = -1
           CALL PIMA_PLOT_PCAL ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1  )
      END IF

      IF ( PIM(1)%CONF%ACT_CODE == PIMA__GEAN_CODE ) THEN
           IF ( ILEN(KEYWORD(1)) == 0 ) THEN
                CALL ERR_LOG ( 7101, -2, 'PIMA', 'keyword antab_type '// &
     &              'or vlba_log_file with a value log file name should '// &
     &              'follow gean operation code' )
                CALL EXIT ( 1 )
           END IF
           IF ( KEYWORD(1) .NE. 'evn_antab_file'  .AND. &
     &          KEYWORD(1) .NE. 'pima_antab_file' .AND. &
     &          KEYWORD(1) .NE. 'vlba_log_file'   .AND. &
     &          KEYWORD(1) .NE. 'vlba_gain'       .AND. &
     &          KEYWORD(1) .NE. 'evn_gain'        .AND. &
     &          KEYWORD(1) .NE. 'lvim_dir'        .AND. &
     &          KEYWORD(1) .NE. 'pcal_on'         .AND. &
     &          KEYWORD(1) .NE. 'pcal_off'        .AND. &
     &          KEYWORD(1) .NE. 'tsys_on'         .AND. &
     &          KEYWORD(1) .NE. 'tsys_off'        .AND. &
     &          KEYWORD(1) .NE. 'wvr'                   ) THEN
                CALL ERR_LOG ( 7102, -2, 'PIMA', 'Upsupported option '// &
     &               KEYWORD(1)(1:I_LEN(KEYWORD(1)))//' for operation gean'// &
     &              ' evn_antab_file, pima_antab_file, vlba_log_file, '// &
     &              'vlba_gain, evn_gain, lvim_dir, pcal_on, pcal_off, or wvr '// &
     &              'were exepcted' )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(VALUE(1)) == 0 ) THEN
                CALL ERR_LOG ( 7103, -2, 'PIMA', 'No value is provided '// &
     &              'after option '//KEYWORD(1)(1:I_LEN(KEYWORD(1)))// &
     &              ' . Name of the antab_file was exepcted' )
                CALL EXIT ( 1 )
           END IF
!
           IF ( KEYWORD(1) == 'pcal_on'  .OR. &
     &          KEYWORD(1) == 'pcal_off' .OR. &
     &          KEYWORD(1) == 'tsys_on'  .OR. &
     &          KEYWORD(1) == 'tsys_off' .OR. &
     &          KEYWORD(1) == 'wvr'           ) THEN
!
                CONTINUE 
              ELSE 
                INQUIRE ( FILE=VALUE(1), EXIST=LEX )
                IF ( .NOT. LEX ) THEN
                     CALL ERR_LOG ( 7104, -2, 'PIMA', 'Calibration file '// &
     &                    VALUE(1)(1:I_LEN(VALUE(1)))//' was not found' )
                     CALL EXIT ( 1 )
                END IF
           END IF
!
           IF ( KEYWORD(1) .EQ. 'evn_antab_file'  .OR. &
     &          KEYWORD(1) .EQ. 'pima_antab_file'      ) THEN
!
                IUER = -1
                CALL PIMA_GET_ANTAB ( PIM(1), VTD, KEYWORD(1), VALUE(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7105, -2, 'PIMA', 'Error in '// &
     &                   'an attempt to parse antenna calibration '// &
     &                   ' file '//VALUE(1)(1:I_LEN(VALUE(1)))//' and '// &
     &                   'insert it into pima object ' )
                     CALL EXIT ( 1 )
                END IF
              ELSE IF ( KEYWORD(1) .EQ. 'vlba_log_file' ) THEN
                IUER = -1
                CALL PIMA_GET_VLBA_LOG ( PIM(1), VTD, KEYWORD(1), &
     &                                   VALUE(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7106, -2, 'PIMA', 'Error in '// &
     &                   'an attempt to parse antenna calibration and '// &
     &                   'insert it into pima object ' )
                     CALL EXIT ( 1 )
                END IF
              ELSE IF ( KEYWORD(1) .EQ. 'vlba_gain' ) THEN
                IF ( KEYWORD(2) .NE. 'gain_band' ) THEN
                     CALL ERR_LOG ( 7107, -2, 'PIMA', 'The second keyword '// &
     &                   'is missing: gain_band: was expected' )
                     CALL EXIT ( 1 )
                END IF
!
                IUER = -1
                CALL PIMA_GET_VLBA_GAIN ( PIM(1), VALUE(1), VALUE(2), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7108, -2, 'PIMA', 'Failure in an '// &
     &                   'attempt to get the gain from external VLBA key '// &
     &                   'file: '//KEYWORD(1) )
                     CALL EXIT ( 1 )
                END IF
              ELSE IF ( KEYWORD(1) .EQ. 'evn_gain' ) THEN
                IUER = -1
                CALL PIMA_GET_EVN_GAIN ( PIM(1), VALUE(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7109, -2, 'PIMA', 'Failure in an '// &
     &                   'attempt to get the gain from external VLBA key '// &
     &                   'file: '//KEYWORD(1) )
                     CALL EXIT ( 1 )
                END IF
              ELSE IF ( KEYWORD(1) .EQ. 'pcal_on'  .OR. &
     &                  KEYWORD(1) .EQ. 'pcal_off'      ) THEN
                IUER = -1
                CALL PIMA_PCAL_CONTROL ( PIM, KEYWORD(1), VALUE(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7110, -2, 'PIMA', 'Failure to '// &
     &                   'suppress or restore phase calibration for station '// &
     &                    VALUE(1) )
                     CALL EXIT ( 1 )
                END IF
              ELSE IF ( KEYWORD(1) .EQ. 'tsys_on'  .OR. &
     &                  KEYWORD(1) .EQ. 'tsys_off'      ) THEN
                IUER = -1
                CALL PIMA_TSYS_CONTROL ( PIM, KEYWORD(1), VALUE(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7111, -2, 'PIMA', 'Failure to '// &
     &                   'suppress or restore Tsys for station '// &
     &                    VALUE(1) )
                     CALL EXIT ( 1 )
                END IF
              ELSE IF ( KEYWORD(1) .EQ. 'wvr' ) THEN
                IUER = -1
                CALL PIMA_LOAD_WVR ( PIM(1), VALUE(1), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7112, -2, 'PIMA', 'Failure to load WVR '// &
     &                   'data into PIMA' )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7113, -2, 'PIMA', 'Unrecognized keyword '// &
     &                   'that followed operation gean: '// &
     &                    KEYWORD(1)(1:I_LEN(KEYWORD(1)))//' while '// &
     &                   'evn_antab_file, pima_antab_file or vlba_log_file '// &
     &                   'were expected' )
                     CALL EXIT ( 1 )
                END IF
           END IF
!
! -------- Write updated object PIM(1) into the file with extension .pim
!
           IUER = -1
           CALL PIMA_WRITE_OBS ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7114, IUER, 'PIMA', 'Error in attempt to '// &
          &         'write observation file into output directory '// &
     &              PIM(1)%CONF%EXPER_DIR )
                CALL EXIT ( 1 )
           END IF
      END IF
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__GENA_CODE ) THEN
!
! -------- GEN Antab task
!
           IF ( KEYWORD(1)(1:1) == " " ) THEN
                CALL ERR_LOG ( 7115, -2, 'PIMA', 'Task GENA requires 1st '// &
     &              'argument sta' )
                CALL EXIT ( 1 )
           END IF
           IF ( KEYWORD(1) .NE. 'sta' ) THEN
                CALL ERR_LOG ( 7116, -2, 'PIMA', 'Upsupported first keyword '// &
     &               KEYWORD(1)(1:I_LEN(KEYWORD(1)))//' for task GENA. '// &
     &              ' keyword sta was expected' )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(KEYWORD(2)) == 0 ) THEN
                CALL ERR_LOG ( 7117, -2, 'PIMA', 'Task GENA requires 2st '// &
     &              'argument type' )
                CALL EXIT ( 1 )
           END IF
           IF ( KEYWORD(2) .NE. 'type' ) THEN
                CALL ERR_LOG ( 7118, -2, 'PIMA', 'Upsupported second keyword'// &
     &               KEYWORD(1)(1:I_LEN(KEYWORD(1)))//' for task GENA. '// &
     &              ' keyword type was expected' )
                CALL EXIT ( 1 )
           END IF
!e
           IF ( VALUE(2) == 'monica' ) THEN
                IF ( KEYWORD(3) == 'file' )  THEN
                     CONTINUE 
                   ELSE IF ( ILEN(KEYWORD(3)) == 0 )  THEN
                     CALL ERR_LOG ( 7119, -2, 'PIMA', 'Task GENA with type '// &
     &                   'monica requires the third keyword: file' )
                     CALL EXIT ( 1 )
                   ELSE
                     CALL ERR_LOG ( 7120, -2, 'PIMA', 'Upsupported keyword '// &
     &                  KEYWORD(3)(1:I_LEN(KEYWORD(3)))//' for task GENA. '// &
     &                  ' keyword file was expected' )
                     CALL EXIT ( 1 )
                END IF
                IUER = -1
                CALL MONICA_TO_ANTAB ( PIM, VALUE(1), VALUE(3), IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7121, -2, 'PIMA', 'Error in an attempt '// &
     &                   'to parse monica Tsys file and generate output antab '// &
     &                   'file for station '//TRIM(VALUE(2)) )
                     CALL EXIT ( 1 )
                END IF
              ELSE IF ( VALUE(2) == 'const' ) THEN
                IF ( KEYWORD(3) == 'trec' )  THEN
                     CONTINUE 
                   ELSE IF ( ILEN(KEYWORD(3)) == 0 )  THEN
                     CALL ERR_LOG ( 7122, -2, 'PIMA', 'Task GENA with type '// &
     &                   'const requires the third keyword: trec' )
                     CALL EXIT ( 1 )
                   ELSE
                     CALL ERR_LOG ( 7123, -2, 'PIMA', 'Upsupported keyword '// &
     &                  KEYWORD(3)(1:I_LEN(KEYWORD(3)))//' for task GENA. '// &
     &                  ' keyword trec was expected' )
                     CALL EXIT ( 1 )
                END IF
!
                READ ( UNIT=VALUE(3), FMT=*, IOSTAT=IUER ) TREC_CONST
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7124, -2, 'PIMA', 'Error in decording '// &
     &                   'Trec value '//TRIM(VALUE(3))//' -- a positive '// &
     &                   'real number was expected' )
                     CALL EXIT ( 1 )
                END IF
                IF ( KEYWORD(4) == 'tatm' )  THEN
                     CONTINUE 
                   ELSE IF ( ILEN(KEYWORD(4)) == 0 )  THEN
                     CALL ERR_LOG ( 7125, -2, 'PIMA', 'Task GENA with type '// &
     &                   'const requires the fourth keyword: tatm' )
                     CALL EXIT ( 1 )
                   ELSE
                     CALL ERR_LOG ( 7126, -2, 'PIMA', 'Upsupported keyword '// &
     &                  KEYWORD(4)(1:I_LEN(KEYWORD(3)))//' for task GENA. '// &
     &                  ' keyword tatm was expected' )
                     CALL EXIT ( 1 )
                END IF
!
                READ ( UNIT=VALUE(4), FMT=*, IOSTAT=IUER ) TATM_CONST
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7127, -2, 'PIMA', 'Error in decording '// &
     &                   'Tatm value '//TRIM(VALUE(3))//' -- a positive '// &
     &                   'real number was expected' )
                     CALL EXIT ( 1 )
                END IF
!
                IUER = -1
                CALL TREC_CONST_TO_ANTAB ( PIM, VALUE(1), TREC_CONST, TATM_CONST, IUER )
                IF ( IUER .NE. 0 ) THEN
                     CALL ERR_LOG ( 7128, -2, 'PIMA', 'Error in an attempt '// &
     &                   'to generate output antab file with constant Trec '// &
     &                   'for station '//TRIM(VALUE(1)) )
                     CALL EXIT ( 1 )
                END IF
              ELSE
                CALL ERR_LOG ( 7129, -2, 'PIMA', 'Unsupported value of '// &
     &              'keyword type '//TRIM(VALUE(2))// &
     &              ' const or monica were expected' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__MKDB_CODE ) THEN
           IUER = -1
           CALL PIMA_MKDB ( PIM(1), VTD, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7130, IUER, 'PIMA', 'Error in attempt to '// &
          &         'create the output dataset with total path delays' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__DIPC_CODE ) THEN
           IUER = -1
           CALL PIMA_DIPC ( PIM(1), L_OPT, KEYWORD, VALUE, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7131, IUER, 'PIMA', 'Error in attempt to '// &
          &         'to parse the ascii table with DiFX phase calibration '// &
     &              'information' )
                CALL EXIT ( 1 )
           END IF
!
! -------- Write updated object PIM(1) into the file with extension .pim
!
           IUER = -1
           CALL PIMA_WRITE_OBS ( PIM(1), IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7132, IUER, 'PIMA', 'Error in attempt to '// &
          &         'write observation file into output directory '// &
     &              PIM(1)%CONF%EXPER_DIR )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__SPLT_CODE ) THEN
           CALL GETENVAR ( 'PIMAVAR_GETUV_PRENOV18', STR )
           IUER = -1
           CALL PIMA_SPLT ( PIM(1), VTD, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1  )
      END IF
!
      IF ( PIM(1)%CONF%ACT_CODE == PIMA__GACO_CODE ) THEN
           IF ( ILEN(KEYWORD(1)) == 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7133, IUER, 'PIMA', 'Missing value of the '// &
     &             '1st qualifier. init or sou were expected' )
                CALL EXIT ( 1 )
           END IF
           IF ( ILEN(VALUE(1)) == 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7134, IUER, 'PIMA', 'Missing value of the '// &
     &             '1st qualifier '//KEYWORD(1) )
                CALL EXIT ( 1 )
           END IF
           IF ( KEYWORD(1)(1:4) .EQ. 'init' ) THEN
                READ ( UNIT=VALUE(1), FMT='(F10.5)', IOSTAT=IUER ) GAIN_VAL
                IF ( IUER .NE. 0 .OR. GAIN_VAL < 0.0D0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7135, IUER, 'PIMA_GACO_COMP', 'Failure '// &
     &                   'in parsing the value '//VALUE(1)(1:I_LEN(VALUE(1)))// &
     &                   ' -- a non-negative real number was expected' )
                     CALL EXIT ( 1 )
                END IF
!
                IUER = -1
                CALL PIMA_GACO_INIT ( PIM, GAIN_VAL, IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7136, IUER, 'PIMA_GACO_COMP', 'Failure '// &
     &                   'in attempt to inilize gain correction' )
                     CALL EXIT ( 1 )
                END IF 
!
                IUER = -1
                CALL PIMA_WRITE_GACO ( PIM, 0, ' ', IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7137, IUER, 'PIMA_GACO_COMP', 'Failure '// &
     &                   'in attempt to write gain correction' )
                     CALL EXIT ( 1 )
                END IF 
              ELSE IF ( KEYWORD(1)(1:3) == 'sou' ) THEN
                IF ( ILEN(KEYWORD(2)) == 0 ) THEN
                     VALUE(2) = PIM(1)%CONF%EXPER_DIR(1:I_LEN(PIM(1)%CONF%EXPER_DIR))//'/'// &
     &                          PIM(1)%CONF%SESS_CODE(1:I_LEN(PIM(1)%CONF%SESS_CODE))//'_uvs'
                   ELSE
                     IF ( KEYWORD(2)(1:3) .NE. 'dir' ) THEN
                          IUER = -1
                          CALL ERR_LOG ( 7138, IUER, 'PIMA', 'Unsupported name of '// &
     &                        '2nd qualifier '//KEYWORD(2)(1:I_LEN(KEYWORD(2)))// &
     &                        ' while directory was expected' )
                          CALL EXIT ( 1 )
                     END IF
                     IF ( ILEN(VALUE(2)) == 0 ) THEN
                          IUER = -1
                          CALL ERR_LOG ( 7139, IUER, 'PIMA', 'Missing value of the '// &
     &                        '2nd qualifier '//KEYWORD(2) )
                          CALL EXIT ( 1 )
                     END IF
                END IF 
!          
                IUER = -1
                CALL PIMA_GACO_COMP ( PIM, VALUE(1), VALUE(2), IUER )
                IF ( IUER .NE. 0 ) THEN
                     IUER = -1
                     CALL ERR_LOG ( 7140, IUER, 'PIMA', 'Error during '// &
     &                   'computation of the gain correction' )
                     CALL EXIT ( 1 )
                END IF
                WRITE ( 6, '(A)' ) 'Written output file '// &
     &                  PIM(1)%CONF%SPLT_GAIN_CORR_FILE(1:I_LEN(PIM(1)%CONF%SPLT_GAIN_CORR_FILE))
              ELSE
                IUER = -1
                CALL ERR_LOG ( 7141, IUER, 'PIMA', 'Unsupported name of '// &
     &              '1st qualifier '//KEYWORD(1)(1:I_LEN(KEYWORD(1)))// &
     &              ' while sou or init was expected' )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL PIMA_EXIT ( PIM(1), IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7142, IUER, 'PIMA', 'Trap of internal control: '// &
     &         'failure to close open files and deallocate dynamic memory '// &
     &         'used by PIMA' )
           CALL EXIT ( 1 )
      END IF
      DEALLOCATE ( PIM )
      CALL EXIT ( 0 )
      END  SUBROUTINE  PIMA  !#!#
