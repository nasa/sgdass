      PROGRAM    NORML_LAUNCH
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      CHARACTER    STR*32
      INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
      PARAMETER  ( GB = 1024*1024*1024 )
      PARAMETER  ( STACK_SIZE_IN_BYTES = PSOLVE__STACK_SIZE_IN_GIGABYTES * GB )
      INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! --- Set stacksize
!
      IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
      CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
      CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
      CALL ERR_MODE ( 'NO_PROBE' )
      CALL NORML_HEAD()
      END  PROGRAM  NORML_LAUNCH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NORML_HEAD
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'socom.i'
      INCLUDE 'precm.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'fbcom.i'
      INCLUDE 'cnstr.i'
      INCLUDE 'fast.i'
!
      INTEGER*2 ISTS, IWDS
      INTEGER*4 NPARIN, NPARAM_SAVE, NMINUP
      LOGICAL*2 KCOV
      LOGICAL*4 LEX
      CHARACTER CNAME*63, STR*128, STR1*32
      PARAMETER ( IWDS = 10 )
      INTEGER*4 MAT_E
      INTEGER*8 NELEM, MATSIZE
      INTEGER*4 I4P0, I4P1, I4P60, I4P255, I4P256
      LOGICAL*2 F_IO_NRM
      ADDRESS__TYPE :: ADDR_ARR
!
      TYPE ( B3D__STRU   ) ::  B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
!
      DATA       ISTS /1/
      DATA I4P0, I4P1, I4P60, I4P255, &
     &    I4P256/   0,    1,    60,    255,    256  /
      INTEGER*4  IUER
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      ADDRESS__TYPE :: MAT_E4
!CCCCC
!
!     HISTORY
!   JLR  921216  replaced nJ with I4Pn's
!   mwh  940201  implemented dynamic memory allocation for large matrices
!   pet  970317  updated comments
!   pet  970424  Added socom_ext
!   pet  970523  Bug fixed with input CGM
!   pet  980520  Added a trap of consistency of the number of epochs of
!                of piece-wise linear finction modelling station positions.
!                Two parameters: PWCNUMEP from prfil-block and PWCNUM(1) from
!                glbc4-block should be equal, otherwice list of global
!                parameters will be corrupted and global solution will be
!                WRONG.
!   pet  980706  Added call of SET_PATHS in order to allow to set SOLVE_HELP_DIR
!                environment variable if it was not set up before
!   pet  990303  Changed a list of formal parameters for NORML_MAIN.
!                Added a call of END_PROG (which was previously in norml_main)
!   pet 1999.05.31  Made CNSTROBJ a formal parameter
!
!CCCCC
      CALL PRE_PROG()
      INCLUDE 'norml_version.i' ! Set revision date of the current version
      CALL SET_PATHS() ! Setting environment variables to PGPLOT and SOLVE_HELP_DIR
!
      CALL USE_GLBFIL   ( 'OR' )
      CALL USE_GLBFIL_4 ( 'RC' )
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     type *, "veluen_cnsb ",veluen_cnsb
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Timer initialization
!
           CALL TIM_INIT ( )
      END IF
!
      KCOV = ISOLU.EQ.1 .OR. ( ICONT.EQ.0 .AND. IOCGM.NE.0 )
      IF ( KCOV ) THEN
           IF ( ISOLU.EQ.1 ) THEN
                IF ( MERGCGM .NE. ' ' ) THEN
                     CNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMF'//PRE_LETRS
                  ELSE
                     IF ( ILEN(INAMCG) .EQ. 0 ) THEN
                          IUER = -1
                          CALL ERR_LOG ( 8391, IUER, 'NORML_HEAD', &
     &                        'Input CGM matrix was not specified, but '// &
     &                        'SOLVE is going to read it. Please correct '// &
     &                        'your BATCH file and run SOLVE again' )
                          STOP 'NORML  Abnormal termination'
                     END IF
                     CNAME = INAMCG
                ENDIF
              ELSE
                CNAME=ONAMCG
            ENDIF
!
! --------- Test: does this file exist?
!
            CALL CHASHL ( CNAME )
            INQUIRE ( FILE = CNAME, EXIST=LEX )
            IF ( .NOT. LEX  .AND.  CNAME(1:1) .NE. '/' ) THEN
                 CALL CLRCH ( STR )
                 CALL GETENVAR ( 'PSOLVE_CGM_DIR', STR )
                 IF ( STR(1:1) .NE. ' ' ) THEN
                      IF ( STR(ILEN(STR):ILEN(STR)) .NE. '/') &
     &                     STR(ILEN(STR)+1:) = '/'
                      CNAME = STR(1:ILEN(STR))//CNAME
                 END IF
            END IF
!
! --------- Opening input CGM
!
            CALL ACS_CGMFIL   ( CNAME, 'O' )
!
! --------- Read CGM-dependent common blocks
!
            CALL USE_CGMF_COM ( 'R' )
            IF ( KUSER_PART ) THEN
                 NPARAM_SAVE = NPARAM
                 CALL PARCNG()
                 NMINUP = NPARAM - NUM_USER_PART
                 NUM_USER_PART = NPARAM_SAVE - NMINUP
                 NPARAM = NPARAM_SAVE
                 IF ( NUM_USER_PART .GT. 0 ) THEN
!
! ------------------- Create a file of global user parameters. The true is that
! ------------------- if we used external input CGM then the list may not exist.
!
                      CALL CREATE_USRG ( NPARAM, NUM_USER_PART )
                 ENDIF
            ENDIF
!
! --------- Now check
!
            IF ( PWCNUMEP .NE. PWCNUM(1) ) THEN
                 CALL CLRCH   ( STR )
                 CALL INCH    ( INT4(PWCNUMEP), STR   )
                 CALL CLRCH   ( STR1 )
                 CALL INCH    ( INT4(PWCNUM(1)), STR1 )
                 CALL ERR_LOG ( 8392, -1, 'NORML_HEAD', 'Number '// &
     &               'of epochs for the stations those positions are modeled '// &
     &               'by piece-wise function specified in control file is '// &
     &                STR(1:I_LEN(STR))//', but the number of epochs kept '// &
     &               'in CGM file is '//STR1(1:I_LEN(STR1))//'. This is '// &
     &               'fatal situation. It may occur when value of '// &
     &               'keyword PIECE_WISE_STA in control file used for '// &
     &               'backward solution was not equal to the value used '// &
     &               'for back solution' )
                 STOP 'NORML  Abnormal termination'
            END IF
!
            NPARIN = NPARAM
!@            NELEM = I4P256*( (MAT_E( M_GPA, NPARIN) +I4P255)/I4P256 )
            NELEM = MAT_E4 ( M_GPA, NPARIN )
            MATSIZE = NELEM*8
!
! --------- Get memory
!
            CALL GRAB_MEM ( MATSIZE, ADDR_ARR )
         ELSE
           CALL USE_COMMON ( 'ORC' )
!
! -------- Expansion socom to socom_plus
!
           CALL SOCOM_EXT()
           NPARIN = NPARAM
!@           NELEM = I4P256*( (MAT_E( M_GPA, NPARIN ) +I4P255)/I4P256)
           NELEM = MAT_E4 ( M_GPA, NPARIN )
!
! -------- Convert to number of bytes (8 per real*8 element)
!
           MATSIZE = NELEM*8
           CALL GET_MEM ( MATSIZE, ADDR_ARR )
      ENDIF
!
! --- Call the main procedure. We say it to read CGM from disk if we are in
! --- global mode
!
      IUER = -1
      F_IO_NRM = .TRUE.
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'NORML-01' )
      END IF
!
      IF ( KSPOOL ) CALL USE_SPOOL ( 'O' )
      CALL NORML_MAIN ( NPARIN, %VAL(ADDR_ARR), B3DOBJ, CNSTROBJ, CNAME, F_IO_NRM, IUER )
      IF ( KSPOOL ) CALL USE_SPOOL ( 'C' )
!
      IF ( IUER .NE. 0 ) THEN
           WRITE ( 6, '(A)' ) 'NORML: abnormal termination'
           CALL EXIT ( 1 ) 
      END IF
!
      IF ( FAST_MODE .EQ. F__B3D  ) THEN
!
! -------- Freeing dynamic memory allocated for fields of B3DOBJ
!
           IUER = -1
           CALL B3D_FREEMEM ( B3DOBJ, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8393, IUER, 'NORML', 'Error during '// &
     &              'freeing dynamic memory' )
                STOP 'NORML: abnormal termination'
           END IF
      END IF
      CALL STATUS_SET ( 'NORML', STA__BEG )
!
      CALL END_PROG()
      END  SUBROUTINE  NORML_HEAD  !#!#
