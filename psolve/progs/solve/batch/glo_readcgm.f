      SUBROUTINE GLO_READCGM ( IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_READCGM  reads common blocks associated with CGM      *
! *                                                                      *
! * Who  When       What                                                 *
! * pet  2000.10.23 Fixed the bug: the previous version always read      *
! *                 common blocks from the CGMFxx scratch files, since   *
! *                 field MERGCGM was not initialized because two        *
! *                 variables with the same name MERGECGM were in        *
! *                 use!!                                                *
! * pet  2007.06.30 Made changes related to support of station           *
! *                 parameterized with spline.                           *
! *                                                                      *
! * dsm  2011.05.22 Used parameter NUM_USER_GLOB in call to create_usrg  *
! *                 to avoid problem with incorrectly computed           *
! *                 NUM_USER_PART parameter from call to parcng in a     *
! *                 BACK solution.                                       *
! *                                                                      *
! *  ###  07-APR-1999  GLO_READCGM  v1.5  (c) L. Petrov 26-APR-2011 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'plist.i'
      INCLUDE 'dmapp.i'
!
      INTEGER*4   IUER
      CHARACTER   CNAME*160, STR*160, STR1*160, PWCSITES_NEW(MAX_PWC_SITES), &
     &            PWCMAP_FILE*(128)
      INTEGER*4   NPARAM_SAVE, NMINUP
      INTEGER*2   PWCNUM_NEW(MAX_STA), INTRVL_NEW
      LOGICAL*4   LEX
      LOGICAL*2   KSCREEN_OLD
      INTEGER*4   IER
      REAL*8      PWCEP_NEW(MAX_PWC_EPS)
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4, LOC
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4,  EXTERNAL :: I_LEN, ILEN
!
      KSCREEN_OLD = KSCREEN
      KSCREEN = .FALSE.
      IF ( MERGCGM(1:1) .NE. ' ' ) THEN
           CNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMF'//PRE_LETRS
         ELSE
           IF ( ILEN(INAMCG) .EQ. 0 ) THEN
                CALL ERR_LOG ( 4271, IUER, 'GLO_READCGM', &
     &              'Input CGM matrix was not specified, but '// &
     &              'SOLVE is going to read it. Please correct '// &
     &              'your BATCH file and run SOLVE again' )
                KSCREEN = KSCREEN_OLD
                RETURN
            END IF
            CNAME = INAMCG
      END IF
!
! --- Test: does this file exist?
!
      CALL CHASHL ( CNAME )
      INQUIRE ( FILE=CNAME, EXIST=LEX )
      IF ( .NOT. LEX  .AND.  CNAME(1:1) .NE. '/' ) THEN
           CALL CLRCH ( STR )
           CALL GETENVAR ( 'PSOLVE_CGM_DIR', STR )
           IF ( STR(1:1) .NE. ' ' ) THEN
                IF ( STR(ILEN(STR):ILEN(STR)) .NE. '/' ) STR(ILEN(STR)+1:) ='/'
                CNAME = STR(1:ILEN(STR))//CNAME
           END IF
           INQUIRE ( FILE=CNAME, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL ERR_LOG ( 4272, IUER, 'GLO_READCGM', 'Input CGM matrix '// &
     &               CNAME(1:I_LEN(CNAME))//' was not found' )
                KSCREEN = KSCREEN_OLD
                RETURN
           END IF
      END IF
!
! --- Opening input CGM
!
      CALL ACS_CGMFIL   ( CNAME, 'O' )
      CALL USE_CGMF_COM ( 'R' )
#ifdef DEBUG
      write ( 6, * ) 'GLO_READCGM-89  NPARAM= ', NPARAM, ' cname= ', trim(cname) ; call flush ( 6 ) ! %%%%%%
#endif
      IF ( KUSER_PART ) THEN
           NPARAM_SAVE = NPARAM
!
           IF ( L_SPE    > 0       .AND.  &
     &          PARM_NUM > 0       .AND.  &
     &          ADR_SPE .NE. 0            ) THEN
!
                CALL ERR_PASS ( IUER, IER )
                CALL UPDATE_SPE ( PARM_NUM, CPARM_NAMES, L_SPE, %VAL(ADR_SPE), IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4273, IUER, 'GLO_RESTORE', 'Error in '// &
     &                   'an attempt to restore estimation status of '// &
     &                   'SPE from the parameter list' )
                     RETURN 
                END IF
                CALL SPESOL_CREATE ( %VAL(ADR_SPE) )
           END IF
!
           CALL PARCNG()
           NMINUP = NPARAM - NUM_USER_PART
           NUM_USER_PART = NPARAM_SAVE - NMINUP
           NPARAM = NPARAM_SAVE
           IF ( NUM_USER_PART .GT. 0 ) THEN
!
! ------------- Create a file of global user parameters. The true is that
! ------------- if we used external input CGM then the list may not exist.
! ------------- But we can use NUM_USER_GLO to make the USRG scratchfile (dsm 4/22/11)
!
                CALL CREATE_USRG ( NPARAM, NUM_USER_GLOB )
           ENDIF
      ENDIF
!
! --- Now try to read pice_wise_station defintion file. We read it only
! --- for checking whether the number of piece_wise epochs xpecified in the
! --- control file is the saem as the number of epochs in the CGM. Later arcset
! --- will read this file once more ant copies number of epochs, names of the
! --- stations in glbc4. But we need this information now.
!
      IF ( PWCMAP(1:1) .NE. ' '  .AND.  PWCMAP(1:4) .NE. 'NONE' ) THEN
!
! -------- Build the filename
!
           CALL CLRCH ( PWCMAP_FILE )
           IF ( PWCMAP(1:1) .EQ. '/' ) THEN
                PWCMAP_FILE = PWCMAP
              ELSE
                PWCMAP_FILE = PRE_SAV_DIR(:PRE_SV_LEN)//PWCMAP
           END IF
!
! -------- Read the file
!
           CALL GPWC ( PWCNUM_NEW, PWCEP_NEW, PWCSITES_NEW, PWCMAP_FILE, &
     &                 INTRVL_NEW )
!
! -------- Set values
!
           PWCNUM(1) = PWCNUM_NEW(1)
           PWCNUM(2) = PWCNUM_NEW(2)
      END IF
!
      IF ( INT4(PWCNUM(1)) .EQ. 1 .AND. INT4(PWCNUMEP) .EQ. 0 ) THEN
!
! -------- This is not fatal
!
           PWCNUMEP = 1
         ELSE IF ( INT4(PWCNUM(1)) .EQ. 0 .AND.INT4(PWCNUMEP)  .EQ. 1)       &
     &    THEN
!
! -------- This is not fatal: discont_init will do it later anyway
!
           PWCNUM(1) = 1
           PWCNUM(2) = 1
      END IF
!
      IF ( PWCNUMEP .NE. PWCNUM(1) ) THEN
           CALL CLRCH   ( STR )
           CALL INCH    ( INT4(PWCNUM(1)), STR  )
           CALL CLRCH   ( STR1 )
           CALL INCH    ( INT4(PWCNUMEP),  STR1 )
           CALL ERR_LOG ( 4273, IUER, 'GLO_READCGM', 'Number of '// &
     &         'epochs for the stations those positions are modeled '// &
     &         'by piece-wise function specified in control file is '// &
     &          STR(1:I_LEN(STR))//', but the number of epochs kept '// &
     &         'in CGM file is '//STR1(1:I_LEN(STR1))//'. This is '// &
     &         'fatal situation. It may occur when value of '// &
     &         'keyword PIECE_WISE_STA in control file used for '// &
     &         'backward solution was not equal to the value used '// &
     &         'for forward solution which generated the CGM' )
           KSCREEN = KSCREEN_OLD
           RETURN
      END IF
!
      KSCREEN = KSCREEN_OLD
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLO_READCGM  #!#
