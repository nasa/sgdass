      PROGRAM ARCPE
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!     Updated to specificaly type integers which
!-------------------------------------------------
!
! 1.  ARCPE PROGRAM SPECIFICATION
!
! 1.1 Perform arc parameter elimination.
!
! 1.2 REFERENCES:
!
! 2.  ARCPE INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'addcm.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'socom.i'
      INCLUDE 'fast.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: NONE
!       CALLED SUBROUTINES:  ARCPE_MAIN
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2  KBIT, STACM
      LOGICAL*2 KFIXED
      CHARACTER  LPARM(M_GPA)*20
      INTEGER*4  IOFF, FILDES, JBLOCKS, JRND_BLOCKS, MAT_E
      INTEGER*2  IBUFF_ARCPE, IDIRECT(BLOCK_WORDS), TRIMLEN
      CHARACTER  SAVNAM*(NAME_SIZE), SID*60
      COMMON    / SAVCGM / FILDES, IDIRECT
      COMMON    / NAMCGM / SAVNAM
      SAVE      / SAVCGM /, / NAMCGM /
!
      CHARACTER  DATE*8, BUFSTR*80, STR*32
      INTEGER*4  IX1T3(M_GPA), NPARM1, NPARM3
      INTEGER*2  NGTA, NLTA
      INTEGER*4  I4P0, I4P1, I4P60, I4P255, I4P256
      INTEGER*8  M1, M2, MATSIZE, MLEN, LEN8_BYTES, LEN8_BLOCKS
      ADDRESS__TYPE :: MEM_ADR, ADDR_ARR1, ADDR_ARR2
      ADDRESS__TYPE, EXTERNAL :: MAT_E4
      DATA  I4P0, I4P1, I4P60, I4P255, I4P256 / 0, 1, 60, 255, 256 /
      SAVE IX1T3
!C
      TYPE ( B3D__STRU ) ::    B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      CHARACTER  FINAM_NRM*40
      INTEGER*4  IUER
      INTEGER*4, EXTERNAL :: I_LEN
!CCCCC
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   jmg  960610  Fix bug which occurs because
!                the fixed arc is treated differently than the free arcs.
!                Previously the EOP parameters were not estimated in the
!                forward solution -- only in the back.  Now they will be
!                estimated in both directions.  This requires the application
!                of constraints in arcpe to constrain the adjustments to zero.
!   pet  970214  Added support of B1D   parametrization
!   pet  970227  Added support of B1B3D parametrization
!   pet  970519  Added timing facility
!   pet  970917  Error message added when user is trying to call ARCPE in B3D
!                mode
!   pet  971105  Corrected COR_NOV97 bug -- attempt to write arcfile before
!                its creation in the case of calcualtion of correlations
!   pet  980708  Added logic for handling the arc to be marked as "skipped"
!                by PROC
!   pet  980710  Added support of the situation when some sources/stations/
!                baselines were excluded from estimation or solution on the
!                flight by PROC.
!   pet  990406  Added call of TIM_GET
!   pet  2001.05.10  pet  Increased amount of memory allocated for CGM
!                    by 256 bytes since it is read and written by
!                    256-bytes-long blocks.
!   pet  2001.05.10  pet  Added logic which prevents writing output CGM in
!                         the case if its name is defined as NONE
!
! 5.  ARCPE PROGRAM STRUCTURE
!
!CCCCC
      CALL PRE_PROG()
      INCLUDE 'arcpe_version.i' ! Set revision date of the current version
      CALL USE_BUFFER ( IBUFF_ARCPE, INT2(1), 'ORC' )
!
! --- Display initial message on screenC
!
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
           CALL START_MN()
           CALL SETCR_MN ( I4P60, I4P0 )
           WRITE  ( BUFSTR, 8417   ) DATE
           CALL REVERSE_ON_MN()
 8417      FORMAT ( "ARCPE Ver ",A )
           CALL ADDSTR_F ( BUFSTR )
           CALL REVERSE_OFF_MN()
      ENDIF
!
! --- Set timer
!
      CALL TIM_INIT()
!
! --- Handle commons
!
      CALL USE_GLBFIL   ( 'OR'  )
      CALL USE_GLBFIL_4 ( 'RC'  )
      CALL USE_COMMON   ( 'ORC' )
      CALL SOCOM_EXT()
!
      IF ( IBUFF_ARCPE .EQ. 1 ) THEN
!
! -------- Our arc marked as "skipped" by PROC
!
           IF ( IOCGM .EQ. 2  .AND. &
     &          .NOT.( OUTCGM(1:4) .EQ. 'NONE' .AND. TRIMLEN(OUTCGM) .EQ. 4)) THEN
!
! ------------- This "arc" appeared to be skipped by PROC, but we need save
! ------------- CGM in permanent file
!
! ------------- Openning previous CGM and reading its header.
! ------------- We do it to learn the number of global parameters saved there.
! ------------- (NB: ACS_CGMFIL write CGM-file name to SAVNAM)
!
                CALL ACS_CGMFIL   ( ONAMCG, 'O' )
                CALL USE_CGMF_COM ( 'R' )
                CALL ACS_CGMFIL   ( ONAMCG, 'C' )
!
! ------------- Specify output CGM file name -- SAVNAM
!
                CALL HOL2CHAR ( SOLUID, INT2(1), INT2(60), SID )
                CALL CREATE_CGMF ( 'SAVE', SID, NPARAM, 'M', OUTCGM )
!
! ------------- Calculate the number of 256-bytes blocks to be transferred from
! ------------- previous CGM to the new CGM
!
!@                JBLOCKS = JRND_BLOCKS(MAT_E(M_GPA,NPARAM)*REALL_WORDS)
!@                JBLOCKS = JBLOCKS+JSOCOM_BLOCKS+JPARFIL_BLOCKS+JPLIST_BLOCKS+1
!@                CALL COPY_FILE ( ONAMCG(1:I_LEN(ONAMCG)) , &
!@     &                           SAVNAM(1:I_LEN(SAVNAM)), 'Q', JBLOCKS )
!
                 LEN8_BYTES  = 8*(3*M_GPA + INT8(NPARMF)*INT8(NPARAM+1)/2)
                 LEN8_BLOCKS = LEN8_BLOCKS + JBLOCKS + JSOCOM_BLOCKS + JPARFIL_BLOCKS + JPLIST_BLOCKS + 1
!
! ------------- Copy JBLOCKS from the previous temporary CGM to permanemt CGM
!
                 CALL COPY_FILE ( ONAMCG(1:I_LEN(ONAMCG)), &
     &                            SAVNAM(1:I_LEN(SAVNAM)), 'Q', INT(LEN8_BLOCKS,KIND=4) )
!
! ------------- .. and eventually we save the name of the new file in GLBFIL
! ------------- common block
!
                CALL CLRCH ( ONAMCG )
                ONAMCG = SAVNAM
                CALL USE_GLBFIL ( 'OWC' )
           END IF
           CALL END_PROG() ! end of the work for skipped arc
      END IF
!
      STACM = KBIT( PRE_IBATCH, INT2(8))
      IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD  .OR. &
     &     FAST_MODE .EQ. F__B1D       ) THEN
!
! -------- Calculalatoin various lists of the parameters to be estimated
! -------- as well as their crossreference.
! -------- IX1T3 is the cross reference from PROC order of parameters
! -------- to ARCPE order
!
           CALL LISTS_ARC ( IX1T3, NPARM1, NPARM3, NGTA, NLTA, STACM, LPARM )
!
! -------- Array ARR1 starts at beginning
! -------- Array ARR2 starts at the next page boundary after array ARR1
!
!@           M1=I4P1
!@           M2=I4P1+I4P256*((MAT_E(M_GPA,NPARM1)+I4P255)/I4P256) + I4P256
           M2 = MAT_E4 ( M_GPA, NPARM1 )
           MATSIZE = M2*8 ! Convert to number of bytes (8 per real*8 element)
!
! -------- Grabbing dynamic memory
!
           IUER = -1
           CALL GRAB_MEM ( IUER, MLEN, MEM_ADR, 2,     &
     &                     INT8(2)*MATSIZE, ADDR_ARR1, &
     &                     INT8(2)*MATSIZE, ADDR_ARR2  )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( MLEN, STR )
                IUER  = -1
                CALL ERR_LOG ( 8500, IUER, 'ARCPE', 'Failure to allocate '// &
     &                         STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                CALL EXIT ( 1 ) 
           END IF
!
! -------- Reading normal equation to the array ARR2. FULL case
!
           CALL USE_NRMFIL ( %VAL(ADDR_ARR2), NPARM1, 'ORC' )
        ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
           CALL CLRCH ( FINAM_NRM )
           FINAM_NRM = PRE_SCR_DIR(1:PRE_SD_LEN)//'NRMF'//PRE_LETRS
!
! -------- Reading fields of B1B3DOBJ and B3DOBJ objects. B1B3D case
!
           IUER=-1
           CALL RDNOR_B1B3D ( FINAM_NRM, B3DOBJ, B1B3DOBJ, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8501, -1, 'ARCPE', &
     &              'Error during reading file '//FINAM_NRM(1:I_LEN(FINAM_NRM))// &
     &              ' with temporary '// &
     &              'data structure for B1B3D algorithm while database '// &
     &               B3DOBJ%DBNAME_MES//' was processing' )
                STOP 'ARCPE: Abnormal termination'
           ENDIF
!
           IUER = -1
           CALL REPARAM ( B3DOBJ%U_STA, B3DOBJ%UIS_STA,B3DOBJ%U_BAS, &
     &          B3DOBJ%UIS_BAS,B3DOBJ%R_SOU, B3DOBJ%RIS_SOU,B3DOBJ%R_STA, &
     &          B3DOBJ%RIS_STA,B3DOBJ%R_BAS, B3DOBJ%RIS_BAS, 0, IUER )
           IF ( IUER .NE. 0 ) THEN
                CALL ERR_LOG ( 8502, -1, 'ARCPE', &
     &              'Error during attempt to reparameterize solution '// &
     &              'while database '//B3DOBJ%DBNAME_MES//' was processing' )
                STOP 'ARCPE: Abnormal termination'
           ENDIF
!
! -------- Calculalatoin various lists of the parameters to be estimated
! -------- as well as their crossreference.
! -------- IX1T3 is the cross reference from PROC order of parameters
! -------- to ARCPE order
!
           CALL LISTS_ARC ( IX1T3, NPARM1, NPARM3, NGTA, NLTA, STACM, LPARM )
!
! -------- Array ARR1 starts at beginning
! -------- Array ARR2 starts at the next page boundary after array ARR1
!
!@           M1=I4P1
!@           M2=I4P1+I4P256*((MAT_E(M_GPA,NPARM1)+I4P255)/I4P256) + I4P256
           M2 = MAT_E4 ( M_GPA, NPARM1 )
           MATSIZE = M2*8 ! Convert to number of bytes (8 per real*8 element)
!
! -------- Grabbing dynamic memory
!
           IUER = -1
           CALL GRAB_MEM ( IUER, MLEN, MEM_ADR, 2,     &
     &                     INT8(2)*MATSIZE, ADDR_ARR1, &
     &                     INT8(2)*MATSIZE, ADDR_ARR2  )
           IF ( IUER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( MLEN, STR )
                IUER  = -1
                CALL ERR_LOG ( 8503, IUER, 'ARCPE', 'Failure to allocate '// &
     &                         STR(1:I_LEN(STR))//' bytes of dynamic memory' )
                CALL EXIT ( 1 ) 
           END IF
        ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
           CALL ERR_LOG ( 8503, IUER, 'ARCPE', 'Wrong fast mode: '// &
     &         'attempt to call ARCPE when FAST_MODE is B3D. Only fast modes '// &
     &         'B1B3D or NONE are supported for the global solutions. Please '// &
     &         'change or add the line in SETUP section of your batch file: '// &
     &         'FAST_MODE B1B3D' )
           STOP 'ARCPE: Abnormal termination'
      END IF
!
! --- Apply eop constraint if fixed arc
!
      KFIXED  = KBIT( PRE_IBATCH, INT2(14) )
      IOFF = 3*M_GPA
      IF ( KBIT( PRE_IBATCH, INT2(14)) ) THEN
           CALL FIX_EOP ( B3DOBJ, B1B3DOBJ, %VAL(ADDR_ARR2), IOFF, LPARM, NPARM1 )
      ENDIF
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_GET ( 'ARCPE_01' )
      END IF
!
! --- Call the good old main module
!
      CALL ARCPE_MAIN ( M2, NPARM1, IX1T3, %VAL(ADDR_ARR1), %VAL(ADDR_ARR2), &
     &                  STACM, B3DOBJ, B1B3DOBJ )
      IF ( STACM ) THEN
!
! -------- If doing correlations save commons
!
           CALL USE_COMMON ( 'ORC' )
           CALL SOCOM_EXT()
           CALL USE_PARFIL ( 'ORC' )
           CALL ACS_ARCFIL ( SAVAF, STACM, 'O' )
           CALL USE_ARCF_COM ( 'W' )
      ENDIF
!
      CALL END_PROG()
      END  !#!  ARCPE  #!#
