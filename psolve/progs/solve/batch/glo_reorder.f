      SUBROUTINE GLO_REORDER ( GLBMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GLO_REORDER  reorders CGM. List of parameters which is    *
! *   kept in GLBMEM  data structure as an input may be in arbitrary     *
! *   order. GLO_REORDER changes the order of parameters in order to     *
! *   comply SOLVE partl rules. It sorts lists of stations and sources   *
! *   in according with criterion specified by variables SORT_SOU,       *
! *   SORT_STA kept in common block area glbc4.i . CGM and combined      *
! *   global vectors are reordered in order to comply rules of SOLVE     *
! *   (partl).                                                           *
! *                                                                      *
! *   To make reordering GLO_REORDER firstly makes emergency copy of the *
! *   CGM in the file CGMFxx, then reorder lists and then reads CGM      *
! *   emergency copy in the order in which it has been written and then  *
! *   fills the CGM in according with new order of parameters.           *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GLBMEM ( RECORD    ) -- Data structure which keeps addresses of  *
! *                             CGM, list of global parameters, global   *
! *                             socom, prfil and temporary normal        *
! *                             matrices. Defined in ../include/glbp.i   *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 07-JAN-1999  GLO_REORDER   v2.2  (c) L. Petrov 28-FEB-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbp.i'
      INCLUDE    'precm.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INCLUDE    'prfil.i'
      INCLUDE    'fast.i'
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
      INTEGER*4  IUER
!
      INTEGER*4    MU_BLO, LE_BLO, M_TMP
      PARAMETER  ( MU_BLO = 16384  )
      PARAMETER  ( LE_BLO = 256    ) ! Number of bytes in one block
      PARAMETER  ( M_TMP  = MU_BLO*LE_BLO ) ! 4Mb
      REAL*8     ARR_TMP(M_TMP), B_TMP(M_GPA)
      CHARACTER  STR*(L__GPA), LPARM_NRM(M_GPA)*(L__GPA), CGM_TEMP_NAME*160
      CHARACTER  STR1*128
      INTEGER*4  NPARM_NRM
      INTEGER*2  KERR, IDIRECT(BLOCK_WORDS)
      ADDRESS__TYPE :: IADR
      INTEGER*4  JXBTN(M_GPA), FILDES, J1, J2, J3, J4, IP, JB, IER, &
     &           JL, I, J, JRND_BLOCKS, SAVED_M_GPA
      INTEGER*8  BLO_POS, BLO_GET, JEL_REM, JBL_REM, JEL_POS
      LOGICAL*2  KSHORT, KGLOBAL
      INTEGER*8 LEN8_BYTES, LEN8_BLOCKS, POS8
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INTEGER*8  I8_ARG, J8_ARG, LOCS
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: I_LEN, LTM_DIF, MAT_E
      LOCS(I8_ARG,J8_ARG) = min(I8_ARG,J8_ARG) + (max(I8_ARG,J8_ARG)*(max(I8_ARG,J8_ARG)-1))/2
!
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_REORDER started '
      END IF
!
! --- Copying global SOCOM and PARFIL blocks from internal BATCH data structures
! --- to socom and parfil in order to provide data to GET_NAMES
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  %VAL(GLBMEM%ADR_GLO_SOCOM), PI_VAR   )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, %VAL(GLBMEM%ADR_GLO_PRFIL), VAXOF(1) )
!
! --- Sorting stations and sources lists in prfil in according with values
! --- SORT_STA and SORT_SOU
!
      CALL ERR_PASS ( IUER, IER )
      CALL GLO_SORT ( IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4171, IUER, 'GLO_REORDER', 'Error during an '// &
     &         'attempt to sort lists of stations and/or sources' )
           RETURN
      END IF
!
! --- Get a list of parameters in the form which NORML expects to get it
!
      KSHORT  = .TRUE.
      KGLOBAL = .TRUE.
      KGLOBONLY = .FALSE.
      SOCOM_PLUS_FIRST = SPL__UNDF
      CALL SOCOM_EXT()
      CALL GET_NAMES ( LPARM_NRM, L__GPA, M_GPA, NPARM_NRM, KSHORT, &
     &                 KGLOBAL )
      NPARAM = NPARM_NRM
      IF ( GLBMEM%L_GPA .NE. NPARM_NRM ) THEN
           WRITE ( 6, * ) 'old: GLBMEM%L_GPA = ', GLBMEM%L_GPA
           WRITE ( 6, * ) 'new: NPARM_NRM    = ', NPARM_NRM
           CALL ERR_LOG ( 4172, IUER, 'GLO_REORDER', 'Trap of internal '// &
     &         'control: the number of parameters was changed' )
           OPEN ( UNIT=77, FILE='/tmp/par.new', STATUS='UNKNOWN', IOSTAT=J1 )
           DO 510 J1=1,NPARM_NRM
              WRITE  ( 77, 177 ) J1, LPARM_NRM(J1)
 177          FORMAT ( I5,' >>',A,'<<' )
 510       CONTINUE
           CLOSE ( UNIT=77 )
!
           OPEN ( UNIT=77, FILE='/tmp/par.old', STATUS='UNKNOWN', IOSTAT=J1 )
           DO 520 J1=1,GLBMEM%L_GPA
              CALL LIB$MOVC3 ( L__GPA, %VAL(GLBMEM%ADR_C_GPA + L__GPA*(J1-1)), &
     &                         STR )
              WRITE  ( 77, 177 ) J1, STR(1:20)
 520       CONTINUE
           CLOSE ( UNIT=77 )
!
           WRITE ( 6, * ) ' Look at files /tmp/par.new, /tmp/par.old for '// &
     &                    'more details'
           RETURN
      END IF
!
! --- Comparison the parameters lists kept in GLBMEM and the parameters list
! --- LPARMS_NRM. Make a cross-reference table JXBTN: from BATCH list to NORML
! --- list
!
      DO 410 J1=1,GLBMEM%L_GPA
         CALL LIB$MOVC3 ( L__GPA, %VAL(GLBMEM%ADR_C_GPA + L__GPA*(J1-1)), STR )
         IP = LTM_DIF ( 1, GLBMEM%L_GPA, LPARM_NRM, STR )
         IF ( IP .LE. 0 ) THEN
              CALL ERR_LOG ( 4173, IUER, 'GLO_REORDER', 'Trap of internal '// &
     &            'control: parameter '//STR//' is not found' )
              RETURN
         END IF
         JXBTN(J1) = IP
 410  CONTINUE
!
! --- Reordering normal vector
!
      CALL NOUT_R8 ( M_GPA, B_TMP )
      DO 420 J2=1,GLBMEM%L_GPA
         CALL R8ELEM_ADD ( B_TMP, INT8(JXBTN(J2)), %VAL(GLBMEM%ADR_CGV), INT8(J2) )
 420  CONTINUE
      CALL COPY_V ( GLBMEM%L_GPA, B_TMP, %VAL(GLBMEM%ADR_CGV) )
!
! --- Make a filename for temporary CGM
!
      CALL CLRCH ( CGM_TEMP_NAME )
      CGM_TEMP_NAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CGMB'//PRE_LETRS
!
! --- Create a temporary CGM if it didn't exist and read the first block
!
      CALL CGM_CREATE8 ( IDIRECT, CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME)), &
     &                   FILDES, NPARM_NRM, 'U', KERR )
      IF ( KERR .NE. INT2(0) ) THEN
           WRITE ( 6, * ) ' kerr = ',kerr
           CALL ERR_LOG ( 4174, IUER, 'GLO_REORDER', 'Error during attempt '// &
     &         'to create/read temorary CGM file '// &
     &          CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME))//' during CGM reordering' )
           RETURN
      END IF
!
      JL=IDIRECT(3)
      POS8 = IDIRECT(3)*INT8(256)
      CALL MEMCPY ( SAVED_M_GPA, IDIRECT(6), %VAL(4) )
      IF ( SAVED_M_GPA .NE. M_GPA ) THEN
           WRITE ( 6, * ) ' SAVED_M_GPA = ', SAVED_M_GPA, ' M_GPA = ',M_GPA
           CALL ERR_LOG ( 4175, IUER, 'GLO_REORDER', 'Size mismatch in '// &
     &         'temorary CGM file '//CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME)) )
           RETURN
      ENDIF
!
! --- Writing CGM in the temporary CGM file
!
!@      JB = JRND_BLOCKS ( MAT_E(MAX_PAR,NPARM_NRM)*REALL_WORDS )
!@      CALL USE_FILE ( CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME)), FILDES, &
!@     &     %VAL(GLBMEM%ADR_CGM), JB, JL, 'W' )
      LEN8_BYTES  = 8*(3*M_GPA + INT8(NPARM_NRM)*INT8(NPARM_NRM+1)/2)
      LEN8_BLOCKS = (LEN8_BYTES+INT8(255))/INT8(256) 
      CALL USE_FILE8 ( CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME)), FILDES, &
     &     %VAL(GLBMEM%ADR_CGM), LEN8_BLOCKS, POS8, 'W' )
      CALL CGM_ACCESS ( IDIRECT, CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME)), &
     &                  FILDES, 'C' )
!
! --- Open a fresh file with temporary CGM
!
      CALL CGM_ACCESS ( IDIRECT, CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME)), &
     &                  FILDES, 'O' )
!
! --- Now reordering CGM.
! --- We will read CGM from temporary file by small portions: by M_TMP bytes
! --- and put these portions in temporary array ARR_TMP. Then we will put
! --- elements from ARR_TMP to CGM in GLBMEM in according with cross-reference
! --- table JXBTN
!
!@      BLO_POS = JL
!@      JBL_REM = JB
      BLO_POS = POS8
      JBL_REM = LEN8_BLOCKS
      JEL_REM = 0
      JEL_POS = 0
!
! --- Zeroing a memory area where CGM was located
!
      CALL NOUT8_R8 ( (INT8(GLBMEM%L_GPA)*INT8(GLBMEM%L_GPA+1))/2, %VAL(GLBMEM%ADR_CGM) )
!
      DO 430 J3=1,GLBMEM%L_GPA
         DO 440 J4=1,J3
            IF ( JEL_REM .EQ. 0 ) THEN
                 IF ( JBL_REM .LE. 0 ) THEN
                      WRITE ( 6, * ) ' J3=',J3, ' J4=',J4,' JB=',JB,' JL=',JL
                      WRITE ( 6, * ) ' JBL_REM = ',JBL_REM, ' JEL_REM = ', &
     &                JEL_REM
                      WRITE ( 6, * ) ' JEL_POS = ',JEL_POS
                      CALL ERR_LOG ( 4176, IUER, 'GLO_REORDER', 'Trap of '// &
     &                    'internal control: file ended prematurely' )
                      RETURN
                 END IF
!
! -------------- Reading BLO_GET blocks from temporary CGM and transferring
! -------------- them to ARR_TMP
!
                 BLO_GET = MIN ( MU_BLO, JBL_REM )
                 CALL USE_FILE8 ( CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME)), FILDES, &
     &                            ARR_TMP, BLO_GET, BLO_POS, 'R' )
!
! -------------- Decrement of counter of remained blocks and increment of
! -------------- counter of the current block position in the file
!
                 JBL_REM = JBL_REM - BLO_GET
                 BLO_POS = BLO_POS + BLO_GET
!
! -------------- Set counter of available elements in ARR_TMP and position of
! -------------- the first available element
!
                 JEL_REM = BLO_GET*LE_BLO/8
                 JEL_POS = 1
            END IF
!
! --------- Moving element J3, J4 of the previous CGM to the position in
! --------- the new CGM
!
            IADR = GLBMEM%ADR_CGM + 8*( LOCS( INT8(JXBTN(J3)), INT8(JXBTN(J4)) ) - 1 )
            CALL MOVE_R8 ( ARR_TMP(JEL_POS), %VAL(IADR) )
!
! --------- Increment position of the next element in the buffer and decrement
! --------- counter of remaining elements
!
            JEL_POS = JEL_POS + 1
            JEL_REM = JEL_REM - 1
 440     CONTINUE
 430  CONTINUE
!
! --- Closing file with temporary CGM
!
      CALL CGM_ACCESS ( IDIRECT, CGM_TEMP_NAME(1:I_LEN(CGM_TEMP_NAME)), &
     &                  FILDES, 'C' )
!
! --- Calculate the number of global parameters, and the index of beginning
! --- of the sections of stations, sources and other parameters. The number
! --- of parameters is written to socom
!
      CALL PARCNG()
!
! --- Copying global SOCOM and PARFIL blocks back to internal BATCH
! --- data structures
!
      CALL LIB$MOVC3 ( JSOCOM_BYTES,  PI_VAR,   %VAL(GLBMEM%ADR_GLO_SOCOM) )
      CALL LIB$MOVC3 ( JPARFIL_BYTES, VAXOF(1), %VAL(GLBMEM%ADR_GLO_PRFIL) )
!
! --- Copying the list of parameters in CGM in new order in according with
! --- changes which we have just done.
!
      CALL LIB$MOVC3 ( L__GPA*GLBMEM%L_GPA, %REF(LPARM_NRM), &
     &                 %VAL(GLBMEM%ADR_C_GPA) )
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' GLO_REORDER ended '
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLO_REORDER  #!#
