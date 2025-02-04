      SUBROUTINE WRITE_CNSTR ( CNSTROBJ, CNI_MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRITE_CNSTR  writes down constraint equations in the      *
! *   file CNQLxx/CNQGxx. It also writes **some** elements of normal     *
! *   matrix of constraint equations on the disks in the file            *
! *   CSPRxx/CSPGxx although this feature is considered depricated.      *
! *                                                                      *
! *  ###  20-JAN-1998  WRITE_CNSTR  v3.1  (c) L. Petrov 04-AUG-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT    NONE 
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  CNI_MODE, IUER
      INTEGER*4  J1, J2, J3, LUN, LUN_DBG, IER
      LOGICAL*4  LEX, FL_DEBUG_CNS
      CHARACTER  FNAME*128, FIL_DBG*128, STR*80, STR1*80
      INTEGER*4  GET_UNIT, I_LEN
!
      CALL GETENVAR ( 'DEBUG_CNS_WRI', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR(1:1) == 'Y' ) THEN
           FL_DEBUG_CNS = .TRUE.
         ELSE
           FL_DEBUG_CNS = .FALSE.
      END IF
!
      IF ( CNI_MODE .NE. CNSTROBJ%CNS_TYP ) THEN
           CALL CLRCH   ( STR  )
           CALL INCH    ( CNI_MODE, STR )
           CALL CLRCH   ( STR1 )
           CALL INCH    ( CNSTROBJ%CNS_TYP, STR1 )
           CALL ERR_LOG ( 8371, IUER, 'WRITE_CNSTR', 'CNI_MODE mismatch: '// &
     &         'argument for WRITE_CNSTR is '//STR(1:I_LEN(STR))//' while '// &
     &         'the object CNSTROBJ keeps '//STR1(1:I_LEN(STR1)) )
           RETURN
      END IF
!
! --- Open file for constraint equations
!
      IF ( CNI_MODE .EQ. CNI__LOC ) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNQL'//PRE_LETRS
         ELSE IF ( CNI_MODE .EQ. CNI__GLO ) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNQG'//PRE_LETRS
         ELSE IF ( CNI_MODE .EQ. CNI__ULC ) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ULCC'//PRE_LETRS
         ELSE IF ( CNI_MODE .EQ. CNI__UGL ) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'UGLC'//PRE_LETRS
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( CNI_MODE, STR )
           CALL ERR_LOG ( 8371, IUER, 'WRITE_CNSTR', 'Wrong second argument: '// &
     &          STR )
           RETURN
      END IF
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=FNAME,  EXIST=LEX )
      IF ( LEX ) THEN
           CALL UNLINK ( FNAME(1:I_LEN(FNAME))//CHAR(0) )
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FNAME, 'UNKNOWN', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8372, IUER, 'WRITE_CNSTR', 'Error in an attempt '// &
     &         'to open output file '//FNAME )
           RETURN
      END IF
!
! --- Write the label of the file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'B1', LEN(CNS__LABEL), CNS__LABEL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8373, IUER, 'WRITE_CNSTR', 'Error in attempt to '// &
     &         'write in output file '//FNAME )
           RETURN
      END IF
!
      CALL WRBIN_ARRAY ( LUN, 'I4',  1, CNSTROBJ%CNS_TYP, IER )
!
      CALL WRBIN_ARRAY ( LUN, 'B1', 10, CNSTROBJ%OBJ_NAM, IER )
!
! --- Write down constraint equations information
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_ARRAY ( LUN, 'I4', 1, CNSTROBJ%N_EQUAT, IER )
      CALL WRBIN_ARRAY ( LUN, 'I4', 1, CNSTROBJ%N_ECNST, IER )
      CALL WRBIN_ARRAY ( LUN, 'I4', 1, CNSTROBJ%N_OFD,   IER )
      IF ( FL_DEBUG_CNS ) THEN
           LUN_DBG = GET_UNIT()
           IF ( CNSTROBJ%OBJ_NAM == 'GLOBAL' ) THEN
                FIL_DBG = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNSG'//PRE_LETRS
              ELSE
                FIL_DBG = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNSS'//PRE_LETRS
           END IF
           OPEN ( FILE=FIL_DBG, UNIT=LUN_DBG, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8373, IUER, 'WRITE_CNSTR', 'Error in attempt to '// &
     &              'open the output file '//FIL_DBG )
                RETURN
           END IF
           WRITE ( 6, * ) 'Constraint debugging information is written in file '// &
     &                    TRIM(FIL_DBG)
           WRITE ( UNIT=LUN_DBG, FMT=210 ) CNSTROBJ%CNS_TYP, &
     &                                     CNSTROBJ%OBJ_NAM, &
     &                                     CNSTROBJ%N_EQUAT, &
     &                                     CNSTROBJ%N_ECNST, &
     &                                     CNSTROBJ%N_OFD
 210       FORMAT ( 'CNS_TYP: ', I6, ' CNS_NAM: ', A10, ' N_EQUAT: ', I8, &
     &              ' N_ECNST: ', I8, ' N_OFD: ', I8 )
      END IF
!
      IF ( CNSTROBJ%N_EQUAT .GT. 0 ) THEN
!
! -------- Write down description of constraint
!
           CALL WRBIN_ARRAY ( LUN, 'B1', 8*CNSTROBJ%N_EQUAT,  CNSTROBJ%ABB_CNS, &
     &                        IER )
           CALL WRBIN_ARRAY ( LUN, 'B1', 32*CNSTROBJ%N_EQUAT, CNSTROBJ%DSC_CNS, &
     &                        IER )
           CALL WRBIN_ARRAY ( LUN, 'B1', 16*CNSTROBJ%N_EQUAT, CNSTROBJ%UNT_CNS, &
     &                        IER )
           CALL WRBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_EQUAT,    CNSTROBJ%SBI_CNS, &
     &                        IER )
           CALL WRBIN_ARRAY ( LUN, 'R8', CNSTROBJ%N_EQUAT,    CNSTROBJ%SIG_CNS, &
     &                        IER )
           CALL WRBIN_ARRAY ( LUN, 'L4', CNSTROBJ%N_EQUAT,    CNSTROBJ%GLO_CNS, &
     &                        IER )
           CALL WRBIN_ARRAY ( LUN, 'L4', CNSTROBJ%N_EQUAT,    CNSTROBJ%USR_CNS, &
     &                        IER )
           IF ( FL_DEBUG_CNS .AND. CNSTROBJ%N_EQUAT > 0 ) THEN
                DO 510 J1=1,CNSTROBJ%N_EQUAT
                   WRITE ( UNIT=LUN_DBG, FMT=220 ) J1, CNSTROBJ%ABB_CNS(J1), &
     &                                                 CNSTROBJ%DSC_CNS(J1), &
     &                                                 CNSTROBJ%UNT_CNS(J1), &
     &                                                 CNSTROBJ%SBI_CNS(J1), &
     &                                                 CNSTROBJ%SIG_CNS(J1), &
     &                                                 CNSTROBJ%RTP_CNS(J1), &
     &                                                 CNSTROBJ%GLO_CNS(J1), &
     &                                                 CNSTROBJ%USR_CNS(J1)
 220               FORMAT ( 'Indcns: ', I6, ' Abb_cns: ', A, ' Dsc_cns: ', A, &
     &                      ' Unt_cns: ', A, ' SBI_cns: ', I5, &
     &                      ' Sig_cns: ', 1PD14.6, ' Rtp_cns: ', 1PD14.6, &
     &                      ' Glo_cns: ', L1, ' Usr_cns: ', L 1)
 510            CONTINUE 
           END IF
      END IF
!
      IF ( CNSTROBJ%N_ECNST .GT. 0 ) THEN
!
! ------ Write down coefficients of constraint equations
!
         CALL WRBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_ECNST, CNSTROBJ%EQU_INE, IER )
         CALL WRBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_ECNST, CNSTROBJ%EQU_INP, IER )
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'R8', CNSTROBJ%N_ECNST, CNSTROBJ%EQU_CNS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8374, IUER, 'WRITE_CNSTR', 'Error in an attempt '// &
     &            'to write in output file '//FNAME )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER )
         CALL WRBIN_ARRAY ( LUN, 'R8', CNSTROBJ%N_EQUAT, CNSTROBJ%RTP_CNS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8375, IUER, 'WRITE_CNSTR', 'Error in an attempt '// &
     &            'to write in output file '//FNAME )
              RETURN
         END IF
         IF ( FL_DEBUG_CNS ) THEN
              DO 520 J2=1,CNSTROBJ%N_ECNST
                 WRITE ( UNIT=LUN_DBG, FMT=230 ) J2, CNSTROBJ%EQU_INE(J2), &
     &                                               CNSTROBJ%EQU_INP(J2), &
     &                                               CNSTROBJ%EQU_CNS(J2)
 230             FORMAT ( 'Icnst: ', I8, ' Iequ: ', I8, ' Ipar: ', I8, ' Equ: ', 1PD19.12 )
 520          CONTINUE 
         END IF
      END IF
!
      IF ( CNSTROBJ%N_OFD .GT. 0 ) THEN
!
! -------- Write down coefficients of the off-diagonal terms of weight matrix
! -------- of constratins
!
           CALL WRBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_OFD, CNSTROBJ%INE1_OFD, IER )
           CALL WRBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_OFD, CNSTROBJ%INE2_OFD, IER )
!
           CALL ERR_PASS ( IUER, IER )
           CALL WRBIN_ARRAY ( LUN, 'R8', CNSTROBJ%N_OFD, CNSTROBJ%WEI_OFD, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8376, IUER, 'WRITE_CNSTR', 'Error in an '// &
     &              'attempt to write in output file '//FNAME )
                RETURN
           END IF
!
           IF ( FL_DEBUG_CNS ) THEN
                DO 530 J3=1,CNSTROBJ%N_OFD
                   WRITE ( UNIT=LUN_DBG, FMT=240 ) J3, CNSTROBJ%EQU_INE(J2), &
     &                                                 CNSTROBJ%EQU_INP(J2), &
     &                                                 CNSTROBJ%WEI_OFD(J2)
 240               FORMAT ( 'Off Iequ1: ', I8, ' Iequ2: ', I8, ' Ipar: ', I8, &
     &                      ' Wei: ', 1PD19.12 )
 530            CONTINUE 
         END IF
      END IF
      IF ( FL_DEBUG_CNS ) THEN
           CLOSE ( UNIT=LUN_DBG )
      END IF
!
      IER = 0
      CALL BINF_CLOSE  ( LUN, IER )
      IF ( CNSTROBJ%N_MATEL .EQ. 0 ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
! --- Pre SEP-2002 style of constraints information whcih kept normal matrix
! --- of constraints
!
      IF ( CNI_MODE .EQ. CNI__LOC ) THEN
           FNAME = PRE_SCR_DIR(:PRE_SD_LEN)//'CSPR'//PRE_LETRS
         ELSE IF ( CNI_MODE .EQ. CNI__GLO ) THEN
           FNAME = PRE_SCR_DIR(:PRE_SD_LEN)//'CSPG'//PRE_LETRS
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( CNI_MODE, STR )
           CALL ERR_LOG ( 8376, IUER, 'WRITE_CNSTR', 'Wrong parameter '// &
     &                   'CNI_MODE: '//STR )
           RETURN
      END IF
!
      LUN = GET_UNIT()
      OPEN ( LUN, FILE=FNAME, STATUS='UNKNOWN' )
!
! --- Writing the list of modified elements of the normal matrix
!
      IF ( CNSTROBJ%N_MATEL .GT. 0 ) THEN
           DO 410 J1=1,CNSTROBJ%N_MATEL
              WRITE ( LUN, * ) CNSTROBJ%NP1(J1), CNSTROBJ%NP2(J1), &
     &                        CNSTROBJ%MAT_UPD(J1)
 410       CONTINUE
      END IF
      WRITE ( LUN, * )  0, 0, 0.0
!
! --- Writing the list of modified elements
!
      IF ( CNSTROBJ%N_VECEL .GT. 0 ) THEN
           DO 420 J2=1,CNSTROBJ%N_VECEL
              WRITE ( LUN, * ) CNSTROBJ%NR(J2), CNSTROBJ%VEC_UPD(J2)
 420       CONTINUE
      END IF
      WRITE ( LUN, * )  0, 0.0
!
! --- Writing the list of type of constraints
!
      IF ( CNSTROBJ%N_TYCNS .GT. 0 ) THEN
           DO 430 J3=1,CNSTROBJ%N_TYCNS
              WRITE ( LUN, 130 ) J3, CNSTROBJ%ABBR(J3), CNSTROBJ%DESCR(J3), &
     &                               CNSTROBJ%UNITS(J3), CNSTROBJ%GLO(J3), &
     &                               CNSTROBJ%SIGMA(J3)
 130          FORMAT ( I3, 1X, A8, 1X, A32, 1X, A16, 1X, L1, 1X, E22.16 )
 430       CONTINUE
      END IF
      WRITE ( LUN, 130 ) 0, ' ', ' ', ' ', .FALSE., 0.0D0
      CLOSE ( UNIT=LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRITE_CNSTR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_CNSTR ( CNSTROBJ, CNI_MODE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_CNSTR  reads constraints from the disks file CSPRxx  *
! *   into the object  CNSTROBJ.                                         *
! *                                                                      *
! *  ###  20-JAN-98    READ_CNSTR  v3.0  (c)  L. Petrov 27-SEP-2002 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'cnstr.i'
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4  CNI_MODE, IUER
      INTEGER*4  K, LUN, NEL, J1, J2, J3, IER
      LOGICAL*4  LEX
      CHARACTER  FNAME*128, STR*128, STR1*128
      INTEGER*4  I_LEN
!
! --- Open file for constraint equations
!
      IF ( CNI_MODE .EQ. CNI__LOC ) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNQL'//PRE_LETRS
         ELSE IF ( CNI_MODE .EQ. CNI__GLO ) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNQG'//PRE_LETRS
         ELSE IF ( CNI_MODE .EQ. CNI__ULC ) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'ULCC'//PRE_LETRS
         ELSE IF ( CNI_MODE .EQ. CNI__UGL ) THEN
           FNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'UGLC'//PRE_LETRS
         ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( CNI_MODE, STR )
           CALL ERR_LOG ( 8381, IUER, 'READ_CNSTR', 'Wrong second argument: '// &
     &          STR )
           RETURN
      END IF
!
      INQUIRE ( FILE=FNAME, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
!
! -------- File does not exists. Nothing to do
!
           CNSTROBJ%N_EQUAT = 0
           CNSTROBJ%N_ECNST = 0
           CNSTROBJ%N_OFD   = 0
           CNSTROBJ%OBJ_NAM = 'unknown   '
           CNSTROBJ%CNS_TYP = CNI__UND
           CALL ERR_LOG ( 0, IUER )
           RETURN
         ELSE
!
! -------- Open the file
!
           CALL ERR_PASS ( IUER, IER )
           CALL BINF_OPEN ( FNAME, 'OLD', LUN, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8382, IUER, 'READ_CNSTR', 'Error in '// &
     &               'an attempt to open input file '//FNAME )
                RETURN
           END IF
!
           CALL RDBIN_ARRAY ( LUN, 'B1', LEN(CNS__LABEL), STR, NEL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8383, IUER, 'READ_CNSTR', 'Error in attempt '// &
     &              'to the scratch file '//FNAME(1:I_LEN(FNAME))// &
     &              'with constraint equations ' )
                RETURN
           END IF
!
           IF ( STR(1:LEN(CNS__LABEL)) .NE. CNS__LABEL ) THEN
                CALL TRAN ( 13, STR, STR )
                CALL ERR_LOG ( 8384, IUER, 'READ_CNSTR', 'Wrong format of '// &
     &              'Solve constraints file '//FNAME(1:I_LEN(FNAME))// &
     &              ' -- the first line is '//STR(1:I_LEN(STR))// &
     &              'while '//CNS__LABEL//' was expected.' )
                RETURN
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_ARRAY ( LUN, 'I4', 1, CNSTROBJ%CNS_TYP, NEL, IER )
           IF ( CNI_MODE .NE. CNSTROBJ%CNS_TYP ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( CNI_MODE, STR )
                CALL CLRCH ( STR1 )
                CALL INCH  ( CNSTROBJ%CNS_TYP, STR )
                CALL ERR_LOG ( 8385, IUER, 'READ_CNSTR', 'constraint '// &
     &              'mode mismatch: argument of READ_CNSTR mode: '// &
     &               STR(1:I_LEN(STR))//' while the CNSTROBJ from file '// &
     &               FNAME(1:I_LEN(FNAME))//' has mode '//STR1 )
                RETURN
           END IF
!
           CALL RDBIN_ARRAY ( LUN, 'B1', 10, CNSTROBJ%OBJ_NAM, NEL, IER )
!
! -------- Read constraint equations information
!
           CALL RDBIN_ARRAY ( LUN, 'I4', 1, CNSTROBJ%N_EQUAT, NEL, IER )
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_ARRAY ( LUN, 'I4', 1, CNSTROBJ%N_ECNST, NEL, IER )
!
           CALL ERR_PASS ( IUER, IER )
           CALL RDBIN_ARRAY ( LUN, 'I4', 1, CNSTROBJ%N_OFD,   NEL, IER )
!
           IF ( CNSTROBJ%N_EQUAT .GT. 0 ) THEN
!
! ------------- Read description of constraint
!
                CALL RDBIN_ARRAY ( LUN, 'B1', 8*CNSTROBJ%N_EQUAT, &
     &                             CNSTROBJ%ABB_CNS, NEL, IER )
                CALL RDBIN_ARRAY ( LUN, 'B1', 32*CNSTROBJ%N_EQUAT, &
     &                             CNSTROBJ%DSC_CNS, NEL, IER )
                CALL RDBIN_ARRAY ( LUN, 'B1', 16*CNSTROBJ%N_EQUAT, &
     &                             CNSTROBJ%UNT_CNS, NEL, IER )
                CALL RDBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_EQUAT, &
     &                             CNSTROBJ%SBI_CNS, NEL, IER )
                CALL RDBIN_ARRAY ( LUN, 'R8', CNSTROBJ%N_EQUAT, &
     &                             CNSTROBJ%SIG_CNS, NEL, IER )
                CALL RDBIN_ARRAY ( LUN, 'L4', CNSTROBJ%N_EQUAT, &
     &                             CNSTROBJ%GLO_CNS, NEL, IER )
                CALL RDBIN_ARRAY ( LUN, 'L4', CNSTROBJ%N_EQUAT, &
     &                             CNSTROBJ%USR_CNS, NEL, IER )
           END IF
!
           IF ( CNSTROBJ%N_ECNST .GT. 0 ) THEN
!
! ------------- Read coefficients of constraint equations
!
                CALL RDBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_ECNST, &
     &                             CNSTROBJ%EQU_INE, NEL, IER )
                CALL RDBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_ECNST, &
     &                             CNSTROBJ%EQU_INP, NEL, IER )
!
                CALL ERR_PASS    ( IUER, IER )
                CALL RDBIN_ARRAY ( LUN, 'R8', CNSTROBJ%N_ECNST, &
     &                             CNSTROBJ%EQU_CNS, NEL, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8386, IUER, 'READ_CNSTR', 'Error in an '// &
     &                   'attempt to read input file '//FNAME )
                     RETURN
                END IF
!
                CALL ERR_PASS ( IUER, IER )
                CALL RDBIN_ARRAY ( LUN, 'R8', CNSTROBJ%N_EQUAT, &
     &                             CNSTROBJ%RTP_CNS, NEL, IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 8387, IUER, 'READ_CNSTR', 'Error in an '// &
     &                   'attempt to read from the input file '//FNAME )
                     RETURN
                END IF
          END IF
!
          IF ( CNSTROBJ%N_OFD .GT. 0 ) THEN
!
! ------------ Read coefficients of the off-diagonal terms of weight matrix
! ------------ of constratins
!
               CALL RDBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_OFD, CNSTROBJ%INE1_OFD, &
     &                            NEL, IER )
               CALL RDBIN_ARRAY ( LUN, 'I4', CNSTROBJ%N_OFD, CNSTROBJ%INE2_OFD, &
     &                            NEL, IER )
!
               CALL ERR_PASS    ( IUER, IER )
               CALL RDBIN_ARRAY ( LUN, 'R8', CNSTROBJ%N_OFD, CNSTROBJ%WEI_OFD, &
     &                            NEL, IER )
               IF ( IER .NE. 0 ) THEN
                    CALL ERR_LOG ( 8388, IUER, 'READ_CNSTR', 'Error in an '// &
     &                  'attempt to write in output file '//FNAME )
                    RETURN
               END IF
          END IF
!
! ------- Close the file
!
          IER = 0
          CALL BINF_CLOSE ( LUN, IER )
      END IF
!
      IF ( CNI_MODE .EQ. CNI__LOC  .OR.  CNI_MODE .EQ. CNI__GLO ) THEN
          IF ( CNI_MODE .EQ. CNI__LOC ) THEN
               FNAME = PRE_SCR_DIR(:PRE_SD_LEN)//'CSPR'//PRE_LETRS
            ELSE IF ( CNI_MODE .EQ. CNI__GLO ) THEN
               FNAME = PRE_SCR_DIR(:PRE_SD_LEN)//'CSPG'//PRE_LETRS
          END IF
!
! ------- Check whether the file exists and if not -- go home
!
         INQUIRE ( FILE=FNAME, EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              CNSTROBJ%N_MATEL = 0
              CNSTROBJ%N_VECEL = 0
              CNSTROBJ%N_TYCNS = 0
              CALL ERR_LOG ( 0, IUER )
              RETURN
           ELSE
              OPEN ( 66, FILE=FNAME, STATUS='OLD' )
!
! ----------- Reading modified elements of the normal matrix
!
              DO 410 J1=1,MAX_CNSTR
                 READ ( 66, * ) CNSTROBJ%NP1(J1), CNSTROBJ%NP2(J1), &
     &                          CNSTROBJ%MAT_UPD(J1)
                 IF ( CNSTROBJ%NP1(J1) .EQ. 0 ) THEN
                      CNSTROBJ%N_MATEL = J1-1
                      GOTO 810
                 END IF
 410          CONTINUE
              CNSTROBJ%N_MATEL = MAX_CNSTR
 810          CONTINUE
!
! ----------- Reading modified elements of the normal vector
!
              DO 420 J2=1,MAX_CNSTR
                 READ ( 66, * ) CNSTROBJ%NR(J2), CNSTROBJ%VEC_UPD(J2)
                 IF ( CNSTROBJ%NR(J2) .EQ. 0 ) THEN
                      CNSTROBJ%N_VECEL = J2-1
                      GOTO 820
                 END IF
 420          CONTINUE
              CNSTROBJ%N_VECEL = MAX_CNSTR
 820          CONTINUE
!
! ----------- Reading type of constraints
!
              DO 430 J3=1,MAX_TYCNS
                 READ ( 66, 130 ) K, CNSTROBJ%ABBR(J3),  CNSTROBJ%DESCR(J3), &
     &                               CNSTROBJ%UNITS(J3), CNSTROBJ%GLO(J3), &
     &                               CNSTROBJ%SIGMA(J3)
 130             FORMAT ( I3, 1X, A8, 1X, A32, 1X, A16, 1X, L1, 1X, E22.16 )
                 IF ( K .EQ. 0 ) THEN
                      CNSTROBJ%N_TYCNS = J3-1
                      GOTO 830
                 END IF
 430          CONTINUE
!
              CNSTROBJ%N_TYCNS = MAX_TYCNS
 830          CONTINUE
!
              CLOSE ( UNIT=66 )
          END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  READ_CNSTR  #!#
