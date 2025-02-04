        SUBROUTINE DO_USER_CONSTRAINT ( FGLOBAL_L4, LPARM, NPARM, CNSTROBJ, &
     &                                  NOR_MAT, NOR_VEC, IUER )
!CCCCCC
!
!***  subroutines for use by user_constraint part of norml.
!
!     program to execute user constraints.
!     modifications
!
!     jmg 9611?? remove dump_con routine.
!     kdb 961125 fix error message
!     JMG 970217 Modified so that constraints don't have to be 0.
!     PET 980602 Changed interface.
!        I.e, new constraints can be of the form C dot P=V
!        with some sigma.
!        Here C is the constraint vector.
!             P is the paramter   vector.
!             V is the value. (previously V was assumed to be 0.)
!
!        This is done by making following changes to normal equations:
!          N --->  N + C C^T/sigma^2
!          B --->  B + V*C/sigma^2
!     pet 980206   Modified interface
!     pet 1999.05.31  Made dynamic memory allocation for equation of
!                     constraints. Maximal number of equations of constraintes
!                     ( MAX_USC ) was moved to ../include/solve.i
!                     Added formal parametr IUER and error handling.
!     pet 2017.11.07  Updated for INTEGER*4  
!
!CCCCCC
        IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
        INCLUDE  'solve.i'
        INCLUDE  'socom.i'
        INCLUDE  'precm.i'
        INCLUDE  'prfil.i'
        INCLUDE  'glbcm.i'
        INCLUDE  'glbc3.i'
        INCLUDE  'glbc4.i'
        INCLUDE  'cnstr.i'
        INCLUDE  'fast.i'
        TYPE ( CNSTR__STRU ) ::    CNSTROBJ
!
!     cnsfname=file containing parameter names
!     cnsvname=constraint vectors.
!
        INTEGER*4  NPARM, NPARM_C, FILDES, LUN, CNI_CODE, CNI_TYPE
        LOGICAL*4  FGLOBAL_L4
        INTEGER*2  IPARM
        CHARACTER  CNSFNAME*128, CNSVNAME*128, LPARM(NPARM)*20
        INTEGER*4  JBYTES, JBLOCKS
        REAL*8     NOR_MAT(*), NOR_VEC(*)
!
! Cvec stores constraint vector, sigmas, and value.
!
        INTEGER*4  IUER, NUM_CON, IER
        INTEGER*8        MEM_LEN, CARR_LEN
        ADDRESS__TYPE :: MEM_ADR, CARR_ADR
        CHARACTER  STR*20, HEADER_STR*80, TOKEN*16
        INTEGER*2   INT2_ARG
        INTEGER*4   INT4
        INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
        INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_UNIT
!CCCCCCC
!
! ----- Write out the empty list of constraints and the list of parameter names
!
        CNSFNAME = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNSF'//PRE_LETRS
        NUM_CON = 0
        LUN = GET_UNIT ()
        OPEN  ( LUN, FILE=CNSFNAME, STATUS='UNKNOWN' )
        WRITE ( LUN, '(I10, I10, I10, I10)' ) NPARM, NUM_CON, CNI__UND, CNI__UND
        WRITE ( LUN, '(A20)'  ) (LPARM(IPARM),IPARM=1,NPARM)
        CLOSE ( LUN )
!
! ----- Run user constraint program.
!
        CALL RUN_PROG ( USER_CONST_PROG, 'WAIT', INT2(1) )
!
! ----- Read the file which contain the number of contraints, their
! ----- type and implementation code
!
        LUN = GET_UNIT ()
        OPEN ( UNIT=LUN, FILE=CNSFNAME, STATUS='OLD' )
!
! ----- Read the header line
!
        READ  ( LUN, '(A)' ) HEADER_STR
        CLOSE ( LUN )
!
! ----- Parse the line. It should contain 4 words ( the last two can be
! ----- omitted ): the total number of parameters, the number of constraints,
! ----- the type of the constrain and the type of implementation
!
        CALL SPLITSTRING ( HEADER_STR, TOKEN, HEADER_STR )
        READ ( UNIT=TOKEN, FMT='(I9)' ) NPARM_C
        CALL SPLITSTRING ( HEADER_STR, TOKEN, HEADER_STR )
        READ ( UNIT=TOKEN, FMT='(I9)' ) NUM_CON
        CALL SPLITSTRING ( HEADER_STR, TOKEN, HEADER_STR )
        IF ( ILEN(TOKEN) .GT. 0 ) THEN
             READ ( UNIT=TOKEN, FMT='(I9)' ) CNI_CODE
           ELSE
             CNI_CODE = CNI__UND
        END IF
!
        CALL SPLITSTRING ( HEADER_STR, TOKEN, HEADER_STR )
        IF ( ILEN(TOKEN) .GT. 0 ) THEN
             READ ( UNIT=TOKEN, FMT='(I9)' ) CNI_TYPE
           ELSE
             CNI_CODE = CNI__UND
        END IF
!
! ----- Check validity of the header
!
        IF ( NUM_CON .GT. MAX_USC ) THEN
             WRITE ( 6, * ) ' num_con = ',num_con,' max_usc = ',max_usc
             CALL FERR ( INT2(220), 'DO_USER: Too many constraint vectors '// &
     &           'in user file', INT2(0), INT2(0) )
             CALL ERR_LOG ( 8461, IUER, 'DO_USER_CONSTRAINT', &
     &           'Too many constraint vectors in user constraints file' )
             RETURN
        ENDIF
!
        IF ( NPARM_C .NE. NPARM ) THEN
             WRITE ( 6, * ) ' nparm_c = ',nparm_c,' nparm=',nparm
             CALL FERR ( INT2(222), &
     &           'DO_USER: The number of global parameters '// &
     &           'was calculated in USER_CONSTRAINTS programm erroneously', &
     &            INT2(0), INT2(0) )
             CALL ERR_LOG ( 8462, IUER, 'DO_USER_CONSTRAINT', &
     &           'The number of global parameters was calculated in '// &
     &           'USER_CONSTRAINTS programm erroneously' )
             RETURN
        END IF
!
        IF ( CNI_CODE .EQ. CNI__LOC  .OR. &
     &       CNI_CODE .EQ. CNI__GLO  .OR. &
     &       CNI_CODE .EQ. CNI__MIX  .OR. &
     &       CNI_CODE .EQ. CNI__UND       ) THEN
!
             CONTINUE
           ELSE
             CALL CLRCH ( STR )
             CALL INCH  ( CNI_CODE, STR )
             CALL ERR_LOG ( 8463, IUER, 'DO_USER_CONSTRAINT', &
     &           'The third word of file '//CNSFNAME(1:I_LEN(CNSFNAME))// &
     &           ' is not valid: '//STR  )
             RETURN
        END IF
!
        IF ( CNI_TYPE .EQ. CNI__ACI  .OR. &
     &       CNI_TYPE .EQ. CNI__DIR       ) THEN
!
             CONTINUE
           ELSE
             CALL CLRCH ( STR )
             CALL INCH  ( CNI_CODE, STR )
             CALL ERR_LOG ( 8464, IUER, 'DO_USER_CONSTRAINT', &
     &           'The fourth word of file '//CNSFNAME(1:I_LEN(CNSFNAME))// &
     &           ' is not valid: '//STR )
             RETURN
        END IF
!
! ----- No constraints this arc? Then done!
!
        IF ( NUM_CON .EQ. 0 ) THEN
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
!
        CARR_LEN = NUM_CON*(NPARM_C+2)*8 + 256
!
        CALL ERR_PASS ( IUER, IER )
        CALL GRAB_MEM ( IER, MEM_LEN,  MEM_ADR, 1, CARR_LEN, CARR_ADR )
        IF ( IER .NE. 0 ) THEN
             WRITE ( 6, * ) ' num_con = ',num_con,' nparm_c = ',nparm_c, &
     &              ' carr_len =',carr_len
             CALL CLRCH   ( STR )
             CALL IINCH   ( CARR_LEN, STR )
             CALL ERR_LOG ( 8465, IUER, 'DO_USER_CONSTRAINT', &
     &           'Error in attempt to grab '//STR(1:I_LEN(STR))//' bytes of '// &
     &           'dynamic memory for equations for user constraints' )
             RETURN
        END IF
!
! ----- Now open file containing constraint vectors, and read them in.
!
        CNSVNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'CNSV'//PRE_LETRS
        CALL BIN_OPEN ( CNSVNAME, FILDES, 'O' )
!
! ----- nparm+2=dim(constraint vector)+1 (sigma)+1(value)
!
        JBYTES  = NUM_CON*(NPARM_C+2)*8
        JBLOCKS = (JBYTES+255) /256
!
! ----- Read the vector of constraints
!
        CALL BIN_READ  ( CNSVNAME, FILDES, %VAL(CARR_ADR), JBLOCKS )
        CALL BIN_CLOSE ( CNSVNAME, FILDES )
        IF ( CNI_TYPE .EQ. CNI__DIR ) THEN
!
! ---------- Add constraints directly to normal matrix and normal vector
!
             CALL ERR_PASS ( IUER, IER )
             CALL ADD_DIR_CONSTRAINT ( NPARM, NUM_CON, CNI_CODE, &
     &                                 %VAL(CARR_ADR), NOR_MAT, NOR_VEC, &
     &                                 CNSTROBJ, IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8466, IUER, 'DO_USER_CONSTRAINT', &
     &                'Error in an attempt to grab '//STR(1:I_LEN(STR))// &
     &                ' bytes of dynamic memory for equations for user '// &
     &                'constraints' )
                  RETURN
             END IF
           ELSE
!
! ---------- Add constraints to the data structure CNSTROBJ
!
             CALL ERR_PASS ( IUER, IER )
             CALL ADD_USER_CONSTRAINT ( %VAL(CARR_ADR), NPARM, &
     &                                  NUM_CON, FGLOBAL_L4, CNSTROBJ, &
     &                                  IER )
             IF ( IER .NE. 0 ) THEN
                  CALL ERR_LOG ( 8467, IUER, 'DO_USER_CONSTRAINT', &
     &                'Error in an attempt to add user constraint equations '// &
     &                'to CNSTROBJ object' )
                  RETURN
             END IF
        END IF
        CALL FREE_MEM ( CARR_ADR )
!
        IF ( FAST_DBG .EQ. F__PRI ) THEN
             WRITE ( 6, * ) ' do_user       fast_mode = ',fast_mode,' ncnstr = ', &
     &                              cnstrobj%n_ecnst
        END IF
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  DO_USER_CONSTRAINT  #!#
!
! ------------------------------------------------------------------------
!
        SUBROUTINE ADD_USER_CONSTRAINT ( CVEC, NPARM, NUM_CON, FGLOBAL_L4, &
     &                                   CNSTROBJ, IUER )
        IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
        INCLUDE   'solve.i'
        INCLUDE   'cnstr.i'
!CCCCCC
!
!       modifications
!       jmg 9611?? No longer hard code the constraint to 1.d11.
!       JMG 970217 See comments above.
!       pet 980206 Added printing information about applying user constraints
!       pet 980211 Added (restored) support of update of the elements
!                  of normal vector
!       pet 2002.09.24   Completely re-wrote. Changed internal logic: the
!                        new version puts equations of constraitns in CNSTROBJ,
!                        while the old version put normal matrix of constraint
!                        equations
!
!CCCCCC
        INTEGER*4 NPARM, NUM_CON, IUER
        REAL*8    CVEC(NPARM+2,NUM_CON)
        REAL*8    VAL, SIG
!
        INTEGER*4  J1, J2, IER
        LOGICAL*4  FGLOBAL_L4
        TYPE ( CNSTR__STRU ) ::    CNSTROBJ
!CCCCCC
!
!     on entry
!     CVEC         -- first nparm entries constraint vector.
!     cvec(nparm+1)-- sigma
!     cvec(nparm+2)-- value (to constrain to)
!
!CCCCCC
        IF ( NUM_CON .GT. 0 ) THEN
             CALL ERR_LOG ( 0, IUER )
             RETURN
        END IF
!
        DO 410 J1=1,NUM_CON
!
! -------- Add the J1-th equation of constraints, non-zero elements only
!
           SIG = CVEC(NPARM+1,J1)
           VAL = CVEC(NPARM+2,J1)
           DO 420 J2=1,NPARM
              IF ( CVEC(J2,J1) .NE. 0.0D0 ) THEN
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_NAM ( 'USER', J1, 'USER CONSTRAINT', &
     &                  '????', VAL, SIG, FGLOBAL_L4, CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8721, IUER, 'ADD_USER_CONSTRAINT', &
     &                      'Error in an attempt to put information about '// &
     &                      'the velocity origin constraints' )
                        RETURN
                   END IF
!
! ---------------- Add coefficients of constraint equations
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL ADDCNS_EQU ( 'USER', J1, J2, 1.0D0, &
     &                                FGLOBAL_L4, CNSTROBJ, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 8722, IUER, 'ADD_USER_CONSTRAINT', &
     &                      'Error in an attempt to put coefficients of the '// &
     &                      'right ascension suppression constraint' )
                        RETURN
                   END IF
              END IF
 420       CONTINUE
 410    CONTINUE
!
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END  !#!  ADD_USER_CONSTRAINT  #!#
