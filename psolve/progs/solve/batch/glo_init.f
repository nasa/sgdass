      SUBROUTINE GLO_INIT ( RESTRT_L2, CGMNMR, GLBMEM, IUER )
! ************************************************************************
! *                                                                      *
! *   Procedure  GLO_INIT initializes data structures for BATCH solution *
! *   in NO TRAIN mode. It grabs dynamic memory if needed and restores   *
! *   CGM if it is called in the restoration mode.                       *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * RESTART_L2 ( LOGICAL*2 ) -- .TRUE. if GLO_INIT is called in          *
! *                             restoration mode. GLO_INIT grabs dynamic *
! *                             memory for expected number of global     *
! *                             parameters in initial mode. And it       *
! *                             corrects this quantity for the number of *
! *                             global parameters which are already in   *
! *                             CGM if it is called in restoration mode. *
! *     CGMNMR ( CHARACTER ) -- Name of the input CGM. If the name is    *
! *                             not blank and not "NONE" then the common *
! *                             blocks from input CGM are read and the   *
! *                             number of global parameters is extracted.*
! *                             Then dynamic memory is grabbed to        *
! *                             allocate input CGM and GLBMEM.L_GPA --   *
! *                             the numher of global parameters is set.  *
! *                             Output value GLBMEM.L_GPA > 0 is an      *
! *                             indicator for CTRLS that input CGM       *
! *                             matrix should be read.                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *     GLBMEM ( RECORD    ) -- Data structure which keeps addresses of  *
! *                             CGM, list of global parameters, global   *
! *                             socom, prfil and temporary normal        *
! *                             matrices. Defined in ../include/glbp.i   *
! *                             Input value of GLBMEM_L_GPA =< 0 is      *
! *                             an indicator that it is the first call   *
! *                             og GLO_INIT.                             *
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
! *  2001.05.10  pet  Increased amount of memory allocated for CGM       *
! *                   by 256 bytes since it is read and written by       *
! *                      256-bytes-long blocks.                          *
! *                                                                      *
! *  ###  11-JAN-99    GLO_INIT    v2.2  (c)  L. Petrov 10-MAY-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'plist.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'glbp.i'
      INCLUDE    'fast.i'
      INCLUDE    'precm.i'
      INCLUDE    'socom.i'
      INCLUDE    'prfil.i'
!
      LOGICAL*2  RESTRT_L2
      INTEGER*4  IUER
      TYPE ( GLB_MEM__STRU ) ::  GLBMEM  ! defined in glbp.i
!
      CHARACTER  CGMNMR*(*), STR*32, STR1*32, STR2*32
      INTEGER*4  IER
      INTEGER*8  NELEM, LEN_GLBMEM, MEM_LEN, MFK_SIZ
      ADDRESS__TYPE :: MEM_ADR, MFK_ADR
      PARAMETER  ( MFK_SIZ = 32768 ) ! Size of faked memory block
      LOGICAL*4  F_MFK
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
      LEN_GLBMEM = LOC(GLBMEM%LAST_FIELD) - LOC(GLBMEM%FIRST_FIELD) + 4
!
      IF ( GLBMEM%L_GPA .LE. 0 ) THEN
           F_MFK = .TRUE.
         ELSE
           F_MFK = .FALSE.
      END IF
!
! --- Clear GLBMEM
!
      CALL NOUT8 ( LEN_GLBMEM, GLBMEM%FIRST_FIELD )
!
      IF ( TRAIN .OR. ISLTY2 .EQ. 'I' ) THEN
!
! -------- Oh, we are in TRAIN  mode  or we are in independent mode --
! -------- we came here erroneously. Come back!
!
           CALL ERR_LOG ( 0, IUER )
           RETURN
      END IF
!
      IF ( RESTRT_L2 ) THEN
!
! -------- We are in restoration mode. Then compute the number of parameters
! -------- for the CGM
!
           GLBMEM%NPAR_CGM = MAX ( TGLBLS + INC_PARLIM, GLO_PARLIM )
           IF ( GLBMEM%NPAR_CGM .GT. NRMFL_PARMS ) THEN
!
! ------------- It should not exceed max allowed number of parameters
!
                GLBMEM%NPAR_CGM = NRMFL_PARMS
           END IF
         ELSE
           IF ( ILEN(CGMNMR) .EQ. 0  .OR. &
     &          ( ILEN(CGMNMR) .EQ. 4  .AND.  CGMNMR(1:4) .EQ. 'NONE' ) .OR. &
     &          ( ILEN(CGMNMR) .EQ. 4  .AND.  CGMNMR(1:4) .EQ. 'SAVE' )   ) THEN
!
! ------------- We are in creation mode. No input CGM has been specified
!
                GLBMEM%NPAR_CGM = GLO_PARLIM
              ELSE
!
! ------------- We are in creation mode. Input CGM has been specified
!
                INAMCG = CGMNMR
!
! ------------- Reading common blocks of the Input CGM matrix
!
                CALL ERR_PASS    ( IUER, IER )
                CALL GLO_READCGM ( IER )
                IF ( IER .NE. 0 ) THEN
                     CALL ERR_LOG ( 4211, IUER, 'GLO_INIT', 'Error in '// &
     &                   'reading input CGM matrix' )
                     RETURN
                END IF
                IF ( NPARAM < 1 ) THEN
                     CALL ERR_LOG ( 4212, IUER, 'GLO_INIT', 'No pararmeters '// &
     &                   'were found in reading input CGM matrix. Please check '// &
     &                   'whether the input CGM corresponds to the current '// &
     &                   'Solve version' )
                     RETURN
                END IF
!
                GLBMEM%NPAR_CGM = MAX ( NPARAM+INC_PARLIM, GLO_PARLIM )
           END IF
!
           IF ( GLBMEM%NPAR_CGM .LT. GLO_PARLIM__LIM ) THEN
                CALL CLRCH   ( STR1 )
                CALL CLRCH   ( STR2 )
                CALL INCH    ( GLBMEM%NPAR_CGM, STR1 )
                CALL INCH    ( GLO_PARLIM,      STR2 )
                CALL ERR_LOG ( 4213, IUER, 'GLO_INIT', 'Parameter '// &
     &              'GLO_PARLIM  is too small: '//STR1(1:I_LEN(STR1))// &
     &               ' -- less than the low limit '//STR2(1:I_LEN(STR2)) )
                RETURN
           END IF
      END IF
!
      IF ( F_MFK ) THEN
!
! -------- A special cunning trick: we allocate "faked" block of memory in order
! -------- to secure memory defragmentation. This block will be released
! -------- after grabbing memory for global parameters. Address space will
! -------- have a "hole":
! -------- ___________________________________
! -------- |xxxxxxx|  hole  |gggggggggggggggg|
! -------- ~~~~~~~~|~~~~~~~~|~~~~~~~~~~~~~~~~|
! -------- This hole is left to force furthers call to dynamic memory for small
! -------- pieces of memory to grant dynamic memory from this hole but and the
! -------- space after global CGM will be used for granting (and releasing)
! -------- large amount of memory. This trick prevent memory defragmentation.
!
! -------- This trick is done during the first call of GLO_INIT before
! -------- processing the first session
!
           CALL GRAB_MEM ( IER, MEM_LEN, MEM_ADR, 1, &
     &                          MFK_SIZ, MFK_ADR    )
      END IF
!
! --- Allocation of a pool of memory for CGM, combined global vector
!
      NELEM = (INT8(GLBMEM%NPAR_CGM)*INT8(GLBMEM%NPAR_CGM+1))/2 + 3*M_GPA + &
     &         BLOCK_BYTES
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) '(BATCH) glo_init:  GLBMEM%NPAR_CGM=',GLBMEM%NPAR_CGM
           WRITE ( 6, * ) '(BATCH) glo_init:  TGLBLS =',TGLBLS, &
     &                                      ' NPARAM=',NPARAM, &
     &                                      ' NELEM = ', NELEM
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GRAB_MEM ( IER, GLBMEM%LEN_GLO,           GLBMEM%ADR_GLO,       4, &
     &                     INT8(8)*NELEM,            GLBMEM%ADR_CGM_BLO,   &
     &                     INT8(L__GPA)*INT8(M_GPA), GLBMEM%ADR_C_GPA,     &
     &                     INT8(JSOCOM_BYTES),       GLBMEM%ADR_GLO_SOCOM, &
     &                     INT8(JPARFIL_BYTES),      GLBMEM%ADR_GLO_PRFIL  )
#ifdef DEBUG
      WRITE ( 6, * ) '(BATCH) glo_init(188):  NELEM= ', NELEM, ' GLBMEM%LEN_GLO= ', GLBMEM%LEN_GLO ; call flush ( 6 )
      WRITE ( 6, * ) '(BATCH) glo_init:  GLBMEM%NPAR_CGM=',GLBMEM%NPAR_CGM
      WRITE ( 6, * ) '(BATCH) glo_init:  TGLBLS =',TGLBLS, &
     &                                 ' NPARAM=',NPARAM, &
     &                                 ' NELEM = ', NELEM
#endif
      IF ( IER .NE. 0 ) THEN
!
! -------- Failure
!
           CALL CLRCH   ( STR )
           CALL INCH    ( GLBMEM%NPAR_CGM, STR )
           CALL ERR_LOG ( 4214, IUER, 'GLO_INIT', 'Failure in grabbing '// &
     &         ' dynamic memory for CGM sized to handle '// &
     &          STR(1:I_LEN(STR))//' parameters' )
           GLBMEM%LEN_GLO = -1
           RETURN
         ELSE
!
! -------- Success
!
           GLBMEM%ADR_CGM = GLBMEM%ADR_CGM_BLO + 8*(3*M_GPA)
           GLBMEM%ADR_CGV = GLBMEM%ADR_CGM_BLO + 8*(2*M_GPA)
           GLBMEM%L_GPA   = 0
!
! -------- Zeroing the piece of grabbed memory for global parameters
!
           CALL NOUT8 ( GLBMEM%LEN_GLO, %VAL(GLBMEM%ADR_GLO) )
      END IF
      IF ( F_MFK ) THEN
!
! -------- Release a faked block of memory
!
           CALL FREE_MEM ( MEM_ADR )
      END IF
!
      IF ( ILEN(CGMNMR) .GT. 0 ) THEN
!
! -------- Copy number of parameters. It serves as an indicator of that
! -------- we have to read an input CGM matrx soon.
!
           GLBMEM%L_GPA = NPARAM
      END IF
#ifdef DEBUG
   write ( 6, * ) 'BATCH glo_init(231) GLBMEM%L_GPA = ', GLBMEM%L_GPA, ' nparam = ', nparam, &
     &            ' CGMNMR= ', trim(cgmnmr), ' nelem= ', nelem, &
     &            ' glbmem%len_glo= ', glbmem%len_glo, ' restrt_l2= ', restrt_l2 ; call flush ( 6 ) ! %%%%%%%%%%%%
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GLO_INIT  #!#
