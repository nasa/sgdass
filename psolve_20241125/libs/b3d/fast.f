      SUBROUTINE F__CLR_IND ( IPAR, FAST_MODE, PLACE, B3DOBJ, B1B3DOBJ  )
! ************************************************************************
! *                                                                      *
! *   Routine  F__CLR_IND  cleans fields of data structure PLACE.        *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      IPAR ( INTEGER*4 ) -- Mode switch.                              *
! *                       IPAR=0 -- makes clearings needed before        *
! *                                 consideration new observation.       *
! *                       IPAR=1 -- makes clearings needed before        *
! *                                 consideration new common segment.    *
! *                       IPAR=2 -- makes segments counters              *
! *                                 initializations needed before        *
! *                                 consideration the first observation. *
! *                       IPAR=3 -- makes accumulators clearings and     *
! *                                 segments counters initializations    *
! *                                 needed before consideration the      *
! *                                 first observation.                   *
! *                       IPAR=4 -- cleans all fields of B3DOBJ,         *
! *                                 B1B3DOBJ before the first            *
! *                                 observation. It cleans status of     *
! *                                 allocation of memory. It is assumed  *
! *                                 that dynamic memory has not been     *
! *                                 allocated before. Argument PLACE     *
! *                                 can be omitted in this mode.         *
! * FAST_MODE ( INTEGER*4 ) -- Fast mode switch.                         *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   PLACE  ( RECORD, OPT ) -- Object with data structure for place of  *
! *                           parameters in the list of derivatives.     *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *                                                                      *
! *  ###   31-DEC-96  F__CLR_IND  v4.0  (c)  L. Petrov   05-MAY-99  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  IPAR, FAST_MODE
      INTEGER*8  G, L, S, GA, LA, SA
      INTEGER*4  INT4
      INTEGER*2  INT2_ARG
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      B3DOBJ%MARKER_1 = FAST_MODE
!
      IF ( IPAR .EQ. 0  .OR.  IPAR .EQ. 3 ) THEN
!
! -------- Clear accumulator for derivatives
!
           PLACE%N_GEN   = 0
           PLACE%N_GLO   = 0
           PLACE%N_LOC   = 0
           PLACE%N_SG1   = 0
           PLACE%N_SG2   = 0
      END IF
!
      IF ( IPAR .EQ. 2  .OR.  IPAR .EQ. 3 ) THEN
!
! -------- Initialization of segment's counters
!
           PLACE%CLO_SEG      = 1
           PLACE%ATM_SEG      = 1
           PLACE%EOP_SEG      = 1
           PLACE%CLO_SEG_LAST = 1
           PLACE%ATM_SEG_LAST = 1
           PLACE%EOP_SEG_LAST = 1
           PLACE%PREV_CSG     = 1
           PLACE%LAST_CSG     = 1
           PLACE%CURR_CSG     = 1
           PLACE%STATUS       = F__UND
      END IF
!C
      IF ( IPAR .EQ. 3 ) THEN
           IF ( FAST_MODE .EQ. F__NONE  .OR.  FAST_MODE .EQ. F__PRD   .OR. &
     &          FAST_MODE .EQ. F__B1D                                 )  THEN
!
! ------------- Clrearing some fields in B3DOBJ object which are to be used
! ------------- in non-B3D modes
!
                CALL NOUT_I4 ( M_GPA, B3DOBJ%BLO )
                CALL NOUT_I4 ( M_GPA, B3DOBJ%PL  )
              ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------- Initilize by zero entrire segment of dynamic memory used
! ------------- for accumulatros and internal data structures in B3D mode
!
                CALL NOUT8 ( B3DOBJ%MEM_SIZE, %VAL(B3DOBJ%MEM_ADR) )
              ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------- Initilize by zero entrire segment of dynamic memory used
! ------------- for accumulatros and internal data structures in B1B3D mode
!
                CALL NOUT8 ( B1B3DOBJ%MEM_SIZE, %VAL(B1B3DOBJ%MEM_ADR) )
           END IF
      END IF
!
      IF ( IPAR .EQ. 1  .AND.  FAST_MODE .EQ. F__B3D ) THEN
!
! -------- Clear accumulators for blocks of normal matrix in the B3D case
!
           G = B3DOBJ%N_GLO
           S = B3DOBJ%SB
           GA = (INT8(G)*INT8(G+1))/2
           SA = (S*(S+1))/2
!
           CALL NOUT8 ( 8*GA,  %VAL(B3DOBJ%AD_N00) )
           CALL NOUT8 ( 8*G,   %VAL(B3DOBJ%AD_VG0) )
!
           CALL NOUT8 ( 8*S*G, %VAL(B3DOBJ%AD_N10) )
           CALL NOUT8 ( 8*SA,  %VAL(B3DOBJ%AD_N11) )
           CALL NOUT8 ( 8*S*S, %VAL(B3DOBJ%AD_N12) )
           CALL NOUT8 ( 8*S,   %VAL(B3DOBJ%AD_VS1) )
!
           CALL NOUT8 ( 8*S*G, %VAL(B3DOBJ%AD_N20) )
           CALL NOUT8 ( 8*S*S, %VAL(B3DOBJ%AD_N21) )
           CALL NOUT8 ( 8*SA,  %VAL(B3DOBJ%AD_N22) )
           CALL NOUT8 ( 8*S,   %VAL(B3DOBJ%AD_VS2) )
      END IF
!
      IF ( IPAR .EQ. 1  .AND.  FAST_MODE .EQ. F__B1B3D ) THEN
!
! -------- Clear accumulators for blocks of normal matrix in the B1B3D case
!
           G = B3DOBJ%N_GLO
           L = B3DOBJ%N_LOC
           S = B3DOBJ%SB
           GA = (INT8(G)*INT8(G+1))/2
           LA = (L*(L+1))/2
           SA = (S*(S+1))/2
!
           CALL NOUT8 ( 8*GA,  %VAL(B1B3DOBJ%AD_NGG) )
           CALL NOUT8 ( 8*G,   %VAL(B1B3DOBJ%AD_VG ) )
!
           CALL NOUT8 ( 8*L*G, %VAL(B1B3DOBJ%AD_NLG) )
           CALL NOUT8 ( 8*LA,  %VAL(B1B3DOBJ%AD_NLL) )
           CALL NOUT8 ( 8*L,   %VAL(B1B3DOBJ%AD_VL ) )
!
           CALL NOUT8 ( 8*S*G, %VAL(B1B3DOBJ%AD_NS1G)  )
           CALL NOUT8 ( 8*S*L, %VAL(B1B3DOBJ%AD_NS1L)  )
           CALL NOUT8 ( 8*SA,  %VAL(B1B3DOBJ%AD_NS1S1) )
           CALL NOUT8 ( 8*S*S, %VAL(B1B3DOBJ%AD_NS1S2) )
           CALL NOUT8 ( 8*S,   %VAL(B1B3DOBJ%AD_VS1)   )
!
           CALL NOUT8 ( 8*S*G, %VAL(B1B3DOBJ%AD_NS2G)  )
           CALL NOUT8 ( 8*S*L, %VAL(B1B3DOBJ%AD_NS2L)  )
           CALL NOUT8 ( 8*SA,  %VAL(B1B3DOBJ%AD_NS2S2) )
           CALL NOUT8 ( 8*S*S, %VAL(B1B3DOBJ%AD_NS2S1) )
           CALL NOUT8 ( 8*S,   %VAL(B1B3DOBJ%AD_VS2)   )
      END IF
!
      IF ( IPAR .EQ. 4 ) THEN
!
! -------- Zeroing entirely B3DOBJ and B1B3DOBJ
!
           CALL NOUT8 ( LOC(B3DOBJ%LAST_FIELD) - LOC(B3DOBJ%FIRST_FIELD) + SIZEOF(B3DOBJ%LAST_FIELD), &
     &                  B3DOBJ )
           CALL NOUT8 ( LOC(B1B3DOBJ%LAST_FIELD)  - &
     &                  LOC(B1B3DOBJ%FIRST_FIELD) + SIZEOF(B3DOBJ%LAST_FIELD), B1B3DOBJ )
!
! -------- Set status: memory is not allocated
!
           B3DOBJ%MEM_STAT   = F__MFR
           B1B3DOBJ%MEM_STAT = F__MFR
      END IF
!
      RETURN
      END  !#!  F__CLR_IND  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE F__PUT_IND ( NC, ITYP, EQU, PLACE, B3DOBJ )
! ************************************************************************
! *                                                                      *
! *   Routine   F__PUT_IND  add index of the next derivative to data     *
! *   structure  PLACE.                                                  *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *       NC ( INTEGER*4 ) -- Index the derivative in the list of        *
! *                           parameters.                                *
! *     ITYP ( INTEGER*4 ) -- Type of the parameter for this derivative: *
! *                        ITYP = F__GLO -- global parameter.            *
! *                        ITYP = F__CUS -- segmented parameter for the  *
! *                                         current segment.             *
! *                        ITYP = F__NES -- segmented parameter for the  *
! *                                         next segment.                *
! *      EQU ( REAL*8    ) -- The array of thte derivative for current   *
! *                           observations. The order of parameters is   *
! *                           the order for full case.                   *
! *   PLACE  ( RECORD    ) -- Object with data structure for place of    *
! *                           parameters in the list of derivatives.     *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! *   Comment: 1) names F__GLO, F__CUS, F__NES  are defined in fast.i    *
! *            2) F__PUT_IND does nothing in the case when               *
! *                        MODE = F__RAT, but PLACE.STATUS .NE. F__RAT   *
! *                                                                      *
! *  ###  31-DEC-1996  F__PUT_IND  v4.1 (c)  L. Petrov  23-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU   ) ::  B3DOBJ
      INTEGER*4  NC
      REAL*8     EQU(M_GPA,2)
      CHARACTER  STR*20, STR1*20
      INTEGER*4  ITYP, INT4
      INTEGER*2  INT2_ARG
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( PLACE%STATUS .NE. F__DEL  .AND.  PLACE%STATUS .NE. F__RAT ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( PLACE%STATUS, STR )
           CALL ERR_LOG ( 9001, -2, 'F__PUT_IND', 'Internal error: '// &
     &         'Parameter PLACE%STATUS has wrong '// &
     &          'value: '//STR(1:I_LEN(STR)) )
           STOP 'Abnormal termination'
      END IF
!
      IF ( NC .GT. B3DOBJ%N_PAR .OR. NC < 1 ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( NC, STR )
           CALL CLRCH ( STR1 )
           CALL INCH  ( B3DOBJ%N_PAR, STR1 )
           CALL ERR_LOG ( 9002, -2, 'F__PUT_IND', 'A parameter with index '// &
     &          STR(1:I_LEN(STR))//' requested. It is either negative of '// &
     &         'exceeds the total number of parameters: '// &
     &          STR1(1:I_LEN(STR1)) )
           nc = nc - 1111111111
           write ( 6, * ) equ(nc,1)
           write ( 6, * ) 'oj' ! %%%
           CALL EXIT ( 1 )
      END IF
!
      IF ( B3DOBJ%BLO( NC ) .EQ. 0 ) THEN
!
! -------- Global parameter
!
           PLACE%N_GLO   = PLACE%N_GLO + 1
           PLACE%EQU_GLO ( PLACE%N_GLO ) = EQU(NC,1)
           PLACE%RAT_GLO ( PLACE%N_GLO ) = EQU(NC,2)
           PLACE%IND_GLO ( PLACE%N_GLO ) = B3DOBJ%PL(NC)
        ELSE IF ( B3DOBJ%BLO( NC ) .EQ. -1 ) THEN
!
! -------- Local parameter
!
           PLACE%N_LOC   = PLACE%N_LOC + 1
           PLACE%EQU_LOC ( PLACE%N_LOC ) = EQU(NC,1)
           PLACE%RAT_LOC ( PLACE%N_LOC ) = EQU(NC,2)
           PLACE%IND_LOC ( PLACE%N_LOC ) = B3DOBJ%PL(NC)
        ELSE IF ( ( ITYP .EQ. F__CUS  .AND.        B3DOBJ%CURR(NC) ) .OR. &
     &            ( ITYP .EQ. F__NES  .AND.  .NOT. B3DOBJ%NEXT(NC) ) )  THEN
!
! -------- Segmented parameter which should be put for the current segment
!
           PLACE%N_SG1   = PLACE%N_SG1 + 1
           IF ( PLACE%N_SG1 .GT. MAX_PSG ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( MAX_PSG, STR )
                CALL ERR_LOG ( 9003, -2, 'F__PUT_IND', 'The number of '// &
     &             'segmented parameters in one segment is too large. '// &
     &             'Parmeter MAX_PSG from fast.i appeared to be '// &
     &             'insufficient: MAX_PSG = '//STR(1:I_LEN(STR))// &
     &             ' Fatal error during processing database '// &
     &              B3DOBJ%DBNAME_MES )
                STOP 'Abnormal termination'
           END IF
           PLACE%EQU_SG1 ( PLACE%N_SG1 ) = EQU(NC,1)
           PLACE%RAT_SG1 ( PLACE%N_SG1 ) = EQU(NC,2)
           PLACE%IND_SG1 ( PLACE%N_SG1 ) = B3DOBJ%PL(NC)
        ELSE IF ( ( ITYP .EQ. F__NES  .AND.        B3DOBJ%NEXT(NC) ) .OR. &
     &            ( ITYP .EQ. F__CUS  .AND.  .NOT. B3DOBJ%CURR(NC) ) )  THEN
!
! -------- Segmented parameter which should be put for the next segment
!
           PLACE%N_SG2   = PLACE%N_SG2 + 1
           IF ( PLACE%N_SG2 .GT. MAX_PSG ) THEN
                CALL CLRCH ( STR )
                CALL INCH ( MAX_PSG, STR )
                CALL ERR_LOG ( 9004, -2, 'F__PUT_IND', 'The number of '// &
     &             'segmented parameters in one segment is too large. '// &
     &             'Parmeter MAX_PSG from fast.i appeared to be '// &
     &             'insufficient: MAX_PSG = '//STR(1:I_LEN(STR))// &
     &             ' Fatal error during processing database '// &
     &              B3DOBJ%DBNAME_MES )
                STOP 'Abnormal termination'
           END IF
           PLACE%EQU_SG2 ( PLACE%N_SG2 ) = EQU(NC,1)
           PLACE%RAT_SG2 ( PLACE%N_SG2 ) = EQU(NC,2)
           PLACE%IND_SG2 ( PLACE%N_SG2 ) = B3DOBJ%PL(NC)
        ELSE
           CALL CLRCH ( STR )
           CALL INCH  ( ITYP, STR )
           IF ( ITYP .EQ. F__GLO ) STR = 'F__GLO'
           IF ( ITYP .EQ. F__CUS ) STR = 'F__CUS'
           IF ( ITYP .EQ. F__NES ) STR = 'F__NES'
           CALL ERR_LOG ( 9005, -2, 'F__PUT_IND', 'Internal error. Wrong '// &
     &         'ITYP: '//STR(1:I_LEN(STR))//'. It is result of some '// &
     &         'inconsistency between PARTL, GET_NAMES and MAP_PARAM. '// &
     &         'What''s to do? Send verbose bug report to Leonid Petrov '// &
     &         'Leonid.Petrov@gsfc.nasa.gov . Try setenv UNF_DISABLE "CAEB". '// &
     &         'Try change parametrization. Maybe it''ll help.       |  '// &
     &         'Fatal error during processing database '//B3DOBJ%DBNAME_MES )
            WRITE ( 6, * ) ' nc=',nc
            WRITE ( 6, * ) ' b3dobj%blo( nc ) =',b3dobj%blo( nc )
            WRITE ( 6, * ) ' b3dobj%pl( nc ) =',b3dobj%pl( nc )
            WRITE ( 6, * ) ' b3dobj%next(nc) =',b3dobj%next(nc) , &
     &             ' b3dobj%curr(nc) =',b3dobj%curr(nc)
            WRITE ( 6, * ) ' loc1 =', loc ( b3dobj%blo ), &
     &             ' loc2 = ',loc ( b3dobj%blo ( nc) )
!@            NC = -1
!@            WRITE ( 6, * ) ' b3dobj%next(nc) =',b3dobj%next(nc) 
            STOP 'F__PUT__IND'
      END IF
!
! --- Putting parameter for the place for all parameters (not divided for
! --- global, local and segmented ones
!
      PLACE%N_GEN   = PLACE%N_GEN + 1
      PLACE%EQU_GEN ( PLACE%N_GEN ) = EQU(NC,1)
      PLACE%RAT_GEN ( PLACE%N_GEN ) = EQU(NC,2)
      PLACE%IND_GEN ( PLACE%N_GEN ) = NC
!
      RETURN
      END  !#!  F__PUT_IND  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   F__NEXT_COMSEG ( PLACE, B3DOBJ )
! ************************************************************************
! *                                                                      *
! *   Logical function  F__NEXT_COMSEG is true when current observation  *
! *   is the first observation for the next common segment.              *
! *   If  F__NEXT_COMSEG  fields B3DOBJ%CURR_CSG and B3DOBJ%LAST_CSG     *
! *   will be updated. Fields PLACE.CLO_SEG_LAST PLACE.ATM_SEG_LAST are  *
! *   being updated in any case.                                         *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   PLACE  ( RECORD    ) -- Object with data structure for place of    *
! *                           parameters in the list of derivatives.     *
! *                                                                      *
! *  ###  08-JAN-97  F__NEXT_COMSEG  v3.1 (c) L. Petrov  11-JUL-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      LOGICAL*4  F__NEXT_COMSEG
      INTEGER*4  NEW_CSG
!
      F__NEXT_COMSEG = .FALSE.
!
! --- Test:  Did we reach the boundary of the common segment
!
      IF ( B3DOBJ%K_CLO .GT. 0 ) THEN
           IF ( (PLACE%CLO_SEG-1)/B3DOBJ%K_CLO + 1 .GT. PLACE%CURR_CSG ) THEN
                 F__NEXT_COMSEG = .TRUE.
                 NEW_CSG = (PLACE%CLO_SEG-1)/B3DOBJ%K_CLO + 1
           END IF
      END IF
      IF ( B3DOBJ%K_ATM .GT. 0 ) THEN
           IF ( (PLACE%ATM_SEG-1)/B3DOBJ%K_ATM + 1 .GT. PLACE%CURR_CSG ) THEN
                 F__NEXT_COMSEG = .TRUE.
                 NEW_CSG = (PLACE%ATM_SEG-1)/B3DOBJ%K_ATM + 1
           END IF
      END IF
      IF ( B3DOBJ%K_EOP .GT. 0 ) THEN
           IF ( (PLACE%EOP_SEG-1)/B3DOBJ%K_EOP + 1 .GT. PLACE%CURR_CSG ) THEN
                 F__NEXT_COMSEG = .TRUE.
                 NEW_CSG = (PLACE%EOP_SEG-1)/B3DOBJ%K_EOP + 1
           END IF
      END IF
!
      IF ( F__NEXT_COMSEG ) THEN
           PLACE%PREV_CSG = PLACE%LAST_CSG
           PLACE%LAST_CSG = PLACE%CURR_CSG
           PLACE%CURR_CSG = NEW_CSG
      END IF
!
! --- Test of "imposible events"
!
      IF ( B3DOBJ%K_CLO .GT.0  .AND.  PLACE%CLO_SEG .LT. PLACE%CLO_SEG_LAST ) THEN
           WRITE ( 6, * ) ' PLACE%CLO_SEG =',PLACE%CLO_SEG, &
     &            ' PLACE%CLO_SEG_LAST=',PLACE%CLO_SEG_LAST
           CALL ERR_LOG ( 8701, -2, 'F__NEXT_COMSEG', 'Internal error: '// &
     &         'Wrong order of clock segments. Send bug '// &
     &         'report to Leonid.Petrov@gsfc.nasa.gov ' )
           STOP 'PROC: abnormal termination'
      END IF
!
      IF ( B3DOBJ%K_ATM .GT.0  .AND.  PLACE%ATM_SEG .LT. PLACE%ATM_SEG_LAST ) THEN
           WRITE ( 6, * ) ' PLACE%ATM_SEG =',PLACE%ATM_SEG, &
     &            ' PLACE%ATM_SEG_LAST=',PLACE%ATM_SEG_LAST
           CALL ERR_LOG ( 8702, -2, 'F__NEXT_COMSEG', 'Internal error: '// &
     &         'Wrong order of atmosphere segments. Send bug '// &
     &         'report to Leonid.Petrov@gsfc.nasa.gov ' )
           STOP 'PROC: abnormal termination'
      END IF
!
      IF ( B3DOBJ%K_EOP .GT.0  .AND.  PLACE%EOP_SEG .LT. PLACE%EOP_SEG_LAST ) THEN
           WRITE ( 6, * ) ' PLACE%EOP_SEG =',PLACE%EOP_SEG, &
     &            ' PLACE%EOP_SEG_LAST=',PLACE%EOP_SEG_LAST
           CALL ERR_LOG ( 8703, -2, 'F__NEXT_COMSEG', 'Internal error: '// &
     &         'Wrong order of EOP segments. Send bug '// &
     &         'report to Leonid.Petrov@gsfc.nasa.gov ' )
           STOP 'PROC: abnormal termination'
      END IF
!
! --- Update "last" segments
!
      PLACE%CLO_SEG_LAST = PLACE%CLO_SEG
      PLACE%ATM_SEG_LAST = PLACE%ATM_SEG
      PLACE%EOP_SEG_LAST = PLACE%EOP_SEG
!
      RETURN
      END  !#!  F__NEXT_COMSEG  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE B3D_GETMEM ( FAST_COV, B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  B3D_GETMEM  allocates dynamic memory for internal fields  *
! *   of the object  B3DOBJ.                                             *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * FAST_COV ( INTEGER*4 ) -- Mode of calculation of covariance matrix.  *
! *                           If FAST_COV = F__FUL then memory for       *
! *                           keeping off-diagonal terms of the          *
! *                           covariance matrix and temporary Q vector   *
! *                           ( for keeping Q*a during solution update ) *
! *                           is initialized.                            *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  31-DEC-96   B3D_GETMEM   v2.2  (c)  L. Petrov  22-SEP-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  FAST_COV, IUER, J0, J1, J2, J3, J4, J5, J6, J7, I_LEN
      INTEGER*8  ISH, G, SB, SX
      CHARACTER  STR*20
!
      IF ( B3DOBJ%MEM_STAT .NE. F__MFR ) THEN
           CALL ERR_LOG ( 6201, IUER, 'B3D_GETMEM', 'B3DOBJ has not '// &
     &         'been initialized' )
           RETURN
      END IF
!
! --- Antiinitalization
!
      B3DOBJ%AD_B0 = -1
      B3DOBJ%AD_Z0 = -1
      B3DOBJ%AD_E0 = -1
      B3DOBJ%AD_U0 = -1
      B3DOBJ%AD_Q0 = -1
!
      DO 400 J0 = 1,MAX_SEG
         B3DOBJ%AD_B (J0) = -1
         B3DOBJ%AD_C (J0) = -1
         B3DOBJ%AD_D (J0) = -1
         B3DOBJ%AD_ZS(J0) = -1
         B3DOBJ%AD_ES(J0) = -1
         B3DOBJ%AD_US(J0) = -1
         B3DOBJ%AD_QS(J0) = -1
 400  CONTINUE
!
! --- Calculation the size of memory and belonging to element B3DOBJ shifts
! --- with respect to the beginning of the requested dynamic memory
!
      G  = B3DOBJ%N_GLO
      SB = B3DOBJ%SB
      SX = B3DOBJ%SX
      ISH = 0
      B3DOBJ%AD_B0  = ISH
         ISH = ISH + 8*(G*(G+1))/2
      B3DOBJ%AD_Z0  = ISH
         ISH = ISH + 8*G
      B3DOBJ%AD_E0  = ISH
         ISH = ISH + 8*G
      B3DOBJ%AD_U0  = ISH
         ISH = ISH + 8*G
!
      DO 410 J1=1,B3DOBJ%NBS -1
         B3DOBJ%AD_B(J1) = ISH
            ISH = ISH + 8*SB*G
 410  CONTINUE
      B3DOBJ%AD_BX = ISH
         ISH = ISH + 8*SX*G
!
      DO 420 J2=1,B3DOBJ%NBS -1
         B3DOBJ%AD_C(J2) = ISH
            ISH = ISH + 8*(SB*(SB+1))/2
 420  CONTINUE
      B3DOBJ%AD_CX = ISH
         ISH = ISH + 8*(SX*(SX+1))/2
!
      DO 430 J3=1,B3DOBJ%NBS - 1
         B3DOBJ%AD_D(J3) = ISH
            ISH = ISH + 8*SB*SB
 430  CONTINUE
      B3DOBJ%AD_DX = ISH
         ISH = ISH + 8*SX*SB
!
      DO 440 J4=1,B3DOBJ%NBS - 1
         B3DOBJ%AD_ZS(J4) = ISH
            ISH = ISH + 8*SB
 440  CONTINUE
      B3DOBJ%AD_ZSX = ISH
         ISH = ISH + 8*SX
!
      DO 450 J5=1,B3DOBJ%NBS - 1
         B3DOBJ%AD_ES(J5) = ISH
            ISH = ISH + 8*SB
 450  CONTINUE
      B3DOBJ%AD_ESX = ISH
         ISH = ISH + 8*SX
!
      DO 460 J6=1,B3DOBJ%NBS - 1
         B3DOBJ%AD_US(J6) = ISH
            ISH = ISH + 8*SB
 460  CONTINUE
      B3DOBJ%AD_USX = ISH
         ISH = ISH + 8*SX
      B3DOBJ%MEM_SIZE2 = ISH  !  Size of memory to be written on disk
!
      IF ( FAST_COV .EQ. F__FUL ) THEN
           B3DOBJ%AD_Q0 = ISH
           ISH = ISH + 8*G
!
           DO 470 J7=1,B3DOBJ%NBS - 1
              B3DOBJ%AD_QS(J7) = ISH
                 ISH = ISH + 8*SB
 470       CONTINUE
           B3DOBJ%AD_QSX = ISH
               ISH = ISH + 8*SB  ! should be SX , but SB>=SX )
           B3DOBJ%AD_CVF = ISH
               ISH = ISH + 8*SB*SB*((B3DOBJ%NBS-1)*(B3DOBJ%NBS-2))/2
      END IF
!
      B3DOBJ%AD_N00 = ISH
         ISH = ISH + 8*(G*(G+1))/2
      B3DOBJ%AD_N10 = ISH
         ISH = ISH + 8*SB*G
      B3DOBJ%AD_N11 = ISH
         ISH = ISH + 8*(SB*(SB+1))/2
      B3DOBJ%AD_N20 = ISH
         ISH = ISH + 8*SB*G
      B3DOBJ%AD_N21 = ISH
         ISH = ISH + 8*SB*SB
      B3DOBJ%AD_N22 = ISH
         ISH = ISH + 8*(SB*(SB+1))/2
      B3DOBJ%AD_N12 = ISH
         ISH = ISH + 8*SB*SB
      B3DOBJ%AD_VG0 = ISH
         ISH = ISH + 8*G
      B3DOBJ%AD_VS1 = ISH
         ISH = ISH + 8*SB
      B3DOBJ%AD_VS2 = ISH
         ISH = ISH + 8*SB
!
! --- Properly getting dynamic memory
!
      B3DOBJ%MEM_SIZE = ISH
      CALL GET_MEM ( B3DOBJ%MEM_SIZE, B3DOBJ%MEM_ADR )
      IF ( B3DOBJ%MEM_ADR .EQ. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( B3DOBJ%MEM_SIZE, STR )
           CALL ERR_LOG ( 6202, IUER, 'B3D_GETMEM', 'Error '// &
     &         'during attempt to get '//STR(1:I_LEN(STR))// &
     &         ' bytes dynamic memory' )
           RETURN
      END IF
!
! --- Belonging to actual addresses
!
      B3DOBJ%AD_B0 = B3DOBJ%AD_B0 + B3DOBJ%MEM_ADR
      B3DOBJ%AD_Z0 = B3DOBJ%AD_Z0 + B3DOBJ%MEM_ADR
      B3DOBJ%AD_E0 = B3DOBJ%AD_E0 + B3DOBJ%MEM_ADR
      B3DOBJ%AD_U0 = B3DOBJ%AD_U0 + B3DOBJ%MEM_ADR
!
      DO 510 J1=1,B3DOBJ%NBS -1
         B3DOBJ%AD_B(J1) = B3DOBJ%AD_B(J1) + B3DOBJ%MEM_ADR
 510  CONTINUE
      B3DOBJ%AD_BX  = B3DOBJ%AD_BX  + B3DOBJ%MEM_ADR
!
      DO 520 J2=1,B3DOBJ%NBS -1
         B3DOBJ%AD_C(J2) = B3DOBJ%AD_C(J2) + B3DOBJ%MEM_ADR
 520  CONTINUE
      B3DOBJ%AD_CX  = B3DOBJ%AD_CX  + B3DOBJ%MEM_ADR
!
      DO 530 J3=1,B3DOBJ%NBS -1
         B3DOBJ%AD_D(J3) = B3DOBJ%AD_D(J3) + B3DOBJ%MEM_ADR
 530  CONTINUE
      B3DOBJ%AD_DX  = B3DOBJ%AD_DX  + B3DOBJ%MEM_ADR
!
      DO 540 J4=1,B3DOBJ%NBS -1
         B3DOBJ%AD_ZS(J4)= B3DOBJ%AD_ZS(J4)+ B3DOBJ%MEM_ADR
 540  CONTINUE
      B3DOBJ%AD_ZSX = B3DOBJ%AD_ZSX + B3DOBJ%MEM_ADR
!
      DO 550 J5=1,B3DOBJ%NBS -1
         B3DOBJ%AD_ES(J5)= B3DOBJ%AD_ES(J5)+ B3DOBJ%MEM_ADR
 550  CONTINUE
      B3DOBJ%AD_ESX = B3DOBJ%AD_ESX + B3DOBJ%MEM_ADR
!
      DO 560 J6=1,B3DOBJ%NBS -1
         B3DOBJ%AD_US(J6)= B3DOBJ%AD_US(J6)+ B3DOBJ%MEM_ADR
 560  CONTINUE
      B3DOBJ%AD_USX = B3DOBJ%AD_USX + B3DOBJ%MEM_ADR
!
      IF ( FAST_COV .EQ. F__FUL ) THEN
           B3DOBJ%AD_Q0 = B3DOBJ%AD_Q0 + B3DOBJ%MEM_ADR
         ELSE
           B3DOBJ%AD_Q0 = -1
      END IF
!
      DO 570 J7=1,B3DOBJ%NBS - 1
         IF ( FAST_COV .EQ. F__FUL ) THEN
              B3DOBJ%AD_QS(J7) = B3DOBJ%AD_QS(J7) + B3DOBJ%MEM_ADR
           ELSE
              B3DOBJ%AD_QS(J7) = -1
         END IF
 570  CONTINUE
      IF ( FAST_COV .EQ. F__FUL ) THEN
           B3DOBJ%AD_QSX = B3DOBJ%AD_QSX + B3DOBJ%MEM_ADR
         ELSE
           B3DOBJ%AD_QSX = -1
      END IF
      IF ( FAST_COV .EQ. F__FUL ) THEN
           B3DOBJ%AD_CVF = B3DOBJ%AD_CVF + B3DOBJ%MEM_ADR
         ELSE
           B3DOBJ%AD_CVF = -1
      END IF
!
      B3DOBJ%AD_N00  = B3DOBJ%AD_N00  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_N10  = B3DOBJ%AD_N10  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_N11  = B3DOBJ%AD_N11  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_N20  = B3DOBJ%AD_N20  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_N21  = B3DOBJ%AD_N21  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_N22  = B3DOBJ%AD_N22  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_N12  = B3DOBJ%AD_N12  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_VG0  = B3DOBJ%AD_VG0  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_VS1  = B3DOBJ%AD_VS1  + B3DOBJ%MEM_ADR
      B3DOBJ%AD_VS2  = B3DOBJ%AD_VS2  + B3DOBJ%MEM_ADR
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        type *,'B3D_GETMEM:   b3dobj%mem_size = ',b3dobj%mem_size   ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF ( FAST_COV .EQ. F__FUL ) THEN
           B3DOBJ%MEM_STAT = F__MFL
         ELSE
           B3DOBJ%MEM_STAT = F__MSL
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B3D_GETMEM  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE B3D_FREEMEM ( B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  B3D_FREEMEM  frees dynamic memory for internal field      *
! *   of the object  B3DOBJ  allocated earlier by B3D_GETMEM.            *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  31-DEC-96   B3D_FREEMEM  v1.3  (c)  L. Petrov  30-DEC-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  IUER, J1, J2, J3, J4, J5, J6, J7
!
      IF ( B3DOBJ%MEM_STAT .NE. F__MSL  .AND.  B3DOBJ%MEM_STAT .NE. F__MFL) THEN
           CALL ERR_LOG ( 6211, IUER, 'B3D_FREEMEM', 'Dynamic memory for '// &
     &         'B3DOBJ has not been allocated earlier' )
           RETURN
      END IF
!
! --- Actual retriving memory pool
!
      CALL FREE_MEM ( B3DOBJ%MEM_ADR )
!
! --- Diasabling addresses. It is being done to prevent acidental using
! --- memory used previous addresses. Errors occured with applying to
! --- addresses -1 are less hard then applying to addresses of memory
! --- having been freed and allocated for another purposes...
!
      B3DOBJ%AD_B0 = -1
      B3DOBJ%AD_Z0 = -1
      B3DOBJ%AD_E0 = -1
      B3DOBJ%AD_U0 = -1
      B3DOBJ%AD_Q0 = -1
!
      DO 510 J1=1,B3DOBJ%NBS
         B3DOBJ%AD_B(J1) = -1
 510  CONTINUE
      DO 520 J2=1,B3DOBJ%NBS
         B3DOBJ%AD_C(J2) = -1
 520  CONTINUE
      DO 530 J3=1,B3DOBJ%NBS
         B3DOBJ%AD_D(J3) = -1
 530  CONTINUE
      DO 540 J4=1,B3DOBJ%NBS
         B3DOBJ%AD_ZS(J4)= -1
 540  CONTINUE
      DO 550 J5=1,B3DOBJ%NBS
         B3DOBJ%AD_ES(J5)= -1
 550  CONTINUE
      DO 560 J6=1,B3DOBJ%NBS
         B3DOBJ%AD_US(J6)= -1
 560  CONTINUE
      DO 570 J7=1,B3DOBJ%NBS
         B3DOBJ%AD_QS(J7)= -1
 570  CONTINUE
!
      B3DOBJ%AD_N00  = -1
      B3DOBJ%AD_N10  = -1
      B3DOBJ%AD_N11  = -1
      B3DOBJ%AD_N12  = -1
      B3DOBJ%AD_N20  = -1
      B3DOBJ%AD_N21  = -1
      B3DOBJ%AD_N22  = -1
      B3DOBJ%AD_VG0  = -1
      B3DOBJ%AD_VS1  = -1
      B3DOBJ%AD_VS2  = -1
!
      B3DOBJ%AD_ZSX  = -1
      B3DOBJ%AD_ESX  = -1
      B3DOBJ%AD_USX  = -1
      B3DOBJ%AD_QSX  = -1
      B3DOBJ%AD_CVF  = -1
!
      B3DOBJ%MEM_SIZE  = 0
      B3DOBJ%MEM_SIZE2 = 0
      B3DOBJ%MEM_STAT  = F__MFR
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  B3D_FREEMEM  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE WRNOR_B3D ( FINAM, B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  WRNOR_B3D  open file FINAM, writes down on disk two       *
! *   records 1) object B3DOBJ, 2) content of dynamic memory allocated   *
! *   in object  B3DOBJ and after that closes the file. This routine     *
! *   intended for transferring object B3DOBJ from the executable to     *
! *   executable.                                                        *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    FINAM ( CHARACTER ) -- File name where B3DOBJ will be written.    *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###   09-JAN-97   WRNOR_B3D   v2.2  (c)  L. Petrov  15-NOV-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      CHARACTER  FINAM*(*)
      INTEGER*8  LENR 
      INTEGER*4  IUER, LUN, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
! --- Opening file for writing binary data
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'NEW', LUN, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6301, IUER, 'WRNOR_B3D', 'Error during '// &
     &       'opening output file '//FINAM(1:I_LEN(FINAM))// &
     &       ' for writing normal equations for B3D algorithm' )
         RETURN
      END IF
!
! --- Putting the indentifier of the current version into B3D record
!
      B3DOBJ%IDENT = IDENT_B3D
!
! --- Calculation length (in bytes) of fields to be written in first record
!
      LENR = LOC(B3DOBJ%MARKER_1) - LOC(B3DOBJ%FIRST_FIELD) + SIZEOF(B3DOBJ%FIRST_FIELD)
!
! --- Writing the object first B3DOBJ in file
!
      CALL ERR_PASS ( IUER, IER )
      CALL WRBIN_RECORD ( LUN, INT(LENR,KIND=4), B3DOBJ, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6302, IUER, 'WRNOR_B3D', 'Error during '// &
     &       'writing the first record in output file '// &
     &        FINAM(1:I_LEN(FINAM))//' for normal equations of B3D '// &
     &       'algorithm' )
         RETURN
      END IF
!
! --- Writing various fields B3DOBJ
!
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%BLO,          IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%PL,           IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%CURR,         IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%NEXT,         IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_GLO,  B3DOBJ%INF_GLO,      IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%N_LOC,  B3DOBJ%INF_LOC,      IER )
      CALL WRBIN_RECORD ( LUN, 4*MAX_PSG*B3DOBJ%NBS, B3DOBJ%INF_SGM, IER )
      CALL WRBIN_RECORD ( LUN, 4*B3DOBJ%SX,     B3DOBJ%INF_SGX,      IER )
      CALL WRBIN_RECORD ( LUN, 8*B3DOBJ%NOBS_T, B3DOBJ%DT,           IER )
      CALL WRBIN_RECORD ( LUN, 8*B3DOBJ%NOBS_T, B3DOBJ%RT,           IER )
!
      IF ( B3DOBJ%MEM_STAT .EQ. F__MSL  .OR.  B3DOBJ%MEM_STAT .EQ. F__MFL ) THEN
!
! ------ Writing the content of the dynamic memory allocated in various fields
! ------ held by B3DOBJ object
!
         IF ( B3DOBJ%MEM_SIZE2 > INT8(2)*INT8(1024)**3 ) THEN
              CALL ERR_LOG ( 6309, IUER, 'WRNOR_B3D', 'Trap of internal '// &
     &            'control: an attempt to write more than 2Gb data chunk. '// &
     &            ' time came to develop routine WRBIN_RECORD64' )
              RETURN
         END IF
         CALL WRBIN_RECORD ( LUN, INT(B3DOBJ%MEM_SIZE2,KIND=4), %VAL(B3DOBJ%MEM_ADR), &
     &                       IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6303, IUER, 'WRNOR_B3D', 'Error during '// &
     &            'writing the last record in output file '// &
     &             FINAM(1:I_LEN(FINAM))//' for normal equations of B3D '// &
     &            'algorithm' )
              RETURN
         END IF
      END IF
!
! --- Closing the files
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6304, IUER, 'WRNOR_B3D', 'Error during '// &
     &         'closing output file '//FINAM(1:I_LEN(FINAM))// &
     &         ' for writing normal equations for B3D algorithm' )
         RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  WRNOR_B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE RDNOR_B3D ( FINAM, B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  RDNOR_B3D  open file FINAM, read from disk two records    *
! *   1) object B3DOBJ, 2) and content of dynamic memory allocated       *
! *   in object  B3DOBJ during previous work, gets dynamic memory in     *
! *   object  B3DOBJ, puts there the last record and after that closes   *
! *   the file. This routine intended for transferring object B3DOBJ     *
! *   from the executable to executable.                                 *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    FINAM ( CHARACTER ) -- File name where B3DOBJ will be written.    *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *     IUER ( INTEGER*4 ) -- Universal error habdler.                   *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###   09-JAN-97   RDNOR_B3D   v2.0  (c)  L. Petrov  04-MAR-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      CHARACTER  FINAM*(*)
      INTEGER*8  LENR, LEN_B3DOBJ
      INTEGER*4  IUER, LUN, NB, NBYTES_READ, IER, I_LEN
!
! --- Test: was dynamic memory allocated?
!
      IF ( B3DOBJ%MEM_STAT .EQ. F__MSL  .OR.  B3DOBJ%MEM_STAT .EQ. F__MFL ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL B3D_FREEMEM ( B3DOBJ, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6311, IUER, 'RDNOR_B3D', 'Error during '// &
     &              'freeing dynamic memory before reading' )
                RETURN
           END IF
      END IF
!
! --- Opening file for reading binary data
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6312, IUER, 'RDNOR_B3D', 'Error during '// &
     &         'opening input file '//FINAM(1:I_LEN(FINAM))// &
     &         ' with normal equations of B3D algorithm' )
           RETURN
      END IF
!
! --- Calculation length (in bytes) of whole object B3DOBJ
!
      LEN_B3DOBJ = LOC(B3DOBJ%LAST_FIELD) - LOC(B3DOBJ%FIRST_FIELD) + SIZEOF(B3DOBJ%FIRST_FIELD)
!
! --- Zeroing B3DOBJ
!
      CALL NOUT8  ( LEN_B3DOBJ, B3DOBJ )
!
! --- Calculation length (in bytes) of fields to be read in first record
!
      LENR = LOC(B3DOBJ%MARKER_1) - LOC(B3DOBJ%FIRST_FIELD) + SIZEOF(B3DOBJ%FIRST_FIELD)
!
      CALL ERR_PASS ( IUER, IER )
      CALL RDBIN_RECORD ( LUN, LENR, B3DOBJ, NBYTES_READ, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6313, IUER, 'RDNOR_B3D', 'Error during '// &
     &       'reading the first record of input file '// &
     &        FINAM(1:I_LEN(FINAM))//' for normal equations of B3D '// &
     &       'algorithm' )
         RETURN
      END IF
!
! --- Test: Does the length coincide?
!
      IF ( LENR .NE. NBYTES_READ ) THEN
           WRITE ( 6, * ) '        LENR =', LENR, &
     &            ' NBYTES_READ =', NBYTES_READ
           CALL ERR_LOG ( 6314, IUER, 'RDNOR_B3D', 'Internal error' )
           RETURN
      END IF
!
! --- Test: Does the first letters of the IDENT are the B3D ?
!
      IF ( B3DOBJ%IDENT(1:3) .NE. 'B3D' ) THEN
           CALL ERR_LOG ( 6315, IUER, 'RDNOR_B3D', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' contains wrong context' )
           RETURN
      END IF
!
! --- Test of the coincidence of the IDENT
!
      IF ( B3DOBJ%IDENT .NE. IDENT_B3D ) THEN
           CALL ERR_LOG ( 6316, IUER, 'RDNOR_B3D', 'File '// &
     &          FINAM(1:I_LEN(FINAM))//' contains records with wrong version '// &
     &         'of fast.i: "'//B3DOBJ%IDENT//'" instead of "'//IDENT_B3D//'"' )
           RETURN
      END IF
!C
!
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%BLO,          NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%PL,           NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%CURR,         NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_PAR,  B3DOBJ%NEXT,         NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_GLO,  B3DOBJ%INF_GLO,      NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%N_LOC,  B3DOBJ%INF_LOC,      NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*MAX_PSG*B3DOBJ%NBS, B3DOBJ%INF_SGM, NB, IER )
      CALL RDBIN_RECORD ( LUN, 4*B3DOBJ%SX,     B3DOBJ%INF_SGX,      NB, IER )
      CALL RDBIN_RECORD ( LUN, 8*B3DOBJ%NOBS_T, B3DOBJ%DT,           NB, IER )
      CALL RDBIN_RECORD ( LUN, 8*B3DOBJ%NOBS_T, B3DOBJ%RT,           NB, IER )
!C
      IF ( B3DOBJ%MEM_STAT .EQ. F__MSL  .OR.  B3DOBJ%MEM_STAT .EQ. F__MFL ) THEN
!
! ------ Alolocation of dynamic memory
!
         B3DOBJ%MEM_STAT = F__MFR
         CALL ERR_PASS ( IUER, IER )
         CALL B3D_GETMEM ( F__SEG, B3DOBJ, IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 6317, IUER, 'RDNOR_B3D', 'Error during '// &
     &          'getting dynamic memory for location blocks of normal '// &
     &          'equations' )
            RETURN
         END IF
!
! ------ Reading data to the pool of allocated dynamic memory
!
         CALL ERR_PASS ( IUER, IER )
         CALL RDBIN_RECORD ( LUN, INT(B3DOBJ%MEM_SIZE2,KIND=4), %VAL(B3DOBJ%MEM_ADR), &
     &                       NBYTES_READ, IER )
         IF ( IER .NE. 0 ) THEN
            CALL ERR_LOG ( 6318, IUER, 'RDNOR_B3D', 'Error during '// &
     &          'reading the last record from the input file '// &
     &           FINAM(1:I_LEN(FINAM))//' for normal equations of B3D '// &
     &          'algorithm' )
            RETURN
         END IF
!
! ------ Test: Were all data read?
!
         IF ( B3DOBJ%MEM_SIZE2 .NE. NBYTES_READ ) THEN
              WRITE ( 6, * ) ' B3DOBJ%MEM_SIZE2 =',B3DOBJ%MEM_SIZE2, &
     &               ' NBYTES_READ      =',NBYTES_READ
            CALL ERR_LOG ( 6319, IUER, 'RDNOR_B3D', 'Internal error' )
            RETURN
          END IF
      END IF
!
! --- Closing input file
!
      CALL BINF_CLOSE ( LUN, IER )
      IF ( IER .NE. 0 ) THEN
         CALL ERR_LOG ( 6320, IUER, 'RDNOR_B3D', 'Error during '// &
     &       'closing input file '//FINAM(1:I_LEN(FINAM))// &
     &       ' for normal equations of B3D algorithm' )
         RETURN
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  RDNOR_B3D  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION   FULL_B3D ( B3DOBJ, IROW, ICOL, TYP_I, NBS_I, &
     &                      IR_I, IC_I )
! ************************************************************************
! *                                                                      *
! *   Function FULL_B3D  returns the address of the element of submatrix *
! *   of normal matrix in B3D parametrization which corresponds to       *
! *   a specified element in full matrix.                                *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! * B3DOBJ ( RECORD    ) -- Object with data structure for B3D extension *
! *                         of SOLVE.                                    *
! *   IROW ( INTEGER*4 ) -- Row in full matris for the specified element *
! *   ICOL ( INTEGER*4 ) -- Column in full matris for the specified      *
! *                         element.                                     *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *  TYP_I ( CHARACTER, OPT  ) -- Type of the submartix. One of "G", "B" *
! *                               "C", "D".                              *
! *  NBS_I ( INTEGER*4, OPT  ) -- Number of block.                       *
! *   IR_I ( INTEGER*4, OPT  ) -- Row in submatrix for corresponding     *
! *                               element.                               *
! *   IC_I ( INTEGER*4, OPT  ) -- Column in submatrix for corresponding  *
! *                               element.                               *
! *                                                                      *
! *   In the case when an element specified which doesn't have           *
! *   correspondence to non-zero element of double-bordered              *
! *   block-threediagonal matrix, FULL_B3D = -1 (and also NBS_I = -1,    *
! *   ( IR_I = -1, IC = -1 ).                                            *
! *                                                                      *
! *  ###   13-Jan-97   FULL_B3D    v2.0  (c)  L. Petrov  22-Jan-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      ADDRESS__TYPE :: FULL_B3D
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  IROW, ICOL, NBS_I, IR_I, IC_I, NBS, IR, IC
      CHARACTER  TYP_I*(*)
      CHARACTER  TYP*1
!
      INTEGER*4  ISWAP, IR_BLO, IC_BLO, IR_PLA, IC_PLA, I, J
      INTEGER*8  LA, N, G, SB, SX
      INTEGER*8  LOCC
      LOCC(I,J) = min(I,J) + (INT8(max(I,J))*INT8(max(I,J)-1))/2
!
      N  = B3DOBJ%NBS
      G  = B3DOBJ%N_GLO
      SB = B3DOBJ%SB
      SX = B3DOBJ%SX
!
! --- Setting default for the case when we won't find correspondence
!
      TYP =  '?'
      NBS =  -1
      IR  =  -1
      IC  =  -1
      FULL_B3D = -1
!
! --- Test of validity of input data
!
      IF ( IROW .LE. 0  .OR.  IROW .GT.  G + (N-1)*SB + SX ) GOTO 810 ! TO End
      IF ( ICOL .LE. 0  .OR.  ICOL .GT.  G + (N-1)*SB + SX ) GOTO 810 ! TO End
!
! --- Extracting number of block and place in the block for parameters for
! --- rows and columns
!
      IR_BLO = B3DOBJ%BLO(IROW)
      IR_PLA = B3DOBJ%PL (IROW)
      IC_BLO = B3DOBJ%BLO(ICOL)
      IC_PLA = B3DOBJ%PL (ICOL)
!
! --- Testing cvarious combinations of IR_BLO, IC_BLO
!
      IF ( IR_BLO .EQ. 0   .AND.   IC_BLO .EQ. 0 ) THEN
!
! -------- Oh! It is, of course, global-global block.
!
           TYP = 'G'
           NBS =  0
           IR  =  IR_PLA
           IC  =  IC_PLA
           LA  =  LOCC( IR, IC ) - 1
           FULL_B3D = B3DOBJ%AD_B0 + 8*LA
      END IF
      IF ( IR_BLO .EQ. 0   .AND.   IC_BLO .GT. 0 ) THEN
!
! -------- Oh! It is, of course, global-segmented block.
!
           TYP = 'B'
           NBS =  IC_BLO
           IR  =  IC_PLA
           IC  =  IR_PLA
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                LA = (IC-1)*SB + IR -1
                FULL_B3D = B3DOBJ%AD_B(NBS) + 8*LA
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                LA = (IC-1)*SX + IR -1
                FULL_B3D = B3DOBJ%AD_BX     + 8*LA
           END IF
      END IF
      IF ( IC_BLO .EQ. 0   .AND.   IR_BLO .GT. 0 ) THEN
!
! -------- Oh! It is, of course, global-segmented block.
!
           TYP = 'B'
           NBS =  IR_BLO
           IR  =  IR_PLA
           IC  =  IC_PLA
           LA = (IC-1)*SB + IR -1
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                LA = (IC-1)*SB + IR -1
                FULL_B3D = B3DOBJ%AD_B(NBS) + 8*LA
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                LA = (IC-1)*SX + IR -1
                FULL_B3D = B3DOBJ%AD_BX     + 8*LA
           END IF
      END IF
      IF ( IR_BLO .GT. 0  .AND.  ( IR_BLO .EQ. IC_BLO ) ) THEN
!
! -------- Oh! It is, of course, diagonal segmented-segmented block.
!
           TYP = 'C'
           NBS =  IR_BLO
           IR  =  IR_PLA
           IC  =  IC_PLA
           LA = LOCC( IR, IC ) -1
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                FULL_B3D = B3DOBJ%AD_C(NBS) + 8*LA
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                FULL_B3D = B3DOBJ%AD_CX     + 8*LA
           END IF
      END IF
      IF ( IR_BLO .GT. 0  .AND.  ( IC_BLO - IR_BLO .EQ.  1 ) ) THEN
!
! -------- Oh! It is, of course, down-diagonal cross segmented-segmented block.
!
           TYP = 'D'
           NBS =  IC_BLO
           IR  =  IC_PLA
           IC  =  IR_PLA
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                LA = (IC-1)*SB + IR -1
                FULL_B3D = B3DOBJ%AD_D(NBS) + 8*LA
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                LA = (IC-1)*SX + IR -1
                FULL_B3D = B3DOBJ%AD_DX     + 8*LA
           END IF
      END IF
      IF ( IC_BLO .GT. 0  .AND.  ( IR_BLO - IC_BLO .EQ.  1 ) ) THEN
!
! -------- Oh! It is, of course, down-diagonal cross segmented-segmented block.
!
           TYP = 'D'
           NBS =  IR_BLO
           IR  =  IR_PLA
           IC  =  IC_PLA
           IF ( NBS .LT. B3DOBJ%NBS ) THEN
                LA = (IC-1)*SB + IR -1
                FULL_B3D = B3DOBJ%AD_D(NBS) + 8*LA
             ELSE IF ( NBS .EQ. B3DOBJ%NBS ) THEN
                LA = (IC-1)*SX + IR -1
                FULL_B3D = B3DOBJ%AD_DX     + 8*LA
           END IF
      END IF
!
      IF ( (TYP .EQ. 'G' .OR. TYP .EQ. 'C')  .AND. (IR .GT. IC) ) THEN
!
! -------- Permutation rows-columns for symmetric matrix
!
           ISWAP = IR
           IR    = IC
           IC    = ISWAP
      END IF
!
 810  CONTINUE
!
! --- Testing: was actual argument were set up, If yes, moving there
! --- output parameters
!
      IF ( LOC(TYP_I) .NE. 0 ) TYP_I = TYP
      IF ( LOC(NBS_I) .NE. 0 ) NBS_I = NBS
      IF ( LOC(IR_I)  .NE. 0 ) IR_I  = IR
      IF ( LOC(IC_I)  .NE. 0 ) IC_I  = IC
!
      RETURN
      END  !#!  FULL_B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EXPAND_B3D ( B3DOBJ, MAT, VEC, DSP, SCL )
! ************************************************************************
! *                                                                      *
! *   Routine  EXPAND_B3D  expand normal submatrices and subvectors      *
! *   enclosed in object B3DOBJ  to full matrix and full vectors. It is  *
! *   assumed that matrix MAT was zeroed earlier. Only those elements of *
! *   MAT which correspond elements B3D are being changed.               *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * _________________________ OUTPUT PARAMETERS: _______________________ *
! *                                                                      *
! *      MAT ( REAL*8    ) -- Full normal matrix. Only 1) global-global  *
! *                           symmetric, 2) band of global-local         *
! *                           parameters 3) main block diagonal band     *
! *                           4) down-diagonal band are filled. Other    *
! *                           elements of matrix MAT remain unchanged.   *
! *      VEC ( REAL*8    ) -- Full vector of estimates (E-vector).       *
! *      DSP ( REAL*8    ) -- Full vector of right parts of normal       *
! *                           equations (Z-vector). It can also contain  *
! *                           variances of the estimates.                *
! *      SCL ( REAL*8    ) -- Full vector of scales of the parameters    *
! *                           (S-vector).                                *
! *                                                                      *
! *  ###  24-JAN-97   EXPAND_B3D   v1.0  (c)  L. Petrov  24-Jan-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE     ( B3D__STRU ) ::  B3DOBJ
      REAL*8     MAT(*), VEC(*), DSP(*), SCL(*)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7
      INTEGER*8  G, GA, SB, SBA, SX, S, NP
      ADDRESS__TYPE :: AD_G, AD_B, AD_C, AD_D, AD_E, AD_Z, &
     &                 AD_U, AD_BS, AD_DS, IND_FULL
      INTEGER*4  I, J
      INTEGER*8  LOCC  
      LOCC(I,J) = min(I,J) + (INT8(max(I,J))*INT8(max(I,J)-1))/2
!
      G   = B3DOBJ%N_GLO
      GA  = (INT8(G)*INT8(G+1))/2
      SB  = B3DOBJ%SB
      SBA = (SB*(SB+1))/2
      SX  = B3DOBJ%SX
      SBA = (SX*(SX+1))/2
!
! --- Moving global parameters
!
      AD_G = B3DOBJ%AD_B0
!
! --- Setting up initital addressos of the vectors
!
      AD_E = B3DOBJ%AD_E0
      AD_Z = B3DOBJ%AD_Z0
      AD_U = B3DOBJ%AD_U0
!
      DO 410 J1=1,G
         DO 420 J2=1,J1
            IND_FULL = LOCC ( B3DOBJ%INF_GLO(J1), B3DOBJ%INF_GLO(J2) )
            CALL MOVE_R8 ( %VAL(AD_G), MAT(IND_FULL) )
            AD_G = AD_G + 8
 420     CONTINUE
!
         IND_FULL = B3DOBJ%INF_GLO(J1)
         CALL MOVE_R8 ( %VAL(AD_E), VEC(IND_FULL) )
         CALL MOVE_R8 ( %VAL(AD_Z), DSP(IND_FULL) )
         CALL MOVE_R8 ( %VAL(AD_U), SCL(IND_FULL) )
!
         AD_E = AD_E + 8
         AD_Z = AD_Z + 8
         AD_U = AD_U + 8
 410  CONTINUE
!
      NP = G  ! Pointer to the index of the last global parameter
!
! --- CYCLE ON BLOCKS OF SEGMENTED PARAMETERS
!     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      DO 430 J3=1,B3DOBJ%NBS
!
! ------ Setting up address of the first elements of vectors and matrices
! ------ associated with local parameters
!
         IF ( J3 .LT. B3DOBJ%NBS ) THEN
              AD_BS= B3DOBJ%AD_B(J3)
              AD_C = B3DOBJ%AD_C(J3)
              AD_DS= B3DOBJ%AD_D(J3)
              AD_E = B3DOBJ%AD_ES(J3)
              AD_Z = B3DOBJ%AD_ZS(J3)
              AD_U = B3DOBJ%AD_US(J3)
              S    = SB
           ELSE IF ( J3 .EQ. B3DOBJ%NBS ) THEN
              AD_BS= B3DOBJ%AD_BX
              AD_C = B3DOBJ%AD_CX
              AD_DS= B3DOBJ%AD_DX
              AD_E = B3DOBJ%AD_ESX
              AD_Z = B3DOBJ%AD_ZSX
              AD_U = B3DOBJ%AD_USX
              S    = SX
         END IF
!
! ------ Cycle on local parameters
!
         DO 440 J4=1,S
            NP = NP + 1
!
! --------- Scanning of J4-th ROW of J2-th global-segmented block of parameters
!
            AD_B = AD_BS + 8*(J4-1) ! Address of the element B(J4,1)
            DO 450 J5=1,G
               IND_FULL = LOCC ( B3DOBJ%INF_SGM(J4,J3), B3DOBJ%INF_GLO(J5) )
               CALL MOVE_R8 ( %VAL(AD_B), MAT(IND_FULL) )
               AD_B = AD_B +8*S ! Now AD_B will keep address B(J4,J5+1)
 450        CONTINUE
!
! --------- Scanning of J4-th ROW of J2-th downdiagonal segmented-segmented
! --------- block
!
            IF ( J3 .GT. 1 ) THEN
                 AD_D = AD_DS + 8*(J4-1) ! Address of element D(J4,1)
                 DO 460 J6=1,SB
                    IND_FULL = LOCC ( B3DOBJ%INF_SGM(J4,J3), &
     &                                B3DOBJ%INF_SGM(J6,J3-1)  )
                    CALL MOVE_R8 ( %VAL(AD_D), MAT(IND_FULL) )
                    AD_D = AD_D +8*S ! Now AD_D will keep address D(J4,J6+1)
 460             CONTINUE
            END IF
!
! --------- Scanning of J4-th ROW of J2-th downdiagonal segmented-segmented
! --------- block
!
            DO 470 J7=1,J4
               IND_FULL = LOCC ( B3DOBJ%INF_SGM(J4,J3), B3DOBJ%INF_SGM(J7,J3) )
               CALL MOVE_R8 ( %VAL(AD_C), MAT(IND_FULL) )
               AD_C = AD_C +8 ! Now AD_C will keep address of the next
!                             ! element of the matrix C
 470        CONTINUE
!
! --------- Moving vector elements to full vectors
!
            IND_FULL = B3DOBJ%INF_SGM(J4,J3)
            CALL MOVE_R8 ( %VAL(AD_E), VEC(IND_FULL) )
            CALL MOVE_R8 ( %VAL(AD_Z), DSP(IND_FULL) )
            CALL MOVE_R8 ( %VAL(AD_U), SCL(IND_FULL) )
!
            AD_E = AD_E + 8
            AD_Z = AD_Z + 8
            AD_U = AD_U + 8
 440     CONTINUE
 430  CONTINUE
!
      RETURN
      END  !#!  EXPAND_B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MOVE_R8 ( A, B )
      REAL*8     A, B
      B=A
      RETURN
      END  !#!  MOVE_R8  #!#
