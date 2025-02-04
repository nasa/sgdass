      SUBROUTINE MARESINIT_B3D ( B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Procedure  MARESINIT_B3D  unscales vector of the estimates and     *
! *   writes unscales vector of the parameters estimates into arrays     *
! *   Q0, QS, QSX.                                                       *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                         Input:  switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will pe put on     *
! *                                 stdout.                              *
! *                         Output: 0 in the case of successful          *
! *                                 completion and non-zero in the case  *
! *                                 of error.                            *
! *                                                                      *
! *  ###  22-SEP-97  MARESINIT_B3D  v1.0 (c)  L. Petrov  22-SEP-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  IUER, J1
      IF ( B3DOBJ%MEM_STAT .NE. F__MFL ) THEN
           WRITE ( 6, * ) ' B3DOBJ%MEM_STAT = ', B3DOBJ%MEM_STAT 
           CALL ERR_LOG ( 6711, IUER, 'MARESINIT_B3D', 'Memory has not been '// &
     &         'allocated for Q(x) vectors in data structure B3DOBJ' )
           RETURN
      END IF
!
! --- Element-by-element multiplications
!
      CALL VEC_MULT_VECTOR ( %VAL(B3DOBJ%AD_E0), %VAL(B3DOBJ%AD_U0), &
     &                            B3DOBJ%N_GLO,  %VAL(B3DOBJ%AD_Q0) )
      DO 410 J1=1,B3DOBJ%NBS-1
         CALL VEC_MULT_VECTOR ( %VAL(B3DOBJ%AD_ES(J1)), %VAL(B3DOBJ%AD_US(J1)), &
     &                          B3DOBJ%SB, %VAL(B3DOBJ%AD_QS(J1))  )
 410  CONTINUE
!
      CALL VEC_MULT_VECTOR ( %VAL(B3DOBJ%AD_ESX), %VAL(B3DOBJ%AD_USX), &
     &                            B3DOBJ%SX,      %VAL(B3DOBJ%AD_QSX)  )
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  MARESINIT_B3D  #!#
!
! ------------------------------------------------------------------------
!
      FUNCTION MARES_B3D ( MODE, OC, PLACE, B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MARES_B3D calculates a postfit residual after LSQ          *
! *   parameters estimation in B3D mode. It is assumed that estiamtes    *
! *   were unscaled and copied into Q-vectors previously by              *
! *   MARESINIT_B3D. It is assumed that an equation of conditions is     *
! *   stored in PLACE data structure.                                    *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     MODE ( INTEGER*4 ) -- Type of observable. F__DEL (delay) or      *
! *                           F__RAT (fringe rate) values are supported. *
! *       OC ( REAL*8    ) -- O-C. If MODE=F__DEL it is delay in sec;    *
! *                           if MODE=F__RAT it is fringe rate           *
! *                           (dimensionless).                           *
! *    PLACE ( RECORD    ) -- Object with data structure for place of    *
! *                           parameters in the list of derivatives.     *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * _________________________ Ouput parameters: ________________________ *
! *                                                                      *
! * <MARES_B3D> ( REAL*8 ) -- Postfit residual for the considered        *
! *                           observation -- sec for dealy and           *
! *                           dimensionless for fringe rate.             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! * IUER ( INTEGER*4, OPT ) -- Universal error handler.                  *
! *                         Input:  switch IUER=0 -- no error messages   *
! *                                 will be generated even in the case   *
! *                                 of error. IUER=-1 -- in the case of  *
! *                                 error the message will pe put on     *
! *                                 stdout.                              *
! *                         Output: 0 in the case of successful          *
! *                                 completion and non-zero in the case  *
! *                                 of error.                            *
! *                                                                      *
! *  ###  16-SEP-1997   MARES_B3D   v1.1 (c) L. Petrov  19-OCT-2017 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      REAL*8     MARES_B3D, OC, GLO_CONT, SG1_CONT, SG2_CONT, DP_VV_V
      REAL*8,    ALLOCATABLE :: VEC(:)
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU   ) ::  B3DOBJ
      INTEGER*4  MODE, IUER, ISG_CUR, ISG_NES, I_LEN
      CHARACTER  STR*80
!
! --- Testing variable MODE
!
      IF ( MODE .NE. F__DEL  .AND.  MODE .NE. F__RAT ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( MODE, STR )
           CALL ERR_LOG ( 6721, -1, 'MARES_B3D', 'Internal error: '// &
     &         'Parameter MODE has wrong value: '//STR(1:I_LEN(STR)) )
           RETURN
      END IF
!
      IF ( MODE .EQ. F__RAT  .AND.  PLACE%STATUS .NE. F__RAT) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( PLACE%STATUS, STR )
           CALL ERR_LOG ( 6722, -1, 'MARES_B3D', 'Internal error: '// &
     &         'Parameter MODE = F__RAT, BUT PLACE%MODE has '// &
     &          'value: '//STR(1:I_LEN(STR)) )
           RETURN
      END IF
      IF ( B3DOBJ%MEM_STAT .NE. F__MFL ) THEN
           CALL ERR_LOG ( 6723, IUER, 'MARES_B3D', 'Memory has not been '// &
     &         'allocated for Q(x) vectors in data structure B3DOBJ' )
           RETURN
      END IF
!
! --- Copying segment pointers
!
      ISG_CUR = PLACE%CURR_CSG
      ISG_NES = PLACE%CURR_CSG+1
      ALLOCATE  ( VEC(MAX(PLACE%N_GLO,PLACE%N_SG1,PLACE%N_SG2)) )
!
! --- Clearing temp vector
!
      CALL NOUT ( 8*PLACE%N_GLO, VEC )
!
! --- Gathering estimates of global parameters participated in this observation
! --- to short vector VEC
!
      CALL DGATHER ( PLACE%N_GLO, PLACE%IND_GLO, %VAL(B3DOBJ%AD_Q0), VEC )
!
! --- Calculation of dot product of the estimats of global parameters with
! --- global part of the equation of conditions
!
      IF ( MODE .EQ. F__DEL ) THEN
           GLO_CONT = DP_VV_V ( PLACE%N_GLO, VEC, PLACE%EQU_GLO )
        ELSE
           GLO_CONT = DP_VV_V ( PLACE%N_GLO, VEC, PLACE%RAT_GLO )
      END IF
!
! --- Clearing temp vector again
!
      CALL NOUT ( 8*PLACE%N_SG1, VEC )
!
! --- Gathering estimates of local-current parameters participated in this
! --- observation to short vector VEC
!
      CALL DGATHER ( PLACE%N_SG1, PLACE%IND_SG1, %VAL(B3DOBJ%AD_QS(ISG_CUR)), &
     &               VEC )
!
! --- Calculation of dot product of the estimats of local-current parameters
! --- with local-curreent part of the equation of conditions
!
      IF ( MODE .EQ. F__DEL ) THEN
           SG1_CONT = DP_VV_V ( PLACE%N_SG1, VEC, PLACE%EQU_SG1 )
        ELSE
           SG1_CONT = DP_VV_V ( PLACE%N_SG1, VEC, PLACE%RAT_SG1 )
      END IF
!
      IF ( PLACE%N_SG2 .EQ. 0 ) THEN
!
! -------- Premature exit in the case when next segment appeared to be empty.
! -------- But it is legal sitation only when: 1) it is the last segment,
! -------- 2) IT was additional segments. Let's test it.
!
           IF ( PLACE%CURR_CSG .NE. B3DOBJ%NBS  .OR. &
     &          ( B3DOBJ%NX_ATM .EQ. 0 .AND. B3DOBJ%NX_CLO .EQ. 0 ) ) THEN
                WRITE ( 6, * )  ' PLACE%CURR_CSG = ', PLACE%CURR_CSG
                WRITE ( 6, * )  ' B3DOBJ%NBS     = ', B3DOBJ%NBS
                WRITE ( 6, * )  ' B3DOBJ%NX_ATM  = ', B3DOBJ%NX_ATM
                WRITE ( 6, * )  ' B3DOBJ%NX_CLO  = ', B3DOBJ%NX_CLO
                CALL ERR_LOG ( 6724, IUER, 'MARES_B3D', ' PLACE%N_SG2 = 0 ' )
                RETURN
           END IF
           SG2_CONT = 0.D0
        ELSE
!
! -------- Clearing temp vector again
!
           CALL NOUT ( 8*PLACE%N_SG2, VEC )
!
! -------- Gathering estimates of local-next parameters participated in this
! -------- observation to short vector VEC
!
           IF ( ISG_NES .LT. B3DOBJ%NBS ) THEN
                CALL DGATHER ( PLACE%N_SG2, PLACE%IND_SG2, &
     &                         %VAL(B3DOBJ%AD_QS(ISG_NES)), VEC )
             ELSE IF ( ISG_NES .EQ. B3DOBJ%NBS ) THEN
                CALL DGATHER ( PLACE%N_SG2, PLACE%IND_SG2, %VAL(B3DOBJ%AD_QSX), &
     &                         VEC )
           END IF
!
! -------- Calculation of dot product of the estimats of local-next
! -------- parameters with local-curreent part of the equation of conditions
!
           IF ( MODE .EQ. F__DEL ) THEN
                SG2_CONT = DP_VV_V ( PLACE%N_SG2, VEC, PLACE%EQU_SG2 )
             ELSE
                SG2_CONT = DP_VV_V ( PLACE%N_SG2, VEC, PLACE%RAT_SG2 )
           END IF
      ENDIF
!
! --- And finally, calculation the residual
!
      MARES_B3D = OC - ( GLO_CONT + SG1_CONT + SG2_CONT )
!
      DEALLOCATE ( VEC )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  MARES_B3D  #!#
