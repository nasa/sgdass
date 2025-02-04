      SUBROUTINE ADD_B3D ( MODE, OC, SIGMA, PLACE, B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_B3D  updates accumulators -- submatrixes and          *
! *   subvectors for normal system for the case of B3D algorithm for     *
! *   the next equations.                                                *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    MODE ( REAL*8     ) -- Type of observable. Acceptable type is:    *
! *                        MODE = F__DEL -- delay.                       *
! *                        MODE = F__RAT -- delay rate.                  *
! *      OC ( REAL*8     ) -- Right part of the equation of condition.   *
! *   SIGMA ( REAL*8     ) -- A priori uncertainty of the observations.  *
! *   PLACE  ( RECORD    ) -- Object with data structure for place of    *
! *                           parameters in the list of derivatives.     *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *     IUER ( INTEGER*4, OPT ) -- Universal error habdler.              *
! *            Input: swicth IUER=0 -- no error messages will be         *
! *                                 generated even in the case of error. *
! *                          IUER=-1 -- in the case of error the message *
! *                                  will pe put on stdout.              *
! *            Default input value = -1                                  *
! *            Output: 0 in the case of successful completion and error  *
! *                    code in the case of error.                        *
! *                                                                      *
! *  ###   09-JAN-97    ADD_B3D    v1.5  (c)  L. Petrov  01-MAR-2005 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      REAL*8     OC, SIGMA
      INTEGER*4  MODE, SG1, SG2, IUER
      INTEGER*4  NN, I_LEN
      CHARACTER  STR*20
!
      IF ( MODE .NE. F__DEL  .AND.  MODE .NE. F__RAT ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( MODE, STR )
           CALL ERR_LOG ( 8471, -1, 'ADD_B3D', 'Internal error: '// &
     &         'Parameter MODE has wrong value: '//STR(1:I_LEN(STR)) )
           STOP 'Abnormal termination'
      END IF
!
      IF ( MODE .EQ. F__RAT  .AND.  PLACE%STATUS .NE. F__RAT) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( PLACE%STATUS, STR )
           CALL ERR_LOG ( 8472, -1, 'ADD_B3D', 'Internal error: '// &
     &         'Parameter MODE = F__RAT, BUT PLACE%MODE has '// &
     &          'value: '//STR(1:I_LEN(STR)) )
           STOP 'Abnormal termination'
      END IF
!
! --- Calculation SG1 -- the number of parameters at current block and
!                 SG2 -- the number of parameters at the last block
!
      IF ( PLACE%CURR_CSG .LT. B3DOBJ%NBS-1 ) THEN
           SG1 = B3DOBJ%SB
           SG2 = B3DOBJ%SB
        ELSE IF ( PLACE%CURR_CSG .EQ. B3DOBJ%NBS-1 ) THEN
           SG1 = B3DOBJ%SB
           SG2 = B3DOBJ%SX
!!        ELSE IF ( PLACE.CURR_CSG .EQ. B3DOBJ.NBS   .AND.
!!     #            PLACE.N_SG2    .EQ. 0                  ) THEN
!!           SG1 = B3DOBJ.SX
!!           SG2 = -1
!!        ELSE IF ( PLACE.CURR_CSG .GT. B3DOBJ.NBS-1 .AND.
!!     #            PLACE.N_SG2    .GT. 0                  ) THEN
        ELSE IF ( PLACE%CURR_CSG .GT. B3DOBJ%NBS-1 ) THEN
          WRITE ( 6, * ) ' place%n_sg1 = ', place%n_sg1,' place%n_sg2 = ', place%n_sg2
          WRITE ( 6, * ) ' place%n_glo = ', place%n_glo,' place%n_gen = ', place%n_gen
          WRITE ( 6, * ) ' place%curr_csg =',place%curr_csg,' b3dobj%nbs =', b3dobj%nbs
!
          WRITE ( 6, * ) ' place%ind_sg1 = ', ( place%ind_sg1(nn), nn=1, place%n_sg1 )
          WRITE ( 6, * ) ' place%ind_sg2 = ', ( place%ind_sg2(nn), nn=1, place%n_sg2 )
          WRITE ( 6, * ) ' place%ind_glo = ', ( place%ind_glo(nn), nn=1, place%n_glo )
          WRITE ( 6, * ) ' place%ind_gen = ', ( place%ind_gen(nn), nn=1, place%n_gen )
          CALL ERR_LOG ( 8473, IUER, 'ADD_B3D', 'An observation after '// &
     &         'the last segment which attempt use the next segment '// &
     &         'turned up. This is unusual situation. '// &
     &         'It may occur due to erroneous calculation segments '// &
     &         'interval. :-(  Hint: turn on FAST_DBG = "PRINTOUT", run '// &
     &         'again and look at file /tmp/param.fil' )
          RETURN
      END IF
!
! --- Updating global-global accumulator
!
      IF ( PLACE%N_GLO > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &                         PLACE%N_GLO,  PLACE%IND_GLO,       PLACE%EQU_GLO, &
     &                         B3DOBJ%N_GLO, %VAL(B3DOBJ%AD_VG0), &
     &                                       %VAL(B3DOBJ%AD_N00)  )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &                         PLACE%N_GLO,  PLACE%IND_GLO,       PLACE%RAT_GLO, &
     &                         B3DOBJ%N_GLO, %VAL(B3DOBJ%AD_VG0), &
     &                                       %VAL(B3DOBJ%AD_N00)  )
           END IF
      END IF
!
! --- Updating segmented(current)-global accumulator
!
      IF ( PLACE%N_GLO > 0  .AND.  PLACE%N_SG1 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_RCT ( OC, SIGMA,                                 &
     &                         PLACE%N_SG1, PLACE%IND_SG1, PLACE%EQU_SG1, &
     &                         PLACE%N_GLO, PLACE%IND_GLO, PLACE%EQU_GLO, &
     &                         SG1,         B3DOBJ%N_GLO,  %VAL(B3DOBJ%AD_N10) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_RCT ( OC, SIGMA,                                 &
     &                         PLACE%N_SG1, PLACE%IND_SG1, PLACE%RAT_SG1, &
     &                         PLACE%N_GLO, PLACE%IND_GLO, PLACE%RAT_GLO, &
     &                         SG1,         B3DOBJ%N_GLO,  %VAL(B3DOBJ%AD_N10) )
           END IF
      END IF
!
! --- Updating segmented(current)-segmented(current) accumulator
!
      IF ( PLACE%N_SG1 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &                         PLACE%N_SG1,  PLACE%IND_SG1, PLACE%EQU_SG1, &
     &                         SG1,          %VAL(B3DOBJ%AD_VS1), &
     &                                       %VAL(B3DOBJ%AD_N11)           )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &                         PLACE%N_SG1,  PLACE%IND_SG1, PLACE%RAT_SG1, &
     &                         SG1,          %VAL(B3DOBJ%AD_VS1), &
     &                                       %VAL(B3DOBJ%AD_N11)           )
           END IF
      END IF
!
      IF ( PLACE%N_SG2 .EQ. 0 ) THEN
!
! -------- Premature exit in the case when next segment appeared to be empty.
! -------- But it is legal sitation only when: 1) it is the last segment,
! -------- 2) It was the additional exrtra segments. Let's test it.
!
           IF ( PLACE%CURR_CSG .NE. B3DOBJ%NBS  .OR. &
     &          ( B3DOBJ%NX_ATM .EQ. 0 .AND. B3DOBJ%NX_CLO .EQ. 0 ) ) THEN
                WRITE ( 6, * )  ' PLACE%N_SG1    = ', PLACE%N_SG1
                WRITE ( 6, * )  ' PLACE%N_SG2    = ', PLACE%N_SG2
                WRITE ( 6, * )  ' PLACE%CURR_CSG = ', PLACE%CURR_CSG
                WRITE ( 6, * )  ' B3DOBJ%NBS     = ', B3DOBJ%NBS
                WRITE ( 6, * )  ' B3DOBJ%NX_ATM  = ', B3DOBJ%NX_ATM
                WRITE ( 6, * )  ' B3DOBJ%NX_CLO  = ', B3DOBJ%NX_CLO
                CALL ERR_LOG ( 8474, IUER, 'ADD_B3D', ' PLACE%N_SG2 = 0 ' )
                RETURN
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN
      ENDIF
!
! --- Updating segmented(next)-global accumulator
!
      IF ( PLACE%N_GLO > 0  .AND.  PLACE%N_SG2 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
               CALL ADD_RCT ( OC, SIGMA, &
     &                        PLACE%N_SG2,  PLACE%IND_SG2, PLACE%EQU_SG2, &
     &                        PLACE%N_GLO,  PLACE%IND_GLO, PLACE%EQU_GLO, &
     &                        SG2,          B3DOBJ%N_GLO,  %VAL(B3DOBJ%AD_N20) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
               CALL ADD_RCT ( OC, SIGMA, &
     &                        PLACE%N_SG2,  PLACE%IND_SG2, PLACE%RAT_SG2, &
     &                        PLACE%N_GLO,  PLACE%IND_GLO, PLACE%RAT_GLO, &
     &                        SG2,          B3DOBJ%N_GLO,  %VAL(B3DOBJ%AD_N20) )
           END IF
      END IF
!
! --- Updating segmented(next)-segmented(current) accumulator
!
      IF ( PLACE%N_SG1 > 0  .AND.  PLACE%N_SG2 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &                         PLACE%N_SG2, PLACE%IND_SG2, PLACE%EQU_SG2, &
     &                         PLACE%N_SG1, PLACE%IND_SG1, PLACE%EQU_SG1, &
     &                         SG2,         SG1,           %VAL(B3DOBJ%AD_N21) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &                         PLACE%N_SG2, PLACE%IND_SG2, PLACE%RAT_SG2, &
     &                         PLACE%N_SG1, PLACE%IND_SG1, PLACE%RAT_SG1, &
     &                         SG2,         SG1,           %VAL(B3DOBJ%AD_N21) )
           END IF
      END IF
!
! --- Updating segmented(next)-segmented(next) accumulator
!
      IF ( PLACE%N_SG2 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &                         PLACE%N_SG2, PLACE%IND_SG2, PLACE%EQU_SG2, &
     &                         SG2,         %VAL(B3DOBJ%AD_VS2),          &
     &                                      %VAL(B3DOBJ%AD_N22) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &                         PLACE%N_SG2, PLACE%IND_SG2, PLACE%RAT_SG2, &
     &                         SG2,         %VAL(B3DOBJ%AD_VS2),          &
     &                                      %VAL(B3DOBJ%AD_N22) )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ADD_B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NSG_B3D ( NP, NC, B3DOBJ )
! ************************************************************************
! *                                                                      *
! *   Routine  NSG_B3D  updates a set of sumatrices  B3DOBJ  when new    *
! *   segment boundary are being reached. It is assumed that             *
! *   matrices/vectors were updating for all observations of the NC-th   *
! *   common segment.                                                    *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *     NP ( INTEGER*4 ) -- Index of the previous segment.               *
! *     NC ( INTEGER*4 ) -- Index of the current segment.                *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! * B3DOBJ ( RECORD    ) -- Object with data structure for B3D extension *
! *                         of SOLVE.                                    *
! *                                                                      *
! *  ###  09-JAN-97   NSG_B3D      v2.0  (c)  L. Petrov  29-JAN-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      INTEGER*4  NC, NP
      INTEGER*4  G, S, SX, J1
      INTEGER*8  GA, SA, SXA
!
      G   = B3DOBJ%N_GLO
      GA  = (INT8(G)*INT8(G+1))/2
      S   = B3DOBJ%SB
      SA  = (INT8(S)*INT8(S+1))/2
      SX  = B3DOBJ%SX
      SXA = (INT8(SX)*INT8(SX+1))/2
!
! --- Update global-global block  B0
!
      CALL ADD_VV8 ( GA, %VAL(B3DOBJ%AD_B0), %VAL(B3DOBJ%AD_N00) )
!
! --- Update global part of vector right parts Z0
!
      CALL ADD_VV  ( G, %VAL(B3DOBJ%AD_Z0), %VAL(B3DOBJ%AD_VG0) )
!
! --- Putting cross multiplications l2 conditional submatrix in next block
!
      IF ( NC .LT. B3DOBJ%NBS-1 ) THEN
           CALL COPY_V  ( S*G,  %VAL(B3DOBJ%AD_N20), %VAL(B3DOBJ%AD_B(NC+1))  )
           CALL COPY_V  ( S*S,  %VAL(B3DOBJ%AD_N21), %VAL(B3DOBJ%AD_D(NC+1))  )
           CALL COPY_V8 ( SA,   %VAL(B3DOBJ%AD_N22), %VAL(B3DOBJ%AD_C(NC+1))  )
           CALL COPY_V  ( S,    %VAL(B3DOBJ%AD_VS2), %VAL(B3DOBJ%AD_ZS(NC+1)) )
         ELSE IF ( NC .EQ. B3DOBJ%NBS-1 ) THEN
           CALL COPY_V  ( SX*G, %VAL(B3DOBJ%AD_N20), %VAL(B3DOBJ%AD_BX)  )
           CALL COPY_V  ( SX*S, %VAL(B3DOBJ%AD_N21), %VAL(B3DOBJ%AD_DX)  )
           CALL COPY_V8 ( SXA,  %VAL(B3DOBJ%AD_N22), %VAL(B3DOBJ%AD_CX)  )
           CALL COPY_V  ( SX,   %VAL(B3DOBJ%AD_VS2), %VAL(B3DOBJ%AD_ZSX) )
      END IF
!
      IF ( NC .EQ. 1 .OR. (NC-NP) .GT. 1 ) THEN
!
! -------- Special case of the first block
!
           CALL COPY_V  ( S*G, %VAL(B3DOBJ%AD_N10), %VAL(B3DOBJ%AD_B(NC))  )
           CALL COPY_V8 ( SA,  %VAL(B3DOBJ%AD_N11), %VAL(B3DOBJ%AD_C(NC))  )
           CALL COPY_V  ( S,   %VAL(B3DOBJ%AD_VS1), %VAL(B3DOBJ%AD_ZS(NC)) )
        ELSE
!
! -------- Update for cross multiplcation L1 conditional submatrix
!
           CALL ADD_VV  ( S*G, %VAL(B3DOBJ%AD_B(NC)),  %VAL(B3DOBJ%AD_N10) )
           CALL ADD_VV8 ( SA,  %VAL(B3DOBJ%AD_C(NC)),  %VAL(B3DOBJ%AD_N11) )
           CALL ADD_VV  ( S,   %VAL(B3DOBJ%AD_ZS(NC)), %VAL(B3DOBJ%AD_VS1) )
      END IF
      IF ( (NC-NP) .GE. 3 ) THEN
!
! -------- Zeroing missed blocks
!
           DO 410 J1=NP+2,NC-1
              CALL NOUT_R8  ( S*G,  %VAL(B3DOBJ%AD_B(J1))  )
              CALL NOUT_R8  ( S*S,  %VAL(B3DOBJ%AD_D(J1))  )
              CALL NOUT8_R8 ( SA,   %VAL(B3DOBJ%AD_C(J1))  )
              CALL NOUT_R8  ( S,    %VAL(B3DOBJ%AD_ZS(J1)) )
 410       CONTINUE
      END IF
      RETURN
      END  !#!  NSG_B3D  #!#
