      SUBROUTINE ADD_B1B3D ( MODE, OC, SIGMA, PLACE, B3DOBJ, B1B3DOBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_B1B3D  updates accumulators -- submatrixes and        *
! *   subvectors for normal system for the case of B1B3D algorithm for   *
! *   the next equations.                                                *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *    MODE ( REAL*8     ) -- Type of observable. Acceptable type is:    *
! *                        MODE = F__DEL -- delay.                       *
! *                        MODE = F__RAT -- delay rate.                  *
! *       OC ( REAL*8    ) -- Right part of the equation of condition.   *
! *    SIGMA ( REAL*8    ) -- A priori uncertainty of the observations.  *
! *    PLACE ( RECORD    ) -- Object with data structure for place of    *
! *                           parameters in the list of derivatives.     *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
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
! *  ### 26-FEB-1997   ADD_B1B3D   v1.3  (c)  L. Petrov 01-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( PLACE__STRU ) ::  PLACE
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      REAL*8     OC, SIGMA
      INTEGER*4  MODE, G, L, SG1, SG2, IUER
      INTEGER*4  NN, I_LEN
      CHARACTER  STR*20
!
      G = B3DOBJ%N_GLO
      L = B3DOBJ%N_LOC
!
! --- Calculcation SG1 -- the number of parameters at current block and
!                  SG2 -- the number of parameters at the last block
!
!
      IF ( MODE .NE. F__DEL  .AND.  MODE .NE. F__RAT ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( MODE, STR )
           CALL ERR_LOG ( 8481, -1, 'ADD_B1B3D', 'Internal error: '// &
     &         'Parameter MODE has wrong value: '//STR(1:I_LEN(STR)) )
           STOP 'Abnormal termination'
      END IF
!
      IF ( MODE .EQ. F__RAT  .AND.  PLACE%STATUS .NE. F__RAT) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( PLACE%STATUS, STR )
           CALL ERR_LOG ( 8482, -1, 'ADD_B1B3D', 'Internal error: '// &
     &         'Parameter MODE = F__RAT, BUT PLACE%MODE has '// &
     &          'value: '//STR(1:I_LEN(STR)) )
           STOP 'Abnormal termination'
      END IF
!
      IF ( PLACE%CURR_CSG .LT. B3DOBJ%NBS-1 ) THEN
           SG1 = B3DOBJ%SB
           SG2 = B3DOBJ%SB
        ELSE IF ( PLACE%CURR_CSG .EQ. B3DOBJ%NBS-1 ) THEN
           SG1 = B3DOBJ%SB
           SG2 = B3DOBJ%SX
        ELSE IF ( PLACE%CURR_CSG .GT. B3DOBJ%NBS-1 ) THEN
           WRITE ( 6, * ) ' place%n_sg1 = ', place%n_sg1
           WRITE ( 6, * ) ' place%n_sg2 = ', place%n_sg2
           WRITE ( 6, * ) ' place%n_glo = ', place%n_glo
           WRITE ( 6, * ) ' place%n_gen = ', place%n_gen
!
           WRITE ( 6, * ) ' place%ind_sg1 = ', ( place%ind_sg1(nn), nn=1, &
     &     place%n_sg1 )
           WRITE ( 6, * ) ' place%ind_sg2 = ', ( place%ind_sg2(nn), nn=1, &
     &     place%n_sg2 )
           WRITE ( 6, * ) ' place%ind_glo = ', ( place%ind_glo(nn), nn=1, &
     &     place%n_glo )
           WRITE ( 6, * ) ' place%ind_gen = ', ( place%ind_gen(nn), nn=1, &
     &     place%n_gen )
!
           CALL ERR_LOG ( 8483, IUER, 'ADD_B1B3D', 'An observation after '// &
     &         'the last segment which attempt use the next segment '// &
     &         'turned up. This is unusual situation. '// &
     &         'It may occur due to erroneous calculation segments '// &
     &         'interval. :-(  Hint: turn on FAST_DBG = "PRINTOUT", run '// &
     &         'again and look at file /tmp/param.fil' )
           RETURN
      END IF
      IF ( PLACE%N_GLO > 0 ) THEN
!
! -------- Updating global-global accumulator
!
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &               PLACE%N_GLO, PLACE%IND_GLO,        PLACE%EQU_GLO, &
     &               G,           %VAL(B1B3DOBJ%AD_VG), %VAL(B1B3DOBJ%AD_NGG) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &               PLACE%N_GLO, PLACE%IND_GLO,        PLACE%RAT_GLO, &
     &               G,           %VAL(B1B3DOBJ%AD_VG), %VAL(B1B3DOBJ%AD_NGG) )
           END IF
      END IF
!
! --- Updating local-global accumulator
!
      IF ( PLACE%N_GLO > 0  .AND.  PLACE%N_LOC > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_LOC, PLACE%IND_LOC, PLACE%EQU_LOC,        &
     &               PLACE%N_GLO, PLACE%IND_GLO, PLACE%EQU_GLO,        &
     &               L,           G,             %VAL(B1B3DOBJ%AD_NLG) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_LOC, PLACE%IND_LOC, PLACE%RAT_LOC,        &
     &               PLACE%N_GLO, PLACE%IND_GLO, PLACE%RAT_GLO,        &
     &               L,           G,             %VAL(B1B3DOBJ%AD_NLG) )
           END IF
      END IF
!
! --- Updating local-local accumulator
!
      IF ( PLACE%N_LOC > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &               PLACE%N_LOC, PLACE%IND_LOC,        PLACE%EQU_LOC,        &
     &               L,           %VAL(B1B3DOBJ%AD_VL), %VAL(B1B3DOBJ%AD_NLL) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &               PLACE%N_LOC, PLACE%IND_LOC,        PLACE%RAT_LOC,        &
     &               L,           %VAL(B1B3DOBJ%AD_VL), %VAL(B1B3DOBJ%AD_NLL) )
           END IF
      END IF
!
! --- Updating segmented(current)-global accumulator
!
      IF ( PLACE%N_GLO > 0  .AND.  PLACE%N_SG1 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG1, PLACE%IND_SG1, PLACE%EQU_SG1, &
     &               PLACE%N_GLO, PLACE%IND_GLO, PLACE%EQU_GLO, &
     &               SG1,         G,             %VAL(B1B3DOBJ%AD_NS1G) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG1, PLACE%IND_SG1, PLACE%RAT_SG1, &
     &               PLACE%N_GLO, PLACE%IND_GLO, PLACE%RAT_GLO, &
     &               SG1,         G,             %VAL(B1B3DOBJ%AD_NS1G) )
           END IF
      END IF
!
! --- Updating segmented(current)-local  accumulator
!
      IF ( PLACE%N_LOC > 0  .AND.  PLACE%N_SG1 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG1, PLACE%IND_SG1, PLACE%EQU_SG1,         &
     &               PLACE%N_LOC, PLACE%IND_LOC, PLACE%EQU_LOC,         &
     &               SG1,         L,             %VAL(B1B3DOBJ%AD_NS1L) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG1, PLACE%IND_SG1, PLACE%RAT_SG1,         &
     &               PLACE%N_LOC, PLACE%IND_LOC, PLACE%RAT_LOC,         &
     &               SG1,         L,             %VAL(B1B3DOBJ%AD_NS1L) )
           END IF
      END IF
!
! --- Updating segmented(current)-segmented(current) accumulator
!
      IF ( PLACE%N_SG1 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &               PLACE%N_SG1, PLACE%IND_SG1,         PLACE%EQU_SG1, &
     &               SG1,         %VAL(B1B3DOBJ%AD_VS1), %VAL(B1B3DOBJ%AD_NS1S1) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &               PLACE%N_SG1, PLACE%IND_SG1,         PLACE%RAT_SG1, &
     &               SG1,         %VAL(B1B3DOBJ%AD_VS1), %VAL(B1B3DOBJ%AD_NS1S1) )
           END IF
      END IF
      IF ( PLACE%N_SG2 .EQ. 0 ) THEN
!
! -------- Premature exit in the case when the next segment appeared to be
! -------- empty. But it is legal sitation only when: 1) it is the last
! -------- segment, 2) It was the additional segment. Let's test it.
!
           IF ( PLACE%CURR_CSG .NE. B3DOBJ%NBS  .OR. &
     &          ( B3DOBJ%NX_ATM .EQ. 0 .AND. B3DOBJ%NX_CLO .EQ. 0 ) ) THEN
                WRITE ( 6, * )  ' PLACE%CURR_CSG = ', PLACE%CURR_CSG
                WRITE ( 6, * )  ' B3DOBJ%NBS     = ', B3DOBJ%NBS
                WRITE ( 6, * )  ' B3DOBJ%NX_ATM  = ', B3DOBJ%NX_ATM
                WRITE ( 6, * )  ' B3DOBJ%NX_CLO  = ', B3DOBJ%NX_CLO
                CALL ERR_LOG ( 8484, IUER, 'ADD_B1B3D', ' PLACE%N_SG2 = 0 ' )
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
     &               PLACE%N_SG2, PLACE%IND_SG2, PLACE%EQU_SG2,         &
     &               PLACE%N_GLO, PLACE%IND_GLO, PLACE%EQU_GLO,         &
     &               SG2,         G,             %VAL(B1B3DOBJ%AD_NS2G) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG2, PLACE%IND_SG2, PLACE%RAT_SG2,         &
     &               PLACE%N_GLO, PLACE%IND_GLO, PLACE%RAT_GLO,         &
     &               SG2,         G,             %VAL(B1B3DOBJ%AD_NS2G) )
           END IF
      END IF
!
! --- Updating segmented(next)-local accumulator
!
      IF ( PLACE%N_LOC > 0  .AND.  PLACE%N_SG2 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG2,  PLACE%IND_SG2,  PLACE%EQU_SG2, &
     &               PLACE%N_LOC,  PLACE%IND_LOC,  PLACE%EQU_LOC, &
     &               SG2,          L,              %VAL(B1B3DOBJ%AD_NS2L)  )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG2,  PLACE%IND_SG2,  PLACE%RAT_SG2, &
     &               PLACE%N_LOC,  PLACE%IND_LOC,  PLACE%RAT_LOC, &
     &               SG2,          L,              %VAL(B1B3DOBJ%AD_NS2L)  )
           END IF
      END IF
!
! --- Updating segmented(next)-segmented(current) accumulator
!
      IF ( PLACE%N_SG1 > 0  .AND.  PLACE%N_SG2 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG2, PLACE%IND_SG2, PLACE%EQU_SG2,          &
     &               PLACE%N_SG1, PLACE%IND_SG1, PLACE%EQU_SG1,          &
     &               SG2,         SG1,           %VAL(B1B3DOBJ%AD_NS2S1) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_RCT ( OC, SIGMA, &
     &               PLACE%N_SG2, PLACE%IND_SG2, PLACE%RAT_SG2,          &
     &               PLACE%N_SG1, PLACE%IND_SG1, PLACE%RAT_SG1,          &
     &               SG2,         SG1,           %VAL(B1B3DOBJ%AD_NS2S1) )
           END IF
      END IF
!
! --- Updating segmented(next)-segmented(next) accumulator
!
      IF ( PLACE%N_SG2 > 0 ) THEN
           IF ( MODE .EQ. F__DEL ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &               PLACE%N_SG2, PLACE%IND_SG2,         PLACE%EQU_SG2, &
     &               SG2,         %VAL(B1B3DOBJ%AD_VS2), %VAL(B1B3DOBJ%AD_NS2S2) )
              ELSE IF ( MODE .EQ. F__RAT ) THEN
                CALL ADD_TRG ( OC, SIGMA, &
     &               PLACE%N_SG2, PLACE%IND_SG2,         PLACE%RAT_SG2, &
     &               SG2,         %VAL(B1B3DOBJ%AD_VS2), %VAL(B1B3DOBJ%AD_NS2S2) )
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  ADD_B1B3D  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE NSG_B1B3D ( NP, NC, B3DOBJ, B1B3DOBJ )
! ************************************************************************
! *                                                                      *
! *   Routine  NSG_B1B3D  updates a set of sumatrices  B1B3DOBJ  when    *
! *   new  segment boundary are being reached. This routine is a part of *
! *   a set of routines implementing B1B3D algorithm of solving LSQ      *
! *   problem. It is assumed that matrices/vectors were updating for all *
! *   observations of the NC-th common segment.                          *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *       NP ( INTEGER*4 ) -- Index of the previous segment.             *
! *       NC ( INTEGER*4 ) -- Index of the current segment.              *
! *   B3DOBJ ( RECORD    ) -- Object with data structure for B3D         *
! *                           extension of SOLVE.                        *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! * B1B3DOBJ ( RECORD    ) -- Object with data structure for B1B3D       *
! *                           extension of SOLVE.                        *
! *                                                                      *
! *  ###  26-FEB-97   NSG_B1B3D    v1.0  (c)  L. Petrov  26-FEB-97  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      INTEGER*4  NC, NP
      INTEGER*4  G, GA, L, LA, S, SA, SX, SXA, J1
!
      G   = B3DOBJ%N_GLO
      GA  = (G*(G+1))/2
      L   = B3DOBJ%N_LOC
      LA  = (L*(L+1))/2
      S   = B3DOBJ%SB
      SA  = (S*(S+1))/2
      SX  = B3DOBJ%SX
      SXA = (SX*(SX+1))/2
!
! --- Update global-global block  W00
!
      CALL ADD_VV ( GA, %VAL(B1B3DOBJ%AD_W00), %VAL(B1B3DOBJ%AD_NGG) )
!
! --- Update common part of vector right parts Z00
!
      CALL ADD_VV ( G, %VAL(B1B3DOBJ%AD_Z00), %VAL(B1B3DOBJ%AD_VG) )
!
! --- Update local-local block  BI0
!
      CALL ADD_VV ( LA, %VAL(B1B3DOBJ%AD_BI0), %VAL(B1B3DOBJ%AD_NLL) )
!
! --- Update common part of vector right parts ZI0
!
      CALL ADD_VV ( L, %VAL(B1B3DOBJ%AD_ZI0), %VAL(B1B3DOBJ%AD_VL) )
!
! --- Update local-global block  WI0
!
      CALL ADD_VV8 ( INT8(L)*INT8(G), %VAL(B1B3DOBJ%AD_WI0), %VAL(B1B3DOBJ%AD_NLG) )
!
! --- Putting cross multiplications submatrix in next block
!
      IF ( NC .LT. B3DOBJ%NBS-1 ) THEN
         CALL COPY_V(S*G, %VAL(B1B3DOBJ%AD_NS2G),  %VAL(B1B3DOBJ%AD_WIJ(NC+1)) )
         CALL COPY_V(S*L, %VAL(B1B3DOBJ%AD_NS2L),  %VAL(B1B3DOBJ%AD_BIJ(NC+1)) )
         CALL COPY_V(S*S, %VAL(B1B3DOBJ%AD_NS2S1), %VAL(B1B3DOBJ%AD_DIJ(NC+1)) )
         CALL COPY_V(SA,  %VAL(B1B3DOBJ%AD_NS2S2), %VAL(B1B3DOBJ%AD_CIJ(NC+1)) )
         CALL COPY_V(S,   %VAL(B1B3DOBJ%AD_VS2),   %VAL(B1B3DOBJ%AD_ZIJ(NC+1)) )
        ELSE IF ( NC .EQ. B3DOBJ%NBS-1 ) THEN
         CALL COPY_V(SX*G,%VAL(B1B3DOBJ%AD_NS2G),  %VAL(B1B3DOBJ%AD_WIJ(NC+1)) )
         CALL COPY_V(SX*L,%VAL(B1B3DOBJ%AD_NS2L),  %VAL(B1B3DOBJ%AD_BIJ(NC+1)) )
         CALL COPY_V(SX*S,%VAL(B1B3DOBJ%AD_NS2S1), %VAL(B1B3DOBJ%AD_DIJ(NC+1)) )
         CALL COPY_V(SXA, %VAL(B1B3DOBJ%AD_NS2S2), %VAL(B1B3DOBJ%AD_CIJ(NC+1)) )
         CALL COPY_V(SX,  %VAL(B1B3DOBJ%AD_VS2),   %VAL(B1B3DOBJ%AD_ZIJ(NC+1)) )
      END IF
!
      IF ( NC .EQ. 1 .OR. (NC-NP) .GT. 1 ) THEN
!
! ------ Special case of the first block
!
         CALL COPY_V (S*G, %VAL(B1B3DOBJ%AD_NS1G),  %VAL(B1B3DOBJ%AD_WIJ(NC)) )
         CALL COPY_V (S*L, %VAL(B1B3DOBJ%AD_NS1L),  %VAL(B1B3DOBJ%AD_BIJ(NC)) )
         CALL COPY_V (SA,  %VAL(B1B3DOBJ%AD_NS1S1), %VAL(B1B3DOBJ%AD_CIJ(NC)) )
         CALL COPY_V (S,   %VAL(B1B3DOBJ%AD_VS1),   %VAL(B1B3DOBJ%AD_ZIJ(NC)) )
        ELSE
!
! ------ Update for cross multiplcation L1 conditional submatrix
!
         CALL ADD_VV (S*G, %VAL(B1B3DOBJ%AD_WIJ(NC)), %VAL(B1B3DOBJ%AD_NS1G)  )
         CALL ADD_VV (S*L, %VAL(B1B3DOBJ%AD_BIJ(NC)), %VAL(B1B3DOBJ%AD_NS1L)  )
         CALL ADD_VV (SA,  %VAL(B1B3DOBJ%AD_CIJ(NC)), %VAL(B1B3DOBJ%AD_NS1S1) )
         CALL ADD_VV (S,   %VAL(B1B3DOBJ%AD_ZIJ(NC)), %VAL(B1B3DOBJ%AD_VS1)   )
      END IF
      IF ( (NC-NP) .GE. 3 ) THEN
!
! -------- Zeroing missed blocks
!
           DO 410 J1=NP+2,NC-1
              CALL NOUT_R8 ( S*G,  %VAL(B1B3DOBJ%AD_WIJ(J1))  )
              CALL NOUT_R8 ( S*L,  %VAL(B1B3DOBJ%AD_BIJ(J1))  )
              CALL NOUT_R8 ( SA,   %VAL(B1B3DOBJ%AD_CIJ(J1))  )
              CALL NOUT_R8 ( S*S,  %VAL(B1B3DOBJ%AD_DIJ(J1))  )
              CALL NOUT_R8 ( S,    %VAL(B1B3DOBJ%AD_ZIJ(J1)) )
 410       CONTINUE
      END IF
      RETURN
      END  !#!  NSG_B1B3D  #!#
