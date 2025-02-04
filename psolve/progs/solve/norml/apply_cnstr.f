      SUBROUTINE APPLY_CNSTR ( FAST_MODE, FAST_DBG, CNSTROBJ, NPAR, &
     &                         NOR_MAT, NOR_VEC, B3DOBJ, B1B3DOBJ, &
     &                         FL_FULL_WEI, WEI_CNS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  APPLY_CNSTR  applies constraints gathered in the data     *
! *   structure  CNSTROBJ. It updates elements of the normal matrix and  *
! *   normal vector (if neccessary).                                     *
! *                                                                      *
! * _________________________ Input Parameters: ________________________ *
! *                                                                      *
! *   FAST_MODE ( INTEGER*4 ) -- switch for the mode. Aceptable modes    *
! *                              are F__B3D and F__B1B3D.                *
! *   FAST_DBG  ( INTEGER*4 ) -- Verbosity mode switch for debug. Should *
! *                              be zero for normal work.                *
! *   CNSTROBJ  ( RECORD    ) -- The data structure with information     *
! *                              about constraints (position where the   *
! *                              matrix should be modified and the value *
! *                              of constraints).                        *
! *       NPAR  ( INTEGER*4 ) -- The number of parameters.               *
! * FL_FULL_WEI ( LOGICAL*4 ) -- Flag, whether to build the full matrix  *
! *                              of constraints. If .FALSE., then        *
! *                              off-diagonal terms in weight are        *
! *                              ignored even if they exists and WEI_CNS *
! *                              matrix is not creatred.                 *
! *                                                                      *
! * _________________________ Output Parameters: _______________________ *
! *                                                                      *
! *     WEI_CNS ( REAL*8    ) -- Weight matrix of constraint in upper    *
! *                              triangular repretation presentation.    *
! *                              Dimension: CNSTROBJ.N_EQUAT             *
! *                              NB: the matrix is created only if       *
! *                              FL_FULL_WEI == .TRUE.                   *
! *                                                                      *
! * _________________________ Modified Parameters: _____________________ *
! *                                                                      *
! *     NOR_MAT ( REAL*8    ) -- Normal matrix (full).                   *
! *     NOR_VEC ( REAL*8    ) -- Normal vector (full).                   *
! *      B3DOBJ ( RECORD    ) -- Object with data structure for B3D      *
! *                              extension of SOLVE.                     *
! *    B1B3DOBJ ( RECORD    ) -- Object with data structure for B3D      *
! *                              extension of SOLVE.                     *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ###  19-JAN-1998  APPLY_CNSTR  v6.3  (c) L. Petrov  10-APR-2022 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'precm.i'
      INCLUDE    'fast.i'
      INCLUDE    'glbcm.i'
      INCLUDE    'cnstr.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INTEGER*4  FAST_MODE, FAST_DBG
      TYPE     ( CNSTR__STRU ) ::  CNSTROBJ
      TYPE     ( B3D__STRU   ) ::  B3DOBJ
      TYPE     ( B1B3D__STRU ) ::  B1B3DOBJ
      LOGICAL*4  FL_FULL_WEI, FL_NOMECHI
      INTEGER*4  NPAR, IUER
      REAL*8     NOR_MAT(*), NOR_VEC(*), WEI_CNS(*)
!
      REAL*8     SIG_MIN 
      PARAMETER  ( SIG_MIN =1.D-23 )
      REAL*8     WEI_DIAG(MAX_EQUAT), EQU_VEC(M_GPA), NOR_SAVE
      INTEGER*4  NBL, IR, IC, J1, J2, J3, J4, J5, J6, &
     &           J7, J8, J9, J10, J11, J12, J13, N_EQU, &
     &           IND_EQU(M_GPA), LUN
      ADDRESS__TYPE :: IAD, IAD_DUMMY
      INTEGER*8  IPOS
      CHARACTER  TYP*1
      CHARACTER  DEBUG_STR*32, UNF_DISABLE_ENV*13, FILCNS*128, STR*128
      SAVE       UNF_DISABLE_ENV
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN
      ADDRESS__TYPE, EXTERNAL :: FULL_B3D, FULL_B1B3D
      INTEGER*4  I, J
      INTEGER*8  LOCS
      LOCS(I,J) = INT8(min(I,J)) +(INT8(max(I,J))*INT8(max(I,J)-1))/2
!
      IF ( CNSTROBJ%N_EQUAT .LE. 0 ) THEN
!
! -------- Nothing to do
!
           GOTO 810
      END IF
!
      CALL GETENVAR (    'DEBUG_CNS', DEBUG_STR ) 
      CALL TRAN     ( 11, DEBUG_STR,  DEBUG_STR ) 
      IF ( DEBUG_STR == 'YES' .OR. DEBUG_STR == 'NAM' .OR. &
     &     ( DEBUG_STR == 'MAT' .AND. FAST_MODE == F__NONE )  ) THEN
           LUN = GET_UNIT ()
           IF ( CNSTROBJ%OBJ_NAM == 'GLOBAL' ) THEN
                FILCNS = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNDG'//PRE_LETRS
              ELSE
                FILCNS = PRE_SCR_DIR(1:PRE_SD_LEN)//'CNDS'//PRE_LETRS
           END IF
           OPEN ( UNIT=LUN, FILE=FILCNS, STATUS='UNKNOWN' )
           WRITE ( 6, * ) 'APPLY_CNSTR: debug output is written in '//TRIM(FILCNS)
      END IF
      IF ( DEBUG_STR == 'MAT' .AND. FAST_MODE == F__NONE ) THEN
!
! -------- Zero the CGM in the special debugging mode
!
           WRITE ( 6, * ) 'Zero out the CGM for debugging purposes!'
           CALL NOUT8_R8 ( (INT8(NPAR)*INT8(NPAR+1))/2, NOR_MAT )
      END IF
!
! --- Clear weight matrix of constraints
!
      IF ( FL_FULL_WEI ) THEN
           CALL NOUT_R8 ( (INT8(CNSTROBJ%N_EQUAT)*INT8(CNSTROBJ%N_EQUAT+1))/INT8(2), WEI_CNS )
         ELSE
           CALL NOUT_R8 ( M_GPA, WEI_DIAG )
      END IF
      CALL GETENVAR ( 'NO_MECHI', STR )
      CALL TRAN ( 11, STR, STR )
      IF ( STR == 'YES' ) THEN
           FL_NOMECHI = .TRUE.
         ELSE 
           FL_NOMECHI = .FALSE.
      END IF
      IF ( DEBUG_STR == 'YES' .OR. DEBUG_STR == 'NAM' ) THEN
           WRITE ( LUN, * ) 'APPLY_CNSTR-132 NPAR= ', NPAR, ' CNSTROBJ%N_EQUAT= ', CNSTROBJ%N_EQUAT, ' CNSTROBJ%N_ECNST= ', CNSTROBJ%N_ECNST
           CALL FLUSH ( 6 )
      END IF
!
! --- Set diagonal elements of the weight matrix of constraints
!
      DO 410 J1=1,CNSTROBJ%N_EQUAT
         IF ( DABS( CNSTROBJ%SIG_CNS(J1) ) .LT. SIG_MIN ) THEN
              WRITE ( 6, * ) ' J1=',J1, &
     &                       ' CNSTROBJ%SIG_CNS(J1) = ',CNSTROBJ%SIG_CNS(J1), &
     &                       ' CNSTROBJ%SBI_CNS(J1) = ',CNSTROBJ%SBI_CNS(J1), &
     &                       ' CNSTROBJ%GLO_CNS(J1) = ',CNSTROBJ%GLO_CNS(J1), &
     &                       ' CNSTROBJ%USR_CNS(J1) = ',CNSTROBJ%USR_CNS(J1)
              CALL ERR_LOG ( 3431, IUER, 'APPLY_CNSTR', 'Trap of internal '// &
     &            'control: too small sigma for "'//CNSTROBJ%ABB_CNS(J1)// &
     &            '" constraint: '//CNSTROBJ%DSC_CNS(J1) )
              RETURN
         END IF
         IF ( FL_FULL_WEI ) THEN
              WEI_CNS( LOCS(J1,J1) ) = 1.D0/CNSTROBJ%SIG_CNS(J1)**2
              IF ( DEBUG_STR == 'YES' ) THEN
                   WRITE ( LUN, 210 ) J1, CNSTROBJ%ABB_CNS(J1), CNSTROBJ%GLO_CNS(J1), &
     &                                    CNSTROBJ%USR_CNS(J1), CNSTROBJ%EQU_INE(J1), &
     &                                    CNSTROBJ%EQU_INP(J1), WEI_CNS( LOCS(J1,J1) )
              END IF
            ELSE 
              IF ( DEBUG_STR == 'YES' ) THEN
                   IF ( CNSTROBJ%DYN(J1)%STS .EQ. CNS__ALC ) THEN
                        DO 420 J2=1,CNSTROBJ%DYN(J1)%L_CNS
                           WRITE ( LUN, 210 ) J1, CNSTROBJ%ABB_CNS(J1), &
     &                                            CNSTROBJ%GLO_CNS(J1), &
     &                                            CNSTROBJ%USR_CNS(J1), &
     &                                            CNSTROBJ%DYN(J1)%EQU_INE(J2), &
     &                                            CNSTROBJ%DYN(J1)%EQU_INP(J2), &
     &                                            CNSTROBJ%SIG_CNS(J1), &
     &                                            J2
 420                    CONTINUE 
                      ELSE
                        WRITE ( LUN, 210 ) J1, CNSTROBJ%ABB_CNS(J1), &
     &                                         CNSTROBJ%GLO_CNS(J1), &
     &                                         CNSTROBJ%USR_CNS(J1), &
     &                                         CNSTROBJ%EQU_INE(J1), &
     &                                         CNSTROBJ%EQU_INP(J1), &
     &                                         CNSTROBJ%SIG_CNS(J1), &
     &                                         0
                   ENDIF
 210               FORMAT ( 'Cns: ', I6,' Abb: ', A, ' Flags: ', L1, 1X, L1, &
     &                       ' ine: ', I6, ' inp: ', I6, ' Sig= ', 1PD13.6, 1X, &
     &                       'Sub_ind: ', I6 )
                   CALL FLUSH ( 6 )
              END IF
              WEI_DIAG(J1) = 1.D0/CNSTROBJ%SIG_CNS(J1)**2
         END IF
 410  CONTINUE
!
      IF ( FL_FULL_WEI  .AND.  CNSTROBJ%N_OFD > 0 ) THEN
!
! -------- Set off-diagonal elements of matrix of constraints
!
           DO 430 J3=1,CNSTROBJ%N_OFD
              IF ( DEBUG_STR == 'YES' ) THEN
                   WRITE ( LUN, * ) ' OFD: J3=', J3, &
     &                              ' OFD_INE = ', CNSTROBJ%INE1_OFD(J3), &
     &                                             CNSTROBJ%INE2_OFD(J3), &
     &                              ' WEI=', CNSTROBJ%WEI_OFD(J3)
              END IF
              WEI_CNS ( LOCS(CNSTROBJ%INE1_OFD(J3),CNSTROBJ%INE2_OFD(J3)) ) = &
     &                  CNSTROBJ%WEI_OFD(J3)
 430       CONTINUE
      END IF
!
#ifdef DEBUG
   write ( 6, * ) 'aplly_cnstr(204) fl_nomechi= ', fl_nomechi, ' cnstrobj%n_ecnst= ', cnstrobj%n_ecnst
#endif
      IF ( FL_NOMECHI ) THEN
!
! -------- Fast method
!
           DO 440 J4=1,CNSTROBJ%N_EQUAT
              N_EQU = 0
              DO 450 J5=1,CNSTROBJ%N_ECNST
                 IF ( CNSTROBJ%EQU_INE(J5) == J4 ) THEN
                      N_EQU = N_EQU + 1
                      IF ( N_EQU > M_GPA ) THEN
                           CALL ERR_LOG ( 3432, IUER, 'APPLY_CNSTR', 'Trap of intenal '// &
     &                         'control: N_EQU > M_GPA' ) 
                           RETURN 
                      END IF
                      IND_EQU(N_EQU) = CNSTROBJ%EQU_INP(J5) 
                      EQU_VEC(N_EQU) = CNSTROBJ%EQU_CNS(J5) 
                 END IF
 450          CONTINUE 
              CALL ADD_TRG ( CNSTROBJ%RTP_CNS(J4), CNSTROBJ%SIG_CNS(J4), &
     &                       N_EQU,  IND_EQU, EQU_VEC, &
     &                       NPARAM, NOR_VEC, NOR_MAT )
 440       CONTINUE 
      END IF
!
      IF ( .NOT. FL_NOMECHI ) THEN
           DO 460 J6=1,CNSTROBJ%N_ECNST
              DO 470 J7=1,J6
                 IF ( CNSTROBJ%EQU_INE(J6) .NE. CNSTROBJ%EQU_INE(J7) ) GOTO 470
!
! -------------- Find IAD -- the address of the element in submatrices B3D
! -------------- which corresponds to the updated element of the full normal
! -------------- matrix
!
                 IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD ) THEN
                      IPOS = LOCS ( CNSTROBJ%EQU_INP(J6), CNSTROBJ%EQU_INP(J7)  )
                      IF ( FL_FULL_WEI  .AND.  DEBUG_STR == 'YES' ) THEN
!
! ------------------------ Debugging output
!
                           WRITE ( LUN, 110 ) &
     &                                  CNSTROBJ%EQU_INE(J6), &
     &                                  CNSTROBJ%ABB_CNS(CNSTROBJ%EQU_INE(J6)), &
     &                                  CNSTROBJ%SBI_CNS(CNSTROBJ%EQU_INE(J6)), &
     &                                  CNSTROBJ%EQU_INP(J7), &
     &                                  CNSTROBJ%EQU_INP(J6), &
     &                                  CNSTROBJ%EQU_CNS(J7), &
     &                                  CNSTROBJ%EQU_CNS(J6), &
     &                     WEI_CNS( LOCS(CNSTROBJ%EQU_INE(J6),CNSTROBJ%EQU_INE(J7)) )
 110                       FORMAT ( 'ine: ',I4, &
     &                              ' abr: ',A8,' sbi: ',I4, &
     &                              ' par_1: ', I5,' par_2: ', I5, &
     &                                          ' cns_1: ', 1PD15.8, &
     &                                          ' cns_2: ', 1PD15.8, &
     &                                          '   wei: ', 1PD15.8 )
                      END IF
                      IF ( FL_FULL_WEI  ) THEN
                           NOR_MAT(IPOS) = NOR_MAT(IPOS) + CNSTROBJ%EQU_CNS(J6)* &
     &                                                     CNSTROBJ%EQU_CNS(J7)* &
     &                     WEI_CNS( LOCS(CNSTROBJ%EQU_INE(J6),CNSTROBJ%EQU_INE(J7)) )
                         ELSE 
                           NOR_MAT(IPOS) = NOR_MAT(IPOS) + &
     &                                         CNSTROBJ%EQU_CNS(J6)* &
     &                                         CNSTROBJ%EQU_CNS(J7)* &
     &                                         WEI_DIAG(CNSTROBJ%EQU_INE(J6))
                           IF ( DEBUG_STR == 'YES' ) THEN
                                WRITE ( LUN, 120 ) J6, &
     &                                             CNSTROBJ%ABB_CNS(CNSTROBJ%EQU_INE(J6)), &
     &                                             CNSTROBJ%EQU_INP(J6), &
     &                                             CNSTROBJ%EQU_INP(J7), &
     &                                             CNSTROBJ%EQU_CNS(J6)* &
     &                                             CNSTROBJ%EQU_CNS(J7), &
     &                                             WEI_DIAG(CNSTROBJ%EQU_INE(J6)), &
     &                                             IPOS
 120                            FORMAT ( 'APPLY_CNST I_cns: ', I7, ' Abb: ', A, &
     &                                  ' Equ: ', I5, 1X, I5, &
     &                                  ' Val= ', 1PD15.7, ' Wei_d= ', 1PD15.7, ' Ipos: ', I15 )
                           END IF
                      END IF
                    ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
                      IAD = FULL_B3D ( B3DOBJ, CNSTROBJ%EQU_INP(J6), &
     &                                 CNSTROBJ%EQU_INP(J7), TYP, NBL, IR, IC )
                    ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                      IAD = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, CNSTROBJ%EQU_INP(J6), &
     &                                   CNSTROBJ%EQU_INP(J7), TYP, NBL, IR, IC )
                 END IF
!
                 IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D ) THEN
                      IF ( IR .EQ. -1  .OR.  IC .EQ. -1 ) THEN
                           write ( 6, * ) ' j3=',j3,' j4=',j4, &
     &                           ' equ_inp(j3)=', cnstrobj%equ_inp(j3), &
     &                           ' equ_inp(j4)=', cnstrobj%equ_inp(j4), &
     &                           ' equ_ine(j3)=', cnstrobj%equ_ine(j3), &
     &                           ' equ_ine(j4)=', cnstrobj%equ_ine(j4)
                           write ( 6, * ) ' equ_cns(j3) = ', cnstrobj%equ_cns(j3), &
     &                                    ' equ_cns(j4) = ', cnstrobj%equ_cns(j4)
                           write ( 6, * ) ' nbl=',nbl,' ir=',ir,' ic=',ic,' iad=',iad
                           write ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                           write ( 6, * ) ' ipos=',ipos,' abr =', &
     &                                      cnstrobj%abb_cns(cnstrobj%equ_ine(j3))
!
                           CALL ERR_LOG ( 3433, IUER, 'APPLY_CNSTR', 'Internal '// &
     &                         'error: failure to calculate index for '// &
     &                         'corresponding elements: B3D, B1B3D <---> FULL.'// &
     &                         'Look at /tmp/param.fil' )
!
! ------------------------ Call SOCOM_EXT in debigging mode
!
                           SOCOM_PLUS_FIRST = SPL__UNDF
                           UNF_DISABLE_ENV = 'UNF_DISABLE=V'
                           CALL PUTENV ( UNF_DISABLE_ENV(1:I_LEN(UNF_DISABLE_ENV))//CHAR(0) )
                           CALL SOCOM_EXT()
!
                           RETURN
                      END IF
!
                      IF ( FL_FULL_WEI ) THEN
                           CALL R8_UPDATE ( IAD, CNSTROBJ%EQU_CNS(J6)*CNSTROBJ%EQU_CNS(J7)* &
     &                                           WEI_CNS( LOCS(CNSTROBJ%EQU_INE(J6),CNSTROBJ%EQU_INE(J7)) ) )
                           IF ( DEBUG_STR == 'YES' ) THEN
                                WRITE ( LUN, * ) ' CNS: J6=', J6, ' J7=', J7, &
     &                                           ' INE: ', CNSTROBJ%EQU_INE(J6), &
     &                                                    CNSTROBJ%EQU_INE(J7), &
     &                                           ' WEI= ', WEI_CNS( LOCS(CNSTROBJ%EQU_INE(J6),CNSTROBJ%EQU_INE(J7)) ) 
                      END IF
                         ELSE 
                           CALL R8_UPDATE ( IAD, (CNSTROBJ%EQU_CNS(J6)* &
     &                                            CNSTROBJ%EQU_CNS(J7))* & 
     &                                            WEI_DIAG(CNSTROBJ%EQU_INE(J6)) )
                           IF ( DEBUG_STR == 'YES' ) THEN
                                WRITE ( LUN, * ) ' CNS: J6=', J6, ' J7=', J7, &
     &                                           ' INE: ', CNSTROBJ%EQU_INE(J6), &
     &                                                     CNSTROBJ%EQU_INE(J7), &
     &                                           ' WEI= ', WEI_DIAG(CNSTROBJ%EQU_INE(J6))
                           END IF
                      END IF
                 END IF
 470          CONTINUE
!
! ----------- Apply right hand part of constraints
!
              IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD ) THEN
!
! ---------------- Full matrix
!
                   IF ( FL_FULL_WEI ) THEN
                        NOR_VEC( CNSTROBJ%EQU_INP(J6) )= NOR_VEC( CNSTROBJ%EQU_INP(J6) )+ &
     &                           CNSTROBJ%EQU_CNS(J6)* &
     &                           CNSTROBJ%RTP_CNS( CNSTROBJ%EQU_INE(J6) )* &
     &                           WEI_CNS( LOCS(CNSTROBJ%EQU_INE(J6),CNSTROBJ%EQU_INE(J6)) )
                      ELSE 
                        NOR_VEC( CNSTROBJ%EQU_INP(J6) )= NOR_VEC( CNSTROBJ%EQU_INP(J6) )+ &
     &                           CNSTROBJ%EQU_CNS(J6)* &
     &                           CNSTROBJ%RTP_CNS( CNSTROBJ%EQU_INE(J6) )* &
     &                           WEI_DIAG( CNSTROBJ%EQU_INE(J6) )
                   END IF
                 ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ---------------- B3D case
!
                   IAD_DUMMY = FULL_B3D ( B3DOBJ, CNSTROBJ%EQU_INP(J6), &
     &                                    CNSTROBJ%EQU_INP(J6), TYP, NBL, IR, IC )
                   IF ( TYP .EQ. 'G' ) THEN
                        IAD = B3DOBJ%AD_Z0 + 8*(IR-1)
                      ELSE IF ( TYP .EQ. 'C' ) THEN
                        IF ( NBL .LT. B3DOBJ%NBS ) THEN
                             IAD = B3DOBJ%AD_ZS(NBL) + 8*(IR-1)
                          ELSE IF ( NBL .EQ. B3DOBJ%NBS ) THEN
                             IAD = B3DOBJ%AD_ZSX + 8*(IR-1)
                        END IF
                   END IF
                 ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ---------------- B1B3D case
!
                   IAD_DUMMY = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, CNSTROBJ%EQU_INP(J6), &
     &                                      CNSTROBJ%EQU_INP(J6), TYP, NBL, IR, IC )
                   IF ( TYP .EQ. 'G' ) THEN
                        IAD = B1B3DOBJ%AD_Z00 + 8*(IR-1)
                      ELSE IF ( TYP .EQ. 'L' ) THEN
                        IAD = B1B3DOBJ%AD_ZI0 + 8*(IR-1)
                      ELSE IF ( TYP .EQ. 'C' ) THEN
                        IAD = B1B3DOBJ%AD_ZIJ(NBL) + 8*(IR-1)
                   END IF
              END IF
!
              IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D ) THEN
                   IF ( IR .EQ. -1  .OR.  IC .EQ. -1 ) THEN
                        WRITE ( 6, * ) ' j3=',j3,' nr = ', cnstrobj%nr(j3), &
     &                         ' nbl=',nbl,' ir=',ir,' ic=',ic,' iad=',iad
                        WRITE ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                        WRITE ( 6, * ) ' b1b3dobj%ad_w00 = ',b1b3dobj%ad_w00
                        WRITE ( 6, * ) ' cnstrobj%n_vecel = ',cnstrobj%n_vecel
                        CALL ERR_LOG ( 3434, IUER, 'APPLY_CNSTR', 'Internal '// &
     &                           'error: failure to calculate index for '// &
     &                           'corresponding elements: B3D, B1B3D <---> FULL' )
                        RETURN
                   END IF
!
                   IF ( FL_FULL_WEI ) THEN
                        CALL R8_UPDATE ( IAD, &
     &                                   CNSTROBJ%EQU_CNS(J6)* &
     &                                   CNSTROBJ%RTP_CNS( CNSTROBJ%EQU_INE(J6) )* &
     &                  WEI_CNS( LOCS(CNSTROBJ%EQU_INE(J6),CNSTROBJ%EQU_INE(J6)) ) )
                      ELSE
                        CALL R8_UPDATE ( IAD, &
     &                                   CNSTROBJ%EQU_CNS(J6)* &
     &                                   CNSTROBJ%RTP_CNS( CNSTROBJ%EQU_INE(J6) )* &
     &                                   WEI_DIAG( CNSTROBJ%EQU_INE(J6) ) )
                   END IF
               END IF
 460       CONTINUE
      ENDIF
!
! --- Now the apply dynamic constraint equations
!
      DO 480 J8=1,CNSTROBJ%N_EQUAT
         IF ( CNSTROBJ%DYN(J8)%STS == CNS__ALC  .AND. &
     &        CNSTROBJ%DYN(J8)%L_CNS > 0              ) THEN
!
! ----------- Well, we have dynamic constraint equation J8
!
              IF ( DEBUG_STR == 'YES' ) THEN
                   WRITE  ( LUN, 220 ) CNSTROBJ%CNS_TYP,     &
     &                                 CNSTROBJ%ABB_CNS(J8), &
     &                                 CNSTROBJ%DSC_CNS(J8), &
     &                                 CNSTROBJ%N_EQUAT,     &
     &                                 CNSTROBJ%N_ECNST,     &
     &                                 CNSTROBJ%N_OFD
 220               FORMAT ( 'DYN_CNS:  TYP: ', I6, ' CNS_NAM: ', A10, ' CNS_DESC: ', A32, &
     &                      ' N_EQUAT: ', I8, ' N_ECNST: ', I8, ' N_OFD: ', I8 )
              END IF
              DO 490 J9=1,CNSTROBJ%DYN(J8)%L_CNS
                 DO 4100 J10=1,J9
                    IF ( CNSTROBJ%DYN(J8)%EQU_INE(J9) .NE. CNSTROBJ%DYN(J8)%EQU_INE(J10) ) GOTO 4100
!
! ----------------- Find IAD -- the address of the element in submatrices B3D
! ----------------- which corresponds to the updated element of the full normal
! ----------------- matrix
!
                    IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD ) THEN
                         IPOS = LOCS ( CNSTROBJ%DYN(J8)%EQU_INP(J9), CNSTROBJ%DYN(J8)%EQU_INP(J10)  )
                         IF ( FL_FULL_WEI  .AND.  DEBUG_STR == 'YES' ) THEN
!
! --------------------------- Debugging output
!
                              WRITE ( LUN, 110 ) &
     &                             CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                             CNSTROBJ%ABB_CNS(CNSTROBJ%DYN(J8)%EQU_INE(J9)), &
     &                             CNSTROBJ%SBI_CNS(CNSTROBJ%DYN(J8)%EQU_INE(J9)), &
     &                             CNSTROBJ%DYN(J8)%EQU_INP(J10), &
     &                             CNSTROBJ%DYN(J8)%EQU_INP(J9), &
     &                             CNSTROBJ%DYN(J8)%EQU_CNS(J10), &
     &                             CNSTROBJ%DYN(J8)%EQU_CNS(J9), &
     &                             WEI_CNS( LOCS(CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                                           CNSTROBJ%DYN(J8)%EQU_INE(J10)) )
                         END IF
                         IF ( FL_FULL_WEI  ) THEN
                              NOR_MAT(IPOS) = NOR_MAT(IPOS) + &
     &                                           CNSTROBJ%DYN(J8)%EQU_CNS(J9)* &
     &                                           CNSTROBJ%DYN(J8)%EQU_CNS(J10)* &
     &                                           WEI_CNS( LOCS(CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                                                         CNSTROBJ%DYN(J8)%EQU_INE(J10)) )
                           ELSE 
                              NOR_SAVE = NOR_MAT(IPOS) 
                              NOR_MAT(IPOS) = NOR_MAT(IPOS) + &
     &                                           CNSTROBJ%DYN(J8)%EQU_CNS(J9)* &
     &                                           CNSTROBJ%DYN(J8)%EQU_CNS(J10)* &
     &                                           WEI_DIAG(CNSTROBJ%DYN(J8)%EQU_INE(J9))
                              IF ( DEBUG_STR == 'YES' ) THEN
                                   WRITE ( LUN, 130 ) J8, J9, &
     &                                          CNSTROBJ%ABB_CNS(CNSTROBJ%DYN(J8)%EQU_INE(J9)), &
     &                                          CNSTROBJ%SBI_CNS(CNSTROBJ%DYN(J8)%EQU_INE(J9)), &
     &                                          CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                                          CNSTROBJ%DYN(J8)%EQU_INE(J10), &
     &                                          CNSTROBJ%DYN(J8)%EQU_INP(J9), &
     &                                          CNSTROBJ%DYN(J8)%EQU_INP(J10), &
     &                                          CNSTROBJ%DYN(J8)%EQU_CNS(J9)*CNSTROBJ%DYN(J8)%EQU_CNS(J10), &
     &                                          WEI_DIAG(CNSTROBJ%DYN(J8)%EQU_INE(J9)), IPOS, NOR_SAVE
 130                               FORMAT ( '          dyn: ', I7, 1X, I7, ' Abb: ', A, &
     &                                      ' Sbi: ', I4, ' Ind_equ: ', I6, 1X, I6, &
     &                                      ' Ind_par: ', I5, 1X, I5, &
     &                                      ' Val= ', 1PD15.7, ' Wei_d= ', 1PD15.7, ' Ipos= ', I15, ' NOR_VAL: ', 1PD15.7 )
                              END IF
                         END IF
                       ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
                         IAD = FULL_B3D ( B3DOBJ, &
     &                                    CNSTROBJ%DYN(J8)%EQU_INP(J9), &
     &                                    CNSTROBJ%DYN(J8)%EQU_INP(J10), &
     &                                    TYP, NBL, IR, IC )
                       ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                         IAD = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, &
     &                                      CNSTROBJ%DYN(J8)%EQU_INP(J9), &
     &                                      CNSTROBJ%DYN(J8)%EQU_INP(J10), &
     &                                      TYP, NBL, IR, IC )
                    END IF
!
                    IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D ) THEN
                        IF ( IR .EQ. -1  .OR.  IC .EQ. -1 ) THEN
                             write ( 6, * ) ' j3=',j3,' j4=',j4, &
     &                             ' DYN(J8)%EQU_INP(j3)=', cnstrobj%DYN(J8)%EQU_INP(j3), &
     &                             ' DYN(J8)%EQU_INP(j4)=', cnstrobj%DYN(J8)%EQU_INP(j4), &
     &                             ' DYN(J8)%EQU_INE(j3)=', cnstrobj%DYN(J8)%EQU_INE(j3), &
     &                             ' DYN(J8)%EQU_INE(j4)=', cnstrobj%DYN(J8)%EQU_INE(j4)
                             write ( 6, * ) ' j1=',j1, &
     &                             ' nbl=',nbl,' ir=',ir,' ic=',ic,' iad=',iad
                             write ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                             write ( 6, * ) ' ipos=',ipos,' abr =', &
     &                             cnstrobj%abb_cns(cnstrobj%DYN(J8)%EQU_INE(j3))
!
                             CALL ERR_LOG ( 3435, IUER, 'APPLY_CNSTR', &
     &                           'Internal error: failure to calculate '// &
     &                           'index for corresponding elements: '// &
     &                           'B3D, B1B3D <---> FULL' )
                             RETURN
                        END IF
!
                        IF ( FL_FULL_WEI ) THEN
                             CALL R8_UPDATE ( IAD, &
     &                                        CNSTROBJ%DYN(J8)%EQU_CNS(J9)* &
     &                                        CNSTROBJ%DYN(J8)%EQU_CNS(J10)* &
     &                            WEI_CNS( LOCS(CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                                          CNSTROBJ%DYN(J8)%EQU_INE(J10)) ) )
                          ELSE 
                             CALL R8_UPDATE ( IAD, &
     &                                        CNSTROBJ%DYN(J8)%EQU_CNS(J9)* &
     &                                        CNSTROBJ%DYN(J8)%EQU_CNS(J10)* & 
     &                            WEI_DIAG(CNSTROBJ%DYN(J8)%EQU_INE(J9)) )
                        END IF
!
                        IF ( DEBUG_STR == 'YES' ) THEN
                             WRITE ( LUN, 230 ) J9, J10, CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                                                  CNSTROBJ%DYN(J8)%EQU_INE(J10), &
     &                                                  CNSTROBJ%DYN(J8)%EQU_CNS(J9), &
     &                                                  CNSTROBJ%DYN(J8)%EQU_CNS(J10), &
     &                                                  WEI_CNS( LOCS(CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                                                                CNSTROBJ%DYN(J8)%EQU_INE(J10)) ) 
 230                         FORMAT ( '    dyn_cns:  I_cns: ', I6, 1X, I6, ' I_equ: ', I8, 1X, I8, &
     &                                ' Cns: ', 1PD15.7, 1X, 1PD15.7, ' Wei: ', 1PD15.7 )
                        END IF
                    END IF
 4100            CONTINUE 
!
! -------------- Apply right hand part of constraints
!
                 IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD ) THEN
!
! ------------------- Full matrix
!
                      IF ( FL_FULL_WEI ) THEN
                           NOR_VEC( CNSTROBJ%DYN(J8)%EQU_INP(J9) )= &
     &                              NOR_VEC( CNSTROBJ%DYN(J8)%EQU_INP(J9) )+ &
     &                        CNSTROBJ%DYN(J8)%EQU_CNS(J9)* &
     &                        CNSTROBJ%RTP_CNS( CNSTROBJ%DYN(J8)%EQU_INE(J9) )* &
     &                        WEI_CNS( LOCS(CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                                      CNSTROBJ%DYN(J8)%EQU_INE(J9)) )
                        ELSE 
                           NOR_VEC( CNSTROBJ%DYN(J8)%EQU_INP(J9) )= &
     &                              NOR_VEC( CNSTROBJ%DYN(J8)%EQU_INP(J9) )+ &
     &                        CNSTROBJ%DYN(J8)%EQU_CNS(J9)* &
     &                        CNSTROBJ%RTP_CNS( CNSTROBJ%DYN(J8)%EQU_INE(J9) )* &
     &                        WEI_DIAG( CNSTROBJ%DYN(J8)%EQU_INE(J9) )
                      END IF
                   ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ------------------- B3D case
!
                      IAD_DUMMY = FULL_B3D ( B3DOBJ, &
     &                                       CNSTROBJ%DYN(J8)%EQU_INP(J9), &
     &                                       CNSTROBJ%DYN(J8)%EQU_INP(J9), &
     &                                       TYP, NBL, IR, IC )
                      IF ( TYP .EQ. 'G' ) THEN
                           IAD = B3DOBJ%AD_Z0 + 8*(IR-1)
                        ELSE IF ( TYP .EQ. 'C' ) THEN
                           IF ( NBL .LT. B3DOBJ%NBS ) THEN
                                IAD = B3DOBJ%AD_ZS(NBL) + 8*(IR-1)
                              ELSE IF ( NBL .EQ. B3DOBJ%NBS ) THEN
                                IAD = B3DOBJ%AD_ZSX + 8*(IR-1)
                           END IF
                      END IF
                    ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ------------------ B1B3D case
!
                     IAD_DUMMY = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, &
     &                                        CNSTROBJ%DYN(J8)%EQU_INP(J9), &
     &                                        CNSTROBJ%DYN(J8)%EQU_INP(J9), &
     &                                        TYP, NBL, IR, IC )
                     IF ( TYP .EQ. 'G' ) THEN
                          IAD = B1B3DOBJ%AD_Z00 + 8*(IR-1)
                       ELSE IF ( TYP .EQ. 'L' ) THEN
                          IAD = B1B3DOBJ%AD_ZI0 + 8*(IR-1)
                       ELSE IF ( TYP .EQ. 'C' ) THEN
                          IAD = B1B3DOBJ%AD_ZIJ(NBL) + 8*(IR-1)
                     END IF
                 END IF
!
                 IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D ) THEN
                      IF ( IR .EQ. -1  .OR.  IC .EQ. -1 ) THEN
                           WRITE ( 6, * ) ' j6=',j6,' nr = ', cnstrobj%nr(j6), &
     &                                    ' nbl=',nbl,' ir=',ir,' ic=',ic, &
     &                                    ' iad=',iad
                           WRITE ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                           WRITE ( 6, * ) ' b1b3dobj%ad_w00 = ',b1b3dobj%ad_w00
                           WRITE ( 6, * ) ' cnstrobj%n_vecel = ',cnstrobj%n_vecel
                           CALL ERR_LOG ( 3436, IUER, 'APPLY_CNSTR', &
     &                         'Internal error: failure to calculate index '// &
     &                         'for corresponding elements: B3D, B1B3D '// &
     &                         '<---> FULL' )
                           RETURN
                       END IF
!
                       IF ( FL_FULL_WEI ) THEN
                            CALL R8_UPDATE ( IAD, &
     &                           CNSTROBJ%DYN(J8)%EQU_CNS(J9)* &
     &                           CNSTROBJ%RTP_CNS( CNSTROBJ%DYN(J8)%EQU_INE(J9) )* &
     &                           WEI_CNS( LOCS(CNSTROBJ%DYN(J8)%EQU_INE(J9), &
     &                           CNSTROBJ%DYN(J8)%EQU_INE(J9)) ) )
                          ELSE
                            CALL R8_UPDATE ( IAD, &
     &                           CNSTROBJ%DYN(J8)%EQU_CNS(J9)* &
     &                           CNSTROBJ%RTP_CNS( CNSTROBJ%DYN(J8)%EQU_INE(J9) )* &
     &                           WEI_DIAG( CNSTROBJ%DYN(J8)%EQU_INE(J9) ) )
                       END IF
                 END IF
 490          CONTINUE 
         END IF
 480  CONTINUE
 810  CONTINUE
!
      IF ( CNSTROBJ%N_MATEL .LE. 0  .AND.  CNSTROBJ%N_ECNST .LE. 0 ) THEN
           IF ( FAST_DBG .EQ. F__PRI  .OR.  FAST_DBG .EQ. F__APP  ) THEN
                WRITE ( 6, * ) ' apply_cnstr: ', 'No constraints were applied '
           END IF
        ELSE
           IF ( FAST_DBG .EQ. F__PRI  .OR.  FAST_DBG .EQ. F__APP ) THEN
                IF ( CNSTROBJ%N_MATEL .GT. 0 ) THEN
                     WRITE ( 6, * ) ' apply_cnstr: ',CNSTROBJ%N_MATEL, &
     &                      ' non-zero elements of constraint matrix'
                END IF
                IF ( CNSTROBJ%N_ECNST .GT. 0 ) THEN
                     WRITE ( 6, * ) ' apply_cnstr: ',CNSTROBJ%N_EQUAT, &
     &                      ' constraint equations'
                     WRITE ( 6, * ) ' apply_cnstr: ',CNSTROBJ%N_ECNST, &
     &                      ' non-zero elements of constraint equations'
                END IF
           END IF
      END IF
!
      IF ( CNSTROBJ%N_MATEL .GT. 0 ) THEN
           DO 4110 J11=1,CNSTROBJ%N_MATEL
!
! ----------- Find IAD -- the address of the element in submatrices B3D
! ----------- which correspond to the updated element of the full normal
! ----------- matrix
!
              IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD ) THEN
                   IPOS = LOCS ( CNSTROBJ%NP1(J11), CNSTROBJ%NP2(J11) )
                   NOR_MAT(IPOS) = NOR_MAT(IPOS) + CNSTROBJ%MAT_UPD(J11)
                ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
                   IAD = FULL_B3D   ( B3DOBJ, CNSTROBJ%NP1(J11), &
     &                                CNSTROBJ%NP2(J11), TYP, NBL, IR, IC )
                ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
                   IAD = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, CNSTROBJ%NP1(J11), &
     &                                CNSTROBJ%NP2(J11), TYP, NBL, IR, IC )
              END IF
!
              IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D ) THEN
                   IF ( IR .EQ. -1  .OR.  IC .EQ. -1 ) THEN
                        WRITE ( 6, * ) ' j1=',j1,' np1 = ', cnstrobj%np1(j5), &
     &                                   ' np2 = ', cnstrobj%np2(j5), &
     &                         ' nbl=',nbl,' ir=',ir,' ic=',ic,' iad=',iad
                        WRITE ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                        WRITE ( 6, * ) ' cnstrobj%n_matel = ',cnstrobj%n_matel
                        CALL ERR_LOG ( 3437, IUER, 'APPLY_CNSTR', 'Internal '// &
     &                      'error: failure to calculate index for '// &
     &                      'corresponding elements: B3D, B1B3D <---> FULL' )
                        RETURN
                   END IF
!
                   CALL R8_UPDATE ( IAD, CNSTROBJ%MAT_UPD(J11) )
               END IF
 4110      CONTINUE
           IF ( FAST_DBG .EQ. F__PRI ) THEN
                WRITE ( 6, * ) ' apply_cnstr: ',CNSTROBJ%N_MATEL, ' elements of ', &
     &                 'the normal matrix have been changed.'
           END IF
      END IF
!
! --- Update of normal vector if neccessary
!
      IF ( CNSTROBJ%N_VECEL .LE. 0 ) THEN
           CONTINUE
         ELSE
           DO 4120 J12=1,CNSTROBJ%N_VECEL
!
! ----------- Find IAD -- the address of the element in subvecter B3D
! ----------- which corresponds to the updated element of the full normal
! ----------- matrix
!
              IF ( FAST_MODE .EQ. F__NONE .OR. FAST_MODE .EQ. F__PRD ) THEN
!
! ---------------- Full matrix
!
                   IPOS = CNSTROBJ%NR(J12)
                   NOR_VEC(IPOS) = NOR_VEC(IPOS) + CNSTROBJ%VEC_UPD(J12)
                ELSE IF ( FAST_MODE .EQ. F__B3D ) THEN
!
! ---------------- B3D case
!
                   IAD_DUMMY = FULL_B3D   ( B3DOBJ, CNSTROBJ%NR(J12), &
     &                                      CNSTROBJ%NR(J12), TYP, NBL, IR, IC )
                   IF ( TYP .EQ. 'G' ) THEN
                        IAD = B3DOBJ%AD_Z0 + 8*(IR-1)
                     ELSE IF ( TYP .EQ. 'C' ) THEN
                        IF ( NBL .LT. B3DOBJ%NBS ) THEN
                             IAD = B3DOBJ%AD_ZS(NBL) + 8*(IR-1)
                          ELSE IF ( NBL .EQ. B3DOBJ%NBS ) THEN
                             IAD = B3DOBJ%AD_ZSX + 8*(IR-1)
                        END IF
                   END IF
                ELSE IF ( FAST_MODE .EQ. F__B1B3D ) THEN
!
! ---------------- B1B3D case
!
                   IAD_DUMMY = FULL_B1B3D ( B3DOBJ, B1B3DOBJ, CNSTROBJ%NR(J12), &
     &                                      CNSTROBJ%NR(J12), TYP, NBL, IR, IC )
                   IF ( TYP .EQ. 'G' ) THEN
                        IAD = B1B3DOBJ%AD_Z00 + 8*(IR-1)
                     ELSE IF ( TYP .EQ. 'L' ) THEN
                        IAD = B1B3DOBJ%AD_ZI0 + 8*(IR-1)
                     ELSE IF ( TYP .EQ. 'C' ) THEN
                        IAD = B1B3DOBJ%AD_ZIJ(NBL) + 8*(IR-1)
                   END IF
              END IF
!
              IF ( FAST_MODE .EQ. F__B3D  .OR.  FAST_MODE .EQ. F__B1B3D ) THEN
                   IF ( IR .EQ. -1  .OR.  IC .EQ. -1 ) THEN
                        WRITE ( 6, * ) ' j6=',j6,' nR = ', cnstrobj%nr(j6), &
     &                         ' nbl=',nbl,' ir=',ir,' ic=',ic,' iad=',iad
                        WRITE ( 6, * ) ' b3dobj%ad_b0 = ',b3dobj%ad_b0
                        WRITE ( 6, * ) ' b1b3dobj%ad_w00 = ',b1b3dobj%ad_w00
                        WRITE ( 6, * ) ' cnstrobj%n_vecel = ',cnstrobj%n_vecel
                        CALL ERR_LOG ( 3438, IUER, 'APPLY_CNSTR', 'Internal '// &
     &                      'error: failure to calculate index for '// &
     &                      'corresponding elements: B3D, B1B3D <---> FULL' )
                        RETURN
                   END IF
!
                   CALL R8_UPDATE ( IAD, CNSTROBJ%VEC_UPD(J12) )
               END IF
 4120       CONTINUE
      END IF
!
      DO 4130 J13=1,CNSTROBJ%N_EQUAT
         IF ( CNSTROBJ%DYN(J13)%STS == CNS__ALC  .AND. &
     &        CNSTROBJ%DYN(J13)%L_CNS > 0              ) THEN
              DEALLOCATE ( CNSTROBJ%DYN(J13)%EQU_INE )
              DEALLOCATE ( CNSTROBJ%DYN(J13)%EQU_INP )
              DEALLOCATE ( CNSTROBJ%DYN(J13)%EQU_CNS )
              CNSTROBJ%DYN(J13)%STS   = 0
              CNSTROBJ%DYN(J13)%L_CNS = 0
         END IF
 4130 CONTINUE 
      IF ( DEBUG_STR == 'YES' ) THEN
           CLOSE ( UNIT=LUN )
      END IF
      IF ( ( DEBUG_STR == 'MAT' .OR. DEBUG_STR == 'NOR_MAT' ) .AND. &
     &     ( FAST_MODE == F__NONE .OR. FAST_MODE == F__PRD  ) ) THEN
!
! -------- Write down the normal matrix
!
           STR = 'SAVE'
           CALL UNLINK ( TRIM(OUTCGM)//CHAR(0) )
           CALL CREATE_CGMF ( STR, SOLUID_CHR, NPAR, 'M', OUTCGM )
           CALL PARCNG()
           CALL USE_CGMF_COM ( 'W' )
!
! -------- NB: a trick: USE_CGNF_MAT expectsd top get ARR array
!
           CALL USE_CGMF_MAT ( %VAL(LOC(NOR_MAT) - 8*3*M_GPA), NPAR, 'W' )
           CALL ACS_CGMFIL   ( OUTCGM, 'C' )
           IF ( DEBUG_STR == 'MAT' ) THEN
                WRITE ( 6, * ) 'Matrix of constraint is written in '//TRIM(OUTCGM)
              ELSE IF ( DEBUG_STR == 'NOR_MAT' ) THEN
                WRITE ( 6, * ) 'Updated normal matrix is written in '//TRIM(OUTCGM)
           END IF
           WRITE ( 6, * ) 'APPLY_CNSTG-750: I stop solve here'
           CALL EXIT ( 1 )
      END IF
#ifdef DEBUG
   write ( 6, * ) 'aplly_cnstr(796) ' ; call flush ( 6 ) 
#endif
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  APPLY_CNSTR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADD_CNSTR ( IND1, IND2, VAL, CNSTROBJ )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_CNSTR  adds information to the data structure         *
! *   CNSTROBJ about the indices of the normal matrix to be updated for  *
! *   the imposing constraint and the amount of the update.              *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      IND1 ( INTEGER*4 ) -- The first index of the updated element    *
! *                            (in full matrix).                         *
! *      IND2 ( INTEGER*4 ) -- The second index of the updated element   *
! *                            (in full matrix).                         *
! *       VAL ( REAL*8    ) -- The value which should be added to the    *
! *                            element (IND1,IND2) of the normal matrix  *
! *                            to apply the constraints.                 *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *  CNSTROBJ ( RECORD    ) -- The data structure with information about *
! *                            constraints (position where the matrix    *
! *                            should be modified and the value of       *
! *                            constraints).                             *
! *                                                                      *
! *  ###  19-JAN-98   ADD_CNSTR    v2.0  (c)  L. Petrov  19-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'cnstr.i'
      INTEGER*4  IND1, IND2, J1
      REAL*8     VAL, EPS
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  STR*10
      PARAMETER  ( EPS = 1.D-31 ) ! Machine zero
      INTEGER*4  I_LEN
!
      IF ( IND1 .LE. 0  .OR.  IND2 .LE. 0 ) RETURN  ! Incorrect indices
!
! --- Firstly check: does constraint have non-zero value. If yes, nothing to do
!
      IF ( DABS(VAL) .LT. EPS ) RETURN
!
      IF ( CNSTROBJ%N_MATEL .GT. 0 ) THEN
!
! -------- Secondly, scan the list of the constraints to determine whether the
! -------- element IND1, IND2 has been marked already as the element to be
! -------- modified
!
           DO 410 J1=1,CNSTROBJ%N_MATEL
              IF ( CNSTROBJ%NP1(J1) .EQ. MIN ( IND1, IND2 )  .AND. &
     &             CNSTROBJ%NP2(J1) .EQ. MAX ( IND1, IND2 )         ) THEN
!
! ---------------- Yes, we found such an element
!
                   CNSTROBJ%MAT_UPD(J1) = CNSTROBJ%MAT_UPD(J1) + VAL
                   RETURN
              END IF
 410       CONTINUE
      END IF
!
! --- No, we didn't find such an element. Create the new record of constraints
! --- data stucture
!
      CNSTROBJ%N_MATEL = CNSTROBJ%N_MATEL + 1
      IF ( CNSTROBJ%N_MATEL  .GT. MAX_CNSTR ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MAX_CNSTR, STR )
           CALL ERR_LOG ( 6666, -1, 'ADD_CNSTR', 'The number of constraints '// &
     &          'exceeded the limit '//STR(1:I_LEN(STR))//' constant '// &
     &          'MAX_CNSTR in ../solve/include/cnstr.i appeared too small' )
!
! -------- Writing pre-mortal records of constraints for diagnostic
!
           CNSTROBJ%N_MATEL = MAX_CNSTR
           CALL  WRITE_CNSTR ( CNSTROBJ, CNI__LOC, 0 )
!
           STOP 'ADD_CNSTR (PROC?/NORML?)'
      END IF
!
! --- ... and add the new element
!
      CNSTROBJ%NP1(CNSTROBJ%N_MATEL) = MIN ( IND1, IND2 )
      CNSTROBJ%NP2(CNSTROBJ%N_MATEL) = MAX ( IND1, IND2 )
      CNSTROBJ%MAT_UPD(CNSTROBJ%N_MATEL) = VAL
!
      RETURN
      END  !#!  ADD_CNSTR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADD_RP_CNSTR ( IND, VAL, CNSTROBJ )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_RP_CNSTR  adds information to the data structure      *
! *   CNSTROBJ about the index of the element od the normal vector to be *
! *   updated for imposing constraint and the amount of the update.      *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *       IND ( INTEGER*4 ) -- Index of the updated element (in full     *
! *                            vector).                                  *
! *       VAL ( REAL*8    ) -- The value which should be added to the    *
! *                            element (IND) of the normal vector to     *
! *                            apply the constraints.                    *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *  CNSTROBJ ( RECORD    ) -- The data structure with information about *
! *                            constraints (position where the matrix    *
! *                            should be modified and the value of       *
! *                            constraints).                             *
! *                                                                      *
! *  ###  11-FEB-98  ADD_RP_CNSTR   v1.0  (c) L. Petrov  11-FEB-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE 'solve.i'
      INCLUDE 'cnstr.i'
      INTEGER*4  IND, J1
      REAL*8     VAL, EPS
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  STR*10
      PARAMETER  ( EPS = 1.D-31 ) ! Machine zero
      INTEGER*4  I_LEN
!
      IF ( IND .LE. 0  ) RETURN  ! Incorrect indices
!
! --- Firstly check: does constraint have non-zero value. If yes, nothing to do
!
      IF ( DABS(VAL) .LT. EPS ) RETURN
!
      IF ( CNSTROBJ%N_VECEL .GT. 0 ) THEN
!
! -------- Secondly scan the list of the constraints to determine whether the
! -------- element IND has been marked already as the element to be
! -------- modified
!
           DO 410 J1=1,CNSTROBJ%N_VECEL
              IF ( CNSTROBJ%NR(J1) .EQ. IND ) THEN
!
! ---------------- Yes, we found such an element
!
                   CNSTROBJ%VEC_UPD(J1) = CNSTROBJ%VEC_UPD(J1) + VAL
                   RETURN
              END IF
 410       CONTINUE
      END IF
!
! --- No we didn't find such an element. Create the new record of constraints
! --- data stucture
!
      CNSTROBJ%N_VECEL = CNSTROBJ%N_VECEL + 1
      IF ( CNSTROBJ%N_VECEL .GT. MAX_VECEL ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MAX_VECEL, STR )
           CALL ERR_LOG ( 6669, -1, 'ADD_RP_CNSTR', 'The number of '// &
     &         'constraints exceeded the limit '//STR(1:I_LEN(STR))// &
     &         ' constant MAX_VECEL in ../solve/include/cnstr.i appeared '// &
     &         'too small' )
!
! -------- Writing pre-mortal records of constraints for diagnostic
!
           CNSTROBJ%N_VECEL = MAX_VECEL
           CALL  WRITE_CNSTR ( CNSTROBJ, CNI__LOC, 0 )
!
           STOP 'ADD_CNSTR (PROC?/NORML?)'
      END IF
!
! --- ... and add the new element
!
      CNSTROBJ%NR(CNSTROBJ%N_VECEL)      = IND
      CNSTROBJ%VEC_UPD(CNSTROBJ%N_VECEL) = VAL
!
      RETURN
      END  !#!  ADD_RP_CNSTR  #!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ADD_TYCNS ( ABBR, DESCR, UNITS, SIGMA, GLO, CNSTROBJ )
! ************************************************************************
! *                                                                      *
! *   Routine  ADD_TYCNS  adds information to the data structure         *
! *   CNSTROBJ about the type of the constraint applied. It compares     *
! *   the abbreviation of the constraint with abbreaviations kept in     *
! *   the fields of the object CNSTROBJ. If the constrain with the same  *
! *   type have been applied already ADD_TYCNS finishs its work. If not, *
! *   it creates a new field in CNSTROBJ and adds there information      *
! *   about the constraint.                                              *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *      ABBR ( CHARACTER ) -- Abbreviation of the constraint. It is     *
! *                            unique identifier of the constraint.      *
! *                            It has length 8 symbols.                  *
! *     DESCR ( CHARACTER ) -- Short (up to 32 simbols) descrition of    *
! *                            the constraint.                           *
! *     UNITS ( CHARACTER ) -- Line (up to 16 symbols) with units of     *
! *                            the constraint.                           *
! *     SIGMA ( REAL*8    ) -- sigma of the constrain. The value of      *
! *                            contribution toi the normal matrix due    *
! *                            to constraint is reciprocal to the        *
! *                            the square of sigma.                      *
! *       GLO ( LOGICAL*4 ) -- .TRUE. means that constrain was applied   *
! *                            the global parameters in CGM.             *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *  CNSTROBJ ( RECORD    ) -- The data structure with information about *
! *                            constraints (position where the matrix    *
! *                            should be modified and the value of       *
! *                            constraints).                             *
! *                                                                      *
! *  ###  05-FEB-98   ADD_TYCNS    v1.0  (c)  L. Petrov  05-FEB-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'cnstr.i'
      CHARACTER  ABBR*(*), DESCR*(*), UNITS*(*)
      REAL*8     SIGMA
      LOGICAL*4  GLO
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      CHARACTER  STR*10
      INTEGER*4  J1, I_LEN
!
      IF ( CNSTROBJ%N_TYCNS .GT. 0 ) THEN
!
! -------- Firstly scan the list of the constraints to determine whether the
! -------- constraint with abbreavation ABBR has been applied already
!
           DO 410 J1=1,CNSTROBJ%N_TYCNS
              IF ( CNSTROBJ%ABBR(J1) .EQ. ABBR ) THEN
!
! ---------------- Yes, we found such an constrain. Good bye!
!
                   RETURN
              END IF
 410       CONTINUE
      END IF
!
! --- No we didn't find such an element. Create the new record of constraints
! --- data stucture
!
      CNSTROBJ%N_TYCNS = CNSTROBJ%N_TYCNS + 1
      IF ( CNSTROBJ%N_TYCNS  .GT. MAX_TYCNS ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MAX_TYCNS, STR )
           CALL ERR_LOG ( 6667, -1, 'ADD_TYCNS', 'The number of constraints '// &
     &          'types exceeded the limit '//STR(1:I_LEN(STR))//' constant '// &
     &          'MAX_TYCNS in ../solve/include/cnstr.i appeared too small' )
           STOP 'ADD_TYCNS (PROC?/NORML?)'
      END IF
!
! --- ... and add the new element
!
      CALL CLRCH ( CNSTROBJ%ABBR(CNSTROBJ%N_TYCNS) )
      CNSTROBJ%ABBR(CNSTROBJ%N_TYCNS) = ABBR
!
      CALL CLRCH ( CNSTROBJ%DESCR(CNSTROBJ%N_TYCNS) )
      CNSTROBJ%DESCR(CNSTROBJ%N_TYCNS) = DESCR
!
      CALL CLRCH ( CNSTROBJ%UNITS(CNSTROBJ%N_TYCNS) )
      CNSTROBJ%UNITS(CNSTROBJ%N_TYCNS) = UNITS
!
      CNSTROBJ%SIGMA(CNSTROBJ%N_TYCNS) = SIGMA
      CNSTROBJ%GLO(CNSTROBJ%N_TYCNS)   = GLO
!
      RETURN
      END  !#!  ADD_TYCNS  #!#
