      SUBROUTINE HPE_NN_CNST ( CNS_TYPE, FL_NN_LISTING, L_PAR, C_PAR, HPE, &
     &                         CNSTROBJ, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine HPE_NN_CNST imposes net-rotation or net-translation        *
! *   constraints on cosine and sine components of harmonic site         *
! *   position variations. The current version of HPE_NN_CNST sets the   *
! *   right hand site of constraint equations to zero.                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *      CNS_TYPE ( CHARACTER ) -- Constraint type. One of               *
! *                                'HPE_NNT' -- 6 equations of           *
! *                                 no-net translation of cosine and     *
! *                                 sine amplitude of harmonic           *
! *                                 variations for X, Y and Z            *
! *                                 coordinates.                         *
! *                                                                      *
! *                                 OR                                   *
! *                                                                      *
! *                                'HPE_NNT' -- 6 equations of           *
! *                                 no-net rotation of cosine and sine   *
! *                                 amplitude of harmonic variations     *
! *                                 with resepect to axes X, Y and Z.    *
! *                                 coordinates.                         *
! * FL_NN_LISTING ( LOGICAL*4 ) -- If .TRUE. then the list of stations   *
! *                                participated in constraint equations  *
! *                                will be put in the spool-file.        *
! *         L_PAR ( INTEGER*4 ) -- The total number of global parameter. *
! *         C_PAR ( INTEGER*4 ) -- The list of global parameters.        *
! *           HPE ( RECORD    ) -- Array of records defined in           *
! *                                $PSOLVE_ROOT/include/solve.i which    *
! *                                keeps information about harmonic site *
! *                                position variations estimation.       *
! *                                Dimension: L_HPE.                     *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    CNSTROBJ  ( RECORD    ) -- Object whcih accumulates information   *
! *                               about constraints: names, coefficient  *
! *                               of constraint equations, etc.          *
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
! *  ### 25-FEB-2005  HPE_NN_CNST  v1.0 (c)  L. Petrov  25-FEB-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'prfil.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
      INTEGER*4  L_PAR, IUER
      TYPE ( B3D__STRU   ) :: B3DOBJ
      TYPE ( CNSTR__STRU ) :: CNSTROBJ
      TYPE ( HPE__TYPE   ) :: HPE(L_HPE)
      INTEGER*4    M_CON
      PARAMETER  ( M_CON = 6  )
      LOGICAL*1  FL_NN_LISTING
      LOGICAL*4  GLOBAL_FLAG
      CHARACTER  CNS_TYPE*(*), C_PAR(L_PAR)*(*)
      CHARACTER  C_STA(MAX_STA)*8, C_STA_SRT(MAX_STA)*8, &
     &           C_WAV(MAX_STA)*8, CNS_ABR*8, OUT*4096
      REAL*8     COO(3,MAX_STA), SIG(M__HPE), CONS_EQU(M_GPA,M_CON), &
     &           CONS_RHS(M_CON), REA, FACT
      PARAMETER  ( REA = 6378136.0D0 )
      CHARACTER  C_CMP(M_CON)*2
      DATA       C_CMP &
     &           / &
     &             'XC', & ! 1
     &             'YC', & ! 2
     &             'ZC', & ! 3
     &             'XS', & ! 4
     &             'YS', & ! 5
     &             'ZS'  & ! 6
     &           / 
      DATA       GLOBAL_FLAG / .TRUE. /
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, L_STA, L_WAV, &
     &           ICMP, ISTA, IWAV, IP, IER
      INTEGER*2   INT2_ARG
      INTEGER*4   INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      L_STA = 0
      L_WAV = 0
!
! --- Cycle through the list of parameters, We nened to collect the list
! --- of stations whose harmonic position variations were estimated, the
! --- apriori coordinates of these stations and the list of harmonics names
!
      DO 410 J1=1,L_PAR
         IF ( C_PAR(J1)(9:12) .EQ. ' XC ' ) THEN
!
! ----------- This parameters look like cosine X coordinate of a site position
! ----------- variations. Let's look at each harmonics
!
              DO 420 J2=1,L_HPE
                 IF ( C_PAR(J1)(13:20) == HPE(J2)%NAME ) THEN
!
! ------------------- Aga: the harmonics name coincides. Search for the station name
!
                      ISTA = LTM_DIF ( 0, L_STA, C_STA, C_PAR(J1)(1:8) )
                      IF ( ISTA .LE. 0 ) THEN
!
! ------------------------ Add the station name to the list
!
                           L_STA = L_STA + 1
                           C_STA(L_STA) = C_PAR(J1)(1:8) 
                           ISTA = LTM_DIF ( 0, INT4(NUMSTA), ISITN_CHR, &
     &                                      C_PAR(J1)(1:8) )
!
! ------------------------ Extraction coordinates of the stations
!
                           COO(1,L_STA) = VSITEC(1,ISTA)
                           COO(2,L_STA) = VSITEC(2,ISTA)
                           COO(3,L_STA) = VSITEC(3,ISTA)
                      END IF
!
! ------------------- Search for the harmonics name
!
                      IWAV = LTM_DIF ( 0, L_WAV, C_WAV, C_PAR(J1)(13:20) )
                      IF ( IWAV .LE. 0 ) THEN
!
! ------------------------ Add harmonics name to the list
!
                           L_WAV = L_WAV + 1
                           C_WAV(L_WAV) = C_PAR(J1)(13:20)
                           IF ( CNS_TYPE == 'HPE_NNT' ) THEN
                                SIG(L_WAV) = HPE(J2)%NNT_CNS_SIGMA
                              ELSE IF ( CNS_TYPE == 'HPE_NNR' ) THEN
                                SIG(L_WAV) = HPE(J2)%NNR_CNS_SIGMA
                              ELSE 
                                CALL ERR_LOG ( 8561, IUER, 'HPE_NN_CNST', &
     &                              'Unsupported constraint type: '//CNS_TYPE )
                                RETURN 
                           END IF
                      END IF
                 END IF
 420          CONTINUE 
         END IF
 410  CONTINUE 
!
! --- Now cycle over harmonics
!
      DO 430 J3=1,L_WAV
!
! ------ Zeroing constraint equations
!
         DO 440 J4=1,M_CON
            CALL NOUT_R8 ( M_GPA, CONS_EQU(1,J4) )
            CONS_RHS(J3) = 0.D0
 440     CONTINUE
!
! ------ By the way, of reciprocal weight of the constraints is zero,
! ------ it means no constraints should be applied, i.e. nothing to do
!
         IF ( SIG(J3) .LE. 0.0D0 ) GOTO  430
!
         DO 450 J5=1,L_PAR
            IF ( C_PAR(J5)(13:20)  .NE. C_WAV(J3) ) GOTO 450
            ICMP = LTM_DIF ( 1, M_CON, C_CMP, C_PAR(J5)(10:11) )
            ISTA = LTM_DIF ( 0, L_STA, C_STA, C_PAR(J5)(1:8)   )
            IF ( ICMP .LE. 0  .OR.  &
     &           ISTA .LE. 0        ) GOTO 450
!
! --------- Put the element to the NO_NET_TRANSLATION or
! --------- NO_NET_ROTATION equations
!
            FACT = REA/( COO(1,ISTA)**2 + COO(2,ISTA)**2 + COO(3,ISTA)**2 )
            IF ( CNS_TYPE == 'HPE_NNT' ) THEN
                 IF (         C_PAR(J5)(10:11) .EQ. 'XC' ) THEN
                      CONS_EQU(J5, 1) = 1.D0
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'YC' ) THEN
                      CONS_EQU(J5, 2) = 1.D0
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'ZC' ) THEN
                      CONS_EQU(J5, 3) = 1.D0
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'XS' ) THEN
                      CONS_EQU(J5, 4) = 1.D0
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'YS' ) THEN
                      CONS_EQU(J5, 5) = 1.D0
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'ZS' ) THEN
                      CONS_EQU(J5, 6) = 1.D0
                 END IF
               ELSE IF ( CNS_TYPE == 'HPE_NNR' ) THEN
                 IF (          C_PAR(J5)(10:11) .EQ. 'XC' ) THEN
                      CONS_EQU(J5, 2) =  COO(3,ISTA)*FACT
                      CONS_EQU(J5, 3) = -COO(2,ISTA)*FACT
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'YC' ) THEN
                      CONS_EQU(J5, 1) = -COO(3,ISTA)*FACT
                      CONS_EQU(J5, 3) =  COO(1,ISTA)*FACT
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'ZC' ) THEN
                      CONS_EQU(J5, 1) =  COO(2,ISTA)*FACT
                      CONS_EQU(J5, 2) = -COO(1,ISTA)*FACT
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'XS' ) THEN
                      CONS_EQU(J5, 5) =  COO(3,ISTA)*FACT
                      CONS_EQU(J5, 6) = -COO(2,ISTA)*FACT
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'YS' ) THEN
                      CONS_EQU(J5, 4) = -COO(3,ISTA)*FACT
                      CONS_EQU(J5, 6) =  COO(1,ISTA)*FACT
                    ELSE IF ( C_PAR(J5)(10:11) .EQ. 'ZS' ) THEN
                      CONS_EQU(J5, 4) =  COO(2,ISTA)*FACT
                      CONS_EQU(J5, 5) = -COO(1,ISTA)*FACT
                 END IF
            END IF
 450     CONTINUE 
!
! ------ No sycle over constraint equations
!
         DO 460 J6=1,M_CON
!
! --------- Build the constraint abbreviated name
!
            IF ( CNS_TYPE == 'HPE_NNT' ) THEN
                 CNS_ABR(1:5) = 'HPNT_'
                 CALL INCH ( J3, CNS_ABR(6:8) )
                 CALL CHASHR (   CNS_ABR(6:8) )
                 CALL BLANK_TO_ZERO ( CNS_ABR(6:8) )
               ELSE IF ( CNS_TYPE == 'HPE_NNR' ) THEN
                 CNS_ABR(1:5) = 'HPNR_'
                 CALL INCH ( J3, CNS_ABR(6:8) )
                 CALL CHASHR (   CNS_ABR(6:8) )
                 CALL BLANK_TO_ZERO ( CNS_ABR(6:8) )
            END IF
!
! --------- Insert information about constraint, name, description,
! --------- abreviation, right hand side, reciprocal weight (sigma), type
!
            CALL ERR_PASS ( IUER, IER )
            CALL ADDCNS_NAM ( CNS_ABR, J6, CNS_TYPE//'constraint for '// &
     &                        C_WAV(J3), 'meter', CONS_RHS(J3), SIG(J3), &
     &                        GLOBAL_FLAG, CNSTROBJ, IER )
            IF ( IER .NE. 0 ) THEN
                 CALL ERR_LOG ( 8562, IUER, 'HPE_NN_CNST', 'Error in '// &
     &               'an attempt to put information about '// &
     &                CNS_TYPE//C_WAV(J3)//' constraints into CNSTROBJ' )
                 RETURN
            END IF
!
! --------- Insert the coefficicent of constraint equation into
! --------- the CNSTROBJ object
!
            DO 470 J7=1,L_PAR
               IF ( CONS_EQU(J7,J6) .NE. 0.0D0 ) THEN
                    CALL ERR_PASS ( IUER, IER )
                    CALL ADDCNS_EQU ( CNS_ABR, J6, J7, CONS_EQU(J7,J6), &
     &                                GLOBAL_FLAG, CNSTROBJ, IER )
                    IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 8563, IUER, 'HPE_NN_CNST', 'Failure '// &
     &                       'in putting a coefficient of an equation of '// &
     &                       'the '//CNS_TYPE//C_WAV(J3)//' constraint' )
                         RETURN
                    END IF
               END IF
 470        CONTINUE
 460     CONTINUE
!
      IF ( FAST_DBG .EQ. F__PRI ) THEN
           WRITE ( 6, * ) ' '//CNS_ABR//'      fast_mode = ',fast_mode,' n_cnstr = ', &
     &                            cnstrobj%n_ecnst
      END IF
      IF ( FL_NN_LISTING ) THEN
!
! -------- Writing the list of stations in spool-file
!
           WRITE ( 23, '(A,I4,A)' ) CNS_ABR//': ', L_STA, ' stations '// &
     &                              'participated in '//CNS_ABR//' constraints: '
           IF ( L_STA .GT. 0 ) THEN
!
! -------------- Sorting the list of stations
!
                 CALL LIB$MOVC3 ( 8*L_STA, C_STA, C_STA_SRT )
                 CALL SORT_CH   ( L_STA, C_STA_SRT )
!
! -------------- Cleaning the output line with the list of stations
!
                 CALL CLRCH ( OUT )
                 DO 480 J8=1,L_STA
!
! ----------------- Add the J8-th station to the list of stations for further
! ----------------- printing
!
                    IP  = (J8-1)*9 + 1
                    OUT = OUT(1:IP)//C_STA_SRT(J8)
 480             CONTINUE 
!
! -------------- Writing long lines in spool-file by splitting it onto
! -------------- sub-lines no nore than 72 characters long
!
                 CALL WRITE_LONG ( 23, 72, OUT(2:) )
                 WRITE ( 23, '(A)' ) ' '
           END IF
      END IF
 430  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  HPE_NN_CNST
