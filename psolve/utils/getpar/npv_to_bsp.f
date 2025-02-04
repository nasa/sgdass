      SUBROUTINE NPV_TO_BSP ( L_NPV, C_NPV, SOL_ID, SOL_DATE, FILBSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine NPV_TO_BSP 
! *                                                                      *
! *  ### 29-OCT-2007   NPV_TO_BSP  v1.1 (c)  L. Petrov  04-OCT-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INTEGER*4  L_NPV, IUER
      CHARACTER  C_NPV(*)*(*), SOL_ID*(*), SOL_DATE*(*), FILBSP*(*)
      INTEGER*4  M_HLP, MC
      PARAMETER  ( M_HLP = 16*1024 )
      PARAMETER  ( MC    = (9*(VTD__M_SPD+M__SPE)*(VTD__M_SPD+M__SPE))/2 )
      CHARACTER  C_STA(M__SPE)*8, COO_STR(3,M__SPE)*14, &
     &           VEL_STR(3,M__SPE)*14, STR*128, REF_EPOCH_STR(M__SPE)*23, &
     &           EPOCH_STR(1-M__SPD:M__SPN,M__SPE)*23, SOLVE_HELP_DIR_STR*128, &
     &           HELP_FILE*128, HELP_BUF(M_HLP)*128
      LOGICAL*4  FL_SPE 
      REAL*8     BSPL_EST(3,1-M__SPD:M__SPN,M__SPE), PHI_GCN, PHI_GDT, &
     &           LAMBDA, H_ELL, RD, G_ACC, P_EST(3,M__SPE), V_EST(3,M__SPE), &
     &           VAL, COO(3), VEL(3), COV_VAL
      REAL*8,    ALLOCATABLE :: COV(:,:,:,:)
      INTEGER*4  IDEG(M__SPE), NNOD(M__SPE), IND_NOD(MC), IND_CMP(MC)
      INTEGER*4  L_STA, I_STA, I_CMP, LUN, J1, J2, J3, J4, J5, J6, J7, J8, &
     &           J9, J10, I_NOD, N_HLP, IND_NOD1, IND_NOD2, &
     &           IND_CMP1, IND_CMP2, IND_EQU1, IND_EQU2, LOC_IND, &
     &           LOC_IND1, LOC_IND2, IER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ADD_CLIST, GET_UNIT, IFIND_PL, I_LEN, ILEN, LTM_DIF
      INTEGER*4  LOCS, I, J, LOCI, L_NOD, L_DEG
      LOCS(I,J)   = min(I,J) +(max(I,J)*(max(I,J)-1))/2
      LOCI(I_NOD,I_CMP,L_NOD,L_DEG) = (I_NOD+L_DEG) + (L_NOD+L_DEG+1)*(I_CMP-1)
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILBSP, STATUS='UNKNOWN', IOSTAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4571, IUER, 'NPV_TO_BSP', 'Error in opening '// &
     &         'output file with B-spline model for site position '// &
     &         'variations '//FILBSP )
           RETURN 
      END IF
!
      L_STA = 0
      I_STA = 0
      FL_SPE = .FALSE. 
      ALLOCATE ( COV(MC,3,3,M__SPE) )
      DO 410 J1=1,L_NPV
         IF ( C_NPV(J1)(1:5) == 'L_SPE' ) THEN
              FL_SPE = .TRUE.
              GOTO 410
         END IF
         IF ( .NOT. FL_SPE ) GOTO 410
         IF ( C_NPV(J1)(4:7) == 'APS:'  ) THEN
              CALL VTD_NAME_REPAIR ( C_NPV(J1)(9:16) )
              CALL ERR_PASS ( IUER, IER ) 
              I_STA = ADD_CLIST ( M__SPE, L_STA, C_STA, C_NPV(J1)(9:16), &
     &                            IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4572, IUER, 'NPV_TO_BSP', 'TRap of '// &
     &                 'internal control: too many stations. Please, '//&
     &                 'increase parameter M__SPE in solve.templ' )
                   DEALLOCATE ( COV )
                   RETURN 
              END IF
              I_CMP = INDEX ( 'XYZ', C_NPV(J1)(18:18) )
              COO_STR(I_CMP,I_STA) = C_NPV(J1)(79:92)
              REF_EPOCH_STR(I_STA) = C_NPV(J1)(51:73)
            ELSE IF ( C_NPV(J1)(4:7) == 'APV:'  ) THEN
              CALL VTD_NAME_REPAIR ( C_NPV(J1)(9:16) )
              I_STA = LTM_DIF ( 0, L_STA, C_STA, C_NPV(J1)(9:16) )
              I_CMP = INDEX ( 'XYZ', C_NPV(J1)(18:18) )
              VEL_STR(I_CMP,I_STA) = C_NPV(J1)(79:92)
            ELSE IF ( C_NPV(J1)(1:8) == '   I_SPE'  ) THEN
!@              CALL CHIN ( C_NPV(J1)(10:13), I_STA )
              I_STA = I_STA + 1
              CALL CHIN ( C_NPV(J1)(22:25), NNOD(I_STA) )
              CALL CHIN ( C_NPV(J1)(34:37), IDEG(I_STA) )
!
! ----------- Initialiazation
!
              CALL NOUT_I4 ( MC, IND_NOD )
              CALL NOUT_I4 ( MC, IND_CMP )
              CALL NOUT_R8 ( MC*3*3, COV(1,1,1,I_STA) )
            ELSE IF ( C_NPV(J1)(1:7) == '   EST:'  ) THEN
              CALL VTD_NAME_REPAIR ( C_NPV(J1)(9:16) )
              I_STA = LTM_DIF ( 0, L_STA, C_STA, C_NPV(J1)(9:16) )
              CALL CHIN ( C_NPV(J1)(42:46), I_NOD )
              EPOCH_STR(I_NOD,I_STA) = C_NPV(J1)(51:73)
              I_CMP = INDEX ( 'XYZ', C_NPV(J1)(18:18) )
              LOC_IND = LOCI(I_NOD,I_CMP,NNOD(I_STA),IDEG(I_STA))
              IF ( LOC_IND .LE. 0 ) THEN
                   WRITE ( 6, * ) ' I_STA = ', I_STA, ' NNOD(I_STA), IDEG(I_STA) = ', NNOD(I_STA) , IDEG(I_STA)
                   WRITE ( UNIT=STR, FMT=210 ) J1
 210               FORMAT ( 'WARNING: error in processing line ', I6, &
     &                      ' of npv-file: wrong index ' )
                   CALL ERR_LOG ( 4573, IUER, 'NPV_TO_BSP', STR(1:I_LEN(STR))// &
     &                  ' '//C_NPV(J1) )
                   DEALLOCATE ( COV )
                   RETURN 
              END IF
              CALL CHIN ( C_NPV(J1)(33:37), IND_NOD(LOC_IND) )
              IND_CMP(LOC_IND) = I_CMP
              READ ( UNIT=C_NPV(J1)(79:92), FMT='(F14.4)' ) BSPL_EST(I_CMP,I_NOD,I_STA)
            ELSE IF ( C_NPV(J1)(1:7) == '   NOD:'  ) THEN
              CALL VTD_NAME_REPAIR ( C_NPV(J1)(9:16) )
              I_STA = LTM_DIF ( 0, L_STA, C_STA, C_NPV(J1)(9:16) )
              CALL CHIN ( C_NPV(J1)(42:46), I_NOD )
              IF ( I_STA .LE. 0 ) THEN
                   GOTO 410 ! For backward compatibility
              END IF
              EPOCH_STR(I_NOD,I_STA) = C_NPV(J1)(51:73)
            ELSE IF ( C_NPV(J1)(1:7) == '   POS:'  ) THEN
              CALL VTD_NAME_REPAIR ( C_NPV(J1)(9:16) )
              I_STA = LTM_DIF ( 0, L_STA, C_STA, C_NPV(J1)(9:16) )
              I_CMP = INDEX ( 'XYZ', C_NPV(J1)(18:18) )
              READ ( UNIT=C_NPV(J1)(79:92), FMT='(F14.4)' ) VAL
              READ ( UNIT=COO_STR(I_CMP,I_STA), FMT='(F14.4)' ) COO(I_CMP)
              P_EST(I_CMP,I_STA) = VAL + COO(I_CMP)
              LOC_IND = LOCI(NNOD(I_STA),I_CMP,NNOD(I_STA),IDEG(I_STA))
              IF ( LOC_IND .LE. 0 ) THEN
                   WRITE ( UNIT=STR, FMT=210 ) J1
                   CALL ERR_LOG ( 4574, IUER, 'NPV_TO_BSP', STR(1:I_LEN(STR))// &
     &                  ' '//C_NPV(J1) )
                   DEALLOCATE ( COV )
                   RETURN 
              END IF
              CALL CHIN ( C_NPV(J1)(33:37), IND_NOD(LOC_IND) )
              IND_CMP(LOC_IND) = I_CMP
            ELSE IF ( C_NPV(J1)(1:7) == '   VEL:'  ) THEN
              CALL VTD_NAME_REPAIR ( C_NPV(J1)(9:16) )
              I_STA = LTM_DIF ( 0, L_STA, C_STA, C_NPV(J1)(9:16) )
              I_CMP = INDEX ( 'XYZ', C_NPV(J1)(18:18) )
              READ ( UNIT=C_NPV(J1)(79:92), FMT='(F14.4)' ) VAL
              READ ( UNIT=VEL_STR(I_CMP,I_STA), FMT='(F14.4)' ) VEL(I_CMP)
              V_EST(I_CMP,I_STA) = VAL + VEL(I_CMP)
              LOC_IND = LOCI(NNOD(I_STA)+1,I_CMP,NNOD(I_STA),IDEG(I_STA))
              IF ( LOC_IND .LE. 0 ) THEN
                   WRITE ( UNIT=STR, FMT=210 ) J1
                   CALL ERR_LOG ( 4575, IUER, 'NPV_TO_BSP', STR(1:I_LEN(STR))// &
     &                  ' '//C_NPV(J1) )
                   DEALLOCATE ( COV )
                   RETURN 
              END IF
              CALL CHIN ( C_NPV(J1)(33:37), IND_NOD(LOC_IND) )
              IND_CMP(LOC_IND) = I_CMP
            ELSE IF ( C_NPV(J1)(1:7) == '   COV:'  ) THEN
              I_STA = LTM_DIF ( 0, L_STA, C_STA, C_NPV(J1)(9:16) )
              CALL CHIN ( C_NPV(J1)(24:28), IND_EQU1 )
              CALL CHIN ( C_NPV(J1)(36:40), IND_EQU2 ) 
              IF ( I_STA .LE. 0  ) THEN
                   WRITE ( UNIT=STR, FMT=210 ) J1
                   CALL ERR_LOG ( 4576, IUER, 'NPV_TO_BSP', STR(1:I_LEN(STR))// &
     &                  ' '//C_NPV(J1) )
                   DEALLOCATE ( COV )
                   RETURN 
              END IF
              IND_NOD1 = IFIND_PL ( 3*(NNOD(I_STA)+IDEG(I_STA)+1), IND_NOD, &
     &                              IND_EQU1 )
              IND_NOD2 = IFIND_PL ( 3*(NNOD(I_STA)+IDEG(I_STA)+1), IND_NOD, &
     &                              IND_EQU2 )
              IF ( IND_NOD1 .LE. 0  .OR.  IND_NOD2 .LE. 0 ) THEN
                   WRITE ( UNIT=STR, FMT=210 ) J1, C_NPV(J1)
                   CALL ERR_LOG ( 4577, IUER, 'NPV_TO_BSP', STR(1:I_LEN(STR))// &
     &                  ' '//C_NPV(J1) )
                   DEALLOCATE ( COV )
                   RETURN 
              END IF
!
              IND_CMP1 = IND_CMP(IND_NOD1)
              IND_CMP2 = IND_CMP(IND_NOD2)
              IF ( IND_CMP1 .LE. 0  .OR.  IND_CMP2 .LE. 0 ) THEN
                   WRITE ( UNIT=STR, FMT=210 ) J1, C_NPV(J1)
                   CALL ERR_LOG ( 4578, IUER, 'NPV_TO_BSP', STR(1:I_LEN(STR))// &
     &                  ' '//C_NPV(J1) )
                   DEALLOCATE ( COV )
                   RETURN 
              END IF
              READ ( UNIT=C_NPV(J1)(45:63), FMT='(F19.12)' ) &
     &               COV(LOCS(IND_NOD1,IND_NOD2),IND_CMP1,IND_CMP2,I_STA)
         END IF
 410  CONTINUE 
!
      IF ( L_STA == 0 ) THEN
           WRITE ( LUN, FMT='(A)' ) BSPPOS__LABEL 
           WRITE ( LUN, FMT='(A)' ) '# '
           WRITE ( LUN, FMT='(A)' ) '# No position variations available'
           WRITE ( LUN, FMT='(A)' ) '# '
           WRITE ( LUN, FMT='(A,I4)' ) 'L_STA: ', L_STA
           WRITE ( LUN, FMT='(A)' ) BSPPOS__LABEL  
           CLOSE ( UNIT=LUN )
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      WRITE ( LUN, FMT='(A)' ) BSPPOS__LABEL  
      WRITE ( LUN, FMT='(A)' ) '# '
      WRITE ( LUN, FMT='(A)' ) '# This file was generated on '//GET_CDATE()
      WRITE ( LUN, FMT='(A)' ) '# '
      WRITE ( LUN, FMT='(A)' ) '# Model of site positions as a sum of the 1st order polynomial'
      WRITE ( LUN, FMT='(A)' ) '# and an expansion over the B-spline basis from the VLBI solution'
      WRITE ( LUN, FMT='(A)' ) '# '
      WRITE ( LUN, FMT='(A)' ) 'SOL_ID:   '//SOL_ID(1:I_LEN(SOL_ID))
      WRITE ( LUN, FMT='(A)' ) 'SOL_DATE: '//SOL_DATE(1:I_LEN(SOL_DATE))
      WRITE ( LUN, FMT='(A)' ) '# '
      WRITE ( LUN, FMT='(A,I4)' ) 'N_STA: ', L_STA
      WRITE ( LUN, FMT='(A)' ) '#'
!
! --- Get directory with help files
!
      CALL GETENVAR ( 'PSOLVE_HELP_DIR', SOLVE_HELP_DIR_STR )
      IF ( ILEN(SOLVE_HELP_DIR_STR) .LE. 0 ) THEN
           SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR
      END IF
      IF ( SOLVE_HELP_DIR_STR(I_LEN(SOLVE_HELP_DIR_STR):I_LEN(SOLVE_HELP_DIR_STR)) .NE. '/' ) THEN
           SOLVE_HELP_DIR_STR = SOLVE_HELP_DIR_STR(1:I_LEN(SOLVE_HELP_DIR_STR))//'/'
      END IF
!
      HELP_FILE = SOLVE_HELP_DIR_STR(1:I_LEN(SOLVE_HELP_DIR_STR))//BSPPOS_FORMAT_HELP
      CALL ERR_PASS ( IUER, IER ) 
      CALL RD_TEXT ( HELP_FILE, M_HLP, HELP_BUF, N_HLP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4579, IUER, 'NPV_TO_BSP', 'Failure in an '// &
     &         'attempt to read help file '//HELP_FILE )
           RETURN 
      END IF
!
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#============================ Beginning of format description: ================='
      WRITE ( LUN, FMT='(A)' ) '#'
!
      DO 420 J2=1,N_HLP
         WRITE ( LUN, FMT='(A)' ) '# '//HELP_BUF(J2)(1:I_LEN(HELP_BUF(J2)))
 420  CONTINUE 
!
      WRITE ( LUN, FMT='(A)' ) '#'
      WRITE ( LUN, FMT='(A)' ) '#============================ End of format description: ======================='
      WRITE ( LUN, FMT='(A)' ) '#'
!
      DO 430 J3=1,L_STA
         READ ( UNIT=COO_STR(1,J3), FMT='(F14.4)' ) COO(1)
         READ ( UNIT=COO_STR(2,J3), FMT='(F14.4)' ) COO(2)
         READ ( UNIT=COO_STR(3,J3), FMT='(F14.4)' ) COO(3)
         CALL REF_ELL ( 0, COO, PHI_GCN, PHI_GDT, LAMBDA, H_ELL, RD, G_ACC )
         CALL CLRCH ( STR )
         STR(1:2) = 'S: ' 
         STR(4:11) = C_STA(J3)
         WRITE ( LUN, FMT=110 ) C_STA(J3), COO, PHI_GCN/DEG__TO__RAD, &
     &                          LAMBDA/DEG__TO__RAD,  H_ELL
 110     FORMAT ( 'S: ',A,2X,3(F13.4,1X), 1X, F8.4, 1X, F8.4, 1X, F6.1 )
 430  CONTINUE 
      WRITE ( LUN, FMT='(A)' ) '#'
      DO 440 J4=1,L_STA
         WRITE ( LUN, FMT=120 ) IDEG(J4), J4, C_STA(J4)
         WRITE ( LUN, FMT=130 ) NNOD(J4), J4, C_STA(J4)
 120     FORMAT ( 'L_DEG:  ',I3, 2X, 'STA: ', I4, 2X, A )
 130     FORMAT ( 'N_NOD:  ',I3, 2X, 'STA: ', I4, 2X, A )
         WRITE ( LUN, FMT=140 ) J4, C_STA(J4), REF_EPOCH_STR(J4)
         WRITE ( LUN, FMT=150 ) J4, C_STA(J4), P_EST(1:3,J4)
         WRITE ( LUN, FMT=160 ) J4, C_STA(J4), V_EST(1:3,J4)
 140     FORMAT ( 'R_EPC:       STA: ', I4, 2X, A, 2X, A ) 
 150     FORMAT ( 'P_EST:       STA: ', I4, 2X, A, 1X, 3(1X,F14.5)   )
 160     FORMAT ( 'P_VEL:       STA: ', I4, 2X, A, 1X, 3(1X,1PD14.6) )
         DO 450 J5=1-IDEG(J4),NNOD(J4)
            WRITE ( LUN, FMT=170 ) J5, J4, C_STA(J4), EPOCH_STR(J5,J4)
 170        FORMAT ( 'EPOCH:  ', I3, 2X, 'STA: ', I4, 2X, A, 2X, A ) 
 450     CONTINUE 
!
         DO 460 J6=1-IDEG(J4),NNOD(J4)
            IF ( J6 == NNOD(J4) ) THEN
                 BSPL_EST(1,J6,J4) = 0.0D0
                 BSPL_EST(2,J6,J4) = 0.0D0
                 BSPL_EST(3,J6,J4) = 0.0D0
            END IF
            WRITE ( LUN, FMT=180 ) J6, J4, C_STA(J4), BSPL_EST(1:3,J6,J4) 
 180        FORMAT ( 'B_SPL:  ', I3, 2X, 'STA: ', I4, 2X, A, 3X, 3(F13.6,1X) )
 460     CONTINUE 
!
         DO 470 J7=1-IDEG(J4),NNOD(J4)+1
            DO 480 J8=J7,NNOD(J4)+1
               DO 490 J9=1,3
                  LOC_IND1 = LOCI(J7,J9,NNOD(J4),IDEG(J4))
                  DO 4100 J10=1,3
                     LOC_IND2 = LOCI(J8,J10,NNOD(J4),IDEG(J4))
                     IF ( J10 .LE. J9 ) THEN
                          COV_VAL = COV(LOCS(LOC_IND1,LOC_IND2),J10,J9,J4)
                        ELSE 
                          COV_VAL = COV(LOCS(LOC_IND1,LOC_IND2),J9,J10,J4)
                     END IF
                     WRITE ( LUN, FMT=190 ) J4, C_STA(J4), J9, J7, J10, J8, &
     &                                      COV_VAL
 190                 FORMAT ( 'B_COV:  ', 5X, 'STA: ', I4, 2X, A, 1X, &
     &                        ' I1_CMP: ',I1,' I1_NOD: ',I4,' I2_CMP: ',I1, &
     &                        ' I2_NOD: ',I4,' COV: ', 1PD14.6 )
 4100             CONTINUE 
 490           CONTINUE 
 480        CONTINUE 
 470     CONTINUE 
         WRITE ( LUN, FMT='(A)' ) '#'
 440  CONTINUE 
!
      WRITE ( LUN, FMT='(A)' ) BSPPOS__LABEL  
      CLOSE ( UNIT=LUN )
      DEALLOCATE ( COV )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  NPV_TO_BSP  !#!#
