      PROGRAM    SRC_ALIGNMENT_MAIN 
! ************************************************************************
! *                                                                      *
! *   Program 
! *                                                                      *
! * ## 26-FEB-2023 SRC_ALIGNMENT_MAIN v1.0 (c) L. Petrov  27-FEB-2023 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      TYPE       ( SOURCE_CAT__TYPE ) :: CAT_APR(MAX_SOU), CAT_REF(MAX_SOU), CAT_EST(MAX_SOU)
      INTEGER*4  M_VTD, MIND
      PARAMETER  ( MIND  =  64 )
      CHARACTER  FIL_SOL*128, FIL_CNT*128, FIL_APR*128, FIL_REF*128, FIL_EST*128, FIL_VTD*128, &
     &           FIL_CNS*128, FIL_NNR_CNS_OUT*128, CA_SOU(MAX_SOU)*8, CR_SOU(MAX_SOU)*8, &
     &           CE_SOU(MAX_SOU)*8, CC_SOU(MAX_SOU)*8, OUT(MAX_SOU)*256, ARG_STR*1024, &
     &           SOL_ID*64, STR*128, STRL*4096, SOLNAME*128, BUF(MAX_SOU)*256
      CHARACTER  GET_VERSION*54
      REAL*8     SIG_NNR
      LOGICAL*1  LEX, FL_REF(MAX_SOU), FL_NNR_SRC, FL_EST
      INTEGER*4  NUMOBS_MIN, LA_SOU, LR_SOU, LE_SOU, LC_SOU, MDC, NO, IP, IL, &
     &           J1, J2, J3, J4, J5, J6, J7, N_CNT, N_CNS, N_VTD, N_APR, LIND, &
     &           IND(2,MIND), IVRB, IUER
      INTEGER*4, EXTERNAL ::  ILEN, I_LEN, LINDEX
      CHARACTER, EXTERNAL ::  GET_CDATE*19
!
      INCLUDE   'src_alignment_version.i'
      IVRB = 1
      SIG_NNR = 1.D-10
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: src_alignment generic_solution_name ref_sou_file numobs_min [ivrb]'
           CALL EXIT ( 1 ) 
         ELSE
           CALL GETARG ( 1, FIL_SOL ) 
           CALL GETARG ( 2, FIL_REF ) 
           CALL GETARG ( 3, STR     ) 
           CALL CHIN ( STR , NUMOBS_MIN )
           IF ( IARGC() .GE. 4 ) THEN
                CALL GETARG ( 4, STR     ) 
                CALL CHIN   ( STR, IVRB  )
           END IF
      END IF
      CALL GET_COMMAND ( ARG_STR )
!
! --- Extract solution name from the input arguments
!
      CALL CLRCH  ( SOLNAME )
      SOLNAME = FIL_SOL
      IL = LINDEX ( SOLNAME, '/' )
      IF ( IL .GT. 0 ) THEN
           CALL CLRCH  ( SOLNAME(1:IL) )
           CALL CHASHL ( SOLNAME )
      END IF
      IP = INDEX ( SOLNAME, '.' )
      IF ( IP .GT. 0 ) CALL CLRCH  ( SOLNAME(IP:) )
      FIL_CNT = TRIM(FIL_SOL)//'.cnt'
      FIL_VTD = TRIM(FIL_SOL)//'.vtd'
      FIL_EST = TRIM(FIL_SOL)//'.sou'
      FIL_CNS = TRIM(FIL_SOL)//'.cns'
!
      INQUIRE ( FILE=FIL_EST, EXIST=FL_EST )
      INQUIRE ( FILE=FIL_CNT, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 7301, IUER, 'SRC_ALIGNMENT_MAIN', 'Trap of internal control: '// &
     &         'control file file '//TRIM(FIL_CNT)//' derived from the generic '// &
     &         'solution name '//TRIM(FIL_SOL)//' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_CNT, MAX_SOU, BUF, N_CNT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7302, IUER, 'SRC_ALIGNMENT_MAIN', 'Failure in reading '// &
     &         'VTD file '//FIL_VTD )
           CALL EXIT ( 1 )
      END IF
!
      CALL CLRCH ( FIL_VTD )
      LC_SOU = 0
      DO 410 J1=1,N_CNT
         IF ( BUF(J1)(1:1) == '#' ) GOTO 410
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'VTD_CONF' .AND. LIND > 1 ) THEN
              FIL_VTD = BUF(J1)(IND(1,2):IND(2,2))
         END IF
         IF ( NUMOBS_MIN == 0 ) THEN
              IF ( INDEX ( BUF(J1), 'NO_NET_ROTATION_SOURCE' ) > 0 ) THEN
                   FL_NNR_SRC = .TRUE.
                   GOTO 410
              END IF 
              IF ( BUF(J1)(1:1) == '$' ) THEN
                   FL_NNR_SRC = .FALSE.
              END IF 
              IF ( FL_NNR_SRC ) THEN
                   IF ( BUF(J1)(IND(1,1):IND(2,1)) == 'RIGHT_PART' ) GOTO 410
                   IF ( BUF(J1)(IND(1,1):IND(2,1)) == '*'          ) GOTO 410
                   CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
                   DO 420 J2=1,LIND
                      IF ( BUF(J1)(IND(1,J2):IND(1,J2)+1) == '\' ) GOTO 420
                      LC_SOU = LC_SOU + 1
                      CC_SOU(LC_SOU) = BUF(J1)(IND(1,J2):IND(1,J2)+7)
 420               CONTINUE 
              END IF
         END IF
 410  CONTINUE 
!
      IF ( ILEN(FIL_VTD) == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7303, IUER, 'SRC_ALIGNMENT_MAIN', 'Trap of internal '// &
     &         'control: source apriori file was not found in the solution '// &
     &         'control file '//FIL_CNT )
           CALL EXIT ( 1 )
      END IF
!
      INQUIRE ( FILE=FIL_VTD, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 7304, IUER, 'SRC_ALIGNMENT_MAIN', 'Trap of internal control: '// &
     &         'aprior VTD file '//TRIM(FIL_VTD)//' derived friom the generic '// &
     &         'solution name '//TRIM(FIL_SOL)//' does not exist' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_VTD, MAX_SOU, BUF, N_VTD, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7305, IUER, 'SRC_ALIGNMENT_MAIN', 'Failure in reading '// &
     &         'VTD file '//FIL_VTD )
           CALL EXIT ( 1 )
      END IF
!
      CALL CLRCH ( FIL_APR )
      DO 430 J3=1,N_VTD
         IF ( BUF(J3)(1:1) == '#' ) GOTO 430
         CALL EXWORD ( BUF(J3), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
         IF ( BUF(J3)(IND(1,1):IND(2,1)) == 'SOURCE_COORDINATES:' .AND. LIND > 1 ) THEN
              FIL_APR = BUF(J3)(IND(1,2):IND(2,2))
         END IF
 430  CONTINUE 
!
      IF ( ILEN(FIL_APR) == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7306, IUER, 'SRC_ALIGNMENT_MAIN', 'Trap of internal '// &
     &         'control: source apriori file was not found in the VTD '// &
     &         'control file '//FIL_APR )
           CALL EXIT ( 1 )
      END IF
      INQUIRE ( FILE=FIL_APR, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 7307, IUER, 'SRC_ALIGNMENT_MAIN', 'Wrong 1st argument: '// &
     &         'cannot find apriori source position file '//TRIM(FIL_APR)// &
     &         ' specitied in the VTD file '//FIL_VTD )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_APR, MAX_SOU, BUF, N_APR, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7308, IUER, 'SRC_ALIGNMENT_MAIN', 'Failure in reading '// &
     &         'VTD file '//FIL_VTD )
           CALL EXIT ( 1 )
      END IF
!
      CALL CLRCH ( SOL_ID )
      DO 440 J4=1,N_APR
         IF ( BUF(J4)(1:1) == '#' ) THEN
              CALL EXWORD ( BUF(J4), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
              IF ( BUF(J4)(IND(1,2):IND(2,2)) == 'Soluition' .AND. &
     &             BUF(J4)(IND(1,3):IND(2,3)) == 'ID:'             ) THEN
                   SOL_ID = BUF(J4)(IND(1,4):IND(2,4))
              END IF
         END IF
 440  CONTINUE 
      IF ( ILEN(SOL_ID) == 0 ) SOL_ID = SOLNAME
      FIL_NNR_CNS_OUT  = '/tmp/'//TRIM(SOL_ID)//'_net_sou_cns.cnt'
!
      INQUIRE ( FILE=FIL_REF, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           IUER = -1
           CALL ERR_LOG ( 7309, IUER, 'SRC_ALIGNMENT_MAIN', 'Wrong 2nd argument: '// &
     &         'cannot find reference source position file '//FIL_REF )
           CALL EXIT ( 1 )
      END IF
!
      IF ( NUMOBS_MIN < 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7310, IUER, 'SRC_ALIGNMENT_MAIN', 'Wrong 3rd argument: '// &
     &         ' the minimum number of source should be a non-negative ineger' )
           CALL EXIT ( 1 )
        ELSE IF ( NUMOBS_MIN > 0 ) THEN
           IF ( .NOT. FL_EST ) THEN
                IUER = -1
                CALL ERR_LOG ( 7311, IUER, 'SRC_ALIGNMENT_MAIN', 'Wrong 2nd '// &
     &              'argument: cannot find estimated source position '// &
     &              'file '//FIL_EST )
                CALL EXIT ( 1 )
           END IF
      END IF
!
      IUER = -1
      CALL READ_SOU ( FIL_APR, MAX_SOU, LA_SOU, CAT_APR, CA_SOU, MDC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7312, IUER, 'SRC_ALIGNMENT_MAIN', 'Error in reading '// &
     &         'apriori source position catalogue '//FIL_APR )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL READ_SOU ( FIL_REF, MAX_SOU, LR_SOU, CAT_REF, CR_SOU, MDC, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7313, IUER, 'SRC_ALIGNMENT_MAIN', 'Error in reading '// &
     &         'reference source position catalogue '//FIL_REF )
           CALL EXIT ( 1 )
      END IF
!
      IF ( FL_EST ) THEN
           IUER = -1
           CALL READ_SOU ( FIL_EST, MAX_SOU, LE_SOU, CAT_EST, CE_SOU, MDC, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7314, IUER, 'SRC_ALIGNMENT_MAIN', 'Error in reading '// &
     &              'estimated source positions file '//FIL_EST )
                CALL EXIT ( 1 )
           END IF
         ELSE
           LE_SOU = LR_SOU
           CE_SOU = CR_SOU
           CAT_EST = CAT_REF
      END IF
!
      INQUIRE ( FILE=FIL_CNS, EXIST=LEX )
      IF ( NUMOBS_MIN == 0 .AND. LEX ) THEN
           IUER = -1
           CALL RD_TEXT ( FIL_CNS, MAX_SOU, BUF, N_CNS, IUER )
           IF ( IUER .NE. 0 ) THEN
                IUER = -1
                CALL ERR_LOG ( 7315, IUER, 'SRC_ALIGNMENT_MAIN', 'Failure in reading '// &
     &              'VTD file '//FIL_VTD )
                CALL EXIT ( 1 )
           END IF
           FL_NNR_SRC = .FALSE.
           LC_SOU = 0
           DO 450 J5=1,N_CNS
              IF ( INDEX ( BUF(J5), 'NNR_SRC:' ) > 0 ) THEN
                   FL_NNR_SRC = .TRUE.
                   GOTO 450
              END IF 
              IF ( INDEX ( BUF(J5), 'NNR_SRC sigma:' ) > 0 ) THEN
                   FL_NNR_SRC = .FALSE.
              END IF 
              IF ( FL_NNR_SRC ) THEN
                   CALL EXWORD ( BUF(J5), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IUER )
                   DO 460 J6=1,LIND
                      LC_SOU = LC_SOU + 1
                      CC_SOU(LC_SOU) = BUF(J5)(IND(1,J6):IND(1,J6)+7)
 460               CONTINUE 
              END IF
 450       CONTINUE 
      END IF
      NO = 0 
      NO = NO + 1 ; OUT(NO) = '*'
      NO = NO + 1 ; OUT(NO) = '* '//TRIM(ARG_STR)
      NO = NO + 1 ; OUT(NO) = '* '//TRIM(GET_VERSION())
      NO = NO + 1 ; OUT(NO) = '* VLBI solution ID: '//TRIM(SOL_ID)
      IF ( FL_EST ) THEN
           NO = NO + 1 ; OUT(NO) = '* A posteriori source catalogue:    '//TRIM(FIL_EST)
         ELSE
           NO = NO + 1 ; OUT(NO) = '* A posteriori source catalogue:    none'
      END IF
      NO = NO + 1 ; OUT(NO) = '* A priori     source catalogue:    '//TRIM(FIL_APR)
      NO = NO + 1 ; OUT(NO) = '* Reference    source catalogue:    '//TRIM(FIL_REF)
      NO = NO + 1 ; OUT(NO) = '* Constraint file was generated on: '//GET_CDATE()
      NO = NO + 1 ; OUT(NO) = '*'
!
      IUER = -1
      CALL SRC_ALIGNMENT ( LA_SOU, LR_SOU, LE_SOU, CA_SOU, CR_SOU, CE_SOU, &
     &                     CAT_APR, CAT_REF, CAT_EST, FL_EST, &
     &                     SOL_ID, NUMOBS_MIN, FIL_APR, SIG_NNR, &
     &                     LC_SOU, CC_SOU, IVRB, NO, OUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 7316, IUER, 'SRC_ALIGNMENT_MAIN', 'Error in an attempt'// &
     &         ' to compute the right hand side of NNR-SRC constraints' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL WR_TEXT ( NO, OUT, FIL_NNR_CNS_OUT, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( IVRB > 0 ) THEN
           DO 470 J7=1,20
              WRITE ( 6, '(A)' ) TRIM(OUT(J7))
 470       CONTINUE 
           WRITE ( 6, '(A)' ) ' '
           WRITE ( 6, '(A)' ) 'Created NNR-SRC  right-hand side constraint file '// &
     &                         TRIM(FIL_NNR_CNS_OUT)
      END IF
!
      END  PROGRAM  SRC_ALIGNMENT_MAIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE SRC_ALIGNMENT ( LA_SOU, LR_SOU, LE_SOU, CA_SOU, CR_SOU, CE_SOU, &
     &                           CAT_APR, CAT_REF, CAT_EST, FL_EST, &
     &                           SOL_ID, NUMOBS_MIN, FIL_APR, SIG_NNR, &
     &                           LC_SOU, CC_SOU, IVRB, NO, OUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine SRC_ALIGNMENT
! *                                                                      *
! * ### 27-FEB-2023  SRC_ALIGNMENT  v1.0 (c)  L. Petrov  27-FEB-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'getpar.i'
      TYPE       ( SOURCE_CAT__TYPE ) :: CAT_APR(MAX_SOU), CAT_REF(MAX_SOU), CAT_EST(MAX_SOU)
      REAL*8     SIG_NNR
      LOGICAL*1  FL_EST
      CHARACTER  CA_SOU(MAX_SOU)*(*), CR_SOU(MAX_SOU)*(*), CE_SOU(MAX_SOU)*(*), &
     &           CC_SOU(MAX_SOU)*(*), OUT(MAX_SOU)*(*), FIL_APR*(*), SOL_ID*(*)
      INTEGER*4  LA_SOU, LR_SOU, LE_SOU, LC_SOU, NUMOBS_MIN, IVRB, NO, IUER
      REAL*8     ALP_REF(MAX_SOU), DEL_REF(MAX_SOU), E_ALP_REF(MAX_SOU), E_DEL_REF(MAX_SOU)
      REAL*8     D_RA_ALP(MAX_SOU), D_RA_DEL(MAX_SOU), D_EA_ALP(MAX_SOU), D_EA_DEL(MAX_SOU), &
     &           D_ER_ALP(MAX_SOU), D_ER_DEL(MAX_SOU)
      REAL*8     PSI_RA(3), PSI_EA(3), PSI_ER(3), ROT_RA(3), ROT_EA(3), ROT_ER(3), &
     &           E_ROT_RA(3), E_ROT_EA(3), E_ROT_ER(3)
      REAL*8     ERR_FLOOR
      CHARACTER  CAM_SOU(MAX_SOU)*8, STR*128, STRL*2048
      LOGICAL*1  LEX, FL_REF(MAX_SOU), FL_USED_CC, FL_USE_THIS_SOU
      INTEGER*4  J1, J2, J3, J4, J5, IB, IE, IP, NR, IND_CA, LAM_SOU, LR, IER
      INTEGER*4, EXTERNAL :: LTM_DIF
!
      ERR_FLOOR = 0.10D0*MAS__TO__RAD
!
      FL_REF  = .FALSE.
      PSI_RA  = 0.0D0
      PSI_EA  = 0.0D0
      PSI_ER  = 0.0D0
      LAM_SOU = 0
      IF ( LC_SOU > 0 ) THEN
           FL_USED_CC = .TRUE.
         ELSE 
           FL_USED_CC = .FALSE.
      END IF 
      LR = 0
      DO 410 J1=1,LR_SOU
         DO 420 J2=1,LE_SOU
            FL_USE_THIS_SOU = .FALSE.
            IF ( FL_USED_CC ) THEN
                 IF ( LTM_DIF ( 0, LC_SOU, CC_SOU, CR_SOU(J1) ) > 0 ) THEN
                      FL_USE_THIS_SOU = .TRUE.
                 END IF
               ELSE
                 IF ( CAT_EST(J2)%NOBS_USED .GE. NUMOBS_MIN ) FL_USE_THIS_SOU = .TRUE.
            END IF
!
            IF ( CE_SOU(J2) == CR_SOU(J1) .AND. FL_USE_THIS_SOU ) THEN
                 FL_REF(J1) = .TRUE.
                 IND_CA = 0
                 DO 430 J3=1,LA_SOU
                    IF ( CA_SOU(J3) == CR_SOU(J1) ) THEN
                         IF ( IND_CA .NE. 0 ) THEN
                              CALL ERR_PASS  ( IUER, IER )
                              CALL ADD_CLIST ( MAX_SOU, LAM_SOU, CAM_SOU, CA_SOU(J3), IER )
                         END IF
                         IND_CA = J3
                    END IF
 430             CONTINUE 
                 IF ( IND_CA == 0 ) THEN
                      CALL ERR_LOG ( 7351, IUER, 'SRC_ALIGNMENT', 'Trap of internal '// &
     &                    'control: cannot find source '//CR_SOU(J1)//' in the apriori '// &
     &                    'catalogue '//FIL_APR )
                      RETURN
                 END IF
!
                 LR = LR + 1
                 ALP_REF(LR)   = CAT_REF(J1)%ALP
                 DEL_REF(LR)   = CAT_REF(J1)%DEL
                 E_ALP_REF(LR) = CAT_EST(J2)%ALP_ERR
                 E_DEL_REF(LR) = CAT_EST(J2)%DEL_ERR
                 D_RA_ALP(LR)  = CAT_REF(J1)%ALP - CAT_APR(IND_CA)%ALP 
                 D_RA_DEL(LR)  = CAT_REF(J1)%DEL - CAT_APR(IND_CA)%DEL
                 D_EA_ALP(LR)  = CAT_EST(J2)%ALP - CAT_APR(IND_CA)%ALP 
                 D_EA_DEL(LR)  = CAT_EST(J2)%DEL - CAT_APR(IND_CA)%DEL
                 D_ER_ALP(LR)  = CAT_EST(J2)%ALP - CAT_REF(J1)%ALP 
                 D_ER_DEL(LR)  = CAT_EST(J2)%DEL - CAT_REF(J1)%DEL
!!
!!    write ( 6, * ) 'sou: ', cr_sou(j1), ' d_ra_alp= ', sngl(d_ra_alp*rad__to__mas), ' d_ra_del= ', sngl(d_ra_del*rad__to__mas) ! %%%%%
!!    write ( 6, * ) 'sou: ', cr_sou(j1), ' d_ra_alp= ', sngl(d_ea_alp*rad__to__mas), ' d_ra_del= ', sngl(d_ea_del*rad__to__mas) ! %%%%%
!!!!!!!
!
!                 PSI_RA(1) = PSI_RA(1) + DSIN(CAT_REF(J1)%ALP)*D_RA_DEL &
!     &                                 - DCOS(CAT_REF(J1)%DEL)*DCOS(CAT_REF(J1)%ALP)*DSIN(CAT_REF(J1)%DEL)* &
!     &                                   D_RA_ALP
!                 PSI_RA(2) = PSI_RA(2) - DCOS(CAT_REF(J1)%ALP)*D_RA_DEL &
!     &                                 - DCOS(CAT_REF(J1)%DEL)*DSIN(CAT_REF(J1)%ALP)*DSIN(CAT_REF(J1)%DEL)* &
!     &                                   D_RA_ALP
!                 PSI_RA(3) = PSI_RA(3) + DCOS(CAT_REF(J1)%DEL)**2*D_RA_ALP
!!
!                 PSI_EA(1) = PSI_EA(1) + DSIN(CAT_REF(J1)%ALP)*D_EA_DEL &
!     &                                 - DCOS(CAT_REF(J1)%DEL)*DCOS(CAT_REF(J1)%ALP)*DSIN(CAT_REF(J1)%DEL)* &
!     &                                   D_EA_ALP
!                 PSI_EA(2) = PSI_EA(2) - DCOS(CAT_REF(J1)%ALP)*D_EA_DEL &
!     &                                 - DCOS(CAT_REF(J1)%DEL)*DSIN(CAT_REF(J1)%ALP)*DSIN(CAT_REF(J1)%DEL)* &
!     &                                   D_EA_ALP
!                 PSI_EA(3) = PSI_EA(3) + DCOS(CAT_REF(J1)%DEL)**2*D_EA_ALP
!!
!                 PSI_ER(1) = PSI_ER(1) + DSIN(CAT_REF(J1)%ALP)*D_ER_DEL &
!     &                                 - DCOS(CAT_REF(J1)%DEL)*DCOS(CAT_REF(J1)%ALP)*DSIN(CAT_REF(J1)%DEL)* &
!     &                                   D_ER_ALP
!                 PSI_ER(2) = PSI_ER(2) - DCOS(CAT_REF(J1)%ALP)*D_ER_DEL &
!     &                                 - DCOS(CAT_REF(J1)%DEL)*DSIN(CAT_REF(J1)%ALP)*DSIN(CAT_REF(J1)%DEL)* &
!     &                                   D_ER_ALP
!                 PSI_ER(3) = PSI_ER(3) + DCOS(CAT_REF(J1)%DEL)**2*D_ER_ALP
!!!!!!!
                 PSI_RA(1) = PSI_RA(1) - DCOS(ALP_REF(LR))*DTAN(DEL_REF(LR))*D_RA_ALP(LR) + DSIN(ALP_REF(LR))*D_RA_DEL(LR)
                 PSI_RA(2) = PSI_RA(2) - DSIN(ALP_REF(LR))*DTAN(DEL_REF(LR))*D_RA_ALP(LR) - DCOS(ALP_REF(LR))*D_RA_DEL(LR)
                 PSI_RA(3) = PSI_RA(3) + D_RA_ALP(LR)
!
                 PSI_EA(1) = PSI_EA(1) - DCOS(ALP_REF(LR))*DTAN(DEL_REF(LR))*D_EA_ALP(LR) + DSIN(ALP_REF(LR))*D_EA_DEL(LR)
                 PSI_EA(2) = PSI_EA(2) - DSIN(ALP_REF(LR))*DTAN(DEL_REF(LR))*D_EA_ALP(LR) - DCOS(ALP_REF(LR))*D_EA_DEL(LR)
                 PSI_EA(3) = PSI_EA(3) + D_EA_ALP(LR)
!
                 PSI_ER(1) = PSI_ER(1) - DCOS(ALP_REF(LR))*DTAN(DEL_REF(LR))*D_ER_ALP(LR) + DSIN(ALP_REF(LR))*D_ER_DEL(LR)
                 PSI_ER(2) = PSI_ER(2) - DSIN(ALP_REF(LR))*DTAN(DEL_REF(LR))*D_ER_ALP(LR) - DCOS(ALP_REF(LR))*D_ER_DEL(LR)
                 PSI_ER(3) = PSI_ER(3) + D_ER_ALP(LR)
!
            END IF
 420     CONTINUE 
         IF ( FL_REF(J1) ) THEN
              CALL ERR_PASS  ( IUER, IER )
              CALL ADD_CLIST ( MAX_SOU, LC_SOU, CC_SOU, CR_SOU(J1), IER )
         END IF
 410  CONTINUE 
      IF ( LAM_SOU > 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( LAM_SOU, STR )
           CALL LIST_TO_LINE ( LAM_SOU, CAM_SOU, ' ', STRL )
           CALL ERR_LOG ( 7352, IUER, 'SRC_ALIGNMENT', 'Trap of internal '// &
     &         'control: there are '//TRIM(STR)//' sources that are defined '// &
               'in the apriori catalogue '//TRIM(FIL_APR)//' more than '// &
     &         'once: '//STRL )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL ROTANG_SOU ( LR, ALP_REF, DEL_REF, D_RA_ALP, D_RA_DEL, &
     &                  E_ALP_REF, E_DEL_REF, ERR_FLOOR, &
     &                  ROT_RA, E_ROT_RA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7353, IUER, 'SRC_ALIGNMENT', 'Error in '// &
     &         'computing rotation angles between the refrence '// &
     &         'and the apriori source catalogues' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL ROTANG_SOU ( LR, ALP_REF, DEL_REF, D_EA_ALP, D_EA_DEL, &
     &                  E_ALP_REF, E_DEL_REF, ERR_FLOOR, &
     &                  ROT_EA, E_ROT_EA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7354, IUER, 'SRC_ALIGNMENT', 'Error in '// &
     &         'computing rotation angles between the estimated '// &
     &         'and the apriori source catalogues' )
           RETURN
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL ROTANG_SOU ( LR, ALP_REF, DEL_REF, D_ER_ALP, D_ER_DEL, &
     &                  E_ALP_REF, E_DEL_REF, ERR_FLOOR, &
     &                  ROT_ER, E_ROT_ER, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7355, IUER, 'SRC_ALIGNMENT', 'Error in '// &
     &         'computing rotation angles between the estimated '// &
     &         'and the reference catalogues' )
           RETURN
      END IF
!
      NO = NO + 1
      WRITE ( OUT(NO), 110 ) LR_SOU
 110  FORMAT ( '* Total number of reference sources: ', I4 )
!
      NO = NO + 1
      IF ( FL_USED_CC ) THEN
           IF ( NUMOBS_MIN > 0 ) THEN
                WRITE ( OUT(NO), 120 ) TRIM(SOL_ID), LC_SOU
 120            FORMAT ( '* Number of reference sources inhereted from solution ', A, &
    &                     ' : ', I5 )
             ELSE      
                WRITE ( OUT(NO), 122 ) TRIM(SOL_ID), LC_SOU
 122            FORMAT ( '* Number of reference sources defined in the control file of solution ', A, &
    &                     ' : ', I5 )
           END IF
         ELSE
           WRITE ( OUT(NO), 124 ) TRIM(SOL_ID), NUMOBS_MIN, LC_SOU
 124       FORMAT ( '* Number of reference sources in solution ', A, &
     &              ' that had at least ', I5, ' observations: ', I5 )
      END IF
!
      IF ( FL_EST ) THEN
           NO = NO + 1
           WRITE ( OUT(NO), 130 ) 'Net rotation', LC_SOU, ROT_EA
 130       FORMAT ( '* ', A, ' over ', I4, ' reference sources, ', &
     &              'estimated versus a priori:    ', 3(1PD13.6, 1X), ' rad' )
         ELSE
           NO = NO + 1
           WRITE ( OUT(NO), 135 ) 'Net rotation', LC_SOU
 135       FORMAT ( '* ', A, ' over ', I4, ' reference sources, ', &
     &              'estimated versus a priori:    ', ' not computed' )
      END IF
!
      IF ( FL_EST ) THEN
           NO = NO + 1
           WRITE ( OUT(NO), 140 ) 'Net rotation', LC_SOU, ROT_ER
 140       FORMAT ( '* ', A, ' over ', I4, ' reference sources, ', &
     &              'estimated versus a reference: ', 3(1PD13.6, 1X), ' rad' )
         ELSE
           NO = NO + 1
           WRITE ( OUT(NO), 145 ) 'Net rotation', LC_SOU
 145       FORMAT ( '* ', A, ' over ', I4, ' reference sources, ', &
     &              'estimated versus a reference: ', ' not computed' )
      END IF
!
      NO = NO + 1
      WRITE ( OUT(NO), 150 ) 'Net rotation', LC_SOU, ROT_RA
 150  FORMAT ( '* ', A, ' over ', I4, ' reference sources, ', &
     &         'reference versus a priori:    ', 3(1PD13.6, 1X), ' rad' )
!
      NO = NO + 1
      OUT(NO) = '*'
!
      NO = NO + 1
      IF ( FL_EST ) THEN
           WRITE ( OUT(NO), 130 ) 'Constraint  ', LC_SOU, PSI_EA
         ELSE
           WRITE ( OUT(NO), 135 ) 'Constraint  ', LC_SOU
      END IF
!
      NO = NO + 1
      IF ( FL_EST ) THEN
           WRITE ( OUT(NO), 140 ) 'Constraint  ', LC_SOU, PSI_ER
         ELSE
           WRITE ( OUT(NO), 145 ) 'Constraint  ', LC_SOU
      END IF
!
      NO = NO + 1
      WRITE ( OUT(NO), 150 ) 'Constraint  ', LC_SOU, PSI_RA
!
      NO = NO + 1
      OUT(NO) = '*'
!
      NO = NO + 1
      WRITE ( OUT(NO), 160 ) SIG_NNR
 160  FORMAT ( '  NO_NET_ROTATION_SOURCE         SIGMA   ', 1PD9.2, '          UNIFORM  \ * nnr_src' )
!
      NO = NO + 1
      WRITE ( OUT(NO), 170 ) PSI_RA
 170  FORMAT ( '    RIGHT_PART ', 3(1PD13.6,1X), ' NO EXCEPT  \ * nnr_src' )
!
      NR = LC_SOU/8
      IF ( NR*8 < LC_SOU ) NR = NR + 1
      CALL SORT_CH ( LC_SOU, CC_SOU )
      DO 440 J4=1,NR
         IB = (J4-1)*8 + 1
         IE =  J4*8
         IF ( IE > LC_SOU ) IE = LC_SOU
         CALL CLRCH ( STRL )
         IP = 0
         DO 450 J5=IB,IE
            STRL(7+IP:14+IP) = CC_SOU(J5)
            IP = IP + 9
 450     CONTINUE 
         IF ( J4 < NR ) THEN
              STRL(79:79) = '\'
         END IF
         NO = NO + 1
         OUT(NO) = STRL
 440  CONTINUE 

      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  SRC_ALIGNMENT  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE ROTANG_SOU ( N, ALP, DEL, D_RA, D_DE, E_RA, E_DE, &
     &                        ERR_FLOOR, PSI, E_PSI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  ROTANG_SOU
! *                                                                      *
! *  ### 28-FEB-2023  ROTANG_SOU  v1.0 (c)  L. Petrov  28-FEB-2023 ###   *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INTEGER*4  N, IUER
      REAL*8     ALP(N), DEL(N), D_RA(N), D_DE(N), E_RA(N), E_DE(N), &
     &           ERR_FLOOR, PSI(3), E_PSI(3)
      REAL*8     WEI, RC
      REAL*8     NOR_MAT(6), NOR_VEC(3), EQU_VEC(3)
      INTEGER*4  J1, IER
!
      NOR_MAT = 0.0D0
      NOR_VEC = 0.0D0
      DO 410 J1=1,N
         EQU_VEC(1) = -DCOS(ALP(J1))*DTAN(DEL(J1))
         EQU_VEC(2) = -DSIN(ALP(J1))*DTAN(DEL(J1))
         EQU_VEC(3) =  1.D0
         WEI = 1.D0/DSQRT ( ERR_FLOOR**2 + E_RA(J1)**2 )
         CALL DIAD_CVT_S ( WEI**2, 3, EQU_VEC, EQU_VEC, NOR_MAT )
         CALL NORVEC_UPD ( 3, WEI, D_RA(J1), EQU_VEC, NOR_VEC )
!
         EQU_VEC(1) =  DSIN(ALP(J1))
         EQU_VEC(2) = -DCOS(ALP(J1))
         EQU_VEC(3) =  0.D0
         WEI = 1.D0/DSQRT ( ERR_FLOOR**2 + E_DE(J1)**2 )
         CALL DIAD_CVT_S ( WEI**2, 3, EQU_VEC, EQU_VEC, NOR_MAT )
         CALL NORVEC_UPD ( 3, WEI, D_DE(J1), EQU_VEC, NOR_VEC )
 410  CONTINUE 
!
      CALL ERR_PASS ( IUER, IER )
      CALL INVS ( 3, NOR_MAT, RC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 7511, IUER, 'ROTANG_SOU', 'Error in an '// &
     &         'attempt to invert normal matrix' )
           RETURN 
      END IF
      CALL MUL_MV_SV_V ( 3, NOR_MAT, 3, NOR_VEC, 3, PSI, IER )
      E_PSI(1) = DSQRT ( NOR_MAT(1) )
      E_PSI(2) = DSQRT ( NOR_MAT(3) )
      E_PSI(3) = DSQRT ( NOR_MAT(6) )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  ROTANG_SOU  !#!#
