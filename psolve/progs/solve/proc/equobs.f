      SUBROUTINE EQUOBS_INIT ( EQUOBS )
! ************************************************************************
! *                                                                      *
! *   Routine EQUOBS_INIT initialize EQUOBS obejct.                      *
! *                                                                      *
! *  ### 18-MAY-2021  EQUOBS_QUIT  v1.0 (c)  L. Petrov  18-MAY-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'equobs.i'
      TYPE ( PSOLVE__EQUOBS_TYPE ) :: EQUOBS
      INTEGER*4  J1
!
      DO 410 J1=1,MAX_OBS
         EQUOBS%OBS(J1)%NPAR = 0
         EQUOBS%OBS(J1)%DEL_OC  = 0.0D0
         EQUOBS%OBS(J1)%DEL_ERR = 0.0D0
         EQUOBS%OBS(J1)%RAT_OC  = 0.0D0
         EQUOBS%OBS(J1)%RAT_ERR = 0.0D0
         IF ( ASSOCIATED ( EQUOBS%OBS(J1)%DER_RAT ) ) DEALLOCATE ( EQUOBS%OBS(J1)%DER_RAT )
         IF ( ASSOCIATED ( EQUOBS%OBS(J1)%DER_DEL ) ) DEALLOCATE ( EQUOBS%OBS(J1)%DER_DEL )
         IF ( ASSOCIATED ( EQUOBS%OBS(J1)%IND_PAR ) ) DEALLOCATE ( EQUOBS%OBS(J1)%IND_PAR )
         EQUOBS%OBS(J1)%DER_RAT => NULL()
         EQUOBS%OBS(J1)%DER_DEL => NULL()
         EQUOBS%OBS(J1)%IND_PAR => NULL()
 410  CONTINUE 
      EQUOBS%NOBS = 0
      EQUOBS%NTOT_PAR = 0
      EQUOBS%STATUS = EQU__INIT
      RETURN
      END  SUBROUTINE  EQUOBS_INIT  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EQUOBS_QUIT ( EQUOBS )
! ************************************************************************
! *                                                                      *
! *   Routine EQUOBS_QUIT deallocate memory used by EQUOBS.              *
! *                                                                      *
! *  ### 18-MAY-2021  EQUOBS_QUIT  v1.0 (c)  L. Petrov  18-MAY-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'equobs.i'
      TYPE ( PSOLVE__EQUOBS_TYPE ) :: EQUOBS
      INTEGER*4  J1
!
      IF ( EQUOBS%NOBS > 0 ) THEN
           DO 410 J1=EQUOBS%NOBS,1,-1
              IF ( ASSOCIATED ( EQUOBS%OBS(J1)%DER_RAT ) ) DEALLOCATE ( EQUOBS%OBS(J1)%DER_RAT )
              IF ( ASSOCIATED ( EQUOBS%OBS(J1)%DER_DEL ) ) DEALLOCATE ( EQUOBS%OBS(J1)%DER_DEL )
              IF ( ASSOCIATED ( EQUOBS%OBS(J1)%IND_PAR ) ) DEALLOCATE ( EQUOBS%OBS(J1)%IND_PAR )
              EQUOBS%OBS(J1)%NPAR = 0
              EQUOBS%OBS(J1)%DEL_OC  = 0.0D0
              EQUOBS%OBS(J1)%DEL_ERR = 0.0D0
              EQUOBS%OBS(J1)%RAT_OC  = 0.0D0
              EQUOBS%OBS(J1)%RAT_ERR = 0.0D0
 410       CONTINUE 
      END IF
      EQUOBS%NOBS = 0
      EQUOBS%STATUS = EQU__UNDF
      RETURN
      END  SUBROUTINE  EQUOBS_QUIT !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EQUOBS_UPD ( FL_RAT, IND_OBS, DOC, DOERR, ROC, RERR, &
     &                        NTOT_PAR, NOBS_PAR, IND_PAR, DEL_EQU, &
     &                        DEL_RAT, EQUOBS, IUER  )
! ************************************************************************
! *                                                                      *
! *   Routine EQUOBS_UPD
! *                                                                      *
! *  ### 18-MAY-2021  EQUOBS_UPD   v1.0 (c)  L. Petrov  18-MAY-2021 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'equobs.i'
      INCLUDE   'socom.i'
      LOGICAL*1  FL_RAT
      INTEGER*4  IND_OBS, NTOT_PAR, NOBS_PAR, IND_PAR(NOBS_PAR), IUER
      REAL*8     DOC, DOERR, ROC, RERR, DEL_EQU(NOBS_PAR), DEL_RAT(NOBS_PAR)
      TYPE     ( PSOLVE__EQUOBS_TYPE ) :: EQUOBS
      CHARACTER  STR*32, STR1*32, STR2*32
      INTEGER*4  IER
!
      IF ( EQUOBS%STATUS == EQU__INIT .OR. EQUOBS%STATUS == EQU__FILLED ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG (  7541, IUER, 'EQUOBS_UPD', 'Trap of internal control: '// &
     &         'EQUOBS object was not initialized' )
           RETURN 
      END IF
!
      IF ( EQUOBS%NTOT_PAR .NE. 0 .AND. EQUOBS%NTOT_PAR .NE. NTOT_PAR ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( IND_OBS,         STR )
           CALL INCH  ( EQUOBS%NTOT_PAR, STR1 )
           CALL INCH  ( NTOT_PAR,        STR2 )
           CALL ERR_LOG (  7542, IUER, 'EQUOBS_UPD', 'Trap of internal control '// &
     &         'when processing the '//TRIM(STR)//'th observtion: the total '// &
     &         'number of parameters changed from '//TRIM(STR1)//' to '//STR2 )
           RETURN 
      END IF
      IF ( EQUOBS%NTOT_PAR > NPARAM ) THEN
           CALL CLRCH ( STR  )
           CALL CLRCH ( STR1 )
           CALL CLRCH ( STR2 )
           CALL INCH  ( IND_OBS,         STR )
           CALL INCH  ( EQUOBS%NTOT_PAR, STR1 )
           CALL INCH  ( NPARAM,          STR2 )
           CALL ERR_LOG ( 7543, IUER, 'EQUOBS_GEN_NOR', 'Trap of internal control: '// &
     &         'when processing the '//TRIM(STR)//'th observtion: the total '// &
     &         'number of parameters '//TRIM(STR1)//' exceeded the limit '// &
     &         TRIM(STR2)//' set in variable NPARAM in socom.i' )
           RETURN 
      END IF
!
      IF ( EQUOBS%NOBS > MAX_OBS ) THEN
           CALL CLRCH ( STR  )
           CALL INCH  ( MAX_OBS, STR )
           CALL ERR_LOG (  7544, IUER, 'EQUOBS_UPD', 'Trap of internal control: '// &
     &         ' too many observations -- '//TRIM(STR)//'. You need increase '// &
     &         'constant MAX_OBS in solve.i' )
           RETURN 
      END IF
!
      EQUOBS%NTOT_PAR = NTOT_PAR
      EQUOBS%NOBS = EQUOBS%NOBS + 1
      EQUOBS%OBS(EQUOBS%NOBS)%IND_OBS = IND_OBS
      EQUOBS%OBS(EQUOBS%NOBS)%NPAR = NOBS_PAR
      IF ( .NOT. FL_RAT ) THEN
            ALLOCATE ( EQUOBS%OBS(EQUOBS%NOBS)%IND_PAR(NOBS_PAR), &
     &                 EQUOBS%OBS(EQUOBS%NOBS)%DER_DEL(NOBS_PAR), STAT=IER  )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR  ) 
                 CALL CLRCH ( STR1 ) 
                 CALL INCH  ( NOBS_PAR*12, STR  )
                 CALL INCH  ( IND_OBS,  STR1 )
                 CALL ERR_LOG ( 7545, IUER, 'EQUOBS_UPD', 'Error in allocation '// &
     &               'of '//TRIM(STR)//' bytes of memory for the '//TRIM(STR1)// &
     &               'th observation' )
                 RETURN 
            END IF
         ELSE
            ALLOCATE ( EQUOBS%OBS(EQUOBS%NOBS)%IND_PAR(NOBS_PAR), &
     &                 EQUOBS%OBS(EQUOBS%NOBS)%DER_DEL(NOBS_PAR), &
     &                 EQUOBS%OBS(EQUOBS%NOBS)%DER_RAT(NOBS_PAR), STAT=IER  )
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR  ) 
                 CALL CLRCH ( STR1 ) 
                 CALL INCH  ( NOBS_PAR*20, STR  )
                 CALL INCH  ( IND_OBS,  STR1 )
                 CALL ERR_LOG (  7546, IUER, 'EQUOBS_UPD', 'Error in allocation '// &
     &               'of '//TRIM(STR)//' bytes of memory for the '//TRIM(STR1)// &
     &               'th observation' )
                 RETURN 
            END IF
      END IF
!
      EQUOBS%OBS(EQUOBS%NOBS)%IND_PAR = IND_PAR
      EQUOBS%OBS(EQUOBS%NOBS)%DER_DEL = DEL_EQU
      EQUOBS%OBS(EQUOBS%NOBS)%DEL_OC  = DOC
      EQUOBS%OBS(EQUOBS%NOBS)%DEL_ERR = DOERR
      IF ( FL_RAT ) THEN
           EQUOBS%OBS(EQUOBS%NOBS)%DER_RAT = DEL_RAT
           EQUOBS%OBS(EQUOBS%NOBS)%RAT_OC  = ROC
           EQUOBS%OBS(EQUOBS%NOBS)%RAT_ERR = RERR
      END IF
!
      EQUOBS%STATUS = EQU__FILLED
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  EQUOBS_UPD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE EQUOBS_GEN_NOR ( EQUOBS, ARR, ASM_NAM, ASM_STA_FIL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  EQUOBS_GEN_NOR
! *                                                                      *
! * ### 19-MAY-2021   EQUOBS_GEN_NOR  v1.0 (c) L. Petrov 19-MAY-2021 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'equobs.i'
      INCLUDE   'socom.i'
      TYPE     ( PSOLVE__EQUOBS_TYPE ) :: EQUOBS
      INTEGER*4  IUER
      CHARACTER  ASM_NAM*(*), ASM_STA_FIL*(*)
      REAL*8     ARR(*)
      REAL*8,    ALLOCATABLE :: EQU_MAT(:,:), EQU_VEC(:)
      REAL*8     DEL_ERR_MIN
      PARAMETER  ( DEL_ERR_MIN = 1.0D-14 )
      CHARACTER  STR*128, STR1*128
      INTEGER*4  JA, JB, J1, J2, J3, J4, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
   write ( 6, * ) 'Before EQUOBS_GEN_NOR ' ! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      JA = 1+3*M_GPA
      JB = 1+2*M_GPA
!
      IF ( EQUOBS%STATUS == EQU__FILLED ) THEN
           CONTINUE 
         ELSE
           CALL ERR_LOG ( 7551, IUER, 'EQUOBS_GEN_NOR', 'Trap of internal control: '// &
     &         'EQUOBS object was not initialized' )
           RETURN 
      END IF
      IF ( EQUOBS%NTOT_PAR > NPARAM ) THEN
           CALL ERR_LOG ( 7552, IUER, 'EQUOBS_GEN_NOR', 'Trap of internal '// &
     &         'control: the total number of parameters is greater than '// &
     &         'varaialbe NPAR from socom.i' )
           RETURN 
      END IF
!
      CALL NOUT_R8 (  EQUOBS%NTOT_PAR,                        ARR(JB) )
      CALL NOUT_R8 ( (EQUOBS%NTOT_PAR*(EQUOBS%NTOT_PAR+1))/2, ARR(JA) )
!
      IF ( ILEN(ASM_NAM) == 0 ) THEN
           DO 410 J1=1,EQUOBS%NOBS
              CALL ADD_TRG ( EQUOBS%OBS(J1)%DEL_OC, EQUOBS%OBS(J1)%DEL_ERR, EQUOBS%OBS(J1)%NPAR, &
     &                       EQUOBS%OBS(J1)%IND_PAR, EQUOBS%OBS(J1)%DER_DEL, &
     &                       EQUOBS%NTOT_PAR, ARR(JB), ARR(JA) )
 
 410       CONTINUE 
        ELSE
           ALLOCATE ( EQU_MAT(EQUOBS%NTOT_PAR,EQUOBS%NOBS), EQU_VEC(EQUOBS%NOBS), &
     &                STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH  ( STR )
                CALL IINCH8 ( INT8(8)*INT8(EQUOBS%NTOT_PAR)*INT8(EQUOBS%NOBS) + &
     &                        INT8(8)*INT8(EQUOBS%NOBS), STR )
                CALL ERR_LOG ( 7553, IUER, 'EQUOBS_GEN_NOR', 'Failure to '// &
     &              'allocate '//TRIM(STR)//' bytes of dynamic memory for '// &
     &              'arrays EQU_MAT and EQU_VEC' )
                RETURN 
           END IF
           EQU_MAT = 0.0D0
           EQU_VEC = 0.0D0
           DO 420 J2=1,EQUOBS%NOBS
              DO 430 J3=1,EQUOBS%OBS(J2)%NPAR
                 EQU_MAT(EQUOBS%OBS(J2)%IND_PAR,J2) = EQUOBS%OBS(J2)%DER_DEL
 430          CONTINUE 
              EQU_VEC(J2) = EQUOBS%OBS(J2)%DEL_OC/MIN(EQUOBS%OBS(J2)%DEL_ERR,DEL_ERR_MIN)
 420       CONTINUE 
!
! -------- Make normal matrix
!
           IER=-1
           CALL MUL_MM_IT_S ( EQUOBS%NOBS, EQUOBS%NTOT_PAR, EQU_MAT, &
     &                        EQUOBS%NOBS, EQUOBS%NTOT_PAR, EQU_MAT, &
     &                        EQUOBS%NOBS, ARR(JA), IER )
!
! -------- Make normal vector
!
           IER=-1
           CALL MUL_MV_IV_V ( EQUOBS%NOBS, EQUOBS%NTOT_PAR, ARR(JA), &
     &                        EQUOBS%NTOT_PAR, EQU_VEC, &
     &                        EQUOBS%NOBS, ARR(JA), IER )
           DEALLOCATE ( EQU_MAT, EQU_VEC )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE EQUOBS_GEN_NOR  !#! #
