      SUBROUTINE PROC_DO ( ARR, B3DOBJ, B1B3DOBJ, CNSTROBJ, SNGCHK_CMP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PROC_DO  creates normal equations and normal vector for   *
! *   parameter estimation using LSQ.                                    *
! *                                                                      *
! *  PRE-99 code of ../proc/proc.f was used for PROC_DO                  *
! *                                                                      *
! *  ###  03-JAN-1999    PROC_DO   v1.5  (c)  L. Petrov 06-DEC-2022 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'fast.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'socom.i'
      INCLUDE   'precm.i'
      INCLUDE   'oborg.i'
      INCLUDE   'prfil.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'cnstr.i'
      INCLUDE   'equobs.i'
!
      REAL*8     ARR(*)
      TYPE ( B3D__STRU   ) ::  B3DOBJ
      TYPE ( B1B3D__STRU ) ::  B1B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      TYPE ( PSOLVE__EQUOBS_TYPE ) :: EQUOBS
      INTEGER*4  SNGCHK_CMP, IUER
      INTEGER*4  J1, J2, JA, JB, IER
      CHARACTER  MONUMENT_SAVE(MAX_ARC_STA)*10, VTD_CONF_USE*128, STR*128
      LOGICAL*1  NORMEQ_EXT 
      LOGICAL*2  KBIT
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
      CALL GETENVAR ( 'NORMEQ_EXT', STR )
      IF  ( STR == 'YES' .OR. STR == 'yes' ) THEN
            NORMEQ_EXT = .TRUE.
         ELSE
            NORMEQ_EXT = .FALSE.
      END IF 
      JA = 1+3*M_GPA
      JB = 1+2*M_GPA
!
! --- Open the NAMFIL
!
      CALL OPENNAMFIL()
!
! --- Some initializations
!
      CALL PRELP ( ARR )
      IF ( FAST_DBG .EQ. F__APP ) THEN
           WRITE ( 6, * ) ' PROC_DO started'
      END IF
      IF ( NORMEQ_EXT ) THEN
           CALL EQUOBS_INIT ( EQUOBS )
      END IF
!
! --- Main loop
!
      CALL ERR_PASS ( IUER, IER )
      CALL LOOP     ( B3DOBJ, B1B3DOBJ, ARR, EQUOBS, SNGCHK_CMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8391, IUER, 'PROC_DO', 'Error in attempt '// &
     &         'to build normal equations' )
           RETURN
      END IF
!
      IF ( GIM_EST ) THEN
!
! -------- Perform adjustment of the ionospheric bias
!
           CALL CLRCH ( VTD_CONF_USE )
           IF ( FL_VTD_SES ) THEN
                VTD_CONF_USE = VTD_CONF_SES
              ELSE
                IF ( FL_VTD_GLB ) THEN
                     VTD_CONF_USE = VTD_CONF_GLB
                END IF
           END IF
!
           CALL GETENVAR ( 'VTD_CONF', STR )
           IF ( .NOT. KBATCH ) THEN
                 VTD_CONF_USE = STR
           END IF
!
           CALL ERR_PASS ( IUER, IER )
           CALL IONO_BIAS_ADJ ( DBNAME_CH, %VAL(VTD_ADR), &
     &                          GIM_INFO_DIR, GIM_DTEC_DIR, BCL_FIL, BRK_FIL, &
     &                          GIM_MODE, GIM_DEG, GIM_TIM_STEP, GIM_SCALE, &
     &                          GIM_COLLECT_INFO, GIM_EST, GIM_WRI, GIM_VERB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8392, IUER, 'PROC_DO', 'Error in estimation '// &
     &              'of the ionosphere bias model for experiment '//DBNAME_CH )
                RETURN
           END IF
      END IF
      IF ( GIM_RGR ) THEN
!
! -------- Perform comutation of errors of ionospheric path delay from GNSS
! -------- and write them into output files in the specified directories
! -------- GIM_ADDW_DIR, GIM_DEL_DIR, GIM_NOI_DIR
!
           CALL ERR_PASS ( IUER, IER )
           CALL IONO_REGR_MOD ( DBNAME_CH, GIM_INFO_DIR, %VAL(VTD_ADR), GIM_ADDW_DIR, &
     &                          GIM_DEL_DIR, GIM_NOI_DIR, GIM_SEED, GIM_VERB, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8392, IUER, 'PROC_DO', 'Error in estimation '// &
     &              'of the ionosphere bias model for experiment '//DBNAME_CH )
                RETURN
           END IF
      END IF
      IF ( GIM_EST .OR. GIM_RGR ) THEN
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
!
      IF ( NORMEQ_EXT ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL EQUOBS_GEN_NOR ( EQUOBS, ARR, ASM_NAM, ASM_STA_FIL, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8393, IUER, 'PROC_DO', 'Error in attempt '// &
     &              'to generate normal matrix' )
                RETURN
           END IF
      END IF
!
      IF ( KECC ) THEN
!
! -------- Saving monument names. We discaard alll other possible changes
! -------- in prfil, except this. I am affreaid of direct saving parfil --
! -------- Solve is so capricious! -pet-
!
           DO 410 J1=1,INT4(NUMSTA)
              MONUMENT_SAVE(J1) = MONUMENTS_CHR(J1)
 410       CONTINUE
!
           CALL USE_PARFIL ( 'OR' )
           DO 420 J2=1,INT4(NUMSTA)
              MONUMENTS_CHR(J2) = MONUMENT_SAVE(J2)
 420       CONTINUE
           CALL USE_PARFIL ( 'WC' )
      END IF
!               
      IF ( FAST_DBG .EQ. F__TIM ) THEN
           CALL TIM_INIT() ! Set timer
      END IF
      IF ( SNGCHK_CMP .EQ. SNGCHK_CMP__SKIP ) THEN
           IF ( TRAIN ) THEN
!
! ------------ Write status: skip the database since it failed singularity check
!
               CALL USE_BUFFER ( INT2(1), INT2(1), 'OWC' )
           END IF
         ELSE
!
! -------- Write the equations of constraints and normal equations to disk
! -------- (if needed)
!
           CALL ERR_PASS ( IUER, IER )
           CALL PSTLP ( B3DOBJ, B1B3DOBJ, CNSTROBJ, ARR, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 8394, IUER, 'PROC_DO', 'Error in PSTLP' )
                RETURN
           END IF
!
           IF ( TRAIN ) THEN
!
! ------------- Write status: normal termination
!
                CALL USE_BUFFER ( INT2(0), INT2(1), 'OWC' )
           END IF
      END IF
      IF ( NORMEQ_EXT ) THEN
           CALL EQUOBS_QUIT ( EQUOBS )
      END IF
!
! --- Close the NAMFIL
!
      CALL CLOSENAMFIL()
!
! --- Screen closing in the interactive mode
!
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) CALL END_MN
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Timing printout
!
           CALL TIM_GET ( 'PROC-05' )
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PROC_DO  #!#
