      SUBROUTINE GVH_FROM_MARK3 ( GVH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine GVH_FROM_MARK3
! *                                                                      *
! * ### 01-AUG-2019 GVH_FROM_MARK3 v1.2 (c)  L. Petrov  17-DEC-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
#ifdef GVH_STANDALONE
      INCLUDE   'gvh_solve.i'
#else
      INCLUDE   'solve.i'
#endif
      INCLUDE   'astro_constants.i'
      INCLUDE   'gvh.i'
      INCLUDE   'gvf_db.i'
      TYPE     ( GVH__STRU ) :: GVH
      INTEGER*4  IUER
!
      INTEGER*4  J1, J2, J3, J4, J5, J6, IER
      CHARACTER  C_STA(GVH__MSTA)*8, DESCR*1024, STR*128, STR_ARR2(2)*2
      INTEGER*4  CLASS, TYP, DIMS(2), LEN_REC, LEN_DATA, SEG_IND, NUM_FIELDS, &
     &           ADIM1, ADIM2, QC_DIMS(2)
      INTEGER*4  NUMOBS, NUMSCA, NUMSTA, NOBS_STA(GVH__MSTA), OBS_TAB(3,GVH__MOBS)
      CHARACTER  BAND_NAM(2)*1
      CHARACTER, ALLOCATABLE :: QUALCODE(:,:)*1, QUALCODE2(:,:)*2
      ADDRESS__TYPE ADR_DATA, ADR_CONV
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      CALL ERR_PASS ( IUER, IER )
      CALL GVH_PREGET ( GVH, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1911, IUER, 'GVH_FROM_MARK3', 'Error in an attempt to '// &
     &         'execute GVH_PREGET' )
           RETURN 
      END IF
!
! --- Read Mandatory LCODES from the GVF database
!
      DO 410 J1=1,GVH__NMLCODE
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_INQ_LCODE ( GVH, GVH__MLCODE(J1), DESCR, CLASS, TYP, DIMS, &
     &                        NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                        ADR_DATA, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1912, IUER, 'GVH_FROM_MARK3', 'Error in an attempt to '// &
     &            'inquire lcode '//GVH__MLCODE(J1) )
              RETURN 
         END IF
!
         CALL GET_MEM ( INT8(LEN_DATA), ADR_DATA )
         IF ( ADR_DATA == 0 ) THEN
              CALL CLRCH ( STR ) 
              CALL IINCH ( LEN_DATA, STR )
              CALL ERR_LOG ( 1913, IUER, 'GVH_FROM_MARK3', 'Error in an attempt to '// &
     &            'allocate '//TRIM(STR)//' bytes of dynamic memory' )
              RETURN 
         END IF
!
         CALL ERR_PASS   ( IUER, IER )
         CALL GVH_GLCODE ( GVH, GVH__MLCODE(J1), 0, 0, LEN_DATA, &
     &                     DIMS(1), DIMS(2), %VAL(ADR_DATA), IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1914, IUER, 'GVH_FROM_MARK3', 'Error in an attempt to '// &
     &            'get data of lcode '//GVH__MLCODE(J1)//' from the input database' )
              RETURN 
         END IF
!
         IF ( GVH__MLCODE(J1) == 'NOBS_STA' ) THEN
              CALL MEMCPY ( NOBS_STA, %VAL(ADR_DATA), %VAL(LEN_DATA) )
           ELSE IF ( GVH__MLCODE(J1) == 'NUMB_OBS' ) THEN
              CALL MEMCPY ( NUMOBS,   %VAL(ADR_DATA), %VAL(LEN_DATA) )
           ELSE IF ( GVH__MLCODE(J1) == 'NUMB_SCA' ) THEN
              CALL MEMCPY ( NUMSCA,   %VAL(ADR_DATA), %VAL(LEN_DATA) )
           ELSE IF ( GVH__MLCODE(J1) == 'NUMB_STA' ) THEN
              CALL MEMCPY ( NUMSTA,   %VAL(ADR_DATA), %VAL(LEN_DATA) )
           ELSE IF ( GVH__MLCODE(J1) == 'OBS_TAB ' ) THEN
              CALL MEMCPY ( OBS_TAB,  %VAL(ADR_DATA), %VAL(LEN_DATA) )
           ELSE IF ( GVH__MLCODE(J1) == 'SITNAMES' ) THEN
              CALL MEMCPY ( C_STA,    %VAL(ADR_DATA), %VAL(LEN_DATA) )
         END IF
         CALL FREE_MEM ( ADR_DATA )
 410  CONTINUE 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'QUALCODE', DESCR, CLASS, TYP, QC_DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1915, IUER, 'GVH_FROM_MARK3', 'Error in an attempt to '// &
     &         'inquire lcode BAND_NAM' )
           RETURN 
      END IF
      ALLOCATE ( QUALCODE(QC_DIMS(2),NUMOBS) )
      ALLOCATE ( QUALCODE2(QC_DIMS(2),NUMOBS) )
      DO 420 J2=1,NUMOBS
         CALL ERR_PASS   ( IUER, IER )
         IF ( QC_DIMS(1) == 1 ) THEN
              CALL GVH_GLCODE ( GVH, 'QUALCODE', J2, 1, QC_DIMS(1)*QC_DIMS(2), &
     &                          ADIM1, ADIM2, QUALCODE(1,J2), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1916, IUER, 'GVH_FROM_MARK3', 'Error in an '// &
     &                 'attempt to get lcde BAND_NAM' )
                   RETURN 
              END IF
              IF ( QUALCODE(1,J2) == ' ' ) QUALCODE(1,J2) = '0'
              IF ( QC_DIMS(2) > 1 ) THEN
                   IF ( QUALCODE(2,J2) == ' ' ) QUALCODE(2,J2) = '0'
              END IF
           ELSE
              CALL GVH_GLCODE ( GVH, 'QUALCODE', J2, 1, DIMS(1)*DIMS(2), &
     &                          ADIM1, ADIM2, QUALCODE2(1,J2), IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 1916, IUER, 'GVH_FROM_MARK3', 'Error in an '// &
     &                 'attempt to get lcode QUALCODE' )
                   RETURN 
              END IF
              IF ( QUALCODE2(1,J2)(1:1) == ' ' ) QUALCODE2(1,J2)(1:1) = '_'
              IF ( QUALCODE2(1,J2)(2:2) == ' ' ) QUALCODE2(1,J2)(2:2) = '0'
              IF ( QC_DIMS(2) > 1 ) THEN
                   IF ( QUALCODE2(2,J2)(1:1) == ' ' ) QUALCODE2(2,J2)(1:1) = '_'
                   IF ( QUALCODE2(2,J2)(2:2) == ' ' ) QUALCODE2(2,J2)(2:2) = '0'
              END IF
         END IF
 420  CONTINUE 
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_INQ_LCODE ( GVH, 'BAND_NAM', DESCR, CLASS, TYP, DIMS, &
     &                     NUM_FIELDS, SEG_IND, LEN_REC, LEN_DATA, &
     &                     ADR_DATA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1917, IUER, 'GVH_FROM_MARK3', 'Error in an attempt to '// &
     &         'inquire lcode BAND_NAM' )
           RETURN 
      END IF
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_GLCODE ( GVH, 'BAND_NAM', 1, 1, DIMS(1)*DIMS(2), ADIM1, ADIM2, &
     &                  BAND_NAM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1918, IUER, 'GVH_FROM_MARK3', 'Error in an '// &
     &         'attempt to get lcde BAND_NAM' )
           RETURN 
      END IF
      IF ( BAND_NAM(1) == ' ' .OR. BAND_NAM(1) == CHAR(0) ) BAND_NAM(1) = 'X'
      IF ( BAND_NAM(2) == ' ' .OR. BAND_NAM(2) == CHAR(0) ) BAND_NAM(2) = 'S'
!
      CALL ERR_PASS   ( IUER, IER )
      CALL GVH_PLCODE ( GVH, 'BAND_NAM', 1, 1, BAND_NAM, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1919, IUER, 'GVH_FROM_MARK3', 'Error in an '// &
     &         'attempt to put lcode BAND_NAM' )
           RETURN 
      END IF
!
      DO 430 J3=1,NUMOBS
         CALL ERR_PASS   ( IUER, IER )
         IF ( QC_DIMS(1) == 1 ) THEN
              CALL GVH_PLCODE ( GVH, 'QUALCODE', J3, 1, QUALCODE(1,J3), IER )
            ELSE 
              CALL GVH_PLCODE ( GVH, 'QUALCODE', J3, 1, QUALCODE2(1,J3), IER )
         END IF
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 1920, IUER, 'GVH_FROM_MARK3', 'Error in an '// &
     &            'attempt to put lcode QUALCODE' )
              RETURN 
         END IF
 430  CONTINUE 
      DEALLOCATE ( QUALCODE  )
      DEALLOCATE ( QUALCODE2 )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GVH_FROM_MARK3  !#!#
