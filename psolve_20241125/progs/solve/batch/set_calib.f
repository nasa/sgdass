      SUBROUTINE SET_CALIB ( FL_RESET, L_KEE, L_ENB, L_DIS, KEECAL, ENBCAL, &
     &                       DISCAL, IONCTL, DBNAME_MES, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  SET_CALIB  sets calibration status according to           *
! *   POST-MAY2000 syntax of BATCH control language.                     *
! *                                                                      *
! *   History                                                            *
! *   Who   When        What                                             *
! *   pet   2000.09.15  Added support of IONCTL argument. The previous   *
! *                     version ignored ssetting ionosphere flag.        *
! *   pet   2002.03.06  The previous version did not disable zenith      *
! *                     troposphere path calibrations.                   *
! *                     Changed implementation of "RESET YES" option --  *
! *                     the previous version purged site and zenith      *
! *                     calibrations. Therefore, when later user enabled *
! *                     calibration, th placements of the calibration    *
! *                     was changed, and as a result user got not the    *
! *                     calibration which he or she wanted.              *
! *   pet   2007.05.29  Support case when SCAL keeps not names of        *
! *                     calibration from CORFxx file, but their lcodes   *
! *                     (which is logical, right?)                       *
! *                                                                      *
! *  ### 12-MAY-2000   SET_CALIB   v1.3 (c)  L. Petrov  29-MAY-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'cals.i'
      INCLUDE    'socom.i'
      LOGICAL*4  FL_RESET
      INTEGER*4  L_KEE, L_ENB, L_DIS, IUER
      CHARACTER  KEECAL(MAX_CAL)*8, ENBCAL(MAX_CAL)*8, DISCAL(MAX_CAL)*8, &
     &           DBNAME_MES*(*), IONCTL*(*)
      TYPE ( CALS_STRU ) ::  CALR, CALW
      CHARACTER  CAL_LCODE*8
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, &
     &           IR1, IR2, IW1, IW2, IER
      INTEGER*4  LTM_DIF
      LOGICAL*4  DATYP_INQ
!
! --- Reading calibration status from superfile
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_R   ( INT2(1), 0, 0, CALR, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2921, IUER, 'SET_CALIB', 'Error in reading '// &
     &         'calibration information ' )
           RETURN
      END IF
!
! --- Make a copy of calibration block.
!
      CALW = CALR
      IF ( FL_RESET ) THEN
!
! -------- Resetting. Status for all zenith path delay and station dependent
! -------- calibration is set to "not applied"
!
           DO 410 J1=1,CALW%L_STA ! all stations
              DO 420 J2=1,MC_SCAL ! all statoin-dependent calibrations
                 CALW%SCAL_APL(J2,J1) = .FALSE.
 420          CONTINUE
!
              DO 430 J3=1,MC_ZENC ! all zenith calibrations
                 CALW%ZENC_APL(J3,J1) = .FALSE.
 430          CONTINUE
 410      CONTINUE
      END IF
!
      IF ( L_KEE .GT. 0 ) THEN
           DO 440 J4=1,L_KEE
!
! ----------- Look for the KEECAL(J4) calibration among  station-dependent
! ----------- calibrations and zenith flyby calibrations
!
              IR1 = LTM_DIF ( 0, CALR%L_SCAL, CALR%SCAL, KEECAL(J4) )
              IF ( IR1 == 0  .AND. KEECAL(J4) == 'cable   ' ) THEN
!
! ---------------- Support the case when SCAL keeps lcode name of the 
! ---------------- calibration instead of the calibration name from CORFxx
!
                   CAL_LCODE = 'CABL_DEL'
                   IR1 = LTM_DIF ( 0, CALR%L_SCAL, CALR%SCAL, CAL_LCODE  )
              END IF              
              IF ( IR1 == 0  .AND. KEECAL(J4) == 'pcal rmv' ) THEN
!
! ---------------- Support the case when SCAL keeps lcode name of the 
! ---------------- calibration instead of the calibration name from CORFxx
!
                   CAL_LCODE = 'UNPHASCL'
                   IR1 = LTM_DIF ( 0, CALR%L_SCAL, CALR%SCAL, CAL_LCODE  )
              END IF              
!
              IR2 = LTM_DIF ( 0, CALR%L_ZENC, CALR%ZENC, KEECAL(J4) )
              IF ( IR1 .GT. 0 ) THEN
                   DO 450 J5=1,CALW%L_STA
                      CALW%SCAL_APL(IR1,J5) = CALR%SCAL_APL(IR1,J5)
 450               CONTINUE
                 ELSE IF ( IR2 .GT. 0 ) THEN
                   DO 460 J6=1,CALW%L_STA
                      CALW%ZENC_APL(IR2,J6) = CALR%ZENC_APL(IR2,J6)
 460               CONTINUE
              END IF
 440       CONTINUE
      END IF
!
      IF ( L_ENB .GT. 0 ) THEN
           DO 470 J7=1,L_ENB
!
! ----------- Look for the ENBCAL(J7) calibration among  station-dependent
! ----------- calibrations and zenith flyby calibrations
!
              IR1 = LTM_DIF ( 0, CALR%L_SCAL, CALR%SCAL, ENBCAL(J7) )
              IF ( IR1 == 0  .AND. ENBCAL(J7) == 'cable   ' ) THEN
!
! ---------------- Support the case when SCAL keeps lcode name of the 
! ---------------- calibration instead of the calibration name from CORFxx
!
                   CAL_LCODE = 'CABL_DEL'
                   IR1 = LTM_DIF ( 0, CALR%L_SCAL, CALR%SCAL, CAL_LCODE  )
              END IF              
!
              IF ( IR1 == 0  .AND. ENBCAL(J7) == 'pcal rmv' ) THEN
!
! ---------------- Support the case when SCAL keeps lcode name of the 
! ---------------- calibration instead of the calibration name from CORFxx
!
                   CAL_LCODE = 'UNPHASCL'
                   IR1 = LTM_DIF ( 0, CALR%L_SCAL, CALR%SCAL, CAL_LCODE  )
              END IF              
              IR2 = LTM_DIF ( 0, CALR%L_ZENC, CALR%ZENC, ENBCAL(J7) )
!
              IF ( IR1 .GT. 0 ) THEN
!
! ---------------- Set the calibration if it is available
!
                   DO 480 J8=1,CALW%L_STA
                      IF ( CALW%SCAL_AVL(IR1,J8) ) THEN
                           CALW%SCAL_APL(IR1,J8) = .TRUE.
                         ELSE
                           CALW%SCAL_APL(IR1,J8) = .FALSE.
                           IF ( G_WARNING ) THEN
                                WRITE (  6, '(A)' ) 'WARNING: '//DBNAME_MES// &
     &                                 ' SET_CALIB: Calibration '//ENBCAL(J7)// &
     &                                 ' is not available for station '// &
     &                                   CALW%STANAM(J8)
                                WRITE ( 23, '(A)' ) 'WARNING: '//DBNAME_MES// &
     &                                 ' SET_CALIB: Calibration '//ENBCAL(J7)// &
     &                                 ' is not available for station '// &
     &                                   CALW%STANAM(J8)
                           END IF
                      END IF
 480               CONTINUE
                 ELSE IF ( IR2 .GT. 0 ) THEN
!
! ---------------- Zenith flyby calibration
! ---------------- Set calibration's status
!
                   DO 490 J9=1,CALW%L_STA
                      IF ( CALW%ZENC_AVL(IR2,J9) ) THEN
                           CALW%ZENC_APL(IR2,J9) = .TRUE.
                         ELSE
                           CALW%ZENC_APL(IR2,J9) = .FALSE.
                           IF ( G_WARNING ) THEN
                                WRITE (  6, '(A)' ) 'WARNING: '//DBNAME_MES// &
     &                                 ' SET_CALIB: Zenith calibration '// &
     &                                   ENBCAL(J7)// &
     &                                 ' is not available for station '// &
     &                                   CALW%STANAM(J9)
                                WRITE ( 23, '(A)' ) 'WARNING: '//DBNAME_MES// &
     &                                 ' SET_CALIB: Zenith calibration '// &
     &                                   ENBCAL(J7)// &
     &                                 ' is not available for station '// &
     &                                   CALW%STANAM(J9)
                           END IF
                      END IF
 490               CONTINUE
                 ELSE
!
! ---------------- Calibration was not found
!
                   IF ( G_WARNING ) THEN
                        WRITE (  6, '(A)' ) 'WARNING: '//DBNAME_MES// &
     &                         ' SET_CALIB: Calibration '//ENBCAL(J7)// &
     &                         ' is not available'
                        WRITE ( 23, '(A)' ) 'WARNING: '//DBNAME_MES// &
     &                         ' SET_CALIB: Calibration '//ENBCAL(J7)// &
     &                         ' is not available'
                   END IF
              END IF
 470       CONTINUE
      END IF
!
      IF ( L_DIS .GT. 0 ) THEN
!
! -------- Setting off disabled calibrations
!
           DO 4100 J10=1,L_DIS
!
! ----------- Look for the calibration name in CALW
!
              IW1 = LTM_DIF ( 0, CALW%L_SCAL, CALW%SCAL, DISCAL(J10) )
              IF ( IW1 == 0  .AND. DISCAL(J10) == 'cable   ' ) THEN
!
! ---------------- Support the case when SCAL keeps lcode name of the 
! ---------------- calibration instead of the calibration name from CORFxx
!
                   CAL_LCODE = 'CABL_DEL'
                   IW1 = LTM_DIF ( 0, CALR%L_SCAL, CALR%SCAL, CAL_LCODE  )
              END IF              
!
              IF ( IW1 == 0  .AND. DISCAL(J10) == 'pcal rmv' ) THEN
!
! ---------------- Support the case when SCAL keeps lcode name of the 
! ---------------- calibration instead of the calibration name from CORFxx
!
                   CAL_LCODE = 'UNPHASCL'
                   IW1 = LTM_DIF ( 0, CALR%L_SCAL, CALR%SCAL, CAL_LCODE  )
              END IF              
              IW2 = LTM_DIF ( 0, CALW%L_ZENC, CALW%ZENC, DISCAL(J10) )
              IF ( IW1 .GT. 0 ) THEN
!
! ---------------- Set off station-dependent calibration
!
                   DO 4110 J11=1,CALW%L_STA
                      CALW%SCAL_APL(IW1,J11) = .FALSE.
 4110              CONTINUE
                ELSE IF ( IW2 .GT. 0 ) THEN
!
! ---------------- Set off zenith calibration
!
                   DO 4120 J12=1,CALW%L_STA
                      CALW%ZENC_APL(IW2,J12) = .FALSE.
 4120              CONTINUE
              END IF
 4100      CONTINUE
      END IF
!
! --- Set ionosphere calibration
!
      IF ( IONCTL .EQ. 'IN' ) THEN
!
! -------- Not to change anything: leave as it was in superfile
!
           CONTINUE
         ELSE
           DO 4130 J13=1,CALW%L_STA
              CALW%GION_APL(J13) = .FALSE.
              CALW%PION_APL(J13) = .FALSE.
              IF ( IONCTL .EQ. 'ON' ) THEN
!
! ---------------- If ON and available, then set
!
                   IF ( DATYP_INQ ( IDATYP, PHSONL__DTP ) ) THEN
!
! --------------------- Phase delay solution. Try to set PION
!
                        IF ( CALW%PION_AVL(J13) ) THEN
                             CALW%PION_APL(J13) = .TRUE.
                           ELSE IF ( CALW%GION_AVL(J13) ) THEN
                             CALW%GION_APL(J13) = .TRUE.
                        END IF
                      ELSE
!
! --------------------- ... not phase delay solution -- set GION
!
                        IF ( CALW%GION_AVL(J13) ) CALW%GION_APL(J13) = .TRUE.
                   END IF
              END IF
!@  write ( 6, * ) ' j13=', j13,' av: ', CALW%GION_AVL(J13), ' ap: ', CALW%GION_APL(J13) ! %%%%
 4130      CONTINUE
      END IF
!
! --- Writing updated calibration information back in NAMFIL block
!
      CALL ERR_PASS ( IUER, IER )
      CALL CALS_W   ( INT2(1), CALW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 2996, IUER, 'SET_CALIB', 'Error in writing '// &
     &         'calibration information back in the scratch area' )
           RETURN
      END IF
!
!@  call pause ( 'set_calib zz' ) ! %%%
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE   SET_CALIB  !#!#
