      FUNCTION   REPA_DF_FRIPLO ( DIAGI_S, REP, MODE )
! ************************************************************************
! *                                                                      *
! *   Routine REPA_DF_FRIPLO 
! *                                                                      *
! * ### 06-FEB-2010  REPA_DF_FRIPLO  v1.2 (c) L. Petrov 15-MAY-2024 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE    'solve.i'
      INCLUDE    'repa.i'
      INCLUDE    'diagi.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  REPA_DF_FRIPLO 
      INTEGER*4  MODE
      CHARACTER  STR*128, STA_NAM(2)*8, PLO_FIL*128, COM*256, &
     &           PREF_TYP*32, BAND_NAM*1, PLO_TYP*3
      LOGICAL*1  LEX
      REAL*4     XMIN, XMAX, YMIN, YMAX, XRAD_WC, YRAD_WC, RAD_MM
      REAL*4,    ALLOCATABLE :: XARR1(:), XARR2(:), XARR3(:), &
     &                          YARR1(:), YARR2(:), YARR3(:)
      INTEGER*4  J1, IND_BAS, IND_BND, IND_CLR, IND_PT, IND_OBS, &
     &           NPTS, IMARK, MARK_CLR
      LOGICAL*4, EXTERNAL :: DATYP_INQ
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! --- Determine the radii of the circle
!
      IF ( DIAGI_S%IPST(1) .EQ. 2  .OR.  DIAGI_S%IPST(1) .EQ. 4 ) THEN
           RAD_MM = DIAGI_S%RAD_SMALL
           NPTS   = NPTS_SMALL
         ELSE IF ( DIAGI_S%IPST(1) .EQ. 3  .OR.  DIAGI_S%IPST(1) .EQ. 5 ) THEN
           RAD_MM = DIAGI_S%RAD_LARGE
           NPTS   = NPTS_LARGE
      END IF
!
      XMIN = DIAGI_S%XMIN
      XMAX = DIAGI_S%XMAX
      YMIN = DIAGI_S%YMIN
      YMAX = DIAGI_S%YMAX
      IMARK = 18
      MARK_CLR = 2
!
      XRAD_WC = RAD_MM*(XMAX-XMIN)/(DIAGI_S%XRIGHT - DIAGI_S%XLEFT )
      YRAD_WC = RAD_MM*(YMAX-YMIN)/(DIAGI_S%YTOP   - DIAGI_S%YBOT  )
      CALL PGCOL_RGB ( MARK_CLR, IRGB_DEF(2,1,1), IRGB_DEF(2,1,2), &
     &                                            IRGB_DEF(2,1,3)  )
      ALLOCATE ( XARR1(MAX(1,DIAGI_S%NPOI(1))) )
      ALLOCATE ( YARR1(MAX(1,DIAGI_S%NPOI(1))) )
      ALLOCATE ( XARR2(MAX(1,DIAGI_S%NPOI(2))) )
      ALLOCATE ( YARR2(MAX(1,DIAGI_S%NPOI(2))) )
      ALLOCATE ( XARR3(MAX(1,DIAGI_S%NPOI(3))) )
      ALLOCATE ( YARR3(MAX(1,DIAGI_S%NPOI(3))) )
!
      IF ( DIAGI_S%NCLR .GE. 1 .AND. DIAGI_S%NPOI(1) > 0 ) THEN
           CALL LIB$MOVC3 ( 4*DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), &
     &                      XARR1 )
           CALL LIB$MOVC3 ( 4*DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_Y4(1)), &
     &                      YARR1 )
      END IF
      IF ( DIAGI_S%NCLR .GE. 2 .AND. DIAGI_S%NPOI(2) > 0 ) THEN
           CALL LIB$MOVC3 ( 4*DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), &
     &                      XARR2 )
           CALL LIB$MOVC3 ( 4*DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_Y4(2)), &
     &                      YARR2 )
      END IF
      IF ( DIAGI_S%NCLR .GE. 3 .AND. DIAGI_S%NPOI(3) > 0 ) THEN
           CALL LIB$MOVC3 ( 4*DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), &
     &                      XARR3 )
           CALL LIB$MOVC3 ( 4*DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_Y4(3)), &
     &                      YARR3 )
      END IF
!
! --- Determine the index of the baseline which corresponds to the current
! --- window
!
      IND_BAS = 0
      DO 410 J1=1,REP%N_BAS
         IF ( LOC(REP%DIAGI(J1)) .EQ. LOC(DIAGI_S) ) IND_BAS = J1
 410  CONTINUE 
!
! --- Find the point which is the closest to the cursor
!
      CALL REPA_SEARCH_CLOSEST ( DIAGI_S%XC, DIAGI_S%YC, DIAGI_S%NCLR,        &
           DIAGI_S%NPOI(1), %VAL(DIAGI_S%ADR_X4(1)), %VAL(DIAGI_S%ADR_Y4(1)), &
           DIAGI_S%NPOI(2), %VAL(DIAGI_S%ADR_X4(2)), %VAL(DIAGI_S%ADR_Y4(2)), &
     &     DIAGI_S%NPOI(3), %VAL(DIAGI_S%ADR_X4(3)), %VAL(DIAGI_S%ADR_Y4(3)), &
     &     DIAGI_S%XMIN, DIAGI_S%XMAX, DIAGI_S%YMIN, DIAGI_S%YMAX,            &
     &     IND_CLR, IND_PT )
!
      IF ( IND_PT .EQ. 0 ) THEN
           DEALLOCATE ( XARR1 )
           DEALLOCATE ( YARR1 )
           DEALLOCATE ( XARR2 )
           DEALLOCATE ( YARR2 )
           DEALLOCATE ( XARR3 )
           DEALLOCATE ( YARR3 )
!
           REPA_DF_FRIPLO = 1
           RETURN 
      END IF
!
      IF ( IND_CLR .EQ. REPA__I_GOO ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_GOO(IND_PT)
        ELSE IF ( IND_CLR .EQ. REPA__I_BAD ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_BAD(IND_PT)
        ELSE IF ( IND_CLR .EQ. REPA__I_UNR ) THEN
           IND_OBS = REP%PLT(IND_BAS)%IND_UNR(IND_PT)
      END IF
!
      STA_NAM(1) = REP%LIS%C_BAS(IND_BAS)(1:8)
      STA_NAM(2) = REP%LIS%C_BAS(IND_BAS)(9:16)
      CALL VTD_NAME_REPAIR ( STA_NAM(1) )
      CALL VTD_NAME_REPAIR ( STA_NAM(2) )
      CALL TRAN ( 12, STA_NAM(1), STA_NAM(1)  ) 
      CALL TRAN ( 12, STA_NAM(2), STA_NAM(2)  ) 
!
      IF ( MODE == 1 .OR. MODE == 4 ) THEN
           PREF_TYP = 'fr1d_frq_'
         ELSE IF ( MODE == 2 .OR. MODE == 5 ) THEN
           PREF_TYP = 'fr1d_tim_'
         ELSE IF ( MODE == 3 .OR. MODE == 6 ) THEN
           PREF_TYP = 'fr1d_drf_'
      END IF
      IF ( MODE == 1 .OR. MODE == 2 .OR. MODE == 3 ) THEN
           IF ( DATYP_INQ ( REP%DATYP_I2, COMB__DTP ) ) THEN
                IND_BND = 1
              ELSE
                IF ( DATYP_INQ ( REP%DATYP_I2, XBAND__DTP ) ) THEN
                     IND_BND = 1
                   ELSE
                     IND_BND = 2
                END IF
           END IF
         ELSE IF ( MODE == 4 .OR. MODE == 5 .OR. MODE == 6 ) THEN
           IF ( DATYP_INQ ( REP%DATYP_I2, COMB__DTP ) ) THEN
                IND_BND = 2
              ELSE
                IF ( DATYP_INQ ( REP%DATYP_I2, XBAND__DTP ) ) THEN
                     IND_BND = 2
                   ELSE
                     IND_BND = 1
                END IF
           END IF
      END IF
!
      CALL TRAN ( 12, REP%BAND_NAM(IND_BND), BAND_NAM )
!
! --- Build the plot file name
!
      PLO_FIL = REP%FPL_DIR(1:I_LEN(REP%FPL_DIR))//'/'// &
     &         PREF_TYP(1:I_LEN(PREF_TYP))// &
     &         REP%OBS(IND_OBS)%SCAN_PIMA(1:9)// &
     &         '_'//BAND_NAM//'_'//STA_NAM(1)//'_'//STA_NAM(2)//'_all.gif'
      CALL BLANK_TO_UNDERSCORE ( PLO_FIL(1:I_LEN(PLO_FIL)) )
      INQUIRE ( FILE=PLO_FIL, EXIST=LEX )
      PLO_TYP = 'gif'
      IF ( .NOT. LEX ) THEN
           PLO_FIL = REP%FPL_DIR(1:I_LEN(REP%FPL_DIR))//'/'// &
     &               PREF_TYP(1:I_LEN(PREF_TYP))// &
     &               REP%OBS(IND_OBS)%SCAN_PIMA(1:9)// &
     &               '_'//BAND_NAM//'_'//STA_NAM(1)//'_'//STA_NAM(2)//'_all.ps'
           CALL BLANK_TO_UNDERSCORE ( PLO_FIL(1:I_LEN(PLO_FIL)) )
           PLO_TYP = 'ps'
           INQUIRE ( FILE=PLO_FIL, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                PLO_FIL = REP%FPL_DIR(1:I_LEN(REP%FPL_DIR))//'/'// &
     &                    PREF_TYP(1:I_LEN(PREF_TYP))// &
     &                    REP%OBS(IND_OBS)%SCAN_PIMA(1:9)// &
     &                    '_'//BAND_NAM//'_'//STA_NAM(2)//'_'//STA_NAM(1)//'_all.gif'
                CALL BLANK_TO_UNDERSCORE ( PLO_FIL(1:I_LEN(PLO_FIL)) )
                INQUIRE ( FILE=PLO_FIL, EXIST=LEX )
                PLO_TYP = 'gif'
                IF ( .NOT. LEX ) THEN
                     PLO_FIL = REP%FPL_DIR(1:I_LEN(REP%FPL_DIR))//'/'// &
     &                         PREF_TYP(1:I_LEN(PREF_TYP))// &
     &                         REP%OBS(IND_OBS)%SCAN_PIMA(1:9)// &
     &                         '_'//BAND_NAM//'_'//STA_NAM(2)//'_'//STA_NAM(1)//'_all.ps'
                     CALL BLANK_TO_UNDERSCORE ( PLO_FIL(1:I_LEN(PLO_FIL)) )
                     INQUIRE ( FILE=PLO_FIL, EXIST=LEX )
                     PLO_TYP = 'ps'
                END IF
           END IF
      END IF
!
      IF ( LEX ) THEN
           CALL PGBBUF  ! starting bufferization
           IF ( DIAGI_S%IPQ .EQ. 0  .OR.  DIAGI_S%ICQ .EQ. 0 ) THEN
                WRITE ( 6, '(A)' ) 'Found file '//PLO_FIL(1:I_LEN(PLO_FIL))// &
     &                             ' but cannot identify the point' 
                CONTINUE 
              ELSE
!
! ------------- There were previous requests
!
                CALL PGSAVE()
!
! ------------- Extinguishing the marker at the point previously requested
!
                CALL PGSCI  ( 0 )
                IF ( DIAGI_S%ICQ .EQ. 1  .AND.  DIAGI_S%IPQ .LE. DIAGI_S%NPOI(1) ) THEN
                     CALL PGPT ( 1, XARR1(DIAGI_S%IPQ), YARR1(DIAGI_S%IPQ), IMARK )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 2  .AND.  DIAGI_S%IPQ .LE. DIAGI_S%NPOI(2) ) THEN
                     CALL PGPT ( 1, XARR2(DIAGI_S%IPQ), YARR2(DIAGI_S%IPQ), IMARK )
                   ELSE IF ( DIAGI_S%ICQ .EQ. 3  .AND.  DIAGI_S%IPQ .LE. DIAGI_S%NPOI(3) ) THEN
                     IF ( DIAGI_S%IPQ .LE. DIAGI_S%NPOI(3) ) THEN
                          CALL PGPT ( 1, XARR3(DIAGI_S%IPQ), YARR3(DIAGI_S%IPQ), IMARK )
                     END IF
                END IF
!
! ------------- Lighting again the point previously requested since marker spoiled
! ------------- point representation
!
                CALL PGSCI  ( ITAB_CLR(DIAGI_S%ICQ,1) )
                IF ( DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 1 ) THEN
                     IF ( DIAGI_S%ICQ .EQ. 1  .AND.  DIAGI_S%IPQ .LE. DIAGI_S%NPOI(1) ) THEN
                          CALL PGPNTS ( 1, XARR1(DIAGI_S%IPQ), YARR1(DIAGI_S%IPQ), &
     &                                  1, 1, 1 )
                        ELSE IF ( DIAGI_S%ICQ .EQ. 2  .AND.  DIAGI_S%IPQ .LE. DIAGI_S%NPOI(2) ) THEN
                          CALL PGPNTS ( 1, XARR2(DIAGI_S%IPQ), YARR2(DIAGI_S%IPQ), &
     &                                  1, 1, 1 )
                        ELSE IF ( DIAGI_S%ICQ .EQ. 3  .AND.  DIAGI_S%IPQ .LE. DIAGI_S%NPOI(3) ) THEN
                          CALL PGPNTS ( 1, XARR3(DIAGI_S%IPQ), YARR3(DIAGI_S%IPQ), &
     &                                  1, 1, 1 )
                     END IF
                  ELSE IF ( DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 2  .OR.  &
     &                      DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 3        ) THEN
!
! ------------------ Outlined circles
!
                     CALL PGSLW ( 1 )
                     CALL PGSFS ( 1 )
                     CALL PGSCI ( 0 )
                     IF ( DIAGI_S%ICQ .EQ. 1 ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR1(DIAGI_S%IPQ), &
     &                                      YARR1(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                        ELSE IF ( DIAGI_S%ICQ .EQ. 2 ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR2(DIAGI_S%IPQ), &
     &                                      YARR2(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                        ELSE IF ( DIAGI_S%ICQ .EQ. 3 ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR3(DIAGI_S%IPQ), &
     &                                      YARR3(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                     END IF
                     CALL PGSFS ( 2 )
                     CALL PGSCI ( ITAB_CLR(DIAGI_S%ICQ,1) )
                     IF ( DIAGI_S%ICQ .EQ. 1 ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR1(DIAGI_S%IPQ), &
     &                                      YARR1(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                        ELSE IF ( DIAGI_S%ICQ .EQ. 2 ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR2(DIAGI_S%IPQ), &
     &                                      YARR2(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                        ELSE IF ( DIAGI_S%ICQ .EQ. 3 ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR3(DIAGI_S%IPQ), &
     &                                      YARR3(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                     END IF
                  ELSE IF ( DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 4  .OR.  &
     &                      DIAGI_S%IPST(DIAGI_S%ICQ) .EQ. 5        ) THEN
                     CALL PGSLW ( 1 )
                     CALL PGSFS ( 1 )
                     CALL PGSCI ( ITAB_CLR(DIAGI_S%ICQ,1) )
                     IF ( DIAGI_S%ICQ .EQ. 1  .AND.  DIAGI_S%IPQ < DIAGI_S%NPOI(1) ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR1(DIAGI_S%IPQ), &
     &                                      YARR1(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                        ELSE IF ( DIAGI_S%ICQ .EQ. 2  .AND.  DIAGI_S%IPQ < DIAGI_S%NPOI(2) ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR2(DIAGI_S%IPQ), &
     &                                      YARR2(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                        ELSE IF ( DIAGI_S%ICQ .EQ. 3  .AND.  DIAGI_S%IPQ < DIAGI_S%NPOI(3) ) THEN
                          CALL PGCIRC_PET ( NPTS, XARR3(DIAGI_S%IPQ), &
     &                                      YARR3(DIAGI_S%IPQ), XRAD_WC, YRAD_WC )
                     END IF
                END IF
                CALL PGUNSA()
           END IF
           CALL PGSAVE()
           CALL PGSCI ( ITAB_CLR(2,2) )
!
! -------- Extinguishing this point at the plot
!
           IF ( DIAGI_S%IPST(IND_CLR) .EQ. 1 ) THEN
                IF ( IND_CLR .EQ. 1 ) THEN
                     CALL PGPNTS ( 1, XARR1(IND_PT), YARR1(IND_PT), 1, 1, 1 )
                   ELSE IF ( IND_CLR .EQ. 2 ) THEN
                     CALL PGPNTS ( 1, XARR2(IND_PT), YARR2(IND_PT), 1, 1, 1 )
                   ELSE IF ( IND_CLR .EQ. 3 ) THEN
                     CALL PGPNTS ( 1, XARR3(IND_PT), YARR3(IND_PT), 1, 1, 1 )
                END IF
              ELSE IF ( DIAGI_S%IPST(IND_CLR) .EQ. 2  .OR.  &
     &                  DIAGI_S%IPST(IND_CLR) .EQ. 3        ) THEN
!
! ------------- Outlined circles
!
                CALL PGSFS ( 2  )
                IF ( IND_CLR .EQ. 1 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR1(IND_PT), YARR1(IND_PT), &
     &                                 XRAD_WC, YRAD_WC )
                   ELSE IF ( IND_CLR .EQ. 2 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR2(IND_PT), YARR2(IND_PT), &
     &                                 XRAD_WC, YRAD_WC )
                   ELSE IF ( IND_CLR .EQ. 3 ) THEN
                     CALL PGCIRC_PET ( NPTS, XARR3(IND_PT), YARR3(IND_PT), &
     &                                 XRAD_WC, YRAD_WC )
                END IF  
              ELSE IF ( DIAGI_S%IPST(IND_CLR) .EQ. 4  .OR.  &
     &                  DIAGI_S%IPST(IND_CLR) .EQ. 5        ) THEN
!
! -------------- Filled circles
!
                 CALL PGSFS ( 1  )
                 IF ( IND_CLR .EQ. 1 ) THEN
                      CALL PGCIRC_PET ( NPTS, XARR1(IND_PT), YARR1(IND_PT), &
     &                                  XRAD_WC, YRAD_WC )
                   ELSE IF ( IND_CLR .EQ. 2 ) THEN
                      CALL PGCIRC_PET ( NPTS, XARR2(IND_PT), YARR2(IND_PT), &
     &                                  XRAD_WC, YRAD_WC )
                   ELSE IF ( IND_CLR .EQ. 3 ) THEN
                      CALL PGCIRC_PET ( NPTS, XARR3(IND_PT), YARR3(IND_PT), &
     &                                  XRAD_WC, YRAD_WC )
                 END IF  
           END IF
!
! -------- Putting a marker with warning light
!
           CALL PGSCI ( MARK_CLR )
           IF ( IND_CLR .EQ. 1 ) THEN
                CALL PGPT  ( 1, XARR1(IND_PT), YARR1(IND_PT), IMARK )
             ELSE IF ( IND_CLR .EQ. 2 ) THEN
                CALL PGPT  ( 1, XARR2(IND_PT), YARR2(IND_PT), IMARK )
             ELSE IF ( IND_CLR .EQ. 3 ) THEN
                CALL PGPT  ( 1, XARR3(IND_PT), YARR3(IND_PT), IMARK )
           END IF
           CALL PGUNSA()
           CALL PGUPDT()
!
           IF ( PLO_TYP == 'gif' ) THEN
                COM = REP%GIF_VIEWER(1:I_LEN(REP%GIF_VIEWER))//' '//PLO_FIL
              ELSE IF ( PLO_TYP == 'ps' ) THEN
                COM = REP%SOLVE_PS_VIEWER(1:I_LEN(REP%SOLVE_PS_VIEWER))//' '//PLO_FIL
           END IF
           CALL SYSTEM ( COM(1:I_LEN(COM))//CHAR(0) )
!
           DIAGI_S%IPQ = IND_PT
           DIAGI_S%ICQ = IND_CLR
        ELSE
           WRITE ( 6, '(A)' ) 'Not found file '//PLO_FIL(1:I_LEN(PLO_FIL))
      END IF
!
      DEALLOCATE ( XARR1 )
      DEALLOCATE ( YARR1 )
      DEALLOCATE ( XARR2 )
      DEALLOCATE ( YARR2 )
      DEALLOCATE ( XARR3 )
      DEALLOCATE ( YARR3 )
!
      REPA_DF_FRIPLO = 1
      RETURN
      END  FUNCTION  REPA_DF_FRIPLO  !#!#
