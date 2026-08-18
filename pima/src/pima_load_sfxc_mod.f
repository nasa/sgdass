      SUBROUTINE PIMA_LOAD_SFXC_MOD ( PIM, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_LOAD_KJCC_MOD  reads the directory of files in SFXC   *
! *   internal binary format with VLBI interferomeric model, parses      *
! *   them, and creates MOD and MDC sub-objects for the station objects  *
! *   in the main PIMA data structure.                                   *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       PIM ( PIMA__TYP ) -- Object with information related to        *
! *                            program PIMA.                             *
! *       VTD ( PIMA__TYP ) -- Object with information related to        *
! *                            the package VTD for computation of the    *
! *                            VLBI Time Delay.                          *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                      Input: IUER=0 -- no error message will be       *
! *                                       printed even in the case       *
! *                                       of error.                      *
! *                             IUER=-1,-2,-3 -- in the case of error    *
! *                                       the message will be put on     *
! *                                       stdout.                        *
! *                             IUER=-3 -- in the case of error after    *
! *                                        printing the error message    *
! *                                        the program will terminate.   *
! *                       Output:                                        *
! *                             if input value of IUER =-2,-3 -- IUER    *
! *                                        is not modified.              *
! *                             otherwise, the output value of IUER is 0 *
! *                             in the case of successful and            *
! *                             positive non-zero in the vase of errors. *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 13-SEP-2015 PIMA_LOAD_SFXC_MOD v2.1 (d) L. Petrov 19-DEC-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE      ) :: PIM
      TYPE     ( VTD__TYPE       ) :: VTD
      TYPE     ( VTD__OBS_TYPE   ) :: OBS_TYP
      TYPE     ( PIMA__SFXC_TYPE ), POINTER :: MOD(:) 
      INTEGER*4  IUER
      INTEGER*4    M_BUF, MIND
      PARAMETER  ( M_BUF = 1024 )
      PARAMETER  ( MIND  =   32 )
      CHARACTER  FINAM*128, MDC_FILE*128, MDU_FILE*128, MOB_FILE*128, &
     &           MOD_FILE*128, FILCLK*128, &
     &           BUF(M_BUF)*128, DATE_STR*19, STR*128, STR1*128, ERRSTR*128
      INTEGER*8  DIR_DESC(16)
      REAL*8     TIM_SCAN_EPS, TIM_MOD_MIN_STEP 
      PARAMETER  ( TIM_SCAN_EPS     = 90.0D0 )
      PARAMETER  ( TIM_MOD_MIN_STEP = 0.01D0 )
      REAL*8     TIM_ARR(PIM__MUV), DEL_ARR(PIM__MUV), DEL_SPL(PIM__MUV), &
     &           DEL_TMP(PIM__MUV), DEL_FIRST, DEL(0:2), TIM_POI, &
     &           TIM_OBS_BEG, TIM_OBS_END, TIM_MOD_BEG, TIM_MOD_END, &
     &           TAI_CLK_EPC, UTC_CLK_EPC, CLK_VAL, CLK_RATE
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, J13, J14, &
     &           J15, J16, J17, J18, J19, J20, ISTA, &
     &           LEV, IL, IP, IS, L_FIL, M_MOD, L_MOD, L_ARR, &
     &           IND_STA, IND_SOU, IND_SCA, IND_MOD_BEG, IND_MOD_END, &
     &           LAST_IND_SCA, LAST_IND_STA, LAST_IND_MOD, FIRST_IND_MOD, &
     &           IND_POI, K_MOD(PIM__MSTA), LUN_MDU, LUN_MOB, LUN_MOD, &
     &           LUN_MDC, N_CLK, IND(2,MIND), LIND, MJD_CLK_EPC, IER
      LOGICAL*1  FL_FOU(2), FL_DIR
      REAL*8,    EXTERNAL :: FSPL8, DSPL8, D2SPL8
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, GET_UNIT, ILEN, I_LEN, &
     &                       IXMN8, LTM_DIF
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, VEX_TO_DATE*19
      LOGICAL*1, EXTERNAL :: IS_DIR_EXIST
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_SFXC, PIMA_COMPAR_MOD
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_SFXC, PIMA_COMPAR_MOD
#endif
!
      M_MOD = 16 * PIM%NEPC * PIM%NSTA
      ALLOCATE ( MOD(M_MOD), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 16 * PIM%NEPC * PIM%NSTA * SIZEOF(MOD(1)), STR )
           CALL ERR_LOG ( 6411, IUER, 'PIMA_LOAD_SFXC_MOD', 'Trap of '// &
     &         'internal control: failure in an attempt to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes od dynamic memory for '// &
     &         'array MOD' )
           RETURN 
      END IF
      L_FIL = 0
      L_MOD = 0
      LEV   = 0
      CALL CLRCH ( FILCLK ) 
!  
      DO 410 J1=1,PIM%CONF%L_INM
         FL_DIR = IS_DIR_EXIST ( PIM%CONF%INTMOD_FILE(J1), ERRSTR )
         IF ( FL_DIR ) THEN
              DO 420 J2=1,PIM__MEPC   
                 IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, PIM%CONF%INTMOD_FILE(J1), FINAM )
                 IF ( IS .NE. 0 ) THEN
                      CALL ERR_LOG ( 6412, IUER, 'PIMA_LOAD_SFXC_MOD', 'Error '// &
     &                    'in reading input directory '// &
     &                     PIM%CONF%INTMOD_FILE(J1)(1:I_LEN(PIM%CONF%INTMOD_FILE(J1)))// &
     &                    ' with SFXC Inteferometric Model: '//FINAM )
                      RETURN 
                 END IF
                 IF ( LEV == 0 ) GOTO 820 ! End of work
!
                 IL = ILEN(FINAM)
                 IF ( IL < 10 ) GOTO 420
                 IF ( FINAM(IL-3:IL) == '.del' ) THEN
                      L_FIL = L_FIL + 1
                      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                           WRITE ( 6, * ) 'Processing input file '//TRIM(FINAM)
                      END IF
                      CALL ERR_PASS ( IUER, IER )
                      CALL PIMA_PARSE_SFXC_MOD ( FINAM, L_FIL, M_MOD, L_MOD, MOD, &
     &                                           PIM%UTC_MTAI, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 6413, IUER, 'PIMA_LOAD_SFXC_MOD', 'Error '// &
     &                         'in parsing SFXC delay file '//FINAM )
                           RETURN 
                      END IF
                   ELSE IF ( FINAM(IL-3:IL) == '.clk' ) THEN
                      FILCLK = FINAM
                 END IF
 420          CONTINUE 
 820          CONTINUE 
            ELSE
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                   WRITE ( 6, * ) 'Processing input file '//TRIM(PIM%CONF%INTMOD_FILE(J1))
              END IF
              CALL ERR_PASS ( IUER, IER )
              CALL PIMA_PARSE_SFXC_MOD ( PIM%CONF%INTMOD_FILE(J1), L_FIL, M_MOD, &
     &                                   L_MOD, MOD, PIM%UTC_MTAI, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6414, IUER, 'PIMA_LOAD_SFXC_MOD', 'Error '// &
     &                 'in parsing SFXC delay file '//PIM%CONF%INTMOD_FILE(J1) )
                   RETURN 
              END IF
              L_FIL = L_FIL + 1
         END IF
 410  CONTINUE 
!
      IF ( L_MOD < 1 ) THEN
           CALL ERR_LOG ( 6415, IUER, 'PIMA_LOAD_SFXC_MOD', 'Did not '// &
     &         'found delay files' )
           DEALLOCATE ( MOD )
           RETURN 
      END IF
!
      CALL FOR_QSORT ( MOD, L_MOD, SIZEOF(MOD(1)), PIMA_COMPAR_SFXC )
!
      DO 430 J3=1,PIM%NSTA
         IND_MOD_BEG = 0
         IND_MOD_END = 0
         DO 440 J4=1,L_MOD
            IF ( PIM%STA(J3)%ORIG_NAME == MOD(J4)%STA_NAM ) THEN
                 IF ( IND_MOD_BEG == 0 ) IND_MOD_BEG = J4
                 IND_MOD_END = J4
            END IF
!
            IND_SOU = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, MOD(J4)%SOU_NAM )
            IF ( J3 == 1 .AND. IND_SOU < 1 ) THEN
                 DO 450 J5=1,PIM%NSOU
                    IF ( MOD(J4)%SOU_NAM == PIM%SOU(J5)%DB_NAME ) THEN
                         IND_SOU = J5
                         MOD(J4)%SOU_NAM = PIM%C_SOU(IND_SOU)
                   END IF
 450             CONTINUE 
            END IF
 440     CONTINUE 
!
         IF ( IND_MOD_BEG == 0 ) THEN
              IF ( PIM%CONF%CHECK_SEVERITY == 2 ) THEN
                   CALL ERR_LOG ( 6416, IUER, 'PIMA_LOAD_SFXC_MOD', 'Error during '// &
     &                 'parsing SFXC delay files: no delay model was found '// &
     &                 'for station '//PIM%STA(J3)%NAME//' that observed '// &
     &                 'in experiment '//TRIM(PIM%CONF%SESS_CODE)//' -- you may '// &
     &                 'consider setting CHECK_SEVERITY: 1 if you are sure' )
                   DEALLOCATE ( MOD )
                   RETURN 
                ELSE
                   WRITE ( 6, * ) 'PIMA_LOAD_SFXC_MOD: Error during '// &
     &                 'parsing SFXC delay files: no delay model was found '// &
     &                 'for station '//PIM%STA(J3)%NAME//' that observed '// &
     &                 'in experiment '//TRIM(PIM%CONF%SESS_CODE)
                   WRITE ( 6, * ) 'Nevertheless, continiue'
              END IF
         END IF
!
         PIM%STA(J3)%L_MOD = 0
         DO 460 J6=IND_MOD_BEG,IND_MOD_END 
            IF ( J6 > IND_MOD_BEG ) THEN
                 IF ( .NOT. ( MOD(J6)%STA_NAM == MOD(J6-1)%STA_NAM .AND. &
     &                        MOD(J6)%SOU_NAM == MOD(J6-1)%SOU_NAM .AND. &
     &                        MOD(J6)%MJD     == MOD(J6-1)%MJD     .AND. &
     &                        ( MOD(J6)%TAI - MOD(J6-1)%TAI ) < TIM_MOD_MIN_STEP ) ) THEN
!
                      PIM%STA(J3)%L_MOD = PIM%STA(J3)%L_MOD + 1
                 END IF
               ELSE 
                 PIM%STA(J3)%L_MOD = PIM%STA(J3)%L_MOD + 1
            END IF
 460     CONTINUE 
!
         IF ( ASSOCIATED ( PIM%STA(J3)%MOD ) ) THEN
              DEALLOCATE ( PIM%STA(J3)%MOD )
         END IF
         ALLOCATE ( PIM%STA(J3)%MOD(PIM%STA(J3)%L_MOD), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL IINCH ( PIM%STA(J3)%L_MOD*SIZEOF(PIM%STA(J3)%MOD), STR )
              CALL ERR_LOG ( 6417, IUER, 'PIMA_LOAD_SFXC_MOD', 'Failure to '// &
     &            'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &            'memory for PIM%STA(J3)%MOD object' )
              RETURN
         END IF
 430  CONTINUE 
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           WRITE ( 6, 210 ) PIM%CONF%INTMOD_FILE(1)(1:I_LEN(PIM%CONF%INTMOD_FILE(1))), &
     &                      L_FIL, L_MOD
 210       FORMAT ( ' SFXC_MOD: Model dir:  ', A, ' L_fil= ', I4, ' L_mod= ', I8 )
      END IF
!
      LAST_IND_SCA = 0
      LAST_IND_STA = 0
      K_MOD = 0
      L_ARR = 0
      DO 470 J7=1,L_MOD
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              STR = MJDSEC_TO_DATE ( MOD(J7)%MJD, MOD(J7)%TAI, IER )
              WRITE  ( 6, 220 ) J7, MOD(J7)%STA_NAM, MOD(J7)%MJD, &
     &                          MOD(J7)%TAI, MOD(J7)%SOU_NAM, STR(1:21)
 220          FORMAT ( 'SFXC_MOD: ', I7, 2X, A, 2X, I5, 2X, F8.2, 2X, A, 2X, A, ' TAI' )
         END IF
         IND_SOU = LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, MOD(J7)%SOU_NAM )
         IF ( IND_SOU < 1 ) THEN
              CALL ERR_LOG ( 6418, IUER, 'PIMA_LOAD_SFXC_MOD', 'Error during '// &
     &            'parsing SFXC delay files: source '//MOD(J7)%SOU_NAM// &
     &            ' definded there did not observe in experiment '//PIM%CONF%SESS_CODE )
              DEALLOCATE ( MOD )
              RETURN 
         END IF
!
         IND_STA = 0
         DO 480 J8=1,PIM%NSTA
            IF ( PIM%STA(J8)%ORIG_NAME == MOD(J7)%STA_NAM ) THEN
                 IND_STA = J8
            END IF
 480     CONTINUE 
         IF ( IND_STA < 1 ) THEN
              CALL ERR_LOG ( 6419, IUER, 'PIMA_LOAD_SFXC_MOD', 'Error during '// &
     &            'parsing SFXC delay files: station '//MOD(J7)%STA_NAM// &
     &            ' defined there did not observe in experiment '//PIM%CONF%SESS_CODE )
              DEALLOCATE ( MOD )
              RETURN 
         END IF
!
         IND_SCA = 0
         DO 490 J9=1,PIM%NSCA
            IF ( PIM%SCA(J9)%SOU_IND == IND_SOU ) THEN
                 IF ( (MOD(J7)%MJD*86400.0D0 + MOD(J7)%TAI - (PIM%MJD_0*86400.0D0 + PIM%TAI_0)) > &
     &                PIM%TIM_R8(PIM%SCA(J9)%TIM_IND) - TIM_SCAN_EPS .AND. &
     &                (MOD(J7)%MJD*86400.0D0 + MOD(J7)%TAI - (PIM%MJD_0*86400.0D0 + PIM%TAI_0)) < &
     &                PIM%TIM_R8(PIM%SCA(J9)%TIM_IND+PIM%SCA(J9)%NUM_EPC-1) + TIM_SCAN_EPS ) THEN
                      IND_SCA = J9
                 END IF
            END IF
 490     CONTINUE 
         IF ( IND_SCA == 0 ) THEN
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                   WRITE ( 6, * ) 'PIMA_LOAD_SFXC-278 j7= ', j7, ' sta= ', MOD(J7)%STA_NAM, &
     &                            ' sou= ', MOD(J7)%SOU_NAM, ' tai= ', MOD(J7)%TAI
              END IF
              GOTO 470 ! Bypass the record that does not correspond to any observation
         END IF
!
         IF ( .NOT. ( IND_SCA == LAST_IND_SCA .AND. IND_STA == LAST_IND_STA ) ) THEN
              LAST_IND_MOD = L_MOD
              FIRST_IND_MOD = J7
              DO 4100 J10=J7+1,L_MOD
                 IF ( LTM_DIF ( 0, PIM%NSOU, PIM%C_SOU, MOD(J10)%SOU_NAM ) .NE. IND_SOU ) THEN
                      LAST_IND_MOD = J10 - 1
                      GOTO 8100
                 END IF
 4100         CONTINUE 
 8100         CONTINUE 
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                   WRITE ( 6, * ) 'PIMA_LOAD_SFXC_MOD Sta: ', MOD(J7)%STA_NAM, MOD(J7)%SOU_NAM, &
     &                             J7, LAST_IND_MOD
                   CALL FLUSH ( 6 )
              END IF
!
              DEL_FIRST = MOD(FIRST_IND_MOD)%DEL
              L_ARR = 0
              DO 4110 J11=FIRST_IND_MOD,LAST_IND_MOD
                 L_ARR = L_ARR + 1
                 TIM_ARR(L_ARR) = (MOD(J11)%MJD*86400.0D0 + MOD(J11)%TAI) - &
     &                            (PIM%MJD_0*86400.0D0 + PIM%TAI_0)
                 DEL_ARR(L_ARR) = MOD(J11)%DEL - DEL_FIRST
                 IF ( L_ARR > 1 ) THEN
                      IF ( ( TIM_ARR(L_ARR) - TIM_ARR(L_ARR-1) ) < TIM_MOD_MIN_STEP ) THEN
                           L_ARR = L_ARR - 1
                           GOTO 4110
                      END IF
                 END IF
!                 WRITE ( 6, * ) ' l_arr= ', int2(l_arr), ' TIM = ', sngl(TIM_ARR(l_arr)), &
!     &                          ' sou= ', mod(j11)%sou_nam, &
!     &                          ' sta= ', mod(j11)%sta_nam, & ! %%%%%%%%%%%%%%%%%%W
!     &                          ' fin= ', int2(mod(j11)%fil_del_ind) ! %%%%%%%%%%%%%%%%%%W
 4110         CONTINUE 
              IF ( L_ARR > 3 ) THEN
                   CALL ERR_PASS    ( IUER, IER )
                   CALL MAKE_SPLINE ( 1, L_ARR, TIM_ARR, DEL_ARR, 0.0D0, 0.0D0, &
     &                                DEL_SPL, DEL_TMP, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 6420, IUER, 'PIMA_LOAD_SFXC_MOD', 'Error in '// &
     &                      'an attempt to comute iterpolating spline for SFXC delay' )
                        DEALLOCATE ( MOD )
                        RETURN 
                   END IF
                 ELSE
                   CALL ERR_LOG ( 6421, IUER, 'PIMA_LOAD_SFXC_MOD', 'Trap of internal '// &
     &                 'control: too few points of apriori path delay for a '// &
     &                 'station '//PIM%C_STA(IND_STA)//' scan '// &
     &                  PIM%SCA(IND_SCA)%SCAN_NAME )
                   DEALLOCATE ( MOD )
                   RETURN 
              END IF
         END IF
!         
         TIM_POI = (MOD(J7)%MJD*86400.0D0 + MOD(J7)%TAI) - &
     &             (PIM%MJD_0*86400.0D0 + PIM%TAI_0)
         IF ( J7 == FIRST_IND_MOD ) THEN
              IND_POI = 1
            ELSE 
              IND_POI = IXMN8 ( L_ARR, TIM_ARR, TIM_POI )
         END IF
         IF ( IND_POI < 1 ) THEN
              WRITE ( 6, * ) 'L_ARR=', L_ARR, ' TIM_ARR= ', TIM_ARR(1), TIM_ARR(L_ARR), ' TIM_POI= ', TIM_POI 
              CALL ERR_LOG ( 6422, IUER, 'PIMA_LOAD_SFXC_MOD', 'Trap of internal '// &
     &            'control: ind_poi is less than 1' )
              DEALLOCATE ( MOD )
              RETURN 
         END IF
         DEL(0) = FSPL8  ( TIM_POI, L_ARR, TIM_ARR, DEL_ARR, IND_POI, DEL_SPL ) + &
     &                     DEL_FIRST
         DEL(1) = DSPL8  ( TIM_POI, L_ARR, TIM_ARR, DEL_ARR, IND_POI, DEL_SPL )
         DEL(2) = D2SPL8 ( TIM_POI, L_ARR, TIM_ARR, DEL_ARR, IND_POI, DEL_SPL )/ &
     &                     2.0D0
         LAST_IND_SCA = IND_SCA
         LAST_IND_STA = IND_STA
!
         IF ( J7 > 1 ) THEN
              IF ( MOD(J7)%STA_NAM == MOD(J7-1)%STA_NAM .AND. &
     &             MOD(J7)%SOU_NAM == MOD(J7-1)%SOU_NAM .AND. &
     &             MOD(J7)%MJD     == MOD(J7-1)%MJD     .AND. &
     &             ( MOD(J7)%TAI - MOD(J7-1)%TAI ) < TIM_MOD_MIN_STEP ) THEN
!
                   IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
                        WRITE ( 6, * ) 'PIMA_LOAD_SFXC-370 j7= ', j7, ' sta= ', MOD(J7)%STA_NAM, &
     &                            ' sou= ', MOD(J7)%SOU_NAM, ' tai= ', MOD(J7)%TAI
                   END IF
                   GOTO 470
              END IF
         END IF
!
         K_MOD(IND_STA) = K_MOD(IND_STA) + 1
         PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%TIM_BEG = TIM_POI 
         IF ( IND_POI < L_ARR ) THEN
              PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%TIM_END = TIM_ARR(IND_POI+1)
            ELSE 
              PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%TIM_END = &
     &               TIM_POI + (TIM_ARR(L_ARR) - TIM_ARR(L_ARR-1) )
         END IF
         PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%SOU_IND  = IND_SOU
         IF ( IND_SCA > 0 ) THEN
              PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%SCANNAME = PIM%SCA(IND_SCA)%SCAN_NAME 
            ELSE 
              PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%SCANNAME = 'unknown'
         END IF
	 PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%GDEL_POL(0:2,1) = DEL(0:2)
	 PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%PDEL_POL(0:2,1) = DEL(0:2)
	 PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%PRAT_POL(0,1)   = DEL(1)
	 PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%PRAT_POL(1,1)   = 2.0D0*DEL(2)
	 PIM%STA(IND_STA)%MOD(K_MOD(IND_STA))%PRAT_POL(2,1)   = 0.0D0
 470  CONTINUE 
!
      PIM%NMOD = 0
      DO 4120 J12=1,PIM%NSTA
         PIM%NMOD = PIM%NMOD + PIM%STA(J12)%L_MOD
         CALL FOR_QSORT ( PIM%STA(J12)%MOD, PIM%STA(J12)%L_MOD, &
     &                    SIZEOF(PIM%STA(J12)%MOD(1)), PIMA_COMPAR_MOD )
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
              WRITE ( 6, * ) 'SFXC_MOD: Sta: ', PIM%C_STA(J12), ' L_MOD= ', PIM%STA(J12)%L_MOD
         END IF 
 4120 CONTINUE 
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 4 ) THEN
           WRITE ( 6, * ) 'SFXC_MOD: all PIM%NMOD= ', PIM%NMOD
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           IP = I_LEN(PIM%CONF%EXPER_DIR)
           MDU_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mdu'
           MOB_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mob'
           MOD_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mod'
           IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                MDU_FILE = PIM%CONF%EXPER_DIR(1:IP)//MDU_FILE
                MOB_FILE = PIM%CONF%EXPER_DIR(1:IP)//MOB_FILE
                MOD_FILE = PIM%CONF%EXPER_DIR(1:IP)//MOD_FILE
              ELSE
                MDU_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MDU_FILE
                MOB_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MOB_FILE
                MOD_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MOD_FILE
           END IF
           LUN_MDU = GET_UNIT()
           OPEN ( UNIT=LUN_MDU, FILE=MDU_FILE, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 6423, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &              'Failure to open information file '// &
     &               MDU_FILE(1:I_LEN(MDU_FILE))//' -- error '//STR )
                RETURN
           END IF
!
           LUN_MOB = GET_UNIT()
           OPEN ( UNIT=LUN_MOB, FILE=MOB_FILE, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 6424, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &              'Failure to open information file '// &
     &               MOB_FILE(1:I_LEN(MOB_FILE))//' -- error '//STR )
                RETURN
           END IF
!
           LUN_MOD = GET_UNIT()
           OPEN ( UNIT=LUN_MOD, FILE=MOD_FILE, STATUS='UNKNOWN', IOSTAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 6425, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &              'Failure to open information file '// &
     &               MOD_FILE(1:I_LEN(MOD_FILE))//' -- error '//STR )
                RETURN
           END IF
      END IF
!
! --- Cycle over observations
!
      DO 4130 J13=1,PIM%NOBS
         FL_FOU(1) = .FALSE.
         FL_FOU(2) = .FALSE.
         TIM_OBS_BEG =  1.D30
         TIM_OBS_END = -1.D30
         DO 4140 J14=1,PIM%OBS(J13)%NUVS
            IF ( PIM%OBS(J13)%UV_IND(1,J14) > 0 ) THEN
                 IF ( FL_FOU(1) ) THEN
                      TIM_OBS_BEG = MIN ( TIM_OBS_BEG, &
     &                                    PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J13)%UV_IND(1,J13))%TIM_IND) )
                   ELSE
                      TIM_OBS_BEG = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J13)%UV_IND(1,J14))%TIM_IND)
                 END IF
                 FL_FOU(1) = .TRUE.
            END IF
!
            IF ( PIM%OBS(J13)%UV_IND(PIM%OBS(J13)%NUM_EPC(J14),J14) > 0 ) THEN
                 IF ( FL_FOU(2) ) THEN
                       TIM_OBS_END = MAX ( TIM_OBS_END, &
     &                                     PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J13)%UV_IND(PIM%OBS(J13)%NUM_EPC(J14),J14))%TIM_IND) )
                    ELSE
                       TIM_OBS_END = PIM%TIM_R8(PIM%UV_IND(PIM%OBS(J13)%UV_IND(PIM%OBS(J13)%NUM_EPC(J14),J14))%TIM_IND)
                 END IF
                 FL_FOU(2) = .TRUE.
            END IF
 4140    CONTINUE
!
         IF ( .NOT. FL_FOU(1) ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J13, STR )
              CALL ERR_LOG ( 6426, IUER, 'PIMA_LOAD_SFXC_MOD', 'Trap of internal '// &
     &            'control: did not found a valid UV index for the 1st '// &
     &            'AP of observation #'//STR )
              RETURN
         END IF
!
         IF ( .NOT. FL_FOU(2) ) THEN
               CALL CLRCH ( STR )
               CALL INCH  ( J13, STR )
               CALL ERR_LOG ( 6427, IUER, 'PIMA_LOAD_SFXC_MOD', 'Trap of internal '// &
     &             'control: did not found a valid UV index for the last '// &
     &             'AP of observation #'//STR )
               RETURN
         END IF
!
         PIM%OBS(J13)%MOD_IND_BEG = 0
         PIM%OBS(J13)%MOD_IND_END = 0
         DO 4150 J15=1,2
            DO 4160 J16=1,PIM%STA(PIM%OBS(J13)%STA_IND(J15))%L_MOD
!
! ------------ Do not consider sources others than the source observed
! ------------ in the J13-th observation
!
               IF ( PIM%STA(PIM%OBS(J13)%STA_IND(J15))%MOD(J16)%SOU_IND .NE. &
     &              PIM%OBS(J13)%ROOT_SOU_IND ) GOTO 4160
!
               TIM_MOD_BEG = PIM%STA(PIM%OBS(J13)%STA_IND(J15))%MOD(J16)%TIM_BEG
               TIM_MOD_END = PIM%STA(PIM%OBS(J13)%STA_IND(J15))%MOD(J16)%TIM_END
!
! ------------ Check whether the model interval fits into the observation
! ------------ range.
! ------------ We consider four cases:
! ------------ a) Observation start is within the model range
! ------------ b) Observation end   is within the model range
! ------------ c) the model range is within the observation range
! ------------ d) the observation range is within the model range
!
               IF ( ( TIM_MOD_BEG < TIM_OBS_BEG + PIM%CONF%AP_TOLERANCE .AND. &
     &                TIM_MOD_END > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE       &
     &               ) .OR. &
     &             ( TIM_MOD_BEG < TIM_OBS_END + PIM%CONF%AP_TOLERANCE .AND. &
     &               TIM_MOD_END > TIM_OBS_END - PIM%CONF%AP_TOLERANCE       &
     &               ) .OR. &
     &             ( TIM_MOD_BEG > TIM_OBS_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &               TIM_MOD_END < TIM_OBS_END + PIM%CONF%AP_TOLERANCE       &
     &               ) .OR. &
     &             ( TIM_OBS_BEG > TIM_MOD_BEG - PIM%CONF%AP_TOLERANCE .AND. &
     &               TIM_OBS_END < TIM_MOD_END + PIM%CONF%AP_TOLERANCE       &
     &               ) ) THEN
!
                     IF ( PIM%OBS(J13)%MOD_IND_BEG(J15) == 0 ) THEN
                          PIM%OBS(J13)%MOD_IND_BEG(J15) = J16
                     END IF
!
                     PIM%OBS(J13)%MOD_IND_END(J15) = J16
                     PIM%STA(PIM%OBS(J13)%STA_IND(J15))%MOD(J16)%SCANNAME = &
     &                       PIM%SCA(PIM%OBS(J13)%SCA_IND)%SCAN_NAME
               END IF
 4160       CONTINUE
            IF ( PIM%OBS(J13)%MOD_IND_END(J15) < PIM%OBS(J13)%MOD_IND_BEG(J15) )    THEN
                 write ( 6, * ) 'CRASH: j15= ', j15, ' ind_obs= ', j13, &
     &                          ' sta = ', pim%c_sta(pim%obs(j13)%sta_ind(j15))
                 write ( 6, * ) 'ind_beg= ', PIM%OBS(J13)%MOD_IND_BEG(J15), &
     &                          ' ind_end= ', PIM%OBS(J13)%MOD_IND_END(J15)
                 write ( 6, * ) 'tim_obs= ', TIM_OBS_BEG, TIM_OBS_END
                 write ( 6, * ) 'tim_mod= ', TIM_MOD_BEG, TIM_MOD_END
                 CALL CLRCH ( STR )
                 CALL INCH  ( J15, STR )
                 CALL ERR_LOG ( 6428, IUER, 'PIMA_LOAD_SFXC_MOD', 'Trap of internal '// &
     &               'control: wrong model index for observation '//STR )
                 RETURN 
            END IF
 4150    CONTINUE
!
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              IF ( PIM%OBS(J13)%MOD_IND_BEG(1) > 0 .AND. &
     &             PIM%OBS(J13)%MOD_IND_END(1) > 0 .AND. &
     &             PIM%OBS(J13)%MOD_IND_BEG(2) > 0 .AND. &
     &             PIM%OBS(J13)%MOD_IND_END(2) > 0       ) THEN
!
                   WRITE ( UNIT=LUN_MDU, FMT=130 ) J13, &
     &                     PIM%OBS(J13)%MOD_IND_BEG, &
     &                     PIM%OBS(J13)%MOD_IND_END, &
     &                     PIM%C_SOU(PIM%OBS(J13)%SOU_IND), &
     &                     PIM%C_STA(PIM%OBS(J13)%STA_IND(1)), &
     &                     PIM%C_STA(PIM%OBS(J13)%STA_IND(2)), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%STA(PIM%OBS(J13)%STA_IND(1))%MOD(PIM%OBS(J13)%MOD_IND_BEG(1))%TIM_BEG, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%STA(PIM%OBS(J13)%STA_IND(1))%MOD(PIM%OBS(J13)%MOD_IND_END(1))%TIM_END, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%STA(PIM%OBS(J13)%STA_IND(2))%MOD(PIM%OBS(J13)%MOD_IND_BEG(2))%TIM_BEG, -2 ), &
     &                     MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                            PIM%STA(PIM%OBS(J13)%STA_IND(2))%MOD(PIM%OBS(J13)%MOD_IND_END(2))%TIM_END, -2 )
 130               FORMAT ( 'Obs: ',I6,' MOD_BEG: ', I8, 1X, I8, &
     &                      ' MOD_END: ', I8, 1X, I8, &
     &                      ' C_SOU: ', A, &
     &                      ' C_STA: ', A8, 1X, A8, &
     &                      ' TIM_1: ', A30, 1X, A30, &
     &                      ' TIM_2: ', A21, 1X, A21 )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( IER, STR )
                        CALL ERR_LOG ( 6429, IUER, 'PIMA_LOAD_SFXC_MOD', 'Failure '// &
     &                      'in writing into file '// &
     &                       MDU_FILE(1:I_LEN(MDU_FILE))// &
     &                      ' -- error '//STR )
                        RETURN
                   END IF
                 ELSE
                   STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + &
     &                                    PIM%TIM_R8(PIM%OBS(J13)%TIM_BEG_IND), &
     &                                    -2 )
                   WRITE ( UNIT=LUN_MDU, FMT=135 ) J13, &
     &                     PIM%OBS(J13)%MOD_IND_BEG, &
     &                     PIM%OBS(J13)%MOD_IND_END, &
     &                     PIM%C_SOU(PIM%OBS(J13)%SOU_IND), &
     &                     PIM%C_STA(PIM%OBS(J13)%STA_IND(1)), &
     &                     PIM%C_STA(PIM%OBS(J13)%STA_IND(2)), &
     &                     STR(1:22)
 135               FORMAT ( 'NO MODEL for obs ', I6, &
     &                      ' MOD_BEG: ', I4, 1X, I4, &
     &                      ' MOD_END: ', I4, 1X, I4, &
     &                      ' C_SOU: ', A, &
     &                      ' C_STA: ', A8, 1X, A8, &
     &                      ' TIM: ', A22 )
              END IF
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
              STR  = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(PIM%OBS(J13)%TIM_BEG_IND), IER )
              STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%TIM_R8(PIM%OBS(J13)%TIM_END_IND), IER )
!
              WRITE ( UNIT=LUN_MOB, FMT=140, IOSTAT=IER ) J13, &
     &                     PIM%C_STA(PIM%OBS(J13)%STA_IND(1)), &
     &                     PIM%C_STA(PIM%OBS(J13)%STA_IND(2)), &
     &                     PIM%C_SOU(PIM%OBS(J13)%SOU_IND), &
     &                     PIM%SCA(PIM%OBS(J13)%SCA_IND)%SCAN_NAME, &
     &                     STR(1:21), STR1(1:21), &
     &                     PIM%OBS(J13)%MOD_IND_BEG(1), PIM%OBS(J13)%MOD_IND_END(1), &
     &                     PIM%OBS(J13)%MOD_IND_BEG(2), PIM%OBS(J13)%MOD_IND_END(2)
 140          FORMAT ( 'MOB Obs: ', I6, ' Sta: ', A, 1X, A, ' Sou: ', A, ' Sca: ', A, &
     &                 ' Tai_beg: ', A, ' Tai_end: ', A, &
     &                 ' Mod_1_inds ', I7, 1X, I7, &
     &                 ' Mod_2_inds ', I7, 1X, I7 )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( IER, STR )
                   CALL ERR_LOG ( 6430, IUER, 'PIMA_LOAD_SFXC', 'Failure '// &
     &                 'in writing into file '// &
     &                  MOB_FILE(1:I_LEN(MOB_FILE))//' -- error '//STR )
                   RETURN
              END IF
         END IF
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              WRITE ( 6, * ) 'SFXC_MOD: Obs: ', J13, ' Mod_inds= ', PIM%OBS(J13)%MOD_IND_BEG, PIM%OBS(J13)%MOD_IND_END
         END IF
 4130 CONTINUE
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           DO 4170 J17=1,PIM%NSTA
              DO 4180 J18=1,PIM%STA(J17)%L_MOD
                 STR  = MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                                   PIM%TAI_0 + PIM%STA(J17)%MOD(J18)%TIM_BEG, IER )
                 STR1 = MJDSEC_TO_DATE ( PIM%MJD_0, &
     &                                   PIM%TAI_0 + PIM%STA(J17)%MOD(J18)%TIM_END, IER )
                 IF ( PIM%STA(J17)%MOD(J18)%SOU_IND .GE. 1 .AND. &
     &                PIM%STA(J17)%MOD(J18)%SOU_IND .LE. PIM%NSOU  ) THEN
!
                      WRITE ( UNIT=LUN_MOD, FMT=150, IOSTAT=IER ) &
     &                   PIM%C_STA(J17), &
     &                   PIM%C_SOU(PIM%STA(J17)%MOD(J18)%SOU_IND), &
     &                   PIM%STA(J17)%MOD(J18)%TIM_BEG, &
     &                   PIM%STA(J17)%MOD(J18)%TIM_END, &
     &                   J18, STR(1:21), STR1(1:21), &
     &                   PIM%STA(J17)%MOD(J18)%GDEL_POL(0:PIM__MDPL-1,1)
 150                  FORMAT ( 'MOD: Sta ',A, ' Sou: ', A, &
     &                         ' Tim_beg: ', F20.10, ' Tim_end: ', F20.10, ' | ', &
     &                        ' Ind_mod: ', I8, 2X, A, 2X, A, ' | Gdelay: ', 6(1PD20.12,1X) )
                    ELSE
!
! ------------------- Wrong source index
!
                      WRITE ( UNIT=LUN_MOD, FMT=155, IOSTAT=IER ) &
     &                        PIM%C_STA(J17), &
     &                        PIM%STA(J17)%MOD(J18)%SOU_IND, &
     &                        PIM%STA(J17)%MOD(J18)%TIM_BEG, &
     &                        PIM%STA(J17)%MOD(J18)%TIM_END, &
     &                        J18, STR(1:21), STR1(1:21)
 155                  FORMAT ( 'MOD: Sta ',A, ' SOU_IND: ', I12, &
     &                    ' Tim_beg: ', F20.10, ' Tim_end: ', F20.10, ' | ', &
     &                    ' Ind_mod: ', I8, 2X, A, 2X, A )
                 END IF
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( IER, STR )
                      CALL ERR_LOG ( 6431, IUER, 'PIMA_LOAD_SFXC', 'Failure '// &
     &                    'in writing into file '// &
     &                     MOD_FILE(1:I_LEN(MOD_FILE))//' -- error '//STR )
                      RETURN
                 END IF
 4180         CONTINUE 
 4170      CONTINUE 
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           CLOSE ( UNIT=LUN_MDU )
           CLOSE ( UNIT=LUN_MOB )
           CLOSE ( UNIT=LUN_MOD )
      END IF
      IF ( ILEN(FILCLK) > 0 ) THEN
           IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                WRITE ( 6, 230 ) TRIM(FILCLK)
 230            FORMAT ( ' SFXC_MOD: Clock file: ', A )
!
                IP = I_LEN(PIM%CONF%EXPER_DIR)
                MDC_FILE = PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//'.mdc'
                IF ( PIM%CONF%EXPER_DIR(IP:IP) .EQ. '/' ) THEN
                     MDC_FILE = PIM%CONF%EXPER_DIR(1:IP)//MDC_FILE
                   ELSE
                     MDC_FILE = PIM%CONF%EXPER_DIR(1:IP)//'/'//MDC_FILE
                END IF
                LUN_MDC = GET_UNIT()
                OPEN ( UNIT=LUN_MDC, FILE=MDC_FILE, STATUS='UNKNOWN', IOSTAT=IER )
                IF ( IER .NE. 0 ) THEN
                     CALL CLRCH ( STR )
                     CALL INCH  ( IER, STR )
                     CALL ERR_LOG ( 6432, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &                   'Failure to open information file '// &
     &                    MDC_FILE(1:I_LEN(MDC_FILE))//' -- error '//STR )
                     RETURN
                END IF
                WRITE ( UNIT=LUN_MDC, FMT='(A)' ) '# SFXC style of clock model '// &
     &                                            'loaded from file '//TRIM(FILCLK)
           END IF
!
! -------- Read clock file
!
           CALL ERR_PASS ( IUER, IER )
           CALL RD_TEXT  ( FILCLK, M_BUF, BUF, N_CLK, IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL INCH  ( IER, STR )
                CALL ERR_LOG ( 6433, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &              'Failure to readin clock file '//FILCLK )
                RETURN
           END IF
!
           DO 4190 J19=1,N_CLK
              IF ( BUF(J19)(1:1)  == '#' ) GOTO 4190
              IF ( ILEN(BUF(J19)) ==  0  ) GOTO 4190
!
              CALL EXWORD ( BUF(J19), MIND, LIND, IND, CHAR(0)//CHAR(9)//CHAR(32)//':;', IER )
              IF ( LIND .NE. 6 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J19, STR )
                   CALL ERR_LOG ( 6434, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &                 'Error in parsing the '//TRIM(STR)//' -th line of the '// &
     &                 'clock file '//TRIM(FILCLK)//' the number of parsable '// &
     &                 'words is less than 6' )
                   RETURN
              END IF
              IF ( BUF(J19)(IND(1,3):IND(2,3)) == 'usec'     .AND. &
     &             BUF(J19)(IND(1,6):IND(2,6)) == 'usec/sec'       ) THEN
                   CONTINUE 
                 ELSE
                   CALL CLRCH ( STR )
                   CALL INCH  ( J19, STR )
                   CALL ERR_LOG ( 6435, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &                 'Error in parsing the '//TRIM(STR)//'-th line of the '// &
     &                 'clock file '//TRIM(FILCLK)//' word 3 is not usec '// &
     &                 'and/or word is not 6' )
                   RETURN
              END IF
              ISTA = 0
              CALL TRAN ( 11, BUF(J19)(IND(1,1):IND(2,1)), BUF(J19)(IND(1,1):IND(2,1)) )
              IP = IND(2,1) - IND(1,1) + 1
              DO 4200 J20=1,PIM%NSTA
                 IF ( PIM%STA(J20)%ORIG_NAME(1:IP) == BUF(J19)(IND(1,1):IND(2,1)) ) THEN
                      ISTA = J20
                 END IF
 4200         CONTINUE 
!
! ----------- Check whether this station observed
!
              IF ( ISTA == 0 ) GOTO 4190
!
              READ ( UNIT=BUF(J19)(IND(1,2):IND(2,2)), FMT='(F14.6)', IOSTAT=IER ) CLK_VAL
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J19, STR )
                   CALL ERR_LOG ( 6436, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &                 'Error in parsing the '//TRIM(STR)//' -th line of '// &
     &                 'the clock file '//TRIM(FILCLK)//' -- wrong format of clock offset: '// &
     &                  BUF(J19)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
!
              CLK_VAL = CLK_VAL*1.D-6
              READ ( UNIT=BUF(J19)(IND(1,5):IND(2,5)), FMT='(F14.6)', IOSTAT=IER ) CLK_RATE
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J19, STR )
                   CALL ERR_LOG ( 6437, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &                 'Error in parsing the '//TRIM(STR)//' -th line of '// &
     &                 'the clock file '//TRIM(FILCLK)//' -- wrong format of clock rate: '// &
     &                  BUF(J19)(IND(1,2):IND(2,2)) )
                   RETURN
              END IF
              CLK_RATE = CLK_RATE*1.D-6
              CALL ERR_PASS ( IUER, IER ) 
              DATE_STR = VEX_TO_DATE ( BUF(J19)(IND(1,4):IND(2,4)), IER )
              IF ( LIND .NE. 6 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J19, STR )
                   CALL ERR_LOG ( 6438, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &                 'Error in parsing the '//TRIM(STR)//' -th line of '// &
     &                 'the clock file '//TRIM(FILCLK)//' -- wrong date: '// &
     &                  BUF(J19)(IND(1,4):IND(2,4)) )
                   RETURN
              END IF
!
              CALL ERR_PASS ( IUER, IER ) 
              CALL DATE_TO_TIME ( DATE_STR, MJD_CLK_EPC, UTC_CLK_EPC, IER )
              IF ( IER .NE.  0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J19, STR )
                   CALL ERR_LOG ( 6439, IUER, 'PIMA_LOAD_SFXC_MOD', &
     &                 'Error in parsing the '//TRIM(STR)//' -th line of '// &
     &                 'the clock file '//TRIM(FILCLK)//' -- wrong date: '// &
     &                  BUF(J19)(IND(1,4):IND(2,4)) )
                   RETURN
              END IF
              TAI_CLK_EPC = UTC_CLK_EPC - PIM%UTC_MTAI
!
              PIM%STA(ISTA)%MDC%CLO_OFFS     = CLK_VAL
              PIM%STA(ISTA)%MDC%CLO_RATE     = CLK_RATE
              PIM%STA(ISTA)%MDC%CLO_OFFS_ERR = 0.0
              PIM%STA(ISTA)%MDC%CLO_RATE_ERR = 0.0
              PIM%STA(ISTA)%MDC%MJD_REF      = MJD_CLK_EPC
              PIM%STA(ISTA)%MDC%TAI_REF      = TAI_CLK_EPC
              PIM%STA(ISTA)%MDC%CLO_MODEL_STATUS = PIMA__MDC_GLO_EXCLUDED
!
              IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
                   DATE_STR = MJDSEC_TO_DATE ( PIM%STA(ISTA)%MDC%MJD_REF, &
     &                                         PIM%STA(ISTA)%MDC%TAI_REF, IER )
                   WRITE ( LUN_MDC, 240 ) PIM%C_STA(ISTA), DATE_STR, &
     &                                    PIM%STA(ISTA)%MDC%CLO_OFFS, &
     &                                    PIM%STA(ISTA)%MDC%CLO_RATE
 240               FORMAT ( 'CLOCK_MODEL Sta: ', A, ' Ref_date: ', A, &
     &                      ' Clock_off: ',  1PD13.6, &
     &                      ' Clock_rate: ', 1PD13.6 )
              END IF
 4190      CONTINUE 
         ELSE IF ( PIM%CONF%DEBUG_LEVEL .GE. 1 ) THEN
           WRITE ( 6, '(A)' ) 'WARNING: no clock file was found in '// &
     &                         TRIM(PIM%CONF%INTMOD_FILE(1))
      END IF
!
      IF ( PIM%CONF%DEBUG_LEVEL .GE. 2 ) THEN
           CLOSE ( UNIT=LUN_MDC )
      END IF
!
      DEALLOCATE ( MOD )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_LOAD_SFXC_MOD  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PIMA_PARSE_SFXC_MOD ( FINAM, FIL_IND, M_MOD, L_MOD, MOD, &
     &                                 UTC_MTAI, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_PARSE_SFXC_MOD 
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ## 15-SEP-2015 PIMA_PARSE_SFXC_MOD v2.1 (d) L. Petrov 28-DEC-2025 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'pima.i'
      CHARACTER  FINAM*(*)
      REAL*8     UTC_MTAI
      INTEGER*4  FIL_IND, M_MOD, L_FIL, L_MOD, IUER
      TYPE     ( PIMA__SFXC_TYPE ) :: MOD(M_MOD)
      CHARACTER  STR*128, STA_NAM*8, SOU_NAM*8, SCA_NAM*16
      INTEGER*1, ALLOCATABLE :: ARR(:)
      INTEGER*8  SIZE_I8
      REAL*8     R8_ARR(7)
      REAL*4     R4_ARR(7)
      TYPE ( SFXC__IM_FILE_HEADER_TYPE ) :: IM_HDR
      TYPE ( SFXC__IM_OBS_HEADER_TYPE  ) :: OBS_HDR
      TYPE ( SFXC__IM_OBS_RECORD_TYPE  ) :: OBS_REC
!
      INTEGER*4  HDR_LEN
      INTEGER*4  IS, LUN, IB, IE, J1, J2, J3, UNIX_DATE, I4_VAR, &
     &           MJD, I4_VAL, IP, IER
      LOGICAL*1  FL_DEBUG
      INTEGER*2  I2_VAL
      INTEGER*8  OFFSET
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, READ, FILE_INFO
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      FL_DEBUG = .FALSE.
!
      IS = FILE_INFO ( FINAM(1:I_LEN(FINAM))//CHAR(0), UNIX_DATE, SIZE_I8 )
      IF ( IS .NE. 0 ) THEN
           CALL GERROR ( STR )
           CALL ERR_LOG ( 6438, IUER, 'PIMA_PARSE_SFXC_MOD', 'Error '// &
     &         'in collecting information about input file '//FINAM )
           RETURN 
      END IF
!
      ALLOCATE ( ARR(SIZE_I8) )
!
      CALL ERR_PASS  ( IUER, IER )
      CALL BINF_OPEN ( FINAM, 'OLD', LUN, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6439, IUER, 'PIMA_PARSE_SFXC_MOD', 'Cannot '// &
     &         'open input file '//FINAM )
           DEALLOCATE ( ARR )
           RETURN 
      END IF
!
      IS = READ ( %VAL(LUN), ARR, %VAL(SIZE_I8) )
      IF ( IS .LT. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 6440, IUER, 'PIMA_PARSE_SFXC_MOD', 'Error '// &
     &         'during reading the the contents of the SFXC model '// &
     &         'file '//FINAM )
           DEALLOCATE ( ARR )
           RETURN
      END IF
!
      CALL BINF_CLOSE ( LUN, IER )
!
      CALL MEMCPY ( HDR_LEN,  ARR(1), %VAL(4) )
      CALL CLRCH  ( STA_NAM )
      IF ( FL_DEBUG ) THEN
           WRITE ( 6, * ) 'PIMA_LOAD_SFXC_MOD-922  HDR_LEN= ', HDR_LEN
      END IF
      IF ( HDR_LEN == 3 ) THEN
!
! -------- Pre-2018 style SFXC delay file
!
           CALL MEMCPY ( STA_NAM(1:HDR_LEN-1), ARR(5) )
           CALL MEMCPY ( STR(1:80), ARR(5+HDR_LEN) )
           SOU_NAM = STR(1:8)
           CALL MEMCPY ( MJD, ARR(86+HDR_LEN), %VAL(4) )
           OFFSET = 90 + HDR_LEN
        ELSE IF ( HDR_LEN == 7 ) THEN
!
! -------- 2019 style SFXC delay file
!
           OFFSET = 2  + HDR_LEN
           CALL MEMCPY ( IM_HDR%STA_NAM,  ARR(OFFSET), %VAL(2)  )
           CALL TRAN ( 11, IM_HDR%STA_NAM, IM_HDR%STA_NAM )
           OFFSET = OFFSET + 3
           CALL MEMCPY ( OBS_HDR%SCA_NAM, ARR(OFFSET)    )
           IP = INDEX  ( OBS_HDR%SCA_NAM, CHAR(0) )
           IF ( IP > 1 ) CALL CLRCH ( OBS_HDR%SCA_NAM(IP:) )
           CALL MEMCPY ( OBS_HDR%SOU_NAM,  ARR(OFFSET+81) )
           CALL MEMCPY ( OBS_HDR%MJD,      ARR(OFFSET+162), %VAL(4) )
           OFFSET = OFFSET + SFXC__IM_OBS_HEADER_LEN
         ELSE IF ( HDR_LEN == 11 ) THEN
!
! -------- Post-2020 style SFXC delay file
!
           OFFSET = 1
!
! -------- Populate IM_HDR
!
           CALL MEMCPY ( IM_HDR%MAGIC_I4, ARR(OFFSET),    %VAL(12)  )
           CALL MEMCPY ( IM_HDR%STA_NAM,  ARR(OFFSET+12), %VAL(2)  )
           CALL TRAN ( 11, IM_HDR%STA_NAM, IM_HDR%STA_NAM )
           OFFSET = OFFSET + SFXC__IM_FILE_HEADER_LEN 
!
! -------- Populate OBS_HDR
!
           CALL MEMCPY ( OBS_HDR%SCA_NAM,  ARR(OFFSET)    )
           IP = INDEX ( OBS_HDR%SCA_NAM, CHAR(0) )
           IF ( IP > 1 ) CALL CLRCH ( OBS_HDR%SCA_NAM(IP:) )
           CALL MEMCPY ( OBS_HDR%SOU_NAM,  ARR(OFFSET+81) )
           CALL MEMCPY ( OBS_HDR%MJD,      ARR(OFFSET+162), %VAL(4) )
           OFFSET = OFFSET + SFXC__IM_OBS_HEADER_LEN
           IF ( FL_DEBUG ) THEN
                WRITE ( 6, * ) 'PIMA_LOAD_SFXC-956  '//trim(finam), ' siz= ', size_i8 ! %%%
                WRITE ( 6, * ) 'MAGIC= ',   IM_HDR%MAGIC_I4
                WRITE ( 6, * ) 'STA_NAM= ', IM_HDR%STA_NAM
                WRITE ( 6, * ) 'SOU_NAM= ', TRIM(OBS_HDR%SOU_NAM)
                WRITE ( 6, * ) 'SCA_NAM= ', TRIM(OBS_HDR%SCA_NAM)
                WRITE ( 6, * ) 'MJD=     ', OBS_HDR%MJD
                WRITE ( 6, * ) 'siz=     ', SIZEOF(IM_HDR), SIZEOF(OBS_HDR)
                WRITE ( 6, * ) 'offset=  ', OFFSET
                call flush ( 6 )
           END IF
      END IF
      DO 410 J1=1,SIZE_I8
         IF ( HDR_LEN == 11 )  THEN
!
! ----------- Post 2020 format of SFXC delay data
! ----------- Copy the data record
!
              CALL MEMCPY ( OBS_REC, ARR(OFFSET), %VAL(SIZEOF(OBS_REC)) )
              IF ( OBS_REC%TIM_STEP == 0.0 ) THEN
!
! ---------------- Ooops! The data record iz zero. Thus, this is a delimeter.
! ---------------- Skip the delimiter.
!
                   OFFSET = OFFSET + SFXC__IM_OBS_RECORD_LEN
                   IF ( OFFSET - 1 == SIZE_I8 ) THEN
!
! --------------------- We reach the end of the file. Good bye
!
                        DEALLOCATE ( ARR )
                        CALL ERR_LOG ( 0, IUER )
                        RETURN 
                   END IF
                   IF ( FL_DEBUG ) THEN
                        WRITE ( 6, * ) 'OFFSET= ', OFFSET, ' SIZE= ', SIZE_I8 
                        CALL FLUSH ( 6 )
                   END IF
!
! ---------------- Now we stand on the header record.
! ---------------- Let us OBS_HDR by parsing the header record
!
                   CALL MEMCPY ( OBS_HDR%SCA_NAM,  ARR(OFFSET)              )
                   CALL MEMCPY ( OBS_HDR%SOU_NAM,  ARR(OFFSET+81)           )
                   CALL MEMCPY ( OBS_HDR%MJD,      ARR(OFFSET+162), %VAL(4) )
                   IP = INDEX ( OBS_HDR%SCA_NAM, CHAR(0) )
                   IF ( IP > 1 ) CALL CLRCH ( OBS_HDR%SCA_NAM )
                   OFFSET = OFFSET + SFXC__IM_OBS_HEADER_LEN
              END IF
!
              L_MOD = L_MOD + 1
              IF ( L_MOD > M_MOD ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_MOD, STR )
                   CALL ERR_LOG ( 6442, IUER, 'PIMA_PARSE_SFXC_MOD', &
     &                 'Trap of internal control while parsing '// &
     &                 'the contents of the SFXC model '// &
     &                 'file '//FINAM(1:I_LEN(FINAM))//' -- '// &
     &                 ' parameter M_MOD '//STR(1:I_LEN(STR))//' is '// &
     &                 ' too small' )
                   DEALLOCATE ( ARR )
                   RETURN
              END IF
!
              MOD(L_MOD)%STA_NAM = IM_HDR%STA_NAM
              MOD(L_MOD)%SOU_NAM = OBS_HDR%SOU_NAM
              MOD(L_MOD)%MJD     = OBS_HDR%MJD
              MOD(L_MOD)%TAI     = OBS_REC%UTC - UTC_MTAI
              MOD(L_MOD)%DEL     = OBS_REC%DEL
              CALL TRAN ( 11, MOD(L_MOD)%STA_NAM, MOD(L_MOD)%STA_NAM )
              CALL TRAN ( 11, MOD(L_MOD)%SOU_NAM, MOD(L_MOD)%SOU_NAM )
              MOD(L_MOD)%FIL_DEL_IND = FIL_IND
              IF ( FL_DEBUG ) THEN
                   STR = MJDSEC_TO_DATE ( MOD(L_MOD)%MJD, MOD(L_MOD)%TAI, IER )
                   WRITE ( 6, * ) 'Ind: ', L_MOD, &
     &                            ' Sta ', MOD(L_MOD)%STA_NAM, &
     &                            ' Sou ', MOD(L_MOD)%SOU_NAM, &
     &                            ' Tim =', MOD(L_MOD)%MJD, SNGL(MOD(L_MOD)%TAI), &
     &                            ' Del = ', OBS_REC%DEL, &
     &                            ' Date = ', STR(1:21)//' TAI'
              END IF
!
              OFFSET = OFFSET + SFXC__IM_OBS_RECORD_LEN
           ELSE IF ( HDR_LEN == 7 )  THEN
!
! ----------- 2019 format of SFXC delay data
! ----------- Copy the data record
!
              CALL MEMCPY ( OBS_REC, ARR(OFFSET), %VAL(SIZEOF(OBS_REC)) )
              IF ( OBS_REC%TIM_STEP == 0.0 ) THEN
!
! ---------------- Ooops! The data record iz zero. Thus, this is a delimeter.
! ---------------- Skip the delimiter.
!
                   OFFSET = OFFSET + SFXC__IM_OBS_RECORD_LEN
                   IF ( OFFSET - 1 == SIZE_I8 ) THEN
!
! --------------------- We reach the end of the file. Good bye
!
                        DEALLOCATE ( ARR )
                        CALL ERR_LOG ( 0, IUER )
                        RETURN 
                   END IF
                   IF ( FL_DEBUG ) THEN
                        WRITE ( 6, * ) 'OFFSET= ', OFFSET, ' SIZE= ', SIZE_I8 
                        CALL FLUSH ( 6 )
                   END IF
!
! ---------------- Now we stand on the header record.
! ---------------- Let us OBS_HDR by parsing the header record
!
                   CALL MEMCPY ( OBS_HDR%SCA_NAM,  ARR(OFFSET)              )
                   CALL MEMCPY ( OBS_HDR%SOU_NAM,  ARR(OFFSET+81)           )
                   CALL MEMCPY ( OBS_HDR%MJD,      ARR(OFFSET+162), %VAL(4) )
                   IP = INDEX ( OBS_HDR%SCA_NAM, CHAR(0) )
                   IF ( IP > 1 ) CALL CLRCH ( OBS_HDR%SCA_NAM )
                   OFFSET = OFFSET + SFXC__IM_OBS_HEADER_LEN
              END IF
!
              L_MOD = L_MOD + 1
              IF ( L_MOD > M_MOD ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_MOD, STR )
                   CALL ERR_LOG ( 6442, IUER, 'PIMA_PARSE_SFXC_MOD', &
     &                 'Trap of internal control while parsing '// &
     &                 'the contents of the SFXC model '// &
     &                 'file '//FINAM(1:I_LEN(FINAM))//' -- '// &
     &                 ' parameter M_MOD '//STR(1:I_LEN(STR))//' is '// &
     &                 ' too small' )
                   DEALLOCATE ( ARR )
                   RETURN
              END IF
!
              MOD(L_MOD)%STA_NAM = IM_HDR%STA_NAM
              MOD(L_MOD)%SOU_NAM = OBS_HDR%SOU_NAM
              MOD(L_MOD)%MJD     = OBS_HDR%MJD
              MOD(L_MOD)%TAI     = OBS_REC%UTC - UTC_MTAI
              MOD(L_MOD)%DEL     = OBS_REC%DEL
              CALL TRAN ( 11, MOD(L_MOD)%STA_NAM, MOD(L_MOD)%STA_NAM )
              CALL TRAN ( 11, MOD(L_MOD)%SOU_NAM, MOD(L_MOD)%SOU_NAM )
              MOD(L_MOD)%FIL_DEL_IND = FIL_IND
              IF ( FL_DEBUG ) THEN
                   STR = MJDSEC_TO_DATE ( MOD(L_MOD)%MJD, MOD(L_MOD)%TAI, IER )
                   WRITE ( 6, * ) 'Ind: ', L_MOD, &
     &                            ' Sta ', MOD(L_MOD)%STA_NAM, &
     &                            ' Sou ', MOD(L_MOD)%SOU_NAM, &
     &                            ' Tim =', MOD(L_MOD)%MJD, SNGL(MOD(L_MOD)%TAI), &
     &                            ' Del = ', OBS_REC%DEL, &
     &                            ' Date = ', STR(1:21)//' TAI'
              END IF
!
              OFFSET = OFFSET + SFXC__IM_OBS_RECORD_LEN
           ELSE IF ( HDR_LEN == 3 )  THEN
!
! ----------- Pre-2020 SFFX delay format
!
              CALL MEMCPY ( R8_ARR, ARR(OFFSET), %VAL(7*8) )
              IF ( R8_ARR(1) == 0.0D0 .AND. R8_ARR(5) == 0.0D0 ) THEN
                   IF ( OFFSET + 7*8  - 1 == SIZE_I8 ) THEN
                        DEALLOCATE ( ARR )
                        CALL ERR_LOG ( 0, IUER )
                        RETURN 
                   END IF
                   OFFSET = OFFSET + 7*8
                   IF ( OFFSET + 84 > M_MOD ) THEN
                        CALL ERR_LOG ( 6444, IUER, 'PIMA_PARSE_SFXC_MOD', 'Error '// &
          &                 'during parsing the contents of the SFXC model '// &
          &                 'file '//FINAM(1:I_LEN(FINAM))//': an attempt to '// &
          &                 'read beyond the file' )
                        DEALLOCATE ( ARR )
                        RETURN 
                   END IF
!
                   CALL MEMCPY ( STR(1:80), ARR(OFFSET) )
                   SOU_NAM = STR(1:8)
                   OFFSET = 81 + OFFSET
                   CALL MEMCPY ( MJD, ARR(OFFSET), %VAL(4) )
                   OFFSET = 4 + OFFSET
                 ELSE
                   L_MOD = L_MOD + 1
                   IF ( L_MOD > M_MOD ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( M_MOD, STR )
                        CALL ERR_LOG ( 6445, IUER, 'PIMA_PARSE_SFXC_MOD', &
     &                      'Trap of internal control while parsing '// &
     &                      'the contents of the SFXC model '// &
     &                      'file '//FINAM(1:I_LEN(FINAM))//' -- '// &
     &                      ' parameter M_MOD '//STR(1:I_LEN(STR))//' is '// &
     &                      ' too small' )
                        DEALLOCATE ( ARR )
                        RETURN
                   END IF
!
                   MOD(L_MOD)%STA_NAM = STA_NAM
                   MOD(L_MOD)%SOU_NAM = SOU_NAM
                   MOD(L_MOD)%MJD     = MJD
                   MOD(L_MOD)%TAI     = R8_ARR(1) - UTC_MTAI
                   MOD(L_MOD)%DEL     = R8_ARR(5)
                   CALL TRAN ( 11, MOD(L_MOD)%STA_NAM, MOD(L_MOD)%STA_NAM )
                   CALL TRAN ( 11, MOD(L_MOD)%SOU_NAM, MOD(L_MOD)%SOU_NAM )
                   MOD(L_MOD)%FIL_DEL_IND = FIL_IND
!
                   OFFSET = OFFSET + 7*8
             END IF
         END IF
         IF ( OFFSET > M_MOD ) THEN
              CALL ERR_LOG ( 6446, IUER, 'PIMA_PARSE_SFXC_MOD', 'Error '// &
     &            'during parsing the contents of the SFXC model '// &
     &            'file '//FINAM(1:I_LEN(FINAM))//': an attempt to '// &
     &            'read beyond the file' )
              DEALLOCATE ( ARR )
              RETURN 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 6447, IUER, 'PIMA_PARSE_SFXC_MOD', 'Error '// &
     &    'during parsing the contents of the SFXC model '// &
     &    'file '//FINAM(1:I_LEN(FINAM))//' the trailing record '// &
     &    'was not found' )
      DEALLOCATE ( ARR )
!
      RETURN
      END  SUBROUTINE  PIMA_PARSE_SFXC_MOD   !#!#
