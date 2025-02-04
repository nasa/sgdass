      SUBROUTINE AAM_FCS_SPL ( DIR_AAM, MJD_BEG, TAI_BEG, MJD_END, TAI_END, &
     &                         MJD_ASM_END, TAI_ASM_END, M_FIL, L_EPC, &
     &                         TIM_ARR, AAM_VAL, AAM_SPL, DATA_SOURCE, &
     &                         DATA_TITLE, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  AAM_FCS_SPL 
! *                                                                      *
! *  ### 09-AUG-2015  AAM_FCS_SPL  v3.2 (c)  L. Petrov  18-MAY-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
!
      CHARACTER  DIR_AAM*(*), DATA_SOURCE*(*), DATA_TITLE*(*) 
      INTEGER*4  MJD_BEG, MJD_END, MJD_ASM_END, M_FIL, L_EPC, IUER
      REAL*8     TAI_BEG, TAI_END, TAI_ASM_END, TIM_ARR(M_FIL), &
     &           AAM_VAL(M_FIL,15), AAM_SPL(M_FIL,15), FCS_INTRV
      CHARACTER  FIL_AAM(MALO__FIL)*128, FILNAM*128, STR*128, EXT*4, &
     &           DATE_AAM*19, DATE_FCS*19, DATE_FCS_LAST*19
      INTEGER*8  DIR_DESC(16)
      INTEGER*4  LEV, L_FIL, IS, IL, J1, J2, J3, J4, IND_EPC, LAST_IND_EPC, &
     &           IDAY, MJD_AAM, MJD_FCS, MJD_MOM, IVRB, IER
      REAL*8     FCS_TIM(MALO__FIL), TMP_ARR(MALO__FIL), TAI_AAM, TAI_FCS, &
     &           FCS_AGE, TAI_MOM, IMOM_NOIB(3), IMOM_IB(3), HMOM(3), &
     &           EXF_NOIB(3), EXF_IB(3)
      REAL*8       MALO__FCS_STEP 
      PARAMETER  ( MALO__FCS_STEP = 3*3600.0D0 ) 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF, GET_UNIT, LINDEX
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR, GET_FILE_FROM_DIR 
      REAL*8,    EXTERNAL :: FSPL8 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
!
      IVRB = 0
      EXT = '.txt'
      LEV = 0
      L_FIL = 0
      FCS_TIM = 1.0D30
      DO 410 J1=1,16*MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_AAM, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 5121, IUER, 'AAM_FCS_SPL', 'Error in '// &
     &            'reading input directory '// &
     &             DIR_AAM(1:I_LEN(DIR_AAM))//'  '//FILNAM )
              RETURN 
         END IF
!
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '+' ) .LT. 1 ) GOTO 410
         IF ( INDEX ( FILNAM, EXT//'~' ) .GT. 0 ) GOTO 410
         IL = ILEN(FILNAM)
         IF ( IL < 29 ) GOTO 410
!
         L_FIL = L_FIL + 1
         IF ( L_FIL > MALO__FIL )  THEN
              CALL CLRCH ( STR )
              CALL INCH  ( MALO__FIL, STR )
              CALL ERR_LOG ( 5122, IUER, 'AAM_FCS_SPL', 'Too many '// &
     &            'files in directory '//DIR_AAM(1:I_LEN(DIR_AAM))// &
     &             ' -- more than '//STR )
              RETURN 
         END IF
         FIL_AAM(L_FIL) = FILNAM 
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( L_FIL == 0 ) THEN
           CALL ERR_LOG ( 5107, IUER, 'AAM_FCS_SPL', 'No AAM files '// &
     &         'within the specified range was found in the directory '// &
     &          DIR_AAM )
           RETURN 
      END IF
      CALL SORT_FAST_CH ( L_FIL, FIL_AAM )
!
      L_EPC = 0
      DO 420 J2=1,L_FIL
         IL = ILEN(FIL_AAM(J2))
         DATE_FCS = FIL_AAM(J2)(IL-28:IL-25)//'.'// &
     &              FIL_AAM(J2)(IL-24:IL-23)//'.'// &
     &              FIL_AAM(J2)(IL-22:IL-18)//':00:00'
!
         DATE_AAM = FIL_AAM(J2)(IL-16:IL-13)//'.'// &
     &              FIL_AAM(J2)(IL-12:IL-11)//'.'// &
     &              FIL_AAM(J2)(IL-10:IL-6)//':'// &
     &              FIL_AAM(J2)(IL-5:IL-4)//':00'
!
         CALL ERR_PASS ( IUER, IER ) 
         CALL DATE_TO_TIME ( DATE_FCS, MJD_FCS, TAI_FCS, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5123, IUER, 'AAM_FCS_SPL', 'Failure in '// &
     &            'extracting the forecast date from the AAM file '// &
     &             FIL_AAM(J2) )
              RETURN
         END IF
!
         CALL ERR_PASS ( IUER, IER ) 
         CALL DATE_TO_TIME ( DATE_AAM, MJD_AAM, TAI_AAM, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 5124, IUER, 'AAM_FCS_SPL', 'Failure in '// &
     &            'extracting the forecast date from the AAM file '// &
     &             FIL_AAM(J2) )
              RETURN
         END IF
!
         IF ( L_EPC == 0 ) THEN
              L_EPC = 1
              TIM_ARR(L_EPC) = 0.D0
              FCS_TIM(L_EPC) = (MJD_AAM - MJD_FCS)*86400.0D0 + (TAI_AAM - TAI_FCS)
              IF ( L_EPC == 0 ) THEN
                   L_EPC = 1
                   TIM_ARR(L_EPC) = 0
              END IF
              MJD_BEG = MJD_AAM
              TAI_BEG = TAI_AAM
              MJD_ASM_END = MJD_AAM
              TAI_ASM_END = TAI_AAM
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PARSE_AAM ( FIL_AAM(J2), MJD_MOM, TAI_MOM, &
     &                         IMOM_NOIB, IMOM_IB, HMOM, EXF_NOIB, EXF_IB, &
     &                         DATA_SOURCE, DATA_TITLE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5125, IUER, 'AAM_FCS_SPL', 'Failure in '// &
     &                 'parsing the AAM file '//FIL_AAM(J2) )
                   RETURN
              END IF
!              
              AAM_VAL(L_EPC,1)  = IMOM_NOIB(1)
              AAM_VAL(L_EPC,2)  = IMOM_NOIB(2)
              AAM_VAL(L_EPC,3)  = IMOM_NOIB(3)
              AAM_VAL(L_EPC,4)  = IMOM_IB(1)
              AAM_VAL(L_EPC,5)  = IMOM_IB(2)
              AAM_VAL(L_EPC,6)  = IMOM_IB(3)
              AAM_VAL(L_EPC,7)  = HMOM(1)
              AAM_VAL(L_EPC,8)  = HMOM(2)
              AAM_VAL(L_EPC,9)  = HMOM(3)
              AAM_VAL(L_EPC,10) = EXF_NOIB(1)
              AAM_VAL(L_EPC,11) = EXF_NOIB(2)
              AAM_VAL(L_EPC,12) = EXF_NOIB(3)
              AAM_VAL(L_EPC,13) = EXF_IB(1)
              AAM_VAL(L_EPC,14) = EXF_IB(2)
              AAM_VAL(L_EPC,15) = EXF_IB(3)
              LAST_IND_EPC = 1
              GOTO 420
         END IF
!
         IND_EPC = IDNINT ( ((MJD_AAM - MJD_BEG)*86400.0D0 + (TAI_AAM - TAI_BEG))/ &
     &                      MALO__FCS_STEP ) + 1
         FCS_AGE = (MJD_AAM - MJD_FCS)*86400.0D0 + (TAI_AAM - TAI_FCS)
         IF ( DABS(FCS_AGE) < MALO__FCS_STEP ) THEN
              MJD_ASM_END = MJD_AAM
              TAI_ASM_END = TAI_AAM
         END IF
         IF ( IND_EPC > LAST_IND_EPC ) THEN
!
! ----------- New epoch
!
              L_EPC = L_EPC + 1
              LAST_IND_EPC = IND_EPC
              IF ( IVRB > 10 ) THEN
                   WRITE ( 6, * ) 'New epoch: ', INT2(L_EPC), INT2(IND_EPC), &
     &                             MJDSEC_TO_DATE ( MJD_AAM, TAI_AAM, -2 ), ' ', &
     &                             DATE_FCS, ' ', DATE_AAM, ' ', &
     &                             FIL_AAM(J2)(1:I_LEN(FIL_AAM(J2)))
              END IF
              TIM_ARR(L_EPC) = (IND_EPC - 1)*MALO__FCS_STEP
              FCS_TIM(L_EPC) = FCS_AGE
!
              CALL ERR_PASS  ( IUER, IER )
              CALL PARSE_AAM ( FIL_AAM(J2), MJD_MOM, TAI_MOM, &
     &                         IMOM_NOIB, IMOM_IB, HMOM, EXF_NOIB, EXF_IB, &
     &                         DATA_SOURCE, DATA_TITLE, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5126, IUER, 'AAM_FCS_SPL', 'Failure in '// &
     &                 'parsing the AAM file '//FIL_AAM(J2) )
                   RETURN
              END IF
              AAM_VAL(L_EPC,1)  = IMOM_NOIB(1)
              AAM_VAL(L_EPC,2)  = IMOM_NOIB(2)
              AAM_VAL(L_EPC,3)  = IMOM_NOIB(3)
              AAM_VAL(L_EPC,4)  = IMOM_IB(1)
              AAM_VAL(L_EPC,5)  = IMOM_IB(2)
              AAM_VAL(L_EPC,6)  = IMOM_IB(3)
              AAM_VAL(L_EPC,7)  = HMOM(1)
              AAM_VAL(L_EPC,8)  = HMOM(2)
              AAM_VAL(L_EPC,9)  = HMOM(3)
              AAM_VAL(L_EPC,10) = EXF_NOIB(1)
              AAM_VAL(L_EPC,11) = EXF_NOIB(2)
              AAM_VAL(L_EPC,12) = EXF_NOIB(3)
              AAM_VAL(L_EPC,13) = EXF_IB(1)
              AAM_VAL(L_EPC,14) = EXF_IB(2)
              AAM_VAL(L_EPC,15) = EXF_IB(3)
            ELSE 
              IF ( FCS_AGE < FCS_TIM(IND_EPC) ) THEN
                   IF ( IVRB > 10 ) THEN
                        WRITE ( 6, * ) 'New fcs:   ', DATE_FCS, ' ', DATE_AAM, ' ', &
     &                                  FIL_AAM(J2)
                   END IF
!
! ---------------- Old epoch, but newer forecast
!
                   FCS_TIM(IND_EPC) = FCS_AGE
!
                   CALL ERR_PASS  ( IUER, IER )
                   CALL PARSE_AAM ( FIL_AAM(J2), MJD_MOM, TAI_MOM, &
     &                              IMOM_NOIB, IMOM_IB, HMOM, EXF_NOIB, EXF_IB, &
     &                              DATA_SOURCE, DATA_TITLE, IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 5127, IUER, 'AAM_FCS_SPL', 'Failure in '// &
     &                      'parsing the AAM file '//FIL_AAM(J2) )
                        RETURN
                   END IF
                   AAM_VAL(IND_EPC,1)  = IMOM_NOIB(1)
                   AAM_VAL(IND_EPC,2)  = IMOM_NOIB(2)
                   AAM_VAL(IND_EPC,3)  = IMOM_NOIB(3)
                   AAM_VAL(IND_EPC,4)  = IMOM_IB(1)
                   AAM_VAL(IND_EPC,5)  = IMOM_IB(2)
                   AAM_VAL(IND_EPC,6)  = IMOM_IB(3)
                   AAM_VAL(IND_EPC,7)  = HMOM(1)
                   AAM_VAL(IND_EPC,8)  = HMOM(2)
                   AAM_VAL(IND_EPC,9)  = HMOM(3)
                   AAM_VAL(IND_EPC,10) = EXF_NOIB(1)
                   AAM_VAL(IND_EPC,11) = EXF_NOIB(2)
                   AAM_VAL(IND_EPC,12) = EXF_NOIB(3)
                   AAM_VAL(IND_EPC,13) = EXF_IB(1)
                   AAM_VAL(IND_EPC,14) = EXF_IB(2)
                   AAM_VAL(IND_EPC,15) = EXF_IB(3)
              END IF
         END IF
 420  CONTINUE 
!
      IF ( IVRB > 10 ) THEN
           DO 530 J3=1,L_EPC
!!              write ( 6, '("FF: ", I5,1X,A,1X, F7.1, 1X, F7.1, 1X, 1PD12.5)' ) J3, FIL_FIN(J3)(1:I_LEN(FIL_FIN(J3))), TIM_ARR(J3)/3600.D0, FCS_TIM(J3)/3600.0D0, AAM_VAL(J3,1)  ! %%%
              STR = MJDSEC_TO_DATE ( MJD_BEG, TAI_BEG + TIM_ARR(J3), IER )
              WRITE ( 6, 110 ) J3, STR(1:19), FCS_TIM(J3)/3600.0D0 
 110          FORMAT ( I5, ') Date: ', A, ' FCS_TIM = ', F8.2 )
 530       CONTINUE 
      END IF
!
      DO 430 J3=1,15
         CALL ERR_PASS  ( IUER, IER )
         CALL MAKE_SPLINE ( 3, L_EPC, TIM_ARR, AAM_VAL(1,J3), 0.0D0, 0.0D0, &
     &                      AAM_SPL(1,J3), TMP_ARR, IER )
         IF ( IER .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( J3, STR )
              CALL ERR_LOG ( 5128, IUER, 'AAM_FCS_SPL', 'Failure in '// &
     &            'computing spline coefficients for component '//STR )
              RETURN
         END IF
 430  CONTINUE 
!
      MJD_END = MJD_BEG
      TAI_END = TAI_BEG + TIM_ARR(L_EPC)
      IDAY    = TAI_END/86400.0D0
      IF ( IDAY > 0 ) THEN
           MJD_END = MJD_END + IDAY
           TAI_END = TAI_END - IDAY*86400.0D0
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE AAM_FCS_SPL  !#!#
