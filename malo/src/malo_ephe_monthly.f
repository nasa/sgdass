      SUBROUTINE MALO_EPHE_MONTHLY ( MALO, DIR_EPH, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_EPHE_MONTHLY
! *                                                                      *
! * ### 11-MAY-2013 MALO_EPHE_MONTHLY v1.2 (c) L. Petrov 15-JUN-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( MALO__TYPE ) :: MALO_EPHE
      CHARACTER  DIR_EPH*(*)
      INTEGER*4  IVRB, IUER
      INTEGER*8  DIR_DESC(16), IP8
      CHARACTER  FILNAM*128, STR*128, EXT*4, C_FIL(MALO__FIL)*128, &
     &           DATE_FIL*19, MON_LAST*7, DIR*128, FILOUT*128
      PARAMETER  ( EXT = '.eph' )
      INTEGER*4  M_MON
      PARAMETER  ( M_MON = 60*12 )
      INTEGER*4  J1, J2, J3, J4, J5, J6, I_EPC, LEV, L_FIL, MJD, &
     &           MJD_BEG, MJD_END, MJD_LAST, MON_RANGE(2,M_MON), N_MON, &
     &           IDAY, ID, IL, IP, IS, IER
      REAL*8,    ALLOCATABLE :: DSPL(:,:,:)
      REAL*8     DUMMY_R8
      LOGICAL*1  LEX
      INTEGER*4  L_STA, L_EPC, L_TIM, N_EPC
      REAL*8     SEC, SEC_BEG, SEC_END, SEC_LAST, TIM_INT, TIM_EPS
      PARAMETER  ( TIM_EPS = 50.0 )
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*32, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, LINDEX, &
     &                       UNLINK
      INTEGER*8, EXTERNAL :: OPENDIR, CLOSEDIR
!
      DIR = DIR_EPH
      DIR_DESC(1) = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
      IF ( DIR_DESC(1) .LE. 0 ) THEN
           ID = LINDEX ( DIR, '/' )
           IF ( ID .LE. 1 ) THEN
                CALL ERR_LOG ( 6121, IUER, 'MALO_EPHE_MONTHLY', &
     &              'Directory wiht displacements time series '// &
     &               DIR(1:I_LEN(DIR))//' does not exist' )
                RETURN
           END IF
           CALL CLRCH ( DIR(ID:) )
           DIR_DESC(1) = OPENDIR ( DIR(1:I_LEN(DIR))//CHAR(0) ) 
           IF ( DIR_DESC(1) .LE. 0 ) THEN
                CALL ERR_LOG ( 6122, IUER, 'MALO_EPHE_MONTHLY', &
     &              'Directory wiht displacements time series '// &
     &               DIR(1:I_LEN(DIR))//' does not exist' )
                RETURN
              ELSE 
                IP = CLOSEDIR ( %VAL(DIR_DESC(1)) )
           END IF
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC(1)) )
      END IF
!
      L_FIL = 0
      LEV = 0
      DO 410 J1=1,16*MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6123, IUER, 'MALO_EPHE_MONTHLY', 'Error in '// &
     &            'reading input directory '//DIR(1:I_LEN(DIR))// &
     &            '  '//FILNAM )
              RETURN
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IL = ILEN(FILNAM)
         IF ( IL < 18                      ) GOTO 410
         IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 410
         IF ( FILNAM(IL-8:IL-8)   .NE. '_' ) GOTO 410
         IF ( FILNAM(IL-17:IL-17) .NE. '_' ) GOTO 410
!
         L_FIL = L_FIL + 1
         C_FIL(L_FIL) = FILNAM 
 410  CONTINUE 
 810  CONTINUE 
      IF ( L_FIL == 0 ) THEN 
           IF ( IVRB .GE. 1 ) THEN
                WRITE ( 6, '(A)' ) 'MALO_EPHE_MONTHLY: no eph-files have been found'
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN 
        ELSE IF ( L_FIL == 1 ) THEN 
           IF ( IVRB .GE. 1 ) THEN
                WRITE ( 6, '(A)' ) 'MALO_EPHE_MONTHLY: only one eph-files have been found'
           END IF
           CALL ERR_LOG ( 0, IUER )
           RETURN 
      END IF
      CALL SORT_FAST_CH ( L_FIL, C_FIL )
!
      N_MON = 0 
      MON_RANGE = 0
      DO 420 J2=1,L_FIL
         IL = ILEN(C_FIL(J2))
         DATE_FIL = C_FIL(J2)(IL-16:IL-13)//'.'//C_FIL(J2)(IL-12:IL-11)//'.'// &
     &              C_FIL(J2)(IL-10:IL-6)//':'//C_FIL(J2)(IL-5:IL-4)// &
     &              ':00'
!
         CALL ERR_PASS ( IUER, IER )
         CALL DATE_TO_TIME ( DATE_FIL, MJD, SEC, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6125, IUER, 'MALO_EPHE_MONTHLY', 'Error in '// &
     &            'parsing the name of the file in EPHEDISP format '// &
     &             C_FIL(J2) )
              RETURN
         END IF
         IF ( J2 == 1 ) THEN
              MJD_BEG = MJD
              SEC_BEG = SEC 
              N_MON = 1
              MON_RANGE(1,N_MON) = J2
            ELSE IF ( J2 == L_FIL ) THEN
              MJD_END = MJD
              SEC_END = SEC 
            ELSE IF ( J2 == 2 ) THEN
              TIM_INT = (MJD - MJD_BEG)*86400.0D0 + (SEC - SEC_BEG)
            ELSE
              IF ( DABS( ((MJD - MJD_LAST)*86400.0D0 + (SEC - SEC_LAST)) - &
     &                     TIM_INT) > TIM_EPS ) THEN
                   CALL CLRCH ( STR )
                   write ( 6, * ) 'DT=  ', (MJD - MJD_LAST)*86400.0D0 + (SEC - SEC_LAST) ! %%%
                   WRITE ( UNIT=STR(1:15), FMT='(1PE15.7)' ) TIM_INT
                   CALL ERR_LOG ( 6126, IUER, 'MALO_EPHE_MONTHLY', 'Missing epochs '// &
     &                 'between files '//C_FIL(J2-1)(1:I_LEN(C_FIL(J2-1)))//' and '// &
     &                 C_FIL(J2)(1:I_LEN(C_FIL(J2)))//' while expected interval is '// &
     &                 STR)
                   RETURN
              END IF
         END IF
         IF ( J2  > 1 ) THEN
              IF ( DATE_FIL(1:7) .NE. MON_LAST ) THEN
                   N_MON = N_MON + 1
                   MON_RANGE(1,N_MON) = J2
              END IF
         END IF
         MON_RANGE(2,N_MON) = J2
         MJD_LAST = MJD
         SEC_LAST = SEC
         MON_LAST = DATE_FIL(1:7)
 420  CONTINUE 
!
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, * ) 'MALO_EPHE_MONTHLY MJD_BEG= ', MJD_BEG, ' SEC_BEG= ', SNGL(SEC_BEG), &
     &                                     ' MJD_END= ', MJD_END, ' SEC_END= ', SNGL(SEC_END)
           WRITE ( 6, * ) 'MALO_EPHE_MONTHLY L_FIL= ', L_FIL, ' TIM_INT= ', SNGL(TIM_INT)
           WRITE ( 6, * ) 'MALO_EPHE_MONTHLY N_MON = ', N_MON
           WRITE ( 6, * ) 'MALO_EPHE_MONTHLY MON_RANGE = ', MON_RANGE(1,1), MON_RANGE(2,1)
           WRITE ( 6, * ) 'MALO_EPHE_MONTHLY MON_RANGE = ', MON_RANGE(1,2), MON_RANGE(2,2)
           WRITE ( 6, * ) 'MALO_EPHE_MONTHLY MON_RANGE = ', MON_RANGE(1,3), MON_RANGE(2,3)
           WRITE ( 6, * ) 'MALO_EPHE_MONTHLY MON_RANGE = ', MON_RANGE(1,4), MON_RANGE(2,4)
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_EPHEDISP_READ ( C_FIL(1), MALO_EPHE, DUMMY_R8, MALO__INQ, &
     &                          L_STA, L_EPC, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6127, IUER, 'MALO_EPHE_MONTHLY', 'Error in '// &
     &         'attempt to read the first EPHEDISP file '//C_FIL(1) )
           RETURN
      END IF
      IF ( IVRB .GE. 4 ) THEN
           WRITE ( 6, * ) 'MALO_EPHE_MONTHLY L_STA= ', MALO_EPHE%NSTA, &
     &                                     ' L_EPC= ', MALO_EPHE%NTIM
      END IF     
      L_STA = MALO_EPHE%NSTA
      L_TIM = MALO_EPHE%NTIM
      CALL MALO_FREE ( MALO_EPHE, IER )
!
      N_EPC = 32*86400.0/TIM_INT
      ALLOCATE ( DSPL(3,L_STA,N_EPC), STAT=IER  )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*3*L_STA*N_EPC, STR )
           CALL ERR_LOG ( 6128, IUER, 'MALO_EPHE_MONTHLY', 'Error in '// &
     &         'attempt to allocate '//STR(1:I_LEN(STR))//' bytes of '// &
     &         'dynamic memory for array DSPL' )
           RETURN
      END IF
!
      DO 430 J3=1,N_MON
         I_EPC = 1
         DO 440 J4=MON_RANGE(1,J3),MON_RANGE(2,J3)
            CALL ERR_PASS ( IUER, IER )
            CALL MALO_EPHEDISP_READ ( C_FIL(J4), MALO_EPHE, DSPL(1,1,I_EPC), &
     &                                MALO__REA, L_STA, L_TIM, IER )
!
            IF ( IER .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL ERR_LOG ( 6129, IUER, 'MALO_EPHE_MONTHLY', 'Error in '// &
     &               'an attempt to read mass loading displacements in '// &
     &               'EPHEDISP format from input file '//C_FIL(J4) )
                 RETURN
            END IF
            IF ( J4 < MON_RANGE(2,J3) ) THEN
                 CALL MALO_FREE ( MALO_EPHE, IER )
                 I_EPC = I_EPC + L_TIM
            END IF
 440     CONTINUE 
!!         DSPL(1:3,1:L_STA,1:I_EPC) = 1.0E3*DSPL(1:3,1:L_STA,1:I_EPC)
!
         IL = ILEN(C_FIL(MON_RANGE(1,J3)))
         FILOUT = C_FIL(MON_RANGE(1,J3))(1:IL-17)// &
     &            C_FIL(MON_RANGE(1,J3))(IL-16:IL-13)//'_'// &
     &            C_FIL(MON_RANGE(1,J3))(IL-12:IL-11)//EXT
!
         MALO_EPHE%NTIM = I_EPC
         IF ( ASSOCIATED ( MALO_EPHE%MJD_ARR ) ) DEALLOCATE ( MALO_EPHE%MJD_ARR )
         IF ( ASSOCIATED ( MALO_EPHE%TAI_ARR ) ) DEALLOCATE ( MALO_EPHE%TAI_ARR )
!
         ALLOCATE ( MALO_EPHE%MJD_ARR(MALO_EPHE%NTIM), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL IINCH ( 4*MALO_EPHE%NTIM, STR )
              CALL ERR_LOG ( 6130, IUER, 'MALO_EPHE_MONTHLY', 'Error in '// &
     &            'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'of dynamic memory for array MALO_EPHE%MJD_ARR' )
              RETURN
         END IF
!
         ALLOCATE ( MALO_EPHE%TAI_ARR(MALO_EPHE%NTIM), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL IINCH ( 8*MALO_EPHE%NTIM, STR )
              CALL ERR_LOG ( 6131, IUER, 'MALO_EPHE_MONTHLY', 'Error in '// &
     &            'an attempt to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &            'of dynamic memory for array MALO_EPHE%MJD_ARR' )
              RETURN
         END IF
!
         I_EPC = 0
         DO 450 J5=MON_RANGE(1,J3),MON_RANGE(2,J3)
            I_EPC = I_EPC + 1
            MALO_EPHE%MJD_ARR(I_EPC) = MJD_BEG
            MALO_EPHE%TAI_ARR(I_EPC) = SEC_BEG + TIM_INT*(J5-1)
            IDAY = (MALO_EPHE%TAI_ARR(I_EPC) + TIM_EPS)/86400.0D0 
            MALO_EPHE%MJD_ARR(I_EPC) = MALO_EPHE%MJD_ARR(I_EPC) + IDAY
            MALO_EPHE%TAI_ARR(I_EPC) = MALO_EPHE%TAI_ARR(I_EPC) - IDAY*86400.0D0
 450     CONTINUE 
         MALO_EPHE%NTIM = I_EPC
         MALO_EPHE%MJD_BEG = MALO_EPHE%MJD_ARR(1)
         MALO_EPHE%TAI_BEG = MALO_EPHE%TAI_ARR(1)
         MALO_EPHE%MJD_END = MALO_EPHE%MJD_ARR(I_EPC)
         MALO_EPHE%TAI_END = MALO_EPHE%TAI_ARR(I_EPC)
!
         CALL ERR_PASS ( IUER, IER )
         CALL MALO_EPHEDISP_WRITE ( MALO_EPHE, DSPL, MALO__LABEL, FILOUT, &
     &                              MALO%CONF%LOA_FINAM_DESCR, &
     &                              MALO%CONF%LOA_FINAM_COMM, &
     &                              MALO%CONF%EPHEDISP_FINAM_FMT, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 6132, IUER, 'MALO_EPHE_MONTHLY', 'Error in '// &
     &            'an attempt to write the output file with loading '// &
     &            'in EPHEDISP format '//FILOUT )
              RETURN
         END IF
         CALL MALO_FREE ( MALO_EPHE, IER )
!
         IF ( .NOT. MALO%CONF%KEEP_EPHEDISP_ORIG ) THEN
              DO 460 J6=MON_RANGE(1,J3),MON_RANGE(2,J3)
                 INQUIRE ( FILE=C_FIL(J6), EXIST=LEX )
                 IF ( LEX )  THEN
                      IS = UNLINK ( C_FIL(J6)(1:I_LEN(C_FIL(J6)))//CHAR(0) )
                      IF ( IS .NE. 0 ) THEN
                           CALL ERR_LOG ( 6133, IUER, 'MALO_EPHE_MONTHLY', &
     &                         'Error in an attempt to remove temporary file '// &
     &                         'with displacements in EPHEDISP format '//C_FIL(J6) )
!!                           RETURN
                      END IF
                 END IF
 460          CONTINUE 
         END IF
 430  CONTINUE 
      DEALLOCATE ( DSPL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_EPHE_MONTHLY  !#!#
