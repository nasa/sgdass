       SUBROUTINE MALO_READ_AOD1B ( MODE, FIL, MALO, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  MALO_READ_AOD1B  reads spherical hamronics from           *
! *   geophysical fluids in AOD1B format and returns them in array       *
! *   MALO%SPH. It also reads time epochs for these spherical harmonic   *
! *   coefficients and returns them in MALO%MJD_ARR, MALO%TAI_ARR.       *
! *                                                                      *
! *  ### 31-OCT-2013   READ_AOD1B  v1.1 (c)  L. Petrov  05-APR-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  FIL*(*)
      INTEGER*4  MODE, IVRB, IUER
      INTEGER*4  MP 
      PARAMETER  ( MP = 1024*1024  )
      CHARACTER, ALLOCATABLE :: BUF(:)*80
      LOGICAL*1  FL_READ_COEF 
      CHARACTER  STR*128, AOD1B__LABEL*35
      PARAMETER  ( AOD1B__LABEL = 'FILE TYPE ipAOD1BF            : 999' )
      REAL*8     TAI
      INTEGER*4  J1, J2, NP, N, M, L_TIM, N_PROD, IER 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      ALLOCATE ( BUF(MP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( MP*LEN(BUF(0)), STR )
           CALL ERR_LOG ( 6711, IUER, 'MALO_READ_AOD1B', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for the buffer with '// &
     &         'the file '//FIL )
           RETURN 
      END IF
      IF ( IVRB .GE. 2 ) WRITE ( 6, '(A)' ) 'MALO_SPHE: Read input file '// &
     &                                       FIL(1:I_LEN(FIL))
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6712, IUER, 'MALO_READ_AOD1B', 'Error in '// &
     &         'an attempt to read file with OMCT model '//FIL )
           RETURN 
      END IF
!
      IF ( BUF(3)(1:35) == AOD1B__LABEL ) THEN
           CONTINUE 
         ELSE
           CALL CLRCH ( STR )
           CALL TRAN ( 13, BUF(3), STR )
           CALL ERR_LOG ( 6713, IUER, 'MALO_READ_AOD1B', 'Error in '// &
     &         'an attempt to parse file '//FIL(1:I_LEN(FIL))// &
     &         ' its third line is '//STR(1:64)//' while '// &
     &         AOD1B__LABEL//' was expected' )
           RETURN 
      END IF
!
      FL_READ_COEF = .FALSE.
      L_TIM = 0 
      DO 410 J1=1,NP
         IF ( BUF(J1)(1:31) == 'MAXIMUM DEGREE                :' ) THEN
              CALL CHIN ( BUF(J1)(33:40), MALO%ORD_SPHE )
           ELSE IF ( BUF(J1)(1:31) == 'NUMBER OF DATA SETS           :' ) THEN
              CALL CHIN ( BUF(J1)(33:40), N_PROD )
              MALO%NTIM = N_PROD/4
              ALLOCATE ( MALO%SPH(2,0:MALO%ORD_SPHE,0:MALO%ORD_SPHE,3,MALO%NTIM), &
     &                   STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*3*(1+MALO%ORD_SPHE)**2*MALO%NTIM, STR )
                   CALL ERR_LOG ( 6714, IUER, 'MALO_READ_AOD1B', 'Error in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array of spherical '// &
     &                 'harmonics coefficients MALO%SPH' )
                   RETURN 
              END IF
              MALO%SPH_STATUS = MALO__ALLO
!
              ALLOCATE ( MALO%MJD_ARR(MALO%NTIM), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*MALO%NTIM, STR )
                   CALL ERR_LOG ( 6715, IUER, 'MALO_READ_AOD1B', 'Error in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array MALO%MJD_ARR' )
                   RETURN 
              END IF
              ALLOCATE ( MALO%TAI_ARR(MALO%NTIM), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*MALO%NTIM, STR )
                   CALL ERR_LOG ( 6716, IUER, 'MALO_READ_AOD1B', 'Error in '// &
     &                 'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &                 ' bytes of dynamic memory for array MALO%TAI_ARR' )
                   RETURN 
              END IF
           ELSE IF ( BUF(J1)(1:8) == 'DATA SET' ) THEN
              FL_READ_COEF = .FALSE.
              IF ( MODE == 1 .AND. BUF(J1)(66:68) == 'oba' ) THEN
                   FL_READ_COEF = .TRUE.
              END IF
              IF ( FL_READ_COEF ) THEN
                   L_TIM = L_TIM + 1
                   IF ( L_TIM > MALO%NTIM ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( MALO%NTIM, STR )
                        CALL ERR_LOG ( 6717, IUER, 'MALO_READ_AOD1B', 'Trap of '// &
     &                      'internal control: too many epochs for the '// &
     &                      'spherical harmonics were found in the AOD1B '// &
     &                      'file '//FIL(1:I_LEN(FIL))//' more than '// &
     &                      'MALO%NTIM= '//STR )
                        RETURN 
                   END IF
!
                   CALL ERR_PASS ( IUER, IER )
                   CALL DATE_TO_TIME ( BUF(J1)(38:56), MALO%MJD_ARR(L_TIM), &
     &                                 MALO%TAI_ARR(L_TIM), IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( MALO%NTIM, STR )
                        CALL ERR_LOG ( 6718, IUER, 'MALO_READ_AOD1B', 'Trap of '// &
     &                      'internal control: too many epochs for the '// &
     &                      'spherical harmonics were found in the AOD1B '// &
     &                      'file '//FIL(1:I_LEN(FIL))//' more than MALO%NTIM= '// &
     &                      STR )
                        RETURN 
                   END IF
                   MALO%TAI_ARR(L_TIM) = MALO%TAI_ARR(L_TIM) !! - 32.184D0
!
                   GOTO 410
              END IF
         END IF
         IF ( FL_READ_COEF ) THEN
!
! ----------- Degree N, order M
!
              READ ( UNIT=BUF(J1)(1:7),  FMT='(I3,1X,I3)', IOSTAT=IER ) N, M
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6719, IUER, 'MALO_READ_AOD1B', 'Failue in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the AOD1B '// &
     &                      'file '//FIL )
                   RETURN 
              END IF
              IF ( N > MALO%ORD_SPHE .OR. M > MALO%ORD_SPHE ) GOTO 410
              READ ( UNIT=BUF(J1)(9:39), FMT='(E15.9,1X,E15.9)', &
     &                                   IOSTAT=IER ) MALO%SPH(1:2,N,M,MALO__K,L_TIM)
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 6720, IUER, 'MALO_READ_AOD1B', 'Failue in '// &
     &                 'parsing line '//STR(1:I_LEN(STR))//' of the AOD1B '// &
     &                      'file '//FIL )
                   RETURN 
              END IF
         END IF
 410  CONTINUE 
      DEALLOCATE ( BUF )
      MALO%SPH_STATUS = MALO__LOAD
      MALO%MJD_BEG = MALO%MJD_ARR(1)
      MALO%TAI_BEG = MALO%TAI_ARR(1)
      MALO%UTC_BEG = MALO%TAI_BEG + 100.0
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_UTC_TO_TAI ( MALO, MALO%MJD_BEG, MALO%UTC_BEG, TAI, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6721, IUER, 'MALO_READ_AOD1B', 'Error in '// &
     &         'determining the leap second' )
           RETURN 
      END IF
!
      MALO%UTC_BEG = MALO%TAI_BEG + ( MALO%UTC_BEG -  TAI )
      IF ( MALO%NTIM == 1 ) THEN
           MALO%TIM_STEP = 0.0D0
         ELSE 
           MALO%TIM_STEP = (MALO%MJD_ARR(2) - MALO%MJD_ARR(1))*86400.0D0 + &
     &                     (MALO%TAI_ARR(2) - MALO%TAI_ARR(1))
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_READ_AOD1B  !#!
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_AOD1B_SPHE ( MALO, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_AOD1B_SPHE 
! *                                                                      *
! * ### 31-OCT-2013  MALO_AOD1B_SPHE  v1.0 (c) L. Petrov 01-NOV-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( MALO__TYPE ) :: MALO
      INTEGER*4  IVRB, IUER
      CHARACTER  STR*128
      REAL*8     COEF, MALO_K_1_SAVE 
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*4  J1, J2, J3
!
      IF ( MALO%SPH_STATUS .NE. MALO__LOAD ) THEN
           CALL ERR_LOG ( 6751, IUER, 'MALO_AOD1B_SPHE', 'Trap of '// &
     &         'internal contol: spherical harmonics have not been loaded' )
           RETURN 
      END IF
      IF ( MALO%NLOVE < MALO%ORD_SPHE ) THEN
           CALL ERR_LOG ( 6752, IUER, 'MALO_AOD1B_SPHE', 'Trap of '// &
     &         'internal contol: Love numbers have not been loaded' )
           RETURN 
      END IF
!
      MALO_K_1_SAVE = MALO%LOVE(1,MALO__K) 
      MALO%LOVE(1,MALO__K) = 0.0D0 
      DO 410 J1=1,MALO%ORD_SPHE
         COEF = (2.0D0*J1 + 1.0D0)/(1.D0 + MALO%LOVE(J1,MALO__K))* &
     &          (MALO__GRAV*MALO__DENS*REA__WGS84)/3.00
         DO 420 J2=0,J1
            DO 430 J3=1,MALO%NTIM
               MALO%SPH(1:2,J1,J2,MALO__K,J3) = COEF*MALO%SPH(1:2,J1,J2,MALO__K,J3)
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      MALO%LOVE(1,MALO__K) = MALO_K_1_SAVE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_AOD1B_SPHE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_AOD1B_TO_LOVE ( MALO, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_AOD1B_TO_LOVE 
! *                                                                      *
! * ## 01-NOV-2013 MALO_AOD1B_TO_LOVE v1.0 (c) L. Petrov 01-NOV-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE )  :: HEB
      INTEGER*4  IVRB, IUER
      CHARACTER  STR*128, SUBDIR*128, DATE_STR*32, FILHEB*128
      REAL*8,    ALLOCATABLE :: BPR(:,:)
      REAL*4,    ALLOCATABLE :: BPR_R4(:,:)
      INTEGER*8  FSH
      REAL*8     COEF, MALO_K_1_SAVE, LAT_STEP, PHI, LAM
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, ND, IS, DIR_DESC, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, MKDIR, OPENDIR, CLOSEDIR
      INTEGER*8, EXTERNAL :: SPHE_INIT 
      REAL*8,    EXTERNAL :: SPHE_COMP_VAL 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( MALO%SPH_STATUS .NE. MALO__LOAD ) THEN
           CALL ERR_LOG ( 6761, IUER, 'MALO_AOD1B_TO_LOVE', 'Trap of '// &
     &         'internal control: spherical harmonics have not been loaded' )
           RETURN 
      END IF
      IF ( MALO%NLOVE < MALO%ORD_SPHE ) THEN
           CALL ERR_LOG ( 6762, IUER, 'MALO_AOD1B_TO_LOVE', 'Trap of '// &
     &         'internal control: Love numbers have not been loaded' )
           RETURN 
      END IF
!
      MALO_K_1_SAVE = MALO%LOVE(1,MALO__K) 
!
! --- Set the Love number to 0 in order to refer the loading to the center of 
! --- mass
!
      MALO%LOVE(1,MALO__K) = 0.0D0 
      DO 410 J1=0,MALO%ORD_SPHE
         IF ( J1 == 0 ) THEN
              COEF = (MALO__GRAV*MALO__DENS*REA__WGS84)/3.0D0
            ELSE IF ( J1 == 1 ) THEN
              COEF = 1.0D0
            ELSE
              COEF = (2.0D0*J1 + 1.0D0)/(1.D0 + MALO%LOVE(J1,MALO__K))* &
     &               (MALO__GRAV*MALO__DENS*REA__WGS84)/3.00
         END IF
!
         DO 420 J2=0,J1
            DO 430 J3=1,MALO%NTIM
               MALO%SPH(1:2,J1,J2,MALO__H,J3) = MALO%SPH(1:2,J1,J2,MALO__K,J3)* &
     &                                           3.0D0*MALO%LOVE(J1,MALO__H)/(2*J1+1)/ &
     &                                           (MALO__DENS*MALO__GRAV)*COEF
               MALO%SPH(1:2,J1,J2,MALO__L,J3) = MALO%SPH(1:2,J1,J2,MALO__K,J3)* &
     &                                           3.0D0*MALO%LOVE(J1,MALO__L)/(2*J1+1)/ &
     &                                           (MALO__DENS*MALO__GRAV)*COEF
               IF ( IVRB .GE. 7 .OR. IVRB .GE. 8 ) THEN
                    MALO%SPH(1:2,J1,J2,MALO__K,J3) = MALO%SPH(1:2,J1,J2,MALO__K,J3)* &
     &                                               COEF 
               END IF
               IF ( J1 < 2 ) THEN
                    MALO%SPH(1,J1,J2,MALO__H,J3) = 0.0D0
                    MALO%SPH(2,J1,J2,MALO__L,J3) = 0.0D0
                    MALO%SPH(2,J1,J2,MALO__K,J3) = 0.0D0
               END IF
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      MALO%LOVE(1,MALO__K) = MALO_K_1_SAVE 
!
      IF ( IVRB == 8 ) THEN
!
! -------- Initialization of the spherical harmonics package
!
           CALL ERR_PASS ( IUER, IER )
           FSH = SPHE_INIT ( -1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6763, IUER, 'MALO_AOD1B_TO_LOVE', 'Error '// &
     &              'in an attempt to initialize FSH object for '// &
     &              'spherical harmonics transform'  )
                RETURN
           END IF   
!
           ND = 2*(MALO%ORD_SPHE+1)
!
           ALLOCATE ( BPR(2*ND,ND), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*2*ND*ND, STR )
                CALL ERR_LOG ( 6764, IUER, 'MALO_AOD1B_TO_LOVE', 'Error in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dynamic memory for bottom pressure' )
                RETURN
           END IF
!
           ALLOCATE ( BPR_R4(2*ND,ND), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*2*ND*ND, STR )
                CALL ERR_LOG ( 6765, IUER, 'MALO_AOD1B_TO_LOVE', 'Error in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dynamic memory for bottom pressure' )
                RETURN
           END IF
!
           DO 440 J4=1,4
              CALL ERR_PASS ( IUER, IER )
              CALL SPHE_INV_2NN ( %VAL(FSH), MALO%ORD_SPHE, MALO%ORD_SPHE, 1, 1, &
     &                             MALO%SPH(1,0,0,MALO__K,J4), ND, BPR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6766, IUER, 'MALO_AOD1B_TO_LOVE', 'Error '// &
     &                 'in an attempt to compute inverse Fourier transform' )
                   RETURN
              END IF   
!
              STR = MJDSEC_TO_DATE ( MALO%MJD_ARR(J4), MALO%TAI_ARR(J4), -2 )
              BPR_R4 = BPR
!
              CALL ERR_PASS ( IUER, IER )
              CALL PLOT_GRID_R4 ( 1, 7, 0, 1, 2*ND, ND, BPR_R4, &
     &                            'OMCT Bottom pressure at '//STR(1:16), &
     &                            'Pa', 1.0, -1.0, '/tmp/foo', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6767, IUER, 'MALO_AOD1B_TO_LOVE', 'Error in '// &
     &                 'an attempt to show a plot of gridded bottom pressure' )
                   RETURN
              END IF
 440       CONTINUE 
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_AOD1B_TO_LOVE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE MALO_AOD1B_TO_PRES ( MALO, IVRB, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_AOD1B_TO_PRES 
! *                                                                      *
! * ## 11-FEB-2014 MALO_AOD1B_TO_PRES v1.0 (c) L. Petrov 11-FEB-2014 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'malo.i'
      INCLUDE   'heb.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( MALO__TYPE ) :: MALO
      TYPE     ( HEB__TYPE )  :: HEB
      INTEGER*4  IVRB, IUER
      CHARACTER  STR*128, SUBDIR*128, DATE_STR*32, FILHEB*128
      REAL*8,    ALLOCATABLE :: BPR(:,:)
      REAL*4,    ALLOCATABLE :: BPR_R4(:,:)
      INTEGER*8  FSH
      REAL*8     COEF, MALO_K_1_SAVE, LAT_STEP, PHI, LAM
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, ND, IS, DIR_DESC, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      INTEGER*8, EXTERNAL :: SPHE_INIT 
      REAL*8,    EXTERNAL :: SPHE_COMP_VAL 
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
      IF ( MALO%SPH_STATUS .NE. MALO__LOAD ) THEN
           CALL ERR_LOG ( 6761, IUER, 'MALO_AOD1B_TO_PRES', 'Trap of '// &
     &         'internal control: spherical harmonics have not been loaded' )
           RETURN 
      END IF
      IF ( MALO%NLOVE < MALO%ORD_SPHE ) THEN
           CALL ERR_LOG ( 6762, IUER, 'MALO_AOD1B_TO_PRES', 'Trap of '// &
     &         'internal control: Love numbers have not been loaded' )
           RETURN 
      END IF
!
      MALO_K_1_SAVE = MALO%LOVE(1,MALO__K) 
!
! --- Set the Love number to 0 in order to refer the loading to the center of 
! --- mass
!
      MALO%LOVE(1,MALO__K) = 0.0D0 
      DO 410 J1=0,MALO%ORD_SPHE
         IF ( J1 == 0 ) THEN
              COEF = (MALO__GRAV*MALO__DENS*REA__WGS84)/3.0D0
            ELSE
              COEF = (2.0D0*J1 + 1.0D0)/(1.D0 + MALO%LOVE(J1,MALO__K))* &
     &               (MALO__GRAV*MALO__DENS*REA__WGS84)/3.00
         END IF
!
         DO 420 J2=0,J1
            DO 430 J3=1,MALO%NTIM
               MALO%SPH(1:2,J1,J2,MALO__K,J3) = MALO%SPH(1:2,J1,J2,MALO__K,J3)* &
     &                                          COEF 
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
      MALO%LOVE(1,MALO__K) = MALO_K_1_SAVE 
!
      IF ( IVRB == 8 ) THEN
!
! -------- Initialization of the spherical harmonics package
!
           CALL ERR_PASS ( IUER, IER )
           FSH = SPHE_INIT ( -1, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 6763, IUER, 'MALO_AOD1B_TO_PRES', 'Error '// &
     &              'in an attempt to initialize FSH object for '// &
     &              'spherical harmonics transform'  )
                RETURN
           END IF   
!
           ND = 2*(MALO%ORD_SPHE+1)
!
           ALLOCATE ( BPR(2*ND,ND), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 8*2*ND*ND, STR )
                CALL ERR_LOG ( 6764, IUER, 'MALO_AOD1B_TO_PRES', 'Error in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dynamic memory for bottom pressure' )
                RETURN
           END IF
!
           ALLOCATE ( BPR_R4(2*ND,ND), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL CLRCH ( STR )
                CALL IINCH ( 4*2*ND*ND, STR )
                CALL ERR_LOG ( 6765, IUER, 'MALO_AOD1B_TO_PRES', 'Error in '// &
     &              'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &              ' bytes of dynamic memory for bottom pressure' )
                RETURN
           END IF
!
           DO 440 J4=1,4
              CALL ERR_PASS ( IUER, IER )
              CALL SPHE_INV_2NN ( %VAL(FSH), MALO%ORD_SPHE, MALO%ORD_SPHE, 1, 1, &
     &                             MALO%SPH(1,0,0,MALO__K,J4), ND, BPR, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6766, IUER, 'MALO_AOD1B_TO_PRES', 'Error '// &
     &                 'in an attempt to compute inverse Fourier transform' )
                   RETURN
              END IF   
!
              STR = MJDSEC_TO_DATE ( MALO%MJD_ARR(J4), MALO%TAI_ARR(J4), -2 )
              BPR_R4 = BPR
!
              CALL ERR_PASS ( IUER, IER )
              CALL PLOT_GRID_R4 ( 1, 7, 0, 1, 2*ND, ND, BPR_R4, &
     &                            'OMCT Bottom pressure at '//STR(1:16), &
     &                            'Pa', 1.0, -1.0, '/tmp/foo', IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 6767, IUER, 'MALO_AOD1B_TO_PRES', 'Error in '// &
     &                 'an attempt to show a plot of gridded bottom pressure' )
                   RETURN
              END IF
 440       CONTINUE 
           CALL EXIT ( 0 )
      END IF
!
! --- Initialization of the spherical harmonics package
!
      CALL ERR_PASS ( IUER, IER )
      FSH = SPHE_INIT ( -1, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 6768, IUER, 'MALO_AOD1B_TO_PRES', 'Error '// &
     &         'in an attempt to initialize FSH object for '// &
     &         'spherical harmonics transform'  )
           RETURN
      END IF   
!
      ND = 360
      MALO%NLAT = ND
      MALO%NLON = 2*ND
      MALO%NTIM = 4
!
      ALLOCATE ( BPR(MALO%NLON,MALO%NLAT), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*MALO%NLON*MALO%NLAT, STR )
           CALL ERR_LOG ( 6769, IUER, 'MALO_AOD1B_TO_PRES', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for bottom pressure' )
           RETURN
      END IF
!
      ALLOCATE ( MALO%SPR(MALO%NLON,MALO%NLAT,MALO%NTIM), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*MALO%NLON*MALO%NLAT*MALO%NTIM, STR )
           CALL ERR_LOG ( 6771, IUER, 'MALO_AOD1B_TO_PRES', 'Error in '// &
     &         'an attempt to allocate '//STR(1:I_LEN(STR))// &
     &         ' bytes of dynamic memory for bottom pressure' )
           RETURN
      END IF
!
      DO 450 J5=1,MALO%NTIM
         DO 460 J6=1,MALO%NLAT
            PHI = -P2I + (J6-0.5)*PI__NUM/ND
            DO 470 J7=1,MALO%NLON
               LAM = (J7-0.5)*PI__NUM/ND
               BPR(J7,J6) = SPHE_COMP_VAL ( %VAL(FSH), MALO%ORD_SPHE, MALO%ORD_SPHE, &
     &                                      PHI, LAM, 1, 1, MALO%SPH(1,0,0,MALO__K,J5), &
     &                                      IER )
               MALO%SPR(J7,J6,J5) = BPR(J7,J6) 
!!             WRITE ( 6, 210 ) LAM/DEG__TO__RAD, PHI/DEG__TO__RAD, BPR_R4(J7,J6)
 210           FORMAT ( F9.3, F9.3, F10.4 )
 470        CONTINUE 
 460     CONTINUE 
 450  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_AOD1B_TO_PRES  !#!#
