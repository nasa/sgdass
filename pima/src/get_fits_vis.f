      SUBROUTINE GET_FITS_VIS ( FINAM, VIS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GET_FITS_VIS reads the calibrated fringe visibility data  *
! *   for observation of a source from the file in fits format generated *
! *   by AIPS or DIFMAP and fills fields of the object VIS.              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FINAM ( CHARACTER ) -- Name of the input FITS file with visibility  *
! *                         data.                                        *
! *                                                                     *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *    VIS ( VIS__TYPE ) -- object which keeps variables related to the  *
! *                         visibility data for this source.             *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! *                             positive non-zero in the case of errors. *
! *                                                                      *
! *  ### 29-JAN-2007  GET_FITS_VIS  v4.2 (c)  L. Petrov 21-DEC-2018 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE    'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      CHARACTER  FINAM*(*)
      INTEGER*4  IUER
      CHARACTER  COMMENT*128, KEYS(MKEY,MHDR)*80, DATE_CHR*21, STR*128, &
     &           C_STA(MSTA)*8, STA_NAM*8
      LOGICAL*4  ANYF
      REAL*4     NULLVAL, CHW_ARR_R4(SMP__MAX), GAIN_CORR(SMP__MAX), &
     &           GACO_ERR(SMP__MAX)
      REAL*4,    ALLOCATABLE :: ARR2_R4(:,:), ARR3_R4(:,:,:,:)
      REAL*8     JD0, JD1, JD, UV_SCALE(2), SKY_FRQ(SMP__MAX)
      INTEGER*4  BITPIX, INUM, HDU_TYPE, GROUP, PCOUNT, NPOL, NVIS_ARR(SMP__MAX), &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, J12, &
     &           J13, J14, J15, J16, J17, J18, J19, J20, J21, J22, &
     &           IND_DAT(2), IND_BAS, IND_TIM, IFRQ, IP, IER
      INTEGER*4  IND_TAB_FRQ, IND_TAB_GACO, IND_COL_FRQ, IND_COL_CHW, &
     &           IND_GACO_COR, IND_GACO_ERR, IND_GACO_NUV, &
     &           STA_IND(MSTA), IND_TAB_ANN(MSUB), IND_NOSTA(MSUB), &
     &           IND_STANAM(MSUB), LSTA(MSUB), NAXIS2_VAL
      INTEGER*4  LKEY(MHDR), LHDR, FT_STATUS
      INTEGER*8  FPTR
      LOGICAL*1  FL_RR_POL, FL_LL_POL, FL_RL_POL, FL_LR_POL
      INTEGER*4  N1, N2, N3, KP, IS, ISTA, LSUB, NCHN, NARR(8), HDUTYPE
      INTEGER*1, ALLOCATABLE :: BUF(:)
      ADDRESS__TYPE, ALLOCATABLE :: DESC(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, ADD_CLIST, LTM_DIF
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
! --- Release dynamic memory which may have been allocated earlier
!
      CALL VIS_FREE_CHW ( VIS )
!
      FL_RR_POL = .FALSE.
      CALL GETENVAR ( 'FITS_RR_POL_ONLY', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) THEN
           FL_RR_POL = .TRUE.
      END IF
!
      FL_LL_POL = .FALSE.
      CALL GETENVAR ( 'FITS_LL_POL_ONLY', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) THEN
           FL_LL_POL = .TRUE.
      END IF
!
      FL_RL_POL = .FALSE.
      CALL GETENVAR ( 'FITS_RL_POL_ONLY', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) THEN
           FL_RL_POL = .TRUE.
      END IF
!
      FL_LR_POL = .FALSE.
      CALL GETENVAR ( 'FITS_LR_POL_ONLY', STR )
      IF ( STR(1:3) == 'yes' .OR. STR(1:3) == 'YES' ) THEN
           FL_LR_POL = .TRUE.
      END IF
!
! --- Open fits file
!
      FT_STATUS = 0
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_OPEN ( FINAM, FPTR, 'OLD', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4611, IUER, 'GET_FITS_VIS', 'Error in opening '// &
     &                   'fits file '//FINAM )
           RETURN
      END IF
!
! --- Read the headers
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_GET_KEYS ( FPTR, MHDR, MKEY, LHDR, LKEY, KEYS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4612, IUER, 'GET_FITS_VIS', 'Error in attempt '// &
     &         'to read the header of fits file '//FINAM )
           CALL FFITS_CLOSE ( FPTR, -2 )
           RETURN
      END IF
      IND_TAB_ANN  = -1
      IND_TAB_FRQ  = -1
      IND_TAB_GACO = -1
      IND_COL_FRQ  =  2
      IND_GACO_COR = -1
      IND_GACO_ERR = -1
      IND_GACO_NUV = -1
!
! --- Scanning all keyword records for something useful
!
      IND_DAT(1)  = 0
      IND_DAT(2)  = 0
      IND_TIM     = 0
      IND_BAS     = 0
      UV_SCALE(1) = 1.0D0
      UV_SCALE(2) = 1.0D0
      NPOL = 1
      LSUB = 0
      VIS%STATUS_GACO = SMP__UNDF
      DO 410 J1=1,LHDR
         DO 420 J2=1,LKEY(J1)
            IF ( KEYS(J2,J1)(1:20) == "EXTNAME = 'AIPS AN '" ) THEN
                 IF ( KEYS(J2+1,J1)(1:9 ) == "EXTVER  =" )THEN
                      READ ( UNIT= KEYS(J2+1,J1)(27:30), FMT='(I4)' ) LSUB
                   ELSE 
                      LSUB = 1
                 END IF
                 IND_TAB_ANN(LSUB)  = J1
            END IF
            IF ( KEYS(J2,J1)(1:20) == "EXTNAME = 'AIPS FQ '" ) THEN
                 IND_TAB_FRQ  = J1
            END IF
            IF ( KEYS(J2,J1)(1:20) == "EXTNAME = 'GACO    '" ) THEN
                 IND_TAB_GACO = J1
            END IF
            IF ( J1 == 1 ) THEN
                 IF ( KEYS(J2,1)(1:8) == 'NAXIS3  ' ) THEN
                     READ ( UNIT=KEYS(J2,1)(21:30), FMT='(I10)', IOSTAT=IER ) NPOL
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4613, IUER, 'GET_FITS_VIS', 'Error in '// &
     &                        'attempt to decode the number of polarizations '// &
     &                        'from the fits file '//FINAM )
                          CALL FFITS_CLOSE ( FPTR, -2 )
                     END IF
                     VIS%NSTK = NPOL
                   ELSE IF ( KEYS(J2,1)(1:8) == 'NAXIS4  ' ) THEN
!
! ------------------ Get the number of intermediate frequencies
!
                     READ ( UNIT=KEYS(J2,1)(21:30), FMT='(I10)', IOSTAT=IER ) NCHN
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4614, IUER, 'GET_FITS_VIS', 'Error in '// &
     &                        'attempt to decode the number of frequencies '// &
     &                        'from the fits file '//FINAM )
                          CALL FFITS_CLOSE ( FPTR, -2 )
                     END IF
                   ELSE IF ( KEYS(J2,1)(1:8) == 'NAXIS5  ' ) THEN
!
! ------------------ Get the number of intermediate frequencies
!
                     READ ( UNIT=KEYS(J2,1)(21:30), FMT='(I10)', IOSTAT=IER ) VIS%NFRQ
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4615, IUER, 'GET_FITS_VIS', 'Error in '// &
     &                        'attempt to decode the number of frequencies '// &
     &                        'from the fits file '//FINAM )
                          CALL FFITS_CLOSE ( FPTR, -2 )
                     END IF
                     VIS%NFRQ = VIS%NFRQ*NCHN
                     ALLOCATE ( VIS%SKY_FRQ(VIS%NFRQ), STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH ( 8*VIS%NFRQ, STR )
                          CALL ERR_LOG ( 4616, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                        'memory' )
                          RETURN
                     END IF
                  ELSE IF ( KEYS(J2,1)(1:8) == 'PCOUNT  ' ) THEN
!
! ------------------ Get parameter PCOUNT
!
                     READ ( UNIT=KEYS(J2,1)(21:30), FMT='(I10)', IOSTAT=IER ) PCOUNT
                     IF ( IER .NE. 0 ) THEN
                         CALL ERR_LOG ( 4617, IUER, 'GET_FITS_VIS', 'Error in '// &
     &                       'attempt to decode the number of pcouint parameter '// &
     &                       'from the fits file '//FINAM )
                         CALL FFITS_CLOSE ( FPTR, -2 )
                     END IF
                  ELSE IF ( KEYS(J2,1)(1:8) == 'GCOUNT  ' ) THEN
!
! ------------------ Get parameter PCOUNT -- the number of accumulation periods
!
                     IF ( VIS%NFRQ == 0 ) THEN
                          CALL ERR_LOG ( 4618, IUER, 'GET_FITS_VIS', 'Trap of '// &
     &                        'internal control: keyword GCOUNT is defined, but '// &
     &                        'keyword NAXIS5 was not defined. Apparently the '// &
     &                        'fits file '//FINAM(1:I_LEN(FINAM))//' does not '// &
     &                        'conform specifications' )
                          CALL FFITS_CLOSE ( FPTR, -2 )
                     END IF
!
                     READ ( UNIT=KEYS(J2,1)(21:30), FMT='(I10)', IOSTAT=IER ) VIS%NP
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4619, IUER, 'GET_FITS_VIS', 'Error in '// &
     &                        'attempt to decode the number of UV points '// &
     &                        'from the fits file '//FINAM )
                          CALL FFITS_CLOSE ( FPTR, -2 )
                     END IF
!
! ------------------ Allocate dynamic memory
!
                     ALLOCATE ( VIS%MJD(VIS%NP), STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH ( 4*VIS%NP, STR )
                          CALL ERR_LOG ( 4620, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                        'memory' )
                          RETURN
                     END IF
!
                     ALLOCATE ( VIS%TAI(VIS%NP), STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH ( 8*VIS%NP, STR )
                          CALL ERR_LOG ( 4621, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                        'memory' )
                          RETURN
                     END IF
!
                     ALLOCATE ( VIS%VIS(VIS%NFRQ,VIS%NP), STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH ( 8*VIS%NP, STR )
                          CALL ERR_LOG ( 4622, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                        'memory' )
                          RETURN
                     END IF
!
                     ALLOCATE ( VIS%UV(2,VIS%NFRQ,VIS%NP), STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH ( 4*2*VIS%NFRQ*VIS%NP, STR )
                          CALL ERR_LOG ( 4623, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                        'memory' )
                          RETURN
                     END IF
!
                     ALLOCATE ( VIS%WEI(VIS%NFRQ,VIS%NP), STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH ( 4*VIS%NFRQ*VIS%NP, STR )
                          CALL ERR_LOG ( 4624, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                        'memory' )
                          RETURN
                     END IF
!
                     ALLOCATE ( VIS%IND_BAS(VIS%NP), STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH ( 16*VIS%NP, STR )
                          CALL ERR_LOG ( 4625, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                        'memory' )
                          RETURN
                     END IF
!
                     ALLOCATE ( VIS%INT_TIM(VIS%NP), STAT=IER )
                     IF ( IER .NE. 0 ) THEN
                          CALL CLRCH ( STR )
                          CALL IINCH ( 16*VIS%NP, STR )
                          CALL ERR_LOG ( 4626, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &                        'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                        'memory' )
                          RETURN
                     END IF
!                    
                     VIS%STATUS  = SMP__ALLC
                END IF
!                    
                IF ( KEYS(J2,1)(1:8) == 'OBJECT  ' ) THEN
!
! ------------------ Get the name of the object 
!
                     VIS%SOU_NAME = KEYS(J2,1)(12:21)
                     IP = INDEX ( VIS%SOU_NAME, "'" )
                     IF ( IP > 0 ) CALL CLRCH ( VIS%SOU_NAME(IP:) )
                END IF
                IF ( KEYS(J2,1)(1:8) == 'OBSERVER' ) THEN
!
! ------------------ Get the name of the object 
!
                     VIS%EXP_NAME = KEYS(J2,1)(12:23)
                     IP = INDEX ( VIS%EXP_NAME, "'" )
                     IF ( IP > 0 ) CALL CLRCH ( VIS%EXP_NAME(IP:) )
                END IF
                IF ( KEYS(J2,1)(1:8) == 'DATE-OBS' ) THEN
!
! ----------------- Get the observation date
!
                     VIS%DATE_OBS= KEYS(J2,1)(12:21)
                     IF ( VIS%DATE_OBS(5:5) == '-' ) THEN
!
! ----------------------- Transform the date to VTD format
!
                          VIS%DATE_OBS = VIS%DATE_OBS(1:4)//'.'// &
     &                                   VIS%DATE_OBS(6:7)//'.'// &
     &                                   VIS%DATE_OBS(9:10)
                     END IF
                END IF
!
                IF ( KEYS(J2,1)(11:22) == "'FREQ    '" ) THEN
!
! ------------------ Get the reference LO frequency
!
                     IF ( KEYS(J2+1,1)(1:5) == 'CRVAL' ) THEN
                          READ ( UNIT=KEYS(J2+1,1)(11:30), FMT='(F20.10)', IOSTAT=IER ) VIS%FRQ_LO
                        ELSE IF ( KEYS(J2+1,1)(1:5) == 'CRVAL' ) THEN
                          READ ( UNIT=KEYS(J2+1,1)(11:30), FMT='(F20.10)', IOSTAT=IER ) VIS%FRQ_LO
                        ELSE IF ( KEYS(J2+2,1)(1:5) == 'CRVAL' ) THEN
                          READ ( UNIT=KEYS(J2+2,1)(11:30), FMT='(F20.10)', IOSTAT=IER ) VIS%FRQ_LO
                        ELSE IF ( KEYS(J2+3,1)(1:5) == 'CRVAL' ) THEN
                          READ ( UNIT=KEYS(J2+3,1)(11:30), FMT='(F20.10)', IOSTAT=IER ) VIS%FRQ_LO
                     END IF
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4627, IUER, 'GET_FITS_VIS', 'Error in '// &
     &                        'attempt to decode the sky frequency '// &
     &                        'from the fits file '//FINAM )
                          CALL FFITS_CLOSE ( FPTR, -2 )
                          RETURN
                     END IF
                END IF
!
                IF ( KEYS(J2,1)(1:8) == 'DATE-OBS' ) THEN
!
! ------------------ Get the observation date
!
                     CALL CLRCH ( DATE_CHR )
                     IF ( KEYS(J2,1)(14:14) == '/' ) THEN
                          DATE_CHR = '19'//KEYS(J2,1)(18:19)//'_'// &
     &                                     KEYS(J2,1)(15:16)//'_'// &
     &                                     KEYS(J2,1)(12:13)//'_00:00:00.0'
                          DATE_CHR(5:5) = '_'
                          DATE_CHR(8:8) = '_'
                        ELSE
                          DATE_CHR = KEYS(J2,1)(12:21)//'_00:00:00.0'
                          DATE_CHR(5:5) = '_'
                          DATE_CHR(8:8) = '_'
                     END IF
                     IF ( KEYS(J2,1)(22:22) == '(' ) THEN
                          IP = INDEX ( KEYS(J2,1)(22:), ')' ) + 21
                          IF ( IP > 23 ) THEN
                               CALL CHIN ( KEYS(J2,1)(23:IP-1), VIS%NUM_SEG )
                             ELSE
                               VIS%NUM_SEG = 1
                          END IF
                        ELSE 
                          VIS%NUM_SEG = 1
                      END IF
                      IER = -1
                      CALL DATE_TO_TIME ( DATE_CHR, VIS%MJD_REF, VIS%TAI_REF, IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4628, IUER, 'GET_FITS_VIS', 'Error in '// &
     &                         'an attempt to decode the observations date from the '// &
     &                         'fits file '//FINAM )
                           CALL FFITS_CLOSE ( FPTR, -2 )
                           RETURN
                       END IF
                END IF
!
                IF ( KEYS(J2,1)(11:20) == "'DATE    '" ) THEN
                     IF ( IND_DAT(1) == 0 ) THEN
                          CALL CHIN ( KEYS(J2,1)(6:8), IND_DAT(1) )
!
! ----------------------- Get the offset of the observation date: Julian date of the
! ----------------------- New Year night precesseding the observations
!
                          READ ( UNIT=KEYS(J2+2,1)(11:30), FMT='(F20.10)', IOSTAT=IER ) JD0
                          IF ( IER .NE. 0 ) THEN
                               CALL ERR_LOG ( 4629, IUER, 'GET_FITS_VIS', 'Error '// &
     &                             'in attempt to decode the PZER0'// &
     &                              KEYS(J2+2,1)(6:8)//' from the fits file '// &
     &                              FINAM )
                               CALL FFITS_CLOSE ( FPTR, -2 )
                               RETURN
                          END IF
                        ELSE
                          CALL CHIN ( KEYS(J2,1)(6:8), IND_DAT(2) )
                          READ ( UNIT=KEYS(J2+2,1)(11:30), FMT='(F20.10)', IOSTAT=IER ) JD1
                          IF ( IER .NE. 0 ) THEN
                               CALL ERR_LOG ( 4630, IUER, 'GET_FITS_VIS', 'Error '// &
     &                             'in attempt to decode the PZER0'// &
     &                              KEYS(J2+2,1)(6:8)//' from the fits file '// &
     &                              FINAM )
                               CALL FFITS_CLOSE ( FPTR, -2 )
                               RETURN
                          END IF
                     END IF
                END IF
!
                IF ( KEYS(J2,1)(11:20) == "'BASELINE'" ) THEN
                     CALL CHIN ( KEYS(J2,1)(6:8), IND_BAS )
                END IF
!
                IF ( KEYS(J2,1)(11:20) == "'INTTIM  '" ) THEN
                     CALL CHIN ( KEYS(J2,1)(6:8), IND_TIM )
                END IF
!
                IF ( KEYS(J2,1)(11:20) == "'UU---SIN'"  .OR. &
     &               KEYS(J2,1)(11:20) == "'UU--    '"       ) THEN
!
                     READ ( UNIT=KEYS(J2+1,1)(11:30), FMT='(F20.10)', IOSTAT=IER ) UV_SCALE(1)
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4631, IUER, 'GET_FITS_VIS', 'Error '// &
     &                        'in attempt to decode the PSCAL'// &
     &                              KEYS(J2+1,1)(6:8)//' from the fits file '// &
     &                              FINAM )
                          CALL FFITS_CLOSE ( FPTR, -2 )
                          RETURN
                     END IF
                END IF
!
                IF ( KEYS(J2,1)(11:20) == "'VV---SIN'"  .OR. &
     &               KEYS(J2,1)(11:20) == "'VV--    '"       ) THEN
!
                     READ ( UNIT=KEYS(J2+1,1)(11:30), FMT='(F20.10)', IOSTAT=IER ) UV_SCALE(2)
                     IF ( IER .NE. 0 ) THEN
                          CALL ERR_LOG ( 4632, IUER, 'GET_FITS_VIS', 'Error '// &
     &                        'in attempt to decode the PSCAL'// &
     &                              KEYS(J2+1,1)(6:8)//' from the fits file '// &
     &                              FINAM )
                          CALL FFITS_CLOSE ( FPTR, -2 )
                          RETURN
                     END IF
                END IF
                IF ( KEYS(J2,1)(1:9) == "GACO    =" ) THEN
                     IF ( INDEX ( KEYS(J2,1)(10:30), '1' ) > 0 ) THEN
                     END IF
                END IF
            END IF
!
            IF ( IND_TAB_FRQ == J1 ) THEN
                 IF ( KEYS(J2,IND_TAB_FRQ)(11:19) == "'CH WIDTH" ) THEN
                      CALL CHIN ( KEYS(J2,IND_TAB_FRQ)(6:7), IND_COL_CHW )
                 END IF
            END IF
            IF ( KEYS(J2,J1)(1:9) == "NAXIS2  =" ) THEN
                 CALL CHIN ( KEYS(J2,J1)(24:30), NAXIS2_VAL )
            END IF
!
            IF ( LSUB > 0 ) THEN
                 IF ( IND_TAB_ANN(LSUB) == J1 ) THEN
                      IF ( KEYS(J2,IND_TAB_ANN(LSUB))(11:19) == "'NOSTA   " ) THEN
                           CALL CHIN ( KEYS(J2,IND_TAB_ANN(LSUB))(6:7), IND_NOSTA(LSUB) )
                      END IF
!
                      IF ( KEYS(J2,IND_TAB_ANN(LSUB))(11:19) == "'ANNAME  " ) THEN
                           CALL CHIN ( KEYS(J2,IND_TAB_ANN(LSUB))(6:7), IND_STANAM(LSUB) )
                      END IF
                      LSTA(LSUB) = NAXIS2_VAL
                 END IF
            END IF
            IF ( IND_TAB_GACO == J1 ) THEN
                 IF ( KEYS(J2,IND_TAB_GACO)(11:20) == "'GAINCORR'" ) THEN
                      CALL CHIN ( KEYS(J2,IND_TAB_GACO)(6:7), IND_GACO_COR  )
                 END IF
                 IF ( KEYS(J2,IND_TAB_GACO)(11:20) == "'GACOERR '" ) THEN
                      CALL CHIN ( KEYS(J2,IND_TAB_GACO)(6:7), IND_GACO_ERR  )
                 END IF
                 IF ( KEYS(J2,IND_TAB_GACO)(11:20) == "'NVIS    '" ) THEN
                      CALL CHIN ( KEYS(J2,IND_TAB_GACO)(6:7), IND_GACO_NUV  )
                 END IF
                 IF ( KEYS(J2,IND_TAB_GACO)(11:20) == "'NUVS    '" ) THEN
                      CALL CHIN ( KEYS(J2,IND_TAB_GACO)(6:7), IND_GACO_NUV  )
                 END IF
            END IF
 420     CONTINUE
 410  CONTINUE
!
      IF ( VIS%NFRQ .LE. 0 ) THEN
           CALL ERR_LOG ( 4637, IUER, 'GET_FITS_VIS', 'Did not find keyword '// &
     &         'NAXIS5 in the input fits file '//FINAM(1:I_LEN(FINAM))// &
     &         ' -- this file does not conform specifications of the '// &
     &         ' fits file with visibility data' )
           RETURN
      END IF
      IF ( VIS%NP .LE. 0 ) THEN
           CALL ERR_LOG ( 4638, IUER, 'GET_FITS_VIS', 'Did not find keyword '// &
     &         'GCOUNT in the input fits file '//FINAM(1:I_LEN(FINAM))// &
     &         ' -- this file does not conform specifications of the '// &
     &         ' fits file with visibility data' )
           RETURN
      END IF
!
! --- Allocate memory for a temporary array
!
      ALLOCATE ( ARR3_R4(3,NPOL,VIS%NFRQ,VIS%NP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 3*4*VIS%NFRQ*VIS%NP, STR )
           CALL ERR_LOG ( 4639, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory' )
           RETURN
      END IF
!
! --- Position to the header where the data reside
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(IND_TAB_FRQ), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4711, IUER, 'GET_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4640, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &         'position to the second table fo the fits file '// &
     &          FINAM )
           RETURN
      END IF
!
! --- Get the frequency offsets
!
      CALL FFGCVD ( %VAL(FPTR), %VAL(IND_COL_FRQ), %VAL(INT8(1)), &
     &              %VAL(INT8(1)), %VAL(INT8(VIS%NFRQ/NCHN)), %VAL(0.0D0), &
     &              SKY_FRQ, ANYF, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 4712, IUER, 'GET_FITS_VIS', FT_STATUS )
           WRITE ( 6, * ) 'IND_COL_FRQ = ', IND_COL_FRQ, &
     &                    ' VIS%NFRQ = ', VIS%NFRQ
           CALL ERR_LOG ( 4641, IUER, 'GET_FITS_VIS', 'Error in an '// &
     &         'attempt to extract the intermediate frequency offsets '// &
     &         'from the fits file '//FINAM )
           RETURN
      END IF
!
      CALL FFGCVE ( %VAL(FPTR), %VAL(IND_COL_CHW), %VAL(INT8(1)), &
     &              %VAL(INT8(1)), %VAL(INT8(VIS%NFRQ/NCHN)), %VAL(0.0), &
     &              CHW_ARR_R4, ANYF, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4713, IUER, 'GET_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4642, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &         'read auxiliary data from the fits file '//FINAM )
           RETURN
      END IF
!
! --- ... and transform them into sky frequencies
!
      KP = 0
      DO 440 J4=1,VIS%NFRQ/NCHN
         DO 450 J5=1,NCHN
            KP = KP + 1
            VIS%SKY_FRQ(KP) = SKY_FRQ(J4) + VIS%FRQ_LO + (J5-1)*CHW_ARR_R4(J4)
 450     CONTINUE 
 440  CONTINUE
!
! --- Position to the header where the data reside
!
      CALL FFMAHD ( %VAL(FPTR), %VAL(1), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4714, IUER, 'GET_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4643, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &         'position to the first table fo the fits file '// &
     &          FINAM )
           RETURN
      END IF
!
! --- Get the visibility data
!
      CALL FFGPVE ( %VAL(FPTR), %VAL(1), %VAL(INT8(1)), &
     &              %VAL(INT8(3*NPOL*VIS%NFRQ*VIS%NP)), %VAL(0.0), ARR3_R4, &
     &              IER, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4715, IUER, 'GET_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4644, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &         'read uv data from the fits file '//FINAM )
           RETURN
      END IF
!
! --- ... and re-arrange them
!
      DO 460 J6=1,VIS%NP
         DO 470 J7=1,VIS%NFRQ
            IF ( NPOL == 1 .OR. FL_RR_POL ) THEN
                 VIS%VIS(J7,J6) = CMPLX ( ARR3_R4(1,1,J7,J6), ARR3_R4(2,1,J7,J6) )
                 VIS%WEI(J7,J6) = ARR3_R4(3,1,J7,J6)
               ELSE IF ( FL_LL_POL ) THEN
                 VIS%VIS(J7,J6) = CMPLX ( ARR3_R4(1,2,J7,J6), ARR3_R4(2,2,J7,J6) )
                 VIS%WEI(J7,J6) = ARR3_R4(3,2,J7,J6)
               ELSE IF ( FL_RL_POL ) THEN
                 VIS%VIS(J7,J6) = CMPLX ( ARR3_R4(1,3,J7,J6), ARR3_R4(2,3,J7,J6) )
                 VIS%WEI(J7,J6) = ARR3_R4(3,3,J7,J6)
               ELSE IF ( FL_LR_POL ) THEN
                 VIS%VIS(J7,J6) = CMPLX ( ARR3_R4(1,4,J7,J6), ARR3_R4(2,4,J7,J6) )
                 VIS%WEI(J7,J6) = ARR3_R4(3,4,J7,J6)
               ELSE 
!
! -------------- I polarization
!
                 VIS%VIS(J7,J6) = CMPLX ( (ARR3_R4(1,1,J7,J6)+ARR3_R4(1,2,J7,J6))/2.0, &
     &                                    (ARR3_R4(2,1,J7,J6)+ARR3_R4(2,2,J7,J6))/2.0  )
                 VIS%WEI(J7,J6) = SQRT  (  ARR3_R4(3,1,J7,J6)**2 + ARR3_R4(3,2,J7,J6)**2 )/2.0
                 IF ( ARR3_R4(3,1,J7,J6) < 0 ) VIS%WEI(J7,J6) = -VIS%WEI(J7,J6) 
            END IF
 470     CONTINUE
 460  CONTINUE
      DEALLOCATE ( ARR3_R4 )
!
      ALLOCATE ( ARR2_R4(PCOUNT,VIS%NP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PCOUNT*4*VIS%NP, STR )
           CALL ERR_LOG ( 4645, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory' )
           RETURN
      END IF
!
! --- Get UV coordinates
!
      CALL FFGGPE ( %VAL(FPTR), %VAL(1), %VAL(1), %VAL(PCOUNT*VIS%NP), &
     &              ARR2_R4, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4716, IUER, 'GET_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4646, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &         'read auxiliary data from the fits file '//FINAM )
           RETURN
      END IF
!
! --- ... and re-arrange them
!
      DO 480 J8=1,VIS%NP
         DO 490 J9=1,VIS%NFRQ
            VIS%UV(1,J9,J8) = ARR2_R4(1,J8)*VIS%SKY_FRQ(J9)*UV_SCALE(1)
            VIS%UV(2,J9,J8) = ARR2_R4(2,J8)*VIS%SKY_FRQ(J9)*UV_SCALE(2)
 490     CONTINUE
         VIS%IND_BAS(J8) = ARR2_R4(IND_BAS,J8)
!
! ------ Extract Julian date
!
         JD = JD0 + JD1 + ARR2_R4(IND_DAT(1),J8) + ARR2_R4(IND_DAT(2),J8)
         CALL JD_TO_MJD_SEC ( JD, VIS%MJD(J8), VIS%TAI(J8) )
         IF ( IND_TIM > 0 ) THEN
              VIS%INT_TIM(J8) = ARR2_R4(IND_TIM,J8)
            ELSE
              VIS%INT_TIM(J8) = 1.0 ! Default
         END IF
 480  CONTINUE
!
      VIS%NSUB = LSUB
      ALLOCATE ( VIS%C_STA(VIS%NSTA) )
      ALLOCATE ( BUF(9*MSTA*VIS%NSUB) )
      ALLOCATE ( DESC(MSTA*VIS%NSUB) )
      ALLOCATE ( VIS%L_STA(VIS%NSUB) )
      VIS%L_STA(1:VIS%NSUB) = LSTA(1:VIS%NSUB) 
!
! --- The first run over AN NAME table and collect station names
!
      VIS%NSTA = 0
      DO 4100 J10=1,VIS%NSUB
!
! ------ Position the file pointer to the Antenna Table
!
         CALL FFMAHD ( %VAL(FPTR), %VAL(IND_TAB_ANN(J10)), HDUTYPE, FT_STATUS )
         IF ( FT_STATUS .NE.0 ) THEN
              CALL FT_PRINTERROR ( 4717, IUER, 'GET_FITS_VIS', FT_STATUS )
              CALL ERR_LOG ( 4647, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &            'position to the ANNAME table fo the fits file '// &
     &             FINAM )
              RETURN
         END IF
!
         DO 4110 J11=1,VIS%L_STA(J10)
            DESC(J11) = LOC(BUF) + (LEN(C_STA(1))+1)*(J11-1)
 4110    CONTINUE
         CALL FFGCVS ( %VAL(FPTR), %VAL(IND_STANAM(J10)), %VAL(1), &
     &                 %VAL(1), %VAL(VIS%L_STA(J10)), %REF('A'//CHAR(0)), &
     &                 DESC, ANYF, FT_STATUS )
         IF ( FT_STATUS .NE.0 ) THEN
              CALL FT_PRINTERROR ( 4718, IUER, 'GET_FITS_VIS', FT_STATUS )
              CALL ERR_LOG ( 4648, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &            'position to the ANNAME table fo the fits file '// &
     &             FINAM )
              RETURN
         END IF
         DO 4120 J12=1,VIS%L_STA(J10)
            CALL CLRCH  ( STA_NAM )
            CALL MEMCPY ( STA_NAM, %VAL(DESC(J12)) ) ! NB hidden 3rd argument
            IP = INDEX ( STA_NAM, CHAR(0) )
            IF ( IP > 0 .AND. IP < LEN(STA_NAM) ) THEN
!
! -------------- Replace trailing zeroes with trailing blanks
!
                 CALL CLRCH ( STA_NAM(IP:) )
            END IF
            IS = ADD_CLIST ( MSTA, VIS%NSTA, C_STA, STA_NAM, -2 )
!!   write ( 6, * ) 'FF j10=' ,int2(j10), ' j12= ', int2(j12), ' vis%nsta= ', vis%nsta ! %%%%
 4120    CONTINUE
 4100 CONTINUE 
!
! --- ... then sort them
!
      CALL SORT_CH ( VIS%NSTA, C_STA )
      ALLOCATE ( VIS%C_STA(VIS%NSTA)  )
      ALLOCATE ( VIS%LIS_STA(VIS%NSTA,VIS%NSUB) )
      VIS%C_STA(1:VIS%NSTA) = C_STA(1:VIS%NSTA)
      CALL NOUT_I4 ( VIS%NSTA*VIS%NSUB, VIS%LIS_STA )
!
! --- The second run over AN NAME table
!
      DO 4130 J13=1,VIS%NSUB
!
! ------ Position the file pointer to the Antenna Table
!
         CALL FFMAHD ( %VAL(FPTR), %VAL(IND_TAB_ANN(J13)), HDUTYPE, FT_STATUS )
         IF ( FT_STATUS .NE.0 ) THEN
              CALL FT_PRINTERROR ( 4719, IUER, 'GET_FITS_VIS', FT_STATUS )
              CALL ERR_LOG ( 4649, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &            'position to the ANNAME table fo the fits file '// &
     &             FINAM )
              RETURN
         END IF
!
         DO 4140 J14=1,VIS%L_STA(J13)
            DESC(J14) = LOC(BUF) + (LEN(C_STA(1))+1)*(J14-1)
 4140    CONTINUE
!
! ------ Again get station names for this subarray
!
         CALL FFGCVS ( %VAL(FPTR), %VAL(IND_STANAM(J13)), %VAL(1), &
     &                 %VAL(1), %VAL(VIS%L_STA(J13)), %REF('A'//CHAR(0)), &
     &                 DESC, ANYF, FT_STATUS )
         IF ( FT_STATUS .NE.0 ) THEN
              CALL FT_PRINTERROR ( 4720, IUER, 'GET_FITS_VIS', FT_STATUS )
              CALL ERR_LOG ( 4650, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &            'position to the ANNAME table fo the fits file '// &
     &             FINAM )
              RETURN
         END IF
!
! ------ Again we create the station list for the J13-th subarray 
! ------ and put it in C_STA
!
         DO 4150 J15=1,VIS%L_STA(J13)
            CALL CLRCH  ( STA_NAM )
            CALL MEMCPY ( STA_NAM, %VAL(DESC(J15)) ) ! NB hidden 3rd argument
            IP = INDEX ( STA_NAM, CHAR(0) )
            IF ( IP > 0 .AND. IP < LEN(STA_NAM) ) THEN
!
! -------------- Replace trailing zeroes with trailing blanks
!
                 CALL CLRCH ( STA_NAM(IP:) )
            END IF
            C_STA(J15) = STA_NAM
 4150    CONTINUE
!
! ------ Get the station indises
!
         CALL FFGCVK ( %VAL(FPTR), %VAL(IND_NOSTA(J13)), %VAL(1), %VAL(1), &
     &                 %VAL(VIS%L_STA(J13)), %VAL(0), STA_IND, ANYF,  &
     &                 FT_STATUS )
         IF ( FT_STATUS .NE.0 ) THEN
              CALL FT_PRINTERROR ( 4721, IUER, 'GET_FITS_VIS', FT_STATUS )
              CALL ERR_LOG ( 4651, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &         'read auxiliary data from the fits file '//FINAM )
              RETURN
         END IF
!
         DO 4160 J16=1,VIS%L_STA(J13)
!
! --------- Important! STA_IND is the index in the station list for 
! --------- the J13-th subarray only
!
            STA_NAM = C_STA(STA_IND(J16))
            VIS%LIS_STA(J16,J13) = LTM_DIF ( 0, VIS%NSTA, VIS%C_STA, STA_NAM )
 4160    CONTINUE 
 4130 CONTINUE 
!!      IF ( IND_TAB_GACO > 0 ) THEN
      IF ( IND_TAB_GACO == -777 ) THEN
!
! -------- GACO (GAin COrrection) section is present
!
           ALLOCATE ( VIS%GACO(VIS%NSTA), STAT=IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 4633, IUER, 'GET_FITS_VIS', 'Error '// &
     &              'in attempt to allocate memory for array VIS%GACO '// &
     &              'when we process fits file '//FINAM )
                RETURN 
           END IF
           DO 4170 J17=1,VIS%NSTA
              ALLOCATE ( VIS%GACO(J17)%GAIN_CORR(VIS%NFRQ), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4634, IUER, 'GET_FITS_VIS', 'Error '// &
     &                 'in attempt to allocate memory for array '// &
     &                 'VIS%GACO(J17)%GAIN_CORR whee we process '// &
     &                 'fits file '//FINAM )
                   RETURN
              END IF
!
              ALLOCATE ( VIS%GACO(J17)%GACO_ERR(VIS%NFRQ), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4635, IUER, 'GET_FITS_VIS', 'Error '// &
     &                 'in attempt to allocate memory for array '// &
     &                 'VIS%GACO(J17)%GACO_ERR whee we process '// &
     &                 'fits file '//FINAM )
                   RETURN
              END IF
!
              ALLOCATE ( VIS%GACO(J17)%NVIS(VIS%NFRQ), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4636, IUER, 'GET_FITS_VIS', 'Error '// &
     &                 'in attempt to allocate memory for array '// &
     &                 'VIS%GACO(J17)%NVIS whee we process '// &
     &                 'fits file '//FINAM )
                   RETURN
              END IF
              VIS%GACO(J17)%GAIN_CORR =  1.0
              VIS%GACO(J17)%GACO_ERR  = -1.0
              VIS%GACO(J17)%NVIS      =  0
 4170      CONTINUE 
           VIS%STATUS_GACO = SMP__ALLC
!
           CALL FFMAHD ( %VAL(FPTR), %VAL(IND_TAB_GACO), HDUTYPE, FT_STATUS )
           CALL FFGCVE ( %VAL(FPTR), %VAL(IND_GACO_COR), %VAL(INT8(1)), &
     &                   %VAL(INT8(1)), %VAL(INT8(VIS%NFRQ*VIS%NSTA)), %VAL(0.0), &
     &                    GAIN_CORR, ANYF, FT_STATUS )
           IF ( FT_STATUS .NE.0 ) THEN
                CALL FT_PRINTERROR ( 4713, IUER, 'GET_FITS_VIS', FT_STATUS )
                CALL ERR_LOG ( 4652, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &              'read GAIN_CORR data from the fits file '//FINAM )
                RETURN
           END IF
           CALL FFGCVE ( %VAL(FPTR), %VAL(IND_GACO_ERR), %VAL(INT8(1)), &
     &                   %VAL(INT8(1)), %VAL(INT8(VIS%NFRQ*VIS%NSTA)), %VAL(0.0), &
     &                    GACO_ERR, ANYF, FT_STATUS )
           IF ( FT_STATUS .NE.0 ) THEN
                CALL FT_PRINTERROR ( 4713, IUER, 'GET_FITS_VIS', FT_STATUS )
                CALL ERR_LOG ( 4653, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &              'read GAIN_CORR data from the fits file '//FINAM )
                RETURN
           END IF
!!              write ( 6, * ) 'IND_GACO_NUV = ', IND_GACO_NUV ! %%%%
           CALL FFGCVK ( %VAL(FPTR), %VAL(IND_GACO_NUV), %VAL(INT8(1)), &
     &                   %VAL(INT8(1)), %VAL(INT8(VIS%NFRQ*VIS%NSTA)), %VAL(0), &
     &                    NVIS_ARR, ANYF, FT_STATUS )
           IF ( FT_STATUS .NE. 0 ) THEN
                CALL FT_PRINTERROR ( 4713, IUER, 'GET_FITS_VIS', FT_STATUS )
                CALL ERR_LOG ( 4654, IUER, 'GET_FITS_VIS', 'Failure to '// &
     &              'read NVIS data from the fits file '//FINAM )
                RETURN
           END IF
           IP = 1
           DO 4180 J18=1,VIS%NSTA
              DO 4190 J19=1,VIS%NFRQ
                 VIS%GACO(J18)%GAIN_CORR(J19) = GAIN_CORR(IP)
                 VIS%GACO(J18)%GACO_ERR(J19)  = GACO_ERR(IP)
                 VIS%GACO(J18)%NVIS(J19)      = NVIS_ARR(IP)
                 IP = IP + 1
 4190         CONTINUE 
 4180      CONTINUE 
           VIS%STATUS_GACO = SMP__LOAD
         ELSE 
!
! -------- Well, we did not find a gain table correction, but let us 
! -------- check history of the first table. 
!
           CALL FFMAHD ( %VAL(FPTR), %VAL(1), HDUTYPE, FT_STATUS )
           DO 4200 J20=1,LKEY(1)
              IF ( KEYS(J20,1)(9:40) == 'Gain correction has been applied' ) THEN
!
! ---------------- GACO (GAin COrrection) section is present in the history section
!
                   ALLOCATE ( VIS%GACO(VIS%NSTA), STAT=IER )
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4655, IUER, 'GET_FITS_VIS', 'Error '// &
     &                       'in attempt to allocate memory for array VIS%GACO '// &
     &                       'when we process fits file '//FINAM )
                        RETURN 
                   END IF
!
! ---------------- Allocate memory for GAin COrrection
!
                   DO 4210 J21=1,VIS%NSTA
                      ALLOCATE ( VIS%GACO(J21)%GAIN_CORR(VIS%NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4656, IUER, 'GET_FITS_VIS', 'Error '// &
     &                         'in attempt to allocate memory for array '// &
     &                         'VIS%GACO(J21)%GAIN_CORR whee we process '// &
     &                         'fits file '//FINAM )
                           RETURN
                      END IF
!
                      ALLOCATE ( VIS%GACO(J21)%GACO_ERR(VIS%NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4657, IUER, 'GET_FITS_VIS', 'Error '// &
     &                         'in attempt to allocate memory for array '// &
     &                         'VIS%GACO(J21)%GACO_ERR whee we process '// &
     &                         'fits file '//FINAM )
                           RETURN
                      END IF
!
                      ALLOCATE ( VIS%GACO(J21)%NVIS(VIS%NFRQ), STAT=IER )
                      IF ( IER .NE. 0 ) THEN
                           CALL ERR_LOG ( 4658, IUER, 'GET_FITS_VIS', 'Error '// &
     &                         'in attempt to allocate memory for array '// &
     &                         'VIS%GACO(21)%NVIS whee we process '// &
     &                         'fits file '//FINAM )
                           RETURN
                     END IF
!
! ------------------ And initialize it, of course
!
                     VIS%GACO(J21)%GAIN_CORR =  1.0
                     VIS%GACO(J21)%GACO_ERR  = -1.0
                     VIS%GACO(J21)%NVIS      =  0
 4210             CONTINUE 
                  VIS%STATUS_GACO = SMP__ALLC
              END IF
!
              IF ( KEYS(J20,1)(1:7) == "HISTORY" )THEN
!
! ---------------- Check for the history keyword
!
                   IF ( KEYS(J20,1)(9:12)  == 'Sta:' .AND. &
     &                  KEYS(J20,1)(32:36) == 'Gain:' .AND. &
     &                  KEYS(J20,1)(62:66) == 'Nvis:'      ) THEN
!
! --------------------- This is the GACO record. Let is parse it.
! --------------------- If we find wrong station or IF, just ignore them
!
                        ISTA = LTM_DIF ( 0, VIS%NSTA, VIS%C_STA, KEYS(J20,1)(14:20) )
                        IF ( ISTA > 0 ) THEN
                             CALL CHIN ( KEYS(J20,1)(28:29), IFRQ )
                             IF ( IFRQ .GE. 0 .AND. IFRQ .LE. VIS%NFRQ ) THEN
                                  READ ( UNIT=KEYS(J20,1)(38:45), FMT='(F8.4)' ) VIS%GACO(ISTA)%GAIN_CORR(IFRQ)
                                  READ ( UNIT=KEYS(J20,1)(53:59), FMT='(F8.4)' ) VIS%GACO(ISTA)%GACO_ERR(IFRQ)
                                  READ ( UNIT=KEYS(J20,1)(68:74), FMT='(I7)'   ) VIS%GACO(ISTA)%NVIS(IFRQ)
                            END IF
                        END IF
                   END IF
              END IF
 4200      CONTINUE 
      END IF
      DEALLOCATE ( BUF  )
      DEALLOCATE ( DESC )
      DEALLOCATE ( ARR2_R4 )
!
      VIS%FINAM  = FINAM
      VIS%STATUS = SMP__LOAD
!
! --- Close the file and release the logical unit
!
      CALL FFITS_CLOSE ( FPTR, -2 )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GET_FITS_VIS  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE VIS_FREE_CHW ( VIS )
! ************************************************************************
! *                                                                      *
! *   Auxilliary program VIS_FREE frees dinamic memory allocated in the  *
! *   fields of the object VIS which keeps visilibities.                 *
! *                                                                      *
! *  ### 08-FEB-2007    VIS_FREE   v1.0 (c)  L. Petrov  08-FEB-2007 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE    'sou_map.i'
      TYPE      ( VIS__TYPE ) :: VIS
      INTEGER*4  J1
!
! --- Initialization
!
      IF ( ASSOCIATED ( VIS%MJD     ) ) DEALLOCATE ( VIS%MJD )
      IF ( ASSOCIATED ( VIS%TAI     ) ) DEALLOCATE ( VIS%TAI )
      IF ( ASSOCIATED ( VIS%VIS     ) ) DEALLOCATE ( VIS%VIS )
      IF ( ASSOCIATED ( VIS%UV      ) ) DEALLOCATE ( VIS%UV  )
      IF ( ASSOCIATED ( VIS%WEI     ) ) DEALLOCATE ( VIS%WEI )
      IF ( ASSOCIATED ( VIS%IND_BAS ) ) DEALLOCATE ( VIS%IND_BAS )
      IF ( ASSOCIATED ( VIS%INT_TIM ) ) DEALLOCATE ( VIS%INT_TIM )
      IF ( ASSOCIATED ( VIS%SKY_FRQ ) ) DEALLOCATE ( VIS%SKY_FRQ )
      IF ( ASSOCIATED ( VIS%C_STA   ) ) DEALLOCATE ( VIS%C_STA   )
      IF ( ASSOCIATED ( VIS%GACO    ) ) THEN
           DO 410 J1=1,VIS%NSTA
              IF ( ASSOCIATED ( VIS%GACO(J1)%GAIN_CORR ) ) THEN
                   DEALLOCATE ( VIS%GACO(J1)%GAIN_CORR )
              END IF
              IF ( ASSOCIATED ( VIS%GACO(J1)%GACO_ERR  ) ) THEN
                   DEALLOCATE ( VIS%GACO(J1)%GACO_ERR  )
              END IF
              IF ( ASSOCIATED ( VIS%GACO(J1)%NVIS      ) ) THEN
                   DEALLOCATE ( VIS%GACO(J1)%NVIS      )
              END IF
 410       CONTINUE 
           DEALLOCATE ( VIS%GACO   )
      END IF
      CALL NOUT ( SIZEOF(VIS), VIS )
      VIS%STATUS  = SMP__UNDF
!
      RETURN
      END  SUBROUTINE  VIS_FREE_CHW  !#!# 
