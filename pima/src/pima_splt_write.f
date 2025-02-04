#include <mk5_preprocessor_directives.inc>
      SUBROUTINE PIMA_SPLT_WRITE ( PIM, LFRQ, KFRQ, L_UVO, UVO, IND_FRQ, &
     &                             FRQ_ARR, AP_LEN, IND_SOU, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PIMA_SPLT_WRITE creates the output file in FITS-format     *
! *   with calibrated uv-data for the selected source.                   *
! *                                                                      *
! * ### 02-APR-2011 PIMA_SPLT_WRITE v3.4 (c)  L. Petrov  09-MAY-2020 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      INCLUDE   'pima_local.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*4  LFRQ, KFRQ, L_UVO, IND_FRQ(4,KFRQ), &
     &           IND_SOU, IUER
      TYPE     ( UVO__TYPE ) :: UVO(L_UVO)
      REAL*8     FRQ_ARR(KFRQ), AP_LEN
      CHARACTER  FITSDIR*128, FILFITS*128, FILTMPL*128, STR*8192
      INTEGER*2  MODE_MKDIR, TFLOAT__FITSIO, N_GRP
      DATA       MODE_MKDIR  / O'00755' /
      PARAMETER  ( TFLOAT__FITSIO = 42 )
      PARAMETER  ( N_GRP = 7 )
      ADDRESS__TYPE :: DIR_DESC, IP, DESC(PIM__MSTA)
      LOGICAL*1  LEX
      INTEGER*8  FPTR
      REAL*8     IF_FRQ(PIM__MFRQ*PIM__MCHN), STA_COO_ARR(3,PIM__MSTA), &
     &           SEC, TIM_DAYS
      REAL*8     T1(8192), X1(8192), X2(8192), X3(8192)
      REAL*4     CH_WIDTH(PIM__MFRQ*PIM__MCHN), &
     &           TOT_BDWIDTH(PIM__MFRQ*PIM__MCHN)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, J10, J11, IL, ID, IS, &
     &           M_POL, NHDTYPE, FT_STATUS, NOSTA(PIM__MSTA), &
     &           REF_STA(PIM__MSTA), L_STA, SDB_ARR(PIM__MFRQ*PIM__MCHN), &
     &           IFRQ_SEL, MJD_NEW_YEAR, IDAY, ISTA, IND_STA, STA_IND(2), &
     &           KP, KTIM, ISUB, UTIM, IER
      CHARACTER  PIMA_EXE_NAME*128
      LOGICAL*1  FL_STA(PIM__MSTA)
      TYPE     ( FITS_PRIM__STRU ), POINTER :: TABL(:)
      INTEGER*4, EXTERNAL     :: ILEN, I_LEN, MKDIR, UNLINK, LINDEX, IFIND_PL
      ADDRESS__TYPE, EXTERNAL :: FUNC_OPENDIR, CLOSEDIR
      CHARACTER,     EXTERNAL :: MJDSEC_TO_DATE*30
      REAL*4     ATAN_CS_R4, PHAS_CMPL_R4
#ifdef GNU
      INTEGER*4, EXTERNAL :: PIMA_COMPAR_TABL
#else
      INTEGER*2, EXTERNAL :: PIMA_COMPAR_TABL
#endif
!
! --- Build names of output directory
!
      CALL GETENVAR ( 'PIMAVAR_FITS_DIR', FITSDIR )
      IF ( ILEN(FITSDIR) == 0 ) THEN
           FITSDIR =  PIM%CONF%EXPER_DIR(1:I_LEN(PIM%CONF%EXPER_DIR))// &
     &                '/'//PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))// &
     &                '_uvs'
      END IF
      FILFITS = FITSDIR(1:I_LEN(FITSDIR))//'/'// &
     &          PIM%SOU(IND_SOU)%J2000_NAME//'_'// &
     &          PIM%CONF%BAND//'_uva.fits'
!
! --- Build the name of template file
!
      CALL ERR_PASS ( IUER, IER )
      CALL GET_MY_EXE_PATH ( PIMA_EXE_NAME, IER )
      ID = LINDEX ( PIMA_EXE_NAME, '/' )
      IF ( ID < 4 ) THEN
           CALL ERR_LOG ( 8411, IUER, 'PIMA_SPLT_WRITE', 'Trap of internal '// &
     &         'control: too short pima exec name: '//PIMA_EXE_NAME )
           RETURN 
         ELSE 
           FILTMPL = PIMA_EXE_NAME(1:ID-4)//'share/pima/uva.tmpl' 
      END IF
!
! --- Check whether the FITSDIR exists
!
      DIR_DESC = FUNC_OPENDIR ( FITSDIR(1:I_LEN(FITSDIR))//CHAR(0) )
      IF ( DIR_DESC == 0 ) THEN
!
! -------- Does not exist? Let us create it
!
           IS = MKDIR ( FITSDIR(1:I_LEN(FITSDIR))//CHAR(0), &
     &                  %VAL(MODE_MKDIR) )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 8412, IUER, 'PIMA_SPLT_WRITE', 'Failure '// &
     &              'in attempt to create the directory for output '// &
     &              'uv data '//FITSDIR(1:I_LEN(FITSDIR))//' -- '// &
     &              STR )
                RETURN 
           END IF
         ELSE 
           IP = CLOSEDIR ( %VAL(DIR_DESC) )
      END IF
!
! --- Check whether the output FITS file exists
!
      INQUIRE ( FILE=FILFITS, EXIST=LEX )
      IF ( LEX ) THEN
!
! -------- Exists? Let us delete it
!
           IS = UNLINK ( FILFITS(1:I_LEN(FILFITS))//CHAR(0) )
           IF ( IS .NE. 0 ) THEN
                CALL GERROR ( STR )
                CALL ERR_LOG ( 8413, IUER, 'PIMA_SPLT_WRITE', 'Failure '// &
     &              'in attempt to delete existing file '// &
     &               FILFITS(1:I_LEN(FILFITS))//' -- '//STR )
                RETURN 
           END IF
      END IF
!
      IF ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_I   .OR. &
     &     PIM%CONF%SPLT_POLAR == PIMA__POLAR_ALL      ) THEN
           M_POL  = 4
         ELSE 
           M_POL  = 1
      END IF
!
! --- Allocate dynamic memory for temporary arrays
!
      ALLOCATE ( TABL(L_UVO), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8414, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to allocate dynamic memory for the data '// &
     &         'structure TABL' )
           RETURN
      END IF 
!
      FL_STA = .FALSE.
      UTIM = 0
      DO 410 J1=1,L_UVO
         IF ( UVO(J1)%SNR_TOT < PIM%CONF%SPLT_SNR_MIN ) GOTO 410
         STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + UVO(J1)%TIM + PIM%UTC_MTAI, -2 )
         IF ( PIM%CONF%SPLT_STA_BASED .EQ. PIMA__STA_BASED_YES ) THEN
              IF ( UVO(J1)%IND_SUB == 0 ) GOTO 410
         END IF
         UTIM = UTIM + 1
         ALLOCATE ( TABL(UTIM)%GRP_ARR(N_GRP),  STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8415, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to allocate dynamic memory for '// &
     &            'an element GRP_ARR in the data structure TABL' )
              RETURN
         END IF
!
         ALLOCATE ( TABL(UTIM)%UV_DATA(3,M_POL,KFRQ), STAT=IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 8416, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to allocate dynamic memory for '// &
     &            'an element UV_DATA in the data structure TABL' )
              RETURN
         END IF
         CALL NOUT ( SIZEOF(TABL(UTIM)%UV_DATA), TABL(UTIM)%UV_DATA )
!
         FL_STA(UVO(J1)%STA_IND(1)) = .TRUE.
         FL_STA(UVO(J1)%STA_IND(2)) = .TRUE.
 410  CONTINUE 
!
      L_STA = 0
      REF_STA = 0
      DO 420 J2=1,PIM%NSTA
         IF ( FL_STA(J2) ) THEN
              L_STA = L_STA + 1
              REF_STA(J2) = L_STA
         END IF
 420  CONTINUE 
!
! --- Create the output FITS-file
!
      FT_STATUS = 0
      FPTR = 0
      CALL FFINIT ( FPTR, FILFITS(1:I_LEN(FILFITS))//CHAR(0), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8417, IER, 'FFINIT', FT_STATUS )
           CALL ERR_LOG ( 8417, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'an attempt to open output fits-file '//FILFITS )
           RETURN 
      END IF
!
      STR = MJDSEC_TO_DATE ( PIM%MJD_0, 1.D-6, IER )
      STR = STR(1:4)//'.01.01_00:00:00.0'
      CALL DATE_TO_TIME ( STR(1:21), MJD_NEW_YEAR, SEC, IER )
!
! --- Populate so-called group prameters and put them as well as 
! --- output UV data into fields of data structure TABL
!
      UTIM = 0
      DO 430 J3=1,L_UVO
         IF ( UVO(J3)%SNR_TOT < PIM%CONF%SPLT_SNR_MIN ) GOTO 430
!
         IF ( PIM%CONF%SPLT_STA_BASED .EQ. PIMA__STA_BASED_YES ) THEN
              IF ( UVO(J3)%IND_SUB == 0 ) GOTO 430
         END IF
         STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + UVO(J3)%TIM + PIM%UTC_MTAI, -2 )
         UTIM = UTIM + 1
!
! ------ NB: we subtract one day in order to conform to AIPS convention
!
         TIM_DAYS = (PIM%TAI_0 + UVO(J3)%TIM + PIM%UTC_MTAI)/86400.0D0
         TABL(UTIM)%GRP_ARR(1) = UVO(J3)%UVW(1)
         TABL(UTIM)%GRP_ARR(2) = UVO(J3)%UVW(2)
         TABL(UTIM)%GRP_ARR(3) = UVO(J3)%UVW(3)
!
         STA_IND(1) = IFIND_PL ( PIM%SUB%L_STA(UVO(J3)%IND_SUB), &
     &                           PIM%SUB%LIS_STA(1,UVO(J3)%IND_SUB), &
     &                           UVO(J3)%STA_IND(1) )
         STA_IND(2) = IFIND_PL ( PIM%SUB%L_STA(UVO(J3)%IND_SUB), &
     &                           PIM%SUB%LIS_STA(1,UVO(J3)%IND_SUB), &
     &                           UVO(J3)%STA_IND(2) )
!
         TABL(UTIM)%GRP_ARR(4) = 256*STA_IND(1) + STA_IND(2) + (UVO(J3)%IND_SUB-1)*0.01
!
         TABL(UTIM)%GRP_ARR(5) = IDINT ( TIM_DAYS )
         TABL(UTIM)%GRP_ARR(6) = TIM_DAYS - TABL(UTIM)%GRP_ARR(5)
         TABL(UTIM)%GRP_ARR(7) = AP_LEN*PIM%CONF%SPLT_TIM_MSEG 
!
         DO 440 J4=1,KFRQ
            DO 450 J5=1,M_POL
               TABL(UTIM)%UV_DATA(1,J5,J4) = REAL ( UVO(J3)%SPE(J4,J5) )
               TABL(UTIM)%UV_DATA(2,J5,J4) = IMAG ( UVO(J3)%SPE(J4,J5) )
               TABL(UTIM)%UV_DATA(3,J5,J4) = UVO(J3)%WEI(J4,J5)
               IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
                    WRITE  ( 6, 210 ) J3, J5, J4, UVO(J3)%STA_IND, &
     &                                ABS(UVO(J3)%SPE(J4,J5)), &
     &                                PHAS_CMPL_R4 ( UVO(J3)%SPE(J4,J5) ), &
     &                                UVO(J3)%WEI(J4,J5), &
     &                                UVO(J3)%IND_SUB, STA_IND, &
     &                                PIM%TAI_0 + UVO(J3)%TIM, UVO(J3)%IND_OBS
 210                FORMAT ( 'UVO: ', I6, ' Pol: ', I1,' Ifrq: ', I2, &
     &                       ' Ind_sta: ', I2, 1X, I2, ' Vis_Amp: ', F9.6, &
     &                       ' Vis_Phs: ', F9.6, ' Vis_Wei: ', 1PD12.6, &
     &                       ' Sa: ', I2, ' Sta_ind: ', I2, 1X, I2, &
     &                       ' Tai: ', F9.2, ' Ind_obs: ', I4   )
               END IF
               IF ( STA_IND(1) < 1 .OR. STA_IND(2) < 1 ) THEN
                    WRITE ( 6, * ) 'PSW-224 STA_IND: ', UVO(J3)%STA_IND(1), UVO(J3)%STA_IND(2)  
                    WRITE ( 6, * ) 'PSW-225 L_STA:   ', PIM%SUB%L_STA(UVO(J3)%IND_SUB), ' IND_SUB= ', PIM%SUB%L_STA(UVO(J3)%IND_SUB)
                    WRITE ( 6, * ) 'PSW-226 LIS_STA: ', PIM%SUB%LIS_STA(1:PIM%SUB%L_STA(UVO(J3)%IND_SUB),UVO(J3)%IND_SUB)
               END IF
 450       CONTINUE 
 440     CONTINUE 
 430  CONTINUE 
!
! --- Sort the data structure TABL first over time, then over baseline index
!
      CALL FOR_QSORT ( TABL, UTIM, SIZEOF(TABL(1)), PIMA_COMPAR_TABL )
!
! --- Put there ksys
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL FFITS_PUT_KEYS ( PIM, FPTR, FILTMPL, UTIM, KFRQ, M_POL, &
     &                      FRQ_ARR, IND_SOU, L_STA, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8418, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to put keys in the output file '//FILFITS )
           RETURN
      END IF
!
! --- Write random group parameters into the first table
!
      DO 460 J6=1,UTIM
         IF ( PIM%CONF%DEBUG_LEVEL .GE. 6 ) THEN
              ISUB = NINT ( 100*TABL(J6)%GRP_ARR(4) - 100.0*INT(TABL(J6)%GRP_ARR(4)) ) + 1 
              STA_IND(1) = NINT ( TABL(J6)%GRP_ARR(4)/256 )
              STA_IND(2) = NINT ( TABL(J6)%GRP_ARR(4) - (ISUB-1)*0.01 - 256*STA_IND(1) )
!              
              WRITE  ( 6, 220 ) J6, STA_IND, ISUB, TABL(J6)%GRP_ARR(6)*86400.0D0, &
     &                          PIM%C_STA(PIM%SUB%LIS_STA(STA_IND(1),ISUB)), &
     &                          PIM%C_STA(PIM%SUB%LIS_STA(STA_IND(2),ISUB)) 
 220          FORMAT ( 'TABL: ', I6, ' Sta_ind: ', I2, 1X, I2, &
     &                 ' Sa: ', I2, ' Tai: ', F9.2, &
     &                 ' Sta: ', A, ' / ', A  )
         END IF 
!
! ------ Write down group parameters
!
         CALL FFPGPE ( %VAL(FPTR), %VAL(J6), %VAL(1), %VAL(N_GRP), &
     &                 TABL(J6)%GRP_ARR, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8419, IER, 'FFPGPE', FT_STATUS )
              CALL ERR_LOG ( 8419, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to write the uv-data in the output fits-file '// &
     &             FILFITS )
              RETURN 
         END IF
!
! ------ Write down UV data
!
         CALL FFPPRE ( %VAL(FPTR), %VAL(J6), %VAL(INT8(1)), &
     &                 %VAL(INT8(3*M_POL*KFRQ)), TABL(J6)%UV_DATA, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8420, IER, 'FFPPRE', FT_STATUS )
              CALL ERR_LOG ( 8420, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to write the uv-data in the output fits-file '// &
     &             FILFITS )
              RETURN 
         END IF
 460  CONTINUE 
!
! --- Create arrays of IF, Channel widths, total bandwidth and 
! --- sideband signs
!
      DO 470 J7=1,KFRQ
         IF_FRQ(J7) = FRQ_ARR(J7) - FRQ_ARR(1)
         CH_WIDTH(J7) = PIM%FRQ(IND_FRQ(3,J7),PIM%CONF%FRQ_GRP)%CHAN_WIDTH* &
     &                            PIM%CONF%SPLT_FRQ_MSEG
         TOT_BDWIDTH(J7) = CH_WIDTH(J7) 
!@         SDB_ARR(J7) = PIM%FRQ(IND_FRQ(3,J7),PIM%CONF%FRQ_GRP)%SIDE_BAND
!@         IF ( SDB_ARR(J7) == -1 ) THEN
!@              IF_FRQ(J7) = IF_FRQ(J7) - CH_WIDTH(J7) 
!@         END IF
         SDB_ARR(J7) = 1
 470  CONTINUE 
      IFRQ_SEL = 1
!
! --- Move to the 2nd (frequency) table in the output FITS file
!
      CALL FFMAHD ( %VAL(FPTR), %VAL(2), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8421, IER, 'FFMAHD', FT_STATUS )
           CALL ERR_LOG ( 8421, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to move to the 2nd table' )
           RETURN
      END IF
!
! --- Write frequency related information into FQ table
!
      CALL FFPCLK ( %VAL(FPTR), %VAL(1), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(1)), IFRQ_SEL, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8422, IER, 'FFPCLK', FT_STATUS )
           CALL ERR_LOG ( 8422, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to the station index array in the output file '// &
     &          FILFITS )
           RETURN
      END IF
!
      CALL FFPCLD ( %VAL(FPTR), %VAL(2), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(KFRQ)), IF_FRQ, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8423, IER, 'FFPCLD', FT_STATUS )
           CALL ERR_LOG ( 8423, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to write the array of IF frequencies into '// &
     &         'the output file '//FILFITS )
           RETURN
      END IF
!
      CALL FFPCLE ( %VAL(FPTR), %VAL(3), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(KFRQ)), CH_WIDTH, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8424, IER, 'FFPCLE', FT_STATUS )
           CALL ERR_LOG ( 8424, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to write the array of IF frequencies into '// &
     &         'the output file '//FILFITS )
           RETURN
      END IF
!
      CALL FFPCLE ( %VAL(FPTR), %VAL(4), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(KFRQ)), TOT_BDWIDTH, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8425, IER, 'FFPCLE', FT_STATUS )
           CALL ERR_LOG ( 8425, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to write the array of IF frequencies into '// &
     &         'the output file '//FILFITS )
           RETURN
      END IF
! 
      CALL FFPCLK ( %VAL(FPTR), %VAL(5), %VAL(INT8(1)), %VAL(INT8(1)), &
     &              %VAL(INT8(KFRQ)), SDB_ARR, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8426, IER, 'FFPCLK', FT_STATUS )
           CALL ERR_LOG ( 8426, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to write the array of IF frequencies into '// &
     &         'the output file '//FILFITS )
           RETURN
      END IF
!
! --- Move to the 3rd (antenna) table
!
      DO 480 J8=1,PIM%L_SUB
         CALL FFMAHD ( %VAL(FPTR), %VAL(2+J8), NHDTYPE, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8427, IER, 'FFMAHD', FT_STATUS )
              CALL ERR_LOG ( 8427, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to move the 3rd table' )
              RETURN
         END IF
!
! ------ Populate intermediate arrays to be written in the antetnna table
!
         CALL CLRCH ( STR )
         IL = 1
         DO 490 J9=1,PIM%SUB%L_STA(J8)
            IND_STA = PIM%SUB%LIS_STA(J9,J8)
            ID = INDEX ( PIM%STA(IND_STA)%ORIG_NAME(1:8), CHAR(0) )
            IF ( ID > 0 ) CALL REPEAT ( CHAR(0), LEN(PIM%STA(IND_STA)%ORIG_NAME(1:8))-ID+1, &
     &                                               PIM%STA(IND_STA)%ORIG_NAME(ID:) )
!
            STR(IL:IL+9) = PIM%STA(IND_STA)%ORIG_NAME(1:8)//CHAR(0)
            DESC(J9) = LOC(STR(IL:IL))
            NOSTA(J9) = J9
            STA_COO_ARR(1,J9) = PIM%STA(IND_STA)%COO(1)
            STA_COO_ARR(2,J9) = PIM%STA(IND_STA)%COO(2)
            STA_COO_ARR(3,J9) = PIM%STA(IND_STA)%COO(3)
            IL = IL + 9
 490     CONTINUE 
!
! ------ Write information into some (not all!) fields of the antenna table
!
         CALL FFPCLS ( %VAL(FPTR), %VAL(1), %VAL(INT8(1)), &
     &                 %VAL(INT8(1)), %VAL(INT8(PIM%SUB%L_STA(J8))), &
     &                 DESC, FT_STATUS  )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8428, IER, 'FFPCLS', FT_STATUS )
              CALL ERR_LOG ( 8428, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to write the station name array into '// &
     &            'the output file '//FILFITS )
              RETURN
         END IF
!
         CALL FFPCLD ( %VAL(FPTR), %VAL(2), %VAL(INT8(1)), %VAL(INT8(1)), &
     &                 %VAL(INT8(3*PIM%SUB%L_STA(J8))), STA_COO_ARR, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8429, IER, 'FFPCLD', FT_STATUS )
              CALL ERR_LOG ( 8429, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to write the array of station coordinates into '// &
     &            'the output file '//FILFITS )
              RETURN
         END IF
!
         CALL FFPCLK ( %VAL(FPTR), %VAL(4), %VAL(INT8(1)), %VAL(INT8(1)), &
     &                 %VAL(INT8(PIM%SUB%L_STA(J8))), NOSTA, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8430, IER, 'FFPCLK', FT_STATUS )
              CALL ERR_LOG ( 8430, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to the station index array in the output file '// &
     &             FILFITS )
              RETURN
         END IF
 480  CONTINUE 
      CALL FFMAHD ( %VAL(FPTR), %VAL(3+PIM%L_SUB), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8431, IER, 'FFMAHD', FT_STATUS )
           CALL ERR_LOG ( 8431, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to move the last table' )
           RETURN
      END IF
!
      IL = 1
      DO 4100 J10=1,PIM%NSTA
         STR(IL:IL+9) = PIM%STA(J10)%ORIG_NAME(1:8)//CHAR(0)
         DESC(J10) = LOC(STR(IL:IL))
         IL = IL + 9
!
         CALL FFPCLS ( %VAL(FPTR), %VAL(1), %VAL(INT8(J10)), &
     &                 %VAL(INT8(1)), %VAL(INT8(1)), &
     &                 DESC(J10), FT_STATUS  )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8432, IER, 'FFPCLS', FT_STATUS )
              CALL ERR_LOG ( 8432, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to write the station name array into '// &
     &            'the output file '//FILFITS )
              RETURN
         END IF
!
         CALL FFPCLE ( %VAL(FPTR), %VAL(2), %VAL(INT8(J10)), %VAL(INT8(1)), &
     &                 %VAL(INT8(KFRQ)), PIM%GACO(J10)%GAIN_CORR, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8433, IER, 'FFPCLE', FT_STATUS )
              CALL ERR_LOG ( 8433, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to write the array of gain corrections into '// &
     &            'the output file '//FILFITS )
             RETURN
         END IF
!
         CALL FFPCLE ( %VAL(FPTR), %VAL(3), %VAL(INT8(J10)), %VAL(INT8(1)), &
     &                 %VAL(INT8(KFRQ)), PIM%GACO(J10)%GACO_ERR, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8434, IER, 'FFPCLE', FT_STATUS )
              CALL ERR_LOG ( 8434, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to write the array of gain corrections into '// &
     &            'the output file '//FILFITS )
              RETURN
         END IF
! 
         CALL FFPCLK ( %VAL(FPTR), %VAL(4), %VAL(INT8(J10)), %VAL(INT8(1)), &
     &                 %VAL(INT8(KFRQ)), PIM%GACO(J10)%NVIS, FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL ERR_PASS ( IUER, IER )
              CALL FT_PRINTERROR ( 8435, IER, 'FFPCLK', FT_STATUS )
              CALL ERR_LOG ( 8435, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &            'attempt to write the array of teh number of used '// &
     &            'visibilities into the output file '//FILFITS )
              RETURN
         END IF
 4100 CONTINUE 
!
! --- Well, that is it. Close the FITS file
!
      FT_STATUS = 0
      CALL FFCLOS ( %VAL(FPTR), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 8436, IER, 'FFCLOS', FT_STATUS )
           CALL ERR_LOG ( 8436, IUER, 'PIMA_SPLT_WRITE', 'Error in '// &
     &         'attempt to close the output file '//FILFITS )
           RETURN
      END IF
!
! --- Deallocate fields of array TABL
!
      DO 4110 J11=1,UTIM
         DEALLOCATE ( TABL(J11)%GRP_ARR )
         DEALLOCATE ( TABL(J11)%UV_DATA )
 4110 CONTINUE 
      DEALLOCATE ( TABL )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PIMA_SPLT_WRITE  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE FFITS_PUT_KEYS ( PIM, FPTR, FILTMPL, LTIM, KFRQ, LPOL, &
     &                            FRQ_ARR, IND_SOU, L_STA, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  FFITS_PUT_KEYS
! *                                                                      *
! * ### 24-FEB-2011  FFITS_PUT_KEYS  v1.7 (c)  L. Petrov 28-FEB-2016 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'pima.i'
      TYPE     ( PIMA__TYPE  ) :: PIM
      INTEGER*8  FPTR
      CHARACTER  FILTMPL*(*)
      INTEGER*4  LTIM, KFRQ, LPOL, IND_SOU, L_STA, IUER
      REAL*8     FRQ_ARR(KFRQ)
      INTEGER*4    MTMP
      PARAMETER  ( MTMP = 8192 )
      CHARACTER  BUF(MTMP)*80, STR*128
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, NTMP, NHDTYPE, IB, IE, IND_SUB, &
     &           FT_STATUS, MJD_NEW_YEAR, IND_BEG_3TAB, IND_BEG_4TAB, &
     &           IND_END_3TAB, IND_END_4TAB, ITMP, IER
      REAL*8     SEC
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Read template file
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILTMPL, MTMP, BUF, NTMP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 8451, IUER, 'FFITS_PUT_KEYS', 'Error in reading '// &
     &         'template file '//FILTMPL )
           RETURN 
      END IF
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(1), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 8452, IUER, 'FFMAHD', FT_STATUS )
           CALL ERR_LOG ( 8452, IUER, 'FFITS_PUT_KEYS', 'Error in seting '// &
     &         'the table counter to the first table' )
           RETURN 
      END IF      
!
      IND_BEG_3TAB = 0
      IND_END_3TAB = 0
      IND_BEG_4TAB = 0
      IND_END_4TAB = 0
      DO 410 J1=1,NTMP
         IF ( BUF(J1)(1:12) ==  ' 3 XTENSION='     ) IND_BEG_3TAB = J1
         IF ( BUF(J1)(1:16) ==  '# End of Table 3' ) IND_END_3TAB = J1
         IF ( BUF(J1)(1:12) ==  ' 4 XTENSION='     ) IND_BEG_4TAB = J1
         IF ( BUF(J1)(1:16) ==  '# End of Table 4' ) IND_END_4TAB = J1
 410  CONTINUE 
!
      IF ( IND_BEG_3TAB == 0 .OR. IND_END_3TAB == 0 ) THEN
           CALL ERR_LOG ( 8453, IUER, 'FFITS_PUT_KEYS', 'Trap of internal '// &
     &         'control: broken defitions of table 3 in template file '// &
     &          FILTMPL ) 
           RETURN 
      END IF
!
      IF ( IND_BEG_4TAB == 0 .OR. IND_END_4TAB == 0  ) THEN
           CALL ERR_LOG ( 8454, IUER, 'FFITS_PUT_KEYS', 'Trap of internal '// &
     &         'control: broken defitions of table 4 in template file '// &
     &          FILTMPL ) 
           RETURN 
      END IF
      IF ( PIM%L_SUB > 1 ) THEN
!
! -------- If there are subarrays, we need to expand the table
! -------- First, copy the definitions of the 4th table
!
           DO 420 J2=IND_BEG_4TAB,IND_END_4TAB
              BUF(J2 + (PIM%L_SUB-1)*(IND_END_3TAB - IND_BEG_3TAB+1)) = BUF(J2)
 420       CONTINUE 
!
! -------- And then multiplye the 3rd table
!
           DO 430 J3=2,PIM%L_SUB
              DO 440 J4=IND_BEG_3TAB,IND_END_3TAB
                 ITMP = IND_BEG_3TAB + (J3-1)*(IND_END_3TAB - IND_BEG_3TAB + 1) + J4-IND_BEG_3TAB
                 BUF(ITMP) = BUF(J4)
                 IF ( BUF(ITMP)(1:12) == ' 3 EXTVER  =' ) THEN
                      BUF(ITMP)(32:33) = '  '
                      CALL INCH   ( J3, BUF(ITMP)(32:33) )
                      CALL CHASHR (     BUF(ITMP)(32:33) )
                 END IF
 440          CONTINUE 
 430       CONTINUE 
           NTMP = NTMP + (PIM%L_SUB-1)*(IND_END_3TAB - IND_BEG_3TAB+1)
      END IF
!
! --- Now scan the contents of the template file and modify
! --- some fields in accordance with information from PIM structure
! --- that is specific for this experiment
!
      DO 450 J5=1,NTMP
         IF ( BUF(J5)(1:32) == '# End of Table 1 ---------------' ) THEN
              CALL FFPHIS ( %VAL(FPTR), 'Generated on '//GET_CDATE()//CHAR(0), &
     &                      FT_STATUS )
              CALL CLRCH ( STR )
              CALL INCH  ( PIM%L_SUB, STR )
              CALL FFPHIS ( %VAL(FPTR), 'Number of subarrays: '// &
     &                      STR(1:I_LEN(STR))//CHAR(0), FT_STATUS )
              IF ( PIM%GACO_STATUS == PIMA__LOADED ) THEN
                   CALL FFPHIS ( %VAL(FPTR), 'Gain correction has been applied'//CHAR(0), &
     &                           FT_STATUS )
                   DO 460 J6=1,PIM%NSTA
                      CALL FFPHIS ( %VAL(FPTR), 'Gain correction: '//CHAR(0), FT_STATUS )
                      DO 470 J7=1,PIM%GACO(J6)%NFRQ
                         WRITE ( STR, 110 ) PIM%STA(J6)%ORIG_NAME(1:8), J7, &
     &                             PIM%GACO(J6)%GAIN_CORR(J7), PIM%GACO(J6)%GACO_ERR(J7), &
     &                             PIM%GACO(J6)%NVIS(J7) 
 110                     FORMAT ( 'Sta: ', A, '  IF: ', I2,  &
     &                            '  Gain: ', F8.4, '  Err: ', F7.4, '  Nvis: ', I7 )
                         CALL FFPHIS ( %VAL(FPTR), STR(1:I_LEN(STR))//CHAR(0), FT_STATUS )
 470                  CONTINUE 
 460               CONTINUE 
                   CALL FFPHIS ( %VAL(FPTR), 'End of gain table'//CHAR(0), FT_STATUS )
                 ELSE 
                   CALL FFPHIS ( %VAL(FPTR), 'No Gain correction was applied'//CHAR(0), &
     &                           FT_STATUS )
              END IF
         END IF
         IF ( BUF(J5)(1:1) == '#' ) GOTO 450
         FT_STATUS = 0
         IF ( INDEX ( BUF(J5), '@GCOUNT@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5), '@GCOUNT@' ) 
              IE = IB + LEN('@GCOUNT@') - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL INCH   ( LTIM, BUF(J5)(IB:IE) )
              CALL CHASHR (       BUF(J5)(IB:IE) )
            ELSE IF ( INDEX ( BUF(J5), 'NAXIS2' ) > 0 .AND. &
     &                BUF(J5)(1:2) == ' 2' ) THEN
              CALL INCH ( 1, BUF(J5)(32:33) )
              CALL CHASHR (  BUF(J5)(32:33) )
            ELSE IF ( INDEX ( BUF(J5), '@STOKES_PIXEL__@' ) > 0 ) THEN
              IF ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_I ) THEN
                   BUF(J5)(18:33) = '-1.0000000000000'
                 ELSE IF ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_LL ) THEN
                   BUF(J5)(18:33) = ' 0.0000000000000'
                 ELSE IF ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_RL ) THEN
                   BUF(J5)(18:33) = '-1.0000000000000'
                 ELSE IF ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_LR ) THEN
                   BUF(J5)(18:33) = '-2.0000000000000'
                 ELSE 
                   BUF(J5)(18:33) = ' 1.0000000000000'
              END IF
            ELSE IF ( INDEX ( BUF(J5), '@STOKES_DELTA__@' ) > 0 ) THEN
              IF ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_PAR ) THEN
                   BUF(J5)(18:33) = '-1.0000000000000'
                 ELSE IF ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_ALL ) THEN
                   BUF(J5)(18:33) = '-1.0000000000000'
                 ELSE IF ( PIM%CONF%SPLT_POLAR == PIMA__POLAR_I ) THEN
                   BUF(J5)(18:33) = ' 1.0000000000000'
                 ELSE 
                   BUF(J5)(18:33) = '-1.0000000000000'
              END IF
            ELSE IF ( INDEX ( BUF(J5), '@PIMA@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@PIMA@' ) 
              IE = IB +            LEN('@PIMA@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              BUF(J5)(IB:42) = "'"//PIMA__LABEL//"'"
            ELSE IF ( INDEX ( BUF(J5), '@OBSDATE@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@OBSDATE@' ) 
              IE = IB +            LEN('@OBSDATE@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              STR = MJDSEC_TO_DATE ( PIM%MJD_0, 1.D-6, IER )
              STR(5:5) = '-'
              STR(8:8) = '-'
              BUF(J5)(IB:42) = "'"//STR(1:10)//"'"
            ELSE IF ( INDEX ( BUF(J5), '@INSTRUMENT@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@INSTRUMENT@' ) 
              IE = IB +            LEN('@INSTRUMENT@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              BUF(J5)(IB:42) = "'"//PIM%GENERATOR(1:I_LEN(PIM%GENERATOR))//"'"
            ELSE IF ( INDEX ( BUF(J5), '@OBSERVER@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@OBSERVER@' ) 
              IE = IB +            LEN('@OBSERVER@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              BUF(J5)(IB:42) = "'"//PIM%CONF%SESS_CODE(1:I_LEN(PIM%CONF%SESS_CODE))//"'"
            ELSE IF ( INDEX ( BUF(J5), '@OBJECT@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@OBJECT@' ) 
              IE = IB +            LEN('@OBJECT@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              BUF(J5)(IB:42) = "'"//PIM%SOU(IND_SOU)%J2000_NAME//"'"
            ELSE IF ( INDEX ( BUF(J5), '@RA_DEG       @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@RA_DEG       @' ) 
              IE = IB +            LEN('@RA_DEG       @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              WRITE ( UNIT=STR(1:15), FMT='(F15.11)' ) PIM%SOU(IND_SOU)%ALPHA/DEG__TO__RAD
              BUF(J5)(IB:42) = STR(1:15)
            ELSE IF ( INDEX ( BUF(J5), '@DEC_DEG      @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@DEC_DEG      @' ) 
              IE = IB +            LEN('@DEC_DEG      @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              WRITE ( UNIT=STR(1:15), FMT='(F15.11)' ) PIM%SOU(IND_SOU)%DELTA/DEG__TO__RAD
              CALL CHASHL ( STR(1:15) )
              IF ( PIM%SOU(IND_SOU)%DELTA .GE. 0.0D0 .AND. &
     &             PIM%SOU(IND_SOU)%DELTA < 10.0D0*DEG__TO__RAD ) THEN
                   STR = ' +'//STR
                 ELSE IF ( PIM%SOU(IND_SOU)%DELTA .GE. 10.0D0*DEG__TO__RAD  ) THEN
                   STR = '+'//STR
                 ELSE IF ( PIM%SOU(IND_SOU)%DELTA .GE. -10.0D0*DEG__TO__RAD  .AND. &
     &                     PIM%SOU(IND_SOU)%DELTA < 0.0D0            ) THEN
                   STR = ' '//STR
              END IF
              BUF(J5)(IB:42) = STR(1:15)
            ELSE IF ( INDEX ( BUF(J5), '@OBS_RA_DEG   @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@OBS_RA_DEG   @' ) 
              IE = IB +            LEN('@OBS_RA_DEG   @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              WRITE ( UNIT=STR(1:15), FMT='(F15.11)' ) PIM%SOU(IND_SOU)%ALPHA_INP/DEG__TO__RAD
              BUF(J5)(IB:42) = STR(1:15)
            ELSE IF ( INDEX ( BUF(J5), '@OBS_DEC_DEG  @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@OBS_DEC_DEG  @' ) 
              IE = IB +            LEN('@OBS_DEC_DEG  @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              WRITE ( UNIT=STR(1:15), FMT='(F15.11)' ) PIM%SOU(IND_SOU)%DELTA_INP/DEG__TO__RAD
              CALL CHASHL ( STR(1:15) )
              IF ( PIM%SOU(IND_SOU)%DELTA_INP .GE. 0.0D0 .AND. &
     &             PIM%SOU(IND_SOU)%DELTA_INP < 10.0D0*DEG__TO__RAD ) THEN
                   STR = ' +'//STR
                 ELSE IF ( PIM%SOU(IND_SOU)%DELTA_INP .GE. 10.0D0*DEG__TO__RAD  ) THEN
                   STR = '+'//STR
                 ELSE IF ( PIM%SOU(IND_SOU)%DELTA_INP .GE. -10.0D0*DEG__TO__RAD  .AND. &
     &                     PIM%SOU(IND_SOU)%DELTA_INP < 0.0D0            ) THEN
                   STR = ' '//STR
              END IF
              BUF(J5)(IB:42) = STR(1:15)
            ELSE IF ( INDEX ( BUF(J5), '@GACO         @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@GACO         @' ) 
              IE = IB +            LEN('@GACO         @' ) - 1
              IF ( PIM%GACO_STATUS == PIMA__LOADED ) THEN
                   BUF(J5)(IB:42) = '1'
                 ELSE 
                   BUF(J5)(IB:42) = '0'
              END IF
            ELSE IF ( INDEX ( BUF(J5), '@FREQ_REF     @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@FREQ_REF     @' ) 
              IE = IB +            LEN('@FREQ_REF     @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              WRITE ( UNIT=STR(1:15), FMT='(F15.2)' ) FRQ_ARR(1)
              BUF(J5)(IB:42) = STR(1:15)
            ELSE IF ( INDEX ( BUF(J5), '@IF_WIDTH     @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@IF_WIDTH     @' ) 
              IE = IB +            LEN('@IF_WIDTH     @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              WRITE ( UNIT=STR(1:15), FMT='(F15.2)' ) PIM%FRQ(PIM%CONF%BEG_FRQ,PIM%CONF%FRQ_GRP)%BAND_WIDTH
              BUF(J5)(IB:42) = STR(1:15)
            ELSE IF ( INDEX ( BUF(J5), '@JDATE        @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@JDATE        @' ) 
              IE = IB +            LEN('@JDATE        @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              WRITE ( UNIT=STR(1:15), FMT='(F15.7)' ) 2400000.5D0 + PIM%MJD_0
              BUF(J5)(IB:42) = STR(1:15)
            ELSE IF ( INDEX ( BUF(J5), '@NAX1_FQ@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@NAX1_FQ@' ) 
              IE = IB +            LEN('@NAX1_FQ@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:6), FMT='(I6)' ) 4 + 20*KFRQ
              CALL CHASHR ( STR(1:6) )
              BUF(J5)(IB:42) = '   '//STR(1:6)
            ELSE IF ( INDEX ( BUF(J5), '@NAX1_GACO@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@NAX1_GACO@' ) 
              IE = IB +            LEN('@NAX1_GACO@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:6), FMT='(I6)' ) 8 + 12*KFRQ
              CALL CHASHR ( STR(1:6) )
              BUF(J5)(IB:42) = '   '//STR(1:6)
            ELSE IF ( INDEX ( BUF(J5), '@NUM_POL@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@NUM_POL@' ) 
              IE = IB +            LEN('@NUM_POL@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL INCH   ( LPOL, BUF(J5)(IB:IE) )
              CALL CHASHR (       BUF(J5)(IB:IE) )
            ELSE IF ( INDEX ( BUF(J5), '@NUM_FREQ@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@NUM_FREQ@' ) 
              IE = IB +            LEN('@NUM_FREQ@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL INCH   ( KFRQ, BUF(J5)(IB:IE) )
              CALL CHASHR (       BUF(J5)(IB:IE) )
            ELSE IF ( INDEX ( BUF(J5), '@NUM_FR D@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@NUM_FR D@' ) 
              IE = IB +            LEN('@NUM_FR D@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:6), FMT='(I5,"D")' ) KFRQ
              CALL CHASHL ( STR )
              BUF(J5)(IB:42) = "'"//STR(1:8)//"'"
            ELSE IF ( INDEX ( BUF(J5), '@NUM_FR E@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@NUM_FR E@' ) 
              IE = IB +            LEN('@NUM_FR E@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:6), FMT='(I5,"E")' ) KFRQ
              CALL CHASHL ( STR )
              BUF(J5)(IB:42) = "'"//STR(1:8)//"'"
            ELSE IF ( INDEX ( BUF(J5), '@NUM_FR J@' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@NUM_FR J@' ) 
              IE = IB +            LEN('@NUM_FR J@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH  ( STR )
              WRITE ( UNIT=STR(1:6), FMT='(I5,"J")' ) KFRQ
              CALL CHASHL ( STR )
              BUF(J5)(IB:42) = "'"//STR(1:8)//"'"
            ELSE IF ( INDEX ( BUF(J5), '@NUM_FR  @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@NUM_FR  @' ) 
              IE = IB +            LEN('@NUM_FR  @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR(1:5), FMT='(I5)' ) KFRQ
              CALL CHASHR ( STR(1:8) )
              BUF(J5)(IB:42) = '  '//STR(1:8)
            ELSE IF ( INDEX ( BUF(J5), '@STA@' ) > 0 ) THEN
              CALL CHIN ( BUF(J5+5)(32:33), IND_SUB )
              IB = INDEX ( BUF(J5),    '@STA@' ) 
              IE = IB +            LEN('@STA@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH  ( STR )
              CALL INCH   ( PIM%SUB%L_STA(IND_SUB), STR )
              CALL CHASHR ( STR(1:5) )
              BUF(J5)(IB:42) = STR(1:5)
            ELSE IF ( INDEX ( BUF(J5), '@STA J@' ) > 0 ) THEN
              CALL CHIN ( BUF(J5+5)(32:33), IND_SUB )
              IB = INDEX ( BUF(J5),    '@STA J@' ) 
              IE = IB +            LEN('@STA J@' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH  ( STR )
              CALL INCH   ( PIM%NSTA, STR )
              CALL CHASHR ( STR(1:5) )
              BUF(J5)(IB:42) = STR(1:5)
            ELSE IF ( INDEX ( BUF(J5), '@REF_DATE  @' ) > 0 ) THEN
              IB = INDEX ( BUF(J5),    '@REF_DATE  @' ) 
              IE = IB +            LEN('@REF_DATE  @' ) - 1
              CALL CLRCH  ( BUF(J5)(IB:IE) )
              CALL CLRCH  ( STR )
              STR = MJDSEC_TO_DATE ( PIM%MJD_0, PIM%TAI_0 + PIM%UTC_MTAI, IER )
              BUF(J5)(IB:42) = "'"//STR(1:4)//'-'//STR(6:7)//'-'//STR(9:10)//"'"
         END IF
!
         IF ( BUF(J5)(4:11) == 'XTENSION' ) THEN
!
! ----------- Create a new table
!
              CALL FFCRHD ( %VAL(FPTR), FT_STATUS )
              IF ( FT_STATUS .NE. 0 ) THEN
                   CALL FT_PRINTERROR ( 8455, IUER, 'FFCRHD', FT_STATUS )
                   CALL ERR_LOG ( 8455, IUER, 'FFITS_PUT_KEYS', 'Error in '// &
     &                 'attempt to create a new table' )
                   RETURN 
              END IF
         END IF
!
         CALL FFPREC ( %VAL(FPTR), %REF(BUF(J5)(4:)//CHAR(0)), FT_STATUS )
         IF ( FT_STATUS .NE. 0 ) THEN
              CALL FT_PRINTERROR ( 8456, IUER, 'FFPREC', FT_STATUS )
              CALL ERR_LOG ( 8456, IUER, 'FFITS_PUT_KEYS', 'Error in '// &
     &            'execution command FFPREC' ) 
              RETURN 
         END IF      
 450  CONTINUE 
!
! --- Position the table counter to the first table
!
      CALL FFMAHD ( %VAL(FPTR), %VAL(1), NHDTYPE, FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 8457, IUER, 'FFMAHD', FT_STATUS )
           CALL ERR_LOG ( 8457, IUER, 'FFITS_PUT_KEYS', 'Error in '// &
     &         'position the table counter to the first table' )
           RETURN 
      END IF      
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE FFITS_PUT_KEYS  !#!  
