      PROGRAM    LOADING_MODEL_TO_HARPOS
! ************************************************************************
! *                                                                      *
! *   Program LOADING_MODEL_TO_HARPOS converts the model of site         *
! *   displacements from EPHEDISP to HARPOS format.                      *
! *                                                                      *
! * ## 30-MAY-2013 LOADING_MODEL_TO_HARPOS v1.0 (c) L. Petrov 30-MAY-2013 ## *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      TYPE      ( MALO__TYPE ) :: MALO
      CHARACTER  DIR_EPH*128, FILOUT*128, C_FIL(MALO__FIL)*128, STR*128, &
     &           FILNAM*128, FILDSC*128, FILCOM*128, FILFMT*128, EXT*4, &
     &           MALO_TIDE__LABEL*32
      INTEGER*8  DIR_DESC(16), IP8
      PARAMETER  ( MALO_TIDE__LABEL= 'MALO tide Version of  2013.06.07' )
      REAL*8     DUMMY_R8
      REAL*8,    ALLOCATABLE :: DSPL_ARR(:,:,:)
      INTEGER*4  ISEL, LEV, L_FIL, IS, IL, IFRQ, IND_FRQ(2*MALO__MFRQ), &
     &           L_STA, L_EPC, J1, J2, J3, J4, IUER
      INTEGER*4, EXTERNAL :: GET_FILE_FROM_DIR, ILEN, I_LEN
!
      ISEL = 0
      EXT  = '.eph'
!
      IF ( IARGC() < 5 ) THEN
           WRITE ( 6, * ) 'Usage: loading_model_to_harpos dir_eph '// &
     &                    'output_harpos_file fildsc filcom filfmt [selection]' 
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, DIR_EPH )
           CALL GETARG ( 2, FILOUT  )
           CALL GETARG ( 3, FILDSC  )
           CALL GETARG ( 4, FILCOM  )
           CALL GETARG ( 5, FILFMT  )
           IF ( IARGC() .GE. 6 ) THEN
                CALL GETARG ( 3, STR )
                CALL CHIN   ( STR, ISEL )
           END IF
      END IF
!
      L_FIL = 0
      LEV   = 0
      DO 410 J1=1,MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIR_EPH, FILNAM )
         IF ( IS .NE. 0 ) THEN
              CALL ERR_LOG ( 6901, IUER, 'LOADING_MODEL_TO_HARPOS', &
     &            'Error in reading input directory '// &
     &             DIR_EPH(1:I_LEN(DIR_EPH))//'  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 410
!
         IL = ILEN(FILNAM)
         IF ( IL < 6  ) GOTO 410
         CALL CHIN ( FILNAM(IL-5:IL-4), IFRQ )
         IF ( IFRQ < 1 .OR. IFRQ > 2*MALO__MFRQ - 1 ) THEN
              CALL ERR_LOG ( 6902, IUER, 'LOADING_MODEL_TO_HARPOS', &
     &            'Cannot extract frequency index from file '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( ISEL == 0 ) THEN
              CONTINUE 
            ELSE
              IF ( IFRQ == 11 .OR. &
     &             IFRQ == 12 .OR. &
     &             IFRQ == 21 .OR. &
     &             IFRQ == 22      ) THEN
!
                   CONTINUE 
                 ELSE 
                   GOTO 410
              END IF
         END IF
!
         L_FIL = L_FIL + 1
         C_FIL(L_FIL) = FILNAM
 410  CONTINUE 
 810  CONTINUE 
!
      IF ( L_FIL == 0 ) THEN
           CALL ERR_LOG ( 6903, -2, 'LOADING_MODEL_TO_HARPOS', &
     &         'No files with extension '//EXT(1:I_LEN(EXT))// &
     &         ' were found in the input directory '//DIR_EPH )
           CALL EXIT ( 1 )
        ELSE 
           CALL SORT_CH ( L_FIL, C_FIL )
      END IF
!
      WRITE ( 6, * ) 'L_FIL = ', L_FIL
!
      IUER = -1
      CALL MALO_INIT ( MALO, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6904, -2, 'LOADING_MODEL_TO_HARPOS', &
     &         'Failure in an attempt to initialize MALO data '// &
     &         'structure' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL MALO_EPHEDISP_READ ( C_FIL(1), MALO, DUMMY_R8, MALO__INQ, &
     &                          L_STA, L_EPC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6905, -2, 'LOADING_MODEL_TO_HARPOS', &
     &         'Failure in an attempt to read the first EPHEDISP '// &
     &         'file '//C_FIL(1) )
           CALL EXIT ( 1 )
      END IF
!
      L_STA = MALO%NSTA
      L_EPC = MALO%NTIM 
      IF ( MALO%NTIM .NE. 1 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( MALO%NTIM, STR )
           CALL ERR_LOG ( 6906, -2, 'LOADING_MODEL_TO_HARPOS', &
     &         'Trap of internal control when reading the first'// &
     &         ' EPHEDISP file '//C_FIL(1)(1:I_LEN(C_FIL(1)))// &
     &         ' the number of epochs must be 1, but got '//STR )
           CALL EXIT ( 1 )
      END IF
          write  ( 6, * ) ' l_sta = ', l_sta, ' l_epc = ', malo%ntim ! %%%
!
      ALLOCATE ( DSPL_ARR(3,L_STA,L_FIL), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( 8*3*L_STA*L_FIL, STR )
           CALL ERR_LOG ( 6907, -2, 'LOADING_MODEL_TO_HARPOS', &
     &         'Failure to allocate '//STR(1:I_LEN(STR))//' bytes '// &
     &         'of dynamic memory for array DSPL_ARR' )
           CALL EXIT ( 1 )
      END IF
!
      DO 420 J2=1,L_FIL
         IUER = -1
         IL = ILEN(C_FIL(J2))
         CALL CHIN ( C_FIL(J2)(IL-5:IL-4), IFRQ )
         IND_FRQ(J2) = IFRQ
         IL = ILEN(FILNAM)
         CALL MALO_EPHEDISP_READ ( C_FIL(J2), MALO, DSPL_ARR(1,1,J2), &
     &                             MALO__REA, L_STA, L_EPC, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 6908, -2, 'LOADING_MODEL_TO_HARPOS', &
     &            'Failure in reading EPHEDISP file '//C_FIL(J2) )
              CALL EXIT ( 1 )
         END IF
 420  CONTINUE 
!
! --- Write the cosin and sine amplitudes of atmospheric loading in the
! --- output file
!
      IUER = -1
      CALL MALO_HARPOS_WRITE ( L_STA, L_FIL, IND_FRQ, DSPL_ARR,  &
     &                         MALO, MALO_TIDE__LABEL, FILDSC, &
     &                         FILCOM, FILFMT, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 6909, -2, 'LOADING_MODEL_TO_HARPOS', &
     &         'Error in an attempt to write in the output harpos '// &
     &         'file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, '(A)' ) ' Output file: '//FILOUT(1:I_LEN(FILOUT))
!
      END  PROGRAM  LOADING_MODEL_TO_HARPOS  !#!#
