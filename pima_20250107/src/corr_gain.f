       PROGRAM    CORR_GAIN_MAIN
       IMPLICIT   NONE 
       INCLUDE   'pima.i'
       CHARACTER  STR*128
       INTEGER*8    STACK_SIZE_IN_BYTES, GB, IS
       PARAMETER  ( GB = 1024*1024*1024 )
       PARAMETER  ( STACK_SIZE_IN_BYTES = PIMA__STACK_SIZE_IN_GIGABYTES * GB )
       INTEGER*8, EXTERNAL :: SET_STACKSIZE 
!
! ---- Set stacksize
!
       IS = SET_STACKSIZE ( %VAL(STACK_SIZE_IN_BYTES) )
       CALL INCH8    ( STACK_SIZE_IN_BYTES/INT8(1024), STR )
       CALL SETENV   ( 'GOMP_STACKSIZE'//CHAR(0), TRIM(STR)//CHAR(0), %VAL(1) )
       CALL CORR_GAIN()
       END  PROGRAM  CORR_GAIN_MAIN
!
! ------------------------------------------------------------------------
!
       SUBROUTINE  CORR_GAIN()
! ************************************************************************
! *                                                                      *
! *   Program CORR_GAIN
! *                                                                      *
! * ### 03-OCT-2013   CORR_GAIN   v1.0   (c) L. Petrov 02-OCT-2013  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      CHARACTER  FILUVA*128, FILCRG*128, FILOUT*128
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      REAL*8,    ALLOCATABLE :: GAIN(:,:)
      REAL*8     MRE(MSUB)
      CHARACTER  STR*128
      INTEGER*4  IND_REF, J1, J2, J3, IUER
      INTEGER*4  LTM_DIF, ILEN, I_LEN
!
      IF ( IARGC() < 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: corr_gain uva-file gain_file output_fits'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, FILUVA )
           CALL GETARG ( 2, FILCRG )
           CALL GETARG ( 3, FILOUT )
      END IF 
!
      IUER = -1
      CALL GET_FITS_VIS ( FILUVA, VIS, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 5201, IUER, 'CORR_GAIN', 'Cannot read original '// &
     &         'file with visibilities '//FILUVA )
           CALL EXIT ( 1 )
      END IF
!
      ALLOCATE ( GAIN(VIS%NFRQ,VIS%NSTA), STAT=IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 8*VIS%NSTA*VIS%NFRQ, STR )
           IUER = -2
           CALL ERR_LOG ( 5202, IUER, 'EXTRA_GAIN', 'Failure to allocate '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory for array GAIN' )
           CALL EXIT ( 1 )
      END IF
      GAIN = 1.0D0
!
      IUER = -1
      CALL READ_CRG ( FILCRG, VIS, GAIN, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 5203, IUER, 'CORR_GAIN', 'Failure in an '// &
     &         'attempt to read the inpout file with gain corrections '// &
     &          FILCRG )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL APPLY_GAIN ( VIS, GAIN, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 5204, IUER, 'CORR_GAIN', 'Failure in '// &
     &         'an attempt to apply gain to the visibility data' )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL CRG_WRITE_FITS ( FILUVA, VIS, FILOUT, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -2
           CALL ERR_LOG ( 5205, IUER, 'CORR_GAIN', 'Failure in '// &
     &         'an attempt to write the output fits-file '//FILOUT )
           CALL EXIT ( 1 )
      END IF
!
      WRITE ( 6, '(A)' ) 'Created output fits-uv file: '// &
     & FILOUT(1:I_LEN(FILOUT))
      CALL EXIT ( 0 ) 
      END  SUBROUTINE  CORR_GAIN  !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE READ_CRG ( FILCRG, VIS, GAIN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine READ_CRG
! *                                                                      *
! *  ### 03-OCT-2013    READ_CRG   v1.0 (c)  L. Petrov  03-OCT-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      CHARACTER  FILCRG*(*)
      TYPE     ( VIS__TYPE ) :: VIS
      REAL*8     GAIN(VIS%NFRQ,VIS%NSTA)
      INTEGER*4  IUER
      INTEGER*4  MB
      PARAMETER  ( MB = 2048 )
      CHARACTER  BUF(MB)*128, STR*128, STA_NAM*8, GAIN__LABEL*54
      PARAMETER  ( GAIN__LABEL = '# GAIN Correction table.  Format version of 2013.10.03' )
      REAL*8     VAL
      INTEGER*4  J1, J2, J3, ISTA, NB, IFRQ, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, LTM_DIF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FILCRG, MB, BUF, NB, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5211, IUER, 'READ_CRG', 'Cannot read input file '// &
     &         'with gain corrections '//FILCRG )
           RETURN 
      END IF
      IF ( BUF(1)(1:54) == GAIN__LABEL ) THEN
           CONTINUE 
         ELSE 
           CALL CLRCH ( STR )
           STR = BUF(1)
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 5212, IUER, 'READ_CRG', 'Wrong format of input '// &
     &         'gain correction file '//FILCRG(1:I_LEN(FILCRG))// &
     &         ' -- the first line is '//STR(1:I_LEN(STR))// &
     &         ' while '//GAIN__LABEL//' was expected' )
           RETURN 
      END IF
!
      DO 410 J1=1,MB
         IF ( BUF(J1)(1:5) == 'Sta: ' ) THEN
              STA_NAM = BUF(J1)(6:13)
              READ ( UNIT=BUF(J1)(21:23), FMT='(I3)', IOSTAT=IER ) IFRQ
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5213, IUER, 'EXTRA_GAIN', 'Failure in '// &
                       'parsing frequency index field in reading line '// &
     &                  STR(1:I_LEN(STR))//' of input file with gain '// &
     &                 'corrections '//FILCRG )
                   RETURN 
              END IF
              IF ( IFRQ < 1 .OR. IFRQ > VIS%NFRQ ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5214, IUER, 'EXTRA_GAIN', 'Failure in '// &
                       'parsing frequency index field in reading line '// &
     &                  STR(1:I_LEN(STR))//' of input file with gain '// &
     &                 'corrections '//FILCRG(1:I_LEN(FILCRG))// &
     &                 ' its valuie is beyind the range if frequencies '// &
     &                 'used in experiment '//VIS%EXP_NAME )
                   RETURN 
              END IF
!
              READ ( UNIT=BUF(J1)(50:56), FMT='(F7.3)', IOSTAT=IER ) VAL
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5215, IUER, 'EXTRA_GAIN', 'Failure in '// &
                       'parsing gain correction field in reading line '// &
     &                  STR(1:I_LEN(STR))//' of input file with gain '// &
     &                 'corrections '//FILCRG )
                   RETURN 
              END IF
              ISTA = LTM_DIF ( 0, VIS%NSTA, VIS%C_STA, STA_NAM )
              IF ( ISTA < 1 ) THEN
                   WRITE ( 6, 210 ) STA_NAM, VIS%SOU_NAME, VIS%EXP_NAME 
 210               FORMAT ( 'Warning: station ', A, ' did not observe ', &
     &                      'source ', A, ' in experiment ',A )
                   GOTO 410
              END IF
              GAIN(IFRQ,ISTA) = VAL
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_CRG  !#!  
!
! ------------------------------------------------------------------------
!
      SUBROUTINE APPLY_GAIN ( VIS, GAIN, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine APPLY_GAIN 
! *                                                                      *
! *  ### 03-OCT-2013  APPLY_GAIN   v1.0 (c)  L. Petrov  03-OCT-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      REAL*8     GAIN(VIS%NFRQ,VIS%NSTA)
      INTEGER*4  J1, J2, J3, IND_SUB, IND_STA_SUB(2), IND_STA(2), IUER
!
      DO 410 J1=1,VIS%NP
         IND_SUB    = NINT(100.0*(VIS%IND_BAS(J1) - INT(VIS%IND_BAS(J1)))) + 1
         IND_STA_SUB(1) = VIS%IND_BAS(J1)/256
         IND_STA_SUB(2) = VIS%IND_BAS(J1) - IND_STA_SUB(1)*256
         IND_STA(1) = VIS%LIS_STA(IND_STA_SUB(1),IND_SUB) 
         IND_STA(2) = VIS%LIS_STA(IND_STA_SUB(2),IND_SUB) 
         DO 420 J2=1,VIS%NFRQ
            VIS%VIS(J2,J1) = GAIN(J2,IND_STA(1))*GAIN(J2,IND_STA(2))  * VIS%VIS(J2,J1) 
 420     CONTINUE 
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  APPLY_GAIN   !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE CRG_WRITE_FITS ( FILIN, VIS, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine CRG_WRITE_FITS 
! *                                                                      *
! * ### 03-OCT-2013  CRG_WRITE_FITS v1.0 (c)  L. Petrov  03-OCT-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      CHARACTER  FILIN*(*), FILOUT*(*)
      INTEGER*4  IUER 
      CHARACTER  STR*128
      INTEGER*8  FPTR
      REAL*4,    ALLOCATABLE :: ARR3_R4(:,:,:,:)
      INTEGER*4  J1, J2, J3, J4, FT_STATUS, HDUTYPE, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Allocate memory for a temporary array
!
      ALLOCATE ( ARR3_R4(3,VIS%NSTK,VIS%NFRQ,VIS%NP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 4*3*VIS%NSTK*VIS%NFRQ*VIS%NP, STR )
           CALL ERR_LOG ( 5231, IUER, 'CRG_WRITE_FITS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory' )
           RETURN
      END IF
!
      CALL ERR_PASS  ( IUER, IER )
      CALL COPY_FILE ( FILIN, FILOUT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5232, IUER, 'CRG_WRITE_FITS', 'Failure in '// &
     &         'an attempt to copy '//FILIN(1:I_LEN(FILIN))//' to '// &
     &          FILOUT )
           RETURN
      END IF
!
! --- Open output fits file
!
      CALL ERR_PASS ( IUER, IER )
      CALL FFITS_OPEN ( FILOUT, FPTR, 'UPDATE', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5233, IUER, 'CRG_WRITE_FITS', 'Error in opening '// &
     &                   'fits file '//FILIN )
           RETURN
      END IF
!
! --- Position to the header of the input file where the data reside
!
      FT_STATUS = 0
      CALL FFMAHD ( %VAL(FPTR), %VAL(1), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 5234, IER, 'CRG_WRITE_FITS', FT_STATUS )
           CALL ERR_LOG ( 5235, IUER, 'CRG_WRITE_FITS', 'Failure to '// &
     &         'position to the first table of the input fits file '// &
     &          FILIN )
           RETURN
      END IF
!
! --- Get the visibility data
!
      CALL FFGPVE ( %VAL(FPTR), %VAL(1), %VAL(INT8(1)), &
     &              %VAL(INT8(3*VIS%NSTK*VIS%NFRQ*VIS%NP)), %VAL(0.0), ARR3_R4, &
     &              IER, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 5236, IER, 'CRG_WRITE_FITS', FT_STATUS )
           CALL ERR_LOG ( 5237, IUER, 'CRG_WRITE_FITS', 'Failure to '// &
     &         'read uv data from the fits file '//FILIN )
           RETURN
      END IF
!
      DO 410 J1=1,VIS%NP
         DO 420 J2=1,VIS%NFRQ
            DO 430 J3=1,VIS%NSTK
               ARR3_R4(1,J3,J2,J1) = REAL ( VIS%VIS(J2,J1) )
               ARR3_R4(2,J3,J2,J1) = IMAG ( VIS%VIS(J2,J1) )
 430        CONTINUE 
 420     CONTINUE 
 410  CONTINUE 
!
      CALL FFMAHD ( %VAL(FPTR), %VAL(1), HDUTYPE, FT_STATUS )
      DO 440 J4=1,VIS%NP
!
! ------ Write the visibility data
!
         CALL FFPPRE ( %VAL(FPTR), %VAL(J4), %VAL(INT8(1)), &
     &                 %VAL(INT8(3*VIS%NSTK*VIS%NFRQ)), &
     &                 ARR3_R4(1,1,1,J4), FT_STATUS )
         IF ( FT_STATUS .NE.0 ) THEN
              CALL FT_PRINTERROR ( 5238, IER, 'CRG_WRITE_FITS', FT_STATUS )
              CALL ERR_LOG ( 5239, IUER, 'CRG_WRITE_FITS', 'Failure to '// &
     &            'write visibility data to the output fits file '//FILOUT )
              RETURN
         END IF
 440  CONTINUE 
!
! --- Well, that is it. Close the FITS file
!
      FT_STATUS = 0
      CALL FFCLOS ( %VAL(FPTR), FT_STATUS )
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL FT_PRINTERROR ( 5240, IER, 'CRG_WRITE_FITS', FT_STATUS )
           CALL ERR_LOG ( 5241, IUER, 'CRG_WRITE_FITS', 'Error in '// &
     &         'attempt to close input fits-file '//FILIN )
           RETURN
      END IF
!
      DEALLOCATE   ( ARR3_R4 )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  CRG_WRITE_FITS  !#!  
