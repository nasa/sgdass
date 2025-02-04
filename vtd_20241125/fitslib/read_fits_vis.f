      SUBROUTINE READ_FITS_VIS ( FINAM, VIS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_FITS_VIS reads the calibrated fringe visibility data *
! *   for observation of a source from the file in fits format generated *
! *   by AIPS or DIFMAP and fills fields of the object VIS.              *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  FINAM ( CHARACTER ) -- Name of the input FITS file with visibility  *
! *                         data.                                        *      
! *                                                                      *
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
! *  ### 29-JAN-2007  READ_FITS_VIS  v2.3 (c)  L. Petrov 03-OCT-2013 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE    'sou_map.i'
      TYPE     ( VIS__TYPE ) :: VIS
      CHARACTER  FINAM*(*)
      INTEGER*4  IUER
      CHARACTER  COMMENT*128, KEYS(MKEY,MHDR)*80, DATE_CHR*21, STR*128
      LOGICAL*4  ANYF
      REAL*4     NULLVAL
      INTEGER*1, ALLOCATABLE :: BUF(:)
      REAL*4,    ALLOCATABLE :: ARR2_R4(:,:), ARR3_R4(:,:,:)
      REAL*8     JD0, JD1, JD, UV_SCALE(2)
      INTEGER*4  BITPIX, INUM, HDU_TYPE, GROUP, PCOUNT, &
     &           J1, J2, J3, J4, J5, J6, J7, J8, J9, &
     &           IND_DAT(2), IND_BAS, IND_TIM, IND_ANN, IP, IL, IER
      INTEGER*4  IND_TAB_FRQ, IND_COL_FRQ
      INTEGER*4  LKEY(MHDR), LHDR, FT_STATUS, FPTR
      INTEGER*4  N1, N2, N3, NARR(8), HDUTYPE, NAX2
      ADDRESS__TYPE, ALLOCATABLE :: DESC(:)
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30
!
! --- Release dynamic memory which may have been allocated eariler
!
      CALL FITSLIB_VIS_FREE ( VIS )
!
! --- Open fits file
!
      FT_STATUS = 0
      CALL ERR_PASS ( IUER, IER ) 
      CALL FFITS_OPEN ( FINAM, FPTR, 'OLD', IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4611, IUER, 'READ_FITS_VIS', 'Error in opening '// &
     &                   'fits file '//FINAM )
           RETURN
      END IF
!
! --- Read the headers
!
      CALL ERR_PASS ( IUER, IER ) 
      CALL FFITS_GET_KEYS ( FPTR, MHDR, MKEY, LHDR, LKEY, KEYS, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 4612, IUER, 'READ_FITS_VIS', 'Error in attempt '// &
     &         'to read the header of fits file '//FINAM )
           CALL FFITS_CLOSE ( FPTR, -2 )
           RETURN
      END IF
!
! --- Scanning all keyword records for something useful
!
      IND_DAT(1)  = 0
      IND_DAT(2)  = 0
      IND_TIM     = 0
      IND_BAS     = 0
      UV_SCALE(1) = 1.0D0
      UV_SCALE(2) = 1.0D0
      DO 410 J1=1,LKEY(1)
         IF ( KEYS(J1,1)(1:8) == 'NAXIS3  ' ) THEN
              READ ( UNIT=KEYS(J1,1)(21:30), FMT='(I10)', IOSTAT=IER ) VIS%NSTK
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4613, IUER, 'READ_FITS_VIS', 'Error in '// &
     &                 'attempt to decode the number of Stokes parameters'// &
     &                 'from the fits file '//FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
              END IF
           ELSE IF ( KEYS(J1,1)(1:8) == 'NAXIS5  ' ) THEN
!
! ----------- Get the number of intermediate frequencies
!
              READ ( UNIT=KEYS(J1,1)(21:30), FMT='(I10)', IOSTAT=IER ) VIS%NFRQ
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4614, IUER, 'READ_FITS_VIS', 'Error in '// &
     &                 'attempt to decode the number of frequencies'// &
     &                 'from the fits file '//FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
              END IF
              ALLOCATE ( VIS%SKY_FRQ(VIS%NFRQ), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*VIS%NFRQ, STR )
                   CALL ERR_LOG ( 4615, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
           ELSE IF ( KEYS(J1,1)(1:8) == 'PCOUNT  ' ) THEN
!
! ----------- Get parameter PCOUNT
!
              READ ( UNIT=KEYS(J1,1)(21:30), FMT='(I10)', IOSTAT=IER ) PCOUNT
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4616, IUER, 'READ_FITS_VIS', 'Error in '// &
     &                 'attempt to decode the number of pcouint parameter '// &
     &                 'from the fits file '//FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
              END IF
           ELSE IF ( KEYS(J1,1)(1:8) == 'GCOUNT  ' ) THEN
!
! ----------- Get parameter PCOUNT -- the number of accumulation periods
!
              IF ( VIS%NFRQ == 0 ) THEN
                   CALL ERR_LOG ( 4617, IUER, 'READ_FITS_VIS', 'Trap of '// &
     &                 'internal control: keyword GCOUNT is defined, but '// &
     &                 'keyword NAXIS5 was not defined. Apparently the '// &
     &                 'fits file '//FINAM(1:I_LEN(FINAM))//' does not '// &
     &                 'conform specifications' )
                   CALL FFITS_CLOSE ( FPTR, -2 )
              END IF 
!
              READ ( UNIT=KEYS(J1,1)(21:30), FMT='(I10)', IOSTAT=IER ) VIS%NP
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4618, IUER, 'READ_FITS_VIS', 'Error in '// &
     &                 'attempt to decode the number of UV points '// &
     &                 'from the fits file '//FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
              END IF
!
! ----------- Allocate dynamic memory
!
              ALLOCATE ( VIS%MJD(VIS%NP), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*VIS%NP, STR )
                   CALL ERR_LOG ( 4619, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( VIS%TAI(VIS%NP), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*VIS%NP, STR )
                   CALL ERR_LOG ( 4620, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( VIS%VIS(VIS%NFRQ,VIS%NP), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 8*VIS%NP, STR )
                   CALL ERR_LOG ( 4621, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( VIS%UV(2,VIS%NFRQ,VIS%NP), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*2*VIS%NFRQ*VIS%NP, STR )
                   CALL ERR_LOG ( 4622, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( VIS%WEI(VIS%NFRQ,VIS%NP), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 4*VIS%NFRQ*VIS%NP, STR )
                   CALL ERR_LOG ( 4623, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( VIS%IND_BAS(VIS%NP), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 16*VIS%NP, STR )
                   CALL ERR_LOG ( 4624, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              ALLOCATE ( VIS%INT_TIM(VIS%NP), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL IINCH ( 16*VIS%NP, STR )
                   CALL ERR_LOG ( 4625, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &                 'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &                 'memory' )
                   RETURN 
              END IF
!
              VIS%STATUS  = SMP__ALLC
         END IF
!
         IF ( KEYS(J1,1)(1:8) == 'OBJECT  ' ) THEN
!
! ----------- Get the name of the object 
!
              VIS%SOU_NAME = KEYS(J1,1)(12:21)
              IP = INDEX ( VIS%SOU_NAME, "'" )
              IF ( IP > 0 ) CALL CLRCH ( VIS%SOU_NAME(IP:) )
         END IF
         IF ( KEYS(J1,1)(1:8) == 'OBSERVER' ) THEN
!
! ----------- Get the name of the object 
!
              VIS%EXP_NAME = KEYS(J1,1)(12:23)
              IP = INDEX ( VIS%EXP_NAME, "'" )
              IF ( IP > 0 ) CALL CLRCH ( VIS%EXP_NAME(IP:) )
         END IF
         IF ( KEYS(J1,1)(1:8) == 'DATE-OBS' ) THEN
!
! ----------- Get the observation date
!
              VIS%DATE_OBS= KEYS(J1,1)(12:21)
              IF ( VIS%DATE_OBS(5:5) == '-' ) THEN
!
! ---------------- Transform the date to VTD format
!
                   VIS%DATE_OBS = VIS%DATE_OBS(1:4)//'.'// &
     &                            VIS%DATE_OBS(6:7)//'.'// &
     &                            VIS%DATE_OBS(9:10)
              END IF
         END IF
!
         IF ( KEYS(J1,1)(11:22) == "'FREQ    '" ) THEN
!
! ----------- Get the reference LO frequency
!
              IF ( KEYS(J1+1,1)(1:5) == 'CRVAL' ) THEN
                   READ ( UNIT=KEYS(J1+1,1)(11:30), FMT='(F20.0)', IOSTAT=IER ) VIS%FRQ_LO
                 ELSE IF ( KEYS(J1+2,1)(1:5) == 'CRVAL' ) THEN
                   READ ( UNIT=KEYS(J1+2,1)(11:30), FMT='(F20.0)', IOSTAT=IER ) VIS%FRQ_LO
                 ELSE IF ( KEYS(J1+3,1)(1:5) == 'CRVAL' ) THEN
                   READ ( UNIT=KEYS(J1+3,1)(11:30), FMT='(F20.0)', IOSTAT=IER ) VIS%FRQ_LO
              END IF
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4626, IUER, 'READ_FITS_VIS', 'Error in '// &
     &                 'attempt to decode the sky frequency '// &
     &                 'from the fits file '//FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
                   RETURN 
              END IF
         END IF
!
         IF ( KEYS(J1,1)(1:8) == 'DATE-OBS' ) THEN
!
! ----------- Get the observation date
!
              CALL CLRCH ( DATE_CHR )
              IF ( KEYS(J1,1)(14:14) == '/' ) THEN
                   DATE_CHR = '19'//KEYS(J1,1)(18:19)//'_'// &
     &                              KEYS(J1,1)(15:16)//'_'// &
     &                              KEYS(J1,1)(12:13)//'_00:00:00.0'
                   DATE_CHR(5:5) = '_'
                   DATE_CHR(8:8) = '_'
                 ELSE 
                   DATE_CHR = KEYS(J1,1)(12:21)//'_00:00:00.0'
                   DATE_CHR(5:5) = '_'
                   DATE_CHR(8:8) = '_'
              END IF
              IER = -1
              CALL DATE_TO_TIME ( DATE_CHR, VIS%MJD_REF, VIS%TAI_REF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4627, IUER, 'READ_FITS_VIS', 'Error in '// &
     &                 'an attempt to decode the observations date from the '// &
     &                 'fits file '//FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
                   RETURN 
              END IF
         END IF
!
         IF ( KEYS(J1,1)(11:20) == "'DATE    '" ) THEN
              IF ( IND_DAT(1) == 0 ) THEN
                   CALL CHIN ( KEYS(J1,1)(6:8), IND_DAT(1) )
!
! ---------------- Get the offset of the observation date: Julian date of the 
! ---------------- New Year night precesseding the observations
!
                   READ ( UNIT=KEYS(J1+2,1)(11:30), FMT='(F20.0)', IOSTAT=IER ) JD0
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4628, IUER, 'READ_FITS_VIS', 'Error '// &
     &                      'in attempt to decode the PZER0'// &
     &                       KEYS(J1+2,1)(6:8)//' from the fits file '// &
     &                       FINAM )
                        CALL FFITS_CLOSE ( FPTR, -2 )
                        RETURN 
                   END IF
                 ELSE
                   CALL CHIN ( KEYS(J1,1)(6:8), IND_DAT(2) )
                   READ ( UNIT=KEYS(J1+2,1)(11:30), FMT='(F20.0)', IOSTAT=IER ) JD1
                   IF ( IER .NE. 0 ) THEN
                        CALL ERR_LOG ( 4629, IUER, 'READ_FITS_VIS', 'Error '// &
     &                      'in attempt to decode the PZER0'// &
     &                       KEYS(J1+2,1)(6:8)//' from the fits file '// &
     &                       FINAM )
                        CALL FFITS_CLOSE ( FPTR, -2 )
                        RETURN 
                   END IF
              END IF
         END IF
!
         IF ( KEYS(J1,1)(11:20) == "'BASELINE'" ) THEN
              CALL CHIN ( KEYS(J1,1)(6:8), IND_BAS )
         END IF
!
         IF ( KEYS(J1,1)(11:20) == "'INTTIM  '" ) THEN
              CALL CHIN ( KEYS(J1,1)(6:8), IND_TIM )
         END IF
         IF ( KEYS(J1,1)(11:20) == "'UU---SIN'"  .OR. &
     &        KEYS(J1,1)(11:20) == "'UU--    '"       ) THEN
!
              READ ( UNIT=KEYS(J1+1,1)(11:30), FMT='(F20.0)', IOSTAT=IER ) UV_SCALE(1)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4630, IUER, 'READ_FITS_VIS', 'Error '// &
     &                 'in attempt to decode the PSCAL'// &
     &                       KEYS(J1+1,1)(6:8)//' from the fits file '// &
     &                       FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
                   RETURN 
              END IF
         END IF
!
         IF ( KEYS(J1,1)(11:20) == "'VV---SIN'"  .OR. &
     &        KEYS(J1,1)(11:20) == "'VV--    '"       ) THEN
!
              READ ( UNIT=KEYS(J1+1,1)(11:30), FMT='(F20.0)', IOSTAT=IER ) UV_SCALE(2)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4631, IUER, 'READ_FITS_VIS', 'Error '// &
     &                 'in attempt to decode the PSCAL'// &
     &                       KEYS(J1+1,1)(6:8)//' from the fits file '// &
     &                       FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
                   RETURN 
              END IF
         END IF
 410  CONTINUE 
!
      IF ( VIS%NFRQ .LE. 0 ) THEN
           CALL ERR_LOG ( 4632, IUER, 'READ_FITS_VIS', 'Did not find keyword '// &
     &         'NAXIS5 in the input fits file '//FINAM(1:I_LEN(FINAM))// &
     &         ' -- this file does not conform specifications of the '// &
     &         ' fits file with visibility data' )
           RETURN 
      END IF
      IF ( VIS%NP .LE. 0 ) THEN
           CALL ERR_LOG ( 4633, IUER, 'READ_FITS_VIS', 'Did not find keyword '// &
     &         'GCOUNT in the input fits file '//FINAM(1:I_LEN(FINAM))// &
     &         ' -- this file does not conform specifications of the '// &
     &         ' fits file with visibility data' )
           RETURN 
      END IF
!
      NAX2 = -1
      DO 420 J2=1,LKEY(3)
         IF ( KEYS(J2,3)(1:8) == 'NAXIS2  ' ) THEN
!
! ----------- Get the number of intermediate frequencies
!
              READ ( UNIT=KEYS(J2,3)(21:30), FMT='(I10)', IOSTAT=IER ) NAX2
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 4634, IUER, 'READ_FITS_VIS', 'Error in '// &
     &                 'attempt to decode NAXIS2 key from the fits file '//FINAM )
                   CALL FFITS_CLOSE ( FPTR, -2 )
              END IF
           ELSE IF ( KEYS(J2,3)(1:20) == "EXTNAME = 'AIPS AN '" ) THEN
              VIS%NSTA = NAX2
           ELSE IF ( KEYS(J2,3)(11:20) == "'ANNAME  '" ) THEN
              CALL CHIN ( KEYS(J2,3)(6:8), IND_ANN )
         END IF
 420  CONTINUE 
!
! --- Allocate memory for a temporary array
!
      ALLOCATE ( ARR3_R4(3,VIS%NFRQ,VIS%NP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( 3*4*VIS%NFRQ*VIS%NP, STR )
           CALL ERR_LOG ( 4635, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic '// &
     &         'memory' )
           RETURN 
      END IF
!
! --- Position to the header where the data reside
!
      FT_STATUS = 0
      IND_TAB_FRQ = 2
      IND_COL_FRQ = 2
      CALL FFMAHD ( %VAL(FPTR), %VAL(IND_TAB_FRQ), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4628, IUER, 'READ_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4636, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &         'position to the second table fo the fits file '// &
     &          FINAM )
           RETURN 
      END IF
!
! --- Get the frequencies offsets
!
#ifdef SUN
      CALL FFGCVD ( %VAL(FPTR), %VAL(IND_COL_FRQ), %VAL(1), &
     &              %VAL(1), %VAL(VIS%NFRQ), %VAL(0.0D0), &
     &              VIS%SKY_FRQ, ANYF, FT_STATUS )
#else
      CALL FFGCVD ( %VAL(FPTR), %VAL(IND_COL_FRQ), %VAL(INT8(1)), &
     &              %VAL(INT8(1)), %VAL(INT8(VIS%NFRQ)), %VAL(0.0D0), &
     &              VIS%SKY_FRQ, ANYF, FT_STATUS )
#endif
      IF ( FT_STATUS .NE. 0 ) THEN
           CALL FT_PRINTERROR ( 4629, IUER, 'READ_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4637, IUER, 'READ_FITS_VIS', 'Error in an '// &
     &         'attempt to extract the intermediate frequency offsets '// &
     &         'from the fits file '//FINAM )
           RETURN 
      END IF
!
! --- ... and transform them into sky frequencies
!
      DO 430 J3=1,VIS%NFRQ
         VIS%SKY_FRQ(J3) = VIS%SKY_FRQ(J3) + VIS%FRQ_LO
 430  CONTINUE 
!
! --- Position to the header of the table with station information
!
      CALL FFMAHD ( %VAL(FPTR), %VAL(3), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4630, IUER, 'READ_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4638, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &         'position to the first table fo the fits file '// &
     &          FINAM )
           RETURN 
      END IF
!
      ALLOCATE ( BUF((LEN(VIS%C_STA(1))+1)*VIS%NSTA) )
      ALLOCATE ( DESC(VIS%NSTA) )
      DO 440 J4=1,VIS%NSTA
         DESC(J4) = LOC(BUF) + (LEN(VIS%C_STA(1))+1)*(J4-1)
 440  CONTINUE
      CALL FFGCVS ( %VAL(FPTR), %VAL(IND_ANN), %VAL(INT8(1)), &
     &              %VAL(INT8(1)), %VAL(INT8(VIS%NSTA)), %REF('A'//CHAR(0)), &
     &              DESC, ANYF, FT_STATUS )
      DO 450 J5=1,VIS%NSTA
         CALL CLRCH  ( VIS%C_STA(J5) )
         CALL MEMCPY ( VIS%C_STA(J5), %VAL(DESC(J5)) ) ! NB hidden 3rd argument
         IL = ILEN(VIS%C_STA(J5))
         IF ( IL < LEN(VIS%C_STA(J5)) ) THEN
              CALL CLRCH  ( VIS%C_STA(J5)(IL+1:) )
         END IF
 450  CONTINUE 
      DEALLOCATE ( BUF  )
      DEALLOCATE ( DESC )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4630, IUER, 'READ_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4639, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &         'read the station names array from the input fits file '// &
     &          FINAM )
           RETURN 
      END IF
!
! --- Position to the header where the data reside
!
      CALL FFMAHD ( %VAL(FPTR), %VAL(1), HDUTYPE, FT_STATUS )
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4630, IUER, 'READ_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4640, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &         'position to the first table fo the fits file '// &
     &          FINAM )
           RETURN 
      END IF
!
! --- Get the visibility data
!
#ifdef SUN
      CALL FFGPVE ( %VAL(FPTR), %VAL(1), %VAL(1), &
     &              %VAL(3*VIS%NFRQ*VIS%NP), %VAL(0.0), ARR3_R4, &
     &              IER, FT_STATUS )
#else
      CALL FFGPVE ( %VAL(FPTR), %VAL(1), %VAL(INT8(1)), &
     &              %VAL(INT8(3*VIS%NFRQ*VIS%NP)), %VAL(0.0), ARR3_R4, &
     &              IER, FT_STATUS )
#endif
      IF ( FT_STATUS .NE.0 ) THEN
           CALL FT_PRINTERROR ( 4631, IUER, 'READ_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4641, IUER, 'READ_FITS_VIS', 'Failure to '// &
     &         'read uv data from the fits file '//FINAM )
           RETURN 
      END IF
!
! --- ... and re-arrange them
!
      DO 460 J6=1,VIS%NP
         DO 470 J7=1,VIS%NFRQ
            VIS%VIS(J7,J6) = CMPLX ( ARR3_R4(1,J7,J6), ARR3_R4(2,J7,J6) )
            VIS%WEI(J7,J6) = ARR3_R4(3,J7,J6)
 470     CONTINUE 
 460  CONTINUE 
      DEALLOCATE ( ARR3_R4 )
!
      ALLOCATE ( ARR2_R4(PCOUNT,VIS%NP), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL IINCH ( PCOUNT*4*VIS%NP, STR )
           CALL ERR_LOG ( 4642, IUER, 'READ_FITS_VIS', 'Failure to '// &
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
           CALL FT_PRINTERROR ( 4633, IUER, 'READ_FITS_VIS', FT_STATUS )
           CALL ERR_LOG ( 4643, IUER, 'READ_FITS_VIS', 'Failure to '// &
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
         VIS%IND_BAS(J8) = NINT(ARR2_R4(IND_BAS,J8))
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
      END  SUBROUTINE  READ_FITS_VIS  !#!#
