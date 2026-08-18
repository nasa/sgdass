      SUBROUTINE STAPOS_TO_BINDISP ( STA_NAM, MOD_NAM, LP, MJD, TAI, &
     &                               RES_POS, COO, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine STAPOS_TO_BINDISP writes the time series of residual       *
! *   station position displacements into the output file FILOUT in      *
! *   binary BINDISP format.                                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * STA_NAM    ( CHARACTER ) -- Station name.                            *
! * MOD_NAM    ( CHARACTER ) -- Model name.                              *
! * LP         ( INTEGER*4 ) -- The number of elements in station        *
! *                             displacement time series.                *
! * MJD        ( INTEGER*4 ) -- Array of residual displacement epochs,   *
! *                             MJD part. Dimension: LP.                 *
! * TAI        ( REAL*8    ) -- Array of residual displacement epochs,   *
! *                             TAI part. Dimension: LP.                 *
! * RES_POS    ( REAL*8    ) -- Array of residual displacement.          *
! *                             Dimension: (3,LP).                       *
! * COO        ( REAL*8    ) -- Mean station position at the reference   *
! *                             epoch. Dimension: 3. Units: meters.      *
! * FILOUT     ( CHARACTER ) -- Name of the output file.                 *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! * ### 18-JUL-2025 STAPOS_TO_BINDISP v1.1 (d) L. Petrov 02-JAN-2026 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      TYPE     ( BINDISP_HEADER_2 ) ::  HDR2
      TYPE     ( BINDISP_HEADER_4 ) ::  HDR4
      TYPE     ( BINDISP_HEADER_8 ) ::  HDR8
      TYPE     ( BINDISP_DATA     ) ::  BDS
      CHARACTER  STA_NAM*(*), MOD_NAM*(*), FILOUT*(*)
      INTEGER*4  MP, LP, MJD(LP), IUER 
      REAL*8     TAI(LP), COO(3), RES_POS(3,LP)
      INTEGER*4  MIND
      PARAMETER  ( MIND =       32 )
      CHARACTER  STR*128, STR1*128, HEADER(M__HDR)*(LEN__HDR)
      CHARACTER  ENDIAN_FMT*1, FLOAT_FMT*1, FMT_VERS*4
#ifdef BIG_ENDIAN
      PARAMETER  ( ENDIAN_FMT = 'B' ) ! ENDIAN defiend as preprocessor D option
#else
! if LITTLE_ENDIAN
      PARAMETER  ( ENDIAN_FMT = 'L' ) ! ENDIAN defiend as preprocessor D option
#endif
      PARAMETER  ( FLOAT_FMT  = 'I' ) ! IEEE 754/854 float format
!!      PARAMETER  ( FMT_VERS   = '01'   ) ! 2015.07.24 format
      PARAMETER  ( FMT_VERS   = 'last' ) ! 2025.06.22 format
!
      INTEGER*2  KX, KY, KZ, BIT_SUM
      REAL*8     SEC_VAL
      INTEGER*4  LIND, IND(2,MIND), NP, MJD_VAL, LUN, J1, J2, J3, IOS, IER
      INTEGER*4, EXTERNAL :: BIT_CHECKSUM, GET_UNIT, ILEN, I_LEN
!
! --- Zero out the headers
!
      CALL NOUT ( M__HDR*LEN__HDR, HEADER )
!
! --- Create the BINDISP file header. It consists of 8 records
!
! --- 1-st header record: format label
!
      HEADER(1) = 'BINDISP '
!
! --- 2-nd header record: format description
!
      IUER = -1
      IF ( FMT_VERS == "last" ) THEN
           CALL DATE_TO_TIME ( BINDISP_VERSION_DATE, MJD_VAL, SEC_VAL, IER )
        ELSE IF ( FMT_VERS == "01" ) THEN
           CALL DATE_TO_TIME ( BINDISP_VERSION_DATE_01, MJD_VAL, SEC_VAL, IER )
      END IF
!
      HDR2%MJD_FMT    = MJD_VAL
      HDR2%ENDIAN_FMT = ENDIAN_FMT
      HDR2%FLOAT_FMT  = FLOAT_FMT
      HDR2%N_MOD      = 1
      CALL LIB$MOVC3 ( LEN__HDR, HDR2, %REF(HEADER(2)) )
!
! --- 3-rd header record: station name
!
      HEADER(3) = STA_NAM
!
! --- 4-th header record: the number of data records and the sampling
! ---                     interval
!
      HDR4%NUM_REC = LP
      IF ( LP .GE. 2 ) THEN
           HDR4%SAMPLING_INTERVAL = (MJD(2) - MJD(1))*86400.0D0 + &
     &                              (TAI(2) - TAI(1))
         ELSE
           HDR4%SAMPLING_INTERVAL = 86400.0D0
      END IF
      CALL LIB$MOVC3 ( LEN__HDR, HDR4, %REF(HEADER(4)) )
!
! --- 5,6,7-th records: X,Y,Z site coordinates in the crust-fixed reference
! ---                   frame
!
      CALL LIB$MOVC3 ( LEN__HDR, COO(1), %REF(HEADER(5)) )
      CALL LIB$MOVC3 ( LEN__HDR, COO(2), %REF(HEADER(6)) )
      CALL LIB$MOVC3 ( LEN__HDR, COO(3), %REF(HEADER(7)) )
!
! --- 8-th header record: first epoch: MJD and SEC
!
      HDR8%MJD_FIRST = MJD(1)
      HDR8%TAI_FIRST = 0.0D0
      CALL LIB$MOVC3 ( LEN__HDR, HDR8, %REF(HEADER(8)) )
!
! --- 9,10,11-th record: model type, name, version
!
      CALL LIB$MOVC3 ( LEN__HDR, %REF(MOD_NAM(1:8)),   %REF(HEADER(9))  )
      CALL LIB$MOVC3 ( LEN__HDR, %REF(MOD_NAM(9:16)),  %REF(HEADER(10)) )
      CALL LIB$MOVC3 ( LEN__HDR, %REF(MOD_NAM(17:24)), %REF(HEADER(11)) )
!
! --- Open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', ACCESS='DIRECT', &
     &       FORM='UNFORMATTED', RECL=LEN__BDS, IOSTAT=IOS, IOMSG=STR1 )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 5431, IUER, 'STAPOS_TO_BINDISP', 'Error '// &
     &         'in an attempt to open output file with site position '// &
     &         'variations '//TRIM(FILOUT)//' -- '//STR1 )
           RETURN 
      END IF
!
! ----Write the header
!
      DO 410 J1=1,M__HDR
         WRITE ( UNIT=LUN, REC=J1, IOSTAT=IOS, IOMSG=STR1 ) HEADER(J1)
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) ' J1=',J1,' IOS=',IOS
              CALL ERR_LOG ( 5432, IUER, 'STAPOS_TO_BINDISP', 'Error in '// &
     &            'writing in the header of the output file '//TRIM(FILOUT)// &
     &            ' -- '//STR1 )
              RETURN 
         END IF
 410  CONTINUE
!
! --- Write the body of the displacement file
! --- Position encoded with 20 bits, 16 bit is the main part 
! --- and 4 other bits in the exnension part. Internal unit: 1.0D-5 meter
! --- The displacemenet is KM*1.D0-5 + KS*KE*VTD__BDS_MAX.
! --- where KM INTEGER*2 encoded main part, KE is 4 bit encoded extension,
! --- and KS is the extension sign.
! --- Depending on FMT_VERS, KS and KE are arranged in a differnet way
!
      DO 420 J2=1,LP
!
! ------ Encode displacement extension
!
         KX = RES_POS(1,J2)/VTD__BDS_MAX
         KY = RES_POS(2,J2)/VTD__BDS_MAX
         KZ = RES_POS(3,J2)/VTD__BDS_MAX
!
! ------ Encode the residual with respect to the extension
!
         BDS%X_DSP = NINT( (RES_POS(1,J2) - KX*VTD__BDS_MAX)*1.D5 )
         BDS%Y_DSP = NINT( (RES_POS(2,J2) - KY*VTD__BDS_MAX)*1.D5 )
         BDS%Z_DSP = NINT( (RES_POS(3,J2) - KZ*VTD__BDS_MAX)*1.D5 )
         BDS%EXT_DSP  = 0
!
         IF ( FMT_VERS == 'last' ) THEN
!
! ----------- Put signs of extended displacements to bits  12-14
! ----------- Put values to bits 0-3, 4-7, 8-11
!
              CALL MVBITS (  KX,  0, 4,  BDS%EXT_DSP,  0 )
              CALL MVBITS (  KY,  0, 4,  BDS%EXT_DSP,  4 )
              CALL MVBITS (  KZ,  0, 4,  BDS%EXT_DSP,  8 )
              CALL MVBITS (  KX, 15, 1,  BDS%EXT_DSP, 12 )
              CALL MVBITS (  KY, 15, 1,  BDS%EXT_DSP, 13 )
              CALL MVBITS (  KZ, 15, 1,  BDS%EXT_DSP, 14 )
!
! ----------- Compute the bit sum of the a record of 4 16-bit words
! ----------- using bits 1-63 (1-index convention)
!
              BIT_SUM = BIT_CHECKSUM ( BDS%X_DSP, 1, 63 )
!
! ----------- And putting it into bit 64 of BDS(K_EPC,J7)%X_DSP
! ----------- (or bit 16 of BDS(K_EPC,J7)%EXT_DSP)
!
              CALL MVBITS ( BIT_SUM, 0, 1,  BDS%EXT_DSP, 15 )
            ELSE IF ( FMT_VERS == '01' ) THEN
!
! ----------- Put signs to bits  1-3
! ----------- Put values to bits 4-7, 8-11, 12-15
!
              CALL MVBITS (  KX, 15, 1,  BDS%EXT_DSP,  1 )
              CALL MVBITS (  KY, 15, 1,  BDS%EXT_DSP,  2 )
              CALL MVBITS (  KZ, 15, 1,  BDS%EXT_DSP,  3 )
              CALL MVBITS (  KX,  0, 4,  BDS%EXT_DSP,  4 )
              CALL MVBITS (  KY,  0, 4,  BDS%EXT_DSP,  8 )
              CALL MVBITS (  KZ,  0, 4,  BDS%EXT_DSP, 12 )
         END IF
!
! ------ Write the record
!
         WRITE ( UNIT=LUN, REC=J2+M__HDR, IOSTAT=IOS, IOMSG=STR1 ) BDS 
         IF ( IOS .NE. 0 ) THEN
              WRITE ( 6, * ) ' J2=',J2,' IOS=',IOS
              CALL ERR_LOG ( 5433, IUER, 'STAPOS_TO_BINDISP', &
     &            'Error in writing in the body of the output '// &
     &            'file '//TRIM(FILOUT)//' -- '//STR1 )
              RETURN 
         END IF
 420  CONTINUE
!
! --- Close output file
!
      CLOSE ( UNIT=LUN  )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  STAPOS_TO_BINDISP  !#!#
