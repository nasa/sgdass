      PROGRAM    BDSP_PLOT
! ************************************************************************
! *                                                                      *
! *   Program BDSP_PLOT
! *                                                                      *
! *  ### 17-APR-2015    BDSP_PLOT  v2.1 (c)  L. Petrov  28-DEC-2019 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'malo.i'
      INCLUDE   'bindisp.i'
      INCLUDE   'heb.i'
      TYPE     ( HEB__TYPE ) :: HEB
!
      TYPE ( BINDISP_HEADER_2 ) :: REC2
      TYPE ( BINDISP_HEADER_4 ) :: REC4
      TYPE ( BINDISP_HEADER_8 ) :: REC8
      TYPE ( BINDISP_DATA     ) :: RECD
      INTEGER*4  MEL, MP
      PARAMETER  ( MEL = (MALO__NDEG+1)**2 )
      PARAMETER  ( MP  = 128*1024 )
      CHARACTER  DIRIN*128, DATE_BEG*21, DATE_END*21, EXT*3, FILNAM*128, &
     &           FILBDS*128, STR*128, STA_NAM*8, STA_BDS*8, FILOUT*128
      INTEGER*8  DIR_DESC(16), IP8
      REAL*8     TIM_BEG, TIM_END, TIM_FIL, TAI_LOA, POS_XYZ(3), LOA_XYZ(3), &
     &           LOA_UEN(3), XYZ_TO_UEN(3,3), TIM_ARR(MP), VAL_ARR(MP,3)
      REAL*4     DSP_ARR(MEL), DSPL_AVR(MEL)
      INTEGER*2  KX, KY, KZ
      INTEGER*4  MJD_BEG, MJD_END, MJD_FIL, MJD_LOA, J1, J2, J3, J4, IOS, &
     &           KP, IL, IS, LEV, L_FIL, IB, IE, LUN, IND_BEG, IND_END, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: ILEN, I_LEN, GET_FILE_FROM_DIR, GET_UNIT, LINDEX, &
     &                       OPENDIR, CLOSEDIR
!
      EXT = '.bds'
      IF ( IARGC() < 4 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: sta_nam bdsp_dir date_beg date_end [filout]'
           CALL EXIT ( 1 )
         ELSE 
           CALL GETARG ( 1, STA_NAM  )
           CALL TRAN   ( 11, STA_NAM, STA_NAM )
           CALL GETARG ( 2, DIRIN )
           CALL GETARG ( 3, DATE_BEG )
           CALL GETARG ( 4, DATE_END )
           IF ( IARGC() .GE. 5 ) THEN
                CALL GETARG ( 5, FILOUT )
              ELSE
                CALL CLRCH ( STR )
           END IF
      END IF
!
! --- Check input name: is it an exisitig file or existing directory
!
      DIR_DESC(1) = OPENDIR ( DIRIN(1:I_LEN(DIRIN))//CHAR(0) )
      IF ( DIR_DESC(1) > 0 ) THEN
           IP8 = CLOSEDIR ( %VAL(DIR_DESC(1)) )
         ELSE 
           IUER = -1
           CALL ERR_LOG ( 6701, IUER, 'BDSP_PLOT', 'Cannot find '// &
     &         'directory '//DIRIN )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_BEG, MJD_BEG, TIM_BEG, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6702, IUER, 'BDSP_PLOT', 'Wrong begin date '//DATE_BEG )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL DATE_TO_TIME ( DATE_END, MJD_END, TIM_END, IUER ) 
      IF ( IUER .NE. 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 6703, IUER, 'BDSP_PLOT', 'Wrong end date '//DATE_END )
           CALL EXIT ( 1 )
      END IF
!
      L_FIL = 0
      LEV   = 0
      CALL CLRCH ( FILBDS )
      DO 410 J1=1,MALO__FIL
         IS = GET_FILE_FROM_DIR ( LEV, DIR_DESC, DIRIN, FILNAM )
         IF ( IS .NE. 0 ) THEN
              IUER = -1
              CALL ERR_LOG ( 6704, IUER, 'BDSP_PLOT', 'Error in '// &
     &            'reading input directory '//DIRIN(1:I_LEN(DIRIN))// &
     &            '  '//FILNAM )
              CALL EXIT ( 1 )
         END IF
         IF ( LEV == 0 ) GOTO 810 ! End of work
         IF ( INDEX ( FILNAM, EXT ) .LE. 0 ) GOTO 410
         IF ( INDEX ( FILNAM, '#' ) .GT. 0 ) GOTO 410
!
         IB = LINDEX ( FILNAM, '/' ) + 1
         IE = LINDEX ( FILNAM(IB:), '.' ) + IB - 2
         IF ( IE < IB ) IE = ILEN(IB)
         CALL CLRCH ( STA_BDS )
         STA_BDS = FILNAM(IB:IE)
         CALL TRAN ( 11, STA_BDS, STA_BDS )
         IF ( STA_BDS == STA_NAM ) THEN
              FILBDS = FILNAM
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( ILEN(FILBDS) == 0 ) THEN
           CALL ERR_LOG ( 6705, IUER, 'BDSP_PLOT', 'No loading for '// &
     &         'station '//STA_NAM//' was fonund in directory '//DIRIN )
           CALL EXIT ( 1 )
      END IF
!
! --- Open the file with site displacements
!
      LUN = GET_UNIT ()
      OPEN ( UNIT=LUN, FILE=FILBDS, STATUS='UNKNOWN', &
     &       ACCESS='DIRECT', FORM='UNFORMATTED', RECL=LEN__BDS, &
     &       IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 2837, IUER, 'VTD_SET_BINDISP', 'Error '// &
     &          STR(1:I_LEN(STR))//' in opening file '//FILBDS )
           RETURN
      END IF
!
      DO 420 J2=1,M__HDR   
         READ ( UNIT=LUN, REC=J2 ) RECD
         IF ( J2 == 2 ) THEN
              CALL MEMCPY ( REC2, RECD, %VAL(LEN__HDR) )
            ELSE IF ( J2 == 4 ) THEN
              CALL MEMCPY ( REC4, RECD, %VAL(LEN__HDR) )
            ELSE IF ( J2 == 5 ) THEN
              CALL MEMCPY ( POS_XYZ(1), RECD, %VAL(LEN__HDR) )
            ELSE IF ( J2 == 6 ) THEN
              CALL MEMCPY ( POS_XYZ(2), RECD, %VAL(LEN__HDR) )
            ELSE IF ( J2 == 7 ) THEN
              CALL MEMCPY ( POS_XYZ(3), RECD, %VAL(LEN__HDR) )
            ELSE IF ( J2 == 8 ) THEN
              CALL MEMCPY ( REC8, RECD, %VAL(LEN__HDR) )
         END IF
 420  CONTINUE 
      CALL MAKE_XYZ_TO_UEN  ( POS_XYZ, XYZ_TO_UEN )
      IND_BEG = ( (MJD_BEG*86400.0D0 + TIM_BEG) - &
     &            (REC8%MJD_FIRST*86400.0D0 + REC8%TAI_FIRST) )/ &
     &          REC4%SAMPLING_INTERVAL
      IND_END = ( (MJD_END*86400.0D0 + TIM_END) - &
     &            (REC8%MJD_FIRST*86400.0D0 + REC8%TAI_FIRST) )/ &
     &          REC4%SAMPLING_INTERVAL
      IF ( IND_BEG < 1 ) IND_BEG = 1
      IF ( IND_END > REC4%NUM_REC ) IND_END = REC4%NUM_REC 
!
      KP = 0
      DO 430 J3=IND_BEG,IND_END
         READ ( UNIT=LUN, REC=J3+M__HDR ) RECD
         IF ( BTEST ( RECD%EXT_DSP, 1 ) ) THEN
              KX = -1
           ELSE
              KX =  0
         END IF
         IF ( BTEST ( RECD%EXT_DSP, 2 ) ) THEN
              KY = -1
           ELSE
              KY =  0
         END IF
         IF ( BTEST ( RECD%EXT_DSP, 3 ) ) THEN
              KZ = -1
           ELSE
              KZ =  0
         END IF
!
         CALL MVBITS ( RECD%EXT_DSP,  4, 4, KX,  0 )
         CALL MVBITS ( RECD%EXT_DSP,  8, 4, KY,  0 )
         CALL MVBITS ( RECD%EXT_DSP, 12, 4, KZ,  0 )
         LOA_XYZ(1) = RECD%X_DSP/100.D0 + KX*VTD__BDS_MAX*1.0D3
         LOA_XYZ(2) = RECD%Y_DSP/100.D0 + KY*VTD__BDS_MAX*1.0D3
         LOA_XYZ(3) = RECD%Z_DSP/100.D0 + KZ*VTD__BDS_MAX*1.0D3
         CALL MUL_MV_IV_V ( 3, 3, XYZ_TO_UEN, 3, LOA_XYZ, 3, LOA_UEN, IUER )
         KP = KP + 1
         TIM_ARR(KP) = ( (REC8%MJD_FIRST*86400.0D0 + REC8%TAI_FIRST) - &
                         (J2000__MJD*86400.0D0 + 43200.0D0) + &
     &                   (J3-1)*REC4%SAMPLING_INTERVAL )/ &
     &                 YEAR__TO__SEC + 2000.0D0
         VAL_ARR(KP,1:3) = LOA_UEN(1:3)
 430  CONTINUE 
      CLOSE ( UNIT=LUN )
      IUER = -1
      CALL DIAGI_SETDEF ( IUER, 'DIAGI_UNIT', 'Time in years' )
!
      CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'Up loading at station '// &
     &                    STA_NAM(1:I_LEN(STA_NAM))//' in mm' )
      CALL DIAGI_1 ( KP, TIM_ARR, VAL_ARR(1,1), IUER )
!
      CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'East loading at station '// &
     &                    STA_NAM(1:I_LEN(STA_NAM))//' in mm' )
      CALL DIAGI_1 ( KP, TIM_ARR, VAL_ARR(1,2), IUER )
!
      CALL DIAGI_SETDEF ( IUER, 'DIAGI_CTIT', 'North loading at station '// &
     &                    STA_NAM(1:I_LEN(STA_NAM))//' in mm' )
      CALL DIAGI_1 ( KP, TIM_ARR, VAL_ARR(1,3), IUER )
!
      END  PROGRAM   BDSP_PLOT  !#!  
