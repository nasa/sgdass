      SUBROUTINE PARSE_STAPOS ( FIL_STAPOS, MOD_NAM, STA_NAM, MJD_REF, &
     &                          TAI_REF, MP, LP, MJD, TAI, RES_POS,   &
     &                          COO, VEL, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine PARSE_STAPOS parses the inut file with station position    *
! *   evolution and generates time series of output residual station     *
! *   position displacements after subtracting the mean position at the  *
! *   reeference poehc and the contribution of linear velocity specified *
! *   in the header of the input file.                                   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * FIL_STAPOS ( CHARACTER ) -- Name of the input file.                  *
! * MP         ( INTEGER*4 ) -- Maximum number of elements in station    *
! *                             displacement time series.                *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * MOD_NAM    ( CHARACTER ) -- Model name.                              *
! * STA_NAM    ( CHARACTER ) -- Station name.                            *
! * MJD_REF    ( INTEGER*4 ) -- Station position reference date -- MJD   *
! *                             part.                                    *
! * TAI_REF    ( REAL*8    ) -- Station position reference date -- TAI   *
! *                             part.                                    *
! * LP         ( INTEGER*4 ) -- The number of elements in station        *
! *                             displacement time series.                *
! * MJD        ( INTEGER*4 ) -- Array of residual displacement epochs,   *
! *                             MJD part. Dimension: MP. PARSE_STAPPOS   *
! *                             fills LP elements.                       *
! * TAI        ( REAL*8    ) -- Array of residual displacement epochs,   *
! *                             TAI part. Dimension: MP. PARSE_STAPPOS   *
! *                             fills LP elements.                       *
! * RES_POS    ( REAL*8    ) -- Array of residual displacement.          *
! *                             Dimension: (3,MP). PARSE_STAPPOS fills   *
! *                             LP elements. Units: meters.              *
! * COO        ( REAL*8    ) -- Mean station position at the reference   *
! *                             epoch. Dimension: 3. Units: meters.      *
! * VEL        ( REAL*8    ) -- Mean station velocity at the reference   *
! *                             epoch. Dimension: 3. Units: meter/sec.   *
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
! *  ### 18-JUL-2025  PARSE_STAPOS v1.0 (d)  L. Petrov  20-JUL-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      CHARACTER  FIL_STAPOS*(*), MOD_NAM*(*), STA_NAM*(*)
      INTEGER*4  MP, MJD_REF, LP, MJD(MP), IUER 
      REAL*8     TAI_REF, TAI(MP), COO(3), VEL(3), RES_POS(3,MP)
      INTEGER*4  MIND
      PARAMETER  ( MIND =       32 )
      CHARACTER  BUF(MP)*128, STR*128, STR1*128
      REAL*8     TIM_STEP, EPS
      PARAMETER  ( EPS = 0.2D0  ) ! Tolerance in time step
      INTEGER*4  LIND, IND(2,MIND), NP, J1, J2, J3, J4, J5, IER
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! --- Read input file into BUF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FIL_STAPOS, MP, BUF, NP, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5411, IUER, 'PARSE_STAPOS', 'Error in reading '// &
     &         'station position file '//FIL_STAPOS )
           RETURN 
      END IF
!
! --- Check the magic
!
      IF ( BUF(1)(1:LEN(STAPOS__LABEL)) .NE. STAPOS__LABEL ) THEN
           CALL ERR_LOG ( 5412, IUER, 'PARSE_STAPOS', 'Wrong magic of '// &
     &         'the station position file '//TRIM(FIL_STAPOS)//' was found: '// &
     &          TRIM(BUF(1))//' while '//STAPOS__LABEL//' was expected' )
           RETURN 
      END IF 
!
      LP = 0
!
! --- Cycle over the lines of the input file
!
      DO 410 J1=1,NP
!
! ------ Bypass empty lines
!
         IF ( ILEN(BUF(J1)) == 0 ) GOTO 410
         CALL CLRCH ( STR )
         CALL INCH  ( J1, STR )
!
! ------ Split the line into words
!
         CALL EXWORD ( BUF(J1), MIND, LIND, IND, CHAR(0)//CHAR(32)//CHAR(9), IER )
         IF ( BUF(J1)(1:25) == '# Station position model:' ) THEN
!
! ----------- Extract the name of the station displacements model 
!
              MOD_NAM = BUF(J1)(IND(1,5):IND(2,LIND))
            ELSE IF ( BUF(J1)(1:42) == '# Station position at the reference epoch:' ) THEN
              IF ( LIND < 10 ) THEN
                   CALL ERR_LOG ( 5414, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                    'parsing mean position in line '//TRIM(STR)//' of file '// &
     &                     TRIM(FIL_STAPOS)//' -- too fiew words: '//BUF(J1) )
                   RETURN 
              END IF
!
! ----------- Extract the mean station position at the reference epoch
!
              DO 420 J2=1,3
                 READ ( UNIT=BUF(J1)(IND(1,7+J2):IND(2,7+J2)), FMT='(F14.4)', &
     &                  IOSTAT=IER, IOMSG=STR1 ) COO(J2)
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 5413, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                    'parsing position in line '//TRIM(STR)//' of file '// &
     &                     TRIM(FIL_STAPOS)//' -- '//TRIM(STR1)//' from field '// &
     &                     BUF(J1)(IND(1,7+J2):IND(2,7+J2)) )
                      RETURN 
                 END IF
 420          CONTINUE 
            ELSE IF ( BUF(J1)(1:19) == '# Station velocity:' ) THEN
!
! ----------- Extract the station velocity at the reference epoch
!
              IF ( LIND < 7 ) THEN
                   CALL ERR_LOG ( 5414, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                    'parsing velocity in line '//TRIM(STR)//' of file '// &
     &                     TRIM(FIL_STAPOS)//' -- too fiew words: '//BUF(J1) )
                   RETURN 
              END IF
              DO 430 J3=1,3
                 READ ( UNIT=BUF(J1)(IND(1,3+J3):IND(2,3+J3)), FMT='(F14.4)', &
     &                  IOSTAT=IER , IOMSG=STR1 ) VEL(J3)
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 5415, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                    'parsing velocity in line '//TRIM(STR)//' of file '// &
     &                     TRIM(FIL_STAPOS)//' -- '//TRIM(STR1)//' from field '// &
     &                     BUF(J1)(IND(1,3+J3):IND(2,3+J3)) )
                      RETURN 
                 END IF
!
! -------------- Transform velocity from mm/year to meters/sec
!
                 VEL(J3) = 0.001D0*VEL(J3)/YEAR__TO__SEC
 430          CONTINUE 
            ELSE IF ( BUF(J1)(1:31) == '# The position reference epoch:' ) THEN
              IF ( LIND < 6 ) THEN
                   CALL ERR_LOG ( 5414, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                    'parsing mean position in line '//TRIM(STR)//' of file '// &
     &                     TRIM(FIL_STAPOS)//' -- too fiew words: '//BUF(J1) )
                   RETURN 
              END IF
!
! ----------- Parse the mean position reference epoch
!
              CALL DATE_TO_TIME ( BUF(J1)(IND(1,6):IND(2,6)), MJD_REF, TAI_REF, IER )
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5416, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                 'parsing position reference date in line '//TRIM(STR)// &
     &                 ' of file '//TRIM(FIL_STAPOS)//' from field '// &
     &                 BUF(J1)(IND(1,6):IND(2,6)) )
                   RETURN 
              END IF
            ELSE IF ( BUF(J1)(1:1)  .NE. '#' ) THEN
!
! ----------- Process the line with displacement
!
              IF ( LIND < 7 ) THEN
                   CALL ERR_LOG ( 5414, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                    'parsing mean position in line '//TRIM(STR)//' of file '// &
     &                     TRIM(FIL_STAPOS)//' -- too fiew words: '//BUF(J1) )
                   RETURN 
              END IF
              LP = LP + 1
!
! ----------- Extract station name
!
              STA_NAM =   BUF(J1)(IND(1,1):IND(2,1))
!
! ----------- Parse the epoch (MJD part)
!
              CALL CHIN ( BUF(J1)(IND(1,3):IND(2,3)), MJD(LP) )
              IF ( MJD(LP) < VTD__MJD_MIN .OR. MJD(LP) > VTD__MJD_MAX ) THEN
                   CALL ERR_LOG ( 5417, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                  'parsing date in line '//TRIM(STR)// &
     &                  ' of file '//TRIM(FIL_STAPOS)//' from field '// &
     &                  BUF(J1)(IND(1,3):IND(2,3))//' -- it is out of range' )
                   RETURN 
              END IF
!
! ----------- Parse the epoch (TAI part)
!
              READ ( UNIT=BUF(J1)(IND(1,4):IND(2,4)), FMT='(F7.1)', &
     &                  IOSTAT=IER, IOMSG=STR1 ) TAI(LP)
              IF ( IER .NE. 0 ) THEN
                   CALL ERR_LOG ( 5418, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                  'parsing time in line '//TRIM(STR)// &
     &                  ' of file '//TRIM(FIL_STAPOS)//' from field '// &
     &                  BUF(J1)(IND(1,4):IND(2,4))//' -- '//STR1 )
                   RETURN 
              END IF
!
              IF ( LP == 2 ) THEN
!
! ---------------- Determine the time step
!
                   TIM_STEP = (MJD(LP) - MJD(LP-1))*86400.0D0 + &
     &                        (TAI(LP) - TAI(LP-1))
                 ELSE IF ( LP > 1 ) THEN
!
! ----------------- Check whether the time step is the same as for the 1st/2nd epochs
!
                    IF ( DABS ( (MJD(LP) - MJD(LP-1))*86400.0D0 + &
     &                          (TAI(LP) - TAI(LP-1)) - TIM_STEP ) > EPS ) THEN
                         CALL ERR_LOG ( 5419, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                       'parsing the line '//TRIM(STR)// &
     &                       ' of file '//TRIM(FIL_STAPOS)//' -- the time '// &
     &                       'differnece with respect to the epochs of the '// &
     &                       'previous lines is not the same' )
                         RETURN 
                    END IF
              END IF
!
! ----------- Extract station position...
!
              DO 440 J4=1,3
                 READ ( UNIT=BUF(J1)(IND(1,4+J4):IND(2,4+J4)), FMT='(F13.4)', &
     &                  IOSTAT=IER, IOMSG=STR1 ) RES_POS(J4,LP)
                 IF ( IER .NE. 0 ) THEN
                      CALL ERR_LOG ( 5419, IUER, 'PARSE_STAPOS', 'Error in '// &
     &                    'parsing position in line '//TRIM(STR)//' of file '// &
     &                     TRIM(FIL_STAPOS)//' -- '//TRIM(STR1)//' from field '// &
     &                     BUF(J1)(IND(1,4+J4):IND(2,4+J4)) )
                      RETURN 
                 END IF
!
! -------------- ... and compute the residual position
!
                 RES_POS(J4,LP) = RES_POS(J4,LP) - &
     &                            ( COO(J4) + VEL(J4)*((MJD(LP) - MJD_REF)*86400.0D0 + &
     &                                                 (TAI(LP) - TAI_REF) ) )
 440          CONTINUE 
         END IF
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  PARSE_STAPOS  !#!#
