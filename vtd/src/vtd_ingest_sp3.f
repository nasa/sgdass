      SUBROUTINE VTD_INGEST_SP3 ( L_FIL, C_FIL, VTD, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_INGEST_SP3 parses a file with ephemeride and put       *
! *   results of parsing, satellite ephemeride and clock in the          *
! *   VTD object. Time argument is placed in array %TIM_ARR. Positions   *
! *   are placed in array %SPL_ARR. Satellite clock are place in array   *
! *   %SPL_CLO_ARR.                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * L_FIL ( INTEGER*4 ) -- The number of ephemeride files.               *
! * L_CIL ( CHARACTER ) -- Array of ephemeride files. Dimension: L_FIL.  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *       VTD ( RECORD    ) -- Object which keeps configuration and data *
! *                            related to VLBI Theoretical Delay (VTD)   *
! *                            package.                                  *
! *  IUER ( INTEGER*4, OPT ) -- Universal error handler.                 *
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
! *  ### 22-AUG-2025 VTD_INGEST_SP3  v1.0 (d) L. Petrov  07-SEP-2025 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      INCLUDE   'astro_constants.i'
      TYPE     ( VTD__TYPE      ) :: VTD
      INTEGER*4  L_FIL, IUER
      REAL*8     TIM_STEP
      CHARACTER  C_FIL(L_FIL)*(*)
      CHARACTER  STR*128, STR1*128, SAT_STR*(3*VTD__M_NZO), DATE_STR*28
      CHARACTER  CU_SAT(VTD__M_SOU)*3
      CHARACTER, ALLOCATABLE :: BUF(:)*64
      LOGICAL*1  FL_REP
      INTEGER*4  IS, UNIX_DATE, M_BUF, N_BUF, ILS, IND_SOU_SAT, IND_SP3_SAT, &
     &           INDS_SAT_SOU(VTD__M_SOU), IND_NZO, IND_EPO, IND_EPO_PREV, &
     &           MJD_EPO, MJD_BEG, LU_SAT, J1, J2, J3, IER
      REAL*8     TAI_EPO, TAI_BEG
      INTEGER*8  SIZE_I8
      INTEGER*4, EXTERNAL :: FILE_INFO, LTM_DIF
!
      FL_REP = .FALSE.
      IND_EPO_PREV = 0
      MJD_BEG = -1
      TAI_BEG =  0.0D0
!
! --- Create a list of satellite names that are in the VTD source list
!
      LU_SAT = 0
      DO 410 J1=1,VTD%L_SOU
         IF ( VTD%SOU(J1)%OBJ_TYPE == VTD__ES ) THEN
              LU_SAT = LU_SAT + 1
              CU_SAT(LU_SAT) = VTD%SOU(J1)%IVS_NAME
              INDS_SAT_SOU(LU_SAT) = J1
         END IF
 410  CONTINUE 
      CALL SORT_CH ( LU_SAT, CU_SAT )
!
! --- Cycle over files
!
      DO 420 J2=1,L_FIL
!
! ------ Get information about the ephemeride file
!
         IS = FILE_INFO ( TRIM(C_FIL(J2))//CHAR(0), UNIX_DATE, SIZE_I8 )
         IF ( IS .NE. 0 ) THEN
              CALL GERROR ( STR )
              CALL ERR_LOG ( 2731, IUER, 'VTD_INGEST_SP3', 'Error in an '// &
     &            'attempt in get information about the satellite '// &
     &            'ephemeride file '//TRIM(C_FIL(J2))//' -- '//STR )
              RETURN 
         END IF
!
! ------ Allocate memory for the contents of the ephemeride file
!
         M_BUF = SIZE_I8/60 + 256
         IF ( .NOT. ALLOCATED ( BUF ) ) THEN
              ALLOCATE ( BUF(M_BUF), STAT=IER )
              IF ( IER .NE. 0 ) THEN
                   CALL CLRCH ( STR) 
                   CALL INCH  ( M_BUF, STR ) 
                   CALL ERR_LOG ( 2732, IUER, 'VTD_INGEST_SP3', 'Error in '// &
     &                 'attempt in allocate '//TRIM(STR)//' bytes of dynamic '// &
     &                 'memory for the buffer for the ephemeride file' )
                   RETURN 
              END IF
         END IF
!
! ------ Read the ephemeride file
!
         CALL ERR_PASS ( IUER, IER )
         CALL RD_TEXT  ( C_FIL(J2), M_BUF, BUF, N_BUF, IER )
         IF ( IER .NE. 0 ) THEN
              CALL ERR_LOG ( 2733, IUER, 'VTD_INGEST_SP3', 'Error in '// &
     &            'reading the ephemeride file '//TRIM(C_FIL(J2))//' -- '//STR )
              DEALLOCATE ( BUF ) 
              RETURN 
         END IF
         IF ( N_BUF < 2 ) THEN
              CALL CLRCH ( STR) 
              CALL INCH  ( M_BUF, STR ) 
              CALL ERR_LOG ( 2734, IUER, 'VTD_INGEST_SP3', 'Malformed '// &
     &            'ephemeride file '//TRIM(C_FIL(J2))//' -- it should have '// &
     &            'at least 2 lines, but has only '//TRIM(STR)//' lines' )
              DEALLOCATE ( BUF ) 
              RETURN 
         END IF
!
         IF ( .NOT. ( BUF(1)(1:2) .EQ. '#d' .AND. &
     &                BUF(2)(1:2) .EQ. '##' .AND. &
     &                BUF(N_BUF)(1:3) .EQ. 'EOF' ) ) THEN
              CALL ERR_LOG ( 2735, IUER, 'VTD_INGEST_SP3', 'Malformed '// &
     &            'ephemeride file '//TRIM(C_FIL(J2))//' the first line is '// &
     &            'expected to start from #d and the last line is expected '// &
     &            'to contain EOF, but the first line was '//BUF(1) )
              DEALLOCATE ( BUF ) 
              RETURN 
         END IF
!
         ILS = 1
         CALL CLRCH ( SAT_STR )
!
! ------ Get the time stemp
!
         READ ( UNIT=BUF(2)(25:38), FMT='(F14.8)'  ) TIM_STEP
         DO 430 J3=3,N_BUF
            IF ( BUF(J3)(1:1) == 'P' ) THEN
!
! -------------- Parse the line with ephemeride + clock
! -------------- Get the satellite index
!
! -------------- Search the satellite name among those satellites that we observe
! -------------- Bypass lines with satellites that we did not observe
!
                 IND_SOU_SAT = LTM_DIF ( 0, LU_SAT, CU_SAT, BUF(J3)(2:4) ) 
                 IF ( IND_SOU_SAT .LE. 0 ) GOTO 430
!
! -------------- Check whether this satellite ID has been defined
!
                 IND_SP3_SAT = (INDEX ( SAT_STR(1:ILS), BUF(J3)(2:4) ) - 1)/3 + 1
                 IF ( IND_SP3_SAT < 1 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 2737, IUER, 'VTD_INGEST_SP3', 'Trap '// &
     &                    'of internal control. An unknown satellite '// &
     &                     BUF(J3)(2:4)//' is defined in line '//TRIM(STR)// &
     &                    ' of the ephemeride file '//C_FIL(J2) )
                      DEALLOCATE ( BUF ) 
                      RETURN 
                 END IF
!
                 IND_NZO = VTD%L_NZO + IND_SOU_SAT
                 VTD%NZO(IND_NZO)%STATUS = VTD__LOAD
                 VTD%NZO(IND_NZO)%NAME = BUF(J3)(2:4) 
                 VTD%SOU(INDS_SAT_SOU(IND_SOU_SAT))%IND_NZO = IND_NZO
!
! -------------- Put time. TIM_ARR is counted from  VTD%NZO(IND_NZO)%MJD_BEG, VTD%NZO(IND_NZO)%TIM_BEG epoch
! -------------- in seconds
!
                 VTD%NZO(IND_NZO)%TIM_ARR(IND_EPO) = (IND_EPO-1)*TIM_STEP
!
! -------------- Read satellite positions
!
                 READ ( UNIT=BUF(J3)(5:18),  FMT='(F14.6)', IOSTAT=IER, IOMSG=STR1 ) VTD%NZO(IND_NZO)%SPL_ARR(IND_EPO,1)
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 2738, IUER, 'VTD_INGEST_SP3', 'Error in reading '// &
     &                    'X-component of the orbit on line '//TRIM(STR)// &
     &                    ' of the ephemeride file '//C_FIL(J2)//' '//STR1 )
                      DEALLOCATE ( BUF ) 
                      RETURN 
                 END IF
!
                 READ ( UNIT=BUF(J3)(19:32), FMT='(F14.6)', IOSTAT=IER, IOMSG=STR1 ) VTD%NZO(IND_NZO)%SPL_ARR(IND_EPO,2)
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 2739, IUER, 'VTD_INGEST_SP3', 'Error in reading '// &
     &                    'Y-component of the orbit on line '//TRIM(STR)// &
     &                    ' of the ephemeride file '//C_FIL(J2)//' '//STR1 )
                      DEALLOCATE ( BUF ) 
                      RETURN 
                 END IF
!
                 READ ( UNIT=BUF(J3)(33:46), FMT='(F14.6)', IOSTAT=IER, IOMSG=STR1 ) VTD%NZO(IND_NZO)%SPL_ARR(IND_EPO,3)
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 2740, IUER, 'VTD_INGEST_SP3', 'Error in reading '// &
     &                    'Z-component of the orbit on line '//TRIM(STR)// &
     &                    ' of the ephemeride file '//C_FIL(J2)//' '//STR1 )
                      DEALLOCATE ( BUF ) 
                      RETURN 
                 END IF
!
! -------------- Convert positions to meters
!
                 VTD%NZO(IND_NZO)%SPL_ARR(IND_EPO,1:3) = 1000.0D0* VTD%NZO(IND_NZO)%SPL_ARR(IND_EPO,1:3)
!
! -------------- and satellite clock
!
                 READ ( UNIT=BUF(J3)(47:60), FMT='(F14.6)', IOSTAT=IER, IOMSG=STR1 ) VTD%NZO(IND_NZO)%SPL_CLO_ARR(IND_EPO)
                 IF ( IER .NE. 0 ) THEN
                      CALL CLRCH ( STR )
                      CALL INCH  ( J3, STR )
                      CALL ERR_LOG ( 2741, IUER, 'VTD_INGEST_SP3', 'Error in reading '// &
     &                    'satellite clock on line '//TRIM(STR)// &
     &                    ' of the ephemeride file '//C_FIL(J2)//' '//STR1 )
                      DEALLOCATE ( BUF ) 
                      RETURN 
                 END IF
              ELSE IF ( BUF(J3)(1:2) == '+ ' ) THEN
!
! -------------- Append the satellite names to the string
!
                 SAT_STR(ILS:ILS+50) = BUF(J3)(10:60)
                 ILS = ILS + 51
              ELSE IF ( BUF(J3)(1:2) == '++' ) THEN
                 CONTINUE 
              ELSE IF ( BUF(J3)(1:1) == '*' ) THEN
!
! -------------- Get the epoch in GPS time 
!
                 DATE_STR = BUF(J3)(4:7)//'.'//BUF(J3)(9:10)//'.'//BUF(J3)(12:13)//'_'// &
     &                      BUF(J3)(15:16)//':'//BUF(J3)(18:19)//':'//BUF(J3)(21:31)
                 CALL BLANK_TO_ZERO ( DATE_STR )
!
! -------------- ... and transform it TAI
!
                 CALL DATE_TO_TIME ( DATE_STR, MJD_EPO, TAI_EPO, IER )
                 TAI_EPO = TAI_EPO - VTD__GPS_M_TAI
                 IF ( TAI_EPO > 86400.0D0 ) THEN
                      TAI_EPO = TAI_EPO - 86400.0D0
                      MJD_EPO = MJD_EPO + 1
                 END IF
!
! -------------- Get the epoch index
!
                 IF ( MJD_BEG .LE. 0 ) THEN
                      MJD_BEG = MJD_EPO
                      TAI_BEG = TAI_EPO
                 END IF
                 IND_EPO = NINT ( ( (MJD_EPO - MJD_BEG)*86400.0D0 + &
     &                              (TAI_EPO - TAI_BEG) )/TIM_STEP ) + 1
                 IF ( IND_EPO == IND_EPO_PREV ) THEN
                      FL_REP = .TRUE.
                    ELSE IF ( IND_EPO - IND_EPO_PREV .NE. 1 ) THEN
!
! ------------------- Check whether the epochs are equi-distant
!
                      CALL CLRCH ( STR) 
                      CALL INCH  ( J3, STR ) 
                      write ( 6, * ) 'ind_epo= ', ind_epo, ' ind_epo_prev= ', ind_epo_prev
                      CALL ERR_LOG ( 2736, IUER, 'VTD_INGEST_SP3', 'Error in parsing '// &
     &                    'line '//TRIM(STR)//' of the ephemeride file '//TRIM(C_FIL(J2))// &
     &                    ' -- the time step is out of order' )
                      DEALLOCATE ( BUF ) 
                      RETURN 
                    ELSE 
                      FL_REP = .FALSE.
                 END IF
                 IND_EPO_PREV = IND_EPO
            END IF
 430     CONTINUE 
 420  CONTINUE 
!
! --- Deallocate the buffer
!
      DEALLOCATE ( BUF ) 
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_INGEST_SP3  !#!#
