      PROGRAM    LINVEL_PROC
! ************************************************************************
! *                                                                      *
! *   Program  LINVEL_PROC  analyzes contents of files with site         *
! *   positions and site velocities and generates a table and plot       *
! *   of station position time series at a given range of dates.         *
! *                                                                      *
! *      Usage: linvel_plot sta-file vel-file sta_nam [-plot]            *
! *            [-table] [-dates date_min date_max step_in_days]          *
! *                                                                      *
! *      sta-file -- A file with station position in of the formats      *
! *                  recognized by VTD package.                          *
! *      vel-file -- A file with station velocity in of the formats      *
! *                  recognized by VTD package.                          *
! *       sta-nam -- station name.                                       *
! *       -plot   -- to enable plotting.                                 *
! *       -table  -- to enable generating of a table with station        *
! *                  positions within a range.                           *
! *       -dates  -- specifies the date range and step. If the argument  *
! *                  is omitted, the begin and start range is taken      *
! *                  from the station position file and the step is set  *
! *                  to 10 days.                                         *
! *      date_min -- the beginning date of the interval. The date is in  *
! *                  YYYY.MM.DD_hh:mm:ss format.                         *
! *      date_min -- the end date of the interval. The date is in        *
! *                  YYYY.MM.DD_hh:mm:ss format.                         *
! *      step_in_days -- time step in days.                              *
! *                                                                      *
! *   Copyright (c) 1975-2025 United States Government as represented by *
! *   the Administrator of the National Aeronautics and Space            *
! *   Administration. All Rights Reserved.                               *
! *   License: NASA Open Source Software Agreement (NOSA).               *
! *                                                                      *
! *  ### 06-JUL-2025   LINVEL_PROC  v1.0 (d) L. Petrov  07-JUL-2025 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'vtd.i'
      TYPE ( VTD__TYPE ) :: VTD
      INTEGER*4  M_BUF
      PARAMETER  ( M_BUF = 4096 )
      CHARACTER  FILSTA*128, FILVEL*128, STA_NAM*8, STR*128, DATE_BEG_STR*20, &
     &           DATE_END_STR*20, DATE_STP_STR*20, BUF(M_BUF)*512
      LOGICAL*1  FL_PLOT, FL_TABLE, FL_LOAD_COV
      REAL*8     TAI_BEG, TAI_END, TIM_STP
      INTEGER*4  L_BSP, MJD_BEG, MJD_END, I_STA, KA, J1, J2, J3, IUER
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LINDEX, LTM_DIF
!
      CALL CLRCH ( DATE_BEG_STR )
      CALL CLRCH ( DATE_END_STR )
      CALL CLRCH ( DATE_STP_STR )
      FL_PLOT     = .FALSE.
      FL_TABLE    = .FALSE.
      MJD_BEG     = 0
      MJD_END     = 0
      TAI_BEG     = 0.0D0
      TAI_END     = 0.0D0
      TIM_STP     = 10.0D0*86400.0D0
!
      IF ( IARGC() .LT. 3 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: linvel_proc sta-file vel-file sta_nam [-plot] [-table] '// &
     &                        '[-dates date_min date_max step_in_days]'
           CALL EXIT ( 0 )
         ELSE
!
! -------- Parse options
!
           CALL GETARG ( 1, FILSTA )
           CALL GETARG ( 2, FILVEL )
           CALL GETARG ( 3, STA_NAM )
           KA = 4
           IF ( IARGC() > 2 ) THEN
                DO 410 J1=3,IARGC()
                   IF ( KA > IARGC() ) GOTO 810
                   CALL GETARG ( KA, STR )
                   IF ( STR(1:2) == '-p' .OR. STR(1:3) == '--p' ) THEN
                        FL_PLOT = .TRUE.
                      ELSE IF ( STR(1:2) == '-t' .OR. STR(1:3) == '--t' ) THEN
                        FL_TABLE = .TRUE.
                      ELSE IF ( STR(1:2) == '-d' .OR. STR(1:3) == '--d' ) THEN
                        IF ( IARGC() < KA + 3 ) THEN
                             WRITE ( 6, * ) 'Too few arguments: three arguments should follow -dates'
                             CALL EXIT ( 1 )
                        END IF
!
                        KA = KA + 1
                        CALL GETARG ( KA, DATE_BEG_STR)
!
                        KA = KA + 1
                        CALL GETARG ( KA, DATE_END_STR )
!
                        KA = KA + 1
                        CALL GETARG ( KA, DATE_STP_STR )
                      ELSE
                        WRITE ( 6, * ) 'Unsupported option '//STR(1:I_LEN(STR))
                        CALL EXIT ( 1 )
                   END IF
                   KA = KA + 1
 410            CONTINUE
 810            CONTINUE
           END IF
      END IF
!
      IF ( ILEN(DATE_BEG_STR) > 0 ) THEN
!
! -------- Parse the dates
!
           IUER = -1
           CALL DATE_TO_TIME ( DATE_BEG_STR, MJD_BEG, TAI_BEG, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'Error in parsing date_beg argument'
                CALL EXIT ( 1 )
           END IF
           IUER = -1
           CALL DATE_TO_TIME ( DATE_END_STR, MJD_END, TAI_END, IUER )
           IF ( IUER .NE. 0 ) THEN
                WRITE ( 6, * ) 'Error in parsing date_end argument'
                CALL EXIT ( 1 )
           END IF
           IF ( INDEX ( DATE_STP_STR, '.' ) < 1 ) THEN
                DATE_STP_STR = TRIM(DATE_STP_STR)//'.0'
           END IF
           READ ( UNIT=DATE_STP_STR, FMT='(F12.5)' ) TIM_STP
           TIM_STP = 86400.0D0*TIM_STP
      END IF
!!   write ( 6, * ) ' BEG: ', MJD_beg, tai_beg, ' END: ', mjd_end, tai_end, ' tim_stp= ', tim_stp ; call pause ( 'sdsda' ) ! %%%%
!
!
! --- Load station coordinates from the external file specifed in VTD%CONF
!
      IUER = -1
      CALL VTD_INIT ( VTD, IUER )
      IF ( IUER .NE. 0 ) THEN
           IUER =  -1
           CALL ERR_LOG ( 5401, IUER, 'LINVEL_POLT', 'Error in an attempt '// &
     &              'to initialize VTD' )
           CALL EXIT (  1 )
      END IF
!
      VTD%CONF%FINAM_STACOO = FILSTA
      VTD%CONF%FINAM_STAVEL = FILVEL
!
! --- Set a flag indicating that the station list will not be specified, but
! --- taken from the station position file
!
      VTD%STA(1)%STA_INP = 'F'
!
      IUER = -1
      CALL VTD_LOAD_STACOO ( VTD, M_BUF, BUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5402, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &              'to load station coordinates from the input file '// &
     &               VTD%CONF%FINAM_STACOO )
           CALL EXIT (  1 )
      END IF
!
! --- Load station velocities from the external file specifed in VTD%CONF
!
      IUER = -1
      CALL VTD_LOAD_STAVEL ( VTD, M_BUF, BUF, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 5403, IUER, 'VTD_LOAD', 'Error in an attempt '// &
     &         'to load station velocities from the input file '// &
     &          VTD%CONF%FINAM_STAVEL )
           CALL EXIT (  1 )
      END IF
!
! --- Search for the station name
!
      I_STA = 0
      DO 430 J3=1,VTD%L_STA
         IF ( VTD%STA(J3)%IVS_NAME == STA_NAM ) THEN
              I_STA = J3
         END IF
 430  CONTINUE
      IF ( I_STA == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5404, IUER, 'LINVEL_PROC', 'There are no '// &
     &         'parameters of linear for station '//STA_NAM(1:I_LEN(STA_NAM))// &
     &         ' in file '//FILSTA )
           CALL EXIT ( 1 )
      END IF
!
      IF ( MJD_BEG == 0 .AND. MJD_END == 0 ) THEN
!
! -------- Dates were not specified? Let us take dates from the
! -------- station position file
!
           MJD_BEG = VTD%STA(I_STA)%MJD_DA_BEG
           MJD_END = VTD%STA(I_STA)%MJD_DA_END
      END IF
      IF ( MJD_BEG == 0 .AND. MJD_END == 0 ) THEN
           IUER = -1
           CALL ERR_LOG ( 5405, IUER, 'LINVEL_PROC', 'The interval for '// &
     &         'plot/table is not derined neither in the station file, '// &
     &         'nor explicitely' )
           CALL EXIT ( 1 )
      END IF
!
! --- Build time series, make plots and print station position
! --- discontinuity(ies)
!
      IUER = -1
      CALL LINVEL_SER ( VTD, I_STA, FL_TABLE, FL_PLOT, &
     &                  MJD_BEG, TAI_BEG, MJD_END, TAI_END, TIM_STP, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL EXIT ( 1 )
      END IF
!
      END  PROGRAM  LINVEL_PROC !#!#
!
! ------------------------------------------------------------------------
!
      SUBROUTINE LINVEL_SER ( VTD, I_STA, FL_TABLE, FL_PLOT, &
     &                        MJD_BEG, TAI_BEG, MJD_END, TAI_END, TIM_STP, &
     &                        IUER )
! ************************************************************************
! *                                                                      *
! *   Routine LINVEL_SER generates the table of station positions and    *
! *   if FL_TABLE == .TRUE., prints it in standard output and if         *
! *   FL_PLOT == .TRUE., displays plots of station positoin evolution.   *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * VTD      ( VTD__TYPE ) -- Initialized VTD object with loaded station *
! *                           positions and station velocities.          *
! * I_STA    ( INTEGER*4 ) -- Index of the given station in the          *
! *                           station position file.                     *
! * FL_TABLE ( LOGICAL*1 ) -- If .TRUE., then print the table with       *
! *                           station position time series.              *
! * FL_PLOT  ( LOGICAL*1 ) -- If .TRUE., then display plot table with    *
! *                           station position time series.              *
! * MJD_BEG  ( INTEGER*4 ) -- MJD date of the beginning the interval for *
! *                           the time series of station positions in    *
! *                           days.                                      *
! * TAI_BEG  ( REAL*8    ) -- TAI date of the beginning the interval for *
! *                           the time series of station positions in    *
! *                           seconds.                                   *
! * MJD_END  ( INTEGER*4 ) -- MJD date of the end of the interval for    *
! *                           the time series of station positions in    *
! *                           days.                                      *
! * TAI_END  ( INTEGER*4 ) -- TAI date of the end of the interval for    *
! *                           the time series of station positions in    *
! *                           seconds.                                   *
! * TIM_STP  ( REAL*8    ) -- Time step of the output time series        *
! *                           in days.                                   *
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
! *  ### 07-JUL-2025   LINVEL_SER  v1.2 (d)  L. Petrov  12-AUG-2026 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'astro_constants.i'
      INCLUDE   'vtd.i'
      INCLUDE   'diagi.i'
      TYPE     ( VTD__TYPE  ) :: VTD
      LOGICAL*1  FL_PLOT, FL_TABLE
      INTEGER*4  I_STA, MJD_BEG, MJD_END, IUER
      REAL*8     TAI_BEG, TAI_END, TIM_STP
      INTEGER*4    MS
      PARAMETER  ( MS = 64*1024 )
      TYPE       ( DIAGI_STRU ) ::  DIAGI_S
      REAL*8     T(MS), COO(MS,6), ERR(MS,6), XYZ_TO_UEN(3,3), &
     &           COV_XYZ(3,3), COV_UEN(3,3), MAT_33(3,3), &
     &           TIM_OBS, TIM(MS), TIM_REF, COV(2,2), VEC2(2), &
     &           EPS
      REAL*8,    ALLOCATABLE :: MAT(:,:)
      PARAMETER  ( EPS     = 1.D0 )
      CHARACTER  STR*32, STR1*32, CMP(3)*1, ZAG*128, UNIT*128
      DATA CMP   / 'X', 'Y', 'Z' /
      INTEGER*4  NTIM, MJD_OBS, IDAY, J1, J2, J3, IER
      INTEGER*4  IBST, ILST, IOST, IPST, IWST, IDEV, ICL1, ICL2, ICL3
      INTEGER*4  DIAGI_LEN
      CHARACTER, EXTERNAL :: MJDSEC_TO_DATE*30, TIM_TO_DATE*23, GET_CDATE*19
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      NTIM = ((MJD_END*86400.0D0 + TAI_END) - (MJD_BEG*86400.0D0 + TAI_BEG))/TIM_STP + 1
      TIM_REF = (VTD%STA(I_STA)%MJD_REF - J2000__MJD)*86400.0D0 + VTD%STA(I_STA)%TAI_REF
!
      IF ( FL_TABLE ) THEN
!
! -------- Print the table header
!
           STR = MJDSEC_TO_DATE ( VTD%STA(I_STA)%MJD_REF, VTD%STA(I_STA)%TAI_REF, IER )
           WRITE( 6, '(A)' ) STAPOS__LABEL
           WRITE( 6, '(A)' ) '# '
           WRITE( 6, '(A)' ) '# Generated on '//GET_CDATE()
           WRITE( 6, '(A)' ) '# Station position model:  VLBI solution'
           WRITE( 6, '(A)' ) '# Station position file:  '//TRIM(VTD%CONF%FINAM_STACOO)
           WRITE( 6, '(A)' ) '# Station velocity file:  '//TRIM(VTD%CONF%FINAM_STAVEL)
           WRITE( 6, 110   ) VTD%STA(I_STA)%COO_TRS(1:3,1)
           WRITE( 6, 120   ) VTD%STA(I_STA)%VEL_TRS*1.D3*YEAR__TO__SEC
           WRITE( 6, 130   ) STR(1:19)
 110       FORMAT ( '# Station position at the reference epoch: ', 3(F13.4,1X), ' meter' )
 120       FORMAT ( '# Station velocity: ', 24X, 3(2X,F8.2,4X), 'mm/year' )
 130       FORMAT ( '# The position reference epoch:            ', A, '                        TAI' )
           WRITE ( 6, '(A)' ) '# Position variation frame:                                                           NO'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '# ======================================================================================'
           WRITE ( 6, '(A)' ) '# Byte-by-byte Description:'
           WRITE ( 6, '(A)' ) '# --------------------------------------------------------------------------------------'
           WRITE ( 6, '(A)' ) '# Bytes Format Units Label   Explanations'
           WRITE ( 6, '(A)' ) '# --------------------------------------------------------------------------------------'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '#   1- 8  A8     ---   Name    IVS station name'
           WRITE ( 6, '(A)' ) '#  10-28  A19    ---   CalDate Calendar date of the epoch in YYYY.MM.DD-hh:mm:ss format'
           WRITE ( 6, '(A)' ) '#  30-34  I5     days  MJDDate MJD of the epoch'
           WRITE ( 6, '(A)' ) '#  36-42  F7.1   sec   TAIDate TAI of the epoch after the midnight'
           WRITE ( 6, '(A)' ) '#  44-56  F13.4  meter Xpos    X-coordinate of station position on a given epoch'
           WRITE ( 6, '(A)' ) '#  58-70  F13.4  meter Ypos    Y-coordinate of station position on a given epoch'
           WRITE ( 6, '(A)' ) '#  72-84  F13.4  meter Zpos    Z-coordinate of station position on a given epoch'
           WRITE ( 6, '(A)' ) '#  88-93  F6.4   meter Xerr    Errors if X-coordinate of stations positions on  a given epoch'
           WRITE ( 6, '(A)' ) '#  95-100 F6.4   meter Yerr    Errors of Y-coordinate of stations positions on  a given epoch'
           WRITE ( 6, '(A)' ) '# 102-107 F6.4   meter Zerr    Errors of Z-coordinate of stations positions on  a given epoch'
           WRITE ( 6, '(A)' ) '#'
           WRITE ( 6, '(A)' ) '# Name                  Date   MJD     TAI    X-position    Y-position    Z-position   X-err  Y-err  Z-err'
           WRITE ( 6, '(A)' ) '#'
      END IF
!
! --- Generate th output time seroes
!
      DO 410 J1=1,NTIM
!
! ------ Get MJD_OBS, TAI_OBS -- the epoch of the J1-th site position
!
         TIM_OBS = TAI_BEG + (J1-1)*TIM_STP
         IDAY = TIM_OBS/86400.0D0
         MJD_OBS = MJD_BEG + IDAY
         TIM_OBS = TIM_OBS - IDAY*86400.0D0
         TIM(J1) = (MJD_OBS - J2000__MJD)*86400.0D0 + TIM_OBS
!
! ------ Compute position and errors
!
         DO 420 J2=1,3
            COO(J1,J2) = VTD%STA(I_STA)%COO_TRS(J2,1) + &
     &                   VTD%STA(I_STA)%VEL_TRS(J2)*(TIM(J1) - TIM_REF)
            COV(1,1) = VTD%STA(I_STA)%ERR_VEC(J2)**2
            COV(1,2) = VTD%STA(I_STA)%ERR_VEC(J2)*VTD%STA(I_STA)%ERR_VEC(J2+3)* &
     &                 VTD%STA(I_STA)%COR_COO_VEL(LOCS(J2,J2+2))
            COV(2,1) = COV(1,2) 
            COV(2,2) = VTD%STA(I_STA)%ERR_VEC(J2+3)**2
            VEC2(1)  = 1.0D0
            VEC2(2)  = (TIM(J1) - TIM_REF)
            ERR(J1,J2) = DSQRT ( COV(1,1)*VEC2(1)*VEC2(1) + &
     &                           COV(1,2)*VEC2(1)*VEC2(2) + &
     &                           COV(2,1)*VEC2(2)*VEC2(1) + &
     &                           COV(2,2)*VEC2(2)*VEC2(2)   )
 420     CONTINUE
         IF ( FL_TABLE ) THEN
              STR = MJDSEC_TO_DATE ( MJD_OBS, TIM_OBS, IER )
              WRITE ( 6, 140 ) VTD%STA(I_STA)%IVS_NAME, STR(1:19), MJD_OBS, &
     &                         TIM_OBS, COO(J1,1:3), ERR(J1,1:3)
 140          FORMAT ( A8, 1X, A19, 1X, I5, 1X, F7.1, 1X, 3(F13.4,1X), &
     &                 2X,3(F6.4,1X) )
         END IF
!
! ------ Subtract the site position at the reference epoch
! ------ and transform time unit for plotting
!
         COO(J1,1:3) = COO(J1,1:3) - VTD%STA(I_STA)%COO_TRS(1:3,1)
         TIM(J1) = TIM(J1)/(86400.0D0*365.25D0) + 2000.0D0
 410  CONTINUE
      IF ( FL_PLOT ) THEN
           CALL DIAGI_DEF ( IBST, ILST, IOST, IPST, IWST, IDEV, ZAG, &
     &                      UNIT, ICL1, ICL2, ICL3, IER )
           DO 430 J3=1,3
!
! ----------- Setting plotting pasrameters
!
              CALL DIAGI_SETDEF ( IER, 'DIAGI_CTIT', &
     &                            CMP(J3)//'-coordinate of '//VTD%STA(I_STA)%IVS_NAME//' (m)' )
              CALL DIAGI_SETDEF ( IER, 'DIAGI_UTIT', 'Time in years' )
!
! ----------- ... and plotting
!
              CALL DIAGI_1 ( NTIM, TIM, COO(1,J3), IER )
 430       CONTINUE
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  LINVEL_SER  !#!
