      PROGRAM    EX1
      IMPLICIT   NONE
      INCLUDE   'gvh.i'
      INTEGER*4  IUER
      TYPE ( GVH__STRU ) ::  GVH
      CHARACTER  BUF(80)*64, SOUNAM*8, FILENAME*128, C_STA(10)*8
      REAL*8     GRDEL(40), GRFRQ(2), SEC(40)
      REAL*4     PHFRQ(2)
      INTEGER*4  NUMOBS, NUMSCA, NUMSTA, NOBS_STA(3), OBS_TAB(3,40), &
     &           OBS2_TAB(5,40)
      INTEGER*4  J1, J2, J3, J4, J5, J6, J7, J8, J9, ISCA, NAS, N1, N2, ISEED
      REAL*4     PHAP1(16,50), PHAP2(13,24)
      INTEGER*2  LO_FRQ(16,40,2), PCA(3,16,40,2)
      INTEGER*4  I_LEN
      DATA       ( ( OBS_TAB(N1,N2), N1=1,3), N2=1,20 ) &
     &           / &
     &             1,  1,  2, &  !   1
     &             1,  1,  3, &  !   2
     &             1,  2,  3, &  !   3
     &             2,  1,  2, &  !   4
     &             2,  1,  3, &  !   5
     &             2,  2,  3, &  !   6
     &             3,  1,  2, &  !   7
     &             3,  1,  3, &  !   8
     &             4,  2,  3, &  !   9
     &             5,  1,  2, &  !  10
     &             5,  1,  3, &  !  11
     &             5,  2,  3, &  !  12
     &             6,  1,  2, &  !  13
     &             6,  1,  3, &  !  14
     &             6,  2,  3, &  !  15
     &             7,  1,  2, &  !  16
     &             7,  1,  3, &  !  17
     &             8,  2,  3, &  !  18
     &             9,  1,  2, &  !  19
     &             9,  1,  3  &  !  20
     &           /
!
      NUMOBS = 20
      NUMSCA = 9
      NUMSTA = 3
      NOBS_STA(1) = 7
      NOBS_STA(2) = 9
      NOBS_STA(3) = 9
      NAS = NOBS_STA(1) + NOBS_STA(2) + NOBS_STA(3)
      ISEED = 98278345
      GRFRQ(1) = 8.2D3
      GRFRQ(2) = 2.2D3
      PHFRQ(1) = 8.3E3
      PHFRQ(2) = 2.3E3
      C_STA(1) = 'GILCREEL'
      C_STA(2) = 'KAUAI   '
      C_STA(3) = 'NRAO85 3'
      FILENAME = '/tmp/sample.agv'
!
      DO 410 J1=1,NUMOBS
         GRDEL(J1) = RAN(ISEED)
 410  CONTINUE 
      DO 420 J2=1,NUMOBS
         GRDEL(J2) = RAN(ISEED)
         SEC(J2) = 60000 + OBS_TAB(1,J2)
         DO 430 J3=1,2
            DO 440 J4=1,16
               LO_FRQ(J4,OBS_TAB(1,J2),J3) = 200.0 + 1000.0*RAN(ISEED)
               PCA(1,J4,OBS_TAB(1,J2),J3) = 32765.0*RAN(ISEED)
               PCA(2,J4,OBS_TAB(1,J2),J3) = 32765.0*RAN(ISEED)
               PCA(3,J4,OBS_TAB(1,J2),J3) = 32765.0*RAN(ISEED)
 440        CONTINUE 
 430     CONTINUE 
 420  CONTINUE 
!
      IUER = -1
      CALL GVH_PUT_OBS_TAB ( .FALSE., NUMOBS, NUMSTA, NOBS_STA, OBS_TAB, &
     &                       OBS2_TAB, IUER )
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      write ( 6, 127 ) ( j1, (obs2_tab(j2,j1), j2=1,5),j1=1,20 ) ! %%%%%
 127  FORMAT ( 'obs2_tab(',i2,')  ', 5i5 )                       ! %%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      IUER = -1
      CALL GVH_INIT ( GVH, IUER )
      WRITE ( 6, * ) '0: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%%
      GVH%NSEG = 2
!
      IUER = -1
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '1 CHARACTER ASCII', IUER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '2 INTEGER*2 IEEE-231', IUER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '3 INTEGER*4 IEEE-231', IUER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '4 REAL*4 IEEE 754-1985', IUER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_TYPE:',  '5 REAL*8 IEEE 754-1985', IUER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:', '81 Session', IUER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:', '82 Scan', IUER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:', '83 Station', IUER )
      CALL GVH_PPREA ( GVH, 1, 'DEF_CLASS:', '84 Baseline', IUER )
      CALL GVH_PPREA ( GVH, 1, 'GENERATOR:', 'ex1 '//GVH__LABEL, IUER )
      CALL GVH_PPREA ( GVH, 1, 'FILENAME:',  'sample.bgv', IUER )
      WRITE ( 6, * ) '1: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%%
!
      BUF(1) = 'Dbedit: na444, NEOSA  | geo_export'
      BUF(2) = 'VLBI experiment NA444 ( NEOSA )'
      BUF(3) = 'Created by dbedit Ver. 2001.09.19  revision 6.71'
      BUF(4) = '        run by user vlbi ( vlbi@casa.usno.navy.mil )'
      BUF(5) = '            at USN -- United State Naval Observatory'
      BUF(6) = '               on casa at 2001.11.14-20:32:32 local time'
      BUF(7) = 'Dbedit control file history entry: na444, NEOSA  | geo_export'
      BUF(8) = 'Directory /scratch/na444/8444'
!
      IUER = -1
      CALL GVH_PTEXT_CHP ( GVH, 1, 'History of version 1', 8, BUF, IUER )
      WRITE ( 6, * ) '2: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%
!
      BUF(11) = 'CALC 9.12 Ver. 2001.01.12  Tue Nov 20 10:48:02 2001       '// &
     &          'lyra'
      BUF(12) = 'Calc 9.12 with External Inputs  -DG-'
!
      IUER = -1
      CALL GVH_PTEXT_CHP ( GVH, 1, 'History of version 2. Created '// &
     &                          '2001-11-04T15:48:12 UTC', 2, BUF(11), IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'EXPSERNO', GVH__I4, GVH__SES, 1, 1, &
     &               'Experiment serial number at correlator', 1, IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'CORPLACE', GVH__C1, GVH__SES, 8, 1, &
     &               'Correlator name', 1, IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'SEC_TAG ', GVH__R8, GVH__SCA, 1, 1, &
     &               'Seconds portion of time tag', 1, IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'SOURCE  ', GVH__C1, GVH__SCA, 8, 1, &
     &               'Source name', 1, IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'LO_FREQ ', GVH__I2, GVH__STA, 2, 16, &
     &               'Local Osciallator frequencies per channel in MHz', &
     &                1, IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'CALBYFRQ', GVH__I2, GVH__STA, 2, 16, &
     &               'Phase cal amp(0-10000), phs(0.01*deg), freq(KHz) '// &
     &               'per channel', 1, IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'GROBSDEL', GVH__R8, GVH__BAS, 2, 1, &
     &               'Observed group delay in sec', 1, IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'GRIONFRQ', GVH__R8, GVH__BAS, 2, 1, &
     &               'Effective ionosphere frequency for group delay (MHz)', &
     &                2, IUER )
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'PHIONFRQ', GVH__R4, GVH__BAS, 2, 1, &
     &               'Effective ionosphere frequency for phase delay (MHz)', &
     &                2, IUER )
      WRITE ( 6, * ) '3: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%
!
      IUER = -1
      CALL GVH_PTOC ( GVH, 'PHASE_AP', GVH__R4, GVH__BAS, -16, -512, &
     &               'Fringe phase per channel, per AP (rad)', 1, IUER )
!
      IUER = -1
      CALL GVH_PREPUT ( GVH, NUMOBS, NUMSCA, NUMSTA, NOBS_STA, C_STA, OBS_TAB, &
     &                  1, IUER )
      WRITE ( 6, * ) '4: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%
!
      IUER = -1
      CALL GVH_PLCODE ( GVH, 'EXPSERNO', 1, 0, 7685, IUER )
!
      IUER = -1
      CALL GVH_PLCODE ( GVH, 'CORPLACE', 1, 0, 'HAYSTACK', IUER )
!
      DO 450 J5=1,NUMOBS
         ISCA = OBS_TAB(1,J5)
         SOUNAM = 'src.    '
         CALL INCH   ( ISCA, SOUNAM(7:8) )
         CALL CHASHR (       SOUNAM(7:8) )
         IUER = -1
         CALL GVH_PLCODE ( GVH, 'SOURCE  ', J5, 0, SOUNAM, IUER )
         IUER = -1
         SEC = ISCA + ISCA/100.0D0
         CALL GVH_PLCODE ( GVH, 'SEC_TAG ', J5, 0, SEC, IUER )
!
         IUER = -1
         CALL GVH_PLCODE ( GVH, 'GROBSDEL', J5, 0, GRDEL(J5),    IUER )
         IUER = -1
         CALL GVH_PLCODE ( GVH, 'GRIONFRQ', J5, 0, GRFRQ,        IUER )
         IUER = -1
         CALL GVH_PLCODE ( GVH, 'PHIONFRQ', J5, 0, PHFRQ,        IUER )
         IUER = -1
         CALL GVH_PLCODE ( GVH, 'LO_FREQ ', J5, 1, LO_FRQ(1,OBS_TAB(1,J5),1), &
     &                     IUER )
         IUER = -1
         CALL GVH_PLCODE ( GVH, 'LO_FREQ ', J5, 2, LO_FRQ(1,OBS_TAB(1,J5),2), &
     &                     IUER )
         IUER = -1
         CALL GVH_PLCODE ( GVH, 'CALBYFRQ', J5, 1, PCA(1,1,OBS_TAB(1,J5),1), &
     &                     IUER )
         IUER = -1
         CALL GVH_PLCODE ( GVH, 'CALBYFRQ', J5, 2, PCA(1,1,OBS2_TAB(1,J5),2), &
     &                     IUER )
 450  CONTINUE
!
      DO 460 J6=1,16
         DO 470 J7=1,50
            PHAP1(J6,J7) = 3.141592*2.0*(RAN(ISEED))
 470     CONTINUE
 460  CONTINUE
!
      DO 480 J8=1,13
         DO 490 J9=1,24
            PHAP2(J8,J9) = 3.141592*2.0*(RAN(ISEED))
 490     CONTINUE
 480  CONTINUE
!
      IUER = -1
      CALL GVH_PHLCODE ( GVH, 'PHASE_AP', 11, 0, 16, 50, PHAP1, IUER )
!
      IUER = -1
      CALL GVH_PHLCODE ( GVH, 'PHASE_AP', 18, 0, 13, 24, PHAP1, IUER )
!
      WRITE ( 6, * ) '5: GVH%DMA =',GVH%DMA, ' GVH%DMS =',GVH%DMS ! %%
!           IUER = -1
!           CALL ERR_LOG ( 5000, IUER, 'EX1', 'mu-mu!' )
!
   write ( 6, * ) ' ntoc-1 = ' , GVH%TOCS(1)%NTOC ! %%%%%%%%%
   write ( 6, * ) ' ntoc-2 = ' , GVH%TOCS(2)%NTOC ! %%%%%%%%%
      FILENAME = '/tmp/sample.agv'
      IUER = -1
      CALL GVH_WRITE_AGV ( GVH, 1, GVH__CRT, FILENAME(1:I_LEN(FILENAME)), &
     &                     IUER )
      IUER = -1
      CALL GVH_WRITE_AGV ( GVH, 2, GVH__APP, FILENAME(1:I_LEN(FILENAME)), &
     &                     IUER )
      WRITE ( 6, * ) ' output avg-file: '//FILENAME(1:I_LEN(FILENAME))
!
      IUER = -1
      FILENAME = '/tmp/sample-1.agv'
      CALL GVH_WRITE_AGV ( GVH, 1, GVH__CRT, FILENAME(1:I_LEN(FILENAME)), &
     &                     IUER )
      WRITE ( 6, * ) ' output avg-file: '//FILENAME(1:I_LEN(FILENAME))
!
      FILENAME = '/tmp/sample-2.agv'
      IUER = -1
      CALL GVH_WRITE_AGV ( GVH, 2, GVH__CRT, FILENAME(1:I_LEN(FILENAME)), &
     &                     IUER )
      WRITE ( 6, * ) ' output avg-file: '//FILENAME(1:I_LEN(FILENAME))
!
      FILENAME = '/tmp/sample.bgv'
      IUER = -1
      CALL GVH_WRITE_BGV ( GVH, 1, GVH__CRT, FILENAME(1:I_LEN(FILENAME)), &
     &                     IUER )
      WRITE ( 6, * ) '1: output bvg-file: '//FILENAME(1:I_LEN(FILENAME))
      IUER = -1
      CALL GVH_WRITE_BGV ( GVH, 2, GVH__APP, FILENAME(1:I_LEN(FILENAME)), &
     &                     IUER )
!      WRITE ( 6, * ) '2: output bvg-file: '//FILENAME(1:I_LEN(FILENAME))
!!
!      IUER = -1
!      CALL GVH_WRITE_BGV ( GVH, 1, GVH__CRT, FILENAME(1:I_LEN(FILENAME))//'_1', &
!     &                     IUER )
!      WRITE ( 6, * ) ' output bvg-file: '//FILENAME(1:I_LEN(FILENAME))//'_1'
!      IUER = -1
!      CALL GVH_WRITE_BGV ( GVH, 2, GVH__CRT, FILENAME(1:I_LEN(FILENAME))//'_2', &
!     &                     IUER )
!      WRITE ( 6, * ) ' output bvg-file: '//FILENAME(1:I_LEN(FILENAME))//'_2'
!
      END  !#!  EX1  #!#
