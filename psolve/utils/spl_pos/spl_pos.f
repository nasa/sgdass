      PROGRAM    SPL_POS
! ************************************************************************
! *                                                                      *
! *   Program SPL_POS analyzes a Solve solution listing in spool-format, *
! *   extracts the part related to estimates of site positions modeled   *
! *   with a spline and 1) prepares 6 plots of site position evolution   *
! *   for each station: X, Y, Z as well as Up, East, North components;   *
! *   2) genereates sps modification file which describes station        *
! *   position evolution modeled with a spline. This file is suitable    *
! *   for using in Solve as a substitution file.                         *
! *                                                                      *
! *  ### 08-MAR-2005     SPL_POS   v1.0 (c)  L. Petrov  11-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'solve.i'
      INCLUDE   'bsp.i'
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 16*1024 )
      CHARACTER  SPOOL_FIL*128, BUF(MBUF)*160, FILGEN*128, STR*160, STR1*32
      CHARACTER  FILPLT*128, FIL_BSPPOS*128, SPLPOS_VERS*54
      LOGICAL*4  LEX, FL_SPE
      INTEGER*4  IOS, J1, J2, J3, IP, IND_SEC, L_SPE, LUN, NBUF, IUER
      TYPE       ( BSPSTA__TYPE ), ALLOCATABLE :: SPL(:)
      LOGICAL*4  FL_SPLINE_ONLY 
      CHARACTER, EXTERNAL :: GET_VERSION*54
      INTEGER*4, EXTERNAL :: GET_UNIT, ILEN, I_LEN, LINDEX
!
      INCLUDE  'spl_pos_version.i'
      SPLPOS_VERS = GET_VERSION()
!
      FL_SPLINE_ONLY = .FALSE.
      IF ( IARGC() .LT. 2 ) THEN
           WRITE ( 6, '(A)' ) 'Usage: spl_pos  <spool_file> '// &
     &                        '<generic_output_name> [-spline_only]'
           CALL EXIT ( 1 ) 
         ELSE
           CALL GETARG ( 1, SPOOL_FIL )
           CALL GETARG ( 2, FILGEN    )
           IF ( IARGC() .GE. 3 ) THEN
                CALL GETARG ( 3, STR       )
                IF ( STR == '-spline_only' ) THEN
                     FL_SPLINE_ONLY = .TRUE.
                   ELSE
                     CALL ERR_LOG ( 601, -1, 'SPL_POS', 'The third argument '// &
     &                    STR(1:I_LEN(STR))//' was not recognized. Onlly '// &
     &                   '-spline_only is supported' )
                     CALL EXIT  ( 1 ) 
                END IF
           END IF
      END IF
!
      IP = LINDEX ( FILGEN, '.' )
      IF ( IP > 0 ) THEN
           CALL CLRCH ( FILGEN(IP:) )
      END IF
!
! --- Check whether the spool file exists
!
      INQUIRE ( FILE=SPOOL_FIL, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 602, -1, 'SPL_POS', 'Spool file '// &
     &                    SPOOL_FIL(1:I_LEN(SPOOL_FIL))//' was not found' )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Open the spool file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=SPOOL_FIL, STATUS='OLD', IOSTAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( IOS, STR )
           CALL ERR_LOG ( 603, -1, 'SPL_POS', 'Spool file '// &
     &                    SPOOL_FIL(1:I_LEN(SPOOL_FIL))//' was not found' )
           CALL EXIT ( 1 ) 
      END IF
!
! --- Infinite cycle of reading teh spool file, We searc hfor the
! --- so-called SPE-section. Its contsnts will be put into the buffer array
! --- BUF
!
      L_SPE = 0
      NBUF = 0
      DO 410 J1=1,1024*1024*1024
         READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
         IF ( IOS .EQ. -1 ) GOTO 810
         IF ( IOS .NE.  0 ) THEN
              CALL CLRCH ( STR ) 
              CALL INCH  ( J1, STR ) 
              CALL CLRCH ( STR1 ) 
              CALL INCH  ( IOS, STR1 ) 
              CALL ERR_LOG ( 604, -1, 'SPL_POS', 'Error '// &
     &             STR1(1:I_LEN(STR1))//' in reading line '// &
     &             STR(1:I_LEN(STR))//' of spool file '//SPOOL_FIL )
              CALL EXIT ( 1 ) 
         END IF
         IF ( STR(1:6) == '1  SPE' ) THEN
!
! ----------- Aga! The SPE-section started
!
              FL_SPE = .TRUE.
              GOTO 410
         END IF
         IF ( STR(1:6) == '2  SPE' ) THEN
!
! ----------- Ogo! The SPE-section ended
!
              FL_SPE = .FALSE.
              GOTO 810
         END IF
!
         IF ( FL_SPE ) THEN
!
! ----------- COpy the line into the buffer
!
              NBUF = NBUF + 1
              IF ( NBUF == 1 ) IND_SEC = J1
              IF ( NBUF > MBUF ) THEN
                   CALL CLRCH ( STR ) 
                   CALL INCH  ( MBUF, STR )
                   CALL ERR_LOG ( 605, -1, 'SPL_POS', 'Parameter MBUF '// &
     &                 'is too small: '//STR )
                   CALL EXIT ( 1 )
              END IF
              BUF(NBUF) = STR
!
              IF ( STR(1:5) == 'L_SPE' ) THEN
!
! ---------------- Learn the nunber of stations whose coordinates were modeled
! ---------------- with a spline
!
                   CALL CHIN ( STR(6:15), L_SPE )
                   IF ( L_SPE < 1  .OR.  L_SPE > M__SPE ) THEN
                        CALL ERR_LOG ( 606, -1, 'SPL_POS', 'Erro in '// &
     &                      'line '//STR(1:I_LEN(STR))//' of spool file '// &
     &                      SPOOL_FIL(1:I_LEN(SPOOL_FIL))//' -- parameter '// &
     &                     'L_SPE is out of range' )
                        CALL EXIT  ( 1 )
                   END IF
              END IF
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( NBUF == 0 ) THEN
           WRITE ( 6, '(A)' ) 'No spline position estimates found'
           CALL EXIT ( 0 ) 
      END IF
      IF ( NBUF > 0  .AND. FL_SPE ) THEN
           WRITE ( 6, '(A)' ) 'No end of SPE-section was found in the '// &
     &                        'spool file'
           CALL EXIT ( 0 ) 
      END IF
      IF ( L_SPE .LE. 0  ) THEN
           WRITE ( 6, '(A)' ) 'No stations with coordinates modeled with '// &
     &                        ' a spline were found'
           CALL EXIT ( 0 ) 
      END IF
!
! --- Allocate memory for SPL-objects
!
      ALLOCATE ( SPL(L_SPE), STAT=IOS )
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 607, -1, 'SPL_POS', 'Error in allocating '// &
     &         'memory for SPE array'  )
           CALL EXIT ( 1 ) 
      END IF
      WRITE ( 6, *) ' NBUF=',NBUF, ' L_SPE= ',L_SPE
!
! --- Parse the SPE-section of the listing and put the parsed information into
! --- the SPL array
!
      IUER = -1 
      CALL PARSE_SPE_LISTING ( NBUF, BUF, L_SPE, SPL, IND_SEC, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 608, -1, 'SPL_POS', 'Error in an attempt to '// &
     &         'parse the spool file listing' )
           CALL EXIT ( 1 ) 
      END IF
!
! --- For each station, each component make a plot
!
      DO 420 J2=1,L_SPE
         IUER = -1 
         CALL WRI_BSPPOS ( SPL(J2), FILGEN, SPLPOS_VERS, FIL_BSPPOS, IUER )
         IF ( IUER .NE. 0 ) THEN
              CALL ERR_LOG ( 609, -1, 'SPL_POS', 'Error in an attempt to '// &
     &            'write the output BSPPOS-file when data for station '// &
     &             SPL(J2)%STATION//' were being processed' ) 
              CALL EXIT ( 1 ) 
         END IF
         WRITE ( 6, '(A)' ) 'Output file: '//FIL_BSPPOS(1:I_LEN(FIL_BSPPOS))
!
         DO 430 J3=1,3
            IUER = -1 
            CALL PLOT_SPL ( SPL(J2), FL_SPLINE_ONLY, .FALSE., J3, FILGEN, &
     &                      FILPLT, IUER )
            IF ( IUER .NE. 0 ) THEN
                 WRITE ( 6, * ) ' J2=',J2, ' J3=',J3,' Station: ', &
     &                          SPL(J2)%STATION
                 CALL ERR_LOG ( 610, -1, 'SPL_POS', 'Error in an attempt '// &
     &               'to plot the site evolution described with spline' )
                 CALL EXIT ( 1 ) 
            END IF
            WRITE ( 6, '(A)' ) 'Output file: '//FILPLT(1:I_LEN(FILPLT))
!
            IUER = -1 
            CALL PLOT_SPL ( SPL(J2), FL_SPLINE_ONLY, .TRUE., J3, FILGEN, &
     &                      FILPLT, IUER )
            IF ( IUER .NE. 0 ) THEN
                 WRITE ( 6, * ) ' J2=',J2, ' J3=',J3,' Station: ', &
     &                          SPL(J2)%STATION
                 CALL ERR_LOG ( 611, -1, 'SPL_POS', 'Error in an attempt '// &
     &               'to plot the site evolution described with spline' )
                 CALL EXIT ( 1 ) 
            END IF
            WRITE ( 6, '(A)' ) 'Output file: '//FILPLT(1:I_LEN(FILPLT))
 430     CONTINUE 
 420  CONTINUE 
!
      END  PROGRAM  SPL_POS
