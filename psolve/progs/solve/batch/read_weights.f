      SUBROUTINE READ_WEIGHTS ( M_WEI, L_WEI, WEIGHT_TYPE, LF_WEI, FILE_WEI, &
     &                          SUPNAM_WEI, SUPVER_WEI, BASELINE_WEI, &
     &                          ARR_WEI, IUER )
! ************************************************************************
! *                                                                      *
! *   Subroutine  READ_WEIGHTS  read external weights file, extracts     *
! *   from there                                                         *
! *   a) superfile name                                                  *
! *   b) superfile version                                               *
! *   c) baseline name                                                   *
! *   d) group delay solution, delay reweight (picoseconds)              *
! *   e) group delay solution, rate  reweight (femtoseconds/second)      *
! *   f) phase delay solution, delay reweight (picoseconds)              *
! *   g) phase delay solution, rate  reweight (femtoseconds/second)      *
! *                                                                      *
! *   reweight is the quantity which us qudratically added to fringe     *
! *   supplied weights in order to form the resiprocal weights actually  *
! *   used for parametrers esimation: w = 1 /sqrt ( sig**2 + rewei**2 )  *
! *                                                                      *
! *   Weight file can be of three types:                                 *
! *                                                                      *
! *   I. WEIGHT_TYPE "A" (for weighting by session or arc)               *
! *                                                                      *
! *   YYMMMDDXX database name                                            *
! *   version number                                                     *
! *   group delay reweight (picoseconds)                                 *
! *   group delay solution delay rate reweight (femtosec/sec)            *
! *   phase delay reweight (picoseconds)                                 *
! *   phase delay solution delay rate reweight (femtosec/sec)            *
! *                                                                      *
! *   II. WEIGHT_TYPE "S" (for weighting by site)                        *
! *                                                                      *
! *   database1 ver sitenam1 #####.## #####.## #####.## #####.## ...     *
! *                 sitenam4 #####.## #####.## #####.## #####.##         *
! *   database1 ver sitenam5 #####.## #####.## #####.## #####.## ...     *
! *                 sitenam8 #####.## #####.## #####.## #####.##         *
! *   for as many sites as needed                                        *
! *   database2 ver sitenam1 #####.## #####.## #####.## #####.## ...     *
! *                 sitenam4 #####.## #####.## #####.## #####.##         *
! *   and so on.                                                         *
! *                                                                      *
! *   (the specific field sizes are:                                     *
! *                                                                      *
! *   col 1-9:   YYMMMDDXX database name                                 *
! *   col 10-13: version number                                          *
! *                                                                      *
! *   1. site name of 8 characters                                       *
! *   2. group delay solution, delay reweight (picoseconds)              *
! *   3. group delay solution, rate reweight (femtoseconds/second)       *
! *   4. phase delay solution, delay reweight (picoseconds)              *
! *   5. phase delay solution, rate reweight (femtoseconds/second)       *
! *   (all four reweight values have fortran format f8.2                 *
! *                                                                      *
! *   III. WEIGHT_TYPE "B" (for weighting by baseline)                   *
! *                                                                      *
! *   col 1-9:   YYMMMDDXX database name                                 *
! *   col 10-13: version number                                          *
! *   col 15-22: station1                                                *
! *   col 24-31: station1                                                *
! *   col 53-61: F8.2  phase delay solution, delay reweight (picoseconds)*
! *                                                                      *
! *   Each element of the output arrays SUPNAM_WEI, SUPVER_WEI,          *
! *   BASELINE_WEI, ARR_WEI corresponds to one records in weight file.   *
! *   In the case of session weights one superfile/version has one       *
! *   record. Nore than one record corresponds to superfile/version for  *
! *   other weighting types.                                             *
! *                                                                      *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *        M_WEI ( INTEGER*4 ) -- Maximal number of records in the       *
! *                               weights file.                          *
! *  WEIGHT_TYPE ( CHARACTER ) -- Type of additicve corrections to       *
! *                               weights : one of                       *
! *                               "A" -- session-dependent reweights;    *
! *                               "B" -- baseline-dependent reweights;   *
! *                               "S" -- site-dependent reweights.       *
! *       LF_WEI ( INTEGER*4 ) -- The number of input weight files.      * 
! *     FILE_WEI ( CHARACTER ) -- Array of full filenames of the weight  *
! *                               files. Dimension: LF_WEI.              *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *        L_WEI ( INTEGER*4 ) -- Number of reweight records read from   *
! *                               the file.                              *
! *   SUPNAM_WEI ( CHARACTER ) -- Array of superfile names. Dimension:   *
! *                               L_WEI.                                 *
! *   SUPVER_WEI ( INTEGER*2 ) -- Array of superfile versions.           *
! *                               Dimension: L_WEI.                      *
! * BASELINE_WEI ( CHARACTER ) -- Arrays of baseline names. It is empty  *
! *                               for session types, has only first 8    *
! *                               characters filled for site-dependent   *
! *                               weighting, and has 16 characters long  *
! *                               baseline name for baseline weighting.  *
! *                               Dimension: L_WEI.                      *
! *      ARR_WEI ( REAL*8    ) -- Array of re-weights values.            *
! *                               Dimension: 4,L_WEI                     *
! *                               1) group delay solution, delay         *
! *                                  reweight (picoseconds);             *
! *                               2) group delay solution, rate          *
! *                                  reweight (femtoseconds/second);     *
! *                               3) phase delay solution, delay         *
! *                                  reweight (picoseconds);             *
! *                               4) phase delay solution, rate          *
! *                                  reweight (femtoseconds/second).     *
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
! *  ### 05-SEP-2001  READ_WEIGHTS v2.0 (c)  L. Petrov  11-MAY-2006 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*4  M_WEI, L_WEI, LF_WEI, IUER
      INTEGER*2  SUPVER_WEI(M_WEI)
      CHARACTER  WEIGHT_TYPE*(*), FILE_WEI(*)*(*), &
     &           SUPNAM_WEI(M_WEI)*(*), BASELINE_WEI(M_WEI)*(*)
      REAL*8     ARR_WEI(4,M_WEI)
      CHARACTER  STR*256, STR2*32, SUPNAM_KEEP*10
      INTEGER*2  SUPVER_KEEP
      LOGICAL*4  LEX
      INTEGER*4  LUN, IOS, NN, J1, J2, J3, IDAT_I4 
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, GET_UNIT
!
! --- Initialization
!
      L_WEI = 0
      DO 410 J1=1,LF_WEI
!
! ------ Check whether the file exists
!
         INQUIRE ( FILE=FILE_WEI(J1), EXIST=LEX )
         IF ( .NOT. LEX ) THEN
              CALL ERR_LOG ( 3561, IUER, 'READ_WEIGHTS', 'Weights file '// &
     &                       FILE_WEI(J1)(1:I_LEN(FILE_WEI(J1)))// &
     &                       ' was not found' )
              RETURN
         END IF
!
! ------ Get free logical unit and open the weights file
!
         LUN = GET_UNIT()
         OPEN ( UNIT=LUN, FILE=FILE_WEI(J1), STATUS='OLD', IOSTAT=IOS )
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH ( IOS, STR )
              CALL ERR_LOG ( 3562, IUER, 'READ_WEIGHTS', 'Error '// &
     &                       STR(1:I_LEN(STR))//' in attempt to open '// &
     &                      'weights file '//FILE_WEI(J1) )
              RETURN
         END IF
!
! ------ Now scan weights file
!
         DO 420 J2=1,M_WEI
!
! --------- Read the next line
!
            READ ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR
            IF ( IOS .EQ. -1 ) GOTO 810
            IF ( IOS .NE. 0 ) THEN
                 CALL CLRCH ( STR  )
                 CALL CLRCH ( STR2 )
                 CALL INCH  ( IOS, STR )
                 CALL INCH  ( J2, STR2 )
                 CALL ERR_LOG ( 3563, IUER, 'READ_WEIGHTS', 'Error '// &
     &                STR(1:I_LEN(STR))//' in reading line '// &
     &                STR2(1:I_LEN(STR2))//' of weights file '// &
     &                FILE_WEI(J1)(1:I_LEN(FILE_WEI(J1))) )
                 RETURN
            END IF
!
! --------- Bypass comments
!
            IF ( ILEN(STR) .EQ.  0  ) GOTO 420
            IF ( STR(1:1)  .EQ. '*' ) GOTO 420
            IF ( STR(1:1)  .EQ. '#' ) GOTO 420
!
            L_WEI = L_WEI + 1 ! conuters of valid (not-comments) weights records
!
! --------- Get superfile name
!
            CALL CHIN ( STR(1:8), IDAT_I4 ) 
            IF ( IDAT_I4 > 19700000  .AND.  IDAT_I4 < 21000000 ) THEN
                 SUPNAM_WEI(L_WEI) = STR
                 CONTINUE 
               ELSE 
                 SUPNAM_WEI(L_WEI) = '$'//STR(1:9)
            END IF
!
! --------- Get superfile version
!
            READ ( UNIT=STR(11:13), FMT='(I3)', IOSTAT=IOS ) SUPVER_WEI(L_WEI)
            IF ( IOS .NE. 0 ) THEN
                 CALL ERR_LOG ( 3564, IUER, 'READ_WEIGHTS', 'Error '// &
     &               'in decoding superfile version in line '// &
     &                STR(1:I_LEN(STR))//' of weights file '//FILE_WEI(J1) )
                 RETURN
            END IF
            IF ( WEIGHT_TYPE .EQ. 'A' ) THEN
                 CALL ERR_LOG ( 3565, IUER, 'READ_WEIGHTS', 'Weights type A '// &
     &               '(session-dependent) is not supported any more :-(' )
                 RETURN
               ELSE IF ( WEIGHT_TYPE .EQ. 'B' ) THEN
!
! -------------- Baseline weights file
!
                 IF ( STR(23:23) .NE. '/' ) THEN
                      CALL ERR_LOG ( 3566, IUER, 'READ_WEIGHTS', 'Ill-formated '// &
     &                    'line '//STR(1:I_LEN(STR))//' in weights file '// &
     &                     FILE_WEI(J1)(1:I_LEN(FILE_WEI(J1)))// &
     &                    ' it does not follow baseline-type format '// &
     &                    'specifications' )
                      RETURN
                 END IF
!
! -------------- Remove underscores and form the record for a baseline
!
!@U                 CALL UNDSCR ( STR(15:22) )
!@U                 CALL UNDSCR ( STR(24:31) )
!
                 CALL VTD_NAME_REPAIR ( STR(15:22) )
                 CALL VTD_NAME_REPAIR ( STR(24:31) )
                 BASELINE_WEI(L_WEI)=STR(15:22)//STR(24:31)
!
! -------------- Read weights values
!
                 READ ( UNIT=STR(32:71), FMT='(4F10.2)', IOSTAT=IOS) &
     &                ( ARR_WEI(NN,L_WEI), NN=1,4 )
                 IF ( IOS .NE. 0 ) THEN
                      CALL ERR_LOG ( 3567, IUER, 'READ_WEIGHTS', 'Error '// &
     &                    'in decoding weights values in line '// &
     &                     STR(1:I_LEN(STR))//' of weights file '//FILE_WEI(J1) )
                      RETURN
                 END IF
               ELSE IF ( WEIGHT_TYPE .EQ. 'S' ) THEN
!
! -------------- Station weights file
!
                 IF ( STR(29:29) .NE. '.' ) THEN
                      CALL ERR_LOG ( 3568, IUER, 'READ_WEIGHTS', 'Ill-formated '// &
     &                    'line '//STR(1:I_LEN(STR))//' in weights file '// &
     &                     FILE_WEI(J1)(1:I_LEN(FILE_WEI(J1)))// &
     &                    ' it does not follow site-type format '// &
     &                    'specifications' )
                      RETURN
                 END IF
!
! -------------- Site weights file have up to 4 concateated weight record.
! -------------- We shift first 14 characters and process 4 parts of the site
! -------------- record
!
                 CALL CLRCH  ( STR(1:14) )
                 CALL CHASHL ( STR       )
                 SUPNAM_KEEP = SUPNAM_WEI(L_WEI)
                 SUPVER_KEEP = SUPVER_WEI(L_WEI)
                 L_WEI = L_WEI-1 ! decrement records counter since this counter
!                             ! will be just incremented
                 DO 430 J3=1,4
                    IF ( STR(1:1) .NE. ' ' ) THEN
                         L_WEI = L_WEI + 1
!
! ---------------------- Copy superfile name and version from the previous record
!
                         SUPNAM_WEI(L_WEI) = SUPNAM_KEEP
                         SUPVER_WEI(L_WEI) = SUPVER_KEEP
!
! ---------------------- Replace undescores with blanks. Keep the second site
! ---------------------- of a "baseline" blank
!
!@U                         CALL UNDSCR ( STR(1:8) )
!@U                         BASELINE_WEI(L_WEI) = STR(1:8)//'        '
                         CALL VTD_NAME_REPAIR ( BASELINE_WEI(L_WEI)(1:8)   )
                         CALL VTD_NAME_REPAIR ( BASELINE_WEI(L_WEI)(10:17) )
!
! ---------------------- Read site weigths values
!
                         READ ( UNIT=STR(9:44), FMT='(4F9.2)', IOSTAT=IOS) &
     &                          ( ARR_WEI(NN,L_WEI), NN=1,4 )
                         IF ( IOS .NE. 0 ) THEN
                              CALL ERR_LOG ( 3569, IUER, 'READ_WEIGHTS', &
     &                            'Error in decoding weights values in '// &
     &                            'line '//STR(1:I_LEN(STR))// &
     &                            ' of weights file '//FILE_WEI(J1) )
                             RETURN
                         END IF
!
! ---------------------- Shift the line to the left at 45 characters
!
                         CALL CLRCH  ( STR(1:45) )
                         CALL CHASHL ( STR       )
                    END IF
 430             CONTINUE
               ELSE
                 CALL ERR_LOG ( 3570, IUER, 'READ_WEIGHTS', 'Unknown '// &
     &               'weights type '//WEIGHT_TYPE )
                 RETURN
           END IF
 420     CONTINUE
!
! ------ Normally we shoud not reach this place
!
         CALL CLRCH ( STR )
         CALL INCH  ( M_WEI, STR )
         CALL ERR_LOG ( 3571, IUER, 'READ_WEIGHTS', 'The number of records '// &
     &                 'in weights files '// &
     &                  FILE_WEI(1)(1:I_LEN(FILE_WEI(1)))//' '// &
     &                  FILE_WEI(2)(1:I_LEN(FILE_WEI(2)))//' '// &
     &                  FILE_WEI(3)(1:I_LEN(FILE_WEI(3)))//' '// &
     &                  FILE_WEI(4)(1:I_LEN(FILE_WEI(4)))//' '// &
     &                 ' exceeded the maximum value MAX4_WEIREC defined in '// &
     &                 'solve.templ as '//STR(1:I_LEN(STR))//' It is '// &
     &                 'a fatal sitation. You should either reduce the '// &
     &                 'lenght of your weight file or to change MAX4_WEIREC '// &
     &                 'in solve.templ and then ... recompile and relink '// &
     &                 'all Solve programs. Good luck' )
         RETURN
 810     CONTINUE
         CLOSE ( UNIT=LUN )
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  READ_WEIGHTS  !#!#
