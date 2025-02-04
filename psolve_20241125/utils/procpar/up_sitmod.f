      PROGRAM    UP_SITMOD
! ************************************************************************
! *                                                                      *
! *   Program  UP_SITMOD  updates station position mod-file.             *
! *                                                                      *
! *   First  file -- master file which should contain positions of ALL   *
! *                  stations.                                           *
! *   Second file -- result of getpar (with estension .sta) -- contains  *
! *                  station positions from Solve solution.              *
! *   Third  file -- reference catalogue of station positions.           *
! *   Fourth file -- replacements list of stations. The list of stations *
! *                  whose positions should be taken from the reference  *
! *                  catalogue.                                          *
! *   Fifth  file -- output file name.                                   *
! *                                                                      *
! *   $Ex/up_sitmod.e  \                                                 *
! *                    2009a_astro \                                     *
! *                    $Vs/2009a_astro/2009a_astro_apr.sit \             *
! *                    /tmp/2009a_astro_xs.sta \                         * 
! *                    $Vs/2009a_astro/2009a_nnr.list \                  *
! *                    keepITRFdef                                       *
! *                                                                      *
! *  ### 26-SEP-2001   UP_SITMOD   v1.6 (c)  L. Petrov  20-OCT-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  M_STA
      PARAMETER  ( M_STA = 1024 )
      CHARACTER  FIL_APR_STA*128, FIL_EST_STA*128, FIL_REF*128, FIL_REP*128, FIL_OUT*128
      CHARACTER  BUF_APR_STA(M_STA)*80, BUF_EST_STA(M_STA)*512, &
     &           BUF_REF(M_STA)*512, BUF_REP(M_STA)*80,  BUF_OUT(M_STA)*256, &
     &           COMMAND_LINE*512
      CHARACTER  SOL_NAME*16, SIT_EPOCH*10, STR*128
      INTEGER*4  N_APR_STA, N_EST_STA, N_REF, N_REP, NOUT
      CHARACTER  C_REP(M_STA)*8, STA_NAM*8, SOL_STR*16, REF_STR*16
      LOGICAL*4  FL_OVR
      REAL*8     SIT_X, SIT_Y, SIT_Z, SEC
      INTEGER*4  L_REP, IP, J1, J2, J3, J4, J5, J6, J7, IYEA, IMON, IDAY, &
     &           K_STA, K_REF, K_UPD, MJD, IUER
      CHARACTER, EXTERNAL :: GET_CDATE*19
      INTEGER*4, EXTERNAL :: I_LEN, ILEN, LINDEX, LTM_DIF
!
      FL_OVR = .FALSE.
      SOL_NAME = 'unknown'
!
      CALL CLRCH ( SIT_EPOCH )
      IF ( IARGC() .LT. 2 ) THEN
           WRITE ( 6, * ) 'Usage: up_sitmod apr_sit_file est_sit_file fil_out '// &
     &                   '[keepITRFdef sta_repl_file sta_ref_file]'
           CALL EXIT ( 1 )
         ELSE
           CALL GET_COMMAND ( COMMAND_LINE )
           CALL GETARG ( 1, FIL_APR_STA )
           CALL GETARG ( 2, FIL_EST_STA )
           CALL GETARG ( 3, FIL_OUT     )
           CALL GETARG ( 4, STR         )
           IF ( IARGC() .GE. 4 ) THEN
                CALL TRAN ( 12, STR, STR )
                IF ( STR == 'keepitrfdef' ) THEN
                     FL_OVR = .TRUE.
                   ELSE IF ( STR == 'nokeepitrfdef' ) THEN
                     FL_OVR = .FALSE.
                   ELSE
                     CALL ERR_LOG ( 1401, -2, 'UP_SITMOD', 'Wrong 4th '// &
     &                   'qualifier: '//STR(1:I_LEN(STR))// &
     &                   ' -- only nokeepITRFdef or keepITRFdef are allowed' )
                     CALL EXIT ( 1 )
                 END IF
            END IF
            IF ( FL_OVR .AND. IARGC() < 5 ) THEN
                CALL ERR_LOG ( 1402, -2, 'UP_SITMOD', 'Missing the 5th '// &
     &              'argument: station position replacement file '// &
     &              'from the reference catalogue' )
                CALL EXIT ( 1 )
            END IF
            IF ( FL_OVR .AND. IARGC() < 6 ) THEN
                CALL ERR_LOG ( 1403, -2, 'UP_SITMOD', 'Missing the 6th '// &
     &              'argument: station position reference file '// &
     &              'from the reference catalogue' )
                CALL EXIT ( 1 )
            END IF
            IF ( IARGC() .GE. 5 ) THEN
                 CALL GETARG ( 5, FIL_REP )
            END IF
            IF ( IARGC() .GE. 6 ) THEN
                 CALL GETARG ( 6, FIL_REF )
            END IF
      END IF
!
! --- Create short line with solution name and refrence catalogue names
!
      CALL CLRCH ( SOL_STR )
      CALL CLRCH ( REF_STR )
      SOL_STR = FIL_EST_STA(LINDEX( FIL_EST_STA, '/' )+1: )
      REF_STR = FIL_REF(LINDEX( FIL_REF, '/' )+1: )
!
! --- Read all files and put their contents in buffers
!
      IUER = -1
      CALL RD_TEXT ( FIL_APR_STA, M_STA, BUF_APR_STA, N_APR_STA, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( BUF_APR_STA(1)(1:33) .NE. '$$  SIT-MODFILE Format 2001.09.26' ) THEN
           CALL ERR_LOG ( 1403, -2, 'UP_VELMOD', 'Unsupported format of '// &
     &         'the input file for a priori site positions '//FIL_APR_STA )
           CALL EXIT ( 1 )
      END IF
!
      IUER = -1
      CALL RD_TEXT ( FIL_EST_STA, M_STA, BUF_EST_STA, N_EST_STA, IUER )
      IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
      IF ( BUF_EST_STA(1)(1:46) .NE. '# GETPAR_STA format version 1.1  of 2023.03.11' ) THEN
           CALL ERR_LOG ( 1404, -2, 'UP_SITMOD', 'Unsupported format of '// &
     &         'the input file for site position estimates '//FIL_EST_STA )
           CALL EXIT ( 1 )
      END IF
!
      IF ( FL_OVR ) THEN
           IUER = -1
           CALL RD_TEXT ( FIL_REF, M_STA, BUF_REF, N_REF, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
!
           IUER = -1
           CALL RD_TEXT ( FIL_REP, M_STA, BUF_REP, N_REP, IUER )
           IF ( IUER .NE. 0 ) CALL EXIT ( 1 )
         ELSE
           N_REP = 0
           N_REF = 0
      END IF
!
! --- Create the header of the output file
!
      NOUT = 1
      BUF_OUT(NOUT) = '$$  SIT-MODFILE Format 2001.09.26'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  EPOCH '//SOL_NAME
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  Generated with '//TRIM(COMMAND_LINE)
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  File was generated by up_sitmod at '//GET_CDATE()
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  VLBI station positions'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  ~~~~~~~~~~~~~~~~~~~~~~'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  Generated from solution unknown'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  '
      IF ( FL_OVR ) THEN
           NOUT = NOUT + 1
           BUF_OUT(NOUT) = '$$  Position of some stations a replaced by '// &
     &                     'values from '//FIL_REF(1:I_LEN(FIL_REF))
           NOUT = NOUT + 1
           BUF_OUT(NOUT) = '$$  They overwrote positions from the solution.'
           NOUT = NOUT + 1
           BUF_OUT(NOUT) = '$$  Positions which were not estimated are '// &
     &                     'taken from '//FIL_APR_STA(1:I_LEN(FIL_APR_STA))
           NOUT = NOUT + 1
           BUF_OUT(NOUT) = '$$  '
      END IF
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  Modifications:'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$  Who  When        What'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
      NOUT = NOUT + 1
      BUF_OUT(NOUT) = '$$'
!
! --- Now we copy contents of the master file into the output file
!
      DO 410 J1=1,N_APR_STA
         IF ( BUF_APR_STA(J1)(1:1)  .NE. ' ' ) GOTO 410
         NOUT = NOUT + 1
         BUF_OUT(NOUT) = BUF_APR_STA(J1)
         CALL CLRCH ( BUF_OUT(NOUT)(71:) )
!
! ------ Add comment word: apriori
!
         BUF_OUT(NOUT)(72:) = 'apriori'
 410  CONTINUE
!
! --- Now scan the output buffer again and look for the stations which are the
! --- same as in the solution buffers
!
      K_UPD = 0
      DO 420 J2=1,NOUT
         IF ( BUF_OUT(J2)(1:4) .EQ. '    ' ) THEN
              STA_NAM = BUF_OUT(J2)(5:13)
         END IF
         DO 430 J3=1,N_EST_STA  ! Cycle overa ll lines in solution buffer
            IF ( BUF_OUT(J2)(1:27) == '$$  Generated from solution' ) THEN
                 IF ( BUF_EST_STA(J3)(1:14) == '# Solution ID:' ) THEN
!
! ------------------- Copy the solution name
!
                      SOL_NAME = BUF_EST_STA(J3)(21:)
                      BUF_OUT(J2)(29:) = SOL_NAME 
                 END IF
            END IF
!
            IF ( BUF_OUT(J2)(1:17) == '$$  EPOCH unknown' ) THEN
                 IF ( BUF_EST_STA(J3)(1:26) == '# Position reference date:' ) THEN
!
! ------------------- Copy the solution epoch
!
                      BUF_OUT(J2)(11:29) = BUF_EST_STA(J3)(28:46)
                 END IF
            END IF
            IF ( BUF_OUT(J2)(1:1) .EQ. '$' ) GOTO 430
            IF ( BUF_EST_STA(J3)(1:7) .NE. 'STA_GCX' ) GOTO 430
!
! --------- Transforms the date of episodic motion from blanks to zero
!
            CALL VTD_NAME_REPAIR ( BUF_EST_STA(J3)(11:18) )
            CALL BLANK_TO_ZERO ( BUF_EST_STA(J3)(20:25) )
            IF ( BUF_EST_STA(J3)(11:18) .EQ. STA_NAM             .AND. &
     &           BUF_EST_STA(J3)(20:21) .EQ. BUF_OUT(J2)(63:64)  .AND. &
     &           BUF_EST_STA(J3)(22:23) .EQ. BUF_OUT(J2)(66:67)  .AND. &
     &           BUF_EST_STA(J3)(24:25) .EQ. BUF_OUT(J2)(69:70)        ) THEN
!
! -------------- Well. This is the same station and the same episodic event
! -------------- Read the data
!
                 READ ( UNIT=BUF_EST_STA(J3)(31:45),  FMT='(F15.5)'  ) SIT_X
                 READ ( UNIT=BUF_EST_STA(J3)(65:79),  FMT='(F15.5)'  ) SIT_Y
                 READ ( UNIT=BUF_EST_STA(J3)(99:113), FMT='(F15.5)'  ) SIT_Z
                 READ ( UNIT=BUF_EST_STA(J3)(20:21),  FMT='(I2)' ) IYEA
                 READ ( UNIT=BUF_EST_STA(J3)(22:23),  FMT='(I2)' ) IMON
                 READ ( UNIT=BUF_EST_STA(J3)(24:25),  FMT='(I2)' ) IDAY
!
! -------------- Write the data from solution buffer to the output buffer
!
                 CALL CLRCH ( BUF_OUT(J2) )
                 WRITE ( BUF_OUT(J2), FMT='(4X, A8, 3(F15.3, 1X ), 1X, &
     &                   3(1X,I2) )' ) STA_NAM, SIT_X/1000.0D0, SIT_Y/1000.0D0, &
     &                                 SIT_Z/1000.0D0, IYEA, IMON, IDAY
                 CALL BLANK_TO_ZERO ( BUF_OUT(J2)(63:64) )
                 CALL BLANK_TO_ZERO ( BUF_OUT(J2)(66:67) )
                 CALL BLANK_TO_ZERO ( BUF_OUT(J2)(69:70) )
!
! -------------- ... and put the solution name label
!
                 BUF_OUT(J2)(72:) = SOL_NAME
            END IF
            IF ( BUF_EST_STA(J3)(11:18) .EQ. STA_NAM ) THEN
                 BUF_OUT(J2)(72:) = SOL_NAME
                 IF ( BUF_OUT(J2)(63:70) == '00 00 00' ) THEN
                      K_UPD = K_UPD + 1
                 END IF
            END IF
 430     CONTINUE
 420  CONTINUE
!
! --- Now read the list of strong stations: L_REP, C_REP
!
      L_REP = 0
      IF ( N_REP > 0 ) THEN
           DO 440 J4=1,N_REP
              IF ( BUF_REP(J4)(1:1) .EQ. '*' ) GOTO 440
              IF ( BUF_REP(J4)(1:1) .EQ. '#' ) GOTO 440
              IF ( BUF_REP(J4)(1:1) .EQ. '$' ) GOTO 440
              IF ( BUF_REP(J4)(1:1) .EQ. '!' ) GOTO 440
              IF ( ILEN(BUF_REP(J4)) .LE. 0  ) GOTO 440
              CALL VTD_NAME_REPAIR ( BUF_REP(J4)(1:8) )
              L_REP = L_REP + 1
              C_REP(L_REP) = BUF_REP(J4)
 440       CONTINUE
      END IF
!
! --- Scan the output buffer the third time
!
      K_STA = 0
      K_REF = 0
      DO 450 J5=1,NOUT
         IF ( BUF_OUT(J5)(1:1)  .NE. ' ' ) THEN
!
! ----------- This is comment line. Prepend the prefix for propser sorting
!
              BUF_OUT(J5) = '@         '//BUF_OUT(J5)
              CALL INCH   ( J5, BUF_OUT(J5)(3:10) )  ! line number
              CALL CHASHR (     BUF_OUT(J5)(3:10) )
              GOTO 450
         END IF
!
! ------ Try to find the station freom this line in the list of strong stations
!
         STA_NAM = BUF_OUT(J5)(5:13)
         K_STA = K_STA + 1
         IP = LTM_DIF ( 0, L_REP, C_REP, STA_NAM )
         IF ( IP .GT. 0 ) THEN
!
! ----------- Wow! THis station is th strong one. Try to find its position in
! ----------- the reference catalogue
!
              IF ( N_REF > 0 ) THEN
                   DO 460 J6=1,N_REF
                      IF ( BUF_REF(J6)(1:1)  .NE. ' ' ) GOTO 460
                      CALL VTD_NAME_REPAIR ( BUF_REF(J6)(5:12) )
                      IF ( FL_OVR                                      .AND. &
     &                     BUF_REF(J6)(5:12)  .EQ. STA_NAM             .AND. &
     &                     BUF_REF(J6)(63:64) .EQ. BUF_OUT(J5)(63:64)  .AND. &
     &                     BUF_REF(J6)(66:67) .EQ. BUF_OUT(J5)(66:67)  .AND. &
     &                     BUF_REF(J6)(69:70) .EQ. BUF_OUT(J5)(69:70)        ) THEN
!
! ------------------------ Well, the station name and the date of episodic motuion
! ------------------------ are the same. Replace the lins with the line from the
! ------------------------ reference buffer
!
                           BUF_OUT(J5) = BUF_REF(J6)
                           CALL CLRCH ( BUF_OUT(J5)(71:) )
                           BUF_OUT(J5)(72:) = REF_STR
                           K_REF = K_REF + 1
                      END IF
 460               CONTINUE
              END IF
         END IF
!
! ------ Add the postfix which would allow to find the line after sorting
!
         BUF_OUT(J5)(1:1) = '|'
         CALL INCH   ( J5, BUF_OUT(J5)(101:108) )
         CALL CHASHR (     BUF_OUT(J5)(101:108) )
 450  CONTINUE
!
! --- Sort the list
!
      CALL SORT_CH ( NOUT, BUF_OUT )
!
! --- Open output file
!
      OPEN ( UNIT=11, FILE=FIL_OUT, STATUS='UNKNOWN' )
!
      DO 470 J7=1,NOUT
         IF ( ILEN(BUF_OUT(J7)) .LT. 11 ) GOTO 470
         IF ( BUF_OUT(J7)(1:1) .EQ. '@' ) THEN
!
! ----------- This was the header line. Remve the prefix and print it
!
              WRITE ( 11, FMT='(A)' ) BUF_OUT(J7)(11:I_LEN(BUF_OUT(J7)))
           ELSE IF ( BUF_OUT(J7)(1:1) .EQ. '|' ) THEN
!
! ----------- This was was the data line. Remove the postfix and print it
!
              WRITE ( 11, FMT='(A)' ) ' '// &
     &                BUF_OUT(J7)(2:I_LEN(BUF_OUT(J7)(1:100)))
           ELSE
!
! ----------- This is for testing. This should never happen
!
              WRITE ( 11, FMT='(A)' ) 'mumu: '// &
     &                BUF_OUT(J7)(1:I_LEN(BUF_OUT(J7)))
         END IF
 470  CONTINUE
      CLOSE ( UNIT=11 )
!
      WRITE ( 6, * ) 'Output file: ',FIL_OUT(1:I_LEN(FIL_OUT))
      WRITE ( 6, * ) 'Number of stations processed:           ', INT2(K_STA)
      WRITE ( 6, * ) 'Number of stations updated:             ', INT2(K_UPD-K_REF)
      WRITE ( 6, * ) 'Number of reference stations unchanged: ', INT2(K_REF)
!
      END  !#!  UP_SITMOD  #!#
