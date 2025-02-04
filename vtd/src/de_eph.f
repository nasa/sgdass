      SUBROUTINE READ_DE_EPH ( FILEPH, DE_EPH, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  READ_DE_EPH  reads the file of planetary ephemedires      *
! *   DE403 or DE405 from the original JPL file and stores it in the     *
! *   fields of object DE_EPH.                                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   FILEPH ( CHARACTER     ) -- Name of the ephemerides file.          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *   DE_EPH ( RECORD        ) -- object which keeps information         *
! *                               extracted from the DE403 or DE405      *
! *                               ephemerides file.                      *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
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
! *   References:                                                        *
! *   1) Standish E.M., Newhall X.X., Williams J.G., Folkner W.F.,       *
! *      1995, JPL IOM 314.10-127                                        *
! *   2) http://www.willbell.com/software/jpl.htm                        *
! *                                                                      *
! *  ### 24-JAN-2004   READ_DE_EPH  v1.2 (c) L. Petrov  04-JAN-2023 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'de440_eph.i'
      CHARACTER  FILEPH*(*)
      INTEGER*4  IUER
!
      TYPE ( DE440__TYPE      ) :: DE_EPH
      TYPE ( DE440_HEA__TYPE  ) :: HEA
      REAL*8     BUF(DE440__RECLEN)
      CHARACTER  STR*128
      REAL*8     F1
      LOGICAL*4  FL_ENC, LEX
      INTEGER*4  IC, J1, J2, J3, J4, J5, J6, J7, J8, J9, ISH, &
     &           REC_LEN, UNIX_DATE, LUN, N1, IER
      INTEGER*8  SIZE_I8
      INTEGER*4, EXTERNAL :: READ, INT8_TO_I4, I_LEN, LOC__SUN$$_STR
      LOGICAL*4  IS_R8_NAN
#ifdef LITTLE_ENDIAN
      FL_ENC = .FALSE.
#else
      FL_ENC = .TRUE.
#endif

! --- Initialization
!
      CALL NOUT ( SIZEOF(DE_EPH), DE_EPH )
!
! --- Check whether the file exists
!
      INQUIRE ( FILE=FILEPH, EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL ERR_LOG ( 9031, IUER, 'READ_DE_EPH', 'Planetary ephemerides '// &
     &         'file '//FILEPH(1:I_LEN(FILEPH))//' was not found' )
           RETURN 
      END IF
!
! --- Get information about the file
!
      CALL FILE_INFO ( FILEPH(1:I_LEN(FILEPH))//CHAR(0), UNIX_DATE, SIZE_I8 )
!
! --- Determine the record size
!
      IF ( MOD(INT8_TO_I4(SIZE_I8),DE440__RECLEN) .EQ. 0 ) THEN
           REC_LEN = DE440__RECLEN
        ELSE IF ( MOD(INT8_TO_I4(SIZE_I8),DE440__RECSH) .EQ. 0 ) THEN
           REC_LEN = DE440__RECSH
        ELSE 
           CALL CLRCH ( STR )
           CALL INCH  ( INT8_TO_I4(SIZE_I8), STR )
           CALL ERR_LOG ( 9032, IUER, 'READ_DE_EPH', 'The size of the '// &
     &         'plantary ephemerides file '//FILEPH(1:I_LEN(FILEPH))// &
     &         ' is not multiple of 8144'//STR )
           RETURN 
      END IF
!
! --- Open the binary file with ephemerides
!
      CALL ERR_PASS ( IUER, IER )
      CALL BINF_OPEN ( FILEPH, 'OLD', LUN, IER )
      IF ( IER .LT. 0 ) THEN
           CALL ERR_LOG ( 9033, IUER, 'READ_DE_EPH', 'Error during '// &
     &         'an attempt to open the file with DE_EPH epherides: '// &
     &          FILEPH )
           RETURN 
      END IF
!
! --- Read the first record. The first record is special one: it has the header
!
      IC = READ ( %VAL(LUN), HEA, %VAL(REC_LEN) )
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 9034, IUER, 'READ_DE_EPH', 'Error during '// &
     &         'reading the first record' )
           RETURN 
      END IF
!
! --- Now extract the fields from the header
!
!
! --- TIT -- name of the ephemerides
!
#ifdef SUN
      CALL LIB$MOVC3 (  84*3, %VAL(LOC__SUN$$_STR(HEA%TIT(1))),  DE_EPH%TIT(1) )
      CALL LIB$MOVC3 ( 400*6, %VAL(LOC__SUN$$_STR(HEA%CNAM(1))), DE_EPH%HEA_NAM(1) )
#else
      CALL LIB$MOVC3 (  84*3, %REF(HEA%TIT(1)),  %REF(DE_EPH%TIT(1)) )
      CALL LIB$MOVC3 ( 400*6, %REF(HEA%CNAM(1)), %REF(DE_EPH%HEA_NAM(1)) )
#endif
!
      IF ( HEA%TIT(1)(1:LEN(DE440_EPH__LABEL)) .NE. DE440_EPH__LABEL ) THEN
           STR = HEA%TIT(1)
           CALL TRAN ( 13, STR, STR )
           CALL ERR_LOG ( 9035, IUER, 'READ_DE_EPH', 'Wrong magic of the JPLE DE'// &
     &         'ephemeride file: got '//TRIM(STR)//', but expected '//DE440_EPH__LABEL )
           CALL BINF_CLOSE ( LUN, -2 )
           RETURN
      END IF
!
! --- DATE_BEG_JD and DATTE_END_JD -- Julian date of the first and the last 
! --- record
!
      ISH = 1
      CALL LIB$MOVC3 ( 8, HEA%ARR(ISH), DE_EPH%DATE_BEG_JD )
      IF ( FL_ENC ) CALL ENDIAN_CNV_R8 ( DE_EPH%DATE_BEG_JD  )
      ISH = ISH + 8
!
      CALL LIB$MOVC3 ( 8, HEA%ARR(ISH), DE_EPH%DATE_END_JD )
      IF ( FL_ENC ) CALL ENDIAN_CNV_R8 ( DE_EPH%DATE_END_JD  )
      ISH = ISH + 8
!
! --- STEP_DAY -- step in days between major records
!
      CALL LIB$MOVC3 ( 8, HEA%ARR(ISH), DE_EPH%STEP_DAY )
      IF ( FL_ENC ) CALL ENDIAN_CNV_R8 ( DE_EPH%STEP_DAY )
      ISH = ISH + 8
      IF ( NINT(DE_EPH%STEP_DAY) < 1 .OR. NINT(DE_EPH%STEP_DAY) > 64 ) THEN
!
! -------- An attempt to correct endianness of the file. 
! -------- Let us assume that DE_EPH%STEP_DAY is wrong because 
! -------- either endian conversion was not done when it is needed
! -------- or done when it is not needed
!
! -------- Flip endian converion flag
!
           FL_ENC = .NOT. FL_ENC 
!
! -------- Convert three field that we jus read
!
           CALL ENDIAN_CNV_R8 ( DE_EPH%DATE_BEG_JD  )
           CALL ENDIAN_CNV_R8 ( DE_EPH%DATE_END_JD  )
           CALL ENDIAN_CNV_R8 ( DE_EPH%STEP_DAY )
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR(1:18), FMT='(1D18.10)' ) DE_EPH%STEP_DAY 
           IF ( IS_R8_NAN ( DE_EPH%STEP_DAY ) ) THEN
                DE_EPH%STEP_DAY = 0.0
           END IF
!
! -------- Check again
!
           IF ( NINT(DE_EPH%STEP_DAY) < 1 .OR. NINT(DE_EPH%STEP_DAY) > 64 ) THEN
!
! ------------- Oh no! The problem is not with endianness, with something else
!
                CALL ERR_LOG ( 9035, IUER, 'READ_DE_EPH', 'Trap of internal '// &
     &              'control: variable time stamp from the ephemeride file '// &
     &              FILEPH(1:I_LEN(FILEPH))//' is out of range 1-64 days: '// &
     &              STR(1:I_LEN(STR))//' -- apparently the ephemeride file '// &
     &              'is damaged' )
                RETURN 
           END IF
      END IF
!
      CALL LIB$MOVC3 ( 4, HEA%ARR(25),   DE_EPH%NCON )
      IF ( FL_ENC ) CALL ENDIAN_CNV_I4 ( DE_EPH%NCON )
      ISH = ISH + 4
!
! --- AU -- astronomical unit
!
      CALL LIB$MOVC3 ( 8, HEA%ARR(29),   DE_EPH%AU )
      IF ( FL_ENC ) CALL ENDIAN_CNV_R8 ( DE_EPH%AU )
      ISH = ISH + 8
!
! --- EMRAT -- the ration of the Earth to Moon mass
!
      CALL LIB$MOVC3 ( 8, HEA%ARR(37),   DE_EPH%EMRAT )
      IF ( FL_ENC ) CALL ENDIAN_CNV_R8 ( DE_EPH%EMRAT )
!
! --- Extract array IPT -- 
!
      ISH = ISH + 8
      ISH = 45
      DO 410 J1=1,12
         DO 420 J2=1,3
            CALL LIB$MOVC3 ( 4, HEA%ARR(ISH),  DE_EPH%IPT(J2,J1)  )
            IF ( FL_ENC ) CALL ENDIAN_CNV_I4 ( DE_EPH%IPT(J2,J1) )
            ISH = ISH + 4
 420     CONTINUE 
 410  CONTINUE 
      CALL LIB$MOVC3 ( 4, HEA%ARR(ISH),  DE_EPH%NUMDE  )
      IF ( FL_ENC ) CALL ENDIAN_CNV_I4 ( DE_EPH%NUMDE )
      ISH = ISH + 4
!
      DO 430 J3=1,3
         CALL LIB$MOVC3 ( 4, HEA%ARR(ISH),  DE_EPH%IPT(J3,13)  )
         IF ( FL_ENC ) CALL ENDIAN_CNV_I4 ( DE_EPH%IPT(J3,13) )
         ISH = ISH + 4
 430  CONTINUE 
      CALL LIB$MOVC3 ( (DE440__MHEAD-DE440__MTOES)*6, HEA%ARR(ISH), &
     &                 DE_EPH%HEA_NAM(DE440__MTOES+1) )
      ISH = ISH + (DE440__MHEAD-DE440__MTOES)*6
!
      DO 440 J4=1,3
         CALL LIB$MOVC3 ( 4, HEA%ARR(ISH),  DE_EPH%IPT(J4,14)  )
         IF ( FL_ENC ) CALL ENDIAN_CNV_I4 ( DE_EPH%IPT(J4,14) )
         ISH = ISH + 4
 440  CONTINUE 
!
      DO 450 J5=1,3
         CALL LIB$MOVC3 ( 4, HEA%ARR(ISH),  DE_EPH%IPT(J5,15)  )
         IF ( FL_ENC ) CALL ENDIAN_CNV_I4 ( DE_EPH%IPT(J5,15) )
         ISH = ISH + 4
 450  CONTINUE 
!
! --- Read the next record
!
#ifdef SUN
      IC = READ ( %VAL(LUN), %VAL(LOC__SUN$$_STR(BUF)), %VAL(REC_LEN) )
#else
      IC = READ ( %VAL(LUN), %REF(BUF), %VAL(REC_LEN) )
#endif
      IF ( IC .LT. 0 ) THEN
           CALL CLRCH   ( STR )
           CALL GERROR  ( STR )
           CALL ERR_LOG ( 9036, IUER, 'READ_DE_EPH', 'Error during '// &
     &         'reading the record' )
           RETURN 
      END IF
!
! --- Put contents of the record in the header
!
      DO 460 J6=1,DE440__MHEAD
#ifdef GNU
         CALL LIB$MOVC3 ( 8, BUF(J6), DE_EPH%HEA_VAL(J6) )
#else
         DE_EPH%HEA_VAL(J6) = BUF(J6)
#endif
         IF ( FL_ENC ) CALL ENDIAN_CNV_R8 ( DE_EPH%HEA_VAL(J6) )
 460  CONTINUE 
!
! --- Read all other records with Chebyechev coeffients and put them in
! --- the field BUF
!
      DO 470 J7=1,DE440__NREC
         IC = READ ( %VAL(LUN), DE_EPH%BUF(1,J7), %VAL(REC_LEN) )
         IF ( IC .LT. 0 ) THEN
              CALL CLRCH   ( STR )
              CALL GERROR  ( STR )
              CALL ERR_LOG ( 9037, IUER, 'READ_DE_EPH', 'Error during '// &
     &            'reading the record' )
              CALL EXIT ( 1 )
         END IF
         IF ( IC .LE. 0 ) GOTO 870
!
         IF ( FL_ENC ) THEN
              DO 480 J8=1,REC_LEN/8
                 CALL ENDIAN_CNV_R8 ( DE_EPH%BUF(J8,J7) )
 480          CONTINUE 
         END IF 
 470  CONTINUE 
 870  CONTINUE 
!
! --- Close the file
!
      CALL BINF_CLOSE ( LUN, -2 )
      DE_EPH%STATUS = DE440__LOADED
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE READ_DE_EPH
!
! ------------------------------------------------------------------------
!
      SUBROUTINE PLANETA_DE_EPH ( DE_EPH, MJD, TAI, NAM_PLAN, COO, VEL, ACC, &
     &                            IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  PANETA_DE_EPH extracts interpolating  coefficients from   *
! *   the epherides DE_EPH for the specified celestial body and computes *
! *   baricentric coordinate, velocity and acceleration at the specified *
! *   moment of time. It is assumed that routine READ_DE_EPH was called  *
! *   before call of PLANETA_DE_EPH.                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    DE_EPH ( RECORD   ) -- object which keeps information extracted   *
! *                           from the DE ephemerides file.              *
! *      MJD ( INTEGER*4 ) -- Modified Julian date of the midnight of    *
! *                           the date of interest.                      *
! *      TAI ( REAL*8    ) -- Time elapsed from the midnight at TAI      *
! *                           scale of the the date of interest.         *
! * NAM_PLAN ( CHARACTER ) -- Name of the celestial body. One of         *
! *                           'MERCURY', 'VENUS', 'EARTH', 'MARS',       *
! *                           'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE',  *
! *                           'PLUTO', 'MOON', 'SUN', or 'LIBRATIONS'    *
! *                           for Lunar librations.                      *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      COO ( REAL*8    ) -- Vector of coordinates of the celestial     *
! *                           body wrt the barycenter of the Solar       *
! *                           System. Dimension: 3. Units: meters.       *
! *      VEL ( REAL*8    ) -- Vector of velocities of the celestial      *
! *                           body wrt the barycenter of the Solar       *
! *                           System. Dimension: 3. Units: m/s.          *
! *      ACC ( REAL*8    ) -- Vector of accelerations of the celestial   *
! *                           body wrt the barycenter of the Solar       *
! *                           System. Dimension: 3. Units: m/s^2.        *
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
! *  ### 25-JAN-2004  PLANETA_DE_EPH  v1.2 (c) L. Petrov 18-JAN-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'de440_eph.i'
      TYPE      ( DE440__TYPE ) :: DE_EPH
      INTEGER*4  MJD, IUER
      CHARACTER  NAM_PLAN*(*)
      REAL*8     TAI, COO(3), VEL(3), ACC(3)
      CHARACTER  NAME_BODY*16
      REAL*8     TDB, JD(2), COO_EA(3), VEL_EA(3), ACC_EA(3), COO_MO(3), &
     &           VEL_MO(3), ACC_MO(3)
      INTEGER*4  IND_BODY, J1, J2, IER
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( DE_EPH%STATUS .NE. DE440__LOADED ) THEN
           CALL ERR_LOG ( 1231, IUER, 'PLANETA_DE_EPH', 'It looks like '// &
     &         'epherides DE_EPH has not yet been read. Check whether you '// &
     &         'have called READ_DE_EPH' )
           RETURN 
      END IF
!
! --- Transform time from TAI to TDB, since TDB is the argument of DE_EPH
!
      CALL TAI_TO_TDB ( MJD, TAI, TDB )
      JD(1) = MJD + 2400000.5D0
      JD(2) = TDB/86400.0D0
!
! --- Transform the name of the celestial body to the upper case.
!
      CALL CLRCH ( NAME_BODY )
      CALL TRAN  ( 11, NAM_PLAN, NAME_BODY )
!
! --- Find the index of the celstial body
!
      IF ( NAME_BODY .EQ. 'MERCURY' ) THEN
           IND_BODY = 1
        ELSE IF ( NAME_BODY .EQ. 'VENUS' ) THEN
           IND_BODY = 2
        ELSE IF ( NAME_BODY .EQ. 'EARTH' ) THEN
           IND_BODY = 3
        ELSE IF ( NAME_BODY .EQ. 'MARS' ) THEN
           IND_BODY = 4
        ELSE IF ( NAME_BODY .EQ. 'JUPITER' ) THEN
           IND_BODY = 5
        ELSE IF ( NAME_BODY .EQ. 'SATURN' ) THEN
           IND_BODY = 6
        ELSE IF ( NAME_BODY .EQ. 'URANUS' ) THEN
           IND_BODY = 7
        ELSE IF ( NAME_BODY .EQ. 'NEPTUNE' ) THEN
           IND_BODY = 8
        ELSE IF ( NAME_BODY .EQ. 'PLUTO' ) THEN
           IND_BODY = 9
        ELSE IF ( NAME_BODY .EQ. 'MOON' ) THEN
           IND_BODY = 10
        ELSE IF ( NAME_BODY .EQ. 'SUN' ) THEN
           IND_BODY = 11
        ELSE IF ( NAME_BODY(1:3) .EQ. 'LIB' .OR. NAME_BODY(1:3) .EQ. 'LBR' ) THEN
           IND_BODY = 13
        ELSE 
           CALL ERR_LOG ( 1232, IUER, 'PLANETA_DE_EPH', 'Wrong 4-th '// &
     &         'argument: '//NAM_PLAN(1:I_LEN(NAM_PLAN))// &
     &         ' -- name of the celestial body was not recognized' )
           RETURN 
      END IF
!
      IF ( IND_BODY .EQ. 3 ) THEN
!
! -------- In the case of the Earth, we should first interpolate position of
! -------- the baricenter EARTH/MOON, then compute position of the Moon wrt
! -------- the the baricenter Earth/Moon and then get position of the Earth
! -------- wrt baricenter of the Solar system
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL INTRP_DE_EPH ( DE_EPH, 3, JD, COO_EA, VEL_EA, ACC_EA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1233, IUER, 'PLANETA_DE_EPH', 'Error in '// &
     &              'an attempt to interpolate coordinates of the Earth '// &
     &              'on the requested epoch' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL INTRP_DE_EPH ( DE_EPH, 10, JD, COO_MO, VEL_MO, ACC_MO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1234, IUER, 'PLANETA_DE_EPH', 'Error in '// &
     &              'an attempt to interpolate coordinates of the Moon '// &
     &              'on the requested epoch' )
                RETURN 
           END IF
           DO 410 J1=1,3
              COO(J1) = COO_EA(J1) - COO_MO(J1)/(1.0D0 + DE_EPH%EMRAT)
              VEL(J1) = VEL_EA(J1) - VEL_MO(J1)/(1.0D0 + DE_EPH%EMRAT)
              ACC(J1) = ACC_EA(J1) - ACC_MO(J1)/(1.0D0 + DE_EPH%EMRAT)
 410       CONTINUE 
        ELSE IF ( IND_BODY .EQ. 10 ) THEN
!
! -------- In the case of the Moon we should first interpoate position of
! -------- the baricenter EARTH/MOON, then compute position of the Moon wrt
! -------- the the baricenter Earth/Moon and then get position of the Moon
! -------- wrt baricenter of the Solar system
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL INTRP_DE_EPH ( DE_EPH, 3, JD, COO_EA, VEL_EA, ACC_EA, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1235, IUER, 'PLANETA_DE_EPH', 'Error in '// &
     &              'an attempt to interpolate coordinates of the Earth '// &
     &              'on the requested epoch' )
                RETURN 
           END IF
!
           CALL ERR_PASS ( IUER, IER ) 
           CALL INTRP_DE_EPH ( DE_EPH, 10, JD, COO_MO, VEL_MO, ACC_MO, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1236, IUER, 'PLANETA_DE_EPH', 'Error in '// &
     &              'an attempt to interpolate coordinates of the Moon '// &
     &              'on the requested epoch' )
                RETURN 
           END IF
           DO 420 J2=1,3
              COO(J2) = COO_EA(J2) + COO_MO(J2)*DE_EPH%EMRAT/(1.0D0+DE_EPH%EMRAT)
              VEL(J2) = VEL_EA(J2) + VEL_MO(J2)*DE_EPH%EMRAT/(1.0D0+DE_EPH%EMRAT)
              ACC(J2) = ACC_EA(J2) + ACC_MO(J2)*DE_EPH%EMRAT/(1.0D0+DE_EPH%EMRAT)
 420       CONTINUE 
        ELSE IF ( IND_BODY .EQ. 13 ) THEN
          CALL ERR_PASS ( IUER, IER ) 
           CALL INTRP_DE_EPH ( DE_EPH, IND_BODY, JD, COO, VEL, ACC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1237, IUER, 'PLANETA_DE_EPH', 'Error in '// &
     &              'an attempt to interpolate lunar librations on the '// &
     &              'requested epoch' )
                RETURN 
           END IF
!
! -------- Routine INTRP_DE_EPH scales the ouput by 1.D3 to transform
! -------- km to m. Since librations are in radians, we need undo it as well
!
           COO = 1.0D-3*COO
           VEL = 1.0D-3*VEL 
           ACC = 1.0D-3*ACC
        ELSE 
           CALL ERR_PASS ( IUER, IER ) 
           CALL INTRP_DE_EPH ( DE_EPH, IND_BODY, JD, COO, VEL, ACC, IER )
           IF ( IER .NE. 0 ) THEN
                CALL ERR_LOG ( 1237, IUER, 'PLANETA_DE_EPH', 'Error in '// &
     &              'an attempt to interpolate coordinates of '// &
     &               NAM_PLAN(1:I_LEN(NAM_PLAN))//' on the requested epoch' )
                RETURN 
           END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE PLANETA_DE_EPH 
!
! ------------------------------------------------------------------------
!
      SUBROUTINE INTRP_DE_EPH ( DE_EPH, IND_BODY, JD, COO, VEL, ACC, IUER )
! ************************************************************************
! *                                                                      *
! *   Auxiliary Routine  INTRP_DE_EPH  performs interpolation with using *
! *   Chebyshev polynomials of the celestial body at the Julian date JD. *
! *   It takes coefficients of the Chebyshev polynomials from the DE_EPH *
! *   ephemerides, which are assumed to be read from the original DE_EPH *
! *   JPL ephemerides file.                                              *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *    DE_EPH ( RECORD    ) -- object which keeps information extracted  *
! *                           from the DE_EPH ephemerides file.          *
! * IND_BODY ( INTEGER*4 ) -- Internal DE_EPH index of the celestial     *
! *                           body.                                      *
! *       JD ( REAL*8    ) -- Arrays of Julian dates of the moment of    *
! *                           interest in TDB. Dimension: 2.             *
! *                           Units: days. The first elemetn of the      *
! *                           array is a Julian day of the midnight,     *
! *                           the second element is a fraction of a day  *
! *                           elapsed since midnight.                    *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      COO ( REAL*8    ) -- Vector of coordinates of the celestial     *
! *                           body wrt the barycenter of the Solar       *
! *                           System. Dimension: 3. Units: meters.       *
! *      VEL ( REAL*8    ) -- Vector of velocities of the celestial      *
! *                           body wrt the barycenter of the Solar       *
! *                           System. Dimension: 3. Units: m/s.          *
! *      ACC ( REAL*8    ) -- Vector of accelerations of the celestial   *
! *                           body wrt the barycenter of the Solar       *
! *                           System. Dimension: 3. Units: m/s^2.        *
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
! * ### 24-JAN-2004   INTRP_DE_EPH  v1.2 (c)  L. Petrov  27-OCT-2023 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'de440_eph.i'
      TYPE     ( DE440__TYPE ) :: DE_EPH
      INTEGER*4  IND_BODY, IUER
      REAL*8     JD(2), COO(3), VEL(3), ACC(3)
      INTEGER*4  IND_REC, IND_SUBREC, IND_EPH(3), J1, IER
      REAL*8     JD_0, DT_JD, TIM_JD, EPS_JD
      PARAMETER  ( EPS_JD  = 0.001D0/86400.0D0 ) 
      REAL*8     CHINT, CHIV1, CHIV2 
      CHARACTER  STR*128, STR1*32, STR2*32, STR3*32
      CHARACTER, EXTERNAL :: JD_TO_DATE*30
      INTEGER*4, EXTERNAL :: I_LEN
!
      IF ( DE_EPH%STATUS .NE. DE440__LOADED ) THEN
           CALL ERR_LOG ( 1241, IUER, 'INTRP_DE_EPH', 'It looks like '// &
     &         'epherides DE_EPH has not yet been read. Check whether you '// &
     &         'have called READ_DE_EPH' )
           RETURN 
      END IF
!
! --- Check validity of JD dates
!
      IF ( JD(1) + JD(2) .LT. DE_EPH%DATE_BEG_JD - EPS_JD ) THEN
           WRITE ( 6, * ) ' JD=',JD
           WRITE ( 6, * ) ' DE_EPH%DATE_BEG_JD = ', DE_EPH%DATE_BEG_JD 
           WRITE ( 6, * ) ' DE_EPH%DATE_END_JD = ', DE_EPH%DATE_END_JD 
           WRITE ( 6, * ) ' DE_EPH%STEP_DAY    = ', DE_EPH%STEP_DAY
           CALL ERR_LOG ( 1242, IUER, 'INTRP_DE_EPH', 'The ephemerides '// &
     &         'does not have data for the requested date. The reuqested '// &
     &         'date is too early' )
           RETURN 
         ELSE IF ( JD(1) + JD(2) .LT. DE_EPH%DATE_BEG_JD ) THEN
           JD(1) = DE_EPH%DATE_BEG_JD 
           JD(2) = 0.0D0
      END IF
!
      IF ( JD(1) + JD(2) .GT. DE_EPH%DATE_END_JD + EPS_JD ) THEN
           WRITE ( 6, * ) ' JD=',JD
           WRITE ( 6, * ) ' DE_EPH%DATE_BEG_JD = ', DE_EPH%DATE_BEG_JD 
           WRITE ( 6, * ) ' DE_EPH%DATE_END_JD = ', DE_EPH%DATE_END_JD 
           WRITE ( 6, * ) ' DE_EPH%STEP_DAY    = ', DE_EPH%STEP_DAY
           CALL ERR_LOG ( 1243, IUER, 'INTRP_DE_EPH', 'The ephemerides '// &
     &         'does not have data for the requested date. Teh reuqested '// &
     &         'date is too old' )     
           RETURN                      
         ELSE IF ( JD(1) + JD(2) .GT. DE_EPH%DATE_END_JD ) THEN
           JD(1) = DE_EPH%DATE_END_JD 
           JD(2) = 0.0D0
      END IF
!
! --- Compute the record index
!
      IND_REC = ( (JD(1) - DE_EPH%DATE_BEG_JD) +JD(2) )/DE_EPH%STEP_DAY + 1
      IF ( IND_REC < 1 .OR. IND_REC > DE440__NREC ) THEN
           WRITE ( 6, * ) ' JD=',JD
           WRITE ( 6, * ) ' DE_EPH%DATE_BEG_JD = ', DE_EPH%DATE_BEG_JD 
           WRITE ( 6, * ) ' DE_EPH%DATE_END_JD = ', DE_EPH%DATE_END_JD 
           WRITE ( 6, * ) ' DE_EPH%STEP_DAY    = ', DE_EPH%STEP_DAY
           STR  = JD_TO_DATE ( JD(1), IER )
           STR1 = JD_TO_DATE ( DE_EPH%DATE_BEG_JD, IER )
           STR2 = JD_TO_DATE ( DE_EPH%DATE_END_JD, IER )
           CALL INCH ( NINT((DE_EPH%DATE_END_JD - DE_EPH%DATE_BEG_JD)/DE_EPH%STEP_DAY), STR3 )
           CALL ERR_LOG ( 1244, IUER, 'INTRP_DE_EPH', 'Trap if of internal '// &
     &         'control: the ephemerides file is too long. It spans from '// &
     &          STR1(1:10)//' to '//STR2(1:10)//'. You need increase NREC '// &
     &         'parameter  in de440_eph.i  to at least '//TRIM(STR3)// &
     &         ' and rebuild VTD' )
           RETURN
      END IF
!
! --- Compute the sub-record index
!
      IND_SUBREC = ( (JD(1) - DE_EPH%DATE_BEG_JD) + JD(2) - &
     &               (IND_REC-1)*DE_EPH%STEP_DAY )/ &
     &               (DE_EPH%STEP_DAY/DE_EPH%IPT(3,IND_BODY)) + 1
      JD_0   = DE_EPH%DATE_BEG_JD + (IND_REC-1)*DE_EPH%STEP_DAY + &
     &               (IND_SUBREC-1)*DE_EPH%STEP_DAY/DE_EPH%IPT(3,IND_BODY)
      DT_JD  = DE_EPH%STEP_DAY/DE_EPH%IPT(3,IND_BODY) 
      TIM_JD = (JD(1) - JD_0) + JD(2)
      IF ( TIM_JD < 1.0D-30 ) TIM_JD = 10.D-14
!
      IND_EPH(1) = DE_EPH%IPT(1,IND_BODY) + &
     &             (IND_SUBREC-1)*DE_EPH%IPT(2,IND_BODY)*3
      IND_EPH(2) = IND_EPH(1) + DE_EPH%IPT(2,IND_BODY)
      IND_EPH(3) = IND_EPH(1) + DE_EPH%IPT(2,IND_BODY)*2
!
! --- Call routines for Chebyshev interpolation and transfrom units to SI
!
      DO 410 J1=1,3
         IER = -1
         COO(J1) = CHINT ( DE_EPH%IPT(2,IND_BODY), 0.0D0, DT_JD, TIM_JD, &
     &                     DE_EPH%BUF(IND_EPH(J1),IND_REC), IER )*1.0D3
         IER = -1
         VEL(J1) = CHIV1 ( DE_EPH%IPT(2,IND_BODY), 0.0D0, DT_JD, TIM_JD, &
     &                     DE_EPH%BUF(IND_EPH(J1),IND_REC), IER )*1.0D3/86400.0D0
         IER = -1
         ACC(J1) = CHIV2 ( DE_EPH%IPT(2,IND_BODY), 0.0D0, DT_JD, TIM_JD, &
     &                     DE_EPH%BUF(IND_EPH(J1),IND_REC), IER )* &
     &                                                       1.0D3/86400.0D0**2
 410  CONTINUE 
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  INTRP_DE_EPH  #!#
