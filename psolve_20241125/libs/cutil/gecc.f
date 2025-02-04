      SUBROUTINE GECC ( ECCMAP_FILE, JFIRST_OBS, JLAST_OBS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine  GECC  gets values of eccentricity vectors for all         *
! *   stations participated in the session and puts them in the common   *
! *   block defined in flyby.i for futher mapping.                       *
! *                                                                      *
! *   It reads NAMFIL, extracts information about old eccentricity which *
! *   has been already applied. Then it reads mapping files, get new     *
! *   eccentricity vectors for all stations. Both set of vectors are     *
! *   transformed to XYZ crust-fixed system.                             *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! * ECCDAT_FILE ( CHARACTER ) -- Filename of the eccentricity file.      *
! *                              Eccentricity file should be in          *
! *                              ECC-format (POST_OCT99).                *
! *  JFIRST_OBS ( REAL*8    ) -- Julian date of the first observation of *
! *                              the session.                            *
! *   JLAST_OBS ( REAL*8    ) -- Julian date of the  last observation of *
! *                              the session.                            *
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
! *  pet  2000.06.14  Changed routines in according with changes in      *
! *                   the format of NAMFIL (one extra digit added for    *
! *                   eccentricity values ).                             *
! *                                                                      *
! *  pet  2000.07.26  Corrected a bug: the was a mess up with indices    *
! *                   of stations in the previous version.               *
! *                                                                      *
! *  pet  2001.01.11  Corrected a bug: MONU_NAME shold have length 10    *
! *                   symbols, not 8. Added putting monument names in    *
! *                   the data structure flyby.i                         *
! *                                                                      *
! *  ###  15-OCT-99       GECC     v1.2  (c)  L. Petrov 11-JAN-2001 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE   'solve.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'socom.i'
      INCLUDE   'prfil.i'
      INCLUDE   'bindisp.i'
      INCLUDE   'flyby.i'
!
      CHARACTER  ECCMAP_FILE*(*)
      REAL*8     JFIRST_OBS, JLAST_OBS
      INTEGER*4  IUER
!
      INTEGER*2  ICONT, IERR
      INTEGER*4  J1, J2, IOS1, IOS2, IOS3, L_STA, IL, IER
      CHARACTER  MONU_NAME(MAX_ARC_STA)*10, MONU_TYPE(MAX_ARC_STA)*2, JBUF*70, &
     &           OUT*1024, DBNAME*16, C_STA(MAX_ARC_STA)*8, &
     &           ECC_TYP(MAX_ARC_STA)*2
      REAL*8     ECC_ORIG(3,MAX_ARC_STA), ECC_ARR(3,MAX_ARC_STA)
      LOGICAL*2  NO_HIS
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INTEGER*4, EXTERNAL :: LTM_DIF
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
!
! --- Read INIT namfil-card to learn database name
!
      CALL GETCARD ( INT2(1), 'INIT', INT2(1), JBUF, IERR )
      IF ( IERR .LT. INT2(0) ) THEN
           WRITE ( 6, * ) ' GEC:  IERR=',IERR
           CALL ERR_LOG ( 651, IUER, 'GECC', 'Error in reading NAMFIL '// &
     &         'card INIT' )
           RETURN
      END IF
      DBNAME = JBUF(11:20)//'<'//JBUF(23:24)//'>'
!
      L_STA = 0
      DO 410 J1=1,MAX_ARC_STA
!
! ------ Read SITE namfil card
!
         IF ( J1 .EQ. 1 ) THEN
              ICONT = INT2(1)
            ELSE
              ICONT = INT2(0)
         END IF
!
         CALL GETCARD ( INT2(1), 'SITE', ICONT, JBUF, IERR )
         IF ( IERR .EQ. INT2(1) ) GOTO 810 ! No more cards.
         IF ( IERR .LT. INT2(0) ) THEN
              WRITE ( 6, * ) ' GEC: J1=',J1,' IERR=',IERR
              CALL ERR_LOG ( 652, IUER, 'GECC', 'Error in reading NAMFIL '// &
     &            'card SITE' )
              RETURN
         END IF
         L_STA = L_STA + 1
!
! ------ Extract station name
!
         C_STA(L_STA) = JBUF(6:13)
!
! ------ Extract eccentricity values
!
         READ ( UNIT=JBUF(25:35), FMT='(F11.5)', IOSTAT=IOS1 ) ECC_ARR(1,L_STA)
         READ ( UNIT=JBUF(36:46), FMT='(F11.5)', IOSTAT=IOS2 ) ECC_ARR(2,L_STA)
         READ ( UNIT=JBUF(47:57), FMT='(F11.5)', IOSTAT=IOS3 ) ECC_ARR(3,L_STA)
         IF ( IOS1 .NE. 0  .OR.  IOS2 .NE. 0  .OR.  IOS3 .NE. 0 ) THEN
              WRITE ( 6, * ) ' GEC: J1=',J1,' IOS1=',IOS1, ' IOS2=',IOS2, &
     &               ' IOS3=',IOS3
              CALL ERR_LOG ( 653, IUER, 'GECC', 'Error in decoding '// &
     &            'eccentricity for station '//C_STA(L_STA)//' in reading '// &
     &            'NAMFIL' )
              RETURN
         END IF
!
! ------ Extract eccentrisity type
!
         ECC_TYP(L_STA) = JBUF(59:60)
         IF ( ECC_TYP(L_STA) .NE. 'XY'  .AND.  ECC_TYP(L_STA) .NE. 'NE' ) THEN
              CALL ERR_LOG ( 654, IUER, 'GECC', 'Wrong eccentricity type: '// &
     &             ECC_TYP(L_STA)//' for the station '//C_STA(L_STA)// &
     &            ' was gotten in reading NAMFIL' )
              RETURN
         END IF
 410  CONTINUE
 810  CONTINUE
!
! --- Check: whether all stations has been read?
!
      IF ( L_STA .NE. INT4(NUMSTA) ) THEN
           WRITE ( 6, * ) ' NUMSTA(socom) =',NUMSTA,'  L_STA(namfil) =',L_STA
           CALL ERR_LOG ( 654, IUER, 'GECC', 'Number of sites in socom and '// &
     &                   'namfil is different' )
           RETURN
      END IF
!
! --- Read an external eccentricity file and extract an array of new values
! --- of eccentricity vector ECC_NEW
!
      NO_HIS = .TRUE.
      CALL ERR_PASS ( IUER, IER )
      CALL GET_ECCDAT ( ECCMAP_FILE, DBNAME, INT4(NUMSTA), ISITN_CHR, VSITEC, &
     &                  JFIRST_OBS, JLAST_OBS, NO_HIS, MONU_NAME, MONU_TYPE, &
     &                  ECC_ORIG, ECC_NEW, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 655, IUER, 'GECC', 'Error in attempt to get new '// &
     &         'values of station eccentricity vectors from mapping '// &
     &         'eccentricity file '//ECCMAP_FILE )
           RETURN
      END IF
!
! --- Check all stations and refine arrays of new and all eccentricity
! --- vectors. Lists of stations ISITN_CHR and list of stations in NAMFIL may
! --- be potentially in different order. So we have to take care of it.
!
      DO 420 J2=1,L_STA
!
! ------ Search the J2-th station in the ISITN_CHR list of station names
!
         IL = LTM_DIF ( 0, L_STA, ISITN_CHR, C_STA(J2) )
         IF ( IL .LE. 0 ) THEN
!
! ----------- Didn't find? Wow!
!
              CALL LIST_TO_LINE ( L_STA, ISITN_CHR, ', ', OUT )
              CALL ERR_LOG ( 656, IUER, 'GECC', 'Trap of internal control: '// &
     &                     'station '//C_STA(J2)//' was not found in '// &
     &                     'the station list isitn_chr from socom: '//OUT )
              RETURN
         END IF
!
         IF ( ECC_TYP(J2) .EQ. 'XY' ) THEN
              ECC_OLD(1,IL) = ECC_ARR(1,J2)
              ECC_OLD(2,IL) = ECC_ARR(2,J2)
              ECC_OLD(3,IL) = ECC_ARR(3,J2)
            ELSE IF ( ECC_TYP(J2) .EQ. 'NE' ) THEN
!
! ----------- Rotation of the old eccentricity vector from NEU to XYZ system
! ----------- Input vector  (NEU): ECC_ARR(x,il)
! ----------- Output vector (XYZ): ECC_OLD(x,IL)
!
              CALL KROT ( VSITEC(1,IL),  VSITEC(2,IL),  VSITEC(3,IL), &
     &                    ECC_ARR(3,J2), ECC_ARR(2,J2), ECC_ARR(1,J2), &
     &                    ECC_OLD(1,IL), ECC_OLD(2,IL), ECC_OLD(3,IL) )
         END IF
         MONU_NAME_NEW(IL) = MONU_NAME(J2)
 420  CONTINUE
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  GECC  #!#
