      SUBROUTINE WRI_HARPOS ( L_HSP, HSP, HARPOS_VERS, AREA_RADIUS, FILOUT, &
     &                        IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRI_HARPOS  writes the file in HARPOS format which         *
! *   describes harmonic displacements of a set of sites: site apriori   *
! *   coordinates, geocentric latitute, longitude, height above the      *
! *   ellipsoid; harmonics names phases and frequencies; sine and        *
! *   cosine ampitudes for Up, East and North components of position     *
! *   variations for each site.                                          *
! *                                                                      *
! *   Caveat: the list of statsions for diffrent harmonics should be     *
! *           the same, otherewise the error will be detected. This is   *
! *           the limitation of HARPOS format.                           *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *       L_HSP ( INTEGER*4 ) -- The number of harmonics for which       *
! *                              position variations were estimated.     *
! *         HSP ( RECORD    ) -- Array of objects which keep information *
! *                              about estimates of harmonics site       *
! *                              position variations and their           *
! *                              covariance matrix.                      *     
! * HARPOS_VERS ( CHARACTER ) -- The string with program name and        *
! *                              version date.                           *
! * AREA_RADIUS ( REAL*8    ) -- The radius of the area for which the    *
! *                              displacement is valid. Units: meters.   *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *      FILOUT ( CHARACTER ) -- Name of the output HAROS file.          *
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
! *  ### 23-MAR-2005   WRI_HARPOS  v1.1 (c)  L. Petrov  28-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'hsp.i'
      INCLUDE   'vtd.i'
      INTEGER*4  L_HSP, IUER
      CHARACTER  HARPOS_VERS*(*), FILOUT*(*)
      TYPE     ( HSP__TYPE ) :: HSP(L_HSP)
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 512 )
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  SYSNAME*128,   NODENAME*128,      HARDWARE*128 
      CHARACTER  FORMAT_DSC*128, STR*128, BUF(MBUF)*128
      REAL*8     LONG, PP, LAT_GCN, RAD, MU, LAT_GDT, HEI_ELL, SCL(2), &
     &           AREA_RADIUS 
      INTEGER*4  IOS, NBUF, J1, J2, J3, J4, J5, J6, IND_COV, LUN, IER
      TYPE ( HARPOS__H_RECORD ) :: HREC
      TYPE ( HARPOS__A_RECORD ) :: AREC
      TYPE ( HARPOS__S_RECORD ) :: SREC
      TYPE ( HARPOS__D_RECORD ) :: DREC
      CHARACTER, EXTERNAL :: GET_CDATE*19, JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN, ILEN
      REAL*8,    EXTERNAL :: ATAN_CS
!
! --- GEt information about the user and about the system
!
      CALL GETINFO_USER   ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
! --- Open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FILOUT, STATUS='UNKNOWN', IOSTAT=IOS ) 
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 1751, IUER, 'WRI_HARPOS', 'Error in an attempt '// &
     &         'to open the output file '//FILOUT )
           RETURN 
      END IF
!
! --- Write the header label
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) HARPOS__LABEL
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 1752, IUER, 'WRI_HARPOS', 'Error in an attempt '// &
     &         'to write in the output file '//FILOUT )
           RETURN 
      END IF
!
! --- Write the header comment: who, where, how
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Created by '// &
     &                               HARPOS_VERS(1:I_LEN(HARPOS_VERS))
      WRITE ( UNIT=LUN, FMT='(A)' ) '#       run by '// &
     &                               USER_REALNAME(1:I_LEN(USER_REALNAME))// &
     &        ' ( '//USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#           on '// &
     &               NODENAME(1:I_LEN(NODENAME))//' at '// &
     &               GET_CDATE()//' local time'
!
! --- Build the name of the format description file
!
      CALL GETENV ( SOLVE_HELP_DIR, STR )
      FORMAT_DSC = SOLVE_HELP_DIR//'/harpos_format.txt'
!
! --- Write section delimieter
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '#============================ Beginning of format description: ================='
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Read format description into the buffer buf
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FORMAT_DSC, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1753, IUER, 'WRI_HARPOS', 'Error in an attempt '// &
     &         'to read BSPPOS format description from file '//FORMAT_DSC )
           RETURN 
      END IF
!
! --- Write contents of the description buffer into comment section
!
      DO 410 J1=1,NBUF
         WRITE ( UNIT=LUN, FMT='(A)' ) '# '//BUF(J1)(1:I_LEN(BUF(J1)))
 410  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '#============================ End of format description: ======================='
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Harmonic   Phase          Frequency           Acceleration'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      DO 420 J2=1,L_HSP
!
! ------ Prepare the H-record of the ouput file file with harmonic definition
!
         CALL CLRCH ( STR )
         CALL LIB$MOVC3 ( LEN__H_REC, %REF(STR), HREC )
         HREC%REC_ID = 'H'
         HREC%WAVE_ID = HSP(J2)%HAR_NAME
         WRITE ( UNIT=HREC%PHASE, FMT='(1PD13.6)'  ) HSP(J2)%STA(1)%PHASE
         WRITE ( UNIT=HREC%FREQ,  FMT='(1PD19.12)' ) HSP(J2)%STA(1)%FREQ
         WRITE ( UNIT=HREC%ACCEL, FMT='(1PD10.3)'  ) 0.0D0
         CALL LIB$MOVC3 ( LEN__H_REC, HREC, %REF(STR) )
!
! ------ Write down the H-record
!
         WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:LEN__H_REC)
!
         IF ( HSP(J2)%L_STA .NE. HSP(1)%L_STA ) THEN
              CALL ERR_LOG ( 1754, IUER, 'WRI_HARPOS', 'The number of sites '// &
     &            'for harmonic '//HSP(J2)%HAR_NAME//' and '//HSP(1)%HAR_NAME// &
     &            ' is different' )
              RETURN 
         END IF
         DO 430 J3=1,HSP(J2)%L_STA 
            IF ( HSP(J2)%C_STA(J3) .NE. HSP(1)%C_STA(J3) ) THEN
                 CALL ERR_LOG ( 1755, IUER, 'WRI_HARPOS', 'The list of '// &
     &            'sites for harmonic '//HSP(J2)%HAR_NAME//' and '// &
     &             HSP(1)%HAR_NAME//' is different' )
                 RETURN 
            END IF
 430     CONTINUE 
 420  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Displacement is applicable for close stations located within this distance:'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
! --- Prepare the A-record
!
      CALL CLRCH ( STR )
      CALL LIB$MOVC3 ( LEN__A_REC, %REF(STR), AREC )
      AREC%REC_ID = 'A'
      WRITE ( UNIT=AREC%AREA_RD, FMT='(F13.5)'  ) AREA_RADIUS
      CALL LIB$MOVC3 ( LEN__A_REC, AREC, %REF(STR) )
!
! --- Write down the A-record
!
      WRITE ( UNIT=LUN, FMT='(A)' ) STR(1:LEN__A_REC)
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Site ID    X-coord.      Y-coord.      Z-coord.     phi-geoc. longit.  height'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
      DO 440 J4=1,HSP(1)%L_STA ! Cycle over all stations
!
! ------ Prepare S-record
!
         CALL CLRCH ( STR )
         CALL LIB$MOVC3 ( LEN__S_REC, %REF(STR), SREC )
!
         LONG = ATAN_CS ( HSP(1)%COO(1,J4), HSP(1)%COO(2,J4) )
         IF ( LONG < 0.0D0 ) LONG = PI2 + LONG
         PP  = DSQRT ( HSP(1)%COO(1,J4)**2 + HSP(1)%COO(2,J4)**2 )
         LAT_GCN = DATAN( HSP(1)%COO(3,J4)/PP )
!
! ------ Computation station longitude, latitude and ellipsoidal height
!
         RAD = DSQRT ( HSP(1)%COO(1,J4)**2 + HSP(1)%COO(2,J4)**2 + HSP(1)%COO(3,J4)**2 )
         MU  = DATAN ( HSP(1)%COO(3,J4)/PP * &
     &               ( (1.D0 - VTD__FE) + VTD__EXC_SQ*VTD__REA/RAD  ) )
!
         LAT_GDT = DATAN( ( (1.D0 - VTD__FE)*HSP(1)%COO(3,J4) + &
     &                    VTD__EXC_SQ*VTD__REA*DSIN(MU)**3 ) / &
     &                    ( (1.D0 - VTD__FE)* &
     &                    ( PP  - VTD__EXC_SQ*VTD__REA*DCOS(MU)**3 )) )
!
         HEI_ELL = PP*DCOS(LAT_GDT) + HSP(1)%COO(3,J4)*DSIN(LAT_GDT) - &
     &             VTD__REA* DSQRT( 1.D0 - VTD__EXC_SQ*DSIN(LAT_GDT)**2 )
!
         SREC%REC_ID  = 'S'
         SREC%SITE_ID = HSP(1)%STA(J4)%NAME
         CALL BLANK_TO_UNDERSCORE ( SREC%SITE_ID )
         WRITE ( UNIT=SREC%X_COORD,   FMT='(F13.4)' ) HSP(1)%COO(1,J4)
         WRITE ( UNIT=SREC%Y_COORD,   FMT='(F13.4)' ) HSP(1)%COO(2,J4)
         WRITE ( UNIT=SREC%Z_COORD,   FMT='(F13.4)' ) HSP(1)%COO(3,J4)
         WRITE ( UNIT=SREC%GEOC_LAT,  FMT='(F8.4)'  ) LAT_GCN*180.0D0/PI__NUM
         WRITE ( UNIT=SREC%LONGITUDE, FMT='(F8.4)'  ) LONG*180.0D0/PI__NUM
         WRITE ( UNIT=SREC%HEIGHT,    FMT='(F6.1)'  ) HEI_ELL
!
         CALL LIB$MOVC3 ( LEN__S_REC, SREC, %REF(STR) )
!
! ------ Write S-record
!
         WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR(1:LEN__S_REC)
         IF ( IOS .NE. 0 ) THEN
              CALL CLRCH ( STR )
              CALL INCH  ( IOS, STR )
              CALL ERR_LOG ( 1756, IUER, 'WRI_HARPOS', 'Error '// &
     &             STR(1:I_LEN(STR))//' in writing in the output file '// &
     &             FILOUT)
              RETURN
         END IF
 440  CONTINUE
!
      DO 450 J5=1,HSP(1)%L_STA ! Cycle over all stations
         WRITE ( UNIT=LUN, FMT='(A)' ) '#'
         WRITE ( UNIT=LUN, FMT='(A)' ) '#  Wave ID   Site ID     Up-cos  East-cos North-cos   Up-sin  East-sin North-sin'
         WRITE ( UNIT=LUN, FMT='(A)' ) '#'
!
         DO 460 J6=1,L_HSP ! Cycle over all waves
!
! --------- Prepare D-record
!
            CALL CLRCH ( STR )
            CALL LIB$MOVC3 ( LEN__D_REC, %REF(STR), DREC )
!
            DREC%REC_ID  = 'D'
            DREC%WAVE_ID = HSP(J6)%HAR_NAME
            DREC%SITE_ID = HSP(J6)%STA(J5)%NAME
            CALL BLANK_TO_UNDERSCORE ( DREC%SITE_ID )
!
! --------- Here we also transform local phases to the Greenwich phases on
! --------- the flight. We also flip the sign of phase
!
            WRITE ( UNIT=DREC%UP_COS,    FMT='(F8.5)' ) HSP(J6)%EST(1,N__COS,J5)
            WRITE ( UNIT=DREC%EAST_COS,  FMT='(F8.5)' ) HSP(J6)%EST(2,N__COS,J5)
            WRITE ( UNIT=DREC%NORTH_COS, FMT='(F8.5)' ) HSP(J6)%EST(3,N__COS,J5)
!
            WRITE ( UNIT=DREC%UP_SIN,    FMT='(F8.5)' ) HSP(J6)%EST(1,N__SIN,J5)
            WRITE ( UNIT=DREC%EAST_SIN,  FMT='(F8.5)' ) HSP(J6)%EST(2,N__SIN,J5)
            WRITE ( UNIT=DREC%NORTH_SIN, FMT='(F8.5)' ) HSP(J6)%EST(3,N__SIN,J5)
!
            CALL LIB$MOVC3 ( LEN__D_REC, DREC, %REF(STR) )
!
! --------- Write D-record
!
            WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) STR(1:LEN__D_REC)
            IF ( IOS .NE. 0 ) THEN
                 CALL CLRCH ( STR )
                 CALL INCH  ( IOS, STR )
                 CALL ERR_LOG ( 1757, IUER, 'WRI_HARPOS', 'Error '// &
     &                STR(1:I_LEN(STR))//' in writing in the output file '// &
     &                FILOUT )
                 RETURN
            END IF
 460     CONTINUE
 450  CONTINUE
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '#'
      WRITE ( UNIT=LUN, FMT='(A)' ) HARPOS__LABEL
      CLOSE ( LUN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRI_HARPOS 
