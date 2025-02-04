      SUBROUTINE MALO_HARPOS_READ ( FINAM, M_HAR, M_STA, L_HAR, L_STA, &
     &                              C_HAR, C_STA, RD_AREA, &
     &                              HARVAL,  STACOO, HARDSP, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine MALO_HARPOS_READ parses harmonic records, station      *
! *   records and displacement records of the file of site displacements *
! *   in HARPOS format. It is assumd that the files have already been    *
! *   read into the text buffer BUF and some information, the number of  *
! *   harmonics, the number of sites, is already extracted from there.   *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *  FINAM ( CHARACTER ) -- The name of the file with the harmonic site  *
! *                         position variations model. Used for          *
! *                         generating error messages only.              *
! *  N_BUF ( INTEGER*4 ) -- The number of lines in the buffer with       *
! *                         contents of the file with definition of the  *
! *                         harminic model of site position variations.  *
! *    BUF ( CHARACTER ) -- Character array which keeps the image of the *
! *                         file with the harmonic model of site         *
! *                         position variations. Dimension: N_BUF        *
! *  M_HAR ( INTEGER*4 ) -- The maximal number of harmonics in the model *
! *                         of site position variations.                 *
! *  M_STA ( INTEGER*4 ) -- The maximal number of sites in the harmonic  *
! *                         model of site position  variations.          *
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *   L_HAR ( INTEGER*4 ) -- The number of harmonics found in the file.  *
! *   L_STA ( INTEGER*4 ) -- The number of stations found in the file.   *
! *   C_HAR ( CHARACTER ) -- Arrays of hamonic names. Dimension: L_HAR.  *
! *   C_STA ( CHARACTER ) -- Array of site names. Each site name has     *
! *                          length of L_NAM characters. Dimension:      *
! *                          L_STA.                                      *
! * RD_AREA ( REAL*8    ) -- The radius of the area for which            *
! *                          displacements are applicable. Files in      *
! *                          HARPOS format of 2002.12.20 did not define  *
! *                          the radius of the area. The output value    *
! *                          is undefined in this case. Files in HARPOS  *
! *                          format of 2005.03.28 define the radius of   *
! *                          this area in the A-record.                  *
! * STACOO ( REAL*8    ) -- Arrays of site coordinates in a crust        *
! *                         reference frame. Dimension: (3,M_STA).       *
! * HARVAL ( REAL*8    ) -- Two-dimensional array of phases, frequencies *
! *                         and accelerations for each harmonis.         *
! *                         Dimension: (3,M_HAR).                        *
! * HARDSP ( REAL*8    ) -- Four-dimensional array of site position      *
! *                         displacements. The first index rnus through  *
! *                         Up, East, North component of a displacement  *
! *                         vector. The secon index runs through cosine  *
! *                         (1) and sine (2) mode, the third index runs  *
! *                         through harmonics, and the fourth index runs *
! *                         through stations. Dimension: 3,2,M_HAR,M_STA *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
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
! * ### 16-DEC-2002 MALO_HARPOS_READ  v3.0 (c) L. Petrov 22-JUL-2015 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'harpos.i'
      TYPE ( HARPOS__H_RECORD ) :: HREC
      TYPE ( HARPOS__A_RECORD ) :: AREC
      TYPE ( HARPOS__S_RECORD ) :: SREC
      TYPE ( HARPOS__D_RECORD ) :: DREC
      INTEGER*4  M_HAR, M_STA, L_HAR, L_STA, IUER
      INTEGER*4  M__BUF
      PARAMETER  ( M__BUF = 256*1024 )
      CHARACTER  FINAM*(*), C_HAR(M_HAR)*(*), C_STA(M_STA)*(*)
      CHARACTER, ALLOCATABLE :: BUF(:)*128
      REAL*8     RD_AREA, HARVAL(3,M_HAR), STACOO(3,M_STA), &
     &           HARDSP(3,2,M_HAR,M_STA)
      CHARACTER  STR*128
      INTEGER*4  I_WAV, I_STA, IP, N_BUF, IOS, J1, IER
      INTEGER*4, EXTERNAL :: LTM_DIF, I_LEN, LOC__SUN$$_STR
!
      ALLOCATE ( BUF(M__BUF), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL CLRCH ( STR ) 
           CALL INCH  ( M__BUF*SIZEOF(BUF(1)), STR )
           CALL ERR_LOG ( 5711, IUER, 'MALO_HARPOS_READ', 'Failure to '// &
     &         'allocate '//STR(1:I_LEN(STR))//' bytes of dynamic memory '// &
     &         'for array BUF' )
           RETURN 
      END IF
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FINAM, M__BUF, BUF, N_BUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5712, IUER, 'MALO_HARPOS_READ', 'Error in reading '// &
     &         'the file with complex amplitudes of harmonics site position '// &
     &         'variations' )
           RETURN 
      END IF
!
      L_HAR = 0
      L_STA = 0
!
! --- Read the buffer, except the very first and the very last lines which
! --- should be labels of the format identifer
!
      DO 410 J1=2,N_BUF-1
         IF ( BUF(J1)(1:1) .EQ. '#' ) THEN
!
! ----------- This is a comment line
!
              CONTINUE
            ELSE IF ( BUF(J1)(1:2) .EQ. 'H ' ) THEN
!
! ----------- Aga, this is the H-record
!
              L_HAR = L_HAR + 1
              IF ( L_HAR .GT. M_HAR ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_HAR, STR )
                   CALL ERR_LOG ( 5713, IUER, 'MALO_HARPOS_READ', 'Too '// &
     &                 'many harmonics: more than L_HAR='//STR )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ----------- Put the line from the butter into the record BDSUM
!
#ifdef SUN
              CALL LIB$MOVC3 ( LEN__H_REC, %VAL(LOC__SUN$$_STR(BUF(J1))), HREC )
#else
              CALL LIB$MOVC3 ( LEN__H_REC, %REF(BUF(J1)), HREC )
#endif
!
! ----------- Extract the harmonic ID
!
              C_HAR(L_HAR) = HREC%WAVE_ID
              IF ( L_HAR .GT. 1 ) THEN
                   IP = LTM_DIF ( 1, L_HAR-1, C_HAR, C_HAR(L_HAR) )
                   IF ( IP .GT. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( L_HAR, STR )
                        CALL ERR_LOG ( 5714, IUER, 'MALO_HARPOS_READ', &
     &                      'Harmonic '//C_HAR(L_HAR)//' was determined '// &
     &                      'more than once in HARPOS file '//FINAM )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
              END IF
!
! ----------- Read phase, frequency and acceleration of the L_HAR -th harmonic
!
              READ ( UNIT=HREC%PHASE, FMT='(F13.1)' ) HARVAL(1,L_HAR)
              READ ( UNIT=HREC%FREQ,  FMT='(F19.1)' ) HARVAL(2,L_HAR)
              READ ( UNIT=HREC%ACCEL, FMT='(F10.1)' ) HARVAL(3,L_HAR)
            ELSE IF ( BUF(J1)(1:2) .EQ. 'A ' ) THEN
#ifdef SUN
              CALL LIB$MOVC3 ( LEN__A_REC, %VAL(LOC__SUN$$_STR(BUF(J1))), AREC )
#else
              CALL LIB$MOVC3 ( LEN__A_REC, %REF(BUF(J1)), AREC )
#endif
              READ ( UNIT=AREC%AREA_RD, FMT='(F14.6)', IOSTAT=IOS ) RD_AREA
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 5715, IUER, 'MALO_HARPOS_READ', &
     &                 'Error in an attempt to decode the A-record' )
                   DEALLOCATE ( BUF )
                   RETURN
              END  IF
            ELSE IF ( BUF(J1)(1:2) .EQ. 'S ' ) THEN
!
! ----------- U-u-u! It is the S-record
!
              L_STA = L_STA + 1
              IF ( L_STA .GT. M_STA ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( M_STA, STR )
                   CALL ERR_LOG ( 5716, IUER, 'MALO_HARPOS_READ', 'Too '// &
     &                 'many stations: more than M_STA='//STR )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ----------- Put the line from the summary file into the S-record
!
#ifdef SUN
              CALL LIB$MOVC3 ( LEN__S_REC, %VAL(LOC__SUN$$_STR(BUF(J1))), SREC )
#else
              CALL LIB$MOVC3 ( LEN__S_REC, %REF(BUF(J1)), SREC )
#endif
              C_STA(L_STA) = SREC%SITE_ID
              IF ( L_HAR .GT. 1 ) THEN
                   IP = LTM_DIF ( 1, L_STA-1, C_STA, C_STA(L_STA) )
                   IF ( IP .GT. 0 ) THEN
                        CALL CLRCH ( STR )
                        CALL INCH  ( L_HAR, STR )
                        CALL ERR_LOG ( 5717, IUER, 'MALO_HARPOS_READ', &
     &                      'Station '//C_STA(L_STA)//' was determined '// &
     &                      'more than once in HARPOS file '//FINAM )
                        DEALLOCATE ( BUF )
                        RETURN
                   END IF
              END IF
!
! ----------- Extract station coordinates
!
              READ ( UNIT=SREC%X_COORD, FMT='(F13.1)' ) STACOO(1,L_STA)
              READ ( UNIT=SREC%Y_COORD, FMT='(F13.1)' ) STACOO(2,L_STA)
              READ ( UNIT=SREC%Z_COORD, FMT='(F13.1)' ) STACOO(3,L_STA)
            ELSE IF ( BUF(J1)(1:2) .EQ. 'D ' ) THEN
!
! ----------- Oooh! It is the D-record
!
#ifdef SUN
              CALL LIB$MOVC3 ( LEN__D_REC, %VAL(LOC__SUN$$_STR(BUF(J1))), DREC )
#else
              CALL LIB$MOVC3 ( LEN__D_REC, %REF(BUF(J1)), DREC )
#endif
!
! ----------- Search for the name of the harmonic in the D-record
!
              I_WAV = LTM_DIF ( 1, L_HAR, C_HAR, DREC%WAVE_ID )
              IF ( I_WAV .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5718, IUER, 'MALO_HARPOS_READ', &
     &                 'Error in processing the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'line of the input file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- the harmonic "'//DREC%WAVE_ID//'" was not '// &
     &                 'defined in the preceeding H-records')
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ----------- Search for the site name in the D-record
!
              I_STA = LTM_DIF ( 1, L_STA, C_STA, DREC%SITE_ID )
              IF ( I_STA .LE. 0 ) THEN
                   CALL CLRCH ( STR )
                   CALL INCH  ( J1, STR )
                   CALL ERR_LOG ( 5719, IUER, 'MALO_HARPOS_READ', &
     &                 'Error in processing the '//STR(1:I_LEN(STR))//'-th '// &
     &                 'line of the input file '//FINAM(1:I_LEN(FINAM))// &
     &                 ' -- the site "'//DREC%SITE_ID//'" was not '// &
     &                 'defined in the preceeding S-records' )
                   DEALLOCATE ( BUF )
                   RETURN
              END IF
!
! ----------- Read cosine amplitudes of the displacements
!
              READ ( UNIT=DREC%UP_COS,    FMT='(F8.1)' ) HARDSP(1,1,I_WAV,I_STA)
              READ ( UNIT=DREC%EAST_COS,  FMT='(F8.1)' ) HARDSP(2,1,I_WAV,I_STA)
              READ ( UNIT=DREC%NORTH_COS, FMT='(F8.1)' ) HARDSP(3,1,I_WAV,I_STA)
!
! ----------- Read sine amplitudes of the displacements
!
              READ ( UNIT=DREC%UP_SIN,    FMT='(F8.1)' ) HARDSP(1,2,I_WAV,I_STA)
              READ ( UNIT=DREC%EAST_SIN,  FMT='(F8.1)' ) HARDSP(2,2,I_WAV,I_STA)
              READ ( UNIT=DREC%NORTH_SIN, FMT='(F8.1)' ) HARDSP(3,2,I_WAV,I_STA)
            ELSE
              CALL CLRCH ( STR )
              CALL INCH  ( J1, STR )
              CALL ERR_LOG ( 5720, IUER, 'MALO_HARPOS_READ', 'Error in '// &
     &            'parsing the '//STR(1:I_LEN(STR))//'-th line of the '// &
     &            'input file '//FINAM(1:I_LEN(FINAM))//' -- unknown record '// &
     &            'type was encountered' )
              DEALLOCATE ( BUF )
              RETURN
         END IF
 410  CONTINUE
!
      DEALLOCATE ( BUF )
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  MALO_HARPOS_READ  !#!#
