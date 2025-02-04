      SUBROUTINE GMAPF ( TYP, STRING, TOKEN, ERRBAS, NAME, NAME2 )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GMAPF PROGRAM SPECIFICATION
!
! 1.1 Handle map file name.
!
! 1.2 REFERENCES:
!
! 2.  GMAPF INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TYP, STRING
      INTEGER*2 ERRBAS
!
! ERRBAS - Error number for this mapping file type
! STRING - String containing mapping file name (or NONE)
! TYP - Mapping file type (e.g. 'STATIONS', 'SOURCES', etc.)
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) TOKEN,NAME,name2
!
! NAME - Name of the mapping file (or 'NONE')
! TOKEN - Token pulled from STRING
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gmap
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2  IL, TRIMLEN
      CHARACTER  TYPEI*20, STRIN1*80, TOKEN_1*128, TOKEN_2*128, TOKEN_3*128
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   pet  1999.10.15  Improved comments
!   pet  2003.01.14  Fixed a bug: ther previous version worked incorrectly
!                    in parsing PLATE_MODEL file
!   pet  2003.01.29  Fixed a bug: the previous version worked incorrectly
!                    in parsing PLATE_MODEL keyword
!
! 5.  GMAPF PROGRAM STRUCTURE
!
      NAME2 = ' '
      TYPEI = TYP
      IL=MAX(INT2(1),TRIMLEN(TYPEI))
      CALL CLRCH ( TOKEN_1 )
      CALL CLRCH ( TOKEN_2 )
      CALL CLRCH ( TOKEN_3 )
!
! --- Make sure name hasn't already been set
!
      IF ( NAME .NE. ' '  .AND.  NAME .NE. SITPL_FILE ) THEN
           write ( 6, * ) ' name >>',name,' << ' ! %%%%%
           CALL FERR ( ERRBAS, 'BATCH(gmapf): Keyword '//TYPEI(1:IL)// &
     &         ' from section $MAPPING used twice', INT2(0), INT2(0) )
      ENDIF
!
! --- Pull token from STRING
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. ' ' ) THEN
           CALL FERR ( INT2(ERRBAS+5), 'BATCH(gmapf): value of the keyword '// &
     &          TYPEI(1:IL)//' in $MAPPING section has not been suplied. '// &
     &         'File name was expected', INT2(0), INT2(0) )
      ENDIF
!
! --- Set name of this mapping file
!
      NAME = TOKEN
      TOKEN_1 = TOKEN
!
      IF ( TYP .EQ. 'PLATE_MODEL' ) THEN
           CALL CLRCH ( NAME )
!
! -------- Special case for archaic PLATE_MODEL
!
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
           IF ( TOKEN(1:8) .EQ. 'REF_DATE' ) THEN
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                NAME = TOKEN
              ELSE IF ( TOKEN(1:5) .EQ. 'SCALE' ) THEN
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                NAME2 = TOKEN
           ENDIF
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
           IF ( TOKEN(1:5) .EQ. 'SCALE' ) THEN
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                NAME2 = TOKEN
              ELSE IF ( TOKEN(1:8) .EQ. 'REF_DATE' ) THEN
                CALL SPLITSTRING ( STRING, TOKEN, STRING )
                NAME = TOKEN
           ENDIF
           TOKEN = TOKEN_1
        ELSE
!
! -------- Pull another token from STRING, if there is one
!
           CALL SPLITSTRING ( STRING, TOKEN, STRIN1 )
           CALL SPLITSTRING ( STRING, TOKEN, STRING )
           NAME2 = TOKEN
      ENDIF
!
      RETURN
      END  !#!  GMAPF  #!#
