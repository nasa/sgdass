      SUBROUTINE GYNEXL ( DEFVAR, EXCEPTVAR, IEXCEPTSZ, EXCEPT_NAMES, &
     &                    NUM_EXCEPT, NUM_EXCEPT_BEG, M_KEYS, NUM_KEYS, &
     &                    KEYS_ARRAY, MAX_EXCEPT, IBITD, IWORDE, SECT_ID, &
     &                    OPT_ID, TOKEN, STRING )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GYNEXL PROGRAM SPECIFICATION
!
! 1.1 Parse a batch control file line with the format:
      LOGICAL*2 L4TOL2
!       {optional arbitrary keywords} [yes or no] {except list}
!     The arbitrary keywords are passed up to the caller as is for processing.
!
!    restrictions: yes and no are not permitted as arbitrary keywords.
!
! 1.2 REFERENCES:
!
! 2.  GYNEXL INTERFACE
!
! 2.1 Parameter File
!
! 2.2 INPUT Variables:
!
      INTEGER*2 MAX_EXCEPT, IEXCEPTSZ, NUM_EXCEPT_BEG
      INTEGER*2 IBITD,IWORDE
      CHARACTER*(*) SECT_ID,OPT_ID
      CHARACTER*(*) STRING
!
! MAX_EXCEPT - maximum number of exceptions allowed
! IEXCEPTSZ - size of first dimension of exceptvar
! IBITD - applicable bit in default bit array (defvar)
! IWORDE - applicable word in exception bit array (exceptvar(1,iworde))
! SECT_ID - batch control file section
! OPT_ID - option being parsed
! STRING - String to be parsed
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 DEFVAR,EXCEPTVAR(IEXCEPTSZ,*)
      CHARACTER*(*) TOKEN
      INTEGER*2 EXCEPT_NAMES(4,*)
      INTEGER*2 NUM_EXCEPT
      INTEGER*2 M_KEYS, NUM_KEYS
      CHARACTER*(*) KEYS_ARRAY(M_KEYS)
!
! NUM_EXCEPT - number of names in exception list
! EXCEPT_NAMES - list of exceptions (sites or sources)
! NUM_KEYS - number of arbitrary keywords found
! KEYS_ARRAY - list of arbitrary keywords found
! DEFVAR -  bit array for Default set up
! EXCEPTVAR - bit array for exceptions to default set up
! TOKEN - Token picked up from the string
!
! 2.4 COMMON BLOCKS USED
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gsuprs
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2   CFEOF,KBIT
      CHARACTER   TEMP*8
      INTEGER*2   I,LENGTH,CFREAD,IDUM,IVAL,KBITN,IPOS,TRIMLEN,OPTLEN
      CHARACTER   BSLASH*1
      CHARACTER   ERRMSG*128
      INTEGER*4,  EXTERNAL :: I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   kdb  961125  created based on graosp.f.
!   kdb  970312  added ability to parse zero or more arbitrary keywords
!                (but yes and no are not permitted as arbitrary keywords).
!   pet  990103  Added an argument M_KEYS. Fixed a bug: gynexl might overindex
!                array KEYS_ARRAY if there were no YES or NO supplied
!   pet  2007.05.11  Significanly changed the logic. Now, if the station is in &
!                    the list, then the bit BITD is set on (the previos logic set
!                    the value of this bit to the value of DEFVAR
!
! 5.  GYNEXL PROGRAM STRUCTURE
!
      BSLASH = CHAR(92)
      OPTLEN = TRIMLEN ( OPT_ID )
      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
      NUM_KEYS = 0
      DO I=1,M_KEYS
!
         IF ( TOKEN .EQ. 'YES'  .OR. &
     &        TOKEN .EQ. 'NO'        ) GOTO 810
!
         IF ( TOKEN(1:1) .EQ. ' ' ) THEN
              GOTO 810
            ELSE
              NUM_KEYS = NUM_KEYS+1
              KEYS_ARRAY(NUM_KEYS) = TOKEN
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
         END IF
      END DO
!
      WRITE ( UNIT=ERRMSG, FMT='(I9)' ) M_KEYS
      CALL CHASHL ( ERRMSG )
      CALL FERR ( INT2(9077), 'BATCH(GYNEXL): Too many keys: '// &
     &     ERRMSG(1:I_LEN(ERRMSG))//' or qualifier missed: YES or NO', INT2(0), INT2(0) )
      NUM_KEYS = -1
      RETURN
!
 810  CONTINUE
!
      IF ( TOKEN .EQ. 'NO' ) THEN
           CALL SBIT ( DEFVAR, IBITD, INT2(0) )
        ELSE IF ( TOKEN(1:1) .EQ. ' '  .OR.  TOKEN .EQ. 'YES' ) THEN
           CALL SBIT ( DEFVAR, IBITD, INT2(1) )
      ENDIF
!
      CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
      IF ( TOKEN .EQ. 'EXCEPT' ) THEN
           CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
           DO WHILE ( TOKEN .NE. ' ' )
              IF ( TOKEN .EQ. BSLASH ) THEN
                   LENGTH=CFREAD(STRING)
                   IF ( STRING(1:1) .EQ. '$' .OR. CFEOF(IDUM) ) THEN
                        ERRMSG = SECT_ID//' SECTION '// &
     &                           OPT_ID(1:OPTLEN)// &
     &                           ' Illegal continuation line '//STRING(1:10)
                        CALL FERR ( INT2(9020), ERRMSG, INT2(0), INT2(0) )
                   ENDIF
                ELSE
!@U                   CALL UNDSCR(TOKEN )
                   IF ( NUM_EXCEPT_BEG .LE. NUM_EXCEPT .AND. &
     &                  NUM_EXCEPT_BEG .GT. 0                ) THEN
!
! --------------------- Search for duplacates
!
                        DO I=NUM_EXCEPT_BEG,NUM_EXCEPT
                           CALL HOL2CHAR ( EXCEPT_NAMES(1,I), INT2(1), &
     &                                     INT2(8), TEMP )
                           IPOS = I
                           IF ( TOKEN .EQ. TEMP ) THEN
!
! ----------------------------- and bypass putting them in the list the second time
!
                                CALL SBIT ( EXCEPTVAR(1,IWORDE), I, INT2(1) )
                                GOTO 90 
                           END IF
                        ENDDO              
                   END IF
!
                   NUM_EXCEPT = NUM_EXCEPT + 1
                   IF ( NUM_EXCEPT .GT. MAX_EXCEPT ) THEN
                        ERRMSG = SECT_ID//' SECTION '//OPT_ID(1:OPTLEN)// &
     &                          ' too many exceptions at '//TOKEN(1:10)
                        CALL FERR ( INT2(9030), ERRMSG, INT2(0), INT2(0) )
                   ENDIF
!
                   CALL CHAR2HOL ( TOKEN, EXCEPT_NAMES(1,NUM_EXCEPT), INT2(1), &
     &                  INT2(8) )
                   IPOS=NUM_EXCEPT
90                 CONTINUE
                   CALL SBIT ( EXCEPTVAR(1,IWORDE), IPOS, INT2(1) )
              ENDIF ! bslash
              CALL SPLIT_AND_CASEFOLD ( STRING, TOKEN, STRING )
           ENDDO
         ELSE IF ( TOKEN .NE. ' ' ) THEN
           ERRMSG = SECT_ID//' SECTION '//OPT_ID(1:OPTLEN)// &
     &              ' Incorrect EXCEPT clause '//TOKEN(1:10)
           CALL FERR ( INT2(9040), ERRMSG, INT2(0), INT2(0) )
      ENDIF
!
      RETURN
      END  !#!  GYNEXL  #!#
