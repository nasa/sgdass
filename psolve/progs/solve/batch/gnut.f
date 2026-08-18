      SUBROUTINE GNUT(NUTFLG,TOKEN,STRING,FCNPR,FLAG)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GNUT PROGRAM SPECIFICATION
!
! 1.1 Parse NUTATION line from FLAGS or SUPPRESSION section.
!
! 1.2 REFERENCES:
!
! 2.  GNUT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING,FLAG
!
! FLAG - 'FLAG' or 'SUPP', depending on which section we're processing
! STRING - Input string from control file
!
! 2.3 OUTPUT Variables:
!
      CHARACTER*(*) NUTFLG(116),TOKEN
      REAL*8 FCNPR
!
! FCNPR - Free core nutation period
! NUTFLG - Nutation flag
! TOKEN - Token pulled from input string
!
! 2.4 COMMON BLOCKS USED
!!      include '../include/velcm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: gflags,gsuprs
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      LOGICAL*2 CFEOF
      INTEGER*2  L_PER, L_POS
      PARAMETER  ( L_PER = 12, L_POS = 12 )
      CHARACTER  PERIODS(L_PER)*12, POSSIB(L_POS)*9, TOKEN_FLAG*20
      INTEGER*2  I, LENGTH, CFREAD, IDUM, J, IPOS, ITYPE, IPER, TRIMLEN
      INTEGER*2  ITERM,IERR,IL(L_PER),ILIMIT
      INTEGER*4  IOS
      CHARACTER*1 BSLASH
!
! --- Possible nutation components
!
      DATA POSSIB/'PSI_EPSBB', &
     &            'IN     BI', &
     &            'OUT    BO', &
     &            'PSI    PB', &
     &            'PSI_IN PI', &
     &            'PSI_OUTPO', &
     &            'EPS    EB', &
     &            'EPS_IN EI', &
     &            'EPS_OUTEO', &
     &            'TIE    TB', &
     &            'TIE_IN TI', &
     &            'TIE_OUTTO'  /
!
! Possible nutation terms
!
      DATA PERIODS /                &
     &               'PRINCIPAL  ', &
     &               '9.3-YEAR   ', &
     &               'ANNUAL     ', &
     &               'SEMI-ANNUAL', &
     &               '122-DAY    ', &
     &               '13.7-DAY   ', &
     &               'OFFSET     ', &
     &               'XY_OFFSET  ', &
     &               'CONSTANT   ', &
     &               'SLOPE/YEAR ', &
     &               'FCN_       ', &
     &               'CALC_      '  & 
      &            /
!
      BSLASH = CHAR(92)
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   JLR   921215  replace '\' with BSLASH
!   pet   1999.11.19   Shortedned the list, since partials derivatives only
!                      for OFFSET is computed by CALC. Improved error message.
!   pet   1999.11.28   Played back the fix of 1999.11.19   and added the check
!                      of mode at the end of the routine
!   pet   2008.04.09   Added support of XY_OFFSET flag
!
! 5.  GNUT PROGRAM STRUCTURE
!
! Initialize the nutation flag array
!
      DO I=1,116
         NUTFLG(I)='NN'
      ENDDO
!
! --- Pull off the first token from the input string
!
      CALL SPLITSTRING ( STRING, TOKEN, STRING )
      CALL CLRCH (  TOKEN_FLAG )
      TOKEN_FLAG = TOKEN ! store the Nutation flag
!
      IF ( TOKEN .EQ. 'NO' ) THEN
!
! ------- If it's NO, then we are finished
!
          RETURN
      ENDIF
!
      DO I=1,L_PER
        IL(I)=TRIMLEN(PERIODS(I))
        IF(FLAG.EQ.'SUPP'.AND.PERIODS(I).EQ.'FCN_') IL(I)=IL(I)-1
      ENDDO
!
      ITYPE=1
!
! --- Loop over tokens in the input string
!
      DO WHILE (TOKEN.NE.' ')
        IPOS=0
        ILIMIT=10
        IF(FLAG.EQ.'SUPP') ILIMIT=L_POS
        IF ( ILIMIT .GT. L_POS ) ILIMIT = L_POS
!
! ----- Check for nutation component
!
        DO J=1,ILIMIT
          IF(TOKEN.EQ.POSSIB(J)(1:7)) IPOS=J
        ENDDO
        IF ( IPOS .NE. 0 ) THEN
             ITYPE=IPOS
!
! ---------- Handle continuation line by reading in next record from control
! ---------- file
!
          ELSE IF(TOKEN.EQ.BSLASH) THEN
             LENGTH=CFREAD(STRING)
             IF ( STRING(1:1) .EQ. '$'  .OR.  CFEOF(IDUM) ) THEN
                  CALL FERR ( INT2(9010), &
     &                'BATCH(gnut) ILLEGAL CONTINUATION LINE '//STRING(1:10), &
     &                 INT2(0), INT2(0) )
             ENDIF
          ELSE
             IPER=0
!
! ---------- Check for nutation period
!
             DO I=1,L_PER
                IF ( PERIODS(I)(1:IL(I)) .EQ. TOKEN ) IPER=I
             ENDDO
!
! ---------- Make sure period specified is valid
!
             IF ( IPER.EQ.0 ) THEN
                 CALL FERR ( INT2(9020), &
     &               'BATCH(gnut) unknown nutation period '//TOKEN(1:16)// &
     &               ' OFFSET was expected', INT2(0), INT2(0) )
                 STOP 'BATCH(gnut)'
                ELSE IF ( IPER.LE.10 .AND. NUTFLG(IPER).NE.'NN' ) THEN
                  CALL FERR( INT2(9030), PERIODS(IPER)//' ENCOUNTED TWICE', &
     &                 INT2(0), INT2(0) )
!
! --------------- Set nutation flag appropriately
!
                ELSE IF(IPER.LE.10) THEN
                  NUTFLG(IPER)=POSSIB(ITYPE)(8:9)
                ELSE IF(IPER.EQ.11) THEN
!
! -------------- Get the free core nutation period if we're in FLAGS section
!
                  IF ( FLAG .EQ. 'FLAG' ) THEN
                       READ ( TOKEN(5:), *, IOSTAT=IOS ) FCNPR
                       CALL FERR ( INT2(IOS), 'DECODING FCN PERIOD', INT2(0), &
     &                             INT2(0) )
                       NUTFLG(11)=POSSIB(ITYPE)(8:9)
                   ELSE IF ( TOKEN(4:) .NE. ' ' ) THEN
                      CALL FERR ( INT2(9032), &
     &                    'NO PERIOD ALLOWED WITH FCN IN '//'SUPPRESION', &
     &                     INT2(0), INT2(0) )
                   ELSE
                      NUTFLG(111)=POSSIB(ITYPE)(8:9)
                 ENDIF
               ELSE IF ( IPER .EQ. L_PER ) THEN
!
! -------------- Get n-term from CALC Wahr nutation series
!
                 READ ( TOKEN(6:), *, IOSTAT=IOS ) ITERM
                 CALL FERR ( INT2(IOS), 'DECODING CALC TERM', INT2(0), INT2(0) )
                 IF ( ITERM.LT.1 .OR. ITERM.GT.106 ) THEN
                      CALL FERR ( ITERM, 'UNKNOWN CALC TERM', INT2(0), INT2(0) )
                   ELSE IF ( NUTFLG(10+ITERM) .NE. 'NN' ) THEN
                      CALL FERR ( ITERM, 'CALC TERM USED TWICE', INT2(0), &
     &                            INT2(0) )
                   ELSE
                      NUTFLG(10+ITERM)=POSSIB(ITYPE)(8:9)
                ENDIF
            ENDIF
        ENDIF
!
! ----- Get next token from input string
!
        CALL SPLITSTRING(STRING,TOKEN,STRING )
      ENDDO
!
! --- Final check: is this token_flag supported
!
      IF ( ( NUTFLG(1)  .EQ. 'NN'  .AND. &
     &       NUTFLG(2)  .EQ. 'NN'  .AND. &
     &       NUTFLG(3)  .EQ. 'NN'  .AND. &
     &       NUTFLG(4)  .EQ. 'NN'  .AND. &
     &       NUTFLG(5)  .EQ. 'NN'  .AND. &
     &       NUTFLG(6)  .EQ. 'NN'        ) .AND. &
     &     ( NUTFLG(7)  .EQ. 'BB'  .OR.  &
     &       NUTFLG(8)  .EQ. 'BB'        )       ) THEN
           CONTINUE 
         ELSE
           CALL FERR ( INT2(9049), 'BATCH(gnut): nutation flag '//TOKEN_FLAG// &
     &         'is not supported any more. Only flags OFFSET and NO '// &
     &         'are supported. You can estimate nutation terms in '// &
     &         'user-partial mode', INT2(0), INT2(0) )
      END IF
!
      RETURN
      END  !#!  GNUT  #!#
