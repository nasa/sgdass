      SUBROUTINE CHWGHT ( STRNG, FOUND, MODE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INTEGER*2 ARRAY_MAX
      PARAMETER (ARRAY_MAX = 10000)
!
! 1.  CHWGHT PROGRAM SPECIFICATION
!
! 1.1 Check whether superfile indicated by STRNG is listed in weight file..
!      If not, terminate with call to FERR if weights 'REQUIRED', prompt user if only 'USED'.
!
! 1.2 REFERENCES:
!
! 2.  CHWGHT INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRNG
      integer*2 mode
!
! STRNG - String containing name and version of superfile from batch control file
!
! 2.3 OUTPUT Variables:
!
      LOGICAL*2 FOUND
!
! FOUND - TRUE if superfile is found in weight file
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'ba2cm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrls
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 VERSION, SUPVERS(ARRAY_MAX), DECIMALTOINT, IERR, TRIMLEN, NDX
      INTEGER*4 IOS
      character*80 cdum
      character*10 arcname, supname(ARRAY_MAX), token, token1, token2
      INTEGER*2  ITOKEN2
      LOGICAL*2  OK_TO_LOAD
      LOGICAL*2  LOOP
      SAVE       SUPVERS, SUPNAME
      INTEGER*4  J1
      LOGICAL*4  LOPEN, LEX
      INTEGER*4, EXTERNAL :: I_LEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  921221  Created
!
!   kdb  971112  Raise the size of the arrays that hold superfile
!                  names and versions read from the input weight file:
!                  raise from 4000 to 10000.
!                At the same time, parameterize this value.
!                Also fix error:  the supname and supvers arrays are
!                  supposed to contain a list of distinct pairs of superfiles
!                  and versions, but the current algorithm will read duplicate
!                  pairs into the arrays for site weight files, because
!                  these weight files may contain multiple lines for each
!                  superfile and version number.  Fix to skip reading
!                  duplicates into the array.
!                Also fix failure to handle a weight file with exactly as
!                  many entries as the array limit.
!   pet   990407 Improved error message. Warning is printed only if G_WARNING
!                was set
!   pet   2000.09.27  Corrected a bug: the previous version did not diagnose
!                     situation when weight file was not found and instead of
!                     error message it created empty new file.
!   pet   2000.09.27  Corrected a bug: the previous version didn't ignore
!                     comment character in weight fiels ( "*" ) and as a result
!                     was unable to find weights correctly if weights for more
!                     than one versions of databases were specified in weight
!                     file.
!
!CCC
!
! 5.  CHWGHT PROGRAM STRUCTURE
!
      IF ( MODE .EQ. 0 ) THEN
           DO 410 J1=1,LF_WEI
!
! ----------- Open weight file
!
              NDX=0
              INQUIRE ( FILE=WEIGHT_FILE(1), EXIST=LEX )
              IF ( .NOT. LEX ) THEN
                   CALL FERR ( INT2(1777), 'BATCH(chwght) Weight file '// &
     &                  WEIGHT_FILE(J1)(1:I_LEN(WEIGHT_FILE(J1)))// &
     &                  ' was not '//'found', INT2(0), INT2(0) )
                   STOP 'BATCH Abnormal termination'
              END IF
              OPEN ( UNIT=65, FILE=WEIGHT_FILE(1), STATUS='OLD', IOSTAT=IOS )
              IF ( IOS .NE. 0 ) THEN
                   CALL FERR ( INT2(IOS), 'BATCH(chwght) Error in '// &
     &                  'attempt to open weight file '// &
     &                  WEIGHT_FILE(J1)(1:I_LEN(WEIGHT_FILE(J1))), &
     &                  INT2(0), INT2(0) )
                   STOP 'BATCH Abnormal termination'
              END IF
              LOOP = .TRUE.
              DO WHILE (LOOP)
                 READ ( 65, '(A)', END=100, IOSTAT=IOS ) CDUM
                 CALL FERR ( INT2(IOS), "BATCH(chwght) Reading weight file "// &
     &                WEIGHT_FILE(J1)(1:I_LEN(WEIGHT_FILE(J1))), &
     &                INT2(0), INT2(0) )
                 CALL SPLITSTRING ( CDUM, TOKEN1, CDUM )
                 CALL SPLITSTRING ( CDUM, TOKEN2, CDUM )
                 ITOKEN2 = DECIMALTOINT ( TOKEN2, IERR )
!
! -------------- Skip duplicate pairs of superfile names and versions,
! -------------- which may occur in a site weight file
!
                 IF ( NDX .GE. 1 ) THEN
                      IF ( ( TOKEN1  .NE. SUPNAME(NDX) .OR. &
     &                       ITOKEN2 .NE. SUPVERS(NDX)      ) .AND. &
     &                       TOKEN1  .NE. '*'                        ) THEN
                           OK_TO_LOAD = .TRUE.
                         ELSE
                           OK_TO_LOAD = .FALSE.
                      ENDIF
                   ELSE
                      IF ( TOKEN1 .EQ. '*' ) THEN
                           OK_TO_LOAD = .FALSE.
                         ELSE
                           OK_TO_LOAD = .TRUE.
                      ENDIF
                 ENDIF
!
                IF ( OK_TO_LOAD ) THEN
                     NDX = NDX+1
                     IF ( NDX .GT. ARRAY_MAX ) THEN
                          CALL FERR ( INT2(246), "BATCH(chwght) weight file "// &
     &                        WEIGHT_FILE(J1)(1:I_LEN(WEIGHT_FILE(J1)))// &
     &                        " exceeds limit", INT2(0), INT2(0) )
                          LOOP = .FALSE.
                       ELSE
                          SUPNAME(NDX) = TOKEN1
                          SUPVERS(NDX) = ITOKEN2
                     ENDIF
                ENDIF
           ENDDO ! LOOP
 100       CONTINUE
           CLOSE ( 65 )
 410    CONTINUE 
        RETURN
      ENDIF ! imode =0
!
      IF ( STRNG(1:1) .EQ. '*' ) THEN
           FOUND = .TRUE.
           RETURN
      ENDIF
      CALL SPLITSTRING ( STRNG, TOKEN, CDUM )
      ARCNAME = TOKEN(2:TRIMLEN(TOKEN))
      CALL SPLITSTRING ( CDUM, TOKEN, CDUM )
      VERSION = DECIMALTOINT ( TOKEN, IERR )
      CALL SPLITSTRING ( CDUM, TOKEN, CDUM )
!
      NDX = 1
      DO WHILE ( ARCNAME .NE. SUPNAME(NDX)  .AND.  NDX .LT. ARRAY_MAX )
         NDX = NDX+1
      ENDDO
!
!@      DO WHILE ( SUPNAME(NDX).EQ.ARCNAME .AND. SUPVERS(NDX).NE.VERSION .AND. &
!@     &           NDX.LT.ARRAY_MAX )
!@         NDX = NDX+1
!@      ENDDO
!@!
!@      IF ( SUPNAME(NDX).EQ.ARCNAME .AND. SUPVERS(NDX).EQ.VERSION ) THEN
      IF ( SUPNAME(NDX).EQ.ARCNAME ) THEN
           FOUND = .TRUE.
        ELSE
           IF ( G_WARNING ) THEN
                WRITE ( 6, 110 ) ARCNAME, VERSION, WEIGHT_FILE(1)
 110            FORMAT ( "BATCH(chwght) Warning: Database ",A10," version ",I2, &
     &                   " was not found in weight file ",A )
                INQUIRE ( UNIT=23, OPENED=LOPEN )
                IF ( LOPEN ) THEN
                     WRITE ( 23, 110 ) ARCNAME, VERSION
                END IF
                FOUND = .FALSE.
           END IF
      ENDIF
!
      RETURN
      END  !#!  CHWGHT  #!#
