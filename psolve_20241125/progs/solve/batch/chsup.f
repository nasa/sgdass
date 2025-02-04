      SUBROUTINE CHSUP ( STRING, FOUND, MODE )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  CHSUP PROGRAM SPECIFICATION
!
! 1.1 Check whether superfile indicated by STRING is listed in SUPCAT.
!      If not, terminate with call to FERR.
!
! 1.2 REFERENCES:
!
! 2.  CHSUP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) STRING
      INTEGER*2     MODE
!
! STRING - String containing name and version of superfile from batch control file
!
! 2.3 OUTPUT Variables:
!
      logical*2 found
!
! FOUND - TRUE if superfile is found in SUPCAT
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: ctrls
!       CALLED SUBROUTINES: None
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 version,supvers(MAX_SUP),decimaltoint,ierr,trimlen,ndx
      INTEGER*4 IOS
      CHARACTER  SUPDIR(MAX_SUP)*50, SUPDATE(MAX_SUP)*13, CDUM*80, FINAM*128
      CHARACTER  ARCNAME*10, SUPNAME(MAX_SUP)*10, TOKEN*10, STR*128
      SAVE       SUPVERS, SUPNAME, SUPDIR, SUPDATE, FINAM
      LOGICAL*4  LOPEN, LEX
      INTEGER*4, EXTERNAL :: I_LEN, ILEN
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   mwh  921221  Created
!   pet  990407  Improved error message. Warning is printed only if G_WARNING
!                was set
!   pet  2000.07.11  Added support of the environment variable SUPCAT_FILE
!                    which overrirdes system-wide default variable SUPCAT_FILE
!                    of the superfile catalogue
!   pet  2006.02.02  Increased the length of superfile name from 20 to 50characters
!
! 5.  CHSUP PROGRAM STRUCTURE
!
      IF ( MODE .EQ. 0 ) THEN
           NDX=0
!
! -------- Get the name of the superfiles catalogue. First try to use an
! -------- environment variablbe
!
           CALL GETENVAR ( 'SUPCAT_FILE', FINAM )
           IF ( ILEN(FINAM) .EQ. 0 ) THEN
!
! ------------- If fails, form the default SUPCAT filename
!
!@                FINAM = PRE_SAV_DIR(:PRE_SV_LEN)//SUPCAT_FILE
           END IF
           INQUIRE ( FILE=FINAM, EXIST=LEX )
           IF ( .NOT. LEX ) THEN
                CALL FERR ( INT2(241), "BATCH(chsup)  Superfile catalogue "// &
     &              "file "//FINAM(1:I_LEN(FINAM))//" was not found.", INT2(0), &
     &               INT2(0) )
                STOP "Batch Abnormal termination"
           END IF
!
! -------- Open superfile caltalogue file
!
           CALL FTN_OPEN ( INT2(65), FINAM, ' ' )
           DO WHILE ( .TRUE. )
              NDX = NDX+1
              IF ( NDX .GT. MAX_SUP ) THEN
                   CALL FERR ( INT2(246), "BATCH(chsup) Superfile catalog "// &
     &                 "exceeds limit", INT2(0), INT2(0) )
              END IF
              READ ( 65, '(A)', END=100, IOSTAT=IOS ) CDUM
              CALL FERR ( INT2(IOS), "BATCH(chsup) Reading superfile catalog", &
     &                    INT2(0), INT2(0) )
              SUPDIR(NDX) = CDUM(15:64)
              SUPDATE(NDX) = CDUM(66:78)
              CALL SPLITSTRING ( CDUM, SUPNAME(NDX), CDUM )
              CALL SPLITSTRING ( CDUM, TOKEN, CDUM )
              SUPVERS(NDX) = DECIMALTOINT ( TOKEN, IERR )
           ENDDO
100        CONTINUE
           CLOSE ( 65 )
           RETURN
      ENDIF
!
      IF ( STRING(1:1) .EQ. '*' ) THEN
           FOUND = .TRUE.
           RETURN
      ENDIF
!
      IF ( MODE == 1 ) THEN
           CALL SPLITSTRING ( STRING, TOKEN, CDUM )
           IF ( TRIMLEN(TOKEN) .LT. 2 ) THEN
                CALL FERR ( INT2(248), 'BATCH(chsup) Wrong superfile name: '// &
          &          TOKEN(1:2), INT2(0), INT2(0) )
                STOP 'BATCH Abnormal termination'
           END IF
           ARCNAME = TOKEN(2:TRIMLEN(TOKEN))
           CALL SPLITSTRING ( CDUM, TOKEN, CDUM )
           VERSION = DECIMALTOINT ( TOKEN, IERR )
           CALL SPLITSTRING ( CDUM, TOKEN, CDUM )
         ELSE 
           STR = STRING
           CALL CHASHL  ( STR ) 
           ARCNAME = STR(1:10)
           CALL SPLITSTRING ( STR(11:), TOKEN, STR )
           VERSION = DECIMALTOINT ( TOKEN, IERR )
      END IF
!
      NDX = 1
      DO WHILE ( ARCNAME.NE.SUPNAME(NDX)  .AND.  NDX.LT.MAX_SUP )
         NDX = NDX+1
      ENDDO
!
      DO WHILE ( SUPNAME(NDX).EQ.ARCNAME  .AND.  &
     &           SUPVERS(NDX).NE.VERSION  .AND.  &
     &           NDX.LT.MAX_SUP )
         NDX = NDX+1
      ENDDO
!
      IF ( SUPNAME(NDX).EQ.ARCNAME .AND. SUPVERS(NDX).EQ.VERSION ) THEN
           FOUND = .TRUE.
           IF ( TRIMLEN(STRING) .LT. 81 ) THEN
                STRING(82:83) = '! '
                STRING(84:133) = SUPDIR(NDX)
                STRING(135:147) = SUPDATE(NDX)
              ELSE
                STRING = STRING(:TRIMLEN(STRING))//' ! '//SUPDIR(NDX)//' '// &
     &                   SUPDATE(NDX)
           ENDIF
         ELSE
           STRING = STRING(:TRIMLEN(STRING))//' ! Not found'
           IF ( G_WARNING ) THEN
                WRITE (  6, '("Warning: superfile ",A10," ver. ",I2, &
     &                        " not found in ",A)' ) ARCNAME, VERSION, &
     &                        FINAM(1:I_LEN(FINAM))
                INQUIRE ( UNIT=23, OPENED=LOPEN )
                IF ( LOPEN ) THEN
                     WRITE ( 23, '("Warning: superfile ",A10," ver. ",I2, &
     &                       " not found in A")' ) ARCNAME, VERSION, &
     &                       FINAM(1:I_LEN(FINAM))
                END IF
                FOUND = .FALSE.
           ENDIF
      ENDIF
!
      RETURN
      END  !#!  CHSUP  #!#
