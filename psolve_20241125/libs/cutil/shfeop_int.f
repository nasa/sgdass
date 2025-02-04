      SUBROUTINE SHFEOP_INT ( HFEOPCAL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  SHFEOP PROGRAM SPECIFICATION
!
! 1.1 Read a high-frequency earth orientation calibration file
!
! 1.2 REFERENCES:
!
! 2.  SHFEOP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      character*(*) hfeopcal
!
!
! 2.3 OUTPUT Variables:
!
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: gerot
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 J, N, NN, I
      INTEGER*4 IOS
      CHARACTER ERRSTR*200, BUFSTR*100, WHOLE_HF_NAME*128
      LOGICAL*2 KBIT
      LOGICAL*4 LEX
      INTEGER*4 I_LEN
!
! 4.  HISTORY
!   WHO   WHEN     WHAT
!   mwh  930507  Created
!   jmg  930624  Put in error calls ferr on unexpected EOF and
!                not enough values in read statement
!   kdb  960605  Fix error message for opening the file to show the whole
!                file name.  Also truncate the whole file name in the open
!                to remove trailing blanks.
!   pet  2000.03.14  Added capacity to specify a full path name in the variable
!                    HFEOPCAL. If HFEOPCAL contains a symbol "/" then it is
!                    considered as a full path. If it doesn't have "/" then
!                    $SAVE_DIR path is prepended before the name.
!
! 5.  SHFEOP PROGRAM STRUCTURE
!
! Get values from the earth orientation data substitution file
!
! *** OPEN EARTH ORIENTATION CALIBRATION FILE
!
      DO I=1,MAX_SDC
         DO J=1,2
           SDC_VAL(J,I) = 0.D0
         ENDDO
         DO J=1,6
           SDC_ARG(J,I) = 0
         ENDDO
      ENDDO
      NUM_SDC_UT1 = 0
      NUM_SDC_XY = 0
      KHFEOP = 0
!
      IF ( HFEOPCAL(:4) .EQ. 'NONE' .OR. HFEOPCAL.EQ.' ' ) THEN
           RETURN
      ENDIF
      KHFEOP = 1
 1    CONTINUE
      CALL CLRCH ( WHOLE_HF_NAME  )
      IF ( INDEX ( HFEOPCAL, '/' ) .GT. 0 ) THEN
           WHOLE_HF_NAME = HFEOPCAL
         ELSE
           WHOLE_HF_NAME = PRE_SAV_DIR(:PRE_SV_LEN)//HFEOPCAL
      END IF
!
! --- For some unclear reason HP-UX Fortran compiler 2.4 failed to open
! --- file nless trailing blanks are removed.
!
      INQUIRE ( FILE=WHOLE_HF_NAME(1:I_LEN(WHOLE_HF_NAME)), EXIST=LEX )
      IF ( .NOT. LEX ) THEN
           CALL FERR ( INT2(178), 'File with high frequency EOP model '// &
     &                 WHOLE_HF_NAME(1:I_LEN(WHOLE_HF_NAME))//' was not '// &
     &                 'found', INT2(0), INT2(0) )
           CALL EXIT ( 1 )
      END IF
      OPEN ( UNIT=40, IOSTAT=IOS, FILE=WHOLE_HF_NAME(1:I_LEN(WHOLE_HF_NAME)), &
     &       STATUS='OLD')
      IF ( IOS .NE. 0 ) THEN
           WRITE ( 6, * ) ' IOS=',IOS
           WRITE ( ERRSTR, &
     &       "('SHFEOP_INT: Failure in opening HF E.O. cal file >>',A,'<<' )") &
     &       WHOLE_HF_NAME(1:I_LEN(WHOLE_HF_NAME))
           CALL FERR ( INT2(179), ERRSTR, INT2(0), INT2(0) )
           CALL EXIT ( 1 )
      ENDIF
!
! --- Read first record of substitution file
!
      BUFSTR(1:1) = '*'
      DO WHILE ( BUFSTR(1:1).EQ.'*' .OR. BUFSTR(1:1).EQ.'$' )
         READ(40,FMT=102,IOSTAT=IOS ) BUFSTR
         CALL FERR ( INT2(IOS), "Reading HF E.O. cal file", INT2(0), INT2(0) )
 102     FORMAT(A)
      ENDDO
      READ ( BUFSTR, *, IOSTAT=IOS ) NUM_SDC_UT1, NUM_SDC_XY
      IF ( NUM_SDC_UT1 .LT. 0  .AND.  NUM_SDC_XY .LT. 0 ) IOS = 1
      IF ( IOS .NE. 0 ) CALL FERR ( INT2(IOS), "Reading HF eop values", &
     &                              INT2(0), INT2(0) )
      DO I=1,NUM_SDC_UT1+NUM_SDC_XY
         BUFSTR(1:1) = '*'
         DO WHILE ( BUFSTR(1:1).EQ.'*' .OR. BUFSTR(1:1).EQ.'$' )
            READ(40,FMT=102,END=500,IOSTAT=ios ) bufstr
            CALL FERR ( INT2(IOS), "Reading HF E.O. cal file", &
     &                  INT2(0), INT2(0) )
         ENDDO
         READ ( BUFSTR, *, END=510, IOSTAT=IOS ) &
     &          ( SDC_ARG(J,I),J=1,6),(SDC_VAL(J,I),J=1,2)
         IF ( IOS .NE. 0 ) CALL FERR ( INT2(IOS), "Reading HF eop values", &
     &                                 INT2(0), INT2(0) )
      ENDDO
!
! --- Convert ut1 values from usec to mas
!
      DO I=1,NUM_SDC_UT1
         DO J=1,2
            SDC_VAL(J,I) = SDC_VAL(J,I)*15.D0/1000.D0
         ENDDO
      ENDDO
!
! --- Convert xy values from usec to mas
!
      DO I=NUM_SDC_UT1+1,NUM_SDC_UT1+NUM_SDC_XY
         DO J=1,2
            SDC_VAL(J,I) = SDC_VAL(J,I)/1000.D0
         ENDDO
      ENDDO
!
! --- Close the calibration file.
!
  999 CLOSE ( UNIT=40, IOSTAT=IOS )
      CALL FERR ( INT2(ios), "Closing HF_EOP file: "//hfeopcal, INT2(0), &
     &     INT2(0) )
!     CALL USE_GLBFIL_4 ( 'W' )
!
      RETURN
!
!
! die on unexpected EOF
500   continue
      WRITE(errstr,"('Unexpected EOF in HF-EOP file ',A30)") hfeopcal
      call ferr( INT2(179), errstr, INT2(0), INT2(-1) )
      stop
510   continue
      WRITE(errstr,"('Bad line in HF-EOP file ',A30)") hfeopcal
      call ferr( INT2(180), errstr, INT2(0), INT2(0) )
      WRITE(errstr,"('Need 6 int, 2 amps. Have: ',A30)") bufstr
      call ferr( INT2(180), errstr, INT2(0), INT2(-1) )
!
      END  SUBROUTINE SHFEOP_INT
