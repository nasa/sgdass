      SUBROUTINE SHFEOP()
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
      INCLUDE 'dmapp.i'
      INCLUDE 'ba2cm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: gerot
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 J, I
      INTEGER*4 IOS
      CHARACTER ERRSTR*160, BUFSTR*160, LNAME*128
      CHARACTER*80 ERRMSG
!
! 4.  HISTORY
!   WHO   WHEN     WHAT
!   mwh  930507  Created
!   kdb  950727  Check for request of excess number of hi-frequency eop values.
!   jmg, 951027  Add error messages for the reading of the high frequency
!   kdb          eop files.
!   pet  980703  Improved error messages, increased length of bufstr, errstr
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
      CALL USE_GLBFIL_4 ( 'OR' )
      DO I=1,MAX_SDC
         DO J=1,2
            SDC_VAL(J,I) = 0.D0
            SDE_VAL(J,I) = 0.D0
         ENDDO
         DO J=1,6
            SDC_ARG(J,I) = 0
            SDE_ARG(J,I) = 0
         ENDDO
      ENDDO
      NUM_SDC_UT1 = 0
      NUM_SDC_XY  = 0
      NUM_SDE_UT1 = 0
      NUM_SDE_XY  = 0
      KHFEOP = 0
      IF ( KHFEOPEST ) KHFEOP = KHFEOP+2
      IF ( KHFEOPCAL ) KHFEOP = KHFEOP+1
!
      IF ( .NOT. KHFEOPCAL ) THEN
           CALL CLRCH ( HFEOPF_CHR )
           HFEOPF_CHR = 'NONE    '
      END IF
      IF ( KHFEOP.EQ.1  .OR.  KHFEOP.EQ.3 ) THEN
           CALL CLRCH ( LNAME )
           IF ( INDEX ( HFEOPCAL, '/' ) .GT. 0 ) THEN
                LNAME = HFEOPCAL
             ELSE
                LNAME = PRE_SAV_DIR(:PRE_SV_LEN)//HFEOPCAL
           END IF
!
           OPEN ( 40, IOSTAT=IOS, FILE=LNAME, STATUS='OLD' )
           IF ( IOS .NE. 0 ) THEN
                ERRSTR = 'SHFEOP: Failure in opening HF E.O. cal file '// &
     &                   PRE_SAV_DIR(:PRE_SV_LEN)//HFEOPCAL
                CALL FERR ( INT2(179), ERRSTR, INT2(0), INT2(0) )
           ENDIF
           CALL CLRCH ( HFEOPF_CHR )
           HFEOPF_CHR = HFEOPCAL
!
! -------- Read first record of substitution file
!
           BUFSTR(1:1) = '*'
           DO WHILE ( BUFSTR(1:1) .EQ. '*'  .OR.  BUFSTR(1:1) .EQ. '$' )
              READ (40, FMT=102, IOSTAT=IOS ) BUFSTR
              CALL FERR ( INT2(IOS), "Reading Hi-freq eop file to find "// &
     &            "num_sdc line ", INT2(0), INT2(0) )
 102          FORMAT(A)
           ENDDO
           READ ( BUFSTR, *, IOSTAT=IOS ) NUM_SDC_UT1, NUM_SDC_XY
           IF ( NUM_SDC_UT1 .LT. 0  .OR.  NUM_SDC_XY .LT. 0 ) IOS = 1
           IF ( IOS .NE. 0 ) THEN
                WRITE ( *, * ) "syntax error in num_sdc high freq eop "// &
     &                         "file line:"
                WRITE ( *, * ) BUFSTR
           END IF
           CALL FERR ( INT2(IOS), "Decoding num_sdc Hi-freq eop values", &
     &                 INT2(0), INT2(0) )
           IF ( NUM_SDC_UT1+NUM_SDC_XY .GT. MAX_SDC ) THEN
                WRITE ( ERRMSG, "('Only',i5, ' hi-freq eop calibration '// &
     &                            'values allowed')" ) MAX_SDC
                CALL FERR ( INT2(180), errmsg, INT2(0), INT2(0) )
           END IF
!
           DO I=1,NUM_SDC_UT1+NUM_SDC_XY
              BUFSTR(1:1) = '*'
              DO WHILE ( BUFSTR(1:1) .EQ.'*' .OR. BUFSTR(1:1) .EQ. '$' )
                 READ ( 40, FMT=102, IOSTAT= IOS ) BUFSTR
                 CALL FERR ( INT2(IOS), "Reading Hi-freq eop file to find "// &
     &                      "sdc_arg line", INT2(0), INT2(0) )
              ENDDO
              READ ( BUFSTR, *, IOSTAT=IOS) (SDC_ARG(J,I),J=1,6), &
     &                                      (SDC_VAL(J,I),J=1,2)
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( *, *)  "syntax error in sdc_arg high freq eop "// &
     &                            "file line:"
                   WRITE ( * ,* ) BUFSTR
              END IF
              CALL FERR ( INT2(IOS), "decoding sdc_arg Hi-freq eop values", &
     &                    INT2(0), INT2(0) )
          ENDDO
!
! ------- Convert ut1 values from usec to mas
!
          DO I=1,NUM_SDC_UT1
             DO J=1,2
                SDC_VAL(J,I) = SDC_VAL(J,I)*15.D0/1000.D0
             ENDDO
          ENDDO
!
! ------- Convert xy values from uasec to mas
!
          DO I=NUM_SDC_UT1+1,NUM_SDC_UT1+NUM_SDC_XY
             DO J=1,2
                SDC_VAL(J,I) = SDC_VAL(J,I)/1000.D0
             ENDDO
          ENDDO
!
! ------- Close the calibration file.
!
          CLOSE ( 40 )
      ENDIF
      IF ( KHFEOP .EQ. 2  .OR. KHFEOP .EQ. 3 ) THEN
           OPEN ( 40, IOSTAT=IOS, FILE=PRE_SAV_DIR(:PRE_SV_LEN)//HFEOPEST, &
     &            STATUS='OLD')
           IF ( IOS .NE. 0 ) THEN
                ERRSTR = 'Failure in opening HF E.O. est '// &
     &                    PRE_SAV_DIR(:PRE_SV_LEN)//HFEOPEST
                CALL FERR ( INT2(279), ERRSTR, INT2(0), INT2(0) )
           ENDIF
!
! -------- Read first record of substitution file
!
           BUFSTR(1:1) = '*'
           DO WHILE ( BUFSTR(1:1) .EQ. '*'  .OR. BUFSTR(1:1) .EQ. '$' )
              READ ( 40, FMT=102, IOSTAT=IOS ) BUFSTR
              CALL FERR ( INT2(IOS), "Reading Hi-freq eop file to find "// &
     &                    "num_sde line", INT2(0), INT2(0) )
           ENDDO
           READ ( BUFSTR, *, IOSTAT=IOS ) NUM_SDE_UT1, NUM_SDE_XY
           IF ( IOS .NE. 0 ) THEN
                WRITE ( *, * ) "syntax error in num_sde high freq eop file "// &
     &                         "line:"
                WRITE ( *, * ) BUFSTR
           END IF
           CALL FERR ( INT2(IOS), "decoding num_sde Hi-freq eop values", &
     &                 INT2(0), INT2(0) )
           IF ( NUM_SDE_UT1+NUM_SDE_XY .GT. MAX_SDC ) THEN
                WRITE ( ERRMSG, "('Only',i5, ' hi-freq eop estimation values '// &
     &                  'allowed')" ) MAX_SDC
                CALL FERR ( INT2(180), ERRMSG, INT2(0), INT2(0) )
           END IF
!
           DO I=1,NUM_SDE_UT1+NUM_SDE_XY
              BUFSTR(1:1) = '*'
              DO WHILE ( BUFSTR(1:1) .EQ. '*' .OR. BUFSTR(1:1) .EQ. '$' )
                 READ ( 40, FMT=102, IOSTAT=IOS ) bufstr
                 CALL FERR ( INT2(IOS), "Reading Hi-freq eop file to find "// &
     &                                  "sde_arg line", INT2(0), INT2(0) )
              ENDDO
              READ ( BUFSTR, *, IOSTAT=IOS ) (SDE_ARG(J,I),J=1,6)
              IF ( IOS .NE. 0 ) THEN
                   WRITE ( *, * ) "syntax error in sde_arg high freq eop "// &
     &                            "file line:"
                   WRITE ( *, * ) BUFSTR
              END IF
              CALL FERR ( INT2(IOS), "decoding sde_arg Hi-freq eop values", &
     &                    INT2(0), INT2(0) )
           ENDDO
!
! -------- Close the calibration file
!
           close (40 )
      ENDIF
1000  CONTINUE
      CALL USE_GLBFIL_4 ( 'WC' )
!
      RETURN
      END  !#!  SHFEOP  #!#
