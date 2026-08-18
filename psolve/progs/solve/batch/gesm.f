      SUBROUTINE GESM ( NESM, ESMSITES, ESMDATES, LNAME )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GESM PROGRAM SPECIFICATION
!
! 1.1 Read the episodic site motion file.
!
! 1.2 REFERENCES:
!
! 2.  GESM INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) LNAME
!
! LNAME - Name of the episodic site motion file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 nesm
      character*8 esmsites(MAX_ESM)
      real*8 esmdates(MAX_ESM)
!
! NESM - Number of entries in the episodic site motion file
! ESMSITES - Site names from the esm file
! ESMDATES - Dates/times from the esm file (yymmdd)
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   IY, IM, ID, IERR, I, IBUF(3), INTRVL, NUMEP, DECIMALTOINT
      INTEGER*4   IOS
      CHARACTER   CBUF1*8, TOKEN*8, STRING*256, CBUF2*6
      REAL*8      FJLDY
      EQUIVALENCE ( CBUF2, IBUF(1) )
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   MWH  910227      Created
!   PET  980815      Improved comments and error messages
!   kdb  990206      Y2K fix.
!   pet  2000.11.21  Again Improved comments and error messages
!   pet  2003.11.13  Replaced ias2b with READ
!
! 5.  GESM PROGRAM STRUCTURE
!
1     CONTINUE
      OPEN ( 40, FILE=LNAME, IOSTAT=IOS, STATUS='OLD' )
      IF ( IOS .NE. 0 ) THEN
           CALL FERR ( INT2(IOS), 'BATCH(gesm) Opening episodic site motion '// &
     &         'file '//LNAME, INT2(0), INT2(0) )
      ENDIF
!
! --- Read in the episodic site motion file.
!
      NESM = 0
      DO WHILE (.TRUE.)
!
! ------ Reading in the sites and dates
!
         STRING = ' '
         INTRVL=0
         NUMEP=1
         READ ( 40, '(A)', END=9590, IOSTAT=IOS )  STRING
         CALL FERR ( INT2(IOS), "BATCH(gesm)  Reading episodic motion file "// &
     &               LNAME, INT2(0), INT2(0) )
         IF ( STRING(1:1) .NE. '*'  .AND.  .NOT. (STRING .EQ. ' ' ) ) THEN
              CBUF1 = STRING(1:8)
              CBUF2 = STRING(10:15)
              CALL SPLITSTRING ( STRING(16:40), TOKEN, STRING )
              IF ( TOKEN .NE. ' ' ) THEN
                   INTRVL = DECIMALTOINT(TOKEN,IERR)
              ENDIF
              CALL SPLITSTRING(STRING,TOKEN,STRING )
              IF ( TOKEN .NE. ' ' ) THEN
                   NUMEP = DECIMALTOINT(TOKEN,IERR)
              ENDIF
!
              IF ( CBUF2(1:1) .EQ. ' ' ) THEN
                   CALL FERR ( INT2(2441), &
     &                 'BATCH(gesm) TIME TAG is omitted in '// &
     &                 'the line "'//STRING(1:40)//'" of the episodic '// &
     &                 'motion file '//LNAME, INT2(0), INT2(0) )
              END IF
!
!              IY = IAS2B ( IBUF, INT2(1), INT2(2) )
!              IM = IAS2B ( IBUF, INT2(3), INT2(2) )
!              ID = IAS2B ( IBUF, INT2(5), INT2(2) )
!
              READ ( UNIT=CBUF2, FMT='(3I2)', IOSTAT=IOS ) IY, IM, ID
              IF ( IOS .NE. 0 ) THEN
                   CALL FERR ( INT2(2449), &
     &                 'BATCH(gesm) Wrong format of the time tag: '// &
     &                  CBUF2//' at the line "'//STRING(1:40)// &
     &                 '" of the episodic motion file '//LNAME, &
     &                 INT2(0), INT2(0) )
              END IF
!
              IF ( IM .LT. 1  .OR. IM .GT. 12 .OR. &
     &             ID .LT. 1  .OR. ID .GT. 31      ) THEN
                   CALL FERR ( INT2(2442), &
     &                 'BATCH(gesm) TIME TAG is wrong in '// &
     &                 'the line "'//STRING(1:40)//'" of the episodic '// &
     &                 'motion file '//LNAME, INT2(0), INT2(0) )
              END IF
              DO I=1,NUMEP
                 NESM = NESM+1
                 IF ( NESM .GT. MAX_ESM ) THEN !error exit
                      CALL FERR ( INT2(2443), &
     &                    'BATCH(gesm) Too many entries in '// &
     &                    'episodic motion file '//LNAME, INT2(0), INT2(0) )
                 END IF  !error exit
!
                 ESMSITES(NESM) = CBUF1
                 ESMDATES(NESM) = FJLDY(IM,ID,IY)
                 IM = IM + INTRVL
                 DO WHILE (IM.GT.12)
                    IM = IM - 12
                    IY = IY + 1
                    IF (IY.EQ.100) IY = 0
                 ENDDO
              ENDDO
        END IF  ! good site card found
      END DO  ! reading in the sites
 9590 CONTINUE
!
      CLOSE ( UNIT=40 )
      RETURN
      END  !#! GESM  #!#
