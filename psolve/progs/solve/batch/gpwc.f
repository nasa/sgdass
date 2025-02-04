      SUBROUTINE GPWC ( PWCNUM, PWCEP, PWCSITES, LNAME, INTRVL )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  GPWC PROGRAM SPECIFICATION
!
! 1.1 Read the episodic site motion file.
!
! 1.2 REFERENCES:
!
! 2.  GPWC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER   LNAME*(*)
!
! LNAME - Name of the episodic site motion file
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2   PWCNUM(MAX_STA), INTRVL
      REAL*8      PWCEP(MAX_PWC_EPS)
      CHARACTER*8 PWCSITES(MAX_PWC_SITES)
!
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
      INTEGER*2 IY, IM, ID, NEP, NSIT, IERR, I, IBUF(3), NUMEP
      INTEGER*4 DECIMALTOINT
      INTEGER*4 IOS
      CHARACTER CBUF1*8, TOKEN*8, STRING*256
      LOGICAL*2 KEP
      REAL*8    FJLDY
      EQUIVALENCE ( CBUF1, IBUF(1) )
!
! 4.  HISTORY
!   WHO   WHEN       WHAT
!   MWH  910814      Created
!   PET  970307      Made error messages a bit more amicable
!   KDB  990206      Y2K changes.
!   PET  1999.05.03  Added initialization of pxcnum
!   PET  2000.08.03  Imporved comments
!   PET  2001.04.04  Corrected the bug: the previous version didn't made
!                    conversion the station name with underscore to the
!                    station name with blank inside the name.
!   pet  2003.11.13  Replaced ias2b with READ
!   pet  2005.03.17  Fixed numerous bugs
!
! 5.  GPWC PROGRAM STRUCTURE
!
      DO I=1,MAX_STA
         PWCNUM(I) = 0 ! Initialization
      ENDDO
!
1     CONTINUE
      OPEN ( 40, FILE=LNAME, IOSTAT=IOS, STATUS='OLD' )
      IF ( IOS .NE. 0 ) THEN
           CALL FERR ( INT2(IOS), 'GPWC(BATCH) Opening station epoch file '// &
     &                 LNAME, INT2(0), INT2(0) )
           GOTO 1
      ENDIF
!
! --- Read in the station epoch file.
!
      NSIT = 0
      NEP = 0
      KEP = .TRUE.
      INTRVL = 0
      DO WHILE ( KEP )
!
! ------ Reading in the dates
!
         STRING= ' '
         CBUF1 = ' '
         READ ( 40, '(A)', END=9590, IOSTAT=IOS ) STRING
         CALL FERR ( INT2(IOS), "GPWC(BATCH) Reading station epoch file "// &
     &               LNAME, INT2(0), INT2(0) )
         IF ( STRING(1:1) .NE.  '*'  .AND. STRING .NE. ' ' ) THEN
              CBUF1 = STRING(1:6)
              READ ( UNIT=CBUF1(1:6), FMT='(3I2)', IOSTAT=IOS ) IY, IM, ID
              IF ( IOS .NE. 0 ) THEN
                   CALL ERR_LOG ( 6181, -3, 'GPWC', 'Wrong format of the '// &
     &                 'date: '//CBUF1(1:6) )
                   STOP 'GMAP(BATCH) Abnormal termination'
              END IF
!
              IF ( IY .GE. 0 ) THEN
                   CALL SPLITSTRING ( STRING(9:40), TOKEN, STRING )
                   IF ( TOKEN .NE. ' ' ) THEN
                        INTRVL = DECIMALTOINT ( TOKEN, IERR )
                        CALL SPLITSTRING ( STRING, TOKEN, STRING )
                        IF ( TOKEN .NE. ' ' ) THEN
                             NUMEP = DECIMALTOINT ( TOKEN, IERR )
                          ELSE
                             NUMEP = 1
                        ENDIF
                        DO I=1,NUMEP
                           NEP = NEP+1
                           IF ( NEP .GT. MAX_PWC_EPS ) THEN ! error exit
                                WRITE ( 6, * ) ' NEP=',NEP
                                CALL FERR ( INT2(121), &
     &                              'GPWC(BATCH) Too many date '// &
     &                              'entries in station epoch file', INT2(0), &
     &                               INT2(0) )
                           END IF  ! error exit
                           PWCEP ( NEP ) = FJLDY ( IM, ID, IY )
                           IM = IM + INTRVL
                           DO WHILE ( IM .GT. 12 )
                              IM = IM-12
                              IY = IY+1
                              IF ( IY .EQ. 100 ) IY = 0
                           ENDDO
                        ENDDO
                     ELSE
!
! --------------------- Default interval = 12 months; default #epochs = 1
!
                        INTRVL = 12
                        NUMEP = 1
                   ENDIF ! TOKEN
                 ELSE
                   KEP = .FALSE.
              ENDIF
         END IF  ! good date card found
!
         READ ( 40, '(A)', END=9590, IOSTAT=IOS ) STRING
!
         DO WHILE ( .TRUE. )
            IF ( STRING(1:1) .NE.  '*'  .AND. STRING .NE. ' ' ) THEN
                 NSIT = NSIT+1
                 IF ( NSIT .GT. MAX_PWC_SITES) THEN ! error exit
                      CALL FERR ( INT2(122), &
     &                    'GPWC(BATCH) Too many station entries '// &
     &                    'in station epoch file', INT2(0), INT2(0) )
                 END IF  ! error exit
                 PWCSITES(NSIT) = STRING(1:8)
!@U                 CALL UNDSCR ( PWCSITES(NSIT) )
            ENDIF
            READ ( 40, '(A)', END=9590, IOSTAT=IOS ) STRING
           CALL FERR ( INT2(IOS), "GPWX(BATCH) Reading station epoch file "// &
     &                 LNAME, INT2(0), INT2(0) )
         ENDDO
      END DO  ! reading in the dates
!
! --- Close the pwc file.
!
 9590 CONTINUE
      IF ( NSIT .LE. 0 ) THEN
           CALL FERR ( INT2(9590), 'GPWC(BATCH) No stations specified in '// &
     &         'continuous position file', INT2(0), INT2(0) )
      END IF
      PWCNUM(1) = NEP
      PWCNUM(2) = NSIT
!
      CLOSE ( 40 )
      RETURN
      END  !#!  GPWC  #!#
