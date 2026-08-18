      SUBROUTINE DSOUC ( FILE_NAME, LSONAM_CHR, NSTAR, SUBRD, JNSTR, &
     &                   JSTRN_CHR, WSTARC, STRDIF, FLYBY_WARNING, NVSTARC )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DSOUC PROGRAM SPECIFICATION
!
! 1.1 Calculate the source position differences
!
! 1.2 REFERENCES:
!
! 2.  DSOUC INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER  FILE_NAME*(*), LSONAM_CHR(*)*8, JSTRN_CHR(*)*8, &
     &           STR1*20, STR2*20
      INTEGER*2  NSTAR, JNSTR
      REAL*8     SUBRD(2,*)
      LOGICAL*2  FLYBY_WARNING
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
! FLYBY_WARNING - True if we want message when no match
! JNSTR - Number of sources in the arc
! JSTRN - Names of sources in the arc
! LSONAM - Substitute source names
! NSTAR - Number of substitute source names
! SUBRD - Substitute RA and DEC
!
! 2.3 OUTPUT Variables:
!
      REAL*8 WSTARC(2,*),NVSTARC(2,*),STRDIF(2,*)
!
! NVSTARC - Source positions from mod file (RA,DEC in radians)
! STRDIF - Position differences
! WSTARC - Array of source positions (RA,DEC in radians)
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
      INTEGER*2   K, I, ITEST, J
      REAL*8      TWOPI, DIFRA, DIFDC, COO_LIM
!!      PARAMETER  ( COO_LIM = 500000.0 ) ! Upper limit of the difference in source
      PARAMETER  ( COO_LIM = 5000.0 ) ! Upper limit of the difference in source
!                                     ! coordinates in mas
      LOGICAL*2   PAUSE_FLYBY
      CHARACTER   BUFSTR*200, ERRSTR*200, DSOUC_IGNORE*32
      DATA TWOPI /.6283185307179587D+1/
!
! 4.  HISTORYd1
!   WHO   WHEN    WHAT
!   AEE   910515  Enhanced error messages written to the error file.
!   pet   981229  Improved formatting error message
!   pet   06-APR-99  Added support of FLUBY_WARNING for BATCH mode also
!   pet   2004.07.20 Added support of a kludge environment variable &
!                    DSOUC_IGNORE
!
! 5.  DSOUC PROGRAM STRUCTURE
!
!     SUBSTITUTE THE SOURCE POSTIONS AND GENERATE THE DIFFERENCE
!       TABLE. INITIALIZE ITEST FOR 'NO NON-ZERO DIFFERENCES'.
!
      PAUSE_FLYBY = .FALSE.
!
      IF ( JNSTR .GT. MAX_ARC_SRC)  THEN
           WRITE ( ERRSTR, 9942 ) JNSTR, MAX_ARC_SRC
9942       FORMAT ( '(DSOUC) Too many sources: ',I4,' but only',I4, &
     &              ' allowed per arc.' )
           CALL FERR ( INT2(1621), ERRSTR, INT2(0), INT2(0) )
      ENDIF
!
      DO I=1,JNSTR
!
! ------ Checking the table read from the data base
!
         ITEST = 0
         J=0
         DO WHILE ( ITEST .EQ. 0   .AND.  J .LT. NSTAR )
!
! --------- Running over the list of substitutes
!
            J=J+1
            IF ( JSTRN_CHR(I) .EQ. LSONAM_CHR(J) ) THEN ! Match found
!
                 NVSTARC(1,I) = SUBRD(1,J)
                 NVSTARC(2,I) = SUBRD(2,J)
                 STRDIF(1,I)  = SUBRD(1,J) - WSTARC(1,I)
                 STRDIF(2,I)  = SUBRD(2,J) - WSTARC(2,I)
                 WSTARC(1,I)  = SUBRD(1,J)
                 WSTARC(2,I)  = SUBRD(2,J)
!
! -------------- Convert differences from radians to milliarcseconds
! -------------- for testing and printing
!
                 DIFRA = (STRDIF(1,I)/TWOPI)*1.296D9
                 DIFDC = (STRDIF(2,I)/TWOPI)*1.296D9
                 ITEST = 1
                 IF ( DABS(DIFRA) .GE. COO_LIM .OR. &
     &                DABS(DIFDC) .GE. COO_LIM      ) THEN
!
! ------------------- Change is too big
!
                      WRITE ( ERRSTR, 1008 ) JSTRN_CHR(I), DIFRA, DIFDC, &
     &                        COO_LIM
 1008                 FORMAT ( "(DSOUC) The change in a position of source ", &
     &                         A8," is too big - quitting!  dif_ra = ",F18.6, &
     &                         " mas, dif_dc = ",F18.6," mas, limit = ",F8.2, &
     &                         " mas" )
                      WRITE ( 23, '(A)' ) ERRSTR(1:I_LEN(ERRSTR))
                      WRITE (  6, '(A)' ) ERRSTR(1:I_LEN(ERRSTR))
                      CALL GETENVAR ( 'DSOUC_IGNORE', DSOUC_IGNORE )
                      IF ( DSOUC_IGNORE(1:3) .EQ. 'YES' .OR. &
     &                     DSOUC_IGNORE(1:3) .EQ. 'yes'      ) THEN
                           WRITE ( ERRSTR, '(A)' ) '(DSOUC) Nevertheless, continue'
                           WRITE ( 23, '(A)' ) ERRSTR(1:I_LEN(ERRSTR))
                           WRITE (  6, '(A)' ) ERRSTR(1:I_LEN(ERRSTR))
                         ELSE
                           CALL FERR ( INT2(1628), ERRSTR, INT2(0), INT2(0) )
                           CALL PAUSE ( 'DSOUC' )
                           CALL EXIT ( 1 )
                      END IF
                 END IF  ! change is too big
            END IF  ! match found
        END DO  ! running over the list of substitutes
!
        IF ( ITEST .EQ. 0 ) THEN  ! Not match found
             IF ( FLYBY_WARNING ) THEN
                  CALL CLRCH ( BUFSTR )
                  WRITE  ( BUFSTR, 101 ) JSTRN_CHR(I)
 101              FORMAT ( 1X,'Warning: (DSOUC) source ',A8,' in not found ', &
     &                        'in the source flyby file' )
                  WRITE (  6, '(A)' ) BUFSTR(1:I_LEN(BUFSTR))
                  IF ( KBATCH ) THEN
                       WRITE ( 23, '(A)' ) BUFSTR(1:I_LEN(BUFSTR))
                     ELSE 
                       IF ( .NOT. PAUSE_FLYBY ) CALL UN_CURSES()
                       PAUSE_FLYBY = .TRUE.
                  END IF
              ENDIF
        END IF  ! Not match found
      END DO  ! Checking the table read from the data base
!
      IF ( PAUSE_FLYBY ) THEN
           CALL FERR ( INT2(1622), '(DSOUC) Flyby warning when parsing file '// &
     &                 FILE_NAME, INT2(0), INT2(0) )
      END IF
!
      RETURN
      END  !#! DSOUC  #!#
