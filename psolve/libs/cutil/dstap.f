      SUBROUTINE DSTAP(LSINAM,NSITE,SUBXYZ,JNSTA,JSITN, &
     &                 WSITEC,SITDIF,FLYBY_WARNING,NVSITEC)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  DSTAP PROGRAM SPECIFICATION
!
! 1.1 Calculate site position differences
!
! 1.2 REFERENCES:
!
! 2.  DSTAP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 NSITE, JNSTA, LSINAM(4,*)
      INTEGER*2 JSITN(4,*)
      LOGICAL*2 FLYBY_WARNING
      REAL*8 SUBXYZ(3,*)
      integer*2 ldbnam(5,max_dbs),idbver(max_dbs),numdd
      integer*4 idbend(max_dbs)
      character*10 ldbnam_c(max_dbs)
      equivalence (ldbnam,ldbnam_c)
!
! FLYBY_WARNING - True if we want message when no match found
! JSITN - Names of stations in this arc
! JNSTA - Number of stations in this arc
! LSINAM - Station names for alternate positions
! NSITE - Number of stations with alternate positions
! SUBXYZ - Alternate positions
!
! 2.3 OUTPUT Variables:
!
      REAL*8 SITDIF(3,*),WSITEC(3,*),NVSITEC(3,*)
!
! NVSITEC - Site coordinates from mod file
! SITDIF - Site position differences
! WSITEC - Array of site coordinates
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES:
!       CALLED SUBROUTINES:
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, K, ITEST, J, TRIMLEN
      LOGICAL*2 PAUSE_FLYBY
      CHARACTER*80 BUFSTR
!
! I,J,K - Loop indices
! ITEST - Flag used for 'no match' condition
! PAUSE_FLYBY - True if we are to pause and warn of missing sites
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!   :95.01.24:jwr: Improved error messages
!   kdb  951207  Integer*4 number of observations.
!   pet  06-APR-99  Added support of FLUBY_WARNING for BATCH mode also
!
! 5.  DSTAP PROGRAM STRUCTURE
!
      PAUSE_FLYBY = .FALSE.
!
!     SUBSTITUTE THE SITE POSTIONS AND GENERATE THE DIFFERENCE
!         TABLE
!
      IF(JNSTA.GT.MAX_ARC_STA) THEN
        CALL DBPOX(NUMDD,LDBNAM,IDBVER,IDBEND )
        If(kscreen) then
          write(6, &
     &    '("cutil/dstap: Fatal error at session ",a10, &
     &    "Ver. ",i4)') ldbnam_c(1),idbver(1)
          write(6, &
     &    '("This arc contains",i3," stations!",/, &
     &    "Only",I3," per arc allowed.")') jnsta,max_arc_sta
        else
          call start_mn()
          CALL clear_mn()
          write(bufstr, &
     &    '("cutil/dstap: Fatal error at session ",a10, &
     &    "Ver. ",i4)') ldbnam_c(1),idbver(1)
          call addstr_f(bufstr )
          call nl_mn()
          write(bufstr, &
     &    '("This arc contains",i3," stations!",/, &
     &    "Only",I3," per arc allowed.")') jnsta,max_arc_sta
          call addstr_f(bufstr )
          call nl_mn()
        endif
        call ferr( INT2(165), 'cutil/dstap 165', INT2(0), INT2(0) )
      ENDIF
!
      DO  I=1,JNSTA
!       checking the table read from the data base.
        ITEST = 0
        J = 0
        DO WHILE (ITEST.EQ.0 .AND. J.LT.NSITE)
!       running over the list of alternate postions
          J=J+1
          IF(JSITN(1,I).EQ.LSINAM(1,J) .AND. &
     &       JSITN(2,I).EQ.LSINAM(2,J) .AND. &
     &       JSITN(3,I).EQ.LSINAM(3,J) .AND. &
     &       JSITN(4,I).EQ.LSINAM(4,J)) &
     &       THEN  !match found
            NVSITEC(1,I)= SUBXYZ(1,J)
            NVSITEC(2,I)= SUBXYZ(2,J)
            NVSITEC(3,I)= SUBXYZ(3,J)
            SITDIF(1,I) = SUBXYZ(1,J) - WSITEC(1,I)
            SITDIF(2,I) = SUBXYZ(2,J) - WSITEC(2,I)
            SITDIF(3,I) = SUBXYZ(3,J) - WSITEC(3,I)
!
!           Allowable difference is 10km. Big!
            IF(DABS(SITDIF(1,I)).LT.10000.D0 .AND. &
     &         DABS(SITDIF(2,I)).LT.10000.D0 .AND. &
     &         DABS(SITDIF(3,I)).LT. &
     &         10000.D0)THEN  !substitution ok
              ITEST = 1
              WSITEC(1,I)= SUBXYZ(1,J)
              WSITEC(2,I)= SUBXYZ(2,J)
              WSITEC(3,I)= SUBXYZ(3,J)
            ELSE  !substitution too big
              CALL DBPOX(NUMDD,LDBNAM,IDBVER,IDBEND )
              if(kscreen) then
                call start_mn()
                CALL clear_mn()
                WRITE(bufstr,153) (JSITN(K,I),K=1,4)
                call addstr_f(bufstr )
                call nl_mn()
                write(bufstr,154) (SUBXYZ(K,J),K=1,3)
                call addstr_f(bufstr )
                call nl_mn()
                write(bufstr,155) (WSITEC(K,I),K=1,3)
                call addstr_f(bufstr )
                call nl_mn()
                write(bufstr,156) (SITDIF(K,I),K=1,3)
                call addstr_f(bufstr )
                call nl_mn()
              else
                write(6,'("cutil/dstap: Fatal error at session ",a10, &
     &          "Ver. ",i4)') ldbnam_c(1),idbver(1)
                WRITE(6,153) (JSITN(K,I),K=1,4)
                write(6,154) (SUBXYZ(K,J),K=1,3)
                write(6,155) (WSITEC(K,I),K=1,3)
                write(6,156) (SITDIF(K,I),K=1,3)
 153            FORMAT(" The change to a site position is too", &
     &          " big - Quitting",1X,4A2)
 154            format(" Sub ",3F15.4," m")
 155            format(" db  ",3F15.4," m")
 156            format(" Dif ",3F15.4," m")
              endif
              call ferr( INT2(166), 'cutil/dstap: 166', INT2(0), INT2(0) )
            END IF  !substitution too big
          END IF  !match found
        END DO  !running over the list of alternate postions
!
        IF ( ITEST .EQ. 0 ) THEN  ! No match found
             IF ( FLYBY_WARNING ) THEN
                  WRITE  ( BUFSTR, 1070 ) (JSITN(K,I),K=1,4)
 1070             FORMAT ( 'Warning: (DSTAP) ', 4A2, 1X, &
     &                     'not in site flyby file' )
                  IF ( KSCREEN ) THEN
                       PAUSE_FLYBY = .TRUE.
                       CALL START_MN()
                       CALL CLEAR_MN()
                       CALL ADDSTR_F ( BUFSTR )
                       CALL NL_MN()
                       CALL REFRESH_MN()
                     ELSE
                       WRITE (  6, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
                       WRITE ( 23, '(A)' ) BUFSTR(1:TRIMLEN(BUFSTR))
                  ENDIF
             ENDIF
        END IF  ! No match found
      END DO  ! Checking the table read from the data base.
!
      IF ( PAUSE_FLYBY ) THEN
           CALL FERR ( INT2(167), '(DSTAP) Flyby warning', INT2(0), INT2(0) )
      END IF
!
      RETURN
      END  !#!  DSTAP  #!#
