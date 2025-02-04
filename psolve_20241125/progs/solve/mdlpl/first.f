      SUBROUTINE FIRST(ISTART,INUMBER,ISTARTC,INUMBERC, &
     &  ISTART_EOP,INUMBER_EOP,IWHICH,BTC)
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  FIRST PROGRAM SPECIFICATION
!
! 1.1 Make parameter list and find start and number of atmosphere and
!     clock parameters
!
! 1.2 REFERENCES:
!
! 2.  FIRST INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*2 ISTART_EOP(*),INUMBER_EOP(*)
      REAL*8 BTC(*)
!
! ISTART_EOP - First earth orientation parameter to use
! INUMBER_EOP - array of earth orientation parameters
! BTC - To save B vector for total clock
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 ISTART(*),INUMBER(*),ISTARTC(*),INUMBERC(*),IWHICH(*)
!
! ISTART - First atmosphere parameter to use
! INUMBER - Array of atmosphere parameters
! ISTARTC - First clock parameter to use
! INUMBERC - Array of clock parameters
! IWHICH - Atmosphere, clock, or eop parameter?
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'oborg.i'
      INCLUDE 'socom.i'
      INCLUDE 'prfil.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'precm.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: mdlpl
!       CALLED SUBROUTINES: ind_par
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 IXY_U
      INTEGER*2 IPARM(10,M_GPA)
      CHARACTER CPARM(M_GPA)*20
      EQUIVALENCE ( IPARM, CPARM )
      COMMON/PARAM/IPARM
!
      INTEGER*2 ISTS, IKEY1(5), IKEY2(5), IDUM, ICHMV, I, J, IEOP_TYPE(3)
      INTEGER*4 NPARM
      INTEGER*2 ICUR_EO_PT, IEOP, IROT, IORD, IROTT, IWDS, ISTR_LEN
      LOGICAL*4 CHECK_STABIT
      logical*2 kbit
!
      PARAMETER  ( IWDS     = 10 ) 
      PARAMETER  ( ISTR_LEN = 2*IWDS ) 
      DATA         ISTS/1/, IEOP_TYPE/1,1,2/
!
! 4.  HISTORY
!  WHO   WHEN        WHAT
!  pet   1999.07.28  Removed unused variables
!  jwr   2002.12.19  TRUE__L2 and FALSE__L2 introduced for -i2 removal
!  pet   2003.11.05  Replaced antient PARMS call with modern GET_NAMES
!  pet   2005.03.18  Replaced archiac calls to ICHMV to modern string &
!                    maniputation routines.
! 5.  FIRST PROGRAM STRUCTURE
!
!   set up glbfil
!
      CALL USE_GLBFIL('OR' )
      CALL USE_GLBFIL_4('RC' )
!
      CALL FLYBY_APRIOR()
!
!**** OPEN DATA FILE AND NORMAL EQUATIONS FILE
!
      CALL ACS_OBSFIL('O' )
!      CALL USE_NRMFIL(SCAL,NPARAM,'ORC')
!
! MAKE THE PARAMETER LIST
!
      CALL SOCOM_EXT()
      CALL GET_NAMES ( CPARM, ISTR_LEN, M_GPA, NPARM, TRUE__L2, FALSE__L2 )
!
! SAVE B VECTOR FOR TOTAL CLOCK
!
!     CALL DWMOV(B,1,BTC,1,NPARAM)
!      iblas1=1
!      nblas=nparam
!      call dcopy(nblas,b,iblas1,btc,iblas1)
!
! OKAY INDEX IT TO FIND THE START AND NUMBER OF ATMOSPHERE AND
! CLOCK PARAMETERS
!
      DO I=1,NUMSTA
!
! ------ Check STABIT_P or STABIT_G bit fields to bypass deselected station
!
         IF ( .NOT. CHECK_STABIT ( I ) ) GOTO 810
!
         IKEY1(1)=ISITN(1,I)
         IKEY1(2)=ISITN(2,I)
         IKEY1(3)=ISITN(3,I)
         IKEY1(4)=ISITN(4,I)
         CALL LIB$MOVC3 ( 2, %REF('A '), IKEY1(5) )
!@         IDUM=ICHMV( IKEY1, INT2(9), 2HA , INT2(1), INT2(2) )
         IKEY2(1)=ISITN(1,I)
         IKEY2(2)=ISITN(2,I)
         IKEY2(3)=ISITN(3,I)
         IKEY2(4)=ISITN(4,I)
         CALL LIB$MOVC3 ( 2, %REF('a '), IKEY2(5) )
!@         IDUM=ICHMV( IKEY2, INT2(9), 2Ha , INT2(1), INT2(2) )
         CALL IND_PAR ( IPARM, IWDS, NPARAM, IKEY1, IKEY2, INT2(9), ISTART(I), &
     &        INUMBER(I), IWHICH )
         CALL LIB$MOVC3 ( 2, %REF('C '), IKEY1(5) )
         CALL LIB$MOVC3 ( 2, %REF('c '), IKEY2(5) )
!@         IDUM=ICHMV( IKEY1, INT2(9), 2HC , INT2(1), INT2(2) )
!@         IDUM=ICHMV( IKEY2, INT2(9), 2Hc , INT2(1), INT2(2) )
         CALL IND_PAR ( IPARM, IWDS, NPARAM, IKEY1, IKEY2, INT2(9), &
     &        ISTARTC(I), INUMBERC(I), IWHICH )
!        DO J=1,NPARAM
!           IF(IWHICH(J).EQ.1) THEN
!
!            If this is a continued clock or atmosphere parameter, it takes
!            up an extra, false space in the b vector.  Ignore the extra space
!
!             B(J)=0.0D0
!
!  UNCOMMENT THIS DO LOOP TO GET MARGINAL SIGMAS
!
!            DO K=1,NPARAM
!              A(INDX4(K,J))=0.0D0
!            ENDDO
!           ENDIF
!         ENDDO
 810     CONTINUE
      ENDDO
!
! --- Earth orientation parameters are located through a different method:
! --- directly through variables in SOCOM.
!
      ICUR_EO_PT = NPARAM - IPSTP + 1 ! This is where the eo parameters start
      DO IEOP = 1,3
        IXY_U = IEOP_TYPE(IEOP)
        ISTART_EOP(IEOP) = ICUR_EO_PT
        IF (EOP_STYLE(IXY_U) .EQ. 1) THEN
!
!         There will be at least an offset and a global rate.
!
          INUMBER_EOP(IEOP) = 2
!
!         There may also be a series of rates associated with individual
!         epochs (the rate breaks).  There will be one for each epoch.
!
          IF (KBIT ( EOPA1_CHOICE(IXY_U), INT2(1) ) ) THEN
!            INUMBER_EOP(IEOP) = INUMBER_EOP(IEOP) + NROT_A1(IXY_U)
            INUMBER_EOP(IEOP) = INUMBER_EOP(IEOP) + NROT_A1(IXY_U)-1
          END IF
!
!         There may also be parameters associated with a diurnal sine wave
!         and/or a semi-diurnal sine wave.  Two parameters are associated
!         with each type of sine; the amplitudes of a sine and a cosine
!         which, when added together, form the diurnal/semi-diurnal sine.
!
          DO J = 1,2 !diurnal, then semi-diurnal
            IF (KBIT ( EOPA1_CHOICE(IXY_U), INT2(J+1)) ) THEN
              INUMBER_EOP(IEOP) = INUMBER_EOP(IEOP) + 2
            END IF
          END DO
        ELSE
          INUMBER_EOP(IEOP) = 0
          DO IROT = 1,NROT
            DO IORD = 1,4
              IF (IROTT(IROT,IEOP,IORD,LROT) .EQ. &
     &          1)INUMBER_EOP(IEOP) = INUMBER_EOP(IEOP) + 1
            END DO
          END DO
        END IF
        ICUR_EO_PT = ISTART_EOP(IEOP) + INUMBER_EOP(IEOP)
      END DO
!
!
!
      RETURN
      END
