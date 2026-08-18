      SUBROUTINE PMCMB ( IRNSV, IX1T3, IUER )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  PMCMB PROGRAM SPECIFICATION
!
! 1.1 Combine the appropriate globl and arc parameters
!
! 1.2 REFERENCES:
!
! 2.  PMCMB INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables: None
!
! 2.3 OUTPUT Variables:
!
      INTEGER*2 IRNSV(2)
      INTEGER*4 IX1T3(*)
!
! IRNSV - Run code
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'erm.i'
      INCLUDE 'socom.i'
      INCLUDE 'socom_plus.i'
      INCLUDE 'prfil.i'
      INCLUDE 'precm.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc2.i'
      INCLUDE 'glbc3.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'baccm.i'
      INTEGER*2 NUMST, ISIT(4,MAX_STA), NUMSR, ISRC(4,MAX_SRC)
      REAL*8    APRIORP(3,MAX_STA), APRIORV(3,MAX_STA), APRIORS(3,MAX_SRC), &
     &          RTIME0
      COMMON  / CGMOUT / ISIT, NUMST, APRIORP, APRIORV, RTIME0, APRIORS, &
     &                   NUMSR, ISRC
!
! 2.5 SUBROUTINE INTERFACE
!
!     CALLING SUBROUTINES: back
!       CALLED SUBROUTINES: setst
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2   IPARM1(10,M_GPA), IPARM2(10,M_GPA), IPARM3(10,M_GPA)
      COMMON     / PARAM / IPARM1, IPARM2, IPARM3
      CHARACTER   LPARM1(M_GPA)*20, LPARM2(M_GPA)*20, LPARM3(M_GPA)*20
      CHARACTER   LPARM22(M_GPA)*20
      EQUIVALENCE (IPARM1,LPARM1), (IPARM2,LPARM2), (IPARM3,LPARM3)
!
      INTEGER*2  ISTS, IWDS
      INTEGER*4  ITEMP, IDUM1, IDUM2, I, J, NPARM2_NEW
      INTEGER*2  ISTR_LEN / 20 /
      INTEGER*4  J1
      LOGICAL*2  KGLOB
      CHARACTER  ERRSTR*256, BUFSTR*80, DATE*8, STR*20
      LOGICAL*4  FL_FOUND
      INTEGER*4  K1
      INTEGER*4  ICMP, INOD, IUER
      DATA       DATE/'93.07.08'/
      INTEGER*2  INT2_ARG
      INTEGER*4  INT4
      INT4(INT2_ARG) = INT(INT2_ARG,KIND=4)
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
!
! 4.  HISTORY
!   WHO   WHEN    WHAT
!   MWH   910114  Added more error messages.
!   AEE   910515  Enhanced error messages written to the error file.
!   jmg   960610  Remove holleriths.
!   pet   970228  Added calculateion IX1T3
!   pet   970403  Simplified comments
!   pet   981228  Fixed a bug in error message 134 (PARMB)
!   pet   990305  Improved comments. Added trap of internal control. Added
!                 support of actual parameter IUER.
!  :2002.12.19:jwr: TRUE__L2 and FALSE__L2 introduced for -i2 removal
!   pet  2005.02.28   Added update of volatile data structures for
!                     non-linear site position motion estimation.
!   pet  2007.06.11   Improved error messages in the case of more than one &
!                     parameter was not found in the CGM
!
! 5.  PMCMB PROGRAM STRUCTURE
!
      IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6)) ) THEN
           CALL CLEAR_MN()
           CALL SETCR_MN ( 63, 0 )
           WRITE ( BUFSTR, 1812 ) DATE
 1812      FORMAT("BACK Ver ",A8)
           CALL REVERSE_ON_MN()
           CALL ADDSTR_F  (BUFSTR )
           CALL REVERSE_OFF_MN()
           CALL NL_MN()
      ENDIF
!
      CALL USE_GLBFIL ( 'OR' )
      OLD_USER_PART = NUM_USER_PART
      KGLOB         = KGLOBONLY
      CALL USE_GLBFIL_2 ( 'R'  )
!
! --- Reading PARF-file   (prfil.i)
!
      CALL USE_PARFIL ( 'ORC' )
!
! --- NPARM1, LPARM1 -- list of all parameters of the current session
!
      KGLOBONLY = .FALSE.
      CALL GET_NAMES ( LPARM1, ISTR_LEN, M_GPA, NPARM1, TRUE__L2, FALSE__L2 )
      CALL DEPAR()
!
! --- NPARM2, LPARM2 -- list of global parameters of the current session
! ---                   (NB: a bit after its meaning will be changed!)
!
      CALL GET_NAMES ( LPARM2, ISTR_LEN, M_GPA, NPARM2, TRUE__L2, FALSE__L2 )
!
! --- Remove local source admittance parameters. In fact GET_NAMES should
! --- be called with L2_TRUE last argument. I'm hesitatant to turn it on
! --- now (2007.08.10), because I'm afrfraid of causing a new regression bug.
! --- This should be tested later.
!
      NPARM2_NEW = 0
      DO 410 J1=1,NPARM2
         IF ( LPARM2(J1)(1:11) .NE. 'LCL_SOU_ADM' ) THEN
              NPARM2_NEW = NPARM2_NEW + 1
              LPARM22(NPARM2_NEW) = LPARM2(J1)
         END IF
 410  CONTINUE 
      NPARM2 = NPARM2_NEW
      IF ( NPARM2 > 0 ) THEN
           CALL LIB$MOVC3 ( 20*NPARM2, %REF(LPARM22), %REF(LPARM2) )
      END IF
!
! --- NPARM3, LPARM3 -- rearranged list of all parameters of the current
! --- session: first arc parameters, then global parameters
!
      CALL CUPARM  ( NPARM1, LPARM1, NPARM2, LPARM2, NPARM3, LPARM3, IGLBLS, &
     &               IARCS, INT2(1), M_GPA )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!      write ( 6, * ) 'pmcmb-1: nparm1 = ',nparm1,' nparm2 = ',nparm2, & ! %%
!     &       ' npram3 = ',nparm3                              ! %%%%%%%%%%%%
!      write ( 6, * ) 'pmcmb-1: iglbls = ', iglbls, ' iarcs = ', iarcs ! %%%%%%
!        do 610 k1=1,nparm1                                    ! %%%%%%%%%%%%
!           write ( 6, * ) 'pmcmb: k1=', k1,'  >>',lparm1(k1),'<< '  ! %%%%%%%%%%%%
! 610    continue                                              ! %%%%%%%%%%%%
!        do 620 k1=1,nparm2                                    ! %%%%%%%%%%%%
!           write ( 6, * ) 'pmcmb: k2=', k1,'  >>',lparm2(k1),'<< '  ! %%%%%%%%%%%%
! 620    continue                                              ! %%%%%%%%%%%%
!        do 630 k1=1,nparm3                                    ! %%%%%%%%%%%%
!           write ( 6, * ) 'pmcmb: k3=', k1,'  >>',lparm3(k1),'<< '  ! %%%%%%%%%%%%
! 630    continue                                              ! %%%%%%%%%%%%
!        write ( 6, * ) ' iarcs = ',iarcs,' iglbls = ',iglbls  ! %%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Get unmix cross reference and ISTRUC array
!
      CALL CXEPAR_OPT20 ( LPARM3, IX3T1,  NPARM3, LPARM1, NPARM1 )
      CALL CXEPAR_OPT20 ( LPARM1, IX1T3,  NPARM1, LPARM3, NPARM3 )
      IF ( NPARM2 .GT. MAX_STRUC*16 ) THEN
           WRITE ( 6, * ) ' NPAR2 =',NPARM2,'  MAX_STRUC = ',MAX_STRUC
           CALL ERR_LOG ( 8591, IUER, 'PMCMB', 'Trap of internal '// &
     &         'control: parameter MAX_STRUC defined in '// &
     &         '$PSOLVE_ROOT/include/glbcm.i is too small. SOLVE has to '// &
     &         'be re-compiled and re-linked to handle your case' )
           CALL EXIT ( 1 )
      END IF
!
      CALL SETST  ( ISTRUC, NPARM1, LPARM1, NPARM2, LPARM2 )
!
! --- Reading COVFxx file. Values from SOCOM, PARFIL and other blocks will
! --- bee read from COVFxx file. Previous values from arc scratch files
! --- will be forgotten.
!
      CALL ACS_COVFIL   ( 'O'  )
      CALL USE_COVF_COM ( 'R' )
      CALL ACS_COVFIL   ( 'C'  )
      RTIME0 = TIME0
!
! --- Create volatile objects for non-linear site position estimates
!
      IF ( L_HPE > 0 .AND. ADR_HPE .NE. 0 ) THEN
           CALL HPESOL_CREATE ( %VAL(ADR_HPE) )
         ELSE
           FL_HPESOL = .FALSE.
      END IF
      IF ( L_SPE > 0 .AND. ADR_SPE .NE. 0 ) THEN
           CALL SPESOL_CREATE ( %VAL(ADR_SPE) )
         ELSE
           FL_SPESOL = .FALSE.
      END IF
!
      IF ( L_EERM > 0 .AND. ADR_EERM .NE. 0 ) THEN
           CALL EERM_CREATE ( %VAL(ADR_EERM) )
         ELSE
           FL_EERM = .FALSE.
      END IF
!
! --- Substitute a priori values of station positions and velocites from
! --- global SOCOM/PARFIL
!
      NUMST=NUMSTA
      DO I=1,NUMST
         DO J=1,4
            ISIT(J,I) = ISITN(J,I)
         ENDDO
         DO J=1,3
            APRIORP(J,I) = VSITEC(J,I)
            APRIORV(J,I) = VSITEV(J,I)
         ENDDO
      ENDDO
!
! --- Substitute a priori values of sources positions from global SOCOM/PARFIL
!
      NUMSR = NUMSTR
      DO I=1,NUMSR
         DO J=1,4
            ISRC(J,I) = ISTRN(J,I)
         ENDDO
         DO J=1,2
            APRIORS(J,I) = VSTARC(J,I)
         ENDDO
      ENDDO
!
! --- Get list of all global parameters of the RUN. NPARM2 now will keep
! --- the number of global parameters in entire RUN.
!
      CALL GET_NAMES ( LPARM2, ISTR_LEN, M_GPA, NPARM2, TRUE__L2, TRUE__L2 )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        do 621 k1=1,nparm2                                             ! %%%%
!           write ( 6, * ) 'pmcmb-621 k2=', k1,'  >>',lparm2(k1),'<< '  ! %%%%
! 621    continue                                                       ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
! --- Save run code of the session
!
      IRNSV(1)=IRNCD(1)
      IRNSV(2)=IRNCD(2)
!
! --- Check for errors
!
      CALL CXEPAR_OPT20 ( LPARM2, IX2T3, NPARM2, LPARM3, NPARM3 )
      DO I=1,NPARM2
         IF ( IX2T3(I).GT.0  .AND.  IX2T3(I) .LE. IARCS ) THEN
!
! ----------- The following write added 910114 by MWH to provide more info
! ----------- when this error occurs
!
              if ( kscreen ) then
                   write ( bufstr, 110 ) i, lparm2(i)
 110               format ( '(PARMB) Arc parameter #',I4,': "',A, &
     &                      '" is in the CGM' )
                   call addstr_f ( bufstr )
                   call nl_mn()
              endif
              WRITE ( ERRSTR, 110 ) I, LPARM2(I)
              CALL FERR ( INT2(134), ERRSTR(1:I_LEN(ERRSTR)), INT2(0), INT2(0) )
         ENDIF
      ENDDO
!
      IF ( IGLBLS.GT.0 ) THEN
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!   write ( 6, * ) 'pmcmb-270 iarcs,iglbls= ', IARCS, IGLBLS, ' NPARM2= ', NPARM2  ! %%%%%%%
!              do j=1,nparm2                                                       ! %%%%%%%
!                 write ( 6, * ) 'j= ',j, ' IX2T3(j)= ', IX2T3(J) ! %%%%%%%%%%%%%%%%%%%%%%%%
!              enddo                                              ! %%%%%%%%%%%%%%%%%%%%%%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
           FL_FOUND = .TRUE.
           DO I=IARCS+1,IARCS+IGLBLS
              DO J=1,NPARM2
                 IF ( IX2T3(J) .EQ. I ) GOTO 10
              ENDDO
              FL_FOUND = .FALSE.
!
! ----------- The following write added 910114 by MWH to provide more info
! ----------- when this error occurs
!
              IF ( KSCREEN ) THEN
                   WRITE  ( BUFSTR, 120 ) I-IARCS, LPARM3(I)
 120               FORMAT ( '(PMCMB) Global parameter #',I4,': "',A, &
     &                      '" not in CGM' )
                   CALL ADDSTR_F ( BUFSTR )
                   CALL NL_MN()
              ENDIF
              WRITE ( ERRSTR, 120  ) I-IARCS, LPARM3(I)
              WRITE (  6, '(A)' ) ERRSTR(1:I_LEN(ERRSTR))
              WRITE ( 23, '(A)' ) ERRSTR(1:I_LEN(ERRSTR))
!@              CALL FERR ( INT2(135), ERRSTR, INT2(0), INT2(0) )
!
10            CONTINUE
           ENDDO
           IF ( .NOT. FL_FOUND ) THEN
                CALL ERR_LOG ( 8592, IUER, 'PMCMB', 'Some global '// &
     &              'parameters have not been found in CGM' )
                RETURN 
           END IF
      ENDIF
!
! --- Add additional globals in for covariance run:  NPARM3 is arc: globals
! --- count, whereas NPARM1 is count of parms in this arc
!
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        do 625 k1=1,nparm2                                    ! %%%%%%%%%%%%
!           write ( 6, * ) 'pmxmb-625 k25=', k1,'  >>',lparm2(k1),'<< ' ! %%%
! 625    continue                                                       ! %%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      IF ( CORLN ) THEN
           CALL CUPARM ( NPARM3, LPARM3, NPARM2, LPARM2, NPARM1, LPARM1, &
     &          IDUM1, IDUM2, INT2(0), M_GPA )
!
! -------- Also switch counts for iparm1 and iparm3, nparm1 should be the
! -------- parameters in this arc, nparm3 is the combined arc:global count
!
           ITEMP=NPARM3
           NPARM3=NPARM1
           NPARM1=ITEMP
!
! -------- call XEPAR:  generate cross reference lists.
! -------- IX2T3 is the wrong name but, it is the correct array to put the
! -------- result in
!
           CALL CXEPAR_OPT20 ( LPARM2, IX2T3, NPARM2, LPARM1, NPARM3 )
      ENDIF
!
! --- Accumulate the number of arc parameters for later stats use
!
      IF ( IARCNM.GT.1 ) THEN
           CNPARAM = CNPARAM+IARCS
        ELSE
           CNPARAM = IARCS
      ENDIF
!
      NUM_USER_PART = OLD_USER_PART
      KGLOBONLY     = KGLOB
      CALL USE_GLBFIL ( 'WC' )
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!        write ( 6, * ) 'PACMB-339 nparm2= ', nparm2                    ! %%%%
!        do 626 k1=1,nparm2                                             ! %%%%
!           write ( 6, * ) 'pmcmb-626 k1=', k1,'  >>',lparm2(k1),'<< '  ! %%%%
! 626    continue                                                       ! %%%%
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  PMCMB  #!#
