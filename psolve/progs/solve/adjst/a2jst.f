      SUBROUTINE A2JST ( SCSIG, EOPTRACE, KCONS, MAT, LBUF_LEN, LBUF, IPTR, &
     &                   PAGEWID, APR_VAL, APR_SIG, EST_VAL, IUER )
      IMPLICIT NONE
!
      INCLUDE    'solve.i'
      INCLUDE    'glbc4.i'
      INCLUDE    'erm.i'
      INCLUDE    'socom.i'
      INCLUDE    'socom_plus.i'
      INCLUDE    'precm.i'
!
!     A2JST has been converted into a small control subroutine.
!     EOP estimation has two styles.
!
!     Style 1 was implemented in March 1997 and it models eop
!     with a global rate offset and segments where the offsets
!     at each segment break is is estimated. The old way of handling
!     this was with segments and estimating rates at each segment
!     break. This old scheme causes excessive computational overhead.
!     the new scheme works will with Petrov's B3D algorithms.
!
!     Style 2 is all old (pre 1997) eop parameteriztions schemes
!     except segmentation.
!
!     These were put into two seperate routines in order to build
!     a fire wall between the code for the two methods. I (Jim Ryan)
!     was afraid I could not make the new mode work without screwing
!     up the old modes. (There are >1500 lines of code for the
!     old modes.
!
!     The new routine a2jst_noeop contains the code for all adjustments
!     in old a2jst which did not involve EOP.
!
!     :97.03.05:jwr: Created.
!     :97.11.14:jwr: An array for eop indicies defined here and passed down.
!     pet   1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                       eliminated common block adj_buf
!     pet   2001.01.26  Added recomputation oif the number of parameters and
!                       some important indeces once more since if the number
!                       of user parameters was changed, some counters,
!                       like IPSTP were changed also.
!     pet   2002.04.08  Added argumens APR_VAL, APR_SIG, EST_VAL. They should
!                       transport some values needed to be put in listing in
!                       SINEX format
!     pet   2003.08.13  Added error control
!     pet   2003.11.18  Forced bypassing an attemptto print EOP when a listing
!                       of global parameters is being prepared
!     pet   2006.01.19  Added support of ERM parameters
!
      INTEGER*4   IUER, IER
      REAL*8      SCSIG(*), MAT(*), EOPTRACE
      REAL*8      APR_VAL(N__SNX), APR_SIG(N__SNX), EST_VAL(N__SNX)
      INTEGER*4   IHEO, ICMP
      LOGICAL*2   KCONS
      INTEGER*2   I, J
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
      INTEGER*4   NPARM, EOP_INDICIES(3,3)
      DO I=1,3
         DO J=1,3
            EOP_INDICIES(I,J) = 0
         END DO
      END DO
!
! --- Recompute the number of parameters once more
!
      CALL PARCN()
!
      NPARM = NPARAM-IPSTP
      IF ( KGLOBALS ) THEN
           NPARM = NPARAM-IPSTP
           IF ( L_EHEO > 0 ) THEN
!
! ------------- Skip EHEO parameters
!
                DO IHEO = 1, L_EHEO ! Running over HEO constituents
                   IF ( HEOSOL(IHEO)%FL_EST(HEO__E1E2) ) THEN
                        NPARAM = NPARAM + 2
                   END IF
                   IF ( HEOSOL(IHEO)%FL_EST(HEO__E3) ) THEN
                        NPARAM = NPARAM + 2
                   END IF
                   IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E1E2) ) THEN
                        NPARAM = NPARAM + 2
                   END IF
                   IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E3) ) THEN
                        NPARAM = NPARAM + 2
                   END IF
                END DO
           END IF
!
           IF ( L_EERM > 0 ) THEN
!
! ------------- Skip EERM parameters
!
                DO I=1,3
                   NPARM = NPARM + EERM%NKNOTS(I) + EERM%DEGREE(I) - 1
                END DO
           END IF
         ELSE
!
! -------- Skip EHEO and EERM parameters if they were estimated for this session
!
           IF ( L_EHEO > 0 ) THEN
                DO IHEO = 1, L_EHEO ! Running over HEO constituents
                   IF ( HEOSOL(IHEO)%FL_EST(HEO__E1E2) ) THEN
                        NPARM = NPARM + 2
                   END IF
                   IF ( HEOSOL(IHEO)%FL_EST(HEO__E3) ) THEN
                        NPARM = NPARM + 2
                   END IF
                   IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E1E2) ) THEN
                        NPARM = NPARM + 2
                   END IF
                   IF ( HEOSOL(IHEO)%FL_EST_VEL(HEO__E3) ) THEN
                        NPARM = NPARM + 2
                   END IF
                END DO
           END IF
!     
           IF ( L_EERM > 0 ) THEN
!
! ------------- Skip EERM parameters
!
                DO ICMP=1,3
                   IF ( IND_EERM_NOD(ICMP) > 0 ) THEN
                        NPARM = NPARM + EERM%DEGREE(ICMP) + 1 + EERM_OVR(ICMP)
                   END IF
                END DO
           END IF
!
           IF ( EOP_STYLE(1) .EQ. EOP__RATES_AND_SEGS  .OR. &
     &          EOP_STYLE(1) .EQ. EOP__SEGS_ONLY            ) THEN
                CALL A2JST_SEGEOP ( SCSIG, EOPTRACE, KCONS, MAT, NPARM, &
     &                              LBUF_LEN, LBUF, IPTR, PAGEWID )
             ELSE
                CALL A2JST_PLNEOP ( SCSIG, EOPTRACE, KCONS, MAT, NPARM, &
     &                              EOP_INDICIES, LBUF_LEN, LBUF, IPTR, &
     &                              PAGEWID, N__SNX, APR_VAL, APR_SIG, EST_VAL )
           ENDIF
      ENDIF
!
      CALL ERR_PASS ( IUER, IER )
      CALL A2JST_NOEOP ( SCSIG, EOPTRACE, KCONS, MAT, NPARM, EOP_INDICIES, &
     &                   LBUF_LEN, LBUF, IPTR, PAGEWID, &
     &                   N__SNX, APR_VAL, APR_SIG, EST_VAL, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 3751, IUER, 'A2JST', 'Errors in A2JST_NOEOP' )
           RETURN
      END IF
      CALL ERR_LOG ( 0, IUER )
!
      RETURN
      END  !#!  A2JST  #!#
