      SUBROUTINE POSMAP2 ( VSC, NPARMC, NPARMV, ISTA, TYPEPR, STPT2, MODE, &
     &                     LBUF_LEN, LBUF, IPTR, PAGEWID )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  POSMAP2 PROGRAM SPECIFICATION
!
! 1.1 MAPP POSITIONS AND VELOCITIES FOR XYZ COORDINATES TO OTHER YEARS
!  ON INPUT WE ASSUME:
!
!    VSITEV(1...3,ISTA) = IS FINAL ADJUSTED VALUE FOR THE VELOCITIES
!    A(...), B(...)  STILL CONTAINS THE INVERTED NORMAL EQUATIONS AND
!                    ADJUSTMENTS, WITH COORDINATE AND VELOCITY PARAMETERS
!                    AND covariances FOR STATION ISTA UNMODIFIED
!    LSITEV, LSITEC, ISITN STILL VALID FOR THIS SOLUTION
!
! 1.2 REFERENCES:
!
! 2.  POSMAP2 INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'astro_constants.i'
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      CHARACTER*(*) TYPEPR, STPT2(MAX_PWC_EPS)
      REAL*8      VSC(3)
      INTEGER*2   NPARMC, NPARMV, ISTA, MODE
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID
      CHARACTER   LBUF(LBUF_LEN)*120
!
!    VSC    - A PRIORI STATION COORDINATES FOR THIS STATIONS
!    NPARMC - PARMETER NUMBER BEFORE FIRST STATION COORDINATE PARAMETER
!             FOR THIS STATION
!    NPARMV - PARMETER NUMBER BEFORE FIRST STATION VELOCITY   PARAMETER
!             FOR THIS STATION
!    ISTA   - STATION NUMBER OF THIS STATION
!    TYPEPR - FOR OUTPUT CONTROL
!    MODE   - 1 = station table
!             2 = minimum sigma
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INCLUDE 'socom.i'
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
      INCLUDE 'prfil.i'
      INCLUDE 'dmapp.i'
      REAL*8 ADJ(6), SIGM(21), AMAP(6,6), RES(6,6), RES2(21), ADJ2(6)
      REAL*8 SIGT(3), ADJT(3), DUM(6), SCALE(6)
      COMMON / TIMMAP / ADJ, SIGM, AMAP, RES, RES2, ADJ2, DUM, SCALE, ADJT, SIGT
!
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: a1jst
!       CALLED SUBROUTINES: utility routines
!
! 3.  LOCAL VARIABLES
!
      INTEGER*2 I, K, NPARM, ICMP(3), IYEAR, IYR
      INTEGER*2 IY, ID, IM, ITIME, YB, YE, NUMPOS, IY2
      INTEGER*4 INDX4, IBLAS0, IBLAS1, IBLAS7, NBLAS, iblas14
      LOGICAL*2 KBIT, EQUAL
      REAL*8    Z, ONEZ, FRACT
      REAL*8    FJLDY, DELTAT, REF, TU, TTEMP
      CHARACTER*1 TBLOUT
!
      DATA ICMP / 2HX , 2HY , 2HZ /
!
! 4.  HISTORY
!  WHO   WHEN    WHAT
!  mwh  940426  Support option to output station positions and sigmas for epoch
!               that minimizes sigmas (option 2).  For algorithm, see memo
!               of March 1, 1994 by J. Gipson (re: Minimum position sigma)
!  kdb  990206  Remove tabs.
!               Also Y2K fixes.
!               Also clean up a little obsolete code.
!  pet 1999.05.28. Made LBUF_LEN, LBUF, IPTR, PAGEWID formal arguments,
!                  eliminated common block adj_buf
!  pet 2001.04.17  Added check before an attempt to divide on zero
!  pet 2002.05.09  Shfted the output line one character to right
!  pet 2002.12.24  Replaced TIME0 with SIT_REF_EPOCH and replaced hard coded
!                  constants with named constants
!  pet 2003.01.17  Fixed a bug related to the lastest update: second term in
!                  computation of REF disappeared. Restored.
!  pet 2004.07.30  Fixed a bug related to INT2--> INT4 conversion
!
! 5.  POSMAP2 PROGRAM STRUCTURE
!
!   coordinate time of 80/10/17:00:00:00 in centuries from j2000
!                  within 1 millisecond, CHECK AGAINST PARTL
!
!
      NPARM=NPARMC
!
!  INITIALIZE
!
!!      ref = time0/100.0D0 - 2451544.5D0/36524.2191D0
      REF = SIT_EST_EPOCH/(100.0D0*JYEAR__DAYS) - &
     &       (J2000__JD-0.5D0)/(100.0D0*JYEAR__DAYS)
!
!  INITIALIZE MAPPING MATRIX
!
      IBLAS0=0
      IBLAS1=1
      Z=0.0D0
      NBLAS=36
      CALL DCOPY ( NBLAS, Z, IBLAS0, AMAP, IBLAS1 )
      NBLAS=6
      ONEZ=1.0D0
      IBLAS7=7
      CALL DCOPY ( NBLAS, ONEZ, IBLAS0, AMAP, IBLAS7 )
      FRACT = 0.D0
      CALL HOL2CHAR ( ITBLOUT, INT2(1), INT2(1), TBLOUT )
      IF ( MODE .EQ. 1 ) THEN
           YB=1979
           NUMPOS=17
           IF ( STAPOSNUM .GT. 0 ) NUMPOS = MIN ( STAPOSNUM, INT2(20) )
           IF ( STAPOSEPOCH .GT. 0 ) THEN
                YB = INT(STAPOSEPOCH)
                FRACT = STAPOSEPOCH - YB - 1.D0/365.D0
           ENDIF
           YE = YB + NUMPOS - 1
!
           IY = 0
           IF ( VSITED(ISTA) .NE. 0 ) THEN
                CALL MDYJL ( IM, ID, IY2, ITIME, VSITED(ISTA) )
                CALL NEWCENTS ( IY2, IY )
                IF ( IY .GT. YB) YB = IY+1
                IF ( IM .EQ. 1   .AND.   ID .EQ. 1) YB = YB - 1
           ENDIF
           DO I=1,NUMSTA
              IF ( I .NE. ISTA .AND. EQUAL( ISITN(1,ISTA), INT2(1), ISITN(1, &
     &             I), INT2(1), INT2(8) ) )THEN
                   IY = 0
                   IF ( VSITED(I) .NE. 0 ) THEN
                        CALL MDYJL(IM,ID,IY2,ITIME,VSITED(I) )
                        CALL NEWCENTS(IY2,IY )
                   END IF
                   IF ( VSITED(I) .GT. VSITED(ISTA)   .AND.  IY .LT. YE ) THEN
                        YE = IY
                        IF ( IM.EQ.1  .AND.  ID.EQ.1 ) YE = YE - 1
                   ENDIF
              ENDIF
           ENDDO
        ELSE ! imode
           IF ( .NOT. KBIT(LSITEV(1,1),ISTA) ) RETURN
           IF ( .NOT. KBIT(LSITEC(1,1),ISTA) ) RETURN
!
! -------- Note: the specific values of yb and ye are not actually used in this
! --------       mode (mode not equal to 1).  All that matters is that they be
! --------       set to the same value, so that the loop below will only
! --------       execute once.
!
           YB=1
           YE=1
           IF ( DABS( SIGM(INDX4(INT2(2),INT2(2))) + &
     &                SIGM(INDX4(INT2(4),INT2(4))) + &
     &                SIGM(INDX4(INT2(6),INT2(6))) ) .GT. 1.D-11 ) THEN
                DELTAT = -(SIGM(INDX4(INT2(2),INT2(1))) + &
     &                     SIGM(INDX4(INT2(4),INT2(3))) + &
     &                     SIGM(INDX4(INT2(6),INT2(5))))/ &
     &                    (SIGM(INDX4(INT2(2),INT2(2))) + &
     &                     SIGM(INDX4(INT2(4),INT2(4))) + &
     &                     SIGM(INDX4(INT2(6),INT2(6))))
              ELSE
                DELTAT = 1.D-11
           END IF
      ENDIF
!
      DO IYEAR=YB,YE
!
! ------ Use julian date, coordinate time is too hard to get for an arbitrary
! ------ date, and the effect is negligible but, for consistency
! ------ with partl, subtract from the correct ref coordinate time
!
! ------ In each mode, set iyr to the 2 digit year format for printing
!
         IF ( MODE .EQ. 1 ) THEN
!!              TTEMP = (FJLDY(1,1,IYEAR)+FRACT*365.D0)/365.2422D0
!!            TU = TTEMP/100.D0 - 2451544.5D0/36524.2191D0
!
              TTEMP = (FJLDY( INT2(1), INT2(1), IYEAR)+ &
     &        FRACT*365.D0)/JYEAR__DAYS
              TU = TTEMP/100.D0 - (J2000__JD-0.5D0)/(100.0D0*JYEAR__DAYS)
!
              DELTAT=(TU-REF)*100.0D0
              CALL DECENTS ( IYEAR, IYR )
            ELSE
              TU = REF+DELTAT/100.D0
!!            TTEMP = TU*36524.22 + 2451544.5D0
              TTEMP = TU*(100.0D0*JYEAR__DAYS) + (J2000__JD - 0.5D0)
              CALL MDYJL ( IM, ID, IY, ITIME, TTEMP )
         ENDIF
!
! ------ Finish mapping matrix
!
         NBLAS=3
         IBLAS14=14
         CALL DCOPY ( NBLAS, DELTAT, IBLAS0, AMAP(1,2), IBLAS14 )
!
! ----- ADJ2 = AMAP * ADJ = MAPPED ADJUSTMENTS TO A PRIORI STATION COORDINATES
!                                            [AND TOTAL VELOCITY (UNCHANGED)]
!
        CALL MRRR ( INT2(6), AMAP, INT2(6), ADJ, INT2(1), ADJ2 )
!
! ----- RES2 = AMAP * SIGM * [AMAP]T = MAPPED COVARIANCE MATRIX
!
        CALL MRPR ( INT2(6), AMAP, INT2(6), SIGM, INT2(6), RES )
        CALL MRRTP ( INT2(6), RES, INT2(6), AMAP, INT2(6), RES2 )
!
        ADJT(1) = ADJ2(1)
        ADJT(2) = ADJ2(3)
        ADJT(3) = ADJ2(5)
        SIGT(1) = SQRT(RES2(INDX4(INT2(1),INT2(1))))
        SIGT(2) = SQRT(RES2(INDX4(INT2(3),INT2(3))))
        SIGT(3) = SQRT(RES2(INDX4(INT2(5),INT2(5))))
        IF ( KSPOOL .AND. KMINOUT  .AND. MODE .EQ. 1 ) THEN
             IF ( STPT2(1) .EQ. 'Comp' .OR. STPT2(1) .EQ. 'Velo' ) THEN
                  STPT2(1)='      '
             END IF
             WRITE ( 23, 998 ) IYR+FRACT, (ISITN(K,ISTA),K=1,4), &
     &              (MONUMENTS(K,ISTA),K=1,2),stpt2(1), &
     &              (ICMP(K),(VSC(K)+ADJT(K))*1000.0D0,SIGT(K)*1000.0D0,K=1,3)
998          FORMAT ( 6X, 'Year ', &
     &                F5.2,1X,4A2,1X,2A2,1X,A6,3(1X,A2,F14.2,1X,F7.2,' mm',1X))
        ENDIF
        IF ( KSPOOL .AND. KFULLOUT ) THEN
             IF ( MODE .EQ. 1 ) THEN
                  CALL SCALER ( RES2, DUM, SCALE, INT2(6) )
                  WRITE(23,999) IYEAR+FRACT,      &
     &                          RES2(INDX4(INT2(1),INT2(3))), &
     &                          RES2(INDX4(INT2(1),INT2(5))), &
     &                          RES2(INDX4(INT2(3),INT2(5))), &
     &                          RES2(INDX4(INT2(1),INT2(2))), &
     &                          RES2(INDX4(INT2(3),INT2(4))), &
     &                          RES2(INDX4(INT2(5),INT2(6)))
  999             FORMAT(' Year ',F5.2,' X-Y Corr.',F6.3,' X-Z Corr.',F6.3, &
     &                                 ' Y-Z Corr.',F6.3,' X-Xv Corr.',F6.3, &
     &                                 ' Y-Yv Corr.',F6.3,' Z-Zv Corr.',F6.3)
               ELSE
                  WRITE ( 23, 997 ) ISTA, (ISITN(K,ISTA),K=1,4), &
     &                              (MONUMENTS(K,ISTA),K=1,5), IY,IM,ID
  997             FORMAT ( ' Minimum sigma: ',I3,'. ',4A2,1X,5A2, &
     &                     ' (Epoch = ',3I2.2,'):' )
             ENDIF
        ENDIF
!
        DO I = 1,3          !Run over X,Y,Z.
           IF ( KSPOOL .AND. KFULLOUT ) THEN
                WRITE ( 23, 1000 )  ( ISITN(K,ISTA), K=1,4), &
     &                 ( MONUMENTS(K,ISTA), K=1,5 ), &
     &                   ICMP(I), 'Comp',(VSC(I)+ADJT(I))*1000.D0,  'mm   ', &
     &                   ADJT(I)*1000.D0, 'mm   ', SIGT(I)*1000.D0, 'mm   ', &
     &                   SIGT(I)*WRMS(3)*1000.D0, 'mm   ', TYPEPR
 1000           FORMAT ( 5X,"  ",4A2,1X,5A2,1X,A2,A4,4X, &
     &                   F16.2 ' ',A5, &
     &                   F12.3,' ',A5,' ', &
     &                   F15.3,' ',A5,' ', &
     &                   F15.3,' ',A5,' ',A6)
           ENDIF
           IF ( KSCREEN ) THEN
                IPTR=IPTR+1
                WRITE ( LBUF(IPTR), 3500)       (ISITN(K,ISTA),K=1,4), &
     &                  (MONUMENTS(K,ISTA),K=1,5), &
     &                   ICMP(I), 'Comp', (VSC(I)+ADJT(I))*1000.0D0,'mm   ', &
     &                   ADJT(I)*1000.D0, 'mm   ', &
     &                   SIGT(I)*WRMS(3)*1000.D0, 'mm   '
                CALL ADDSTR_F ( LBUF(IPTR)(:PAGEWID) )
                CALL NL_MN()
 3500           FORMAT ( 5X, "  ", 4A2,1X,5A2,1X,A2,A4,F16.2," ", &
     &                   A5,' ',F9.2,' ',A5,' ',F9.2,' ',A5)
           ENDIF
        ENDDO
        IF ( MODE .EQ. 2 ) THEN
             CALL COMPMIN ( ADJT, NPARMC, ISTA, TYPEPR, STPT2, 'mm   ', &
     &                      LSITEC, VSC, 'Comp', RES2, &
     &                      LBUF_LEN, LBUF, IPTR, PAGEWID )
        ENDIF
      ENDDO
!
      RETURN
      END  !#!  POSMAP2  #!#
