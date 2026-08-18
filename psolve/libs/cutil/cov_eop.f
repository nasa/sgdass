      SUBROUTINE COV_EOP ( THISJD, FSTJD, INC, NUM, PI, CVINF, WHO, EOPCONS )
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
!
! 1.  COV_EOP PROGRAM SPECIFICATION
!
! 1.1 Get the constraint matrix
!
! 1.2 REFERENCES:
!
! 2.  COV_EOP INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4  NUM, INC
      REAL*8     FSTJD, THISJD, PI, EOPCONS(3)
      CHARACTER  WHO(3)*(*)
!
! EOPCONS - Earth orientation parameter constraint
! FSTJD - First J.D.
! INC - Interval between epochs
! NUM - Number of epochs
! PI - Pi in 11-digit precision
! THISJD - Epoch for which shorter position and parameter names lists
!          should be generated
! WHO - List of earth orientation offset parameter names for this epoch
!
! Global variable MDORCTL control its work:
! MDORCTL = 0 -- apriori covariance matrix is a RSS sum of the covarinace
!                matrix of the mod-file **and** external EOPCONS constraints
! MDORCTL = 1 -- apriori covariance matrix is the covarinace matrix of
!                the mod-file
! MDORCTL = 2 -- apriori covariance matrix is a diagonal mstrix with squares of
!                EOPCONS on the diagonal.
!
! 2.3 OUTPUT Variables:
!
      REAL*8 CVINF(6)
!
! CVINF - THe solve format matrix
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'glbcm.i'
      INCLUDE 'glbc4.i'
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES: /adjst/eop_share
!       CALLED SUBROUTINES: dppfa,dppin
!
! 3.  LOCAL VARIABLES
!
      REAL*8 CVDUM(6), COV1(6), COV2(6), RSSX, RSSY, RSSU, SIGX, SIGY, SIGU
      REAL*8 XPSG, YPSG, UTSG, XYCR, XUCR, YUCR, DAY1, DAY2, DIFJD, T, DT
      REAL*8 RAD_PER_MASEC, SEC_PER_MASEC
      REAL*8 COV_MIN
      PARAMETER  ( COV_MIN = 0.01 ) ! mas -- the srongest allowed constraint
      INTEGER*4 IUER
      CHARACTER ERRSTR*150
      INTEGER*2 I, CNSTR_REC, INT2_ARG
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!   AEE   910515 Enhanced error messages written to the error file.
!   pet   2001.05.29  Improved comments. Changed the logic of the check whether
!                     diagnal elements. Changed the value of this limit.
!
! 5.  COV_EOP PROGRAM STRUCTURE
!
!     Units conversion constants
!
      RAD_PER_MASEC = PI/(180.D0*3600.D0*1000.D0)
      SEC_PER_MASEC = 1.D0/15000.D0
!
! --- Within an interval (or on an endpoint), get the left endpoint
!
      DIFJD = THISJD - FSTJD
      CNSTR_REC = INT(DIFJD/INC) + 1 ! get left endpoint
      IF ( CNSTR_REC .LT. 1  .OR.  CNSTR_REC .GT. NUM ) THEN
           WRITE ( 6, * ) ' THISJD=',THISJD,' FSTJD=',FSTJD
           WRITE ( 6, * ) ' DIFJD =', DIFJD, ' INC=',INC 
           WRITE ( 6, * ) ' CNSTR_REC=',CNSTR_REC, ' NUM=',NUM
           WRITE ( ERRSTR, * ) 'COV_EOP: Left point - eop epoch not '// &
     &                         'within constraint table', CNSTR_REC
           CALL FERR ( INT2(151), ERRSTR, INT2(0), INT2(0) )
      ENDIF
      DAY1 = FSTJD + (CNSTR_REC-1)*INC
      XPSG = WBXSIG(CNSTR_REC)
      YPSG = WBYSIG(CNSTR_REC)
      UTSG = UT1SIG(CNSTR_REC)
      XYCR = WXYCOR(CNSTR_REC)
      XUCR = WXUCOR(CNSTR_REC)
      YUCR = WYUCOR(CNSTR_REC)
      IF ( MDORCTL .EQ. 2 ) THEN
           XPSG = 0.D0
           YPSG = 0.D0
           UTSG = 0.D0
           XYCR = 0.D0
           XUCR = 0.D0
           YUCR = 0.D0
      ENDIF
!
! --- how far into the interval is the point of interpolation?
!
      DT = THISJD - DAY1
!
! --- first convert sigmas
!
      SIGX=XPSG*(100.0D0)*RAD_PER_MASEC
      SIGY=YPSG*(100.0D0)*RAD_PER_MASEC
      SIGU=UTSG/(1.0D6)
!
! --- Get values to rss to diagonal
!
      RSSX=EOPCONS(1)*RAD_PER_MASEC
      RSSY=EOPCONS(2)*RAD_PER_MASEC
      RSSU=EOPCONS(3)/1.0D6
      IF ( MDORCTL .EQ. 1 ) THEN
           RSSX = 0.D0
           RSSY = 0.D0
           RSSU = 0.D0
      ENDIF
!
! --- Form the covariance matrix for the left endpoint in all of its glory
!
      IF ( EOP_FACTOR .NE. 0 ) THEN
           SIGX = SIGX*EOP_FACTOR
           SIGY = SIGY*EOP_FACTOR
           SIGU = SIGU*EOP_FACTOR
      ENDIF
!
      COV1(1) = (SIGX*SIGX)      + RSSX*RSSX
      COV1(2) = XYCR*(SIGX*SIGY)
      COV1(3) = (SIGY*SIGY)      + RSSY*RSSY
      COV1(4) = XUCR*(SIGX*SIGU)
      COV1(5) = YUCR*(SIGY*SIGU)
      COV1(6) = (SIGU*SIGU)      + RSSU*RSSU
!
! --- Now obtain the right endpoint
!
      CNSTR_REC=CNSTR_REC+1 ! Get right endpoint
      IF ( CNSTR_REC .LT. 1.0D0 .OR.  CNSTR_REC .GT. NUM ) THEN
           WRITE ( ERRSTR, * ) 'COV_EOP: Right point - eop epoch not '// &
     &                         'within constraint table', cnstr_rec
           CALL FERR ( INT2(152), ERRSTR, INT2(0), INT2(0) )
      ENDIF
      DAY2 = FSTJD+(CNSTR_REC-1)*INC
      XPSG = WBXSIG(CNSTR_REC)
      YPSG = WBYSIG(CNSTR_REC)
      UTSG = UT1SIG(CNSTR_REC)
      XYCR = WXYCOR(CNSTR_REC)
      XUCR = WXUCOR(CNSTR_REC)
      YUCR = WYUCOR(CNSTR_REC)
      IF ( MDORCTL .EQ. 2 ) THEN
           XPSG = 0.D0
           YPSG = 0.D0
           UTSG = 0.D0
           XYCR = 0.D0
           XUCR = 0.D0
           YUCR = 0.D0
      ENDIF
!
! --- What is the length(in days) of this interval?
!
      T = DAY2-DAY1
!
! --- First convert sigmas
!
      SIGX=XPSG*(100.0D0)*RAD_PER_MASEC
      SIGY=YPSG*(100.0D0)*RAD_PER_MASEC
      SIGU=UTSG/(1.0D6)
!
! --- Form the covariance matrix for the right endpoint in all of its glory
!
      IF ( EOP_FACTOR .NE. 0 ) THEN
           SIGX = SIGX*EOP_FACTOR
           SIGY = SIGY*EOP_FACTOR
           SIGU = SIGU*EOP_FACTOR
      ENDIF
!
      COV2(1) = (SIGX*SIGX)      + RSSX*RSSX
      COV2(2) = XYCR*(SIGX*SIGY)
      COV2(3) = (SIGY*SIGY)      + RSSY*RSSY
      COV2(4) = XUCR*(SIGX*SIGU)
      COV2(5) = YUCR*(SIGY*SIGU)
      COV2(6) = (SIGU*SIGU)      + RSSU*RSSU
!
! --- Now perform straight-line interpolation between the two days by
!
      CVDUM(1) =  COV1(1)*(1.0D0-DT/T) + COV2(1)*(DT/T)
      CVDUM(2) =  COV1(2)*(1.0D0-DT/T) + COV2(2)*(DT/T)
      CVDUM(3) =  COV1(3)*(1.0D0-DT/T) + COV2(3)*(DT/T)
      CVDUM(4) =  COV1(4)*(1.0D0-DT/T) + COV2(4)*(DT/T)
      CVDUM(5) =  COV1(5)*(1.0D0-DT/T) + COV2(5)*(DT/T)
      CVDUM(6) =  COV1(6)*(1.0D0-DT/T) + COV2(6)*(DT/T)
!
! --- If diagonal at leat one element of the covariant matrix are too small
! --- then we make the matrix diagonal and repoace these elements with
! --- their lowest limits
!
      IF ( CVDUM(1) .LT. (COV_MIN*RAD_PER_MASEC)**2 ) THEN
           CVDUM(1) = (COV_MIN*RAD_PER_MASEC)**2
           CVDUM(2) = 0.0D0
           CVDUM(4) = 0.0D0
           CVDUM(5) = 0.0D0
      END IF
      IF ( CVDUM(3) .LT. (COV_MIN*RAD_PER_MASEC)**2 ) THEN
           CVDUM(2) = 0.0D0
           CVDUM(3) = (COV_MIN*RAD_PER_MASEC)**2
           CVDUM(4) = 0.0D0
           CVDUM(5) = 0.0D0
      END IF
      IF ( CVDUM(6) .LT. (COV_MIN*SEC_PER_MASEC)**2 ) THEN
           CVDUM(2) = 0.0D0
           CVDUM(4) = 0.0D0
           CVDUM(5) = 0.0D0
           CVDUM(6) = (COV_MIN*SEC_PER_MASEC)**2
      END IF
!
! --- Scale for inverting
!
      SIGX = DSQRT ( CVDUM(1) )
      SIGY = DSQRT ( CVDUM(3) )
      SIGU = DSQRT ( CVDUM(6) )
!
      CVDUM(1)=1.D0
      CVDUM(2)=CVDUM(2)/(SIGX*SIGY)
      CVDUM(3)=1.D0
      CVDUM(4)=CVDUM(4)/(SIGX*SIGU)
      CVDUM(5)=CVDUM(5)/(SIGY*SIGU)
      CVDUM(6)=1.D0
!
! --- Look at who is not present, zero out rows/cols accordingly
!
      DO I=1,6
         CVINF(I)=0.D0
      ENDDO
!
      IF ( INDEX ( WHO(1), 'X WOBBLE' ) .NE. 0 ) THEN
           CVINF(1)=CVDUM(1)
           IF ( INDEX(WHO(2), 'Y WOBBLE' ) .NE. 0 ) THEN
                CVINF(2)=CVDUM(2)
                CVINF(3)=CVDUM(3)
                IF ( INDEX ( WHO(3), 'UT1' ) .NE. 0 ) THEN
                     CVINF(4)=CVDUM(4)
                     CVINF(5)=CVDUM(5)
                     CVINF(6)=CVDUM(6)
                ENDIF
             ELSE IF ( INDEX ( WHO(2), 'UT1' ) .NE. 0 ) THEN ! y missing
                CVINF(4)=CVDUM(4)
                CVINF(5)=CVDUM(5)
                CVINF(6)=CVDUM(6)
          ENDIF
        ELSE IF ( INDEX ( WHO(1), 'Y WOBBLE' ) .NE. 0 ) THEN
          CVINF(3)=CVDUM(3)
          IF ( INDEX(WHO(2), 'UT1' ) .NE. 0 ) THEN
               CVINF(5)=CVDUM(5)
               CVINF(6)=CVDUM(6)
          ENDIF
        ELSE IF(INDEX(WHO(1),'UT1').NE.0) THEN
          CVINF(6)=CVDUM(6)
      ENDIF
!
! --- Inversion the mastrix CVINF
!
      IUER = -1
      CALL INVS ( 3, CVINF, RCOND, IUER )
      IF ( IUER .NE. 0 ) THEN
           ERRSTR = 'COV_EOP: Trap of internal control -- '// &
     &              'failure to invert covarinace matrix'
           CALL FERR ( INT2(153), ERRSTR, INT2(0), INT2(0) )
      END IF
!
      CVINF(1) = CVINF(1)/(SIGX*SIGX) ! x x cov
      CVINF(2) = CVINF(2)/(SIGX*SIGY) ! x y cov
      CVINF(3) = CVINF(3)/(SIGY*SIGY) ! y y cov
      CVINF(4) = CVINF(4)/(SIGX*SIGU) ! x u cov
      CVINF(5) = CVINF(5)/(SIGY*SIGU) ! y u cov
      CVINF(6) = CVINF(6)/(SIGU*SIGU) ! u u cov
!
      RETURN
      END  !#!  COV_EOP  #!#
