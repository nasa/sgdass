      SUBROUTINE WRI_BSPPOS ( SPL, FIL_TMPL, SPLPOS_VERS, FIL_BSPPOS, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine WRI_BSPPOS  writes the file in BSPPOS format which         *
! *   describes displacements for a specific site represented as an      *
! *   expansion with B-spline basis. BSPPOS file provides the number of  *
! *   knots, the degree of the B-spline basis, apriori site position,    *
! *   time epochs of knots, estimates of B-spline coefficients, global   *
! *   site position, global site velocity and elements of the covariance *
! *   matrix between B-spline coefficients, global site position, global *
! *   site velocity.                                                     *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *            SPL ( RECORD    ) -- Object with station name, spline     *
! *                                 coefficients, epoch nodes etc.       *
! *                                 Contents of  that object describes   *
! *                                 site  position evolution modeled     *
! *                                 with a spline.                       * 
! *       FIL_TMPL ( CHARACTER ) -- Template file name.                  *
! *    SPLPOS_VERS ( CHARACTER ) -- The string with program name and     *
! *                                 version date.                        *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  FIL_BSPPOS ( CHARACTER ) -- Name of the output BSPPOS file. It is   *
! *                              built this way: tttt_ssss_c.bsp where   *
! *                              tttt -- FILTMPL file;                   *
! *                              ssss -- station name (capital letters)  *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *    IUER ( INTEGER*4, OPT ) -- Universal error handler.               *
! *                           Input: switch IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will be put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successful        *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *  ### 14-MAR-2005   WRI_BSPPOS  v1.0 (c)  L. Petrov  11-MAR-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'astro_constants.i'
      INCLUDE   'solve.i'
      INCLUDE   'vtd.i'
      INCLUDE   'bsp.i'
      CHARACTER  FIL_TMPL*(*), FIL_BSPPOS*128, SPLPOS_VERS*(*)
      TYPE       ( BSPSTA__TYPE ) :: SPL
      INTEGER*4  IUER
      INTEGER*4  MBUF
      PARAMETER  ( MBUF = 512 )
      CHARACTER  USER_NAME*128, USER_REALNAME*128, USER_E_ADDRESS*128
      CHARACTER  SYSNAME*128,   NODENAME*128,      HARDWARE*128 
      CHARACTER  FORMAT_DSC*128, STR*128, BUF(MBUF)*128
      REAL*8     LONG, PP, LAT_GCN, RAD, MU, LAT_GDT, HEI_ELL, SCL(2)
      INTEGER*4  IOS, NBUF, J1, J2, J3, J4, J5, J6, J7, IND_COV, LUN, IER
      CHARACTER  CMP(3)*1
      DATA       CMP  /  'X',  'Y',  'Z'  /
      CHARACTER, EXTERNAL :: GET_CDATE*19, JD_TO_DATE*23
      INTEGER*4, EXTERNAL :: GET_UNIT, I_LEN, ILEN
      REAL*8,    EXTERNAL :: ATAN_CS
      INTEGER*4  LOCS, I, J
      LOCS(I,J)=min(I,J) +(max(I,J)*(max(I,J)-1))/2
!
      FIL_BSPPOS = FIL_TMPL(1:I_LEN(FIL_TMPL))//'_'// &
     &         SPL%STATION(1:I_LEN(SPL%STATION))//'.bsp'
!
! --- GEt information about the user and about the system
!
      CALL GETINFO_USER   ( USER_NAME, USER_REALNAME, USER_E_ADDRESS )
      CALL GETINFO_SYSTEM ( SYSNAME, NODENAME, HARDWARE )
!
! --- Open the output file
!
      LUN = GET_UNIT()
      OPEN ( UNIT=LUN, FILE=FIL_BSPPOS, STATUS='UNKNOWN', IOSTAT=IOS ) 
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 1651, IUER, 'WRI_BSPPOS', 'Error in an attempt '// &
     &         'to open the output file '//FIL_BSPPOS )
           RETURN 
      END IF
!
! --- Write the header label
!
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) BSP__LABEL 
      IF ( IOS .NE. 0 ) THEN
           CALL ERR_LOG ( 1652, IUER, 'WRI_BSPPOS', 'Error in an attempt '// &
     &         'to write in the output file '//FIL_BSPPOS )
           RETURN 
      END IF
!
! --- Write the header comment: who, where, how
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Created by '// &
     &                               SPLPOS_VERS(1:I_LEN(SPLPOS_VERS))
      WRITE ( UNIT=LUN, FMT='(A)' ) '#       run by '// &
     &                               USER_REALNAME(1:I_LEN(USER_REALNAME))// &
     &        ' ( '//USER_E_ADDRESS(1:I_LEN(USER_E_ADDRESS))//' )'
      WRITE ( UNIT=LUN, FMT='(A)' ) '#           on '// &
     &               NODENAME(1:I_LEN(NODENAME))//' at '// &
     &               GET_CDATE()//' local time'
!
! --- Build the name of the format description file
!
      CALL GETENV ( SOLVE_HELP_DIR, STR )
      FORMAT_DSC = SOLVE_HELP_DIR//'/bsppos_format.txt'
!
! --- Write section delimieter
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '#============================ Beginning of format description: ================='
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Read format description into the buffer buf
!
      CALL ERR_PASS ( IUER, IER )
      CALL RD_TEXT  ( FORMAT_DSC, MBUF, BUF, NBUF, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 1653, IUER, 'WRI_BSPPOS', 'Error in an attempt '// &
     &         'to read BSPPOS format description from file '//FORMAT_DSC )
           RETURN 
      END IF
!
! --- Write contents of the description buffer into comment section
!
      DO 410 J1=1,NBUF
         WRITE ( UNIT=LUN, FMT='(A)' ) '# '//BUF(J1)(1:I_LEN(BUF(J1)))
 410  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '#============================ End of format description: ======================='
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Write down the total number of knots
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Number of knots'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A,1X,I4)' ) 'K', SPL%L_NOD
!
! --- Write down the degree
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Degree of B-spline'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A,1X,I4)' ) 'D', SPL%DEGREE
!
      LONG = ATAN_CS ( SPL%APR_COO(1), SPL%APR_COO(2) )
      IF ( LONG < 0.0D0 ) LONG = PI2 + LONG
      PP  = DSQRT ( SPL%APR_COO(1)**2 + SPL%APR_COO(2)**2 )
      LAT_GCN = DATAN( SPL%APR_COO(3)/PP )
!
! --- Computation station longitude, latitude and ellipsoidal height
!
      RAD = DSQRT ( SPL%APR_COO(1)**2 + SPL%APR_COO(2)**2 + SPL%APR_COO(3)**2 )
      MU  = DATAN ( SPL%APR_COO(3)/PP * &
     &            ( (1.D0 - VTD__FE) + VTD__EXC_SQ*VTD__REA/RAD  ) )
!
      LAT_GDT = DATAN( ( (1.D0 - VTD__FE)*SPL%APR_COO(3) + &
     &                 VTD__EXC_SQ*VTD__REA*DSIN(MU)**3 ) / &
     &                ( (1.D0 - VTD__FE)* &
     &                ( PP  - VTD__EXC_SQ*VTD__REA*DCOS(MU)**3 )) )
!
      HEI_ELL = PP*DCOS(LAT_GDT) + SPL%APR_COO(3)*DSIN(LAT_GDT) - &
     &          VTD__REA* DSQRT( 1.D0 - VTD__EXC_SQ*DSIN(LAT_GDT)**2 )
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '#  Site ID    X-coord.      Y-coord.      Z-coord.     phi-geoc. longit.  height '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Write down S-record
!
      WRITE ( LUN, 110 ) SPL%STATION, SPL%APR_COO, LAT_GCN*180.0/PI__NUM, &
     &                   LONG*180.0/PI__NUM, HEI_ELL
 110  FORMAT ( 'S  ',A, 1X, 3(1X,F13.4), 2X,F8.4, 1X,F8.4, 1X,F6.1 )
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Knot  Epoch in TAI'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Write down E-records
!
      DO 420 J2=1-SPL%DEGREE,SPL%L_NOD
         WRITE ( LUN, 120 ) J2, &
     &           JD_TO_DATE ( SPL%TIM(J2)/86400.0D0 + J2000__JD, -3 )
 120     FORMAT ( 'E ', I4, 2X, A23 )
 420  CONTINUE 
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Knot    X-coeff.    Y-coeff.    Z-coeff.'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Write down coefficients
!
      DO 430 J3=1-SPL%DEGREE,SPL%L_NOD-1
         WRITE ( LUN, 130 ) J3, SPL%POS(1,J3), SPL%POS(2,J3), SPL%POS(3,J3)
 130     FORMAT ( 'B ', I4, 3(2X, F10.6) )
 430  CONTINUE 
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Glpos.  X-coord.    Y-coord     Z-coord.  Reference epoch'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Write down global site position estimates
!
      WRITE ( LUN, 140 ) SPL%ADJ_COO(1), &
     &                   SPL%ADJ_COO(2), &
     &                   SPL%ADJ_COO(3), &
     &                   JD_TO_DATE ( SPL%TIM_COO/86400.0D0 + J2000__JD, -3 )
 140  FORMAT ( 'P', 5X, 3(2X, F10.6), 2X,A23 )
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Glvel.  X-coord.    Y-coord     Z-coord. ( in mm/yr )'

      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Write down global site position estimates
!
      WRITE ( LUN, 150 ) SPL%ADJ_VEL(1)*1.D3*JYEAR__DAYS*86400.0D0, &
     &                   SPL%ADJ_VEL(2)*1.D3*JYEAR__DAYS*86400.0D0, &
     &                   SPL%ADJ_VEL(3)*1.D3*JYEAR__DAYS*86400.0D0 
 150  FORMAT ( 'V', 5X, 3(2X, F10.6) )
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)' ) '# Ind1 C1 Ind2 C2     Covariance'
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
!
! --- Write down elements of covarinance matrix
!
      DO 440 J4=1-SPL%DEGREE,SPL%L_NOD+1
         IF ( J4 .EQ. SPL%L_NOD+1 ) THEN
              SCL(1) = 1.D3*JYEAR__DAYS*86400.0D0 
            ELSE 
              SCL(1) = 1.0D0
         END IF
         DO 450 J5=1,3
            DO 460 J6=J4,SPL%L_NOD+1
!
! ------------ Set scaling factor ( m/s -- > mm/yr for velocities, 1 otherwise )
!
               IF ( J6 .EQ. SPL%L_NOD+1 ) THEN
                    SCL(2) = 1.D3*JYEAR__DAYS*86400.0D0 
                 ELSE
                    SCL(2) = 1.0D0
               END IF
               DO 470 J7=J5,3
                  IND_COV = LOCS( (J4 + M__SPD - 1)*3 + J5, &
     &                            (J6 + M__SPD - 1)*3 + J7 )
                  IF ( DABS(SPL%COV(IND_COV))*SCL(1)*SCL(2) .GT. 5.D-13 ) THEN
                       WRITE ( LUN, 160 ) J4, &
     &                                    CMP(J5), J6, &
     &                                    CMP(J7), SPL%COV(IND_COV)*SCL(1)*SCL(2)
 160                   FORMAT ( 'C ',I4, 1X,A, 2X,I4, 1X,A, 2X, F18.12 )
                  END IF
 470           CONTINUE 
 460        CONTINUE 
 450     CONTINUE 
 440  CONTINUE 
!
! --- Write doens the trailing record
!
      WRITE ( UNIT=LUN, FMT='(A)' ) '# '
      WRITE ( UNIT=LUN, FMT='(A)', IOSTAT=IOS ) BSP__LABEL 
!
! --- That's it!
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  WRI_BSPPOS 
