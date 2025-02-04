      SUBROUTINE SOTID_PRE ( L_STA, C_STA, R_CFS, TIDCNF, STATID, IUER )
! ************************************************************************
! *                                                                      *
! *     Subroutine  SOTID_PRE  computes time-independent arguments       *
! *   used for computations of site displacements caused by solid Earth  *
! *   tides for the set of stations. These arguments depends on          *
! *   crust-fixed station positions. Routine SOTDID_PRE puts these       *
! *   time-independent arguments into the STATID arrays.                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *  L_STA ( INTEGER*4 ) -- Total number of stations.                    *
! *  C_STA ( CHARACTER ) -- Array of station names. Dimension: L_STA.    *
! *  R_CFS ( REAL*8    ) -- Array of crust-fixed station positions.      *
! *                         Dimension: 3,L_STA. Units: meters.           *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! * TIDCNF ( RECORD    ) -- object which holds configuration parameters  *
! *                         of SOTID.                                    *
! * STATID ( RECORD    ) -- object with information about station.       *
! *                         SOTID_PRE fills some fields of this object.  *
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
! *  ### 23-AUG-2001   SOTID_PRE   v1.3  (c) L. Petrov  14-JUL-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE    'sotid_type.i'
      INCLUDE    'sotid_data.i'
      INTEGER*4  L_STA, IUER
      CHARACTER  C_STA(L_STA)*(SOTID__NAM_LEN)
      REAL*8     R_CFS(3,L_STA)
      TYPE ( STATID__STRU ) ::  STATID(L_STA)
      TYPE ( TIDCNF__STRU ) ::  TIDCNF
      REAL*8      GE
      PARAMETER ( GE = SOTID__GEO_CG / SOTID__EARTH_RAD**2 ) ! Nomimnal gravity
!                           ! acceleration at the Earth's equator
!
      REAL*8     PHI_GCN, RD, REN__CFS(3,3), LONGITUDE, PP, CF, SF, CL, SL, &
     &           CML, SML
      REAL*8     P(3,0:3), PN(3,0:3), DPN(3,0:3)
      REAL*8     NC2(0:2), NC3(0:3), NC20, NC21, NC22, NC30, NC31, NC32, NC33
      CHARACTER  STR*32, STR2*80
      INTEGER*4  NN, J1, J2, J3
      INTEGER*4, EXTERNAL :: ILEN
!
! --- Associated Legandre polynomial normalization coefficients.
! --- Normalization means that the integral of the square of the associated
! --- Legandre polynomial over the unit sphere is 1
!
!
      NC20 = DSQRT (  5.D0/( 4.D0*SOTID__PI) )
      NC21 = DSQRT ( 15.D0/(32.D0*SOTID__PI) )
      NC22 = DSQRT ( 15.D0/(32.D0*SOTID__PI) )
      NC30 = DSQRT (  7.D0/( 4.D0*SOTID__PI) )
      NC31 = DSQRT ( 21.D0/(16.D0*SOTID__PI) )
      NC32 = DSQRT (105.D0/(32.D0*SOTID__PI) )
      NC33 = DSQRT ( 35.D0/(64.D0*SOTID__PI) )
      NC2(0) = NC20; NC2(1) = NC21; NC2(2) = NC22
      NC3(0) = NC30; NC3(1) = NC31; NC3(2) = NC32; NC3(3) = NC33
!
      IF ( L_STA .LT. 0 ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(I12)' ) L_STA
           CALL CHASHL ( STR )
           CALL ERR_LOG ( 5721, IUER, 'SOTID_PRE', 'Wrong value of '// &
     &         'the argument L_STA -- it is too small. L_STA='//STR )
           RETURN
      END IF
!
      IF ( L_STA .GT. SOTID__MAX_STA ) THEN
           CALL CLRCH ( STR )
           WRITE ( UNIT=STR, FMT='(I12)' ) L_STA
           CALL CHASHL ( STR )
           CALL ERR_LOG ( 5722, IUER, 'SOTID_PRE', 'Wrong value of '// &
     &         'the argument L_STA -- it is too large. L_STA='//STR )
           RETURN
      END IF
!
      DO 410 J1=1,L_STA
         IF ( (R_CFS(1,J1)**2 + R_CFS(2,J1)**2 + R_CFS(3,J1)**2 ) < &
     &        (0.99D0*SOTID__EARTH_RAD)**2 ) THEN
!
! ----------- No computation if the stations is in geocenter or in orbit
!
              STATID(J1)%PHI_GCN = 0.0D0
              STATID(J1)%LONGITUDE = 0.0D0
              STATID(J1)%REN__CFS(3,3) = 0.0D0
              STATID(J1)%XRC2(5,3,0:2) = 0.0D0
              STATID(J1)%XRS2(5,3,0:2) = 0.0D0
              STATID(J1)%XIC2(2,3,0:2) = 0.0D0
              STATID(J1)%XIS2(2,3,0:2) = 0.0D0
              STATID(J1)%XRC3(3,0:3) = 0.0D0
              STATID(J1)%XRS3(3,0:3) = 0.0D0
              GOTO 410
         END IF
!
! ------ Calculation longitude LONGITUDE and geocentric lattitude PHI_GCN of
! ------ the J1-th station
!
         IF ( DABS( R_CFS(1,J1) ) .GT. 1.D-8 ) THEN
              LONGITUDE = DATAN ( R_CFS(2,J1)/R_CFS(1,J1) )
           ELSE
              LONGITUDE = SOTID__PI/2.D0
         END IF
!
         IF ( R_CFS(1,J1) .LT. 0.0D0 ) LONGITUDE = LONGITUDE + SOTID__PI
         IF ( LONGITUDE   .LT. 0.0D0 ) LONGITUDE = LONGITUDE + SOTID__PI2 
!
         PP  = DSQRT ( R_CFS(1,J1)**2 + R_CFS(2,J1)**2 )
         IF ( DABS(PP) .LT. 1.D-8 ) PP=1.D-8
         RD = DSQRT ( R_CFS(3,J1)**2 + PP**2    )
         IF ( DABS (RD - SOTID__EARTH_RAD) .GT. SOTID__HEIGHT_TOL ) THEN
              CALL CLRCH ( STR )
              WRITE ( UNIT=STR, FMT='(I12)' ) J1
              CALL CHASHL ( STR  )
!
              CALL CLRCH  ( STR2 )
              WRITE ( UNIT=STR2, FMT='(3(F15.3,2X))' ) (R_CFS(NN,J1), NN=1,3)
              CALL ERR_LOG ( 5723, IUER, 'SOTID_PRE', 'Wrong '// &
     &            'positions of the '//STR(1:ILEN(STR))//'-th '// &
     &            'station '//C_STA(J1)//' -- '//STR2(1:ILEN(STR2))// &
     &            ' -- they are not on the surface of our planet' )
              RETURN
         END IF
!
         PHI_GCN = DATAN( R_CFS(3,J1)/PP )
!
! ------ Calculation matrix of transformation from REN (local topocentric,
! ------ (Radial,East,North) ) to the CFS (crust-fixed (X,Y,Z) ) system
!
         CF = DCOS(PHI_GCN)
         SF = DSIN(PHI_GCN)
         CL = DCOS(LONGITUDE)
         SL = DSIN(LONGITUDE)
!
         REN__CFS(1,1) = CF*CL
         REN__CFS(2,1) = CF*SL
         REN__CFS(3,1) = SF
!
         REN__CFS(1,2) = -SL
         REN__CFS(2,2) =  CL
         REN__CFS(3,2) =  0.D0
!
         REN__CFS(1,3) = -SF*CL
         REN__CFS(2,3) = -SF*SL
         REN__CFS(3,3) =  CF
!
! ------ Calculation un-normalozed associated Legandre polynomials
!
         P(1,0) =   DSIN(PHI_GCN)
         P(1,1) =   DCOS(PHI_GCN)
         P(1,2) =   0.0D0
         P(2,0) = ( 1.5D0* DSIN(PHI_GCN)**2 - 0.5D0 )
         P(2,1) =   2.0D0* DSIN(PHI_GCN)*DCOS(PHI_GCN)
         P(2,2) =          DCOS(PHI_GCN)**2
         P(3,0) = ( 2.5D0* DSIN(PHI_GCN)**3 - 1.5D0*DSIN(PHI_GCN) )
         P(3,1) = ( 2.5D0* DSIN(PHI_GCN)**2 - 0.5D0 )*DCOS(PHI_GCN)
         P(3,2) =          DSIN(PHI_GCN)             *DCOS(PHI_GCN)**2
         P(3,3) =                                     DCOS(PHI_GCN)**3
!
! ------ Calculation associated Legandre function normalized over the
! ------ unit sphere
!
         PN(1,0) = P(1,0)
         PN(1,1) = P(1,1)
         PN(1,2) = P(1,2)
!
         PN(2,0) = NC2(0) * P(2,0)
         PN(2,1) = NC2(1) * P(2,1)
         PN(2,2) = NC2(2) * P(2,2)
!
         PN(3,0) = NC3(0) * P(3,0)
         PN(3,1) = NC3(1) * P(3,1)
         PN(3,2) = NC3(2) * P(3,2)
         PN(3,3) = NC3(3) * P(3,3)
!
! ------ Calculation of the derivatives of the associated Legandre function
! ------ normalized over the unit sphere over geocentric lattitude
!
         DPN(1,0) =  COS(PHI_GCN)
         DPN(1,1) = -SIN(PHI_GCN)
         DPN(1,2) =  0.0D0
         DPN(1,3) =  0.0D0
!
         DPN(2,0) =  NC2(0) * 3.D0 *   DSIN(PHI_GCN) * DCOS(PHI_GCN)
         DPN(2,1) =  NC2(1) * 2.D0 * ( DCOS(PHI_GCN)**2 - DSIN(PHI_GCN)**2 )
         DPN(2,2) = -NC2(2) * 2.D0 *   DCOS(PHI_GCN) * DSIN(PHI_GCN)
         DPN(2,3) =  0.0D0
!
         DPN(3,0) =  NC3(0) * (7.5D0*DSIN(PHI_GCN) - 1.5D0    ) * DCOS(PHI_GCN)
         DPN(3,1) =  NC3(1) * (5.5D0 - 7.5D0*DSIN(PHI_GCN)**2 ) * DSIN(PHI_GCN)
         DPN(3,2) =  NC3(2) * (1.0D0 - 3.0*DSIN(PHI_GCN)**2   ) * DCOS(PHI_GCN)
         DPN(3,3) = -NC3(3) * 3.0D0*DSIN(PHI_GCN) * DCOS(PHI_GCN)**2
!
! ------ Calculation of the lattitude terms of the second order in expression
! ------ for tidal displacements
!
         DO 420 J2=0,2
            CML = DCOS( J2*LONGITUDE )
            SML = DSIN( J2*LONGITUDE )
!
! --------- In-phase terms
!
! --------- Radial components
!
            STATID(J1)%XRC2(1,1,J2)= PN(2,J2)/GE        *CML
            STATID(J1)%XRC2(2,1,J2)= P(2,0)*PN(2,J2)/GE *CML
            STATID(J1)%XRC2(3,1,J2)= 0.0D0
            STATID(J1)%XRC2(4,1,J2)= 1.0D0/GE           *CML
            STATID(J1)%XRC2(5,1,J2)= 1.0D0/GE           *CML
!
            STATID(J1)%XRS2(1,1,J2)= PN(2,J2)/GE        *SML
            STATID(J1)%XRS2(2,1,J2)= P(2,0)*PN(2,J2)/GE *SML
            STATID(J1)%XRS2(3,1,J2)= 0.0D0
            STATID(J1)%XRS2(4,1,J2)= 1.0D0/GE           *SML
            STATID(J1)%XRS2(5,1,J2)= 1.0D0/GE           *SML
!
! --------- East components
!
            STATID(J1)%XRC2(1,2,J2)=-J2/DCOS(PHI_GCN) * PN(2,J2)/GE         *CML
            STATID(J1)%XRC2(2,2,J2)=-J2/DCOS(PHI_GCN) * P(2,0) * PN(2,J2)/GE*CML
            STATID(J1)%XRC2(3,2,J2)= P(1,0)* DPN(2,J2)/GE                   *CML
            STATID(J1)%XRC2(4,2,J2)= DPN(1,J2)/GE                           *CML
            STATID(J1)%XRC2(5,2,J2)= 1.0D0/GE                               *CML
!
            STATID(J1)%XRS2(1,2,J2)=-J2/DCOS(PHI_GCN) * PN(2,J2)/GE         *SML
            STATID(J1)%XRS2(2,2,J2)=-J2/DCOS(PHI_GCN) * P(2,0) * PN(2,J2)/GE*SML
            STATID(J1)%XRS2(3,2,J2)= P(1,0)* DPN(2,J2)/GE                   *SML
            STATID(J1)%XRS2(4,2,J2)= DPN(1,J2)/GE                           *SML
            STATID(J1)%XRS2(5,2,J2)= 1.0D0/GE                               *SML
!
! --------- North components
!
            STATID(J1)%XRC2(1,3,J2)= DPN(2,J2)/GE                           *CML
            STATID(J1)%XRC2(2,3,J2)= P(2,0) * DPN(2,0)/GE                   *CML
            STATID(J1)%XRC2(3,3,J2)=-J2/DCOS(PHI_GCN) * P(1,0) *PN(2,J2)/GE *CML
            STATID(J1)%XRC2(4,3,J2)=-J2/DCOS(PHI_GCN) * PN(1,J2)/GE         *CML
            STATID(J1)%XRC2(5,3,J2)= 1.0D0/GE                               *CML
!
            STATID(J1)%XRS2(1,3,J2)= DPN(2,J2)/GE                           *SML
            STATID(J1)%XRS2(2,3,J2)= P(2,0) * DPN(2,0)/GE                   *SML
            STATID(J1)%XRS2(3,3,J2)=-J2/DCOS(PHI_GCN) * P(1,0) *PN(2,J2)/GE *SML
            STATID(J1)%XRS2(4,3,J2)=-J2/DCOS(PHI_GCN) * PN(1,J2)/GE         *SML
            STATID(J1)%XRS2(5,3,J2)= 1.0D0/GE                               *SML
!
! --------- Out-of-phase terms
!
! --------- Radial components
!
            STATID(J1)%XIC2(1,1,J2)=  PN(2,J2)/GE                    *CML
            STATID(J1)%XIC2(2,1,J2)=  1.D0/GE                        *CML
!
            STATID(J1)%XIS2(1,1,J2)=  PN(2,J2)/GE                    *SML
            STATID(J1)%XIS2(2,1,J2)=  1.D0/GE                        *SML
!
! --------- East components
!
            STATID(J1)%XIC2(1,2,J2)= -J2/DCOS(PHI_GCN) * PN(2,J2)/GE *CML
            STATID(J1)%XIC2(2,2,J2)=  1.D0/GE                        *CML
!
            STATID(J1)%XIS2(1,2,J2)= -J2/DCOS(PHI_GCN) * PN(2,J2)/GE *SML
            STATID(J1)%XIS2(2,2,J2)=  1.D0/GE                        *SML
!
! --------- North components
!
            STATID(J1)%XIC2(1,3,J2)=  DPN(2,J2)/GE                   *CML
            STATID(J1)%XIC2(2,3,J2)=  1.D0/GE                        *CML
!
            STATID(J1)%XIS2(1,3,J2)=  DPN(2,J2)/GE                   *SML
            STATID(J1)%XIS2(2,3,J2)=  1.D0/GE                        *SML
 420     CONTINUE
!
! ------ Calculation of the lattitude terms of the third order in expression
! ------ for tidal displacements
!
         DO 430 J3=0,3
            CML = DCOS( J3*LONGITUDE )
            SML = DSIN( J3*LONGITUDE )
!
            STATID(J1)%XRC3(1,J3)= PN(3,J3)/GE *CML
            STATID(J1)%XRS3(1,J3)= PN(3,J3)/GE *SML
!
            STATID(J1)%XRC3(2,J3)= -J3/DCOS(PHI_GCN) * PN(3,J3)/GE *CML
            STATID(J1)%XRS3(2,J3)= -J3/DCOS(PHI_GCN) * PN(3,J3)/GE *SML
!
            STATID(J1)%XRC3(3,J3)= DPN(3,J3)/GE *CML
            STATID(J1)%XRS3(3,J3)= DPN(3,J3)/GE *SML
 430     CONTINUE
!
! ------ Storing some calculated quantites in data structures
!
         STATID(J1)%PHI_GCN   = PHI_GCN
         STATID(J1)%LONGITUDE = LONGITUDE
         CALL COPY_V ( 9, REN__CFS, STATID(J1)%REN__CFS )
         STATID(J1)%NAME = C_STA(J1)
 410  CONTINUE
!
      TIDCNF%N_STA = L_STA
      CALL ERR_LOG ( 0, IUER, ' ', ' ' )
      RETURN
      END  SUBROUTINE  SOTID_PRE  !#!#
