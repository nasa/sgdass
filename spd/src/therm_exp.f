      SUBROUTINE THERM_EXP ( SPD, ANTI, MALO, IVRB, FILOUT, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine THERM_EXP
! *                                                                      *
! *  ### 21-NOV-2013  THERM_EXP   v1.0 (c)  L. Petrov  21-NOV-2013 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'spd.i'
      INCLUDE   'spd_local.i'
      INCLUDE   'malo.i'
      INCLUDE   'anti.i'
      TYPE     ( SPD_3D__TYPE ) :: SPD
      TYPE     ( ANTENNA_DATA__TYPE ) :: ANTI
      TYPE     ( MALO__TYPE ) :: MALO
      CHARACTER  FILOUT*(*)
      INTEGER*4  IVRB, IUER
      REAL*8       RICH__AXIS 
      PARAMETER  ( RICH__AXIS = 0.6817D0 ) ! Rad + 39_03_36 deg
      REAL*4     ARGS(3) !!
      REAL*8     UEN_TO_XYZ(3,3), TEM, DSPL_UEN(3,SPD__M_STA), LEN, TEMP
      INTEGER*4  J1, J2, J3, J4, I_STA, DIMS(3), INDS(3), &
     &           IND_ANTI(SPD__M_STA), IND_STA(SPD__M_STA), IER
      REAL*4,    EXTERNAL :: VAL_3D_BSPL4 
      REAL*8,    EXTERNAL :: VAL_3D_BSPL
      INTEGER*4, EXTERNAL :: IXMN4 
!
      SPD%DSPL%NSTA = 0
      DO 410 J1=1,ANTI%N_ANT
         I_STA = 0
         DO 420 J2=1,SPD%NSTA
            IF ( SPD%STA(J2)%NAME == ANTI%STA_NAM(J1) ) THEN
                 I_STA = J2
            END IF
 420     CONTINUE 
         IF ( I_STA == 0 ) THEN
              WRITE ( 6, * ) 'Did not find station '//ANTI%STA_NAM(J1) 
            ELSE 
              SPD%DSPL%NSTA = SPD%DSPL%NSTA  + 1
              IND_ANTI(SPD%DSPL%NSTA) = J1
              IND_STA(SPD%DSPL%NSTA)  = I_STA
!              WRITE ( 6, * ) INT2(J1), ANTI%INFO(J1)%NAME, ANTI%INFO(J1)%MOUNTING_TYPE, &
!     &                       SNGL(ANTI%INFO(J1)%FOUNDATION_HEIGHT), SNGL(ANTI%INFO(J1)%FOUNDATION_TE), &
!     &                       SNGL(ANTI%INFO(J1)%PILLAR_HEIGHT), SNGL(ANTI%INFO(J1)%PILLAR_TE)
         END IF
 410  CONTINUE 
      ALLOCATE ( SPD%DSPL%NAME(SPD%DSPL%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5211, IUER , 'THERM_EXP', 'Failure in an '// &
     &         'attempt to allocate memory for array SPD%DSPL%NAME' )
           RETURN 
      END IF
      ALLOCATE ( SPD%DSPL%DSPL_XYZ(3,SPD%DSPL%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5212, IUER , 'THERM_EXP', 'Failure in an '// &
     &         'attempt to allocate memory for array SPD%DSPL%DSPL_XYZ' )
           RETURN 
      END IF
!
      DIMS(1) = MALO%NLEV
      DIMS(2) = MALO%NLON
      DIMS(3) = MALO%NLAT
      MALO%NTIM = 1
!
        write ( 6, * ) 'lon: ', MALO%NLON, MALO%LON ! %%%
        write ( 6, * ) 'lat: ', MALO%NLAT, MALO%LAT ! %%%
      DO 430 J3=1,SPD%DSPL%NSTA
         SPD%DSPL%NAME(J3) = ANTI%INFO(IND_ANTI(J3))%NAME
         CALL MAKE_UEN_TO_XYZ  ( SPD%STA(IND_STA(J3))%COO_CFS, UEN_TO_XYZ )
!
!!         ARGS(1) = SPD%STA(IND_STA)%HEI_GEOID
         ARGS(1) = LOG(SPD%STA(IND_STA(J3))%HEI_GEOID + SPD__U2_GMAO72)*SPD__U1_GMAO72 + SPD__U3_GMAO72
         ARGS(2) = SPD%STA(IND_STA(J3))%LON
         ARGS(3) = SPD%STA(IND_STA(J3))%LAT_GDT
!
         INDS(1) = IXMN4 ( MALO%NLEV, MALO%LEV, ARGS(1) )
         INDS(2) = IXMN4 ( MALO%NLON, MALO%LON, ARGS(2) )
         INDS(3) = IXMN4 ( MALO%NLAT, MALO%LAT, ARGS(3) )
  write ( 6, * ) 'therm-exp 80  args= ', args, ' inds= ', inds; call flush(6) ! %%%%%%%%%%%%%%%%%%%%%%%
         TEMP    = VAL_3D_BSPL ( ARGS, MALO__MDEG, DIMS, INDS, &
     &                           MALO%LEV, MALO%LON, MALO%LAT, &
     &                           MALO%PPWTEM_4D(1-MALO__MDEG,1-MALO__MDEG,1-MALO__MDEG,1,MALO__TEM) )
         DSPL_UEN(1:3,J3) = 0.0D0
         IF ( ANTI%INFO(IND_ANTI(J3))%MOUNTING_TYPE == ANTI__MO_AZEL ) THEN
              LEN = ANTI%INFO(IND_ANTI(J3))%FOUNDATION_HEIGHT* &
     &              ANTI%INFO(IND_ANTI(J3))%FOUNDATION_TE* &
     &                       (TEMP - ANTI%INFO(IND_ANTI(J3))%REF_TEMP) + &
     &              ANTI%INFO(IND_ANTI(J3))%PILLAR_HEIGHT* &
     &              ANTI%INFO(IND_ANTI(J3))%PILLAR_TE* &
     &                       (TEMP - ANTI%INFO(IND_ANTI(J3))%REF_TEMP)
              DSPL_UEN(1,J3) = LEN
           ELSE IF ( ANTI%INFO(IND_ANTI(J3))%MOUNTING_TYPE == ANTI__MO_EQUA ) THEN
              LEN = ANTI%INFO(IND_ANTI(J3))%FOUNDATION_HEIGHT* &
     &              ANTI%INFO(IND_ANTI(J3))%FOUNDATION_TE* &
     &                       (TEMP - ANTI%INFO(IND_ANTI(J3))%REF_TEMP) + &
     &              ANTI%INFO(IND_ANTI(J3))%PILLAR_HEIGHT* &
     &              ANTI%INFO(IND_ANTI(J3))%PILLAR_TE* &
     &                       (TEMP - ANTI%INFO(IND_ANTI(J3))%REF_TEMP)
              DSPL_UEN(1,J3) = ABS(LEN*DSIN(SPD%STA(IND_STA(J3))%LAT_GDT))
              DSPL_UEN(3,J3) = LEN*DCOS(SPD%STA(IND_STA(J3))%LAT_GDT)
           ELSE IF ( ANTI%INFO(IND_ANTI(J3))%MOUNTING_TYPE == ANTI__MO_XYNO ) THEN
              LEN = ANTI%INFO(IND_ANTI(J3))%FOUNDATION_HEIGHT* &
     &              ANTI%INFO(IND_ANTI(J3))%FOUNDATION_TE* &
     &                       (TEMP - ANTI%INFO(IND_ANTI(J3))%REF_TEMP)
              DSPL_UEN(1,J3) = LEN
           ELSE IF ( ANTI%INFO(IND_ANTI(J3))%MOUNTING_TYPE == ANTI__MO_XYEA ) THEN
              LEN = ANTI%INFO(IND_ANTI(J3))%FOUNDATION_HEIGHT* &
     &              ANTI%INFO(IND_ANTI(J3))%FOUNDATION_TE* &
     &                       (TEMP - ANTI%INFO(IND_ANTI(J3))%REF_TEMP)
              DSPL_UEN(1,J3) = LEN
           ELSE IF ( ANTI%INFO(IND_ANTI(J3))%MOUNTING_TYPE == ANTI__MO_RICH ) THEN
              LEN = ANTI%INFO(IND_ANTI(J3))%FOUNDATION_HEIGHT* &
     &              ANTI%INFO(IND_ANTI(J3))%FOUNDATION_TE* &
     &                       (TEMP - ANTI%INFO(IND_ANTI(J3))%REF_TEMP) + &
     &              ANTI%INFO(IND_ANTI(J3))%PILLAR_HEIGHT* &
     &              ANTI%INFO(IND_ANTI(J3))%PILLAR_TE* &
     &                       (TEMP - ANTI%INFO(IND_ANTI(J3))%REF_TEMP)
              DSPL_UEN(1,J3) = LEN*DSIN(RICH__AXIS)
              DSPL_UEN(3,J3) = LEN*DCOS(RICH__AXIS)
         END IF
!
         IF ( IVRB .GE. 3 ) THEN
              WRITE ( 6, * ) ' STA: ', SPD%DSPL%NAME(J3), &
     &                       ' TEMP = ', SNGL(TEMP), &
     &                       ' REF= ', SNGL(ANTI%INFO(IND_ANTI(J3))%REF_TEMP)
         END IF
!!         CALL MUL_MV_IV_V ( 3, 3, UEN_TO_XYZ, 3, DSPL_UEN, 3, SPD%DSPL%DSPL_XYZ(1,J3), IER )
 430  CONTINUE 
!
      MALO%NSTA = SPD%DSPL%NSTA
      ALLOCATE ( MALO%STA(MALO%NSTA), STAT=IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5213, IUER, 'THERM_EXP', 'Failure in an '// &
     &         'attempt to allocate dynamic memory for MALO%STA' )
           RETURN 
      END IF
      DO 440 J4=1,MALO%NSTA
         MALO%STA(J4)%NAME  = SPD%STA(IND_STA(J4))%NAME
         MALO%STA(J4)%COO   = SPD%STA(IND_STA(J4))%COO_CFS
         MALO%STA(J4)%LAT_GDT   = SPD%STA(IND_STA(J4))%LAT_GDT
         MALO%STA(J4)%LON       = SPD%STA(IND_STA(J4))%LON
         MALO%STA(J4)%HEI_ELL   = SPD%STA(IND_STA(J4))%HEI_ELL
         MALO%STA(J4)%HEI_GEOID = SPD%STA(IND_STA(J4))%HEI_GEOID
         MALO%STA(J4)%TAI_BEG   = MALO%UTC_BEG + SPD%CONF%LAG
         MALO%STA(J4)%TAI_END   = MALO%UTC_BEG + SPD%CONF%LAG + SPD%CONF%TIM_INTRV
         MALO%STA(J4)%MJD_BEG   = MALO%MJD_BEG 
         MALO%STA(J4)%MJD_END   = MALO%MJD_BEG 
 440  CONTINUE 
      MALO%MJD_END = MALO%STA(1)%MJD_END   
      MALO%TAI_BEG = MALO%STA(1)%TAI_BEG
      MALO%TAI_END = MALO%STA(1)%TAI_END   
!
      CALL ERR_PASS ( IUER, IER )
      CALL MALO_EPHEDISP_WRITE ( MALO, DSPL_UEN, THERM_EXP_PROG__LABEL, &
     &                           FILOUT, &
     &                           SPD%CONF%FIL_DESC, SPD%CONF%FIL_COMM, &
     &                           SPD%CONF%FIL_FMT, IER )
      IF ( IER .NE. 0 ) THEN
           CALL ERR_LOG ( 5214, IUER, 'THERM_EXP', 'Failure in an '// &
     &         'attempt to write displacements in the output file '// &
     &          FILOUT )
           RETURN 
      END IF
      DEALLOCATE ( MALO%STA )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE THERM_EXP  !#!#
