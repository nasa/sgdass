      SUBROUTINE VTD_METEO_IN ( VTD, STANAM, ATM_PRES, AIR_TEMP, &
     &                          AIR_TEMP_EFF, IUER )
! ************************************************************************
! *                                                                      *
! *   Routine VTD_METEO_IN inserts atmospheric pressure and air          *
! *   temperature for station STANAM in the object VTD .                 *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *   STANAM ( CHARACTER ) -- Name of the station of the baseline.       *
! *                           The station should be in the input         *
! *                           catalogue defined in the control file.     *
! * ATM_PRES ( REAL*8    ) -- Atmospheric pressure at the station in     *
! *                           Pascals.                                   *
! * AIR_TEMP ( REAL*8    ) -- Air temperature at the station in Kelvins. *
! * AIR_TEMP_EFF ( REAL*8  ) -- Effective air temperature for taking     *
! *                             into account thermal expansion of the    *
! *                             antenna (in Kelvins). It is up to the    *
! *                             analyst to descide which effective       *
! *                             temperature is the best. It was found    *
! *                             that the station air temperature lagged  *
! *                             2-3 hours gives the best agreement with  *
! *                             the in situ measuremenets.               *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *     VTD ( RECORD    ) -- Object which keeps configuration and data   *
! *                          related to VLBI Theoretical Delay (VTD)     *
! *                          package.                                    *
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
! *  ### 30-JAN-2004  VTD_METEO_IN  v1.2 (c)  L. Petrov 23-APR-2008 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE 
      INCLUDE   'vtd.i'
      TYPE     ( VTD__TYPE ) :: VTD
      CHARACTER  STANAM*(*)
      REAL*8     ATM_PRES, AIR_TEMP, AIR_TEMP_EFF
      INTEGER*4  IUER
      INTEGER*4  J1, ISTA
!
      ISTA = 0
      DO 410 J1=1,VTD%L_STA
         IF ( STANAM .EQ. VTD%STA(J1)%IVS_NAME ) THEN
              ISTA = J1
              GOTO 810
         END IF
 410  CONTINUE 
 810  CONTINUE 
      IF ( ISTA .EQ. 0 ) THEN
           CALL ERR_LOG ( 2341, IUER, 'VTD_METEO_IN', 'Station '// &
     &          STANAM//' was not found in the VTD list of stations' )
           RETURN 
      END IF
!
      VTD%STA(ISTA)%ATM_PRES = ATM_PRES
      VTD%STA(ISTA)%AIR_TEMP = AIR_TEMP
      VTD%STA(ISTA)%AIR_TEMP_EFF = AIR_TEMP_EFF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  VTD_METEO_IN 
