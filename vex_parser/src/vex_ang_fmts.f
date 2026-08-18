      SUBROUTINE VEX_TO_TIME ( STR_VEX, LN, STR_SLV, IUER )
! ***************************************************************************
! *                                                                         *
! *     Subroutine to convert a time angle given VEX format to the same     *
! *     angle in SOLVE format.                                              *
! *     N.B.: This routine is best suited for ammending the RA in the       *
! *     SOURCE block.                                                       *
! *                                                                         *
! *     INPUT:                                                              *
! *             STR_VEX   =  Angle in VEX format        { CHAR }            *
! *                          ##h##m##.######s                               *
! *                                                                         *
! *             LN        =  Length of String           { INT }             *
! *                                                                         *
! *             IUER      =  Error Handler                 { INT, OPT }     *
! *                          If IUER=0 no error message will be printed,    *
! *                          even in the event of an error. However, for    *
! *                          other possible values, i.e. IUER=-1,-2, & -3,  *
! *                          the error message will print to screen. For    *
! *                          the latter case, i.e. IUER=-3, after printing  *
! *                          the program will terminate.                    *
! *                                                                         *
! *     OUTPUT:                                                             *
! *             STR_SOLVE =  Angle in SOLVE format      { CHAR }            *
! *                          ##:##:##.######                                *
! *                                                                         *
! *     Version 1.0               N. Habana                 07 Apr. 2020    *
! *                                                                         *
! *************************************************************************** 
!
       IMPLICIT    NONE
       CHARACTER   STR_VEX*(*), STR_SLV*(LN-1)
       INTEGER*4   IUER, LN

      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 9001, IUER, 'VEX_TO_TIME',                    &
     &          'ERROR: Unable to run routine due to preceeding '//     &
     &          'error(s). Check the input variables.' )
            RETURN
      END IF
!
! --- Get length of input string 
!
!***!      LN = LEN(STR_VEX)
!
      IF ( LN .GT. 17 ) THEN
           CALL ERR_LOG ( 9002, IUER, 'VEX_TO_TIME',                    &
     &           'Input string is longer than expected. ' )
           RETURN
      END IF
!
! --- Other possible read-in errors.
! --- Notice that the IUER input in these messages is preset to -1, to
!     allow for the printing of these errors, in case the original IUER
!     was 0, at subroutine call.
!
      IF ( STR_VEX(3:3) .NE. 'h' ) THEN 
           CALL ERR_LOG ( 9003, IUER, 'VEX_TO_TIME', 'Wrong string '//  &
     &       'format: missing "h" for hour angle.' )
           RETURN
      END IF
!
      IF ( STR_VEX(6:6) .NE. 'm' ) THEN 
           CALL ERR_LOG ( 9004, IUER, 'VEX_TO_TIME', 'Wrong string '//  &
     &       'format: missing "m" for arcmins.' )
           RETURN
      END IF
!
      IF ( STR_VEX(LN:LN) .NE. 's' ) THEN 
           CALL ERR_LOG ( 9005, IUER, 'VEX_TO_TIME', 'Wrong string '//  &
     &       'format: missing "s" for arcsecs (at the end).' )
           RETURN
      END IF
!
      STR_SLV = STR_VEX(1:2)//':'//STR_VEX(4:5)//':'//STR_VEX(7:LN-1)
      STR_SLV = TRIM(STR_SLV)
!
!****!      DO 110 I1 = 1,LN
!****!         IF ( STR_VEX(I1:I1) .EQ. 'h' ) THEN 
!****!              STR_SLV(I1:I1) = ':'
!****!         ELSE IF ( STR_VEX(I1:I1) .EQ. 'm' ) THEN
!****!              STR_SLV(I1:I1) = ':'
!****!         ELSE IF ( STR_VEX(I1:I1) .EQ. 's' ) THEN
!****!              STR_SLV(I1:I1) = CHAR(0)          ! Null
!****!         ELSE
!****!              STR_SLV(I1:I1) = STR_VEX(I1:I1)
!****!         END IF
!****! 110  CONTINUE
        CALL ERR_LOG ( 0, IUER )
        RETURN
        END SUBROUTINE VEX_TO_TIME
!
!---------------------------------------------------------------------------
!
        SUBROUTINE VEX_TO_ANG ( STR_VEX, LN, STR_SLV, IUER )
! ***************************************************************************
! *                                                                         *
! *     Subroutine to convert a degree angle given VEX format to the same   *
! *     angle in SOLVE format.                                              *
! *     N.B.: -  This routine is best suited for ammending the DEC in the   *
! *              SOURCE block, which is -90<=dec<=90                        *
! *           -  In some instances, the positive degrees may include a +    *
! *              or not.                                                    *
! *           -  It is assumed that the input string has a maximum of 6     *
! *              significant figures after the decimal point, and a minimum *
! *              of 1.                                                      *
! *                                                                         *
! *     INPUT:                                                              *
! *             STR_VEX   =  Angle in VEX format        { CHAR }            *
! *                          ?##d##'##.######"          { ?=+/- }           *
! *                                                                         *
! *             LN        =  Length of STR_VEX          { INT }             *
! *                                                                         *
! *             IUER      =  Error Handler              { INT, OPT }        *
! *                          If IUER=0 no error message will be printed,    *
! *                          even in the event of an error. However, for    *
! *                          other possible values, i.e. IUER=-1,-2, & -3,  *
! *                          the error message will print to screen. For    *
! *                          the latter case, i.e. IUER=-3, after printing  *
! *                          the program will terminate.                    *
! *                                                                         *
! *     OUTPUT:                                                             *
! *             STR_SOLVE =  Angle in SOLVE format      { CHAR }            *
! *                          ?##:##:##.######           { ?=+/- }           *
! *                                                                         *
! *     Version 1.0               N. Habana                 07 Apr. 2020    *
! *                                                                         *
! *************************************************************************** 
!
        IMPLICIT    NONE
        CHARACTER   STR_VEX*(*), STR_SLV*(LN-1)
        INTEGER*4   IUER, LN
!
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 9101, IUER, 'VEX_TO_ANG',                     &
     &          'ERROR: Unable to run routine due to preceeding '//     &
     &          'error(s). Check the input variables.' )
          RETURN
      END IF
!
! --- Get length of input string 
!
!***!      LN = LEN(STR_VEX)
!
      IF ( LN .GT. 17 ) THEN
           CALL ERR_LOG ( 9102, IUER, 'VEX_TO_ANG',                     &
     &          'Input string is longer than expected. ' )
           RETURN
      END IF
!
      IF ( LN .LT. 11 ) THEN                                               ! positive (no "+") &  1 decimal pt
           CALL ERR_LOG ( 9103, IUER, 'VEX_TO_ANG',                     &
     &          'Input string is shorter than expected. ' )
      END IF
!
! --- The angle begins with a +/- sign
!
      IF ( STR_VEX(1:1) .EQ. '-' .OR. STR_VEX(1:1) .EQ. '+') THEN
!
! -------- Possible errors on symbols
!
           IF ( STR_VEX(4:4) .NE. 'd' ) THEN 
                CALL ERR_LOG ( 9104, IUER, 'VEX_TO_ANG', 'Wrong string' &
     &          //' format: missing "d" for degree angle.' )
                RETURN
           END IF
!
           IF ( STR_VEX(7:7) .NE. CHAR(39) ) THEN 
                CALL ERR_LOG ( 9105, IUER, 'VEX_TO_ANG', "Wrong string" &
     &          //" format: missing ' for arcmins." )
                RETURN
           END IF
!
           IF ( STR_VEX(LN:LN) .NE. CHAR(34) ) THEN 
                CALL ERR_LOG ( 9106, IUER, 'VEX_TO_ANG', 'Wrong string' &
     &          //' format: missing " for arcsecs (at the end).' )
                RETURN
           END IF
!
! -------- Change to SOLVE format
!
           STR_SLV(1:10)     = STR_VEX(1:3)//':'//STR_VEX(5:6)//':'//   &
     &          STR_VEX(8:10)
           STR_SLV(11:LN-1)  = STR_VEX(11:LN-1)
           STR_SLV = TRIM(STR_SLV)
      ELSE                                                                  ! Assume Positive with no "+"
!
! -------- Possible errors on symbols
!
           IF ( STR_VEX(3:3) .NE. 'd' ) THEN 
                CALL ERR_LOG ( 9107, IUER, 'VEX_TO_ANG', 'Wrong string' &
     &          //' format: missing "d" for degree angle.' )
                RETURN
           END IF
!
           IF ( STR_VEX(6:6) .NE. CHAR(39) ) THEN 
                CALL ERR_LOG ( 9108, IUER, 'VEX_TO_ANG', "Wrong string" &
     &          //" format: missing ' for arcmins." )
                RETURN
           END IF
!
           IF ( STR_VEX(LN:LN) .NE. CHAR(34) ) THEN 
                CALL ERR_LOG ( 9109, IUER, 'VEX_TO_ANG', 'Wrong string' &
     &          //' format: missing " for arcsecs (at the end).' )
                RETURN
           END IF
!          
! -------- Change to SOLVE format        
!
           STR_SLV(1:9)     = STR_VEX(1:2)//':'//STR_VEX(4:5)//':'//    &
     &          STR_VEX(7:9)
           STR_SLV(10:LN-1)  = STR_VEX(10:LN-1)        
           STR_SLV           = TRIM(STR_SLV)
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE VEX_TO_ANG
