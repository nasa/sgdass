!@@   SUBROUTINE GET_POL_TPS ( ANC, NPOL, IPOLS, IND_POLS, IUER )
      SUBROUTINE GET_POL_TPS ( ANC, NPOL, IPOLS, IUER )
!
! *******************************************************************************************
! *                                                                                         *
! *   Routine  GET_POL_TPS                                                                  *
! *   Get the TPS polarization and how many they are                                        *
! *                                                                                         *
! *   INPUT:                                                                                *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }          *
! *                                                                                         *
! *          IUER          =  Error Handler                       { INT*4, OPT }            *
! *                           If IUER=0 no error message will be printed,  even in          *
! *                           the event of an error. However, for other possible            *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will         *
! *                           print to screen. For the latter case, i.e., IUER = -3,        *
! *                           after printing the program will terminate.                    *
! *                           Default, IUER = -1                                            *
! *                                                                                         *
! *   OUTPUT:                                                                               *
! *          NPOL          =  Number of polarizations        { INT*4 }                      *
! *                                                                                         *
! *          IPOLS         =  The polarizations              { INT*1 } (NPOLx1)             *
! *                                                                                         *
! *          IND_POLS      =  Index of polarizations         { INT*4 } (NPOLx(NTPS/NPOL))   *
! *                                                                                         *
! * ###  14-DEC-2023   GET_POL_TPS                   v1.0 (c)  N. Habana  14-DEC-2023   ### *
! *                                                                                         *
! *******************************************************************************************
!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$ NEED TO FIND AN EFFICIENT WAY OF ASSIGNING THE INDEXES     $$$$$!
!$$$$ OF POLS TO IND_POL                                         $$$$$!
!$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
      
      IMPLICIT   NONE
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC      
      INTEGER*4  NPOL, IUER
      INTEGER*1  IPOLS(ANC__MNPOL), INDP1
!@@@2!      INTEGER*4  IPOLS4_ARR(ANC__MNPOL), IPOLS4, INDP
      INTEGER*4  IND_POLS(ANC__MNPOL, ANC__MTPS)
      INTEGER*4  J1, J2, J3, J4
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*1, EXTERNAL :: ADD_LIS1
! ---
      NPOL = 0
      INDP1 = 0
!     
! --- Go through the polarizations
!
      DO 410 J1 = 1, ANC%NUM_TPS
!
! ------ Convert the current polarization to an INT*4
!
!@@@2!         IPOLS4 = INT ( ANC%TPS(J1)%POL, 4)
!
! ------ Check if this polarization is in the list of polarizations, if
!        not, add it
! ------ N.B: INDP is the index of the polarization on the list
!     
         IUER = -1
!@@@!         INDP = ADD_LIS ( ANC__MNPOL, NPOL, IPOLS4_ARR, IPOLS4, IUER )
!
!
         INDP1 = ADD_LIS1 ( ANC__MNPOL, NPOL, IPOLS, ANC%TPS(J1)%POL, IUER )
!@@@!         IF ( INDP < 0 ) THEN
!
! --------- Update the polarization list
!
!@@@!            IPOLS(INDP) = IPOLS4_ARR(INDP)
!@@@!         END IF
 410  CONTINUE
       
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE
!
! ---------------------------------------------------------------------------------
!
