      SUBROUTINE PCAL_FRQ_POL_IDX ( ANC, FRQ_VAL, IPOL, IND_FRQ, IUER )
!
! **************************************************************************************
! *                                                                                    *
! *   Routine  PCAL_FRQ_POL_IDX                                                        *
! *   Find the index of the given frequency and polarization. If no match exists, just *
! *   take the frequency index, and report no matching polarization could be found.    *
! *                                                                                    *
! *   INPUT:                                                                           *
! *          ANC           =  Parsed Antenna Callibration file    { DERIVED TYPE }     *
! *                                                                                    *
! *          FRQ_VAL       =  Frequency value                     { REAL*8 }           *
! *                                                                                    *
! *          IPOL          =  Polarization                        { INT*1 }            *
! *                           == 1 --> R                                               *
! *                           == 2 --> L                                               *
! *                           == 3 --> H                                               *
! *                           == 4 --> V                                               *
! *                           == 5 --> X                                               *
! *                           == 6 --> Y                                               *
! *                                                                                    *
! *          IUER          =  Error Handler                       { INT*4, OPT }       *
! *                           If IUER=0 no error message will be printed,  even in     *
! *                           the event of an error. However, for other possible       *
! *                           values, i.e. IUER=-1,-2, & -3, the error message will    *
! *                           print to screen. For the latter case, i.e., IUER = -3,   *
! *                           after printing the program will terminate.               *
! *                           Default, IUER = -1                                       *
! *                                                                                    *
! *   OUTPUT:                                                                          *
! *          IND_FRQ       =  Frequency index                     { INT*4 }            *
! *                                                                                    *
! * ###  07-DEC-2022   PCAL_TIME_RAW            v1.0 (c)  N. Habana  07-DEC-2022   ### *
! *                                                                                    *
! **************************************************************************************
!
!$$ TO DO: INCLUDE THE MATCHING OF IPOL

      IMPLICIT   NONE 
      INCLUDE    'atp.i'
      TYPE ( ANC__TYP   ) :: ANC
      INTEGER*4  NUM_PCS, IND_FRQ, IUER
      REAL*8     FRQ_VAL, FREQ_ARR(ANC__MEPC)
      INTEGER*1  POL_ARR(ANC__MEPC), IPOL
      REAL*8, ALLOCATABLE :: ARR1(:), ARR2(:), ARR3(:)
      REAL*8     FRQ_TEMP(ANC__MEPC)
      REAL*8     POL_TEMP(ANC__MEPC), POL_TEMP2(ANC__MEPC)
      INTEGER*4  IDX1, IDX2, IND_FRQ_TMP
      INTEGER*4  J0, J1, J2, J3, J4
      REAL*8     ERR_MAX
      PARAMETER  ( ERR_MAX = 1.D-6 )
      CHARACTER  CPOL*2, CFRQ*8
      LOGICAL*4, EXTERNAL :: IS_R4_NAN, IS_R8_NAN
      INTEGER*4, EXTERNAL :: IXMN8, IXMN8_S, IXMN4, LINDEX, I_LEN
!     
! --- Grab the frequency and polarization arrays, along with their indexes
!
      ALLOCATE (ARR1(ANC%NUM_PCS), ARR2(ANC%NUM_PCS), ARR3(ANC%NUM_PCS))
!
      DO 300 J0 = 1, ANC%NUM_PCS
         ARR1(J0) = ANC%PCS(J0)%SKY_FRQ
         ARR2(J0) = REAL(ANC%PCS(J0)%POL, 8)
         ARR3(J0) = REAL(J0,8)
 300  CONTINUE
!
! --- Sort these in order of increasing frequency
!
      CALL SORT83 ( ANC%NUM_PCS, ARR1, ARR2, ARR3 )
!
! --- Iterate through only half the array. 
!
      IND_FRQ_TMP  = 0
      IDX1     = 0 
      IDX2     = 0
      DO 310 J1 = 1, ANC%NUM_PCS
!     ------
         IF ( J1 == 1 ) THEN
!
! --------- Search index for the closest value to the frequency we want
!
            IDX1 = IXMN8 ( ANC%NUM_PCS, ARR1, FRQ_VAL )
!
! --------- Does this index match the polarization?
!
               IND_FRQ_TMP = IDX1
               GOTO 700
         ELSE
!
! --------- Start searching from where we stopped.
!
            IDX2 = IDX1 + 1
!
            IDX1 = IXMN8_S (IDX2, ANC%NUM_PCS, ARR1, FRQ_VAL )
!
! --------- Does this index match the polarization?
!
               IND_FRQ_TMP = IDX1
         END IF
 310  CONTINUE
! ---
 700  CONTINUE
! ---
      IF ( IND_FRQ_TMP == 0 ) THEN
!
! ------ Convert the polarization to a character
!
         CPOL  =  ANC__POL(IPOL)
!
! ------ Convert the frequency to a character
!
         WRITE ( CFRQ, '(F8.1)' ) FRQ_VAL
         CFRQ = CFRQ(1:I_LEN(CFRQ)-2)
         CALL CHASHL( CFRQ )
! ------ 
         IUER  = -1
         CALL ERR_LOG ( 2912, IUER, 'PCAL_FRQ_POL_IDX',              &
     &           'No match for the given polarization '//CPOL//         &
     &           'and frequency '//CFRQ )

      ELSE
         IND_FRQ = INT( ARR3(IND_FRQ_TMP), 4 )
      END IF
! ---
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END SUBROUTINE PCAL_FRQ_POL_IDX !#!1
!
