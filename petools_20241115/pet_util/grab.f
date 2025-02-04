      SUBROUTINE GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, NPAR, &
     &                            LEN_01,  ADR_01, &
     &                            LEN_02,  ADR_02, &
     &                            LEN_03,  ADR_03, &
     &                            LEN_04,  ADR_04, &
     &                            LEN_05,  ADR_05, &
     &                            LEN_06,  ADR_06, &
     &                            LEN_07,  ADR_07, &
     &                            LEN_08,  ADR_08, &
     &                            LEN_09,  ADR_09, &
     &                            LEN_10,  ADR_10, &
     &                            LEN_11,  ADR_11, &
     &                            LEN_12,  ADR_12, &
     &                            LEN_13,  ADR_13, &
     &                            LEN_14,  ADR_14, &
     &                            LEN_15,  ADR_15, &
     &                            LEN_16,  ADR_16, &
     &                            LEN_17,  ADR_17, &
     &                            LEN_18,  ADR_18, &
     &                            LEN_19,  ADR_19, &
     &                            LEN_20,  ADR_20, &
     &                            LEN_21,  ADR_21, &
     &                            LEN_22,  ADR_22, &
     &                            LEN_23,  ADR_23, &
     &                            LEN_24,  ADR_24, &
     &                            LEN_25,  ADR_25, &
     &                            LEN_26,  ADR_26, &
     &                            LEN_27,  ADR_27, &
     &                            LEN_28,  ADR_28, &
     &                            LEN_29,  ADR_29, &
     &                            LEN_30,  ADR_30, &
     &                            LEN_31,  ADR_31, &
     &                            LEN_32,  ADR_32  )
! ************************************************************************
! *                                                                      *
! *   Routine  GRAB_MEM  allocates continuous area of dynamic memory for *
! *   arrays with specified lengths and sets up addresses for the first  *
! *   bytes of memory for these arrays. The number of arrays should be   *
! *   specified and may vary from 1 to 32. The length and the address of *
! *   the entire allocated pool of memory is returned.                   *
! *   Each returned address is align at a border of 64 bytes, 512 bits.  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     NPAR ( INTEGER*4      ) -- The number of arrays.                 *
! *   LEN_01 ( INTEGER*?      ) -- The length of the 1-st array (bytes). *
! *   LEN_02 ( INTEGER*?, OPT ) -- The length of the 2-nd array (bytes). *
! *   LEN_03 ( INTEGER*?, OPT ) -- The length of the 3-rd array (bytes). *
! *   LEN_04 ( INTEGER*?, OPT ) -- The length of the 4-th array (bytes). *
! *   LEN_05 ( INTEGER*?, OPT ) -- The length of the 5-th array (bytes). *
! *                                                                      *
! *     etc. Only the first NPAR pairs LEN_xx, ADR_xx  should be         *
! *     specifeid in the list of actual arguments. Other unued arguments *
! *     can be omitted.
! *                                                                      *
! * ________________________ Output parameters: ________________________ *
! *                                                                      *
! *  MEM_LEN ( INTEGER*?      ) -- The size of the pool of allocated     *
! *                                memory (bytes).                       *
! *  MEM_ADR ( INTEGER*?      ) -- Address of the pool of allocated      *
! *                                memory.                               *
! *   ADR_01 ( INTEGER*?      ) -- The address of the 1-st array (bytes) *
! *   LEN_02 ( INTEGER*?, OPT ) -- The address of the 2-nd array (bytes) *
! *   LEN_03 ( INTEGER*?, OPT ) -- The address of the 3-rd array (bytes) *
! *   LEN_04 ( INTEGER*?, OPT ) -- The address of the 4-th array (bytes) *
! *   LEN_05 ( INTEGER*?, OPT ) -- The address of the 5-th array (bytes) *
! *                                                                      *
! *     etc.                                                             *
! *                                                                      *
! * ________________________ MODIFIED PARAMETERS: ______________________ *
! *                                                                      *
! *   IUER ( INTEGER*4, OPT ) -- Universal error habdler.                *
! *                           Input: swicth IUER=0 -- no error messages  *
! *                                  will be generated even in the case  *
! *                                  of error. IUER=-1 -- in the case of *
! *                                  error the message will pe put on    *
! *                                  stdout.                             *
! *                           Output: 0 in the case of successfull       *
! *                                   completion and non-zero in the     *
! *                                   case of error.                     *
! *                                                                      *
! *   Comment:                                                           *
! *                                                                      *
! *     Memory for each block will be alligned at the boundary of        *
! *   16-bytes block (address will be a multiple of 16).                 *
! *                                                                      *
! *     GRAB_MEM can be called with reduced list of actual arguments.    *
! *   The number of actual arguments can be reduced up to NPAR*2 + 4.    *
! *   Example:                                                           *
! *                                                                      *
! *     CALL GRAB_MEM ( IUER, MEM_LEN, MEM_ADR, 3,                       *
! *    #                      1024,    ADR_01,                           *
! *    #                      819201,  ADR_02,                           *
! *    #                      512,     ADR_03  )                         *
! *                                                                      *
! *    Memory for three arrays will be allocated at this example.        *
! *    Addresses for these arrays will be ADR_01, ADR_02, ADR_03;        *
! *    the lengths: 1024, 819201 and 512 bytes respectively. Since the   *
! *    size of memeory requested for the second array is not amuliple of *
! *    16 a bit more pool of memory will be allocated in order to allign *
! *    the address of ARR_03 at the boundary of 16-bytes. Therefore      *
! *    MEM_LEN will be a bit more than the sum of lengths (but no more   *
! *    larger than NPAR*16 bytes).                                       *
! *                                                                      *
! *  ###  01-AUG-97    GRAB_MEM    v1.4 (c)  L. Petrov  12-JUN-2009 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  IUER, NPAR
      ADDRESS__TYPE :: MEM_LEN, MEM_ADR
      ADDRESS__TYPE :: LEN_01
      ADDRESS__TYPE, OPTIONAL ::  LEN_02, LEN_03, LEN_04, LEN_05, &
     &           LEN_06, LEN_07, LEN_08, LEN_09, LEN_10, &
     &           LEN_11, LEN_12, LEN_13, LEN_14, LEN_15, &
     &           LEN_16, LEN_17, LEN_18, LEN_19, LEN_20, &
     &           LEN_21, LEN_22, LEN_23, LEN_24, LEN_25, &
     &           LEN_26, LEN_27, LEN_28, LEN_29, LEN_30, &
     &           LEN_31, LEN_32
      ADDRESS__TYPE :: ADR_01
      ADDRESS__TYPE, OPTIONAL ::  ADR_02, ADR_03, ADR_04, ADR_05, &
     &           ADR_06, ADR_07, ADR_08, ADR_09, ADR_10, &
     &           ADR_11, ADR_12, ADR_13, ADR_14, ADR_15, &
     &           ADR_16, ADR_17, ADR_18, ADR_19, ADR_20, &
     &           ADR_21, ADR_22, ADR_23, ADR_24, ADR_25, &
     &           ADR_26, ADR_27, ADR_28, ADR_29, ADR_30, &
     &           ADR_31, ADR_32
      CHARACTER  STR*32
      INTEGER*4  ALGN
      PARAMETER  ( ALGN = 64 ) ! memory alignment: 64 bytes, i.e. 512 bits
      INTEGER*4, EXTERNAL     :: I_LEN 
      ADDRESS__TYPE, EXTERNAL :: ADDRESS_ALIGN
!
! --- Test NPAR
!
      IF ( NPAR .LT. 1 .OR. NPAR .GT. 32 ) THEN
           CALL CLRCH ( STR )
           CALL INCH  ( NPAR, STR )
           CALL ERR_LOG ( 1, IUER, 'GRAB_MEM', 'Wrong value of NPAR: "'// &
     &          STR(1:I_LEN(STR))//'". NPAR should be in the range [1, 32]' )
           RETURN
      END IF
!
! --- Calculation the length of the grabbed memory
!
      MEM_LEN = ALGN
      IF ( NPAR .GE.  1 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_01, ALGN )
      IF ( NPAR .GE.  2 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_02, ALGN )
      IF ( NPAR .GE.  3 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_03, ALGN )
      IF ( NPAR .GE.  4 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_04, ALGN )
      IF ( NPAR .GE.  5 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_05, ALGN )
      IF ( NPAR .GE.  6 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_06, ALGN )
      IF ( NPAR .GE.  7 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_07, ALGN )
      IF ( NPAR .GE.  8 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_08, ALGN )
      IF ( NPAR .GE.  9 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_09, ALGN )
      IF ( NPAR .GE. 10 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_10, ALGN )
      IF ( NPAR .GE. 11 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_11, ALGN )
      IF ( NPAR .GE. 12 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_12, ALGN )
      IF ( NPAR .GE. 13 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_13, ALGN )
      IF ( NPAR .GE. 14 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_14, ALGN )
      IF ( NPAR .GE. 15 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_15, ALGN )
      IF ( NPAR .GE. 16 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_16, ALGN )
      IF ( NPAR .GE. 17 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_17, ALGN )
      IF ( NPAR .GE. 18 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_18, ALGN )
      IF ( NPAR .GE. 19 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_19, ALGN )
      IF ( NPAR .GE. 20 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_20, ALGN )
      IF ( NPAR .GE. 21 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_21, ALGN )
      IF ( NPAR .GE. 22 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_22, ALGN )
      IF ( NPAR .GE. 23 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_23, ALGN )
      IF ( NPAR .GE. 24 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_24, ALGN )
      IF ( NPAR .GE. 25 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_25, ALGN )
      IF ( NPAR .GE. 26 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_26, ALGN )
      IF ( NPAR .GE. 27 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_27, ALGN )
      IF ( NPAR .GE. 28 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_28, ALGN )
      IF ( NPAR .GE. 29 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_29, ALGN )
      IF ( NPAR .GE. 30 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_30, ALGN )
      IF ( NPAR .GE. 31 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_31, ALGN )
      IF ( NPAR .GE. 32 ) MEM_LEN = MEM_LEN + ADDRESS_ALIGN ( LEN_32, ALGN )
#ifdef ADR_32BIT
!
! --- Allocation of the pool of memory
!
      CALL GET_MEM32 ( MEM_LEN, MEM_ADR )
#else
      CALL GET_MEM   ( MEM_LEN, MEM_ADR )
#endif
      IF ( MEM_ADR .EQ. 0 ) THEN
           CALL CLRCH ( STR )
#ifdef ADR_32BIT
           CALL IINCH  ( MEM_LEN, STR )
#else
           CALL IINCH8 ( MEM_LEN, STR )
#endif
           CALL ERR_LOG ( 1, IUER, 'GRAB_MEM', 'Failure in grabbing '// &
     &          STR(1:I_LEN(STR))//' bytes of dynamic memory' )
           RETURN
      END IF
!
! --- Setting up the addresses of the arrays
!
      ADR_01 = MEM_ADR
      IF ( NPAR .GE.  2 ) ADR_02 = ADR_01 + ADDRESS_ALIGN ( LEN_01, ALGN )
      IF ( NPAR .GE.  3 ) ADR_03 = ADR_02 + ADDRESS_ALIGN ( LEN_02, ALGN )
      IF ( NPAR .GE.  4 ) ADR_04 = ADR_03 + ADDRESS_ALIGN ( LEN_03, ALGN )
      IF ( NPAR .GE.  5 ) ADR_05 = ADR_04 + ADDRESS_ALIGN ( LEN_04, ALGN )
      IF ( NPAR .GE.  6 ) ADR_06 = ADR_05 + ADDRESS_ALIGN ( LEN_05, ALGN )
      IF ( NPAR .GE.  7 ) ADR_07 = ADR_06 + ADDRESS_ALIGN ( LEN_06, ALGN )
      IF ( NPAR .GE.  8 ) ADR_08 = ADR_07 + ADDRESS_ALIGN ( LEN_07, ALGN )
      IF ( NPAR .GE.  9 ) ADR_09 = ADR_08 + ADDRESS_ALIGN ( LEN_08, ALGN )
      IF ( NPAR .GE. 10 ) ADR_10 = ADR_09 + ADDRESS_ALIGN ( LEN_09, ALGN )
      IF ( NPAR .GE. 11 ) ADR_11 = ADR_10 + ADDRESS_ALIGN ( LEN_10, ALGN )
      IF ( NPAR .GE. 12 ) ADR_12 = ADR_11 + ADDRESS_ALIGN ( LEN_11, ALGN )
      IF ( NPAR .GE. 13 ) ADR_13 = ADR_12 + ADDRESS_ALIGN ( LEN_12, ALGN )
      IF ( NPAR .GE. 14 ) ADR_14 = ADR_13 + ADDRESS_ALIGN ( LEN_13, ALGN )
      IF ( NPAR .GE. 15 ) ADR_15 = ADR_14 + ADDRESS_ALIGN ( LEN_14, ALGN )
      IF ( NPAR .GE. 16 ) ADR_16 = ADR_15 + ADDRESS_ALIGN ( LEN_15, ALGN )
      IF ( NPAR .GE. 17 ) ADR_17 = ADR_16 + ADDRESS_ALIGN ( LEN_16, ALGN )
      IF ( NPAR .GE. 18 ) ADR_18 = ADR_17 + ADDRESS_ALIGN ( LEN_17, ALGN )
      IF ( NPAR .GE. 19 ) ADR_19 = ADR_18 + ADDRESS_ALIGN ( LEN_18, ALGN )
      IF ( NPAR .GE. 20 ) ADR_20 = ADR_19 + ADDRESS_ALIGN ( LEN_19, ALGN )
      IF ( NPAR .GE. 21 ) ADR_21 = ADR_20 + ADDRESS_ALIGN ( LEN_20, ALGN )
      IF ( NPAR .GE. 22 ) ADR_22 = ADR_21 + ADDRESS_ALIGN ( LEN_21, ALGN )
      IF ( NPAR .GE. 23 ) ADR_23 = ADR_22 + ADDRESS_ALIGN ( LEN_22, ALGN )
      IF ( NPAR .GE. 24 ) ADR_24 = ADR_23 + ADDRESS_ALIGN ( LEN_23, ALGN )
      IF ( NPAR .GE. 25 ) ADR_25 = ADR_24 + ADDRESS_ALIGN ( LEN_24, ALGN )
      IF ( NPAR .GE. 26 ) ADR_26 = ADR_25 + ADDRESS_ALIGN ( LEN_25, ALGN )
      IF ( NPAR .GE. 27 ) ADR_27 = ADR_26 + ADDRESS_ALIGN ( LEN_26, ALGN )
      IF ( NPAR .GE. 28 ) ADR_28 = ADR_27 + ADDRESS_ALIGN ( LEN_27, ALGN )
      IF ( NPAR .GE. 29 ) ADR_29 = ADR_28 + ADDRESS_ALIGN ( LEN_28, ALGN )
      IF ( NPAR .GE. 30 ) ADR_30 = ADR_29 + ADDRESS_ALIGN ( LEN_29, ALGN )
      IF ( NPAR .GE. 31 ) ADR_31 = ADR_30 + ADDRESS_ALIGN ( LEN_30, ALGN )
      IF ( NPAR .GE. 32 ) ADR_32 = ADR_31 + ADDRESS_ALIGN ( LEN_31, ALGN )
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  SUBROUTINE  GRAB_MEM  !#!#
