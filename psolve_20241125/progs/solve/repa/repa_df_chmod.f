      FUNCTION   REPA_DF_CHMOD ( DIAGI_S, REP, NEW_MODE ) 
! ************************************************************************
! *                                                                      *
! *   Function REPA_DF_INIT changes the REPA mode. (REPA mode means the  *
! *   bounding of mouse buttons).                                        *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! * NEW_MODE ( INTEGER*4 ) -- Index of the new mode.                     *
! *  DIAGI_S ( RECORD    ) -- Object which keeps internal parameters for *
! *                           plotting the current window.               *
! *                                                                      *
! * ________________________ Modified parameters: ______________________ *
! *                                                                      *
! *      REP ( RECORD    ) -- Object which keeps internal parameters for *
! *                           program REPA (REsiduals Plots and          *
! *                           Ambiguities).                              *
! *                                                                      *
! *  ### 06-DEC-2004  REPA_DF_CHMOD  v1.0 (c) L. Petrov  06-DEC-2004 ### *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  REPA_DF_CHMOD
      INCLUDE    'solve.i'
      INCLUDE    'diagi.i'
      INCLUDE    'repa.i'
      TYPE     ( DIAGI_STRU ) :: DIAGI_S
      TYPE     ( REP__TYPE  ) :: REP
      INTEGER*4  NEW_MODE
      INTEGER*4, EXTERNAL :: REPA_DF_INIT 
      CHARACTER  STR*32
      INTEGER*4  IP
      INTEGER*4, EXTERNAL :: ILEN, I_LEN
!
      IF ( NEW_MODE .GE. 1  .AND.  NEW_MODE .LE. REP__M_MOD ) THEN
           REP%CNF%MOD_IND = NEW_MODE
           IP = REPA_DF_INIT ( DIAGI_S, REP ) 
           REPA_DF_CHMOD = DIAGI__CONT   
         ELSE 
!
! -------- Invalid mode
!
           CALL CLRCH ( STR ) 
           CALL INCH  ( NEW_MODE, STR )
           CALL ERR_LOG ( 7901, -1, 'REPA_DF_CHMOD', 'Trap of internal '// &
     &         'control: illegal value of NEW_MODE: '//STR )
           REPA_DF_CHMOD = DIAGI__QUIT
      END IF
!
      RETURN
      END  FUNCTION  REPA_DF_CHMOD
