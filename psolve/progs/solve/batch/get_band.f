      FUNCTION   GET_BAND ( FREQ )
! ************************************************************************
! *                                                                      *
! *   Auxilliary function GET_BAND returns the band character designator *
! *   in accordance with the band frequency.                             *
! *                                                                      *
! *  ### 12-FEB-2007     GET_BAND  v1.0 (c)  L. Petrov  12-FEB-2007 ###  *
! *                                                                      *
! ************************************************************************
      CHARACTER  GET_BAND*(*)
      REAL*8     FREQ
!
      IF ( FREQ .GT. 40.D9 .AND. FREQ .LE. 60.D9 ) THEN
           GET_BAND = 'Q'
         ELSE IF ( FREQ .GT. 30.0D9  .AND.  FREQ .LE. 40.0D9 ) THEN
           GET_BAND = 'A'
         ELSE IF ( FREQ .GT. 18.0D9  .AND.  FREQ .LE. 30.0D9 ) THEN
           GET_BAND = 'K'
         ELSE IF ( FREQ .GT. 11.0D9  .AND.  FREQ .LE. 18.0D9 ) THEN
           GET_BAND = 'U'
         ELSE IF ( FREQ .GT.  6.5D9  .AND.  FREQ .LE. 11.0D9 ) THEN
           GET_BAND = 'X'
         ELSE IF ( FREQ .GT.  4.5D9  .AND.  FREQ .LE. 6.5D9 ) THEN
           GET_BAND = 'C'
         ELSE IF ( FREQ .GT.  1.8D9  .AND.  FREQ .LE. 4.5D9 ) THEN
           GET_BAND = 'S'
         ELSE IF ( FREQ .GT.  1.0D9  .AND.  FREQ .LE. 1.8D9 ) THEN
           GET_BAND = 'L'
         ELSE 
           GET_BAND = '?'
      END IF
!
      RETURN
      END  FUNCTION  GET_BAND !#!  
