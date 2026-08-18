      SUBROUTINE USE_NRMFIL ( MAT, NP, STRING )
      IMPLICIT NONE
!
! 1.  USE_NRMFIL PROGRAM SPECIFICATION
!
! 1.1 Access utility for NRMFIL. Passes control to USE_FILE.
!
! 1.2 REFERENCES:
!
! 2.  USE_NRMFIL INTERFACE
!
! 2.1 Parameter File
      INCLUDE 'solve.i'
!
! 2.2 INPUT Variables:
!
      INTEGER*4 NP
      CHARACTER*(*) STRING
      REAL*8 MAT(*)
!
! MAT - NRMFIL matrix
! NP - Number of parameters
! STRING - Requested access type
!
! 2.3 OUTPUT Variables: None
!
! 2.4 COMMON BLOCKS USED
      INCLUDE 'precm.i'
      INTEGER*4 FILDES
      COMMON /SAVNRM/ FILDES
      SAVE /SAVNRM/
!
! 2.5 SUBROUTINE INTERFACE
!
!       CALLING SUBROUTINES:
!       CALLED SUBROUTINES: use_file
!
! 3.  LOCAL VARIABLES
!
      INTEGER*4 MAT_E,JB,JRND_BLOCKS
      INTEGER*4 I4P1
      DATA  I4P1 / 1 /
      CHARACTER*(NAME_SIZE) FNAME
      INTEGER*8 LEN8_BYTES, LEN8_BLOCKS
      INTEGER*4, EXTERNAL :: MAT_E4 
!
! FNAME - Name of file to be accessed
! JB - Number of blocks to read or write
!
! 4.  HISTORY
!   WHO   WHEN   WHAT
!
! 5.  USE_NRMFIL PROGRAM STRUCTURE
!
! First, construct the file name
!
      FNAME=PRE_SCR_DIR(1:PRE_SD_LEN)//'NRMF'//PRE_LETRS
!
! Now figure out the number of blocks and access the file
!
!@      JB=JRND_BLOCKS(MAT_E(MAX_PAR,NP)*REALL_WORDS)
!@      CALL USE_FILE(FNAME,FILDES,MAT,JB,I4P1,STRING)
!
      LEN8_BYTES  = 8*MAT_E4(M_GPA, NP)
      LEN8_BLOCKS = (LEN8_BYTES+INT8(255))/INT8(256)
      CALL USE_FILE8 ( FNAME, FILDES, MAT, LEN8_BLOCKS, INT8(1), STRING )
!
      RETURN
      END  SUBROUTINE  USE_NRMFIL 
