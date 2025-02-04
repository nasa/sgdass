      SUBROUTINE DEFINE_PALETTE ( ITYP, IDEV, NCOL, INIT_COL )
! ************************************************************************
! *                                                                      *
! *   Auxiliary routine  DEFINUE_PALETTE sets palette for plots.         *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     ITYP ( INTEGER*4 ) -- Type of the palette:                       *
! *                           ITYP = 1 -- grey scale;                    *
! *                           ITYP = 2 -- yellow-read-blue;              *
! *     IDEV ( INTEGER*4 ) -- Device code.                               *
! *                           IDEV = 1 -- Xwindow                        *
! *                           IDEV = 2 -- postscript;                    *
! *                           IDEV = 3 -- gif-file.                      *
! *     NCOL ( INTEGER*4 ) -- The number of requested colors.            *
! * INIT_COL ( INTEGER*4 ) -- Initial color index.                       *
! *                                                                      *
! * ### 25-JUL-2002 DEFINE_PALETTE v1.0 (c)  L. Petrov  15-NOV-2012 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INTEGER*4  ITYP, IDEV, NCOL, INIT_COL
      INTEGER*4  IR, J1
      REAL*4     WHITE_CLR, R_CLR, G_CLR, B_CLR
      REAL*4     BRIGHT, CONTRA, WL(10), WR(10), WG(10), WB(10), &
     &           RL(9), RR(9), RG(9), RB(9), &
     &           AL(20), AR(20), AG(20), AB(20), &
     &           PL(21), PR(21), PG(21), PB(21)
      DATA WL /0.0, 0.5, 0.5, 0.7, 0.7, 0.85, 0.85, 0.95, 0.95, 1.0/
      DATA WR /0.0, 1.0, 0.0, 0.0, 0.3,  0.8,  0.3,  1.0,  1.0, 1.0/
      DATA WG /0.0, 0.5, 0.4, 1.0, 0.0,  0.0,  0.2,  0.7,  1.0, 1.0/
      DATA WB /0.0, 0.0, 0.0, 0.0, 0.4,  1.0,  0.0,  0.0, 0.95, 1.0/
      DATA RL /-0.5, 0.0, 0.17, 0.33, 0.50, 0.67, 0.83, 1.0, 1.7/
      DATA RR / 0.0, 0.0,  0.0,  0.0,  0.6,  1.0,  1.0, 1.0, 1.0/
      DATA RG / 0.0, 0.0,  0.0,  1.0,  1.0,  1.0,  0.6, 0.0, 1.0/
      DATA RB / 0.0, 0.3,  0.8,  1.0,  0.3,  0.0,  0.0, 0.0, 1.0/
!
      DATA AL /0.0, 0.1, 0.1, 0.2, 0.2, 0.3, 0.3, 0.4, 0.4, 0.5, &
     &         0.5, 0.6, 0.6, 0.7, 0.7, 0.8, 0.8, 0.9, 0.9, 1.0/
      DATA AR /0.0, 0.0, 0.3, 0.3, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, &
     &         0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0/ 
      DATA AG /0.0, 0.0, 0.3, 0.3, 0.0, 0.0, 0.0, 0.0, 0.8, 0.8, &
     &         0.6, 0.6, 1.0, 1.0, 1.0, 1.0, 0.8, 0.8, 0.0, 0.0/
      DATA AB /0.0, 0.0, 0.3, 0.3, 0.7, 0.7, 0.7, 0.7, 0.9, 0.9, &
     &         0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0/
      REAL*8     COL_COEF
!
      PL( 1) = 0.00;  PR( 1) = 0.00;  PG( 1) = 0.00;  PB( 1) = 0.20
      PL( 2) = 0.05;  PR( 2) = 0.26;  PG( 2) = 0.00;  PB( 2) = 0.35
      PL( 3) = 0.10;  PR( 3) = 0.24;  PG( 3) = 0.05;  PB( 3) = 0.57
      PL( 4) = 0.15;  PR( 4) = 0.22;  PG( 4) = 0.20;  PB( 4) = 0.79
      PL( 5) = 0.20;  PR( 5) = 0.10;  PG( 5) = 0.30;  PB( 5) = 1.00
      PL( 6) = 0.25;  PR( 6) = 0.00;  PG( 6) = 0.50;  PB( 6) = 1.00
      PL( 7) = 0.30;  PR( 7) = 0.00;  PG( 7) = 0.80;  PB( 7) = 1.00
      PL( 8) = 0.35;  PR( 8) = 0.00;  PG( 8) = 1.00;  PB( 8) = 1.00
      PL( 9) = 0.40;  PR( 9) = 0.00;  PG( 9) = 1.00;  PB( 9) = 0.50
      PL(10) = 0.45;  PR(10) = 0.70;  PG(10) = 1.00;  PB(10) = 0.00
      PL(11) = 0.50;  PR(11) = 0.90;  PG(11) = 1.00;  PB(11) = 0.00
      PL(12) = 0.55;  PR(12) = 1.00;  PG(12) = 0.90;  PB(12) = 0.00
      PL(13) = 0.60;  PR(13) = 1.00;  PG(13) = 0.75;  PB(13) = 0.00
      PL(14) = 0.65;  PR(14) = 1.00;  PG(14) = 0.50;  PB(14) = 0.00
      PL(15) = 0.70;  PR(15) = 1.00;  PG(15) = 0.35;  PB(15) = 0.00
      PL(16) = 0.75;  PR(16) = 0.95;  PG(16) = 0.10;  PB(16) = 0.00
      PL(17) = 0.80;  PR(17) = 0.80;  PG(17) = 0.05;  PB(17) = 0.00
      PL(18) = 0.85;  PR(18) = 0.60;  PG(18) = 0.00;  PB(18) = 0.00
      PL(19) = 0.90;  PR(19) = 0.45;  PG(19) = 0.00;  PB(19) = 0.00
      PL(20) = 0.95;  PR(20) = 0.35;  PG(20) = 0.00;  PB(20) = 0.00
      PL(21) = 1.00;  PR(21) = 0.10;  PG(21) = 0.00;  PB(21) = 0.00
!
      WHITE_CLR = 255.0
!
      IF ( IDEV .EQ. 1 ) THEN
           COL_COEF = 0.75   ! color magnification for X window
         ELSE
!!           COL_COEF = 0.75  ! color magnification for files
           COL_COEF = 0.5  ! color magnification for files
      END IF
      IF ( ITYP == 3 ) THEN
           CONTRA = 1.3
           BRIGHT = 0.6
           CALL PGSCIR ( INIT_COL , INIT_COL + NCOL-1 + 6 )
           CALL PGCTAB ( RL, RR, RG, RB, 9, CONTRA, BRIGHT )
           RETURN 
        ELSE IF ( ITYP == 4 ) THEN
           CONTRA = 1.0
           BRIGHT = 0.5
           CALL PGSCIR ( INIT_COL -2, INIT_COL + NCOL-1 + 1 )
           CALL PGCTAB ( WL, WR, WG, WB, 10, CONTRA, BRIGHT )
           RETURN 
        ELSE IF ( ITYP == 5 ) THEN
           CONTRA = 1.0
           BRIGHT = 0.5
           CALL PGSCIR ( INIT_COL, INIT_COL + NCOL-1 )
           CALL PGCTAB ( AL, AR, AG, AB, 20, CONTRA, BRIGHT )
           RETURN 
        ELSE IF ( ITYP == 6 ) THEN
           CONTRA = 1.00
           BRIGHT = 0.50
!
           CALL PGSCIR ( INIT_COL, INIT_COL + NCOL-1 )
           CALL PGCTAB ( PL, PR, PG, PB, 21, CONTRA, BRIGHT )
           RETURN 
        ELSE IF ( ITYP == 7 ) THEN
           CONTRA = 0.75
           BRIGHT = 0.45
!
           CALL PGSCIR ( INIT_COL, INIT_COL + NCOL-1 )
           CALL PGCTAB ( PL, PR, PG, PB, 21, CONTRA, BRIGHT )
           RETURN 
        ELSE IF ( ITYP == 8 ) THEN
           CONTRA = 0.80
           BRIGHT = 0.55!
           CALL PGSCIR ( INIT_COL, INIT_COL + NCOL-1 )
           CALL PGCTAB ( PL, PR, PG, PB, 21, CONTRA, BRIGHT )
           RETURN 
     END IF
!
      DO 410 J1=1,NCOL
         IF ( ITYP .EQ. 1 ) THEN
              R_CLR = WHITE_CLR/255.0 - DFLOAT(J1-1)/DFLOAT(NCOL-1)*COL_COEF
              G_CLR = WHITE_CLR/255.0 - DFLOAT(J1-1)/DFLOAT(NCOL-1)*COL_COEF
              B_CLR = WHITE_CLR/255.0 - DFLOAT(J1-1)/DFLOAT(NCOL-1)*COL_COEF
           ELSE IF ( ITYP .EQ. 2 ) THEN
              IR = NCOL-J1+1
              R_CLR = REAL(IR-1)/REAL(NCOL-1)*0.8 + 0.2
              G_CLR = MAX(0.0, 2.0*REAL(IR-1-NCOL/2)/REAL(NCOL-1))
              B_CLR = 0.2 + 0.4*REAL(NCOL-IR)/REAL(NCOL)
           ELSE IF ( ITYP .EQ. 8 ) THEN
              R_CLR = WHITE_CLR/255.0 - DFLOAT(J1-1)/DFLOAT(NCOL-1)
              G_CLR = WHITE_CLR/255.0 - DFLOAT(J1-1)/DFLOAT(NCOL-1)
              B_CLR = WHITE_CLR/255.0 - DFLOAT(J1-1)/DFLOAT(NCOL-1)
         END IF
!
         CALL PGSCR ( INIT_COL+(J1-1), R_CLR, G_CLR, B_CLR )
 410  CONTINUE
      RETURN
      END  !#!  DEFINE_PALETTE  #!#
