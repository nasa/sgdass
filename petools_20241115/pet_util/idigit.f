        FUNCTION IDIGIT ( ID, IP )
! ************************************************************************
! *                                                                      *
! *     Function IDIGIT returns the value of decimal digit which takes   *
! *     the IP-th plase at the INTEGER*4  number ID. Digits are counted  *
! *     from right to left starting from 1. If IP is larger than the     *
! *     total number of decimal digits in ID then IRIGIT returns 0.      *
! *                                                                      *
! *                     Examples:                                        *
! *                                                                      *
! *     IR = IDIGIT (  21984, -8 )    ===>   IR=4                        *
! *     IR = IDIGIT (  21984,  0 )    ===>   IR=4                        *
! *     IR = IDIGIT (  21984,  1 )    ===>   IR=4                        *
! *     IR = IDIGIT (  21984,  2 )    ===>   IR=8                        *
! *     IR = IDIGIT (  21984,  3 )    ===>   IR=9                        *
! *     IR = IDIGIT (  21984,  4 )    ===>   IR=1                        *
! *     IR = IDIGIT (  21984,  5 )    ===>   IR=2                        *
! *     IR = IDIGIT (  21984,  6 )    ===>   IR=0                        *
! *     IR = IDIGIT (  21984,  7 )    ===>   IR=0                        *
! *     IR = IDIGIT ( -21984,  2 )    ===>   IR=8                        *
! *                                                                      *
! *                                                                      *
! * ________________________ Input parameters: _________________________ *
! *                                                                      *
! *     ID ( INTEGER*4 ) -- The number under investigation.              *
! *     IP ( INTEGER*4 ) -- index of the decimal digit to be checked     *
! *                         (conted from right to left from 1).          *
! *                                                                      *
! * _______________________ Output parameters: _________________________ *
! *                                                                      *
! *     <IDIGIT> ( INTEGER*4 ) -- Value of the IP-th digit in the        *
! *                               integer number ID.                     *
! *  ### 18-AUG-93     IDIGIT     v1.1 (c)  L. Petrov  13-JAN-2000  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE
        INTEGER*4  ID, IP, IDIGIT
        INTEGER*4  IPL, IDG, J1
!
        IDG=IABS(ID)
        IPL=IP
        IF ( IPL.LT.1  ) IPL=1
        IF ( IPL.GT.10 ) IPL=10
!
        IF ( IPL.NE.1 ) THEN
!
! ---------- Shift IDG to right at the IPL-1 decimal digits
!
             DO 410 J1=1,IPL-1
                IDG=IDG/10
  410        CONTINUE
        END IF
!
! ----- Extraction of the last digit
!
        IDIGIT = IDG - (IDG/10)*10
!
        RETURN
        END  !#!  IDIGIT  #!#
