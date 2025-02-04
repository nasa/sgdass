      SUBROUTINE SESTAT_DBS ( DBOBJ, NUMSCA, IT, IUER )
! ************************************************************************
! *                                                                      *
! *   Procedure  SESTAT_DBS  makes eventual update of the data structure *
! *   of the database statistics. It sorts the lists of stations,        *
! *   baselines, sources. It is assumed that all observations have been  *
! *   already processed by SESTAT_OBS.                                   *
! *                                                                      *
! * _______________________ Input parameters: __________________________ *
! *                                                                      *
! *  NUMSCA ( INTEGER*4 ) -- Total number of different scans in the      *
! *                          database.                                   *
! *      IT ( INTEGER*4 ) -- Debugging switch. If IT > 0 then debugging  *
! *                          information will be printed at the screen.  *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *   DBOBJ ( RECORD    ) -- Data structure which keeps general          *
! *                          information about the database such as      *
! *                          lists of the objects.                       *
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
! *  Comment: if IT > 0 (debugging mode) then it is assumed that curses  *
! *           mode has been turned on (by call start_mn) earlier.        *
! *           Otherwise the program will be terminated.                  *
! *                                                                      *
! *  ###  07-JUL-98   SESTAT_DBS   v1.2  (c)  L. Petrov  23-JUL-98  ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE ! Updated by Jim Ryan for I*4 compliance, Sept 2002
      INCLUDE    'solve.i'
      INCLUDE    'obser.i'
      INCLUDE    'prfil.i'
      INCLUDE    'precm.i'
      TYPE ( DBOBJ_O__STRU ) ::  DBOBJ
      INTEGER*4  IUER, NUMSCA
      INTEGER*4  J2, J3, J4, J5, J6, J7, K1, K2, K3, K4, &
     &           IB1, IB2, IB3, IP1, IP2, IP3, IP, IT, &
     &           IP_SOU, IP_STA, IP_STA1, IP_STA2, ISTA1, ISTA2, IER
      LOGICAL*2, EXTERNAL :: KBIT
      INTEGER*4, EXTERNAL :: IFIND_PL
      CHARACTER, EXTERNAL :: GET_CDATE*19
!
      DBOBJ%L_SCA = NUMSCA
!
! --- Sorting station and source lists
!
      CALL SORT_I2 ( DBOBJ%L_STA, DBOBJ%LIS_STA, DBOBJ%KL_STA )
      CALL SORT_I2 ( DBOBJ%L_SOU, DBOBJ%LIS_SOU, DBOBJ%KL_SOU )
!
      DO 420 J2=1,DBOBJ%L_SOU
!
! ------ Putting names of the sourcres to DBOBJ data structure
!
         IP_SOU = DBOBJ%LIS_SOU(J2)
         CALL CLRCH ( DBOBJ%C_SOU(IP_SOU) )
         DBOBJ%C_SOU(J2) = ISTRN_CHR( IP_SOU ) ! ISTRN_CHR from prfil.i
 420  CONTINUE
!
! --- ... stations list
!
      DO 430 J3=1,DBOBJ%L_STA
!
! ------ Putting names of the stations to DBOBJ data structure
!
         IP_STA = DBOBJ%LIS_STA(J3)
         CALL CLRCH ( DBOBJ%C_STA(IP_STA) )
         DBOBJ%C_STA(J3) = ISITN_CHR( IP_STA ) ! ISITN_CHR from prfil.i
 430  CONTINUE
!
! --- Let's check: whether there were the observations at the same baseline but
! --- in opposite order (for, example: NYALES-ONSALA and ONSALA-NYALES)
!
      DO 440 J4=1,DBOBJ%L_BAS
         IP = DBOBJ%LIS_BAS(J4)
         IF ( IFIND_PL ( DBOBJ%L_BAS, DBOBJ%LIS_BAS, -IP ) .GT.0 ) THEN
              CALL NBAST ( IP, ISTA1, ISTA2 )
              IP_STA1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA1 )
              IP_STA2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA2 )
!             CALL ERR_LOG ( 8571, IUER, 'SESSTAT_DBS', 'Database '//
!     #             DBOBJ.NAME//' contains observations made at the baselines '//
!     #             '"'//DBOBJ.C_STA(IP_STA1)//'/'//DBOBJ.C_STA(IP_STA2)//
!     #             '" and '//
!     #             '"'//DBOBJ.C_STA(IP_STA2)//'/'//DBOBJ.C_STA(IP_STA1)//'"' )
!              RETURN
         END IF
!
! ------ Baseline list will be sorted in accordance with increasing modules
! ------ of baseline codes (since baseline code may be negative). To do it
! ------ array DBOBJ.KU_BAS will be spoiled temporarily: the oldest
! ------ decimal digits will be occupied by module of baseline code
! ------ (but 5 youngest digits remained intact).
!
         DBOBJ%KL_BAS(J4) = 65536*ABS(DBOBJ%LIS_BAS(J4)) + DBOBJ%KL_BAS(J4)
  440 CONTINUE
!
! --- After that we sort (in increasing order) a pair of tied arrays:
! --- DBOBJ.KL_BAS and DBOBJ.LIS_BAS in according with increasing
! --- "spoiled" array DBOBJ.KL_BAS
!
      CALL SORT_I2 ( DBOBJ%L_BAS, DBOBJ%KL_BAS, DBOBJ%LIS_BAS )
!
! --- And now -- removing "spoliage" from the array DBOBJ.KL_BAS
!
      DO 450 J5=1,DBOBJ%L_BAS
         DBOBJ%KL_BAS(J5) = DBOBJ%KL_BAS(J5) - 65536*ABS(DBOBJ%LIS_BAS(J5))
!
         CALL NBAST ( DBOBJ%LIS_BAS(J5), ISTA1, ISTA2 )
         IP_STA1 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA1 )
         IP_STA2 = IFIND_PL ( DBOBJ%L_STA, DBOBJ%LIS_STA, ISTA2 )
         DBOBJ%C_BAS(J5)=DBOBJ%C_STA(IP_STA1)//'/'//DBOBJ%C_STA(IP_STA2)
  450 CONTINUE
!
! --- Then we sort lists of used stations and sources. It may be easily done
! --- without problems:
!
      CALL SORT_I2 ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%KU_STA )
      CALL SORT_I2 ( DBOBJ%U_SOU, DBOBJ%UIS_SOU, DBOBJ%KU_SOU )
!
! --- The same technology for sorting used baseline list
!
      DO 460 J6=1,DBOBJ%U_BAS
         DBOBJ%KU_BAS(J6) = 65536*ABS(DBOBJ%UIS_BAS(J6)) + &
     &                                 DBOBJ%KU_BAS(J6)
  460 CONTINUE
      CALL SORT_I2 ( DBOBJ%U_BAS, DBOBJ%KU_BAS, DBOBJ%UIS_BAS )
      DO 470 J7=1,DBOBJ%U_BAS
         DBOBJ%KU_BAS(J7) = DBOBJ%KU_BAS(J7) - 65536*ABS(DBOBJ%UIS_BAS(J7))
  470 CONTINUE
!
! --- Creating the list of closed triangles (if there are any)
!
      IF ( DBOBJ%U_STA .GE. 3 ) THEN
           CALL ERR_PASS ( IUER, IER )
           CALL TRI_GRP  ( DBOBJ%U_STA, DBOBJ%UIS_STA, DBOBJ%U_BAS, &
     &                     DBOBJ%UIS_BAS, MO_TRI, DBOBJ%U_TRI, &
     &                     DBOBJ%UIS_TRI, IER )
           IF ( IER .NE. 0 ) THEN
!
! -------------- It is serious error but not fatal for further processing of
! -------------- the database.
!
!                CALL ERR_LOG ( 8572, IUER, 'SESTAT_DBS', 'Error during '//
!     #              'building list of closed triangles' )
!                RETURN
!                CALL LIB$WAIT ( 3.0 )
                WRITE ( 6, * ) 'But nevertheless going further...'
                DBOBJ%U_TRI = -1
          END IF
        ELSE
           DBOBJ%U_TRI = 0
      END IF
!
! --- Deal done.
!
      DBOBJ%STATUS = DBOBJ__DON
!
      IF ( IT .GT. 0 ) THEN
         IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6) ) ) THEN
              WRITE ( 6, * ) ' SESTAT_DBS: disabling curses...'
              CALL END_MN()  ! postpone curser
              CALL UN_CURSES ( )
         END IF
!
! ------ Debugging printout
!
         WRITE ( 6, * ) '=== Database ',DBOBJ%NAME
         WRITE ( 6, * ) ' '
         WRITE ( 6, * ) ' DBOBJ%L_SCA  = ',DBOBJ%L_SCA
         WRITE ( 6, * ) ' DBOBJ%L_OBS  = ',DBOBJ%L_OBS
         WRITE ( 6, * ) ' DBOBJ%R_OBS  = ',DBOBJ%R_OBS
         WRITE ( 6, * ) ' DBOBJ%U_OBS  = ',DBOBJ%U_OBS
         WRITE ( 6, * ) ' DBOBJ%CG_OBS = ',DBOBJ%CG_OBS
         WRITE ( 6, * ) ' '
!
         WRITE ( 6, * ) ' DBOBJ%L_SOU = ',DBOBJ%L_SOU, ' DBOBJ%U_SOU = ', &
     &   DBOBJ%U_SOU
         WRITE ( 6, * ) ' DBOBJ%L_STA = ',DBOBJ%L_STA, ' DBOBJ%U_STA = ', &
     &   DBOBJ%U_STA
         WRITE ( 6, * ) ' DBOBJ%L_BAS = ',DBOBJ%L_BAS, ' DBOBJ%U_BAS = ', &
     &   DBOBJ%U_BAS
         WRITE ( 6, * ) ' DBOBJ%U_TRI = ',DBOBJ%U_TRI
!
         WRITE ( 6, * ) '----------'
!
         DO 510 K1=1,DBOBJ%U_SOU
            IP = IFIND_PL( DBOBJ%L_SOU, DBOBJ%LIS_SOU, DBOBJ%UIS_SOU(K1) )
            WRITE ( 6, 151 )  K1, DBOBJ%UIS_SOU(K1), DBOBJ%KU_SOU(K1), &
     &      DBOBJ%C_SOU(IP)
 151        FORMAT ( 1X,'(used) SOU ==> I=',I3,' LIS=',I8,' K=',I4, &
     &                  ' Source   >>',A,'<<' )
 510     CONTINUE
!
         WRITE ( 6, * ) '----------'
!
         DO 520 K2=1,DBOBJ%U_STA
            IP = IFIND_PL( DBOBJ%L_STA, DBOBJ%LIS_STA, DBOBJ%UIS_STA(K2) )
            WRITE ( 6, 152 )  K2, DBOBJ%UIS_STA(K2), DBOBJ%KU_STA(K2), &
     &      DBOBJ%C_STA(IP)
 152        FORMAT ( 1X,'(used) STA ==> I=',I3,' LIS=',I8,' K=',I4, &
     &                  ' Station  >>',A,'<<' )
 520     CONTINUE
!
         WRITE ( 6, * ) '----------'
!
         DO 530 K3=1,DBOBJ%U_BAS
            IP = IFIND_PL( DBOBJ%L_BAS, DBOBJ%LIS_BAS, DBOBJ%UIS_BAS(K3) )
            WRITE ( 6, 153 )  K3, DBOBJ%UIS_BAS(K3), DBOBJ%KU_BAS(K3), &
     &      DBOBJ%C_BAS(IP)
 153        FORMAT ( 1X,'(used) BAS ==> I=',I3,' LIS=',I8,' K=',I4, &
     &                  ' Baseline >>',A,'<<' )
 530     CONTINUE
!
         WRITE ( 6, * ) '----------'
!
         DO 540 K4=1,DBOBJ%U_TRI
            IB1 = DBOBJ%UIS_TRI(1,K4)
            IB2 = DBOBJ%UIS_TRI(2,K4)
            IB3 = DBOBJ%UIS_TRI(3,K4)
!
            IP1 = IFIND_PL( DBOBJ%L_BAS, DBOBJ%LIS_BAS, DBOBJ%UIS_BAS(IB1) )
            IP2 = IFIND_PL( DBOBJ%L_BAS, DBOBJ%LIS_BAS, DBOBJ%UIS_BAS(IB2) )
            IP3 = IFIND_PL( DBOBJ%L_BAS, DBOBJ%LIS_BAS, DBOBJ%UIS_BAS(IB3) )
!
            WRITE ( 6, 154 )  K4, &
     &           DBOBJ%LIS_BAS(IP1), DBOBJ%LIS_BAS(IP2), DBOBJ%LIS_BAS(IP3), &
     &           DBOBJ%C_BAS(IP1),   DBOBJ%C_BAS(IP2),   DBOBJ%C_BAS(IP3)
 154        FORMAT ( 1X,'(used) TRI ==> I=',I3,' LIS=',3(I8,2X)/ &
     &               1X,'       Baselines:  ',A,' -- ',A,' -- ',A )
 540     CONTINUE
!
         CALL HIT_CONT ( %VAL(0), %VAL(0) )
         IF ( KSCREEN .AND. KBIT( PRE_IP(2), INT2(6) ) ) THEN
              CALL START_MN()  ! start curser again
         END IF
      END IF
!
      CALL ERR_LOG ( 0, IUER )
      RETURN
      END  !#!  SESTAT_DBS  #!#
