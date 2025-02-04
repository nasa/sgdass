      SUBROUTINE CRES_DO ( IMENU2, NP4, ARR, B3DOBJ, CNSTROBJ, &
     &                     LBUF_LEN, LBUF, IPTR, PAGEWID )
! ************************************************************************
! *                                                                      *
! *   Routine  CRES_DO  computes post-fit residuals and some statisitcs  *
! *                                                                      *
! * _________________________ Input parameters: ________________________ *
! *                                                                      *
! *     IMNEU ( INTEGER*2 ) -- Menu switcher. If IMENU2 = 0 then no menu *
! *                            is displayed.  If IMENU2 = 1 then CRES    *
! *                            menu is built.                            *
! *       NP2 ( INTEGER*2 ) -- Total number of observation in the        *
! *                            current session.                          *
! *       ARR ( REAL*8    ) -- Array which contains vecotr of adjusments *
! *                            and covariance matrix of the solution.    *
! *    B3DOBJ ( RECORD    ) -- Object with data structure for B3D        *
! *                            extension of SOLVE.                       *
! *                                                                      *
! * _________________________ Modified parameters: _____________________ *
! *                                                                      *
! *  LBUF_LEN ( INTEGER*4 ) -- Maximal number of lines in the text       *
! *                            array LBUF.                               *
! *      LBUF ( CHARACTER ) -- Text array LBUF. Length: LBUF_LEN lines.  *
! *      IPTR ( INTEGER*4 ) -- Index of the last filled line in the      *
! *                            text array LBUF.                          *
! *                                                                      *
! * _________________________ Output parameters: _______________________ *
! *                                                                      *
! *  CNSTROBJ  ( RECORD    ) -- The data structure with information      *
! *                             about equations of constraints.          *
! *   PAGEWID ( INTEGER*4 ) -- Widfth of the terminal (in symbols).      *
! *                                                                      *
! *   pet  1999.05.28  Made LBUF_LEN, LBUF, IPTR, PAGEWID formal         *
! *                    arguments, eliminated common block cres_buf       *
! *   pet  2002.05.06  Added an additional parameter in call of THIRD    *
! *   pet  2002.05.14  Added closing GLBFxx file                         *
! *   pet  2002.10.04  Added parameter CNSTROBJ                          *
! *   pet  2005.02.28  Added update of volatile data structures for      *
! *                    non-linear site position motion estimation.       *
! *                                                                      *
! *  ###  15-MAR-99     CRES_DO    v1.6  (c)  L. Petrov 28-FEB-2005 ###  *
! *                                                                      *
! ************************************************************************
      IMPLICIT   NONE
      INCLUDE   'solve.i'
      INCLUDE   'glbcm.i'
      INCLUDE   'glbc4.i'
      INCLUDE   'erm.i'
      INCLUDE   'socom.i'
      INCLUDE   'socom_plus.i'
      INCLUDE   'precm.i'
      INCLUDE   'fast.i'
      INCLUDE   'cnstr.i'
!
      TYPE ( B3D__STRU ) ::  B3DOBJ
      TYPE ( CNSTR__STRU ) ::  CNSTROBJ
      INTEGER*4   LBUF_LEN, IPTR, PAGEWID, IUER
      CHARACTER   LBUF(LBUF_LEN)*120
      INTEGER*2  IMENU2, IRESTYP, NP4
      INTEGER*4  IXX, IYY
      REAL*8     ARR
      REAL*8     JD_DUR_NOM, JD_DUR_ACT
      LOGICAL*2  FL_11
      LOGICAL*2, EXTERNAL :: KBIT 
!
      NPARAM = NP4
      IUER = -1
!
      IF ( FAST_DBG .EQ. F__TIM ) THEN                  
           CALL TIM_INIT()
      END IF
      CALL OPENNAMFIL()
!
! --- Set status
!
      IF ( TRAIN ) CALL STATUS_SET ( 'CRES', STA__BEG )
!
      CALL USE_GLBFIL   ( 'OR' )  ! Reread GLBFIL . Else kuser_part is changed
      CALL USE_GLBFIL_4 (  'R' )  ! Reread GLBFIL . Else kuser_part is changed
!
      IRESTYP = 0
      IF ( .NOT. KBATCH ) CALL GETXY_MN ( IXX, IYY )
      IF ( IMENU2 .EQ. 1 ) THEN
           CALL NL_MN()
           CALL NL_MN()
           CALL CREMU ( IRESTYP )
      END IF
      CALL USE_GLBFIL   ( 'W'  )  ! Write GLBFIL
      CALL USE_GLBFIL_4 ( 'WC' )  ! ... and GLBFIL_4
!
! --- Open and read the site and star names from PARFIL
!
      CALL USE_PARFIL ( 'ORC' )
!
! --- Create volatile objects for non-linear site position estimates and 
! --- for extimation of the empirical Earth roation model
!
      IF ( L_HPE > 0 .AND. ADR_HPE .NE. 0 ) THEN
           CALL HPESOL_CREATE ( %VAL(ADR_HPE) )
         ELSE
           FL_HPESOL = .FALSE.
      END IF
      IF ( L_SPE > 0 .AND. ADR_SPE .NE. 0 ) THEN
           CALL SPESOL_CREATE ( %VAL(ADR_SPE) )
         ELSE
           FL_SPESOL = .FALSE.
      END IF
!
      IF ( L_EERM > 0 .AND. ADR_EERM .NE. 0 ) THEN
           CALL EERM_CREATE ( %VAL(ADR_EERM) )
         ELSE
           FL_EERM = .FALSE.
      END IF
!
! --- Open data file
!
      FL_11 = KBIT( PRE_IBATCH, INT2(11) ) 
      IF ( DBNAME_CH(1:1) .NE. '$' ) CALL SBIT ( PRE_IBATCH, INT2(11), 0 )
      CALL ACS_OBSFIL ( 'O' )
      IF ( DBNAME_CH(1:1) .NE. '$'  .AND.  FL_11 ) CALL SBIT ( PRE_IBATCH, INT2(11), 1 )
!
      CALL FIRST ( LBUF_LEN, LBUF, IPTR, PAGEWID )
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Debugging printout
!
           CALL TIM_GET ( 'CRES-01' )
           CALL TIM_INIT()
      END IF
!
! --- Flyby initialization
!
      CALL FLYBY_APRIOR()
!
      IUER = -1
      CALL SECND ( ARR, B3DOBJ, CNSTROBJ, IRESTYP, JD_DUR_NOM, JD_DUR_ACT, &
     &             LBUF_LEN, LBUF, IPTR, PAGEWID, IUER )
      IF ( IUER .NE. 0 ) THEN
           CALL ERR_LOG ( 7551, -1, 'CRES_DO', 'Errors in SECND' )
           CALL EXIT ( 1 )
      END IF
      CALL CLOSENAMFIL()
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Debugging printout
!
           CALL TIM_GET ( 'CRES-02' )
           CALL TIM_INIT()
      END IF
!
      CALL THIRD ( JD_DUR_NOM, JD_DUR_ACT, B3DOBJ%SUWSQ_TAU, LBUF_LEN, &
     &             LBUF, IPTR, PAGEWID )
!
! --- Close the observation file
!
      CALL ACS_OBSFIL ( 'C' )
      CALL USE_COMMON ( 'OWC' )
      IF ( FAST_DBG .EQ. F__TIM ) THEN
!
! -------- Debugging printout
!
           CALL TIM_GET ( 'CRES-03' )
           CALL TIM_INIT()
      END IF
      IF ( TRAIN ) CALL STATUS_SET ( 'CRES', STA__END )
!
      RETURN
      END  !#!  CRES_DO  #!#
