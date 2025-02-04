        SUBROUTINE TIM_INIT
! ************************************************************************
! *                                                                      *
! *     Routine  TIM_INIT  makes initilazing timer for consequent calls  *
! *     TIM_TP.                                                          *
! *                                                                      *
! *  ###  12-JAN-90      TIM_INIT   v1.1  (c) L. Petrov  05-DEC-96  ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4  ISTATUS_TIMER
        REAL*8     TELP_TIMER, TCPU_TIMER
        REAL*4     TARG_R4(2)
        COMMON   / ADR_TIMER / TELP_TIMER, TCPU_TIMER, ISTATUS_TIMER
        INTEGER*4  TIMEVAL(4)
        INTEGER*2  TIMEZONE(4)
        REAL*4     ETIME
        EXTERNAL BLKDAT_ADR_TIMER
!
        CALL GETTIMEOFDAY ( TIMEVAL, TIMEZONE )
!
        TELP_TIMER = TIMEVAL(1) + TIMEVAL(2)*1.D-6
        TCPU_TIMER = ETIME ( TARG_R4 )
        ISTATUS_TIMER = 1
!
        RETURN
        END  SUBROUTINE TIM_INIT  
!
! ------------------------------------------------------------------------
!
        SUBROUTINE TIM_TP ( IPRN, TCPU, TELP, OUT )
! ************************************************************************
! *                                                                      *
! *     Routine TIM_TP  counts amount of physical and CPU time elappsed  *
! *     from the last call ot the routine TIM_INIT. Time intercal can be *
! *     either display on the screen or/and returned as a real*4 numbers *
! *     or returned as CHARACTER  string.                                *
! *                                                                      *
! * _________________________ INPUT PARAMETERS: ________________________ *
! *                                                                      *
! *     IPRN ( INTEGER*4, OPT )  --  mode switch.                        *
! *            IPRN=1 -- Time intervals are displayed on the screen.     *
! *                      ( Defult mode ).                                *
! *            IPRN=2 -- Time intervals are returned through output      *
! *                      parameters TELP, TCPU                           *
! *            IPRN=3 -- Time intervals are both displayed on the screen *
! *                      and returned through the output parameters      *
! *                      TELP, TCPU                                      *
! *            IPRN=4 -- Time intervals are returned through output      *
! *                      string  OUT.                                    *
! *                                                                      *
! * ________________________ OUTPUT PARAMETERS: ________________________ *
! *                                                                      *
! *                                                                      *
! *   TCPU ( REAL*8, OPT )  --  Interval of Central Processor Time       *
! *                             elapsed from the last call to TIM_INIT.  *
! *                             Precision: 0.02 sec. Calculated if       *
! *                             IPRN=2,3                                 *
! *   TELP ( REAL*8, OPT )  --  Interval of physical time elapsed from   *
! *                             the last call to TIM_INIT. Precision:    *
! *                             0.02 sec. Calculated if IPRN=2,3         *
! *   OUT ( CHARACTER, OPT )  --  String with elapsed physical time and  *
! *                               CPU time. The length of the string is  *
! *                               49 symbols. Right tail is padded by    *
! *                               blanks. Calculated if IPRN=4           *
! *                                                                      *
! *    Before calling TIM_TP the routine TIM_INIT should be called.      *
! *    All arguments could be omitted.                                   *
! *    Accessible forms:                                                 *
! *          CALL TIM_TP ( )                                             *
! *          CALL TIM_TP ( IPRN=4, OUT=STR )                             *
! *                                                                      *
! *  ###  12-JAN-1990    TIM_TP    v6.2  (c) L. Petrov  21-JUL-2002 ###  *
! *                                                                      *
! ************************************************************************
        IMPLICIT   NONE 
        INTEGER*4, OPTIONAL :: IPRN
        REAL*8,    OPTIONAL :: TELP, TCPU
        CHARACTER, OPTIONAL :: OUT*(*)
        INTEGER*4  IPRN_USE
        REAL*8     TELP_USE, TCPU_USE
        CHARACTER  OUT_USE*64
!
        INTEGER*4  ISTATUS_TIMER
        REAL*8     TELP_TIMER, TCPU_TIMER
        COMMON   / ADR_TIMER / TELP_TIMER, TCPU_TIMER, ISTATUS_TIMER
        EXTERNAL   BLKDAT_ADR_TIMER
        INTEGER*4  TIMEVAL(4), HOUR, IMIN, SEC, TICK
        INTEGER*2  TIMEZONE(4)
        REAL*4     TARG_R4(2)
        INTEGER*4  IB, ILN, IER
        REAL*4     ETIME
        INTEGER*4, EXTERNAL :: ILEN, I_LEN
#ifdef GNU
        INTRINSIC  FLUSH
#endif
!
        IF ( PRESENT(IPRN) .AND.  LOC(IPRN) .NE. 0 ) THEN
             IPRN_USE=IPRN
           ELSE
             IPRN_USE=1
        END IF
!
        IF ( ISTATUS_TIMER .EQ. 0 ) THEN
             TELP_USE = -1.1111111E11
             TCPU_USE = -1.1111111E11
!
             IER=-1
             CALL ERR_LOG ( 88, IER, 'TIM_TP', 'Timer has not '// &
     &                           'been initilized' )
             IF ( PRESENT(TELP)  .AND.  LOC(TELP) .NE. 0 ) TELP = TELP_USE
             IF ( PRESENT(TCPU)  .AND.  LOC(TCPU) .NE. 0 ) TCPU = TCPU_USE
             IF ( PRESENT(OUT)   .AND.  LOC(OUT)  .NE. 0 ) THEN
                  CALL CLRCH  ( OUT )
                  CALL REPEAT ( '*', 49, OUT )
             END IF
             RETURN
        END IF
!
! ----- Get the current time
!
        CALL GETTIMEOFDAY ( TIMEVAL, TIMEZONE )
!
! ----- Compute amount of elapsed time from the call of TIM_INIT
!
        TELP_USE = ( TIMEVAL(1) + TIMEVAL(2)*1.D-6 ) - TELP_TIMER 
!
! ----- Compute amount of CPU time from the call of TIM_INIT
!
        TCPU_USE = ETIME ( TARG_R4 ) - TCPU_TIMER
!
        HOUR=TCPU_USE/3600
        IMIN=(TCPU_USE - HOUR*3600.0D0)/60
        SEC =(TCPU_USE - HOUR*3600.0D0 - IMIN*60.0D0)
        TICK=(TCPU_USE - HOUR*3600.0D0 - IMIN*60.0D0 -  SEC)*100
        IF ( TICK .GE. 100 ) TICK = 99
        CALL CLRCH ( OUT_USE )
        IB = 1
        OUT_USE(IB:) ='CPU time:'
        ILN=ILEN(OUT_USE)+2
        CALL INCH   ( HOUR,  OUT_USE(ILN:ILN+1) )
        CALL CHASHR (        OUT_USE(ILN:ILN+1) )
        CALL BLANK_TO_ZERO ( OUT_USE(ILN:ILN+1) )
        OUT_USE(ILN+2:)=':'
        ILN=ILEN(OUT_USE)+1
        CALL INCH   ( IMIN,  OUT_USE(ILN:ILN+1) )
        CALL CHASHR (        OUT_USE(ILN:ILN+1) )
        CALL BLANK_TO_ZERO ( OUT_USE(ILN:ILN+1) )
        OUT_USE(ILN+2:)=':'
        ILN=ILEN(OUT_USE)+1
        CALL INCH   ( SEC,   OUT_USE(ILN:ILN+1) )
        CALL CHASHR (        OUT_USE(ILN:ILN+1) )
        CALL BLANK_TO_ZERO ( OUT_USE(ILN:ILN+1) )
        OUT_USE(ILN+2:)='.'
        ILN=ILEN(OUT_USE)+1
        CALL INCH   ( TICK,  OUT_USE(ILN:ILN+1) )
        CALL CHASHR (        OUT_USE(ILN:ILN+1) )
        CALL BLANK_TO_ZERO ( OUT_USE(ILN:ILN+1) )
!
        IB = ILEN(OUT_USE) + 4
        HOUR=TELP_USE/3600
        IMIN =(TELP_USE - HOUR*3600.0D0)/60
        SEC  =(TELP_USE - HOUR*3600.0D0 - IMIN*60.0D0)
        TICK =(TELP_USE - HOUR*3600.0D0 - IMIN*60.0D0 - SEC)*100
        IF ( TICK .GE. 100 ) TICK = 99
        OUT_USE(IB:) ='Elapsed time: '
        ILN=ILEN(OUT_USE)+2
        CALL INCH   ( HOUR,  OUT_USE(ILN:ILN+1) )
        CALL CHASHR (        OUT_USE(ILN:ILN+1) )
        CALL BLANK_TO_ZERO ( OUT_USE(ILN:ILN+1) )
        OUT_USE(ILN+2:)=':'
        ILN=ILEN(OUT_USE)+1
        CALL INCH   ( IMIN,  OUT_USE(ILN:ILN+1) )
        CALL CHASHR (        OUT_USE(ILN:ILN+1) )
        CALL BLANK_TO_ZERO ( OUT_USE(ILN:ILN+1) )
        OUT_USE(ILN+2:)=':'
        ILN=ILEN(OUT_USE)+1
        CALL INCH   ( SEC,   OUT_USE(ILN:ILN+1) )
        CALL CHASHR (        OUT_USE(ILN:ILN+1) )
        CALL BLANK_TO_ZERO ( OUT_USE(ILN:ILN+1) )
        OUT_USE(ILN+2:)='.'
        ILN=ILEN(OUT_USE)+1
        CALL INCH   ( TICK,  OUT_USE(ILN:ILN+1) )
        CALL CHASHR (        OUT_USE(ILN:ILN+1) )
        CALL BLANK_TO_ZERO ( OUT_USE(ILN:ILN+1) )
!
        IF ( IPRN_USE .EQ. 1  .OR.  IPRN_USE .EQ. 3 ) THEN
             WRITE ( 6, 110 ) OUT_USE(1:I_LEN(OUT_USE))
 110         FORMAT ( A )
             CALL FLUSH ( 6 )
        END IF
!
        IF ( IPRN_USE .EQ. 2  .OR.  IPRN_USE .EQ. 3 ) THEN
             IF ( PRESENT(TELP) .AND. LOC(TELP) .NE. 0 ) TELP = TELP_USE
             IF ( PRESENT(TELP) .AND. LOC(TCPU) .NE. 0 ) TCPU = TCPU_USE
          ELSE IF ( IPRN_USE  .EQ.  4 ) THEN
             IF ( PRESENT(OUT) .AND. LOC(OUT) .NE. 0 ) THEN
                  CALL CLRCH ( OUT )
                  OUT = OUT_USE
             END IF
        END IF
!
        RETURN
        END  !#!  TIM_TP  #!#
!
! ------------------------------------------------------------------------
!
        BLOCK DATA   BLKDAT_ADR_TIMER
        IMPLICIT   NONE 
        INTEGER*4  ISTATUS_TIMER
        REAL*8     TELP_TIMER, TCPU_TIMER
        COMMON   / ADR_TIMER / TELP_TIMER, TCPU_TIMER, ISTATUS_TIMER
        DATA ISTATUS_TIMER / 0 /
        END  !#!  BLKDAT_ADR_TIMER  #!#
