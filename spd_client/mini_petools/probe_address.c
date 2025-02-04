#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

/************************************************************************/
/*                                                                      */
/*   Function probe_read_address ( address ) returns .TRUE. if the      */
/*   address is accessible for reading and .FALSE. if the address is    */
/*   not is accessible for reading. Analogously function                */
/*   probe_write_address ( address ) returns . TRUE. if the address is  */
/*   accessible for writing and .FALSE. if the address is not is        */
/*   accessible for writing.                                            */
/*                                                                      */
/*   The purpose of these functions is to check dangerous address in    */
/*   order to prevent abnormal termination of the process.              */
/*   Returned value is of Fortran LOGICAL*4 type. Its numerical value   */
/*   depends on Fortran compiler.                                       */
/*                                                                      */
/*   Example in Fortran:                                                */
/*                                                                      */
/*   LOGICAL*4  PROBE_READ_ADDRESS, PROBE_WRITE_ADDRESS                 */
/*   ...                                                                */
/*                                                                      */
/*   IF ( PROBE_WRITE_ADDRESS ( IVAR ) ) THEN                           */
/*        IVAR = 1                                                      */
/*      ELSE                                                            */
/*        WRITE ( 7, '(A,I)' ) 'Variable IVAR is not writable. '// &    */
/*  &                          'Address: ', LOC(IVAR)                   */
/*   END IF                                                             */
/*                                                                      */
/*   Example in C:                                                      */
/*                                                                      */
/*   if ( probe_read_address ( add ) == FORTRAN_FALSE )                 */
/*      {                                                               */
/*        printf ( "Address %d is not readable. Terminating \n", adr ); */
/*        exit ( 1 ) ;                                                  */
/*      }                                                               */ 
/*                                                                      */
/* ## 07-MAY-2004 probe_read_address  v1.0 (c) L. Petrov 07-MAY-2004 ## */
/* ## 07-MAY-2004 probe_write_address v1.0 (c) L. Petrov 07-MAY-2004 ## */
/*                                                                      */
/************************************************************************/

typedef int (*TestFunc)( void* );
static sigjmp_buf check_env;
static int        bSignal;
static int        savesig;


void Signal_Handler_check ( int sig )
{
  bSignal = 1;
  siglongjmp( check_env, sig );
}
int check_memory_acccess ( TestFunc func, void* p )
{
  int result;
/*                           */
/* --- Set Signal Handler    */
/*                           */
  bSignal = 0;
  if ( !sigsetjmp( check_env, savesig ) )
  {
        signal( SIGSEGV,        Signal_Handler_check );
        signal( SIGBUS,         Signal_Handler_check );
        result = func( p );
        signal( SIGSEGV,        SIG_DFL );
        signal( SIGBUS,         SIG_DFL );
  }
/*                           */
/* --- Check, whether we caught signal   */
/*                           */
  if ( bSignal )
        return FORTRAN_FALSE;
  else
        return FORTRAN_TRUE;
}

int GetAtAddress( void* p )
{
  return *((char*)p);
}

int SetAtAddress( void* p )
{
  int save_p;
  save_p = *((char*)p);
  return *((char*)p) = save_p;;
}

int probe_read_address ( void* p )
{
  int rd_acc;
  rd_acc = check_memory_acccess ( (TestFunc)GetAtAddress, p );
  return rd_acc;
}

int probe_write_address ( void* p )
{
  int rd_acc, wr_acc;
  int *save_p_value;
  
  wr_acc = check_memory_acccess ( (TestFunc)SetAtAddress, p );
  return wr_acc;
}
