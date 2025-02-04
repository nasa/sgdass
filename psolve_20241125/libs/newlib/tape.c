/*
    This set of routines handles the controlling of the tape drive
    as well as the reading and writing of tapes for the Crustal Dynamics
    Database Catalog System.

    At the time of this writing, only one tape drive device can be open
    at any given time.

    Author: Linton Floyd  7/1/88
    Added DAT routines ---  Frank Gomez  9/1/92
*/

#ifdef DARWIN
int tp_read(buf,num) 	/* Fortran routines call this */
    short *num;
    char *buf;
{ return 0 ; }
#else

#include <fcntl.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/uio.h>

        char *strcpy();
	void perror();
	short is_open = 0, is_read = 0, is_write = 0;
	int tp_fd;
	char tape_dev[] = "/dev/rmt/0hn"; /* this is the default device */
/*
    "tp_open" opens the tape drive for reading or writing after a call to
    "tp_read" or "tp_write".  Whether or not the tape is repositioned after
     close depends on the particular drive selected.  The way that this
     currently works is that this routine attempts to open the tape drive
     with "read and write" privilege.  If there is an error on opening,
     perhaps due no ring in the tape, another attempt is made using
     read only privilege.  An error here will be returned to the caller.
*/
#ifdef _NEEDED
int tp_set_dev_(dev)
#else
int tp_set_dev(dev)
#endif
    char *dev;
{
    if(is_open)
	return(-1);		/* tape drive is already open => error return */
    else {
	strcpy(tape_dev,dev);
	return(0);
    }
}

#ifdef _NEEDED
int tp_open_() {
#else
int tp_open() {
#endif
    if(is_open) 
        return(0);
    else if((tp_fd = open(tape_dev,O_RDWR)) == -1) {
        if((tp_fd = open(tape_dev,O_RDONLY)) == -1) {
	    perror("open");	    	/* system fatal error handler */
	    return(-1);
	}
    }
    else 
	is_write = 1;
    is_read = 1;
    is_open = 1;
    return(0);
}
#ifdef _NEEDED
int tp_read_(buf,num) 	/* Fortran routines call this */
#else
int tp_read(buf,num) 	/* Fortran routines call this */
#endif
    short *num;
    char *buf;
{
    int iret;
    unsigned unum;
    if(!is_open)
	if ((iret = tp_open()) == -1)
	    return(iret);
    unum = *num;
    if((iret = read(tp_fd,buf,unum)) == -1)
	perror("read"); 
    return(iret);
}
#ifdef _NEEDED
int tp_rdi2_(i2buf,i2num) 	/* Fortran routines call this */
#else
int tp_rdi2(i2buf,i2num) 	/* Fortran routines call this */
#endif
    short *i2num;		/* i2num is the number of shorts in i2buf */
    short *i2buf;		/* i2buf is the input short buffer */
{
    union {
	short *i2;
	char *ch;
    } pt;
    int iret;
    unsigned unum;
    pt.i2 = i2buf;
    if(!is_open)
	if ((iret = tp_open()) == -1)
	    return(iret);
    unum = (*i2num)*2;		/* convert to number of characters */
    if((iret = read(tp_fd,pt.ch,unum)) == -1) {
	perror("read"); 
	return(iret);
    }				/* return number of words (rounding up for  */
    return((iret+1)/2);		/* an odd number of characters)		    */
}
#ifdef _NEEDED
int tp_write_(buf,num)  /* write a character string */
#else
int tp_write(buf,num)  /* write a character string */
#endif
    short *num;
    char *buf;
{
    int iret;
    unsigned unum;
    if(!is_open)
	if ((iret = tp_open()) == -1)
	    return(iret);
    if(is_open && !is_write)
	return(-1);
    unum = *num;
    if((iret = write(tp_fd,buf,unum)) == -1)
	perror("write"); 
    return(iret);
}
#ifdef _NEEDED
int tp_wrti2_(i2buf,i2num)  /* write an array of short integers */
#else
int tp_wrti2(i2buf,i2num)  /* write an array of short integers */
#endif
    short *i2num;
    short *i2buf;
{
    union {
	short *i2;
	char *ch;
    } pt;
    int iret;
    unsigned unum;
    pt.i2 = i2buf;
    if(!is_open)
	if ((iret = tp_open()) == -1)
	    return(iret);
    if(is_open && !is_write)
	return(-1);
    unum = (*i2num)*2;
    if((iret = write(tp_fd,pt.ch,unum)) == -1) {
	perror("write"); 
	return(iret);
    }
    return((iret+1)/2);
}
#ifdef _NEEDED
int tp_close_() {
#else
int tp_close() {
#endif
    int close();
    if(is_open) {
	if(close(tp_fd) < 0) {
	    perror("close");
	    return(-1);
	}
	else {
	    is_open = 0;
	    is_read = 0;
	    is_write = 0;
	}
    }
    return(0);
}

/*
   Magnetic tape operations; see mtio(7) and ioctl(2) to understand the
   following
*/

struct mtop tp_op;
struct mtget tp_get;

#ifdef _NEEDED
int tp_do_op_() {
#else
int tp_do_op() {
#endif
    int iret;
    if(!is_open) {	/* check if tape drive is open; if not, open it */
	if((iret = tp_open()) == -1)
	return(iret);
    }
    tp_op.mt_count = 1;
    if((iret = ioctl(tp_fd,MTIOCTOP,&tp_op)) == -1)  /* OK, do the operation.  */
			perror("ioctl");
    return(iret);
}

#ifdef _NEEDED
int tp_wr_eof_() {
#else
int tp_wr_eof() {
#endif
    tp_op.mt_op = MTWEOF;	/* write EOF */
    return(tp_do_op());
}

#ifdef _NEEDED
int tp_fwd_f_() {
#else
int tp_fwd_f() {
#endif
    tp_op.mt_op = MTFSF;	/* forward space file */
    return(tp_do_op());
}

#ifdef _NEEDED
int tp_bwd_f_() {
#else
int tp_bwd_f() {
#endif
    tp_op.mt_op = MTBSF;	/* backward space file */
    return(tp_do_op());
}

#ifdef _NEEDED
int tp_fwd_r_() {
#else
int tp_fwd_r() {
#endif
    tp_op.mt_op = MTFSR;	/* forward space record */
    return(tp_do_op());
}

#ifdef _NEEDED
int tp_bwd_r_() {
#else
int tp_bwd_r() {
#endif
    tp_op.mt_op = MTBSR;	/* backward space record */
    return(tp_do_op());
}

#ifdef _NEEDED
int tp_rew_() {
#else
int tp_rew() {
#endif
    tp_op.mt_op = MTREW;	/* rewind */
    return(tp_do_op());
}

#ifdef _NEEDED
int tp_rew_ofl_() {
#else
int tp_rew_ofl() {
#endif
    tp_op.mt_op = MTOFFL;	/* rewind, put tape offline */
    return(tp_do_op());
}

/*  This next section for DAT operations only  */
/***********************************************/
/***********************************************/
#ifdef _NEEDED
int tp_dat_do_op_() 
#else
int tp_dat_do_op() 
#endif
{
    int iret;
    if(!is_open) {	/* check if tape drive is open; if not, open it */
	if((iret = tp_open()) == -1)
	return(iret);
    }
    if((iret = ioctl(tp_fd,MTIOCTOP,&tp_op)) == -1)  /* OK, do the operation.  */
	perror("ioctl");
    return(iret);
}

#ifdef _NEEDED
int tp_seek_dat_eod_()
#else
int tp_seek_dat_eod()
#endif
{
#ifdef LINUX
  printf ( "newlib/tape MTEOD is not implemented" );
  exit (1);
#else
    tp_op.mt_op = MTEOD;	/* DAT only; seek to end-of-data */
    tp_op.mt_count = 1;
    return(tp_dat_do_op());
#endif
}

#ifdef _NEEDED
int tp_write_setmark_(num) 
#else
int tp_write_setmark(num) 
#endif
daddr_t *num;
{
#ifdef LINUX
  printf ( "newlib/tape MTWSS is not implemented" );
  exit (1);
#else
    tp_op.mt_op = MTWSS;	/* DAT only; write setmark(s) */
    tp_op.mt_count = *num;
    return(tp_dat_do_op());
#endif
}

#ifdef _NEEDED
int tp_forw_setmark_(num) 
#else
int tp_forw_setmark(num) 
#endif
daddr_t *num;
{
    tp_op.mt_op = MTFSS;	/* DAT only; space forward setmark(s) */
    tp_op.mt_count = *num;
    return(tp_dat_do_op());
}

#ifdef _NEEDED
int tp_back_setmark_(num) 
#else
int tp_back_setmark(num) 
#endif
daddr_t *num;
{
    tp_op.mt_op = MTBSS;	/* DAT only; space backward setmark(s) */
    tp_op.mt_count = *num;
    return(tp_dat_do_op());
}

#ifdef _NEEDED
int tp_saw_setmark_() {	/* setmark was just encountered */
#else
int tp_saw_setmark() {	/* setmark was just encountered */
#endif
    do_stat_get();
    return((GMT_SM(tp_get.mt_gstat)==0)? 0 : 1);
}
/***********************************************/
/***********************************************/

/*
   Magnetic tape status get, see mtio(7) and ioctl(2) for details of what the
   heck is going on here.
*/

#ifdef _NEEDED
void do_stat_get_() {
#else
  /* void do_stat_get() { */
do_stat_get() { 
#endif
    if(!is_open) {	/* check if tape drive is open; if not, open it */
	if(tp_open() == -1)
	return;
    }
    if(ioctl(tp_fd,MTIOCGET,&tp_get) == -1) perror("ioctl");
}
#ifdef _NEEDED
int tp_is_stream_() {	/* Is the tape drive a streamer? */
#else
int tp_is_stream() {	/* Is the tape drive a streamer? */
#endif
#ifdef LINUX
  printf ( "newlib/tape MT_ISSTREAM is not implemented" );
  exit (1);
#else
    do_stat_get();
    return(tp_get.mt_type == MT_ISSTREAM);
#endif
}

#ifdef _NEEDED
int tp_is_eof_()	{	/* Are we at EOF?   */
#else
int tp_is_eof()	{	/* Are we at EOF?   */
#endif
/*    int iret;  */
    do_stat_get();
    return((GMT_EOF(tp_get.mt_gstat)==0)? 0 : 1);
}

#ifdef _NEEDED
int tp_is_bot_()	{	/* Are we at Beginning Of Tape?   */
#else
int tp_is_bot()	{	/* Are we at Beginning Of Tape?   */
#endif
    do_stat_get();
    return((GMT_BOT(tp_get.mt_gstat)==0)? 0 : 1);
}

#ifdef _NEEDED
int tp_is_eot_()	{	/* Are we at End Of Tape?   */
#else
int tp_is_eot()	{	/* Are we at End Of Tape?   */
#endif
    do_stat_get();
    return((GMT_EOT(tp_get.mt_gstat)==0)? 0 : 1);
}

#ifdef _NEEDED
int tp_is_ring_() {	/* Is there a ring in the tape?   */
#else
int tp_is_ring() {	/* Is there a ring in the tape?   */
#endif
    do_stat_get();	/* (Is it not write protected?)	Note: 1 <---> 0 here */
    return((GMT_WR_PROT(tp_get.mt_gstat)==0)? 1 : 0);
}

#ifdef _NEEDED
int tp_is_online_() {	/* Is the tape online?   */
#else
int tp_is_online() {	/* Is the tape online?   */
#endif
    do_stat_get();
    return((GMT_ONLINE(tp_get.mt_gstat)==0)? 0 : 1);
}

#ifdef _NEEDED
int tp_is_6250_() {	/* Is the tape of density 6250 bpi (i.e. GCR)?   */
#else
int tp_is_6250() {	/* Is the tape of density 6250 bpi (i.e. GCR)?   */
#endif
    do_stat_get();
    return((GMT_D_6250(tp_get.mt_gstat)==0)? 0 : 1);
}

#ifdef _NEEDED
int tp_is_1600_() {	/* Is the tape of density 1600 bpi (i.e. PE)?   */
#else
int tp_is_1600() {	/* Is the tape of density 1600 bpi (i.e. PE)?   */
#endif
    do_stat_get();
    return((GMT_D_1600(tp_get.mt_gstat)==0)? 0 : 1);
}

#ifdef _NEEDED
int tp_is_800_() {	/* Is the tape of density 800 bpi (i.e. NRZI)?   */
#else
int tp_is_800() {	/* Is the tape of density 800 bpi (i.e. NRZI)?   */
#endif
    do_stat_get();
    return((GMT_D_800(tp_get.mt_gstat)==0)? 0 : 1);
}

#ifdef _NEEDED
int tp_door_open_() {	/* Is the tape door open?   */
#else
int tp_door_open() {	/* Is the tape door open?   */
#endif
    do_stat_get();
    return((GMT_DR_OPEN(tp_get.mt_gstat)==0)? 0 : 1);
}

#ifdef _NEEDED
int tp_im_rpt_() {	/* Is immediate reporting on?   */
#else
int tp_im_rpt() {	/* Is immediate reporting on?   */
#endif
    do_stat_get();
    return((GMT_IM_REP_EN(tp_get.mt_gstat)==0)? 0 : 1);
}
/* tp_is_open was written 12/30/94 by kdb for use in double-checking the
   tape handling of tar tapes.*/
#ifdef _NEEDED
int tp_is_open_() {	/* Is the tape drive currently open to the program?*/
#else
int tp_is_open() {	/* Is the tape drive currently open to the program? */
#endif
    return(is_open);
}
#endif
