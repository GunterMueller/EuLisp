/// Copyright 1988 David Michael Betz
/// Copyright 1994 Russell Bradford
/// Copyright 2010, 2011 Henry G. Weller
///-----------------------------------------------------------------------------
//  This file is part of
/// ---                           EuLisp System 'EuXLisp'
///-----------------------------------------------------------------------------
//
//  EuXLisp is free software: you can redistribute it and/or modify it under the
//  terms of the GNU General Public License version 2 as published by the Free
//  Software Foundation.
//
//  EuXLisp is distributed in the hope that it will be useful, but WITHOUT ANY
//  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
//  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
//  details.
//
//  You should have received a copy of the GNU General Public License along with
//  this program.  If not, see <http://www.gnu.org/licenses/>.
//
///-----------------------------------------------------------------------------
/// Title: OS specific functions
///  Maintainer: Henry G. Weller
///-----------------------------------------------------------------------------

#include "euxlisp.h"
#include <errno.h>
#include <unistd.h>
#include <sys/times.h>
#include "euxlisp.h"

#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>

#include "../RLCompletion/eulisp_keywords.h"
#include "../RLCompletion/euxlisp_keywords.h"
#include "../RLCompletion/keyword_completion.h"
#endif

///-----------------------------------------------------------------------------
/// External variables
///-----------------------------------------------------------------------------
#include "euxlsymbols.h"
extern FILE *tfp;
extern int errno;

// Used in euxlisp.c
int reading;

// Line buffer size when not using readline
#define LBSIZE 200

///-----------------------------------------------------------------------------
/// Local variables
///-----------------------------------------------------------------------------
#ifdef READLINE
static char* lbuf;
static char rl_histfile[255];
#endif

static int lindex;
static int lcount;
static int lposition;

#define xputc putchar
#define MAXFDS                  32
#ifndef FD_SET
#define FD_SET(fd,fdset)        (fdset)->fds_bits[0] |= (1<<(fd))
#define FD_CLR(fd,fdset)        (fdset)->fds_bits[0] &= ~(1<<(fd))
#define FD_ZERO(fdset)          (fdset)->fds_bits[0] = 0
#define FD_ISSET(fd,fdset)      (((fdset)->fds_bits[0]) & (1<<(fd)))
typedef struct fd_set
{
    long fds_bits[32];
} fd_set;
#endif

///-----------------------------------------------------------------------------
/// Forward declarations
///-----------------------------------------------------------------------------
static void osflushn();
void ostputc(), ostputs(), oscheck(), osflush();
void check_if_disabled();

///-----------------------------------------------------------------------------
/// Functions
///-----------------------------------------------------------------------------
// main - the main function
int main(int argc, char *argv[])
{
    xlmain(argc, argv);
    return 0;
}

// osinit - initialize
void osinit(char *banner)
{
    extern int quiet;

    if (!quiet)
    {
        ostputs(banner);
        #ifdef SOCK
        ostputs("s");
        #endif
        ostputc('\n');
    }
    lposition = 0;
    lindex = 0;
    lcount = 0;

    #ifdef READLINE
    if (!quiet)
    {
        rl_attempted_completion_function = keyword_completion;
        rl_bind_key('\t', rl_complete);

        char* eulisp_history = "/.eulisp_history";
        char* home = getenv("HOME");
        if (home == NULL)
        {
            ostputs("Cannot find environment variable HOME for reading ~/");
            oserror(eulisp_history);
        }
        else
        {
            strcpy(rl_histfile, home);
            strcat(rl_histfile, eulisp_history);

            if (!read_history(rl_histfile))
            {
                ostputs("Reading readline history from ");
                ostputs(rl_histfile);
                ostputc('\n');
            }
        }
    }
    else
    #endif
    {
        lbuf = malloc(LBSIZE);
    }
}

// osfinish - clean up before returning to the operating system
void osfinish()
{
    extern int quiet;

    #ifdef READLINE
    if (quiet)
    #endif
    {
        free(lbuf);
    }
}

// oserror - print an error message
void oserror(char *msg)
{
    ostputs("error: ");
    ostputs(msg);
    ostputc('\n');
}

// osrand - return a random number between 0 and n-1
#ifdef DOBBS
int osrand(int n)
{
    // make sure we don't get stuck at zero
    if (rseed == 0L)
    {
        rseed = 1L;
    }

    // algorithm taken from Dr. Dobbs Journal, November 1985, page 91
    long k1 = rseed / 127773L;
    if ((rseed = 16807L * (rseed - k1 * 127773L) - k1 * 2836L) < 0L)
    {
        rseed += 2147483647L;
    }

    // return a random number between 0 and n-1
    return ((int)(rseed % (long)n));
}
#else
// Wichmann & Hill
int osrand(int n)
{
    static int x = 50, y = 100, z = 150;

    x = (171 * x) % 30269;
    y = (172 * y) % 30307;
    z = (170 * z) % 30323;

    double val = (x / 30269.0) + (y / 30307.0) + (z / 30323.0);
    val -= (int)val;

    return (int)(n * val);
}
#endif

// osaopen - open an ascii file
FILE *osaopen(char *name, char *mode)
{
    return (fopen(name, mode));
}

// osbopen - open a binary file
FILE *osbopen(char *name, char *mode)
{
    return (fopen(name, mode));
}

// osclose - close a file
int osclose(FILE *fp)
{
    return (fclose(fp));
}

// ostell - get the current file position
long ostell(FILE *fp)
{
    return (ftell(fp));
}

// osunlink - remove a file
int osunlink(char *path)
{
    return remove(path);
}

// osseek - set the current file position
int osseek(FILE *fp, long offset, int whence)
{
    return (fseek(fp, offset, whence));
}

// osagetc - get a character from an ascii file
int osagetc(FILE *fp)
{
    reading = 1;
    int ch = getc(fp);
    reading = 0;

    return ch;
}

// osaputc - put a character to an ascii file
int osaputc(int ch, FILE *fp)
{
    return (putc(ch, fp));
}

// osbgetc - get a character from a binary file
int osbgetc(FILE *fp)
{
    reading = 1;
    int ch = getc(fp);
    reading = 0;
    return ch;
}

// osbputc - put a character to a binary file
int osbputc(int ch, FILE *fp)
{
    return (putc(ch, fp));
}

// Read a string, and return a pointer to it.
// Returns NULL on EOF.
#ifdef READLINE
void rlgets()
{
    // If the buffer has already been allocated,
    // return the memory to the free pool.
    if (lbuf)
    {
        free(lbuf);
        lbuf = (char *)NULL;
    }

    // Make the prompt from the current module name
    static char prompt[255];
    int debug_depth = 0;

    euxlValue thread_module = get_module("thread");

    if (thread_module)
    {
        // Tell the debugger that readline is handling the prompt
        setvalue(xlenter_module("*debug-rl*", thread_module), true);

        // Get the debug-depth; 0 = no error
        debug_depth =
            getfixnum(getvalue(xlenter_module("*debug-depth*", thread_module)));
    }

    if (debug_depth)
    {
        sprintf
        (
            prompt,
            "[error%d] %s> ",
            debug_depth,
            getstring(getmname(current_module))
        );
    }
    else
    {
        sprintf
        (
            prompt,
            "%s> ",
            getstring(getmname(current_module))
        );
    }

    // Get a line from the user.
    lbuf = readline(prompt);

    // If the line has any text in it,
    // save it on the history.
    if (lbuf && *lbuf)
    {
        add_history(lbuf);
        write_history(rl_histfile);
    }
}
#endif

// ostgetc - get a character from the terminal
int ostgetc()
{
    extern int ctrl_c;
    extern int quiet;

    // Check for a buffered character
    if (lcount--)
    {
        #ifdef READLINE
        // If it is the last character return \n
        if (!quiet && lcount == 0)
        {
            return '\n';
        }
        else
        #endif
        {
            return (lbuf[lindex++]);
        }
    }

    reading = 1;

    #ifdef READLINE
    if (!quiet)
    {
        rlgets();
    }
    else
    #endif
    {
        fgets(lbuf, LBSIZE, stdin);
    }

    reading = 0;

    if (ctrl_c)
    {
        lcount = 0;
        ctrl_c = 0;
        osflush();
        ostputc('\n');
        xltoplevel();
    }

    // If the line buffer is not allocated or the stdin is at eof
    // return EOF
    #ifdef READLINE
    if ((!quiet && !lbuf) || feof(stdin))
    #else
    if (feof(stdin))
    #endif
    {
        if (!quiet)
        {
            ostputc('\n');
        }
        clearerr(stdin);
        lcount = 0;
        return EOF;
    }

    lcount = strlen(lbuf);

    // For readline pretend the buffer is one longer than it really is
    // for the \n which readline strips
    #ifdef READLINE
    if (!quiet)
    {
        lcount++;
    }
    #endif

    // write it to the transcript file
    if (tfp)
    {
        for (lindex = 0; lindex < lcount; ++lindex)
        {
            osaputc(lbuf[lindex], tfp);
        }
    }

    lindex = 0;
    lcount--;

    #ifdef READLINE
    // If it is the last character return \n
    if (!quiet && lcount == 0)
    {
        return '\n';
    }
    else
    #endif
    {
        return (lbuf[lindex++]);
    }
}

int osselect(FILE *fp)
{
    int ch, fd;
    fd_set readfds;
    struct timeval poll;

    fd = fileno(fp);
    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    poll.tv_sec = 0;
    poll.tv_usec = 0;

    if (select(MAXFDS, &readfds, NULL, NULL, &poll) < 0)
    {
        return NOCHAR;
    }

    if (FD_ISSET(fd, &readfds))
    {
        ch = getc(fp);
        ungetc(ch, fp);
    }
    else
    {
        ch = NOCHAR;
    }

    return ch;
}


// ospeekchar
int ospeekchar(FILE *fp)
{
    // grub around in the internals of stdio.h
    #ifdef __linux
    if (fp->_IO_read_ptr < fp->_IO_save_end)
    {
        return (int)(*(unsigned char *)fp->_IO_read_ptr);
    }
    #else
    #ifdef __BSD_NET2
    // for 386BSD and the like (Torek's stdio)
    if (fp->_r > 0)
    {
        return (int)*fp->_p;
    }
    #else
    // otherwise it generally looks like this
    if (fp->_cnt > 0)
    {
        return (int)*fp->_ptr;
    }
    #endif // linux
    #endif

    return osselect(fp);
}

// ostputc - put a character to the terminal
void ostputc(int ch)
{
    // check for control characters
    oscheck();

    // output the character
    if (ch == '\n')
    {
        xputc('\n');
        lposition = 0;
    }
    else
    {
        xputc(ch);
        lposition++;
    }

    // output the character to the transcript file
    if (tfp)
    {
        osaputc(ch, tfp);
    }
}

// ostputs - output a string to the terminal
void ostputs(char *str)
{
    while (*str != '\0')
    {
        ostputc(*str++);
    }
}

// osflush - flush the terminal input buffer
void osflush()
{
    lindex = lcount = lposition = 0;
}

// oscheck - check for control characters during execution
void oscheck()
{
    extern int ctrl_c;

    if (ctrl_c)
    {
        ctrl_c = 0;
        osflushn();
        xltoplevel();
    }
}

// oscheck_int - check for control characters during interpreting
void oscheck_int()
{
    extern int ctrl_c;

    if (ctrl_c)
    {
        ctrl_c = 0;
        osflushn();
        xltoplevel_int();
    }
}

// osflushn - flush the input line buffer and start a new line
static void osflushn()
{
    osflush();
    ostputc('\n');
}

void set_ticks_per_second()
{
    setvalue(xlenter("ticks-per-second"), cvfixnum(sysconf(_SC_CLK_TCK)));
}

#define TIMES_SIZE 3
#define TIMES_REAL(x) x->n_vdata[0]
#define TIMES_USER(x) x->n_vdata[1]
#define TIMES_SYS(x) x->n_vdata[2]

euxlValue x_cpu_time()
{
    static struct tms buffer;

    euxlValue res = newvector(TIMES_SIZE);

    FIXTYPE r = (FIXTYPE)times(&buffer);
    if (r == -1)
    {
        return NIL;
    }

    FIXTYPE u = (FIXTYPE)buffer.tms_utime;
    FIXTYPE s = (FIXTYPE)buffer.tms_stime;

    TIMES_REAL(res) = cvfixnum(r);
    TIMES_USER(res) = cvfixnum(u);
    TIMES_SYS(res) = cvfixnum(s);

    return res;
}

void check_if_disabled(char *name)
{
    extern int no_system;

    if (no_system)
    {
        xlcerror("function disabled", cvstring(name), NIL);
    }
}

// xsystem - execute a system command
euxlValue xsystem()
{
    static char *cfn_name = "system";
    char *cmd;

    check_if_disabled(cfn_name);

    cmd = (char *)getstring(xlgastring());
    xllastarg();

    return (system(cmd) == 0 ? true : cvfixnum((FIXTYPE) errno));
}

#if 0
// tmpfile - open a temporary file
euxlValue xtmpfile()
{
    static char *cfn_name = "tmpfile";

    xllastarg();

    FILE *fp = tmpfile();
    if (fp == NULL)
    {
        xlcerror("failed to create temporary file", cvstring(cfn_name), NIL);
    }

    return cvstream(fp, PF_INPUT | PF_OUTPUT);
}

#else

euxlValue xtmpfile()
{
    static char *cfn_name = "tmpfile";

    xllastarg();

    // tmpfile doesn't seem to work on dos gcc
    FILE *fp = tmpfile();
    if (fp == NULL)
    {
        xlcerror("failed to create temporary file", cvstring(cfn_name), NIL);
    }

    euxlValue stream = cvstream(fp, PF_INPUT | PF_OUTPUT);

    return stream;
}
#endif

// xgetenv - getenv
euxlValue xgetenv()
{
    static char *cfn_name = "getenv";
    extern char *getenv();

    euxlValue arg = xlgastring();
    xllastarg();

    char *str = getenv(getstring(arg));
    if (str == NULL)
    {
        return NIL;
    }

    return cvstring(str);
}

// xputenv - (putenv name val)
// some contortions to keep a chunk of string memory
euxlValue xputenv()
{
    static char *cfn_name = "putenv";
    extern int putenv();
    extern euxlValue s_supplied_env, xassoc();

    euxlValue name = xlgastring();
    euxlValue val = xlgastring();
    xllastarg();

    char buf[1024];
    sprintf(buf, "%s=%s", getstring(name), getstring(val));
    euxlValue new = cvstring(buf);

    int retval = putenv(getstring(new));
    if (retval > 0)
    {
        return NIL;
    }

    check(3);
    push(new);

    euxlValue env = getvalue(s_supplied_env);
    push(env);
    push(name);
    xlargc = 2;
    euxlValue old = xassoc();

    if (old)
    {
        rplacd(old, new);
    }
    else
    {
        env = cons(cons(name, new), env);
        setvalue(s_supplied_env, env);
    }

    drop(1);
    return new;
}

///-----------------------------------------------------------------------------
