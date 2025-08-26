/* $Id$ */

/* simple readline front-end with blocking communications in both directions */

#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/time.h> /* necessary on Darwin for struct timeval */
#include <sys/uio.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <stdio.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <errno.h>
#include <signal.h>

static int sig = 0;

static void 
handler(int s) { 
	sig = 1; 
}

static int
have_signal(void) { return sig; }

static void
clear_signal(void) { sig = 0; }

static void
install_int_handler(void) { signal(SIGINT, SIG_IGN); signal(SIGTTOU, SIG_IGN); signal(SIGTSTP, SIG_IGN); /*signal(SIGTTIN, SIG_IGN);*/ }

static void
deinstall_int_handler(void) { signal(SIGINT, NULL); }

/**********************************************************************/

void
fatal(const char *whatfailed) 
{
  printf("FATAL %s : errno=%d, %s\n", whatfailed, errno, strerror(errno)); 
  fflush(stdout);
  exit(-1);
}

int debug=0;

/**********************************************************************/

static char *line=NULL;

void
readline_callback(char *r_line)
{ 
  if (!r_line) exit(0); /* EOF */

  if (strlen(r_line)>0) add_history(r_line);

  line = r_line; 
  rl_callback_handler_remove();
}

struct inelem;
typedef struct inelem inelem;

struct inelem {
  char *data;
  inelem *next, *prev;
};

void 
mainloop2(int fd)
{
  /*FILE *in = fdopen(fd, "r");*/
  char *prompt=malloc(1);
  int doing_keyboard = 0;
  char *tcpdata=malloc(1);
  size_t p=0;
  inelem *inlist;

  /* sentinel */
  inlist = (inelem *)malloc(sizeof(inelem));
  inlist->next = inlist;
  inlist->prev = inlist;

  *prompt='\0';

  /*if (!in) fatal("fdopen");*/

  for(;;) {
    int selres;
    fd_set readfds;
    struct timeval timeo;

    timeo.tv_sec=0;
    timeo.tv_usec=100*1000;

    FD_ZERO(&readfds);
    FD_SET(fd, &readfds);
    if (doing_keyboard) FD_SET(0,  &readfds);

    if ((selres = select(fd+1, &readfds, NULL, NULL, &timeo))<0) {
      if (errno != EINTR)
        fatal("select");
    }

    if (have_signal())  clear_signal();

    if(FD_ISSET(0,&readfds) && doing_keyboard) {
      /* tell readline about it, if appropriate */
      rl_callback_read_char();

      /* if we got a full line, send it off */
      if (line) {
	char *l=line;

	if (debug) { printf("readline!\n"); fflush(stdout);}

	do { if(write(fd,l,1)<0)return; } while (*l++);

	free(line);
	
	line = NULL;

	doing_keyboard=0;
      }
    }
    
    if(FD_ISSET(fd, &readfds)) {
      /* read data off command channel and squirrel it away */
      /*int c=fgetc(in);*/
      char c;
      if (read(fd,&c,1)<1) return;

      /*if (c==EOF) return; */
      tcpdata[p++] = c;

      if (c)
	tcpdata = realloc(tcpdata,p+1);
      else {
	if (tcpdata[0]=='A') {
	  /* display immediately */
	  if(fputs(&tcpdata[1],stdout)==EOF) return;
	  fflush(stdout);
	  free(tcpdata);
	  tcpdata=(char *)malloc(1);
	  p = 0;
	} else {
	  /* cons on new elem */
	  inelem *new=(inelem *)malloc(sizeof(inelem));

	  new->data = tcpdata;

	  new->next = inlist->next;
	  new->prev = inlist;
	  new->next->prev = new;
	  inlist->next= new;
	}
	tcpdata=(char *)malloc(1);
	p = 0;
      }
    }

    if(!doing_keyboard) {
      /* taking input from counterparty */
      while (inlist->prev != inlist) {
	inelem *old = inlist->prev;
	inlist->prev = old->prev;
	old->prev->next = inlist;

	switch(old->data[0]) {
	case 'S':  /* handle SIGINT */
	  install_int_handler();
	  break;
        case 'I':  /* stop handling SIGINT */
	  deinstall_int_handler();
	  break;
	case 'N': 
	  rl_callback_handler_install(prompt,readline_callback);
          fflush(stdout);
	  doing_keyboard=1; 
	  break;
	case 'Q': 
	  return; 
	  break;
	case 'E': 
	  if(write(fd,"E",2)<0)return;
	  break;
	case 'P':
	  free(prompt);
	  prompt=strdup(&(old->data[1]));
	  break;
	case 'D':
	  if (fputs(&(old->data[1]),stdout)==EOF) return;
	  break;
	default:
	  fatal("fgetc");
	}
	free(old->data);
	free(old);
      }
    }
  }
}

int 
main(int argc, const char **argv) 
{
  int tgtport;
  int s;  
  struct sockaddr_in servaddr;

  if (argc < 2) fatal("argc");

  tgtport = atoi(argv[1]);

  if (argc >= 3 && !strcmp(argv[2],"-d")) debug=1;

  if (debug) { printf("%s pid=%d\n", argv[0], getpid()); }

  if((s = socket(PF_INET, SOCK_STREAM, 0))<0) fatal("socket");

  bzero(&servaddr, sizeof(servaddr));

  /* this stuff really isn't necessary */
  servaddr.sin_family      =  AF_INET;
  servaddr.sin_addr.s_addr =  htonl(INADDR_LOOPBACK);
  servaddr.sin_port        =  htons(tgtport);

  if(connect(s, (struct sockaddr *)&servaddr, sizeof(servaddr)) < 0)
     fatal("connect");

  mainloop2(s);
  exit(0);
}
