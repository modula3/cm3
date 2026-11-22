#include <sys/types.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <sys/time.h>
 
#include <errno.h>
#include <termios.h>
#include <unistd.h>

/*
 * Make a pre-existing termios structure into "raw" mode: character-at-a-time
 * mode with no characters interpreted, 8-bit data path.
 */
void
cfmakeraw(t)
        struct termios *t;
{

        t->c_iflag &= ~(IMAXBEL|IXOFF|INPCK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON|IGNPAR);
        t->c_iflag |= IGNBRK;
        t->c_oflag &= ~OPOST;
        t->c_lflag &= ~(ECHO|ECHOE|ECHOK|ECHONL|ICANON|ISIG|IEXTEN|NOFLSH|TOSTOP);
        t->c_cflag &= ~(CSIZE|PARENB);
        t->c_cflag |= CS8|CREAD;
        t->c_cc[VMIN] = 1;
        t->c_cc[VTIME] = 0;
}
