/* Copyright (C) 1993, Digital Equipment Corporation                         */
/* All rights reserved.                                                      */
/* See the file COPYRIGHT for a full description.                            */

#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/fcntl.h>
#include <sys/dirent.h>

typedef struct
	{
	int	dd_fd;		/* file descriptor */
	int	dd_loc;	
	int	dd_size;
	char	*dd_buf;	
	struct	dirent **nlist;
	}	DIR;



void *malloc(size_t);


struct dirent *GetNextDir(buf,nbytes)
char buf[];
int	*nbytes;
{
	struct dirent *p,*d;
	
	p = (struct dirent *) buf;
	
	*nbytes = *nbytes - p->d_reclen;
	if(*nbytes == 0)
		return(NULL);

	d = (struct dirent *) &buf[p->d_reclen];
	return(d);
}

int Scandir(dirname, namelist)
	char *dirname;
	struct dirent *(*namelist[]);
{
	int	dir_fd;
	int	nbytes;
	int 	nitems,done;
	char	buf[8192];
	char *cp1, *cp2;
	struct stat stb;
	struct dirent *d, *p, **names,*bp;
	long arraysz;

	if ((dir_fd = open(dirname,O_RDONLY))== -1)
		return(-1);
	if (fstat(dir_fd, &stb) < 0)
		return(-1);

	/*
	 * estimate the array size by taking the size of the directory file
	 * and dividing it by a multiple of the minimum size entry. 
	 */
	arraysz = (stb.st_size / 24);
	names = (struct dirent **)malloc(arraysz * sizeof(struct dirent *));
	if (names == NULL)
		return(-1);

	nitems = 0;
	d = (struct dirent *) buf;
	while ((nbytes = getdents(dir_fd, d, 8192)) > 0) 
	{

		while(d != NULL)
		{
			p = (struct dirent *)malloc(d->d_reclen + 1);
			if (p == NULL)
				return(-1);
			p->d_ino = d->d_ino;
			p->d_off = d->d_off;
			p->d_reclen = d->d_reclen;
			for (cp1 = p->d_name, cp2 = d->d_name; 
				*cp1++ = *cp2++; );
			/*
		 	* Check to make sure the array has space left and
		 	* realloc the maximum size.
		 	*/
			if (++nitems >= arraysz) 
			{

				/* just might have grown */
				if (fstat(dir_fd, &stb) < 0)
					return(-1);	
				arraysz = stb.st_size / 12;
				names = (struct dirent **)realloc((char *)names,
					arraysz * sizeof(struct dirent *));
				if (names == NULL)
					return(-1);
			}
			names[nitems-1] = p;
			d = GetNextDir(d,&nbytes);
		}	
		d = (struct dirent *) buf;
	}
	close(dir_fd);
	*namelist = names;
	return(nitems);
}


struct dirent *readdir(dirp)
DIR *dirp;
{
	if((dirp->dd_loc + 1) >= dirp->dd_size)
		return(NULL);
	else
	{
		dirp->dd_loc += 1;
		return(dirp->nlist[dirp->dd_loc - 1]);
	}
}

DIR *opendir(dir)
const char *dir;
{
	int cnt;
	int fd;
	DIR *junk = NULL;
	struct	dirent **nlist;

	if((cnt = Scandir(dir, &nlist)) > 0)
		if((junk = (DIR *) malloc(sizeof(DIR))) != NULL)
		{
			junk->dd_fd = fd;
			junk->dd_loc = 0;
			junk->dd_size = cnt;
			junk->nlist = nlist;
		}

	return(junk);
}


long telldir(dirp)
DIR *dirp;
{
	return(dirp->dd_loc);
}

void seekdir(dirp,loc)
DIR *dirp;
long loc;
{
	dirp->dd_loc = loc;
}

void rewinddir(dirp)
DIR *dirp;
{
	dirp->dd_loc = 0L;
}

int  closedir(dirp)
DIR *dirp;
{
	long i;

  	for (i=0; i < dirp->dd_size; i++)
    		free((void *) dirp->nlist[i]);

  	free((void *) dirp->nlist);

	return(0);
}



