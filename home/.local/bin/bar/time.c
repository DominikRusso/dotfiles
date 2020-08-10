#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#define BUFLEN 25

/**
 * prints ISO 8601 like datetime as soon as second changes
 *
 * the extra hundredth of a second is to make sure that
 * time() is definitely called after the second changes
 *
 * no error handling for the sake of efficiency
 */
int
main()
{
	time_t rawtime;
	struct tm *ptm;
	char buf[BUFLEN];
	struct timeval tv;
	struct timespec ts = {0, 0};

	while (1) {
		rawtime = time(NULL);
		ptm = localtime(&rawtime);
		strftime(buf, BUFLEN, "%a %F %T", ptm);
		printf(buf);
		fflush(stdout);
		gettimeofday(&tv, NULL);
		ts.tv_nsec = 1010000000 - tv.tv_usec * 1000;
		nanosleep(&ts, NULL);
	}
}

