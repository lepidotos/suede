/*
 * gen_uuid.c --- generate a DCE-compatible uuid
 *
 * Copyright (C) 1996, 1997 Theodore Ts'o.
 *
 * %Begin-Header%
 * This file may be redistributed under the terms of the GNU Public
 * License.
 * %End-Header%
 */

#include <config.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#ifdef HAVE_SYS_SOCKIO_H
#include <sys/sockio.h>
#endif
#ifdef HAVE_NET_IF_H
#include <net/if.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "uuidP.h"

#ifdef HAVE_SRANDOM
#define srand(x) 	srandom(x)
#define rand() 		random()
#endif

/*
 * Generate a series of random bytes.  Use /dev/urandom if possible,
 * and if not, use srandom/random.
 */
static void get_random_bytes(void *buf, int nbytes)
{
	static int fd = -2;
	int i;
	char *cp = (char *) buf;

	if (fd == -2) {
		fd = open("/dev/urandom", O_RDONLY);
		srand((getpid() << 16) ^ getuid() ^ time(0));
	}
	if (fd >= 0) {
		while (nbytes > 0) {
			i = read(fd, cp, nbytes);
			if (i < 0) {
				if ((errno == EINTR) || (errno == EAGAIN))
					continue;
				break;
			}
			nbytes -= i;
			cp += i;
		}
	}
	if (nbytes == 0)
		return;

	/* XXX put something better here if no /dev/random! */
	for (i=0; i < nbytes; i++)
		*cp++ = rand() & 0xFF;
	return;
	
}

/*
 * Get the ethernet hardware address, if we can find it...
 */
static int get_node_id(unsigned char *node_id)
{
#ifdef HAVE_NET_IF_H
	int 		sd;
	struct ifreq 	ifr, *ifrp;
	struct ifconf 	ifc;
	char buf[1024];
	int		n, i;
	unsigned char 	*a;
	
/*
 * BSD 4.4 defines the size of an ifreq to be
 * max(sizeof(ifreq), sizeof(ifreq.ifr_name)+ifreq.ifr_addr.sa_len
 * However, under earlier systems, sa_len isn't present, so the size is 
 * just sizeof(struct ifreq)
 */
#ifdef HAVE_SA_LEN
#ifndef max
#define max(a,b) ((a) > (b) ? (a) : (b))
#endif
#define ifreq_size(i) max(sizeof(struct ifreq),\
     sizeof((i).ifr_name)+(i).ifr_addr.sa_len)
#else
#define ifreq_size(i) sizeof(struct ifreq)
#endif /* HAVE_SA_LEN*/

	sd = socket(AF_INET, SOCK_DGRAM, IPPROTO_IP);
	if (sd < 0) {
		return -1;
	}
	memset(buf, 0, sizeof(buf));
	ifc.ifc_len = sizeof(buf);
	ifc.ifc_buf = buf;
	if (ioctl (sd, SIOCGIFCONF, (char *)&ifc) < 0) {
		close(sd);
		return -1;
	}
	n = ifc.ifc_len;
	for (i = 0; i < n; i+= ifreq_size(*ifr) ) {
		ifrp = (struct ifreq *)((char *) ifc.ifc_buf+i);
		strncpy(ifr.ifr_name, ifrp->ifr_name, IFNAMSIZ);
#ifdef SIOCGIFHWADDR
		if (ioctl(sd, SIOCGIFHWADDR, &ifr) < 0)
			continue;
		a = (unsigned char *) &ifr.ifr_hwaddr.sa_data;
#else
#ifdef SIOCGENADDR
		if (ioctl(sd, SIOCGENADDR, &ifr) < 0)
			continue;
		a = (unsigned char *) ifr.ifr_enaddr;
#else
		/*
		 * XXX we don't have a way of getting the hardware
		 * address
		 */
		close(sd);
		return 0;
#endif /* SIOCGENADDR */
#endif /* SIOCGIFHWADDR */
		if (!a[0] && !a[1] && !a[2] && !a[3] && !a[4] && !a[5])
			continue;
		if (node_id) {
			memcpy(node_id, a, 6);
			close(sd);
			return 1;
		}
	}
	close(sd);
#endif
	return 0;
}

/* Assume that the gettimeofday() has microsecond granularity */
#define MAX_ADJUSTMENT 10

static int get_clock(guint32 *clock_high, guint32 *clock_low, guint16 *ret_clock_seq)
{
	static int			adjustment = 0;
	static struct timeval		last = {0, 0};
	static guint16			clock_seq;
	struct timeval 			tv;
	unsigned long long		clock_reg;
	
try_again:
	gettimeofday(&tv, 0);
	if ((last.tv_sec == 0) && (last.tv_usec == 0)) {
		get_random_bytes(&clock_seq, sizeof(clock_seq));
		clock_seq &= 0x1FFF;
		last = tv;
		last.tv_sec--;
	}
	if ((tv.tv_sec < last.tv_sec) ||
	    ((tv.tv_sec == last.tv_sec) &&
	     (tv.tv_usec < last.tv_usec))) {
		clock_seq = (clock_seq+1) & 0x1FFF;
		adjustment = 0;
	} else if ((tv.tv_sec == last.tv_sec) &&
	    (tv.tv_usec == last.tv_usec)) {
		if (adjustment >= MAX_ADJUSTMENT)
			goto try_again;
		adjustment++;
	} else
		adjustment = 0;
	
	clock_reg = tv.tv_usec*10 + adjustment;
	clock_reg += ((unsigned long long) tv.tv_sec)*10000000;
	clock_reg += (((unsigned long long) 0x01B21DD2) << 32) + 0x13814000;

	*clock_high = clock_reg >> 32;
	*clock_low = clock_reg;
	*ret_clock_seq = clock_seq;
	return 0;
}

void uuid_generate_time(uuid_t out)
{
	static unsigned char node_id[6];
	static int has_init = 0;
	struct uuid uu;
	guint32	clock_mid;

	if (!has_init) {
		if (get_node_id(node_id) <= 0) {
			get_random_bytes(node_id, 6);
			/*
			 * Set multicast bit, to prevent conflicts
			 * with IEEE 802 addresses obtained from
			 * network cards
			 */
			node_id[0] |= 0x80;
		}
		has_init = 1;
	}
	get_clock(&clock_mid, &uu.time_low, &uu.clock_seq);
	uu.clock_seq |= 0x8000;
	uu.time_mid = (guint16) clock_mid;
	uu.time_hi_and_version = (clock_mid >> 16) | 0x1000;
	memcpy(uu.node, node_id, 6);
	uuid_pack(&uu, out);
}

void uuid_generate_random(uuid_t out)
{
	uuid_t	buf;
	struct uuid uu;

	get_random_bytes(buf, sizeof(buf));
	uuid_unpack(buf, &uu);

	uu.clock_seq = (uu.clock_seq & 0x3FFF) | 0x8000;
	uu.time_hi_and_version = (uu.time_hi_and_version & 0x0FFF) | 0x4000;
	uuid_pack(&uu, out);
}

/*
 * This is the generic front-end to uuid_generate_random and
 * uuid_generate_time.  It uses uuid_generate_random only if
 * /dev/urandom is available, since otherwise we won't have
 * high-quality randomness.
 */
void uuid_generate(uuid_t out)
{
	static int	has_random = -1;

	if (has_random < 0) {
		if (access("/dev/urandom", R_OK) == 0)
			has_random = 1;
		else
			has_random = 0;
	}
	if (has_random)
		uuid_generate_random(out);
	else
		uuid_generate_time(out);
}

