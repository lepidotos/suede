#include <ctype.h>
#include <stdlib.h>
#include <signal.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "config.h"

/*  #ifdef HAVE_MMAP */
#if 0
#include <sys/mman.h>
#ifndef MAP_FAILED
#define MAP_FAILED ( (void *) -1 )
#endif
#endif

#include "mpg123.h"
#include "id3.h"
#include "id3_header.h"

/* max = 1728 */
#define MAXFRAMESIZE 1792

int tabsel_123[2][3][16] =
{
	{
    {0, 32, 64, 96, 128, 160, 192, 224, 256, 288, 320, 352, 384, 416, 448,},
       {0, 32, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384,},
       {0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320,}},

	{
       {0, 32, 48, 56, 64, 80, 96, 112, 128, 144, 160, 176, 192, 224, 256,},
	    {0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160,},
	    {0, 8, 16, 24, 32, 40, 48, 56, 64, 80, 96, 112, 128, 144, 160,}}
};

long mpg123_freqs[9] =
{44100, 48000, 32000, 22050, 24000, 16000, 11025, 12000, 8000};

struct bitstream_info bsi;

extern gint mpg123_bitrate, mpg123_frequency, mpg123_length;
extern gchar *mpg123_title, *mpg123_filename;
extern gboolean mpg123_stereo;

static int fsizeold = 0, ssize;
static unsigned char bsspace[2][MAXFRAMESIZE + 512];	/* MAXFRAMESIZE */
static unsigned char *bsbuf = bsspace[1], *bsbufold;
static int bsnum = 0;

unsigned char *mpg123_pcm_sample;
int mpg123_pcm_point = 0;

static FILE *filept;

static int get_fileinfo(void);

static int fullread(FILE * fd, unsigned char *buf, int count)
{
	int ret, cnt = 0;

	while (cnt < count)
	{
		ret = fread(buf + cnt, 1, count - cnt, fd);
		if (ret < 0)
			return ret;
		if (ret == 0)
			break;
		cnt += ret;
	}
	return cnt;
}

#define HDRCMPMASK 0xfffffd00

/*  #ifdef HAVE_MMAP */
#if 0

static unsigned char *mapbuf;
static unsigned char *mappnt;
static unsigned char *mapend;

static int stream_init(void)
{
	long len;

	len = get_fileinfo();
	if (len < 0)
		return -1;

	mappnt = mapbuf = mmap(NULL, len, PROT_READ, MAP_SHARED, filept, 0);
	if (!mapbuf || mapbuf == MAP_FAILED)
		return -1;

	mapend = mapbuf + len;

	return 0;
}

static void stream_rewind(void)
{
	mappnt = mapbuf;
}

void mpg123_stream_close (void)
{
	if (filept) {
		munmap(mapbuf, mapend - mapbuf);
		close (filept);
		filept = 0;
	}
}

static int stream_head_read(unsigned long *newhead)
{
	unsigned long nh;

	if (filept) {
		if (mappnt + 4 > mapend)
			return FALSE;

		nh = (*mappnt++) << 24;
		nh |= (*mappnt++) << 16;
		nh |= (*mappnt++) << 8;
		nh |= (*mappnt++);

		*newhead = nh;

	}
	else
	{
		unsigned char hbuf[4];

		if (fullread(filept, hbuf, 4) != 4)
			return FALSE;

		*newhead = ((unsigned long) hbuf[0] << 24) |
			((unsigned long) hbuf[1] << 16) |
			((unsigned long) hbuf[2] << 8) |
			(unsigned long) hbuf[3];
	}
	return TRUE;

}

static int stream_head_shift(unsigned long *head)
{

	if (filept) {
		if (mappnt + 1 > mapend)
			return FALSE;
		*head <<= 8;
		*head |= *mappnt++;
		*head &= 0xffffffff;
	}
	else
	{
		unsigned char hbuf;

		if (fullread(filept, &hbuf, 1) != 1)
			return 0;
		*head <<= 8;
		*head |= hbuf;
		*head &= 0xffffffff;
	}
	return TRUE;
}

static int stream_mpg123_read_frame_body(unsigned char *buf,
					 int size)
{
	if (filept) {
#if 1
		if (mappnt + size > mapend)
			return FALSE;
#else
		long l;

		if (size > (mapend - mappnt))
		{
			l = mapend - mappnt;
			memcpy(buf, mappnt, l);
			memset(buf + l, 0, size - l);
		}
		else
#endif
			memcpy(buf, mappnt, size);

		mappnt += size;
	}
	else
	{
		long l;

		if ((l = fullread(filept, buf, size)) != size)
		{
			if (l <= 0)
				return 0;
			memset(buf + l, 0, size - l);
		}
	}

	return TRUE;
}

static int stream_back_bytes(int bytes)
{
	if ((mappnt - bytes) < mapbuf || (mappnt - bytes + 4) > mapend)
		return -1;
	mappnt -= bytes;
	return 0;
}

void mpg123_stream_jump_to_frame(struct frame *fr, int frame)
{
	if (mapbuf + frame * (fr->framesize + 4) < mapend)
	{
		mpg123__init();
		stream_rewind();
		mpg123_read_frame(fr);
		mappnt = mapbuf + frame * (fr->framesize + 4);

		mpg123_read_frame(fr);
	}
}

void mpg123_stream_jump_to_byte(struct frame *fr, int byte)
{
	if (mapbuf + byte < mapend)
	{
		mappnt = mapbuf + byte;
		mpg123_read_frame(fr);
	}
}

int mpg123_stream_check_for_xing_header(struct frame *fr, XHEADDATA * xhead)
{
	unsigned char *head_data;
	int ret = 0;

	stream_back_bytes(fr->framesize + 4);

	if (mappnt + (fr->framesize + 4) < mapend)
	{
		ret = mpg123_get_xing_header(xhead, mappnt);
		mappnt += fr->framesize + 4;
	}
	return ret;
}

#else

static int stream_init(void)
{
	if (get_fileinfo() < 0)
		return -1;
	return 0;
}

void mpg123_stream_close(void)
{
	if (filept) {
		fclose (filept);
		filept = 0;
	}
}

/**************************************** 
 * HACK,HACK,HACK: step back <num> frames 
 * can only work if the 'stream' isn't a real stream but a file
 */

static int stream_head_read(unsigned long *newhead)
{
	unsigned char hbuf[4];

	if (fullread(filept, hbuf, 4) != 4)
		return FALSE;

	*newhead = ((unsigned long) hbuf[0] << 24) |
		((unsigned long) hbuf[1] << 16) |
		((unsigned long) hbuf[2] << 8) |
		(unsigned long) hbuf[3];

	return TRUE;
}

static int stream_head_shift(unsigned long *head)
{
	unsigned char hbuf;

	if (fullread(filept, &hbuf, 1) != 1)
		return 0;
	*head <<= 8;
	*head |= hbuf;
	*head &= 0xffffffff;
	return 1;
}

static int stream_mpg123_read_frame_body(unsigned char *buf,
					 int size)
{
	long l;

	if ((l = fullread(filept, buf, size)) != size)
	{
		if (l <= 0)
			return 0;
		memset(buf + l, 0, size - l);
	}
	return 1;
}

void mpg123_stream_jump_to_frame(struct frame *fr, int frame)
{
	mpg123_read_frame_init();
	fseek(filept, frame * (fr->framesize + 4), SEEK_SET);
	mpg123_read_frame(fr);
}

void mpg123_stream_jump_to_byte(struct frame *fr, int byte)
{
	fseek(filept, byte, SEEK_SET);
	mpg123_read_frame(fr);
}

int mpg123_stream_check_for_xing_header(struct frame *fr, XHEADDATA * xhead)
{
	unsigned char *head_data;
	int ret;

	fseek(filept, -(fr->framesize + 4), SEEK_CUR);
	head_data = malloc(fr->framesize + 4);
	fread(head_data, 1, fr->framesize + 4, filept);
	ret = mpg123_get_xing_header(xhead, head_data);
	free(head_data);
	return ret;
}

#endif

static int get_fileinfo(void)
{
	guchar buf[3];

	if (filept == NULL)
		return -1;
	if (fseek(filept, 0, SEEK_END) < 0)
		return -1;

	mpg123_info->filesize = ftell(filept);
	if (fseek(filept, -128, SEEK_END) < 0)
		return -1;
	if (fullread(filept, buf, 3) != 3)
		return -1;
	if (!strncmp(buf, "TAG", 3))
		mpg123_info->filesize -=128;
	if (fseek(filept, 0, SEEK_SET) < 0)
		return -1;

	if (mpg123_info->filesize <= 0)
		return -1;

	return mpg123_info->filesize;
}

void mpg123_read_frame_init(void)
{
	memset(bsspace[0],0,MAXFRAMESIZE + 512);
	memset(bsspace[1],0,MAXFRAMESIZE + 512);
	mpg123_info->output_audio = FALSE;
}

/*
 * Function read_id3v2_tag (head)
 *
 *    Read ID3v2 tag from stream.  Return TRUE upon success, or FALSE if
 *    an error occurred.
 *
 */
static gboolean read_id3v2_tag(unsigned long head)
{
	struct
	{
		char id3[3];
		struct id3_taghdr_t tag;
	} id3header;
	gchar *id3buf, *songname;
	int hdrsize;
	id3_t *id3d;
	struct id3tag_t tag;
	/*
	 * Read ID3tag header.
	 */
	*(unsigned long *) &id3header = g_htonl(head);
	if (fullread(filept, ((char *) &id3header) + sizeof (head),
		     sizeof (id3header) - sizeof (head))
	    != sizeof (id3header) - sizeof (head))
		return FALSE;

	hdrsize = ID3_GET_SIZE28(g_ntohl(id3header.tag.th_size));

	/*
	 * A invalid header could fool us into requesting insane
	 * amounts of memory.  Make sure the header size is
	 * reasonable.
	 */
	if ((mpg123_info->filesize && hdrsize > (int)mpg123_info->filesize) ||
	    (!mpg123_info->filesize && hdrsize > 1000000))
		return FALSE;

	if (mpg123_cfg.disable_id3v2)
	{
		guint8 *tmp = g_malloc(hdrsize);
		gboolean ret;
		ret = (fullread(filept, tmp, hdrsize) == hdrsize);
		g_free(tmp);
		return ret;
	}

	id3buf = g_malloc(hdrsize + sizeof (id3header));
	memcpy(id3buf, &id3header, sizeof (id3header));

	/*
	 * Read ID3tag body.
	 */
	if (fullread(filept, id3buf + sizeof (id3header),
		     hdrsize) != hdrsize)
	{
		g_free(id3buf);
		return FALSE;
	}

	/*
	 * Get info from tag.
	 */
	id3d = id3_open_mem(id3buf, 0);
	mpg123_get_id3v2(id3d, &tag);
	if(!mpg123_info->first_frame)
	{
		songname = mpg123_title;
		mpg123_title = mpg123_format_song_title(&tag, mpg123_filename);
		//mpg123_ip.set_info(mpg123_title, mpg123_length,
		//		   mpg123_bitrate * 1000,
		//		   mpg123_frequency, mpg123_stereo);
		if(songname)
			g_free(songname);
	}
	else
	{
		
		mpg123_title = mpg123_format_song_title(&tag, mpg123_filename);
	}
	id3_close(id3d);
	g_free(id3buf);

	return TRUE;
}

int mpg123_head_check(unsigned long head)
{
	if ((head & 0xffe00000) != 0xffe00000)
		return FALSE;
	if (!((head >> 17) & 3))
		return FALSE;
	if (((head >> 12) & 0xf) == 0xf)
		return FALSE;
	if (!((head >> 12) & 0xf))
		return FALSE;
	if (((head >> 10) & 0x3) == 0x3)
		return FALSE;
	if (((head >> 19) & 1) == 1 && ((head >> 17) & 3) == 3 && ((head >> 16) & 1) == 1)
		return FALSE;
	if ((head & 0xffff0000) == 0xfffe0000)
		return FALSE;
	
	return TRUE;
}

/*****************************************************************
 * read next frame
 */
int mpg123_read_frame(struct frame *fr)
{
	unsigned long newhead;

	fsizeold = fr->framesize;	/* for Layer3 */

	if (!stream_head_read(&newhead))
		return FALSE;

	if (!mpg123_head_check(newhead) || !mpg123_decode_header(fr, newhead))
	{
		int try = 0;

		do
		{
			try++;
			if ((newhead & 0xffffff00) == ('I' << 24) + ('D' << 16) + ('3' << 8))
			{
				read_id3v2_tag(newhead);
				if (!stream_head_read(&newhead))
					return FALSE;
			}
			else if (!stream_head_shift(&newhead))
				return 0;
			
		}
		while ((!mpg123_head_check(newhead) ||
			!mpg123_decode_header(fr,newhead)) &&
		       try < (256 * 1024));
		if(try >= (256 * 1024))
			return FALSE;
		
		mpg123_info->filesize -= try;
	}
	/* flip/init buffer for Layer 3 */
	bsbufold = bsbuf;
	bsbuf = bsspace[bsnum] + 512;
	bsnum = (bsnum + 1) & 1;

	if (!stream_mpg123_read_frame_body(bsbuf, fr->framesize))
		return 0;

	bsi.bitindex = 0;
	bsi.wordpointer = (unsigned char *) bsbuf;


	return 1;

}

/*
 * the code a header and write the information
 * into the frame structure
 */
int mpg123_decode_header(struct frame *fr, unsigned long newhead)
{
	if (newhead & (1 << 20))
	{
		fr->lsf = (newhead & (1 << 19)) ? 0x0 : 0x1;
		fr->mpeg25 = 0;
	}
	else
	{
		fr->lsf = 1;
		fr->mpeg25 = 1;
	}
	fr->lay = 4 - ((newhead >> 17) & 3);
	if (fr->mpeg25)
	{
		fr->sampling_frequency = 6 + ((newhead >> 10) & 0x3);
	}
	else
		fr->sampling_frequency = ((newhead >> 10) & 0x3) + (fr->lsf * 3);
	fr->error_protection = ((newhead >> 16) & 0x1) ^ 0x1;

	if (fr->mpeg25)		/* allow Bitrate change for 2.5 ... */
		fr->bitrate_index = ((newhead >> 12) & 0xf);

	fr->bitrate_index = ((newhead >> 12) & 0xf);
	fr->padding = ((newhead >> 9) & 0x1);
	fr->extension = ((newhead >> 8) & 0x1);
	fr->mode = ((newhead >> 6) & 0x3);
	fr->mode_ext = ((newhead >> 4) & 0x3);
	fr->copyright = ((newhead >> 3) & 0x1);
	fr->original = ((newhead >> 2) & 0x1);
	fr->emphasis = newhead & 0x3;

	fr->stereo = (fr->mode == MPG_MD_MONO) ? 1 : 2;

	ssize = 0;

	if (!fr->bitrate_index)
		return (0);

	switch (fr->lay)
	{
		case 1:
			fr->do_layer = mpg123_do_layer1;
			mpg123_init_layer2();	/* inits also shared tables with layer1 */
			fr->framesize = (long) tabsel_123[fr->lsf][0][fr->bitrate_index] * 12000;
			fr->framesize /= mpg123_freqs[fr->sampling_frequency];
			fr->framesize = ((fr->framesize + fr->padding) << 2) - 4;
			break;
		case 2:
			fr->do_layer = mpg123_do_layer2;
			mpg123_init_layer2();	/* inits also shared tables with layer1 */
			fr->framesize = (long) tabsel_123[fr->lsf][1][fr->bitrate_index] * 144000;
			fr->framesize /= mpg123_freqs[fr->sampling_frequency];
			fr->framesize += fr->padding - 4;
			break;
		case 3:
			fr->do_layer = mpg123_do_layer3;
			if (fr->lsf)
				ssize = (fr->stereo == 1) ? 9 : 17;
			else
				ssize = (fr->stereo == 1) ? 17 : 32;
			if (fr->error_protection)
				ssize += 2;
			fr->framesize = (long) tabsel_123[fr->lsf][2][fr->bitrate_index] * 144000;
			fr->framesize /= mpg123_freqs[fr->sampling_frequency] << (fr->lsf);
			fr->framesize = fr->framesize + fr->padding - 4;
			break;
		default:
			return (0);
	}
	if(fr->framesize > MAXFRAMESIZE)
		return 0;
	return 1;
}

void mpg123_open_stream(char *bs_filenam, int fd)
{
	if ((filept = fopen(bs_filenam, "rb")) != NULL) {
		if (stream_init() == -1) {
			mpg123_info->eof = 1;
		}
	} else {
		mpg123_info->eof = 1;
	}
}

void mpg123_set_pointer(long backstep)
{
	bsi.wordpointer = bsbuf + ssize - backstep;
	if (backstep)
		memcpy(bsi.wordpointer, bsbufold + fsizeold - backstep, backstep);
	bsi.bitindex = 0;
}

double mpg123_compute_bpf(struct frame *fr)
{
	double bpf;

	switch (fr->lay)
	{
		case 1:
			bpf = tabsel_123[fr->lsf][0][fr->bitrate_index];
			bpf *= 12000.0 * 4.0;
			bpf /= mpg123_freqs[fr->sampling_frequency] << (fr->lsf);
			break;
		case 2:
		case 3:
			bpf = tabsel_123[fr->lsf][fr->lay - 1][fr->bitrate_index];
			bpf *= 144000;
			bpf /= mpg123_freqs[fr->sampling_frequency] << (fr->lsf);
			break;
		default:
			bpf = 1.0;
	}

	return bpf;
}

int mpg123_calc_numframes(struct frame *fr)
{
	return (int) (mpg123_info->filesize / mpg123_compute_bpf(fr));
}
