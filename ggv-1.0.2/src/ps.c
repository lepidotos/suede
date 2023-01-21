/* -*- Mode: C; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 8 -*- */
/*
 * ps.c -- Postscript scanning and copying routines.
 * Copyright (C) 1992, 1998  Timothy O. Theisen
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *   Author: Tim Theisen           Systems Programmer
 * Internet: tim@cs.wisc.edu       Department of Computer Sciences
 *     UUCP: uwvax!tim             University of Wisconsin-Madison
 *    Phone: (608)262-0438         1210 West Dayton Street
 *      FAX: (608)262-9777         Madison, WI   53706
 */

/* 18/3/98 Jake Hamby patch */

/*
 * 98/03/17: Jake Hamby (jehamby@lightside.com):
 * Added support for compressed/gzipped Postscript and PDF files.
 * Compressed files are gunzipped to a temporary file, and PDF files are
 * scanned by calling Ghostscript to generate a fake DSC file.
 * This is based on code from GV 3.5.8, which is available at:
 *    http://wwwthep.physik.uni-mainz.de/~plass/gv/
 */
 
/* GV by 	Johannes Plass
 *			Department of Physics
 *			Johannes Gutenberg University
 *			Mainz, Germany
 *		
 *			<plass@thep.physik.uni-mainz.de>
 */
 
/* end of patch */

#include <stdlib.h>
#include <stdio.h>
#ifndef SEEK_SET
#define SEEK_SET 0
#endif
#ifndef BUFSIZ
#define BUFSIZ 1024
#endif
#include <ctype.h>
#include <X11/Xos.h>		/* #includes the appropriate <string.h> */
#include "ps.h"

#ifdef BSD4_2
#define memset(a,b,c) bzero(a,c)
#endif

/* length calculates string length at compile time */
/* can only be used with character constants */
#define length(a) (sizeof(a)-1)
#define iscomment(a, b)	(strncmp(a, b, length(b)) == 0)
#define DSCcomment(a) (a[0] == '%' && a[1] == '%')

	/* list of standard paper sizes from Adobe's PPD. */

struct documentmedia papersizes[PAPER_SIZE_COUNT+1] = {
    {"BBox",	         0,  0},
    {"Letter",		 612,  792,},
    {"Tabloid",		 792, 1224,},
    {"Ledger",		1224,  792,},
    {"Legal",		 612, 1008,},
    {"Statement",	 396,  612,},
    {"Executive",	 540,  720,},
    {"A3",		 842, 1190,},
    {"A4",		 595,  842,},
    {"A5",		 420,  595,},
    {"B4",		 729, 1032,},
    {"B5",		 516,  729,},
    {"Folio",		 612,  936,},
    {"Quarto",		 610,  780,},
    {"10x14",		 720, 1008,},
    {NULL,		   0,    0}
};

#if NeedFunctionPrototypes
static char *readline(char *line, int size, FILE *fp,
		      long *position, unsigned int *line_len);
static char *gettextline(char *line);
static char *get_next_text(char *line, char **next_char);
static int blank(char *line);
#else
static char *readline();
static char *gettextline();
static char *get_next_text();
static int  blank();
#endif
static char    *skipped_line = "% ps_io_fgetchars: skipped line";


/*
 *	psscan -- scan the PostScript file for document structuring comments.
 *
 *	This scanner is designed to retrieve the information necessary for
 *	the ghostview previewer.  It will scan files that conform to any
 *	version (1.0, 2.0, 2.1, or 3.0) of the document structuring conventions.
 *	It does not really care which version of comments the file contains.
 *	(The comments are largely upward compatible.)  It will scan a number
 *	of non-conforming documents.  (You could have part of the document
 *	conform to V2.0 and the rest conform to V3.0.  It would be similar
 *	to the DC-2 1/2+, it would look funny but it can still fly.)
 *
 *	This routine returns a pointer to the document structure.
 *	The structure contains the information relevant to previewing.
 *      These include EPSF flag (to tell if the file is a encapsulated figure),
 *      Page Media (for the Page Size), Bounding Box (to minimize backing
 *      pixmap size or determine window size for encapsulated PostScript), 
 *      Orientation of Paper (for default transformation matrix), and
 *      Page Order.  The title and CreationDate are also retrieved to
 *      help identify the document.
 *
 *      The following comments are examined:
 *
 *      Header section: 
 *      Must start with %!PS-Adobe-.  Version numbers ignored.
 *      Also allowed to be just %!PS, many files seem to have that.
 *
 *      %!PS-Adobe-* [EPSF-*]
 *      %%BoundingBox: <int> <int> <int> <int>|(atend)
 *      %%CreationDate: <textline>
 *      %%Orientation: Portrait|Landscape|(atend)
 *      %%Pages: <uint> [<int>]|(atend)
 *      %%PageOrder: Ascend|Descend|Special|(atend)
 *      %%Title: <textline>
 *      %%DocumentMedia: <text> <real> <real> <real> <text> <text>
 *      %%DocumentPaperSizes: <text>
 *      %%EndComments
 *
 *      Note: Either the 3.0 or 2.0 syntax for %%Pages is accepted.
 *            Also either the 2.0 %%DocumentPaperSizes or the 3.0
 *            %%DocumentMedia comments are accepted as well.
 *
 *      The header section ends either explicitly with %%EndComments or
 *      implicitly with any line that does not begin with %X where X is
 *      a not whitespace character.
 *
 *      If the file is encapsulated PostScript the optional Preview section
 *      is next:
 *
 *      %%BeginPreview
 *      %%EndPreview
 *
 *      This section explicitly begins and ends with the above comments.
 *
 *      Next the Defaults section for version 3 page defaults:
 *
 *      %%BeginDefaults
 *      %%PageBoundingBox: <int> <int> <int> <int>
 *      %%PageOrientation: Portrait|Landscape
 *      %%PageMedia: <text>
 *      %%EndDefaults
 *
 *      This section explicitly begins and ends with the above comments.
 *
 *      The prolog section either explicitly starts with %%BeginProlog or
 *      implicitly with any nonblank line.
 *
 *      %%BeginProlog
 *      %%EndProlog
 *
 *      The Prolog should end with %%EndProlog, however the proglog implicitly
 *      ends when %%BeginSetup, %%Page, %%Trailer or %%EOF are encountered.
 *
 *      The Setup section is where the version 2 page defaults are found.
 *      This section either explicitly begins with %%BeginSetup or implicitly
 *      with any nonblank line after the Prolog.
 *
 *      %%BeginSetup
 *      %%PageBoundingBox: <int> <int> <int> <int>
 *      %%PageOrientation: Portrait|Landscape
 *      %%PaperSize: <text>
 *      %%EndSetup
 *
 *      The Setup should end with %%EndSetup, however the setup implicitly
 *      ends when %%Page, %%Trailer or %%EOF are encountered.
 *
 *      Next each page starts explicitly with %%Page and ends implicitly with
 *      %%Page or %%Trailer or %%EOF.  The following comments are recognized:
 *
 *      %%Page: <text> <uint>
 *      %%PageBoundingBox: <int> <int> <int> <int>|(atend)
 *      %%PageOrientation: Portrait|Landscape
 *      %%PageMedia: <text>
 *      %%PaperSize: <text>
 *
 *      The tralier section start explicitly with %%Trailer and end with %%EOF.
 *      The following comment are examined with the proper (atend) notation
 *      was used in the header:
 *
 *      %%Trailer
 *      %%BoundingBox: <int> <int> <int> <int>|(atend)
 *      %%Orientation: Portrait|Landscape|(atend)
 *      %%Pages: <uint> [<int>]|(atend)
 *      %%PageOrder: Ascend|Descend|Special|(atend)
 *      %%EOF
 *
 *
 *  + A DC-3 received severe damage to one of its wings.  The wing was a total
 *    loss.  There was no replacement readily available, so the mechanic
 *    installed a wing from a DC-2.
 */
 
#include <glib.h>


struct document *psscan (FILE *file, int respect_eof)
{
    struct document *doc;
    int bb_set = NONE;
    int pages_set = NONE;
    int page_order_set = NONE;
    int orientation_set = NONE;
    int page_bb_set = NONE;
    int page_media_set = NONE;
    int preread;		/* flag which tells the readline isn't needed */
    int i;
    unsigned int maxpages = 0;
    unsigned int nextpage = 1;	/* Next expected page */
    unsigned int thispage;
    int ignore = 0;		/* whether to ignore page ordinals */
    char *label;
    char line[PSLINELENGTH];	/* 255 characters + 1 newline + 1 NULL */
    char text[PSLINELENGTH];	/* Temporary storage for text */
    long position;		/* Position of the current line */
    long beginsection;		/* Position of the beginning of the section */
    unsigned int line_len; 	/* Length of the current line */
    unsigned int section_len;	/* Place to accumulate the section length */
    char *next_char;		/* 1st char after text returned by get_next_text() */
    char *cp;
    struct documentmedia *dmp;

    if (!file)
        return NULL;

            rewind(file);

    if (!readline (line, sizeof line, file, &position, &line_len)) {
	g_print ("psscan: empty input file.\n");
	return(NULL);
    }

    /* HP printer job language data follows. Some printer drivers add pjl
     * commands to switch a pjl printer to postscript mode. If no PS header
     * follows, this seems to be a real pjl file. */
    if (iscomment(line, "\033%-12345X@PJL")) {
	/* read until first DSC comment */
	while (readline (line, sizeof line, file, &position, &line_len)
	       && (line[0] != '%')) ;
        if (line[0] != '%') {
	    g_print ("psscan error: input files seems to be a PJL file.\n");
	    return(NULL);
	}
    }

    /* Header comments */

    /* Header should start with "%!PS-Adobe-", but some programms omit
     * parts of this or add a ^D at the beginning. */
    if (iscomment (line,"%!PS") || iscomment (line, "\004%!PS")) {
	doc = g_new0 (struct document, 1);

        /* ignore possible leading ^D */
	if (*line == '\004') {
	    position++;
	    line_len--;
	}

/* Jake Hamby patch 18/3/98 */

	sscanf(line, "%*s %s", text);
	/*doc->epsf = iscomment(text, "EPSF-");*/
	doc->epsf = iscomment(text, "EPSF"); /* Hamby - This line changed */
	doc->beginheader = position;
	section_len = line_len;
    } else {
      /* There are postscript documents that do not have
	 %PS at the beginning, usually unstructured. We should GS decide
	 For instance, the tech reports at this university:

	 http://svrc.it.uq.edu.au/Bibliography/svrc-tr.html?94-45

	 add ugly PostScript before the actual document. 

	 GS and gv is
	 able to display them correctly as unstructured PS.

	 In a way, this makes sense, a program PostScript does not need
	 the !PS at the beginning.

      */
	doc = g_new0 (struct document, 1);
	return(doc);
    }

    preread = 0;
    while (preread || readline(line, sizeof line, file, &position, &line_len)) {
	if (!preread) section_len += line_len;
	preread = 0;
	if (line[0] != '%' ||
	    iscomment(line+1, "%EndComments") ||
	    line[1] == ' ' || line[1] == '\t' || line[1] == '\n' ||
	    !isprint(line[1])) {
	    break;
	} else if (line[1] != '%') {
	    /* Do nothing */
	} else if (doc->title == NULL && iscomment(line+2, "Title:")) {
	    doc->title = gettextline(line+length("%%Title:"));
	} else if (doc->date == NULL && iscomment(line+2, "CreationDate:")) {
	    doc->date = gettextline(line+length("%%CreationDate:"));
	} else if (bb_set == NONE && iscomment(line+2, "BoundingBox:")) {
	    sscanf(line+length("%%BoundingBox:"), "%s", text);
	    if (strcmp(text, "(atend)") == 0) {
		bb_set = ATEND;
	    } else {
		if (sscanf(line+length("%%BoundingBox:"), "%d %d %d %d",
			   &(doc->boundingbox[LLX]),
			   &(doc->boundingbox[LLY]),
			   &(doc->boundingbox[URX]),
			   &(doc->boundingbox[URY])) == 4)
		    bb_set = 1;
		else {
		    float fllx, flly, furx, fury;
		    if (sscanf(line+length("%%BoundingBox:"), "%f %f %f %f",
			       &fllx, &flly, &furx, &fury) == 4) {
			bb_set = 1;
			doc->boundingbox[LLX] = fllx;
			doc->boundingbox[LLY] = flly;
			doc->boundingbox[URX] = furx;
			doc->boundingbox[URY] = fury;
			if (fllx < doc->boundingbox[LLX])
			    doc->boundingbox[LLX]--;
			if (flly < doc->boundingbox[LLY])
			    doc->boundingbox[LLY]--;
			if (furx > doc->boundingbox[URX])
			    doc->boundingbox[URX]++;
			if (fury > doc->boundingbox[URY])
			    doc->boundingbox[URY]++;
		    }
		}
	    }
	} else if (orientation_set == NONE &&
		   iscomment(line+2, "Orientation:")) {
	    sscanf(line+length("%%Orientation:"), "%s", text);
	    if (strcmp(text, "(atend)") == 0) {
		orientation_set = ATEND;
	    } else if (strcmp(text, "Portrait") == 0) {
		doc->orientation = PORTRAIT;
		orientation_set = 1;
	    } else if (strcmp(text, "Landscape") == 0) {
		doc->orientation = LANDSCAPE;
		orientation_set = 1;
	    }
	} else if (page_order_set == NONE && iscomment(line+2, "PageOrder:")) {
	    sscanf(line+length("%%PageOrder:"), "%s", text);
	    if (strcmp(text, "(atend)") == 0) {
		page_order_set = ATEND;
	    } else if (strcmp(text, "Ascend") == 0) {
		doc->pageorder = ASCEND;
		page_order_set = 1;
	    } else if (strcmp(text, "Descend") == 0) {
		doc->pageorder = DESCEND;
		page_order_set = 1;
	    } else if (strcmp(text, "Special") == 0) {
		doc->pageorder = SPECIAL;
		page_order_set = 1;
	    }
	} else if (pages_set == NONE && iscomment(line+2, "Pages:")) {
	    sscanf(line+length("%%Pages:"), "%s", text);
	    if (strcmp(text, "(atend)") == 0) {
		pages_set = ATEND;
	    } else {
		switch (sscanf(line+length("%%Pages:"), "%d %d",
			       &maxpages, &i)) {
		    case 2:
			if (page_order_set == NONE) {
			    if (i == -1) {
				doc->pageorder = DESCEND;
				page_order_set = 1;
			    } else if (i == 0) {
				doc->pageorder = SPECIAL;
				page_order_set = 1;
			    } else if (i == 1) {
				doc->pageorder = ASCEND;
				page_order_set = 1;
			    }
			}
		    case 1:
			if (maxpages > 0)
			    doc->pages = g_new0 (struct page, maxpages);
		}
	    }
	} else if (doc->nummedia == NONE &&
		   iscomment(line+2, "DocumentMedia:")) {
	    float w, h;
	    doc->media = g_new0 (struct documentmedia, 1);
	    doc->media[0].name = get_next_text(line+length("%%DocumentMedia:"),
					 &next_char);
	    if (doc->media[0].name != NULL) {
		if (sscanf(next_char, "%f %f", &w, &h) == 2) {
		    doc->media[0].width = w + 0.5;
		    doc->media[0].height = h + 0.5;
		}
		if (doc->media[0].width != 0 && doc->media[0].height != 0)
		    doc->nummedia = 1;
		else
		    g_free(doc->media[0].name);
	    }
	    preread=1;
	    while (readline(line, sizeof line, file, &position, &line_len) &&
		   DSCcomment(line) && iscomment(line+2, "+")) {
		section_len += line_len;
		doc->media = g_renew (struct documentmedia,
				      doc->media, doc->nummedia+1);
		doc->media[doc->nummedia].name = get_next_text(line+length("%%+"),
							 &next_char);
		if (doc->media[doc->nummedia].name != NULL) {
		    if (sscanf(next_char, "%f %f", &w, &h) == 2) {
			doc->media[doc->nummedia].width = w + 0.5;
			doc->media[doc->nummedia].height = h + 0.5;
		    }
		    if (doc->media[doc->nummedia].width != 0 &&
			doc->media[doc->nummedia].height != 0) doc->nummedia++;
		    else
			g_free(doc->media[doc->nummedia].name);
		}
	    }
	    section_len += line_len;
	    if (doc->nummedia != 0) doc->default_page_media = doc->media;
	} else if (doc->nummedia == NONE &&
		   iscomment(line+2, "DocumentPaperSizes:")) {

	    doc->media = g_new0 (struct documentmedia, 1);
	    doc->media[0].name = get_next_text(line+length("%%DocumentPaperSizes:"),
					 &next_char);
	    if (doc->media[0].name != NULL) {
		doc->media[0].width = 0;
		doc->media[0].height = 0;
		for (dmp=papersizes; dmp->name != NULL; dmp++) {
		    /* Note: Paper size comment uses down cased paper size
		     * name.  Case insensitive compares are only used for
		     * PaperSize comments.
		     */
		    if (strcasecmp(doc->media[0].name, dmp->name) == 0) {
			g_free(doc->media[0].name);
			doc->media[0].name = g_strdup (dmp->name);
			doc->media[0].width = dmp->width;
			doc->media[0].height = dmp->height;
			break;
		    }
		}
		if (doc->media[0].width != 0 && doc->media[0].height != 0)
		    doc->nummedia = 1;
		else
		    g_free(doc->media[0].name);
	    }
	    while ( (cp = get_next_text(next_char, &next_char)) ) {
		doc->media = g_renew (struct documentmedia,
				      doc->media, doc->nummedia+1);
		doc->media[doc->nummedia].name = cp;
		doc->media[doc->nummedia].width = 0;
		doc->media[doc->nummedia].height = 0;
		for (dmp=papersizes; dmp->name != NULL; dmp++) {
		    /* Note: Paper size comment uses down cased paper size
		     * name.  Case insensitive compares are only used for
		     * PaperSize comments.
		     */
		    if (strcasecmp(doc->media[doc->nummedia].name,
			       dmp->name) == 0) {
			g_free(doc->media[doc->nummedia].name);
			doc->media[doc->nummedia].name = g_strdup (dmp->name);
			doc->media[doc->nummedia].name = dmp->name;
			doc->media[doc->nummedia].width = dmp->width;
			doc->media[doc->nummedia].height = dmp->height;
			break;
		    }
		}
		if (doc->media[doc->nummedia].width != 0 &&
		    doc->media[doc->nummedia].height != 0) doc->nummedia++;
		else
		    g_free(doc->media[doc->nummedia].name);
	    }
	    preread=1;
	    while (readline(line, sizeof line, file, &position, &line_len) &&
		   DSCcomment(line) && iscomment(line+2, "+")) {
		section_len += line_len;
		next_char = line + length("%%+");
		while ((cp = get_next_text(next_char, &next_char)) ) {
		    doc->media = g_renew (struct documentmedia,
					  doc->media, doc->nummedia+1);
		    doc->media[doc->nummedia].name = cp;
		    doc->media[doc->nummedia].width = 0;
		    doc->media[doc->nummedia].height = 0;
		    for (dmp=papersizes; dmp->name != NULL; dmp++) {
			/* Note: Paper size comment uses down cased paper size
			 * name.  Case insensitive compares are only used for
			 * PaperSize comments.
			 */
			if (strcasecmp(doc->media[doc->nummedia].name,
				   dmp->name) == 0) {
			    doc->media[doc->nummedia].width = dmp->width;
			    doc->media[doc->nummedia].height = dmp->height;
			    break;
			}
		    }
		    if (doc->media[doc->nummedia].width != 0 &&
			doc->media[doc->nummedia].height != 0) doc->nummedia++;
		    else
			g_free(doc->media[doc->nummedia].name);
		}
	    }
	    section_len += line_len;
	    if (doc->nummedia != 0) doc->default_page_media = doc->media;
	}
    }

    if (DSCcomment(line) && iscomment(line+2, "EndComments")) {
	readline(line, sizeof line, file, &position, &line_len);
	section_len += line_len;
    }
    doc->endheader = position;
    doc->lenheader = section_len - line_len;

    /* Optional Preview comments for encapsulated PostScript files */ 

    beginsection = position;
    section_len = line_len;
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }

    if (doc->epsf && DSCcomment(line) && iscomment(line+2, "BeginPreview")) {
	doc->beginpreview = beginsection;
	beginsection = 0;
	while (readline(line, sizeof line, file, &position, &line_len) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndPreview"))) {
	    section_len += line_len;
	}
	section_len += line_len;
	readline(line, sizeof line, file, &position, &line_len);
	section_len += line_len;
	doc->endpreview = position;
	doc->lenpreview = section_len - line_len;
    }

    /* Page Defaults for Version 3.0 files */

    if (beginsection == 0) {
	beginsection = position;
	section_len = line_len;
    }
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }

    if (DSCcomment(line) && iscomment(line+2, "BeginDefaults")) {
	doc->begindefaults = beginsection;
	beginsection = 0;
	while (readline(line, sizeof line, file, &position, &line_len) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndDefaults"))) {
	    section_len += line_len;
	    if (!DSCcomment(line)) {
		/* Do nothing */
	    } else if (doc->default_page_orientation == NONE &&
		iscomment(line+2, "PageOrientation:")) {
		sscanf(line+length("%%PageOrientation:"), "%s", text);
		if (strcmp(text, "Portrait") == 0) {
		    doc->default_page_orientation = PORTRAIT;
		} else if (strcmp(text, "Landscape") == 0) {
		    doc->default_page_orientation = LANDSCAPE;
		}
	    } else if (page_media_set == NONE &&
		       iscomment(line+2, "PageMedia:")) {
		cp = get_next_text(line+length("%%PageMedia:"), NULL);
		for (dmp = doc->media, i=0; i<doc->nummedia; i++, dmp++) {
		    if (strcmp(cp, dmp->name) == 0) {
			doc->default_page_media = dmp;
			page_media_set = 1;
			break;
		    }
		}
		g_free(cp);
	    } else if (page_bb_set == NONE &&
		       iscomment(line+2, "PageBoundingBox:")) {
		if (sscanf(line+length("%%PageBoundingBox:"), "%d %d %d %d",
			   &(doc->default_page_boundingbox[LLX]),
			   &(doc->default_page_boundingbox[LLY]),
			   &(doc->default_page_boundingbox[URX]),
			   &(doc->default_page_boundingbox[URY])) == 4)
		    page_bb_set = 1;
		else {
		    float fllx, flly, furx, fury;
		    if (sscanf(line+length("%%PageBoundingBox:"), "%f %f %f %f",
			       &fllx, &flly, &furx, &fury) == 4) {
			page_bb_set = 1;
			doc->default_page_boundingbox[LLX] = fllx;
			doc->default_page_boundingbox[LLY] = flly;
			doc->default_page_boundingbox[URX] = furx;
			doc->default_page_boundingbox[URY] = fury;
			if (fllx < doc->default_page_boundingbox[LLX])
			    doc->default_page_boundingbox[LLX]--;
			if (flly < doc->default_page_boundingbox[LLY])
			    doc->default_page_boundingbox[LLY]--;
			if (furx > doc->default_page_boundingbox[URX])
			    doc->default_page_boundingbox[URX]++;
			if (fury > doc->default_page_boundingbox[URY])
			    doc->default_page_boundingbox[URY]++;
		    }
		}
	    }
	}
	section_len += line_len;
	readline(line, sizeof line, file, &position, &line_len);
	section_len += line_len;
	doc->enddefaults = position;
	doc->lendefaults = section_len - line_len;
    }

    /* Document Prolog */

    if (beginsection == 0) {
	beginsection = position;
	section_len = line_len;
    }
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }

    if (!(DSCcomment(line) &&
	  (iscomment(line+2, "BeginSetup") ||
	   iscomment(line+2, "Page:") ||
	   iscomment(line+2, "Trailer") ||
	   iscomment(line+2, "EOF")))) {
	doc->beginprolog = beginsection;
	beginsection = 0;
	preread = 1;

	while ((preread ||
		readline(line, sizeof line, file, &position, &line_len)) &&
	       !(DSCcomment(line) &&
	         (iscomment(line+2, "EndProlog") ||
	          iscomment(line+2, "BeginSetup") ||
	          iscomment(line+2, "Page:") ||
	          iscomment(line+2, "Trailer") ||
	          iscomment(line+2, "EOF")))) {
	    if (!preread) section_len += line_len;
	    preread = 0;
	}
	section_len += line_len;
	if (DSCcomment(line) && iscomment(line+2, "EndProlog")) {
	    readline(line, sizeof line, file, &position, &line_len);
	    section_len += line_len;
	}
	doc->endprolog = position;
	doc->lenprolog = section_len - line_len;
    }

    /* Document Setup,  Page Defaults found here for Version 2 files */

    if (beginsection == 0) {
	beginsection = position;
	section_len = line_len;
    }
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }

    if (!(DSCcomment(line) &&
	  (iscomment(line+2, "Page:") ||
	   iscomment(line+2, "Trailer") ||
	   (respect_eof && iscomment(line+2, "EOF"))))) {
	doc->beginsetup = beginsection;
	beginsection = 0;
	preread = 1;
	while ((preread ||
		readline(line, sizeof line, file, &position, &line_len)) &&
	       !(DSCcomment(line) &&
	         (iscomment(line+2, "EndSetup") ||
	          iscomment(line+2, "Page:") ||
	          iscomment(line+2, "Trailer") ||
	          (respect_eof && iscomment(line+2, "EOF"))))) {
	    if (!preread) section_len += line_len;
	    preread = 0;
	    if (!DSCcomment(line)) {
		/* Do nothing */
	    } else if (doc->default_page_orientation == NONE &&
		iscomment(line+2, "PageOrientation:")) {
		sscanf(line+length("%%PageOrientation:"), "%s", text);
		if (strcmp(text, "Portrait") == 0) {
		    doc->default_page_orientation = PORTRAIT;
		} else if (strcmp(text, "Landscape") == 0) {
		    doc->default_page_orientation = LANDSCAPE;
		}
	    } else if (page_media_set == NONE &&
		       iscomment(line+2, "PaperSize:")) {
		cp = get_next_text(line+length("%%PaperSize:"), NULL);
		for (dmp = doc->media, i=0; i<doc->nummedia; i++, dmp++) {
		    /* Note: Paper size comment uses down cased paper size
		     * name.  Case insensitive compares are only used for
		     * PaperSize comments.
		     */
		    if (strcasecmp(cp, dmp->name) == 0) {
			doc->default_page_media = dmp;
			page_media_set = 1;
			break;
		    }
		}
		g_free(cp);
	    } else if (page_bb_set == NONE &&
		       iscomment(line+2, "PageBoundingBox:")) {
		if (sscanf(line+length("%%PageBoundingBox:"), "%d %d %d %d",
			   &(doc->default_page_boundingbox[LLX]),
			   &(doc->default_page_boundingbox[LLY]),
			   &(doc->default_page_boundingbox[URX]),
			   &(doc->default_page_boundingbox[URY])) == 4)
		    page_bb_set = 1;
		else {
		    float fllx, flly, furx, fury;
		    if (sscanf(line+length("%%PageBoundingBox:"), "%f %f %f %f",
			       &fllx, &flly, &furx, &fury) == 4) {
			page_bb_set = 1;
			doc->default_page_boundingbox[LLX] = fllx;
			doc->default_page_boundingbox[LLY] = flly;
			doc->default_page_boundingbox[URX] = furx;
			doc->default_page_boundingbox[URY] = fury;
			if (fllx < doc->default_page_boundingbox[LLX])
			    doc->default_page_boundingbox[LLX]--;
			if (flly < doc->default_page_boundingbox[LLY])
			    doc->default_page_boundingbox[LLY]--;
			if (furx > doc->default_page_boundingbox[URX])
			    doc->default_page_boundingbox[URX]++;
			if (fury > doc->default_page_boundingbox[URY])
			    doc->default_page_boundingbox[URY]++;
		    }
		}
	    }
	}
	section_len += line_len;
	if (DSCcomment(line) && iscomment(line+2, "EndSetup")) {
	    readline(line, sizeof line, file, &position, &line_len);
	    section_len += line_len;
	}
	doc->endsetup = position;
	doc->lensetup = section_len - line_len;
    }

    /* Added this (Nov. 2, 1999) when I noticed that
       a Postscript file would load in gv but not in ggv

       dmg@csg.uwaterloo.ca */

    /* BEGIN Windows NT fix ###jp###
       Mark Pfeifer (pfeiferm%ppddev@comet.cmis.abbott.com) told me
       about problems when viewing Windows NT 3.51 generated postscript
       files with gv. He found that the relevant postscript files
       show important postscript code after the '%%EndSetup' and before
       the first page comment '%%Page: x y'.
    */
    if (doc->beginsetup) {
      while (!(DSCcomment(line) &&
	      (iscomment(line+2, "EndSetup") ||
	      (iscomment(line+2, "Page:") ||
	       iscomment(line+2, "Trailer") ||
	       (respect_eof && iscomment(line+2, "EOF"))))) &&
             (readline(line, sizeof line, file, &position, &line_len))) {
        section_len += line_len;
        doc->lensetup = section_len - line_len;
	doc->endsetup = position;
      }
    }
    /* END Windows NT fix ###jp##*/

    /* Individual Pages */

    if (beginsection == 0) {
	beginsection = position;
	section_len = line_len;
    }
    while (blank(line) &&
	   readline(line, sizeof line, file, &position, &line_len)) {
	section_len += line_len;
    }


newpage:
    while (DSCcomment(line) && iscomment(line+2, "Page:")) {
	if (maxpages == 0) {
	    maxpages = 1;
	    doc->pages = g_new0 (struct page, maxpages);
	}
	label = get_next_text(line+length("%%Page:"), &next_char);
	if (sscanf(next_char, "%d", &thispage) != 1) thispage = 0;
	if (nextpage == 1) {
	    ignore = thispage != 1;
	}
	if (!ignore && thispage != nextpage) {
	    g_free(label);
	    doc->numpages--;
	    goto continuepage;
	}
	nextpage++;
	if (doc->numpages == maxpages) {
	    maxpages++;
	    doc->pages = g_renew (struct page, doc->pages, maxpages);
	}
	memset(&(doc->pages[doc->numpages]), 0, sizeof(struct page));
	page_bb_set = NONE;
	doc->pages[doc->numpages].label = label;
	if (beginsection) {
	    doc->pages[doc->numpages].begin = beginsection;
	    beginsection = 0;
	} else {
	    doc->pages[doc->numpages].begin = position;
	    section_len = line_len;
	}
continuepage:
	while (readline(line, sizeof line, file, &position, &line_len) &&
	       !(DSCcomment(line) &&
	         (iscomment(line+2, "Page:") ||
	          iscomment(line+2, "Trailer") ||
	          (respect_eof && iscomment(line+2, "EOF"))))) {
	    section_len += line_len;
	    if (!DSCcomment(line)) {
		/* Do nothing */
	    } else if (doc->pages[doc->numpages].orientation == NONE &&
		iscomment(line+2, "PageOrientation:")) {
		sscanf(line+length("%%PageOrientation:"), "%s", text);
		if (strcmp(text, "Portrait") == 0) {
		    doc->pages[doc->numpages].orientation = PORTRAIT;
		} else if (strcmp(text, "Landscape") == 0) {
		    doc->pages[doc->numpages].orientation = LANDSCAPE;
		}
	    } else if (doc->pages[doc->numpages].media == NULL &&
		       iscomment(line+2, "PageMedia:")) {
		cp = get_next_text(line+length("%%PageMedia:"), NULL);
		for (dmp = doc->media, i=0; i<doc->nummedia; i++, dmp++) {
		    if (strcmp(cp, dmp->name) == 0) {
			doc->pages[doc->numpages].media = dmp;
			break;
		    }
		}
		g_free(cp);
	    } else if (doc->pages[doc->numpages].media == NULL &&
		       iscomment(line+2, "PaperSize:")) {
		cp = get_next_text(line+length("%%PaperSize:"), NULL);
		for (dmp = doc->media, i=0; i<doc->nummedia; i++, dmp++) {
		    /* Note: Paper size comment uses down cased paper size
		     * name.  Case insensitive compares are only used for
		     * PaperSize comments.
		     */
		    if (strcasecmp(cp, dmp->name) == 0) {
			doc->pages[doc->numpages].media = dmp;
			break;
		    }
		}
		g_free(cp);
	    } else if ((page_bb_set == NONE || page_bb_set == ATEND) &&
		       iscomment(line+2, "PageBoundingBox:")) {
		sscanf(line+length("%%PageBoundingBox:"), "%s", text);
		if (strcmp(text, "(atend)") == 0) {
		    page_bb_set = ATEND;
		} else {
		    if (sscanf(line+length("%%PageBoundingBox:"), "%d %d %d %d",
			    &(doc->pages[doc->numpages].boundingbox[LLX]),
			    &(doc->pages[doc->numpages].boundingbox[LLY]),
			    &(doc->pages[doc->numpages].boundingbox[URX]),
                            &(doc->pages[doc->numpages].boundingbox[URY])) == 4) {
			if (page_bb_set == NONE) page_bb_set = 1;
                    } else {
			float fllx, flly, furx, fury;
			if (sscanf(line+length("%%PageBoundingBox:"),
				   "%f %f %f %f",
				   &fllx, &flly, &furx, &fury) == 4) {
			    if (page_bb_set == NONE) page_bb_set = 1;
			    doc->pages[doc->numpages].boundingbox[LLX] = fllx;
			    doc->pages[doc->numpages].boundingbox[LLY] = flly;
			    doc->pages[doc->numpages].boundingbox[URX] = furx;
			    doc->pages[doc->numpages].boundingbox[URY] = fury;
			    if (fllx <
				    doc->pages[doc->numpages].boundingbox[LLX])
				doc->pages[doc->numpages].boundingbox[LLX]--;
			    if (flly <
				    doc->pages[doc->numpages].boundingbox[LLY])
				doc->pages[doc->numpages].boundingbox[LLY]--;
			    if (furx >
				    doc->pages[doc->numpages].boundingbox[URX])
				doc->pages[doc->numpages].boundingbox[URX]++;
			    if (fury >
				    doc->pages[doc->numpages].boundingbox[URY])
				doc->pages[doc->numpages].boundingbox[URY]++;
			}
		    }
		}
	    }
	}
	section_len += line_len;
	doc->pages[doc->numpages].end = position;
	doc->pages[doc->numpages].len = section_len - line_len;
	doc->numpages++;
    }

    /* Document Trailer */

    if (beginsection) {
	doc->begintrailer = beginsection;
	beginsection = 0;
    } else {
	doc->begintrailer = position;
	section_len = line_len;
    }

    preread = 1;
    while ((preread ||
	    readline(line, sizeof line, file, &position, &line_len)) &&
	   !(respect_eof && DSCcomment(line) && iscomment(line+2, "EOF"))) {
	if (!preread) section_len += line_len;
	preread = 0;
	if (!DSCcomment(line)) {
	    /* Do nothing */
	} else if (iscomment(line+2, "Page:")) {
	    g_free(get_next_text(line+length("%%Page:"), &next_char));
	    if (sscanf(next_char, "%d", &thispage) != 1) thispage = 0;
	    if (!ignore && thispage == nextpage) {
		if (doc->numpages > 0) {
		    doc->pages[doc->numpages-1].end = position;
		    doc->pages[doc->numpages-1].len += section_len - line_len;
		} else {
		    if (doc->endsetup) {
			doc->endsetup = position;
			doc->endsetup += section_len - line_len;
		    } else if (doc->endprolog) {
			doc->endprolog = position;
			doc->endprolog += section_len - line_len;
		    }
		}
		goto newpage;
	    }
	} else if (!respect_eof && iscomment(line+2, "Trailer")) {
	    /* What we thought was the start of the trailer was really */
	    /* the trailer of an EPS on the page. */
	    /* Set the end of the page to this trailer and keep scanning. */
	    if (doc->numpages > 0) {
		doc->pages[ doc->numpages-1 ].end = position;
		doc->pages[ doc->numpages-1 ].len += section_len - line_len;
	    }
	    doc->begintrailer = position;
	    section_len = line_len;
	} else if (bb_set == ATEND && iscomment(line+2, "BoundingBox:")) {
	    if (sscanf(line+length("%%BoundingBox:"), "%d %d %d %d",
		       &(doc->boundingbox[LLX]),
		       &(doc->boundingbox[LLY]),
		       &(doc->boundingbox[URX]),
		       &(doc->boundingbox[URY])) != 4) {
		float fllx, flly, furx, fury;
		if (sscanf(line+length("%%BoundingBox:"), "%f %f %f %f",
			   &fllx, &flly, &furx, &fury) == 4) {
		    doc->boundingbox[LLX] = fllx;
		    doc->boundingbox[LLY] = flly;
		    doc->boundingbox[URX] = furx;
		    doc->boundingbox[URY] = fury;
		    if (fllx < doc->boundingbox[LLX])
			doc->boundingbox[LLX]--;
		    if (flly < doc->boundingbox[LLY])
			doc->boundingbox[LLY]--;
		    if (furx > doc->boundingbox[URX])
			doc->boundingbox[URX]++;
		    if (fury > doc->boundingbox[URY])
			doc->boundingbox[URY]++;
		}
	    }
	} else if (orientation_set == ATEND &&
		   iscomment(line+2, "Orientation:")) {
	    sscanf(line+length("%%Orientation:"), "%s", text);
	    if (strcmp(text, "Portrait") == 0) {
		doc->orientation = PORTRAIT;
	    } else if (strcmp(text, "Landscape") == 0) {
		doc->orientation = LANDSCAPE;
	    }
	} else if (page_order_set == ATEND && iscomment(line+2, "PageOrder:")) {
	    sscanf(line+length("%%PageOrder:"), "%s", text);
	    if (strcmp(text, "Ascend") == 0) {
		doc->pageorder = ASCEND;
	    } else if (strcmp(text, "Descend") == 0) {
		doc->pageorder = DESCEND;
	    } else if (strcmp(text, "Special") == 0) {
		doc->pageorder = SPECIAL;
	    }
	} else if (pages_set == ATEND && iscomment(line+2, "Pages:")) {
	    if (sscanf(line+length("%%Pages:"), "%*u %d", &i) == 1) {
		if (page_order_set == NONE) {
		    if (i == -1) doc->pageorder = DESCEND;
		    else if (i == 0) doc->pageorder = SPECIAL;
		    else if (i == 1) doc->pageorder = ASCEND;
		}
	    }
	}
    }
    section_len += line_len;
    if (DSCcomment(line) && iscomment(line+2, "EOF")) {
	readline(line, sizeof line, file, &position, &line_len);
	section_len += line_len;
    }
    doc->endtrailer = position;
    doc->lentrailer = section_len - line_len;

#if 0
    section_len = line_len;
    preread = 1;
    while (preread ||
	   readline(line, sizeof line, file, &position, &line_len)) {
	if (!preread) section_len += line_len;
	preread = 0;
	if (DSCcomment(line) && iscomment(line+2, "Page:")) {
	    g_free(get_next_text(line+length("%%Page:"), &next_char));
	    if (sscanf(next_char, "%d", &thispage) != 1) thispage = 0;
	    if (!ignore && thispage == nextpage) {
		if (doc->numpages > 0) {
		    doc->pages[doc->numpages-1].end = position;
		    doc->pages[doc->numpages-1].len += doc->lentrailer +
						       section_len - line_len;
		} else {
		    if (doc->endsetup) {
			doc->endsetup = position;
			doc->endsetup += doc->lentrailer +
					 section_len - line_len;
		    } else if (doc->endprolog) {
			doc->endprolog = position;
			doc->endprolog += doc->lentrailer +
					  section_len - line_len;
		    }
		}
		goto newpage;
	    }
	}
    }
#endif
    return doc;
}

/*
 *	psfree -- free dynamic storage associated with document structure.
 */

void
psfree(doc)
    struct document *doc;
{
    int i;

    if (doc) {
      /*
    printf("This document exists\n");
      */
	for (i=0; i<doc->numpages; i++) {
	    if (doc->pages[i].label) g_free(doc->pages[i].label);
	}
	for (i=0; i<doc->nummedia; i++) {
	    if (doc->media[i].name) g_free(doc->media[i].name);
	}
	if (doc->title) g_free(doc->title);
	if (doc->date) g_free(doc->date);
	if (doc->pages) g_free(doc->pages);
	if (doc->media) g_free(doc->media);
	g_free(doc);
    }
}

/*
 * gettextine -- skip over white space and return the rest of the line.
 *               If the text begins with '(' return the text string
 *		 using get_next_text().
 */

static char * gettextline(char *line)
{
        char *cp;

        while (*line && (*line == ' ' || *line == '\t'))
                line++;
        if (*line == '(') {
                return get_next_text(line, NULL);
        } else {
                if (strlen(line) == 0)
                        return NULL;

                cp = g_strdup (line);

                /* Remove end of line */
                if (cp[strlen(line)-2] == '\r' && cp[strlen(line)-1] == '\n')
                        /* Handle DOS \r\n */
                        cp[strlen(line)-2] = '\0';
                else if (cp[strlen(line)-1] == '\n' || cp[strlen(line)-1] == '\r')
                        /* Handle mac and unix */
                        cp[strlen(line)-1] = '\0';

                return cp;
        }
}

/*
 *	get_next_text -- return the next text string on the line.
 *		   return NULL if nothing is present.
 */

static char *
get_next_text(line, next_char)
    char *line;
    char **next_char;
{
    char text[PSLINELENGTH];	/* Temporary storage for text */
    char *cp;
    int quoted=0;

    while (*line && (*line == ' ' || *line == '\t')) line++;
    cp = text;
    if (*line == '(') {
	int level = 0;
	quoted=1;
	line++;
	while (*line && !(*line == ')' && level == 0 )) {
	    if (*line == '\\') {
		if (*(line+1) == 'n') {
		    *cp++ = '\n';
		    line += 2;
		} else if (*(line+1) == 'r') {
		    *cp++ = '\r';
		    line += 2;
		} else if (*(line+1) == 't') {
		    *cp++ = '\t';
		    line += 2;
		} else if (*(line+1) == 'b') {
		    *cp++ = '\b';
		    line += 2;
		} else if (*(line+1) == 'f') {
		    *cp++ = '\f';
		    line += 2;
		} else if (*(line+1) == '\\') {
		    *cp++ = '\\';
		    line += 2;
		} else if (*(line+1) == '(') {
		    *cp++ = '(';
		    line += 2;
		} else if (*(line+1) == ')') {
		    *cp++ = ')';
		    line += 2;
		} else if (*(line+1) >= '0' && *(line+1) <= '9') {
		    if (*(line+2) >= '0' && *(line+2) <= '9') {
			if (*(line+3) >= '0' && *(line+3) <= '9') {
			    *cp++ = ((*(line+1) - '0')*8 + *(line+2) - '0')*8 +
				    *(line+3) - '0';
			    line += 4;
			} else {
			    *cp++ = (*(line+1) - '0')*8 + *(line+2) - '0';
			    line += 3;
			}
		    } else {
			*cp++ = *(line+1) - '0';
			line += 2;
		    }
		} else {
		    line++;
		    *cp++ = *line++;
		}
	    } else if (*line == '(') {
		level++;
		*cp++ = *line++;
	    } else if (*line == ')') {
		level--;
		*cp++ = *line++;
	    } else {
		*cp++ = *line++;
	    }
	}
    } else {
	while (*line && !(*line == ' ' || *line == '\t' || *line == '\n'))
	    *cp++ = *line++;
    }
    *cp = '\0';
    if (next_char) *next_char = line;
    if (!quoted && strlen(text) == 0) return NULL;
    return g_strdup (text);
}

/*
 *	readline -- Read the next line in the postscript file.
 *                  Automatically skip over data (as indicated by
 *                  %%BeginBinary/%%EndBinary or %%BeginData/%%EndData
 *		    comments.)
 *		    Also, skip over included documents (as indicated by
 *		    %%BeginDocument/%%EndDocument comments.)
 */
/*
static char * readline (fd, lineP, positionP, line_lenP)
   FileData fd;
   char **lineP;
   long *positionP;
   unsigned int *line_lenP;
*/

#ifdef WE_MIGHT_WANT_TO_INCLUDE_THIS_NEW_READLINE


static char *
readline(lineP, size, fp, positionP, line_lenP)
    char *lineP;
    int size;
    FILE *fp;
    long *positionP;
    unsigned int *line_lenP;
{
   unsigned int nbytes=0;
   int skipped=0;
   char text[PSLINELENGTH];
   char line[PSLINELENGTH];
   char save[PSLINELENGTH];
   char buf[BUFSIZ];
   char *cp;
   unsigned int num;
   int i;

   if (positionP) *positionP = ftell(fp);
   cp = fgets(line, size, fp);
   if (cp == NULL) {
      *line_lenP = 0;
      *lineP     = '\0';
      return(NULL); 
   }

   *line_lenP = strlen(line);

#define IS_COMMENT(comment)				\
           (DSCcomment(line) && iscomment(line+2,(comment)))
#define IS_BEGIN(comment)				\
           (iscomment(line+7,(comment)))

#define SKIP_WHILE(cond)				\
	   while (readline(line, size, fp, NULL, &nbytes) \
             && (cond)) *line_lenP += nbytes;\
           skipped=1;

#define SKIP_UNTIL_1(comment) {				\
           SKIP_WHILE((!IS_COMMENT(comment)))           \
        }
#define SKIP_UNTIL_2(comment1,comment2) {		\
           SKIP_WHILE((!IS_COMMENT(comment1) && !IS_COMMENT(comment2)))\
        }

   if  (!IS_COMMENT("Begin"))     {} /* Do nothing */
   else if IS_BEGIN("Document:")  SKIP_UNTIL_1("EndDocument")
   else if IS_BEGIN("Feature:")   SKIP_UNTIL_1("EndFeature")
#ifdef USE_ACROREAD_WORKAROUND
   else if IS_BEGIN("File")       SKIP_UNTIL_2("EndFile","EOF")
#else
   else if IS_BEGIN("File")       SKIP_UNTIL_1("EndFile")
#endif
   else if IS_BEGIN("Font")       SKIP_UNTIL_1("EndFont")
   else if IS_BEGIN("ProcSet")    SKIP_UNTIL_1("EndProcSet")
   else if IS_BEGIN("Resource")   SKIP_UNTIL_1("EndResource")
   else if IS_BEGIN("Data:")      {
	text[0] = '\0';
	strcpy(save, line+7);
	if (sscanf(line+length("%%BeginData:"), "%d %*s %s", &num, text) >= 1) {
	    if (strcmp(text, "Lines") == 0) {
		for (i=0; i < num; i++) {
		    cp = fgets(line, size, fp);
		    *line_lenP += cp ? strlen(line) : 0;
		}
	    } else {
		while (num > BUFSIZ) {
		    fread(buf, sizeof (char), BUFSIZ, fp);
		    *line_lenP += BUFSIZ;
		    num -= BUFSIZ;
		}
		fread(buf, sizeof (char), num, fp);
		*line_lenP += num;
	    }
	}
        SKIP_UNTIL_1("EndData")
    } 
   else if IS_BEGIN("Binary:") {
        strcpy(save, line+7);
	if(sscanf(line+length("%%BeginBinary:"), "%d", &num) == 1) {
	    while (num > BUFSIZ) {
		fread(buf, sizeof (char), BUFSIZ, fp);
		*line_lenP += BUFSIZ;
		num -= BUFSIZ;
	    }
	    fread(buf, sizeof (char), num, fp);
	    *line_lenP += num;
	}
        SKIP_UNTIL_1("EndBinary")
	*line_lenP += nbytes;
    }

   if (skipped) {
      *line_lenP += nbytes;
      strcpy(lineP, skipped_line);
   } else {
      strcpy(lineP, line);
   }
   return lineP;
}

#endif

static char *
readline(line, size, fp, position, line_len)
    char *line;
    int size;
    FILE *fp;
    long *position;
    unsigned int *line_len;
{
    char text[PSLINELENGTH];	/* Temporary storage for text */
    char save[PSLINELENGTH];	/* Temporary storage for text */
    char *cp;
    unsigned int num;
    unsigned int nbytes;
    int i;
    char buf[BUFSIZ];

    if (position) *position = ftell(fp);
    cp = fgets(line, size, fp);
    if (cp == NULL) line[0] = '\0';
    *line_len = strlen(line);
    if (!(DSCcomment(line) && iscomment(line+2, "Begin"))) {
	/* Do nothing */
    } else if (iscomment(line+7, "Document:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndDocument"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Feature:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndFeature"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "File:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndFile"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Font:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndFont"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "ProcSet:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndProcSet"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Resource:")) {
	strcpy(save, line+7);
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndResource"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Data:")) {
	text[0] = '\0';
	strcpy(save, line+7);
	if (sscanf(line+length("%%BeginData:"), "%d %*s %s", &num, text) >= 1) {
	    if (strcmp(text, "Lines") == 0) {
		for (i=0; i < num; i++) {
		    cp = fgets(line, size, fp);
		    *line_len += cp ? strlen(line) : 0;
		}
	    } else {
		while (num > BUFSIZ) {
		    fread(buf, sizeof (char), BUFSIZ, fp);
		    *line_len += BUFSIZ;
		    num -= BUFSIZ;
		}
		fread(buf, sizeof (char), num, fp);
		*line_len += num;
	    }
	}
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndData"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    } else if (iscomment(line+7, "Binary:")) {
	strcpy(save, line+7);
	if(sscanf(line+length("%%BeginBinary:"), "%d", &num) == 1) {
	    while (num > BUFSIZ) {
		fread(buf, sizeof (char), BUFSIZ, fp);
		*line_len += BUFSIZ;
		num -= BUFSIZ;
	    }
	    fread(buf, sizeof (char), num, fp);
	    *line_len += num;
	}
	while (readline(line, size, fp, NULL, &nbytes) &&
	       !(DSCcomment(line) && iscomment(line+2, "EndBinary"))) {
	    *line_len += nbytes;
	}
	*line_len += nbytes;
	strcpy(line, save);
    }
    return cp;
}


/*
 *	pscopy -- copy lines of Postscript from a section of one file
 *		  to another file.
 *                Automatically switch to binary copying whenever
 *                %%BeginBinary/%%EndBinary or %%BeginData/%%EndData
 *		  comments are encountered.
 */

void
pscopy(from, to, begin, end)
    FILE *from;
    FILE *to;
    long begin;			/* set negative to avoid initial seek */
    long end;
{
    char line[PSLINELENGTH];	/* 255 characters + 1 newline + 1 NULL */
    char text[PSLINELENGTH];	/* Temporary storage for text */
    unsigned int num;
    int i;
    char buf[BUFSIZ];

    if (begin >= 0) fseek(from, begin, SEEK_SET);
    while (ftell(from) < end) {

	fgets(line, sizeof line, from);
	fputs(line, to);

	if (!(DSCcomment(line) && iscomment(line+2, "Begin"))) {
	    /* Do nothing */
	} else if (iscomment(line+7, "Data:")) {
	    text[0] = '\0';
	    if (sscanf(line+length("%%BeginData:"),
		       "%d %*s %s", &num, text) >= 1) {
		if (strcmp(text, "Lines") == 0) {
		    for (i=0; i < num; i++) {
			fgets(line, sizeof line, from);
			fputs(line, to);
		    }
		} else {
		    while (num > BUFSIZ) {
			fread(buf, sizeof (char), BUFSIZ, from);
			fwrite(buf, sizeof (char), BUFSIZ, to);
			num -= BUFSIZ;
		    }
		    fread(buf, sizeof (char), num, from);
		    fwrite(buf, sizeof (char), num, to);
		}
	    }
	} else if (iscomment(line+7, "Binary:")) {
	    if(sscanf(line+length("%%BeginBinary:"), "%d", &num) == 1) {
		while (num > BUFSIZ) {
		    fread(buf, sizeof (char), BUFSIZ, from);
		    fwrite(buf, sizeof (char), BUFSIZ, to);
		    num -= BUFSIZ;
		}
		fread(buf, sizeof (char), num, from);
		fwrite(buf, sizeof (char), num, to);
	    }
	}
    }
}

/*
 *	pscopyuntil -- copy lines of Postscript from a section of one file
 *		       to another file until a particular comment is reached.
 *                     Automatically switch to binary copying whenever
 *                     %%BeginBinary/%%EndBinary or %%BeginData/%%EndData
 *		       comments are encountered.
 */

char *pscopyuntil(FILE *from, FILE *to, long begin, long end, const char *comment)
{
    char line[PSLINELENGTH];	/* 255 characters + 1 newline + 1 NULL */
    char text[PSLINELENGTH];	/* Temporary storage for text */
    unsigned int num;
    int comment_length;
    int i;
    char buf[BUFSIZ];
    char *cp;

    if (comment != NULL)
      comment_length = strlen(comment);
    else
      comment_length = 0;
    if (begin >= 0) fseek(from, begin, SEEK_SET);

    while (ftell(from) < end && !feof(from)) {
	fgets(line, sizeof line, from);

	/* iscomment cannot be used here,
	 * because comment_length is not known at compile time. */
	if (comment != NULL && strncmp(line, comment, comment_length) == 0) {
	    return g_strdup (line);
	}
	fputs(line, to);
	if (!(DSCcomment(line) && iscomment(line+2, "Begin"))) {
	    /* Do nothing */
	} else if (iscomment(line+7, "Data:")) {
	    text[0] = '\0';
	    if (sscanf(line+length("%%BeginData:"),
		       "%d %*s %s", &num, text) >= 1) {
		if (strcmp(text, "Lines") == 0) {
		    for (i=0; i < num; i++) {
			fgets(line, sizeof line, from);
			fputs(line, to);
		    }
		} else {
		    while (num > BUFSIZ) {
			fread(buf, sizeof (char), BUFSIZ, from);
			fwrite(buf, sizeof (char), BUFSIZ, to);
			num -= BUFSIZ;
		    }
		    fread(buf, sizeof (char), num, from);
		    fwrite(buf, sizeof (char), num, to);
		}
	    }
	} else if (iscomment(line+7, "Binary:")) {
	    if(sscanf(line+length("%%BeginBinary:"), "%d", &num) == 1) {
		while (num > BUFSIZ) {
		    fread(buf, sizeof (char), BUFSIZ, from);
		    fwrite(buf, sizeof (char), BUFSIZ, to);
		    num -= BUFSIZ;
		}
		fread(buf, sizeof (char), num, from);
		fwrite(buf, sizeof (char), num, to);
	    }
	}
    }
    return NULL;
}

/*
 *	blank -- determine whether the line contains nothing but whitespace.
 */

static int blank(char *line)
{
  char *cp = line;
  
  while (*cp == ' ' || *cp == '\t') cp++;
  return *cp == '\n' || (*cp == '%' && (line[0] != '%' || line[1] != '%'));
}

/*##########################################################*/
/* pscopydoc */
/* Copy the headers, marked pages, and trailer to fp */
/*##########################################################*/

void pscopydoc(FILE *dest_file,
	       char *src_filename,
	       struct document *d,
	       gint *pagelist)
{
  FILE *src_file;
  char text[PSLINELENGTH];
  char *comment;
  gboolean pages_written = FALSE;
  gboolean pages_atend = FALSE;
  int pages;
  int page = 1;
  int i, j;
  int here;
  /*
  FileData fd;
  */
  
  src_file = fopen(src_filename, "r");
  /*
  fd = ps_io_init(src_file);
  */
  i=0;
  pages=0;
  for (i=0;i<d->numpages;i++)    {
    if (pagelist[i]) pages++;
  }

  here = d->beginheader;

  while ((comment=pscopyuntil(src_file,dest_file,here,d->endheader,"%%Pages:"))) {

       here = ftell(src_file);
       if (pages_written || pages_atend) {
          g_free(comment);
          continue;
       }
       sscanf(comment+length("%%Pages:"), "%s", text);
       if (strcmp(text, "(atend)") == 0) {
          fputs(comment, dest_file);
          pages_atend = TRUE;
       } else {
          switch (sscanf(comment+length("%%Pages:"), "%*d %d", &i)) {
             case 1:
                fprintf(dest_file, "%%%%Pages: %d %d\n", pages, i);
                break;
             default:
                fprintf(dest_file, "%%%%Pages: %d\n", pages);
                break;
          }
          pages_written = TRUE;
       }
       g_free(comment);
  }
   pscopyuntil(src_file, dest_file, d->beginpreview, d->endpreview,NULL);
   pscopyuntil(src_file, dest_file, d->begindefaults, d->enddefaults,NULL);
   pscopyuntil(src_file, dest_file, d->beginprolog, d->endprolog,NULL);
   pscopyuntil(src_file, dest_file, d->beginsetup, d->endsetup,NULL);

   for (i = 0; i < d->numpages; i++) {
     if (d->pageorder == DESCEND) j = (d->numpages - 1) - i;
     else                         j = i;

     j = i;
     if (pagelist[j]) {

       comment = pscopyuntil(src_file,dest_file,
			     d->pages[i].begin,d->pages[i].end, "%%Page:");
       fprintf(dest_file, "%%%%Page: %s %d\n",d->pages[i].label, page++);
       g_free(comment);
       pscopyuntil(src_file, dest_file, -1, d->pages[i].end,NULL);
     }
   }

   here = d->begintrailer;
   while ((comment = pscopyuntil(src_file, dest_file, here, d->endtrailer, "%%Pages:"))) {
      here = ftell(src_file);
      if (pages_written) {
         g_free(comment);
         continue;
      }
      switch (sscanf(comment+length("%%Pages:"), "%*d %d", &i)) {
         case 1:
            fprintf(dest_file, "%%%%Pages: %d %d\n", pages, i);
            break;
         default:
            fprintf(dest_file, "%%%%Pages: %d\n", pages);
            break;
      }
      pages_written = TRUE;
      g_free(comment);
   }

   fclose(dest_file);
   fclose(src_file);
}
