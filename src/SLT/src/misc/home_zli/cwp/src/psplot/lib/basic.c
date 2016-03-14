/* Copyright (c) Colorado School of Mines, 1990.*/
/* All rights reserved.                       */

/*
Basic C function interface to PostScript
AUTHOR:  Dave Hale, Colorado School of Mines, 06/27/89
MODIFIED:  Craig Artley, Colorado School of Mines, 08/30/91
		Change beginps/endps functions to allow BoundingBox at top.
*/

#include <stdio.h>
#include "psplot.h"

/* Conforming PostScript defaults */
static struct {
	int llx,lly,urx,ury;
} BBox = {0,0,612,792};
static int BBoxSet = 0;
static int BBoxOut = 0;
static int nPages = 0;

/* Font metrics (take from the .afm files in the font library) */
#define NFONTS 14
#define FONTSCALE 0.001
typedef struct {
	char *name;
	int fontbbox[4];
	int capheight;
	int xheight;
	int descender;
	int ascender;
} FontMetric;
static FontMetric FontMetrics[NFONTS] = {
{"Helvetica", -174, -220, 1001, 944, 729, 525, -219, 729},
{"Helvetica-Oblique", -178, -220, 1108, 944, 729, 525, -219, 729},
{"Helvetica-Bold", -173, -221, 1003, 936, 729, 542, -219, 729},
{"Helvetica-BoldOblique", -177, -221, 1107, 936, 729, 542, -219, 729},
{"Times-Roman", -170, -223, 1024, 896, 662, 448, -217, 682},
{"Times-Italic", -176, -252, 990, 930, 660, 446, -206, 684},
{"Times-Bold", -172, -256, 1008, 965, 681, 460, -210, 670},
{"Times-BoldItalic", -168, -232, 1014, 894, 662, 458, -203, 682},
{"Courier", -200, -451, 800, 900, 583, 437, -207, 624},
{"Courier-Oblique", -200, -451, 800, 900, 583, 437, -207, 624},
{"Courier-Bold", -200, -451, 800, 900, 633, 487, -257, 674},
{"Courier-BoldOblique", -200, -451, 800, 900, 633, 487, -257, 674},
{"Symbol", -180, -293, 1090, 1010, 729, 525, -219, 729},
{"Unknown", -174, -220, 1001, 944, 729, 525, -219, 729}
};

/* PostScript definitions to make output files smaller */
static char *Prologue = "\
/M {moveto} def\n\
/RM {rmoveto} def\n\
/L {lineto} def\n\
/RL {rlineto} def\n\
/S {stroke} def\n\
/F {fill} def\n\
/GS {gsave} def\n\
/GR {grestore} def\n\
/SH {show} def\n\
/SW {stringwidth} def\n\
/NP {newpath} def\n\
/CP {closepath} def\n\
/SC {scale} def\n\
/RO {rotate} def\n\
/TR {translate} def\n\
/CAT {concat} def\n\
/CLW {currentlinewidth} def\n\
/SLW {setlinewidth} def\n\
";

/* private functions (for internal use only) */
static int fontindex(const char *fontname)
{
	int i;

	/* look for font name in FontMetrics table */
	for (i=0; i<NFONTS; i++)
		if (0==strcmp(fontname,FontMetrics[i].name))
			return i;

	/* if font name not found, then return index of unknown font */
	return i;
}

/* public functions */
void beginps(void)
{
	fprintf(stdout,"%%!PS-Adobe-2.0 EPSF-1.2\n");
	fprintf(stdout,"%%%%DocumentFonts:\n");
	if (BBoxSet) {
		fprintf(stdout,"%%%%BoundingBox: %d %d %d %d\n",
			BBox.llx,BBox.lly,BBox.urx,BBox.ury);
		BBoxOut = 1;
	} else {
		fprintf(stdout,"%%%%BoundingBox: (atend)\n");
	}
	fprintf(stdout,"%%%%Pages: (atend)\n");
	fprintf(stdout,"%%%%EndComments\n");
	fprintf(stdout,"%s\n",Prologue);
	fprintf(stdout,"%%%%EndProlog\n");
	fprintf(stdout,"GS\n");
}

void endps(void)
{
	fprintf(stdout,"GR\n");
	fprintf(stdout,"%%%%Trailer\n");
	if (!BBoxOut) {
		fprintf(stdout,"%%%%BoundingBox: %d %d %d %d\n",
			BBox.llx,BBox.lly,BBox.urx,BBox.ury);
		BBoxOut = 1;
	}
	fprintf(stdout,"%%%%Pages: %d\n",nPages);
}
void begineps(void)
{
	fprintf(stdout,"%%!PS-Adobe-2.0 EPSF-1.2\n");
	fprintf(stdout,"%%%%DocumentFonts:\n");
	if (BBoxSet) {
		fprintf(stdout,"%%%%BoundingBox: %d %d %d %d\n",
			BBox.llx,BBox.lly,BBox.urx,BBox.ury);
		BBoxOut = 1;
	} else {
		fprintf(stdout,"%%%%BoundingBox: (atend)\n");
	}
	fprintf(stdout,"%%%%EndComments\n");
	fprintf(stdout,"%s\n",Prologue);
	fprintf(stdout,"%%%%EndProlog\n");
	fprintf(stdout,"GS\n");
}

void endeps(void)
{
	fprintf(stdout,"GR\n");
	fprintf(stdout,"%%%%Trailer\n");
	if (!BBoxOut) {
		fprintf(stdout,"%%%%BoundingBox: %d %d %d %d\n",
			BBox.llx,BBox.lly,BBox.urx,BBox.ury);
		BBoxOut = 1;
	}
}

void newpage(const char *label, int ordinal)
{
	fprintf(stdout,"%%%%Page: %s %d\n",label,ordinal);
	nPages++;
}

void boundingbox(int llx, int lly, int urx, int ury)
{
	BBox.llx = llx;
	BBox.lly = lly;
	BBox.urx = urx;
	BBox.ury = ury;
	BBoxSet = 1;
}

void showpage(void)
{
	fprintf(stdout,"showpage\n"); 
}

void gsave(void)
{
	fprintf(stdout,"GS\n");
}

void grestore(void)
{
	fprintf(stdout,"GR\n");
}

void newpath(void)
{
	fprintf(stdout,"NP\n");
}

void closepath(void)
{
	fprintf(stdout,"CP\n");
}

void clip()
{
	fprintf(stdout,"clip\n");
}

void translate(float tx, float ty)
{
	fprintf(stdout,"%.4g %.4g TR\n",tx,ty);
}

void scale(float sx, float sy)
{
	fprintf(stdout,"%.4g %.4g SC\n",sx,sy);
}

void rotate(float angle)
{
	fprintf(stdout,"%.4g RO\n",angle);
}

void concat(float m[])
{
	fprintf(stdout,"[%.4g %.4g %.4g %.4g %.4g %.4g] CAT\n",
		m[0],m[1],m[2],m[3],m[4],m[5]);
}

void setgray(float gray)
{
	fprintf(stdout,"%.4g setgray\n",gray);
}

void setlinewidth(float width)
{
	fprintf(stdout,"%.4g SLW\n",width);
}

void setlinejoin(int code)
{
	fprintf(stdout,"%d setlinejoin\n",code);
}

void setdash(float dash[], int ndash, float offset)
{
	int i;
	fprintf(stdout,"[ ");
	for (i=0; i<ndash; i++)
		fprintf(stdout,"%.4g ",dash[i]);
	fprintf(stdout,"] %.4g setdash\n",offset);
}

void moveto(float x, float y)
{
	fprintf(stdout,"%.4g %.4g M\n",x,y); 
}

void rmoveto(float x, float y)
{
	fprintf(stdout,"%.4g %.4g RM\n",x,y); 
}

void lineto(float x, float y)
{
	fprintf(stdout,"%.4g %.4g L\n",x,y); 
}

void rlineto(float x, float y)
{
	fprintf(stdout,"%.4g %.4g RL\n",x,y); 
}

void arc(float x, float y, float r, float ang1, float ang2)
{
	fprintf(stdout,"%.4g %.4g %.4g %.4g %.4g arc\n",x,y,r,ang1,ang2);
}

void stroke(void)
{
	fprintf(stdout,"S\n"); 
}

void fill(void)
{
	fprintf(stdout,"F\n");
}

void show(const char *str)
{
	fprintf(stdout,"(%s) SH\n",str);
}

void justshow(float just, const char *str)
{
	fprintf(stdout,"(%s) SW exch %.4g mul\n",str,just);
	fprintf(stdout,"exch %.4g mul RM (%s) SH\n",just,str);
}

void image(int w, int h, int bps, float m[], unsigned char *samples)
{
	int nline=39,i,rowbytes,nbytes;
	char line[80],*linei,*hexi;
	static int hexbuilt=0;
	static char hex[513];

	/* build table of hex codes if not already built */
	if (!hexbuilt) {
		for (i=0; i<256; i++)
			sprintf(&hex[2*i],"%02.2x",i);
		hexbuilt = 1;
	}

	/* determine number of bytes per row and total number of bytes */
	rowbytes = 1+(w*bps-1)/8;
	nbytes = rowbytes*h;

	/* set up the image */
	fprintf(stdout,"/picstr %d string def\n",rowbytes);
	fprintf(stdout,"%d %d %d [%.4g %.4g %.4g %.4g %.4g %.4g]\n",
		w,h,bps,m[0],m[1],m[2],m[3],m[4],m[5]);
	fprintf(stdout,"{currentfile picstr readhexstring pop} image\n");

	/* encode and write the image in lines of 78 hex characters */
	while(nbytes) {
		if (nbytes<nline) nline = nbytes;
		for (i=0,linei=line; i<nline; i++) {
			hexi = hex+2*(*samples++);
			*linei++ = *hexi++;
			*linei++ = *hexi;
		}
		*linei++ = '\n';
		*linei = '\0';
		fputs(line,stdout);
		nbytes -= nline;
	}
}

void setfont(const char *fontname, float fontsize)
{
	fprintf(stdout,"/%s findfont %.4g scalefont setfont\n",fontname,fontsize);
}

void fontbbox(const char *fontname, float fontsize, float bbox[])
{
	int i = fontindex(fontname);
	int *b = FontMetrics[i].fontbbox;
	bbox[0] = b[0]*FONTSCALE*fontsize;
	bbox[1] = b[1]*FONTSCALE*fontsize;
	bbox[2] = b[2]*FONTSCALE*fontsize;
	bbox[3] = b[3]*FONTSCALE*fontsize;
}

float fontheight(const char *fontname, float fontsize)
{
	int i;
	float h;
	i = fontindex(fontname);
	h = FontMetrics[i].fontbbox[3] - FontMetrics[i].fontbbox[1];
	h *= FONTSCALE*fontsize;
	return h;
}

float fontwidth(const char *fontname, float fontsize)
{
	int i;
	float w;
	i = fontindex(fontname);
	w = FontMetrics[i].fontbbox[2] - FontMetrics[i].fontbbox[0];
	w *= FONTSCALE*fontsize;
	return w;
}

float fontcapheight(const char *fontname, float fontsize)
{
	int i = fontindex(fontname);
	return FontMetrics[i].capheight*FONTSCALE*fontsize;
}

float fontxheight(const char *fontname, float fontsize)
{
	int i = fontindex(fontname);
	return FontMetrics[i].xheight*FONTSCALE*fontsize;
}

float fontdescender(const char *fontname, float fontsize)
{
	int i = fontindex(fontname);
	return FontMetrics[i].descender*FONTSCALE*fontsize;
}

float fontascender(const char *fontname, float fontsize)
{
	int i = fontindex(fontname);
	return FontMetrics[i].ascender*FONTSCALE*fontsize;
}

void polyline(const float *x, const float *y, int n)
{
	int i;

	gsave();
	newpath();
	moveto(x[0],y[0]);
	for (i=1; i<n; i++) {
		lineto(x[i],y[i]);
		if (i%200==0) {
			stroke();
			moveto(x[i],y[i]);
		}
	}
	stroke();
	grestore();
}

void markto(float x, float y, int index, float size)
{
	gsave();
	translate(x,y);
	scale(size,size);
	fprintf(stdout,"CLW %0.4g div SLW\n",size);
	newpath();
	switch (index%NMARKS) {
	case MPLUS: /* plus */
		moveto(-0.5,0.0);
		rlineto(1.0,0.0);
		moveto(0.0,-0.5);
		rlineto(0.0,1.0);
		stroke();
		break;
	case MASTERISK: /* asterisk */
		moveto(-0.5,0.0);
		rlineto(1.0,0.0);
		moveto(-0.25,-0.433);
		rlineto(0.5,0.866);
		moveto(-0.25,0.433);
		rlineto(0.5,-0.866);
		stroke();
		break;
	case MCROSS: /* X */
		moveto(-0.5,-0.5);
		rlineto(1.0,1.0);
		moveto(-0.5,0.5);
		rlineto(1.0,-1.0);
		stroke();
		break;
	case MTRIANGLE: /* triangle */
		moveto(-0.5,-0.25);
		rlineto(1.0,0.0);
		rlineto(-0.5,0.809);
		closepath();
		stroke();
		break;
	case MSQUARE: /* square */
		moveto(-0.5,-0.5);
		rlineto(1.0,0.0);
		rlineto(0.0,1.0);
		rlineto(-1.0,0.0);
		closepath();
		stroke();
		break;
	case MCIRCLE: /* circle */
		arc(0.0,0.0,0.5,0.0,360.0);
		stroke();
		break;
	case MFILLEDTRIANGLE: /* filled triangle */
		moveto(-0.5,-0.25);
		rlineto(1.0,0.0);
		rlineto(-0.5,0.809);
		closepath();
		fill();
		break;
	case MFILLEDSQUARE: /* filled square */
		moveto(-0.5,-0.5);
		rlineto(1.0,0.0);
		rlineto(0.0,1.0);
		rlineto(-1.0,0.0);
		closepath();
		fill();
		break;
	case MFILLEDCIRCLE: /* filled circle */
		arc(0.0,0.0,0.5,0.0,360.0);
		fill();
		break;
	}
	grestore();
}

void rectclip(float x, float y, float width, float height)
{
	newpath();
	moveto(x,y);
	rlineto(width,0.0);
	rlineto(0.0,height);
	rlineto(-width,0.0);
	closepath();
	clip();
	newpath();
}

void rectfill(float x, float y, float width, float height)
{
	gsave();
	newpath();
	moveto(x,y);
	rlineto(width,0.0);
	rlineto(0.0,height);
	rlineto(-width,0.0);
	closepath();
	fill();
	grestore();
}

void rectstroke(float x, float y, float width, float height)
{
	gsave();
	newpath();
	moveto(x,y);
	rlineto(width,0.0);
	rlineto(0.0,height);
	rlineto(-width,0.0);
	closepath();
	stroke();
	grestore();
}
