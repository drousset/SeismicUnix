
#include "su.h"
#include "segy.h"

void getval(string type, Value *val, double *vd);


/* hdsearch() searches input segy headers to position "fp" to the begining of
   the trace indicated by Value "valp" of keyword "key", return position of
   fp in bytes  

	Tape input: search not valid, return the begining of tape 
	Pipe input: search not valid, return the begining of pipe 
	Disk input: search is valid, return the begining of trace position 
	Other input: error, job aborted

  Input:
	FILE *fp	file pointer of input segy dataset	
	char *key	keyword to search (see segy.h)
	double valp	Value of keyword to search
  Output:
	FILE *fp 	file pointer to the first trace whose keyword is
			equal to or less than valp
  Return:
	int hdsearch()	position of the first trace found in bytes from the
			begining of the input file 
  Notes:
	valp is a DOUBLE floating point number

 Author:	Zhiming Li		      		6/22/92
		
*/ 

int hdsearch(FILE *fp, char *key, double valp) {
	
	filetype ftype; 
	int startbyte;

	switch (ftype = filestat(fileno(fp))) {
                case TAPE:
			startbyte = 0;
			return startbyte;
                case PIPE:
			startbyte = 0;
			return startbyte;
		case DISK:
			startbyte = disksearch(fp,key,valp);
			efseek(fp,startbyte,0);
			return startbyte;
                default:
			err(" unsupported input filetype %s",
				printstat(fileno(fp)));
			return -1; /* not executed */
	}

}

/* disksearch() searches input segy headers to position "fp" to the begining of
   the trace indicated by Value "valp" of keyword "key", return position of
   fp in bytes  

  Input:
	FILE *fp		file pointer of input segy DISK dataset	
	char *key		keyword to search (see segy.h)
	double valp		Value of keyword to search
  Output:
	FILE *fp 		file pointer to the first trace whose keyword is
				equal to or less than valp
  Return:
	int disksearch()	position of the first trace found in bytes 
				from the begining of the input DISK file 

Note:

FILE *fp and int disksearch will be
	the end position of input if last trace's  Value(key)<valp, or 
	the first trace position of input if its Value(key)>=valp, or 
	the first trace position of input whose Value(key)>=valp, otherwise 

 Author:	Zhiming Li		      		6/22/92
		
*/ 

int disksearch(FILE *fp, char *key, double valp) {
	segytrace tra;
	int index, idbytes, nsegy;
	int x1, x2, x3, x, startbyte,iter=0;
	double y1, y2, y3, y; 
	Value val;
	string type;

	/* get index and type of key */
	index = getindex(key);
	type = hdtype(key);
	/* 3600 bytes of id headers */
	idbytes = EBCBYTES + BNYBYTES;

	x = 0;
	y = valp;
	
	/* find Value of keyword at first trace */
	efseek(fp,idbytes,0);
	efread((char*)&tra, 1, HDRBYTES, fp);
	gethval(&tra, index, &val);
	getval(type, &val, &y1);
	x1 = 0;

	/* trace length*/
	nsegy = tra.ns*sizeof(float)+HDRBYTES;

	/* find Value of keyword at last trace */
	efseek(fp, 0, SEEK_END);
	x2 = (eftell(fp)-idbytes)/nsegy - 1;
	efseek(fp, x2*nsegy+idbytes, 0);
	efread((char*)&tra, 1, HDRBYTES, fp);
	gethval(&tra, index, &val);
	getval(type, &val, &y2);

	/* see if search is needed */
	if(y2<y1) {
		err(" Input header %s Value not in increasing order ",key); 
	} else if(y2<y) {
		x = (x2+1)*nsegy + idbytes;	
		efseek(fp, x, 0);
		return x;
	} else if(y1>=y) {
		x = idbytes;	
		efseek(fp, x, 0);
		return x;
	} else if(y2==y1) {
		x = idbytes;	
		efseek(fp, x, 0);
		return x;
	} 

/* bineary search */
search:
	x3 = (x1 + x2)/2;
	/*
	iter++;
	fprintf(stderr,"x1=%d x2=%d x3=%d iter=%d \n",x1,x2,x3,iter);
	fprintf(stderr,"y1=%f y2=%f y=%f \n",y1,y2,y);
	*/

	if(x3>x1 && x3<=x2) {
		efseek(fp, x3*nsegy+idbytes, 0);
		efread((char*)&tra, 1, HDRBYTES, fp);
		gethval(&tra, index, &val);
		getval(type,&val,&y3);
		if (y3>=y) {
			x2 = x3;
			y2 = y3;
		} else {
			x1 = x3;
			y1 = y3;
		}
		if(y2>y1) goto search;
	}
	
	x = (x1+1)*nsegy + idbytes;
	efseek(fp, x, 0);
	return x;

}

/* getval() converts union Value "val" to its Value, accoring to "type" of 
	the Value, and returns the Value as double Value "vd"	

Author: Zhiming Li		       		6/22/92		*/
 
void getval(string type, Value *val, double *vd) {
        switch (*type) {
        case 's':
                err("can't change char header word");
        break;
        case 'h':
                *vd = val->h;
        break;
        case 'u':
                *vd = val->u;
        break;
        case 'l':
                *vd = val->l;
        break;
        case 'v':
                *vd = val->v;
        break;
        case 'i':
                *vd = val->i;
	break;
        case 'p':
                *vd = val->p;
        break;
        case 'f':
                *vd = val->f;
        break;
        case 'd':
                *vd = val->d;
        break;
        default:
                err("unknown type %s", type);
        break;
        }
}


/* test main program

segytrace tr;
main(int argc, char **argv)
{
	int start, v;
	char *key;
	string type;
	int startbytes;
	FILE *fp=stdin;
	double vd;
	Value val;

        initargs(argc, argv);
	
	getparstring("key",&key); 
	getparint("start",&start); 

	type = hdtype(key);
	
	vd = start;
	
	fprintf(stderr,"before hdsearch \n");

	startbytes = hdsearch(fp, key, vd); 

	fprintf(stderr,"after hdsearch \n");

	fgettr(fp,&tr);
	gethdval(&tr, key, &val);

	getval(type, &val, &vd);
	v = vd;
	fprintf(stderr,"key=%s start=%d startbytes=%d gethdval=%d \n",
		key,start,startbytes,v);
}
*/
