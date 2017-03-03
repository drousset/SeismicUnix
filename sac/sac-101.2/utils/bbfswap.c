/** 
 * Copyright (c) 2008, Brian Savage < savage _AT_ uri.edu >
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:
 * 
 * * Redistributions of source code must retain the above copyright 
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright 
 *   notice, this list of conditions and the following disclaimer in 
 *   the documentation and/or other materials provided with the distribution.
 * * Neither the name of the author nor the names of its contributors may be 
 *   used to endorse or promote products derived from this software without 
 *   specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND 
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR 
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE 
 * USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 *
 * The views and conclusions contained in the software and documentation are 
 * those of the authors and should not be interpreted as representing official
 * policies, either expressed or implied.
 *
 * BBF Swap 
 * BlackBoard File Swap
 *   The program was written to efficiently swap Blackboard varaible files
 *   produced using SAC (Seismic Analysis Code)
 *
 * To compile
 *   % cc -o bbfswap bbfswap.c
 *
 * To run
 *   % ./bbfswap input_bbf_file
 *   %    The output will be input_bbf_file.swp
 *   % ./bbfswap -o input_bbf_file
 *   %    The bbf_file will be overwritten
 *   % ./bbfswap -v input_bbf_file
 *   %    Detail about the bbf_file will displayed 
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

struct Header {
  long int delete;
  long int readonly;
  long int indirect;
  long int shared;
  long int reserved;
  long int AppBit1;
  long int AppBit2;
  long int type;
  long int namelength;
  long int valuelength;
  long int descriptionlength;
};

#define DELETEBIT      (1UL<<31)  /* 1st  */
#define READONLY       (1<<30)    /* 2nd  */
#define INDIRECT       (1<<29)    /* 3rd  */
#define SHARED         (1<<28)    /* 4th  */
#define RESERVED       (1<<27)    /* 5th  */
#define APPBIT1        (1<<21)    /* 11th */
#define APPBIT2        (1<<20)    /* 12th */

#define TYPESHIFT      22
#define TYPEBIT        037

#define NAMESHIFT      17
#define NAMEBIT        07

#define VALUEISLONGBIT (1<<16)    /* 16th */
#define VALUEBIT       (0177777)
#define VALUESHIFT     22

static int verbose = 0;

#define BBFSWAP              "bbfswap"
#define BBF_HEADER_IDENTITY  "VARS"
#define BBF_HEADER_VERSION   1L
#define BBF_VARS_HEADER_SIZE sizeof(long int)

#define TRUE   1
#define FALSE  0

#define MESSAGE_INFO       1
#define MESSAGE_DEBUG      2

typedef struct __vars vars;
struct __vars {
  long int hdr;
  char *name;
  char *value;
  int namelength;
  int valuelength;
};

typedef struct __bbf bbf;
struct __bbf {
  char id[5];
  long int ver;
  long int hdr;
  char *name;
  vars *v;
  int n;
  int namelength;
};

void
error(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, BBFSWAP ": Error: ");
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  exit(-1);
}

void
message(int type, char *fmt, ...) {
  va_list ap;
  if(verbose >= type) {
    fprintf(stderr, BBFSWAP ": ");
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
  }
}

void
usage() {
  fprintf(stderr, 
	  "Usage: " BBFSWAP " [-vo] [-x entension] inputfiles\n"
	  "    Convert Blackboard variable files (BBF) between byte orders\n"
	  "    -v Verbose Mode\n"
	  "    -x extension Added to the end of the files\n"
	  "       Default .swp\n"
	  "    -o Overwrite the original file\n"
	  );
}

void
swap(char *p, int size) {
  int i;
  char tmp;

  for(i = 0; i < size/2; i++) {
    tmp         = p[i];
    p[i]        = p[size-i-1];
    p[size-i-1] = tmp;
  }
}

bbf *
bbf_new() {
  bbf *b;
  message(MESSAGE_DEBUG, "Creating Internal BBF Storage\n");
  b = (bbf *) malloc(sizeof(bbf));
  b->id[0] = 0;
  b->ver   = 0;
  b->hdr   = 0;
  b->name  = NULL;
  b->v     = NULL;
  b->n     = 0;
  return b;
}

void
bbf_set_header(bbf           *b, 
	       struct Header *h,
	       char          *id, 
	       long int       ver, 
	       long int       hdr, 
	       char          *name) {
  int n;
  message(MESSAGE_DEBUG, "Setting BBF Header Values\n");
  if(!b) 
    return;
  b->ver = ver;
  b->hdr = hdr;
  message(MESSAGE_DEBUG, "Setting name\n");
  n = (h->namelength * 4);
  b->name = (char *) malloc(sizeof(char) * (n+1));
  strcpy(b->name, name);
  b->namelength = n;

  message(MESSAGE_DEBUG, "Setting id\n");
  strcpy(b->id, id);

  return;
}

void
bbf_set_var(bbf           *b,
	    struct Header *h,
	    long int       hdr,
	    char          *name,
	    char          *value) {
  int k;
  int n = b->n;
  
  (b->n)++;
  b->v = (vars *) realloc(b->v, sizeof(vars) * b->n);

  (b->v[n]).hdr   = hdr;
  k = (h->namelength * 4) ;
  b->v[n].name  = (char *) malloc(sizeof(char) * (k+1));
  strcpy(b->v[n].name, name);
  b->v[n].namelength = k;

  k = (h->valuelength * 4);
  b->v[n].value  = (char *) malloc(sizeof(char) * (k+1));
  strcpy(b->v[n].value, value);
  b->v[n].valuelength = k;

  return;
}

void
bbf_write(bbf *b, char *output, int doswap) {
  int i;
  FILE *fp;

  message(MESSAGE_INFO, "Writing out bbf: %s\n", output);
  if((fp = fopen(output, "w")) == NULL) {
    error("opening file for writing: %s\n", output);
  }

  message(MESSAGE_DEBUG, "id:  >%s<\n", b->id);
  fwrite(b->id,  sizeof(char), strlen(BBF_HEADER_IDENTITY), fp); 

  message(MESSAGE_DEBUG, "ver: %lu\n", b->ver);
  if(doswap) 
    swap((char *)&(b->ver), sizeof(long int));
  fwrite(&(b->ver), sizeof(b->ver), 1, fp);

  message(MESSAGE_DEBUG, "hdr: %lu\n", b->hdr);
  if(doswap) 
    swap((char *)&(b->hdr), sizeof(long int));
  fwrite(&(b->hdr), BBF_VARS_HEADER_SIZE, 1, fp);

  message(MESSAGE_DEBUG, "name: >%s<\n", b->name);
  fwrite(b->name, sizeof(char), b->namelength, fp);

  for(i = 0; i < b->n; i++) {
    message(MESSAGE_DEBUG, "   hdr:   %lu\n", b->v[i].hdr);
    if(doswap) 
      swap((char *)&(b->v[i].hdr), sizeof(long int));

    if(fwrite(&(b->v[i].hdr), BBF_VARS_HEADER_SIZE, 1, fp) != 1) {
      error("writing variable header\n");
    }
    message(MESSAGE_DEBUG, "   name[%d]:  >%s<\n", b->v[i].namelength, b->v[i].name);
    
    if(fwrite(b->v[i].name, sizeof(char), b->v[i].namelength, fp) != b->v[i].namelength) {
      error("writing variable name\n");
    }
    message(MESSAGE_DEBUG, "   value[%d]: >%s<\n", b->v[i].valuelength, b->v[i].value);
    if(fwrite(b->v[i].value, sizeof(char), b->v[i].valuelength, fp) != b->v[i].valuelength) {
      error("writing variable value\n");
    }
  }
  message(MESSAGE_DEBUG, "closing file\n");
  fclose(fp);
  return;
}


unsigned long int 
encodeHeader(struct Header *h) {
  unsigned long int *p;
  unsigned long int s;
  s = 0L;
  /* Turn on all the correct Bits */
  if(h->delete)
    s |= DELETEBIT;
  if(h->readonly)
    s |= (READONLY);
  if(h->indirect)
    s |= (INDIRECT);
  if(h->shared)
    s |= (SHARED);
  if(h->reserved)
    s |= (RESERVED);
  if(h->AppBit1)
    s |= (APPBIT1);
  if(h->AppBit2)
    s |= (APPBIT2);

  /* Set the Type */
  s |= ( h->type  & TYPEBIT ) << TYPESHIFT;
  /* Set the Name Length */
  s |= (( h->namelength - 1 ) & NAMEBIT ) << NAMESHIFT;

  /* Set the Value Length */
  if(h->valuelength -1 > VALUEBIT) {
    s |= VALUEISLONGBIT;
    *p = s;
    *++p = (h->valuelength - 1L);
  } else {
    s |= ((h->valuelength - 1L) & VALUEBIT);
  }

  return s;
}

struct Header *
decodeHeader(long int z) {
  struct Header *h;
  h = (struct Header *) malloc(sizeof(struct Header));
  /* Are the specific Bits ON ? */
  h->delete   = ( (z & DELETEBIT)   != 0);
  h->readonly = ( (z & READONLY) != 0);
  h->indirect = ( (z & INDIRECT) != 0);
  h->shared   = ( (z & SHARED)   != 0);
  h->reserved = ( (z & RESERVED) != 0);
  h->AppBit1  = ( (z & APPBIT1)  != 0);
  h->AppBit2  = ( (z & APPBIT2)  != 0);
 
  /* Get the Type */
  h->type              = (z >> TYPESHIFT) & TYPEBIT;
  /* Get the Length of the Name in 4 byte words */
  h->namelength        = ((z >> NAMESHIFT) & NAMEBIT) + 1 ;
  /* Get the Description Length */
  h->descriptionlength = (z & VALUEISLONGBIT) ? 2 : 1;
  /* Get the Value Length */
  h->valuelength       = (z & VALUEISLONGBIT) ? (z+1) : (z & VALUEBIT) + 1;
  return h;
}

char *
int2bin(unsigned long int a) {
  char *str,*tmp;
  int cnt = 31;
  str = (char *) malloc(sizeof(char) * 33); /*32 + 1 , becoz its a 32 bit bin number*/
  tmp = str;
  while ( cnt > -1 ){
    str[cnt]= '0';
    cnt --;
  }
  cnt = 31;
  while (a > 0){
    if (a%2==1){
      str[cnt] = '1';
    }
    cnt--;
    a = a/2 ;
  }
  return tmp;   
  
}

void
dumpHeader(struct Header *h) {
  message(MESSAGE_DEBUG, "Delete:      %ld\n", h->delete);
  message(MESSAGE_DEBUG, "Readonly:    %ld\n", h->readonly);
  message(MESSAGE_DEBUG, "Indirect:    %ld\n", h->indirect);
  message(MESSAGE_DEBUG, "Shared:      %ld\n", h->shared);
  message(MESSAGE_DEBUG, "Reserved:    %ld\n", h->reserved);
  message(MESSAGE_DEBUG, "AppBit1:     %ld\n", h->AppBit1);
  message(MESSAGE_DEBUG, "AppBit2:     %ld\n", h->AppBit2);
  message(MESSAGE_DEBUG, "Type:        %ld\n", h->type);
  message(MESSAGE_DEBUG, "Description: %ld\n", h->descriptionlength);
  message(MESSAGE_DEBUG, "NameLength:  %ld 4-byte words\n", h->namelength);
  message(MESSAGE_DEBUG, "ValueLength: %ld 4-byte words\n", h->valuelength);
}

bbf *
bbf_read(char *input, int *doswap) {
  FILE *fp;
  bbf *b;
  long int ver;
  unsigned long int hdr;
  char id[5];
  char *name, *value;
  struct Header *h;

  message(MESSAGE_INFO, "Reading in bbf: %s\n", input);

  /* Open the file for reading */
  if((fp = fopen(input, "r")) == NULL) {
    error("reading in file: %s\n", input);
  }
  /* Read in the Identifier */
  fread(&id[0], sizeof(char), strlen(BBF_HEADER_IDENTITY), fp);
  if(strncasecmp(BBF_HEADER_IDENTITY, id, strlen(BBF_HEADER_IDENTITY)) != 0) {
    error("Input is not a Blackboard Variable File: %s\n", input);
  }
  id[strlen(BBF_HEADER_IDENTITY)] = 0;
  message(MESSAGE_INFO, "Identify:  %s\n", id);
  
  /* Read in the Version Number */
  fread(&ver, sizeof(BBF_HEADER_VERSION), 1, fp);
  message(MESSAGE_INFO, "Version:     %lu \n", ver);
  *doswap = FALSE;
  if(ver != BBF_HEADER_VERSION) {
    message(MESSAGE_INFO, "Header Version is unrecognized\n");
    swap((char *) &ver, sizeof(long int));
    if(ver != BBF_HEADER_VERSION) {
      error("Blackboard variable File has incorrect version number: %s\n", input);
    }
    *doswap = TRUE;
    message(MESSAGE_INFO, "Version:     %lu  (After Swap)\n", ver);
  }
  /* Read in the Header for the Entire file */
  fread(&hdr, sizeof(long int), 1, fp);
  message(MESSAGE_DEBUG, "Header:      %lu\n", hdr);
  if(*doswap) {
    swap((char*)&hdr, sizeof(long int));
  }  
  message(MESSAGE_DEBUG, "Header:      %lu\n", hdr);
  message(MESSAGE_DEBUG,
	  "%s\n         %s\n", int2bin(hdr), int2bin(encodeHeader(decodeHeader(hdr))));
  
  /* Decode the Header */
  h = decodeHeader(hdr);
  dumpHeader(h);
  
  /* Make space for the name and read it in */
  name = (char *)malloc(sizeof(char) * ((h->namelength * 4) + 1));
  fread(&name[0], sizeof(char), (h->namelength * 4), fp);
  name[h->namelength * 4] = 0;
  message(MESSAGE_INFO, "filename: %s\n", name);
  
  b = bbf_new();
  /* Write Identifies, Version Number, Header, and Name */
  bbf_set_header(b, h, id, ver, hdr, name);
  
  free(name);
  name  = NULL;
  value = NULL;
  
  /* Read in a Header for each Value */
  while(fread(&hdr, BBF_VARS_HEADER_SIZE, 1, fp) == 1) {
    message(MESSAGE_DEBUG, "------------------------------------\n");
    message(MESSAGE_DEBUG, "Header:      %lu\n", hdr);
    if(*doswap) {
      swap((char*)&hdr, sizeof(long int));
    }
    message(MESSAGE_DEBUG, "Header:      %lu\n", hdr);
    message(MESSAGE_DEBUG, 
	    "%s\n         %s\n", int2bin(hdr), int2bin(encodeHeader(decodeHeader(hdr))));
    
    /* Decode the Header */
    h = decodeHeader(hdr);
    dumpHeader(h);
    
    /* Read in the Name */
    name = (char *)realloc(name, sizeof(char) * ((h->namelength * 4) + 1));
    if(fread(&name[0], sizeof(char), (h->namelength * 4), fp) != (h->namelength * 4)) {
      error("reading in variable name\n");
    }
    name[h->namelength * 4] = 0;
    message(MESSAGE_INFO, "Name:       \"%s\"\n", name);
    
    /* Read in the Value */
    value = (char *)realloc(value, sizeof(char) * ((h->valuelength * 4) + 1));
    if(fread(&value[0], sizeof(char), (h->valuelength * 4), fp) != (h->valuelength * 4)) {
      error("reading in variable value\n");
    }
    value[h->valuelength * 4] = 0;
    message(MESSAGE_INFO, "Value:      \"%s\"\n", value);
    
    /* Write the variable out */
    bbf_set_var(b, h, hdr, name, value);
    
    /* Break when we have com to the end */
    if(strncmp(name, "NIL", 3) == 0) {
      break;
    }
  }
  message(MESSAGE_DEBUG, "closing file, moving to next file\n");
  fclose(fp);
  return b;
}



int
main(int argc, char *argv[]) {

  int i;
  int doswap;
  char *input;
  char *output;
  char *ext;
  int options;
  bbf *b;

  ext = NULL;
  i = 1;
  options = TRUE;
  if(argc <= 1) 
    usage();

  while(i < argc) {
    if(argv[i][0] == '-' && options) {
      switch(argv[i][1]) {
      case 'v': 
	verbose++;
	message(MESSAGE_DEBUG, "Verbose Mode On\n");
	break;
      case 'x':
	i++;
	ext = strdup(argv[i]);
	break;
      case 'o':
	ext = strdup("");
	break;
      }
    } else {
      /* Turn off Options, Create Extension, run through files */
      if(options == TRUE) {
	if(ext == NULL) {
	  ext = strdup(".swp");
	}
	options = FALSE;
      }
      
      /* Create Output File */
      input = argv[i];
      output = (char *)malloc(sizeof(char) * (strlen(input) + strlen(ext) + 1));
      sprintf(output, "%s%s", input,ext);

      b = bbf_read(input, &doswap);

      bbf_write(b, output, !doswap);

      free(output);
    }
    i++;
  }
  exit(0);
  
}

