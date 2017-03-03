#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "config.h"
#include "smMemory.h"


static unsigned long BytesAllocated   = 0;
static unsigned long TotalSystemPages = 0;
static unsigned long PageSize         = 0;

typedef struct MEMLIST{
   void *ptr;
   size_t size;
   struct MEMLIST *prev;
   struct MEMLIST *next;
}Memlist;
Memlist *MemHead = 0;
Memlist *MemTail = 0;


static void AddToMemList(void *ptr, size_t size)
{
   Memlist *NewElem = (Memlist*) malloc(sizeof(Memlist) );
   NewElem->ptr     = ptr;
   NewElem->size    = size;
   NewElem->prev    = 0;
   NewElem->next    = 0;
   if(!MemTail){
      MemHead = NewElem;
      MemTail = NewElem;
   }
   else{
      NewElem->prev = MemTail;
      MemTail->next = NewElem;
      MemTail = NewElem;
   }
   BytesAllocated += size;
}
/* --------------------------------------------------- */


static void DropFromMemList(void *ptr)
{
   Memlist *curElem = MemHead;
   while(curElem){
      if(curElem->ptr == ptr){
         BytesAllocated -= curElem->size;
         if(curElem->prev)
            curElem->prev->next = curElem->next;
         if(curElem->next)
            curElem->next->prev = curElem->prev;
         if(curElem == MemHead)
            MemHead = curElem->next;
         if(curElem == MemTail)
            MemTail = curElem->prev;
         free(curElem);
         return;
      }
      curElem = curElem->next;
   }
}
/* --------------------------------------------------- */



      

void * smMalloc(size_t size)
{
   void *ptr = malloc(size);
   AddToMemList(ptr, size);
   return ptr;
}
/* --------------------------------------------------- */


void *smCalloc(size_t count, size_t size)
{
   void *ptr = calloc(count, size);
   AddToMemList(ptr, count * size);
   return ptr;
}
/* --------------------------------------------------- */



void *smRealloc(void *ptr, size_t size)
{
   DropFromMemList(ptr);
   ptr = realloc(ptr, size);
   AddToMemList(ptr, size);
   return ptr;
}
/* --------------------------------------------------- */




void smFree(void *ptr)
{
   DropFromMemList(ptr);
   free(ptr);
}
/* --------------------------------------------------- */




size_t smTotalPhysicalBytes(void)
{
#ifdef SOLARIS
   if(!TotalSystemPages)TotalSystemPages = sysconf(_SC_PHYS_PAGES);
   if(!PageSize) PageSize                = sysconf(_SC_PAGESIZE);
   return TotalSystemPages * PageSize;
#endif
#ifdef LINUX
   if(!TotalSystemPages)TotalSystemPages = sysconf(_SC_PHYS_PAGES);
   if(!PageSize) PageSize                = sysconf(_SC_PAGESIZE);
   return TotalSystemPages * PageSize;
#endif
#ifdef OSX
   return 200000000;   
#endif
}
/* --------------------------------------------------- */


               


size_t smAvailPhysicalBytes(void)
{
#ifdef SOLARIS
   unsigned long AvailSystemPages = sysconf(_SC_AVPHYS_PAGES);
   if(!PageSize) PageSize                = sysconf(_SC_PAGESIZE);
   return AvailSystemPages * PageSize;
#endif
#ifdef LINUX
   unsigned long AvailSystemPages = sysconf(_SC_AVPHYS_PAGES);
   if(!PageSize) PageSize                = sysconf(_SC_PAGESIZE);
   return AvailSystemPages * PageSize;
#endif
#ifdef OSX
   return 200000000;   
#endif
}
/* --------------------------------------------------- */


size_t smAllocatedBytes(void)
{
   return BytesAllocated;
}
/* --------------------------------------------------- */


double smFracPhysMemUsed(void)
{
   return ((double) BytesAllocated) / ((double) smTotalPhysicalBytes() );
}
/* --------------------------------------------------- */


void smPrintContents(void)
{
   unsigned Address;
   Memlist *curElem = MemHead;
   while(curElem){
      Address = (unsigned) curElem->ptr;
      printf("Address = 0x%x\t\t Size = %d\n",Address, curElem->size);
      curElem = curElem->next;
   }   
}
/* --------------------------------------------------- */

