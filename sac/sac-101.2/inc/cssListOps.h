#ifndef CSS_LIST_OPS_H
#define CSS_LIST_OPS_H


#ifndef NULL
#       define NULL 0
#endif 



DBlist dblNewTree(void);
DBtable dblCreateTableInstance(DBlist list, dblObject StrucType);
void dblDeleteTableInstance(dblObject StrucType, DBlist dblList,  DBtable pntr);
void dblDeleteTree(DBlist Tree);
void dblCopyTable(dblObject StrucType,  DBtable origPntr, DBtable copyPntr);
void dblCopyTableElement(dblObject StrucType, DBelement origPntr, DBtable copyPntr);
DBlist dblCopyTree(DBlist OldTree);
int dblSortWfdiscList(DBlist TreePtr, long *Index, int Nitems);
int dblDeleteWfdiscs(DBlist tree, int *index, int Nitems);
DBlist dblMergeTrees(DBlist tree2, DBlist tree, long takeEvid );
int dblGetNumWaveformsInMemory(DBlist tree);

DBtable dblGetTableInstance(DBobj targetObj, dblObject specifier, long index);
DBobj dblGetTableObject(DBobj targetObj, dblObject specifier);
DBtable dblNextTableInstance(DBtable tableInstance, DBlist dblList, dblObject specifier);
void dblSetTraceData(DBtable tableInstance, DataType dtype, void *data, long npts);
long dblGetTableIndex(dblObject StrucType,  DBtable pntr);
int dblAddComment(DBlist list, dblObject StrucType, DBtable rowPntr, const char* comment);
char** dblGetComments(DBlist list, dblObject StrucType, DBtable rowPntr, int *Nlines);

       /* *****************User Data Access Functions****************** */
void dblSetUserDataComment(DBlist list, const char *comment);
char * dblGetUserDataComment(DBlist list);
void dblSetUserDataMatrixComment(DBlist list, const char *comment);
char * dblGetUserDataMatrixComment(DBlist list);
void dblSetUserDataMatrixData(DBlist list, long Nrows, long Ncols, 
			      ComplexFloat **matrix);
ComplexFloat **dblGetUserDataMatrixData(DBlist list, long *Nrows, long *Ncols);
DBdataComm dblCreateDataComment(DBlist list);
void dblSetUserDataComComment(DBdataComm DCpntr, const char *comment);
void dblDeleteUserDataComStruct(DBdataComm DCpntr, DBlist list);
DBdataComm dblGetNextDataComment(DBdataComm DCpntr, DBlist list);
char *dblGetUserDataComComment(DBdataComm DCpntr);
DBdataComm dblGetNextMatchingDataComment(DBdataComm DCpntr, DBlist list, 
					 char * comment);
DBdataComm dblGetNextDCMatchingIndex(DBdataComm DCpntr, DBlist list, long index); 
void dblSetDCIndex(DBdataComm DCpntr, long index); 
long dblGetDCIndex(DBdataComm DCpntr); 
void dblSetDCType(DBdataComm DCpntr, enum dataType type);
void dblSetSacUserNum(DBdataComm DCpntr, int index);
int dblGetSacUserNum(DBdataComm DCpntr);
DBdataComm dblGetNextDCSacUserMatch(DBlist list, long index, int Num);

 
enum dataType dblGetDCType(DBdataComm DCpntr); 

void dblSetUserDataComData(DBdataComm DCpntr, const ComplexFloat *data, long Npts);
ComplexFloat *dblGetUserDataComData(DBdataComm DCpntr, long *Npts);
int dblGetSeismograms( struct wfdiscList *wfStruc, char*, char*);
int dblWriteUserData(DBlist list, char * filename);
int dblReadUserData(DBlist list, char * filename, int OverWrite);


int dblWfidInUse(DBlist tree, long wfid);
void dblReplaceWfid(DBlist tree, long OldWfid, long NewWfid);
long dblNextAvailableWfid(DBlist tree);
struct originList *dblOridInUse(DBlist tree, long orid);
long dblNextAvailableOrid(DBlist tree);
void dblReplaceOrid(DBlist tree, long OldOrid, long NewOrid);
struct eventList *dblEvidInUse(DBlist tree, long evid);
long dblNextAvailableEvid(DBlist tree);
void dblReplaceEvid(DBlist tree, long OldEvid, long NewEvid);
int dblAridInUse(DBlist tree, long arid);
long dblNextAvailableArid(DBlist tree);
void dblReplaceArid(DBlist tree, long OldArid, long NewArid);
int dblChanidInUse(DBlist tree, long chanid);
long dblNextAvailableChanid(DBlist tree);
void dblReplaceChanid(DBlist tree, long OldChanid, long NewChanid);
int dblInidInUse(DBlist tree, long inid);
long dblNextAvailableInid(DBlist tree);
void dblReplaceInid(DBlist tree, long OldInid, long NewInid);


char *MakeUniqueSiteName(DBlist tree, char *NewName);
char *MakeUniqueChanName(DBlist tree, char *sta, char *NewChan);
char *CSSstrcpy(char *fresh, char *old);
int CSSstrcmp(char *s1, char *s2);



/*             ****** Functions check for NULL valued CSS header fields. ******     */
int isValidFloat ( int table , int field , double value );
int isValidInt ( int table , int field , long value );
int isValidString ( int table , int field , char *value );


/*             ****** Functions to print formatted data from tree ******            */
void dblPrintUserDataInstance(DBdataComm DCpntr, FILE *unit);
void dblPrintUserDataList(DBlist list, FILE *unit);

void dblDumpTable(DBlist list, dblObject StrucType, FILE *unit,...);
void dblPrintTableInstance(DBtable tableInstance,  dblObject specifier, FILE *unit);
void dblTableOfContents(DBlist dblList, FILE *unit);

userData *dblCreateUserDataStruc(DBlist list);
void dblDestroyUserDataStruc(DBlist list);

struct trace * dblGetData ( long int index ) ;

#define AF_LIST (struct affiliationList *)
#define AR_LIST (struct arrivalList *)
#define OR_LIST (struct originList *)
#define RE_LIST (struct remarkList *)
#define WF_LIST (struct wfdiscList *)
#define WT_LIST (struct wftagList *)
#define AS_LIST (struct assocList *)
#define EV_LIST (struct eventList *)
#define GR_LIST (struct gregionList *)
#define IN_LIST (struct instrumentList *)
#define OE_LIST (struct origerrList *)
#define SE_LIST (struct sensorList *)
#define SL_LIST (struct siteList *)
#define SC_LIST (struct sitechanList *)
#define UD_LIST (userData *)
#define SA_LIST (struct stassocList *)
#define SD_LIST (struct sacdataList *)



#define NULL_AF_LIST AF_LIST NULL  
#define NULL_AR_LIST AR_LIST NULL
#define NULL_OR_LIST OR_LIST NULL
#define NULL_RE_LIST RE_LIST NULL
#define NULL_WF_LIST WF_LIST NULL
#define NULL_WT_LIST WT_LIST NULL
#define NULL_AS_LIST AS_LIST NULL
#define NULL_EV_LIST EV_LIST NULL
#define NULL_GR_LIST GR_LIST NULL
#define NULL_IN_LIST IN_LIST NULL
#define NULL_OE_LIST OE_LIST NULL
#define NULL_SE_LIST SE_LIST NULL
#define NULL_SL_LIST SL_LIST NULL
#define NULL_SC_LIST SC_LIST NULL
#define NULL_UD_LIST UD_LIST NULL
#define NULL_SA_LIST SA_LIST NULL
#define NULL_SD_LIST SD_LIST NULL



#endif
