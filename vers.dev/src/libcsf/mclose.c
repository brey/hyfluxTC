#ifndef lint
 static const char *rcs_id =
 "$Header: /home/pcrtree/SRC.RCS/libs/csf/RCS/mclose.c,v 2.1 1996/12/29 19:35:21 cees Exp $";
#endif
 
/*******************************************************/
/*	Mclose.c				       */
/*******************************************************/
/*	close map and if write permission write all    */
/*	resident data to file                  	       */
/*******************************************************/
 
#include "csf.h"
#include "csfimpl.h"
 
#include <string.h>  /* memset() */
 
/* close a map
 * the Mclose function closes a map
 * if the map is being used for output
 * all header data is rewritten first
 * returns Upon succesful completion 0 is returned.
 * Otherwise, a non-zero value is returned
 *
 * Merrno
 * WRITE_ERROR (map descriptor still in tact if this happens)
 */
int Mclose(
	MAP *m) /* map to close, map descriptor
	           * is deallocated
	           */
{
	CHECKHANDLE_GOTO(m, error);
 
	if (m->minMaxStatus == MM_WRONGVALUE)
	{
		CsfSetVarTypeMV( &(m->raster.minVal), m->raster.cellRepr);
		CsfSetVarTypeMV( &(m->raster.maxVal), m->raster.cellRepr);
	}
 
	/* if write permission , write all header data to file */
	if (WRITE_ENABLE(m))
	{
		char filler[MAX_HEADER_FILL_SIZE];
    		(void)memset(filler, 0x0, MAX_HEADER_FILL_SIZE);
 
		if (m->main.byteOrder != ORD_OK) {
		 CsfSwap((void*)&(m->raster.minVal), CELLSIZE(m->raster.cellRepr),1);
		 CsfSwap((void*)&(m->raster.maxVal), CELLSIZE(m->raster.cellRepr),1);
		}
 
		fseek(m->fp,ADDR_MAIN_HEADER,SEEK_SET);
		if(m->write((void*)&(m->main.signature),sizeof(char), CSF_SIG_SPACE,m->fp)
		                                                   != CSF_SIG_SPACE ||
		   m->write((void*)&(m->main.version),sizeof(UINT2),1,m->fp)!=1 ||
		   m->write((void*)&(m->main.gisFileId),sizeof(UINT4),1,m->fp)!=1 ||
		   m->write((void*)&(m->main.projection),sizeof(UINT2),1,m->fp)!=1 ||
		   m->write((void*)&(m->main.attrTable),sizeof(UINT4),1,m->fp)!=1 ||
		   m->write((void*)&(m->main.mapType),sizeof(UINT2),1,m->fp)!=1 ||
		     fwrite((void*)&(m->main.byteOrder),sizeof(UINT4),1,m->fp)!=1 ||
		   m->write((void*)filler, sizeof(char), MAIN_HEADER_FILL_SIZE ,m->fp)
		                                                      != MAIN_HEADER_FILL_SIZE )
		{	
			M_ERROR(WRITE_ERROR);
			goto error;
		}
		fseek(m->fp,ADDR_SECOND_HEADER, SEEK_SET);
 
		if (    m->write((void*)&(m->raster.valueScale),sizeof(UINT2),1,m->fp) !=1 ||
			m->write((void*)&(m->raster.cellRepr), sizeof(UINT2),1,m->fp) !=1 ||
			  fwrite((void*)&(m->raster.minVal), sizeof(CSF_VAR_TYPE),1,m->fp) !=1 ||
			  fwrite((void*)&(m->raster.maxVal), sizeof(CSF_VAR_TYPE),1,m->fp) !=1 ||
			m->write((void*)&(m->raster.xUL), sizeof(REAL8),1,m->fp) !=1 ||
			m->write((void*)&(m->raster.yUL), sizeof(REAL8),1,m->fp) !=1 ||
			m->write((void*)&(m->raster.nrRows), sizeof(UINT4),1,m->fp) !=1 ||
			m->write((void*)&(m->raster.nrCols), sizeof(UINT4),1,m->fp) !=1 ||
			m->write((void*)&(m->raster.cellSizeX), sizeof(REAL8),1,m->fp) !=1 ||
			m->write((void*)&(m->raster.cellSizeY), sizeof(REAL8),1,m->fp) !=1 ||
			m->write((void*)&(m->raster.angle), sizeof(REAL8),1,m->fp) !=1 ||
		        m->write((void*)filler, sizeof(char), RASTER_HEADER_FILL_SIZE ,m->fp)
		                                                      != RASTER_HEADER_FILL_SIZE )
		{	
			M_ERROR(WRITE_ERROR);
			goto error;
		}
	}
 
	(void)fclose(m->fp);
	CsfUnloadMap(m);
 
	/* clear the space, to avoid typical errors such as
	   accessing the map after Mclose */
    	(void)memset((void *)m->fileName, 0x0, strlen(m->fileName));
    	CSF_FREE(m->fileName);
 
    	(void)memset((void *)m, 0x0, sizeof(MAP));
	CSF_FREE(m);
 
	return(0);
error:	return(1);
}
