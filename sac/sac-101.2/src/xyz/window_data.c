void window_data(array,nxsize,nysize,wdata,jxstart,jxstop,jystart,jystop)
float *array;
long nxsize, nysize;
float *wdata;
long jxstart, jxstop, jystart, jystop;
{

  long i, j;

  for (i = jystart-1; i < jystop; i++){
    for (j = jxstart-1; j < jxstop; j++){
      *(wdata++) = *(array + (i*nxsize) + j);
    }
  }

  return;
}
