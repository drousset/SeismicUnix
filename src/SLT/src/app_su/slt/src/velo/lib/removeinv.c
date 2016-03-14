/* remove velocity inversion in input t-v pairs */
/*	input:
	t 	times of velocity picks (maxntv by nf 2-d array)
	v	velocity picks  (maxntv by nf 2-d array)
	nps	number of t-v pairs	(nf 1-d array)
	maxntv	maximum number of t-v pairs in t and v array per location
	nf	number of velocity locations
	rminv	remove velocity inversion
		1=delete the t-v pair
		2=replace velocity with previous velocity
	output:
	t,v,nps
*/

void removeinv(float *t, float *v, int *nps, int nf, int maxntv, int rminv) {

	float *tt, *vv;
	int i, j, ii;

	tt = (float*) emalloc(maxntv*sizeof(float));
	vv = (float*) emalloc(maxntv*sizeof(float));

	for(j=0;j<nf;j++) {
		tt[0] = t[j*maxntv];
		vv[0] = v[j*maxntv];
		ii = 0;
		for(i=1;i<nps[j];i++) {
			if(v[j*maxntv+i]<vv[ii]) {
				if(rminv==2) {
					ii = ii + 1;
					tt[ii] = t[j*maxntv+i];
					vv[ii] = vv[ii-1];
				}
			} else {
				ii = ii + 1;
				tt[ii] = t[j*maxntv+i];
				vv[ii] = v[j*maxntv+i];
			}
		}
		ii = ii + 1;
		nps[j] = ii;
		for(i=0;i<ii;i++) {
			t[j*maxntv+i] = tt[i];
			v[j*maxntv+i] = vv[i];
		}
	}
	free(tt);
	free(vv);
}
