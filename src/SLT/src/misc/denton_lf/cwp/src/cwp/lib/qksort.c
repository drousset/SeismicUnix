/* Copyright (c) Colorado School of Mines, 1990.
/* All rights reserved.                       */

/*****************************************************************************
sort functions based on C. A. R. Hoare's quicksort algorithm:

qksort		sort an array such that a[0] <= a[1] <= ... <= a[n-1]

qkfind		partially sort an array so that the element a[m] has
		the value it would have if the entire array were sorted
		such that a[0] <= a[1] <= ... <= a[n-1]
*****************************************************************************/

#define NSTACK 50	/* maximum sort length is 2^NSTACK */
#define NSMALL 7	/* size of array for which insertion sort is fast */
#define FM 7875		/* constants used to generate random pivots */
#define FA 211
#define FC 1663

static void
qkpart (int n, float a[], int p, int q, int *j, int *k)
/*****************************************************************************
quicksort partition (FOR INTERNAL USE ONLY):
Take the value x of a random element from the subarray a[p:q] of
a[0:n-1] and rearrange the elements in this subarray in such a way
that there exist integers j and k with the following properties:
  p <= j < k <= q, provided that p < q
  a[l] <= x,  for p <= l <= j
  a[l] == x,  for j < l < k
  a[l] >= x,  for k <= l <= q
Note that this effectively partitions the subarray with bounds
[p:q] into lower and upper subarrays with bounds [p:j] and [k:q].
******************************************************************************
Input:
n		number of elements in a
a		array[p:q] to be rearranged
p		lower bound of subarray; must be less than q
q		upper bound of subarray; must be greater then p

Output:
a		array[p:q] rearranged
j		upper bound of lower output subarray
k		lower bound of upper output subarray
******************************************************************************
Notes:
This function is adapted from procedure partition by
Hoare, C.A.R., 1961, Communications of the ACM, v. 4, p. 321.
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 01/13/89
*****************************************************************************/
{
	int pivot,left,right;
	float apivot,temp;
	static long int seed=0L;
 
	/* choose random pivot element between p and q, inclusive */
	seed = (seed*FA+FC)%FM;
	pivot = p+(q-p)*(float)seed/(float)FM;
	if (pivot<p) pivot = p;
	if (pivot>q) pivot = q;
	apivot = a[pivot];

	/* initialize left and right pointers and loop until break */
	for (left=p,right=q;;) {
		/*
		 * increment left pointer until either
		 * (1) an element greater than the pivot element is found, or
		 * (2) the upper bound of the input subarray is reached
		 */
		while (a[left]<=apivot && left<q) left++;
 
		/*
		 * decrement right pointer until either
		 * (1) an element less than the pivot element is found, or
		 * (2) the lower bound of the input subarray is reached
		 */
		while (a[right]>=apivot && right>p) right--;
 
		/* if left pointer is still to the left of right pointer */
		if (left<right) {
			/* exchange left and right elements */
			temp = a[left];
			a[left++] = a[right];
			a[right--] = temp;
		} 
		/* else, if pointers are equal or have crossed, break */
		else break;
	}
	/* if left pointer has not crossed pivot */
	if (left<pivot) {

		/* exchange elements at left and pivot */
		temp = a[left];
		a[left++] = a[pivot];
		a[pivot] = temp;
	}
	/* else, if right pointer has not crossed pivot */
	else if (pivot<right) {

		/* exchange elements at pivot and right */
		temp = a[right];
		a[right--] = a[pivot];
		a[pivot] = temp;
	}
	/* left and right pointers have now crossed; set output bounds */
	*j = right;
	*k = left;
}

static void
qkinss (int n, float a[], int p, int q)
/*****************************************************************************
quicksort insertion sort (FOR INTERNAL USE ONLY):
Sort a subarray bounded by p and q so that
a[p] <= a[p+1] <= ... <= a[q]
******************************************************************************
Input:
n		number of elements in a
a		subarray[p:q] containing elements to be sorted
p		lower bound of subarray; must be less than q
q		upper bound of subarray; must be greater then p

Output:
a		subarray[p:q] sorted
******************************************************************************
Notes:
Adapted from Sedgewick, R., 1983, Algorithms, Addison Wesley, p. 96.
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 01/13/89
*****************************************************************************/
{
	int i,j;
	float ai;

	for (i=p+1; i<=q; i++) {
		for (ai=a[i],j=i; j>p && a[j-1]>ai; j--)
			a[j] = a[j-1];
		a[j] = ai;
	}
}

void
qksort (int n, float a[])
/*****************************************************************************
Sort an array such that a[0] <= a[1] <= ... <= a[n-1]
******************************************************************************
Input:
n		number of elements in array a
a		array[n] containing elements to be sorted

Output:
a		array[n] containing sorted elements
******************************************************************************
Notes:
n must be less than 2^NSTACK, where NSTACK is defined above.

This function is adapted from procedure quicksort by
Hoare, C.A.R., 1961, Communications of the ACM, v. 4, p. 321;
the main difference is that recursion is accomplished
explicitly via a stack array for efficiency; also, a simple
insertion sort is used to sort subarrays too small to be
partitioned efficiently.
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 01/13/89
*****************************************************************************/
{
	int pstack[NSTACK],qstack[NSTACK],j,k,p,q,top=0;

	/* initialize subarray lower and upper bounds to entire array */
	pstack[top] = 0;
	qstack[top++] = n-1;

	/* while subarrays remain to be sorted */
	while(top!=0) {

		/* get a subarray off the stack */
		p = pstack[--top];
		q = qstack[top];

		/* while subarray can be partitioned efficiently */
		while(q-p>NSMALL) {

			/* partition subarray into two subarrays */
			qkpart(n,a,p,q,&j,&k);

			/* save larger of the two subarrays on stack */
			if (j-p<q-k) {
				pstack[top] = k;
				qstack[top++] = q;
				q = j;
			} else {
				pstack[top] = p;
				qstack[top++] = j;
				p = k;
			}
		}
		/* use insertion sort to finish sorting small subarray */
		qkinss(n,a,p,q);
	}
}

void
qkfind (int m, int n, float a[])
/*****************************************************************************
Partially sort an array so that the element a[m] has the value it
would have if the entire array were sorted such that 
a[0] <= a[1] <= ... <= a[n-1]
******************************************************************************
Input:
m		index of element to be found
n		number of elements in array a
a		array[n] to be partially sorted

Output:
a		array[n] partially sorted
******************************************************************************
Notes:
This function is adapted from procedure find by
Hoare, C.A.R., 1961, Communications of the ACM, v. 4, p. 321.
******************************************************************************
Author:		Dave Hale, Colorado School of Mines, 01/13/89
*****************************************************************************/
{
	int j,k,p,q;

	/* initialize subarray lower and upper bounds to entire array */
	p = 0;  q = n-1;

	/* while subarray can be partitioned efficiently */
	while(q-p>NSMALL) {

		/* partition subarray into two subarrays */
		qkpart(n,a,p,q,&j,&k);

		/* if desired value is in lower subarray, then */
		if (m<=j)
			q = j;

		/* else, if desired value is in upper subarray, then */
		else if (m>=k)
			p = k;
		
		/* else, desired value is between j and k */
		else
			return;
	}
			
	/* completely sort the small subarray with insertion sort */
	qkinss(n,a,p,q);
}
