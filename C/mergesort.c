#include <stdio.h>

// A down-and-dirty integer msort.

void merge(int* a, int l, int m, int h) {
	int n = (h-l)+1;
	int tmp[n];

	int i = l;
	int j = m+1;
	int k = 0;
	while (i <= m && j <= h) {
		if (a[i] <= a[j]) {
			tmp[k++] = a[i++];
		} else {
			tmp[k++] = a[j++];
		}
	}

	// Copy what's leftover.
	while (i <= m) {
		tmp[k++] = a[i++];
	}
	while (j <= h) {
		tmp[k++] = a[j++];
	}

	for (k = 0; k < n; k++) {
		a[l + k] = tmp[k];
	}
}

void msort(int* a, int lo, int hi) {
	if (lo < hi) {
		int mid = (lo+hi)/2;
		msort(a, lo, mid);
		msort(a, mid+1, hi);
		merge(a, lo, mid, hi);
	}
}

int main(void) {
	int n = 10;
	int test[n];
	test[0] = 9;
	test[1] = 4;
	test[2] = 1;
	test[3] = -18;
	test[4] = 3;
	test[5] = 6;
	test[6] = 7;
	test[7] = 2;
	test[8] = 0;
	test[9] = -2;

	// Print prior to msort.
	for (int i = 0; i < n; i++) {
		printf("%d ", test[i]);
	}
	printf("\n");

	msort(test, 0, n-1);

	// Print after msort.
	for (int i = 0; i < n; i++) {
		printf("%d ", test[i]);
	}
	printf("\n");
}
