#include <stdio.h>
#include <string.h>

void swap(int* a, int* b) {
	int tmp = *a;
	*a = *b;
	*b = tmp;
}

void isort(int* a, int n) {
	if (n <= 1) {
		// Nothing to do.
		return;
	}

	// Go across all elements 
	for (int i = 1; i <= n; i++) {
		int x = a[i];
		int j = i-1;
		while (j >= 0 && a[j] > x) {
			a[j+1] = a[j];
			j--;
		}
		a[j+1] = x;
	}
}

int main(void) {
	int n = 14;
	int test[] = {7, 9, 3, 6, 4, 1, 10, 11, 7, 12, 13, 3, 2, 1};
	isort(test, n);
	for (int i = 0; i < n; i++) {
		printf("%d ", test[i]);
	}
	printf("\n");
}
