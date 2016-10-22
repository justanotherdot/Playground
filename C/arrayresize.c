#include <stdio.h>
#include <stdlib.h>

// Data definitions.
typedef struct {
	int cap;
	int size;
	int* array;
} Vector;

// Prototypes.
Vector* createVector(int cap);
Vector* resize(Vector* v);
int     get(Vector* v, int ix);
int     size(Vector* v);
int     cap(Vector* v);
void    set(Vector* v, int ix, int val);
void    freeVector(Vector* v);

// Function implementations.
Vector* createVector(int cap) {
	Vector* new = malloc(sizeof(Vector));
	new->array  = malloc(sizeof(int) * cap);
	new->cap    = cap;
	new->size   = 0;
	for (int i = 0; i < cap; i++) {
		new->array[i] = 0;
	}
	return new;
}

int get(Vector* v, int ix) {
	return v->array[ix];
}

void set(Vector* v, int ix, int val) {
	if (v->size == v->cap) {
		resize(v);
	}
	v->size++;
	v->array[ix] = val;
}

Vector* resize(Vector* v) {
	// Housekeeping.
	int newCap = (v->cap)*2;
	int currSize = v->size;
	int* newArr  = malloc(newCap * sizeof(int));
	// Copy over the array into the new one.
	for (int i = 0; i < currSize; i++) {
		newArr[i] = v->array[i];
	}
	// Initialize the new array;
	for (int i = currSize; i < newCap; i++) {
		newArr[i] = 0;
	}
	// Free and reassign.
	free(v->array);
	v->array = newArr;
	v->cap = newCap;
	return v;
}

int size(Vector* v) {
	return v->size;
}

int cap(Vector* v) {
	return v->cap;
}

void freeVector(Vector* v) {
	free(v->array);
	free(v);
}

void runExample() {
	int n = 10000000;
	Vector* v = createVector(10);
	for (int i = 0; i <= n; i++) {
		set(v, i, 7);
	}
	printf("size: %d\n", cap(v));
	printf("val : %d\n", get(v, 12));
	freeVector(v);
}

// -- MAIN -- 
int main(void) {
	runExample();
}
