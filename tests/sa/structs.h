#ifndef STRUCTS_H
#define STRUCTS_H

typedef struct {
	int num;
	char __padding[12];
} int16b_t __attribute__((aligned(16)));

typedef struct {
	unsigned int next;
	int result;
	char send;
	char __padding[7];
} meta_t __attribute__((aligned(16)));

typedef struct {
	unsigned int count;
	int partial;
	char __padding[8];
} partial_t __attribute__((aligned(16)));

#endif // STRUCTS_H
