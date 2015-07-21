/* Kevin Tse, kevintse */
/* CS152, Winter 2015 */
/* Homework 5 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "huff.h"

huff *huff_singleton(char c, int n) {
	huff* h = malloc(sizeof(huff));
	h->tag = LEAF;
	h->h.leaf.c = c;
	h->h.leaf.n = n;
	return h;
}

void huff_show(huff *h) {
	if (!h)
		return;
	switch (h-> tag) {
		case LEAF:
			printf("Leaf: %c % d\n", h->h.leaf.c, h->h.leaf.n);
			break;
		case NODE:
			printf("Node %d\n", h->h.node.n);
			huff_show(h->h.node.lsub);
			huff_show(h->h.node.rsub);
			break;
		default:
			fprintf(stderr, "Invalid input");
	}
}


/* Given a string, makes an array of huff*/
huff* h_array(char* s, int* n) {
	int len = strlen(s);
	int i, j;
	huff* hl = malloc(sizeof(huff) * len);
	(*n) = 0; /* n = the number of elements in hl*/
	for (i = 0; i < len ; i++) {
		char c = s[i];
		for (j = 0; j < (*n); j++) {
			if (hl[j].h.leaf.c == c) {
				(hl[j].h.leaf.n)++;
				j = *n;
			}
		}
		if ((c >= 'a') && (c <= 'f')) {
			c -= 32;
			(*n)++;
			huff_singleton(c, 1);
		} else if ((c >= 'A') && (c <= 'F')) {
			(*n)++;
			huff_singleton(c, 1);
		} else if (c == ' ') {
			(*n)++;
			huff_singleton(c, 1);
		}
	}
	return hl;
}

/* Given an array of huff and its length*/
/* returns a huff_list*/
huff_list* h_list(char* s) {
	int len = strlen(s);
	int i,j;
	huff* ha = malloc(sizeof(huff) * len);
	char* existing_chars = malloc(sizeof(char) * len);
	int n_of_char = 0;
	for (i = 0; i < len; i++) {
		char curr = s[i];
		if ((n_of_char == 0) || 
			(strchr(existing_chars, curr) == NULL)) {
			existing_chars[n_of_char] = curr;
			ha[n_of_char] = (*huff_singleton(curr, 1));
			n_of_char++;
		} else {
			for (j = 0; j < n_of_char; j++) {
				if (ha[j].h.leaf.c == curr)
					ha[j].h.leaf.n++;
			}
		}
	}

	huff_list* hs = malloc(sizeof(huff_list));
	if (n_of_char == 0) 
		hs = NULL;
	hs->h = (&ha[0]);
	hs->next = NULL;
	huff_list* first = hs;
	for (i = 1; i < n_of_char; i++) {
		huff_list* temp = malloc(sizeof(huff_list));
		temp->h = (&ha[i]);
		temp->next = NULL;
		hs->next = temp;
		hs = hs->next; 
	}

	return first; /* function working as intended*/
}

int huff_weight(huff *h) {
	switch(h->tag) {
		case LEAF :
			return h->h.leaf.n;
			break;
		case NODE :
			return h->h.node.n;
			break;
		default :
			fprintf(stderr, "Input is not a LEAF or NODE");
			break;
	}
}

void isort(huff_list* hs) {
	huff_list *curr = hs;
	huff_list *next = hs->next;
	huff *temp;
	while (next) {
		while (next != curr) {
			if ((huff_weight(next->val) < huff_weight(curr->val))
				&&
				next->val) {
				/* adopted from Shaw's in class code */
				temp = curr->val;
				curr->val = next->val;
				next->val = temp;
			}
			curr = curr->next;
		}
		/* restart from beginning*/
		curr = hs;
		/* next position to check*/
		next = next->next;
	}
}

/* merge two huffs into a node(huff) */
huff* merge(huff* h1, huff* h2) {
	huff* result = malloc(sizeof(huff));
	result->tag = NODE;
	result->h.node.n = huff_weight(h1) + huff_weight(h2);
	result->h.node.lsub = h1;
	result->h.node.rsub = h2;
	return result;
}

/* apply the effect of merge to a sorted list*/
/* will not work as intended if list is not sorted*/
void merge_list(huff_list* hs) {
	if (!hs) /* hs == NULL */
		return;
	else if (!(hs->next)) /* singleton tree*/
		return;
	else
		hs->val = merge(hs->val, hs->next->val);
	/* merge the first two huffs (the ones with least weight */
		hs->next = hs->next->next;
}

huff* tree_maker(huff_list *hs) {
	while (!hs->next)
		isort(hs);
		merge_list(hs);
	return hs->val;
}

/* This is done under the help of classmate Zach Krebs*/
/* he gave suggestions of this implementation because*/
/* mine original method is not working*/

/* find the path of char in binary*/
int path(huff* h, char c, int p) {
	if (h->tag == LEAF && h->h.leaf.c == c)
		return p;
	else if (h->tag == LEAF)
		return 0;
	else if (h->tag == NODE)
		return path(h->h.node.lsub,c,p<<1) +
			   path(h->h.node.rsub,c,(p<<1)+1);
}

/* pull out the nth digit of the binary number b*/
char bit_puller(int b, int n) {
	return (b >> (n-1)) & 1;
}

/* convert the binary number to string*/
char* path_string(huff* h, char c) {
	int i;
	int p = path(h,c,1);
	int len = floor(log(p)/log(2)) + 1;
	char* s = malloc(len*(sizeof(char)));
	s[len-1] = 0;
	for (i = 0; i < len - 1; i++) 
		s[len] = bit_puller(p,len-1-i) + '0';
	return s;
}

/* print out the string and its coding*/
void print_code(char* s) {
	int i, len = strlen(s);
	int* x = 0;
	huff_list* hs = h_list(h_array(s, x), (*x));
	huff* t = tree_maker(hs);
	char* l;
	for (i = 0; i < len; i++) {
		l = path_string(t,s[i]);
		printf("%s", l);
	}
	printf("\n");
}