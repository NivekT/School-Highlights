/* Kevin Tse */
// Given a string, the program uses the Huffman coding algorithm for 
// compression of text data and prints the result.
//Ex. BEE → 011
//    HUBBUB → 000111011
//    REFEREE → 0110010111


/* Struct definition*/

#ifndef HUFF_H
#define HUFF_H

typedef struct leaf leaf;
typedef struct node node;
typedef struct huff huff;

enum huff_tag { LEAF, NODE };

struct leaf {
  char c;
  int n;
};

struct node {
  int n;
  huff *lsub;
  huff *rsub;
};

union huff_union {
  leaf leaf;
  node node;
};

struct huff {
  enum huff_tag tag;
  union huff_union h;
};

typedef struct huff_list huff_list;

struct huff_list {
  huff *val;
  huff_list *next;
};

/* Here follow prototypes for a selection of functions you will likely
   want. You will need to write more, but this is a start.
 */ 

/* Construct a singleton Huffman tree from a character and a count. */
huff *huff_singleton(char c, int n);

/* Return the weight of a Huffman tree. */
int huff_weight(huff *h);

void isort(huff_list* hs);

/* Merge two Huffman trees. */
/* See step 2 of the algorithm on the web page for details. */
huff *merge(huff *h1, huff *h2);

/* Display a Huffman tree at the console. */
/* Essential for development and debugging. */
void huff_show(huff *h);

/* Given a string, makes an array of huff*/
huff* h_array(char* s, int* n);

/* Given an array of huff and its length*/
/* returns a huff_list*/
huff_list* h_list(huff* h, int len);

/* apply the effect of merge to a sorted list*/
/* will not work as intended if list is not sorted*/
void merge_list(huff_list* hs);

huff* tree_maker(huff_list *hs);

/* find the path of char in binary*/
int path(huff* h, char c, int p);

/* pull out the nth digit of the binary number b*/
char bit_puller(int b, int n);


/* convert the binary number to string*/
char* path_string(huff* h, char c);

/* print out the string and its coding*/
void print_code(char* s);

#endif /* HUFF_H */