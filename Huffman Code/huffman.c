/* Kevin Tse, kevintse */
/* CS152, Winter 2015 */
/* Homework 5 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "huff.h"

int main(int argc, char* argv[]) {
	int* x = 0;
	huff_list* hs1 = h_list(h_array(argv[1], x), (*x));
	huff_list* hs2 = h_list(h_array(argv[1], x), (*x));
	huff* t = tree_maker(hs1);
	printf("%d\n", huff_weight(t));
	while (hs2) {
		char c = hs2->val->h.leaf.c;
		printf("%c=%s\n", c, path_string(t,c));
		hs2 = hs2->next;
	}
	print_code(argv[1]);
	return 0;
}