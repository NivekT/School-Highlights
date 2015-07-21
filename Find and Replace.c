#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

int main(int argc, char *argv[]) {
  char *src = argv[1];
  char *from = argv[2];
  char *to = argv[3];
  char dest[100] = {0};
  find_replace(src, from, to, dest);
  int i;
  for (i = 0 ; i < strlen(dest); i++)
	printf("%c", dest[i]);
  printf("\n");
  return 0;
}

void find_replace(char* src, char* from, char* to, char* dest)
{
	int i = 0, j = 0, n = 0;
	int srclen = strlen(src);
	int frlen = strlen(from);
	int tolen = strlen(to);
	if (strlen(to) > strlen(from)) {
		fprintf(stderr,
			"Error in input: 'to' is longer than 'from'");
		exit(1);
	} else {
		while (i < srclen) {
			while (src[i+n] == from[n]) 
				n++;
			if (n != frlen) {
				dest[j] = src[i];
				n = 0;
				i++, j++;
			} else {
				for (n = 0; n < tolen; n++, j++)
					dest[j] = to[n];
				n = 0;
				i = i + frlen; 
			}
		}
	}
}

int main() {
	return 0;
}