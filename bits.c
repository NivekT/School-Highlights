// Without calling any functions, loops, macros, casting, data types, 
// or any other operations (e.g. &&, ||, -, or ?:) other than
// the explicitly allowed ones. Complete the function using the 
// the minimum number of bitwise operations. 

/*
 * anyOddBit - return 1 if any odd-numbered bit in word set to 1
 *   Examples anyOddBit(0x5) = 0, anyOddBit(0x7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 2
 */
int anyOddBit(int x) {
       /* 
	  Filter is used to construct an integer with 1's exactly in the odd
	  indexed bits. This is then 'anded' with the input to determine if
	  the input has any of these bits. If it does the number is double 
	  negated to ensure the value of 1; if it does not the double negative
	  returns 0.
	*/
       int filter = (170 << 24) + (170 << 16) + (170 << 8) + 170;
       int rv = !(!(x & filter));
       return rv;

}

/* 
 * bang - Compute !x without using !
 *   Examples: bang(3) = 0, bang(0) = 1
 *   Legal ops: ~ & ^ | + << >>
 *   Max ops: 12
 *   Rating: 4 
 */
int bang(int x) {
  /* 
     The filter is an integer 0111...1. 
     x & filter changes all negative non-zero numbers to positive
     numbers, but the result, n, may not equal to -x. This does 
     not impact the function of this program.
     (n + 2) >> (n + 1) maps all non-zero numbers to zero, while
     if n == 0, it returns 1, because a non-zero number will right
     shift til all of its bits are zero. 
   */
   /*
   int filter = (127 << 24) + (255 << 16) + (255 << 8) + 255;
   int n = x & filter; // fails at 1000000
   int rv = (n + 2 + (x >> 31)) >> (n + 1);
   return rv;
   */
  int rv = x | x >> 16;
  rv |= (rv >> 8);
  rv |= (rv >> 4);
  rv |= (rv >> 2);
  rv |= (rv >> 1);
  return (~rv) & 1;
}

/*
 * bitCount - returns count of number of 1's in word
 *   Examples: bitCount(5) = 2, bitCount(7) = 3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 40
 *   Rating: 4
 */
int bitCount(int x) {
  /*
    Adding the bits at the even positions together with the one in odd positions
    Resulting in a set of 16 new positions, each has 2 bits, the integer given 
    at these 2 bits is either 00, 01, 10, representing the sum of the number of 1's
    from the original pair of odd and even position
    Then, add the even and odd positions of these 16 positions together, basically 
    summing up the number of 1's from the previous positions
    Resulting in 8 pairs of 4 bits positions
    Repeat, until we obtain a single 32-bit position, which contain all the sums of 1's
  */
  /*
    int mask1 = 2863311530; // 10101010...
  */
  int mask1 = (((((85 << 8) + 85) << 8) + 85) << 8) + 85; //1431655765; 01010101... 
  int mask2 = (((((51 << 8) + 51) << 8) + 51) << 8) + 51; // 858993459; 00110011... 
  int mask3 = (((((15 << 8) + 15) << 8) + 15) << 8) + 15; // 252645135;  //00001111... 
  int mask4 = (255 << 16) + 255; //16711935;  //0000000011111111... 
  int mask5 = (255 << 8) + 255; //65535;  //00000000000000001111111111111111... 
  /* 
  int a = (x & mask1) + ((x & mask1) >> 1); doesn't work
  int b = (a & mask2) + ((a & mask2) >> 2);
  int c = (b & mask3) + ((b & mask3) >> 4);
  int d = (c & mask4) + ((c & mask4) >> 8);

  int a = ((x << 1) & mask1) + (x & mask1); the leftmost digit is ignored
  */
  int a = (x & mask1) + ((x >> 1) & mask1);
  //int a = x + (~((x >> 1) & mask1) + 1);
  int b = (a & mask2) + ((a >> 2) & mask2);
  int c = (b & mask3) + ((b >> 4) & mask3);
  //int c = (b) + ((b >> 4) & mask3);
  int d = (c & mask4) + ((c >> 8) & mask4);
  // int rv = (c & mask4) + ((c >> 8) & mask4);
  // int rv = (d & mask5) + ((d >> 16) & mask5);
  int rv = (d + (d >> 16)) & mask5;
  return rv;
}

/* 
 * bitMask - Generate a mask consisting of all 1's 
 *   lowbit and highbit
 *   Examples: bitMask(5,3) = 0x38
 *   Assume 0 <= lowbit <= 31, and 0 <= highbit <= 31
 *   If lowbit > highbit, then mask should be all 0's
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 16
 *   Rating: 3
 */
int bitMask(int highbit, int lowbit) {
  int a = ~0 << lowbit;
  int b = ~0 << (highbit);
  b = b << 1;
  return a & ~b;
}


/*
 * bitParity - returns 1 if x contains an odd number of 0's
 *   Examples: bitParity(5) = 0, bitParity(7) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 4
 */
int bitParity(int x) {
  int y = x ^ (x >> 16);
  y = y ^ (y >> 8);
  y = y ^ (y >> 4);
  y = y ^ (y >> 2);
  y = y ^ (y >> 1);
  return y & 1;
}

/* 
 * getByte - Extract byte n from word x
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: getByte(0x12345678,1) = 0x56
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int getByte(int x, int n) {
  return (x >> (n << 3)) & 255;
}


/* howManyBits - return the minimum number of bits required to represent x in
 *             two's complement
 *  Examples: howManyBits(12) = 5
 *            howManyBits(298) = 10
 *            howManyBits(-5) = 4
 *            howManyBits(0)  = 1
 *            howManyBits(-1) = 1
 *            howManyBits(0x80000000) = 32
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 90
 *  Rating: 4
 */
int howManyBits(int x) {

  int rv, a, b, c, d, e;
  int z = ~(x >> 31);
  int y = (z & x) | (~z & ~x);

  a = ((y >> 16) & ((255 << 8) + 255));
  y = y >> ((!!a) << 4);

  b = ((y >> 8) & 255); 
  y = y >> ((!!b) << 3); 

  c = ((y >> 4) & 15);
  y = y >> ((!!c) << 2);

  d = ((y >> 2)) & 3;
  y = y >> ((!!d) << 1);

  e = ((y >> 1)) & 1;
  y = y >> ((!!e));

  rv = ((!a) << 4) + ((!b) << 3) + ((!c) << 2) + ((!d) << 1) + (!e) + (!(y & 1));
  // right now rv is the number of zeros needed
  rv = 34 + ~rv;
  return rv;
}


/* 
 * isAsciiDigit - return 1 if 0x30 <= x <= 0x39 (ASCII codes for characters '0' to '9')
 *   Example: isAsciiDigit(0x35) = 1.
 *            isAsciiDigit(0x3a) = 0.
 *            isAsciiDigit(0x05) = 0.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 15
 *   Rating: 3
 */
int isAsciiDigit(int x) {
  /*
    Check if 0x30 <= x <= 0x39, returns 1 if true, 0 if false
  */
  int rv=!((x + ~0x2F) >> 31) & !(!((x + ~0x39) >> 31));   
  return rv;
}

/* 
 * isEqual - return 1 if x == y, and 0 otherwise 
 *   Examples: isEqual(5,5) = 1, isEqual(4,5) = 0
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 5
 *   Rating: 2
 */
int isEqual(int x, int y) {
  /*
    x ^ y returns 0 if x == y, and a non-zero number
    if x != y, such that !(x ^ y) returns the desired
    result.
   */
  int rv = !(x ^ y);
  return rv;
}

/* 
 * isGreater - if x > y  then return 1, else return 0 
 *   Example: isGreater(4,5) = 0, isGreater(5,4) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isGreater(int x, int y) {
  /*
    Check if x - y is positive
  */
  int equal = !(x ^ y);
  int xs = (x >> 31) & 1;
  int ys = (y >> 31) & 1;
  int pos_neg = !(xs) & ys;
  int neg_pos = xs & !(ys);
  return pos_neg | (!((x + ~y) >> 31) & (!equal) & !(neg_pos));
}


/* 
 * isLessOrEqual - if x <= y  then return 1, else return 0 
 *   Example: isLessOrEqual(4,5) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 24
 *   Rating: 3
 */
int isLessOrEqual(int x, int y) {
  /*
    Check if x - y is negative or zero
  */
  int equal = !(x ^ y);
  int xs = (x >> 31) & 1;
  int ys = (y >> 31) & 1;
  int pos_neg = !(xs) & ys;
  int neg_pos = xs & !(ys);
  return !(pos_neg | (!((x + ~y) >> 31) & (!equal) & !(neg_pos)));
}

/* 
 * isNonNegative - return 1 if x >= 0, return 0 otherwise 
 *   Example: isNonNegative(-1) = 0.  isNonNegative(0) = 1.
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 3
 */
int isNonNegative(int x) {
  /*
    x >> 32 returns 0 if x >= 0, and -1 if x < 0. 
    !(x >> 32) returns 1 if x >= 0, and 
    returns 0 if x < 0.
   */
  int rv = !(x >> 31);
  return rv;
}

/* 
 * isNotEqual - return 0 if x == y, and 1 otherwise 
 *   Examples: isNotEqual(5,5) = 0, isNotEqual(4,5) = 1
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 6
 *   Rating: 2
 */
int isNotEqual(int x, int y) {
  /*
    x ^ y returns 0 if x == y, and a non-zero number
    if x != y, such that !(!(x ^ y)) returns the desired
    result, as ! returns 0 for non-zero and 1 for zero.
   */
  int rv = !(!(x ^ y));
  return rv;
}

/*
 * leftBitCount - returns count of number of consective 1's in
 *     left-hand (most significant) end of word.
 *   Examples: leftBitCount(-1) = 32, leftBitCount(0xFFF0F0F0) = 12
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 50
 *   Rating: 4
 */
int leftBitCount(int x) {
  int rv, a, b, c, d, e;
  //int filter = (127 << 24) + (255 << 16) + (255 << 8) + 255; // 01111111
  int y = ~x;

  a = ((y >> 16) & ((255 << 8) + 255));//~(1<<31)) >> 15; // left half of y
  y = y >> ((!!a) << 4);

  b = ((y >> 8) & 255); 
  // int b = ((y >> 1))
  y = y >> ((!!b) << 3); // conditional shifting

  c = ((y >> 4) & 15);
  y = y >> ((!!c) << 2);

  d = ((y >> 2)) & 3;
  y = y >> ((!!d) << 1);

  e = ((y >> 1)) & 1;
  y = y >> ((!!e));

  rv = ((!a) << 4) + ((!b) << 3) + ((!c) << 2) + ((!d) << 1) + (!e) + (!(y & 1)); 
  // (!(y & 1)) check if last bit is 1 or 0
  return rv;
}

/* 
 * logicalShift - shift x to the right by n, using a logical shift
 *   Can assume that 0 <= n <= 31
 *   Examples: logicalShift(0x87654321,4) = 0x08765432
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3 
 */
int logicalShift(int x, int n) {
  /* 
     The filter is an integer 0111...1. 
     "(x >> 1) & filter" first right shifts x by 1, 
     then changes the most significant bit to zero.
     y >> (n - 1) completes the rest of the shift. Since the most 
     significant bit has become zero, the rest of the right shifts 
     will fill in zeroes on the left end of the number
     as a logical shift should.
   */
  int y = x >> n;
  int a = (32 + ~n);
  int filter = ~(((~0) << a) << 1);
  return y & filter;
}

/* 
 * rempwr2 - Compute x%(2^n), for 0 <= n <= 30
 *   Negative arguments should yield negative remainders
 *   Examples: rempwr2(15,2) = 3, rempwr2(-35,3) = -3
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int rempwr2(int x, int n) {
  int s = x >> 31;
  int rem = (~(~0 << n)) & x;
  int mask = (!(!rem) << 31) >> 31;
  return rem + ((~(1 << n) + 1) & s & mask);
}

/* 
 * replaceByte(x,n,c) - Replace byte n in x with c
 *   Bytes numbered from 0 (LSB) to 3 (MSB)
 *   Examples: replaceByte(0x12345678,1,0xab) = 0x1234ab78
 *   You can assume 0 <= n <= 3 and 0 <= c <= 255
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 10
 *   Rating: 3
 */
int replaceByte(int x, int n, int c) {
  int rv = x & ~(0xFF << (n << 3));
  rv |= (c << (n << 3));
  return rv;
}

/* 
 * rotateRight - Rotate x to the right by n
 *   Can assume that 0 <= n <= 31
 *   Examples: rotateRight(0x87654321,4) = 0x18765432
 *   Legal ops: ~ & ^ | + << >> !
 *   Max ops: 25
 *   Rating: 3 
 */
int rotateRight(int x, int n) {
  //int rv = (x << (33 + ~n)) << 1 | ((x >> n) & (~((~0) << (32 + ~n))));
  int a = (33 + ~n) & 31;
  int rv = ((x >> n)) & (~((~0) << a)) | ((x << a));
  return rv;
}

/*
 * satMul3 - multiplies by 3, saturating to Tmin or Tmax if overflow
 *  Examples: satMul3(0x10000000) = 0x30000000
 *            satMul3(0x30000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0x70000000) = 0x7FFFFFFF (Saturate to TMax)
 *            satMul3(0xD0000000) = 0x80000000 (Saturate to TMin)
 *            satMul3(0xA0000000) = 0x80000000 (Saturate to TMin)
 *  Legal ops: ! ~ & ^ | + << >>
 *  Max ops: 25
 *  Rating: 3
 */
int satMul3(int x) {
  int sign = (x >> 31) & 1;
  int two = x << 1;
  int three = two + x;
  int twosign = (two >> 31) & 1;
  int threesign = (three >> 31) & 1;
  int neg = (((sign ^ twosign) | (sign ^ threesign)) << 31) >> 31;
  int min = (1 << 31);
  int filter = (sign << 31) >> 31;
  return (three & ~neg) | (neg & ((~filter & (~min)) | (filter & min)));
}

/* 
 * subOK - Determine if can compute x-y without overflow
 *   Example: subOK(0x80000000,0x80000000) = 1,
 *            subOK(0x80000000,0x70000000) = 0, 
 *   Legal ops: ! ~ & ^ | + << >>
 *   Max ops: 20
 *   Rating: 3
 */
int subOK(int x, int y) {
  int xs = x >> 31 & 1;
  int ys = y >> 31 & 1;
  int diffs = (x + ~y + 1) >> 31 & 1;
  return !(((xs & (!ys) & (!diffs)) | ((!xs) & ys & diffs)));
}
