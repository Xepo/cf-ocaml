/*Was too lazy to get ocaml to write a png.  So it outputs an intermediate file and this creates a png from it*/
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <imlib2.h>
using namespace std;

int w,h;

char buf[512];
char *pbuf;
char *pend;
FILE *file;

char bufget()
{
     if (pbuf >= pend)
     {
          if (feof(file) || ferror(file))
               printf("endoffile reached\n");
          pbuf = buf;
          pend = pbuf + fread(buf, 1, 512, file);
     }
     return *(pbuf++);

}
void load_row(FILE *file, DATA32 *pData, int y)
{
  char c;
  for(int x=0; x<w; x++)
  {
    do {
         c = bufget();
    } while (c < '0' || c > '9');
    float f = (float)(c - '0') * (255. / 9.);
    int v = f;
    pData[y*h+x] = v << 16 | v << 8 | v;
  }
}
/* main program */
int main(int argc, char **argv)
{
  char c;
  char c2;
  if (argc < 2)
  {
       printf("Usage: create_png infile outfile\n");
       return 1;
  }
  pbuf = buf;
  pend = pbuf-1;

  file = fopen(argv[1], "r");

  fscanf(file, "%d%c%d%c", &w, &c, &h, &c2);
  printf("image:%d,%d\n", w, h);
  DATA32 *pData =(DATA32*) malloc(sizeof(DATA32) * h * w);
  if (!pData) printf("badimage");

  for(int y =0; y<h; y++)
  {
       if (y % 100 == 99 || y == h-1)
          printf("row %d/%d\n", y+1, h);
    load_row(file, pData, y);
  }

  Imlib_Image im = imlib_create_image_using_data(w,h,pData);
  if (!im) printf("badimage");
  imlib_context_set_image(im);

  imlib_image_set_format("png");
  imlib_save_image(argv[2]);
}
