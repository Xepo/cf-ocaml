#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <vector>
#include <imlib2.h>
using namespace std;

int w,h;

void load_row(FILE *file, DATA32 *pData, int y)
{
  char c;
  for(int x=0; x<w; x++)
  {
    do {
      fread(&c, 1, 1, file);
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

  FILE *file = fopen("image.in", "r");
  fscanf(file, "%d%c%d%c", &w, &c, &h, &c);
  printf("image:%d,%d\n", w, h);
  DATA32 *pData =(DATA32*) malloc(sizeof(DATA32) * h * w);
  if (!pData) printf("badimage");

  for(int y =0; y<h; y++)
    load_row(file, pData, y);

  Imlib_Image im = imlib_create_image_using_data(w,h,pData);
  if (!im) printf("badimage");
  imlib_context_set_image(im);

  imlib_image_set_format("png");
  imlib_save_image("out.png");
}
