#include <stdio.h>
#include <stdlib.h>
#include <imlib2.h>
#include <caml/mlvalues.h>

double min(double x, double y) { return x < y ? x : y; }
double max(double x, double y) { return x > y ? x : y; }
int get_color(value pixel, int i)
{
     double d = Double_field(Field(pixel, i), 0);
      int r = max(0, min(255, (int)(255. * d)));
      return r;
}

CAMLprim value
render_png(value ml_filename, value ml_w, value ml_h, value ml_arr)
{
     long w = Int_val(ml_w);
     long h = Int_val(ml_h);
     DATA32 *pData =(DATA32*) malloc(sizeof(DATA32) * w * h);
     long len = Wosize_val(ml_arr);

     if (w*h != len)
     {
          printf("Width*height != size of array: w:%ld h:%ld arrsize:%ld\n", w, h, len);
          return Val_false; 
     }
     for(long y=0; y<h; y++)
     {
          printf("row %ld/%ld\n", y+1, h);
          for(long x=0; x<w; x++)
          {
               value pixel = Field(ml_arr, y*h+x);
               int r = get_color(pixel, 0);
               int g = get_color(pixel, 1);
               int b = get_color(pixel, 2);
               pData[y*h+x] = b << 16 | g << 8 | r;
          }
     }
     Imlib_Image im = imlib_create_image_using_data(w,h,pData);
     if (!im) 
     {
          printf("bad image\n");
          return Val_false;
     }
     imlib_context_set_image(im);

     imlib_image_set_format("png");
     imlib_save_image(String_val(ml_filename));

     return Val_true;
}

