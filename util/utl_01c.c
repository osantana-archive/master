#include <dos.h>
#include <clipper.h>

char *t1[5] = { "│┤╡╢╖╕╣║╗╝╜╛┐└┴┬├─┼╞╟╚╔╩╦╠═╬╧╨╤╥╙╘╒╓╫╪┘┌ ",
                "11120022022101101011220202021200210021100",
                "01211220221210110110000220222121000012100",
                "11122122200010011011202022020012001221010",
                "00000000000001111112122222222121122112010" };

char conct_01(int,char);

CLIPPER CONECTOR()
{
   int p1, p2;
   register char v1, v2, v3, v4, lc;
   p1 = _parni(1);
   p2 = _parni(2);
   v1 = conct_01(1,p1 != 00 ? peekb(0xB800,(p1-1)*160+p2*2) : 32);
   v2 = conct_01(2,p2 != 00 ? peekb(0xB800,p1*160+(p2-1)*2) : 32);
   v3 = conct_01(3,p2 != 24 ? peekb(0xB800,(p1+1)*160+p2*2) : 32);
   v4 = conct_01(4,p2 != 79 ? peekb(0xB800,p1*160+(p2+1)*2) : 32);
   for (lc=0;lc<41;lc++) if (v1 == t1[1][lc] && v2 == t1[2][lc] && \
       v3 == t1[3][lc] && v4 == t1[4][lc]) break;
   pokeb(0xb800,p1*160+p2*2,t1[0][lc]);
   _retl(1);
}

char conct_01(int c1,char c2)
{
   register char lt,rt;
   rt = 48;
   for (lt=0;lt<40;lt++)
   {
      if(c2 == t1[0][lt])
      {
         rt = t1[c1][lt];
         break;
      }
   }
   return(rt);
}


