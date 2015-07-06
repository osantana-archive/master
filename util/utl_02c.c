#include <clipper.h>
#include <dir.h>
#include <io.h>
#include <dos.h>
#include <stdlib.h>
#include <time.h>

CLIPPER mkdirct()
{
   char *utl_pth = _parc(1);
   if (! (PCOUNT==1 || ISCHAR(1)))
      _retl(0);
   else
      _retl(mkdir(utl_pth)+1);
}

CLIPPER rmdirct()
{
   char *utl_pth = _parc(1);
   if (! (PCOUNT==1 || ISCHAR(1)))
      _retl(0);
   else
      _retl(rmdir(utl_pth)+1);
}

CLIPPER chdirct()
{
   char *utl_pth = _parc(1);
   if (! (PCOUNT==1 || ISCHAR(1)))
      _retl(0);
   else
      _retl(chdir(utl_pth)+1);
}

CLIPPER setdrive()
{
   if (! (PCOUNT==1 || ISNUM(1)))
      _retni(0);
   else
      _retni(setdisk(_parni(1)));
}

CLIPPER getdrive()
{
   _retni(getdisk());
}

CLIPPER chgattr()
{
   if (! (PCOUNT==3 || ISCHAR(1) || ISNUM(2) || ISNUM(3)))
      _retni(0);
   else
      _retni(_chmod(_parc(1),_parni(3),_parni(2)));
}

CLIPPER portout()
{
   if (! (PCOUNT==2 || ISNUM(1) || ISNUM(2)))
      _retl(0);
   else
   {
      outportb(_parni(1),(char)_parni(2));
      _retl(1);
   }
}

CLIPPER portin()
{
   if (! (PCOUNT==1 || ISNUM(1)))
      _retni(0);
   else
      _retni((int)inportb(_parni(1)));
}

CLIPPER memin()
{
   if (! (PCOUNT==2 || ISNUM(1) || ISNUM(2)))
      _retni(0);
   else
      _retni((char)peekb(_parni(1),_parni(2)));
}

CLIPPER memout()
{
   if (! (PCOUNT==3 || ISNUM(1) || ISNUM(2) || ISNUM(3)))
      _retl(0);
   else
   {
      pokeb(_parni(1),_parni(2),(char)_parni(3));
      _retl(1);
   }
}

CLIPPER randomic()
{
   if (! (PCOUNT==1 || ISNUM(1)))
      _retni(0);
   else
      _retni(random(_parni(1)));
}

CLIPPER dosversion()
{
   union REGS utl_reg;
   utl_reg.h.ah = 0x30;
   int86(0x21,&utl_reg,&utl_reg);
   _retni(utl_reg.x.ax);
}
