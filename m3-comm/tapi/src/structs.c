
#include <tapi.h>

#define CHECK_STRUCT(nm,tipe) { \
  struct { char a;  tipe b; } x; \
  int size, align; \
  size = sizeof (tipe); \
  align = ((char*)&x.b) - ((char*)&x); \
  printf ("%4d  %4d %s\n", size, align, nm); \
};

int main ()
{
  CHECK_STRUCT ("HCALL__", struct HCALL__)
  CHECK_STRUCT ("HLINE__", struct HLINE__)
  CHECK_STRUCT ("HPHONE__", struct HPHONE__)
  CHECK_STRUCT ("HLINEAPP__", struct HLINEAPP__)
  CHECK_STRUCT ("HPHONEAPP__", struct HPHONEAPP__)
  CHECK_STRUCT ("phonebuttoninfo_tag", struct phonebuttoninfo_tag)
  CHECK_STRUCT ("phoneextensionid_tag", struct phoneextensionid_tag)
  CHECK_STRUCT ("phonecaps_tag", struct phonecaps_tag)
  CHECK_STRUCT ("phonestatus_tag", struct phonestatus_tag)
  CHECK_STRUCT ("varstring_tag", struct varstring_tag)
  CHECK_STRUCT ("lineaddresscaps_tag", struct lineaddresscaps_tag)
  CHECK_STRUCT ("lineaddressstatus_tag", struct lineaddressstatus_tag)
  CHECK_STRUCT ("linedialparams_tag", struct linedialparams_tag)
  CHECK_STRUCT ("linecallinfo_tag", struct linecallinfo_tag)
  CHECK_STRUCT ("linecalllist_tag", struct linecalllist_tag)
  CHECK_STRUCT ("linecallparams_tag", struct linecallparams_tag)
  CHECK_STRUCT ("linecallstatus_tag", struct linecallstatus_tag)
  CHECK_STRUCT ("lineextensionid_tag", struct lineextensionid_tag)
  CHECK_STRUCT ("linedevcaps_tag", struct linedevcaps_tag)
  CHECK_STRUCT ("linedevstatus_tag", struct linedevstatus_tag)
  CHECK_STRUCT ("lineforward_tag", struct lineforward_tag)
  CHECK_STRUCT ("lineforwardlist_tag", struct lineforwardlist_tag)
  CHECK_STRUCT ("linegeneratetone_tag", struct linegeneratetone_tag)
  CHECK_STRUCT ("linemediacontrolcallstate_tag", struct linemediacontrolcallstate_tag)
  CHECK_STRUCT ("linemediacontroldigit_tag", struct linemediacontroldigit_tag)
  CHECK_STRUCT ("linemediacontrolmedia_tag", struct linemediacontrolmedia_tag)
  CHECK_STRUCT ("linemediacontroltone_tag", struct linemediacontroltone_tag)
  CHECK_STRUCT ("linemonitortone_tag", struct linemonitortone_tag)
  CHECK_STRUCT ("linereqmakecall_tag", struct linereqmakecall_tag)
  CHECK_STRUCT ("linereqmediacall_tag", struct linereqmediacall_tag)
  CHECK_STRUCT ("linetermcaps_tag", struct linetermcaps_tag)
  CHECK_STRUCT ("linetranslateoutput_tag", struct linetranslateoutput_tag)
  CHECK_STRUCT ("linetranslatecaps_tag", struct linetranslatecaps_tag)
  CHECK_STRUCT ("linelocationentry_tag", struct linelocationentry_tag)
  CHECK_STRUCT ("linecardentry_tag", struct linecardentry_tag)
  CHECK_STRUCT ("linecountrylist_tag", struct linecountrylist_tag)
  CHECK_STRUCT ("linecountryentry_tag", struct linecountryentry_tag)
  CHECK_STRUCT ("lineproviderlist_tag", struct lineproviderlist_tag)
  CHECK_STRUCT ("lineproviderentry_tag", struct lineproviderentry_tag)
  return 0;
}
