; Backup til maxtor

; ++++++++++++++++++++++++++++++++++++++
; brixx computer in Sydney
; ++++++++++++++++++++++++++++++++++++++
; backdir ='/export/brixx2/bruntt/backup/'
; ++++++++++++++++++++++++++++++++++++++

 backdir ='/media/usbdisk/backup/' & mount ='/media/usbdisk/' ; brixx
 backdir ='/mnt/sda1/backup/' & mount ='/mnt/sda1/' ; manowar

; backdir ='/media/usbdisk1/backup/'
; ++++++++++++++++++++++++++++++++++++++

; ++++++++++++++++++++++++++++++++++++++
; laptop, manowar
; ++++++++++++++++++++++++++++++++++++++
;  backdir = '/mnt/drive/backup/'
;  backdir = '/mnt/sda1/backup/'
;  backdir = '/mnt/sda5/backup/'
;  backdir = '/mnt/sdb5/backup/'
; ++++++++++++++++++++++++++++++++++++++

do_mount = 0B
if n_elements(mount) ne 0 then do_mount = 1B

print,' %%% HAVE YOU BOOTED LAPTOP WTH MAXTOR ON?'
hitme,s9
print,' %%% HAVE YOU MOUNTED THE DISC BY CLICKING THE '+backdir+' ICON ? '
hitme,s9

default9, checksize, 1B
default9, doit, 1B

m4_get_basedir, add ; itinerary
add2 = '/export/brixx2/bruntt/'



; IMPORTANT: Remember the slash at the end of each ENTRY:
;
dirs = ['bin/','binary/','Documents/',$
        'ess/','evol/','hp/','idl/','idlvwa/',  $
        'papers/','Pictures/', 'software/',$
        'wire/','wire_process/','wire_analysis/','mails/']

dirs2 = ['VWA/','VWA2/']

; private ---> mails in Sydney.
; results ---> psicen spectra from FEROS 2006.

; "dirs2" is the array that is actually used below!

dirs2 = [add + dirs, add2 + dirs2, '/import/suphys2/bruntt/private/','/export/brixx2/bruntt/psicen/results/']
dirs2 = ['/export/brixx2/bruntt/psicen/results/','/export/brixx1/bruntt/m67/','/export/brixx1/bruntt/idl/','/export/brixx1/bruntt/wire/']



; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=
; Returning from AAT in January 2007. Need to transfer directories
; from laptop to maxtor --> brixx stationary computer at USyd.
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=

base = '/home/bruntt/'
dd = ['VWA2', 'binary', 'wire_process', 'idl',$
      'VWA', 'hp', 'psicen', 'm67',$
      'wire_analysis', 'papers', 'idlvwa',$
      'wire', 'Picturesnew', 'procyon', 'Documents']
dirs2 = [base + dd + '/', '/mnt/hda5/data/deltaOri/']
; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++=


nd = n_elements(dirs2)


; I don't think this is used? March 1 2006?
; files = ['14RAPPORT.txt']
; nf = n_elements(files)


time = systime(0)
x = strsplit(time,' ',/extract)
fil = x(2) + x(1) + x(4) + '.tar'

spawnrob,'pwd',orgdir


x = strsplit(backdir,'/',/extract) & nx = n_elements(x)
s = '/'
for j=0,nx-2 do s = s + x(j) + '/'
spawnrob,'ls -1 '+s,pp
wg = where(strmatch(pp,'backup') eq 1,cg)

if cg ne 1 then begin
 print,' '
 print,' %%% Must have root permission to write to maxtor: '
 print,'su'
 print,' mount /mnt/sda1/'
 print,' mkdir '+backdir
 print,' chown bruntt '+backdir
 print,''
 print,' *** PROGRAM STOPS HERE !!! '
 print,''
 stop
endif


td = backdir


; ==========================================================
; Erase old files:
; ==========================================================
if do_mount then spawnrob,'mount ' + mount
spawnrob,'rm -fr ' + td + '/*'
; spawnrob,'mkdir ' + td ; dir must be created by ROOT !!!

for i=0,nd-1 do begin
 temp = dirs2(i)
 x2 = strsplit(temp,'/',/extract)
 nx2 = n_elements(x2)
 tempname = x2(nx2-1) + '.tar'


; Go to basis dir: do not make the 'complete' path in the tar archive
 xdir = strsplit(dirs2(i),'/',/extract)
 nx = n_elements(xdir)
 dirtemp = ''
 dirbase = '/' + xdir(0) + '/' + xdir(1) + '/'
 cd,dirbase
 for k=2,nx-1 do dirtemp = dirtemp + xdir(k) + '/'

 command = 'tar -cvf ' + td + tempname + ' ' + dirtemp
  print,command

 command2 = 'du -hs ' + dirtemp
 if checksize then begin
   spawnrob, command2, res
   hh = strsplit(res,'M',/extract) & sizm = hh(0)
   hh = strsplit(res,'G',/extract) & sizg = hh(0)
   if n_elements(sizg) ge 2 then begin
    sizgiga = float(sizg)
    if sizgiga ge 4 then begin
      print,''
      print,' *** FILE SIZE > 4 Gb. This will cause problems!'
      print,' *** Solution: Create sub directories with less data!'
      print,''
      stop
    endif
   endif
   ; print, ' *** Check that filesize is < 4 Gb: ' + h(0)
 endif

 if doit then begin
  if do_mount then spawnrob,'mount ' + mount
  spawnrob,command
 endif

endfor

spawnrob,'ls -1 ' + td,tared
ntar = n_elements(tared)

print,''
print,' %%% The following tar files were created: '
for j=0,ntar-1 do print,tared(j)

cd,orgdir

END
