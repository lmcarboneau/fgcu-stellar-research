PRO  m4_get_basedir, base

base = ''

spawnrob,'hostname',host

; Hans Bruntt's stationary PC:
if strmatch(host,'*amalthea*') eq 1 then begin
 base = '/ai40/bruntt/' ; '/ai1/bruntt/'
endif

; if strmatch(host,'*phys*') eq 1 then begin
;  base = '/ai40/bruntt/'
; endif

; Hans Bruntt's Dell laptop: inspiron 9200, januar 2005
if strmatch(host,'*manowar*') eq 1 then begin
 base = '/home/bruntt/'
endif

; Hans Bruntt's stationary PC in Sydney, Feb. 2006
if strmatch(host,'brixx.physics.usyd.edu.au') eq 1 then begin
 base = '/export/brixx1/bruntt/'
endif

; Rahmi, Dec 2006+
if strmatch(host,'vesta*') eq 1 or strmatch(host,'centaurus*')  or $
   strmatch(host,'gonzo*') eq 1 or strmatch(host,'drteeth*') then begin
 base = '/import/suphys2/bruntt/'
endif

; NBI inspiron 9200, januar 2005
if strmatch(host,'*taurus*') eq 1 then begin
 base = '/home/bruntt/'
endif

; NBI inspiron 9200, januar 2005
if strmatch(host,'*leo*') eq 1 then begin
 base = '/home/bruntt/'
endif

; Hans Bruntt's Dell laptop: inspiron 8600
if strmatch(host,'*brunttlt*') eq 1 then begin
 base = '/mnt/WinC/linux/'
endif

; USAFA
if strmatch(host,'*usafa*') eq 1 then begin
 base = '/home/bruntt/'
endif

; USAFA Heather change your home path here:
if strmatch(host,'*thales*') eq 1 then begin
 base = '/home/hlp/'
endif

; USAFA ; Derek: Change your home path here:
if strmatch(host,'*archimedes*') eq 1 then begin
 base = '/home/buzasi/'
endif



END
