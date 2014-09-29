PRO wire_temp_info, dirr, target, addt=addt

; ============================================================
spawnrob,'ls -1 ' + dirr  + '/astropkt38*',files
nl = n_elements(files)

w = where(strmatch(files,'*.gz') eq 1,c)
for i=0,c-1 do $
 spawnrob,'gzip -d ' + files(w(i)),comm

spawnrob,'ls -1 ' + dirr  + '/astropkt38*',files
nl = n_elements(files)
; ============================================================

; ============================================================
nf = n_elements(files)
; ============================================================

; ============================================================
if n_elements(addt) eq 0 then addt = 0.5 ; comp. w/ Uytterhoeven (Lam Sco)
; ============================================================

; ============================================================
; Define result structure: temperature and Heliocentric JD
; ============================================================
cnt = 0L
nmax = 4.5e6
log = replicate( {hjd:0D, t:0.}, nmax)
; ============================================================

; ============================================================
; Get the RA/DEC for this object (needed for HJD calculation)
; ============================================================
wire_red_setup, position_file, target_file
wire_target_info, target_file, info
ninfo= n_elements(info)
winfo = where(strmatch(info.object,target) eq 1,cinfo)
if cinfo ne 1 then begin
 print,' %%% Target not found in wire_corr_hjd.pro: ' + target
 stop
endif

ra = info(winfo).ra2 & dec = info(winfo).dec2
; ============================================================

; ============================================================
; For each input file, restore the wire structure:
; ============================================================
for f=0,nf-1 do begin
; ============================================================

; ============================================================
file = files(f)
close,1
openr,1,file
; ============================================================

; ============================================================
while not eof(1) do begin
; ============================================================

 temp = ''
 readf,1,temp

 hh = strsplit(temp,',',/extract) & ng = n_elements(g)
 dateall     = hh(0)

  g = strsplit(dateall,'-',/extract)
  if n_elements(g) le 2 or $
     strmatch(dateall,'*:*') ne 1 or $
     strmatch(dateall,'*A*') eq 1 or $
     strmatch(dateall,'*c*') eq 1 then goto,skip_pt

 temperature  = float(hh(1))
 flag         = long(hh(2))

 ; Repaired .gz files may have currupt time information:
  kolon = strsplit(dateall,':',/extract)
  if n_elements(kolon) ge 4 then goto,skip_pt

; "?" in front of year ?
  if strlen(strcompress(g(0),/remove_all)) ge 3 then goto,skip_pt 

  year = float(g(0))
  day  = float(g(1))
  time = g(2)
  g2 = strsplit(time,':',/extract)
  hour = float(g2(0)) 
  min  = double(g2(1)) + ( double(g2(2)) / 60. )

   year = year + 2000D
   multbad = 1.0
   if (year ge 2009) or (year le 2002) then begin
     print,' % Something is wrong with the times ... year is: ',year
     multbad = -10.
;     stop
   endif

   juldate, [year, 1, 1, hour, min], jd_temp ; Julian Date
   jd_temp = jd_temp + day + addt

   hjd_temp = helio_jd( jd_temp, ra, dec)   ; Heliocentric Julian Date

   log(cnt).hjd = hjd_temp * multbad
   log(cnt).t   = temperature
   cnt = cnt + 1

skip_pt:

 endwhile ; next line in file
endfor ; next file

; ============================================================
if cnt ge 1 then log = log(0:cnt-1) else log = -1
w = where(log.hjd gt 52500.,c) & if c ge 1 then log = log(w)
print,' %%% Stored ' + strcompress(c) + ' temperatures...'

outfile = dirr + '/' + target + '_info.idl'
save,filename=outfile,log,/compress

print,' %%% Saved file: ' + outfile
; ============================================================

; ============================================================
; Zip the files to save space:
; ============================================================
spawnrob,'ls -1 ' + dirr  + '/astropkt38*',files
nl = n_elements(files)
for i=0,nl-1 do $
 spawnrob,'gzip ' + files(i),comm2
; ============================================================

if c ge 10 then $
 plot,log.hjd-53000.,smooth(log.t,101,/edge),psym=3,min_value=-100,$
  xtit='Temperature',ytit='HJD - 53000',tit='WIRE'

END
