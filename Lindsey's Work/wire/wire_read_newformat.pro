 PRO wire_read_newformat,file,target,save_file,wire,cnt,$
  addt=addt,every=every,maxread=maxread

; This version is for the new Wire format for data > year 2004
; (c) March 2nd 2004 by H. Bruntt
; Example: 
; wire_read_newformat,$
;  '/data1/bruntt/wire/newformat/rfd39pb2004-046-1603.tlm',1,wire,npt
;

; spawnrob,'ls -1 /data2/bruntt/wire/newdat/data_2004*/astropkt*',file
; w = where(strmatch(file,'*.gz') eq 1,c)
; for i=0,c-1 do spawnrob,'gzip -d ' + file(w(i))
; spawnrob,'ls -1 /data2/bruntt/wire/newdat/data_2004*/astropkt*',file 
; save_file = 1

; file2 = 'alphaCen' ; debug
; file2 = file

if n_elements(addt) eq 0 then addt = 0D ; add time to julian date?
if n_elements(every) eq 0 then mode = '' else mode = 'quick'
cnt_quick = 0
if n_elements(maxread) eq 0 then maxread=1e6
readcnt = 0L

nslot = 5 ; number of stars read out!

wire_red_setup, position_file, target_file

wire_target_info, target_file, info
ninfo= n_elements(info)
cc8 = 0
for l=0,ninfo-1 do begin
 winfo = where(strmatch(target,'*'+info(l).object+'*') eq 1,cinfo)
 if cinfo ge 1 then begin
   cc8 = cc8 + cinfo
   wfind = l
   ra_dec = fltarr(2,3)
   ra_dec(0,0) = info(l).ra_1  & ra_dec(0,1) = info(l).ra_2  & ra_dec(0,2) = info(l).ra_3
   ra_dec(1,0) = info(l).dec_1 & ra_dec(1,1) = info(l).dec_2 & ra_dec(1,2) = info(l).dec_3 
 endif
endfor
if cc8 ne 1 then begin
 print,' *** Ra + Dec information not available OR currupt format !!! '
 print,' *** Check file: ' + target_file
 print,' *** Look for your star: ' + target
 stop
endif

information = info(wfind)

if n_elements(ra_dec) eq 0 then begin
 print,' *** Missing RA/DEC information to compute HELIOCENTRIC times in wire_read.pro !'
 stop
endif

ra  = ten(reform(ra_dec(0,*))) * 15.
dec = ten(reform(ra_dec(1,*)))

; if ra lt 0. then ra = ra + 360.
; --------------------------

a = strsplit(file,'.',/extract) & na = n_elements(a)
outfile = ''

for i=0,na-2 do outfile = outfile + a(i) + '.'
outfile = outfile + 'idl'

print,'In:  '+file
print,'Out: '+outfile

; five stars are read out from the CCD, each slot has 8x8 pixles 
npt = 175000L ; jan. 2006 750000L / 5.


s= ''

; Read the header: count the number of lines in header!

; spawnrob,'wc '+file, aa9
; g9 = strsplit(aa9(0),' ',/extract) & cn = long( g9(0) )


openr,1,file
;;; for i=0,cn-1 do readf,1,s ; read header (again)

wire = replicate({x:intarr(nslot),y:intarr(nslot),hjd:dblarr(nslot), $
                  stamp:strarr(nslot), $
                  d:lonarr(nslot,8,8)},npt)

header = ''
cnt = 0L
ll = ''

cnt = lonarr(nslot)
cnt(*) = 0L

; ====================================
; READING LOOP
; ====================================
; The time stamp that you noticed is in the format YYMMDD_HHMMSS.SSZ

while (not eof(1)) and (readcnt lt maxread) do begin
 readf,1,header
 readcnt = readcnt + 1

 if mode eq 'quick' then begin
  while (not eof(1)) and (cnt_quick lt every) do begin
    cnt_quick = cnt_quick + 1
    readf,1,header  
  endwhile
 endif

  a = strsplit(header,',',/extract)

  dateall = a(0)
  g = strsplit(dateall,'-',/extract)
  if n_elements(g) le 2 or $
     strmatch(dateall,'*:*') ne 1 or $
     strmatch(dateall,'*A*') eq 1 or $
     strmatch(dateall,'*c*') eq 1 then goto,skip_pt

 ; Repaired .gz files may have currupt time information:
  kolon = strsplit(dateall,':',/extract)
  if n_elements(kolon) ge 4 then goto,skip_pt


  if strlen(strcompress(g(0),/remove_all)) eq 3 then goto,skip_pt ; "?" in front of year!

  year = float(g(0))
  day  = float(g(1))
  time = g(2)
  g2 = strsplit(time,':',/extract)
  hour = float(g2(0)) 
  min  = double(g2(1)) + ( double(g2(2)) / 60. )

  slot = float(a(1)) - 1.
  if strmatch(a(1),'*NV') eq 1 or slot le -1 then goto,skip_pt
  if slot ge 5 then slot = 4

   year = year + 2000D
   multbad = 1.0
   if year gt 2020 then begin
     print,' % Something is wrong with the times ... year is: ',year
     multbad = -1.
   endif
; stop ; mission should be ended ... or year = 19?? 

   juldate, [year, 1, 1, hour, min], jd_temp ; Julian Date
   jd_temp = jd_temp + day 
   wire(cnt(slot)).hjd(slot) = jd_temp * multbad + addt

   ; print,a(0), jd_temp,format='(A25,D15.7)' ; print times for 

; FOR NEW FORMAT: two object with very different RA/DEC --- do
; not compute helio_jd here !!!
;   hjd_temp = helio_jd( jd_temp, ra, dec)   ; Heliocentric Julian Date
;  print, 'JD - HJD = ', (hjd_temp-jd_temp)*86400. ; difference in seconds ...
; jd  = jd_temp
; hjd = hjd_temp
;  wire(cnt(slot)).hjd(slot) = hjd



  wire(cnt(slot)).stamp(slot) = strcompress(dateall,/remove_all)
  ; unique time stamp ... for finding data in org. .data files!

   wire(cnt(slot)).x(slot) = float(a(5))
   wire(cnt(slot)).y(slot) = float(a(6))

negative_detected = 0B ; any "NV" data detected for this CCD stamp?

 for row=0,7 do begin
  startc = 7 + row*8
  endc   = 7 + (row+1)*8 - 1
  if endc ge n_elements(a) then goto,skip_pt ; repaired .gz files
  dat = a(startc:endc) ; string format

  w_nv = where(strmatch(dat,'*NV*') eq 1,c_nv)
  if c_nv ge 1 then begin
     negative_detected = 1
     dat(w_nv) = 0 ; NV = negative value ? Set to ZERO
  endif
  wire(cnt(slot)).d(slot,7-row,*) = dat
 endfor

  ;  print,reform(wire(cnt(slot)).d(slot,*,*))

  ; Only store data that seems to be
  ; interesting, ie. not 0 0 0 0 0 0 data !

   cut = reform(wire(cnt(slot)).d(0,*,*))  &  axx = median(cut)
   gg = uniq(cut) & ngg = n_elements(gg)
   if negative_detected eq 0 and $ ; neg. values not allowed
        max(cut) gt 50. and ngg gt 5 then begin

    ; showim,cut,axx*0.8,axx*1.2  ;  debug !!
    cnt(slot) = cnt(slot) + 1

    if cnt(slot) gt npt then stop
   endif

   skip_pt:

endwhile
close,1


cnt_max = max(cnt)

if cnt_max ge 1 then begin ; any valid points in the input file?
    wire = wire(0:cnt_max-1)
    
    outfile = ''
    a = strsplit(file,'.',/extract) & na = n_elements(a)
    for i=0,na-2 do outfile = outfile + a(i) + '.'
    outfile = outfile + '_' + target + '.idl.newformat'
    
    if save_file eq 1 then begin
     save,filename=outfile,wire, information
     print,'Wire output file saved as: '+outfile
    endif
    
endif else print,' *** No valid data to save for input file: '+file
; ====================================================

end

