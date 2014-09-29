PRO wire_read,file,save_file,wire,cnt,addt=addt

; Example: wire_read,'/ai38/bruntt/wire/data/991019.3142.1AltairA7V.data'
;wire_read,'/ai38/bruntt/wire/data/991019.3142.1AltairA7V.data',1,wire,npt
;wire_read,'/ai38/bruntt/wire/data/991019.3142.3AltairA7V.data',0,wire,npt
;wire_read,'/ai38/bruntt/wire/data/991019.3142.5AltairA7V.data'
;wire_read,'/ai38/bruntt/wire/data/991019.3142.7AltairA7V.data'
;wire_read,'/ai38/bruntt/wire/data/991019.3142.9AltairA7V.data'
;wire_read,'/ai43/bruntt/wire/altair/991105.3314.20AltairA7V.data',0,wirex,npt

nslot = 5 ; number of stars read out!
if n_elements(addt) eq 0 then addt = 0.5 ; added 27th Oct 2004!

;if strmatch(file,'*Altair*') eq 1 then $
; ra_dec = [ [19,50,47.00],[+08,52,06.0] ]
;if strmatch(file,'*HD113226*') eq 1 then $ 
; ra_dec = [ [13,02,10.60],[+10,57,32.9] ]
;if strmatch(file,'*betaUMi*') eq 1 then $
; ra_dec = [ [14,50,42.33],[+74,09,19.8] ]
;if strmatch(file,'*HD071129*') eq 1 then $
; ra_dec = [ [08,22,30.8356],[-59,30,34.139] ]
;if strmatch(file,'*216956*') eq 1 then $
; ra_dec = [ [22,57,39.0465],[-29,37,20.050] ]
;if strmatch(file,'*AlphaBoo*') eq 1 then $
; ra_dec = [ [14,15,39.6720],[+19,10,56.677] ]
;if strmatch(file,'*AlphaPav*') eq 1 then $
; ra_dec = [ [20,25,38.8578],[-56,44,06.324] ]
;if strmatch(file,'*BetaCas*') eq 1 then $
; ra_dec = [ [00,09,10.6851],[+59,08,59.207] ]
;if strmatch(file,'*GammaEqu*') eq 1 then $
; ra_dec = [ [21,10,20.5001],[+10,07,53.686] ]
;if strmatch(file,'*EpsilonPeg*') eq 1 then $
; ra_dec = [ [21,44,11.1581],[+09,52,30.041] ]
;if strmatch(file,'*GammaLeo*') eq 1 then $
; ra_dec = [ [10,19,58.3545],[+19,50,29.359] ]
;if strmatch(file,'*HR1910*') eq 1 then $
; ra_dec = [ [05,37,38.69  ],[+21,08,33.2  ] ]


; Added March 2nd 2004 by H. Bruntt:
; wire_target_info, '~/wire/targets_wire.txt', info
; Robust filenames 27th of October 2004:
wire_red_setup, position_file, target_file
wire_target_info, target_file, info

ninfo= n_elements(info)
cc8 = 0
for l=0,ninfo-1 do begin
 winfo = where(strmatch(file,'*'+info(l).object+'*') eq 1,cinfo)
 if cinfo ge 1 then begin
   cc8 = cc8 + cinfo
   wfind = l
   ra_dec = fltarr(2,3)
   ra_dec(0,0) = info(l).ra_1  & ra_dec(0,1) = info(l).ra_2  & ra_dec(0,2) = info(l).ra_3
   ra_dec(1,0) = info(l).dec_1 & ra_dec(1,1) = info(l).dec_2 & ra_dec(1,2) = info(l).dec_3 
 endif
endfor
if cc8 ne 1 then stop
information = info(wfind)

if n_elements(ra_dec) eq 0 then begin
 print,' *** Missing RA/DEC information to compute HELIOCENTRIC times in wire_read.pro !'
 stop
endif

; IMPORTANT TO REFORM!
ra  = ten(reform(ra_dec(0,*))) * 15.
dec = ten(reform(ra_dec(1,*)))

; Sanity check:
; print,'RA: ', ra ,info(wfind).ra2
; print,'DEC: ',dec,info(wfind).dec2

; --------------------------

a = strsplit(file,'.',/extract) & na = n_elements(a)
outfile = ''

for i=0,na-2 do outfile = outfile + a(i) + '.'
outfile = outfile + 'idl'

;print,'In:  '+file
;print,'Out: '+outfile

; five stars are read out from the CCD, each slot has 8x8 pixles 
npt = 250000L / 5.

get_lun,u
openr,u,file
s= ''

; Read the header: count the number of lines in header!
cn = 0L
head_read:

if (not eof(u)) then readf,u,s else goto,end_of_file
  ;  print,s
  cn = cn + 1
  if strmatch(s,'DATIME=>*') eq 0 then goto,head_read
end_of_file:
close,u
free_lun,u

get_lun,u
openr,u,file
 for i=0,cn-2 do readf,u,s ; read header (again)

wire = replicate({x:intarr(nslot),y:intarr(nslot),hjd:dblarr(nslot), $
                  stamp:strarr(nslot), $
                  d:lonarr(nslot,8,8)},npt)

header = ''
cnt = 0L
 ll = ''

; cntrr = findgen(nslot) & cnt_limit = total(cntrr)
; cnt_limit = 0 + 1 + 2 + 3 + 4
; sl = 0
;  mm = 0B
; Philosophy for storing data: Avoid empty slots ...
; sl: fill up the slots ... 

cnt = lonarr(nslot)
cnt(*) = 0L

; ====================================
; READING LOOP
; ====================================
; The time stamp that you noticed is in the format YYMMDD_HHMMSS.SSZ

while not eof(u) do begin
 readf,u,header
 if header eq '' or strmatch(header,'*DATIME*') eq 0 then goto,agg ; empty line

  a = strsplit(header,'>',/extract)

  d = a ; strsplit(header,'>',/extract)
  e = strsplit(d(2),',',/extract)
   slot = float(e(0)) - 1 ; running from 1 .. 5 (I use 0..4)

  b = strsplit(a(1),'_',/extract)
   time1 = b(0) ; float(b(0))
  c = strsplit(b(1),'Z',/extract)
   time2 = c(0) ; double(c(0))


  wire_hjd, time1, time2, ra, dec, jd, hjd


   wire(cnt(slot)).hjd(slot) = jd + addt ; hjd + addt ; addt added 27th Oct 2004 !!



;  print,' %%% STORING JULIAN DATE (NOT HJD) ... CONVERT TO HJD IN WIRE_POS'

  gg = strsplit(a(1),'Z',/extract)
  wire(cnt(slot)).stamp(slot) = gg(0) ; time stamp ... for finding data in org. .data files!

  f = strsplit(d(3),',',/extract)
   wire(cnt(slot)).x(slot) = float(f(0))
   wire(cnt(slot)).y(slot) = float(d(4))

 for row=0,7 do begin
  readf,u,ll
  ll2 = strsplit(ll,' ',/extract)
  ll3 = float(ll2)
;  w = where(ll3 le 0.,c) & if c ge 1 then ll3(w) = ll3(w) + 65536. ; Digital saturation 2^16. 
  wire(cnt(slot)).d(slot,7-row,*) = ll3
 endfor

  cnt(slot) = cnt(slot) + 1
  if cnt(slot) gt npt then stop

; Debug:
;  print,header
;  print,cnt
;  print,total(wire(cnt(slot)-1).d(slot,*,*)) - 64. * wire(cnt(slot)-1).d(slot,0,0)
;  print,''
;  s = get_kbrd(1)

; Debug:
;   cut = reform(wire(cnt).d(0,*,*))  &  a = median(cut)
;   showim,cut,a*0.9,a*1.2  &   surface,cut

 agg:
endwhile
close,u
free_lun,u

;plot,wire.time2-median(wire.time2),psym=3
;cut = reform(wire(100).d(0,*,*))
;a = median(cut)
;showim,cut,a*0.9,a*1.2

; ====================================================
; print,' %%% Data points: '+strcompress(string(cnt),/remove_all)+' in file: '+file

cnt_max = max(cnt)

if cnt_max ge 1 then begin ; any valid points in the input file?
    wire = wire(0:cnt_max-1)
    
    outfile = ''
    a = strsplit(file,'.',/extract) & na = n_elements(a)
    for i=0,na-2 do outfile = outfile + a(i) + '.'
    outfile = outfile + 'idl'
    
    if save_file eq 1 then begin
     save,filename=outfile,wire, information
     print,'Wire output file saved as: '+outfile
    endif
    
endif else print,' *** No valid data to save for input file: '+file
; ====================================================

end
