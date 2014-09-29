; First you identify all targets with wire_auto_field. 
; Then run wire_simbad.pro... that program will give you
; instructions! Then run this program: wire_combine_info

; ++++++++++++++++++++++++++++
; Known problems:
; 1. Some rotational velocities are given as upper estimates.
; program cannot handle this: for star HD 95689 replace:
;  ^ ROT  |<17   (   )|1970CoKwa.189.....U|
; with
;  ^ ROT  | -17   (   )|1970CoKwa.189.....U|
;
; 2. Some entries have their uvby data printed twice. 
; Remove one of the lines. Stars: 87901,197989,100287, 100287 (entry
; found twice)

; 3. Remember that the input file: vizier_par.out
; ... Must only have 6 entries per star!

; 4. Leave one empty line at the end of file vizier_par.out

; ++++++++++++++++++++++++++++


; ++++++++++++++++++++++++++++
; History:
; Updated 2006 Nov 8 by HB

; Add this name to the output files from SIMBAD/VizieR
addout = '.nov06b'

m4_get_basedir,basedir
f = basedir + 'wire/wire_essential/xy_positions4.idl'
restore, f

debug_read = 1B

nmax = 750 ; max 750 objects

; 'Syncronized' with wire_combine_gamma_info:
wireobj = replicate({nam:'', hd:0L, hdnam:'', hip:0L, $
                     p:0B, $ ; prim / sec?
                     ra:0., ra1:'', dec:0., dec1:'', $
                     V:0., BV:0., par:-1.0, epar:99., $
                     vsini:999., evsini:99., $
                     hbeta:99., by:99., c1:99., m1:99., $
                     ehbeta:9., eby:9., ec1:9., em1:9., $
                     nhbeta:0., nuvby:0., $
                     spec1:'', spec2:'', spec3:'',$
                     lumclass:0B, class:0B, $
                     adu:0L, mag:0., ptp:0., rms:0., nf:0, $
                     t1:0., t2:0., dt:0., $
                     lumn:-1., elumn:-1., mbol:99., embol:99., $
                     teff:-1., logg:-1., feh: 9.9, $   ; == adopted Teff
                     eteff:-1., elogg:-1., efeh:-1., $
                       teff_cas:-1., logg_cas:-1., feh_cas: 9.9, $    ; Cassini 1997 calibr.
                       eteff_cas:-1., elogg_cas:-1., efeh_cas:-1., $
                      teff_nap:-1., logg_nap:-1., feh_nap: 9.9, $    ; Napiwotzski 1998 calibr.
                      eteff_nap:-1., elogg_nap:-1., efeh_nap:-1., $
                       teff_rib:-1., logg_rib:-1., feh_rib: 9.9, $    ; Ribas 1997 calibr.
                       eteff_rib:-1., elogg_rib:-1., efeh_rib:-1., $
                      mass_templogg:0., radius_templogg:0., $
                     extby:-1., e_extby:-1., $
                     mv_templogg:-1., emv_templogg:-1., $        
                     vmic:0., vmac:0., obs:'', $
                     bc: 99., ebc:-1., dist:-1., edist:-1., $
                     lcfile1:'', lcfile1_sub:'', $
                     lcfile2:'', lcfile2_sub:'', $
                     lcfile3:'', lcfile3_sub:'', $
                     wirefile:''}, nmax)

; lumclass:
; V IV III II I
; 5  4  3  2  1

; Class
; OBAFAKM
; 1234567

; ==========================================================
; Input given to Simbad. Made with program wire_simbad.pro
; ==========================================================
base = basedir + 'wire/wire_field/simbad_parameters.txt'
openr,1,base
slut_head = 0B
cnt = 0
hdnam = strarr(500)
temp = ''

while not eof(1) do begin
  readf,1,temp
  if slut_head eq 0 then begin
   if strmatch(temp,'*}*') then slut_head=1B
  endif else begin
  
    hdnam(cnt) = temp
    cnt = cnt + 1
  endelse
endwhile


hdnam = hdnam(0:cnt-1)
wireobj(0:cnt-1).nam = hdnam 
  ; hdnam = unique entries in the file simbad_parameters.txt


close,1

; Gamma Leo does not have a HD number!
wleo = where(strmatch(hdnam,'Gamma Leo',/fold) eq 1,leo)
if leo eq 1 then begin
 wireobj(wleo).nam   = 'Gamma Leo'
 wireobj(wleo).hd    = 888001L
 wireobj(wleo).hdnam = 'HD 888001'
 wireobj(wleo).hip   = 50583L 
endif

; ==========================================================

; ==========================================================
; Read simbad output
; ==========================================================
base = basedir + 'wire/wire_field/simbad_hd.out' + addout
openr,1,base
slut_head = 0B
cnt = 0
hdnam = strarr(500)
temp = ''

while not eof(1) do begin
  readf,1,temp
  if strcompress(temp,/remove_all) ne '' then begin  
    hdnam(cnt) = temp
    cnt = cnt + 1
  endif
endwhile
close,1


hdnam = hdnam(0:cnt-1)
wireobj(0:cnt-1).hdnam = hdnam
for j=0,cnt-1 do begin
 g  = strsplit(wireobj(j).hdnam,' ',/extract)
 wg = where(strmatch(wireobj(j).hdnam,'*HD*') eq 1,cg)
 if cg eq 1 then begin
   wireobj(j).hd = long(g(1)) 
 endif else begin
   if strmatch(hdnam(j),'*CCDM*J10199*1951AB*') then begin
         wireobj(j).nam   = 'Gamma Leo'
         wireobj(j).hd    = 888001L
         wireobj(j).hdnam = 'HD 888001'
         wireobj(j).hip   = 50583L 
   endif
   if strmatch(hdnam(j),'*SAO 11802*') then begin
         wireobj(j).nam   = 'IM Cas'
         wireobj(j).hd    = 888002L
         wireobj(j).hdnam = 'HD 888002'
         wireobj(j).hip   = 7139
   endif
endelse

endfor


; ==========================================================


s       = sort(wireobj.hd)
wireobj = wireobj(s) ; sort by HD number
g       = uniq(wireobj.hd)
wireobj = wireobj(g) ; remove multiple entries

print,' %%% Number of unique entries in wireobj: ',n_elements(wireobj)
print,''
wait,3

; ==========================================================
; Read simbad output
; ==========================================================
base = basedir + 'wire/wire_field/simbad_parameters.out' + addout
openr,1,base
newobj = 1B
cnt = 0
nam = strarr(500)
temp = ''

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
while not eof(1) do begin

  sum = ''
  while newobj and not eof(1) do begin
     readf,1,temp 
     sum = sum + temp
     if (strcompress(temp,/remove_all) eq '' and $
        strmatch(sum,'*HD*')) or $
        (strcompress(temp,/remove_all) eq '' and $
        strmatch(sum,'*CCDM*')) or $
        (strcompress(temp,/remove_all) eq '' and $
        strmatch(sum,'*SAO 11802*')) then newobj=0B ; new object!
 endwhile ; ADDED CCDM ON 23RD FEB 2005, ADDED SAO 11802 on 8 NOV 2006

;   wmmm = where(strmatch(sum,'*71129*') eq 1, cmmm) 
;   if cmmm eq 1 then stop

   if strcompress(sum,/remove_all) eq '' then goto,skip_lst

   g = strsplit(sum,'^',/extract)
   g2 = strsplit(g(0),' ',/extract)
   hdname = g2(0) + ' ' + g2(1)
   wleo3 = where(strmatch(hdname,'*J10199*1951AB*',/fold) eq 1,cleo3)
    if cleo3 eq 1 then hdname = 'HD 888001'
   wleo4 = where(strmatch(hdname,'*SAO*11802*',/fold) eq 1,cleo4)
    if cleo4 eq 1 then hdname = 'HD 888002'

   w = where(wireobj.hdnam eq hdname,c)
   if c ne 1 then begin
      print,' %%% Star read from simbad_parameters.out' + addout+' not found in simbad_parameters.txt'
      print,' %%% Did you query SIMBAD with updated file: '+$
                   basedir + 'wire/wire_field/simbad_parameters.txt'
      stop
   endif

 
   ; Debug
   ; if wireobj(w).hd eq 101666 then print,g

 
   wireobj(w).spec1 = strcompress(g(1),/remove_all) ; simbad spec type
 
 ; The order of these if's is important!!!
   if strmatch(wireobj(w).spec1,'*I*') eq 1 then $
    wireobj(w).lumclass = 1
   if strmatch(wireobj(w).spec1,'*II*') eq 1 then $
    wireobj(w).lumclass = 2
   if strmatch(wireobj(w).spec1,'*III*') eq 1 then $
    wireobj(w).lumclass = 3
   if strmatch(wireobj(w).spec1,'*V*') eq 1 then $
    wireobj(w).lumclass = 4
   if strmatch(wireobj(w).spec1,'*IV*') eq 1 then $
    wireobj(w).lumclass = 5

   if strmatch(wireobj(w).spec1,'*O*') eq 1 then $
    wireobj(w).class = 1
   if strmatch(wireobj(w).spec1,'*B*') eq 1 then $
    wireobj(w).class = 2
   if strmatch(wireobj(w).spec1,'*A*') eq 1 then $
    wireobj(w).class = 3
   if strmatch(wireobj(w).spec1,'*F*') eq 1 then $
    wireobj(w).class = 4
   if strmatch(wireobj(w).spec1,'*G*') eq 1 then $
    wireobj(w).class = 5
   if strmatch(wireobj(w).spec1,'*K*') eq 1 then $
    wireobj(w).class = 6
   if strmatch(wireobj(w).spec1,'*M*') eq 1 then $
    wireobj(w).class = 7
   if strmatch(wireobj(w).spec1,'*C*') eq 1 then $
    wireobj(w).class = 8
   if strmatch(wireobj(w).spec1,'*S*') eq 1 then $
    wireobj(w).class = 8

;   if wireobj(w).hd eq 38307 then stop 
;   if wireobj(w).hd eq 40183 then stop 
;   if wireobj(w).hd eq 216672 then stop  ; print,wireobj(w).spec1

   bv1 = strsplit(g(2),' ',/extract)
   bv2 = bv1(0)

   s1 = strmatch(bv1(1),'*V*')
   if s1 then bv2 = bv2 + ' ' + bv1(2) else $
              bv2 = bv2 + ' ' + bv1(1)

   bv = strsplit(bv2,' ',/extract)

   wireobj(w).v  = float(bv(1)) ; V MAGNITUDE
 if strmatch(g(2),'*V4*') eq 0 then $  ; Exception: B magnitude missing for HD 59950
   wireobj(w).bv = float(bv(0)) - float(bv(1)) else wireobj(w).bv = -99.

; H Beta ?
; DEBUG: Problems with the conversion of data: Print the "g"
; parameter to identify the star:
; print,g  ; for i=0,n_elements(g)-1 do print,g(i)

; if strmatch(sum,'*23 54 23.0324*') then stop
; if strmatch(sum,'*07 28 02.0747 *') then stop
; if strmatch(sum,'*05 37 38.6858 *') then stop

   if strcompress(g(3),/remove_all) ne '' then begin
 
       hb1 = strsplit(g(3),'|',/extract)
       v = strsplit(hb1(1),' ',/extract)
       wireobj(w).hbeta = float(v(0))
       if n_elements(v) eq 2 then $
         wireobj(w).ehbeta = float(v(1))
         if strcompress(hb1(2),/remove_all) ne '' then $
            wireobj(w).nhbeta = fix(hb1(2))

   endif

; Stromgren uvby available ?

; Two formats for uvby data:
; uvby1 m|  .282  .002|  .137  .002|  .909  .004|   20| AB |1998A&AS..129..431H|
; uvby m| -.092|  .065|  .010|     |1980A&AS...40....1H|

   if strcompress(g(4),/remove_all) ne '' then begin
 
       uvby = strsplit(g(4),'|',/extract)
       
       uvby2  = strsplit(uvby(0),' ',/extract) & uvby2 = uvby2(0)

; Format
      if strcompress(uvby2,/remove_all) eq 'uvby1' then begin

       v = strsplit(uvby(1),' ',/extract)
       wireobj(w).by = float(v(0))
       if n_elements(v) eq 2 then begin
        if strcompress(v(1)) ne '~' then $
         wireobj(w).eby = float(v(1)) else $
         wireobj(w).eby = 8.
       endif 

; c1 and m1 were swapped on the 23rd of Febr. 2005:
       v = strsplit(uvby(2),' ',/extract)
       wireobj(w).m1 = float(v(0))
       if n_elements(v) eq 2 then begin
        if strcompress(v(1)) ne '~' then $
         wireobj(w).em1 = float(v(1)) else $
         wireobj(w).em1 = 8.
       endif 

       v = strsplit(uvby(3),' ',/extract)
       if v(0) ne '~' then begin
        wireobj(w).c1 = float(v(0))
         if n_elements(v) eq 2 then begin
            if strcompress(v(1)) ne '~' then $
             wireobj(w).ec1 = float(v(1)) else $
             wireobj(w).ec1 = 8.
         endif
      endif
     endif

; Format uvby
      if strcompress(uvby2,/remove_all) eq 'uvby' then begin

       v = strsplit(uvby(1),' ',/extract)
       wireobj(w).by = float(v(0))
       if n_elements(v) eq 1 then begin
        if strcompress(v(0)) ne '~' then $
         wireobj(w).eby = 99.
       endif 

; c1 and m1 were swapped on the 23rd of Febr. 2005:
       v = strsplit(uvby(2),' ',/extract)
       wireobj(w).m1 = float(v(0))
       if n_elements(v) eq 1 then begin
        if strcompress(v(0)) ne '~' then $
         wireobj(w).em1 = 99.
       endif 

       v = strsplit(uvby(3),' ',/extract)
       wireobj(w).c1 = float(v(0))
       if n_elements(v) eq 1 then begin
        if strcompress(v(0)) ne '~' then $
         wireobj(w).ec1 = 99.
      endif
     endif




   endif


     


; Rotational velocity, projected? vsini
   if strcompress(g(5),/remove_all) ne '' then begin
 
       rot1 = strsplit(g(5),'|',/extract)
       sr = strsplit(rot1(1),' ',/extract)
       wireobj(w).vsini = fix(sr(0))
       if n_elements(sr) ge 2 then begin
          s2 = strsplit(rot1(1),'(',/extract)
          s4 = strsplit(s2(1),')',/extract)
          tal = strcompress(s4(0),/remove_all)
          if tal ne '' and tal ne '~' then $
           wireobj(w).evsini = fix(s4(0))          
          ; print,sr,s2,s4
       endif
   endif

; RA / DEC
   if strcompress(g(6),/remove_all) ne '' then begin
       s = strsplit(g(6),' ',/extract)
       ra1  = float([s(0), s(1), s(2)])
       dec1 = float([s(3), s(4), s(5)])
 
        ra = ten(ra1) * 15.
       dec = ten(dec1)

       wireobj(w).ra  = ten(ra1) * 15
       wireobj(w).dec = ten(dec1)

       wireobj(w).ra1  = s(0) + ' ' + s(1) + ' ' + s(2)
       wireobj(w).dec1 = s(3) + ' ' + s(4) + ' ' + s(5)
   endif


skip_lst:

   newobj = 1
   ; print,sum

no_simbad_info:

endwhile


close,1

b = where(wireobj.nam ne '' and wireobj.class eq 0,cb)
if cb ge 1 then begin
 print,' %%% Possible bad stars ... Apparently no spec type? '
 print,nam,wireobj(b).hd
endif


; ==========================================================
; Import VIZIER information from HIPPARCOS
; ==========================================================
base = basedir + 'wire/wire_field/vizier_par.out' + addout
openr,1,base
cnt = 0
temp = ''
init = 1B
cl = 0L ; count number of lines read

while not eof(1) do begin

; Keep reading lines until the string "HD" is found within the line:
 if strmatch(temp,'*HD*') eq 0 then begin
    readf,1,temp & cl = cl + 1
 endif
 if init then begin
  if strmatch(temp,'*HD*') eq 1 then init = 0B
 endif 

 if debug_read then print,' %%% read temp = ' + temp + ' ... Init = ' + string(init,format='(I1)')
 if init eq 0 then begin

  if strmatch(temp,'*Available*') then goto,skip_ln
  if strmatch(temp,'*results with*') then goto,skip_ln
  if strmatch(temp,'*No object found around*') then goto,skip_star ; Parallax not avail!

  cnt = 0
  if strcompress(temp,/remove_all) eq '' then goto,skip_ln

  hd1 = strsplit(temp,' ',/extract)
  hd0 = long(hd1(1))
  whd = where(wireobj.hd eq hd0,chd)

  if chd eq 0 and temp eq 'SAO 11802' then whd = where(wireobj.hd eq 888002,chd)

; ---
  if chd ne 1 then begin
    print,' *** ABORTED == Star in VIZIER output file not found in wireobj: ', hd0  
    stop
  endif
; ---

  res = replicate({hd:0L, v:99., hip:0L, par:-1., epar:-1., bv:9.}, 10)
  if eof(1) then goto,skip_ln

  hd_number = -1
  x = strsplit(temp,' ',/extract)
  if x(0) eq 'HD' then hd_number = x(1)
 
  temp_empty = '' & readf,1,temp_empty & cl = cl + 1 ; read the empty line
  if temp_empty ne '' then stop

; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
  while (strcompress(temp,/remove_all) ne '') and $
        (strmatch(temp,'*HD*') eq 0 or cnt eq 0) do begin
   readf,1,temp & cl = cl + 1

   if debug_read then print,' >>> Read line: ' + temp
   xx = strsplit(temp,' ',/extract) & nxx = n_elements(xx)
   if nxx ge 7 then stop ; problem with input format!
     
   if strcompress(temp,/remove_all) ne ''  and  nxx eq 6 then begin
    p = strsplit(temp,' ',/extract)
    res(cnt).hd  = hd_number ; long(p(0))
    res(cnt).hip = long(p(1))
    res(cnt).v  = float(p(2))
    res(cnt).bv = float(p(5))
    res(cnt).par = float(p(3))
    res(cnt).epar = float(p(4))
    cnt = cnt + 1
   endif else if debug_read then print,' %%% Terminate star: ' + temp

  endwhile
; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++

; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
if cnt eq 0 then begin
 print,' *** Counter "cnt" is zero. Something is wrong!!'
 stop
endif
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   res = res(0:cnt-1)

   if cnt eq 1 then wpar = 0
   if cnt ge 2 then begin
    distv  = abs(wireobj(whd).v - res.v)
    distbv = abs(wireobj(whd).bv - res.bv)
    w1 = where(distv eq min(distv),c1)
    w2 = where(distv eq min(distv),c2)
    w1 = w1(0) & w2 = w2(0)
    if w1 ne w2 then begin
      print,' %%% Could not pick HIP star from V, BV magn!!'
      stop
    endif
    wpar = w1 ; select hipparcos star!
   endif

    wireobj(whd).hip  = res(wpar).hip
    wireobj(whd).par  = res(wpar).par
    wireobj(whd).epar = res(wpar).epar

 endif

 skip_star:
 skip_ln:

endwhile
; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++
close,1


plot,wireobj.hbeta,wireobj.by,psym=1,$
 xr=[2.4,3],yr=[-0.5,1.5],xtit='H!I!4b!N!3',ytit='b-y',charsi=1.4

plot,wireobj.by,wireobj.c1,psym=1,$
 xr=[-.5,1.5],yr=[0,1],ytit='c!I1!N',xtit='b-y',charsi=1.4

plot,wireobj.by,wireobj.m1,psym=1,$
 xr=[-.5,1.5],yr=[-.5,1.5],ytit='m!I1!N',xtit='b-y',charsi=1.4

plot,wireobj.ra,wireobj.dec,psym=1,$
 xr=[0,360],xsty=1,yr=[-100,100],ysty=1,symsi=.5

plot,wireobj.bv,wireobj.vsini,psym=1,$
 xr=[-.5,2],xsty=1,yr=[0,300],ysty=1,symsi=.5,$
 xtit='B - V',ytit='!17v !3sin !17i!3'

plot_io,wireobj.v,wireobj.par,psym=1,$
 xr=[-2,8],xsty=1,yr=[.1,300],ysty=1,symsi=.5,$
 xtit='V',ytit='Parallax'

plot,wireobj.bv, wireobj.v,psym=1,yr=[1,-2],/nodata
plotsym,0 & w = where(wireobj.vsini gt 50,c)
oplot,wireobj(w).bv, wireobj(w).v,psym=8
plotsym,0,/fill & wl = where(wireobj.vsini le 50,cl)
oplot,wireobj(wl).bv, wireobj(wl).v,psym=8

ws = where(wireobj.nam ne '',cs)
wireobj = wireobj(ws)

; ==========================================================
; Parallax info for three stars:
; ==========================================================
w1 = where(wireobj.hd eq 2905,c1) & if c1 eq 1 then begin
 wireobj(w1).par = 0.79 & wireobj(w1).epar = 0.52
endif
w1 = where(wireobj.hd eq 223385,c1) & if c1 eq 1 then begin
 wireobj(w1).par = 0.20 & wireobj(w1).epar = 0.67
endif
w1 = where(wireobj.hd eq 888001,c1) & if c1 eq 1 then begin
 wireobj(w1).par = 25.96 & wireobj(w1).epar = 0.83
endif



; ==========================================================

outfile = basedir + 'wire/wire_essential/wire_sec_info.idl' ; default
; outfile = basedir + 'wire/wire_essential/wire_sec_info_NEW.idl' ; safety copy
save,filename=outfile,wireobj
print,' %%% Saved info file as: ' + outfile
print,' %%% ' + strcompress(cs) + ' objects 

print,''
print,' %%% To get Teffs run: .r wire_comp_teff_calibr'
print,' %%% To plot HR diagram: .r wire_hr_all'
print,''



END
