; Combine info from SIMBAD / HIPPARCOS / TEFF from TEMPLOGG
; Updated gammador to include all 37 stars in Febr' 2006

mode = 'gammador'
; mode = 'corot_atype'

nmax = 750 ; max 750 objects
gam = replicate({nam:'', hd:0L, hdnam:'', hip:0L, $
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
                     teff:-1., logg:-1., feh: 9.9, $
                     lumn:-1., elumn:-1., mbol:99., embol:99., $
                     eteff:-1., elogg:-1., efeh:-1., $
                       teff_cas:-1., logg_cas:-1., feh_cas: 9.9, $    ; Cassini 1997 calibr.
                       eteff_cas:-1., elogg_cas:-1., efeh_cas:-1., $
                      teff_nap:-1., logg_nap:-1., feh_nap: 9.9, $    ; Napiwotzski 1998 calibr.
                      eteff_nap:-1., elogg_nap:-1., efeh_nap:-1., $
                       teff_rib:-1., logg_rib:-1., feh_rib: 9.9, $    ; Ribas 1997 calibr.
                       eteff_rib:-1., elogg_rib:-1., efeh_rib:-1., $
                      mass_templogg:0., radius_templogg:0., $
                     lcfile3:'', $
                     extby:-1., e_extby:-1., $
                     mv_templogg:-1., emv_templogg:-1., $
                     vmic:0., vmac:0., obs:'', $
                     bc: 99., ebc:-1., dist:-1., edist:-1.}, nmax)

; lumclass:
; V IV III II I
; 5  4  3  2  1

; Class:
; OBAFAKM
; 1234567

; ==========================================================
; Input given to Simbad. Made with program wire_simbad.pro
; ==========================================================
m4_get_basedir,basedir
case mode of
 'gammador':     base = basedir + '/VWA/MEGA/info/simbad_gamma_parameters2.txt'
 'corot_atype':  base = basedir + '/VWA/MEGA/info/simbad_corot_atype_parameters.txt' ;
endcase

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
gam(0:cnt-1).nam = hdnam


close,1


; ==========================================================



; ==========================================================
; Read simbad output
; ==========================================================

case mode of
 'gammador':    base = basedir + '/VWA/MEGA/info/vizier_gamma_hipparc2.txt' 
 'corot_atype': base = basedir + '/VWA/MEGA/info/vizier_corot_atype_hipparc.txt'
endcase


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
gam(0:cnt-1).hdnam = hdnam
for j=0,cnt-1 do begin
 g  = strsplit(gam(j).hdnam,' ',/extract)
 wg = where(strmatch(gam(j).hdnam,'*HD*') eq 1,cg)

 if cg eq 1 then begin
  x = strlen(gam(j).hdnam)
  gam(j).hd = strmid(gam(j).hdnam,2,x-2)
endif


; Is there a space in there?
 if cg ge 2 then gam(j).hd = long(g(1)) 


endfor

; ==========================================================
skip_hd:


; ==========================================================
; Read simbad output
; ==========================================================
; base = '~/VWA/MEGA/info/simbad_gamma_parameters.out'
;  '~/papers/gammador/simbad_corot_atype_parameters.out'
;  '~/papers/gammador/vizier_corot_atype_hipparc.out'

case mode of
 'gammador':    base = basedir + '/VWA/MEGA/info/simbad_gamma_parameters2.out'
 'corot_atype': base = basedir + '/papers/gammador/simbad_corot_atype_parameters.out'
endcase

openr,1,base
newobj = 1B
cnt = 0
nam = strarr(500)
temp = ''

while not eof(1) do begin

  sum = ''
  while newobj and not eof(1) do begin
     readf,1,temp 
     sum = sum + temp
     if (strcompress(temp,/remove_all) eq '' and $
        strmatch(sum,'*HD*')) or $
        (strcompress(temp,/remove_all) eq '' and $
        strmatch(sum,'*CCDM*')) then newobj=0B ; new object!
 endwhile ; ADDED CCDM ON 23RD FEB 2005

;   wmmm = where(strmatch(sum,'*71129*') eq 1, cmmm) 
;   if cmmm eq 1 then stop

   if strcompress(sum,/remove_all) eq '' then goto,skip_lst

   g = strsplit(sum,'^',/extract)
   g2 = strsplit(g(0),' ',/extract)
   hdname = g2(0) + ' ' + g2(1)

   x = strlen(hdname)
   ll = long(strmid(hdname,2,x-2))
   if ll le 9 then $
    hdname2 = 'HD00000' + strcompress(string(ll,format='(I9)'),/remove_all)
   if ll le 99 and ll ge 10 then $
    hdname2 = 'HD0000' + strcompress(string(ll,format='(I9)'),/remove_all)
   if ll le 999 and ll ge 100 then $
    hdname2 = 'HD000' + strcompress(string(ll,format='(I9)'),/remove_all)
   if ll le 9999 and ll ge 1000 then $
    hdname2 = 'HD00' + strcompress(string(ll,format='(I9)'),/remove_all)
   if ll le 99999 and ll ge 10000 then $
    hdname2 = 'HD0' + strcompress(string(ll,format='(I9)'),/remove_all)
   if ll le 999999 and ll ge 100000 then $
    hdname2 = 'HD' + strcompress(string(ll,format='(I9)'),/remove_all)


   w = where(gam.hdnam eq hdname2,c)
   if c ne 1 then begin
      print,' %%% Star not found in SIMBAD result: ' + hdname2
      print,' %%% Did you query SIMBAD with updated file: '+basedir+'/VWA/MEGA/info/simbad_gamma_parameters.txt ?'
      ; goto,fail_star
      stop
   endif

 
   ; Debug
   ; if gam(w).hd eq 101666 then print,g

 
   gam(w).spec1 = strcompress(g(1),/remove_all) ; simbad spec type
 
 ; The order of these if's is important!!!
   if strmatch(gam(w).spec1,'*I*') eq 1 then $
    gam(w).lumclass = 1
   if strmatch(gam(w).spec1,'*II*') eq 1 then $
    gam(w).lumclass = 2
   if strmatch(gam(w).spec1,'*III*') eq 1 then $
    gam(w).lumclass = 3
   if strmatch(gam(w).spec1,'*V*') eq 1 then $
    gam(w).lumclass = 4
   if strmatch(gam(w).spec1,'*IV*') eq 1 then $
    gam(w).lumclass = 5

   if strmatch(gam(w).spec1,'*O*') eq 1 then $
    gam(w).class = 1
   if strmatch(gam(w).spec1,'*B*') eq 1 then $
    gam(w).class = 2
   if strmatch(gam(w).spec1,'*A*') eq 1 then $
    gam(w).class = 3
   if strmatch(gam(w).spec1,'*F*') eq 1 then $
    gam(w).class = 4
   if strmatch(gam(w).spec1,'*G*') eq 1 then $
    gam(w).class = 5
   if strmatch(gam(w).spec1,'*K*') eq 1 then $
    gam(w).class = 6
   if strmatch(gam(w).spec1,'*M*') eq 1 then $
    gam(w).class = 7


   bv1 = strsplit(g(2),' ',/extract)
   bv2 = bv1(0)

   s1 = strmatch(bv1(1),'*V*')
   if s1 then bv2 = bv2 + ' ' + bv1(2) else $
              bv2 = bv2 + ' ' + bv1(1)

   bv = strsplit(bv2,' ',/extract)

   gam(w).v  = float(bv(1))
   gam(w).bv = float(bv(0)) - float(bv(1)) 

; H Beta ?
   if strcompress(g(3),/remove_all) ne '' then begin
 
       hb1 = strsplit(g(3),'|',/extract)
       v = strsplit(hb1(1),' ',/extract)
       gam(w).hbeta = float(v(0))
       if n_elements(v) eq 2 then $
       gam(w).ehbeta = float(v(1))
       gam(w).nhbeta = fix(hb1(2))

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
       gam(w).by = float(v(0))
       if n_elements(v) eq 2 then begin
        if strcompress(v(1)) ne '~' then $
         gam(w).eby = float(v(1)) else $
         gam(w).eby = 8.
       endif 

; c1 and m1 were swapped on the 23rd of Febr. 2005:
       v = strsplit(uvby(2),' ',/extract)
       gam(w).m1 = float(v(0))
       if n_elements(v) eq 2 then begin
        if strcompress(v(1)) ne '~' then $
         gam(w).em1 = float(v(1)) else $
         gam(w).em1 = 8.
       endif 

       v = strsplit(uvby(3),' ',/extract)
       gam(w).c1 = float(v(0))
       if n_elements(v) eq 2 then begin
        if strcompress(v(1)) ne '~' then $
         gam(w).ec1 = float(v(1)) else $
         gam(w).ec1 = 8.
      endif
     endif

; Format uvby
      if strcompress(uvby2,/remove_all) eq 'uvby' then begin

       v = strsplit(uvby(1),' ',/extract)
       gam(w).by = float(v(0))
       if n_elements(v) eq 1 then begin
        if strcompress(v(0)) ne '~' then $
         gam(w).eby = 99.
       endif 

; c1 and m1 were swapped on the 23rd of Febr. 2005:
       v = strsplit(uvby(2),' ',/extract)
       gam(w).m1 = float(v(0))
       if n_elements(v) eq 1 then begin
        if strcompress(v(0)) ne '~' then $
         gam(w).em1 = 99.
       endif 

       v = strsplit(uvby(3),' ',/extract)
       gam(w).c1 = float(v(0))
       if n_elements(v) eq 1 then begin
        if strcompress(v(0)) ne '~' then $
         gam(w).ec1 = 99.
      endif
     endif




  endif

     


; Rotational velocity, projected? vsini
   if strcompress(g(5),/remove_all) ne '' then begin
 
       rot1 = strsplit(g(5),'|',/extract)
       sr = strsplit(rot1(1),' ',/extract)
       gam(w).vsini = fix(sr(0))
       if n_elements(sr) ge 2 then begin
          s2 = strsplit(rot1(1),'(',/extract)
          s4 = strsplit(s2(1),')',/extract)
          tal = strcompress(s4(0),/remove_all)
          if tal ne '' and tal ne '~' then $
           gam(w).evsini = fix(s4(0))          
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

       gam(w).ra  = ten(ra1) * 15
       gam(w).dec = ten(dec1)

       gam(w).ra1  = s(0) + ' ' + s(1) + ' ' + s(2)
       gam(w).dec1 = s(3) + ' ' + s(4) + ' ' + s(5)
   endif


skip_lst:

   newobj = 1
   ; print,sum

endwhile
close,1

b = where(gam.nam ne '' and gam.class eq 0,cb)
if cb ge 1 then begin
 print,' %%% Possible bad stars ... no spec type? '
 print,nam,gam(b).hd
endif


; ==========================================================
; Import VIZIER information from HIPPARCOS
; ==========================================================
; base = '~/VWA/MEGA/info/vizier_gamma_hipparc.out'
;  '~/papers/gammador/simbad_corot_atype_parameters.out'
;  '~/papers/gammador/vizier_corot_atype_hipparc.out'

case mode of
 'gammador':    base = basedir + '/VWA/MEGA/info/vizier_gamma_hipparc2.out'
 'corot_atype': base = basedir + '/papers/gammador/vizier_corot_atype_hipparc.out'
endcase

openr,1,base
cnt = 0
temp = ''
init = 1B

while not eof(1) do begin
 readf,1,temp
 h = where(strmatch(temp,'*HD*',/fold) eq 1,ch)
 if ch eq 1 then begin
  x = strlen(temp) & temp = 'HD ' + strmid(temp,2,x-2)
 endif

 if init then begin
  if strmatch(temp,'HD*') eq 1 then init = 0B
 endif 
 if init eq 0 then begin

  if strmatch(temp,'*Available*') then goto,skip_ln
  if strmatch(temp,'*results with*') then goto,skip_ln
  if strmatch(temp,'*No object found around*') then goto,skip_star ; Par ax no avail!

  cnt = 0
  if strcompress(temp,/remove_all) eq '' then goto,skip_ln

;  hd1 = strsplit(temp,' ',/extract)
;  hd0 = long(hd1(1))

  x = strlen(temp) & hd1 = strmid(temp,2,x-2)
  hd0 = long(hd1)
  whd = where(gam.hd eq hd0,chd)

  if chd ne 1 then begin
    print,' %%% Star in VIZIER output file not found in gamma: ', hd0  
    stop
endif

  res = replicate({hd:0L, v:99., hip:0L, par:-1., epar:-1., bv:9.}, 10)
  if not eof(1) then readf,1,temp else goto,skip_ln
  temp = 'Read HIP stars ... '

  while strcompress(temp,/remove_all) ne '' do begin
   readf,1,temp
   if strcompress(temp,/remove_all) ne '' then begin
    p = strsplit(temp,' ',/extract)
    res(cnt).hd  = long(p(0))
    res(cnt).hip = long(p(1))
    res(cnt).v  = float(p(2))
    res(cnt).bv = float(p(5))
    res(cnt).par = float(p(3))
    res(cnt).epar = float(p(4))
    cnt = cnt + 1
   endif
  endwhile

   res = res(0:cnt-1)

   if cnt eq 1 then wpar = 0
   if cnt ge 2 then begin
    distv  = abs(gam(whd).v - res.v)
    distbv = abs(gam(whd).bv - res.bv)
    w1 = where(distv eq min(distv),c1)
    w2 = where(distv eq min(distv),c2)
    w1 = w1(0) & w2 = w2(0)
    if w1 ne w2 then begin
      print,' %%% Could not pick HIP star from V, BV magn!!'
      stop
    endif
    wpar = w1 ; select hipparcos star!
   endif


    gam(whd).hip  = res(wpar).hip
    gam(whd).par  = res(wpar).par
    gam(whd).epar = res(wpar).epar

   

 endif

 skip_star:
 skip_ln:

endwhile
close,1


plot,gam.hbeta,gam.by,psym=1,$
 xr=[2.4,3],yr=[-0.5,1.5],xtit='H!I!4b!N!3',ytit='b-y',charsi=1.4

plot,gam.by,gam.c1,psym=1,$
 xr=[-.5,1.5],yr=[0,1],ytit='c!I1!N',xtit='b-y',charsi=1.4

plot,gam.by,gam.m1,psym=1,$
 xr=[-.5,1.5],yr=[-.5,1.5],ytit='m!I1!N',xtit='b-y',charsi=1.4

plot,gam.ra,gam.dec,psym=1,$
 xr=[0,360],xsty=1,yr=[-100,100],ysty=1,symsi=.5

plot,gam.bv,gam.vsini,psym=1,$
 xr=[-.5,2],xsty=1,yr=[0,300],ysty=1,symsi=.5,$
 xtit='B - V',ytit='!17v !3sin !17i!3'

plot_io,gam.v,gam.par,psym=1,$
 xr=[-2,8],xsty=1,yr=[.1,300],ysty=1,symsi=.5,$
 xtit='V',ytit='Parallax'

plot,gam.bv, gam.v,psym=1,yr=[8,-2],/nodata
plotsym,0 & w = where(gam.vsini gt 50,c)
oplot,gam(w).bv, gam(w).v,psym=8
plotsym,0,/fill & wl = where(gam.vsini le 50,cl)
if cl ge 2 then oplot,gam(wl).bv, gam(wl).v,psym=8

ws = where(gam.nam ne '',cs)
gam = gam(ws)

; ==========================================================
; Parallax info for three stars:
; ==========================================================
w1 = where(gam.hd eq 2905,c1) & if c1 eq 1 then begin
 gam(w1).par = 0.79 & gam(w1).epar = 0.52
endif
w1 = where(gam.hd eq 223385,c1) & if c1 eq 1 then begin
 gam(w1).par = 0.20 & gam(w1).epar = 0.67
endif
w1 = where(gam.hd eq 888001,c1) & if c1 eq 1 then begin
 gam(w1).par = 25.96 & gam(w1).epar = 0.83
endif



; ==========================================================


case mode of
 'gammador':    begin
     outfile = basedir + '/VWA/MEGA/info/gammador_info.idl'
     save,filename=outfile,gam
  endcase
 'corot_atype': begin
     outfile = basedir + '/VWA/MEGA/info/corot_atype_info.idl'
     cor = gam
     save,filename=outfile,cor   
 endcase
endcase

print,' %%% Saved info file as: ' + outfile
print,' %%% ' + strcompress(cs) + ' objects 




END
