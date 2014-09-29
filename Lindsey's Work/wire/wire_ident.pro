; Identify a WIRE field

; ===================================================================================
input_dir = '/ai39/bruntt/wire/altair'
; ===================================================================================

dops = 1

; ===================================================================================
b = strsplit(input_dir,'/',/extract) & nb = n_elements(b)
nam = b(nb-1)
; ===================================================================================

; ===================================================================================
out = input_dir + '/' + nam + '_identify.idl'
outps = input_dir + '/' + nam + '_identify.ps'
; ===================================================================================

if dops eq 1 then begin
 a4,x=18,y=17.2,name=outps
endif

; ===================================================================================
spawn,'ls -1 '+out,a
if a(0) ne '' then begin
 restore,out
 print,' %%% Restored old wire ident file: '+out
endif
if n_elements(usno) ne 0 then goto,here 
; ---> don't recalculate (USNO/GSC queries take quite a while to reach
; (about 30-60 minutes in total).
; ===================================================================================

; ===================================================================================
if n_elements(wire2) eq 0 then begin
 spawn,'ls -1 '+input_dir +'/*.idl1',a
 restore,a(0)
endif
; ===================================================================================


col=getcolor(/load)

;querygsc.pro
;./pro/querysimbad.pro
;./pro/queryusno.pro
;./pro/querydss.pro
;./pro/querygsc.pro


; Get RA and DEC from SIMBAD:
querysimbad,'Altair',ra,dec

d = 400.  ; Approx. size of WIRE CCD in arcminutes (800x800 pixels)
position = dblarr(2) & position(0) = ra & position(1) = dec

; Guide Star Catalog:
magran2 = [-5,10.5] ; get stars in this magnitude range
gsc = querygsc_large_field(position, 350., Magrange=magran2)

; US Naval Observatory:
magran = [-5,9] ; get stars in this magnitude range
usno = QueryUSNO_large_field(position, 350., Magrange=magran )


; Combine the two star catalogues
comb = wire_comb_usno_gsc(usno, gsc)

; QueryDSS, position, image_b, header_b, survey = '2b'
; QueryDSS, position, image_r, header_r, survey = '2r'

; a = median(image_b)
; showim,image_b,a*0.95,a*1.05

; ===================================================================================
here:
; ===================================================================================

; ===================================================================================
if dops ne 1 then begin
 window,3,xsize=550,ysize=550,title='WIRE Field Identification for '+nam
 wset,3
endif
; ===================================================================================

; TBD: WIRE count rates
;  cr = (534400 + 246230*(nbv)+ 976300*(nbv^2)) * 0.5 * (10.^(-0.4*vv)) ; in 0.5 seconds

wire_read_simbad,'/ai39/bruntt/wire/altair/altair_neighbours_simbad.txt', sim
mag = -2.5 * alog10(sim.wr) + 25.0
symmag,mag,0.5,1.5,pz,con 

; ===================================================================================
prad = 4.5

wg = where(sim.wr gt 500.,nn)
plot,sim.ra,sim.dec,psym=1,symsi=.1,/nodata,xsty=1,ysty=1,$
 xr=[-1,1]*prad + ra,yr=[-1,1]*prad + dec,$
 xtit='RA [deg]',ytit='DEC [deg]',tit='WIRE Field'
plotsym,0
   for i=0,nn-1 do $
     if abs(sim(wg(i)).ra  - ra)  lt prad*.95 and $
        abs(sim(wg(i)).dec - dec) lt prad*.95 then $
     plots,sim(wg(i)).ra,sim(wg(i)).dec,psym=8,symsi=pz(wg(i))
; ===================================================================================

; w = where(comb.wr gt 10,nn)
;plot,comb.ra,comb.dec,psym=1,symsi=.1,/nodata,xsty=1,ysty=1,$
; xr=[-1,1]*prad + ra,yr=[-1,1]*prad + dec
;plotsym,0, /fill
;   w = where(comb.wr gt 10.,n) & plotsym,0 ; ,/fill
;   for i=0,nn-1 do $
;     if abs(comb(w(i)).ra  - ra)  lt prad and $
;        abs(comb(w(i)).dec - dec) lt prad then $
;     plots,comb(w(i)).ra,comb(w(i)).dec,psym=8,symsi=comb(w(i)).pz
; ===================================================================================
;wc = where(comb.wr gt 10. and comb.wr lt 1e9)
;mag = -2.5 * alog10(comb(wc).wr) + 25.0
;symmag,mag,0.5,1.5,pz,con 


; ===================================================================================

nstar = n_elements(xm)
xm2 = ( xm - xm(0) ) / 60. + ra 
ym2 = ( ym - ym(0) ) / 60. + dec
mm = -2.5*alog10(fl) + 25.0

wgx = where(comb.id eq 'Altair',cg)
dx = 0. ; xm2(0) - comb(wgx).ra
dy = 0. ; ym2(0) - comb(wgx).dec
xm2 = xm2 - dx
ym2 = ym2 - dy

; get same symbol scale as the catalog stars!
sz = ( (con.r1 - con.r2) / (con.mx-con.mi) ) * (con.mx - mm) + con.r2

; ===================================================================================
; Plot position of WIRE targets!
; ===================================================================================

colx = col.yellow
if dops eq 1 then colx=0

for star=0,nstar-1 do begin
 rmax = sqrt( (xm2(0)-xm2(star))^2. + (ym2(0)-ym2(star))^2. )
 rmin = rmax
 TVELLIPSE, rmax, rmin, ra-dx, dec-dy, /DATA,color=colx,linestyle=2
endfor

fac = !PI/180
ang = 99.5  
inc_ang = 10.
key = ''

flipx = -1.0
flipy =  1.0

 xm2a = (xm2 - xm2(0)) * (flipx) + xm2(0)
 ym2a = (ym2 - ym2(0)) * (flipy) + ym2(0)

 xm3 =   cos(ang*fac) * (xm2a-xm2a(0)) - sin(ang*fac) * (ym2a-ym2a(0)) + xm2a(0)
 ym3 =   sin(ang*fac) * (xm2a-xm2a(0)) + cos(ang*fac) * (ym2a-ym2a(0)) + ym2a(0)


plotsym,0,/fill

if dops eq 1 then $
 for i=0,nstar-1 do $
  plots,xm3(i),ym3(i),psym=8,symsize=sz(i)*1.1,col=colx

if dops eq 1 then goto,skipp

device,set_Graphics_function = 6 ; temporary plot

while key ne 'x' do begin

; Plot new points
for i=0,nstar-1 do $
 plots,xm3(i),ym3(i),psym=8,symsize=sz(i)*1.1,col=colx

key = get_kbrd(1)

if key eq 'a' then ang = ang + inc_ang
if key eq 's' then ang = ang - inc_ang
if key eq 'A' then ang = ang + inc_ang * 0.15
if key eq 'S' then ang = ang - inc_ang * 0.15

if key eq 'a' or key eq 's' or key eq 'A' or key eq 'S' then begin
 if ang lt   0. then ang = ang + 360.
 if ang gt 360. then ang = ang - 360.

; remove old points
 for i=0,nstar-1 do $
  plots,xm3(i),ym3(i),psym=8,symsize=sz(i)*1.1,col=colx

 xm2a = (xm2 - xm2(0)) * (flipx) + xm2(0)
 ym2a = (ym2 - ym2(0)) * (flipy) + ym2(0)

 xm3 =   cos(ang*fac) * (xm2a-xm2a(0)) - sin(ang*fac) * (ym2a-ym2a(0)) + xm2a(0)
 ym3 =   sin(ang*fac) * (xm2a-xm2a(0)) + cos(ang*fac) * (ym2a-ym2a(0)) + ym2a(0)
endif

endwhile

device,set_Graphics_function = 3 ; ordinary plot

skipp:

for k=0,nstar-1 do begin
 dist = sqrt( (xm3(k)-sim(wg).ra)^2. + (ym3(k)-sim(wg).dec)^2.)
 wk = where(dist lt 0.5 and dist eq min(dist),ck)
 if ck eq 1 then begin
   xyouts,sim(wg(wk)).ra,sim(wg(wk)).dec+0.2,$
          '!18' + sim(wg(wk)).nam + '!3',$
          alignment=0.5,charsi=1.2
   xyouts,sim(wg(wk)).ra,sim(wg(wk)).dec-0.4,$
          '!18' + sim(wg(wk)).type + '!3',$
          alignment=0.5,charsi=1.2
 
   print,k, sim(wg(wk)).nam,  sim(wg(wk)).type, format='(I3,A20,X,A5)'
 endif
endfor

; ===================================================================================

save,filename=out,usno,gsc,comb,xm,ym,fl,xm2,ym2,nstar,ra,dec,position,nn,nam,sz

print,' %%% Saved restore file: '+out

if dops eq 1 then begin
 device,/close
 set_plot,'x'
 print,' %%%  $gv '+outps + ' &'
 print,' %%%  $lpr -Phpnew '+outps + ' '
endif

END

; =============
; Cut material:
; =============

; symmag,usno.r_mag,0.5,1.5,pz,con ; get symbol sizes
; nn = n_elements(usno)
; for i=0,nn-1 do plots,usno(i).ra,usno(i).dec,psym=8,symsi=pz(i)
; plots,ra,dec,psym=8,col=col.red,symsi=2

; plotsym,0
; wstar = where(gsc.obj_type eq 0,cstar) ; plot only stars!
; symmag,gsc(wstar).v_mag,0.5,1.5,pz_gsc ; get symbol sizes
; for i=0,nn_gsc-1 do plots,gsc(wstar(i)).ra,gsc(wstar(i)).dec,$
;  psym=8,symsi=pz_gsc(i),col=col.green
