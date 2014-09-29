; Make this test:
; If the jitter is +-0.01 pixel,
; will this cause a correlation of measured flux and measured FWHM?

; TBD:
; Need to fit gaussian to find x,y center (already done)
; and then place aperture - circle at the centre - and sum pixels!
; Try also changing - systematically - FWHM ?

!P.charthick=1
!P.charsize=2

 ; add pure Poisson noise?
add_poisson = 0B

; Nominal x,y center of central pixel:
xcentre = 0.270 & ycentre = 0.175 ; Delta Ser Aug 2006

xcentre = 0.50 & ycentre = 0.5 ; Delta Ser Aug 2006

; Systematic drift in x,y ?
driftx =  0.0
drifty = -0.0

; Random jitter on:
randomx = 0.01
randomy = 0.01 ; 0.02

ndo = 1200 ; number of simulations


wire_get_apertures, nap, apertures, apuse
ap_choose = 3

; Summing which pixels:
 s1 = 1 & s2 = 6 ; 5x5 = 25 pixels  C
 s1 = 2 & s2 = 5 ; 4x4 = 16 pixels B ; normal for a bright star
; s1 = 2 & s2 = 4 ; 3x3 =  9 pixels
; s1 = 3 & s2 = 4 ; 2x2 =  4 pixels A

; When summing N pixels: drift of 0.05 pixels cause changes in flux
; those pixels of :

; [ x=0.25, y= 0.5]  [x=0.0,y=0.5]
; N   Flux-change   Flux-change 
; ================= ================
; 25   0.02 %       0.05%     
; 16   0.57 %       0.3%
;  9   3.2% %       2.7%
;  4   4.5% %       2.8%

; There is a clear correlation of flux & x-position (when drifting)
; 0.3% = ~3 mmag


fwhm   =  1.8 ; fwhm in pixels
vmag   =  2.0 ; magnitude in Johnson V 
bv     =  0.0 ; (I assume B-V = 0)

gain = 15. ; e/adu

wire_counts, bv + vmag, vmag, counts

nelec = gain * counts
sigma = fwhm / 2.35

; scale gaussian with complete volume --> vol. of gauss will be Nelectrons
vol_gauss = 2. * !PI * sigma^2.
amp_gauss = nelec / vol_gauss 

; This is the array:
nx = 8 & ny = 8
g = fltarr(nx,ny)

xcen2 = nx * 0.5 + xcentre
ycen2 = ny * 0.5 + ycentre


gfit = replicate({x:0., y:0., fwhm:0., flux:0., npix:0B},ndo)


for cnt=0L,ndo-1 do begin

xcen = xcen2 + (driftx*cnt)/(ndo-1.) + randomx * randomn(seed)
ycen = ycen2 + (drifty*cnt)/(ndo-1.) + randomy * randomn(seed)

for i=0,nx-1 do $
 for j=0,ny-1 do $
  g(i,j) = amp_gauss * exp(-( (xcen-i)^2. + (ycen-j)^2.) / (2. * sigma^2.) )

; Add poisonian noise:
if add_poisson then begin
 g2 = g  &  for i=0,nx-1 do g2(i,*) = poidev(g(i,*))
 g = g2
endif

; Analytical integration: volume is: 2. * !PI * sigma^2.
; print,total(g) / ( 2. * !PI * sigma^2. )
; print,total(g) / nelec ; Number of electrons in gaussian

x = gauss2dfit(g,term) ; 2,3: sigma   4,5: x,y

gfit(cnt).x = term(4)
gfit(cnt).y = term(5)
gfit(cnt).fwhm = avg([term(2),term(3)]) * 2.35

r_ap = apertures * gfit(cnt).fwhm ; apertures scaled with FWHM

dist = g ; result array == dist
dist_circle, dist, nx, term(4), term(5)
wap = where(dist lt r_ap(ap_choose),cap)

gfit(cnt).flux = total(g(wap)) ; no background to subtract?
gfit(cnt).npix = cap

; gfit(cnt).flux = total(g(s1:s2,s1:s2))

endfor

sz = .2

me = median(gfit.flux/nelec)
dm = stdev(gfit.flux/nelec) * 5.

xtt = 'Data point number'

!P.multi=[0,2,5]
 plot,gfit.x,yr=xcen2+[-1,1]*(.02+abs(driftx)+randomx),ytit='X value',psym=1,symsi=sz,xtit=xtt
 plot,gfit.y,yr=ycen2+[-1,1]*(.02+abs(drifty)+randomy),ytit='Y value',psym=1,symsi=sz,xtit=xtt
 plot,gfit.fwhm,yr=fwhm+[-1,1]*.005,ytit='FWHM',psym=1,symsi=sz,xtit=xtt
 plot,gfit.flux/nelec,ytit='Rel. Flux',yr=me+[-1,1]*dm,psym=1,symsi=sz,xtit=xtt
 plot,gfit.fwhm,gfit.flux/nelec,ytit='Rel. Flux',yr=me+[-1,1]*dm,xr=fwhm+[-1,1]*.005,xtit='FWHM',psym=1,symsi=sz
 plot,gfit.x,gfit.y,xtit='X value',ytit='Y value',xr=xcen2+[-1,1]*(.02+abs(driftx)+randomx),yr=ycen2+[-1,1]*(.02+abs(drifty)+randomy),psym=1,symsi=sz

 plot,gfit.x,gfit.flux/nelec,xtit='X value',ytit='Flux',xr=xcen2+[-1,1]*(.02+abs(driftx)+randomx),yr=me+[-1,1]*dm,psym=1,symsi=sz
 plot,gfit.y,gfit.flux/nelec,xtit='Y value',ytit='Flux',xr=ycen2+[-1,1]*(.02+abs(drifty)+randomy),yr=me+[-1,1]*dm,psym=1,symsi=sz

 plot,gfit.x,gfit.fwhm,xtit='X value',ytit='FWHM',xr=xcen2+[-1,1]*(.02+abs(driftx)+randomx),yr=fwhm+[-1,1]*0.005,psym=1,symsi=sz
 plot,gfit.y,gfit.fwhm,xtit='Y value',ytit='FWHM',xr=ycen2+[-1,1]*(.02+abs(drifty)+randomy),yr=fwhm+[-1,1]*0.005,psym=1,symsi=sz

!P.multi=0

; id = g & id(*,*) = 0. & id(wap) = 1.
; surface,g
; surface,g*id

;!P.multi=0 ; [0,1,2]
; contour,g/max(g),levels=[.05,.1,.2,.3]
;
; contour,g*id/max(g*id),levels=[.05,.1,.2,.3],/noerase,col=col.red,c_line=2
;
; surface,g/max(g)
; surface,g/max(g) - g*id/max(g*id)
;
; contour,g/max(g) - g*id/max(g*id)
;
; contour,g*id/max(g*id),levels=[.05,.1,.2,.3],/noerase,col=col.red,c_line=2

END

