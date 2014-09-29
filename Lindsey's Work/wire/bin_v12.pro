; Se paa V12 i NGC 188, combined spectra:
; compute synth spectra at each of the 17th epochs

compute_synth = 0B

; Parameters for each star:
teff1 = 5875. & logg1 = 4.18 & feh1  = -0.1
teff2 = 5825. & logg2 = 4.18 & feh2  = -0.1

if compute_synth then begin
 vwa_synth_template, teff1, logg1, feh1, 5000.,5200., $
 '~/binary/jvc/ngc188/synth/',name='primary',$
 vsini=10.0,res=13000.

 vwa_synth_template, teff2, logg2, feh2, 5000.,5200., $
 '~/binary/jvc/ngc188/synth/',name='secondary',$
 vsini=10.0,res=13000.
endif


restore,"~/binary/jvc/ngc188/synth/primary.idl"   & pri = s
restore,"~/binary/jvc/ngc188/synth/secondary.idl" & sec = s

readcol,'/home/bruntt/binary/jvc/ngc188/26apr05/spect/v12_ntemp_cor.out',$
 hjd,epoch,phase, vobs, vcal, oc, wt, $
  format='D, I, A, F,F, F,F',/silent
w = where(hjd gt 50000.,c)
hjd  = hjd(w)
vcal = vcal(w)

hjd1 = hjd(0:16) & vr1 = vcal(0:16)
hjd2 = hjd(17:*) & vr2 = vcal(17:*)

wildcard = '/home/bruntt/binary/jvc/ngc188/26apr05/spect/hcdpv*.asc'
spawnrob,'ls -1 ' + wildcard, ll
nl = n_elements(ll)

w1 = lonarr(n_elements(hjd1))
w2 = lonarr(n_elements(hjd2))

tlim  = 0.001

for i=0,nl-1 do begin
 x = strsplit(ll(i),'_',/extract)
 x2 = strsplit(x(1),'.',/extract)
 dec = double(x2(1))
 dec2 = dec / 10000.
 hjd_search = long(x2(0)) + dec2

 ss = where(abs(hjd_search - hjd1) lt tlim,cc)
 if cc eq 1 then w1(ss) = i

 ss2 = where(abs(hjd_search - hjd2) lt tlim,cc2)
 if cc2 eq 1 then w2(ss) = i
endfor

; Create uniform grid of wavelengths
dwl = 0.01
wl1 = 5000.
wl2 = 5200.
nwl = ceil((wl2-wl1) / dwl)
wl = wl1 + findgen(nwl) * dwl

nspec = n_elements(w1)

ratio = [.5 , .9, 1.]
nratio = n_elements(ratio)

f = replicate( {spec:fltarr(nratio,nwl), spec1:fltarr(nwl), spec2:fltarr(nwl), obs:fltarr(nwl), $
                mag1:0D, mag2:0D, $
                min1:fltarr(nratio), min2:fltarr(nratio), $
                min1s:fltarr(nratio), min2s:fltarr(nratio)}, nspec)

cspeed = double(2.997924580e5)

mag_wl = 5183.64 ; aprox WL og magnisium line

; ======================================================================
; For each flux ratio and each spectrum
; ======================================================================
for sp=0,nspec-1 do begin
 readcol, ll(sp), wlobs, flobs, format='F,F',/silent

; ======================================================================
 for rr=0,nratio-1 do begin

; Shift the spectra to the given radial velocity
 wuse1 = w1(sp)     &  wuse2 = w2(sp)
 vrad1 = -1.0 * vr1(wuse1) & vrad2 =  -1.0 * vr2(wuse2)

 newlambda1 = pri.wl / (( 1. + (vrad1 / cspeed )))
 newlambda2 = sec.wl / (( 1. + (vrad2 / cspeed )))

; Predicted location of magnesium line center:
 f(sp).mag1 = mag_wl / (( 1. + (vrad1 / cspeed )))
 f(sp).mag2 = mag_wl / (( 1. + (vrad2 / cspeed )))
 
 spec1 = interpol(pri.fl, newlambda1, wl)
 spec2 = interpol(sec.fl, newlambda2, wl)

; Assuming that the same amount of light comes out at these wavelengths:
 frac1 = 1. / (1. + ratio(rr)) 
 frac2 = ratio(rr) / (1 + ratio(rr))  
; JVC finder de samme relationer! e.g. for ratio=0.5, frac1=0.666, frac2=0.333

 spec = spec1 * frac1 + spec2 * frac2

 f(sp).spec(rr,*) = spec
 f(sp).spec1 = spec1
 f(sp).spec2 = spec2
 f(sp).obs = interpol(flobs, wlobs, wl)

; Determine depth of mag. line
 wllim = 0.20

 x1  = where(abs(wlobs-f(sp).mag1) lt wllim,c1)
 x2  = where(abs(wlobs-f(sp).mag2) lt wllim,c2)
 x1s = where(abs(wl-f(sp).mag1) lt wllim,c1)
 x2s = where(abs(wl-f(sp).mag2) lt wllim,c2)

 wlshift = abs(f(sp).mag1 - f(sp).mag2)
 if wlshift gt (wllim * 3.) then begin

  wx1 = where(flobs(x1) eq min(flobs(x1)),cx1)
  f(sp).min1(rr) = flobs(x1(wx1))
  wx1s = where(f(sp).spec(rr,x1s) eq min(f(sp).spec(rr,x1s)),cx1s)
  f(sp).min1s(rr) = f(sp).spec(rr,x1s(wx1s))

  wx2 = where(flobs(x2) eq min(flobs(x2)),cx2)
  f(sp).min2(rr) = flobs(x2(wx2))
  wx2s = where(f(sp).spec(rr,x2s) eq min(f(sp).spec(rr,x2s)),cx2s)
  f(sp).min2s(rr) = f(sp).spec(rr,x2s(wx2s))

 endif


; plot,wlobs,flobs,xr=[5150,5200],yr=[.4,1.05]
; oplot,wl,spec1,col=col.red
; oplot,wl,spec2,col=col.sky
; oplot,wl,spec,col=col.green
; hitme, s9

 
endfor ; next flux ratio
endfor ; next spectrum

xer=2 & yer=3 & n6791pos,xer,yer,0.07,0,0,pos

plot,[0,1],xsty=6,ysty=6,/nodata ; erase window
xclear = replicate([' '], 8)

; Which ratios to plot:
r1 = 1 & off1 = -0.1 & off1a = -0.12
r2 = 2 & off2 =  0.1 & off2a =  0.12

cnt = 0L

for i=0,16 do begin
 xclear = replicate([' '], 8) & if (i+1) mod yer eq 0 then xclear = ''

 plot,wl,f(i).obs,xr=[5170,5200],yr=[.4,1.05],position=pos(*,i mod (xer*yer)),/noerase, $
   xtickname = xclear, ytickname = xclear

 oplot,wl,f(i).spec(r1,*),col=col.sky
 oplot,wl,f(i).spec(r2,*),col=col.green

; Primary star: at the WL of Magnesium: observed and synth line depths:
 oplot,f(i).mag1*[1.,1]+off1, [f(i).min1(r1),f(i).min1s(r1)],col=col.red,thick=1
 oplot,f(i).mag1*[1.,1]+off1a,[f(i).min1(r2),f(i).min1s(r2)],col=col.red,thick=1,line=2

; Secondary star: at the WL of Magnesium: observed and synth line depths:
 oplot,f(i).mag2*[1.,1]+off2, [f(i).min2(r1),f(i).min2s(r1)],col=col.yellow,thick=2
 oplot,f(i).mag2*[1.,1]+off2a,[f(i).min2(r2),f(i).min2s(r2)],col=col.yellow,thick=2,line=2


 cnt = cnt + 1
 cnt = cnt mod (xer * yer)

 if cnt eq 0 then hitme,s9
 if cnt eq 0 then plot,[0,1],xsty=6,ysty=6,/nodata ; erase window

endfor

 

END
