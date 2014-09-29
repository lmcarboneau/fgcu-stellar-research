PRO wire_artif_lc, n, wire, $
 xvar=xvar, yvar=yvar, $
 fwhmvar=fwhmvar, fwhmavg=fwhmavg, nstar=nstar, $
 inc_fwhm=inc_fwhm, inc_xyvar=inc_xyvar

; Example:
; wire_artif_lc, 1000, wire

; x,y position varirer med +- 0.1 pix
; FWHM er fast
; Flux er som white noise i hver pixel:

wire = replicate( {x:fltarr(5), y:fltarr(5), hjd:dblarr(5), stamp:strarr(5), $
                   d:fltarr(5,8,8)}, n)

; Specifics for light curve of WIRE:
duty_cycle = 20. ; duty cycle in percent
orbit_cnt = long(ceil( (duty_cycle/100.) * 2 / (174.19 * 1e-6)))
t0 = 53211D

; Determine uncertainty of x,y position and FWHM:
default9, xvar, 0.01
default9, yvar, 0.01
default9, fwhmvar, 0.0
default9, fwhmavg, 1.8

default9, xcen, 3.7
default9, ycen, 3.6

default9, nstar, 1

default9, inc_fwhm, 0
default9, inc_xyvar, 0

gain = 15.
wn = 600. ; white noise in ppm pr. data point (remember merge factor is 31)
fl   = (1./(wn*1e-6)^2.) / gain ; typical flux in ADU
ampl = fl / 6.2 ; amplitude of double gaussian

backg = 30. ; typical background for *polaris* observsations

cnt = 0L
orbit_cnter = 0L

del2 = 0.2 ; flux scale of secondary gaussian

; print, 1.8 * sqrt(10000./!PI) * sqrt(31.) ; white noise pr. data point

ccd = fltarr(8,8)


for star=0,nstar-1 do begin

; Increase uncertainty in x,y position
if inc_xyvar then begin
 xvar = xvar + 0.01 * (star-1.)
 yvar = yvar + 0.01 * (star-1.)
endif

; Increase uncertainty in FWHM
if inc_fwhm then begin
 fwhmvar = fwhmvar + 0.01 * (star-1)
endif

wire(*).x(star) = 260. + 30. * (star)
wire(*).y(star) = 262. + 20. * (star)

for i=0L,n-1 do begin


xc = xcen + randomn(seed) * xvar
yc = ycen + randomn(seed) * yvar
fwhm = fwhmavg + randomn(seed) * fwhmvar
scale  = 2. * (0.8*fwhm/2.35)^2. ; gaussian factor
scale2 = 2. * (1.6*fwhm/2.35)^2. ; gaussian factor2

for x=0,7 do $
 for y=0,7 do $
  ccd(x,y) = (       exp( -( (x-xc)^2. + (y-yc)^2.) / scale ) + $
              del2 * exp( -( (x-xc)^2. + (y-yc)^2.) / scale2 )) * ampl


;;; ccd = fl * ccd / total(ccd)

ccd = ccd + backg 

; Add noise from photon statistics:
noise = ( sqrt(gain * ccd) * randomn(seed,8,8) ) / gain

ccd2 = ccd + noise

; plot,ccd(3,*),line=2
; oplot,wire(1000).d(0,3,*)/max(wire(1000).d(0,3,*),col=col.sky

wire(i).d(star,*,*) = ccd2

if (cnt mod orbit_cnt) eq 0 then begin
 add_orbit_all = ((100. - duty_cycle)/100.) /(174.19 * 1e-6)
 orbit_cnter = orbit_cnter + 1
 wire(i).hjd(star) = t0 + (cnt * 0.5 + add_orbit_all*float(orbit_cnter)) / 86400D
endif else begin
 wire(i).hjd(star) = t0 + (cnt * 0.5 + add_orbit_all*float(orbit_cnter)) / 86400D
endelse


wire(i).stamp(star) = 'Arty: ' + strcompress(wire(i).hjd(star),/remove_all)

cnt = cnt + 1

endfor ; next data point
endfor ; next slot/star

print,orbit_cnt, orbit_cnter

END
