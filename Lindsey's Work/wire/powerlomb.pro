pro powerlomb,y,x,oplt=oplt,title=title,xtitle=xtitle,ytitle=ytitle,$
  hifac=hifac, ofac=ofac, $
  spectrum=spectrum, wavenumbers=wavenumbers,$
  noplot=noplot

; Author: Hans Ludwig
; y = data
; x = time (optional./.)

;  Calculates a 1D power spectrum based on Lomb-Scargle estimate
;  Can handle non-equidistantly sampled data sets
;  Uses IDL routine lnp_test which in turn implements the routine
;  fasper of the Numerical Recipies
;
;  The resulting spectrum satisfies 
;  aver(a^2)=(wavenumbers(1)-wavenumbers(0))*total(spectrum).

default9, hifac, 1.0 ; max(frequency)= hifac x average(Nyquist freq.)
default9, ofac, 4.0  ; oversampling factor
default9, title, ''
default9, xtitle, 'Frequency / (2 Nyquist frequency)'
default9, ytitle, 'Power'
if n_params() lt 2 then x=findgen(n_elements(y)) ; no x given

a = lnp_test(x,y,/double, hifac=1.0, ofac=ofac, wk1=wavenumbers, wk2=spectrum)

; wk1 is a linear frequency scale
k0 = wavenumbers(1)-wavenumbers(0)
spectrum = spectrum/total(spectrum)*total(y^2)/(k0*float(n_elements(y)))

if keyword_set(noplot) then return

if n_elements(oplt) eq 0 then begin
  plot_oo,wavenumbers(1:*),spectrum(1:*),$      ; skip DC in log-log
    title=title,xtitle=xtitle,ytitle=ytitle
endif else begin
  oplot,wavenumbers(1:*),spectrum(1:*),$
    line=oplt
endelse

return
end
