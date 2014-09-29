PRO wire_stetson, dat, wei, debug, silent=silent, $
                  astet=astet, bstet=bstet, pz=pz, xrr=xrr

; Calculate Stetson weights

default9, silent, 0
default9, pz, 3 ; symbols
default9, xrr, 2.0 ; x-range

if n_elements(debug) eq 0 then debug = 0

; Define weight array
np = n_elements(dat)
wei = fltarr(np)

; Mean level of data
   me = median(dat)
   noise = robust_sigma(dat)
   fivesigma = 5.0 * noise 
   threesigma = 3. * noise

; Stetson weights - default coefficients A and B:
default9, astet, 0.7
default9, bstet, 8.0

   fudge_weight = (1. + (abs(dat-me)/(astet*fivesigma))^bstet)^(-1.)
   fudge_weight = fudge_weight / total(fudge_weight)

; The final weights
   wei = fudge_weight

  w = where(abs(dat-me) gt threesigma,c)

  if silent eq 0 then $
   print,' %%% Number of points outside three sigma range: '+$
          strcompress(string(c),/remove_all) + ' (' + $
          strcompress(string((100.*c)/np,format='(F9.3)'),/remove_all) + '%)'

if debug eq 1 then begin
 plot,dat,wei/max(wei),psym=pz,xr=[-1,1]*fivesigma * xrr,$
   xtit='!4D !3mag',ytit='W!LSTETSON!N',tit='Stetson Weights',ysty=3,xsty=3
 plots,me+threesigma,!y.crange,line=2  
 plots,me-threesigma,!y.crange,line=2  
 plots,!x.crange,0.
endif

END
