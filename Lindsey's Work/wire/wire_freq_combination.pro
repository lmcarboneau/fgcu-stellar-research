PRO wire_freq_combination, file, fcomb, $
      flim=flim, random=random,nc=nc,nran=nran,$
      allfc=allfc, debug=debug,cnt=cnt,$
      fileformat=fileformat

; Set keyword random to make a list of
; random frequencies in the approx. same
; WL interval
default9, random, 0B
default9, nran, -1

ff = findfile(file,Count=cntr)
if cntr ne 1 then begin
 print,' %%% File not found: ' + file
 return
endif


default9, flim, 0.015
default9, purge, 1B ; remove double/triple entries, 
                    ; eg A = B+C, B=A+C is the same combination!
default9, debug, 0B ; debug print out stuff?
default9, fileformat, 'period04' ; format of file

if fileformat eq 'period04' then $
 readcol,file,aa,f,a,p,format='a,d,d,d',/silent
if fileformat eq 'fourcol' then $
 readcol,file,f,a,p,snr,format='d,f,f,f',/silent

nf = n_elements(f) & if nf le 1 then stop

default9, nc, 2
       ; nc: integer specifying combination "order", 
       ;     For nc=2, you get +-1, +-2 times each Freq.

; ----------------------------------------
; Use random frequencies instead?
; ----------------------------------------
if random then begin
 if nran le 1 then nran = n_elements(f)
 fmax = max(f) & fmin = min(f) & fran = fmax - fmin
 f = fmin + randomu(seed,nran) * fran
 nf = n_elements(f)
 print,' %%% I placed ' + string(nf,format='(I3)') + $
       ' random freq. in the range: ' + $
       string(fmin,format='(F9.1)') + ' to ' + $
       string(fmax,format='(F9.1)') + ' c/day'
 if debug then begin
   print,' %%% The random frequencies are: '
   for i=0,nf-1 do print,f(i),format='(F9.2)'
 endif

endif
; ----------------------------------------

cnt = 0
maxcomb = 1000
fcomb = fltarr(7,maxcomb)

for i=0,nf-1 do begin ; check all frequencies

freq  = f(i) ; the freq. you want to check
wf = where(f ne freq,cf)
ff = f(wf)   ; frequencies to compare to


for j=0,cf-1 do begin

 f2 = ff(j)
 id = wf(j)

 for l=-nc,nc do begin

  if l eq 0 then goto, skipzero

;  if abs(freq-27.05270) lt 0.001 then stop

  fcheck = freq + f2 * double(l) 
  wc = where(abs(ff - fcheck) lt flim,cc)
  if cc ge 1 then begin
    fcomb(0,cnt) = 1 + wf(wc) ; the matching (original) id
    fcomb(1,cnt) = ff(wc) ; the frequency of the mode
    fcomb(2,cnt) = l
    fcomb(3,cnt) = 1 + wf(j)
    fcomb(4,cnt) = f2 
    fcomb(5,cnt) = i + 1
    fcomb(6,cnt) = freq 
    cnt = cnt + 1
  endif

skipzero:
endfor                          ; combination FF = F1 +- N * F2

endfor                          ; for each frequency to check

endfor ; for each individual frequency

if cnt ge 1 then $
 fcomb = fcomb(*,0:cnt-1) else fcomb = -1

allfc = fcomb

if purge and cnt ge 1 then begin

; ++++++++++++++++++++++++++++++++++++++++++++++++++
; Remove combinations that are redundant (Dec 2006)
; ++++++++++++++++++++++++++++++++++++++++++++++++++
nrep = 1 & rep = 0 ; repeat this process 10 times to weed out redundant information
while rep le nrep do begin
s = sort(fcomb(0,*))
fcomb2 = fcomb(*,s)
u = uniq(fcomb2(0,*))
nu = n_elements(u)

; ++++++++++++++++++++++++++++++++++++++++++++++++++
for k=0,nu-1 do begin

; ++++++++++++++++++++++++++++++++++++++++++++++++++
 w = where(fcomb2(0,*) gt 0 and $
           fcomb2(1,*) eq fcomb2(1,u(k)),cw) ; Same freq. in the first "row"
 if cw ge 2 then begin
   for p1=0,cw-1 do begin  
    for p2=0,cw-1 do begin  
     if p1 ne p2 then begin
      if fcomb2(0,w(p1)) ge 0 and fcomb2(0,w(p2)) ge 0 and $
         fcomb2(4,w(p1)) eq fcomb2(6,w(p2)) then begin
       if debug then print,fcomb2(*,w(p1)), ' --> ', fcomb2(*,w(p2))
       fcomb2(0,w(p2)) = -100.
      endif
     endif
   endfor
  endfor
 endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++

; Below
; 0   1        2    3   4         5   6
; F18 17.46   -1  * F28 26.95    +F12 44.39
; F28 26.95   -1  * F18 17.46    +F12 44.39

; F18 17.46   -1  * F28 26.95    +F12 44.39
; F28 26.95   -1  * F18 17.46    +F12 44.39




; ++++++++++++++++++++++++++++++++++++++++++++++++++
 w = where( fcomb2(0,*) gt 0 and $
           (fcomb2(1,*) eq fcomb2(1,u(k)) or $
            fcomb2(6,*) eq fcomb2(1,u(k))),cw) ; Same freq. in the first+third row
 if cw ge 2 then begin
;if fcomb2(0,w(0)) eq 12 then stop & print,fcomb2(*,w)
   for p1=0,cw-1 do begin  
    for p2=0,cw-1 do begin  
     if p1 ne p2 then begin
      if fcomb2(0,w(p1)) ge 0 and fcomb2(0,w(p2)) ge 0 and $
         fcomb2(1,w(p1)) eq fcomb2(6,w(p2)) and $
         fcomb2(4,w(p1)) eq fcomb2(4,w(p2)) then begin
       if debug then print,fcomb2(*,w(p1)), ' ==> ', fcomb2(*,w(p2))
       fcomb2(0,w(p2)) = -100.
       endif
     endif
   endfor
  endfor
endif


; if cw ge 2 and fcomb2(0,w(0)) eq 12 then stop & print,fcomb2(*,w)


; ++++++++++++++++++++++++++++++++++++++++++++++++++

; Below:
; F18 17.46   -1  * F28 26.95    +F12 44.39
; F28 26.95   -1  * F18 17.46    +F12 44.39

; ++++++++++++++++++++++++++++++++++++++++++++++++++
 w = where(fcomb2(0,*) gt 0 and $
     (fcomb2(1,*) eq fcomb2(1,u(k)) or $
      fcomb2(4,*) eq fcomb2(1,u(k))),cw) ; Same freq. in the first+second "row"
 if cw ge 2 then begin
; stop & print,fcomb2(*,w)
   for p1=0,cw-1 do begin  
    for p2=0,cw-1 do begin  
     if p1 ne p2 then begin
      if fcomb2(0,w(p1)) ge 0 and fcomb2(0,w(p2)) ge 0 and $
         fcomb2(1,w(p1)) eq fcomb2(4,w(p2)) and $
         fcomb2(6,w(p1)) eq fcomb2(6,w(p2)) then begin
       if debug then print,fcomb2(*,w(p1)), ' >>>> ', fcomb2(*,w(p2))
       fcomb2(0,w(p2)) = -100.
       ; print,''
      endif
     endif
   endfor
  endfor
 endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++

; ++++++++++++++++++++++++++++++++++++++++++++++++++
 w = where(fcomb2(0,*) gt 0 and $
     (fcomb2(1,*) eq fcomb2(1,u(k)) or $
      fcomb2(4,*) eq fcomb2(1,u(k))),cw) ; Same freq. in the first+second "row"
 if cw ge 2 then begin
; stop & print,fcomb2(*,w)
   for p1=0,cw-1 do begin  
    for p2=0,cw-1 do begin  
     if p1 ne p2 then begin
      if fcomb2(0,w(p1)) ge 0 and fcomb2(0,w(p2)) ge 0 and $
         fcomb2(1,w(p1)) eq fcomb2(6,w(p2)) and $
         fcomb2(6,w(p1)) eq fcomb2(4,w(p2)) then begin
       if debug then print,fcomb2(*,w(p1)), ' >>>> ', fcomb2(*,w(p2))
       fcomb2(0,w(p2)) = -100.
       endif
     endif
   endfor
  endfor
 endif
; ++++++++++++++++++++++++++++++++++++++++++++++++++


; if cw ge 2 and fcomb2(0,w(0)) eq 12 then stop & print,fcomb2(1,u(k)) & print,fcomb2(*,w)


endfor

; ++++++++++++++++++++++++++++++++++++++++++++++++++
wgood = where(fcomb2(0,*) ge 0,cnt)
fcomb = fcomb2(*,wgood)
rep = rep + 1
endwhile
; ++++++++++++++++++++++++++++++++++++++++++++++++++

endif

print, ' %%% I found ' + string(cnt,format='(I4)') + ' combination frequencies!'
; help,fcomb
 

x = sort(fcomb(0,*))         
for k=0,cnt-1 do print,'F',fcomb(0:2,x(k)),'* F',fcomb(3:4,x(k)),'+F',fcomb(5:6,x(k)),$
format='(A1,I2,F7.2,I5, A5,I2,F7.2, A6, I2,F7.2)'


END
