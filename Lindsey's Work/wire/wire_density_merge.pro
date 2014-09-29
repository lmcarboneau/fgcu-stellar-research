PRO wire_density_merge, files, outfile = outfile

; Merge the power density files for several directories
;
; Example:
;
; Make the time series of the simulations:
;
; granulation, '~/wire/wire_lc/procyon_2000.dat', $
;  [500,750,1000.], [.8,.85,.9,.95,1.,1.1], [8,10,12,14,16,18], 1.0, wn = 110.,$
;   outdir='~/wire/wire_sim00_itA/'
; 
; granulation, '~/wire/wire_lc/procyon_2000.dat', $
;  [500,750,1000.], [.8,.85,.9,.95,1.,1.1], [8,10,12,14,16,18], 1.0, wn = 110.,$
;   outdir='~/wire/wire_sim00_itB/'
;
; Calculate power density spectra for all simulations:
; 
; wire_density_all,$ 
;  '/home/bruntt/wire/wire_sim00_itA/procyon_2000*.dat', $
;  outdir='/home/bruntt/wire/wire_sim00_itA/density/', fmax=7000.
; 
; wire_density_all,$ 
;  '/home/bruntt/wire/wire_sim00_itB/procyon_2000*.dat', $
;  outdir='/home/bruntt/wire/wire_sim00_itB/density/', fmax=7000.
;
; ... launch this:

; wire_density_merge,$
; ['/home/bruntt/wire/wire_sim99_itA/density/',$
;  '/home/bruntt/wire/wire_sim99_itB/density/],$
;  outfile = '~/wire/wire_sim99_itG/Procyon1999_merge_density.idl'

nf = n_elements(files)

w99 = where(strmatch(files,'*99*') eq 1,c99)
w00 = where(strmatch(files,'*00*') eq 1,c00)
if c99 ge 1 then obsfile = 'N.A.1999.N.A.'
if c00 ge 1 then obsfile = 'N.A.2000.N.A.' ; for the right output file name
default9, obsfile, ' N. A. '
print,' %%% obsfile : ' + obsfile

for i=0,nf-1 do begin

; Read the filenames and their parameters in structure, comp
 wildcard = files(i) + '/*.density'
 comp = 0B



 wire_density_compare, wildcard, obsfile, /readonly, comp=comp, /silent

; Make master structure:
 if i eq 0 then begin
  nsim = n_elements(comp)
  nall = nf * nsim 
  restore,comp(0).nam & nd = n_elements(psim)
  master = replicate({nam:'NA',$
   tgran:-1., agran:-1., amode:-1., lifet:-1., $
   wn:0.,f:fltarr(nd), d:fltarr(nd), id:0}, nall)
  cntr = 0
 endif

 for g=0,nsim-1 do begin
  restore,comp(g).nam ; get psim structure

  master(cntr).tgran = comp(g).tgran ; store simulation parameters
  master(cntr).agran = comp(g).agran
  master(cntr).amode = comp(g).amode
  master(cntr).lifet = comp(g).lifet
  master(cntr).wn    = comp(g).wn
  master(cntr).id    = g

  master(cntr).f     = psim.f2 ; store frequency
  master(cntr).d     = psim.d2 ; store power density

  cntr = cntr + 1
 endfor

endfor

; Get the unique simulation parameters
tgran = master.tgran & s = sort(tgran) & tgran = tgran(s)
agran = master.agran & s = sort(agran) & agran = agran(s)
amode = master.amode & s = sort(amode) & amode = amode(s)
lifet = master.lifet & s = sort(lifet) & lifet = lifet(s)

u_tgran = uniq(tgran) & tgran2 = tgran(u_tgran)
u_agran = uniq(agran) & agran2 = agran(u_agran)
u_amode = uniq(amode) & amode2 = amode(u_amode)
u_lifet = uniq(lifet) & lifet2 = lifet(u_lifet)

n_tgran = n_elements(tgran2)
n_agran = n_elements(agran2)
n_amode = n_elements(amode2)
n_lifet = n_elements(lifet2)

simpar = {tgran:fltarr(n_tgran), $
                   agran:fltarr(n_agran), $
                   amode:fltarr(n_amode), $
                   lifet:fltarr(n_lifet)}

simpar.tgran = tgran2
simpar.agran = agran2
simpar.amode = amode2
simpar.lifet = lifet2


  merg = replicate({nam:'NA',$
   tgran:-1., agran:-1., amode:-1., lifet:-1., $
   wn:0.,f:fltarr(nd), d:fltarr(nd), e:fltarr(nd), $
   id:0, nsim:0B}, nsim)

  cnt = 0L

print,' %%% Progress [0..'+strcompress(nsim,/remove_all) + '] :'

smoof = 25. ; use several point to get a realistic error estimate

for i=0,n_tgran-1 do begin
 for j=0,n_agran-1 do begin
  for k=0,n_amode-1 do begin
   for l=0,n_lifet-1 do begin
 
; Find all simulations with the same parameters
    w = where(master.tgran eq simpar.tgran(i) and $
              master.agran eq simpar.agran(j) and $
              master.amode eq simpar.amode(k) and $
              master.lifet eq simpar.lifet(l), c)

    merg(cnt).nsim = c
    merg(cnt).id   = master(w(0)).id
    merg(cnt).tgran = master(w(0)).tgran
    merg(cnt).agran = master(w(0)).agran
    merg(cnt).amode = master(w(0)).amode
    merg(cnt).lifet = master(w(0)).lifet
    merg(cnt).wn    = master(w(0)).wn

    merg(cnt).f = master(w(0)).f

; Mean value for these simulations
    for p=0L,nd-1 do $
     merg(cnt).d(p) = avg(master(w).d(p))
    for p=0L,nd-1 do $
     merg(cnt).e(p) = robust_sigma((master(w).d(p)))

; Typical error for these simulations
;    sm = fltarr(smoof) ; auxil. array
;    for p=0,nd-1-smoof do begin
;     for x=0,smoof-1 do $
;      sm(x) = robust_sigma(master(w).d(p+x))
;     resistant_mean, sm, 3, me, sd, nr
;     merg(cnt).e(p) = me ; robust error
;    endfor
;    merg(cnt).e(p:p+smoof-1) = merg(cnt).e(p-1) ; edge effect ...

;plot_oo,merg(cnt).f,merg(cnt).e
;plot_oo,merg(cnt).d

;plot_oo, master(w(0)).f, master(w(0)).d,xr=[10,10000]
;oplot, master(w(1)).f, master(w(1)).d
;oplot, master(w(2)).f, master(w(2)).d

; Errors look weird around the orbital harmonics

    cnt = cnt + 1 ; go to next unique simulation
    
  endfor
 endfor
endfor

print,cnt,format='(I4,$)'

endfor


print, ''

default9, outfile, '~/wire/wire_master.idl'
outfile2 = outfile + '.raw'
default9, outfile2, '~/wire/wire_master.idl2'

save, filename=outfile2, master, simpar
save, filename=outfile,  merg,   simpar

print,' %%% Saved raw master file: ' + outfile2
print,' %%% Saved merged file: ' + outfile

END
