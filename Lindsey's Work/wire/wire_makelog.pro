

spawn,'ls -1 /ai40/bruntt/wire/collection/*.idl',ll
nl = n_elements(ll)

logg = replicate( { mint:0., maxt:0., object:'', $
 filename:'', np:0L, tp:0., np2:0L, noise:0.,deltat:0.}, nl)

for i=0,nl-1 do begin

restore,ll(i)

w = where(wireult.mag(0) gt 4.,c)

if c le 10 then w = findgen(n_elements(wireult))

mint = min(wireult(w).hjd)
maxt = max(wireult(w).hjd)
ptp_robust_fin, wireult(w).mag(0), noise, 1

logg(i).mint = mint
logg(i).maxt = maxt
logg(i).noise = noise

a = strsplit(ll(i),'/',/extract) & na = n_elements(a)
logg(i).filename = a(na-1)
b = strsplit(logg(i).filename,'_',/extract)
logg(i).object = b(0)

logg(i).np = c ; number of good points
logg(i).np2 = n_elements(wireult) ; total number of points
logg(i).tp = c / (maxt - mint) ; antal punkter pr. dag

logg(i).deltat = maxt - mint

;; if i ge 2 then stop

endfor

outfile = '/ai40/bruntt/wire/collection/wire_logfile.idl'
save,filename=outfile,logg
print,' %%% restore filename: '+outfile

a = sort(logg.mint)
log2 = logg(a)

w = uniq(log2.object)
log2 = log2(w)
nl2 = n_elements(log2)

for i=0,nl2-1 do $
 print,log2(i).object, log2(i).mint, log2(i).deltat, log2(i).np, $
 format='(A15, F8.1, F8.2, I8)'

end
