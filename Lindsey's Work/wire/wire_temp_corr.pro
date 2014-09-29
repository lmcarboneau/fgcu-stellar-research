; PRO wire_corr_hjd, files

nf = n_elements(files)

for f=0,nf-1 do begin

restore,files(f)


endfor



END
