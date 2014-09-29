pro a4e, x=x, y=y, name=name
	if (N_ELEMENTS(name) EQ 0) then name='idl.ps'
	if (N_ELEMENTS(x) EQ 0) then x = 17.78
	if (N_ELEMENTS(y) EQ 0) then y = 25.4
	set_plot, 'ps'
	device, xsize=x, ysize=y, yoffset=27.3-y, COLOR=0, BITS=4, filename=name, /encapsulated
;	device, xsize=x, ysize=y-0.4, yoffset=26.9-y, COLOR=0, BITS=4, filename=name
end
