all: a1basic-check

%.lst %.p: %.asm
	asl $< -o $*.p -LC

a1basic.bin: a1basic.p
	p2bin -r '$$e000-$$efff' a1basic.p a1basic.bin

a1basic-check: a1basic.bin
	echo "56d5cd968557c81a99cde298d76030f65bb7ce9a85bc2ff0fed5726d50b91499 a1basic.bin" | sha256sum -c -

