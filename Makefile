HCPKGS = -hide-all-packages -package base -package parsec -package unix -package fgl
HCFLAGS = -O $(HCPKGS)

.PHONY : all prof clean exposed

all:
	ghc $(HCFLAGS) -o exi --make Main.hs

prof:
	ghc $(HCFLAGS) -prof -o exi.p --make -auto-all Main.hs

clean:
	rm *.o *.hi
	find Portage \( -name '*.o' -o -name '*.hi' \) -exec rm '{}' \;

exposed:
	find Portage -name '*.hs' | sed -e 's|/|.|g' -e 's|.hs$$|,|' -e 's/^/\t\t\t/'
