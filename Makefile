HCFLAGS = -O

all:
	ghc $(HCFLAGS) -o exi --make Main.hs

prof:
	ghc $(HCFLAGS) -prof -o exi.p --make -auto-all Main.hs

clean:
	rm *.o *.hi Portage/*.o Portage/*.hi
