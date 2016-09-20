all:
	happy -gca Pico/ParPico.y
	alex -g Pico/LexPico.x
	ghc --make Pico/TestPico.hs -o Pico/TestPico

clean:
	-rm -f Pico/*.log Pico/*.aux Pico/*.hi Pico/*.o Pico/*.dvi

distclean: clean
	-rm -f Pico/DocPico.* Pico/LexPico.* Pico/ParPico.* Pico/LayoutPico.* Pico/SkelPico.* Pico/PrintPico.* Pico/TestPico.* Pico/AbsPico.* Pico/TestPico Pico/ErrM.* Pico/SharedString.* Pico/ComposOp.* Pico/pico.dtd Pico/XMLPico.* Makefile*
		-rmdir -p Pico/

