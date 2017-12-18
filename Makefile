default:
	$(MAKE) -C Porridge lib
	cp Porridge/porridge.cmi Porridge/porridge.cmxa Porridge/porridge.a Source/
	$(MAKE) -C Source

clean:
	$(MAKE) -C Porridge clean
	$(MAKE) -C Source clean
