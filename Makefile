default:
	$(MAKE) -C Porridge
	cp Porridge/porridge.cmi Porridge/porridge.cmxa Source/
	$(MAKE) -C Source

clean:
	$(MAKE) -C Porridge clean
	$(MAKE) -C Source clean
