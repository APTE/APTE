default:
	$(MAKE) -C Porridge
	$(MAKE) -C Porridge cleanPorridge
	$(MAKE) -C Source

clean:
	$(MAKE) -C Porridge clean
	$(MAKE) -C Source clean
