default:
	$(MAKE) -C Porridge install
	$(MAKE) -C Source

clean:
	$(MAKE) -C Porridge clean
	$(MAKE) -C Source clean
