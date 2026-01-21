.PHONY: deploy build

deploy: bin/main.uf2
	@echo "Hold bootsel button on pico while you plug in the USB cable and press any key to continue. You can release the bootsel while Loading into Flash."
	@read anykey
	picotool load bin/main.uf2
	@echo "Now reboot the device by replugging the USB cable."

build: bin/main

bin/main.uf2: bin/main
	picotool uf2 convert bin/main -t elf bin/main.uf2

bin/main: src/* *.gpr config/*
	alr build --development
