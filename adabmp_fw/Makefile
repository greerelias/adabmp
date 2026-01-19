.PHONY: deploy build

deploy: bin/adabmp_fw.uf2
	@echo "Hold bootsel button on pico while you plug in the USB cable and press any key to continue. You can release the bootsel while Loading into Flash."
	@read anykey
	picotool load bin/adabmp_fw.uf2
	@echo "Now reboot the device by replugging the USB cable."

build: bin/adabmp_fw

bin/adabmp_fw.uf2: bin/adabmp_fw
	picotool uf2 convert bin/adabmp_fw -t elf bin/adabmp_fw.uf2

bin/adabmp_fw: src/* *.gpr config/*
	alr build --development
