# adabmp

> An Ada-based FPGA bitstream and firmware programmer for Artix-7 devices, using a Raspberry Pi Pico as the programming interface.

---

## Table of Contents

- [Overview](#overview)
- [Requirements](#requirements)
- [Development Environment Setup](#development-environment-setup)
  - [Installing Alire](#1-installing-alire)
  - [Installing VSCode and the Ada & SPARK Plugin](#2-installing-vscode-and-the-ada--spark-plugin)
  - [Installing picotool](#3-installing-picotool)
- [Installation](#installation)
- [Usage](#usage)
  - [Test Connection](#test-connection-to-the-programmer)
  - [Board Info](#retrieve-target-board-information)
  - [Configure Target](#configure-the-target)
  - [UART Mode](#start-uart-mode)
  - [Flash Bitstream](#flash-bitstream)
  - [Flash Firmware](#flash-firmware)
- [Hardware Setup](#hardware-setup)

---

## Overview

`adabmp` is a command-line tool that programs Artix-7 FPGAs over JTAG using a Raspberry Pi Pico as the programmer. It supports loading bitstreams directly into SRAM, storing bitstreams in external flash, flashing NEORV32 firmware, and bridging UART communication — all from a host running Ubuntu Linux.

---

## Requirements

### Host System
| Requirement | Minimum |
|---|---|
| OS | Ubuntu 24.04 LTS |
| CPU | 2 GHz Dual Core |
| RAM | 4 GB |
| Storage | 1 GB free |
| USB | USB 2.0 |

### Hardware
- **Raspberry Pi Pico** — acts as the JTAG/UART programmer
- **USB cables** — to connect Pico to host and target
- **Assorted Male/Female Flying Leads with Dupont connectors** — for JTAG/UART wiring
- **Artix-7 FPGA Development Board with JTAG header** (e.g., Digilent Basys-3)

> **Note:** The Basys-3 board does not come with header pins pre-installed for the JTAG connection. Refer to the [Basys-3 Reference Manual](https://digilent.com/reference/programmable-logic/basys-3/reference-manual) for details.

---

## Development Environment Setup

These instructions are for an **x86-64 system running Ubuntu 24.04 LTS**. If you need to install Ubuntu first, see the [official Ubuntu install guide](https://ubuntu.com/tutorials/install-ubuntu-desktop).

### 1. Installing Alire

Alire is the Ada package manager required to build this project.

1. Visit [https://alire.ada.dev/](https://alire.ada.dev/) and click **"Download Alire for Linux"**.

2. Open a terminal, navigate to your Downloads folder, and unzip the archive (replace `alire-filename` with the actual downloaded filename, likely `alr-2.1.0-bin-x86_64-linux`):

   ```bash
   mkdir temp && unzip -d temp <alire-filename>.zip
   ```

3. Copy the Alire binary to a directory on your system path:

   ```bash
   sudo cp temp/bin/alr /usr/local/bin
   ```

4. Verify the installation:

   ```bash
   alr
   ```

   You should see the Alire version and a list of available options.

5. Clean up the temporary files:

   ```bash
   rm -rf temp <alire-filename>.zip
   ```

---

### 2. Installing VSCode and the Ada & SPARK Plugin

1. Install VSCode via snap:

   ```bash
   sudo snap install --classic code
   ```

2. Open VSCode:

   ```bash
   code
   ```

3. Click the **Extensions** icon in the left toolbar, search for `Ada`, and install the **Ada & SPARK** extension.

---

### 3. Installing picotool

`picotool` is used to flash firmware to the Raspberry Pi Pico.

1. Install dependencies:

   ```bash
   sudo apt update
   sudo apt install git cmake build-essential libusb-1.0-0-dev
   ```

2. Clone and build picotool:

   ```bash
   git clone https://github.com/raspberrypi/picotool.git
   cd picotool
   mkdir build && cd build
   cmake ..
   make -j$(nproc)
   ```

3. Copy the binary to your system path:

   ```bash
   sudo cp picotool /usr/local/bin/
   ```

4. Verify the installation:

   ```bash
   picotool help
   ```

   A list of available commands confirms successful installation.

5. Add udev rules to allow USB access without root:

   ```bash
   sudo nano /etc/udev/rules.d/99-picotool.rules
   ```

   Paste the following line, then save and exit (`Ctrl+S`, `Ctrl+X`):

   ```
   SUBSYSTEM=="usb", ATTR{idVendor}=="2e8a", MODE="0666"
   ```

6. Reload udev rules:

   ```bash
   sudo udevadm control --reload-rules
   sudo udevadm trigger
   ```

7. Test the full setup by connecting your Pico in BOOTSEL mode and running:

   ```bash
   picotool info
   ```

---

## Installation

1. **Clone the repository:**

   ```bash
   git clone https://github.com/greerelias/adabmp
   ```

   Or download the ZIP from the [GitHub page](https://github.com/greerelias/adabmp) via the green **Code** button → **Download ZIP**, then extract it.

2. **Build the host application:**

   ```bash
   cd adabmp
   alr build
   ```

3. **Build the Pico firmware:**

   ```bash
   cd adabmp_fw
   alr build
   ```

4. **Flash the firmware to the Pico:**

   Connect the Pico to your host via USB while holding the **BOOTSEL** button, then run:

   ```bash
   make
   ```

   Press any key when prompted to begin the transfer. When complete, unplug and replug the Pico to restart it.

   <img width="420" height="71" alt="picotool_flash" src="https://github.com/user-attachments/assets/d1b8b1fa-a222-4f13-bd1e-7e9ffa29b3d7" />


6. **Wire the Pico to your target FPGA** using flying leads. See the [Hardware Setup](#hardware-setup) section below for pinout details.

---

## Usage

Navigate to the `/bin/` directory in the project root and run:

```bash
./adabmp
```

&nbsp;
<img width="468" height="78" alt="cli_args" src="https://github.com/user-attachments/assets/bbc04e3a-d94a-429f-ac79-0b21248f6096" />
&nbsp;

This prints all available commands and arguments. The general usage pattern is:

```bash
./adabmp -<flag> [optional arguments]
```

Make sure the Pico programmer is connected to both the host and the target when running any command.

---

### Test Connection to the Programmer

Verifies that the programmer firmware is installed and working. Displays the current firmware version and the result of a connection test.

```bash
./adabmp -t
# or
./adabmp --test
```

---

### Retrieve Target Board Information

Reads and decodes the target FPGA's IDCODE. Also useful for verifying that the JTAG connection is working correctly.

```bash
./adabmp -i
# or
./adabmp --info
```

---

### Configure the Target

Loads a bitstream directly into the FPGA's SRAM. The FPGA begins running immediately, but the configuration is **lost on power down**. For persistent storage, use [Flash Bitstream](#flash-bitstream) instead.

```bash
./adabmp -c <path/to/bitstream.bit>
# or
./adabmp --configure <path/to/bitstream.bit>
```

A progress bar displays transfer status.

---

### Start UART Mode

Puts the programmer into UART bridge mode for communicating with the NEORV32 UART bootloader or any other UART interface on the target. The serial port name is printed after execution.

```bash
./adabmp -u
# or
./adabmp --uart
```

> **Note:** To exit UART mode, unplug the Pico from the host to reset it.

---

### Flash Bitstream

Stores a bitstream in the target's external flash memory. The FPGA will automatically load this configuration at power-on.

```bash
./adabmp -f <path/to/bitstream.bit>
# or
./adabmp --flash <path/to/bitstream.bit>
```

Progress bars display each step. The target resets and loads the stored bitstream upon completion.

> **Note (Basys-3 only):** The Basys-3 board may require additional configuration to enable SPI flash boot mode. Refer to the Basys-3 reference manual.

---

### Flash Firmware

Stores NEORV32 firmware in flash at a specified base address. If no address is provided, the default address `0x300000` is used.

```bash
./adabmp -fw <path/to/firmware.bin> [base_address]
# or
./adabmp --firmware <path/to/firmware.bin> [base_address]
```

Progress is displayed during transfer.

---

## Hardware Setup

Connect the Raspberry Pi Pico to the target FPGA using flying leads with Dupont connectors. Refer to the following for pinout information:

- **Pico JTAG & UART Pinout** — see Figure 36 in the project documentation
- **Basys-3 JTAG & UART Connections** — see Figures 37–39 in the project documentation

> **Tip:** If using the Digilent Basys-3, note that JTAG header pins are not pre-installed. You will need to solder them before making connections.

### Pico & Basys-3 Diagram
<img width="624" height="260" alt="pico_basys3" src="https://github.com/user-attachments/assets/31795bb7-81fe-48f7-80c6-55b63ab6aaf7" />

### Pico Pinout Diagram
<img width="1320" height="1093" alt="pico_pinout" src="https://github.com/user-attachments/assets/70b7006c-d9a6-451b-976b-11b15838afe5" />

### Basys-3 JTAG Pins Diagram
<img width="702" height="711" alt="jtag_connections" src="https://github.com/user-attachments/assets/8f2fd0d9-6f54-4786-9f6d-622e897ebd29" />

### Basys-3 UART Connections Diagram - PMOD Connecter JA
<img width="928" height="433" alt="uart_connections" src="https://github.com/user-attachments/assets/6f823d1b-535c-4959-88f5-22a5ac44bfa5" />


