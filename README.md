# xframes-fortran

## Instructions

### Install Fortran

#### Raspberry PI 5

So far I have been **unable to run the app** on my Raspberry Pi 5.

`sudo apt install gfortran`

##### Install FPM

###### Through Conda

Based on these instructions: https://fpm.fortran-lang.org/install/index.html

Download and run https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-aarch64.sh



#### Windows

Grab the installer from https://www.msys2.org/#installation

Ensure `pacman` is in `PATH`. Open a terminal and run 

`pacman -S mingw-w64-ucrt-x86_64-gcc-fortran`

Assuming you installed msys2 in `C:\msys64`, add the following to `PATH`:

`C:\msys64\ucrt64\bin`

Install the Fortran Package Manager

Based on https://fpm.fortran-lang.org/install/index.html#install

`pacman -S git mingw-w64-x86_64-gcc-fortran mingw-w64-x86_64-fpm`

Assuming you installed msys2 in `C:\msys64`, add `C:\msys64\mingw64\bin` to `PATH`

Finally, build the app via fpm

`fpm build --link-flag "-L./"`

Run the app via fpm

`fpm run --link-flag "-L./"`

## Screenshots

Windows 11

![image](https://github.com/user-attachments/assets/cd3aab3b-8201-4f50-b4c9-94165d44e5e7)

