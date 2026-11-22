# batchtools ()

* GitHub: <https://github.com/futureverse/future.batchtools>
* Email: <mailto:henrikb@braju.com>

Run `revdepcheck::revdep_details(, "batchtools")` for more info

## Error before installation

### Devel

```
checking for gcc... gcc -std=gnu2x
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether the compiler supports GNU C... yes
checking whether gcc -std=gnu2x accepts -g... yes
checking for gcc -std=gnu2x option to enable C11 features... none needed
checking for pkg-config... /usr/bin/pkg-config
...
gcc -std=gnu2x -I"/wynton/home/cbi/shared/software/CBI/_rocky8/R-4.6.1-gcc13/lib64/R/include" -DNDEBUG -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -I/usr/include/openmpi-x86_64 -pthread  -DMPI2 -DOPENMPI  -I/usr/local/include    -fpic  -g -O2  -c conversion.c -o conversion.o
gcc -std=gnu2x -I"/wynton/home/cbi/shared/software/CBI/_rocky8/R-4.6.1-gcc13/lib64/R/include" -DNDEBUG -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -I/usr/include/openmpi-x86_64 -pthread  -DMPI2 -DOPENMPI  -I/usr/local/include    -fpic  -g -O2  -c internal.c -o internal.o
gcc -std=gnu2x -shared -L/wynton/home/cbi/shared/software/CBI/_rocky8/R-4.6.1-gcc13/lib64/R/lib -L/usr/local/lib64 -o Rmpi.so Rmpi.o conversion.o internal.o -L/usr/lib64/openmpi/lib -Wl,-rpath -Wl,/usr/lib64/openmpi/lib -Wl,--enable-new-dtags -lmpi -L/wynton/home/cbi/shared/software/CBI/_rocky8/R-4.6.1-gcc13/lib64/R/lib -lR
Error: package or namespace load failed for ‘Rmpi’:
 .onLoad failed in loadNamespace() for 'Rmpi', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/scratch/henrik/revdep/future.batchtools/library/batchtools/00LOCK-Rmpi/00new/Rmpi/libs/Rmpi.so':
  /scratch/henrik/revdep/future.batchtools/library/batchtools/00LOCK-Rmpi/00new/Rmpi/libs/Rmpi.so: undefined symbol: MPI_Info_get_string
Error: loading failed
Execution halted


* installing *binary* package ‘backports’ ...
* package ‘backports’ successfully unpacked and SHA256 sums checked
* DONE (backports)
* installing *binary* package ‘base64enc’ ...
* package ‘base64enc’ successfully unpacked and SHA256 sums checked
* DONE (base64enc)
* installing *binary* package ‘brew’ ...
* package ‘brew’ successfully unpacked and SHA256 sums checked
* DONE (brew)
* installing *binary* package ‘brio’ ...
...

The downloaded source packages are in
	‘/scratch/hb/RtmpnKWJRw/downloaded_packages’
Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE
In addition: Warning messages:
1: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘Rmpi’ had non-zero exit status
2: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘Rmpi’ had non-zero exit status


```
### CRAN

```
checking for gcc... gcc -std=gnu2x
checking whether the C compiler works... yes
checking for C compiler default output file name... a.out
checking for suffix of executables... 
checking whether we are cross compiling... no
checking for suffix of object files... o
checking whether the compiler supports GNU C... yes
checking whether gcc -std=gnu2x accepts -g... yes
checking for gcc -std=gnu2x option to enable C11 features... none needed
checking for pkg-config... /usr/bin/pkg-config
...
gcc -std=gnu2x -I"/wynton/home/cbi/shared/software/CBI/_rocky8/R-4.6.1-gcc13/lib64/R/include" -DNDEBUG -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -I/usr/include/openmpi-x86_64 -pthread  -DMPI2 -DOPENMPI  -I/usr/local/include    -fpic  -g -O2  -c conversion.c -o conversion.o
gcc -std=gnu2x -I"/wynton/home/cbi/shared/software/CBI/_rocky8/R-4.6.1-gcc13/lib64/R/include" -DNDEBUG -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -I/usr/include/openmpi-x86_64 -pthread  -DMPI2 -DOPENMPI  -I/usr/local/include    -fpic  -g -O2  -c internal.c -o internal.o
gcc -std=gnu2x -shared -L/wynton/home/cbi/shared/software/CBI/_rocky8/R-4.6.1-gcc13/lib64/R/lib -L/usr/local/lib64 -o Rmpi.so Rmpi.o conversion.o internal.o -L/usr/lib64/openmpi/lib -Wl,-rpath -Wl,/usr/lib64/openmpi/lib -Wl,--enable-new-dtags -lmpi -L/wynton/home/cbi/shared/software/CBI/_rocky8/R-4.6.1-gcc13/lib64/R/lib -lR
Error: package or namespace load failed for ‘Rmpi’:
 .onLoad failed in loadNamespace() for 'Rmpi', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/scratch/henrik/revdep/future.batchtools/library/batchtools/00LOCK-Rmpi/00new/Rmpi/libs/Rmpi.so':
  /scratch/henrik/revdep/future.batchtools/library/batchtools/00LOCK-Rmpi/00new/Rmpi/libs/Rmpi.so: undefined symbol: MPI_Info_get_string
Error: loading failed
Execution halted


* installing *binary* package ‘backports’ ...
* package ‘backports’ successfully unpacked and SHA256 sums checked
* DONE (backports)
* installing *binary* package ‘base64enc’ ...
* package ‘base64enc’ successfully unpacked and SHA256 sums checked
* DONE (base64enc)
* installing *binary* package ‘brew’ ...
* package ‘brew’ successfully unpacked and SHA256 sums checked
* DONE (brew)
* installing *binary* package ‘brio’ ...
...

The downloaded source packages are in
	‘/scratch/hb/RtmpnKWJRw/downloaded_packages’
Error in (function (libdir, packages, quiet, repos)  : 
  all(packages %in% rownames(installed.packages(libdir[1]))) is not TRUE
In addition: Warning messages:
1: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘Rmpi’ had non-zero exit status
2: In utils::install.packages(pkgs = pkgs, lib = lib, repos = myrepos,  :
  installation of package ‘Rmpi’ had non-zero exit status


```
