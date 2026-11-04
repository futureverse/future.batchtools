# batchtools (0.9.18)

* GitHub: <https://github.com/mlr-org/batchtools>
* Email: <mailto:michellang@gmail.com>
* GitHub mirror: <https://github.com/cran/batchtools>

Run `revdepcheck::revdep_details(, "batchtools")` for more info

## In both

*   checking dependencies in R code ... NOTE
     ```
     ...
     localhost::pid32849.R: Unable to initialize verbs NIC /sys/class/infiniband/mlx5_0 (unit 0:0)
     localhost::pid32849: PSM3 can't open nic unit: 0 (err=23)
     localhost::pid32849.R: Unable to create send CQ of size 5080 on mlx5_0: Cannot allocate memory
     localhost::pid32849.R: Unable to initialize verbs NIC /sys/class/infiniband/mlx5_0 (unit 0:0)
     --------------------------------------------------------------------------
     No OpenFabrics connection schemes reported that they were able to be
     used on a specific port.  As such, the openib BTL (OpenFabrics
     support) will be disabled for this port.
     
       Local host:           dev3
       Local device:         mlx5_0
       Local port:           1
       CPCs attempted:       rdmacm, udcm
     --------------------------------------------------------------------------
     localhost::pid32849: PSM3 can't open nic unit: 0 (err=23)
     localhost::pid32849.R: Unable to create send CQ of size 5080 on mlx5_0: Cannot allocate memory
     localhost::pid32849.R: Unable to initialize verbs NIC /sys/class/infiniband/mlx5_0 (unit 0:0)
     --------------------------------------------------------------------------
     Open MPI failed an OFI Libfabric library call (fi_endpoint).  This is highly
     unusual; your job may behave unpredictably (and/or abort) after this.
     
       Local host: dev3
       Location: mtl_ofi_component.c:513
       Error: Invalid argument (22)
     --------------------------------------------------------------------------
     ```

*   checking for detritus in the temp directory ... NOTE
     ```
     Found the following files/directories:
       ‘ompi.dev3.34002’
     ```

