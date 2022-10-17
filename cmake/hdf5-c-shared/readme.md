INSTRUCTIONS FOR BUILDING AN HDF5 APPLICATION
(release, C, C++)

These instructions use the HDF5 source code that is packaged with CMake
(CMake-hdf5-1.N.N.zip or CMake-hdf5-1.N.N.tar.gz).

BUILD HDF5:

 Build HDF5 according to the instructions on the "Building HDF5 with CMake" page:

     https://portal.hdfgroup.org/display/support/Building+HDF5+with+CMake

 If you encounter any issues with building, edit the build script and change -V to -VV.

CREATE A WORKING SPACE AND BUILD THE APPLICATION

  o Set the HDF5_DIR environment variable to the location of the release or debug built cmake files.
    Those are located in the share/cmake/hdf5/ directory in the built binary distribution.

      Unix:
       The HDF5-1.N.N-Linux.tar.gz file is created in the CMake-hdf5-1.N.N directory when HDF5 is
       built on Linux with CMake. Uncompress the file, and  you will find that it contains the
       the share/cmake/hdf5/ directory. Set the HDF5_DIR environment variable to the location
       of this directory:

       setenv HDF5_DIR <mypath>/HDF5-1.N.N-Linux/HDF_Group/HDF5/1.N.N/share/cmake/hdf5

      Windows 64-bit:
       The HDF5-1.N.N-win64.zip is created in the CMake-hdf5-1.N.N directory when HDF5 is built
       on Windows 64-bit. Uncompress the file, and you will find that it contains a directory
       with cmake files in share\cmake\hdf5. Set the HDF5_DIR environment variable to the location
       of this directory.

       On Windows 7, left click on Start at the bottom left and then right-click on
       Computer and select Properties from the menu that pops up. The Control Panel Home window
       opens. Select "Advanced system settings" on the left to bring up the "System Properties".
       Select the "Environment Variables" button near the bottom, where you add User and System
       variables.

    NOTE: You can also set the HDF5_DIR environment variable within a CMake application.

  o From the command-line, create a source and build directory:
       mkdir source
       mkdir build

  o  Place the build script (CMakeLists.txt) and the application in the source directory.

  o  Go into the build directory, and run the following cmake commands to build the application:
       cd build
       cmake source
       cmake --build . --config <Release|Debug>

  o  Run the created executable: runapp