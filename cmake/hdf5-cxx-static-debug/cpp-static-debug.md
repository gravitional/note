INSTRUCTIONS FOR BUILDING AN HDF5 APPLICATION
(static, debug, C, C++, HL, HL C++)

These instructions use the HDF5 source code that is packaged with CMake
(CMake-hdf5-1.N.N.zip or CMake-hdf5-1.N.N.tar.gz).

BUILD HDF5:

 Make the platform specific changes described below, and then follow the instructions
 for building HDF5 that are found here:
   https://portal.hdfgroup.org/display/support/Building+HDF5+with+CMake

 Linux: 

   o Edit build-unix.sh and change the -C option to use "Debug":
      ctest -S HDF5config.cmake,BUILD_GENERATOR=Unix -C Debug -VV -O hdf5.log

   o Edit HDF5options.cmake. Under "Only build static libraries" and uncomment this line:
      set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=OFF")

  Windows 64-bit:

   o Edit build-VS201N-64.bat and change the -C option to use "Debug". For example:
       ctest -S HDF5config.cmake,BUILD_GENERATOR=VS201364 -C Debug -VV -O hdf5.log

   To link to the the static runtime library on Windows 64-bit in debug mode, make the following
   changes to the source code before building:
    
    o Edit the .\<HDF5 source>\config\cmake\UserMacros\Windows_MT.cmake file and
      set the correct options as needed (/MTd, /MT,..) on or around line 33:
        string (REGEX REPLACE "/MD" "/MT" ${flag_var} "${${flag_var}}")
   
    o Edit the .\<HDF5 source>\UserMacros.cmake file and make these changes:

     - Insert the contents of the above file (Windows_MT.cmake) in the section under 
       "Include file for user options". (Alternately, you can try adding an include file
       as directed, but this may not work.)
   
     - Turn BUILD_STATIC_CRT_LIBS on at the bottom of the UserMacros.cmake file:
         option (BUILD_STATIC_CRT_LIBS "Build With Static CRT Libraries" ON)

       Alternately, add this line to the HDF5options.cmake file:
          set (ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_STATIC_CRT_LIBS:BOOL=ON")


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

    NOTE: You can also set the HDF5_DIR environment variable within a CMake application

  o From the command-line, create a source and build directory:
       mkdir source
       mkdir build

  o  Place the build script (CMakeLists.txt) and the application in the source directory.

  o  Go into the build directory, and run the following cmake commands to build the application:
       cd build
       cmake source
       cmake --build . --config <Release|Debug>

  o  Run the created executable: runapp