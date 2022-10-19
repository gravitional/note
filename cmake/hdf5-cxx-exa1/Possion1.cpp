#include <iostream>
#include <string>
#include "hdf5.h"
#include "H5Cpp.h"

using namespace H5;

const H5std_string FILE_NAME("C:/SimCases/d22915-capacitance3d/capacitance3d/Solving/SolvingDomain/result/result.h5");
const H5std_string GROUP_NAME("Post");
const H5std_string GROUP_NAME2("Force");
const H5std_string GROUP_NAME3("Post1_F");
const H5std_string DATASET_NAME("Value0");
const H5std_string ATTR_NAME("Value0");

int main(int argc, char *argv[])
{
    try
    {
        Exception::dontPrint();
        H5File file(FILE_NAME.c_str(), H5F_ACC_RDONLY);
        Group group = file.openGroup(GROUP_NAME.c_str());
        Group group2 = group.openGroup(GROUP_NAME2.c_str());
        Group group3 = group2.openGroup(GROUP_NAME3.c_str());
        DataSet dataset = group3.openDataSet(DATASET_NAME.c_str());
        // Attribute attr = dataset.openAttribute(ATTR_NAME.c_str());
        std::cout << "test string" << std::endl;
        return 0;
    }
    catch (FileIException error)
    {
        error.printErrorStack();
        return -1;
    }
}