# windows 32 api

## sysinfoapi.h

[GetSystemInfo function (sysinfoapi.h)](https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/nf-sysinfoapi-getsysteminfo)

This method can be used to request many system information values. The following table gives the sName value that is used to request the information and the associated type of the returned value.

|sName|Return|type|Description|
|---|---|---|---|

+ `DirectoryServiceAvailable`;`Boolean`
Set to true if the directory service is available; otherwise, false.

+ `DoubleClickTime`; `Integer`
The double-click time, in milliseconds.

+ `ProcessorLevel`;`Integer`
Windows Vista and later. The processor level. Returns 3, 4, or 5, for x386, x486, and Pentium-level processors, respectively.
+ `ProcessorSpeed`;`Integer`
The processor speed, in megahertz (MHz).
+ `ProcessorArchitecture`; `Integer`
The processor architecture. For details, see the discussion of the wProcessorArchitecture member of the [SYSTEM_INFO](https://learn.microsoft.com/en-us/windows/win32/api/sysinfoapi/ns-sysinfoapi-system_info) structure.
+ `PhysicalMemoryInstalled`; `Integer`
The amount of physical memory installed, in bytes.

**The following are valid only on Windows XP.**

+ `IsOS_Professional`; `Boolean`
Set to true if the operating system is Windows XP Professional Edition; otherwise, false.
+ `IsOS_Personal`; `Boolean`
Set to true if the operating system is Windows XP Home Edition; otherwise, false.

**The following is valid only on Windows XP and later.**

+ `IsOS_DomainMember`;`Boolean`
Set to true if the computer is a member of a domain; otherwise, false.
This method is not currently available in Microsoft Visual Basic.
