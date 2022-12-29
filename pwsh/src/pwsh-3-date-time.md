# powershell

[Get-Date](https://learn.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Utility/Get-Date)

In this example,
a `.NET` format specifier is used to customize the output's format.
The output is a String object.

```PowerShell
Get-Date -Format "dddd MM/dd/yyyy HH:mm K"

Tuesday 06/25/2019 16:17 -07:00
```

`Get-Date` uses the `Format` parameter to specify several format specifiers.

The `.NET` format specifiers used in this example are defined as follows:

Specifier Definition

+ `dddd` Day of the week - full name
+ `MM` Month number
+ `dd` Day of the month - 2 digits
+ `yyyy` Year in 4-digit format
+ `HH:mm` Time in 24-hour format - no seconds
+ `K` Time zone offset from Universal Time Coordinate (UTC)
+ `ss`; seconds

For more information about .NET format specifiers, see [Custom date and time format strings](https://learn.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings).

## Custom date and time format strings

[Custom date and time format strings](https://learn.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)

+ `s` The second, from 0 through 59.
2009-06-15T13:45:09 -> 9
More information: The "s" Custom Format Specifier.

+ `ss` The second, from 00 through 59.
More information: The "ss" Custom Format Specifier.
