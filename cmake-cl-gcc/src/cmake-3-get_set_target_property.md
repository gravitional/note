# target property

## [get_target_property][def]

Get a property from a target.

get_target_property(<variable> <target> <property>)
Get a property from a target. The value of the property is stored in the specified <variable>. If the target property is not found, <variable> will be set to <variable>-NOTFOUND. If the target property was defined to be an INHERITED property (see define_property()), the search will include the relevant parent scopes, as described for the define_property() command.

Use set_target_properties() to set target property values. Properties are usually used to control how a target is built, but some query the target instead. This command can get properties for any target so far created. The targets do not need to be in the current CMakeLists.txt file.

[def]: https://cmake.org/cmake/help/latest/command/get_target_property.html

## [set_target_properties][def2]

Targets can have properties that affect how they are built.

```cmake
set_target_properties(<targets> ...
                      PROPERTIES <prop1> <value1>
                      [<prop2> <value2>] ...)
```

Sets properties on targets. The syntax for the command is to list all the targets you want to change, and then provide the values you want to set next. You can use any prop value pair you want and extract it later with the get_property() or get_target_property() command.

Alias Targets do not support setting target properties.

[def2]: https://cmake.org/cmake/help/latest/command/set_target_properties.html#command:set_target_properties