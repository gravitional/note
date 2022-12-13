# powershell 进程监视

[Use PowerShell to Monitor Specific Process Creation](https://devblogs.microsoft.com/scripting/use-powershell-to-monitor-specific-process-creation/)

-UseNewEnvironment

Indicates that this cmdlet uses new environment variables specified for the process. 
By default, the started process runs with the environment variables inherited from the parent process.

On Windows, when you use UseNewEnvironment, 
the new process starts only containing the default environment variables defined for the Machine scope. 
This has the side effect that the $env:USERNAME is set to SYSTEM. None of the variables from the User scope are included.
