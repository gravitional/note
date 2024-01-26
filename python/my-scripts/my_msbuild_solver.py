import subprocess as subp


def prt_sep(istr: str = ''):
    print('<' * 35 + '[ymy] ' + istr, flush=True)


_exe_msbuild = r'C:/Program Files (x86)/Microsoft Visual Studio/2019/Professional/MSBuild/Current/Bin/MSBuild.exe'

_args_build_sol_debug = [
    r'C:/Solver/build/solver.sln', '/m', '/p:Platform=x64', '/v:m', '/t:build',
    '/p:Configuration=debug'
]
_args_build_sol_release = [
    r'C:/Solver/build/solver.sln', '/m', '/p:Platform=x64', '/v:m', '/t:build',
    '/p:Configuration=release'
]

_args_link_base_mod_debug = [
    r'C:/Solver/build/src/common/Common/Base/Base.vcxproj', '/m',
    '/p:Platform=x64', '/v:m', '/t:BuildLink', '/p:Configuration=debug'
]

_args_link_base_mod_release = [
    r'C:/Solver/build/src/common/Common/Base/Base.vcxproj', '/m',
    '/p:Platform=x64', '/v:m', '/t:BuildLink', '/p:Configuration=release'
]


def msbuild_solver():
    prt_sep('Build solver\n')

    prt_sep('Build solver Debug\n')
    p = subp.Popen([_exe_msbuild, *_args_build_sol_debug], shell=False)
    p.communicate()

    prt_sep('Build solver Release\n')
    p = subp.Popen([_exe_msbuild, *_args_build_sol_release], shell=False)
    p.communicate()

    prt_sep('Link base module Debug\n')
    p = subp.Popen([_exe_msbuild, *_args_link_base_mod_debug], shell=False)
    p.communicate()

    prt_sep('Link base module Release\n')
    p = subp.Popen([_exe_msbuild, *_args_link_base_mod_release], shell=False)
    p.communicate()
