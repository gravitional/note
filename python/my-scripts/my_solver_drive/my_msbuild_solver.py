import subprocess as subp
from my_config import prt_sep
from my_config import cfg_exe_msbuild, cfg_sln_solver, cfg_sln_common_Base

_args_build_sol_debug = [
    cfg_sln_solver, '/m', '/p:Platform=x64', '/v:m', '/t:build',
    '/p:Configuration=debug'
]
_args_build_sol_release = [
    cfg_sln_solver, '/m', '/p:Platform=x64', '/v:m', '/t:build',
    '/p:Configuration=release'
]

_args_link_base_mod_debug = [
    cfg_sln_common_Base, '/m', '/p:Platform=x64', '/v:m', '/t:BuildLink',
    '/p:Configuration=debug'
]

_args_link_base_mod_release = [
    cfg_sln_common_Base, '/m', '/p:Platform=x64', '/v:m', '/t:BuildLink',
    '/p:Configuration=release'
]


def msbuild_solver():
    prt_sep('Build solver\n')

    prt_sep('Build solver Debug\n')
    p = subp.Popen([cfg_exe_msbuild, *_args_build_sol_debug], shell=False)
    p.communicate()

    prt_sep('Build solver Release\n')
    p = subp.Popen([cfg_exe_msbuild, *_args_build_sol_release], shell=False)
    p.communicate()

    prt_sep('Link base module Debug\n')
    p = subp.Popen([cfg_exe_msbuild, *_args_link_base_mod_debug], shell=False)
    p.communicate()

    prt_sep('Link base module Release\n')
    p = subp.Popen([cfg_exe_msbuild, *_args_link_base_mod_release], shell=False)
    p.communicate()
