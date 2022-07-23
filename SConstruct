import os

env = Environment(
    CXXFLAGS='-O3 -Wall -fopenmp',
    LINKFLAGS='-fopenmp',
)

for key in ["PATH", "TERM"]:
    env["ENV"][key] = os.environ[key]

if ARGUMENTS.get("cxx", "gcc") == "clang":
    env.Replace(
        CXX="clang++",
        CXXFLAGS="-O3 -Wall -fopenmp=libomp",
        LINKFLAGS="-fopenmp=libomp",
    )

env.Program('picalc.cpp')
