import os
import ycm_core

def DirectoryOfThisScript():
  return os.path.dirname( os.path.abspath( __file__ ) )

def Settings( **kwargs ):
  return {
    'flags': [
       '-x',
       'c++',
       '-std=c++14',
       '-Drir_EXPORTS',
       '-DENABLE_SLOWASSERT',
       '-DMEASURE',
       '-I'+DirectoryOfThisScript()+'/rir/src',
       '-isystem'+DirectoryOfThisScript()+'/external/custom-r/include',
       '-isystem'+DirectoryOfThisScript()+'/external/llvm-11.0.0/include',
       '-Wall',
       '-Wuninitialized',
       '-Wundef',
       '-Winit-self',
       '-Wcast-align',
       '-Woverloaded-virtual',
       '-Wctor-dtor-privacy',
       '-Wmissing-include-dirs',
       '-Wstrict-overflow=5',
       '-Werror',
       '-fno-rtti',
       '-fno-exceptions',
       '-Wimplicit-fallthrough',
    ],
  }
