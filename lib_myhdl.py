# See MyHDL.hs
import imp
# Trampoline code is here to make the invocation from haskell straightforward.
def run(mod_name, mod_text):
    m = imp.new_module(mod_name)
    exec(mod_text, m.__dict__)
    return [[1,2],[3,4]]

