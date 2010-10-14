# Daniel Borchmann, 2010
# This code is a mixture from the sage interfaces of
#  - Lisp
#  - Octave
#
# The copyright is held by William Stein, see the corresponding files
# for more information.

from sage.interfaces.expect import Expect, ExpectElement, ExpectFunction, FunctionElement, gc_disabled, AsciiArtString
from sage.misc.misc import verbose
import random

class ConexpCLJ(Expect):
    def __init__(self,
                 maxread=100000,
                 script_subdirectory=None,
                 logfile=None,
                 server=None,
                 server_tmpdir=None):
        Expect.__init__(self,
                        name = 'conexp-clj',
                        prompt = 'user=> ', # fix this!
                        command = 'cclj',
                        maxread = maxread,
                        server = server,
                        server_tmpdir = server_tmpdir,
                        script_subdirectory = script_subdirectory,
                        restart_on_ctrlc = True,
                        verbose_start = False,
                        logfile = logfile,
                        eval_using_file_cutoff=1024)

    def _read_in_file_command(self, filename):
        return '(load-file "%s")'%filename

    def _start(self, *args, **kwds):
        Expect._start(self, *args, **kwds)

    def _quit_string(self):
        return "(System/exit 0)"

    def console(self):
        conexp_clj_console()

    def eval(self, code, strip=True, **kwds):
        with gc_disabled():
            code = str(code)
            out = self._eval_line(code)
            if out.find("Exception") != -1:
                raise TypeError, "Code:\n\t%s\nconexp-clj ERROR:\n\t%s"%(code,out)
            return out

    def _an_element_impl(self):
        return self(0)

    def set(self, var, value):
        cmd = "(def %s %s)"%(var,value)
        out = self.eval(cmd)

    def get(self, var):
        return self.eval('%s'%var)

    def _repr_(self):
        return "conexp-clj"

    def __reduce__(self):
        return reduce_load_conexp_clj, tuple([])

    def version(self):
        return conexp_clj_version()

    def _object_class(self):
        return ConexpCLJElement

    def _function_class(self):
        return ConexpCLJFunction

    def _function_element_class(self):
        return ConexpCLJFunctionElement

    def _true_symbol(self):
        return "true"

    def _false_symbol(self):
        return "false"

    def _equality_symbol(self):
        raise NotImplementedError

    def help(self, command):
        return self.eval("(doc %s)"%str(command))

    def function_call(self, function, args=None, kwds=None):
        args, kwds = self._convert_args_kwds(args, kwds)
        function = function.replace("_","-")
        self._check_valid_function_name(function)
        return self.new("(%s %s)"%(function, " ".join([s.name() for s in args])))

class ConexpCLJElement(ExpectElement):
    pass

class ConexpCLJFunction(ExpectFunction):
    def _sage_doc_(self):
        M = self._obj.parent()
        return M.help(self._name)

class ConexpCLJFunctionElement(FunctionElement):
    def _sage_doc_(self):
        M = self._obj.parent()
        return M.help(self._name)

conexp_clj = ConexpCLJ(logfile="/home/borch/logfile")

def reduce_load_conexp_clj():
    return conexp_clj

import os
def conexp_clj_console():
    os.system('cclj')

def conexp_clj_version():
    return str(conexp_clj('(conexp-version)').strip())
