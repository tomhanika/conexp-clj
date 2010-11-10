# Daniel Borchmann, 2010
# This code is a mixture from the sage interfaces of
#  - Lisp
#  - Octave
# with some specials for conexp-clj. The copyright is held by William
# Stein, see the corresponding files for more information.

# TODO:
#  - conversion from clojure to python data (and vice versa) is very slow

from sage.interfaces.expect import Expect, ExpectElement, ExpectFunction, FunctionElement, gc_disabled
from sage.misc.sage_eval import sage_eval

###

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
                        command = 'conexp-clj.sh',
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
#        print "Evaluating %s"%code
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

    def clear(self, var):
        self.eval("(ns-unmap *ns* '%s)"%var)

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
        return "="

    def _inequality_symbol(self):
        return "not="

    def _assign_symbol(self):
        raise NotImplementedError

    def _left_list_delim(self):
        return "["

    def _right_list_delim(self):
        return "]"

    def help(self, command):
        print self.eval("(doc %s)"%str(command))

    def function_call(self, function, args=None, kwds=None):
        args, kwds = self._convert_args_kwds(args, kwds)
        self._check_valid_function_name(function)
        if function[0:3] == "is_":
            function = function[3:] + "?"
        function = function.replace("_","-")
        arg_string = " ".join([s.name() for s in args])
        kwd_string = " ".join([":" + key + " " + value.name() for key, value in kwds.iteritems()])
        return self.new("(%s %s %s)"%(function,arg_string,kwd_string))

    def _coerce_impl(self, x, **kwds):
        if x == None or x == False:
            return "nil"
        elif x == True:
            return "true"
        elif isinstance(x, (set, frozenset)):
            return "#{" + str(map(lambda y:self._coerce_impl(y,kwds), x))[1:-1] + "}"
        elif isinstance(x, (list, tuple)):
            return "[" + str(map(lambda y:self._coerce_impl(y,kwds), x))[1:-1] + "]"
        elif isinstance(x, dict):
            string = "{"
            for key in x:
                string += str(self._coerce_impl(key,kwds)) + " " + str(self._coerce_impl(x[key],kwds)) + ", "
            string += "}"
            return string
        else:
            return str(x)


class ConexpCLJElement(ExpectElement):
    def __cmp__(self, other):
        P = self._check_valid()
        if not hasattr(other, 'parent') or P is not other.parent():
            other = P(other)

        # THIS IS DAMN WRONG! (but it works, sometimes...)
        return int(P.eval("(compare (hash %s) (hash %s))"%(self.name(), other.name())))

    def __eq__(self, other):
        P = self._check_valid()
        if not hasattr(other, 'parent') or P is not other.parent():
            other = P(other)

        return "true" == P.eval("(= %s %s)"%(self.name(), other.name()))

    def _operation(self, operation, right):
        P = self._check_valid()
        if not hasattr(right, 'parent') or P is not right.parent():
            right = P(right)

        try:
            return P("(%s %s %s)"%(operation, self._name, right._name))
        except Exception, msg:
            raise TypeError, msg

    def bool(self):
        P = self.parent()
        cmd = '(not (not %s))'%self._name
        return P.eval(cmd) == "true"

    def gen(self, n):
        P = self.parent()
        return P("(nth %s %s)"%(self._name, n))

    def _sage_repr(self):
        raise NotImplementedError

    def sage(self):
        P = self._check_valid()

        if hasattr(self, "__conexp_value__"):
            return self.__conexp_value__

        name = self._name
        val = None

        if "true" == P.eval("(nil? %s)"%name):
            val = None
        elif "true" == P.eval("(set? %s)"%name):
            val = frozenset(self)
        elif "true" == P.eval("(sequential? %s)"%name):
            val = tuple(self)
        elif "true" == P.eval("(instance? conexp.fca.lattices.Lattice %s)"%name):
            edges = map(lambda x: (str(x[0]), str(x[1])),
                        P("(conexp.layouts.util/edges %s)"%name))
            val = LatticePoset([[], edges])
        elif "true" == P.eval("(map? %s)"%name):
            dit = {}
            for pair in [x for x in self]:
                dit[pair[0]] = pair[1]
            val = dit
        else:
            val = sage_eval(str(self))

        self.__conexp_value__ = val
        return val

    def attribute(self, attrname):
        P = self._check_valid()
        return P("(%s %s)"%(attrname, self._name))

    def __pow__(self, n):
        P = self._check_valid()
        if not hasattr(n, 'parent') or P is not n.parent():
            n = P(n)
        return self._operation("expt", n)

    def _matrix_(self, n):
        raise NotImplementedError

    def _vector_(self, n):
        raise NotImplementedError

    def __len__(self):
        P = self._check_valid()
        return int(P.eval("(count %s)"%self._name))

    def __iter__(self):
        self_seq = self.seq()
        for i in xrange(0, len(self)):
            yield self_seq.gen(i)

    def _contains(self, x):
        P = self._check_valid()
        if not hasattr(x, 'parent') or P is not x.parent():
            x = P(x)
        return "true" == P.eval("(contains? %s %s)"%(self._name, x))

    def __getitem__(self, key):
        P = self._check_valid()
        return P("(get %s %s)"%(self._name, key))



class ConexpCLJFunction(ExpectFunction):
    def _sage_doc_(self):
        M = self._obj.parent()
        return M.help(self._name)


class ConexpCLJFunctionElement(FunctionElement):
    def _sage_doc_(self):
        M = self._obj.parent()
        return M.help(self._name)

###

conexp_clj = ConexpCLJ()

def reduce_load_conexp_clj():
    return conexp_clj

import os
def conexp_clj_console():
    if "nt" == os.name():
        os.system("conexp-clj.bat")
    else:
        os.system("conexp-clj.sh")

def conexp_clj_version():
    return str(conexp_clj('(conexp-version)')).strip()

###
